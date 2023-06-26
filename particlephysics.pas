UNIT particlePhysics;

{$mode objfpc}{$H+}

INTERFACE
USES vectors,GL,serializationUtil;

TYPE
  TTemplateStar=record
    p0,v0,color:TVector3;
    radius,mass:Tfloat;
  end;
  TStar = record
    periodicTrajectory:boolean;
    trajectory:array of record
      p:TVector3;
      time:double;
    end;
    p,v,a,color:TVector3;
    radius,mass : Tfloat;
  end;

  TParticle = record
    p,v,a:TVector3;
    flaggedForRemoval:boolean;
  end;

  { TStarSys }

  TStarSys=object //Declaring there as object(T_serializable) leads to a enormous overhead (~ +200%) due to 800 virtual method tables in RAM
    optimized:boolean;
    qualityMeasure,
    massDelta,        //ranged 1 - 100
    vDelta,           //ranged ln(1) - ln(infinity)
    rDelta:Tfloat;    //ranged ln(1) - ln(infinity)
    starCount:longint;
    star:array[0..4] of TTemplateStar;
    PROCEDURE initialize(CONST starCount_:longint);
    PROCEDURE randomizeStars(CONST restrictedVelocities:boolean);
    PROCEDURE normalize;
    PROCEDURE seminormalize;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
    PROCEDURE evaluateSystem;
    FUNCTION quality:Tfloat;

    FUNCTION mult(CONST factor:double):TStarSys;
    FUNCTION plus(CONST other:TStarSys):TStarSys;
    FUNCTION simplexStep(CONST index:longint; CONST h:double):TStarSys;
  end;

  TsysTarget=(none,highestStability,smallMassVariance,bigMassVariance,uniformVelocities,varyingVelocities);
CONST
  CsysTargetName:array[TsysTarget] of string=('random','most stable','Target: small mass variance','Target: big mass variance','Target: uniform velocities','Target: varying velocities');
TYPE

  { TCachedSystems }
  PCachedSystems=^TCachedSystems;
  TCachedSystems=object(T_serializable)
    sys:array[2..5,0..199] of TStarSys;
    rejection:array[2..5] of record
      count,tally:longint;
      best :double;
    end;
    sysRange:array[2..5] of record
      massDelta,
      vDelta   ,
      rDelta   :array[0..1] of Tfloat;
    end;
    sysCs:TRTLCriticalSection;
    lastResponse:TStarSys;
    backgroundRunning:longint;
    destroying:boolean;
    statCounter:longint;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE writeStatistics;
    PROCEDURE writeDetails;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE prepareSystem(CONST starCount:longint);
    PROCEDURE prepareAnySystem;
    FUNCTION getSystem(CONST starCount:longint; CONST target:TsysTarget):TStarSys;
    PROCEDURE startBackgroundCalculation;
    FUNCTION predefinedSystemCount:longint;
    FUNCTION getPredefinedSystem(CONST index:byte):TStarSys;
    FUNCTION predefinedSystemName(CONST index:byte):string;
  end;

  TDustInitMode = (dim_stableDisk,
                   dim_stableDiskReversed,
                   dim_randomCloud,
                   dim_stillBackgroundCloud,
                   dim_singleRing,
                   dim_threeRings,
                   dim_shell,
                   dim_planetLike,
                   dim_lonelyCloud);
CONST
  CDustInitModeName:array[TDustInitMode] of string=
    ('stable disk',
     'stable disk (reversed)',
     'random cloud',
     'background dust',
     'singleRing',
     'threeRings',
     'shell',
     'sattelites',
     'lonely cloud');
TYPE

  { TParticleEngine }

  TParticleEngine = class
  private
    star,trStar: array of TStar;
    lastStarTimeStep:Tfloat;
    dust :  array of TParticle;
    removalCounter:longint;
    sqrSystemRadius:double;
    PROCEDURE MoveParticles(CONST dt:Tfloat; CONST stressed:boolean);
  public
    cachedSystems:TCachedSystems;
    totalTime,dtFactor,trajectoryMaxTime:Tfloat;
    precisionFocused:boolean;

    CONSTRUCTOR create;
    DESTRUCTOR destroy; override;
    PROCEDURE update(CONST tickDelta:qword; stressed:boolean);
    PROCEDURE DrawParticles(CONST ParticleList: GLuint; CONST pointSize:Tfloat);

    PROCEDURE initDust(CONST particleCount:longint; starMask:byte; CONST mode:TDustInitMode);
    PROCEDURE initPresetStars(CONST presetIndex:byte);
    PROCEDURE initStars(starCount:longint;  CONST target:TsysTarget);
    PROCEDURE initStars(CONST sys:TStarSys);
    PROCEDURE resetStars;
    FUNCTION dustCount:longint;
    FUNCTION starCount:longint;
    PROCEDURE multiplySize(CONST factor:Tfloat);

  end;

IMPLEMENTATION
USES math,sysutils,LCLProc;
CONST minTimeStep=1E-6;
      maxTimeStep=5E-2;
      maxDustTimeStep=1E-1;
      MAX_QUALITY=1000;

FUNCTION dtOf(CONST aSqrMax:double):double;
  begin
    result:=sqrt(1E-6/sqrt(aSqrMax));
//    writeln('A=',aSqrMax);
    if result>maxTimeStep then result:=maxTimeStep;
    if result<minTimeStep then result:=minTimeStep;
  end;

FUNCTION accelFactor(CONST pSelf,pDrawnTowards:TVector3):TVector3; inline;
  VAR f:Tfloat;
  begin
    result:=pDrawnTowards-pSelf;
    f:=1/sqrt(sqr(result[0])+sqr(result[1])+sqr(result[2]));
    f:=f*f*f;
    result[0]*=f;
    result[1]*=f;
    result[2]*=f;
  end;

{ TCachedSystems }

FUNCTION makeSystemsInBackground(p:pointer):ptrint;
  VAR r: longint;
  begin
    with PCachedSystems(p)^ do begin
      repeat prepareAnySystem until destroying;
      r:=interlockedDecrement(backgroundRunning);
      writeln('Background calculation thread stopped, ',r,' are still running');
    end;
    result:=0;
  end;

CONSTRUCTOR TCachedSystems.create;
  VAR i,j:longint;
  begin
    randomize;
    initCriticalSection(sysCs);
    backgroundRunning:=0;
    for i:=2 to 5 do with rejection[i] do begin
      best:=0;
      count:=0;
      tally:=0;
    end;
    if not loadFromFile(ChangeFileExt(paramStr(0),'.cached_sys')) then begin
      for i:=2 to 5 do with sysRange[i] do begin
        massDelta[0]:=infinity; massDelta[1]:=0;
        vDelta   [0]:=infinity; vDelta   [1]:=0;
        rDelta   [0]:=infinity; rDelta   [1]:=0;
      end;
      for i:=2 to 5 do for j:=0 to 199 do sys[i,j].qualityMeasure:=-infinity;
      writeln('Could not load cached systems... I will calculate some in a separate thread');
    end else writeStatistics;
    startBackgroundCalculation;
  end;

DESTRUCTOR TCachedSystems.destroy;
  VAR i:longint;
  begin
    destroying:=true;
    for i:=2 to 5 do while isInfinite(sys[i,199].qualityMeasure) do prepareSystem(i);
    while backgroundRunning>0 do sleep(1);
    enterCriticalSection(sysCs);
    writeStatistics;
    saveToFile(ChangeFileExt(paramStr(0),'.cached_sys'));
    writeDetails;
    leaveCriticalSection(sysCs);
    doneCriticalSection(sysCs);
  end;

PROCEDURE TCachedSystems.writeStatistics;
  VAR i,j,k:longint;
  begin
    writeln('===============================================================================================\');
    writeln(' Cached system statistics:                                                                     |');
    writeln('              Best   Median    Worst    Mass---------\    Radius-------\    Velocity-----\ Opt |');
    for i:=2 to 5 do begin
      k:=0;
      for j:=0 to 199 do if sys[i,j].optimized then inc(k);
      writeln(' ',i,' stars: ',sys[i,0].qualityMeasure:8:3,' ',sys[i,100].qualityMeasure:8:3,' ',sys[i,199].qualityMeasure:8:3,' ',sysRange[i].massDelta[0]:8:3,' ',sysRange[i].massDelta[1]:8:3,' ',sysRange[i].rDelta[0]:8:3,' ',sysRange[i].rDelta[1]:8:3,' ',sysRange[i].vDelta[0]:8:3,' ',sysRange[i].vDelta[1]:8:3,' ',k:3,' |');
    end;
    writeln('-----------------------------------------------------------------------------------------------|');
    writeln(' Rejections:                                                                                   |');
    for i:=2 to 5 do writeln(' ',i,' stars: ',rejection[i].count:8,' best was: ',rejection[i].best:8:3,'                                                          |');
    writeln('===============================================================================================/');
    statCounter:=0;
  end;

PROCEDURE TCachedSystems.writeDetails;
  VAR handle:textFile;
  PROCEDURE writeSys(CONST sys:TStarSys);
    VAR k:longint;
    begin
      write(handle,sys.qualityMeasure,';');
      for k:=0 to sys.starCount-1 do write(handle,
                                               sys.star[k].mass,';',
                                               sys.star[k].p0[0],';',
                                               sys.star[k].p0[1],';',
                                               sys.star[k].p0[2],';',
                                               sys.star[k].v0[0],';',
                                               sys.star[k].v0[1],';',
                                               sys.star[k].v0[2],';');
      writeln(handle,'');
    end;

  VAR i,j:longint;
  begin
    assign(handle,ChangeFileExt(paramStr(0),'.cached_sys.txt'));
    rewrite(handle);
    for i:=2 to 5 do for j:=0 to 199 do writeSys(sys[i,j]);
    close(handle);
  end;

FUNCTION TCachedSystems.getSerialVersion: dword;
  begin
    result:=3;
  end;

FUNCTION TCachedSystems.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i,j:longint;
  begin
    if not(inherited) then exit(false);
    result:=true;
    for i:=2 to 5 do begin
      with sysRange[i] do for j:=0 to 1 do begin
        massDelta[j]:=stream.readDouble;
        vDelta   [j]:=stream.readDouble;
        rDelta   [j]:=stream.readDouble;
      end;
      for j:=0 to 199 do result:=result and sys[i,j].loadFromStream(stream);
    end;
    result:=result and stream.allOkay;
  end;

PROCEDURE TCachedSystems.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:longint;
  begin
    inherited;
    for i:=2 to 5 do begin
      with sysRange[i] do for j:=0 to 1 do begin
        stream.writeDouble(massDelta[j]);
        stream.writeDouble(vDelta   [j]);
        stream.writeDouble(rDelta   [j]);
      end;
      for j:=0 to 199 do sys[i,j].saveToStream(stream);
    end;
  end;

PROCEDURE randomStarStats(OUT radius,mass:Tfloat; OUT color:TVector3);
  VAR density:Tfloat;
      commonFactor:Tfloat;
      x:Tfloat;
  begin
    mass:=exp(((random+random+random)*2/3-1)*ln(10)); //about log-normal distributed 0.1 - 10, most probable: 1
    // volume = radius^3  //let's forget about the 4/3 pi part; it's just a proportionality factor
    // radius = volume^(1/3)
    //        = (mass/density)^(1/3)
    // 0.05    <= radius                <= 1
    // 0.05^3  <= volume = mass/density <= 1
    // 0.05^-3 >= density/mass          >= 1
    // 800     >= density               >= 10       //to be on the safe side...

    commonFactor:=random; //This links density and brightness
    density:=10+790*commonFactor;
    radius:=power(mass/density,1/3);

    x:=0.2+2.7*commonFactor;
    color:=vectorOf(max(0,min(1,x)),
                    max(0,min(1,x-1)),
                    max(0,min(1,x-2)));
  end;

PROCEDURE multiplyStarMass(VAR star:TStar; CONST factor:Tfloat);
  VAR density,commonFactor,x:Tfloat;
  begin
    density:=star.mass/power(star.radius,3);
    commonFactor:=(density-10)/790;
    star.mass*=factor;
    star.radius:=power(star.mass/density,1/3);
    x:=0.2+2.7*commonFactor;
    star.color:=vectorOf(max(0,min(1,x)),
                         max(0,min(1,x-1)),
                         max(0,min(1,x-2)));
  end;

CONST MAX_ACCEPTED_RADIUS=10;

PROCEDURE optimizeSystem(VAR newSystem:TStarSys);
  VAR temp,step,center,newPoint:TStarSys;
      simplex:array of TStarSys;
      i,j,stepCounter:longint;
      startTicks:qword;
      initialQuality:Tfloat;
      contractionsLeft:longint=5;
  begin
    startTicks:=GetTickCount64;
    initialQuality:=newSystem.quality;

    setLength(simplex,newSystem.starCount*6-5);
    for i:=0 to length(simplex)-1 do begin
      if i=0 then simplex[0]:=newSystem
             else simplex[i]:=newSystem.simplexStep(i-1,0.01);
      for j:=0 to i-1 do if simplex[i].quality>simplex[j].quality then begin
        temp      :=simplex[i];
        simplex[i]:=simplex[j];
        simplex[j]:=temp;
      end;
    end;
    for stepCounter:=0 to 100 do if (simplex[0].quality<MAX_QUALITY) and (contractionsLeft>0) and (GetTickCount64<startTicks+30000) then begin
      center:=simplex[0];
      for i:=1 to length(simplex)-2 do center:=center.plus(simplex[i]);
      center:=center.mult(1/(length(simplex)-1));
      step:=center.mult(-1).plus(simplex[length(simplex)-1]);
      newPoint:=center.plus(step);
      newPoint.seminormalize;

      if newPoint.quality>simplex[length(simplex)-2].quality then begin
        if newPoint.quality>simplex[0].quality then begin
          //new point is better than best...
          temp:=newPoint.plus(step);
          temp.seminormalize;
          if temp.quality>newPoint.quality then begin
            newPoint:=temp;
            inc(contractionsLeft);
          end;
        end;
        i:=length(simplex)-1;
        simplex[i]:=newPoint;
        while (i>0) and (simplex[i-1].quality<simplex[i].quality) do begin
          temp:=simplex[i]; simplex[i]:=simplex[i-1]; simplex[i-1]:=temp; dec(i);
        end;
      end else begin
        if newPoint.quality>simplex[length(simplex)-1].quality
        then newPoint:=simplex[0].mult(-0.5).plus(newPoint                  .mult(0.5))
        else newPoint:=simplex[0].mult(-0.5).plus(simplex[length(simplex)-1].mult(0.5));
        if newPoint.quality>simplex[length(simplex)-1].quality then begin
          i:=length(simplex)-1;
          simplex[i]:=newPoint;
          while (i>0) and (simplex[i-1].quality<simplex[i].quality) do begin
            temp:=simplex[i]; simplex[i]:=simplex[i-1]; simplex[i-1]:=temp; dec(i);
          end;
        end else begin
          //shrink all
          for i:=1 to length(simplex)-1 do begin
            simplex[i]:=simplex[i].plus(simplex[0]).mult(0.5);
            simplex[i].seminormalize;
            dec(contractionsLeft);
            for j:=0 to i-1 do if simplex[i].quality>simplex[j].quality then begin
              temp      :=simplex[i];
              simplex[i]:=simplex[j];
              simplex[j]:=temp;
            end;
          end;
        end;
      end;
    end;
    newSystem:=simplex[0];
    newSystem.optimized:=true;
    {$ifdef debugMode}
    writeln('Optimization of ',newSystem.starCount,'-star system took ',(GetTickCount64-startTicks)/1000:0:3,'s (q:',initialQuality:8:3,'->',newSystem.qualityMeasure:8:3,')');
    {$endif}
  end;

PROCEDURE TCachedSystems.prepareSystem(CONST starCount: longint);
  VAR newSystem:TStarSys;
      i:longint;
      startTicks:qword;
      initialQuality:double;
      toOptimize:longint=-1;
      optimizedCount:longint=0;
  begin
    startTicks:=GetTickCount64;
    enterCriticalSection(sysCs);
    for i:=0 to 199 do if sys[starCount,i].optimized then inc(optimizedCount);
    if rejection[starCount].tally>=100 then begin
      dec(rejection[starCount].tally,100);
      for i:=180 downto 0 do if (sys[starCount,i].qualityMeasure<MAX_QUALITY) and not(sys[starCount,i].optimized) then toOptimize:=i;
      if toOptimize>=0 then begin
        writeln('Now optimizing ',starCount,'-star system of rank ',toOptimize);
        newSystem:=sys[starCount,toOptimize];
        initialQuality:=newSystem.quality;
        for i:=toOptimize to 198 do sys[starCount,i]:=sys[starCount,i+1];
        sys[starCount,199].qualityMeasure:=0; //Will be replaced eventually...
      end;
    end;
    leaveCriticalSection(sysCs);

    if toOptimize>=0 then optimizeSystem(newSystem)
    else begin
      newSystem.initialize(starCount);
      newSystem.randomizeStars(random>0.5);
      if (optimizedCount>=160) and (newSystem.quality>sys[starCount,199].qualityMeasure) and (newSystem.quality<MAX_QUALITY) then begin
        initialQuality:=newSystem.quality;
        optimizeSystem(newSystem);
      end else newSystem.quality;
    end;

    enterCriticalSection(sysCs);
    if newSystem.quality>sys[starCount,199].qualityMeasure then begin
      if newSystem.rDelta   <sysRange[starCount].rDelta   [0] then sysRange[starCount].rDelta   [0]:=newSystem.rDelta;
      if newSystem.rDelta   >sysRange[starCount].rDelta   [1] then sysRange[starCount].rDelta   [1]:=newSystem.rDelta;
      if newSystem.vDelta   <sysRange[starCount].vDelta   [0] then sysRange[starCount].vDelta   [0]:=newSystem.vDelta;
      if newSystem.vDelta   >sysRange[starCount].vDelta   [1] then sysRange[starCount].vDelta   [1]:=newSystem.vDelta;
      if newSystem.massDelta<sysRange[starCount].massDelta[0] then sysRange[starCount].massDelta[0]:=newSystem.massDelta;
      if newSystem.massDelta>sysRange[starCount].massDelta[1] then sysRange[starCount].massDelta[1]:=newSystem.massDelta;

      sys[starCount,199]:=newSystem;
      i:=199;
      while (i>0) and (sys[starCount,i-1].qualityMeasure<sys[starCount,i].qualityMeasure) do begin
        newSystem:=sys[starCount,i];
        sys[starCount,i]:=sys[starCount,i-1];
        sys[starCount,i-1]:=newSystem;
        dec(i);
      end;
      if newSystem.optimized
      then writeln('Created new system of ',starCount,' stars, ranked ',i:3,'; quality: ',initialQuality:8:3,'--[opt]->',newSystem.qualityMeasure:8:3,' (',(GetTickCount64-startTicks)/1000:0:3,'s)')
      else writeln('Created new system of ',starCount,' stars, ranked ',i:3,'; quality: ',newSystem.qualityMeasure:8:3);

      inc(statCounter);
      if statCounter>=100 then writeStatistics;
    end else begin
      inc(rejection[starCount].count);
      inc(rejection[starCount].tally);
      rejection[starCount].best:=max(rejection[starCount].best,newSystem.quality);
    end;
    leaveCriticalSection(sysCs);
  end;

PROCEDURE TCachedSystems.prepareAnySystem;
  VAR k: longint;
  begin
    {$ifdef debugMode}
    //if not(isInfinite(sys[2,199].qualityMeasure)
    //    or isInfinite(sys[3,199].qualityMeasure)
    //    or isInfinite(sys[4,199].qualityMeasure)
    //    or isInfinite(sys[5,199].qualityMeasure))
    //then begin
    //  destroying:=true;
    //  exit;
    //end;
    {$endif}

    repeat k:=2+random(4) until sys[k,199].qualityMeasure<1000;

    //for i:=3 to 5 do if sys[i,199].qualityMeasure*i<sys[k,199].qualityMeasure*k then k:=i;
    prepareSystem(k);
  end;

FUNCTION TCachedSystems.getSystem(CONST starCount: longint; CONST target:TsysTarget): TStarSys;
  FUNCTION isBetter(CONST A,B:TStarSys):boolean;
    begin
      case target of
        highestStability  : result:=(A.quality  >B.quality);
        smallMassVariance : result:=(A.massDelta<B.massDelta);
        bigMassVariance   : result:=(A.massDelta>B.massDelta);
        uniformVelocities : result:=(A.vDelta   <B.vDelta);
        varyingVelocities : result:=(A.vDelta   >B.vDelta);
        else result:=false;
      end;
    end;

  FUNCTION biasedRandom:longint;
    begin
      repeat
        result:=random(200);
      until random>result/200;
    end;

  VAR i:longint;
      alternative:TStarSys;
  begin
    if starCount<=1 then begin
      result.initialize(1);
      with result.star[0] do begin
        randomStarStats(radius,mass,color);
        p0:=ZERO_VECTOR;
        v0:=ZERO_VECTOR;
      end;
    end else begin
      enterCriticalSection(sysCs);
      result:=sys[starCount,biasedRandom];
      if isInfinite(result.qualityMeasure) then begin
        while isInfinite(sys[starCount,0].qualityMeasure) do prepareSystem(starCount);
        result:=sys[starCount,0];
      end else begin
        if target<>none then for i:=0 to 19 do begin
          alternative:=sys[starCount,biasedRandom];
          if isBetter(alternative,result)
          then result:=alternative;
        end;
      end;
      leaveCriticalSection(sysCs);
    end;
    lastResponse:=result;
  end;

PROCEDURE TCachedSystems.startBackgroundCalculation;
  begin
    destroying:=false;
    interLockedIncrement(backgroundRunning);
    writeln('Started new background calculation thread; ',backgroundRunning,' are running');
    beginThread(@makeSystemsInBackground,@self);
  end;

FUNCTION TCachedSystems.getPredefinedSystem(CONST index:byte):TStarSys;
  FUNCTION planar3BodyOf(CONST x0,y0,x1,y1,x2,y2,vx0,vy0,vx1,vy1,vx2,vy2:double):TStarSys;
    begin
      result.initialize(3);
      result.star[0].radius:=0.1;
      result.star[1].radius:=0.1;
      result.star[2].radius:=0.1;
      result.star[0].mass:=1;
      result.star[1].mass:=1;
      result.star[2].mass:=1;
      result.star[0].color:=vectorOf(1,0  ,0);
      result.star[1].color:=vectorOf(1,0.5,0);
      result.star[2].color:=vectorOf(1,1  ,0);

      result.star[0].p0:=vectorOf(x0,y0,0);
      result.star[1].p0:=vectorOf(x1,y1,0);
      result.star[2].p0:=vectorOf(x2,y2,0);
      result.star[0].v0:=vectorOf(vx0,vy0,0);
      result.star[1].v0:=vectorOf(vx1,vy1,0);
      result.star[2].v0:=vectorOf(vx2,vy2,0);
    end;

  FUNCTION planar3BodyOf(CONST vx,vy:double):TStarSys;
    begin
      result:=planar3BodyOf(-1,0,
                             1,0,
                             0,0,vx,vy,
                                 vx,vy,-2*vx,-2*vy);
    end;

  PROCEDURE addBody(CONST p_,v_,color_:TVector3; CONST mass_,radius_:double);
    begin
      result.starCount+=1;
      with result.star[ result.starCount-1] do begin
        p0:=p_; v0:=v_; mass:=mass_; radius:=radius_; color:=color_;
      end;
    end;

  FUNCTION planar2BodyOf(CONST m0,m1,v:double):TStarSys;
    begin
      result.starCount:=2;
      result.star[0].radius:=0.1;
      result.star[1].radius:=0.1;
      result.star[0].mass:=m0;
      result.star[1].mass:=m1;
      result.star[0].color:=vectorOf( 0,0 ,0.6);
      result.star[1].color:=vectorOf( 1,1 ,0.4);
      result.star[0].p0:=vectorOf( 1,0  ,0);
      result.star[1].p0:=vectorOf(-m0/m1,0  ,0);
      result.star[0].v0:=vectorOf(0, v,0);
      result.star[1].v0:=vectorOf(0,-m0/m1*v,0);
    end;
  CONST T0:TVector3=( 1, 1, 1);
        T1:TVector3=(-1,-1, 1);
        T2:TVector3=(-1, 1,-1);
        T3:TVector3=( 1,-1,-1);
  begin
    result.initialize(0);
    case index of
      0: begin
           result:=planar2BodyOf(0.1,10,pi);
           result.star[0].radius:=0.05;
           result.star[1].radius:=1/3;
         end;
      1: result:=planar3BodyOf(-0.97000436, 0.24308753,0,0,0.97000436,-0.24308753, 0.4662036850, 0.4323657300,-0.93240737, -0.86473146,0.4662036850, 0.4323657300);
      2: result:=planar2BodyOf(10,10,0.5*sqrt(10));
      3: result:=planar2BodyOf(10,10,1/3*sqrt(10));
      4: begin
           addBody(vectorOf( sin(0*pi/3),cos(0*pi/3),0),
                   vectorOf(-cos(0*pi/3),sin(0*pi/3),0)*0.7598356856515924 ,
                   vectorOf(1,0,0),1,0.1);
           addBody(vectorOf( sin(2*pi/3),cos(2*pi/3),0),
                   vectorOf(-cos(2*pi/3),sin(2*pi/3),0)*0.7598356856515924 ,
                   vectorOf(0,1,0),1,0.1);
           addBody(vectorOf( sin(4*pi/3),cos(4*pi/3),0),
                   vectorOf(-cos(4*pi/3),sin(4*pi/3),0)*0.7598356856515924 ,
                   vectorOf(0,0,1),1,0.1);
         end;
      5: begin
           addBody(vectorOf( sin(0*pi/4),cos(0*pi/4),0),
                   vectorOf(-cos(0*pi/4),sin(0*pi/4),0)*0.97831834347851587 ,
                   vectorOf(1,0,0),1,0.1);
           addBody(vectorOf( sin(2*pi/4),cos(2*pi/4),0),
                   vectorOf(-cos(2*pi/4),sin(2*pi/4),0)*0.97831834347851587 ,
                   vectorOf(0,1,0),1,0.1);
           addBody(vectorOf( sin(4*pi/4),cos(4*pi/4),0),
                   vectorOf(-cos(4*pi/4),sin(4*pi/4),0)*0.97831834347851587 ,
                   vectorOf(0,0,1),1,0.1);
           addBody(vectorOf( sin(6*pi/4),cos(6*pi/4),0),
                   vectorOf(-cos(6*pi/4),sin(6*pi/4),0)*0.97831834347851587 ,
                   vectorOf(1,1,0),1,0.1);
         end;
      6: begin
           addBody(vectorOf( sin(0*pi/5),cos(0*pi/5),0),
                   vectorOf(-cos(0*pi/5),sin(0*pi/5),0)*1.173193044844357 ,
                   vectorOf(1,0,0),1,0.1);
           addBody(vectorOf( sin(2*pi/5),cos(2*pi/5),0),
                   vectorOf(-cos(2*pi/5),sin(2*pi/5),0)*1.173193044844357 ,
                   vectorOf(0,1,0),1,0.1);
           addBody(vectorOf( sin(4*pi/5),cos(4*pi/5),0),
                   vectorOf(-cos(4*pi/5),sin(4*pi/5),0)*1.173193044844357 ,
                   vectorOf(0,0,1),1,0.1);
           addBody(vectorOf( sin(6*pi/5),cos(6*pi/5),0),
                   vectorOf(-cos(6*pi/5),sin(6*pi/5),0)*1.173193044844357 ,
                   vectorOf(1,1,0),1,0.1);
           addBody(vectorOf( sin(8*pi/5),cos(8*pi/5),0),
                   vectorOf(-cos(8*pi/5),sin(8*pi/5),0)*1.173193044844357,
                   vectorOf(0.5,0.5,0.5),1,0.1);
         end;
      7: result:=planar3BodyOf(0,0,1,0,-1,0,0.0,0.0,0.0,1.118,0.0,-1.118);
      8: result:=planar3BodyOf(0.3361300950,0,0.7699893804,0,-1.1061194753,0,0,1.5324315370,0,-0.6287350978,0,-0.9036964391);
      9: result:=planar3BodyOf(-0.3362325077,0,1.3136838813,0,-0.9774513736,0,0,1.2659472954,0,-0.0329092795,0,-1.2330380159);
     10: begin
           addBody(vectorOf( 1,0,0),
                   vectorOf( 0,0.4702920123413744,-0.6198993549002166),
                   vectorOf( 1,0,0),1,0.1);
           addBody(vectorOf(-1,0,0),
                   vectorOf(0,-0.4702920123413744,-0.6198993549002166) ,
                   vectorOf(0,1,0),1,0.1);
           addBody(vectorOf(0,0,0),
                   vectorOf(0,0    ,2*0.6198993549002166) ,
                   vectorOf(0,0,1),1,0.1);
         end;
     11: begin
           addBody(T0,(T1-T0)*0.18363246390466537+(T2-T0)*0.10717122677880012-(T3-T0)*0.29230278350495464,vectorOf(1,0,0),1,0.1);
           addBody(T1,(T2-T1)*0.18363246390466537+(T3-T1)*0.10717122677880012-(T0-T1)*0.29230278350495464,vectorOf(0,1,0),1,0.1);
           addBody(T2,(T3-T2)*0.18363246390466537+(T0-T2)*0.10717122677880012-(T1-T2)*0.29230278350495464,vectorOf(0,0,1),1,0.1);
           addBody(T3,(T0-T3)*0.18363246390466537+(T1-T3)*0.10717122677880012-(T2-T3)*0.29230278350495464,vectorOf(1,1,0),1,0.1);
         end;
     12: begin
           addBody(vectorOf(0, 1,0),vectorOf( 0.45688142227803608,0.3117880260663991,0.082631775790250189),vectorOf(1,0,0),1,0.1);
           addBody(vectorOf(0,-1,0),vectorOf(-0.45688142227803608,-0.3117880260663991,-0.082631775790250189),vectorOf(0,1,0),1,0.1);
           addBody(vectorOf( 2,0,0),vectorOf(-0.18282017959405006,0.10403639617741009,0.9673654095501405),vectorOf(0,0,1),1,0.1);
           addBody(vectorOf(-2,0,0),vectorOf( 0.18282017959405006,-0.10403639617741009,-0.9673654095501405),vectorOf(1,1,0),1,0.1);
         end;
     13: begin
           addBody(vectorOf(0.0012315610350301044,0.0021858769689238715,-0.00004),vectorOf(0.00024677968826359778,-0.000571849610179078,0.0),
                   vectorOf(1,1,0.5),
                   10,0.5);
           addBody(vectorOf(1.7906857624394787,0,0.01),vectorOf(0,2.3795206142637828,0),
                   vectorOf(0.8,0.8,0.8),
                   0.01,
                   0.05);
           addBody(vectorOf(-3.0222467974695832,0,0.01),vectorOf(0,-1.8076710040847048,0),
                   vectorOf(0.5,0.5,1),
                   0.01,
                   0.1);
           addBody(vectorOf(0,4.9707768727435386,0.01),vectorOf(-1.4320798807774509,0,0),
                   vectorOf(0.7,0.35,0),
                   0.01,0.2);
           addBody(vectorOf(0,-7.15665384166741,0.01),vectorOf(1.185300192513853,0,0),
                   vectorOf(0.2,1,0.2),
                   0.01,0.08);
         end;
    end;
    lastResponse:=result;
  end;

FUNCTION TCachedSystems.predefinedSystemName(CONST index:byte):string;
  begin
    case index of
      0: result:='Star and planet';
      1: result:='Infinity loop';
      2: result:='Two masses on circle';
      3: result:='Two masses on ellipses';
      4: result:='Equilateral triangle';
      5: result:='Square';
      6: result:='Equilateral pentagon';
      7: result:='Three masses on a line';
      8: result:='Broucke A 2';
      9: result:='Henon 26';
     10: result:='Helix';
     11: result:='Tetrahedron';
     12: result:='Symmetric 4';
     13: result:='Planetary';
    end;
  end;

FUNCTION TCachedSystems.predefinedSystemCount:longint;
  begin
    result:=14;
  end;

PROCEDURE TStarSys.initialize(CONST starCount_: longint);
  begin
    optimized:=false;
    qualityMeasure:=-infinity;
    starCount:=starCount_;
    massDelta:=0;
    vDelta:=0;
    rDelta:=0;
  end;

PROCEDURE TStarSys.randomizeStars(CONST restrictedVelocities:boolean);
  VAR i, j: integer;
      closeToOther, boundToOther: boolean;
      dir:TVector3;
  begin
    for i:=0 to starCount-1 do with star[i] do begin
      randomStarStats(radius,mass,color);
      repeat
        p0:=randomInSphere*MAX_ACCEPTED_RADIUS;
        closeToOther:=false;
        for j:=0 to i-1 do closeToOther:=closeToOther or (euklideanNorm(p0-star[j].p0)<2*(radius+star[j].radius));
      until not(closeToOther);
      repeat
        v0:=randomInSphere*5;
        boundToOther:=i=0;
        for j:=0 to i-1 do boundToOther:=boundToOther or (sqrEuklideanNorm(v0-star[j].v0)*euklideanNorm(p0-star[j].p0)<2*max(star[j].mass,star[i].mass))
      until boundToOther;
    end;
    normalize;
    if restrictedVelocities then for i:=0 to starCount-1 do with star[i] do begin
      dir:=p0*(1/euklideanNorm(p0));
      v0-=dir*(dir*v0);
    end;
  end;

PROCEDURE TStarSys.normalize;
  VAR tmp:TTemplateStar;
      i,j:longint;
      transform:TMatrix3x3;
      posCenter,velCenter:TVector3;
      totalMass:double=0;
      maxMass:double=0;
  begin
    posCenter:=ZERO_VECTOR;
    velCenter:=ZERO_VECTOR;
    for i:=0 to starCount-1 do with star[i] do begin
      posCenter+=p0*mass;
      velCenter+=v0*mass;
      totalMass+=   mass;
      maxMass:=max(maxMass,mass);
    end;
    posCenter*=(1/totalMass);
    velCenter*=(1/totalMass);
    maxMass:=10/maxMass;
    for i:=0 to starCount-1 do with star[i] do begin
      p0-=posCenter;
      v0-=velCenter;
    end;

    if starCount=1 then exit;
    for i:=0 to starCount-1 do with star[i] do star[i].mass*=maxMass;

    for i:=1 to starCount-1 do for j:=0 to i-1 do
    if star[i].mass>star[j].mass then begin
      tmp:=star[i];
      star[i]:=star[j];
      star[j]:=tmp;
    end;

    if starCount=2
    then transform:=orthonormalBasisOf(star[1].p0-star[0].p0,star[1].v0-star[0].v0)
    else transform:=orthonormalBasisOf(star[1].p0-star[0].p0,star[2].p0-star[0].p0);
    transform:=invert(transpose(transform));

    for i:=0 to starCount-1 do with star[i] do begin
      p0:=transform*p0;
      v0:=transform*v0;
    end;
  end;

PROCEDURE TStarSys.seminormalize;
  VAR centroid:TVector3=(0,0,0);
      totalImpulse:TVector3=(0,0,0);
      i:longint;
  begin
    for i:=0 to starCount-2 do with star[i] do begin
      centroid    +=p0*mass;
      totalImpulse+=v0*mass;
    end;
    with star[starCount-1] do begin
      p0:=centroid    *(-1/mass);
      v0:=totalImpulse*(-1/mass);
    end;
  end;

FUNCTION TStarSys.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i,j:longint;
  begin
    optimized:=stream.readBoolean;
    qualityMeasure:=stream.readDouble;
    massDelta     :=stream.readDouble;
    vDelta        :=stream.readDouble;
    rDelta        :=stream.readDouble;
    starCount:=stream.readByte;
    if (starCount<1) or (starCount>8) or not(stream.allOkay) then exit(false);
    for i:=0 to starCount-1 do with star[i] do begin
      for j:=0 to 2 do p0[j]   :=stream.readDouble;
      for j:=0 to 2 do v0[j]   :=stream.readDouble;
      for j:=0 to 2 do color[j]:=stream.readDouble;
      radius:=stream.readDouble;
      mass:=stream.readDouble;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE TStarSys.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:longint;
  begin
    stream.writeBoolean(optimized);
    stream.writeDouble(qualityMeasure);
    stream.writeDouble(massDelta     );
    stream.writeDouble(vDelta        );
    stream.writeDouble(rDelta        );
    stream.writeByte(starCount);
    for i:=0 to starCount-1 do with star[i] do begin
      for j:=0 to 2 do stream.writeDouble(p0[j]);
      for j:=0 to 2 do stream.writeDouble(v0[j]);
      for j:=0 to 2 do stream.writeDouble(color[j]);
      stream.writeDouble(radius);
      stream.writeDouble(mass);
    end;
  end;

PROCEDURE TStarSys.evaluateSystem;
  VAR accel:TVector3;
      i,j:longint;
      p0:TVector3x5=((0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0));
      p1:TVector3x5=((0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0));
      v :TVector3x5=((0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0));
      a0:TVector3x5=((0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0));
      a1:TVector3x5=((0,0,0),(0,0,0),(0,0,0),(0,0,0),(0,0,0));
      rMin,rMax,vMin,vMax:array[0..4] of Tfloat;
      dfc:Tfloat;
      maxRadius:double=0;
      maxMass:Tfloat=0;
      minMass:Tfloat=infinity;
      dt:Tfloat=0;
      dtNew:Tfloat;
      {$ifdef debugMode}
      stepsTaken:longint=0;
      stepsRejected:longint=0;
      {$endif}
  PROCEDURE finalize;
    VAR k:longint;
    begin
      vDelta:=0;
      for k:=0 to starCount-1 do begin
        vDelta+=ln(vMax[k]/vMin[k]);
        rDelta+=ln(rMax[k]/rMin[k]);
      end;
      vDelta/=2*starCount;
      rDelta/=2*starCount;
      {$ifdef debugMode}
      writeln('Simulation done after ',stepsTaken,'(+',stepsRejected,') steps T=',qualityMeasure);
      {$endif}
    end;

  VAR previousRejected:boolean=false;
  begin
    for i:=0 to starCount-1 do begin
      p0[i]:=star[i].p0;
      v [i]:=star[i].v0;
      maxMass:=max(maxMass,star[i].mass);
      minMass:=min(minMass,star[i].mass);
      rMin[i]:=euklideanNorm(p0[i]);
      rMax[i]:=rMin[i];
      vMin[i]:=euklideanNorm(v[i]);
      vMax[i]:=vMin[i];
      for j:=0 to i-1 do begin
        accel:=accelFactor(p0[i],p0[j]);
        a0[i]+=accel*star[j].mass;
        a0[j]-=accel*star[i].mass;
      end;
    end;
    qualityMeasure:=0;
    massDelta:=ln(maxMass/minMass);
    dt:=minTimeStep;

    while (qualityMeasure<MAX_QUALITY) do begin
      p1:=p0+v*dt+a0*(dt*dt*0.5);
      for i:=0 to starCount-1 do a1[i]:=ZERO_VECTOR;
      for i:=1 to starCount-1 do for j:=0 to i-1 do begin
        accel:=accelFactor(p1[i],p1[j]);
        a1[i]+=accel*star[j].mass;
        a1[j]-=accel*star[i].mass;
      end;
      dtNew:=dtOf(maxNormDiff(a0,a1));

      if isNan(dtNew) then begin
        {$ifdef debugMode}
        writeln('Whoopsie!');
        {$endif}
        finalize;
        exit;
      end;

      if dtNew*1.01>=dt then begin //add 1% tolerance
        //Step forward...
        v :=v +(a0+a1)*(dt*0.5);
        p0:=p1;
        a0:=a1;
        qualityMeasure+=dt;
        previousRejected:=false;
        {$ifdef debugMode} inc(stepsTaken); {$endif}

        //Collect statistics and check for early exit
        for i:=0 to starCount-1 do begin
          dfc:=sqrEuklideanNorm(p0[i]);
          rMax[i]:=max(rMax[i],dfc);
          rMin[i]:=min(rMin[i],dfc);
          maxRadius:=max(maxRadius,dfc);
          dfc:=sqrEuklideanNorm(v[i]);
          vMax[i]:=max(vMax[i],dfc);
          vMin[i]:=min(vMin[i],dfc);

          for j:=0 to i-1 do begin
            dfc:=sqrEuklideanNorm(p0[i]-p0[j]);
            if dfc<sqr(0.8*(star[i].radius+star[j].radius)) then begin
              finalize;
              exit;
            end;
          end;
        end;
        if maxRadius>MAX_ACCEPTED_RADIUS*MAX_ACCEPTED_RADIUS then begin
          finalize;
          exit;
        end;
      end else begin
        {$ifdef debugMode}inc(stepsRejected){$endif};
        if previousRejected then dtNew*=0.8;
        previousRejected:=true;
      end;
      dt:=dtNew;
    end;
    finalize;
  end;

FUNCTION TStarSys.quality: Tfloat;
  begin
    if isInfinite(qualityMeasure) then evaluateSystem;
    result:=qualityMeasure;
  end;

FUNCTION TStarSys.mult(CONST factor: double): TStarSys;
  FUNCTION multStar(CONST s:TTemplateStar):TTemplateStar;
    begin
      result:=s;
      result.p0*=factor;
      result.v0*=factor;
    end;

  VAR i:longint;
  begin
    result.initialize(starCount);
    for i:=0 to starCount-1 do result.star[i]:=multStar(star[i]);
  end;

FUNCTION TStarSys.plus(CONST other: TStarSys): TStarSys;
  FUNCTION plusStar(CONST a,b:TTemplateStar):TTemplateStar;
    begin
      result:=a;
      result.p0+=b.p0;
      result.v0+=b.v0;
    end;
  VAR i:longint;
  begin
    result.initialize(starCount);
    for i:=0 to starCount-1 do result.star[i]:=plusStar(star[i],other.star[i]);
  end;

FUNCTION TStarSys.simplexStep(CONST index:longint; CONST h:double):TStarSys;
  VAR K:longint;
      i:longint;
  begin
    result.initialize(starCount);
    k:=index;
    for i:=0 to starCount-1 do begin
      result.star[i]:=star[i];
      if (k>=0) and (k<3) then result.star[i].p0[k]+=h*(1-2*random(2)); dec(k,3);
      if (k>=0) and (k<3) then result.star[i].v0[k]+=h*(1-2*random(2)); dec(k,3);
    end;
  end;

VAR starTicks:qword=0;
    dustTicks:qword=0;
    startOfSampling:qword=0;
    sampleCount:longint=0;

FUNCTION cloneStar(CONST star:TStar):TStar;
  VAR i:longint;
  begin
    initialize(result);
    result.periodicTrajectory:=false;
    result.p     :=star.p     ;
    result.v     :=star.v     ;
    result.a     :=star.a     ;
    result.color :=star.color ;
    result.radius:=star.radius;
    result.mass  :=star.mass  ;
    setLength(result.trajectory,length(star.trajectory));
    for i:=0 to length(star.trajectory)-1 do result.trajectory[i]:=star.trajectory[i];
  end;

PROCEDURE TParticleEngine.MoveParticles(CONST dt: Tfloat; CONST stressed:boolean);
  PROCEDURE moveStars;
    CONST TRAJECTORY_RESOLUTION=0.05;
    VAR N:longint;
        p0,p1,v,a0,a1:TVector3x5;

    PROCEDURE updateAccelerations;
      VAR i,j:longint;
          af:TVector3;
      begin
        for i:=0 to N do a1[i]:=ZERO_VECTOR;
        for i:=1 to N do
        for j:=0 to i-1 do begin
          af:=accelFactor(p1[i],p1[j]);
          a1[i]+=af*star[j].mass;
          a1[j]-=af*star[i].mass;
        end;
      end;

    PROCEDURE copySystemToTemp;
      VAR i:longint;
      begin
        for i:=0 to 4 do p0[i]:=ZERO_VECTOR;
        for i:=0 to 4 do p1[i]:=ZERO_VECTOR;
        for i:=0 to 4 do v [i]:=ZERO_VECTOR;
        for i:=0 to 4 do a0[i]:=ZERO_VECTOR;
        for i:=0 to 4 do a1[i]:=ZERO_VECTOR;
        for i:=0 to N do begin
          p0[i]:=star[i].p;
          v [i]:=star[i].v;
          a0[i]:=star[i].a;
        end;
      end;

    PROCEDURE copyTempToSystem;
      VAR i:longint;
      begin
        for i:=0 to N do begin
          star[i].p:=p0[i];
          star[i].v:=v [i];
          star[i].a:=a0[i];
        end;
      end;

    PROCEDURE cleanupTrajectory(VAR s:TStar; CONST realStar:boolean);
      VAR j,i: integer;
          anyRemoved: boolean=false;
      begin
        with s do begin
          i:=0;
          while (i<length(trajectory)) and (trajectory[i].time<totalTime-trajectoryMaxTime) do inc(i);
          if i>0 then begin
            for j:=0 to length(trajectory)-i-1 do trajectory[j]:=trajectory[j+i];
            setLength(trajectory,length(trajectory)-i);
          end;
          if realStar and (length(trajectory)>20) then repeat
            anyRemoved:=false;
            j:=2;
            while (j<length(trajectory)) and (sqrEuklideanNorm(trajectory[j].p-trajectory[0].p)>TRAJECTORY_RESOLUTION*TRAJECTORY_RESOLUTION) do inc(j);
            if j<length(trajectory) then begin
              for j:=0 to length(trajectory)-2 do trajectory[j]:=trajectory[j+1];
              setLength(trajectory,length(trajectory)-1);
              anyRemoved:=true;
              periodicTrajectory:=true;
            end;
          until (length(trajectory)<10) or not(anyRemoved);
        end;
      end;

    PROCEDURE addTrajectoryPoint(VAR s:TStar; CONST point:TVector3);
      VAR i:longint;
      begin
        i:=length(s.trajectory);
        setLength(s.trajectory,i+1);
        s.trajectory[i].p:=point;
        s.trajectory[i].time:=totalTime;
      end;

    PROCEDURE mergeStars(CONST index1,index2:longint);
      VAR i,j:longint;
          pm, vm, am: TVector3;
      begin
        copyTempToSystem;
        setLength(trStar,length(trStar)+2);
        trStar[length(trStar)-1]:=cloneStar(star[index1]);
        trStar[length(trStar)-2]:=cloneStar(star[index2]);

        pm:=star[index1].p*star[index1].mass+star[index2].p*star[index2].mass;
        vm:=star[index1].v*star[index1].mass+star[index2].v*star[index2].mass;
        am:=star[index1].a*star[index1].mass+star[index2].a*star[index2].mass;
        star[index2].color:=star[index1].color*star[index1].mass+star[index2].color*star[index2].mass;
        star[index2].mass+=star[index1].mass;
        star[index2].color*=1/star[index2].mass;
        star[index2].radius:=power(power(star[index2].radius,3)+power(star[index1].radius,3),1/3);
        star[index2].p:=pm*(1/star[index2].mass);
        star[index2].v:=vm*(1/star[index2].mass);
        star[index2].a:=am*(1/star[index2].mass);
        star[index2].periodicTrajectory:=false;
        addTrajectoryPoint(                 star[index2],star[index2].p);
        addTrajectoryPoint(trStar[length(trStar)-1],star[index2].p);
        addTrajectoryPoint(trStar[length(trStar)-2],star[index2].p);

        setLength(star[index2].trajectory,0);
        j:=0;
        for i:=0 to length(star)-1 do if i<>index1 then begin if i<>j then star[j]:=star[i]; inc(j); end;
        setLength(star,j); N:=length(star)-1;
        copySystemToTemp;
      end;

    VAR i,j,k:longint;
        dtEff,dtNew: Tfloat;
        dtRest:Tfloat;
        anyRemoved:boolean=false;
        startTicks:qword;
    begin
      startTicks:=GetTickCount64;
      dtRest:=dt;
      //Cleanup trajectories max. 1x per macro time step:
      for k:=0 to length(star)-1 do if (GetTickCount64-startTicks<1000) then cleanupTrajectory(star[k],true);
      i:=0;
      for k:=0 to length(trStar)-1 do begin
        cleanupTrajectory(trStar[k],false);
        if length(trStar[k].trajectory)>0 then begin
          if i<>k then trStar[i]:=trStar[k];
          inc(i);
        end;
      end;
      setLength(trStar,i);

      anyRemoved:=false;
      N:=length(star)-1;
      copySystemToTemp;

      dtEff:=dt/max(1,ceil(dt/lastStarTimeStep));
      while (dtRest>0) and (GetTickCount64-startTicks<1000) do begin
        p1:=p0+v*dtEff+a0*(dtEff*dtEff*0.5);
        updateAccelerations;
        dtNew:=dtOf(maxNormDiff(a0,a1));
        if dtNew>=dtEff then begin
          dtRest-=dtEff;
          v:=v+(a0+a1)*(dtEff*0.5);
          p0:=p1;
          a0:=a1;

          //Trajectory
          for i:=0 to N do with star[i] do begin
            j:=length(trajectory);
            if (j=0) or (sqrEuklideanNorm(p1[i]-trajectory[j-1].p)>TRAJECTORY_RESOLUTION*TRAJECTORY_RESOLUTION) then addTrajectoryPoint(star[i],p1[i]);
          end;

          //Check for star collisions:
          anyRemoved:=false;
          for i:=1 to length(star)-1 do
          for j:=0 to i-1 do
          if not(anyRemoved) and
             (sqr(p0[i,0]-p0[j,0])+
              sqr(p0[i,1]-p0[j,1])+
              sqr(p0[i,2]-p0[j,2])<sqr(0.8*(star[i].radius+star[j].radius)))
          then begin
            anyRemoved:=true;
            mergeStars(i,j);
          end;

        end;
        dtEff :=dtRest/max(1,ceil(dtRest/dtNew));
      end;
      lastStarTimeStep:=dtNew;
      copyTempToSystem;
    end;

  PROCEDURE moveDust;
    VAR i,j:longint;
        gravitationallyBound:boolean;
        removalFactor:Tfloat;
        sqrDtHalf,
        dtHalf:Tfloat;
        aNew:TVector3;
        sqrStarRadius,sqrStarMass:array[0..4] of Tfloat;

    PROCEDURE moveParticlePrecisely(VAR p0,v0,a0:TVector3); {$ifndef debugMode} inline; {$endif}
      VAR dtRest:double;
          dt_,dtNew:double;
          p1:TVector3;
          a1:TVector3;
          j:longint;

      begin
        dtRest:=dt;
        dt_:=dt;
        while dtRest>0 do begin
          p1:=p0+v0*dt_+a0*(dt_*dt_*0.5);
          a1:=ZERO_VECTOR;
          for j:=0 to length(star)-1 do a1+=accelFactor(p1,star[j].p-star[j].v*(dtRest-dt_)+star[j].a*(0.5*sqr(dtRest-dt_)))*star[j].mass;
          dtNew:=10*dtOf(sqrEuklideanNorm(a0-a1));
          if dtNew*1.01>=dt_ then begin
            v0+=(a0+a1)*(dt_*0.5);
            p0:=p1;
            a0:=a1;
            dtRest-=dt_;
          end;
          dt_:=dtRest/max(1,ceil(dtRest/dtNew));
        end;
      end;

    VAR toleranceFactor:Tfloat;
        totalSqrMass:double=0;
    begin
      for i:=0 to length(star)-1 do begin
        sqrStarRadius[i]:=sqr(star[i].radius);
        sqrStarMass  [i]:=sqr(star[i].mass);
        sqrSystemRadius:=max(sqrSystemRadius,sqrEuklideanNorm(star[i].p));
        totalSqrMass+=star[i].mass;
      end;
      totalSqrMass*=totalSqrMass;

      dtHalf:=dt*0.5;
      sqrDtHalf:=dt*dtHalf;
      if stressed
      then removalFactor:=5*dt
      else removalFactor:=0.1*dt;
      if precisionFocused
      then begin for i:=0 to length(dust)-1 do with dust[i] do if not(flaggedForRemoval) then moveParticlePrecisely(p,v,a); end
      else for i:=0 to length(dust)-1 do with dust[i] do if not(flaggedForRemoval) then begin
        p+=v*dt+a*sqrDtHalf;
        aNew:=ZERO_VECTOR;
        for j:=0 to length(star)-1 do aNew+=accelFactor(p,star[j].p)*star[j].mass;
        v+=(a+aNew)*dtHalf;
        a:=aNew;
      end;

      for i:=0 to length(dust)-1 do with dust[i] do if not(flaggedForRemoval) then begin
        //Remove because of collision:
        for j:=0 to length(star)-1 do begin
          flaggedForRemoval:=flaggedForRemoval or
            (sqr(p[0]-star[j].p[0])+
             sqr(p[1]-star[j].p[1])+
             sqr(p[2]-star[j].p[2])<sqrStarRadius[j]);
        end;
        toleranceFactor:=1+14*sqrSystemRadius/sqrEuklideanNorm(p);

        if not(flaggedForRemoval) and (removalFactor>random) then begin
          gravitationallyBound:=                                                      (sqr(sqrEuklideanNorm(v          ))* sqrEuklideanNorm(p          )<toleranceFactor*totalSqrMass);
          for j:=0 to length(star)-1 do gravitationallyBound:=gravitationallyBound or (sqr(sqrEuklideanNorm(v-star[j].v))* sqrEuklideanNorm(p-star[j].p)<toleranceFactor*sqrStarMass[j]);
          if not(gravitationallyBound) then flaggedForRemoval:=true;
        end;
        if flaggedForRemoval
        then inc(removalCounter)
      end;
      if removalCounter*10>length(dust) then begin //Removing is expensive. Do this only when more than 10% can be removed...
        removalCounter:=0;
        j:=0;
        for i:=0 to length(dust)-1 do if not(dust[i].flaggedForRemoval) then begin
          if i<>j then dust[j]:=dust[i];
          inc(j);
        end;
        setLength(dust,j);
      end;

    end;
  VAR t0,t1,t2:qword;
  begin
    t0:=GetTickCount64;
    if (length(star)>1) or (length(trStar)>0) then moveStars;
    t1:=GetTickCount64;
    moveDust;
    totalTime+=dt;
    t2:=GetTickCount64;

    inc(sampleCount);
    starTicks+=(t1-t0);
    dustTicks+=(t2-t1);
    if ((starTicks+dustTicks)>10000) or (GetTickCount64-startOfSampling>30000)
    then begin
      writeln('Movement cost: stars ',starTicks,' dust ',dustTicks,'; fraction of total: ',starTicks/(GetTickCount64-startOfSampling)*100:0:3,'% / ',dustTicks/(GetTickCount64-startOfSampling)*100:0:3,'%; per step: ',starTicks/sampleCount:0:3,' / ',dustTicks/sampleCount:0:3);
      starTicks:=0;
      dustTicks:=0;
      sampleCount:=0;
      startOfSampling:=GetTickCount64;
    end;
  end;

CONSTRUCTOR TParticleEngine.create;
  begin
    precisionFocused:=false;
    randomize;
    dtFactor:=0;
    cachedSystems.create;
    initStars(cachedSystems.getPredefinedSystem(0));
    trajectoryMaxTime:=16;
  end;

DESTRUCTOR TParticleEngine.destroy;
  begin
    inherited destroy;
    cachedSystems.destroy;
  end;

PROCEDURE TParticleEngine.update(CONST tickDelta: qword; stressed:boolean);
  VAR dt:Tfloat;
      startTicks:qword;
  begin
    startTicks:=GetTickCount64;
    if dtFactor=0 then exit;
    dt:=tickDelta*dtFactor;
    if (dt>maxDustTimeStep) and (dustCount>0) then begin
      stressed:=true;
      dt:=maxDustTimeStep;
    end;
    MoveParticles(dt,stressed);
  end;

PROCEDURE TParticleEngine.DrawParticles(CONST ParticleList: GLuint; CONST pointSize:Tfloat);
  PROCEDURE drawTrajectory(CONST s:TStar);
    VAR j: integer;
        col: TVector3;
        T:double;
    begin
      if s.periodicTrajectory
      then T:=1/(s.trajectory[length(s.trajectory)-1].time-
                 s.trajectory[0                     ].time)
      else T:=1/trajectoryMaxTime;

      glBegin(GL_LINE_STRIP);
      for j:=0 to length(s.trajectory)-1 do begin
        col:=s.color*(1-(totalTime-s.trajectory[j].time)*T);
        glColor3dv(@col);
        glVertex3dv(@s.trajectory[j]);
      end;
      glEnd();
    end;

  VAR i: integer;
      x: TVector3;

  begin
    for i:=0 to length(star)-1 do begin
      glColor3d(star[i].color[0],star[i].color[1],star[i].color[2]);
      glPushMatrix;
      glTranslated(star[i].p[0], star[i].p[1], star[i].p[2]);
      glScaled(star[i].radius,star[i].radius,star[i].radius);
      glCallList(ParticleList);
      glPopMatrix;

      drawTrajectory(star[i]);
    end;
    glPointSize(pointSize);

    for i:=0 to length(trStar)-1 do drawTrajectory(trStar[i]);

    glBegin(GL_POINTS);
    for i:=0 to length(dust)-1 do if not(dust[i].flaggedForRemoval) then begin
      x[0]:=0.2+0.5*i/(length(dust)-1);
      glColor3d(x[0],x[0],x[0]);
      glVertex3dv(@dust[i].p);
    end;
    glEnd();
  end;

PROCEDURE TParticleEngine.initDust(CONST particleCount: longint; starMask: byte; CONST mode: TDustInitMode);
  VAR starIndex:array of longint;
  VAR systemRadius:Tfloat;
      pointNemo:TVector3;
      vNemo:TVector3;
      base:array of TMatrix3x3;
      consensusBase:TMatrix3x3;
      totalMass:double=0;

  FUNCTION insideAnyStar(CONST p:TVector3; CONST toleranceFactor:double=1):boolean;
    VAR k:longint;
    begin
      result:=false;
      for k:=0 to length(star)-1 do if sqrEuklideanNorm(p-star[k].p)<sqr(star[k].radius*toleranceFactor) then exit(true);
    end;

  FUNCTION gravitationalSpeed(CONST p:TVector3):TVector3;
    VAR mTot: Tfloat=0;
        m:Tfloat;
        k: integer;
    begin
      if length(star)>1 then begin
        mTot:=0;
        result:=ZERO_VECTOR;
        for k:=0 to length(star)-1 do begin
          m:=euklideanNorm(accelFactor(p,star[k].p)*star[k].mass);
          result+=star[k].v*m;
          mTot+=m;
        end;
        result*=1/mTot;
      end else result:=vNemo;
    end;

  FUNCTION diskParticle(CONST starIndex:longint; CONST speedFactor:Tfloat):TParticle;
    VAR distanceFromStar,speed:Tfloat;
        e0,e1:Tfloat;
    begin
      repeat
        distanceFromStar:=star[starIndex].radius*1.5+sqrt(random)*3;
        speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar)*speedFactor;
        e1:=random*2*pi;
        e0:=cos(e1);
        e1:=sin(e1);
        result.p:=star[starIndex].p+base[starIndex,0]*e0*distanceFromStar+base[starIndex,1]*e1*distanceFromStar;
      until not insideAnyStar(result.p,2);
      result.v:=star[starIndex].v+base[starIndex,0]*e1*speed           -base[starIndex,1]*e0*speed;
    end;

  FUNCTION consensusDiskParticle(CONST speedFactor:Tfloat):TParticle;
    VAR distanceFromOrigin:Tfloat;
        e0,e1:Tfloat;
        temp:TVector3;
        k:longint;
    begin
      repeat
        distanceFromOrigin:=systemRadius*sqrt(random);
        e1:=random*2*pi;
        e0:=cos(e1);
        e1:=sin(e1);
        result.p:=consensusBase[0]*e0*distanceFromOrigin+consensusBase[1]*e1*distanceFromOrigin;
      until not insideAnyStar(result.p,2);
      result.a:=ZERO_VECTOR;
      for k:=0 to length(star)-1 do begin
        temp:=result.p-star[k].p;
        temp*=(star[k].mass/sqrEuklideanNorm(temp));
        result.a+=temp;
      end;
      result.a*=1/sqrt(euklideanNorm(result.a));
      result.v:=cross(result.a,consensusBase[2])*speedFactor;
    end;

  FUNCTION shell(CONST starIndex:longint):TParticle;
    VAR dx,dv:TVector3;
        distanceFromStar,speed:Tfloat;
    begin
      repeat
        distanceFromStar:=star[starIndex].radius*1.5;
        speed:=sqrt(star[starIndex].mass);
        if speed>distanceFromStar then distanceFromStar:=speed;

        speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
        dx:=randomOnSphere*distanceFromStar;
        //speed is perpendicular to dx
        dv:=cross(dx,randomOnSphere);
        dv*=speed/euklideanNorm(dv);
        result.p:=star[starIndex].p+dx;
      until not insideAnyStar(result.p);
      result.v:=star[starIndex].v+dv;
    end;

  FUNCTION randomParticle(CONST starIndex:longint):TParticle;
    VAR dx,dv:TVector3;
        distanceFromStar,speed:Tfloat;
    begin
      repeat
        distanceFromStar:=star[starIndex].radius*1.5+sqrt(random)*3;
        speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
        dx:=randomOnSphere*distanceFromStar;
        dv:=randomInSphere*(2*speed);
        result.p:=star[starIndex].p+dx;
        result.v:=star[starIndex].v+dv;
      until not insideAnyStar(result.p);
    end;

  FUNCTION ringParticle(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:Tfloat;
        e0,e1:Tfloat;
    begin
      repeat
        distanceFromStar:=star[starIndex].radius*1.5+1.5+0.05*random;
        speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
        e1:=random*2*pi;
        e0:=cos(e1);
        e1:=sin(e1);
        result.p:=star[starIndex].p+base[starIndex,0]*e0*distanceFromStar+base[starIndex,1]*e1*distanceFromStar;
        result.v:=star[starIndex].v+base[starIndex,0]*e1*speed           -base[starIndex,1]*e0*speed;
      until not insideAnyStar(result.p);
    end;

  FUNCTION ring3Particle(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:Tfloat;
        e0,e1:Tfloat;
        k:longint;
    begin
      repeat
        k:=random(3);
        distanceFromStar:=star[starIndex].radius*1.5+1.4+0.1*k+0.05*random;
        speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
        e1:=random*2*pi;
        e0:=cos(e1);
        e1:=sin(e1);
        result.p:=star[starIndex].p+base[starIndex,k]*e0*distanceFromStar+base[starIndex,(k+1) mod 3]*e1*distanceFromStar;
        result.v:=star[starIndex].v+base[starIndex,k]*e1*speed           -base[starIndex,(k+1) mod 3]*e0*speed;
      until not insideAnyStar(result.p);
    end;

  FUNCTION planetLike(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:Tfloat;
        e0,e1,e2:Tfloat;
        alpha,beta:Tfloat;
        dv,dp:TVector3;
    begin
      repeat
        distanceFromStar:=star[starIndex].radius*1.5+1+1E-3*random;
        speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
        alpha:=2*pi/12*(random( 6)+0.01*random+0.495);
        beta :=2*pi/12*(random(12)+0.01*random+0.495);
        e0:=cos(alpha)*cos(beta);
        e1:=sin(alpha)*cos(beta);
        e2:=           sin(beta);
        dp:=base[starIndex,0]*e0*distanceFromStar+base[starIndex,1]*e1*distanceFromStar+base[starIndex,2]*e2*distanceFromStar;
        result.p:=star[starIndex].p+dp;
        case byte(random(6)) of
          0: dv:=cross(dp,base[starIndex,0]);
          1: dv:=cross(dp,base[starIndex,0])*-1;
          2: dv:=cross(dp,base[starIndex,1])   ;
          3: dv:=cross(dp,base[starIndex,1])*-1;
          4: dv:=cross(dp,base[starIndex,2])   ;
          5: dv:=cross(dp,base[starIndex,2])*-1;
        end;
        dv*=speed/euklideanNorm(dv);
        result.v:=star[starIndex].v+dv;
      until not insideAnyStar(result.p);
    end;

  FUNCTION randomBackgroundParticle:TParticle;
    begin
      repeat
        result.p:=randomInSphere*systemRadius;
      until not insideAnyStar(result.p);
      result.v:=gravitationalSpeed(result.p);
    end;

  FUNCTION lonelyCloudParticle:TParticle;
    begin
      repeat
        result.p:=pointNemo+randomInSphere;
      until not insideAnyStar(result.p);
      result.v:=gravitationalSpeed(result.p);
    end;

  PROCEDURE initStarIndexes;
    VAR relevantStars:array of longint;
        i:longint;
    begin
      if length(star) =1 then starMask:=1;
      if length(star)<=2 then starMask:=starMask and 3;
      if length(star)<=3 then starMask:=starMask and 7;
      if length(star)<=4 then starMask:=starMask and 15;
      if length(star)<=5 then starMask:=starMask and 31;
      if starMask=0 then starMask:=255;

      setLength(relevantStars,0);
      for i:=0 to length(star)-1 do if odd(starMask shr i) then begin
        setLength(relevantStars,length(relevantStars)+1);
        relevantStars[length(relevantStars)-1]:=i;
      end;

      for i:=0 to length(starIndex)-1 do starIndex[i]:=relevantStars[i*length(relevantStars) div length(starIndex)];
    end;

  PROCEDURE findPointNemo;
    VAR i,k:longint;
        mTot:Tfloat=0;
        p,a: TVector3;
        step:Tfloat=1;
        curr:Tfloat;
        best:Tfloat=infinity;

        minA,currA:Tfloat;
    begin
      vNemo:=ZERO_VECTOR;
      if length(star)=1 then begin
        pointNemo:=randomOnSphere*systemRadius;
        vNemo:=cross(pointNemo,randomOnSphere);
        vNemo*=sqrt(1.5*star[0].mass/(systemRadius+1))/euklideanNorm(vNemo);
        exit;
      end;

      pointNemo:=ZERO_VECTOR;
      for k:=0 to length(star)-1 do begin
        pointNemo+=star[k].p*sqrt(star[k].mass);
        mTot     +=          sqrt(star[k].mass);
      end;
      pointNemo*=1/mTot;

      a:=ZERO_VECTOR;
      for k:=0 to length(star)-1 do a+=accelFactor(pointNemo,star[k].p)*star[k].mass;
      best:=sqrEuklideanNorm(a);

      for i:=0 to 999 do begin
        minA:=infinity;
        for k:=0 to length(star)-1 do begin
          a:=accelFactor(pointNemo,star[k].p)*star[k].mass;
          currA:=sqrEuklideanNorm(a);
          if currA<minA then begin
            minA:=currA; p:=star[k].p;
          end;
        end;
        p:=p-pointNemo;
        p*=step/euklideanNorm(p); step*=0.99;
        p+=pointNemo;

        a:=ZERO_VECTOR;
        for k:=0 to length(star)-1 do a+=accelFactor(p,star[k].p)*star[k].mass;
        curr:=sqrEuklideanNorm(a);
        if curr<best then begin
          best:=curr;
          pointNemo:=p;
        end;
      end;

      for i:=0 to 99999 do begin
        p:=star[0].p+(star[1].p-star[0].p)*random;
        for k:=2 to length(star)-1 do p:=p+(star[k].p-p)*random;
        a:=ZERO_VECTOR;
        for k:=0 to length(star)-1 do a+=accelFactor(p,star[k].p)*star[k].mass;
        curr:=sqrEuklideanNorm(a);
        if curr<best then begin
          best:=curr;
          pointNemo:=p;
        end;
      end;
    end;

  FUNCTION basisOf(CONST v,a:TVector3; OUT fallback:boolean):TMatrix3x3;
    begin
      result:=orthonormalBasisOf(v,a);
      if isInfinite(result[1,0]) or isNan(result[1,0]) or
         isInfinite(result[1,1]) or isNan(result[1,1]) or
         isInfinite(result[1,2]) or isNan(result[1,2]) then begin
        result:=randomOrthonormalBasis;
        fallback:=true;
      end else fallback:=false;
    end;

  VAR k:longint;
      noneSelected:boolean;
      fallback:boolean;
  begin
    noneSelected:=starMask and ((1 shl length(star))-1) =0;
    vNemo:=ZERO_VECTOR;
    removalCounter:=0;
    setLength(base,length(star));
    consensusBase[2]:=ZERO_VECTOR;
    for k:=0 to length(base)-1 do begin
      base[k]:=basisOf(star[k].v,star[k].a,fallback);
      if not(fallback) then begin
        consensusBase[2]+=base[k,2]*star[k].mass;
        totalMass       +=          star[k].mass;
      end;
    end;
    if totalMass>0 then begin
      consensusBase[2]*=1/totalMass;
      consensusBase[1]:=cross(consensusBase[2],randomOnSphere);   consensusBase[1]*=1/euklideanNorm(consensusBase[1]);
      consensusBase[0]:=cross(consensusBase[1],consensusBase[2]); consensusBase[0]*=1/euklideanNorm(consensusBase[0]);
      consensusBase[2]:=cross(consensusBase[0],consensusBase[1]);
    end else consensusBase:=randomOrthonormalBasis;

    systemRadius:=0;
    for k:=0 to length(star)-1 do systemRadius:=max(systemRadius,euklideanNorm(star[k].p)+1.5*star[k].radius+3);

    findPointNemo;

    setLength(starIndex,particleCount);
    initStarIndexes;

    setLength(dust,particleCount);
    case mode of
      dim_stableDisk:         if noneSelected
                              then for k:=0 to length(dust)-1 do dust[k]:=consensusDiskParticle(    -1)
                              else for k:=0 to length(dust)-1 do dust[k]:=diskParticle(starIndex[k],-1);
      dim_stableDiskReversed: if noneSelected
                              then for k:=0 to length(dust)-1 do dust[k]:=consensusDiskParticle(    1)
                              else for k:=0 to length(dust)-1 do dust[k]:=diskParticle(starIndex[k],1);
      dim_randomCloud: for k:=0 to length(dust)-1 do dust[k]:=randomParticle(starIndex[k]);
      dim_singleRing : for k:=0 to length(dust)-1 do dust[k]:=ringParticle(starIndex[k]);
      dim_threeRings : for k:=0 to length(dust)-1 do dust[k]:=ring3Particle(starIndex[k]);
      dim_shell      : for k:=0 to length(dust)-1 do dust[k]:=shell(starIndex[k]);
      dim_planetLike : for k:=0 to length(dust)-1 do dust[k]:=planetLike(starIndex[k]);
      dim_lonelyCloud: for k:=0 to length(dust)-1 do dust[k]:=lonelyCloudParticle;
      else             for k:=0 to length(dust)-1 do dust[k]:=randomBackgroundParticle;
    end;
    for k:=0 to length(dust)-1 do with dust[k] do begin
      flaggedForRemoval:=false;
      a:=ZERO_VECTOR;
    end;
  end;

PROCEDURE TParticleEngine.initStars(starCount: longint;  CONST target:TsysTarget);
  begin
    initStars(cachedSystems.getSystem(starCount,target));
  end;

PROCEDURE TParticleEngine.initPresetStars(CONST presetIndex:byte);
  begin
    initStars(cachedSystems.getPredefinedSystem(presetIndex));
  end;

OPERATOR :=(CONST x:TTemplateStar):TStar;
  begin
    initialize(result);
    result.radius:=x.radius;
    result.mass:=x.mass;
    result.color:=x.color;
    result.p:=x.p0;
    result.v:=x.v0;
    result.a:=ZERO_VECTOR;
    result.periodicTrajectory:=false;
    setLength(result.trajectory,0);
  end;

PROCEDURE TParticleEngine.initStars(CONST sys: TStarSys);
  VAR k,i:longint;
      acc:TVector3;
  begin
    setLength(star,sys.starCount);
    setLength(trStar,0);
    sqrSystemRadius:=0;
    for k:=0 to length(star)-1 do begin
      star[k]:=sys.star[k];
      star[k].a:=ZERO_VECTOR;
      for i:=0 to k-1 do begin
        acc:=accelFactor(star[k].p,star[i].p);
        star[k].a+=acc*star[i].mass;
        star[i].a-=acc*star[k].mass;
      end;
    end;
    totalTime:=0;
    lastStarTimeStep:=1E-6;
  end;

PROCEDURE TParticleEngine.resetStars;
  begin
    initStars(cachedSystems.lastResponse);
  end;

FUNCTION TParticleEngine.dustCount: longint;
  begin
    result:=length(dust)-removalCounter;
  end;

FUNCTION TParticleEngine.starCount:longint;
  begin
    result:=length(star);
  end;

PROCEDURE TParticleEngine.multiplySize(CONST factor:Tfloat);
  VAR i,j:longint;
      vFactor,aFactor:Tfloat;
  begin
    vFactor:=1/sqrt(factor);
    aFactor:=1/sqr(factor);
    for i:=0 to length(star)-1 do with star[i] do begin
      p*=factor;
      v*=vFactor;
      a*=aFactor;
      for j:=0 to length(trajectory)-1 do trajectory[j].p*=factor;
    end;
    for i:=0 to length(dust)-1 do with dust[i] do begin
      p*=factor;
      v*=vFactor;
      a*=aFactor;
    end;
  end;

end.

