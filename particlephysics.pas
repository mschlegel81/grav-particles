UNIT particlePhysics;

{$mode objfpc}{$H+}

INTERFACE
USES vectors,GL,serializationUtil;

TYPE
  TStar = record
    trajectory:array of TVector3;
    p,v,a,color:TVector3;
    radius,mass : Tfloat;
  end;

  TParticle = record
    p,v,a:TVector3;
    flaggedForRemoval:boolean;
  end;

  TStarSys=object //Declaring there as object(T_serializable) leads to a enormous overhead (~ +200%) due to 800 virtual method tables in RAM
    qualityMeasure,
    massDelta,        //ranged 1 - 100
    vDelta,           //ranged ln(1) - ln(infinity)
    rDelta:Tfloat;    //ranged ln(1) - ln(infinity)
    stars:array of TStar;

    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  end;

  TsysTarget=(none,smallMassVariance,bigMassVariance,uniformRadii,varyingRadii,uniformVelocitied,varyingVelocities);
CONST
  CsysTargetName:array[TsysTarget] of string=('none','small mass variance','big mass variance','uniform distances from center','varying distances from center','uniform velocities','varying velocities');
TYPE

  { TCachedSystems }
  PCachedSystems=^TCachedSystems;
  TCachedSystems=object(T_serializable)
    sys:array[2..5,0..199] of TStarSys;
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
    star :  array of TStar;
    dust :  array of TParticle;
    removalCounter:longint;
    base:array of TMatrix3x3;
    PROCEDURE MoveParticles(CONST dt:Tfloat; CONST stressed:boolean);
  public
    cachedSystems:TCachedSystems;
    totalTime:Tfloat;
    dtFactor:Tfloat;

    CONSTRUCTOR create;
    DESTRUCTOR destroy; override;
    PROCEDURE update(CONST tickDelta:qword; stressed:boolean);
    PROCEDURE DrawParticles(CONST ParticleList: GLuint; CONST pointSize:Tfloat);

    PROCEDURE initDust(CONST particleCount:longint; starMask:byte; CONST mode:TDustInitMode);
    PROCEDURE initStars(starCount:longint;  CONST target:TsysTarget);
    PROCEDURE initStars(CONST sys:TStarSys);
    PROCEDURE resetStars;
    FUNCTION dustCount:longint;
    PROCEDURE multiplySize(CONST factor:Tfloat);

  end;

IMPLEMENTATION
USES math,sysutils,LCLProc;
CONST maxTimeStep=1E-3;
      maxDustTimeStep=1E-1;

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
    if not loadFromFile(ChangeFileExt(paramStr(0),'.cached_sys')) then begin
      for i:=2 to 5 do with sysRange[i] do begin
        massDelta[0]:=infinity; massDelta[1]:=0;
        vDelta   [0]:=infinity; vDelta   [1]:=0;
        rDelta   [0]:=infinity; rDelta   [1]:=0;
      end;
      for i:=2 to 5 do for j:=0 to 199 do sys[i,j].qualityMeasure:=-infinity;
      writeln('Could not load cached systems... I will calculate some in a separate thread');
      startBackgroundCalculation;
    end else writeStatistics;
  end;

DESTRUCTOR TCachedSystems.destroy;
  begin
    destroying:=true;
    while backgroundRunning>0 do sleep(1);
    enterCriticalSection(sysCs);
    writeStatistics;
    saveToFile(ChangeFileExt(paramStr(0),'.cached_sys'));
    leaveCriticalSection(sysCs);
    doneCriticalSection(sysCs);
  end;

PROCEDURE TCachedSystems.writeStatistics;
  begin
    writeln('=============================================================================================================');
    writeln(' Cached system statistics:');
    writeln('                Best     Median      Worst      Mass-----------\      Radius---------\      Velocity-------\');
    writeln(' 2 stars: ',sys[2,0].qualityMeasure:10:3,' ',sys[2,100].qualityMeasure:10:3,' ',sys[2,199].qualityMeasure:10:3,' ',sysRange[2].massDelta[0]:10:3,' ',sysRange[2].massDelta[1]:10:3,' ',sysRange[2].rDelta[0]:10:3,' ',sysRange[2].rDelta[1]:10:3,' ',sysRange[2].vDelta[0]:10:3,' ',sysRange[2].vDelta[1]:10:3);
    writeln(' 3 stars: ',sys[3,0].qualityMeasure:10:3,' ',sys[3,100].qualityMeasure:10:3,' ',sys[3,199].qualityMeasure:10:3,' ',sysRange[3].massDelta[0]:10:3,' ',sysRange[3].massDelta[1]:10:3,' ',sysRange[3].rDelta[0]:10:3,' ',sysRange[3].rDelta[1]:10:3,' ',sysRange[3].vDelta[0]:10:3,' ',sysRange[3].vDelta[1]:10:3);
    writeln(' 4 stars: ',sys[4,0].qualityMeasure:10:3,' ',sys[4,100].qualityMeasure:10:3,' ',sys[4,199].qualityMeasure:10:3,' ',sysRange[4].massDelta[0]:10:3,' ',sysRange[4].massDelta[1]:10:3,' ',sysRange[4].rDelta[0]:10:3,' ',sysRange[4].rDelta[1]:10:3,' ',sysRange[4].vDelta[0]:10:3,' ',sysRange[4].vDelta[1]:10:3);
    writeln(' 5 stars: ',sys[5,0].qualityMeasure:10:3,' ',sys[5,100].qualityMeasure:10:3,' ',sys[5,199].qualityMeasure:10:3,' ',sysRange[5].massDelta[0]:10:3,' ',sysRange[5].massDelta[1]:10:3,' ',sysRange[5].rDelta[0]:10:3,' ',sysRange[5].rDelta[1]:10:3,' ',sysRange[5].vDelta[0]:10:3,' ',sysRange[5].vDelta[1]:10:3);
    writeln('=============================================================================================================');
    statCounter:=0;
  end;

PROCEDURE TCachedSystems.writeDetails;
  PROCEDURE writeSys(CONST sys:TStarSys);
    VAR k:longint;
    begin
      write(sys.qualityMeasure,';');
      for k:=0 to length(sys.stars)-1 do write(sys.stars[k].mass,';',
                                               sys.stars[k].p[0],';',
                                               sys.stars[k].p[1],';',
                                               sys.stars[k].p[2],';',
                                               sys.stars[k].v[0],';',
                                               sys.stars[k].v[1],';',
                                               sys.stars[k].v[2],';');
      writeln;
    end;

  VAR i,j:longint;
  begin
    for i:=2 to 5 do for j:=0 to 199 do writeSys(sys[i,j]);

  end;

FUNCTION TCachedSystems.getSerialVersion: dword;
  begin
    result:=2;
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

    commonFactor:=(random+random+random)/3; //This links density and brightness
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

PROCEDURE TCachedSystems.prepareSystem(CONST starCount: longint);
  VAR i:longint;
      totalMass:Tfloat;
      posCenter,velCenter:TVector3;

  PROCEDURE evaluateSystem(VAR sys:TStarSys);
    CONST dt   :Tfloat=maxTimeStep;
          dtdt2:Tfloat=maxTimeStep*maxTimeStep*0.5;
          dtHalf:Tfloat=maxTimeStep*0.5;
    VAR accel,step:TVector3;
        i,j:longint;
        p,v,a,aNew:array[0..4] of TVector3;
        rMin,rMax,vMin,vMax:array[0..4] of Tfloat;

        totalDistance:Tfloat=0;
        invTotalMass:Tfloat=0;
        maxRadius:Tfloat=0;
        dfc:Tfloat;
        maxMass:Tfloat=0;
        minMass:Tfloat=infinity;

    PROCEDURE finalize;
      VAR k:longint;
      begin
        sys.vDelta:=0;
        for k:=0 to starCount-1 do begin
          sys.vDelta+=ln(vMax[k]/vMin[k]);
          sys.rDelta+=ln(rMax[k]/rMin[k]);
        end;
        sys.vDelta/=2*starCount;
        sys.rDelta/=2*starCount;
      end;

    begin
      with sys do begin
        for i:=0 to starCount-1 do begin
          p[i]:=stars[i].p;
          v[i]:=stars[i].v;
          a[i]:=ZERO_VECTOR;
          maxMass:=max(maxMass,stars[i].mass);
          minMass:=min(minMass,stars[i].mass);
          rMin[i]:=euklideanNorm(p[i]);
          rMax[i]:=rMin[i];
          vMin[i]:=euklideanNorm(v[i]);
          vMax[i]:=vMin[i];
          invTotalMass+=stars[i].mass;
        end;
        invTotalMass:=1/invTotalMass;
        qualityMeasure:=0;
        massDelta:=ln(maxMass/minMass);

        while (qualityMeasure<10000) do begin
          for i:=0 to starCount-1 do begin
            step:=v[i]*dt+ a[i]*dtdt2;
            totalDistance+=euklideanNorm(step)*stars[i].mass; //heavy stars' movement weighs more...
            p[i]         +=              step;

            aNew[i]:=ZERO_VECTOR;
            dfc:=sqrEuklideanNorm(p[i]);
            rMax[i]:=max(rMax[i],dfc);
            rMin[i]:=min(rMin[i],dfc);

            if dfc>maxRadius then maxRadius:=dfc;
            if dfc>400 then begin
              finalize;
              exit; //drifting apart
            end;
          end;
          for i:=1 to starCount-1 do for j:=0 to i-1 do begin
            accel:=accelFactor(p[i],p[j]);
            aNew[i]+=accel*stars[j].mass;
            aNew[j]-=accel*stars[i].mass;
            //Collision
            if euklideanNorm(p[i]-p[j])<(stars[i].radius+stars[j].radius) then begin
              finalize;
              exit;
            end;
          end;
          for i:=0 to starCount-1 do begin
            v[i]+=(a[i]+aNew[i])*dtHalf;
            dfc:=sqrEuklideanNorm(v[i]);
            vMax[i]:=max(vMax[i],dfc);
            vMin[i]:=min(vMin[i],dfc);
          end;
          a:=aNew;
          qualityMeasure:=totalDistance*invTotalMass;
        end;
        finalize;
      end;
    end;

  VAR newSystem:TStarSys;
      closeToOther:boolean;
      boundToOther:boolean;
      j:longint;
      maxMass:Tfloat=0;
      maxRadius:Tfloat=0;
      minDistance:Tfloat=infinity;
      beta:Tfloat;
  begin
    totalMass:=0;
    posCenter:=ZERO_VECTOR;
    velCenter:=ZERO_VECTOR;
    setLength(newSystem.stars,starCount);
    for i:=0 to length(newSystem.stars)-1 do with newSystem.stars[i] do begin
      randomStarStats(radius,mass,color);
      if mass>maxMass then maxMass:=mass;
      repeat
        p:=randomInSphere*(10*random);
        closeToOther:=false;
        for j:=0 to i-1 do closeToOther:=closeToOther or (euklideanNorm(p-newSystem.stars[j].p)<2*(radius+newSystem.stars[j].radius));
      until not(closeToOther);
      repeat
        v:=randomInSphere*2.5;
        boundToOther:=i=0;
        for j:=0 to i-1 do boundToOther:=boundToOther or (sqrEuklideanNorm(v-newSystem.stars[j].v)*euklideanNorm(p-newSystem.stars[j].p)<1.95*max(newSystem.stars[j].mass,newSystem.stars[i].mass))
      until boundToOther;

      totalMass+=mass;
      posCenter+=p*mass;
      velCenter+=v*mass;
    end;
    newSystem.qualityMeasure:=0;
    posCenter*=(1/totalMass);
    velCenter*=(1/totalMass);
    for i:=0 to length(newSystem.stars)-1 do with newSystem.stars[i] do begin
      p-=posCenter;
      v-=velCenter;
    end;

    maxMass:=10/maxMass;
    for i:=0 to length(newSystem.stars)-1 do begin
      multiplyStarMass(newSystem.stars[i],maxMass);
      maxRadius:=max(maxRadius,newSystem.stars[i].radius);
      for j:=0 to i-1 do minDistance:=min(minDistance,euklideanNorm(newSystem.stars[i].p-newSystem.stars[j].p));
    end;
    beta:=(4+10*random)*maxRadius/minDistance;
    if beta<1 then for i:=0 to length(newSystem.stars)-1 do begin
      newSystem.stars[i].p*=beta;
      newSystem.stars[i].v*=1/sqrt(beta);
    end;

    evaluateSystem(newSystem);
    enterCriticalSection(sysCs);
    if newSystem.rDelta   <sysRange[starCount].rDelta   [0] then sysRange[starCount].rDelta   [0]:=newSystem.rDelta;
    if newSystem.rDelta   >sysRange[starCount].rDelta   [1] then sysRange[starCount].rDelta   [1]:=newSystem.rDelta;
    if newSystem.vDelta   <sysRange[starCount].vDelta   [0] then sysRange[starCount].vDelta   [0]:=newSystem.vDelta;
    if newSystem.vDelta   >sysRange[starCount].vDelta   [1] then sysRange[starCount].vDelta   [1]:=newSystem.vDelta;
    if newSystem.massDelta<sysRange[starCount].massDelta[0] then sysRange[starCount].massDelta[0]:=newSystem.massDelta;
    if newSystem.massDelta>sysRange[starCount].massDelta[1] then sysRange[starCount].massDelta[1]:=newSystem.massDelta;

    if newSystem.qualityMeasure>sys[starCount,199].qualityMeasure then begin
      sys[starCount,199]:=newSystem;
      i:=199;
      while (i>0) and (sys[starCount,i-1].qualityMeasure<sys[starCount,i].qualityMeasure) do begin
        newSystem:=sys[starCount,i-1];
        sys[starCount,i-1]:=sys[starCount,i];
        sys[starCount,i]:=newSystem;
        dec(i);
      end;
      writeln('Created new system of ',starCount,' stars with quality measure: ',newSystem.qualityMeasure:10:3,', ranked ',i);
      inc(statCounter);
      if statCounter>=100 then writeStatistics;
    end;
    leaveCriticalSection(sysCs);
  end;

PROCEDURE TCachedSystems.prepareAnySystem;
  VAR i:longint;
      k:longint=2;
  begin
    for i:=3 to 5 do if sys[i,199].qualityMeasure<sys[k,199].qualityMeasure then k:=i;
    prepareSystem(k);
  end;

FUNCTION TCachedSystems.getSystem(CONST starCount: longint; CONST target:TsysTarget): TStarSys;
  FUNCTION isBetter(CONST A,B:TStarSys):boolean;
    begin
      case target of
        smallMassVariance : result:=(A.massDelta<B.massDelta);
        bigMassVariance   : result:=(A.massDelta>B.massDelta);
        uniformRadii      : result:=(A.rDelta   <B.rDelta);
        varyingRadii      : result:=(A.rDelta   >B.rDelta);
        uniformVelocitied : result:=(A.vDelta   <B.vDelta);
        varyingVelocities : result:=(A.vDelta   >B.vDelta);
        else result:=false;
      end;
    end;

  VAR i:longint;
      alternative:TStarSys;
  begin
    if starCount<=1 then begin
      setLength(result.stars,1);
      with result.stars[0] do begin
        randomStarStats(radius,mass,color);
        p:=ZERO_VECTOR;
        v:=ZERO_VECTOR;
      end;
    end else begin
      enterCriticalSection(sysCs);
      result:=sys[starCount,random(200)];
      if isInfinite(result.qualityMeasure) then begin
        while isInfinite(sys[starCount,0].qualityMeasure) do prepareSystem(starCount);
        result:=sys[starCount,0];
      end else begin
        if target<>none then for i:=0 to 19 do begin
          alternative:=sys[starCount,random(200)];
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

FUNCTION TStarSys.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR starCount: byte;
      i,j:longint;
  begin
    qualityMeasure:=stream.readDouble;
    massDelta     :=stream.readDouble;
    vDelta        :=stream.readDouble;
    rDelta        :=stream.readDouble;
    starCount:=stream.readByte;
    if (starCount<1) or (starCount>8) or not(stream.allOkay) then exit(false);
    setLength(stars,starCount);
    for i:=0 to length(stars)-1 do with stars[i] do begin
      for j:=0 to 2 do p[j]    :=stream.readDouble;
      for j:=0 to 2 do v[j]    :=stream.readDouble;
      for j:=0 to 2 do color[j]:=stream.readDouble;
      radius:=stream.readDouble;
      mass:=stream.readDouble;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE TStarSys.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:longint;
  begin
    stream.writeDouble(qualityMeasure);
    stream.writeDouble(massDelta     );
    stream.writeDouble(vDelta        );
    stream.writeDouble(rDelta        );
    stream.writeByte(length(stars));
    for i:=0 to length(stars)-1 do with stars[i] do begin
      for j:=0 to 2 do stream.writeDouble(p[j]);
      for j:=0 to 2 do stream.writeDouble(v[j]);
      for j:=0 to 2 do stream.writeDouble(color[j]);
      stream.writeDouble(radius);
      stream.writeDouble(mass);
    end;
  end;

VAR starTicks:qword=0;
    dustTicks:qword=0;
    startOfSampling:qword=0;
    sampleCount:longint=0;
PROCEDURE TParticleEngine.MoveParticles(CONST dt: Tfloat; CONST stressed:boolean);
  PROCEDURE moveStars;
    VAR i,j:longint;
        pm,vm:TVector3;
        dtEff: Tfloat;
        dtRest:Tfloat;
        previousAcceleration:array[0..4] of TVector3;
    PROCEDURE updateAccelerations;
      VAR i,j:longint;
          af:TVector3;
      begin
        for i:=0 to length(star)-1 do begin
          previousAcceleration[i]:=star[i].a;
          star[i].a:=ZERO_VECTOR;
        end;
        for i:=1 to length(star)-1 do
        for j:=0 to i-1 do begin
          af:=accelFactor(star[i].p,star[j].p);
          star[i].a+=af*star[j].mass;
          star[j].a-=af*star[i].mass;
        end;
      end;

    begin
      dtRest:=dt;
      while dtRest>0 do begin
        //Time step control:
        dtEff:=dtRest/ceil(dtRest/maxTimeStep);
        dtRest-=dtEff;

        for i:=0 to length(star)-1 do with star[i] do p+=v*dtEff+a*(dtEff*dtEff*0.5);
        updateAccelerations();
        for i:=0 to length(star)-1 do with star[i] do begin
          //Movement (-> Velocity Verlet)
          v+=(previousAcceleration[i]+a)*(dtEff*0.5);

          //Trajectory
           j:=length(trajectory);
           if (j=0) or (sqrEuklideanNorm(p-trajectory[j-1])>0.01) then begin
             setLength(trajectory,j+1);
             trajectory[j]:=p;
           end;
           if ((j>10) and (sqrEuklideanNorm(trajectory[0]-p)<sqr(radius))) or (j>2000) then begin
             for j:=0 to length(trajectory)-2 do trajectory[j]:=trajectory[j+1];
             setLength(trajectory,length(trajectory)-1);
           end;
        end;

        //Check for star collisions:
        for i:=1 to length(star)-1 do
        for j:=0 to i-1 do
        if (sqrt(sqr(star[i].p[0]-star[j].p[0])+
                 sqr(star[i].p[1]-star[j].p[1])+
                 sqr(star[i].p[2]-star[j].p[2]))<0.8*(star[i].radius+star[j].radius)) and (star[i].mass>0) and (star[j].mass>0)
        then begin
          pm:=star[i].p*star[i].mass+star[j].p*star[j].mass;
          vm:=star[i].v*star[i].mass+star[j].v*star[j].mass;
          star[j].color:=star[i].color*star[i].mass+star[j].color*star[j].mass;
          star[j].mass+=star[i].mass;
          star[j].color*=1/star[j].mass;
          star[j].radius:=power(power(star[j].radius,3)+power(star[i].radius,3),1/3);
          star[j].p:=pm*(1/star[j].mass);
          star[j].v:=vm*(1/star[j].mass);
          star[i].mass:=-1; //marker
        end;
        j:=0;
        for i:=0 to length(star)-1 do if star[i].mass>0 then begin
          if i<>j then star[j]:=star[i]; inc(j);
        end;
        setLength(star,j);
      end;
    end;

  PROCEDURE moveDust;
    VAR i,j:longint;
        gravitationallyBound:boolean;
        removalFactor:Tfloat;
        sqrDtHalf,
        dtHalf:Tfloat;
        aNew:TVector3;

        sqrStarRadius,sqrStarMass:array[0..4] of Tfloat;

    begin
      for i:=0 to length(star)-1 do begin
        sqrStarRadius[i]:=sqr(star[i].radius);
        sqrStarMass  [i]:=4*sqr(star[i].mass);

      end;

      dtHalf:=dt*0.5;
      sqrDtHalf:=dt*dtHalf;
      if stressed
      then removalFactor:=5*dt
      else removalFactor:=0.1*dt;
      for i:=0 to length(dust)-1 do with dust[i] do if not(flaggedForRemoval) then begin
        p+=v*dt+a*sqrDtHalf;
        aNew:=ZERO_VECTOR;
        for j:=0 to length(star)-1 do begin
          aNew+=accelFactor(p,star[j].p)*star[j].mass;
          flaggedForRemoval:=flaggedForRemoval or
            (sqr(p[0]-star[j].p[0])+
             sqr(p[1]-star[j].p[1])+
             sqr(p[2]-star[j].p[2])<sqrStarRadius[j]);
        end;
        v+=(a+aNew)*dtHalf;
        a:=aNew;

        if not(flaggedForRemoval) and (removalFactor>random) then begin
          gravitationallyBound:=false;
          for j:=0 to length(star)-1 do gravitationallyBound:=gravitationallyBound or //(sqrEuklideanNorm(v-star[j].v)*euklideanNorm(p-star[j].p)<2*star[j].mass)
                                                                                      (sqr(sqrEuklideanNorm(v-star[j].v))* sqrEuklideanNorm(p-star[j].p)<sqrStarMass[j])
                                                                                   or ((v-star[j].v)*(p-star[j].p)<0);
          if not(gravitationallyBound) then flaggedForRemoval:=true;
        end;
        if flaggedForRemoval then inc(removalCounter);
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
    if length(star)>1 then moveStars;
    t1:=GetTickCount64;
    totalTime+=dt;
    moveDust;
    t2:=GetTickCount64;

    inc(sampleCount);
    starTicks+=(t1-t0);
    dustTicks+=(t2-t1);
    if (starTicks+dustTicks)>10000
    then begin
      writeln('Movement cost: stars ',starTicks,' dust ',dustTicks,'; fraction or total: ',starTicks/(GetTickCount64-startOfSampling)*100:0:3,'% / ',dustTicks/(GetTickCount64-startOfSampling)*100:0:3,'%; per step: ',starTicks/sampleCount:0:3,' / ',dustTicks/sampleCount:0:3);
      starTicks:=0;
      dustTicks:=0;
      sampleCount:=0;
      startOfSampling:=GetTickCount64;
    end;
  end;

CONSTRUCTOR TParticleEngine.create;
  begin
    randomize;
    dtFactor:=0;
    cachedSystems.create;
  end;

DESTRUCTOR TParticleEngine.destroy;
  begin
    inherited destroy;
    cachedSystems.destroy;
  end;

PROCEDURE TParticleEngine.update(CONST tickDelta: qword; stressed:boolean);
  VAR dt:Tfloat;
  begin
    if dtFactor=0 then exit;
    dt:=tickDelta*dtFactor;
    if dt>maxDustTimeStep then begin
      stressed:=true;
      dt:=maxDustTimeStep;
    end;
    MoveParticles(dt,stressed);
  end;

PROCEDURE TParticleEngine.DrawParticles(CONST ParticleList: GLuint; CONST pointSize:Tfloat);
  VAR i: integer;
      x: TVector3;
      j: longint;
  begin
    for i:=0 to length(star)-1 do begin
      glColor3d(star[i].color[0],star[i].color[1],star[i].color[2]);
      glPushMatrix;
      glTranslated(star[i].p[0], star[i].p[1], star[i].p[2]);
      glScaled(star[i].radius,star[i].radius,star[i].radius);
      glCallList(ParticleList);
      glPopMatrix;
      glBegin(GL_LINE_STRIP);
      for j:=0 to length(star[i].trajectory)-1 do begin
        x:=star[i].color*(j/length(star[i].trajectory));
        glColor3d(x[0],x[1],x[2]);
        glVertex3dv(@star[i].trajectory[j]);
//        glVertex3fv(@star[i].trajectory[j]);
      end;
      glEnd();
    end;
    glPointSize(pointSize);
    glBegin(GL_POINTS);
    for i:=0 to length(dust)-1 do if not(dust[i].flaggedForRemoval) then begin
      x[0]:=0.2+0.5*i/(length(dust)-1);
      glColor3d(x[0],x[0],x[0]);
//      glVertex3fv(@dust[i].p);
      glVertex3dv(@dust[i].p);
    end;
    glEnd();
  end;

PROCEDURE TParticleEngine.initDust(CONST particleCount: longint; starMask: byte; CONST mode: TDustInitMode);
  VAR starIndex:array of longint;
  VAR systemRadius:Tfloat;
      pointNemo:TVector3;
      vNemo:TVector3;
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
      distanceFromStar:=star[starIndex].radius*1.5+sqrt(random)*3;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar)*speedFactor;
      e1:=random*2*pi;
      e0:=cos(e1);
      e1:=sin(e1);
      result.p:=star[starIndex].p+base[starIndex,0]*e0*distanceFromStar+base[starIndex,1]*e1*distanceFromStar;
      result.v:=star[starIndex].v+base[starIndex,0]*e1*speed           -base[starIndex,1]*e0*speed;
      result.flaggedForRemoval:=false;
    end;

  FUNCTION shell(CONST starIndex:longint):TParticle;
    VAR dx,dv:TVector3;
        distanceFromStar,speed:Tfloat;
    begin
      distanceFromStar:=star[starIndex].radius*1.5;
      speed:=sqrt(star[starIndex].mass);
      if speed>distanceFromStar then distanceFromStar:=speed;

      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      dx:=randomOnSphere*distanceFromStar;
      //speed is perpendicular to dx
      dv:=cross(dx,randomOnSphere);
      dv*=speed/euklideanNorm(dv);
      result.p:=star[starIndex].p+dx;
      result.v:=star[starIndex].v+dv;
    end;

  FUNCTION randomParticle(CONST starIndex:longint):TParticle;
    VAR dx,dv:TVector3;
        distanceFromStar,speed:Tfloat;
    begin
      distanceFromStar:=star[starIndex].radius*1.5+sqrt(random)*3;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      dx:=randomOnSphere*distanceFromStar;
      dv:=randomInSphere*(2*speed);
      result.p:=star[starIndex].p+dx;
      result.v:=star[starIndex].v+dv;
    end;

  FUNCTION ringParticle(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:Tfloat;
        e0,e1:Tfloat;
    begin
      distanceFromStar:=star[starIndex].radius*1.5+1.5+0.05*random;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      e1:=random*2*pi;
      e0:=cos(e1);
      e1:=sin(e1);
      result.p:=star[starIndex].p+base[starIndex,0]*e0*distanceFromStar+base[starIndex,1]*e1*distanceFromStar;
      result.v:=star[starIndex].v+base[starIndex,0]*e1*speed           -base[starIndex,1]*e0*speed;
    end;

  FUNCTION ring3Particle(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:Tfloat;
        e0,e1:Tfloat;
        k:longint;
    begin
      k:=random(3);
      distanceFromStar:=star[starIndex].radius*1.5+1.4+0.1*k+0.05*random;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      e1:=random*2*pi;
      e0:=cos(e1);
      e1:=sin(e1);
      result.p:=star[starIndex].p+base[starIndex,k]*e0*distanceFromStar+base[starIndex,(k+1) mod 3]*e1*distanceFromStar;
      result.v:=star[starIndex].v+base[starIndex,k]*e1*speed           -base[starIndex,(k+1) mod 3]*e0*speed;
    end;

  FUNCTION planetLike(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:Tfloat;
        e0,e1,e2:Tfloat;
        alpha,beta:Tfloat;
        dv,dp:TVector3;
    begin

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
    end;

  FUNCTION randomBackgroundParticle:TParticle;
    VAR i:longint;
        inStar:boolean=true;
    begin
      repeat
        result.p:=randomInSphere*systemRadius;
        inStar:=false;
        for i:=0 to length(star)-1 do inStar:=inStar or (euklideanNorm(result.p-star[i].p)<1.5*star[i].radius);
      until not inStar;
      result.v:=gravitationalSpeed(result.p);
    end;

  FUNCTION lonelyCloudParticle:TParticle;
    VAR i:longint;
        inStar:boolean=true;
    begin
      repeat
        result.p:=pointNemo+randomInSphere;
        inStar:=false;
        for i:=0 to length(star)-1 do inStar:=inStar or (euklideanNorm(result.p-star[i].p)<1.5*star[i].radius);
      until not inStar;
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

  FUNCTION basisOf(CONST v,a:TVector3):TMatrix3x3;
    begin
      result[0]:=v; result[0]*=1/euklideanNorm(result[0]);
      result[2]:=cross(result[0],a); result[2]*=1/euklideanNorm(result[2]);
      result[1]:=cross(result[0],result[2]);
    end;

  VAR k:longint;
  begin
    vNemo:=ZERO_VECTOR;
    removalCounter:=0;
    setLength(base,length(star));
    for k:=0 to length(base)-1 do base[k]:=basisOf(star[k].v,star[k].a);
    systemRadius:=0;
    for k:=0 to length(star)-1 do systemRadius:=max(systemRadius,euklideanNorm(star[k].p)+1.5*star[k].radius+3);

    findPointNemo;

    setLength(starIndex,particleCount);
    initStarIndexes;

    setLength(dust,particleCount);
    case mode of
      dim_stableDisk:         for k:=0 to length(dust)-1 do dust[k]:=diskParticle(starIndex[k],1);
      dim_stableDiskReversed: for k:=0 to length(dust)-1 do dust[k]:=diskParticle(starIndex[k],-1);
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

PROCEDURE TParticleEngine.initStars(CONST sys: TStarSys);
  VAR k:longint;
  begin
    setLength(star,length(sys.stars));
    for k:=0 to length(star)-1 do begin
      star[k]:=sys.stars[k];
      star[k].a:=ZERO_VECTOR;
      setLength(star[k].trajectory,0);
    end;
    totalTime:=0;
  end;

PROCEDURE TParticleEngine.resetStars;
  begin
    initStars(cachedSystems.lastResponse);
  end;

FUNCTION TParticleEngine.dustCount: longint;
  begin
    result:=length(dust)-removalCounter;
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
      for j:=0 to length(trajectory)-1 do trajectory[j]*=factor;
    end;
    for i:=0 to length(dust)-1 do with dust[i] do begin
      p*=factor;
      v*=vFactor;
      a*=aFactor;
    end;
  end;

end.

