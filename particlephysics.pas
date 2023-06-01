UNIT particlePhysics;

{$mode objfpc}{$H+}

INTERFACE
USES vectors,GL,serializationUtil;

TYPE
  TStar = record
    trajectory:array of TVector3;
    p,v,a,color:TVector3;
    radius,mass : double;
  end;

  TParticle = record
    p,v:TVector3;
    flaggedForRemoval:boolean;
  end;

  { TStarSys }

  TStarSys=object(T_serializable)
    qualityMeasure:double;
    stars:array of TStar;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

  { TCachedSystems }
  PCachedSystems=^TCachedSystems;
  TCachedSystems=object(T_serializable)
    sys:array[2..5,0..99] of TStarSys;
    sysCs:TRTLCriticalSection;
    lastResponse:TStarSys;
    backgroundRunning,destroying:boolean;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE writeStatistics;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    PROCEDURE prepareSystem(CONST starCount:longint; CONST qualityToBeat:double);
    PROCEDURE prepareAnySystem;
    FUNCTION getSystem(CONST starCount:longint):TStarSys;
  end;

  TDustInitMode = (dim_stableDisk,
                   dim_stableOrbit,
                   dim_randomCloud,
                   dim_stillBackgroundCloud,
                   dim_singleRing,
                   dim_threeRings,
                   dim_shell,
                   dim_planetLike,
                   dim_lonelyCloud);

  { TParticleEngine }

  TParticleEngine = class
  private
    star :  array of TStar;
    dust :  array of TParticle;
    removalCounter:longint;
    PROCEDURE MoveParticles(CONST dt:double; CONST stressed:boolean);
  public
    cachedSystems:TCachedSystems;
    totalTime:double;
    dtFactor:double;

    CONSTRUCTOR create;
    DESTRUCTOR destroy; override;
    PROCEDURE update(CONST tickDelta:qword; CONST stressed:boolean);
    PROCEDURE DrawParticles(CONST ParticleList: GLuint);

    PROCEDURE initDust(CONST particleCount:longint; starMask:byte; CONST mode:TDustInitMode);
    PROCEDURE initStars(starCount:longint);
    PROCEDURE initStars(CONST sys:TStarSys);
    PROCEDURE resetStars;
    FUNCTION dustCount:longint;

  end;

IMPLEMENTATION
USES math,sysutils,LCLProc;
CONST maxDustRemovalRadius=1E4;
      minTimeStep=1E-4;
      maxTimeStep=1E-1;
FUNCTION accelFactor(CONST pSelf,pDrawnTowards:TVector3):TVector3;
  VAR f:double;
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
  begin
    with PCachedSystems(p)^ do begin
      repeat prepareAnySystem until destroying;
      backgroundRunning:=false;
    end;
    result:=0;
  end;

CONSTRUCTOR TCachedSystems.create;
  VAR i,j:longint;
  begin
    randomize;
    initCriticalSection(sysCs);
    if not loadFromFile(ChangeFileExt(paramStr(0),'.cached_sys')) then for i:=2 to 5 do for j:=0 to 99 do sys[i,j].qualityMeasure:=-infinity;
    writeStatistics;
    backgroundRunning:=true;
    beginThread(@makeSystemsInBackground,@self);
  end;

DESTRUCTOR TCachedSystems.destroy;
  VAR i:longint;
  begin
    destroying:=true;
    while backgroundRunning do sleep(1);
    enterCriticalSection(sysCs);
    for i:=2 to 5 do while isInfinite(sys[i,99].qualityMeasure) do prepareSystem(i,infinity);
    writeStatistics;
    saveToFile(ChangeFileExt(paramStr(0),'.cached_sys'));
    leaveCriticalSection(sysCs);
    doneCriticalSection(sysCs);
  end;

PROCEDURE TCachedSystems.writeStatistics;
  begin
    writeln('===========================================');
    writeln(' Cached system statistics:');
    writeln('                Best     Median      Worst');
    writeln(' 2 stars: ',sys[2,0].qualityMeasure:10:3,' ',sys[2,50].qualityMeasure:10:3,' ',sys[2,99].qualityMeasure:10:3);
    writeln(' 3 stars: ',sys[3,0].qualityMeasure:10:3,' ',sys[3,50].qualityMeasure:10:3,' ',sys[3,99].qualityMeasure:10:3);
    writeln(' 4 stars: ',sys[4,0].qualityMeasure:10:3,' ',sys[4,50].qualityMeasure:10:3,' ',sys[4,99].qualityMeasure:10:3);
    writeln(' 5 stars: ',sys[5,0].qualityMeasure:10:3,' ',sys[5,50].qualityMeasure:10:3,' ',sys[5,99].qualityMeasure:10:3);
    writeln('===========================================');
  end;

FUNCTION TCachedSystems.getSerialVersion: dword;
  begin
    result:=0;
  end;

FUNCTION TCachedSystems.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i,j:longint;
  begin
    if not(inherited) then exit;
    result:=true;
    for i:=2 to 5 do for j:=0 to 99 do begin
      sys[i,j].create;
      result:=result and sys[i,j].loadFromStream(stream);
    end;
    result:=result and stream.allOkay;
  end;

PROCEDURE TCachedSystems.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:longint;
  begin
    inherited;
    for i:=2 to 5 do for j:=0 to 99 do sys[i,j].saveToStream(stream);
  end;

PROCEDURE randomStarStats(OUT radius,mass:double; OUT color:TVector3);
  VAR density:double;
      commonFactor:double;
      x:double;
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

PROCEDURE TCachedSystems.prepareSystem(CONST starCount: longint; CONST qualityToBeat:double);
  VAR i:longint;
      totalMass:double;
      posCenter,velCenter:TVector3;

  PROCEDURE evaluateSystem(VAR sys:TStarSys);
    TYPE TBind=(Bind01,Bind02,Bind03,Bind04,
                       Bind12,Bind13,Bind14,
                              Bind23,Bind24,
                                     Bind34,Bind_X);
         TBindSet=set of TBind;
    CONST CBind:array[0..4,0..4] of TBind=
            ((Bind_X,Bind01,Bind02,Bind03,Bind04),
             (Bind01,Bind_X,Bind12,Bind13,Bind14),
             (Bind02,Bind12,Bind_X,Bind23,Bind24),
             (Bind03,Bind13,Bind23,Bind_X,Bind34),
             (Bind04,Bind14,Bind24,Bind34,Bind_X));
          CBindTransitive:array[0..9,0..2] of TBind=
            ((Bind01,Bind02,Bind12),
             (Bind01,Bind03,Bind13),
             (Bind01,Bind04,Bind14),
             (Bind02,Bind03,Bind23),
             (Bind02,Bind04,Bind24),
             (Bind03,Bind04,Bind34),
             (Bind12,Bind13,Bind23),
             (Bind12,Bind14,Bind24),
             (Bind13,Bind14,Bind34),
             (Bind23,Bind24,Bind34));
          CRequiredBinds:array[2..5] of TBindSet=([Bind01],
                                                  [Bind02,Bind12       ],
                                                  [Bind03,Bind13,Bind23],
                                                  [Bind04,Bind14,Bind24,Bind34]);
    VAR accel,step:TVector3;
        i,j:longint;
        aMax:double;
        p,v,a:array[0..4] of TVector3;
        dt:double=1E-3;
        dist:double;
        minimumPairwiseDistance:double=infinity;
        totalTime:double=0;
        totalDistance:double=0;
        boundPairs:set of TBind;
    begin
      with sys do begin
        for i:=0 to starCount-1 do begin
          p[i]:=stars[i].p;
          v[i]:=stars[i].v;
        end;
        while (totalDistance<qualityToBeat*starCount) do begin
          for i:=0 to starCount-1 do a[i]:=ZERO_VECTOR;
          for i:=1 to starCount-1 do for j:=0 to i-1 do begin
            accel:=accelFactor(p[i],p[j]);
            a[i]+=accel*stars[j].mass;
            a[j]-=accel*stars[i].mass;
          end;
          aMax:=0;
          for i:=0 to starCount-1 do aMax:=max(aMax,sqrEuklideanNorm(a[i]));
          dt:=sqrt(1E-7/sqrt(aMax));
          if dt>maxTimeStep then dt:=maxTimeStep;
          totalTime+=dt;

          boundPairs:=[];
          for i:=0 to starCount-1 do begin
            v[i]+=a[i]*dt;
            step:=v[i]*dt; totalDistance+=euklideanNorm(step);
            p[i]+=v[i]*dt;
            for j:=0 to i-1 do begin
              dist:=euklideanNorm(p[i]-p[j]);
              if dist<3*(stars[i].radius+stars[j].radius)
              then begin
                //Rejected because of collision
                qualityMeasure:=totalDistance/starCount;
                exit;
              end;
              if dist<minimumPairwiseDistance then minimumPairwiseDistance:=dist;
              if (sqrEuklideanNorm(v[i]-v[j])*dist<1.99*max(stars[j].mass,stars[i].mass)) then include(boundPairs,CBind[i,j]);
            end;
          end;
          for i:=9 downto 0 do
            if (CBindTransitive[i,0] in boundPairs) and
               (CBindTransitive[i,1] in boundPairs) then include(boundPairs,CBindTransitive[i,2]);
          if (boundPairs*CRequiredBinds[starCount]<>CRequiredBinds[starCount]) then begin
            qualityMeasure:=totalDistance/starCount;
            exit;
          end;

        end;
        qualityMeasure:=totalDistance/starCount;
      end;
    end;

  VAR newSytem:TStarSys;
      closeToOther:boolean;
      boundToOther:boolean;
      j:longint;
  begin
    newSytem.create;
    totalMass:=0;
    posCenter:=ZERO_VECTOR;
    velCenter:=ZERO_VECTOR;
    setLength(newSytem.stars,starCount);
    for i:=0 to length(newSytem.stars)-1 do with newSytem.stars[i] do begin
      randomStarStats(radius,mass,color);
      repeat
        p:=randomInSphere*10;
        closeToOther:=false;
        for j:=0 to i-1 do closeToOther:=closeToOther or (euklideanNorm(p-newSytem.stars[j].p)<2*(radius+newSytem.stars[j].radius));
      until not(closeToOther);
      repeat
        v:=randomInSphere;
        boundToOther:=i=0;
        for j:=0 to i-1 do boundToOther:=boundToOther or (sqrEuklideanNorm(v-newSytem.stars[j].v)*euklideanNorm(p-newSytem.stars[j].p)<1.95*max(newSytem.stars[j].mass,newSytem.stars[i].mass))
      until boundToOther;

      v:=randomInSphere;
      totalMass+=mass;
      posCenter+=p*mass;
      velCenter+=v*mass;
    end;

    newSytem.qualityMeasure:=0;

    posCenter*=(1/totalMass);
    velCenter*=(1/totalMass);
    for i:=0 to length(newSytem.stars)-1 do with newSytem.stars[i] do begin
      p-=posCenter;
      v-=velCenter;
    end;

    evaluateSystem(newSytem);
    enterCriticalSection(sysCs);
    if newSytem.qualityMeasure>sys[starCount,99].qualityMeasure then begin
      sys[starCount,99]:=newSytem;
      i:=99;
      while (i>0) and (sys[starCount,i-1].qualityMeasure<sys[starCount,i].qualityMeasure) do begin
        newSytem:=sys[starCount,i-1];
        sys[starCount,i-1]:=sys[starCount,i];
        sys[starCount,i]:=newSytem;
        dec(i);
      end;
      writeln('Created new system of ',starCount,' stars with quality measure: ',newSytem.qualityMeasure:10:3,' (target was',qualityToBeat:10:3,'), ranked ',i);
    end;
    leaveCriticalSection(sysCs);
  end;

PROCEDURE TCachedSystems.prepareAnySystem;
  VAR i:longint;
      k:longint=2;
      qTarget:double;
  begin
    for i:=3 to 5 do if sys[i,99].qualityMeasure<sys[k,99].qualityMeasure then k:=i;
    qTarget:=sys[k,0].qualityMeasure;
    if isInfinite(qTarget) or (qTarget<=0) then qTarget:=1 else qTarget*=2;
    if qTarget>5000 then qTarget:=5000;
    prepareSystem(k,qTarget);
  end;

FUNCTION TCachedSystems.getSystem(CONST starCount: longint): TStarSys;
  VAR i:longint=0;
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
      while (random<0.95) do begin inc(i); if i>=100 then i:=0; end;
      result:=sys[starCount,i];
      if isInfinite(result.qualityMeasure) then begin
        while isInfinite(sys[starCount,0].qualityMeasure) do prepareSystem(starCount,1);
        result:=sys[starCount,0];
      end;
      leaveCriticalSection(sysCs);
    end;
    lastResponse:=result;
  end;

CONSTRUCTOR TStarSys.create;
  begin
    inherited;
  end;

DESTRUCTOR TStarSys.destroy;
  begin
    inherited;
  end;

FUNCTION TStarSys.getSerialVersion: dword;
  begin
    result:=1231825;
  end;

FUNCTION TStarSys.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR starCount: byte;
      i,j:longint;
  begin
    //if not(inherited) then exit(false);
    qualityMeasure:=stream.readDouble;
    starCount:=stream.readByte;
    if (starCount<1) or (starCount>8) or not(stream.allOkay) then exit(false);
    setLength(stars,starCount);
    for i:=0 to length(stars)-1 do with stars[i] do begin
      for j:=0 to 2 do p[j]    :=stream.readSingle;
      for j:=0 to 2 do v[j]    :=stream.readSingle;
      for j:=0 to 2 do color[j]:=stream.readSingle;
      radius:=stream.readSingle;
      mass:=stream.readSingle;
    end;
    result:=stream.allOkay;
  end;

PROCEDURE TStarSys.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:longint;
  begin
    //inherited;
    stream.writeDouble(qualityMeasure);
    stream.writeByte(length(stars));
    for i:=0 to length(stars)-1 do with stars[i] do begin
      for j:=0 to 2 do stream.writeSingle(p[j]);
      for j:=0 to 2 do stream.writeSingle(v[j]);
      for j:=0 to 2 do stream.writeSingle(color[j]);
      stream.writeSingle(radius);
      stream.writeSingle(mass);
    end;
  end;

PROCEDURE TParticleEngine.MoveParticles(CONST dt: double; CONST stressed:boolean);
  PROCEDURE moveStars;
    VAR i,j:longint;
        pm,vm:TVector3;
        aMax:double=0;
        dtEff: double;
        dtRest:double;
    PROCEDURE updateAccelerations;
      VAR i,j:longint;
          af:TVector3;
      begin
        for i:=0 to length(star)-1 do star[i].a:=ZERO_VECTOR;
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
        updateAccelerations;
        //Time step control:
        for i:=0 to length(star)-1 do with star[i] do aMax:=max(aMax,sqrEuklideanNorm(a));
        dtEff:=sqrt(1E-7/sqrt(aMax));
        dtEff:=dtRest/ceil(dtRest/dtEff);
        dtRest-=dtEff;
        //Movement and trajectories:
        for i:=0 to length(star)-1 do with star[i] do begin
          v+=a*dtEff;
          p+=v*dtEff;

          j:=length(trajectory);
          if (j=0) or (sqrEuklideanNorm(p-trajectory[j-1])>0.01) then begin
            setLength(trajectory,j+1);
            trajectory[j]:=p;
          end;
          if ((j>100) and (sqrEuklideanNorm(trajectory[0]-p)<sqr(radius))) or (j>2000) then begin
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
        captured:boolean;
        removalFactor:double;
        boundFactor:double;
    begin
      if stressed
      then removalFactor:=5*dt
      else removalFactor:=  dt;
      if stressed
      then boundFactor:=2
      else boundFactor:=2.5;
      for i:=0 to length(dust)-1 do with dust[i] do if not(flaggedForRemoval) then begin
        captured:=false;
        for j:=0 to length(star)-1 do begin
          v+=             accelFactor(p,star[j].p)*(star[j].mass*dt);
          captured:=captured or (sqrEuklideanNorm(v-star[j].v)*euklideanNorm(p-star[j].p)<boundFactor*star[j].mass);
        end;
        if not(captured) and (removalFactor>random) then flaggedForRemoval:=true;
        p+=v*dt;
        for j:=0 to length(star)-1 do flaggedForRemoval:=flaggedForRemoval or
          (sqr(p[0]-star[j].p[0])+
           sqr(p[1]-star[j].p[1])+
           sqr(p[2]-star[j].p[2])<sqr(star[j].radius));
        if flaggedForRemoval then inc(removalCounter);
      end;
      if removalCounter>0 then inc(removalCounter);
      if removalCounter*100>length(dust) then begin
        removalCounter:=0;
        j:=0;
        for i:=0 to length(dust)-1 do if not(dust[i].flaggedForRemoval) then begin
          if i<>j then dust[j]:=dust[i];
          inc(j);
        end;
        setLength(dust,j);
      end;
    end;

  begin
    if length(star)>1 then moveStars;
    totalTime+=dt;
    {$ifdef debugMode}
    writeln('r0 = ',sqrt(dustFalloffSqrRadius),' - ',BoolToStr(stressed,'stressed','relaxed'));
    {$endif}
    moveDust;
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

PROCEDURE TParticleEngine.update(CONST tickDelta: qword; CONST stressed:boolean);
  begin
    if dtFactor=0 then begin
      cachedSystems.prepareAnySystem;
      exit;
    end;
    MoveParticles(tickDelta*dtFactor,stressed);
  end;

PROCEDURE TParticleEngine.DrawParticles(CONST ParticleList: GLuint);
  VAR i: integer;
      x: TVector3;
  begin
    for i:=0 to length(star)-1 do begin
      glColor3f(star[i].color[0],star[i].color[1],star[i].color[2]);
      glPushMatrix;
      glTranslatef(star[i].p[0], star[i].p[1], star[i].p[2]);
      glScalef(star[i].radius,star[i].radius,star[i].radius);
      glCallList(ParticleList);
      glPopMatrix;

      glBegin(GL_LINE_STRIP);
      for x in star[i].trajectory do glVertex3f(x[0],x[1],x[2]);
      glEnd();
    end;
    glPointSize(2);
    //glColor3f(0.5,0.5,0.5);
    glBegin(GL_POINTS);
    for i:=0 to length(dust)-1 do if not(dust[i].flaggedForRemoval) then begin
      x[0]:=0.2+0.5*i/(length(dust)-1);
      glColor3f(x[0],x[0],x[0]);
      glVertex3f(dust[i].p[0],dust[i].p[1],dust[i].p[2]);
    end;
    glEnd();
  end;

PROCEDURE TParticleEngine.initDust(CONST particleCount: longint; starMask: byte; CONST mode: TDustInitMode);
  VAR starIndex:array of longint;
  VAR base:array of TMatrix3x3;
      systemRadius:double;
      pointNemo:TVector3;
      vNemo    :TVector3;

  FUNCTION diskParticle(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:double;
        e0,e1:double;
    begin
      distanceFromStar:=star[starIndex].radius*1.5+sqrt(random)*3;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      e1:=random*2*pi;
      e0:=cos(e1);
      e1:=sin(e1);
      result.p:=star[starIndex].p+base[starIndex,0]*e0*distanceFromStar+base[starIndex,1]*e1*distanceFromStar;
      result.v:=star[starIndex].v+base[starIndex,0]*e1*speed           -base[starIndex,1]*e0*speed;
      result.flaggedForRemoval:=false;
    end;

  FUNCTION orbitParticle(CONST starIndex:longint):TParticle;
    VAR dx,dv:TVector3;
        distanceFromStar,speed:double;
    begin
      distanceFromStar:=star[starIndex].radius*1.5+sqrt(random)*3;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      dx:=randomOnSphere*distanceFromStar;
      //speed is perpendicular to dx
      dv:=cross(dx,randomOnSphere);
      dv*=speed/euklideanNorm(dv);
      result.p:=star[starIndex].p+dx;
      result.v:=star[starIndex].v+dv;
      result.flaggedForRemoval:=false;
    end;

  FUNCTION shell(CONST starIndex:longint):TParticle;
    VAR dx,dv:TVector3;
        distanceFromStar,speed:double;
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
      result.flaggedForRemoval:=false;
    end;

  FUNCTION randomParticle(CONST starIndex:longint):TParticle;
    VAR dx,dv:TVector3;
        distanceFromStar,speed:double;
    begin
      distanceFromStar:=star[starIndex].radius*1.5+sqrt(random)*3;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      dx:=randomOnSphere*distanceFromStar;
      dv:=randomInSphere*(2*speed);
      result.p:=star[starIndex].p+dx;
      result.v:=star[starIndex].v+dv;
      result.flaggedForRemoval:=false;
    end;

  FUNCTION ringParticle(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:double;
        e0,e1:double;
    begin
      distanceFromStar:=star[starIndex].radius*1.5+1.5+0.05*random;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      e1:=random*2*pi;
      e0:=cos(e1);
      e1:=sin(e1);
      result.p:=star[starIndex].p+base[starIndex,0]*e0*distanceFromStar+base[starIndex,1]*e1*distanceFromStar;
      result.v:=star[starIndex].v+base[starIndex,0]*e1*speed           -base[starIndex,1]*e0*speed;
      result.flaggedForRemoval:=false;
    end;

  FUNCTION ring3Particle(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:double;
        e0,e1:double;
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
      result.flaggedForRemoval:=false;
    end;

  FUNCTION planetLike(CONST starIndex:longint):TParticle;
    VAR distanceFromStar,speed:double;
        e0,e1,e2:double;
        alpha,beta:double;
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
      result.flaggedForRemoval:=false;
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
      result.v:=ZERO_VECTOR;
      result.flaggedForRemoval:=false;
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
      result.v:=vNemo;
      result.flaggedForRemoval:=false;
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
        mTot:double=0;
        p,a: TVector3;
        step:double=1;
        curr:double;
        best:double=infinity;

        minA,currA:double;
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

      for i:=0 to 9999 do begin
        p:=randomInSphere*systemRadius;
        a:=ZERO_VECTOR;
        for k:=0 to length(star)-1 do a+=accelFactor(p,star[k].p)*star[k].mass;
        curr:=sqrEuklideanNorm(a);
        if curr<best then begin
          best:=curr;
          pointNemo:=p;
        end;
      end;

      mTot:=0;
      for k:=0 to length(star)-1 do begin
        curr:=euklideanNorm(accelFactor(pointNemo,star[k].p)*star[k].mass);
        vNemo+=star[k].v*curr;
        mTot+=curr;
      end;
      vNemo*=1/mTot;
    end;

  VAR k:longint;
  begin
    removalCounter:=0;
    setLength(base,length(star));
    for k:=0 to length(base)-1 do base[k]:=randomOrthonormalBasis;
    systemRadius:=0;
    for k:=0 to length(star)-1 do systemRadius:=max(systemRadius,euklideanNorm(star[k].p)+1.5*star[k].radius+3);

    if mode=dim_lonelyCloud then findPointNemo;

    setLength(starIndex,particleCount);
    initStarIndexes;

    setLength(dust,particleCount);
    case mode of
      dim_stableDisk:  for k:=0 to length(dust)-1 do dust[k]:=diskParticle(starIndex[k]);
      dim_stableOrbit: for k:=0 to length(dust)-1 do dust[k]:=orbitParticle(starIndex[k]);
      dim_randomCloud: for k:=0 to length(dust)-1 do dust[k]:=randomParticle(starIndex[k]);
      dim_singleRing : for k:=0 to length(dust)-1 do dust[k]:=ringParticle(starIndex[k]);
      dim_threeRings : for k:=0 to length(dust)-1 do dust[k]:=ring3Particle(starIndex[k]);
      dim_shell      : for k:=0 to length(dust)-1 do dust[k]:=shell(starIndex[k]);
      dim_planetLike : for k:=0 to length(dust)-1 do dust[k]:=planetLike(starIndex[k]);
      dim_lonelyCloud: for k:=0 to length(dust)-1 do dust[k]:=lonelyCloudParticle;
      else             for k:=0 to length(dust)-1 do dust[k]:=randomBackgroundParticle;
    end;
  end;

PROCEDURE TParticleEngine.initStars(starCount: longint);
  begin
    initStars(cachedSystems.getSystem(starCount));
  end;

PROCEDURE TParticleEngine.initStars(CONST sys: TStarSys);
  VAR k:longint;
  begin
    setLength(star,length(sys.stars));
    for k:=0 to length(star)-1 do begin
      star[k]:=sys.stars[k];
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
    result:=length(dust);
  end;

end.

