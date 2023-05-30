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

  TCachedSystems=object(T_serializable)
    sys:array[2..5,0..99] of TStarSys;
    lastResponse:TStarSys;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;

    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    FUNCTION getSystem(CONST starCount:longint):TStarSys;
  end;

  TDustInitMode = (dim_stableDisk,
                   dim_stableOrbit,
                   dim_randomCloud,
                   dim_stillBackgroundCloud);

  { TParticleEngine }

  TParticleEngine = class
  private
    dtMax:double;
    star :  array of TStar;
    dust :  array of TParticle;
    PROCEDURE MoveParticles(CONST dt:double);
  public
    cachedSystems:TCachedSystems;
    starsCollide:boolean;
    sqrDustRemovalRadius:double;
    dtFactor:double;

    CONSTRUCTOR create;
    DESTRUCTOR destroy; override;
    PROCEDURE update(CONST tickDelta:qword; CONST subSteps:longint);
    PROCEDURE DrawParticles(CONST ParticleList: GLuint);

    PROCEDURE initDust(CONST particleCount:longint; starMask:byte; CONST mode:TDustInitMode);
    PROCEDURE initStars(starCount:longint);
    PROCEDURE initStars(CONST sys:TStarSys);
    PROCEDURE resetStars;
    FUNCTION dustCount:longint;

  end;

IMPLEMENTATION
USES math,sysutils,LCLProc;
FUNCTION accelFactor(CONST pSelf,pDrawnTowards:TVector3):TVector3; {$ifndef debugMode} inline; {$endif}
  VAR f:TGLfloat;
  begin
    result:=pDrawnTowards-pSelf;
    f:=1/sqrt(sqr(result[0])+sqr(result[1])+sqr(result[2]));
    f:=f*f*f;
    result[0]*=f;
    result[1]*=f;
    result[2]*=f;
  end;

{ TCachedSystems }

CONSTRUCTOR TCachedSystems.create;
  VAR i,j:longint;
  begin
    if not loadFromFile(ChangeFileExt(paramStr(0),'.cached_sys')) then begin
      for i:=2 to 5 do begin
        for j:=0 to 99 do sys[i,j].qualityMeasure:=infinity;
        for j:=0 to 9999 do getSystem(i);
      end;
    end;
  end;

DESTRUCTOR TCachedSystems.destroy;
  begin
    saveToFile(ChangeFileExt(paramStr(0),'.cached_sys'));
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

FUNCTION TCachedSystems.getSystem(CONST starCount: longint): TStarSys;
  VAR i:longint;
      totalMass:double;
      posCenter,velCenter:TVector3;
  FUNCTION randomStarColor:TVector3;
    VAR x:double;
    begin
      //not black...
      x:=0.1+(random+random+random)/3*2.9;
      result:=vectorOf(max(0,min(1,x)),
                       max(0,min(1,x-1)),
                       max(0,min(1,x-2)));
    end;

  PROCEDURE evaluateSystem(VAR sys:TStarSys);
    VAR accel:TVector3;
        i,j,step:longint;
        aMax:double;
        rMax:double=0;
        rMin:double=infinity;
        eMax:double=0;
        eMin:double=infinity;
        energy,radius:double;
        p,v,a:array[0..4] of TVector3;
        dt:double=1E-3;
    begin
      with sys do begin
        for i:=0 to starCount-1 do begin
          p[i]:=stars[i].p;
          v[i]:=stars[i].v;
        end;
        for step:=0 to 19999 do begin
          for i:=0 to starCount-1 do a[i]:=ZERO_VECTOR;
          for i:=1 to starCount-1 do for j:=0 to i-1 do begin
            accel:=accelFactor(p[i],p[j]);
            a[i]+=accel*stars[j].mass;
            a[j]-=accel*stars[i].mass;
          end;
          // v(t+dt)=v(t)+a(t)*dt
          // p(t+dt)=p(t)+v(t+dt)*dt
          //        =p(t)+v(t)*dt+a(t)*dt²
          // more exact (less stable):
          // p'(t+dt)=p(t)+v(t)*dt+a(t)*dt²/2
          // error: |p-p'| =|p(t)+v(t)*dt+a(t)*dt² - (p(t)+v(t)*dt+a(t)*dt²/2)|
          //               =|a(t)*dt²/2|
          //               =|a(t)|*dt²/2 < errMax
          // dt < sqrt(errMax*2/|a(t)|)
          aMax:=0;
          for i:=0 to starCount-1 do aMax:=max(aMax,sqrEuklideanNorm(a[i]));
          dt:=sqrt(1E-3/sqrt(aMax));

          energy:=0;
          radius:=0;
          for i:=0 to starCount-1 do begin
            v[i]+=a[i]*dt;
            p[i]+=v[i]*dt;

            for j:=0 to i-1 do
              if sqrt(sqr(p[i,0]-p[j,0])+
                      sqr(p[i,1]-p[j,1])+
                      sqr(p[i,2]-p[j,2]))<0.9*(stars[i].radius+stars[j].radius)
              then begin
                qualityMeasure:=infinity;
                exit;
              end;
            energy+=sqrEuklideanNorm(v[i])*stars[i].mass;
            radius:=max(radius,sqrEuklideanNorm(p[i]));
          end;
          if energy<eMin then eMin:=energy;
          if energy>eMax then eMax:=energy;
          if radius<rMin then rMin:=radius;
          if radius>rMax then rMax:=radius;
        end;
        qualityMeasure:=max(eMax/eMin,rMax/rMin);
      end;
    end;

  begin
    result.create;
    totalMass:=0;
    posCenter:=ZERO_VECTOR;
    velCenter:=ZERO_VECTOR;
    setLength(result.stars,starCount);
    for i:=0 to length(result.stars)-1 do with result.stars[i] do begin
      color:=randomStarColor;
      radius:=0.05+sqr((random+random+random)/3); //0.05 - 1.05, expected value ~0.31
      mass:=exp(((random+random+random)*2/3-1)*ln(10)); //about log-normal distributed 0.1 - 10, most probable: 1
      p:=randomInSphere*10;
      v:=randomInSphere;

      totalMass+=mass;
      posCenter+=p*mass;
      velCenter+=v*mass;
    end;

    result.qualityMeasure:=0;

    if starCount=1
    then begin
      with result.stars[0] do begin p:=ZERO_VECTOR; v:=ZERO_VECTOR; end;
      exit(result);
    end
    else begin
      posCenter*=(1/totalMass);
      velCenter*=(1/totalMass);
      for i:=0 to length(result.stars)-1 do with result.stars[i] do begin
        p-=posCenter;
        v-=velCenter;
      end;
    end;

    evaluateSystem(result);
    if result.qualityMeasure<sys[starCount,99].qualityMeasure then begin
      sys[starCount,99]:=result;
      i:=99;
      while (i>0) and (sys[starCount,i-1].qualityMeasure>sys[starCount,i].qualityMeasure) do begin
        result:=sys[starCount,i-1];
        sys[starCount,i-1]:=sys[starCount,i];
        sys[starCount,i]:=result;
        dec(i);
      end;
    end;
    i:=0;
    while (i<99) and (random<0.91) do inc(i);
    result:=sys[starCount,i];
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

PROCEDURE TParticleEngine.MoveParticles(CONST dt: double);
  PROCEDURE accelerateStars;
    VAR i,j:longint;
        af:TVector3;
        aMax:double=0;
    begin
      for i:=0 to length(star)-1 do star[i].a:=ZERO_VECTOR;
      for i:=1 to length(star)-1 do
      for j:=0 to i-1 do begin
        af:=accelFactor(star[i].p,star[j].p);
        star[i].a+=af*star[j].mass;
        star[j].a-=af*star[i].mass;
      end;
      for i:=0 to length(star)-1 do with star[i] do begin
        v+=a*dt;
        aMax:=max(aMax,sqrEuklideanNorm(a));
      end;
      dtMax:=sqrt(1E-3/sqrt(aMax));
    end;

  PROCEDURE moveStars;
    VAR i,j:longint;
        pm,vm:TVector3;
    begin
      for i:=0 to length(star)-1 do with star[i] do begin
        p+=v*dt;

        j:=length(trajectory);
        if (j=0) or (sqrEuklideanNorm(p-trajectory[j-1])>0.01) then begin
          setLength(trajectory,j+1);
          trajectory[j]:=p;
        end;
        if {((j>0) and (sqrEuklideanNorm(trajectory[0]-p)<sqr(radius))) or} (j>10000) then begin
          for j:=0 to length(trajectory)-2 do trajectory[j]:=trajectory[j+1];
          setLength(trajectory,length(trajectory)-1);
        end;
      end;
      if starsCollide then begin
        for i:=1 to length(star)-1 do
        for j:=0 to i-1 do
        if sqrt(sqr(star[i].p[0]-star[j].p[0])+
                sqr(star[i].p[1]-star[j].p[1])+
                sqr(star[i].p[2]-star[j].p[2]))<0.8*(star[i].radius+star[j].radius)
        then begin
          pm:=star[i].p*star[i].mass+star[j].p*star[j].mass;
          vm:=star[i].v*star[i].mass+star[j].v*star[j].mass;
          star[j].mass+=star[i].mass;
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
    begin
      for i:=0 to length(dust)-1 do with dust[i] do begin
        for j:=0 to length(star)-1 do v+=accelFactor(dust[i].p,star[j].p)*(star[j].mass*dt);
        p+=v*dt;
        flaggedForRemoval:=sqr(p[0])+sqr(p[1])+sqr(p[2])>sqrDustRemovalRadius;
        for j:=0 to length(star)-1 do flaggedForRemoval:=flaggedForRemoval or
          (sqr(p[0]-star[j].p[0])+
           sqr(p[1]-star[j].p[1])+
           sqr(p[2]-star[j].p[2])<sqr(star[j].radius));
      end;
      j:=0;
      for i:=0 to length(dust)-1 do if not(dust[i].flaggedForRemoval) then begin
        if i<>j then dust[j]:=dust[i];
        inc(j);
      end;
      setLength(dust,j);
    end;

  begin
    if length(star)>1 then accelerateStars;
    moveDust;
    if length(star)>1 then moveStars;
  end;

CONSTRUCTOR TParticleEngine.create;
  begin
    randomize;
    starsCollide:=true;
    sqrDustRemovalRadius:=1E4;
    dtFactor:=0;
    cachedSystems.create;
  end;

DESTRUCTOR TParticleEngine.destroy;
  begin
    inherited destroy;
    cachedSystems.destroy;
  end;

PROCEDURE TParticleEngine.update(CONST tickDelta: qword; CONST subSteps: longint);
  VAR dt:double;
      i:longint;
  begin
    if dtFactor=0 then exit;
    dt:=tickDelta*dtFactor/subSteps;
    if dt>dtMax then dt:=dtMax;
    for i:=1 to subSteps do MoveParticles(dt);
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
    for i:=0 to length(dust)-1 do begin
      x[0]:=0.2+0.5*i/(length(dust)-1);
      glColor3f(x[0],x[0],x[0]);
      glVertex3f(dust[i].p[0],dust[i].p[1],dust[i].p[2]);
    end;
    glEnd();
  end;

PROCEDURE TParticleEngine.initDust(CONST particleCount: longint; starMask: byte; CONST mode: TDustInitMode);
  FUNCTION randomStarIndex:longint;
    begin
      repeat result:=random(length(star)) until odd(starMask shr result);
    end;

  VAR base:array of TMatrix3x3;

  FUNCTION diskParticle:TParticle;
    VAR distanceFromStar,speed:double;
        e0,e1:double;
        starIndex:longint;
    begin
      starIndex:=randomStarIndex;

      distanceFromStar:=star[starIndex].radius*1.5+sqrt(random)*3;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      e1:=random*2*pi;
      e0:=cos(e1);
      e1:=sin(e1);
      result.p:=star[starIndex].p+base[starIndex,0]*e0*distanceFromStar+base[starIndex,1]*e1*distanceFromStar;
      result.v:=star[starIndex].v+base[starIndex,0]*e1*speed           -base[starIndex,1]*e0*speed;
      result.flaggedForRemoval:=false;
    end;

  FUNCTION orbitParticle:TParticle;
    VAR starIndex:longint;
        dx,dv:TVector3;
        distanceFromStar,speed:double;
    begin
      starIndex:=randomStarIndex;
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

  FUNCTION randomParticle:TParticle;
    VAR starIndex:longint;
        dx,dv:TVector3;
        distanceFromStar,speed:double;
    begin
      starIndex:=randomStarIndex;
      distanceFromStar:=star[starIndex].radius*1.5+sqrt(random)*3;
      speed:=sqrt(star[starIndex].mass)/sqrt(distanceFromStar);
      dx:=randomOnSphere*distanceFromStar;
      dv:=randomInSphere*(2*speed);
      result.p:=star[starIndex].p+dx;
      result.v:=star[starIndex].v+dv;
      result.flaggedForRemoval:=false;
    end;

  FUNCTION randomBackgroundParticle:TParticle;
    VAR i:longint;
        inStar:boolean=true;
    begin
      repeat
        result.p:=randomInSphere*10;
        inStar:=false;
        for i:=0 to length(star)-1 do inStar:=inStar or (euklideanNorm(result.p-star[i].p)<1.5*star[i].radius);
      until not inStar;
      result.v:=ZERO_VECTOR;
      result.flaggedForRemoval:=false;
    end;

  VAR k:longint;
  begin
    if length(star) =1 then starMask:=1;
    if length(star)<=2 then starMask:=starMask and 3;
    if length(star)<=3 then starMask:=starMask and 7;
    if length(star)<=4 then starMask:=starMask and 15;
    if length(star)<=5 then starMask:=starMask and 31;
    if starMask=0 then starMask:=255;

    setLength(base,length(star));
    for k:=0 to length(base)-1 do base[k]:=randomOrthonormalBasis;

    setLength(dust,particleCount);
    case mode of
      dim_stableDisk:  for k:=0 to length(dust)-1 do dust[k]:=diskParticle;
      dim_stableOrbit: for k:=0 to length(dust)-1 do dust[k]:=orbitParticle;
      dim_randomCloud: for k:=0 to length(dust)-1 do dust[k]:=randomParticle;
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
    dtMax:=1E-3;
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

