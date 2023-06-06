UNIT viewWrapper;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,
  particlePhysics,vectors,
  GL,OpenGLContext,
  Controls;

TYPE

  { T_viewState }

  T_viewState=object
    private
      OpenGLControl: TOpenGLControl;
      AreaInitialized: boolean;
      lighting:record
        ambient,
        diffuse,
        specular,
        position: array [0..3] of GLfloat;
      end;
      geometry:record
        ParticleList: GLuint;
        pointSize:double;
      end;
      rotation:record
        rx,ry: single;
        distance:single;
        lockX:boolean;
      end;
      //Frame rate control
      frameRateControl:record
        frameCount            : integer;
        lastTicks,
        LastFrameTicks        : qword;
        TARGET_FPS            : longint;
        TARGET_TICKS_PER_FRAME: double;
        measuredFps:double;
        dustRecommendation:double;
      end;
      //Mouse handling
      mouse:record
        mouseX,
        mouseY : longint;
        isDown : (NO,leftDown,rightDown);
      end;

      PROCEDURE viewMouseDown(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE viewMouseMove(Sender: TObject; Shift: TShiftState; X,Y: integer);
      PROCEDURE viewMouseUp(Sender: TObject; button: TMouseButton; Shift: TShiftState; X, Y: integer);
      PROCEDURE viewResize(Sender: TObject);
      PROCEDURE viewPaint(Sender: TObject);

      PROCEDURE setTargetFPS(CONST value:longint);
      FUNCTION getLight1Brightness:TGLfloat;
      PROCEDURE setLight1Brightness(CONST value:TGLfloat);
      FUNCTION getLight2Brightness:TGLfloat;
      PROCEDURE setLight2Brightness(CONST value:TGLfloat);
      FUNCTION getLight3Brightness:TGLfloat;
      PROCEDURE setLight3Brightness(CONST value:TGLfloat);
    public
      //Physics time
      ParticleEngine: TParticleEngine;

      CONSTRUCTOR create(control:TOpenGLControl);

      PROPERTY targetFPS:longint read frameRateControl.TARGET_FPS write setTargetFPS;
      PROPERTY getFps:double read frameRateControl.measuredFps;
      PROPERTY light1Brightness:TGLfloat read getLight1Brightness write setLight1Brightness;
      PROPERTY light2Brightness:TGLfloat read getLight2Brightness write setLight2Brightness;
      PROPERTY light3Brightness:TGLfloat read getLight3Brightness write setLight3Brightness;
      PROPERTY pointSize:double read geometry.pointSize write geometry.pointSize;

      PROPERTY lockXRotation: boolean read rotation.lockX write rotation.lockX;
      PROPERTY dustRecommendation: double read frameRateControl.dustRecommendation;
      FUNCTION getSerialVersion:dword; virtual;

      DESTRUCTOR destroy;
  end;

IMPLEMENTATION
USES LCLProc;
CONSTRUCTOR T_viewState.create(control: TOpenGLControl);
  begin
    OpenGLControl:=control;
    OpenGLControl.DoubleBuffered:=true;
    ParticleEngine:=TParticleEngine.create;
    ParticleEngine.initStars(2,none);
    ParticleEngine.initDust(0,255,dim_stableDisk);
    ParticleEngine.dtFactor:=0;

    geometry.pointSize:=1;
    rotation.distance:=20;

    with mouse do begin
      mouseX:=0;
      mouseY:=0;
      isDown:=NO;
    end;

    OpenGLControl.OnMouseDown:=@viewMouseDown;
    OpenGLControl.OnMouseMove:=@viewMouseMove;
    OpenGLControl.OnMouseUp  :=@viewMouseUp;
    OpenGLControl.OnResize   :=@viewResize;
    OpenGLControl.OnPaint    :=@viewPaint;

    lockXRotation:=true;

    with lighting do begin
      {ambient color}
      ambient[0]:=0.9;
      ambient[1]:=0.9;
      ambient[2]:=0.9;
      ambient[3]:=0.0;
      {diffuse color}
      diffuse[0]:=0.1;
      diffuse[1]:=0.1;
      diffuse[2]:=0.1;
      diffuse[3]:=0.0;
      {diffuse color}
      specular[0]:=0;
      specular[1]:=0;
      specular[2]:=0;
      specular[3]:=0.0;
    end;

    setTargetFPS(40);
  end;

FUNCTION T_viewState.getSerialVersion: dword;
  begin
    result:=5;
  end;

DESTRUCTOR T_viewState.destroy;
  begin
    ParticleEngine.destroy;
  end;

PROCEDURE T_viewState.viewMouseDown(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    with mouse do begin
      mouseX:=x;
      mouseY:=y;
      case button of
        mbLeft : isDown:=leftDown;
        mbRight: isDown:=rightDown;
      end;
    end;
  end;

PROCEDURE T_viewState.viewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: integer);
  VAR delta: double;
  begin
    with mouse do if isDown=leftDown then begin
      rotation.ry+=(x-mouseX)/OpenGLControl.width*180;
      rotation.rx+=(y-mouseY)/OpenGLControl.height*180;
      mouseX:=x;
      mouseY:=y;
    end else if isDown=rightDown then begin
      delta:=exp((y-mouseY)/OpenGLControl.height);
      rotation.distance*=delta;
      mouseX:=x;
      mouseY:=y;
    end;
  end;

PROCEDURE T_viewState.viewMouseUp(Sender: TObject; button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
  begin
    mouse.isDown:=NO;
  end;

PROCEDURE T_viewState.viewResize(Sender: TObject);
  begin
    if (AreaInitialized) and OpenGLControl.MakeCurrent then
      glViewport (0, 0, OpenGLControl.width, OpenGLControl.height);
  end;

PROCEDURE T_viewState.viewPaint(Sender: TObject);
  PROCEDURE initializeArea;
    PROCEDURE addFace(CONST p0,p1,p2:TVector3; CONST refine:byte);
      CONST FINER_NODE:array[0..3,0..2] of byte = ((0,3,5),(3,1,4),(3,4,5),(5,4,2));
      //      2
      //     / \
      //    /   \
      //   5-----4
      //  / \   / \
      // /   \ /   \
      //0-----3-----1
      VAR p:array[0..5] of TVector3;
          n,commonNormal:TVector3;
          j:longint;
      begin
        p[0]:=p0*(1/euklideanNorm(p0));
        p[1]:=p1*(1/euklideanNorm(p1));
        p[2]:=p2*(1/euklideanNorm(p2));
        if refine=0 then begin
          commonNormal:=p[0]+p[1]+p[2];
          commonNormal*=1/euklideanNorm(commonNormal);
          for j:=0 to 2 do begin
            n:=p[j];
            n*=1/euklideanNorm(n);
            glNormal3f(n[0],n[1],n[2]);
            glVertex3f(n[0],n[1],n[2]);
          end;
        end else begin
          p[3]:=p[0]+p[1];
          p[4]:=p[1]+p[2];
          p[5]:=p[2]+p[0];
          for j:=0 to 3 do addFace(p[FINER_NODE[j,0]],p[FINER_NODE[j,1]],p[FINER_NODE[j,2]],refine-1);
        end;
      end;

    CONST C_IcosahedronNodes:array[0..11] of TVector3=(
       ( 0, 8.50650808352040E-001, 5.25731112119134E-001),
       ( 5.25731112119134E-001, 0, 8.50650808352040E-001),
       ( 8.50650808352040E-001, 5.25731112119134E-001, 0),
       (-5.25731112119134E-001, 0, 8.50650808352040E-001),
       (-8.50650808352040E-001, 5.25731112119134E-001, 0),
       ( 0,-8.50650808352040E-001, 5.25731112119134E-001),
       ( 8.50650808352040E-001,-5.25731112119134E-001, 0),
       (-8.50650808352040E-001,-5.25731112119134E-001, 0),
       ( 0, 8.50650808352040E-001,-5.25731112119134E-001),
       ( 5.25731112119134E-001, 0,-8.50650808352040E-001),
       (-5.25731112119134E-001, 0,-8.50650808352040E-001),
       ( 0,-8.50650808352040E-001,-5.25731112119134E-001));

    CONST C_icosahedronFaces:array[0..19,0..2] of byte=
      ((1,2,0),(0, 4,3),( 5, 6,1),(3, 7, 5),
       (2,9,8),(8,10,4),(11, 9,6),(7,10,11),
       (0,3,1),(1, 3,5),( 9,10,8),(11,10,9),
       (2,8,0),(0, 8,4),( 5,11,6),( 7,11,5),
       (1,6,2),(4, 7,3),( 2, 6,9),(10, 7,4));

    VAR n:TVector3;
        i:longint;
    begin
      frameRateControl.LastFrameTicks:=0;
      frameRateControl.dustRecommendation:=100000;
      {diffuse position}
      n:=vectorOf(1,2,0); n*=1/euklideanNorm(n);
      with lighting do begin
        position[0]:=n[0];
        position[1]:=n[1];
        position[2]:=n[2];
        position[3]:=0.0;
        glLightfv(GL_LIGHT0,GL_AMBIENT ,ambient);
        glLightfv(GL_LIGHT0,GL_DIFFUSE ,diffuse);
        glLightfv(GL_LIGHT0,GL_SPECULAR,specular);
        glEnable (GL_LIGHT0);

        glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
        glEnable( GL_BLEND );
        glEnable(GL_COLOR_MATERIAL);
        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        glClearColor(0.0,0.0,0.0,1.0);
      end;

      glClearDepth(1.0);
      glDepthFunc(GL_LEQUAL);           // the type of depth test to do
      glEnable(GL_DEPTH_TEST);          // enables depth testing
      glShadeModel(GL_SMOOTH);
      glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

      with geometry do begin
        ParticleList:=glGenLists(1);
        glColor3f(1,0.5,0);
        glNewList(ParticleList, GL_COMPILE);
          glBegin(GL_TRIANGLES);
            for i:=0 to 19 do
              addFace(C_IcosahedronNodes[C_icosahedronFaces[i,0]],
                      C_IcosahedronNodes[C_icosahedronFaces[i,1]],
                      C_IcosahedronNodes[C_icosahedronFaces[i,2]],
                      3);
           glEnd;
        glEndList;
        glEnable(GL_LIGHTING);
      end;

      glMatrixMode (GL_PROJECTION);
      glLoadIdentity ();
      glFrustum (-0.1, 0.1, -0.1, 0.1, 0.35, 10000.0);
      glMatrixMode (GL_MODELVIEW);
      glViewport (0, 0, OpenGLControl.width, OpenGLControl.height);
      AreaInitialized:=true;
    end;

  CONST az=1;
  VAR ax:double=az;
      ay:double=az;
      tickDelta: qword;
      currTicks: qword;
  begin
    with frameRateControl do begin
      currTicks:=GetTickCount64;
      tickDelta:=currTicks-lastTicks;
      lastTicks:=currTicks;
      //Frame rate counting:
      inc(frameCount);
      LastFrameTicks+=tickDelta;
      //Dust recommendation
      if ParticleEngine.dtFactor=0
      then sleep(5)
      else if (tickDelta>1) and (ParticleEngine.dustCount>0)
      then dustRecommendation:=dustRecommendation*0.9+0.1*(ParticleEngine.dustCount/tickDelta*TARGET_TICKS_PER_FRAME);

      if (LastFrameTicks>=1000) then begin
        measuredFps:=1E3*frameCount/LastFrameTicks;
        LastFrameTicks-=1000;
        frameCount:=0;
      end;
      ParticleEngine.update(tickDelta,measuredFps<TARGET_FPS);
    end;

    if OpenGLControl.MakeCurrent then begin
      if not AreaInitialized then initializeArea;

      //Update rotation angles
      if (mouse.isDown<>leftDown) then with rotation do begin
        if not(lockX) then begin
          rx-=rx*0.01*tickDelta;
          ry+=   0.01*tickDelta;
        end;

        if ry> 180 then ry-=360;
        if ry<-180 then ry+=360;
        if rx> 180 then rx-=360;
        if rx<-180 then rx+=360;
      end;

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      glLoadIdentity;
      glPushMatrix;
      //Scale to preserve aspect ratio
      if OpenGLControl.width>OpenGLControl.height
      then ax*=OpenGLControl.height/OpenGLControl.width
      else ay*=OpenGLControl.width/OpenGLControl.height;
      glTranslatef(0,0,-rotation.distance);
      glScalef(ax,ay,az);
      //Rotate
      glRotatef(rotation.rx,1.0,0.0,0.0);
      glRotatef(rotation.ry,0.0,1.0,0.0);
      //Draw
      glLightfv(GL_LIGHT0,GL_POSITION,lighting.position);
      glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
      ParticleEngine.DrawParticles(geometry.ParticleList,geometry.pointSize);
      glPopMatrix;

      OpenGLControl.SwapBuffers;
    end;
  end;

PROCEDURE T_viewState.setTargetFPS(CONST value: longint);
  begin
    if value>=1 then with frameRateControl do begin
      TARGET_FPS:=value;
      if TARGET_FPS>100
      then TARGET_TICKS_PER_FRAME:=0
      else TARGET_TICKS_PER_FRAME:=1000/value;
    end;
  end;

FUNCTION T_viewState.getLight1Brightness: TGLfloat;
  begin
    result:=lighting.ambient[0];
  end;

PROCEDURE T_viewState.setLight1Brightness(CONST value: TGLfloat);
  begin
    with lighting do begin
      ambient[0]:=value;
      ambient[1]:=value;
      ambient[2]:=value;
    end;
    AreaInitialized:=false;
  end;

FUNCTION T_viewState.getLight2Brightness: TGLfloat;
  begin
    result:=lighting.diffuse[0];
  end;

PROCEDURE T_viewState.setLight2Brightness(CONST value: TGLfloat);
  begin
    with lighting do begin
      diffuse[0]:=value;
      diffuse[1]:=value;
      diffuse[2]:=value;
    end;
    AreaInitialized:=false;
  end;

FUNCTION T_viewState.getLight3Brightness: TGLfloat;
  begin
    result:=lighting.specular[0];
  end;

PROCEDURE T_viewState.setLight3Brightness(CONST value: TGLfloat);
  begin
    with lighting do begin
      specular[0]:=value;
      specular[1]:=value;
      specular[2]:=value;
    end;
    AreaInitialized:=false;
  end;

end.

