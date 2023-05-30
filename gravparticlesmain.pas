UNIT gravParticlesMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, OpenGLContext, viewWrapper, particlePhysics;

TYPE

  { TGravityMainForm }

  TGravityMainForm = class(TForm)
    AutoRotateCheckbox: TCheckBox;
    resetStarsButton: TButton;
    initDustButton: TButton;
    CheckBox1: TCheckBox;
    Checkbox2: TCheckBox;
    Checkbox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ComboBox1: TComboBox;
    initStarsButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    AutoRotateLabel: TLabel;
    FPS_LABEL: TLabel;
    dustRemainingLabel: TLabel;
    Panel4: TPanel;
    starsLabel: TLabel;
    OpenGLControl1: TOpenGLControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    speedTrackBar: TTrackBar;
    dustLabel: TLabel;
    StarsTrackBar: TTrackBar;
    DustTrackBar: TTrackBar;
    PROCEDURE AutoRotateCheckboxChange(Sender: TObject);
    PROCEDURE ComboBox1Select(Sender: TObject);
    PROCEDURE DustTrackBarChange(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE IdleFunc(Sender: TObject; VAR done: boolean);
    PROCEDURE initDustButtonClick(Sender: TObject);
    PROCEDURE initStarsButtonClick(Sender: TObject);
    PROCEDURE resetStarsButtonClick(Sender: TObject);
    PROCEDURE speedTrackBarChange(Sender: TObject);
    PROCEDURE StarsTrackBarChange(Sender: TObject);
  private
    viewState:T_viewState;
    FUNCTION dustCount:longint;

  public

  end;

VAR
  GravityMainForm: TGravityMainForm;

IMPLEMENTATION

{$R *.lfm}

{ TGravityMainForm }

PROCEDURE TGravityMainForm.FormCreate(Sender: TObject);
  begin
    viewState.create(OpenGLControl1);
    Application.OnIdle:=@IdleFunc;
  end;

PROCEDURE TGravityMainForm.FormDestroy(Sender: TObject);
  begin
    viewState.destroy;
  end;

PROCEDURE TGravityMainForm.DustTrackBarChange(Sender: TObject);
  begin
    dustLabel.caption:='Dust: '+intToStr(dustCount);
  end;

PROCEDURE TGravityMainForm.ComboBox1Select(Sender: TObject);
  begin
    CheckBox1.enabled:=ComboBox1.ItemIndex<>3;
    Checkbox2.enabled:=ComboBox1.ItemIndex<>3;
    Checkbox3.enabled:=ComboBox1.ItemIndex<>3;
    CheckBox4.enabled:=ComboBox1.ItemIndex<>3;
    CheckBox5.enabled:=ComboBox1.ItemIndex<>3;
  end;

PROCEDURE TGravityMainForm.AutoRotateCheckboxChange(Sender: TObject);
  begin
    viewState.lockXRotation:=not(AutoRotateCheckbox.checked);
  end;

PROCEDURE TGravityMainForm.IdleFunc(Sender: TObject; VAR done: boolean);
  begin
    OpenGLControl1.Invalidate;
    done:=false; // tell lcl to handle messages and return immediatly
    FPS_LABEL.caption:=intToStr(round(viewState.getFps))+' fps';
    dustRemainingLabel.caption:='Remaining: '+intToStr(viewState.ParticleEngine.dustCount);
  end;

PROCEDURE TGravityMainForm.initDustButtonClick(Sender: TObject);
  VAR
    mode: TDustInitMode;
    starMask: byte;
  begin
    if ComboBox1.ItemIndex=0 then mode:=dim_stableDisk else
    if ComboBox1.ItemIndex=1 then mode:=dim_stableOrbit else
    if ComboBox1.ItemIndex=2 then mode:=dim_randomCloud else
                                  mode:=dim_stillBackgroundCloud;
    starMask:=0;
    if CheckBox1.checked then starMask+=1;
    if Checkbox2.checked then starMask+=2;
    if Checkbox3.checked then starMask+=4;
    if CheckBox4.checked then starMask+=8;
    if CheckBox5.checked then starMask+=16;
    viewState.ParticleEngine.initDust(dustCount,starMask,mode);
    viewState.resetSubSteps;
  end;

PROCEDURE TGravityMainForm.initStarsButtonClick(Sender: TObject);
  begin
    viewState.ParticleEngine.initStars(StarsTrackBar.position);
  end;

PROCEDURE TGravityMainForm.resetStarsButtonClick(Sender: TObject);
  begin
    viewState.ParticleEngine.resetStars;
  end;

PROCEDURE TGravityMainForm.speedTrackBarChange(Sender: TObject);
  begin
    if speedTrackBar.position=0
    then viewState.ParticleEngine.dtFactor:=0
    else viewState.ParticleEngine.dtFactor:=1E-6*exp(speedTrackBar.position*ln(1E6)/100);
  end;

PROCEDURE TGravityMainForm.StarsTrackBarChange(Sender: TObject);
  begin
    starsLabel.caption:='Stars: '+intToStr(StarsTrackBar.position);
    CheckBox5.visible:=StarsTrackBar.position>=5;
    CheckBox4.visible:=StarsTrackBar.position>=4;
    Checkbox3.visible:=StarsTrackBar.position>=3;
    Checkbox2.visible:=StarsTrackBar.position>=2;
  end;

FUNCTION TGravityMainForm.dustCount: longint;
  begin
    result:=round(exp(ln(1E6)*DustTrackBar.position/DustTrackBar.max));
  end;

end.

