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
    shrinkButton: TButton;
    growButton: TButton;
    Label3: TLabel;
    Panel5: TPanel;
    PointSizeTrackBar: TTrackBar;
    startBgCalc: TButton;
    SetRecommendedDustButton: TButton;
    TIME_LABEL: TLabel;
    stopCalcButton: TButton;
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
    PROCEDURE growButtonClick(Sender: TObject);
    PROCEDURE IdleFunc(Sender: TObject; VAR done: boolean);
    PROCEDURE initDustButtonClick(Sender: TObject);
    PROCEDURE initStarsButtonClick(Sender: TObject);
    PROCEDURE Panel3Resize(Sender: TObject);
    PROCEDURE PointSizeTrackBarChange(Sender: TObject);
    PROCEDURE resetStarsButtonClick(Sender: TObject);
    PROCEDURE SetRecommendedDustButtonClick(Sender: TObject);
    PROCEDURE shrinkButtonClick(Sender: TObject);
    PROCEDURE speedTrackBarChange(Sender: TObject);
    PROCEDURE StarsTrackBarChange(Sender: TObject);
    PROCEDURE startBgCalcClick(Sender: TObject);
    PROCEDURE stopCalcButtonClick(Sender: TObject);
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

PROCEDURE TGravityMainForm.growButtonClick(Sender: TObject);
  begin
    viewState.ParticleEngine.multiplySize(sqrt(2));
  end;

PROCEDURE TGravityMainForm.shrinkButtonClick(Sender: TObject);
  begin
    viewState.ParticleEngine.multiplySize(sqrt(0.5));
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
    TIME_LABEL.caption:='t='+floatToStrF(viewState.ParticleEngine.totalTime,ffFixed,3,3);
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
    if ComboBox1.ItemIndex=4 then mode:=dim_singleRing else
    if ComboBox1.ItemIndex=5 then mode:=dim_threeRings else
    if ComboBox1.ItemIndex=6 then mode:=dim_shell else
    if ComboBox1.ItemIndex=7 then mode:=dim_planetLike else
    if ComboBox1.ItemIndex=8 then mode:=dim_lonelyCloud else
                                  mode:=dim_stillBackgroundCloud;
    starMask:=0;
    if CheckBox1.checked then starMask+=1;
    if Checkbox2.checked then starMask+=2;
    if Checkbox3.checked then starMask+=4;
    if CheckBox4.checked then starMask+=8;
    if CheckBox5.checked then starMask+=16;
    viewState.ParticleEngine.initDust(dustCount,starMask,mode);
  end;

PROCEDURE TGravityMainForm.initStarsButtonClick(Sender: TObject);
  begin
    viewState.ParticleEngine.initStars(StarsTrackBar.position);
  end;

PROCEDURE TGravityMainForm.Panel3Resize(Sender: TObject);
  VAR w:longint;
  begin
    w:=(Panel3.width-15) div 2;
    resetStarsButton.width:=w;
    initStarsButton.width:=w;
    growButton.width:=w;
    shrinkButton.width:=w;
  end;

PROCEDURE TGravityMainForm.PointSizeTrackBarChange(Sender: TObject);
  begin
    viewState.pointSize:=PointSizeTrackBar.position*0.1;
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

PROCEDURE TGravityMainForm.startBgCalcClick(Sender: TObject);
  begin
    viewState.ParticleEngine.cachedSystems.startBackgroundCalculation;
  end;

PROCEDURE TGravityMainForm.stopCalcButtonClick(Sender: TObject);
  begin
    viewState.ParticleEngine.cachedSystems.destroying:=true;
  end;

CONST DUST_COUNT_TAB:array[0..80] of longint=(0,100,126,141,158,178,200,224,251,282,316,355,398,447,501,562,631,708,794,891,1000,1122,1259,1413,1585,1778,1995,2239,2512,2818,3162,3548,3981,4467,5012,5623,6310,7079,7943,8913,10000,11220,12589,14125,15849,17783,19953,22387,25119,28184,31623,35481,39811,44668,50119,56234,63096,70795,79433,89125,100000,112202,125893,141254,158489,177828,199526,223872,251189,281838,316228,354813,398107,446684,501187,562341,630957,707946,794328,891251,1000000);

FUNCTION TGravityMainForm.dustCount: longint;
  begin
    result:=DUST_COUNT_TAB[DustTrackBar.position];
  end;

PROCEDURE TGravityMainForm.SetRecommendedDustButtonClick(Sender: TObject);
  VAR recommended:double;
      i:longint;
  begin
    recommended:=viewState.dustRecommendation;
    i:=80;
    while (i>0) and (DUST_COUNT_TAB[i]>recommended) do dec(i);
    DustTrackBar.position:=i;
  end;

end.

