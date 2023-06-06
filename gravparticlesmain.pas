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
    ComboBox2: TComboBox;
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
    growCount:longint;
    viewState:T_viewState;
    FUNCTION dustCount:longint;
    PROCEDURE updateGrowShrink;

  public

  end;

VAR
  GravityMainForm: TGravityMainForm;

IMPLEMENTATION

{$R *.lfm}

{ TGravityMainForm }

PROCEDURE TGravityMainForm.FormCreate(Sender: TObject);
  VAR dim:TDustInitMode;
      tgt:TsysTarget;
  begin
    viewState.create(OpenGLControl1);
    Application.OnIdle:=@IdleFunc;
    ComboBox1.items.clear;
    for dim in TDustInitMode do ComboBox1.items.add(CDustInitModeName[dim]);
    ComboBox1.ItemIndex:=0;

    ComboBox2.items.clear;
    for tgt in TsysTarget do ComboBox2.items.add('Target: '+CsysTargetName[tgt]);
    ComboBox2.ItemIndex:=0;
    growCount:=0;
  end;

PROCEDURE TGravityMainForm.FormDestroy(Sender: TObject);
  begin
    viewState.destroy;
  end;

PROCEDURE TGravityMainForm.growButtonClick(Sender: TObject);
  begin
    viewState.ParticleEngine.multiplySize(sqrt(2));
    inc(growCount);
    updateGrowShrink;
  end;

PROCEDURE TGravityMainForm.shrinkButtonClick(Sender: TObject);
  begin
    viewState.ParticleEngine.multiplySize(sqrt(0.5));
    dec(growCount);
    updateGrowShrink;
  end;

PROCEDURE TGravityMainForm.DustTrackBarChange(Sender: TObject);
  begin
    dustLabel.caption:='Dust: '+intToStr(dustCount);
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
    d,mode: TDustInitMode;
    starMask: byte;

  begin
    mode:=low(TDustInitMode);
    for d in TDustInitMode do if byte(d)=ComboBox1.ItemIndex then mode:=d;
    starMask:=0;
    if CheckBox1.checked then starMask+=1;
    if Checkbox2.checked then starMask+=2;
    if Checkbox3.checked then starMask+=4;
    if CheckBox4.checked then starMask+=8;
    if CheckBox5.checked then starMask+=16;
    viewState.ParticleEngine.initDust(dustCount,starMask,mode);
  end;

PROCEDURE TGravityMainForm.initStarsButtonClick(Sender: TObject);
  VAR t,tgt:TsysTarget;
  begin
    tgt:=low(TsysTarget);
    for t in TsysTarget do if byte(t)=ComboBox2.ItemIndex then tgt:=t;
    viewState.ParticleEngine.initStars(StarsTrackBar.position,tgt);
    growCount:=0;
    updateGrowShrink;
  end;

PROCEDURE TGravityMainForm.Panel3Resize(Sender: TObject);
  VAR w:longint;
  begin
    w:=(Panel3.ClientWidth-12) div 2;
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
    growCount:=0;
    updateGrowShrink;
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

CONST DUST_COUNT_TAB:array[0..80] of longint=(0,100          ,125   ,141   ,158   ,178   ,200   ,224   ,250   ,282   ,316   ,355   ,400   ,447   ,500   ,562   ,631   ,700   ,800   ,900   ,
                                                1000  ,1122  ,1250  ,1413  ,1585  ,1778  ,2000  ,2239  ,2500  ,2818  ,3162  ,3548  ,4000  ,4467  ,5000  ,5623  ,6310  ,7000  ,8000  ,9000  ,
                                                10000 ,11220 ,12500 ,14125 ,15849 ,17783 ,20000 ,22387 ,25000 ,28184 ,31623 ,35481 ,40000 ,44668 ,50000 ,56234 ,63096 ,70000 ,80000 ,90000 ,
                                                100000,112202,125000,141254,158489,177828,200000,223872,250000,281838,316228,354813,400000,446684,500000,562341,630957,700000,800000,900000,
                                                1000000);

FUNCTION TGravityMainForm.dustCount: longint;
  begin
    result:=DUST_COUNT_TAB[DustTrackBar.position];
  end;

PROCEDURE TGravityMainForm.updateGrowShrink;
  begin
    if growCount<0 then shrinkButton.caption:='shrink ('+intToStr(-growCount)+')' else shrinkButton.caption:='shrink';
    if growCount>0 then growButton  .caption:='grow ('+intToStr(growCount)+')' else growButton.caption:='grow';
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

