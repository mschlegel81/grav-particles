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
    BGTabSheet: TTabSheet;
    PreciseDustCheckBox: TCheckBox;
    Label4: TLabel;
    TrailLengthLabel: TLabel;
    PageControl1: TPageControl;
    Panel6: TPanel;
    TrailLengthTrackBar: TTrackBar;
    SimulationTabSheet: TTabSheet;
    SmoothPointsCheckbox: TCheckBox;
    SmoothPointsLabel: TLabel;
    ComboBox3: TComboBox;
    initFromPresetButton: TButton;
    shrinkButton: TButton;
    growButton: TButton;
    Label3: TLabel;
    Panel5: TPanel;
    PointSizeTrackBar: TTrackBar;
    startBgCalc: TButton;
    SetRecommendedDustButton: TButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
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
    VisualsTabSheet: TTabSheet;
    PROCEDURE AutoRotateCheckboxChange(Sender: TObject);
    PROCEDURE DustTrackBarChange(Sender: TObject);
    PROCEDURE FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE growButtonClick(Sender: TObject);
    PROCEDURE IdleFunc(Sender: TObject; VAR done: boolean);
    PROCEDURE initDustButtonClick(Sender: TObject);
    PROCEDURE initFromPresetButtonClick(Sender: TObject);
    PROCEDURE PageControl1Change(Sender: TObject);
    PROCEDURE Panel3Resize(Sender: TObject);
    PROCEDURE PointSizeTrackBarChange(Sender: TObject);
    PROCEDURE PreciseDustCheckBoxChange(Sender: TObject);
    PROCEDURE resetStarsButtonClick(Sender: TObject);
    PROCEDURE SetRecommendedDustButtonClick(Sender: TObject);
    PROCEDURE shrinkButtonClick(Sender: TObject);
    PROCEDURE SmoothPointsCheckboxChange(Sender: TObject);
    PROCEDURE speedTrackBarChange(Sender: TObject);
    PROCEDURE StarsTrackBarChange(Sender: TObject);
    PROCEDURE startBgCalcClick(Sender: TObject);
    PROCEDURE stopCalcButtonClick(Sender: TObject);
    PROCEDURE TrailLengthTrackBarChange(Sender: TObject);
  private
    quitPosted:boolean;
    lastLabelUpdate:qword;
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
      i:longint;
  begin
    quitPosted:=false;
    lastLabelUpdate:=0;
    viewState.create(OpenGLControl1);
    Application.OnIdle:=@IdleFunc;
    ComboBox1.items.clear;
    for dim in TDustInitMode do ComboBox1.items.add(CDustInitModeName[dim]);
    ComboBox1.ItemIndex:=0;

    ComboBox3.items.clear;
    for tgt in TsysTarget do ComboBox3.items.add('->'+CsysTargetName[tgt]);
    for i:=0 to viewState.ParticleEngine.cachedSystems.predefinedSystemCount-1 do ComboBox3.items.add('Preset #'+intToStr(i)+' '+viewState.ParticleEngine.cachedSystems.predefinedSystemName(i));
    ComboBox3.ItemIndex:=0;
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

PROCEDURE TGravityMainForm.SmoothPointsCheckboxChange(Sender: TObject);
  begin
    viewState.smoothPoints:=SmoothPointsCheckbox.checked;
  end;

PROCEDURE TGravityMainForm.DustTrackBarChange(Sender: TObject);
  begin
    dustLabel.caption:='Dust: '+intToStr(dustCount);
  end;

PROCEDURE TGravityMainForm.FormCloseQuery(Sender: TObject; VAR CanClose: boolean);
  begin
    viewState.ParticleEngine.cachedSystems.destroying:=true;
    CanClose:=viewState.ParticleEngine.cachedSystems.backgroundRunning<=0;
    quitPosted:=true;
  end;

PROCEDURE TGravityMainForm.AutoRotateCheckboxChange(Sender: TObject);
  begin
    viewState.lockXRotation:=not(AutoRotateCheckbox.checked);
  end;

PROCEDURE TGravityMainForm.IdleFunc(Sender: TObject; VAR done: boolean);
  VAR t:qword;
  begin
    OpenGLControl1.Invalidate;
    done:=false; // tell lcl to handle messages and return immediatly

    t:=GetTickCount64;
    if t>lastLabelUpdate+200 then begin
      lastLabelUpdate:=t;
      FPS_LABEL.caption:=intToStr(round(viewState.getFps))+' fps';
      TIME_LABEL.caption:='t='+floatToStrF(viewState.ParticleEngine.totalTime,ffFixed,3,3);
      dustRemainingLabel.caption:='Remaining: '+intToStr(viewState.ParticleEngine.dustCount);
      t:=viewState.ParticleEngine.starCount;
      CheckBox5.visible:=t>=5;
      CheckBox4.visible:=t>=4;
      Checkbox3.visible:=t>=3;
      Checkbox2.visible:=t>=2;
    end;
    if quitPosted and (viewState.ParticleEngine.cachedSystems.backgroundRunning=0) then close;
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

PROCEDURE TGravityMainForm.initFromPresetButtonClick(Sender: TObject);
  VAR presetIndex:longint;
  begin
    presetIndex:=ComboBox3.ItemIndex;
    if presetIndex<0 then exit;
    if presetIndex<=byte(high(TsysTarget)) then begin
      viewState.ParticleEngine.initStars(StarsTrackBar.position,TsysTarget(presetIndex));
    end else begin
      dec(presetIndex,byte(high(TsysTarget))+1);
      if presetIndex>viewState.ParticleEngine.cachedSystems.predefinedSystemCount then exit;
      viewState.ParticleEngine.initPresetStars(presetIndex);
    end;
    growCount:=0;
    updateGrowShrink;
  end;

PROCEDURE TGravityMainForm.PageControl1Change(Sender: TObject);
  VAR i,y:longint;
      yMax:longint=0;
  begin
    for i:=0 to PageControl1.activePage.ControlCount-1 do begin
       y:=PageControl1.activePage.Controls[i].top+
          PageControl1.activePage.Controls[i].height+
          PageControl1.activePage.Controls[i].BorderSpacing.Bottom.size;
       if y>yMax then yMax:=y;
    end;
    PageControl1.ClientHeight:=yMax;
    Panel1.AdjustSize;
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

PROCEDURE TGravityMainForm.PreciseDustCheckBoxChange(Sender: TObject);
  begin
    viewState.ParticleEngine.precisionFocused:=PreciseDustCheckBox.checked;
  end;

PROCEDURE TGravityMainForm.resetStarsButtonClick(Sender: TObject);
  VAR factor:double;
  begin
    if growCount<>0 then begin
      factor:=exp(ln(2)*growCount/2);
      viewState.ParticleEngine.multiplySize(1/factor);
    end;
    viewState.ParticleEngine.resetStars;
    if growCount<>0 then viewState.ParticleEngine.multiplySize(factor);
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
  end;

PROCEDURE TGravityMainForm.startBgCalcClick(Sender: TObject);
  begin
    viewState.ParticleEngine.cachedSystems.startBackgroundCalculation;
  end;

PROCEDURE TGravityMainForm.stopCalcButtonClick(Sender: TObject);
  begin
    viewState.ParticleEngine.cachedSystems.destroying:=true;
  end;

PROCEDURE TGravityMainForm.TrailLengthTrackBarChange(Sender: TObject);
  begin
    viewState.ParticleEngine.trajectoryMaxTime:=exp(TrailLengthTrackBar.position*ln(2)/5);
    TrailLengthLabel.caption:='Trail length: '+floatToStrF(viewState.ParticleEngine.trajectoryMaxTime,ffFixed,3,3);
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

