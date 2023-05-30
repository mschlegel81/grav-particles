PROGRAM gravityMain;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}
  cthreads,
  {$endif}
  {$IFDEF HASAMIGA}
  athreads,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, gravParticlesMain, viewWrapper, lazopenglcontext
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.Scaled:=true;
  Application.initialize;
  Application.CreateForm(TGravityMainForm, GravityMainForm);
  Application.run;
end.

