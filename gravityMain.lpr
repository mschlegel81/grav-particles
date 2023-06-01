PROGRAM gravityMain;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}
  cthreads,
  {$endif}
  {$IFDEF HASAMIGA}
  athreads,
  {$endif}
  {$ifdef Windows}
  sysutils,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, gravParticlesMain, viewWrapper, lazopenglcontext
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=true;
  Application.title:='Gravitating Dust';
  {$ifdef Windows}
  ExecuteProcess('cmd','/C title Gravitating Dust (status)');
  {$endif}
  Application.Scaled:=true;
  Application.initialize;
  Application.CreateForm(TGravityMainForm, GravityMainForm);
  Application.run;
end.

