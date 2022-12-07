program MlGenDemo;

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  ElMlGen in '..\ElMlGen.pas',
  clMlDemo in 'clMlDemo.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MlGen Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
