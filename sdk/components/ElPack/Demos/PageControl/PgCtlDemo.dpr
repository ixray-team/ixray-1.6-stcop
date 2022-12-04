program PgCtlDemo;

uses
  Forms,
  Main in 'Main.pas' {PgCtlDemoMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Page control evaluator';
  Application.CreateForm(TPgCtlDemoMainForm, PgCtlDemoMainForm);
  Application.Run;
end.

