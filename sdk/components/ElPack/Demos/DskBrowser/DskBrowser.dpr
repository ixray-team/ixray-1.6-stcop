program DskBrowser;
{$RANGECHECKS ON}

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  frmSeach in 'frmSeach.pas' {SearchForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.Run;
end.

