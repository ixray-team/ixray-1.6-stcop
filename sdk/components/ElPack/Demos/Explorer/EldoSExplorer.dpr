program EldoSExplorer;

uses
  Forms,
  ExplorerMain in 'ExplorerMain.pas' {ExplorerForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TExplorerForm, ExplorerForm);
  Application.Run;
end.

