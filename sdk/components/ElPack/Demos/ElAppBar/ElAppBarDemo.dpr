program ElAppBarDemo;

uses
  Forms,
  frmMain in 'frmMain.pas' {frmBar},
  frmOpts in 'frmOpts.pas' {OptionsForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBar, frmBar);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.

