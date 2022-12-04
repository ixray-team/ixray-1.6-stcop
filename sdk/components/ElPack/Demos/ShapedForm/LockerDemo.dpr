program LockerDemo;

uses
  ElUxTheme,
  Forms,
  LockerForm in 'LockerForm.pas' {frmLocker};

{$R *.RES}

begin
  ThemesAvailable := false;
  Application.Initialize;
  Application.CreateForm(TfrmLocker, frmLocker);
  Application.Run;
end.

