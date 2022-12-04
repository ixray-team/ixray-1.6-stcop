program ElIniEdit;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Classes,
  SysUtils,
  ElStrUtils,
  IniStrings in 'IniStrings.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'EldoS IniEditor';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

