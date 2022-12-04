program OptionsSample;

uses
  Forms,
  OptionsMain in 'OptionsMain.pas' {OptionsForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.

