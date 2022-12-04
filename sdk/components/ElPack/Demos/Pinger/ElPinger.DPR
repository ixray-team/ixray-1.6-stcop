program ElPinger;

uses
  Forms,
  Windows,
  MAIN in 'MAIN.PAS' {MainForm},
  SiteMan in 'SiteMan.pas',
  PingThread in 'PingThread.pas',
  frmList in 'frmList.pas' {SitesForm},
  frmProp in 'frmProp.pas' {PropForm},
  About in 'About.pas' {AboutBox},
  icmp in 'ICMP.pas';

{$R *.RES}

begin
  FreeLibrary(GetModuleHandle('OleAut32'));
  Application.Title := 'EldoS Pinger';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSitesForm, SitesForm);
  Application.CreateForm(TPropForm, PropForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
