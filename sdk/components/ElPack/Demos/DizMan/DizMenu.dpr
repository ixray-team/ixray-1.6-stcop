library DizMenu;

uses
  SysUtils,
  Windows,
  Registry,
  ActiveX,
  ComServ,
  CtxMenu in 'CtxMenu.pas',
  DizIni in 'DizIni.pas';

{$R *.res}

exports
  DllGetClassObject,    
  DllCanUnloadNow,
  DllRegServer name 'DllRegisterServer',
  DllUnregServer name 'DllUnregisterServer';

begin
end.

