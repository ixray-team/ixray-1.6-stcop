{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2001, EldoS                   }
{                                                    }
{====================================================}

{$I 'ElPack.inc'}
{$R 'Design\ElShellCtl.dcr'}
{$R 'Design\ElPrinters.dcr'}

unit ElProReg;

interface

uses TypInfo,
{$ifdef VCL_6_USED}
     DesignEditors, 
     DesignWindows, 
     DsnConst, 
     DesignIntf,
{$else}
     DsgnIntf,
{$endif}
     Classes,

     ElShellCtl,
     ElPrinter,
     ElTreePrinter;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('EldoS Pro',
  [TElShellTree, TElShellList, TElShellComboBox, TElPrinter, TElTreePrinter]);

end;

end.
