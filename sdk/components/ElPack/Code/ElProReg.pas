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
     DesignEditors, 
     DesignWindows, 
     DsnConst, 
     DesignIntf,
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
