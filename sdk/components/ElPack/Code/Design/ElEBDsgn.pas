{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit ElEBDsgn;

interface

uses ElExpBar,
{$ifdef VCL_6_USED}
  DesignIntf, DesignEditors, DesignWindows, DsnConst
{$else}
  DsgnIntf
{$endif}
;

type
  TElExplorerBarEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

implementation

procedure TElExplorerBarEditor.ExecuteVerb(Index : Integer);
var
  Bar : TElExplorerBar;
begin
  if Index = 0 then
  begin
    Bar := TElExplorerBar(Component);
    TElExplorerBarGroup(Designer.CreateComponent(TElExplorerBarGroup, Bar, 0, 10000, Bar.Width, 100)).Parent := Bar;
  end;
end;

function TElExplorerBarEditor.GetVerb(Index : Integer): string;
begin
  if Index = 0 then
    Result := 'Add group';
end;

function TElExplorerBarEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

