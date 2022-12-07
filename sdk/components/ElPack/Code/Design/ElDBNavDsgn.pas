{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit ElDBNavDsgn;

interface

uses ElDBCtrls,
  DesignIntf, DesignEditors, DesignWindows, DsnConst
;

type
  TElDBNavigatorEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

implementation

type

  THackButton = class(TElDBNavButton);

procedure TElDBNavigatorEditor.ExecuteVerb(Index : Integer);
var
  TBar : TElDBNavigator;
begin
  if Index = 0 then
  begin
    TBar := TElDBNavigator(Component);
    Designer.CreateComponent(TElDBNavButton, TBar, 10000, 10000, TBar.BtnWidth, TBar.BtnHeight);
    (*
    TButton := TElDBNavButton(Designer.CreateComponent(TElDBVanButton, TBar, 10000, 10000, TBar.BtnWidth, TBar.BtnHeight));
    if Assigned(TButton) then
    begin
      if Index = 0 then
        TButton.ButtonType := ebtButton
      else
        TButton.ButtonType := ebtSeparator;
    end;
    *)
  end;
end;

function TElDBNavigatorEditor.GetVerb(Index : Integer): string;
begin
  if Index = 0 then
    Result := 'New B&utton';
end;

function TElDBNavigatorEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
