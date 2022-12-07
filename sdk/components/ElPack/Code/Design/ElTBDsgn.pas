
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit ElTBDsgn;

interface

uses ElToolBar,
  DesignEditors, DesignWindows, DesignIntf, DsnConst
;

type
  TElToolBarEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

implementation

procedure TElToolBarEditor.ExecuteVerb(Index : Integer);
var
  TBar : TElToolBar;
  TButton : TElToolButton;
begin
  case Index of
    0, 1 :
      begin
        TBar := TElToolBar(Component);
        TButton := TElToolButton(Designer.CreateComponent(TElToolButton, TBar, 10000, 10000, TBar.BtnWidth, TBar.BtnHeight));
        if Assigned(TButton) then
        begin
          if Index = 0 then
            TButton.ButtonType := ebtButton
          else
            TButton.ButtonType := ebtSeparator;
        end;
      end;
  end;
end;

function TElToolBarEditor.GetVerb(Index : Integer) : string;
begin
  case Index of
    0 : Result := 'New B&utton';
    1 : Result := 'New Se&parator';
  end;
end;

function TElToolBarEditor.GetVerbCount : Integer;
begin
  Result := 2;
end;

end.

