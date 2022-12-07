{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
{$include elpack2.inc}
{$ifdef ELPACK_SINGLECOMP}
{$I ElPack.inc}
{$else}
{$ifdef LINUX}
{$I ../ElPack.inc}
{$else}
{$I ..\ElPack.inc}
{$endif}
{$endif}

(*

Version History

04/08/2002

  ShowURLInHint functionality was spoiled in Unicode mode. Fixed.

03/06/2002

  Added unicode hint

12/21/2001

  Now the shortcuts can be used not only to focus the linked controls, but also
  to fire OnClick event

05/27/2001 (c) Akzhan Abdulin

    Fixed Color property design-time storing issue (clWindow not stored)

*)

unit ElURLLabel;

interface

uses
  ElHandPt,
  SysUtils,
  Classes,
  ElVCLUtils,
  {$ifndef CLX_USED}
  Windows,
  Messages,
  Controls,
  Graphics,
  ExtCtrls,
  Forms,
{$ifdef VCL_6_USED}
Types,
{$endif}
{$else}
  Qt,
  Types,
  QControls,
  QGraphics,
  QExtCtrls,
  QForms,
{$endif}

  ElCLabel, ShellAPI, Menus;

type
  TElURLLabel = class(TElCustomLabel)
  private
    FShowURLInHint : Boolean;
    FVisited : Boolean;
    FVisitedColor : TColor;
//    FPopupMenu : TPopupMenu;
    FURI : string;
    FHyperLinkColour, FOldColour : TColor; //mc
    FHyperLinkStyle, FOldStyle : TFontStyles; //mc

    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}
    procedure SetHyperLinkStyle(const Value : TFontStyles); //mc
    procedure SetHyperLinkColour(const Value : TColor); //mc
    procedure SetVisitedColor(newValue : TColor);
    procedure SetVisited(newValue : boolean);
    // vcl message handlers                                             //mc
    procedure CMMouseEnter(var Msg : TMessage); message cm_MouseEnter; //mc
    procedure CMMouseLeave(var Msg : TMessage); message cm_MouseLeave; //mc
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    { Protected declarations }
    function GetPopupMenu : TPopupMenu; override;
    procedure OnOpen(Sender : TObject);
    procedure OnCopy(Sender : TObject);

    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$else}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$endif}

  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    procedure Click; override;
    procedure GotoURL;
    procedure CopyURL;
    //destructor Destroy; override;
  published
    property URL : string read FURI write FURI;
    property VisitedColor : TColor read FVisitedColor write SetVisitedColor;
    property Visited : boolean read FVisited write SetVisited;
    property HyperLinkColor : TColor read FHyperLinkColour write SetHyperLinkColour default clBlue; //mc
    property HyperLinkStyle : TFontStyles read FHyperLinkStyle write SetHyperLinkStyle; //mc
    property ShowURLInHint : Boolean read FShowURLInHint write FShowURLInHint default True;
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}

    property Align;
    property Alignment;
{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    property DockOrientation;
    property Floating;
    property DragKind;
{$ENDIF}
    property AutoSize;
    property Caption;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property Transparent;
    {$ifdef VCL_3_USED}
    property Layout;
    {$endif}
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

uses
  ElStrUtils,
  ElShellUtils,
  ClipBrd;

(*

function Regkey (Root: HKey; const Key: string;
                    var ValRes: string): Boolean;
  var
    H: HKey;
    Buf: array [0..255] of char;
    Size: DWORD;
begin
  H := 0;
  Result := FALSE;
  Size := sizeof (Buf);

  try
    if  (RegOpenKeyEx (Root, PChar (Key), 0,
              KEY_QUERY_VALUE, H) = ERROR_SUCCESS)
    AND (RegQueryValueEx (H, nil, nil, nil, @Buf, @Size) =
                          ERROR_SUCCESS)        then
    begin
      while (Size > 0) AND (Buf [Size - 1] = #0) do dec (Size);

      SetString (ValRes, Buf, Size);
      Result := TRUE;
    end;
  finally
    if  H <> 0    then  RegCloseKey (H);
  end;
end;
*)
{-----------}

constructor TElURLLabel.Create(AOwner : TComponent);
begin
  inherited;
  Font.Style := Font.Style + [fsUnderline];
  Font.Color := clBlue;
  VisitedColor := Font.Color;
  Cursor := crURLCursor;
  AutoSize := true;
  FShowURLInHint := True;
  HyperLinkStyle := [fsUnderline];
  HyperlinkColor := clBlue;
end;

procedure TElURLLabel.Click;
begin
  inherited;
  GotoURL;
end;

procedure TElURLLabel.GotoURL;
var
  Param : AnsiString;
begin
  if Enabled then
  begin
    Param := FURI;
    FireURL(Param);
    FVisited := true;
    Font.Color := VisitedColor;
  end;
end;

procedure TElURLLabel.SetVisited(newValue : boolean);
begin
  if (FVisited <> newValue) then
  begin
    FVisited := newValue;
    Repaint;
  end; {if}
end;

procedure TElURLLabel.SetVisitedColor(newValue : TColor);
begin
  if (FVisitedColor <> newValue) then
  begin
    FVisitedColor := newValue;
    if FVisited then Font.Color := newValue;
    Repaint;
  end; {if}
end; {SetVisitedColor}
(*
destructor TElURLLabel.Destroy;
begin
  if Assigned(FPopupMenu) then FPopupMenu.Free;
  inherited;
end;
*)
procedure TElURLLabel.CopyURL;
var
  CB : TClipboard;
begin
  CB := TClipboard.Create;
  CB.Open;
  CB.AsText := URL;
  CB.Close;
  CB.Free;
end;

procedure TElURLLabel.OnOpen(Sender : TObject);
begin
  GotoURL;
end;

procedure TElURLLabel.OnCopy(Sender : TObject);
begin
  CopyURL;
end;

function TElURLLabel.GetPopupMenu : TPopupMenu;
  procedure AddMI (const Caption: string; OnClick: TNotifyEvent);
  var
    Item : TMenuItem;
  begin
    Item := TMenuItem.Create(self);
    Item.Caption := Caption;
    Item.OnClick := OnClick;
    Result.Items.Add(Item);
  end;

begin
  Result := inherited GetPopupMenu;
  if not Assigned(Result) and not (csDesigning in ComponentState) then
  begin
    Result := TPopupMenu.Create(self);
    AddMI ('&Jump to URL', OnOpen);
    AddMI ('&Copy URL', OnCopy);
  end;
end; {GetPopupMenu}

procedure TElURLLabel.CMMouseEnter(var Msg : TMessage);
begin
  inherited;
  fOldStyle := font.Style;
  fOldColour := font.color;
  font.Style := FHyperLinkStyle;
  font.color := FHyperLinkColour;
end;

procedure TElURLLabel.CMMouseLeave(var Msg : TMessage);
begin
  inherited;
  font.Style := fOldStyle;
  font.color := fOldColour;
end;

procedure TElURLLabel.SetHyperLinkStyle(const Value : TFontStyles);
begin
  FHyperLinkStyle := Value;
end;

procedure TElURLLabel.SetHyperLinkColour(const Value : TColor);
begin
  FHyperLinkColour := Value;
end;

procedure TElURLLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if Enabled and ShowAccelChar and IsAccel(Message.CharCode, Caption) then
  begin
    if (FocusControl <> nil) then
      with FocusControl do
        if CanFocus then
        begin
          SetFocus;
          Message.Result := 1;
        end
    else
      Click;
  end;
end;

{$ifdef ELPACK_UNICODE}
procedure TElURLLabel.SetHint(Value: WideString);
var S : String;
    i,
    l : integer;
    T : WideChar;
begin
  FHint := Value;

  S := FHint;
  i := Length(S);
  l := Length(S) + 1 + Length(FHint) * 2;
  SetLength(S, l + 4);

  Move(FHint[1], S[i + 2], Length(FHint) * 2);
  T := #0;
  Move(T, S[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, S[l + 3], sizeof(T));
  inherited Hint := S;
end;

{$ifndef CLX_USED}
procedure TElURLLabel.CMHintShow(var Message: TMessage);
{$else}
function TElURLLabel.HintShow(var HintInfo : THintInfo): Boolean;
{$endif}
var T: WideChar;
{$ifndef CLX_USED}
  HintInfo : PHintInfo;
{$endif}
  l : integer;
  S : String;
  WS: WideString;
begin
{$ifndef CLX_USED}
  inherited;
  HintInfo := PHintInfo(Message.lParam);
{$else}
  inherited HintShow(HintInfo);
  result := true;
{$endif}
  WS := FHint;

  if HintInfo.HintControl = Self then
  begin
    if (Hint = '') and ShowURLInHint then
      WS := Self.URL;
  end;

  if HintInfo.HintStr = GetShortHint(inherited Hint) then
  begin
    WS := GetShortHintW(WS);
    S := WS;
  end
  else
    S := WS;

  l := Length(S) + 1 + Length(WS) * 2;
  SetLength(HintInfo.HintStr, l + 4);
  Move(PChar(S)^, HintInfo.HintStr[1], Length(S) + 1);

  Move(WS[1], HintInfo.HintStr[Length(S) + 2], Length(WS) * 2);
  T := #0;
  Move(T, HintInfo.HintStr[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, HintInfo.HintStr[l + 3], sizeof(T));
end;
{$else}
procedure TElURLLabel.CMHintShow(var Message: TMessage);
var HintInfo : PHintInfo;
begin
  inherited;
  {$ifndef D_2}
  HintInfo := PHintInfo(Message.lParam);
  if HintInfo.HintControl = Self then
  begin
    if (Hint = '') and ShowURLInHint then
      HintInfo.HintStr := Self.URL;
  end;
  {$endif}
end;
{$endif}

end.
