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
  
03/06/2002

  Added unicode hint

07/26/2001

  Added Unicode support

05/27/2001 (c) Akzhan Abdulin

    Fixed Color property design-time storing issue (clWindow not stored)

*)

unit ElBtnCtl;

interface

uses
{$ifndef CLX_USED}
  Controls,
  Messages,
  Windows,
  Graphics,
{$ifdef VCL_6_USED}
Types,
{$endif}
  Forms,
{$else}
  QTypes,
  QControls,
  QGraphics,
  Qt,
  QForms,
{$endif}
  ElVCLUtils,
  ElStrUtils,
  ElXPThemedControl,
  ElTools,
  Classes
{$IFDEF VCL_4_USED}
{$ifndef CLX_USED}
  , ActnList
{$else}
  , QActnList
{$endif}  
{$ENDIF}
  ;

type
  TElButtonControl = class(TElXPThemedControl)
  private
    function IsCaptionStored: Boolean;
    {$ifndef CLX_USED}
    procedure CMDialogChar(var Message : TCMDialogChar); message CM_DIALOGCHAR;
    {$endif}
  protected
    FTransparent  : boolean;
    FTextDrawType : TElTextDrawType;
    F2000DrawFocus: boolean;
    F2000DrawAccel: boolean;
{$ifndef CLX_USED}
    procedure WMMove(var Msg : TWMMove); message WM_MOVE;
{$endif}
    procedure SetTextDrawType(newValue : TElTextDrawType);
  protected
    ClicksDisabled : boolean;
    FCaption: TElFString;
    {$ifdef ELPACK_UNICODE}
    FHint: WideString;
    {$endif}

    {$ifndef CLX_USED}
    FMoneyFlat: Boolean;
    FMoneyFlatActiveColor: TColor;
    FMoneyFlatInactiveColor: TColor;
    FMoneyFlatDownColor: TColor;
    {$endif}
    {$ifndef MSWINDOWS}
    procedure Changed; virtual;
    {$endif}
    function GetChecked : Boolean; virtual; abstract;
    procedure SetChecked(newValue : Boolean); virtual; abstract;
    {$ifndef CLX_USED}
    procedure WndProc(var Message : TMessage); override;
    {$endif}
    procedure SetTransparent(newValue : Boolean); virtual;
{$IFDEF VCL_4_USED}
    function GetActionLinkClass : TControlActionLinkClass; override;
    procedure ActionChange(Sender : TObject; CheckDefaults : Boolean); override;
{$ENDIF}
    property Checked : Boolean read GetChecked write SetChecked default false; { Protected }
    property Transparent : boolean read FTransparent write SetTransparent default false;
    property TextDrawType : TElTextDrawType read FTextDrawType write SetTextDrawType default tdtNormal; { Published }
    {$ifndef CLX_USED}
    procedure CreateParams(var Params : TCreateParams); override;
    {$endif}
    procedure SetCaption(Value: TElFString);

    function GetThemedClassName: WideString; override;
    function GetUIStateDrawFocus: Boolean;
    function GetUIStateDrawAccel: Boolean;
    procedure WMUpdateUIState(var Message: TMessage); message WM_UPDATEUISTATE;
    procedure WMChangeUIState(var Message: TMessage); message WM_CHANGEUISTATE;
    {$ifndef CLX_USED}
    procedure SetMoneyFlat(Value: Boolean);
    procedure SetMoneyFlatActiveColor(Value: TColor);
    procedure SetMoneyFlatInactiveColor(Value: TColor);
    procedure SetMoneyFlatDownColor(Value: TColor);
    function GetMoneyFlat: Boolean;
    {$endif}

    {$ifdef ELPACK_UNICODE}
    procedure SetHint(Value: WideString);

    {$ifndef CLX_USED}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    {$else}
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}
    {$endif}

    property Color nodefault;
    property Caption: TElFString read FCaption write SetCaption stored IsCaptionStored;
    {$ifndef CLX_USED}
    property MoneyFlat: Boolean read GetMoneyFlat write SetMoneyFlat default false;
    property MoneyFlatActiveColor: TColor read FMoneyFlatActiveColor write
        SetMoneyFlatActiveColor stored GetMoneyFlat;
    property MoneyFlatInactiveColor: TColor read FMoneyFlatInactiveColor write
        SetMoneyFlatInactiveColor stored GetMoneyFlat;
    property MoneyFlatDownColor: TColor read FMoneyFlatDownColor write
        SetMoneyFlatDownColor stored GetMoneyFlat;
    {$endif}
  public
    constructor Create(Owner : TComponent); override;
    property UIStateDrawFocus: Boolean read GetUIStateDrawFocus;
    property UIStateDrawAccel: Boolean read GetUIStateDrawAccel;
  published
    {$ifdef ELPACK_UNICODE}
    property Hint: WideString read FHint write SetHint;
    {$endif}
  end;

{$IFDEF VCL_4_USED}

  {$ifndef CLX_USED}
  TElButtonActionLink = class(TWinControlActionLink)
  {$else}
  TElButtonActionLink = class(TWidgetControlActionLink)
  {$endif}
  protected
    FClient : TElButtonControl;
    {$ifndef CLX_USED}
    procedure SetCaption(const Value: string); override;
    {$else}
    procedure SetCaption(const Value: TCaption); override;
    {$endif}
    procedure AssignClient(AClient : TObject); override;
    function IsCheckedLinked : Boolean; override;
    function IsImageIndexLinked : Boolean; override;
    procedure SetChecked(Value : Boolean); override;
    {$ifndef CLX_USED}
    procedure SetHint(const Value: string); override;
    {$else}
    procedure SetHint(const Value: WideString); override;
    {$endif}
  end;

{$ENDIF}
implementation

{$IFDEF VCL_4_USED}

{$ifndef CLX_USED}
procedure TElButtonActionLink.SetCaption(const Value: string);
{$else}
procedure TElButtonActionLink.SetCaption(const Value: TCaption);
{$endif}
begin
  if IsCaptionLinked then FClient.Caption := Value;
  inherited;
end;

procedure TElButtonActionLink.AssignClient(AClient : TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TElButtonControl;
end;

function TElButtonActionLink.IsCheckedLinked : Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Checked = (Action as TCustomAction).Checked);
end;

function TElButtonActionLink.IsImageIndexLinked : Boolean;
begin
  result := true;
end;

procedure TElButtonActionLink.SetChecked(Value : Boolean);
begin
  if IsCheckedLinked then
  begin
    FClient.ClicksDisabled := True;
    try
      FClient.Checked := Value;
    finally
      FClient.ClicksDisabled := False;
    end;
  end;
end;


procedure TElButtonActionLink.SetHint;
begin
  if IsHintLinked then
    FClient.Hint := Value;
  inherited;
end;


procedure TElButtonControl.ActionChange(Sender : TObject; CheckDefaults : Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Checked = False) then
        Self.Checked := Checked;
    end;
    if Sender is TAction then
    begin
      Enabled := TAction(Sender).Enabled;
      Visible := TAction(Sender).Visible;
      HelpContext := TAction(Sender).HelpContext;
      Hint := TAction(Sender).Hint;
      Caption := TAction(Sender).Caption;
    end;
  end;
end;

function TElButtonControl.GetActionLinkClass : TControlActionLinkClass;
begin
  Result := TElButtonActionLink;
end;

{$ENDIF}

{$ifndef MSWINDOWS}
procedure TElButtonControl.Changed;
begin
  // do nothing
end;
{$endif}

{$ifndef CLX_USED}
procedure TElButtonControl.WndProc(var Message : TMessage); { protected }
var
  R : TRect;
begin
  inherited;
  if (Message.Msg = ParentControlRepaintedMessage) and Transparent and (Message.lParam <> integer(Self)) then
  begin
    IntersectRect(R, PRect(Message.WParam)^, BoundsRect);
    if not IsRectEmpty(R) then
    begin
      OffsetRect(R, -Left, -Top);
      InvalidateRect(Handle, @R, true);
      Update;
    end;
    //Repaint;
  end;
end; { WndProc }
{$endif}

procedure TElButtonControl.SetTransparent(newValue : Boolean);
begin
  if (FTransparent <> newValue) then
  begin
    FTransparent := newValue;
    if FTransparent then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    {$ifndef CLX_USED}
    RecreateWnd;
    {$else}
    RecreateWidget;
    {$endif}
  end; { if }
end; { SetTransparent }

{$ifndef CLX_USED}
procedure TElButtonControl.CreateParams(var Params : TCreateParams);
begin
  inherited;
  if Transparent then
    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT
  else
    Params.ExStyle := Params.ExStyle and not WS_EX_TRANSPARENT;
end;

procedure TElButtonControl.WMMove(var Msg : TWMMove);  { private }
begin
  inherited;
  if Transparent then Repaint;
end;  { WMMove }
{$endif}

procedure TElButtonControl.SetTextDrawType(newValue : TElTextDrawType);
begin
  if (FTextDrawType <> newValue) then
  begin
    FTextDrawType := newValue;
    Repaint;
  end; { if }
end; { SetTextDrawType }

type PInteger = ^Integer;

constructor TElButtonControl.Create(Owner : TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  {$ifndef CLX_USED}
  PInteger(@Color)^ := clBtnFace;
  PInteger(@Font.Color)^ := clBtnText;
  {$endif}
  //F2000DrawFocus := (not (IsWin2000Up)) {$ifndef CLX_USED}or not (Win2KHideUIState){$endif};
  F2000DrawFocus := true;
  F2000DrawAccel := F2000DrawFocus;
end;

procedure TElButtonControl.SetCaption(Value: TElFString);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    {$ifndef CLX_USED}
    Perform(CM_TEXTCHANGED, 0, 0);
    {$else}
    TextChanged;
    {$endif}
  end;
end;

function TElButtonControl.IsCaptionStored: Boolean;
begin
{$ifdef VCL_4_USED}
  Result := (ActionLink = nil) or not TElButtonActionLink(ActionLink).IsCaptionLinked;
{$else}
  Result := true;
{$endif}
end;

{$ifndef CLX_USED}
procedure TElButtonControl.CMDialogChar(var Message : TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;
{$endif}

function TElButtonControl.GetThemedClassName: WideString;
begin
  Result := 'BUTTON';
end;

function TElButtonControl.GetUIStateDrawFocus: Boolean;
begin
  if not IsWin2000Up then
    result := true
  else
    Result := F2000DrawFocus; //(SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS = 0);
end;

function TElButtonControl.GetUIStateDrawAccel: Boolean;
begin
  if not IsWin2000Up then
    result := true
  else
    Result := F2000DrawAccel; // (SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEACCEL) = 0;
end;

procedure TElButtonControl.WMUpdateUIState(var Message: TMessage);
begin
  inherited;
  (*
  case LOWORD(Message.wParam) of
    UIS_CLEAR:
      begin
        if HIWORD(Message.wParam) and UISF_HIDEACCEL <> 0 then
        begin
          F2000DrawAccel := false;
        end;
        if HIWORD(Message.wParam) and UISF_HIDEFOCUS <> 0 then
        begin
          F2000DrawAccel := false;
        end;
        Invalidate;
      end;
    // UIS_INITIALIZE:
    UIS_SET:
      begin
        if HIWORD(Message.wParam) and UISF_HIDEACCEL <> 0 then
        begin
          F2000DrawAccel := true;
        end;
        if HIWORD(Message.wParam) and UISF_HIDEFOCUS <> 0 then
        begin
          F2000DrawAccel := true;
        end;
        Invalidate;
      end;
  end;
  *)
end;

procedure TElButtonControl.WMChangeUIState(var Message: TMessage);
begin
  inherited;
end;

{$ifndef CLX_USED}
procedure TElButtonControl.SetMoneyFlat(Value: Boolean);
begin
  if FMoneyFlat <> Value then
  begin
    FMoneyFlat := Value;
    Invalidate;
  end;
end;

procedure TElButtonControl.SetMoneyFlatActiveColor(Value: TColor);
begin
  if FMoneyFlatActiveColor <> Value then
  begin
    FMoneyFlatActiveColor := Value;
    if MoneyFlat then Invalidate;
  end;
end;

procedure TElButtonControl.SetMoneyFlatInactiveColor(Value: TColor);
begin
  if FMoneyFlatInactiveColor <> Value then
  begin
    FMoneyFlatInactiveColor := Value;
    if MoneyFlat then Invalidate;
  end;
end;

procedure TElButtonControl.SetMoneyFlatDownColor(Value: TColor);
begin
  if FMoneyFlatDownColor <> Value then
  begin
    FMoneyFlatDownColor := Value;
    if MoneyFlat then Invalidate;
  end;
end;

function TElButtonControl.GetMoneyFlat: Boolean;
begin
  Result := FMoneyFlat;
end;
{$endif}

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TElButtonControl.CMHintShow(var Message: TMessage);
{$else}
function TElButtonControl.HintShow(var HintInfo : THintInfo): Boolean;
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
  if Length(FHint) = 0 then
  begin
    HintInfo.hintStr := '';
    exit;
  end;

  S := GetShortHint(inherited Hint);
  if HintInfo.HintStr = S then
  begin
    WS := GetShortHintW(FHint);
  end
  else
  begin
    S := FHint;
    WS := FHint;
  end;

  l := Length(S) + 1 + Length(WS) * 2;
  SetLength(HintInfo.HintStr, l + 4);
  Move(PChar(S)^, HintInfo.HintStr[1], Length(S) + 1);

  Move(WS[1], HintInfo.HintStr[Length(S) + 2], Length(WS) * 2);
  T := #0;
  Move(T, HintInfo.HintStr[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, HintInfo.HintStr[l + 3], sizeof(T));
end;

procedure TElButtonControl.SetHint(Value: WideString);
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
{$endif}

end.


