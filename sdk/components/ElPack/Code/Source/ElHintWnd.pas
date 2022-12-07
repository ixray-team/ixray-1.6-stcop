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

07/11/2002

  Fixed the problem with charset not being set for HTML rendering engine

*)

unit ElHintWnd;  { TElHintWindow component. }

interface

uses
{$ifndef CLX_USED}
{$ifdef VCL_6_USED}
  Types,
{$endif}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
{$else}
  QControls,
  QTypes,
  Types,
  Qt,
  QGraphics,
  QForms,
{$endif}
  SysUtils,
  Classes,
{$IFDEF HAS_HTML_RENDER}
  HTMLRender,
{$ENDIF}
  ElVCLUtils,
  ElStrUtils
  ;

type
  TElHintWindow = class(THintWindow)
  protected
    FFont : TFont;
    FActivating : boolean;
    {$ifdef ELPACK_UNICODE}
    FWideCaption : WideString;
    {$endif}
{$IFDEF HAS_HTML_RENDER}
    FRender : TElHTMLRender;
    FIsHTML : boolean;
    FOnLinkClick: TElHTMLLinkClickEvent;
    FOnImageNeeded : TElHTMLImageNeededEvent;
{$ENDIF}
    procedure SetFont(newValue : TFont);
  protected
    FWordWrap: Boolean;
{$ifdef HAS_HTML_RENDER}
    procedure TriggerLinkClickEvent(HRef : TElFString); virtual;
    procedure TriggerImageNeededEvent(Sender: TObject; Src : TElFString; var Image : TBitmap); virtual;
{$endif}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    {$ifdef CLX_USED}
    function WidgetFlags: Integer; override;
    procedure TextChanged; override;
    {$else}
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    {$endif}
    procedure SetWordWrap(Value: Boolean);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

{$ifdef CLX_USED}
    function CalcHintRect(MaxWidth: Integer; const AHint: WideString; AData: Pointer): TRect; override;
{$else}
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
{$endif}
    {$ifdef ELPACK_UNICODE}
    function CalcHintRectW(MaxWidth: Integer; const AHint: WideString; AData: Pointer): TRect;
    procedure ActivateHintW(Rect: TRect; const AHint: WideString); virtual;
    {$endif}
    property Canvas;
  published
    property Font : TFont read FFont write SetFont;  { Published }
    property WordWrap: Boolean read FWordWrap write SetWordWrap default false;
    {$ifdef ELPACK_UNICODE}
    property WideCaption : WideString read FWideCaption write FWideCaption;
    {$endif}
{$IFDEF HAS_HTML_RENDER}
    property IsHTML : boolean read FIsHTML write FIsHTML;
    property OnImageNeeded : TElHTMLImageNeededEvent read FOnImageNeeded write FOnImageNeeded;
    property OnLinkClick: TElHTMLLinkClickEvent read FOnLinkClick write
        FOnLinkClick;
{$ENDIF}
  end;  { TElHintWindow }

{$ifdef ELPACK_UNICODE}
function GetUnicodeHint(Hint : string) : WideString; 
{$endif}

implementation

const WrapFlags : array[boolean] of Integer
      {$ifndef CLX_USED}
      = (0, DT_WORDBREAK);
      {$else}
      = (0, Integer(AlignmentFlags_WordBreak));
      {$endif}

{$ifdef ELPACK_UNICODE}
function GetUnicodeHint(Hint : string) : WideString;
var i : integer;
begin
  result := Hint;
  if (Length(Hint) > 4) then
  begin
    if Copy(Hint, Length(Hint) - 3, 4) = #00#00#$FE#$FF then
    begin
      i := StrLen(PChar(Hint)) + 1;
      SetLength(result, (Length(Hint) - i - 4) div 2);
      Move(Hint[i + 1], Result[1], Length(Result) * 2);
    end;
  end;
end;
{$endif}

{$ifdef HAS_HTML_RENDER}
procedure TElHintWindow.TriggerImageNeededEvent(Sender: TObject; Src : TElFString; var Image : TBitmap);
begin
  if assigned (FOnImageNeeded) then
    FOnImageNeeded(Self, Src, Image);
end;
{$endif}

{$ifdef ELPACK_UNICODE}
function TElHintWindow.CalcHintRectW(MaxWidth: Integer; const AHint: WideString; AData: Pointer): TRect;
{$ifdef CLX_USED}
var R2 : TRect;
{$endif}
begin
{$IFDEF HAS_HTML_RENDER}
  if IsHTML then
  begin
    FRender.Data.DefaultColor := Font.Color;
    FRender.Data.DefaultBgColor := Color;
    FRender.Data.Charset := Font.Charset;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultFont := Font.Name;
    FRender.PrepareText(AHint, 0, false);
    Result := Rect(0, 0, FRender.Data.TextSize.cx, FRender.Data.TextSize.cy + 2);
    Inc(Result.Right, 6);
    {$ifndef CLX_USED}
    Inc(Result.Bottom, 2);
    {$else}
    Inc(Result.Bottom, 2);
    Dec(Result.Top, 2);
    Inc(Result.Right, 2);
    {$endif}
  end
  else
{$ENDIF}
  begin
    Canvas.Font.Assign(Font);
    Result := Rect(0, 0, MaxWidth, 0);
    {$ifndef CLX_USED}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(AHint), -1, Result,
              DT_CALCRECT or DT_LEFT or WrapFlags[WordWrap] or DT_NOPREFIX
              {$ifdef VCL_4_USED}
              or DrawTextBiDiModeFlagsReadingOnly
              {$endif}
              );
    {$else}
    R2.Left := 0; R2.Right := MaxInt;
    R2.Top  := 0; R2.Bottom := MaxInt;
    SetRectEmpty(Result);
    Canvas.Start;
    QPainter_boundingRect(Canvas.Handle,
                          @Result,
                          @R2,
                          Integer(AlignmentFlags_AlignLeft) or
                          Integer(AlignmentFlags_AlignTop),
                          PWideString(@AHint),
                          Length(AHint),
                          nil);
    Canvas.Stop;
    {$endif}
    {$ifndef CLX_USED}
    Inc(Result.Right, 6);
    Inc(Result.Bottom, 2);
    {$else}
    Inc(Result.Bottom, 2);
    Dec(Result.Top, 2);
    Inc(Result.Right, 6);
    {$endif}
  end;
end;

procedure TElHintWindow.ActivateHintW(Rect: TRect; const AHint: WideString);
begin
  FActivating := true;
  FWideCaption := AHint;

  Inc(Rect.Bottom, 4);

  {$ifndef CLX_USED}
  UpdateBoundsRect(Rect);
  {$endif}

  {$ifndef CLX_USED}
  {$ifdef VCL_4_USED}
  if Rect.Top + Height > Screen.DesktopHeight then
    Rect.Top := Screen.DesktopHeight - Height;
  if Rect.Left + Width > Screen.DesktopWidth then
    Rect.Left := Screen.DesktopWidth - Width;
  if Rect.Left < Screen.DesktopLeft then
    Rect.Left := Screen.DesktopLeft;
  if Rect.Bottom < Screen.DesktopTop then
    Rect.Bottom := Screen.DesktopTop;
  {$else}
  if Rect.Top + Height > Screen.Height then
    Rect.Top := Screen.Height - Height;
  if Rect.Left + Width > Screen.Width then
    Rect.Left := Screen.Width - Width;
  if Rect.Left < 0 then Rect.Left := 0;
  if Rect.Bottom < 0 then Rect.Bottom := 0;
  {$endif}
  {$else}
  if Rect.Top + Height > Screen.Height then
    Rect.Top := Screen.Height - Height;
  if Rect.Left + Width > Screen.Width then
    Rect.Left := Screen.Width - Width;
  if Rect.Left < 0 then Rect.Left := 0;
  if Rect.Bottom < 0 then Rect.Bottom := 0;
  {$endif}
  {$ifndef CLX_USED}
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
    SWP_SHOWWINDOW or SWP_NOACTIVATE);
  {$else}
  SetBounds(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);

  QWidget_show(Handle);
  QWidget_raise(Handle);
  {$endif}
  FActivating := false;
  Invalidate;
end;
{$endif ELPACK_UNICODE}

function TElHintWindow.CalcHintRect;
begin
{$IFDEF HAS_HTML_RENDER}
  if IsHTML then
  begin
    FRender.Data.DefaultColor := Font.Color;
    FRender.Data.DefaultBgColor := Color;
    FRender.Data.Charset := Font.Charset;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultFont := Font.Name;
    FRender.PrepareText(AHint, 0, false);
    Result := Rect(0, 0, FRender.Data.TextSize.cx, FRender.Data.TextSize.cy + 2);
    Inc(Result.Right, 6);
    Inc(Result.Bottom, 2);
  end else
{$ENDIF}
  begin
    Canvas.Font.Assign(Font);
    {$ifndef CLX_USED}
    Result := Rect(0, 0, MaxWidth, 0);
    DrawText(Canvas.Handle, PChar(AHint), Length(AHint), Result,
              DT_CALCRECT or DT_LEFT or WrapFlags[WordWrap] or DT_NOPREFIX
              {$ifdef VCL_4_USED}
              or DrawTextBiDiModeFlagsReadingOnly
              {$endif}
              );

    Inc(Result.Right, 6);
    Inc(Result.Bottom, 2);
    {$else}
    result := inherited CalcHintRect(MaxWidth, AHint, AData);
    {$endif}
  end;
end;

procedure TElHintWindow.Paint;  { public }
var R: TRect;
    {$ifdef CLX_USED}
    DefaultDraw : boolean;
    {$endif}
begin
  R := ClientRect;
  Canvas.Font.Assign(Font);

  {$ifdef CLX_USED}
  Canvas.Pen.Color := Font.Color;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Rectangle(R);
  {$endif}
  Inc(R.Left, 2);
  Inc(R.Top, 2);

  {$ifdef CLX_USED}
  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    Inc(R.Top, 2);
  end;
  {$endif}
  {$endif}

{$IFDEF HAS_HTML_RENDER}
  if IsHTML then
  begin

    FRender.Data.DefaultColor := Font.Color;
    FRender.Data.DefaultBgColor := Color;
    FRender.Data.Charset := Font.Charset;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultFont := Font.Name;
    {$ifdef ELPACK_UNICODE}
    FRender.PrepareText(FWideCaption, 0, false);
    {$else}
    FRender.PrepareText(Caption, 0, false);
    {$endif}
    FRender.DrawText(Canvas, Point(0, 0), R, clNone);
  end
  else
{$ENDIF}
  begin
  {$ifndef CLX_USED}
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(FWideCaption), -1, R, DT_LEFT or DT_NOPREFIX or WrapFlags[WordWrap]);
    {$else}
    DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_LEFT or DT_NOPREFIX or WrapFlags[WordWrap]);
    {$endif}
  {$else}
    Canvas.Start;

    if Assigned(Application.Style.DrawHint) then
    begin
      DefaultDraw := True;
      Application.Style.DrawHint(Self, Canvas, R, DefaultDraw);
      if not DefaultDraw then
      begin
        Canvas.stop;
        Exit;
      end;
    end;

    Canvas.TextRect(R, R.Left, R.Top, FWideCaption, Integer(AlignmentFlags_AlignLeft) {or Integer(AlignmentFlags_AlignVCenter)} or WrapFlags[WordWrap]);
    Canvas.stop;
  {$endif}
  end;
end; { Paint }

procedure TElHintWindow.SetFont(newValue : TFont);
{ Sets data member FFont to newValue. }
begin
  FFont.Assign(newValue);
end;  { SetFont }

destructor TElHintWindow.Destroy;
begin
  FFont.Free;
{$IFDEF HAS_HTML_RENDER}
   FRender.Free;
{$ENDIF}
  inherited Destroy;
end;  { Destroy }

constructor TElHintWindow.Create(AOwner : TComponent);
{ Creates an object of type TElHintWindow, and initializes properties. }
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
{$IFDEF HAS_HTML_RENDER}
   FRender := TElHTMLRender.Create;
   FRender.OnImageNeeded := TriggerImageNeededEvent;
{$ENDIF}
end;  { Create }

{$ifdef HAS_HTML_RENDER}
procedure TElHintWindow.TriggerLinkClickEvent(HRef : TElFString);
begin
  if (assigned(FOnLinkClick)) then
    FOnLinkClick(Self, HRef );
end;  { TriggerLinkClickEvent }
{$endif}

procedure TElHintWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X, 
    Y: Integer);
{$ifdef HAS_HTML_RENDER}
var href : TElFString;
{$endif}
begin
{$ifdef HAS_HTML_RENDER}
  FRender.Data.DefaultColor := Font.Color;
  FRender.Data.DefaultBgColor := Color;

  FRender.Data.DefaultStyle := Font.Style;
  FRender.Data.DefaultHeight := Font.Height;
  FRender.Data.DefaultFont := Font.Name;
  if FRender.IsCursorOverLink(Point(X - 2, Y - 2), Point(0, 0), Rect(2, 2, ClientWidth - 2, ClientHeight - 2), href) then
  begin
    TriggerLinkClickEvent(href);
    exit;
  end;
{$endif}
  inherited;
end;

{$ifdef CLX_USED}
function TElHintWindow.WidgetFlags: Integer;
begin
  Result := Integer(WidgetFlags_WStyle_Customize)
              or Integer(WidgetFlags_WType_TopLevel)
              or Integer(WidgetFlags_WStyle_Tool)
              or Integer(WidgetFlags_WStyle_StaysOnTop){
              or Integer(WidgetFlags_WStyle_NoBorder)};
end;
{$endif}

{$ifndef CLX_USED}
procedure TElHintWindow.CMTextChanged(var Message: TMessage);
{$else}
procedure TElHintWindow.TextChanged;
{$endif}
var HRect : TRect;
begin
  if FActivating then Exit;
  {$ifdef ELPACK_UNICODE}
  HRect := CalcHintRectW(10000, FWideCaption, nil);
  {$else}
  HRect := CalcHintRect(10000, Caption, nil);
  {$endif}

  Width := HRect.Right - HRect.Left;
  Height := HRect.Bottom - HRect.Top;
end;

procedure TElHintWindow.SetWordWrap(Value: Boolean);
var HRect : TRect;
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    if FActivating then Exit;
    
    {$ifdef ELPACK_UNICODE}
    HRect := CalcHintRectW(10000, FWideCaption, nil);
    {$else}
    HRect := CalcHintRect(10000, Caption, nil);
    {$endif}

    Width := HRect.Right - HRect.Left;
    Height := HRect.Bottom - HRect.Top;
  end;
end;

end.

