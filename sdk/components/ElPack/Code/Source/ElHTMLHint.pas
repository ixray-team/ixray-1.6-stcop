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

05/20/2001

Fixed the bug when OnShow was not called properly


04/12/2001

Fixed the AV that happens when the application is closed.

*)

unit ElHTMLHint;

interface

uses
  {$ifndef CLX_USED}
{$ifdef VCL_6_USED}
  Types,
{$endif}
  Windows,
  Graphics,
  Controls,
  Forms,
  Messages,
  {$else}
  Types,
  QGraphics,
  QForms,
  QControls,
  {$endif}

  SysUtils,
  Classes,
  ElVCLUtils,
  ElStrUtils,
  ElHintWnd,
  HTMLRender;

type

  TElHTMLHint = class(TComponent)
  private
    FEnabled  : Boolean;
    FHintClass: THintWindowClass;
    FOnShow,
    FOnHide   : TNotifyEvent;
    {$ifdef HAS_HTML_RENDER}
    FOnImageNeeded: TElHTMLImageNeededEvent;
    {$endif}
  protected
    FFontName: TFontName;
    procedure SetEnabled(Value : Boolean); virtual;
    procedure SetOnHide(Value : TNotifyEvent);
    procedure SetOnShow(Value : TNotifyEvent);
    {$ifdef HAS_HTML_RENDER}
    procedure SetOnImageNeeded(Value : TElHTMLImageNeededEvent);
    {$endif}
    procedure SetFontName(const Value: TFontName);
  public
    destructor Destroy; override;
  published
    property Enabled : Boolean read FEnabled write SetEnabled;  { Published }

    property OnShow  : TNotifyEvent read FOnShow write SetOnShow;
    property OnHide  : TNotifyEvent read FOnHide write SetOnHide;
    {$ifdef HAS_HTML_RENDER}
    property OnImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write
        SetOnImageNeeded;
    {$endif}
    property FontName: TFontName read FFontName write SetFontName;
  end;

  TElHTMLHintWindow = class(TElHintWindow)
  protected
    procedure OnShow;
    procedure OnHide;
    {$ifndef CLX_USED}
    procedure WMShowWindow(var Message: TMessage); message WM_SHOWWINDOW;
    procedure WMWindowPosChanged(var Message: TMessage); message
        WM_WINDOWPOSCHANGED;
    {$else}
    procedure VisibleChanged; override;
    {$endif}
    {$ifdef HAS_HTML_RENDER}
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image
        : TBitmap); override;
    {$endif}
    procedure SetupRightCaption(Caption : String);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
{$ifdef CLX_USED}
    function CalcHintRect(MaxWidth: Integer; const AHint: WideString; AData: Pointer): TRect; override;
    procedure ActivateHint(Rect: TRect; const AHint: WideString); override;
{$else}
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
{$endif}
  end;  { TElHTMLHintWindow }

implementation

var OnHintShow,
    OnHintHide   : TNotifyEvent;
{$IFDEF HAS_HTML_RENDER}
    OnHintImageNeeded : TElHTMLImageNeededEvent;
{$endif}
    AFontName    : TFontName;

destructor TElHTMLHint.Destroy;
begin
  OnHintHide := nil;
  OnHintShow := nil;
  {$ifdef HAS_HTML_RENDER}
  OnHintImageNeeded := nil;
  {$endif}
  Enabled := false;
  inherited;
end;

procedure TElHTMLHint.SetEnabled(Value : Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    if (not (csDesigning in ComponentState)) and (not (csDestroying in ComponentState)) then
    begin
      Application.ShowHint := false;

      if FEnabled then
      begin
        if HintWindowClass <> TElHTMLHintWindow then
           FHintClass := HintWindowClass;
        HintWindowClass := TElHTMLHintWindow;
      end
      else
        HintWindowClass := FHintClass;

      Application.ShowHint := true;
    end;
  end;  { if }
end;  { SetEnabled }

{$ifdef HAS_HTML_RENDER}
procedure TElHTMLHint.SetOnImageNeeded(Value : TElHTMLImageNeededEvent);
begin
  OnHintImageNeeded := Value;
  FOnImageNeeded := Value;
end;
  {$endif}

procedure TElHTMLHint.SetOnHide(Value : TNotifyEvent);
begin
  OnHintHide := Value;
  FOnHide := Value;
end;

procedure TElHTMLHint.SetOnShow(Value : TNotifyEvent);
begin
  OnHintShow := Value;
  FOnShow := Value;
end;

procedure TElHTMLHint.SetFontName(const Value: TFontName);
begin
  if FFontName <> Value then
  begin
    FFontName := Value;
    AFontName := Value;
  end;
end;

destructor TElHTMLHintWindow.Destroy;
begin
  inherited Destroy;
end;  { Destroy }

constructor TElHTMLHintWindow.Create(AOwner : TComponent);
{$IFNDEF VCL_5_USED}
var
  NonClientMetrics: TNonClientMetrics;
  FS : TFontStyles;
{$ENDIF}
begin
  inherited Create(AOwner);
  
  {$IFNDEF VCL_5_USED}
  FS := [];
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
  begin
    if NonClientMetrics.lfStatusFont.lfItalic <> 0 then
       Include(FS, fsItalic);
    if NonClientMetrics.lfStatusFont.lfUnderline <> 0 then
       Include(FS, fsUnderline);
    if NonClientMetrics.lfStatusFont.lfStrikeOut <> 0 then
       Include(FS, fsStrikeout);
    if NonClientMetrics.lfStatusFont.lfWeight >= 600 then
       Include(FS, fsBold);
  end;
  {$ENDIF}
  {$ifdef HAS_HTML_RENDER}
  with FRender.Data do
  begin
    DefaultBgColor := clInfoBk;
    DefaultColor   := clInfoText;
    {$IFDEF VCL_5_USED}
    DefaultStyle   := Screen.HintFont.Style;
    DefaultHeight  := Screen.HintFont.Height;
    DefaultFont    := Screen.HintFont.Name;
    Charset        := Screen.HintFont.Charset;

    Font.Name      := Screen.HintFont.Name;
    Font.Style     := Screen.HintFont.Style;
    Font.Charset   := Screen.HintFont.Charset;
    Font.Height    := Screen.HintFont.Height;
    {$ELSE}
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    begin
      DefaultHeight:= NonClientMetrics.lfStatusFont.lfHeight;
      DefaultStyle := FS;
      DefaultFont  := StrPas(NonClientMetrics.lfStatusFont.lfFaceName);
      Charset      := NonClientMetrics.lfStatusFont.lfCharSet;

      Font.Name      := DefaultFont;
      Font.Style     := FS;
      Font.Charset   := Charset;
      Font.Height    := DefaultHeight;
      Font.Color     := clInfoText;
      Color          := clInfoBk;
    end
    else
    begin
      DefaultHeight:= -11;
    end;
    {$ENDIF}
  end;
  {$endif}
end;  { Create }

procedure TElHTMLHintWindow.OnShow;
begin
  if Assigned(OnHintShow) then
    OnHintShow(Self);
end;

{$ifdef HAS_HTML_RENDER}
procedure TElHTMLHintWindow.TriggerImageNeededEvent(Sender : TObject; Src :
    TElFString; var Image : TBitmap);
begin
  if Assigned(OnHintImageNeeded) then
     OnHintImageNeeded(Self, Src, Image);
end;
{$endif}

procedure TElHTMLHintWindow.OnHide;
begin
  if Assigned(OnHintHide) then
    OnHintHide(Self);
end;

{$ifndef CLX_USED}
procedure TElHTMLHintWindow.WMShowWindow(var Message: TMessage);
begin
  if Message.wParam = 0 then
    OnHide;
  inherited;
end;

procedure TElHTMLHintWindow.WMWindowPosChanged(var Message: TMessage);
begin
  inherited;
  if (PWindowPos(Message.lParam).flags and SWP_SHOWWINDOW = SWP_SHOWWINDOW) then
    OnShow;
end;
{$else}
procedure TElHTMLHintWindow.VisibleChanged;
begin
  inherited;
  if Visible then
    OnShow
  else
    OnHide;
end;

{$endif}

procedure TElHTMLHintWindow.SetupRightCaption(Caption : String);
{$ifdef ELPACK_UNICODE}
var i : integer;
{$endif}
begin
  {$ifdef HAS_HTML_RENDER}
  IsHTML := Copy(Caption, 1, 6) = '<html>';
  {$endif}
  {$ifdef ELPACK_UNICODE}
  FWideCaption := Caption;
  if (Length(Caption) > 4) then
  begin
    if Copy(Caption, Length(Caption) - 3, 4) = #00#00#$FE#$FF then
    begin
      i := StrLen(PChar(Caption)) + 1;
      // Caption := Copy(Caption, i, Length(Caption));
      SetLength(FWideCaption, (Length(Caption) - i - 4) div 2);
      Move(Caption[i + 1], FWideCaption[1], Length(FWideCaption) * 2);
      FWideCaption := GetShortHintW(FWideCaption);
    end
    else
    if Copy(Caption, Length(Caption) - 3, 4) = #00#00#$FF#$FF then
    begin
      if Length(Caption) mod 2 = 1 then
        Delete(Caption, 1, 1); 
      SetLength(FWideCaption, (Length(Caption) - 4) div 2);
      Move(Caption[1], FWideCaption[1], Length(FWideCaption) * 2);
      //FWideCaption := GetShortHintW(FWideCaption);
    end;
  end;
  {$endif}
end;

function TElHTMLHintWindow.CalcHintRect;
begin
  if AFontName <> '' then
  begin
    {$ifdef HAS_HTML_RENDER}
    FRender.Data.DefaultFont := AFontName;
    {$endif}
    FFont.Name := AFontName;
  end;
  SetupRightCaption(AHint);
  {$ifdef ELPACK_UNICODE}
  if Length(FWideCaption) > 0 then
    result := inherited CalcHintRectW(MaxWidth, FWideCaption, AData)
  else
  {$ifndef CLX_USED}
    SetRectEmpty(Result);
  {$else}
    FillChar(Result, sizeof(Result), 0);
  {$endif}
  {$else}
  result := inherited CalcHintRect(MaxWidth, AHint, AData);
  {$endif}
end;

{$ifndef CLX_USED}
procedure TElHTMLHintWindow.ActivateHint(Rect: TRect; const AHint: string);
{$else}
procedure TElHTMLHintWindow.ActivateHint(Rect: TRect; const AHint: WideString);
{$endif}
begin
  {$ifdef ELPACK_UNICODE}
  if Length(FWideCaption) > 0 then
    inherited ActivateHintW(Rect, FWideCaption);
  {$else}
  inherited;
  {$endif}
end;

end.


