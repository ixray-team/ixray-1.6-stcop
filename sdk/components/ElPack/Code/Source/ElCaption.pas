
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

06/26/2002

  Fixed the problem with the incorrect background of the caption, painted in Windows XP with themes disabled

02/26/2002

  When setting window caption (only setting it), Visible property of the text item was ignored

01/27/2002

  Form caption works almost correctly (font style is not obeyed) with XP styles
  enabled

10/12/2001

  Form caption is drawn corectly when there are no caption texts specified
  FWndActive flag is set when Active property is changed in run-time. 

09/10/2001

  Now form's complex caption is shown in Alt-Tab list

07/18/2001

  Added Unicode support

07/12/2001

  When no Text items are defined, form default caption is drawn

04/18/2001

  Assignment method implemented (now one can use ElFormCaption in inherited forms).

02/05/2001

  Right-click on the caption, when popup menu was assigned, could cause an AV.
  Fixed.

01/27/2001

  Added Glyph, Layout and Align to TElCaptionText

09/05/2000

  Background styles fixed for Win95/Win98.

*)

unit ElCaption;

{Custom form caption}

interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Menus,
  Buttons,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElTools,
  ElPopBtn,
  ElVCLUtils,
  ElStrUtils,
  ElUxTheme,
  ElTmSchema,
  ElHook;

type

  TElFormCaption = class;
  TElCaptionButtons = class;
  TElCaptionTexts = class;

(*  TElMeasureCaptionPartEvent = procedure(Sender : TObject; Canvas : TCanvas;
                               var Height : integer; var Width : integer) of object;
*)
  TElCaptionText = class(TCollectionItem)
  private
    FActiveColor: TColor;
    FInactiveColor: TColor;
    FFont: TFont;
    FVisible: Boolean;
    FCaption: TElFString;
    FOwnerStyle: boolean;
    FLayout: TButtonLayout;
    FGlyph: TElButtonGlyph;
    FAlign: TAlignment;
    procedure SetOwnerStyle(newValue: boolean);
    procedure SetActiveColor(newValue: TColor);
    procedure SetInactiveColor(newValue: TColor);
    procedure SetFont(newValue: TFont);
    procedure SetVisible(newValue: Boolean);
    procedure SetCaption(newValue: TElFString);
    procedure FontChange(Sender: TObject);
    function GetGlyph: TBitmap;
    procedure SetGlyph(newValue: TBitmap);
    procedure SetLayout(newValue: TButtonLayout);
    procedure GlyphChanged(Sender: TObject);
    procedure SetAlign(newValue: TAlignment);
  protected
    procedure Paint(Canvas: TCanvas; R: TRect); virtual;
    function GetWidth(Canvas: TCanvas; Height : integer): Integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ActiveColor: TColor read FActiveColor write SetActiveColor default clCaptionText;
    property InactiveColor: TColor read FInactiveColor write SetInactiveColor default clInactiveCaptionText;
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Caption: TElFString read FCaption write SetCaption;
    property OwnerStyle: boolean read FOwnerStyle write SetOwnerStyle default true;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout;
    property Align: TAlignment read FAlign write SetAlign;
  end;

  TElCaptionTexts = class(TCollection)
  private
    FCaption: TElFormCaption;
  protected
    function GetItems(Index: integer): TElCaptionText;
    procedure SetItems(Index: integer; newValue: TElCaptionText);
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TElCaptionText;
    property Items[index: integer]: TElCaptionText read GetItems write SetItems; default;
  end;

  TElPaintBkgndType = (pbtActive, pbtInactive, pbtAlways);

  TElCaptionButton = class(TCollectionItem)
  private
    FAlign: TAlignment;
    FCaption: TElFString;
    FGlyph: TElButtonGlyph;
    FButtons: TElCaptionButtons;
    FEnabled: Boolean;
    FFixClick: Boolean;
    FDown: Boolean;
    FVisible: Boolean;
    FOwnerStyle: Boolean;
    FActiveColor: TColor;
    FInactiveColor: TColor;
    FFont: TFont;
    FLayout: TButtonLayout;
    FBtnRect: TRect;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    procedure SetLayout(newValue: TButtonLayout);
    procedure SetFont(newValue: TFont);
    procedure SetOwnerStyle(newValue: Boolean);
    procedure SetActiveColor(newValue: TColor);
    procedure SetInactiveColor(newValue: TColor);
    procedure SetVisible(newValue: Boolean);
    procedure SetEnabled(newValue: Boolean);
    procedure SetDown(newValue: Boolean);
    procedure SetAlign(newValue: TAlignment);
    procedure SetCaption(newValue: TElFString);
    function GetGlyph: TBitmap;
    procedure SetGlyph(newValue: TBitmap);
    procedure GlyphChanged(Sender: TObject);
    procedure FontChange(Sender: TObject);
  protected
    procedure Paint(Canvas: TCanvas; R: TRect); virtual;
    function GetWidth(Canvas: TCanvas; Height : integer): Integer;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Align: TAlignment read FAlign write SetAlign default taRightJustify;
    property Caption: TElFString read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property FixClick: Boolean read FFixClick write FFixClick;
    property Down: Boolean read FDown write SetDown default False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OwnerStyle: Boolean read FOwnerStyle write SetOwnerStyle default True;
    property ActiveColor: TColor read FActiveColor write SetActiveColor default clBtnText;
    property InactiveColor: TColor read FInactiveColor write SetInactiveColor default clBtnText;
    property Font: TFont read FFont write SetFont;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
  end;

  TElCaptionButtons = class(TCollection)
  private
    FCaption: TElFormCaption;
  protected
    function GetItems(Index: integer): TElCaptionButton;
    procedure SetItems(Index: integer; newValue: TElCaptionButton);
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TElCaptionButton;
    property Items[index: integer]: TElCaptionButton read GetItems write SetItems; default;
  end;

  TElCaptionDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; var Rect: TRect) of object;
  TCaptionButtonEvent = procedure(Sender: TObject; Button: TElCaptionButton) of object;

  TElFormCaption = class(TComponent)
  private
    FTexts: TElCaptionTexts;
    FInactiveBitmap: TBitmap;
    FButtons: TElCaptionButtons;
    FPaintBkgnd: TElPaintBkgndType;
    FPopupMenu: TPopupMenu;
    FBitmap: TBitmap;
    FHook: TElHook;
    FActive: Boolean;
    FActiveLeftColor: TColor;
    FActiveRightColor: TColor;
    FInactiveLeftColor: TColor;
    FInactiveRightColor: TColor;
    FBackgroundType: TElBkGndType;
    FNumColors: Integer;
    FAlignment: TAlignment;
    FForm: TForm;
    FSystemFont: Boolean;
    FFont,
      Font2: TFont;
    FWndActive: boolean;
    FRegion,
      FSaveRegion: HRGN;
    FClicked: boolean;
    FInBtn: boolean;
    FBtnPressed: TElCaptionButton;
    FBtnsRect: TRect;
    FOnDrawCaption: TElCaptionDrawEvent;
    FOnButtonClick: TCaptionButtonEvent;
    FOnButtonDblClick: TCaptionButtonEvent;
    FPreventUpdate: Boolean;
    FTheme: HTheme;
    (*FOnMeasureCaptionPart: TElMeasureCaptionPartEvent;*)

    procedure SetActive(newValue: Boolean);
    procedure SetActiveLeftColor(newValue: TColor);
    procedure SetActiveRightColor(newValue: TColor);
    procedure SetInactiveLeftColor(newValue: TColor);
    procedure SetBackgroundType(newValue: TElBkGndType);
    procedure SetPopupMenu(newValue: TPopupMenu);
    procedure SetNumColors(newValue: Integer);
    procedure SetAlignment(newValue: TAlignment);
    procedure SetBitmap(newValue: TBitmap);
    procedure BitmapChange(Sender: TObject);
    procedure OnBeforeHook(Sender: TObject; var Msg: TMessage; var Handled: boolean);
    procedure OnAfterHook(Sender: TObject; var Msg: TMessage; var Handled: boolean);
    procedure SetPaintBkgnd(newValue: TElPaintBkgndType);
    procedure SetInactiveRightColor(newValue: TColor);
    procedure SetButtons(newValue: TElCaptionButtons);
    procedure SetInactiveBitmap(newValue: TBitmap);
    procedure SetSystemFont(newValue: Boolean);
    procedure SetFont(newValue: TFont);
    procedure FontChange(Sender: TObject);
    procedure GetSystemFont;
    procedure SetTexts(newValue: TElCaptionTexts);
  protected
    FUseXPThemes: Boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
    procedure Update;
    procedure PaintCaption(var Msg: TMessage; Step: integer); virtual;
    procedure TriggerDrawCaptionEvent(Canvas: TCanvas; var Rect: TRect); virtual;
    procedure TriggerButtonClickEvent(Button: TElCaptionButton); virtual;
    procedure TriggerButtonDblClickEvent(Button: TElCaptionButton); virtual;
    procedure SetUseXPThemes(Value: Boolean);
    procedure CreateThemeHandle; virtual;
    procedure FreeThemeHandle; virtual;
    procedure AllocThemes;
    (*procedure TriggerMeasureCaptionPart(Sender : TObject; Canvas : TCanvas; var
        Height : integer; var Width : integer);*)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ButtonAtPos(X, Y: integer): TElCaptionButton;
    procedure Assign(Source : TPersistent); override;
    function IsThemeApplied: Boolean;
  published
    property Active: Boolean read FActive write SetActive;
    property ActiveLeftColor: TColor read FActiveLeftColor write SetActiveLeftColor default clBlack;
    property ActiveRightColor: TColor read FActiveRightColor write SetActiveRightColor default clActiveCaption;
    property InactiveLeftColor: TColor read FInactiveLeftColor write SetInactiveLeftColor default clBlack;
    property InactiveRightColor: TColor read FInactiveRightColor write SetInactiveRightColor default clInactiveCaption;
    property BackgroundType: TElBkGndType read FBackgroundType write SetBackgroundType default bgtColorFill;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property NumColors: Integer read FNumColors write SetNumColors default 64;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ActiveBitmap: TBitmap read FBitmap write SetBitmap;
    property PaintBkgnd: TElPaintBkgndType read FPaintBkgnd write SetPaintBkgnd default pbtAlways;
    property Buttons: TElCaptionButtons read FButtons write SetButtons;
    property InactiveBitmap: TBitmap read FInactiveBitmap write SetInactiveBitmap;
    property OnDrawCaption: TElCaptionDrawEvent read FOnDrawCaption write FOnDrawCaption;
    property OnButtonClick: TCaptionButtonEvent read FOnButtonClick write FOnButtonClick;
    property OnButtonDblClick: TCaptionButtonEvent read FOnButtonDblClick write FOnButtonDblClick;
    (*property OnMeasureCaptionPart: TElMeasureCaptionPartEvent read
        FOnMeasureCaptionPart write FOnMeasureCaptionPart;*)
    property SystemFont: Boolean read FSystemFont write SetSystemFont default true;
    property Font: TFont read FFont write SetFont;
    property Texts: TElCaptionTexts read FTexts write SetTexts;
    property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default true;
  end;

implementation

{$IFNDEF VCL_4_USED}
const
  SPI_GETGRADIENTCAPTIONS = 4104;
  COLOR_GRADIENTACTIVECAPTION = 27;
  COLOR_GRADIENTINACTIVECAPTION = 28;
{$ENDIF}

function TElCaptionTexts.GetItems(Index: integer): TElCaptionText;
begin
  result := TElCaptionText(inherited GetItem(Index));
end;

procedure TElCaptionTexts.SetItems(Index: integer; newValue: TElCaptionText);
begin
  inherited SetItem(Index, newValue);
end;

function TElCaptionTexts.GetOwner: TPersistent;
begin
  result := FCaption;
end;

procedure TElCaptionTexts.Update(Item: TCollectionItem);
begin
  if not FCaption.FPreventUpdate then
    FCaption.Update;
end;

function TElCaptionTexts.Add: TElCaptionText;
begin
  result := TElCaptionText(inherited Add);
end;

procedure TElCaptionText.SetActiveColor(newValue: TColor);
begin
  if (FActiveColor <> newValue) then
  begin
    FActiveColor := newValue;
    if visible then Changed(false);
  end; {if}
end; {SetActiveColor}

procedure TElCaptionText.SetInactiveColor(newValue: TColor);
begin
  if (FInactiveColor <> newValue) then
  begin
    FInactiveColor := newValue;
    if visible then Changed(false);
  end; {if}
end; {SetInactiveColor}

procedure TElCaptionText.SetFont(newValue: TFont);
begin
  FFont.Assign(newValue);
end; {SetFont}

procedure TElCaptionText.SetVisible(newValue: Boolean);
begin
  if (FVisible <> newValue) then
  begin
    FVisible := newValue;
    Changed(true);
  end; {if}
end; {SetVisible}

procedure TElCaptionText.Assign(Source: TPersistent);
begin
  if Source is TElCaptionText then
    with Source as TElCaptionText do
    begin
      Self.FActiveColor := FActiveColor;
      Self.FInactiveColor := FInactiveColor;
      Self.FFont.Assign(Font);
      Self.FVisible := FVisible;
      Self.FCaption := FCaption;
      Self.FOwnerStyle := FOwnerStyle;
      Self.Layout := FLayout;
      Self.Glyph := Glyph;
    end
  else
    inherited;
end;

procedure TElCaptionText.FontChange(Sender: TObject);
begin
  if visible then Changed(true);
end;

procedure TElCaptionText.SetCaption(newValue: TElFString);
begin
  if (FCaption <> newValue) then
  begin
    FCaption := newValue;
    if visible then Changed(true);
  end; {if}
end; {SetCaption}

procedure TElCaptionText.SetOwnerStyle(newValue: boolean);
begin
  if (FOwnerStyle <> newValue) then
  begin
    FOwnerStyle := newValue;
    Changed(true);
  end; {if}
end;

destructor TElCaptionText.Destroy;
begin
  FFont.Free;
  FGlyph.Free;
  inherited;
end; {Destroy}

constructor TElCaptionText.Create;
begin
  FGlyph := TElButtonGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FLayout := blGlyphLeft;
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FActiveColor := clCaptionText;
  FInactiveColor := clInactiveCaptionText;
  FVisible := True;
  FOwnerStyle := true;
end; {Create}

function TElCaptionText.GetGlyph: TBitmap;
begin
  Result := FGlyph.Glyph;
end; {GetGlyph}

procedure TElCaptionText.SetGlyph(newValue: TBitmap);
begin
  FGlyph.Glyph := newValue;
  Changed(true);
end; {SetGlyph}

procedure TElCaptionText.SetLayout(newValue: TButtonLayout);
begin
  if (FLayout <> newValue) then
  begin
    FLayout := newValue;
    if Visible then Changed(false);
  end; {if}
end;

procedure TElCaptionText.GlyphChanged(Sender: TObject);
begin
  Changed(false);
end;

procedure TElCaptionText.SetAlign(newValue: TAlignment);
begin
  if (FAlign <> newValue) then
  begin
    FAlign := newValue;
    Changed(true);
  end; {if}
end; {SetAlign}

procedure TElCaptionText.Paint(Canvas: TCanvas; R: TRect);
begin
  FGlyph.Draw(Canvas, R, Point(0, 0), Caption, FLayout, 0, 1, ebsUp, ebsUp, taCenter, false,
    False, false, not Glyph.Empty, true, 0, tdtNormal, clNone, false, 0, 0, 0, false, false{$ifdef HAS_HTML_RENDER}, false, nil{$endif}, true);
end; {Paint}

function TElCaptionText.GetWidth(Canvas: TCanvas; Height : integer): Integer;
var
  ACaption: TElFormCaption;
  SrcRect : TRect;
  PP      : TPoint;
  R       : TRect;
begin
  FillMemory(@SrcRect, SizeOf(TRect), 0);
  SrcRect.Bottom := Height;

  ACaption := TElCaptionTexts(Collection).FCaption;
  if OwnerStyle then
  begin
    with ACaption do
      if FSystemFont then
        Canvas.Font.Assign(Font2)
      else
        Canvas.Font.Assign(FFont);
  end
  else
    Canvas.Font.Assign(Self.Font);
  with ACaption do
  begin
    if FWndActive then
      Canvas.Font.Color := Self.FActiveColor
    else
      Canvas.Font.Color := Self.FInActiveColor;
    if not Self.Glyph.Empty then
    begin
      SrcRect.Right := SrcRect.Bottom - SrcRect.Top;
      Self.FGlyph.GetPaintGlyphSize(SrcRect, PP);
      SrcRect.Right := Max(PP.X + 2, SrcRect.Right);
    end
    else
      if (Self.FCaption = '') then
        SrcRect.Right := SrcRect.Left + (SrcRect.Bottom - SrcRect.Top);
    if (Self.FCaption <> '') then
    begin
      SetRectEmpty(R);
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Self.FCaption), Length(Self.FCaption), R, DT_SINGLELINE or DT_CALCRECT);
      {$else}
      DrawText(Canvas.Handle, PChar(Self.FCaption), Length(Self.FCaption), R,  DT_SINGLELINE or DT_CALCRECT);
      {$endif}

      Inc(SrcRect.Right, R.Right - R.Left);
    end;
  end;
  result := SrcRect.Right - SrcRect.Left + 1;
end;

procedure TElCaptionButton.SetAlign(newValue: TAlignment);
begin
  if (FAlign <> newValue) then
  begin
    FAlign := newValue;
    Changed(true);
  end; {if}
end; {SetAlign}

procedure TElCaptionButton.SetCaption(newValue: TElFString);
begin
  if (FCaption <> newValue) then
  begin
    FCaption := newValue;
    Changed(true);
  end; {if}
end; {SetCaption}

function TElCaptionButton.GetGlyph: TBitmap;
begin
  Result := FGlyph.Glyph;
end; {GetGlyph}

procedure TElCaptionButton.FontChange(Sender: TObject);
begin
  if Visible then Changed(false);
end;

procedure TElCaptionButton.GlyphChanged(Sender: TObject);
begin
  Changed(false);
end;

procedure TElCaptionButton.SetGlyph(newValue: TBitmap);
begin
  FGlyph.Glyph := newValue;
  Changed(true);
end; {SetGlyph}

procedure TElCaptionButton.SetEnabled(newValue: Boolean);
begin
  if (FEnabled <> newValue) then
  begin
    FEnabled := newValue;
    Changed(false);
  end; {if}
end; {SetEnabled}

procedure TElCaptionButton.SetDown(newValue: Boolean);
begin
  if (FDown <> newValue) then
  begin
    FDown := newValue;
    Changed(false);
  end; {if}
end; {SetDown}

procedure TElCaptionButton.Assign(Source: TPersistent);
begin
  if Source is TElCaptionButton then
    with Source as TElCaptionButton do
    begin
      Self.FAlign := FAlign;
      Self.FCaption := FCaption;
      Self.Glyph := Glyph;
      Self.FEnabled := FEnabled;
      Self.FFixClick := FixClick;
      Self.FDown := Down;
      Self.FVisible := Visible;
      Self.FOwnerStyle := OwnerStyle;
      Self.FActiveColor := FActiveColor;
      Self.FInactiveColor := FInactiveColor;
      Self.FFont.Assign(FFont);
      Self.FLayout := FLayout;
    end
  else
    inherited;
end;

procedure TElCaptionButton.Paint(Canvas: TCanvas; R: TRect);
var
  DrawFlags: Integer;
  FState: TElButtonState;
  sid : integer;
  ATheme : HTheme;
begin
  DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
  if (FDown) then DrawFlags := DrawFlags or DFCS_PUSHED;
  if not FButtons.FCaption.IsThemeApplied then
  begin
    DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DrawFlags);
    InflateRect(R, 1, 1);
  end
  else
  begin
    if FDown then
      sid := PBS_PRESSED
    else
      sid := PBS_NORMAL;
    ATheme := OpenThemeData(0, 'BUTTON');
    DrawThemeBackground(ATheme, Canvas.Handle, BP_PUSHBUTTON, sid, R, nil);
    GetThemeBackgroundContentRect(ATheme, Canvas.Handle, BP_PUSHBUTTON, sid, R, R);
    CloseThemeData(ATheme);
  end;
  if not FEnabled then
    FState := ebsDisabled
  else
    if FDown then
    begin
      FState := ebsDown;
      OffsetRect(R, 1, 1);
    end
    else
      FState := ebsUp;
  FGlyph.Draw(Canvas, R, Point(0, 0), Caption, FLayout, 1, 1, FState, FState, taCenter, false,
    Pos(#13#10, FCaption) > 0, false, true, true, 0, tdtNormal, clBtnFace, false, 0, 0, 0, false, false{$ifdef HAS_HTML_RENDER}, false, nil{$endif}, true);
end; {Paint}

procedure TElCaptionButton.SetVisible(newValue: Boolean);
begin
  if (FVisible <> newValue) then
  begin
    FVisible := newValue;
    if not newValue then FBtnRect := Rect(0, 0, 0, 0);
    Changed(true);
  end; {if}
end;

procedure TElCaptionButton.SetOwnerStyle(newValue: Boolean);
begin
  if (FOwnerStyle <> newValue) then
  begin
    FOwnerStyle := newValue;
    if Visible then Changed(true);
  end; {if}
end;

procedure TElCaptionButton.SetActiveColor(newValue: TColor);
begin
  if (FActiveColor <> newValue) then
  begin
    FActiveColor := newValue;
    if Visible then Changed(false);
  end; {if}
end;

procedure TElCaptionButton.SetInactiveColor(newValue: TColor);
begin
  if (FInactiveColor <> newValue) then
  begin
    FInactiveColor := newValue;
    if Visible then Changed(false);
  end; {if}
end;

procedure TElCaptionButton.SetFont(newValue: TFont);
begin
  FFont.Assign(newValue);
end;

procedure TElCaptionButton.SetLayout(newValue: TButtonLayout);
begin
  if (FLayout <> newValue) then
  begin
    FLayout := newValue;
    if Visible then Changed(false);
  end; {if}
end;

destructor TElCaptionButton.Destroy;
begin
  Dec(ButtonCount);
  if ButtonCount <= 0 then
  begin
    Pattern.Free;
    Pattern := nil;
  end;
  FFont.Free;
  FGlyph.Free;
  inherited;
end;

constructor TElCaptionButton.Create;
begin
  FButtons := TElCaptionButtons(Collection);
  FGlyph := TElButtonGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  FLayout := blGlyphLeft;
  inherited;
  Inc(ButtonCount);
  FAlign := taRightJustify;
  FEnabled := True;
  FDown := False;
  FVisible := True;
  FOwnerStyle := True;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FActiveColor := clBtnText;
  FInActiveColor := clBtnText;
end; {Create}

function TElCaptionButton.GetWidth(Canvas: TCanvas; Height : integer): Integer;
var
  ACaption: TElFormCaption;
  ATheme  : HTheme;
  SrcRect : TRect;
  PP      : TPoint;
  R       : TRect;
begin
  FillMemory(@SrcRect, SizeOf(TRect), 0);
  SrcRect.Bottom := Height;

  ACaption := TElCaptionButtons(Collection).FCaption;
  if OwnerStyle then
  begin
    with ACaption do
      if FSystemFont then
        Canvas.Font.Assign(Font2)
      else
        Canvas.Font.Assign(FFont);
  end
  else
    with ACaption do
      Canvas.Font.Assign(Self.Font);
  with ACaption do
  begin
    if FWndActive then
      Canvas.Font.Color := Self.FActiveColor
    else
      Canvas.Font.Color := Self.FInActiveColor;
    if not Self.Glyph.Empty then
    begin
      SrcRect.Right := SrcRect.Bottom - SrcRect.Top;
      Self.FGlyph.GetPaintGlyphSize(SrcRect, PP);
      SrcRect.Right := Max(PP.X + 2, SrcRect.Right);
    end
    else
      if (Self.FCaption = '') then
        SrcRect.Right := SrcRect.Left + (SrcRect.Bottom - SrcRect.Top);
    if (Self.FCaption <> '') then
    begin
      SetRectEmpty(R);
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Self.FCaption), Length(Self.FCaption), R, DT_SINGLELINE or DT_CALCRECT);
      {$else}
      DrawText(Canvas.Handle, PChar(Self.FCaption), Length(Self.FCaption), R,  DT_SINGLELINE or DT_CALCRECT);
      {$endif}
      Inc(SrcRect.Right, R.Right - R.Left);
    end;
    if not ACaption.IsThemeApplied then
      Inc(SrcRect.Right, 4)
    else
    begin
      ATheme := OpenThemeData(0, 'BUTTON');
      GetThemeBackgroundExtent(ATheme, Canvas.Handle, BP_PUSHBUTTON, PBS_NORMAL, @SrcRect, SrcRect);
      CloseThemeData(ATheme);
      Inc(SrcRect.Right, 2)
    end;
  end;
  result := SrcRect.Right - SrcRect.Left;
end;

function TElCaptionButtons.GetItems(Index: integer): TElCaptionButton;
begin
  result := TElCaptionButton(inherited GetItem(Index));
end;

procedure TElCaptionButtons.SetItems(Index: integer; newValue: TElCaptionButton);
begin
  inherited SetItem(Index, newValue);
end;

function TElCaptionButtons.GetOwner: TPersistent;
begin
  result := FCaption;
end;

procedure TElCaptionButtons.Update(Item: TCollectionItem);
begin
  if (FCaption.FForm <> nil) and (FCaption.FForm.HandleAllocated) then FCaption.Update;
end;

function TElCaptionButtons.Add: TElCaptionButton;
begin
  result := TElCaptionButton(inherited Add);
end; {Add}

procedure TElFormCaption.SetActive(newValue: Boolean);
begin
  if (FActive <> newValue) then
  begin
    FActive := newValue;
    FWndActive := FForm.HandleAllocated and (GetForegroundWindow = FForm.Handle);
    begin
      FHook.Active := FActive;
      Update;
    end;
    if Active and (not (csLoading in ComponentState)) then
    begin
      if UseXPThemes and ThemesAvailable then AllocThemes;
      if ThemesAvailable then
        FForm.Caption := '';
    end;
  end; {if}
end; {SetActive}

procedure TElFormCaption.SetActiveLeftColor(newValue: TColor);
begin
  if (FActiveLeftColor <> newValue) then
  begin
    FActiveLeftColor := newValue;
    Update;
  end; {if}
end; {SetActiveLeftColor}

procedure TElFormCaption.SetActiveRightColor(newValue: TColor);
begin
  if (FActiveRightColor <> newValue) then
  begin
    FActiveRightColor := newValue;
    Update;
  end; {if}
end; {SetActiveRightColor}

procedure TElFormCaption.SetInactiveLeftColor(newValue: TColor);
begin
  if (FInactiveLeftColor <> newValue) then
  begin
    FInactiveLeftColor := newValue;
    Update;
  end; {if}
end; {SetInactiveLeftColor}

procedure TElFormCaption.SetBackgroundType(newValue: TElBkGndType);
begin
  if (FBackgroundType <> newValue) then
  begin
    FBackgroundType := newValue;
    Update;
  end; {if}
end; {SetBackgroundType}

procedure TElFormCaption.SetPopupMenu(newValue: TPopupMenu);
begin
  if (FPopupMenu <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FPopupMenu <> nil then FPopupMenu.RemoveFreeNotification(Self);
    {$endif}
    FPopupMenu := newValue;
    if FPopupMenu <> nil then FPopupMenu.FreeNotification(Self);
  end; {if}
end; {SetPopupMenu}

procedure TElFormCaption.SetNumColors(newValue: Integer);
begin
  if (FNumColors <> newValue) then
  begin
    FNumColors := newValue;
    Update;
  end; {if}
end; {SetNumColors}

procedure TElFormCaption.SetAlignment(newValue: TAlignment);
begin
end; {SetAlignment}

procedure TElFormCaption.BitmapChange(Sender: TObject);
begin
  if Active then Update;
end;

procedure TElFormCaption.SetBitmap(newValue: TBitmap);
begin
  FBitmap.Assign(newValue);
end; {SetBitmap}

procedure TElFormCaption.Loaded;
begin
  inherited;
  if Active then
  begin
    if UseXPThemes and ThemesAvailable then AllocThemes;
    if ThemesAvailable then
      FForm.Caption := '';
  end;
  if Active then Update;
end; {Loaded}

procedure TElFormCaption.OnAfterHook(Sender: TObject; var Msg: TMessage; var Handled: boolean);
var S : String;
    i : integer;
begin
  case Msg.Msg of //
    WM_CREATE:
      if UseXPThemes and ThemesAvailable then
        CreateThemeHandle;
    WM_DESTROY:
      if UseXPThemes and ThemesAvailable then
        FreeThemeHandle;
    WM_NCPAINT:
      begin
        if UseXPThemes and ThemesAvailable  then
          AllocThemes;
        PaintCaption(Msg, 2);
      end;
    WM_NCACTIVATE: Update;
    WM_SETICON,
      WM_WINDOWPOSCHANGED,
      WM_INITMENU,
      WM_NCLBUTTONDOWN
    //,WM_SYSCOMMAND
    :
      begin
        RedrawWindow(FForm.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
      end;
    WM_GETTEXTLENGTH:
      begin
        if UseXPThemes and ThemesAvailable then
          AllocThemes;
        if not IsThemeApplied then
        begin
          if (not FPreventUpdate) and (FTexts.Count > 0) then
          begin
            S := '';
            for i := 0 to FTexts.Count -1 do
            begin
              if FTexts[i].Visible then
                S := S + FTexts[i].Caption;
            end;
            Msg.Result := Length(S);
          end
        end
        else
        begin
          Msg.Result := 0;
        end;
      end;
    WM_GETTEXT:
      begin
        if UseXPThemes and ThemesAvailable then
          AllocThemes;
        if not IsThemeApplied then
        begin
          if (not FPreventUpdate) and (FTexts.Count > 0) then
          begin
            S := '';
            for i := 0 to FTexts.Count -1 do
            begin
              if FTexts[i].Visible then
                S := S + FTexts[i].Caption;
            end;

            StrLCopy(PChar(Msg.LParam), PChar(S), Msg.WParam - 1);
            (PChar(Msg.LParam) + Msg.WParam - 1)^ := #0;
          end;
        end
        else
        begin
          PChar(Msg.LParam)^ := #0;
        end;
        (*if (FTexts.Count > 0) and not (FPreventUpdate) then
        begin
          FillMemory(PChar(Msg.LParam), Msg.WParam, 0);
          Msg.Result := 0;
        end;
        *)
      end;
    WM_SETTEXT:
      Update;
    WM_SETTINGCHANGE:
      begin
        if Msg.WParam = SPI_SETNONCLIENTMETRICS then
        begin
          if SystemFont then GetSystemFont;
          Update;
        end;
      end;
  end; // case
end;

procedure TElFormCaption.OnBeforeHook(Sender: TObject; var Msg: TMessage; var Handled: boolean);
var
  b: boolean;
  Btn: TElCaptionButton;
  P: TPoint;
  R: TRect;
begin
  case Msg.Msg of //
    WM_NCACTIVATE:
      begin
        FWndActive := TWMNCActivate(Msg).Active;
        //if not ThemesAvailable then
        begin
          Msg.Result := 1;
          Msg.WParam := 1;
        end;
      end;
    WM_SETTEXT:
      begin
        if UseXPThemes and ThemesAvailable then AllocThemes;
        if IsThemeApplied then
          Msg.lParam := Integer(PChar(''));
      end;
    WM_WINDOWPOSCHANGING:
      with TWMWindowPosChanging(Msg).WindowPos^ do
        if (csCreating in FForm.ControlState) and ((Flags and SWP_HIDEWINDOW) <> 0) then
         Flags := Flags or SWP_NOREDRAW;
    WM_SYSCOMMAND:
      Update;
    WM_NCLBUTTONDOWN:
      begin
        //if not ThemesAvailable then
        begin
          Btn := ButtonAtPos(Msg.lParamLo, Smallint(Msg.lParamHi));
          if Btn <> nil then
          begin
            if Btn.Enabled then
            begin
              FBtnPressed := Btn;
              if Btn.FixClick then
                Btn.FDown := not Btn.FDown
              else
                Btn.FDown := true;
              SetCapture(FForm.Handle);
              FInBtn := true;
              R := Btn.FBtnRect;
              RedrawWindow(FForm.Handle, @R, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
            end;
            Msg.Result := 1;
            Msg.Msg := WM_NULL;
            Handled := true;
          end;
        end;
      end;
    WM_MOUSEMOVE:
      begin
        //if not ThemesAvailable then
        begin
          if FBtnPressed <> nil then
          begin
            P := Point(TWMLBUTTONUP(Msg).XPos, TWMLBUTTONUP(Msg).YPos);
            ClientToScreen(FForm.Handle, P);
            Btn := ButtonAtPos(P.X, P.Y);
            if Btn <> FBtnPressed then
            begin
              if (FBtnPressed <> nil) and (FInBtn) then
                with FBtnPressed do
                  if not FixClick then
                  begin
                    FDown := false; // else Update;
                    R := FBtnPressed.FBtnRect;
                    RedrawWindow(FForm.Handle, @R, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
                  end;
              FInBtn := false;
            end
            else
              if (not FInBtn) and (Btn <> nil) then
              begin
                if not Btn.FFixClick then
                begin
                  Btn.FDown := true;
                  R := Btn.FBtnRect;
                  RedrawWindow(FForm.Handle, @R, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
                end;
                FInBtn := true;
              end;
            Msg.Result := 1;
            Msg.Msg := WM_NULL;
            Handled := true;
          end;
        end;
      end;
    WM_NCLBUTTONDBLCLK:
      begin
        //if not ThemesAvailable then
        begin
          Btn := ButtonAtPos(Msg.lParamLo, Smallint(Msg.lParamHi));
          if Btn <> nil then
          begin
            TriggerButtonDblClickEvent(Btn);
            Msg.Result := 1;
            Msg.Msg := WM_NULL;
            Handled := true;
          end;
        end;
      end;
    WM_LBUTTONUP, WM_LBUTTONDBLCLK:
      begin
        //if not ThemesAvailable then
        begin
          if FBtnPressed <> nil then
          begin
            P := Point(TWMLBUTTONUP(Msg).XPos, TWMLBUTTONUP(Msg).YPos);
            ClientToScreen(FForm.Handle, P);
            Btn := ButtonAtPos(P.X, P.Y);
            if Btn <> FBtnPressed then
            begin
              with FBtnPressed do
              begin
                if not FixClick then
                  Down := false
                else
                  Down := not Down;
                R := FBtnRect;
                RedrawWindow(FForm.Handle, @R, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
              end;
            end
            else
            begin
              with FBtnPressed do
                if not FixClick then
                begin
                  if Down then
                    FDown := false
                  else
                    ;
                  R := FBtnRect;
                  RedrawWindow(FForm.Handle, @R, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
                end;
              if not (csDesigning in ComponentState) then TriggerButtonClickEvent(FBtnPressed);
            end;
            Msg.Result := 1;
            Msg.Msg := WM_NULL;
            Handled := true;
            ReleaseCapture;
          end;
          FBtnPressed := nil;
        end;
      end;
    WM_NCPAINT:
      begin
        if UseXPThemes and ThemesAvailable  then
          AllocThemes;
        PaintCaption(Msg, 1);
      end;
    WM_SETCURSOR:
      begin
        //if not ThemesAvailable then
        begin
          b := true;
          case TWMSetCursor(Msg).HitTest of //
            HTBOTTOM,
              HTTOP: SetCursor(LoadCursor(0, IDC_SIZENS));
            HTLEFT,
              HTRIGHT: SetCursor(LoadCursor(0, IDC_SIZEWE));
            HTBOTTOMLEFT,
              HTTOPRIGHT: SetCursor(LoadCursor(0, IDC_SIZENESW));
            HTTOPLEFT,
              HTBOTTOMRIGHT: SetCursor(LoadCursor(0, IDC_SIZENWSE));
          else
            b := false;
          end; // case
          if b then
          begin
            Msg.Result := 1;
            Handled := true;
          end;
        end;
      end;
    WM_NCRBUTTONDOWN:
      if Assigned(FPopupMenu) then
        if FPopupMenu.AutoPopup then
        begin
          FClicked := True;
          Msg.Result := 0;
          Handled := True;
        end
        else
      else
        if Assigned(FForm.PopupMenu) and (FForm.PopupMenu.AutoPopup) then
        begin
          FClicked := True;
          Msg.Result := 0;
          Handled := True;
        end;
    WM_NCRBUTTONUP:
      if FClicked then
        with TWMMouse(Msg) do
        begin
          FClicked := False;
          if Assigned(FPopupMenu) then
            if (FPopupMenu.AutoPopup) then
            begin
              FPopupMenu.Popup(XPos, YPos);
              Result := 0;
              Handled := True;
            end
            else
          else
            if Assigned(FForm.PopupMenu) and (FForm.PopupMenu.AutoPopup) then
            begin
              FForm.PopupMenu.Popup(XPos, YPos);
              Result := 0;
              Handled := True;
            end;
        end;
  end; // case
end; {WndProc}

procedure TElFormCaption.Notification(AComponent: TComponent; operation: TOperation);
begin
  inherited Notification(AComponent, operation);
  if (operation = opRemove) then
  begin
    if (AComponent = FPopupMenu) then FPopupMenu := nil;
    if (AComponent = FForm) then FForm := nil;
  end
  else
  begin
    if (FForm <> AComponent) then FForm := GetOwnerForm(self);
  end;
end; {Notification}

procedure TElFormCaption.Update;
var S : String;
    i : integer;
begin
  if not (csLoading in ComponentState) then
    SetWindowPos(FForm.Handle, 0, 0, 0, 0, 0,
      SWP_DRAWFRAME or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  if Active and (FTexts.Count > 0) and (not (csDesigning in ComponentState)) then
  begin
    if not IsThemeApplied then
    begin
      S := '';
      for i := 0 to FTexts.Count -1 do
      begin
        if FTexts[i].Visible then
          S := S + FTexts[i].Caption;
      end;
      FForm.Caption := S;
    end
    else
      FForm.Caption := '';
  end;
end;

procedure TElFormCaption.PaintCaption(var Msg: TMessage; Step: integer);

  procedure PaintButtons(Canvas: TCanvas; var R: TRect; CalcOnly: boolean);
  var
    ID: boolean;
    IHandle,
      IHandle2: HICON;
    IconX,
      IconY: integer;
    BtnWidth: integer;
    Flag: DWORD;
    SrcRect: TRect;
    i: integer;
    Btns: TBorderIcons;
    Button: TElCaptionButton;
  begin
    if (((biSystemMenu in FForm.BorderIcons) and (FForm.BorderStyle in [bsSingle, bsSizeable]))
      or (csDesigning in ComponentState)) then
    begin
      Inc(R.Left, 1);
      ID := false;
      if FForm.Icon.Handle <> 0 then
        IHandle := FForm.Icon.Handle
      else
        if Application.Icon.Handle <> 0 then
          IHandle := Application.Icon.Handle
        else
        begin
          IHandle := LoadIcon(0, IDI_APPLICATION);
          ID := true;
        end;
      IconX := GetSystemMetrics(SM_CXSMICON);
      if IconX = 0 then
        IconX := GetSystemMetrics(SM_CXSIZE);
      IconY := GetSystemMetrics(SM_CYSMICON);
      if IconY = 0 then
        IconY := GetSystemMetrics(SM_CYSIZE);
      IHandle2 := CopyImage(IHandle, IMAGE_ICON, IconX, IconY, LR_COPYFROMRESOURCE);
      if not IsThemeApplied then
        DrawIconEx(Canvas.Handle, R.Left + 1, R.Top + 1, IHandle2, 0, 0, 0, 0, DI_NORMAL);
      DestroyIcon(IHandle2);
      if ID then DestroyIcon(IHandle);
      Inc(R.Left, GetSystemMetrics(SM_CXSMICON) + 3);
    end;
    SrcRect := R;
    InflateRect(SrcRect, -2, -2);
    if (csDesigning in ComponentState) then
      Btns := [biSystemMenu, biMinimize, biMaximize]
    else
    begin
      Btns := [];
      if (biSystemMenu in FForm.BorderIcons) then
      begin
        Btns := [biSystemMenu];
        if not (FForm.BorderStyle in [bsToolWindow, bsSizeToolWin]) then
        begin
          if (FForm.BorderStyle = bsDialog) and (biHelp in FForm.BorderIcons) then Include(Btns, biHelp);
          if (((FForm.BorderStyle = bsSingle) or
            (FForm.BorderStyle = bsSizeable)) and
            (biHelp in FForm.BorderIcons) and
            (not (biMinimize in FForm.BorderIcons)) and
            (not (biMaximize in FForm.BorderIcons))) then
            Include(Btns, biHelp);
          if (FForm.BorderStyle <> bsDialog) then
          begin
            if ((biMinimize in FForm.BorderIcons) or
              (biMaximize in FForm.BorderIcons)) then
              Btns := Btns + [biMinimize, biMaximize];
          end;
        end;
      end;
    end;
    BtnWidth := GetSystemMetrics(SM_CXSIZE) - 2;
    if ((FForm.BorderStyle in [bsToolWindow, bsSizeToolWin]) and
      (not (csDesigning in ComponentState))) then
      BtnWidth := GetSystemMetrics(SM_CXSMSIZE) - 2;
    SrcRect.Left := SrcRect.Right - BtnWidth;
    if Btns <> [] then
      FBtnsRect := Rect(-1, R.Top, -1, R.Bottom);
    if biSystemMenu in Btns then
    begin
      FBtnsRect.Right := SrcRect.Right;
      FBtnsRect.Left := SrcRect.Left;
      if not CalcOnly then
        if not IsThemeApplied then
          DrawFrameControl(Canvas.Handle, SrcRect, DFC_CAPTION, DFCS_CAPTIONCLOSE);
      OffsetRect(SrcRect, -BtnWidth - 2, 0);
      Dec(R.Right, BtnWidth + 2);
    end;
    if biMaximize in Btns then
    begin
      if IsZoomed(FForm.Handle) then
        Flag := DFCS_CAPTIONRESTORE
      else
        Flag := DFCS_CAPTIONMAX;
      if not (biMaximize in FForm.BorderIcons) then
        Flag := Flag or DFCS_INACTIVE;
      if FBtnsRect.Left = -1 then
        FBtnsRect.Left := SrcRect.Left;
      if FBtnsRect.Right = -1 then
        FBtnsRect.Left := SrcRect.Right;
      if not CalcOnly then
        if not IsThemeApplied then
          DrawFrameControl(Canvas.Handle, SrcRect, DFC_CAPTION, Flag);
      OffsetRect(SrcRect, -BtnWidth, 0);
      Dec(R.Right, BtnWidth);
    end;
    if biMinimize in Btns then
    begin
      if IsIconic(FForm.Handle) then
        Flag := DFCS_CAPTIONRESTORE
      else
        Flag := DFCS_CAPTIONMIN;
      if not (biMinimize in FForm.BorderIcons) then
        Flag := Flag or DFCS_INACTIVE;
      if FBtnsRect.Left = -1 then
        FBtnsRect.Left := SrcRect.Left;
      if FBtnsRect.Right = -1 then
        FBtnsRect.Left := SrcRect.Right;
      if not CalcOnly then
        if not IsThemeApplied then
          DrawFrameControl(Canvas.Handle, SrcRect, DFC_CAPTION, Flag);
      OffsetRect(SrcRect, -BtnWidth, 0);
      Dec(R.Right, BtnWidth);
    end;
    if (biHelp in Btns) then
    begin
      if FBtnsRect.Left = -1 then
        FBtnsRect.Left := SrcRect.Left;
      if FBtnsRect.Right = -1 then
        FBtnsRect.Left := SrcRect.Right;
      if not CalcOnly then
        if not IsThemeApplied then
          DrawFrameControl(Canvas.Handle, SrcRect, DFC_CAPTION, DFCS_CAPTIONHELP);
      Dec(R.Right, BtnWidth);
    end;
    Dec(R.Right, 2);
    for i := 0 to FButtons.Count - 1 do // Iterate
    begin
      Button := FButtons[i];
      if (not Button.Visible) or (Button.Align = taCenter) then Continue;
      SrcRect.Left := 0;
      SrcRect.Right := Button.GetWidth(Canvas, SrcRect.Bottom - SrcRect.Top);

      if SrcRect.Right >= R.Right - R.Left then
        break
      else
      begin
        if Button.FAlign = taLeftJustify then
          OffsetRect(SrcRect, R.Left, 0)
        else
          OffsetRect(SrcRect, R.Right - SrcRect.Right, 0);
      end;
      Button.FBtnRect := SrcRect;
      Button.Paint(Canvas, SrcRect);
      if Button.FAlign = taLeftJustify then
        inc(R.Left, SrcRect.Right - SrcRect.Left)
      else
        dec(R.Right, SrcRect.Right - SrcRect.Left);
      if R.Left >= R.Right then
        break;
    end; // for
  end;

  procedure PaintText(Canvas: TCanvas; var R: TRect);
  var
    SrcRect: TRect;
    i, X   : integer;
    FOldStyle: TBrushStyle;
    Text: TElCaptionText;
    S : String;
  begin
    FOldStyle := Canvas.Brush.Style;
    Canvas.Brush.Style := bsClear;
    SrcRect := R;
    Inc(R.Left, 2);
    InflateRect(SrcRect, -2, -2);

    // draw left-aligned ones
    if Texts.Count = 0 then
    begin
      FPreventUpdate := true;
      Text := TElCaptionText.Create(FTexts);
      if (csDesigning in ComponentState) then
      begin
        SetLength(S, 1024);
        SendMessage(GetOwnerForm(Self).Handle, WM_GETTEXT, 1023, Integer(PChar(S)^));
        SetLength(S, StrLen(PChar(S)));
        Text.FCaption := S;
      end
      else
        Text.FCaption := GetOwnerForm(Self).Caption;
      SrcRect.Left := 0;
      SrcRect.Right := Min(Text.GetWidth(Canvas, R.Bottom - R.Top), R.Right - R.Left);
      OffsetRect(SrcRect, R.Left, 0);
      Text.Paint(Canvas, SrcRect);
      Text.Free;
      FPreventUpdate := false;
    end
    else
    for i := 0 to Texts.Count - 1 do
    begin
      Text := Texts[i];
      if (not Text.Visible) or (Text.Align <> taLeftJustify) then
        Continue;
      SrcRect.Left := 0;
      SrcRect.Right := Min(Text.GetWidth(Canvas, R.Bottom - R.Top), R.Right - R.Left);
      OffsetRect(SrcRect, R.Left, 0);
      Text.Paint(Canvas, SrcRect);
      //DrawText(Canvas.Handle, PChar(Text.Caption), -1, R, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
      R.Left := SrcRect.Right{ + 1};
      if R.Left >= R.Right then
        break;
    end;

    if R.Left < R.Right then
      // draw right-aligned ones
      for i := 0 to Texts.Count - 1 do
      begin
        Text := Texts[i];
        if (not Text.Visible) or (Text.Align <> taRightJustify) then
          Continue;
        SrcRect.Left := 0;
        X := Min(Text.GetWidth(Canvas, R.Bottom - R.Top), R.Right - R.Left);
        SrcRect.Right := R.Right;
        SrcRect.Left := Max(SrcRect.Right - X, R.Left);

        Text.Paint(Canvas, SrcRect);
        //DrawText(Canvas.Handle, PChar(Text.Caption), -1, R, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
        R.Right := SrcRect.Left - 1;
        if R.Left >= R.Right then
          break;
      end;
    if R.Left < R.Right then
    begin
      // draw centered ones
      X := 0;
      for i := 0 to Texts.Count - 1 do
      begin
        Text := Texts[i];
        if (not Text.Visible) or (Text.Align <> taCenter) then
          Continue;
        Inc(X, Text.GetWidth(Canvas, R.Bottom - R.Top));
      end;
      if (X > 0) and (X < R.Right - R.Left) then
      begin
        R.Left  := (R.Right - R.Left - X) div 2 + R.Left;
        R.Right := R.Left + X;
        for i := 0 to Texts.Count - 1 do
        begin
          Text := Texts[i];
          if (not Text.Visible) or (Text.Align <> taCenter) then
            Continue;
          SrcRect.Left := 0;
          SrcRect.Right := Min(Text.GetWidth(Canvas, R.Bottom - R.Top), R.Right - R.Left);
          OffsetRect(SrcRect, R.Left, 0);
          Text.Paint(Canvas, SrcRect);
          //DrawText(Canvas.Handle, PChar(Text.Caption), -1, R, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS);
          R.Left := SrcRect.Right + 1;
        end;
      end;
    end;

    Canvas.Brush.Style := FOldStyle;
  end;

  procedure PaintBackground(Canvas: TCanvas; var R: TRect);

    procedure FillTitle(R: TRect);
    var
      StartColor,
        EndColor: TColor;
      IsGrad: BOOL;
    begin
      if BackgroundType = bgtColorFill then
      begin
        if IsWin98 or IsWin2000 then
        begin
          SystemParametersInfo(SPI_GETGRADIENTCAPTIONS, 0, @IsGrad, 0);
          if IsGrad then
          begin
            if FWndActive then
            begin
              StartColor := GetSysColor(COLOR_ACTIVECAPTION);
              EndColor := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
            end
            else
            begin
              StartColor := GetSysColor(COLOR_INACTIVECAPTION);
              EndColor := GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
            end;
          end
          else
          begin
            if FWndActive then
            begin
              StartColor := clActiveCaption;
              EndColor := clActiveCaption;
            end
            else
            begin
              StartColor := clInActiveCaption;
              EndColor := clInActiveCaption;
            end;
          end;
        end
        else
        begin
          if FWndActive then
          begin
            StartColor := clActiveCaption;
            EndColor := clActiveCaption;
          end
          else
          begin
            StartColor := clInActiveCaption;
            EndColor := clInActiveCaption;
          end;
        end;
      end
      else
        if (((PaintBkgnd = pbtActive) or (PaintBkgnd = pbtAlways)) and (FWndActive))
          and (NumColors <> 1) then
        begin
          StartColor := FActiveLeftColor;
          EndColor := FActiveRightColor;
        end
        else
          if (((PaintBkgnd = pbtInactive) or (PaintBkgnd = pbtAlways)) and (not FWndActive))
            and (NumColors <> 1) then
          begin
            StartColor := FInActiveLeftColor;
            EndColor := FInActiveRightColor;
          end
          else
          begin
            if FWndActive then
            begin
              StartColor := clActiveCaption;
              EndColor := clActiveCaption;
            end
            else
            begin
              StartColor := clInActiveCaption;
              EndColor := clInActiveCaption;
            end;
          end;
      if StartColor = EndColor then
      begin
        Canvas.Brush.Color := StartColor;
        Canvas.FillRect(R);
      end
      else
        GradientFill(Canvas.Handle, R, StartColor, EndColor, FNumColors, BackgroundType = bgtVertGradient);
    end;

  begin
    if (UseXPThemes and ThemesAvailable and (FTheme <> 0)) then
      exit; 
    if (BackgroundType in [bgtTileBitmap, bgtStretchBitmap, bgtCenterBitmap]) and
      ((FWndActive and FBitmap.Empty) or ((not FWndActive) and FInactiveBitmap.Empty)) then
      FillTitle(R)
    else
      case BackgroundType of
        bgtColorFill,
          bgtCenterBitmap:
          if not (UseXPThemes and ThemesAvailable and (FTheme <> 0)) then
            FillTitle(R);
        bgtTileBitmap:
          begin
            if FWndActive then
              TiledPaint(Canvas, FBitmap, R)
            else
              TiledPaint(Canvas, FInactiveBitmap, R);
          end;
      {bgtCenterBitmap:
        begin
          Canvas.FillRect(R);
          if FWndActive then
          begin
            CenterRects(FBitmap.Width, R.Right - R.Left,
                        FBitmap.Height, R.Bottom - R.Top, SrcRect);
            OffsetRect(SrcRect, R.Left, R.Top);
            Canvas.CopyRect(SrcRect, FInactiveBitmap.Canvas, Rect(SrcRect.Left, ));
          end else
          begin
            CenterRects(FInactiveBitmap.Width, R.Right - R.Left,
                        FInactiveBitmap.Height, R.Bottom - R.Top, SrcRect);
          end;
          Canvas.CopyRect(SrcRect, FInactiveBitmap.Canvas,
            Rect(0, 0, FInactiveBitmap.Width, FInactiveBitmap.Height));
        end;}
        bgtStretchBitmap:
          if FWndActive then
            Canvas.CopyRect(R, FBitmap.Canvas, Rect(0, 0, FBitmap.Width, FBitmap.Height))
          else
            Canvas.CopyRect(R, FInactiveBitmap.Canvas,
              Rect(0, 0, FInactiveBitmap.Width, FInactiveBitmap.Height));
      else
        FillTitle(R);
      end;
  end;

  function CalcCaptionRect: TRect;
  begin
    GetWindowRect(FForm.Handle, Result);
    OffsetRect(Result, -Result.Left, -Result.Top);
    begin
      if IsIconic(FForm.Handle) then
        InflateRect(Result, -GetSystemMetrics(SM_CXFIXEDFRAME), -GetSystemMetrics(SM_CYFIXEDFRAME))
      else
        if not (csDesigning in ComponentState) then
          case FForm.BorderStyle of
            bsToolWindow,
              bsSingle,
              bsDialog:
              InflateRect(Result, -GetSystemMetrics(SM_CXFIXEDFRAME), -GetSystemMetrics(SM_CYFIXEDFRAME));
            bsSizeToolWin,
              bsSizeable:
              InflateRect(Result, -GetSystemMetrics(SM_CXSIZEFRAME), -GetSystemMetrics(SM_CYSIZEFRAME));
          end
        else
          InflateRect(Result, -GetSystemMetrics(SM_CXSIZEFRAME), -GetSystemMetrics(SM_CYSIZEFRAME));
      if (FForm.BorderStyle in [bsToolWindow, bsSizeToolWin]) and (not (csDesigning in ComponentState)) then
        Result.Bottom := Result.Top + GetSystemMetrics(SM_CYSMCAPTION) - 1
      else
        Result.Bottom := Result.Top + GetSystemMetrics(SM_CYCAPTION) - 1;
    end;
  end;

var
  Rect,
    Rect1: TRect;
  FCanvas: TCanvas;
  BmpDC: HDC;
  i: integer;
  BMP: TBitmap;
  WR: TRect;

begin
  if (FForm.BorderStyle = bsNone) and (not (csdesigning in ComponentState)) then exit;
  FCanvas := TCanvas.Create;
  FCanvas.Handle := GetWindowDC(FForm.Handle);
  Rect := CalcCaptionRect;
  if Step = 2 then
  begin
    Bmp := TBitmap.Create;
    Rect1 := Rect;
    OffsetRect(Rect1, -Rect1.Left, -Rect1.Top);
    BmpDC := CreateCompatibleBitmap(FCanvas.Handle, Rect1.Right, Rect1.Bottom);
    Bmp.Handle := BmpDC;
    if FRegion <> 0 then
    begin
      Msg.WParam := Longint(FSaveRegion);
      DeleteObject(FRegion);
      FRegion := 0;
    end;
    if IsThemeApplied then
    begin
      BitBlt(Bmp.Canvas.Handle, 0, 0, Rect1.Right, Rect1.Bottom, FCanvas.Handle, Rect.Left, Rect.Top, SRCCOPY);
    end;
    PaintBackGround(Bmp.Canvas, Rect1);
    PaintButtons(Bmp.Canvas, Rect1, false);
    for i := 0 to Buttons.Count - 1 do
      OffsetRect(Buttons[i].FBtnRect, Rect.Left, Rect.Top);
    PaintText(Bmp.Canvas, Rect1);
    TriggerDrawCaptionEvent(Bmp.Canvas, Rect1);
    BitBlt(FCanvas.Handle, Rect.Left, Rect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    Bmp.Free;
  end
  else
  begin
    GetWindowRect(FForm.Handle, WR);
    if not IsThemeApplied then
    begin
      FRegion := CreateRectRgnIndirect(WR);
      if SelectClipRgn(FCanvas.Handle, HRGN(Msg.wParam)) = error then
        SelectClipRgn(FCanvas.Handle, FRegion);
      OffsetClipRgn(FCanvas.Handle, -WR.Left, -WR.Top);
      ExcludeClipRect(FCanvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      OffsetClipRgn(FCanvas.Handle, WR.Left, WR.Top);
      GetClipRgn(FCanvas.Handle, FRegion);
      FSaveRegion := HRGN(Msg.WParam);
      Msg.WParam := Longint(FRegion);
    end
    else
      FRegion := 0;
  end;
  ReleaseDC(FForm.Handle, FCanvas.Handle);
  FCanvas.Handle := 0;
  FCanvas.Free;
end; {PaintCaption}

procedure TElFormCaption.SetPaintBkgnd(newValue: TElPaintBkgndType);
begin
  if (FPaintBkgnd <> newValue) then
  begin
    FPaintBkgnd := newValue;
    Update;
  end; {if}
end; {SetPaintBkgnd}

procedure TElFormCaption.SetInactiveRightColor(newValue: TColor);
begin
  if (FInactiveRightColor <> newValue) then
  begin
    FInactiveRightColor := newValue;
    Update;
  end; {if}
end; {SetInactiveRightColor}

procedure TElFormCaption.SetButtons(newValue: TElCaptionButtons);
begin
  FButtons.Assign(newValue);
end; {SetButtons}

procedure TElFormCaption.SetInactiveBitmap(newValue: TBitmap);
begin
  FInactiveBitmap.Assign(newValue);
end; {SetInactiveBitmap}

procedure TElFormCaption.FontChange(Sender: TObject);
begin
  Update;
end;

procedure TElFormCaption.GetSystemFont;
var
  NCM: TNonClientMetrics;
  FFontHandle: HFont;
begin
  NCM.cbSize := SizeOf(NCM);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCM, 0) then
  begin
    if (FForm.BorderStyle in [bsToolWindow, bsSizeToolWin]) and (not (csDesigning in ComponentState)) then
      FFontHandle := CreateFontIndirect(NCM.lfSmCaptionFont)
    else
      FFontHandle := CreateFontIndirect(NCM.lfCaptionFont);
  end
  else
    FFontHandle := 0;
  Font2.Handle := FFontHandle;
end;

procedure TElFormCaption.SetFont;
begin
  FFont.Assign(newValue);
end;

procedure TElFormCaption.TriggerDrawCaptionEvent(Canvas: TCanvas; var Rect:
  TRect);
begin
  if (assigned(FOnDrawCaption)) then FOnDrawCaption(Self, Canvas, Rect);
end;

function TElFormCaption.ButtonAtPos(X, Y: integer): TElCaptionButton;
var
  i: integer;
  P, P1: TPoint;
begin
  result := nil;
  P := Point(X, Y);
  for i := 0 to Buttons.Count - 1 do // Iterate
  begin
    P1 := P;
    Dec(P1.X, FForm.Left);
    Dec(P1.Y, FForm.Top);
    if PtInRect(Buttons[i].FBtnRect, P1) then
    begin
      result := Buttons[i];
      exit;
    end;
  end; // for
end; {ButtonAtPos}

procedure TElFormCaption.TriggerButtonClickEvent(Button: TElCaptionButton);
begin
  if (assigned(FOnButtonClick)) then FOnButtonClick(Self, Button);
  if (assigned(Button.FOnClick)) then Button.FOnClick(Button);
end;

procedure TElFormCaption.TriggerButtonDblClickEvent(Button: TElCaptionButton);
begin
  if (assigned(FOnButtonDblClick)) then FOnButtonDblClick(Self, Button);
  if (assigned(Button.FOnDblClick)) then Button.FOnDblClick(Button);
end;

procedure TElFormCaption.SetSystemFont(newValue: Boolean);
begin
  if (FSystemFont <> newValue) then
  begin
    FSystemFont := newValue;
    Update;
  end; {if}
end;

procedure TElFormCaption.SetTexts(newValue: TElCaptionTexts);
begin
  if (FTexts <> newValue) then FTexts.Assign(newValue);
end;

destructor TElFormCaption.Destroy;
begin
  FTexts.Free;
  FButtons.Free;
  FBitmap.Free;
  FFont.Free;
  Font2.Free;
  FInactiveBitmap.Free;
  FHook.Free;
  inherited Destroy;
end; {Destroy}

constructor TElFormCaption.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHook := TElHook.Create(Self);
  FForm := GetOwnerForm(Self);
  with FHook do
  begin
    Control := FForm;
    Active := false;
    DesignActive := true;
    OnBeforeProcess := OnBeforeHook;
    OnAfterProcess := OnAfterHook;
  end; {FHook}
  FNumColors := 64;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChange;
  FActiveRightColor := clActiveCaption;
  FInactiveRightColor := clInactiveCaption;
  FBackgroundType := bgtColorFill;
  FAlignment := taLeftJustify;
  FPaintBkgnd := pbtAlways;
  FInactiveRightColor := clInactiveCaption;
  FButtons := TElCaptionButtons.Create(TElCaptionButton);
  FButtons.FCaption := Self;
  FTexts := TElCaptionTexts.Create(TElCaptionText);
  FTexts.FCaption := Self;
  FInactiveBitmap := TBitmap.Create;
  FInactiveBitmap.OnChange := BitmapChange;
  FSystemFont := true;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  Font2 := TFont.Create;
  GetSystemFont;
  FUseXPThemes := true;
end; {Create}

procedure TElFormCaption.Assign(Source : TPersistent);
begin
  if Source is TElformCaption then
  begin
    Self.FTexts.Assign(TElformCaption(Source).FTexts);
    Self.FInactiveBitmap.Assign(TElformCaption(Source).FInactiveBitmap);
    Self.FButtons.Assign(TElformCaption(Source).FButtons);
    Self.FPaintBkgnd := TElformCaption(Source).FPaintBkgnd;
    Self.FBitmap.Assign(TElFormCaption(Source).FBitmap);
    Self.FInactiveBitmap.Assign(TElFormCaption(Source).FInactiveBitmap);
    Self.FActiveLeftColor := TElFormCaption(Source).FActiveLeftColor;
    Self.FInActiveLeftColor := TElFormCaption(Source).FInActiveLeftColor;
    Self.FActiveRightColor := TElFormCaption(Source).FActiveRightColor;
    Self.FInActiveRightColor := TElFormCaption(Source).FInActiveRightColor;
    Self.FBackgroundType := TElFormCaption(Source).FBackgroundType;
    Self.FNumColors := TElFormCaption(Source).FNumColors;
    Self.FAlignment := TElFormCaption(Source).FAlignment;
    Self.FSystemFont:= TElFormCaption(Source).SystemFont;
    Self.Font.Assign(TElFormCaption(Source).Font);
  end
  else
    inherited;
end;

procedure TElFormCaption.SetUseXPThemes(Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    if ThemesAvailable then
      Update;
    //FForm.RecreateWnd;
  end;
end;

procedure TElFormCaption.CreateThemeHandle;
begin
  if ThemesAvailable then
    FTheme := OpenThemeData(FForm.Handle, 'WINDOW')
  else
    FTheme := 0;
end;

procedure TElFormCaption.FreeThemeHandle;
begin
  if ThemesAvailable then
    CloseThemeData(FTheme);
  FTheme := 0;
end;

function TElFormCaption.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0);
end;

procedure TElFormCaption.AllocThemes;
begin
  if ThemesAvailable and (FTheme = 0) then
    CreateThemeHandle; 
end;

(*
procedure TElFormCaption.TriggerMeasureCaptionPart(Sender : TObject; Canvas :
    TCanvas; var Height : integer; var Width : integer);
begin
end;
*)

end.

