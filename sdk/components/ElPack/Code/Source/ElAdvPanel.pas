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

{$r ElAdvPanel.res}

(*

Version History

12/27/2001

  Now one can correctly set glyphs for Minimize button -- 2 images in one bitmap
  in format suitable for imagelist
  
11/22/2001

  Link* properties now change settings for caption too

10/29/2001

  Fixed the problem with incorrect OnRestore event invokation when the panel is resized.

10/27/2001

  InvertMinButtonArrows property now does really work correctly when the for is just loade :)

10/25/2001

  When panel is saved as minimized, there was no way to restore it in run-time. 
  Fixed.

10/21/2001

  Clicks on caption links were not processed. Fixed.

10/19/2001

  InvertMinButtonArrows property now works correctly when the for is just loaded.

10/06/2001

  InvertMinButtonArrows property added. 
  Caption rectangle is inflated by BevelWidth size now

09/26/2001

  Fixed caption sizing when bevel size is > 0.

09/17/2001

  Default caption alignment taCenter was not set properly. Fixed.
  OnClose event was mapped to OnRestore event handler. Fixed.

*)

unit ElAdvPanel;

interface

uses

  SysUtils,
  Classes,

  ElPanel,
  ElPopBtn,
  ElVCLUtils,
  ElHTMLPanel,
  ElTools,
  ElImgFrm,
  ElStrUtils,
  ElUxTheme,
  ElTmSchema,

  {$ifdef VCL_4_USED}
  ImgList,
  {$endif}
  Windows,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
  Menus,
  ExtCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  Messages;

type

  TCustomElAdvancedPanel = class;
  TElAdvCaptionButton = class;
  TElAdvCaptionPanel = class;
  
  TElPanelCaptionSettings = class(TPersistent)
  private
    FVisible: Boolean;
    FHeight: Integer;
    FShowCloseButton: Boolean;
    FOwner: TCustomElAdvancedPanel;
    FShowMinButton: Boolean;
    FFlatButtons: Boolean;
    FButtonWidth: Integer;
    FAlignment: TAlignment;
    FLayout: TTextLayout;
    FButtonColor: TColor;
    procedure SetText(Value: TElFString);
    procedure SetVisible(Value: Boolean);
    procedure SetHeight(Value: Integer);
    procedure SetShowCloseButton(Value: Boolean);
    procedure SetShowMinButton(Value: Boolean);
    procedure SetFlatButtons(Value: Boolean);
    function GetText: TElFString;
    procedure SetButtonWidth(Value: Integer);
    procedure SetAlignment(Value: TAlignment);
    procedure SetLayout(newValue: TTextLayout);
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function GetParentFont: Boolean;
    procedure SetParentFont(Value: Boolean);
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    procedure SetButtonColor(Value: TColor);
    function GetImageForm: TElImageForm;
    procedure SetImageForm(Value: TElImageForm);
  protected
    FInvertMinButtonArrows: Boolean;
    FAutoSize: Boolean;
    function GetMinButtonGlyph: TBitmap;
    procedure SetMinButtonGlyph(Value: TBitmap);
    function GetCloseButtonGlyph: TBitmap;
    procedure SetCloseButtonGlyph(Value: TBitmap);
    procedure SetInvertMinButtonArrows(Value: Boolean);
    procedure SetAutoSize(Value: Boolean);
    procedure FontChanged;
    procedure AdjustHeight;
  public
    constructor Create(Owner : TCustomElAdvancedPanel);
  published
    property Text: TElFString read GetText write SetText;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Height: Integer read FHeight write SetHeight default 19;
    property ShowCloseButton: Boolean read FShowCloseButton write
        SetShowCloseButton nodefault;
    property ShowMinButton: Boolean read FShowMinButton write SetShowMinButton default true;
    property FlatButtons: Boolean read FFlatButtons write SetFlatButtons default true;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 15;
    property Alignment: TAlignment read FAlignment write SetAlignment default
        taCenter;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property Font: TFont read GetFont write SetFont;
    property ParentFont: Boolean read GetParentFont write SetParentFont;
    property Color: TColor read GetColor write SetColor;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default
        clBtnFace;
    property ImageForm: TElImageForm read GetImageForm write SetImageForm;
    property MinButtonGlyph: TBitmap read GetMinButtonGlyph write SetMinButtonGlyph;
    property CloseButtonGlyph: TBitmap read GetCloseButtonGlyph write
        SetCloseButtonGlyph;
    property InvertMinButtonArrows: Boolean read FInvertMinButtonArrows write
        SetInvertMinButtonArrows default false;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
  end;

  TCustomElAdvancedPanel = class(TCustomElHTMLPanel)
  private
    procedure SetCaptionSettings(Value: TElPanelCaptionSettings);
    procedure WMNCHitTest(var Msg : TMessage); message WM_NCHITTEST;
  protected
    FMinButtonGlyph: TBitmap;
    FImages     : TImageList;
    FSaveHeight : Integer;
    FMinimized: Boolean;
    FCaptionPanel: TElHTMLPanel;
    FCaptionSettings: TElPanelCaptionSettings;
    FMinButton: TElGraphicButton;
    FCloseButton: TElGraphicButton;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnClose: TNotifyEvent;

    procedure SetLinkPopupMenu(newValue : TPopupMenu); override;
    procedure SetLinkColor(newValue : TColor); override;
    procedure SetLinkStyle(newValue : TFontStyles); override;

    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure TriggerMinimizeEvent; virtual;
    procedure TriggerRestoreEvent; virtual;
    procedure AdjustButtonSize;
    procedure OnCaptionSize(Sender : TObject);
    procedure OnMinButtonClick(Sender : TObject);
    procedure OnCloseButtonClick(Sender : TObject);
    procedure TriggerCloseEvent; virtual;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreateWnd; override;
    procedure LinkClickEventTransfer(Sender : TObject; HRef : TElFString);
    procedure SetImageForm(newValue : TElImageForm); override;
    procedure SetUseXPThemes(const Value: Boolean); override;
    procedure AdjustInnerSize(var R : TRect); virtual;
    function GetBevelAdjustment: Integer;
    procedure UpdateMinButtonImages;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadExpHeight(Reader : TReader);
    procedure WriteExpHeight(Writer : TWriter);
    procedure SetTransparentXPThemes(Value: Boolean); override;
    function CreateButton: TElAdvCaptionButton; virtual;
    procedure SetMinimized(Value: Boolean); virtual;
    function CreatePanel: TElAdvCaptionPanel; virtual;
    function GetThemePartID: Integer; virtual;
    procedure UpdateInterior; override;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;

    property Minimized: Boolean read FMinimized write SetMinimized default false;
    property CaptionSettings: TElPanelCaptionSettings read FCaptionSettings write SetCaptionSettings;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetCaptionHeight: Integer; virtual;
    function GetButtonWidth: Integer; virtual;
  end;

  TElAdvancedPanel = class(TCustomElAdvancedPanel)
  published
    property OnImageNeeded;
    property OnLinkClick;
    property Cursor;
    property LinkColor;
    property LinkPopupMenu;
    property LinkStyle;

    property Background;
    property BackgroundType;
    property GradientEndColor;
    property GradientStartColor;
    property GradientSteps;
    property Alignment;
    property Layout;
    property ImageForm;
    property TopGrabHandle;
    property RightGrabHandle;
    property LeftGrabHandle;
    property BottomGrabHandle;

    property Resizable;
    property Movable;
    property OnMove;

    property Align nodefault;
    property BevelInner nodefault;
    property BevelOuter nodefault;
    property BevelSpaceColor nodefault;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Canvas;
    property Color;
{$ifdef MSWINDOWS}
    property DragCursor;
{$endif}
    property DragMode;
    property Enabled;
    property Font;
{$ifdef MSWINDOWS}
    property Locked;
    property MouseCapture;
{$endif}
    property ParentColor;
{$ifdef MSWINDOWS}
    property ParentCtl3D;
{$endif}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Transparent;
    property TransparentXPThemes;
    property UseXPThemes;
    property Visible;
    property SizeGrip;

    property Caption;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
{$ifdef MSWINDOWS}
    property DockOrientation;
    property Floating;
    property BevelKind;
    property DoubleBuffered;
    property DragKind;
{$endif}
{$ENDIF}

    property Minimized;
    property CaptionSettings;
    property OnMinimize;
    property OnRestore;
    property OnClose;
  end;

  TElAdvCaptionPanel = class(TElHTMLPanel)
  private
    FDummyInt : integer;
    FDummyBool: boolean;
    procedure WMNCHitTest(var Msg : TMessage); message WM_NCHITTEST;
  protected
    procedure CreateWnd; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure SetImageForm(newValue : TElImageForm); override;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); 
        override;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property TabStop : boolean write FDummyBool;
    property TabOrder: integer write FDummyInt;
  end;

  TElAdvCaptionButton = class(TElGraphicButton)
  protected
    procedure DrawThemedBackground(Canvas : TCanvas); override;
  public
    constructor Create(AOwner : TComponent); override;
  end;

implementation

var
  CloseBmp,
  UpArrowBmp,
  DownArrowBmp : TBitmap;

const Margin = 1;

type

  THackGraphicButton = TElAdvCaptionButton;

procedure TElAdvCaptionPanel.CreateWnd;
begin
  inherited;
  THackGraphicButton(TCustomElAdvancedPanel(Parent).FMinButton).SetDesigning(false);
  THackGraphicButton(TCustomElAdvancedPanel(Parent).FCloseButton).SetDesigning(false);
end;

procedure TElAdvCaptionPanel.AdjustClientRect(var Rect : TRect);
var ASettings : TElPanelCaptionSettings;
begin
  inherited;
  ASettings := TCustomElAdvancedPanel(Parent).FCaptionSettings;

  if ASettings.FShowMinButton then
    dec(Rect.Right, Margin + TCustomElAdvancedPanel(Parent).GetButtonWidth);
  if ASettings.FShowCloseButton then
    dec(Rect.Right, Margin + TCustomElAdvancedPanel(Parent).GetButtonWidth);
  InflateRect(Rect, - Margin * 2, 0);
end;

procedure TElAdvCaptionPanel.WMNCHitTest(var Msg : TMessage);
begin
  inherited;
  if TCustomElAdvancedPanel(Parent).Movable or (csDesigning in Parent.ComponentState) then
    Msg.Result := HTTRANSPARENT;
end; {WMNCHitTest}

procedure TElAdvCaptionPanel.SetImageForm(newValue : TElImageForm);
begin
  inherited;
  if Parent = nil then exit;
  if TCustomElAdvancedPanel(Parent).FCloseButton <> nil then
    TCustomElAdvancedPanel(Parent).FCloseButton.ImageForm := newValue;
  if TCustomElAdvancedPanel(Parent).FMinButton <> nil then
    TCustomElAdvancedPanel(Parent).FMinButton.ImageForm := newValue;
end;

constructor TElAdvCaptionPanel.Create(AOwner : TComponent);
begin
  inherited;
  BevelInner   := bvNone;
  BevelOuter   := bvNone;
  BorderStyle  := bsNone;

  ShowFocus    := true;

  ControlStyle := [csClickEvents];
end;

procedure TElAdvCaptionPanel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  TElAdvancedPanel(Parent).FCaptionSettings.FontChanged;
end;

procedure TElAdvCaptionPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and (Shift = []) and
     TElAdvancedPanel(Parent).FCaptionSettings.ShowMinButton then
  begin
    TElAdvancedPanel(Parent).Minimized := not TElAdvancedPanel(Parent).Minimized;
    Key := 0;
  end;
  inherited;
end;

type THackForm = class(TForm);


procedure TElAdvCaptionPanel.WMKillFocus(var Message: TMessage);
var CurControl : TWinControl;
begin
  inherited;
  if TElAdvancedPanel(Parent).Minimized then
  begin
    Curcontrol := self;
    repeat
      CurControl := THackForm(GetParentForm(Self)).FindNextControl(Curcontrol, true, true, false);
    until (CurControl = nil) or (CurControl.Parent <> Parent);
    if CurControl <> nil then
      CurControl.SetFocus;
  end;
end;

procedure TElAdvCaptionPanel.WMSetFocus(var Message: TMessage);
var CurControl : TWinControl;
begin
  inherited;
  if not TElAdvancedPanel(Parent).FCaptionSettings.ShowMinButton then
  begin
    Curcontrol := THackForm(GetParentForm(Self)).FindNextControl(Self, true, true, false);
    if (CurControl <> nil) and (TWMSetFocus(Message).FocusedWnd = CurControl.Handle) then
      THackForm(GetParentForm(Self)).SelectNext(Parent, false, true)
    else
      THackForm(GetParentForm(Self)).SelectNext(Self, true, true)
  end;
end;

procedure TElAdvCaptionPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;
end;

procedure TElPanelCaptionSettings.SetText(Value: TElFString);
begin
  FOwner.FCaptionPanel.Caption := Value;
end;

procedure TElPanelCaptionSettings.SetVisible(Value: Boolean);
var R : TRect;
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if not Visible and FOwner.Minimized then
      FOwner.Minimized := false;
    FOwner.FCaptionPanel.Visible := Value;
    R := FOwner.ClientRect;
    FOwner.AlignControls(nil, R);
    FOwner.Invalidate;
  end;
end;

procedure TElPanelCaptionSettings.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if FOwner.FMinimized then
      FOwner.Height := Value;
    FOwner.FCaptionPanel.Height := Value;
    FOwner.Invalidate;
  end;
end;

procedure TElPanelCaptionSettings.SetShowCloseButton(Value: Boolean);
begin
  if FShowCloseButton <> Value then
  begin
    FShowCloseButton := Value;
    FOwner.FCloseButton.Visible := Value;
    FOwner.AdjustButtonSize;
    FOwner.FCaptionPanel.Invalidate;
  end;
end;

procedure TElPanelCaptionSettings.SetShowMinButton(Value: Boolean);
begin
  if FShowMinButton <> Value then
  begin
    FShowMinButton := Value;
    FOwner.FMinButton.Visible := Value;
    FOwner.AdjustButtonSize;
    FOwner.FCaptionPanel.Invalidate;
  end;
end;

procedure TElPanelCaptionSettings.SetFlatButtons(Value: Boolean);
begin
  if FFlatButtons <> Value then
  begin
    FFlatButtons := Value;
    FOwner.FMinButton.Flat := Value;
    FOwner.FCloseButton.Flat := Value;
  end;
end;

function TElPanelCaptionSettings.GetText: TElFString;
begin
  Result := FOwner.FCaptionPanel.Caption;
end;

constructor TElPanelCaptionSettings.Create(Owner : TCustomElAdvancedPanel);
begin
  inherited Create;
  FOwner := Owner;
  FVisible := true;
  FHeight := 19;
  FButtonWidth := 15;
  FShowCloseButton := true;
  FShowMinButton := true;
  FFlatButtons := true;
  FButtonColor := clBtnFace;
  FAlignment := taCenter;
end;

procedure TElPanelCaptionSettings.SetButtonWidth(Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    if AutoSize then
      AdjustHeight;
    FOwner.AdjustButtonSize;
  end;
end;

procedure TElPanelCaptionSettings.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  FOwner.FCaptionPanel.Alignment := Value;
end;

procedure TElPanelCaptionSettings.SetLayout(newValue: TTextLayout);
{ Sets data member FLayout to newValue. }
begin
  if (FLayout <> newValue) then
  begin
    FLayout := newValue;
    FOwner.FCaptionPanel.Layout := newValue;
  end;  { if }
end;  { SetLayout }

function TElPanelCaptionSettings.GetFont: TFont;
begin
  Result := FOwner.FCaptionPanel.Font;
end;

procedure TElPanelCaptionSettings.SetFont(Value: TFont);
begin
  FOwner.FCaptionPanel.Font := Value;
end;

function TElPanelCaptionSettings.GetParentFont: Boolean;
begin
  Result := FOwner.FCaptionPanel.ParentFont;
end;

procedure TElPanelCaptionSettings.SetParentFont(Value: Boolean);
begin
  FOwner.FCaptionPanel.ParentFont := Value;
end;

function TElPanelCaptionSettings.GetColor: TColor;
begin
  Result := FOwner.FCaptionPanel.Color;
end;

procedure TElPanelCaptionSettings.SetColor(Value: TColor);
begin
  FOwner.FCaptionPanel.Color := Value;
end;

procedure TElPanelCaptionSettings.SetButtonColor(Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    FOwner.FMinButton.Color := FButtonColor;
    FOwner.FCloseButton.Color := FButtonColor;
  end;
end;

function TElPanelCaptionSettings.GetImageForm: TElImageForm;
begin
  Result := FOwner.FCaptionPanel.ImageForm;
end;

procedure TElPanelCaptionSettings.SetImageForm(Value: TElImageForm);
begin
  FOwner.FCaptionPanel.ImageForm := Value;
end;

function TElPanelCaptionSettings.GetMinButtonGlyph: TBitmap;
begin
  FOwner.FMinButtonGlyph.Width := 0;
  FOwner.FMinButtonGlyph.Width := FOwner.FImages.Width * 2;
  FOwner.FMinButtonGlyph.Height := FOwner.FImages.Height;
                  
  FOwner.FImages.Draw(FOwner.FMinButtonGlyph.Canvas, 0, 0, 0);
  FOwner.FImages.Draw(FOwner.FMinButtonGlyph.Canvas, FOwner.FImages.Width, 0, 1);
  
  Result := FOwner.FMinButtonGlyph;
end;

procedure TElPanelCaptionSettings.SetMinButtonGlyph(Value: TBitmap);
begin
  FOwner.FMinButtonGlyph.Assign(Value);
  FOwner.FImages.Clear;
  if not Value.Empty then
  begin
    FOwner.FImages.Width := Value.Width div 2;
    FOwner.FImages.Height := Value.Height;
    
    FOwner.FImages.AddMasked(Value, Value.Canvas.Pixels[0, Value.Height - 1]);
  end;
end;

function TElPanelCaptionSettings.GetCloseButtonGlyph: TBitmap;
begin
  Result := FOwner.FCloseButton.Glyph;
end;

procedure TElPanelCaptionSettings.SetCloseButtonGlyph(Value: TBitmap);
begin
  FOwner.FCloseButton.Glyph := Value;
end;

procedure TElPanelCaptionSettings.SetInvertMinButtonArrows(Value: Boolean);
begin
  if FInvertMinButtonArrows <> Value then
  begin
    FInvertMinButtonArrows := Value;
    if not (csLoading in FOwner.ComponentState) then
      FOwner.UpdateMinButtonImages;
  end;
end;

procedure TElPanelCaptionSettings.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    if AutoSize then
      AdjustHeight;
  end;
end;

procedure TElPanelCaptionSettings.FontChanged;
begin
  if AutoSize then AdjustHeight;
end;

procedure TElPanelCaptionSettings.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);

  Height := Max(Metrics.tmHeight, FButtonWidth) + Margin * 2;
end;


procedure TCustomElAdvancedPanel.SetMinimized(Value: Boolean);
//var R : TRect;
begin
  if FMinimized <> Value then
  begin
    if FCaptionSettings.Visible or Value then
    begin
      if Value then
      begin
        FSaveHeight := Height;
        {
        R := ClientRect;
        inherited AdjustClientRect(R);
        }
        FMinimized := Value;
        Height := GetCaptionHeight + GetBevelAdjustment;
        TriggerMinimizeEvent;
      end
      else
      begin
        FMinimized := Value;
        Height := FSaveHeight;
        TriggerRestoreEvent;
      end;

      UpdateMinButtonImages;
      Realign;
    end;
  end;
end;

procedure TCustomElAdvancedPanel.WMSize(var Message: TMessage);
var R : TRect;
begin
  inherited;
  R := ClientRect;
  inherited AdjustClientRect(R);
  AdjustInnerSize(R);

  if Minimized and (Height <> GetCaptionHeight + (*R.Top + (Height - R.Bottom) + *)GetBevelAdjustment) then
  if not (csDesigning in ComponentState) then 
  begin
    FMinimized := false;
    FSaveHeight := Height;
    UpdateMinButtonImages;
    TriggerRestoreEvent;
  end;
end;

procedure TCustomElAdvancedPanel.SetCaptionSettings(Value: TElPanelCaptionSettings);
begin
  FCaptionSettings.Assign(Value);
end;

constructor TCustomElAdvancedPanel.Create(AOwner : TComponent);
begin
  inherited;
  BevelInner := bvNone;
  BevelOuter := bvNone;

  FCaptionSettings := TElPanelCaptionSettings.Create(Self);
  FCaptionPanel := CreatePanel;
  FCaptionPanel.Parent := Self;
  FCaptionPanel.TabStop := true;
  FCaptionPanel.OnImageNeeded := TriggerImageNeededEvent;
  FCaptionPanel.OnLinkClick := LinkClickEventTransfer;

  FMinButton := CreateButton;
  FCloseButton := CreateButton;
  FMinButton.Parent := FCaptionPanel;
  FMinButton.OnClick := OnMinButtonClick;
  FMinButton.Flat := true;
  FMinButton.ThinFrame := true;
  FCloseButton.Parent := FCaptionPanel;
  FCloseButton.OnClick := OnCloseButtonClick;
  FCloseButton.Flat := true;
  FCloseButton.ThinFrame := true;

  FCaptionPanel.OnResize := OnCaptionSize;
  FCaptionPanel.Height := 19;

  FImages := TImageList.Create(Self);
  FImages.Width := 7;
  FImages.Height := 5;
  FCloseButton.Glyph := CloseBmp;
  FImages.AddMasked(UpArrowBmp, clWhite);
  FImages.AddMasked(DownArrowBmp, clWhite);
  FMinButton.Images := FImages;
  FMinButton.UseImageList := true;
  FMinButton.ImageIndex := 0;

  FMinButtonGlyph := TBitmap.Create;
                          
  FCaptionPanel.BoundsRect := Rect(0, 0, Width, GetCaptionHeight);
  AdjustButtonSize;
end;

destructor TCustomElAdvancedPanel.Destroy;
begin
  FCloseButton.Free;
  FCloseButton := nil;
  FMinButton.Free;
  FMinButton := nil;
  FCaptionSettings.Free;
  FCaptionSettings := nil;
  FMinButtonGlyph.Free;
  FMinButtonGlyph := nil;
  inherited;
end;

procedure TCustomElAdvancedPanel.TriggerMinimizeEvent;
begin
  if assigned(FOnMinimize) then FOnMinimize(Self);
end;

procedure TCustomElAdvancedPanel.TriggerRestoreEvent;
begin
  if assigned(FOnRestore) then FOnRestore(Self);
end;

procedure TCustomElAdvancedPanel.OnMinButtonClick(Sender : TObject);
begin
  if not (csDesigning in ComponentState) then
    Minimized := not Minimized;
end;

procedure TCustomElAdvancedPanel.OnCloseButtonClick(Sender : TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    TriggerCloseEvent;
    Hide;
  end;
end;

procedure TCustomElAdvancedPanel.OnCaptionSize(Sender : TObject);
begin
  AdjustButtonSize;
end;

procedure TCustomElAdvancedPanel.AdjustButtonSize;
var R : TRect;
    bw : integer;
begin
  bw := GetButtonWidth;
  R := Rect(FCaptionPanel.Width - bw - Margin, Margin,
            FCaptionPanel.Width - Margin, Margin + bw);
  FCloseButton.BoundsRect := R;

  R := Rect(FCaptionPanel.Width - bw - Margin, Margin,
            FCaptionPanel.Width - Margin, Margin + bw);

  if FCaptionSettings.ShowCloseButton then
    OffsetRect(R, - (bw + Margin), 0);
  FMinButton.BoundsRect := R;
end;

procedure TCustomElAdvancedPanel.TriggerCloseEvent;
begin
  if assigned(FOnClose) then FOnClose(Self);
end;

procedure TCustomElAdvancedPanel.AdjustClientRect(var Rect : TRect);
begin
  inherited;
  if FCaptionSettings.Visible then
    inc(Rect.Top, GetCaptionHeight);
end;

procedure TCustomElAdvancedPanel.CreateWnd;
var R : TRect;
begin
  inherited;
  TElAdvCaptionPanel(FCaptionPanel).SetDesigning(false);
  if IsThemeApplied then
  begin
    R := ClientRect;
    inherited AdjustClientRect(R);
    AdjustInnerSize(R);
  end;
end;

procedure TCustomElAdvancedPanel.WMNCHitTest(var Msg : TMessage);
var P : TPoint;
begin
  inherited;
  if Movable and FCaptionSettings.Visible and (not (csDesigning in ComponentState)) then
  begin
    P := ScreenToClient(SmallPointToPoint(TWMMouse(Msg).Pos));
    if (P.Y >= GetCaptionHeight) and (Msg.Result = HTCLIENT) then
      Msg.Result := HTNOWHERE;
  end;
end; {WMNCHitTest}

procedure TCustomElAdvancedPanel.LinkClickEventTransfer(Sender : TObject; HRef : TElFString);
begin
  TriggerLinkClickEvent(HRef);
end;

procedure TCustomElAdvancedPanel.SetImageForm(newValue : TElImageForm);
begin
  inherited;
  FCaptionPanel.ImageForm := newValue; 
end;

procedure TCustomElAdvancedPanel.SetUseXPThemes(const Value: Boolean);
begin
  inherited;
  FCaptionPanel.UseXPThemes := Value; 
  if IsThemeApplied then
    SetBounds(Left, Top, Width, Height);
end;

procedure TCustomElAdvancedPanel.AdjustInnerSize(var R : TRect);
var ATheme : HTheme;
    //KindOffs: integer;
begin
  if IsThemeApplied then
  begin
    ATheme := OpenThemeData(Handle, 'GLOBALS');
    if ATheme <> 0 then
    begin
      if BevelOuter <> bvNone then
      begin
        InflateRect(R, BevelWidth, BevelWidth);
        GetThemeBackgroundContentRect(ATheme, 0, GP_BORDER, BSS_SUNKEN, R, R);
      end;
      if BevelInner <> bvNone then
      begin
        InflateRect(R, BevelWidth, BevelWidth);
        GetThemeBackgroundContentRect(ATheme, 0, GP_BORDER, BSS_SUNKEN, R, R);
      end;
      CloseThemeData(ATheme);
    end;
  end;
  //KindOffs := 0;
  
  InflateRect(R, BorderWidth, BorderWidth);
  FCaptionPanel.BoundsRect := Rect(R.Left, R.Top, R.Right, R.Top + Min(GetCaptionHeight, R.Bottom - R.Top));
  InflateRect(R, -BorderWidth, -BorderWidth);
end;

function TCustomElAdvancedPanel.GetBevelAdjustment: Integer;
begin
  Result := 0;
  if BevelOuter <> bvNone then
    inc(result, 2 * BevelWidth);
  if BevelInner <> bvNone then
    inc(result, 2 * BevelWidth);
end;

procedure TCustomElAdvancedPanel.UpdateMinButtonImages;
begin
  if Minimized xor FCaptionSettings.InvertMinButtonArrows then
    FMinButton.ImageIndex := 1
  else
    FMinButton.ImageIndex := 0;
end;

procedure TCustomElAdvancedPanel.Loaded;
begin
  inherited;
  UpdateMinButtonImages;
end;

procedure TCustomElAdvancedPanel.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ExpandedHeight', ReadExpHeight, WriteExpHeight, true);
end;

procedure TCustomElAdvancedPanel.ReadExpHeight(Reader : TReader);
begin
  FSaveHeight := Reader.ReadInteger;
end;

procedure TCustomElAdvancedPanel.WriteExpHeight(Writer : TWriter);
var i : TValueType;
begin
  i := vaInt32;
  Writer.Write(i, SizeOf(i));
  Writer.Write(FSaveHeight, SizeOf(FSaveHeight));
end;

procedure TCustomElAdvancedPanel.SetLinkPopupMenu(newValue : TPopupMenu);
begin
  inherited;
  FCaptionPanel.LinkPopupMenu := newValue;
end;

procedure TCustomElAdvancedPanel.SetLinkColor(newValue : TColor);
begin
  inherited;
  FCaptionPanel.LinkColor := newValue;
end;

procedure TCustomElAdvancedPanel.SetLinkStyle(newValue : TFontStyles);
begin
  inherited;
  FCaptionPanel.LinkStyle := newValue;
end;

procedure TCustomElAdvancedPanel.SetTransparentXPThemes(Value: Boolean);
begin
  inherited;
  FCaptionPanel.TransparentXPThemes := Value;
end;

function TCustomElAdvancedPanel.CreateButton: TElAdvCaptionButton;
begin
  Result := TElAdvCaptionButton.Create(FCaptionPanel);
end;

function TCustomElAdvancedPanel.CreatePanel: TElAdvCaptionPanel;
begin
  Result := TElAdvCaptionPanel.Create(Self);
end;

function TCustomElAdvancedPanel.GetThemePartID: Integer;
begin
  result := GP_BORDER;
end;

function TCustomElAdvancedPanel.GetButtonWidth: Integer;
begin
  Result := FCaptionSettings.FButtonWidth;
end;

function TCustomElAdvancedPanel.GetCaptionHeight: Integer;
begin
  if FCaptionSettings <> nil then
    result := FCaptionSettings.Height
  else
    result := 0;
end;

procedure TCustomElAdvancedPanel.UpdateInterior;
var R : TRect;
begin
  inherited;
  if HandleAllocated then
  begin
    R := ClientRect;
    inherited AdjustClientRect(R);
    AdjustInnerSize(R);
  end;
end;

procedure TCustomElAdvancedPanel.WMSetFocus(var Message: TMessage);
begin
  inherited;
  if TWMSetFocus(Message).FocusedWnd = FCaptionPanel.Handle then
    THackForm(GetParentForm(Self)).SelectNext(Self, false, True)
  else
    FCaptionPanel.SetFocus;
end;

procedure TElAdvCaptionButton.DrawThemedBackground;
begin
  inherited;
end;

constructor TElAdvCaptionButton.Create(AOwner : TComponent);
begin
  inherited;
  ParentColor := false;
end;

initialization

  CloseBmp := TBitmap.Create;
  UpArrowBmp := TBitmap.Create;
  DownArrowBmp:= TBitmap.Create;
  CloseBmp.LoadFromResourceName(HInstance, 'ELADVPANELCLOSEBUTTON');
  UpArrowBmp.LoadFromResourceName(HInstance, 'ELADVPANELUPARROW');
  DownArrowBmp.LoadFromResourceName(HInstance, 'ELADVPANELDOWNARROW');

finalization

  CloseBmp.Free;
  UpArrowBmp.Free;
  DownArrowBmp.Free;

end.
