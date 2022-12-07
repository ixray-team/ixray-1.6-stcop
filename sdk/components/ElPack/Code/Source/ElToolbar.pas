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

05/20/2002

  BorderWidth property was ignored. Fixed.

03/26/2002

  Fixed the bug introduced 03/12/2002 that leads to inability to focus any
  control on toolbar.

03/22/2002

  ButtonCount was made public to prevent crashes in early versions of
  Delphi/Builder IDE

03/12/2002

  Fixed focus passing when TElToolbar gains focus 

03/10/2002

  Fixed assignment of properties for buttons that have OwnerSettings = false

01/24/2002

  Improved transparency handling with XP themes enabled 

01/17/2002

  Toolbar buttons now correctly handle change of Visible property when linked 
  to actions

12/24/2001

  When the toolbar is focused, focus is automatically passed to the first control
  that is able to be focused (i.e. is WinControl) 

12/12/2001

  Improved behaviour of "transparent" toolbar with XP styles enabled

11/24/2001

  Added ShowCaption and LargeSize to properties handled with Save/Restore

11/19/2001

  Added AdjustButtonHeight property
  Fixed deletion of buttons from toolbar

11/16/2001

  Button size can be now adjusted automatically depending on button contents
  with AdjustButtonWidth property

11/11/2001

  Storage and StoragePath properties were not accessible in Delphi 3. Fixed.

11/05/2001

  Save and Restore methods added. They save order and visibility of controls
  Setup method added. It shows a dialog window that the user can use to customize
  the toolbar.

10/10/2001

  ThinButtons property added

09/17/2001

  Added Windows XP Themes Support 
  MoreMenu button disabled

12/16/2000

  MoreMenu button behaviour improved.

*)

unit ElToolbar;

interface

uses
  SysUtils,
  Classes,
  {$ifndef CLX_USED}
  Windows,
  Messages,
  Controls,
  Graphics,
  Forms,
  Buttons,
  ExtCtrls,
  Menus,
  ElImgFrm,
{$ifdef VCL_6_USED}
Types,
{$endif}
  {$IFDEF VCL_4_USED}
  ImgList,
  ActnList,
  {$ENDIF}
  {$else}
  QTypes,
  Types,
  QT,
  QControls,
  QGraphics,
  QForms,
  QButtons,
  QExtCtrls,
  QMenus,
  QImgList,
  QActnList,
  {$endif}
  ElIni,
  ElStrToken,
  ElTmSchema,
  ElXPThemedControl,
  ElUxTheme,
  ElPanel,
  ElList,
  ElTools,
  ElPopBtn,
  ElVCLUtils,
  ElSndMap
  ;

{$R ElToolBar.res}

type
  TElBarOrientation = (eboHorz, eboVert);

  TElToolButtonType = (ebtButton, ebtSeparator, ebtDivider);

  TCustomElToolButton = class(TCustomElGraphicButton)
  private
    FLargeGlyph : pointer;
    FGlyph : pointer;
    FButtonType : TElToolButtonType;
    FWrap : Boolean;
    {$ifdef VCL_4_USED}
    ActionVisibleInverted,
    {$endif}
    FSettingVisible,
    FRealVisible   : boolean;
    FFakeBoolProp  : boolean;
    FFakeIntProp   : integer;
    FFakeNotifyEvent : TNotifyEvent;
    {$ifndef CLX_USED}
    {$IFDEF VCL_4_USED}
    FFakeBevelKind : TBevelKind;
    {$ENDIF}
    {$endif}
    FButtonID: Integer;
    FOwnerSettings: Boolean;
    procedure SetWrap(newValue : Boolean);
    procedure SetButtonType(newValue : TElToolButtonType);
    procedure SetLargeGlyph(newValue : TBitmap);
    procedure SetNumLargeGlyphs(newValue : Integer);
    procedure SetGlyph(newValue : TBitmap);
    procedure SetNumGlyphs(newValue : Integer);
    function GetNumGlyphs : integer;
    function GetNumLargeGlyphs : integer;
    function GetGlyph : TBitmap;
    function GetLargeGlyph : TBitmap;
    procedure GlyphChanged(Sender : TObject);
    procedure LargeGlyphChanged(Sender : TObject);
    {$ifndef CLX_USED}
    procedure CMEnabledChanged(var Msg : TMessage); message CM_ENABLEDCHANGED;
    {$endif}
  protected
    {$ifdef CLX_USED}
    procedure EnabledChanged; override;
    {$endif}
    procedure SetUseArrow(newValue : boolean); override;
    procedure SwitchGlyphs(ToLarge : boolean);
    procedure SetFlat(Value : Boolean); override;
    {$ifndef CLX_USED}
    procedure SetParent(AParent : TWinControl); override;
    {$else}
    procedure SetParent(const AParent : TWidgetControl); override;
    {$endif}
    procedure Paint; override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
        override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {$ifdef MSWINDOWS}

    function GetThemePartID: Integer; override;
    function GetThemeStateID: Integer; override;
    function GetThemedClassName: WideString; override;
    function GetArrowThemePartID: Integer; override;
    function GetArrowThemeStateID: Integer; override;
    function GetArrowThemedClassName: WideString; override;
    procedure DrawThemedBackground(Canvas : TCanvas); override;
    {$endif}
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadButtonID(Reader : TReader);
    procedure WriteButtonID(Writer : TWriter);
    {$ifndef CLX_USED}
    procedure CMTextChanged(var Message : TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    {$else}
    procedure VisibleChanged; override;
    procedure TextChanged; override;
    {$endif}
    procedure ImagesChanged(Sender : TObject); override;
    procedure SetLayout(Value : TButtonLayout); override;
    procedure SetMargin(Value : Integer); override;
    procedure SetShowGlyph(newValue : Boolean); override;
    procedure SetShowText(newValue : Boolean); override;
    procedure SetSpacing(Value : Integer); override;
    {$ifdef VCL_4_USED}
    function GetActionLinkClass: TControlActionLinkClass; override;
    {$endif}
    procedure Loaded; override;
    procedure SetOwnerSettings(Value: Boolean);
    procedure SetImageIndex(newValue : Integer); override;
    property Transparent;

    property Wrap : Boolean read FWrap write SetWrap;
    property ButtonType : TElToolButtonType read FButtonType write SetButtonType;
    property LargeGlyph : TBitmap read GetLargeGlyph write SetLargeGlyph; { Published }
    property NumLargeGlyphs : Integer read GetNumLargeGlyphs write SetNumLargeGlyphs; { Protected }
    property Glyph : TBitmap read GetGlyph write SetGlyph; { Published }
    property NumGlyphs : Integer read GetNumGlyphs write SetNumGlyphs; { Published }
    property OwnerSettings: Boolean read FOwnerSettings write SetOwnerSettings
        default true;

    property Default : boolean read FFakeBoolProp write FFakeBoolProp  stored false;
    property ShowFocus: boolean read FFakeBoolProp write FFakeBoolProp  stored false;
    property TabStop : boolean read FFakeBoolProp write FFakeBoolProp stored false;
    property TabOrder: integer read FFakeIntProp write FFakeIntProp stored false;
    property OnEnter : TNotifyEvent read FFakeNotifyEvent write FFakeNotifyEvent stored false;
    property OnExit : TNotifyEvent read FFakeNotifyEvent write FFakeNotifyEvent stored false;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property RealVisible : boolean read FRealVisible default true;
  published

    {$IFDEF VCL_4_USED}
    {$ifndef CLX_USED}
    property BevelKind : TBevelKind read FFakeBevelKind write FFakeBevelKind stored false;
    property DoubleBuffered : boolean read FFakeBoolProp write FFakeBoolProp stored false;
    {$endif}
    {$ENDIF}
  end;

  TElToolButton = class(TCustomElToolButton)
  published

    property Wrap;
    property ButtonType;
    property LargeGlyph;
    property NumLargeGlyphs;
    property Glyph;
    property NumGlyphs;
    property OwnerSettings;

    // ElGraphicButton properties
	property AdjustSpaceForGlyph;
    property PullDownMenu;
    property PopupPlace;
    property DisableAutoPopup;
    property Cancel;
    property ModalResult;
    property AllowAllUp;
    property GroupIndex;
    property Down;
    property Flat;
    property Layout;
    property Margin;
    property Spacing;
    property UseArrow;
    property ShadowFollowsColor;
    property ShowGlyph;
    property ShowText;
    property OnArrowClick;
    property Icon;
    property ImageIsAlphaBlended;
    property IsSwitch;
    property TextDrawType;
    property ThinFrame;
    property DownSound;
    property UpSound;
    property ClickSound;
    property ArrowClickSound;
{$IFDEF USE_SOUND_MAP}
    property SoundMap;
{$ENDIF}
    property UseIcon;
    property ImageIndex;
    property UseImageList;
    property OldStyled;
    property Background;
    property DownBackground;
    property BackgroundDrawBorder;
    property Transparent;
    {$ifdef MSWINDOWS}
    property UseXPThemes;
    {$endif}

    // VCL properties
    property Caption;
    property Enabled;
    property PopupMenu;
    property Color;
    property ParentColor;
    property Align;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;

{$IFDEF VCL_4_USED}
    property Action;
    property Constraints;
{$ENDIF}
{$IFDEF VCL_5_USED}
    property OnContextPopup;
{$ENDIF}
  end;

  TElToolButtonClass = class of TCustomElToolButton;

type
  TElToolBar = class(TElPanel)
  private
    FNoReAlign      : boolean;
    (*
    FMoreMenu       : TPopupMenu;
    FMoreMenuItems  : TElList;
    FMoreMenuVisible,
    FMoreMenuActive : boolean;
    FIgnoreMoreClick: boolean;
    *)
    FShowMoreMenu   : Boolean;

    FTransparentButtons : Boolean;
    FUseImageList : Boolean;
    FImages : TImageList;
    FHotImages : TImageList;
    FDisabledImages : TImageList;
    FUpdateCount : integer;
    FUpdatingButtons: integer;
    FUseLargeGlyphs : Boolean;
    FHidden : Boolean;
    FHideable : Boolean;
    FOrientation : TElBarOrientation;
    FButtonColor : TColor;
    FMinSize : integer;
    FAutoSize : Boolean;
    FFlat : Boolean;
    FLargeBtnWidth : Integer;
    FLargeBtnHeight : Integer;
    FGlyphLayout : TButtonLayout;
    FSpacing : Integer;
    FMargin : Integer;
    FShowGlyph : Boolean;
    FShowCaption : Boolean;
    FLargeSize : Boolean;
    FBtnWidth : Integer;
    FBtnHeight : Integer;
    FBtnOffsHorz : Integer;
    FBtnOffsVert : Integer;
    FAutoWrap : Boolean;
    FCreating : boolean;
    FSaveAlign : TAlign;
    //    FSaveSize: integer;
    FDummy : string;
    {$ifndef CLX_USED}
    FButtonImageForm : TElImageForm;
    FMouseInControl: Boolean;
    {$endif}
    {$ifndef CLX_USED}
    procedure WMNCCalcSize(var Msg : TWMNcCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
    procedure WMNCHitTest(var Msg : TMessage); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Msg : TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMWindowPosChanged(var Msg : TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMEraseBkGnd(var Msg : TWMEraseBkGnd); message WM_EraseBkgnd;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$endif}
  protected
    FThinButtons: Boolean;
    FStorage: TElIniFile;
    FStoragePath: string;
    FAdjustButtonWidth: Boolean;
    FAdjustButtonHeight: Boolean;
    FButtons: TElList;
    FFocusedButton: TElToolButton;
    FTransparent : boolean;
    FImageIsAlphaBlended: Boolean;
    {$ifndef CLX_USED}
    procedure CMEnabledChanged(var Msg : TMessage); message CM_ENABLEDCHANGED;
    {$else}
    procedure EnabledChanged; override;
    {$endif}
    {$ifndef CLX_USED}
    procedure StartMoreMenu;
    procedure PutMoreItemsToBar;
    procedure OnMoreItemClick(Sender : TObject);
    {$endif}
    {$ifndef CLX_USED}
    procedure SetButtonImageForm(newValue : TElImageForm); virtual;
    {$endif}
    procedure SetBtnWidth(newValue : Integer); virtual;
    procedure SetBtnHeight(newValue : Integer); virtual;
    procedure SetFlat(newValue : Boolean); virtual;
    procedure SetLargeSize(newValue : Boolean); virtual;
    procedure SetLargeBtnWidth(newValue : Integer); virtual;
    procedure SetLargeBtnHeight(newValue : Integer); virtual;
    procedure SetButtonColor(newValue : TColor); virtual;
    procedure SetAutoSize(newValue : Boolean);
{$ifdef VCL_6_USED}
    {$ifndef CLX_USED}
    override;
    {$else}
    virtual;
    {$endif}
{$else}
    virtual;
{$endif}
    procedure SetTransparentButtons(newValue : Boolean); virtual;
    procedure SetBtnOffsHorz(newValue : Integer); virtual;
    procedure SetBtnOffsVert(newValue : Integer); virtual;
    procedure SetAutoWrap(newValue : Boolean);
    procedure SetShowGlyph(newValue : Boolean);
    procedure SetShowCaption(newValue : Boolean);
    procedure SetGlyphLayout(newValue : TButtonLayout); virtual;
    procedure SetSpacing(newValue : Integer); virtual;
    procedure SetMargin(newValue : Integer); virtual;
    function GetToolButton(index : integer) : TElToolButton;
    procedure SetToolButton(index : integer; newValue : TElToolButton);
    function GetButtonCount : Integer;
    {$ifdef CLX_USED}
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); override;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); override;
    procedure Resize; override;
    {$endif}
    {$ifndef CLX_USED}
    procedure CMControlListChange(var Msg : TMessage); message CM_CONTROLLISTCHANGE;
    procedure CMControlChange(var Msg : TCMControlChange); message CM_CONTROLCHANGE;
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    {$endif}
    procedure SetMinSize(newValue : integer);
    procedure SetOrientation(newValue : TElBarOrientation);
    //procedure SetHideable(newValue : Boolean);
    //procedure SetHidden(newValue : Boolean);
    procedure SetUseLargeGlyphs(newValue : Boolean);
    procedure SetImages(newValue : TImageList);
    procedure SetHotImages(newValue : TImageList);
    procedure SetDisabledImages(newValue : TImageList);
    procedure SetUseImageList(newValue : Boolean);

    {$ifndef CLX_USED}
    procedure SetShowMoreMenu(newValue : Boolean); virtual;
    procedure SetMoreMenuActive(newValue : Boolean); virtual;
    {$endif}
    procedure AlignControls(AControl : TControl; var Rect : TRect); override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure Loaded; override;
    procedure Paint; override;
    {$ifndef CLX_USED}
    {$endif}
    {$ifndef CLX_USED}
    procedure RedrawMoreBtn;
    {$endif}
    function GetRealClientWidth : integer;
    function GetRealClientHeight : integer;
    {$ifndef CLX_USED}
    function GetMoreBtnRect : TRect;
    {$endif}
    procedure SetThinButtons(Value: Boolean);

    procedure DrawThemedBackground; override;
    function GetThemedClassName: WideString; override;
    function GetFreeButtonID: Integer;
    function GetButtonByID(ID : Integer): TElToolButton;
    procedure SetAdjustButtonWidth(Value: Boolean);
    function GetEffectiveButtonWidth(Button : TCustomElToolButton; IncludeArrow : 
        boolean): Integer;
    procedure SetAdjustButtonHeight(Value: Boolean);
    function GetEffectiveButtonHeight(Button : TCustomElToolButton): Integer;
    {$ifndef CLX_USED}
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure WMMouseLeave(var Message: TMessage); message WM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure StartLeaveTracking;
    {$endif}
    function GetButtonClass: TElToolButtonClass; virtual;
    procedure SetFocusedButton(Value: TElToolButton);
    procedure SetTransparent(newValue : Boolean); override;
    procedure SetUseXPThemes(const Value: Boolean); override;
    procedure DoEnter; override;
    {$ifndef CLX_USED}
    procedure SetParent(AParent: TWinControl); override;
    {$endif}
    {$ifdef CLX_USED}
    procedure FontChanged; override;
    {$endif}
    procedure SetImageIsAlphaBlended(Value: Boolean);

    property FocusedButton: TElToolButton read FFocusedButton write
        SetFocusedButton;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function AddButton(ButtonType : TELToolButtonType) : TElToolButton; virtual;
    procedure OrderedControls(L : TElList);

    procedure AlignButtons; virtual;
    procedure UpdateButtons; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Save;
    procedure Restore;
    {$ifndef CLX_USED}
    function Setup(ShowTextOptions, ShowIconOptions : boolean): Boolean;
    {$endif}
    function GetNextButton(CurrentButton : TCustomElToolButton; Forward :
             boolean; IncludeDisabled : boolean) : TCustomElToolButton;

    property ToolButton[index : integer] : TElToolButton read GetToolButton write SetToolButton;
    property Caption : string read FDummy write FDummy;
    property ButtonCount : Integer read GetButtonCount;
  published
    property BtnWidth : Integer read FBtnWidth write SetBtnWidth default 24;
    property BtnHeight : Integer read FBtnHeight write SetBtnHeight default 24;
    property BtnOffsHorz : Integer read FBtnOffsHorz write SetBtnOffsHorz default 3;
    property BtnOffsVert : Integer read FBtnOffsVert write SetBtnOffsVert default 3;
    property AutoWrap : Boolean read FAutoWrap write SetAutoWrap;
    property ShowGlyph : Boolean read FShowGlyph write SetShowGlyph default True;
    property ShowCaption : Boolean read FShowCaption write SetShowCaption default False;
    property LargeSize : Boolean read FLargeSize write SetLargeSize default False;
    property LargeBtnWidth : Integer read FLargeBtnWidth write SetLargeBtnWidth default 48;
    property LargeBtnHeight : Integer read FLargeBtnHeight write SetLargeBtnHeight default 48;
    property GlyphLayout : TButtonLayout read FGlyphLayout write SetGlyphLayout default blGlyphLeft;
    property Spacing : Integer read FSpacing write SetSpacing default 4;
    property Margin : Integer read FMargin write SetMargin default -1;
    property Flat : Boolean read FFlat write SetFlat default True;
    property AutoSize : Boolean read FAutoSize write SetAutoSize default True;
    property MinSize : integer read FMinSize write SetMinSize default 24;
    property ButtonColor : TColor read FButtonColor write SetButtonColor default clBtnFace;
    {$ifndef CLX_USED}
    property ButtonImageForm : TElImageForm read FButtonImageForm write SetButtonImageForm;
    {$endif}
    property Orientation : TElBarOrientation read FOrientation write SetOrientation default eboHorz;

    property UseLargeGlyphs : Boolean read FUseLargeGlyphs write SetUseLargeGlyphs; { Published }
    property Images : TImageList read FImages write SetImages;
    property HotImages : TImageList read FHotImages write SetHotImages;
    property DisabledImages : TImageList read FDisabledImages write SetDisabledImages;
    property UseImageList : Boolean read FUseImageList write SetUseImageList; { Published }
    property TransparentButtons : Boolean read FTransparentButtons write SetTransparentButtons; { Published }
    property ThinButtons: Boolean read FThinButtons write SetThinButtons;
    property Storage: TElIniFile read FStorage write FStorage;
    property StoragePath: string read FStoragePath write FStoragePath;
    property AdjustButtonWidth: Boolean read FAdjustButtonWidth write
        SetAdjustButtonWidth default true;
    property AdjustButtonHeight: Boolean read FAdjustButtonHeight write
        SetAdjustButtonHeight default true;
    property ImageIsAlphaBlended: Boolean read FImageIsAlphaBlended write 
        SetImageIsAlphaBlended default false;
    {$ifndef CLX_USED}
    property ShowMoreMenu : boolean read FShowMoreMenu write SetShowMoreMenu;
    {$endif}
    {$ifndef CLX_USED}
    property UseXPThemes;
    {$endif}
    
// Not made yet
//    property Hideable : Boolean read FHideable write SetHideable;
//    property Hidden : Boolean read FHidden write SetHidden default False;

{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    {$ifndef CLX_USED}
    property DockOrientation;
    property Floating;
    property BevelKind;
    property DoubleBuffered;
    property DragKind;
    {$endif}
{$ENDIF}
  end;

  {$ifdef VCL_4_USED}
  TElToolButtonActionLink = class(TElGraphicButtonActionLink)
  protected
    function IsVisibleLinked: Boolean; override;
    procedure SetVisible(Value: Boolean); override;
  end;
  {$endif}

const
  DEF_SepSize : integer = 4;

implementation

{$ifndef CLX_USED}
uses frmTbrStp;
{$endif}

var FMoreGlyph : TBitmap;

procedure TCustomElToolButton.GlyphChanged(Sender : TObject);
begin
  if (Parent <> nil) and ((not TElToolbar(Parent).FLargeSize) or (not TElToolbar(Parent).UseLargeGlyphs)) then
  begin
    inherited Glyph := Glyph;
    inherited NumGlyphs := NumGlyphs;
    Invalidate;
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end;
end;

procedure TCustomElToolButton.LargeGlyphChanged(Sender : TObject);
begin
  if (Parent <> nil) and (TElToolbar(Parent).FLargeSize) and (TElToolbar(Parent).UseLargeGlyphs) then
  begin
    inherited Glyph := LargeGlyph;
    inherited NumGlyphs := NumLargeGlyphs;
    Invalidate;
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end;
end;

destructor TCustomElToolButton.Destroy;
begin
  TElButtonGlyph(FGlyph).Free;
  TElButtonGlyph(FLargeGlyph).Free;
  inherited;
end;

constructor TCustomElToolButton.Create(AOwner : TComponent);
begin
  inherited;
  FOwnerSettings := true;
  FGlyph := TElButtonGlyph.Create;
  TElButtonGlyph(FGlyph).OnChange := GlyphChanged;
  FLargeGlyph := TElButtonGlyph.Create;
  TElButtonGlyph(FLargeGlyph).OnChange := LargeGlyphChanged;
  FRealVisible := true;
  ShortcutsEnabled := true;
end;

procedure TCustomElToolButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  //TElToolbar(Parent).UpdateRaisedState;
end;

procedure TCustomElToolButton.MouseDown(Button : TMouseButton; Shift : 
    TShiftState; X, Y : Integer);
begin
  inherited;
  //if (Parent is TElToolbar) and Parent.CanFocus {and Parent.TabStop} then
  //  Parent.SetFocus;
end;

procedure TCustomElToolButton.Paint;
var
  R : TRect;
  VO,
    HO : integer;
begin
  if ButtonType <> ebtButton then
    if Caption <> '' then
      Caption := '';
  inherited;
  if (ButtonType = ebtDivider) {$ifndef CLX_USED}and (not IsThemeApplied){$endif} then
  begin
    {$ifndef CLX_USED}
    if not Transparent then
    {$endif}
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
    end;
    R := ClientRect;
    if Parent is TElToolBar then
    begin
      VO := (Parent as TElToolBar).FBtnOffsVert;
      HO := (Parent as TElToolBar).FBtnOffsHorz;
      if (Parent as TElToolBar).Align in [alLeft, alRight] then
      begin
        R := Rect(HO, (R.Bottom - R.Top) div 2 - 1, (R.Right - R.Left) - HO, (R.Bottom - R.Top) div 2 + 2);
        {$ifndef CLX_USED}
        DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER or BDR_RAISEDINNER, BF_TOP);
        {$else}
        DrawEdge(Canvas, R, esRaised, esLowered, [ebTop]);
        {$endif}
      end
      else
      begin
        R := Rect((R.Right - R.Left) div 2 - 1, VO, (R.Right - R.Left) div 2 + 2, (R.Bottom - R.Top) - VO);
        {$ifndef CLX_USED}
        DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER or BDR_RAISEDINNER, BF_LEFT);
        {$else}
        DrawEdge(Canvas, R, esRaised, esLowered, [ebLeft]);
        {$endif}
      end;
    end
    else
    begin
      R := Rect((R.Right - R.Left) div 2 - 1, 2, (R.Right - R.Left) div 2 + 2, (R.Bottom - R.Top) - 2);
      {$ifndef CLX_USED}
      DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER or BDR_RAISEDINNER, BF_LEFT);
      {$else}
      DrawEdge(Canvas, R, esRaised, esLowered, [ebLeft]);
      {$endif}
    end;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElToolButton.SetParent(AParent : TWinControl);
{$else}
procedure TCustomElToolButton.SetParent(const AParent : TWidgetControl);
{$endif}
begin
  {$ifdef VCL_5_USED}
  if (Parent <> nil) and not (csDestroying in Parent.ComponentState) then
    Parent.RemoveFreeNotification(Self);
  {$endif}
  if Assigned(AParent) then FreeNotification(AParent);
  inherited;
  if Assigned(AParent) and (AParent is TElToolBar) then
    TElToolBar(AParent).UpdateButtons;
end;

procedure TCustomElToolButton.SetUseArrow(newValue : boolean);
begin
  if newValue <> UseArrow then
  begin
    inherited;
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end;
end;

procedure TCustomElToolButton.SetFlat(Value : Boolean);
begin
  if (Value) or (ButtonType = ebtButton) then
    inherited;
end;

procedure TCustomElToolButton.SetWrap(newValue : Boolean);
begin
  if (FWrap <> newValue) then
  begin
    FWrap := newValue;
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end; {if}
end;

procedure TCustomElToolButton.SetButtonType(newValue : TElToolButtonType);
begin
  if (FButtonType <> newValue) then
  begin
    FButtonType := newValue;
    if FButtonType = ebtSeparator then
    begin
      Glyph.Assign(nil);
      Enabled := false;
      Flat := true;
      Caption := '';
    end
    else if FButtonType = ebtDivider then
    begin
      Glyph.Assign(nil);
      Enabled := false;
      Flat := true;
      Caption := '';
    end;
    if Parent is TElToolBar then
      with (Parent as TElToolBar) do
      begin
        UpdateButtons;
        AlignButtons;
      end; // with
  end; {if}
end; {SetButtonType}

function TCustomElToolButton.GetLargeGlyph: TBitmap;
begin
  Result := TElButtonGlyph(FLargeGlyph).Glyph;
end;

procedure TCustomElToolButton.SetLargeGlyph;
{ Sets data member FLargeGlyph to newValue. }
begin
  TElButtonGlyph(FLargeGlyph).Glyph := NewValue;
  if (Parent <> nil) and (TElToolbar(Parent).FLargeSize) and (TElToolbar(Parent).UseLargeGlyphs) then inherited Glyph := newValue;
end; { SetLargeGlyph }

function TCustomElToolButton.GetNumLargeGlyphs: Integer;
begin
  Result := TElButtonGlyph(FLargeGlyph).NumGlyphs;
end;

procedure TCustomElToolButton.SetNumLargeGlyphs;
{ Sets data member FNumLargeGlyphs to newValue. }
begin
  if NewValue < 0 then
    NewValue := 1
  else if NewValue > 4 then
    NewValue := 4;
  if NewValue <> TElButtonGlyph(FLargeGlyph).NumGlyphs then
  begin
    TElButtonGlyph(FLargeGlyph).NumGlyphs := NewValue;
    if (Parent <> nil) and (TElToolbar(Parent).FLargeSize) and (TElToolbar(Parent).UseLargeGlyphs) then inherited NumGlyphs := newValue;
  end;
end; { SetNumLargeGlyphs }

function TCustomElToolButton.GetGlyph: TBitmap;
begin
  Result := TElButtonGlyph(FGlyph).Glyph;
end;

procedure TCustomElToolButton.SetGlyph;
begin
  TElButtonGlyph(FGlyph).Glyph := newValue;
  if (Parent <> nil) and (not TElToolbar(Parent).FLargeSize) then
    inherited Glyph := newValue;
end; { SetGlyph }

function TCustomElToolButton.GetNumGlyphs: Integer;
begin
  Result := TElButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TCustomElToolButton.SetNumGlyphs;
{ Sets data member FNumGlyphs to newValue. }
begin
  if NewValue < 0 then
    NewValue := 1
  else if NewValue > 4 then
    NewValue := 4;
  if NewValue <> TElButtonGlyph(FGlyph).NumGlyphs then
  begin
    TElButtonGlyph(FGlyph).NumGlyphs := NewValue;
    if (Parent <> nil) and (not TElToolbar(Parent).FLargeSize) then inherited NumGlyphs := NewValue;
  end;
end; { SetNumGlyphs }

procedure TCustomElToolButton.SwitchGlyphs(ToLarge : boolean);
begin
  if (Parent <> nil) then
  begin
    if (not ToLarge) then
    begin
      inherited Glyph := Glyph;
      inherited NumGlyphs := NumGlyphs;
    end
    else
    if (TElToolbar(Parent).UseLargeGlyphs) then
    begin
      inherited Glyph := LargeGlyph;
      inherited NumGlyphs := NumLargeGlyphs;
    end;
    if (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElToolButton.CMEnabledChanged(var Msg : TMessage);
{$else}
procedure TCustomElToolButton.EnabledChanged;
{$endif}
begin
  inherited;
  if ButtonType <> ebtButton then Enabled := false;
end;  { CMEnabledChanged }

{$ifdef MSWINDOWS}
function TCustomElToolButton.GetThemePartID: Integer;
begin
  case Self.FButtonType of
    ebtSeparator:
      begin
        result := TP_SPLITBUTTON;
      end;
    ebtDivider:
      begin
        if (Parent as TElToolbar).Orientation = eboVert then
          result := TP_SEPARATORVERT
        else
          result := TP_SEPARATOR;
      end;
    else
      begin
        if (not UseArrow) and (PulldownMenu <> nil) then
          result := TP_DROPDOWNBUTTON
        else
        if UseArrow then
          result := TP_SPLITBUTTON
        else
          result := TP_BUTTON;
      end;
  end;
end;

function TCustomElToolButton.GetThemeStateID: Integer;
begin
  //case Self.FButtonType of
  //  ebtButton:
      begin
        if not Enabled then
          result := TS_DISABLED
        else
        if FState in [ebsDown, ebsExclusive] then
          result := TS_PRESSED
        else
        if (Down and IsSwitch) then
        begin
          if FMouseInControl or FMouseInArrow then
            result := TS_HOTCHECKED
          else
            result := TS_CHECKED;
        end
        else
        if FMouseInControl or FMouseInArrow or FInMenu then
          result := TS_HOT
        else
          result := TS_NORMAL;
      end;
  //end;
end;

function TCustomElToolButton.GetThemedClassName: WideString;
begin
  Result := 'TOOLBAR';
end;

function TCustomElToolButton.GetArrowThemePartID: Integer;
begin
  Result := TP_SPLITBUTTONDROPDOWN;
end;

function TCustomElToolButton.GetArrowThemeStateID: Integer;
begin
  if not Enabled then
    result := TS_DISABLED
  else
  if FState in [ebsArrDown, ebsDown, ebsExclusive] then
    result := TS_PRESSED
  else
  if FMouseInArrow or FMouseInControl then
    result := TS_HOT
  else
    result := TS_NORMAL;
end;

function TCustomElToolButton.GetArrowThemedClassName: WideString;
begin

  Result := 'TOOLBAR'
end;

procedure TCustomElToolButton.DrawThemedBackground;
begin
  if not Transparent then
  begin         
  {$ifndef CLX_USED}
    DrawThemeBackground(Theme, Canvas.Handle, 0, 0, ClientRect, nil);
  {$else}
    Canvas.Start;
    DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), 0, 0, ClientRect, nil);
    Canvas.Stop;
  {$endif}
  end;
end;
{$endif}

procedure TCustomElToolButton.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ButtonID', ReadButtonID, WriteButtonID, true);
end;

procedure TCustomElToolButton.ReadButtonID(Reader : TReader);
begin
  FButtonID := Reader.ReadInteger;
end;

procedure TCustomElToolButton.WriteButtonID(Writer : TWriter);
var i : TValueType;
begin
  i := vaInt32;
  Writer.Write(i, SizeOf(i));
  Writer.Write(FButtonID, SizeOf(FButtonID));
end;

{$ifndef CLX_USED}
procedure TCustomElToolButton.CMTextChanged(var Message : TMessage);
{$else}
procedure TCustomElToolButton.TextChanged;
{$endif}
begin
  inherited;
  if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
end;

procedure TCustomElToolButton.ImagesChanged(Sender : TObject);
begin
  inherited;
  if FUseImageList then
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
end;

procedure TCustomElToolButton.SetLayout(Value : TButtonLayout);
begin
  if Layout <> Value then
  begin
    inherited;
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end;
end;

procedure TCustomElToolButton.SetMargin(Value : Integer);
begin
  if (Value <> FMargin) then
  begin
    inherited;
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end;
end;

procedure TCustomElToolButton.SetShowGlyph(newValue : Boolean);
begin
  if (ShowGlyph <> newValue) then
  begin
    inherited;
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end; { if }
end; { SetShowGlyph }

procedure TCustomElToolButton.SetShowText(newValue : Boolean);
begin
  if (ShowText <> newValue) then
  begin
    inherited;
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end; { if }
end; { SetShowText }

procedure TCustomElToolButton.SetSpacing(Value : Integer);
begin
  if Value <> Spacing then
  begin
    inherited;
    if (Parent <> nil) and (Parent is TElToolBar) then TElToolBar(Parent).AlignButtons;
  end;
end;

{$ifdef VCL_4_USED}
function TCustomElToolButton.GetActionLinkClass: TControlActionLinkClass;
begin
  result := TElToolButtonActionLink;
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElToolButton.CMVisibleChanged(var Message: TMessage);
{$else}
procedure TCustomElToolButton.VisibleChanged;
{$endif}
begin
  if (not FSettingVisible) then
  begin
    FRealVisible := Visible;
    {$ifdef VCL_4_USED}
    if (Action <> nil) then
    begin
      if (not TAction(Action).Visible) xor ActionVisibleInverted then
      begin
        FSettingVisible := true;
        Visible := false;
        FSettingVisible := false;
      end;
    end;
    {$endif}
    inherited;
  end;
end;

procedure TCustomElToolButton.Loaded;
begin
  inherited;
  // FRealVisible := Visible;
end;

procedure TCustomElToolButton.SetOwnerSettings(Value: Boolean);
begin
  if FOwnerSettings <> Value then
  begin
    FOwnerSettings := Value;
  end;
end;

procedure TCustomElToolButton.SetImageIndex(newValue : Integer);
begin
  inherited;
  if (Parent <> nil) and (Parent is TElToolBar) and UseImageList
     and (TElToolbar(Parent).AdjustButtonWidth or TElToolbar(Parent).AdjustButtonHeight) then
    TElToolBar(Parent).AlignButtons;
end;


// =============================================================================

{$ifndef CLX_USED}
procedure TElToolBar.SetButtonImageForm(newValue : TElImageForm);
begin
  {$ifdef VCL_5_USED}
  if (FButtonImageForm <> nil) and (not (csDestroying in FButtonImageForm.ComponentState)) then
    FButtonImageForm.RemoveFreeNotification(Self);
  {$endif}
  FButtonImageForm := newValue;
  if newValue <> nil then
     newValue.FreeNotification(Self);
  if csLoading in ComponentState then exit;
  UpdateButtons;
end;
{$endif}

procedure TElToolBar.SetBtnWidth(newValue : Integer);
begin
  if (FBtnWidth <> newValue) then
  begin
    FBtnWidth := newValue;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end; {if}
end;

procedure TElToolBar.SetBtnHeight(newValue : Integer);
begin
  if (FBtnHeight <> newValue) then
  begin
    FBtnHeight := newValue;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end; {if}
end;

procedure TElToolBar.SetBtnOffsHorz(newValue : Integer);
begin
  if (FBtnOffsHorz <> newValue) then
  begin
    FBtnOffsHorz := newValue;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end; {if}
end;

procedure TElToolBar.SetBtnOffsVert(newValue : Integer);
begin
  if (FBtnOffsVert <> newValue) then
  begin
    FBtnOffsVert := newValue;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end; {if}
end;

procedure TElToolBar.SetAutoWrap(newValue : Boolean);
begin
  if (FAutoWrap <> newValue) then
  begin
    FAutoWrap := newValue;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end; {if}
end;

procedure TElToolBar.UpdateButtons;
var
  i : integer;
  TB : TCustomElToolButton;
begin
  if (ComponentState * [csDestroying, csLoading, csReading] <> []) or (FButtons = nil) or (FUpdateCount > 0) or (FUpdatingButtons > 0) then exit;
  Inc(FUpdatingButtons);
  for i := 0 to FButtons.Count - 1 do // Iterate
  begin
    TB := TCustomElToolButton(FButtons[i]);
    (*
    if FLargeSize then
    begin
      if TB.FButtonType = ebtButton then
      begin
        if TB.UseArrow and (Orientation <> eboVert)
           then TB.SetBounds(TB.Left, TB.Top, FLargeBtnWidth + 14, FLargeBtnHeight)
           else TB.SetBounds(TB.Left, TB.Top, FLargeBtnWidth, FLargeBtnHeight);
      end
      else
      begin
        if (Align in [alLeft, alRight])
           then TB.SetBounds(TB.Left, TB.Top, FLargeBtnWidth, FLargeBtnHeight div DEF_SepSize)
           else TB.SetBounds(TB.Left, TB.Top, FLargeBtnWidth  div DEF_SepSize, FLargeBtnHeight);
      end;
    end
    else
    begin
      if TB.FButtonType = ebtButton then
      begin
        if TB.UseArrow and (Orientation <> eboVert)
           then TB.SetBounds(TB.Left, TB.Top, FBtnWidth + 14, FBtnHeight)
           else TB.SetBounds(TB.Left, TB.Top, FBtnWidth, FBtnHeight);
      end
      else
      begin
        if Align in [alLeft, alRight]
           then TB.SetBounds(TB.Left, TB.Top, FBtnWidth, FBtnHeight div DEF_SepSize)
           else TB.SetBounds(TB.Left, TB.Top, FBtnWidth  div DEF_SepSize, FBtnHeight);
      end;
    end;
    *)
    if TB.OwnerSettings then
    begin
      TB.ShowText := FShowCaption;
      TB.ShowGlyph := FShowGlyph;
      TB.Layout := FGlyphLayout;
      if TB.ButtonType = ebtButton then
        TB.Flat := Self.FFlat;
      TB.Spacing := FSpacing;
      TB.Margin := FMargin;
      TB.Color := FButtonColor;
      TB.Transparent := TransparentButtons;
      TB.Images := FImages;
      TB.HotImages := FHotImages;
      TB.UseImageList := FUseImageList;
      TB.SwitchGlyphs(FLargeSize);
      TB.DisabledImages := FDisabledImages;
      {$ifndef CLX_USED}
      TB.ImageForm := FButtonImageForm;
      {$endif}
      TB.ThinFrame := FThinButtons;
      TB.UseXPThemes := UseXPThemes;
      TB.ImageIsAlphaBlended := ImageIsAlphaBlended;
    end;
  end; // for
  Dec(FUpdatingButtons);
end;

procedure TElToolBar.AlignControls(AControl : TControl; var Rect : TRect);
begin
  if FNoReAlign then exit;
  if (FUpdatingButtons > 0) then exit;
  inherited;
  if (([csLoading, csDestroying] * ComponentState) <> []) or (csCreating in ControlState) or (FCreating) then exit;
  UpdateButtons;
  AlignButtons;
end;

procedure TElToolBar.AlignButtons;
var
  i : integer;
  List2 : TElList;
  Control : TControl;
  CX, CY : integer;
  FMaxX,
    FMaxY : integer;
  b : boolean;
  aoffs, toffs, loffs, boffs, roffs : integer;
  MaxButtonWidth,
  MaxButtonHeight : integer;
begin
  if not HandleAllocated then exit;
  if FNoReAlign then exit;
  if (([csLoading, csReading, csDestroying] * ComponentState) <> []) or (csCreating in ControlState) or (FCreating) or (FUpdateCount > 0) or (FUpdatingButtons > 0) then exit;
  if TopGrabHandle.Visible and TopGrabHandle.Enabled then toffs := TopGrabHandle.Size else toffs := 0;
  if LeftGrabHandle.Visible and LeftGrabHandle.Enabled then loffs := LeftGrabHandle.Size else loffs := 0;
  if RightGrabHandle.Visible and RightGrabHandle.Enabled then roffs := RightGrabHandle.Size else roffs := 0;
  if BottomGrabHandle.Visible and BottomGrabHandle.Enabled then boffs := BottomGrabHandle.Size else boffs := 0;
  aoffs := 0;
  if BorderWidth <> 0 then
  begin
    Inc(loffs, BorderWidth);
    Inc(toffs, BorderWidth);
    inc(roffs, BorderWidth);
    inc(boffs, BorderWidth);
  end;

  if BevelInner <> bvNone then
  begin
    //inc(aoffs, BevelWidth);
    Inc(loffs, BevelWidth);
    Inc(toffs, BevelWidth);
    inc(roffs, BevelWidth);
    inc(boffs, BevelWidth);
  end;
  if BevelOuter <> bvNone then
  begin
    //inc(aoffs, BevelWidth);
    Inc(loffs, BevelWidth);
    Inc(toffs, BevelWidth);
    inc(roffs, BevelWidth);
    inc(boffs, BevelWidth);
  end;

  {$ifndef CLX_USED}
  PutMoreItemsToBar;
  {$endif}
  Inc(FUpdatingButtons);

  // get the list of ordered controls
  List2 := TElList.Create;
  OrderedControls(List2);

  CX := FBtnOffsHorz  + loffs;
  if FHideable and (FOrientation = eboHorz) then Inc(CX, 12);
  CY := FBtnOffsVert + toffs;
  if FHideable and (FOrientation = eboVert) then Inc(CY, 12);
  FMaxX := 0;
  FMaxY := 0;

  if FLargeSize then
    MaxButtonWidth := FLargeBtnWidth
  else
    MaxButtonWidth := FBtnWidth;

  if FLargeSize then
    MaxButtonHeight := FLargeBtnHeight
  else
    MaxButtonHeight := FBtnHeight;

  // if Orientation = eboVert then
    for i := 0 to List2.Count - 1 do // Iterate
    begin
      Control := TControl(List2[i]);
      if (Control is TCustomElToolButton) and (TCustomElToolButton(Control).ButtonType = ebtButton) and
         (not (csDestroying in ComponentState)) then
      begin
        MaxButtonWidth := Max(MaxButtonWidth, GetEffectiveButtonWidth(TCustomElToolButton(Control), Orientation = eboVert));
      end;
    end;

  for i := 0 to List2.Count - 1 do // Iterate
  begin
    Control := TControl(List2[i]);
    if (Control is TCustomElToolButton) and (TCustomElToolButton(Control).ButtonType = ebtButton) and
       (not (csDestroying in ComponentState)) then
    begin
      MaxButtonHeight := Max(MaxButtonHeight, GetEffectiveButtonHeight(TCustomElToolButton(Control)));
    end;
  end;

  for i := 0 to List2.Count - 1 do // Iterate
  begin
    Control := TControl(List2[i]);
    if Control is TCustomElToolButton then
    begin
      if TCustomElToolButton(Control).FButtonType = ebtButton then
      begin
        if TCustomElToolButton(Control).UseArrow and (Orientation <> eboVert) then
          Control.SetBounds(Control.Left, Control.Top, Max(MaxButtonHeight, GetEffectiveButtonWidth(TCustomElToolButton(Control), true)), MaxButtonHeight)
        else
        begin
          if (Orientation = eboVert) then
            Control.SetBounds(Control.Left, Control.Top, MaxButtonWidth, GetEffectiveButtonHeight(TCustomElToolButton(Control)))
          else
            Control.SetBounds(Control.Left, Control.Top, Max(MaxButtonHeight, GetEffectiveButtonWidth(TCustomElToolButton(Control), true)), MaxButtonHeight);
        end
      end
      else
      if Orientation = eboVert then
        Control.SetBounds(Control.Left, Control.Top, GetEffectiveButtonWidth(TCustomElToolButton(Control), false), GetEffectiveButtonHeight(TCustomElToolButton(Control)))
      else
        Control.SetBounds(Control.Left, Control.Top, GetEffectiveButtonWidth(TCustomElToolButton(Control), false), MaxButtonHeight);
    end;

    if FOrientation = eboHorz then
    begin
      if (((Control is TCustomElToolButton) and (Control as TCustomElToolButton).Wrap) or
          (FAutoWrap and ((not AutoSize) or (Align in [alTop, alBottom]))) or
          (FShowMoreMenu and not (csDesigning in ComponentState))) and
         ((CX + Control.Width) > GetRealClientWidth) then
      begin
        b := false; // MoreMenu not involved
        (*
        if FShowMoreMenu and not (csDesigning in ComponentState) then
        begin
          if (not FMoreMenuActive) or (i < List2.Count - 1) or ((CX + Control.Width) >= ClientWidth + GetSystemMetrics(SM_CXHSCROLL)) then
          begin
            FNoReAlign := true;
            for j := List2.Count - 1 downto i do
            begin
              FMoreMenuItems.Insert(0, List2[j]);
              TControl(List2[j]).Parent := nil;
              // List2.Delete(j);
              // b := true;
            end;
            FNoReAlign := false;
            SetMoreMenuActive(true);
            dec(FUpdatingButtons);
            List2.Free;
            exit;
          end
          else
          begin
            if FMoreMenuActive then
               SetMoreMenuActive(false);
          end;
        end;
        *)
        if not b then
        begin
          CX := FBtnOffsHorz + loffs;

          if FHideable and (FOrientation = eboHorz) then Inc(CX, 12);

          Inc(CY, MaxButtonHeight + FBtnOffsVert);
        end;
      end;
      Control.SetBounds(CX, CY, Control.Width, MaxButtonHeight);
      if Control.Visible or (csDesigning in ComponentState) then
      begin
        Inc(CX, Control.Width + FBtnOffsHorz);
        FMaxX := Max(FMaxX, CX);
      end;
    end
    else
    begin
      if (((Control is TCustomElToolButton) and (Control as TCustomElToolButton).Wrap) or
          (FAutoWrap and ((not AutoSize) or (Align in [alLeft, alRight]))) {or
          (FShowMoreMenu and not (csDesigning in ComponentState))}) and
         ((CY + Control.Height) > GetRealClientHeight) then
      begin
        (*
        if FShowMoreMenu and not (csDesigning in ComponentState) then
        begin
          if (not FMoreMenuActive) or (i < List2.Count - 1) or ((CY + Control.Height) >= ClientHeight + GetSystemMetrics(SM_CYVSCROLL)) then
          begin
            FNoReAlign := true;
            for j := List2.Count - 1 downto i do
            begin
              FMoreMenuItems.Insert(0, List2[j]);
              TControl(List2[j]).Parent := nil;
            end;
            FNoReAlign := false;
            SetMoreMenuActive(true);
            dec(FUpdatingButtons);
            List2.Free;
            exit;
          end
          else
          begin
            if FMoreMenuActive then
               SetMoreMenuActive(false);
          end;
        end;
        *)
        CY := FBtnOffsVert + toffs;
        if FHideable and (FOrientation = eboVert) then Inc(CY, 12);
        Inc(CX, MaxButtonWidth + FBtnOffsHorz);
      end;

      Control.SetBounds(CX, CY, MaxButtonWidth, Control.Height);

      if Control.Visible or (csDesigning in ComponentState) then
      begin
        Inc(CY, Control.Height + FBtnOffsVert);
        FMaxY := Max(FMaxY, CY);
      end;
    end;
  end; // for

  if AutoSize then
  begin
    if FOrientation = eboHorz then
    begin
      Height := Max(FMinSize, CY + MaxButtonHeight + FBtnOffsVert + {toffs + }boffs + aoffs);

      if not (Align in [alTop, alBottom]) then
      begin
        Width := Max(FMaxX, FMinSize) + loffs + roffs + aoffs;
        if FHideable and (Width = FMinSize) then Width := Width + 12;
      end;
    end
    else
    begin
      Width := Max(FMinSize, CX + MaxButtonWidth + FBtnOffsHorz + {loffs + }roffs + aoffs);

      if not (Align in [alLeft, alRight]) then
      begin
        Height := Max(FMaxY, FMinSize) + toffs + boffs + aoffs;
        if FHideable and (Height = FMinSize) then
          Height := Height + 12;
      end;
    end;
  end;
  List2.Free;
  //Repaint;
  dec(FUpdatingButtons);
  Invalidate;
end;

procedure TElToolBar.SetShowGlyph(newValue : Boolean);
begin
  if (FShowGlyph <> newValue) then
  begin
    FShowGlyph := newValue;
    if FUpdateCount > 0 then exit;
    if csLoading in ComponentState then exit;
    UpdateButtons;
  end; {if}
end;

procedure TElToolBar.SetShowCaption(newValue : Boolean);
begin
  if (FShowCaption <> newValue) then
  begin
    FShowCaption := newValue;
    if FUpdateCount > 0 then exit;
    if csLoading in ComponentState then exit;
    UpdateButtons;
    AlignButtons;
  end; {if}
end;

procedure TElToolBar.SetLargeSize(newValue : Boolean);
var
  i : integer;
begin
  if (FLargeSize <> newValue) then
  begin
    FLargeSize := newValue;
    if FUpdateCount > 0 then exit;
    if csLoading in ComponentState then exit;
    for i := 0 to FButtons.Count - 1 do // Iterate
    begin
      TCustomElToolButton(FButtons[i]).SwitchGlyphs(FLargeSize);
    end; // for
    UpdateButtons;
    AlignButtons;
  end; {if}
end;

procedure TElToolBar.SetLargeBtnWidth(newValue : Integer);
begin
  if (FLargeBtnWidth <> newValue) then
  begin
    FLargeBtnWidth := newValue;
    if FUpdateCount > 0 then exit;
    if csLoading in ComponentState then exit;
    if FLargeSize then
    begin
      UpdateButtons;
      AlignButtons;
    end;
  end; {if}
end;

procedure TElToolBar.SetLargeBtnHeight(newValue : Integer);
begin
  if (FLargeBtnHeight <> newValue) then
  begin
    FLargeBtnHeight := newValue;
    if FUpdateCount > 0 then exit;
    if csLoading in ComponentState then exit;
    if FLargeSize then
    begin
      UpdateButtons;
      AlignButtons;
    end;
  end; {if}
end;

procedure TElToolBar.SetGlyphLayout(newValue : TButtonLayout);
begin
  if (FGlyphLayout <> newValue) then
  begin
    FGlyphLayout := newValue;
    if FUpdateCount > 0 then exit;
    if csLoading in ComponentState then exit;
    UpdateButtons;
  end; {if}
end;

procedure TElToolBar.SetSpacing(newValue : Integer);
begin
  if (FSpacing <> newValue) then
  begin
    FSpacing := newValue;
    if FUpdateCount > 0 then exit;
    if csLoading in ComponentState then exit;
    UpdateButtons;
  end; {if}
end;

procedure TElToolBar.SetMargin(newValue : Integer);
begin
  if (FMargin <> newValue) then
  begin
    FMargin := newValue;
    if FUpdateCount > 0 then exit;
    if csLoading in ComponentState then exit;
    UpdateButtons;
  end; {if}
end;

procedure TElToolBar.SetFlat(newValue : Boolean);
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;
    if FUpdateCount > 0 then exit;
    if csLoading in ComponentState then exit;
    UpdateButtons;
  end; {if}
end;

function TElToolBar.GetToolButton;
begin
  if (FButtons = nil) then
    result := nil
  else
    result := TElToolButton(FButtons[index]);
end;

procedure TElToolBar.SetToolButton;
begin
  if (FButtons <> nil) then TCustomElToolButton(FButtons[index]).Assign(newValue);
end;

function TElToolBar.GetButtonCount : Integer;
begin
  if (FButtons = nil) then
    result := 0
  else
    result := FButtons.Count;
end;

procedure TElToolBar.Loaded;
var
  IL: TImageList;
begin
  inherited;
  IL := FImages;
  FImages := nil;
  Images := IL;
  IL := FHotImages;
  FHotImages := nil;
  HotImages := IL;
  IL := FDisabledImages;
  FDisabledImages := nil;
  DisabledImages := IL;

  if UseImageList then
  begin
    if FImages <> nil then
    begin
      FUseImageList := false;
      UseImageList := true;
    end;
  end;
  if IsThemeApplied then
  begin
    FUseXPThemes := false;
    UseXPThemes := true;
  end;
  UpdateButtons;
  AlignButtons;
end;

{$ifndef CLX_USED}
procedure TElToolBar.CMControlListChange(var Msg : TMessage);
{$else}
procedure TElToolBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
{$endif}
begin
  inherited;
  {$ifndef CLX_USED}
  if not (boolean(Msg.LParam))
  {$else}
  if not Inserting
  {$endif}
  then AlignButtons;
end;

{$ifdef CLX_USED}
procedure TElToolbar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  inherited;
  if Inserting then
  begin
    if Control is TCustomElToolButton then
    begin
      FButtons.Add(Control);
      UpdateButtons;
    end;
    AlignButtons;
  end
  else
  begin
    if Control is TCustomElToolButton then
    begin
      FButtons.Remove(Control);
    end;
    if HandleAllocated and(not (csDestroying in ComponentState)) then
      AlignButtons;
  end;
end;
{$else}
procedure TElToolBar.CMControlChange(var Msg : TCMControlChange);
begin
  inherited;
  if Msg.Inserting then
  begin
    if Msg.Control is TCustomElToolButton then
    begin
      FButtons.Add(Msg.Control);
      UpdateButtons;
    end;
    AlignButtons;
  end
  else
  begin
    if Msg.Control is TCustomElToolButton then
    begin
      FButtons.Remove(Msg.Control);
    end;
    (*
    if HandleAllocated and(not (csDestroying in ComponentState)) then
      AlignButtons;
    *)
  end;
end;
{$endif}

{$ifdef CLX_USED}
procedure TElToolBar.Resize;
begin
  inherited;
  if FSaveAlign <> Align then
  begin
    if Align <> alClient then
    begin
      if Align in [alLeft, alRight] then
        Orientation := eboVert
      else
        Orientation := eboHorz;
      FSaveAlign := Align;
    end;
  end;
  if AutoWrap or AutoSize or FShowMoreMenu then
     AlignButtons;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElToolBar.WMSize(var Msg : TWMSize);
begin
  inherited;
  if FSaveAlign <> Align then
  begin
    if Align <> alClient then
    begin
      if Align in [alLeft, alRight] then
        Orientation := eboVert
      else
        Orientation := eboHorz;
      FSaveAlign := Align;
    end;
  end;
  if AutoWrap or AutoSize or FShowMoreMenu then
     AlignButtons;
end;
{$endif}

procedure TElToolBar.SetAutoSize(newValue : Boolean);
begin
  if (FAutoSize <> newValue) then
  begin
    FAutoSize := newValue;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end; {if}
end; {SetAutoSize}

function TElToolBar.AddButton(ButtonType : TELToolButtonType) : TElToolButton;
begin
  result := TElToolButton(GetButtonClass.Create(Self));
  Result.Parent := Self;
  Result.Left := 10000;
  Result.Top := 10000;
  Result.ButtonType := ButtonType;
  Result.FreeNotification(Self);
  Result.FButtonID := GetFreeButtonID + 1;
  if UseImageList then
  begin
    Result.UseImageList := true;
    Result.Images := FImages;
    Result.HotImages := FHotImages;
    Result.DisabledImages := FDisabledImages;
    Result.Transparent := TransparentButtons;
    Result.ThinFrame := FThinButtons;
    Result.Flat := FFlat;
    Result.ImageIsAlphaBlended := ImageIsAlphaBlended;
  end;
  UpdateButtons;
end;

procedure TElToolBar.SetMinSize(newValue : integer);
begin
  if (FMinSize <> newValue) then
  begin
    FMinSize := newValue;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end; {if}
end;

procedure TElToolBar.SetButtonColor(newValue : TColor);
begin
  if (FButtonColor <> newValue) then
  begin
    FButtonColor := newValue;
    if csLoading in ComponentState then exit;
    UpdateButtons;
  end; {if}
end;

procedure TElToolBar.SetOrientation(newValue : TElBarOrientation);
begin
  if (FOrientation <> newValue) then
  begin
    FOrientation := newValue;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end; {if}
end; {SetOrientation}
(*
procedure TElToolBar.SetHideable(newValue : Boolean);
begin
  if (FHideable <> newValue) then
  begin
    FHideable := newValue;
    AlignButtons;
    if FHidden then
    begin
      FHidden := false;
      SetHidden(true);
    end;
  end;  {if}
end;  {SetHideable}
*)

procedure TElToolBar.Paint;
//var R : TRect;
begin
  if FUpdateCount > 0 then exit;
  inherited;
end; {Paint}

procedure TElToolBar.SetUseLargeGlyphs(newValue : Boolean);
{ Sets data member FUseLargeGlyphs to newValue. }
begin
  if (FUseLargeGlyphs <> newValue) then
  begin
    FUseLargeGlyphs := newValue;
    if csLoading in ComponentState then exit;
    if FLargeSize then
    begin
      UpdateButtons;
      AlignButtons;
    end;
  end; { if }
end; { SetUseLargeGlyphs }

procedure TElToolBar.BeginUpdate; { public }
begin
  {$ifndef CLX_USED}
  if (FUpdateCount = 0) and (HandleAllocated) then
  begin
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
  end;
  {$endif}
  inc(FUpdateCount);
end; { BeginUpdate }

procedure TElToolBar.EndUpdate; { public }
begin
  dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    UpdateButtons;
    {$ifndef CLX_USED}
    if (FUpdateCount = 0) and (HandleAllocated) then
    begin
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
      //Repaint;
    end;
    {$endif}
    AlignButtons;
  end;
end; { EndUpdate }

procedure TElToolBar.SetImages(newValue : TImageList);
var
  i : integer;
  b, b1 : boolean;
begin
  if (FImages <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if (FImages <> nil) and (not (csDestroying in FImages.ComponentState)) then
      FImages.RemoveFreeNotification(Self);
    {$endif}

    b := (FImages = nil) and (newValue <> nil);
    FImages := newValue;
    if csLoading in ComponentState then exit;
    BeginUpdate;
    for i := 0 to FButtons.Count - 1 do // Iterate
    begin
      b1 := TCustomElToolButton(FButtons[i]).UseImageList;
      TCustomElToolButton(FButtons[i]).Images := FImages;
      TCustomElToolButton(FButtons[i]).UseImageList := b1;
    end;
    if FImages <> nil then FImages.FreeNotification(Self);
    if b then UseImageList := true;
    EndUpdate;
  end; { if }
end; { SetImages }

procedure TElToolBar.SetHotImages(newValue : TImageList);
var
  I : Integer;
begin
  if (FHotImages <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if (FHotImages <> nil) and (not (csDestroying in FHotImages.ComponentState)) then
      FHotImages.RemoveFreeNotification(Self);
    {$endif}
    FHotImages := newValue;
    if csLoading in ComponentState then exit;
    for i := 0 to FButtons.Count - 1 do // Iterate
      TCustomElToolButton(FButtons[i]).HotImages := FHotImages;
    if FHotImages <> nil then FHotImages.FreeNotification(Self);
  end; { if }
end; { SetHotImages }

procedure TElToolBar.SetDisabledImages(newValue : TImageList);
var
  I : Integer;
begin
  if (FDisabledImages <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if (FDisabledImages <> nil) and (not (csDestroying in FDisabledImages.ComponentState)) then
      FDisabledImages.RemoveFreeNotification(Self);
    {$endif}
    FDisabledImages := newValue;
    if csLoading in ComponentState then exit;
    for i := 0 to FButtons.Count - 1 do // Iterate
      TCustomElToolButton(FButtons[i]).DisabledImages := FDisabledImages;
    if FDisabledImages <> nil then FDisabledImages.FreeNotification(Self);
  end; { if }
end; { SetDisabledImages }

procedure TElToolBar.Notification(AComponent : TComponent; operation : TOperation);
// var i : integer;
begin
  inherited Notification(AComponent, operation);
  if AComponent is TCustomElToolButton then
  begin
    if (operation = opRemove) then
    begin
      if FButtons <> nil then
        FButtons.Remove(AComponent);
      (*
      if FMoreMenuItems <> nil then
      begin
        i := FMoreMenuItems.IndexOf(AComponent);
        if i >= 0 then
        begin
          FMoreMenuItems.Delete(i);
          if FMoreMenu <> nil then
            FMoreMenu.Items.Delete(i);
        end;
      end;
      *)
    end
    else
    begin
      // FButtons.Add();
    end;
  end;
  if (operation = opRemove) then
  begin
    if (AComponent = FImages) then Images := nil;
    if (AComponent = FHotImages) then HotImages := nil;
    if (AComponent = FDisabledImages) then DisabledImages := nil;
    {$ifndef CLX_USED}
    if (AComponent = FButtonImageForm) then
    begin
      FButtonImageForm := nil;
      if not (csDestroying in ComponentState) then UpdateButtons;
    end;
    {$endif}
  end; { if }
end; { Notification }

procedure TElToolBar.SetUseImageList(newValue : Boolean);
var
  i : integer;
  B : TCustomElToolButton;
begin
  if (FUseImageList <> newValue) then
  begin
    FUseImageList := newValue;
    if csLoading in ComponentState then exit;
    if Images <> nil then
      for i := 0 to FButtons.Count - 1 do // Iterate
      begin
        B := TCustomElToolButton(FButtons[i]);
        if B.OwnerSettings then
        begin
          B.Images := FImages;
          B.HotImages := FHotImages;
          B.DisabledImages := FDisabledImages;
          B.UseImageList := newValue;
        end;
      end; // for
  end; { if }
end; { SetUseImageList }

procedure TElToolBar.SetTransparentButtons(newValue : Boolean);
var
  i : integer;
  B : TCustomElToolButton;
begin
  if (FTransparentButtons <> newValue) then
  begin
    FTransparentButtons := newValue;
    if csLoading in ComponentState then exit;
    for i := 0 to FButtons.Count - 1 do // Iterate
    begin
      B := TCustomElToolButton(FButtons[i]);
      if (B.OwnerSettings) then
        B.Transparent := newValue;
    end; // for
  end; { if }
end; { SetTransparentButtons }

{$ifndef CLX_USED}
procedure TElToolBar.SetMoreMenuActive(newValue : Boolean);
begin
  (*
  if (FMoreMenuActive <> newValue) then
  begin
    FMoreMenuActive := newValue;
    if FShowMoreMenu then
      RecreateWnd;
  end;
  *)
end;

procedure TElToolBar.SetShowMoreMenu(newValue : Boolean);
{ Sets data member FShowMoreMenu to newValue. }
begin
  if (FShowMoreMenu <> newValue) then
  begin
    // FShowMoreMenu := newValue;
    (*
    if HandleAllocated and (not (csLoading in ComponentState)) and
      (FMoreMenuActive or FShowMoreMenu) then
      RecreateWnd;
    *)
  end;  { if }
end;  { SetShowMoreMenu }

procedure TElToolBar.WMNCCalcSize(var Msg : TWMNcCalcSize);  { private }
begin
  inherited;
  (*
  if (FShowMoreMenu and FMoreMenuActive) and (not (csDesigning in ComponentState)) then
  begin
    if Orientation = eboHorz then
       dec(Msg.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CXHSCROLL))
    else
       dec(Msg.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CYVSCROLL));
  end;
  *)
end;  { WMNCCalcSize }

procedure TElToolBar.WMNCPaint(var Msg : TMessage);  { private }
//var DC : HDC;

    (*
    procedure DrawMoreBtn(DC : HDC);
    var RC, RC1 : TRect;
        Canvas : TCanvas;
    begin
      RC := GetMoreBtnRect;

      Canvas := TCanvas.Create;
      Canvas.Handle := DC;
      Canvas.Brush.Color := Color;
      Canvas.FillRect(rc);
      ElVCLUtils.DrawButtonFrameEx(DC, rc, false, FMoreMenuVisible, Color, false);
      CenterRects(9, RC.Right - RC.Left, 5, RC.Bottom - RC.Top, RC1);
      if Orientation = eboHorz then
      begin
        OffsetRect(RC1, RC.Left, 0);
        RC1.Top := RC.Top + 3;
      end
      else
      begin
        OffsetRect(RC1, 0, RC.Top);
        RC1.Left := RC.Left + 3;
      end;

      DrawTransparentBitmapEx(DC, FMoreGlyph, RC1.Left, RC1.Top, Rect(0, 0, 9, 5), FMoreGlyph.Canvas.Pixels[0, FMoreGlyph.Height - 1]);
      Canvas.Handle := 0;
      Canvas.Free;
    end;
    *)
begin
  inherited;
  (*
  if (FShowMoreMenu and FMoreMenuActive) and (not (csDesigning in ComponentState)) then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC <> 0 then
       DrawMoreBtn(DC)
    else
    begin
      DC := GetWindowDC(Handle);
      DrawMoreBtn(DC);
    end;
    ReleaseDC(Handle, DC);
  end;
  Msg.Result := 0;
  *)
end;  { WMNCPaint }

procedure TElToolBar.WMNCHitTest(var Msg : TMessage);  { private }
(*
var rc : TRect;
    HT,
    VT : integer;
    pt : TPoint;
*)
begin
  inherited;
  (*
  if (FShowMoreMenu and FMoreMenuActive) then
  begin
    if Orientation = eboHorz then
    begin
      HT := GetSystemMetrics(SM_CXHSCROLL);
      RC.Left := Width - (Parent.ClientOrigin.X - ClientOrigin.X) - HT;
      RC.Right := RC.Left + HT;
      RC.Top := Parent.ClientOrigin.Y - ClientOrigin.Y;
      RC.Bottom := RC.Top + ClientHeight;
    end else
    begin
      VT := GetSystemMetrics(SM_CYVSCROLL);
      RC.Top := Height - (Parent.ClientOrigin.Y - ClientOrigin.Y) - VT;
      RC.Bottom := RC.Top + VT;
      RC.Left := Parent.ClientOrigin.X - ClientOrigin.X;
      RC.Right := RC.Left + ClientWidth;
    end;
    pt.x := LOWORD(Msg.lParam);
    pt.y := HIWORD(Msg.lParam);
    pt := ScreenToClient(pt);
    if PtInRect(rc, pt) then
    begin
      if FIgnoreMoreClick then
      begin
        FIgnoreMoreClick := false;
        Msg.Result := HTNOWHERE;
      end
      else
        Msg.result := HTBORDER;
    end;
  end;
  *)
end;  { WMNCHitTest }

procedure TElToolBar.OnMoreItemClick(Sender : TObject);
begin
(*  if (Sender is TMenuItem) and (TMenuItem(Sender).Tag <> 0) then
     TCustomElToolButton(FMoreMenuItems[TMenuItem(Sender).Tag]).Click(false);
     *)
end;

procedure TElToolBar.PutMoreItemsToBar;
begin
  if FNoRealign then exit;
  FNoReAlign := true;
  (*
  while FMoreMenuItems.Count > 0 do
  begin
    if TControl(FMoreMenuItems[0]) is TCustomElToolButton then
      FButtons.Add(TControl(FMoreMenuItems[0]));
    TControl(FMoreMenuItems[0]).Parent := Self;
    FMoreMenuItems.Delete(0);
  end;
  *)
  FNoReAlign := false;
end;

procedure TElToolBar.StartMoreMenu;
(*
var i, j : integer;
    Item : TMenuItem;
    pt   : TPoint;
    rc   : TRect;
*)
begin
  (*
  if FMoreMenuVisible then exit;
  if FMoreMenu = nil then FMoreMenu := TPopupMenu.Create(nil);
  {$IFDEF VCL_5_USED}
  FMoreMenu.Items.Clear;
  {$ELSE}
  while FMoreMenu.Items.Count > 0 do
        FMoreMenu.Items.Delete(0);
  {$ENDIF}
  j := FMoreMenuItems.Count - 1;
  for i := 0 to j do
  begin
    if not (TObject(FMoreMenuItems[i]) is TCustomElToolButton) then continue;
    if TCustomElToolButton(FMoreMenuItems[i]).FButtonType = ebtButton then
    begin
       Item := NewItem(TCustomElToolButton(FMoreMenuItems[i]).Caption, 0, false, true, OnMoreItemClick, 0, '');
       Item.Tag := i;
    end
    else
       Item := NewItem('-', 0, false, false, nil, 0, '');
    FMoreMenu.Items.Add(Item);
  end;
  FMoreMenu.PopupComponent := Self;
  if Orientation = eboHorz then
  begin
    RC := GetMoreBtnRect;
    pt.x := Rc.Right;
    pt.y := Rc.Bottom;
    pt := ClientToScreen(pt);
    FMoreMenuVisible := true;
    OffsetRect(rc, -(Parent.ClientOrigin.X - ClientOrigin.X), - (Parent.ClientOrigin.X - ClientOrigin.X));
    RedrawWindow(Handle, @rc, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE);

    TrackPopupMenu(FMoreMenu.Handle, TPM_RIGHTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON, pt.X, pt.Y, 0, Handle, nil);
  end else
  begin
    RC := GetMoreBtnRect;
    pt.x := Rc.Right;
    pt.y := Rc.Top;
    pt := ClientToScreen(pt);
    FMoreMenuVisible := true;
    OffsetRect(rc, -(Parent.ClientOrigin.X - ClientOrigin.X), - (Parent.ClientOrigin.X - ClientOrigin.X));
    RedrawWindow(Handle, @rc, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE);

    TrackPopupMenu(FMoreMenu.Handle, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON, pt.X, pt.Y, 0, Handle, nil);
  end;
  *)
end;

procedure TElToolBar.WMNCLButtonDown(var Msg : TWMNCLButtonDown);  { private }
(*
var pt : TPoint;
    rc : TRect;
*)
begin
  inherited;
  (*
  if (FShowMoreMenu and FMoreMenuActive) and (FMoreMenuItems.Count > 0) then
  begin
    rc := Self.GetMoreBtnRect;
    pt.x := Msg.XCursor;
    pt.y := Msg.YCursor;
    pt := ScreenToClient(pt);
    if PtInRect(rc, pt) and (not (csDesigning in ComponentState)) then
    begin
      StartMoreMenu;
    end;
  end;
  *)
end;  { WMNCLButtonDown }

{$WARNINGS OFF}
{$WARNINGS ON}

procedure TElToolBar.RedrawMoreBtn;  { protected }
var rc : TRect;
begin
  RC := GetMoreBtnRect;
  OffsetRect(rc, -(Parent.ClientOrigin.X - ClientOrigin.X), - (Parent.ClientOrigin.X - ClientOrigin.X));
  RedrawWindow(Handle, @rc, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE);
end;  { RedrawMoreBtn }
{$endif}

function TElToolBar.GetRealClientWidth : integer;
var R : TRect;
begin
  R := ClientRect;
  AdjustClientRect(R);
  result := R.Right - R.Left;
end;

function TElToolBar.GetRealClientHeight : integer;
var R : TRect;
begin
  R := ClientRect;
  AdjustClientRect(R);
  result := R.Bottom - R.Top;
end;

{$ifndef CLX_USED}
function TElToolBar.GetMoreBtnRect : TRect;  { protected }
var rc : TRect;
    HT,
    VT : integer;

begin
  if Orientation = eboHorz then
  begin
    HT := GetSystemMetrics(SM_CXHSCROLL);
    RC.Left := Width - (Parent.ClientOrigin.X - ClientOrigin.X) - HT;
    RC.Right := RC.Left + HT;
    RC.Top := Parent.ClientOrigin.Y - ClientOrigin.Y;
    RC.Bottom := RC.Top + ClientHeight;
  end else
  begin
    VT := GetSystemMetrics(SM_CYVSCROLL);
    RC.Top := Height - (Parent.ClientOrigin.Y - ClientOrigin.Y) - VT;
    RC.Bottom := RC.Top + VT;
    RC.Left := Parent.ClientOrigin.X - ClientOrigin.X;
    RC.Right := RC.Left + ClientWidth;
  end;
  result := rc;
end;  { GetMoreBtnRect }

procedure TElToolBar.WMWindowPosChanged(var Msg : TWMWindowPosChanged);  { private }
begin
  inherited;
  (*
  if FShowMoreMenu and FMoreMenuActive then
     RedrawMoreBtn;
  *)
end;  { WMWindowPosChanged }
{$endif}

(*
procedure TElToolBar.SetHidden(newValue : Boolean);
begin
  if (FHidden <> newValue) then
  begin
    FHidden := newValue;
    if FHideable then
    begin
      if FHidden then
      begin
        if Orientation = eboHorz then
        begin
          FSaveSize := Height;
          Height := FBtnOffsVert;
        end else
        begin
          FSaveSize := Width;
          Width := FBtnOffsHorz;
        end;
      end else
      begin
        if Orientation = eboHorz then Height := FSaveSize else Width := FSaveSize;
      end;
    end;
  end;  {if}
end;  {SetHidden}
*)

destructor TElToolBar.Destroy;
begin
  FButtons.Free;
  FButtons := nil;
  (*if FMoreMenu <> nil then
     FMoreMenu.Free;
  FMoreMenu := nil;
  FMoreMenuItems.Free;
  FMoreMenuItems := nil;
  *)
  inherited Destroy;
end;

constructor TElToolBar.Create(AOwner : TComponent);
begin
  FCreating := true;
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FCreating := false;
  FBtnWidth := 24;
  FBtnHeight := 24;
  FBtnOffsHorz := 3;
  FBtnOffsVert := 3;
  FButtons := TElList.Create;
  FShowGlyph := True;
  FShowCaption := False;
  FLargeSize := False;
  FLargeBtnWidth := 48;
  FLargeBtnHeight := 48;
  FGlyphLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin := -1;
  FFlat := True;
  Height := 28;
  FAutoSize := True;
  FMinSize := 24;
  FButtonColor := clBtnFace;
  FOrientation := eboHorz;
  TabStop := false;
  FSaveAlign := alTop;
  Align := alTop;
  FHidden := False;
  FAdjustButtonWidth := true;
  FAdjustButtonHeight := true;
  // FMoreMenuItems := TElList.Create;
end;

procedure TElToolBar.SetThinButtons(Value: Boolean);
var
  i : integer;
  B : TCustomElToolButton;
begin
  if FThinButtons <> Value then
  begin
    FThinButtons := Value;
    if csLoading in ComponentState then exit;
    for i := 0 to FButtons.Count - 1 do // Iterate
    begin
      B := TCustomElToolButton(FButtons[i]);
      B.ThinFrame := Value;
    end; // for
  end;
end;

procedure TElToolBar.DrawThemedBackground;
begin
  if not Transparent then
  begin
    {$ifdef CLX_USED}
    {$ifdef MSWINDOWS}
    Canvas.Start;
    DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), 0, 0, ClientRect, nil);
    Canvas.Stop;
    {$endif}
    {$endif}
  end;
end;

function TElToolBar.GetThemedClassName: WideString;
begin
  Result := 'TOOLBAR';
end;

{$ifndef CLX_USED}
procedure TElToolBar.WMEraseBkGnd(var Msg : TWMEraseBkGnd);
var R : TRect;
begin
  if IsThemeApplied then
  begin
    if (not FTransparent) and (not TransparentXPThemes) then
      DrawThemeBackground(Theme, Msg.DC, 0, 1, ClientRect, nil)
    else
    begin
      R := ClientRect;
      DrawThemeParentBackground(Handle, Msg.DC, R);
    end;
  end;
  Msg.Result := 1;
end;

{$endif}


function TElToolBar.GetFreeButtonID: Integer;
begin
  result := 0;
  while GetButtonByID(result) <> nil do
    Inc(Result);
end;

function TElToolBar.GetButtonByID(ID : Integer): TElToolButton;
var i : integer;
begin
  i := 0;
  while i < FButtons.Count do
    if TCustomElToolButton(FButtons[i]).FButtonID = ID then
    begin
      Result := FButtons[i];
      exit;
    end
    else
      inc(i);
  Result := nil;
end;

procedure TElToolBar.Save;
var
  SaveKey : string;
  i : integer;
  VisibleNames,
  HiddenNames   : string;
  AControl      : TControl;
  ControlName   : string;
  DividerCount,
  SeparatorCount: integer;
  List          : TElList;
begin
  if Assigned(FStorage) then
  begin
    SaveKey := FStorage.CurrentKey;
    if FStorage.OpenKey(StoragePath + FStorage.Delimiter + 'Toolbar', true) then
    begin
      VisibleNames   := '';
      HiddenNames    := '';
      DividerCount   := 0;
      SeparatorCount := 0;
      FStorage.WriteBool('', 'LargeIcons', Self.LargeSize);
      FStorage.WriteBool('', 'ShowCaption', Self.ShowCaption);
      List := TElList.Create;
      OrderedControls(List);

      for i := 0 to List.Count - 1 do
      begin
        AControl := TControl(List[i]);
        ControlName := AControl.Name;
        if AControl.Name = '' then
        begin
          if AControl is TCustomElToolButton then
          begin
            case TCustomElToolButton(AControl).ButtonType of
              ebtButton:
                Continue;
              ebtSeparator:
                begin
                  while FindComponent('Separator' + IntToStr(SeparatorCount)) <> nil do
                    inc(SeparatorCount);
                  ControlName := 'Separator' + IntToStr(SeparatorCount);
                  Inc(SeparatorCount);
                end;
              ebtDivider:
                begin
                  while FindComponent('Divider' + IntToStr(DividerCount)) <> nil do
                    inc(DividerCount);
                  ControlName := 'Divider' + IntToStr(DividerCount);
                  Inc(DividerCount);
                end;
            end;
          end
          else
            Continue;
        end;
        if ((AControl is TCustomElToolButton) and TCustomElToolButton(AControl).FRealVisible) or AControl.Visible then
        begin
          if VisibleNames = '' then
            VisibleNames := ControlName
          else
            VisibleNames := VisibleNames + ',' + ControlName;
        end
        else
        begin
          if HiddenNames = '' then
            HiddenNames := ControlName
          else
            HiddenNames := HiddenNames + ',' + ControlName;
        end;
      end;
      List.Free;
      FStorage.WriteString('', 'Visible', VisibleNames);
      FStorage.WriteString('', 'Hidden', HiddenNames);
    end;
  end;
end;

procedure TElToolBar.Restore;
var
  SaveKey,
  S : string;
  i : integer;
  b : boolean;
  VisibleNames,
  HiddenNames   : TStringList;
  Parser        : TElStringTokenizer;
  AControl      : TControl;

  function ControlByName(Name : string) : TControl;
  var i : integer;
  begin
    for i := 0 to ControlCount - 1 do
    begin
      if Controls[i].Name = Name then
      begin
        result := Controls[i];
        exit;
      end;
    end;
    result := nil;
  end;

begin
  if Assigned(FStorage) then
  begin
    SaveKey := FStorage.CurrentKey;
    if FStorage.OpenKey(StoragePath + FStorage.Delimiter + 'Toolbar', false) then
    begin
      BeginUpdate;
      Parser := TElStringTokenizer.Create;
      Parser.Delimiters := ',';

      if FStorage.ReadBool('', 'LargeIcons', LargeSize, b) then
        LargeSize := b;
      if FStorage.ReadBool('', 'ShowCaption', ShowCaption, b) then
        ShowCaption := b;

      VisibleNames := TStringList.Create;
      HiddenNames  := TStringList.Create;

      FStorage.ReadString('', 'Visible', '', S);
      Parser.SourceString := S;
      Parser.FindAll(VisibleNames);

      FStorage.ReadString('', 'Hidden', '', S);
      Parser.SourceString := S;
      Parser.FindAll(HiddenNames);

      // first show/hide controls
      for i := 0 to ControlCount - 1 do // Iterate
      begin
        AControl := Controls[i];
        if AControl.Name = '' then
        begin
          if AControl is TCustomElToolButton then
          begin
            if TCustomElToolButton(AControl).ButtonType = ebtButton then
            begin
              TCustomElToolButton(AControl).Visible := false;
              TCustomElToolButton(AControl).FRealVisible := false;
            end
            else
              TCustomElToolButton(AControl).Free;
          end;
        end
        else
        begin
          if AControl is TCustomElToolButton then
          begin
            if VisibleNames.IndexOf(AControl.Name) <> -1 then
            begin
              {$ifdef VCL_4_USED}
              TCustomElToolButton(AControl).Visible := (not (TCustomElToolButton(AControl).Action is TAction)) or ((TCustomElToolButton(AControl) <> nil) and (TAction(TCustomElToolButton(AControl).Action).Visible));
              {$else}
              AControl.Visible := true;
              {$endif}
              TCustomElToolButton(AControl).FRealVisible := true;
            end
            else
            begin
              TCustomElToolButton(AControl).FRealVisible := false;
              TCustomElToolButton(AControl).Visible := false;
            end;
          end;
        end;
      end; // for

      // now reorder controls and add dividers/separators
      for i := 0 to VisibleNames.Count - 1 do
      begin
        AControl := ControlByName(VisibleNames[i]);
        if AControl = nil then
        begin
          // this might be a divider or separator
          if Pos('Divider', VisibleNames[i]) = 1 then
            AControl := AddButton(ebtDivider)
          else
          if Pos('Separator', VisibleNames[i]) = 1 then
            AControl := AddButton(ebtSeparator)
          else
            Continue;
        end;
        AControl.Left := 10000 + i;
        AControl.Top := 10000 + i;
      end;

      EndUpdate;
      Parser.Free;
      VisibleNames.Free;
      HiddenNames.Free;
      FStorage.OpenKey(SaveKey, false);
    end;
  end;
end;

{$ifndef CLX_USED}
function TElToolBar.Setup(ShowTextOptions, ShowIconOptions : boolean): Boolean;
begin
  result := false;
  with TfrmToolbarSetup.Create(nil) do
  begin
    try
      LoadToolbarControls(Self);

      IconOptionsLabel.Visible := ShowIconOptions;
      IconOptionsCombo.Visible := ShowIconOptions;
      TextOptionsLabel.Visible := ShowTextOptions;
      TextOptionsCombo.Visible := ShowTextOptions;

      if ShowModal = mrOk then
      begin
        SaveToolbarControls(Self);
        Result := true;
        AlignButtons;
      end;
    finally
      Free;
    end;
  end;
end;
{$endif}

procedure TElToolBar.OrderedControls(L : TElList);
var
  np       : integer;
  CC1,
  CC2      : TControl;
  List1    : TElList;
  i        : integer;
  b        : boolean;  
begin
  List1 := TElList.Create;
  // first arrange all controls in the proper order
  for i := 0 to ControlCount - 1 do // Iterate
    List1.Add(Controls[i]);
  while List1.Count > 0 do
  begin
    np := L.Count;
    if L.Count = 0 then
    begin
      L.Add(List1[0]);
      List1.Delete(0);
    end
    else
    begin
      CC1 := TControl(List1[0]);
      CC2 := TControl(L[np - 1]);
      if Orientation = eboHorz then
        b := (CC1.Top < CC2.Top) or ((CC1.Top = CC2.Top) and (CC1.Left < CC2.Left))
      else
        b := (CC1.Left < CC2.Left) or ((CC1.Left = CC2.Left) and (CC1.Top < CC2.Top));
      while b do
      begin
        dec(np);
        if NP = 0 then break;
        CC2 := TControl(L[np - 1]);
        if Orientation = eboHorz then
          b := (CC1.Top < CC2.Top) or ((CC1.Top = CC2.Top) and (CC1.Left < CC2.Left))
        else
          b := (CC1.Left < CC2.Left) or ((CC1.Left = CC2.Left) and (CC1.Top < CC2.Top));
      end;
      L.Insert(np, CC1);
      List1.Delete(0);
    end;
  end;
  List1.Free;
end;

procedure TElToolBar.SetAdjustButtonWidth(Value: Boolean);
begin
  if FAdjustButtonWidth <> Value then
  begin
    FAdjustButtonWidth := Value;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end;
end;

function TElToolBar.GetEffectiveButtonWidth(Button : TCustomElToolButton; 
    IncludeArrow : boolean): Integer;
begin
  if Button.ButtonType = ebtButton then
  begin         
    if AdjustButtonWidth then
    begin
      if Self.LargeSize then
        result := LargeBtnWidth
      else
        result := BtnWidth;
      result := Max(result, Button.MeasureButton(true).x);
    end
    else
    begin
      if Self.LargeSize then
        result := LargeBtnWidth
      else
        result := BtnWidth;
    end;
    //if IncludeArrow and Button.UseArrow then
    //    inc(Result, Button.GetArrowSize);
  end
  else
  begin
    if Self.LargeSize then
      result := LargeBtnWidth div DEF_SepSize
    else
      result := BtnWidth div DEF_SepSize;
  end;
end;

procedure TElToolBar.SetAdjustButtonHeight(Value: Boolean);
begin
  if FAdjustButtonHeight <> Value then
  begin
    FAdjustButtonHeight := Value;
    if csLoading in ComponentState then exit;
    AlignButtons;
  end;
end;

function TElToolBar.GetEffectiveButtonHeight(Button : TCustomElToolButton): 
    Integer;
begin
  if Button.ButtonType = ebtButton then
  begin
    if AdjustButtonHeight then
    begin
      if Self.LargeSize then
        result := LargeBtnHeight
      else
        result := BtnHeight;
      result := Max(result, Button.MeasureButton(false).y);
    end
    else
    begin
      if Self.LargeSize then
        result := LargeBtnHeight
      else
        result := BtnHeight;
    end;
  end
  else
  begin
    if Self.LargeSize then
      result := LargeBtnHeight div DEF_SepSize
    else
      result := BtnHeight div DEF_SepSize;
  end;
end;

{$ifndef CLX_USED}
type
{$ifndef D_4_UP}
  PTrackMouseEvent = ^TTrackMouseEvent;
  tagTRACKMOUSEEVENT = record
    cbSize: DWORD;
    dwFlags: DWORD;
    hwndTrack: HWND;
    dwHoverTime: DWORD;
  end;
  TTrackMouseEvent = tagTRACKMOUSEEVENT;
{$endif}

  TTrackMouseEventProc = function (tme : PTrackMouseEvent) : bool; stdcall;

procedure TElToolBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInControl := true;
  StartLeaveTracking;
end;

procedure TElToolBar.WMMouseLeave(var Message: TMessage);
var i : integer;
begin
  inherited;
  for i := 0 to ControlCount - 1 do
  begin
    //if Controls[i] is TElGraphicButton then
    begin
      Controls[i].Perform(CM_MOUSELEAVE, 0, 0);
    end;
  end;
  FMouseInControl := false;
end;


procedure TElToolBar.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := false;
  inherited;
end;

procedure TElToolBar.WMMouseMove(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl then
  begin
    FMouseInControl := true;
    StartLeaveTracking;
  end;
end;

procedure TElToolBar.StartLeaveTracking;
var TME : TTrackMouseEvent;
    Module : HModule;
    TrackMouseEvent : TTrackMouseEventProc;
begin
  if IsWin98Up or IsWinNTUp then
  begin
    Module := GetModuleHandle('user32.dll');
    if Module <> 0 then
    begin
      TrackMouseEvent := GetProcAddress(Module, 'TrackMouseEvent');
      if @TrackMouseEvent <> nil then
      begin
        TME.cbSize := sizeof(TME);
        TME.dwFlags := TME_LEAVE;
        TME.hwndTrack := Handle;
        TME.dwHoverTime := 0;
        TrackMouseEvent(@tme);
      end;
    end;
  end;
end;

{$endif}

function TElToolBar.GetButtonClass: TElToolButtonClass;
begin
  Result := TElToolButton;
end;

procedure TElToolBar.SetFocusedButton(Value: TElToolButton);
begin
  if FFocusedButton <> Value then
  begin
    FFocusedButton := Value;
  end;
end;

{$ifndef CLX_USED}
procedure TElToolbar.CMEnabledChanged(var Msg : TMessage);
{$else}
procedure TElToolbar.EnabledChanged;
{$endif}
var i : integer;
begin
  inherited;
  for i := 0 to FButtons.Count - 1 do
    TCustomElToolButton(FButtons[i]).Invalidate;
end;  { CMEnabledChanged }

procedure TElToolBar.SetTransparent(newValue : Boolean);
{ Sets data member FTransparent to newValue. }
begin
  if not IsThemeApplied then
    inherited
  else
  begin
    // FTransparent := newValue;
    TransparentXPThemes := true;
    {$ifndef CLX_USED}
    RecreateWnd;
    {$endif}
  end;
end; { SetTransparent }

procedure TElToolBar.SetUseXPThemes(const Value: Boolean);
begin
  if UseXPThemes <> Value then
  begin
    inherited;
    if ThemesAvailable then
    begin
      if (not Value) and Transparent then
      begin
        FTransparent := false;
        Transparent := true;
      end
      else
      if (Value and Transparent) then
      begin
        Transparent := false;
        FTransparent := true;
      end;
      if csLoading in ComponentState then exit;
      UpdateButtons;
    end;
  end;
end;

function TElToolBar.GetNextButton(CurrentButton : TCustomElToolButton; Forward
    : boolean; IncludeDisabled : boolean): TCustomElToolButton;
var i : integer;
begin
  if CurrentButton <> nil then
    i := FButtons.IndexOf(CurrentButton)
  else
    i := -1;
  if i = -1 then
  begin
    if not Forward then
      i := FButtons.Count;
  end;

  result := nil;
  while true do
  begin
    if Forward then
    begin
      inc(i);
      if i >= FButtons.Count then
        i := 0;
    end
    else
    begin
      dec(i);
      if i < 0 then
        i := FButtons.Count - 1;
    end;
    if i >= FButtons.Count then
      result := nil
    else
      result := TCustomElToolButton(FButtons[i]);
    if result = nil then
      break;
    if (IncludeDisabled or Result.Enabled) and Result.Visible then
      break;
  end;
end;

procedure TElToolBar.DoEnter;
var // i : integer;
    Control : TWinControl;
begin
  inherited;
  Control := FindNextControl(Self, true, true, true);
  if Control <> nil then
    Control.SetFocus;
  (*
  for i := 0 to ControlCount - 1 do
  begin
    {$ifndef CLX_USED}
    if Controls[i] is TWinControl then
    begin
      TWinControl(Controls[i]).SetFocus;
      break;
    end;
    {$endif}
  end;
  *)
end;

{$ifndef CLX_USED}
procedure TElToolBar.SetParent(AParent: TWinControl);
begin
  inherited;
  if (AParent <> nil) and (AParent.ClassName = 'TCoolBar') then
    Self.AutoSize := false;
end;

{$endif}

{$ifndef CLX_USED}
procedure TElToolBar.CMFontChanged(var Message: TMessage);
{$else}
procedure TElToolBar.FontChanged;
{$endif}
begin
  inherited;
  AlignButtons;
end;

procedure TElToolBar.SetImageIsAlphaBlended(Value: Boolean);
var i : integer;
begin
  if FImageIsAlphaBlended <> Value then
  begin
    FImageIsAlphaBlended := Value;
    if csLoading in ComponentState then exit;
    for i := 0 to FButtons.Count - 1 do
      TElToolButton(FButtons[i]).ImageIsAlphaBlended := Value; 
  end;
end;

{$ifdef VCL_4_USED}
function TElToolButtonActionLink.IsVisibleLinked: Boolean;
begin
  result := inherited IsVisibleLinked;
end;

procedure TElToolButtonActionLink.SetVisible(Value: Boolean);
var b : boolean;
begin
  inherited;
  if IsVisibleLinked then
  begin
    b := (FClient as TElToolButton).FRealVisible;
    (FClient as TElToolButton).ActionVisibleInverted := true;
    if Value and b then
      FClient.Visible := true
    else
      FClient.Visible := false;
    (FClient as TElToolButton).ActionVisibleInverted := false;
    (FClient as TElToolButton).FRealVisible := b;
  end;
end;
{$endif}

initialization

  FMoreGlyph := TBitmap.Create;
  FMoreGlyph.LoadFromResourceName(HInstance, 'ELTOOLBARMOREARROW');

finalization

  FMoreGlyph.Free;
  FMoreGlyph := nil;

end.

