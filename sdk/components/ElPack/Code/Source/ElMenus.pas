{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Portion copyright (c) 2001, Alexander Hramov     }
{   Portion copyright (c) 1997, Borland Int.         }
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

05/05/2002

  MainMenu blocked painting of popup menus. Fixed

05/03/2002

  Added Clear method

04/27/2002

  Fixed drawing of the separator line in OfficeXP style
  OnClick was not fired for menu items that have sub-menus. Fixed.

04/25/2002

  Fixed some HTML-related bugs
  Sort of fixed the problem with overlapping text and sub-menu arrow

04/17/2002

  Increased spacing between text and shortcut
  Added right-to-left support
  Fixed a bug that corrupted IDE menus (thanks to Graham Powell)

04/04/2002

  Added IsHTML property and possibility to draw HTML

03/13/2002

  Fixed various painting issues

03/12/2002

  Fixed resource leak

03/09/2002

  Increased calculation of space in main menu
  Improved painting of the selected items
  ElPopupMenu.Popup function worked incorrectly. Fixed.

03/06/2002

  Fixed different painting issues

02/26/2002

  Fixed painting, font handling, image list handling issues

02/12/2002

  Add TElMenuItem.Find method


*)

unit ElMenus; { TElMainMenu & TElPopupMenu component. }

{ EldoS Menu }

interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  Clipbrd,
  ActnList,
  ImgList,
  CommCtrl,
  ElTools,
  ElImgFrm,
  ElTmSchema,
  ElUxTheme,
  ElVCLUtils,
  ElXPThemedControl,
  ExtCtrls,
{$ifdef VCL_6_USED}
  Types,
{$endif}

  ElStrUtils,
  ElColor,
  ElHook
  {$ifdef ELPACK_UNICODE}
  , ElUnicodeStrings
  {$endif}
  {$ifdef HAS_HTML_RENDER}
  , HTMLRender
  {$endif}
  ,ElList;

{$warnings off}

{$ifndef VCL_4_USED}
{$define ELMENU_USE_IMAGES}
{$else}
{$undef ELMENU_USE_IMAGES}
{$endif}

type
  TElMenuItem = class;

  EMenuError = class(Exception);
  TMenuBreak = (mbNone, mbBreak, mbBarBreak);
  TMenuChangeEvent = procedure (Sender: TObject; Source: TElMenuItem; Rebuild: Boolean) of object;
  TMenuDrawItemEvent = procedure (Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; Selected: Boolean) of object;
  TMenuMeasureItemEvent = procedure (Sender: TObject; ACanvas: TCanvas;
    var Width, Height: Integer) of object;

  {$ifndef VCL_4_USED}
  TMenuItemAutoFlag = (maAutomatic, maManual, maParent);
  TMenuAutoFlag = maAutomatic..maManual;
  // Types for TElPopupMenu
  TTrackButton = (tbRightButton, tbLeftButton);
  {$endif}
  {$ifndef VCL_5_USED}
  TMenuAnimations = (maLeftToRight, maRightToLeft, maTopToBottom, maBottomToTop, maNone);
  TMenuAnimation = set of TMenuAnimations;
  {$endif}


  THackClass = class(TComponent)
  private
    {$ifdef VCL_4_USED}
    FBiDiMode: TBiDiMode;
    {$endif}
    FItems: TMenuItem;
  end;

  {$ifdef VCL_4_USED}

  { TMenuActionLink }

  TMenuActionLink = class(TActionLink)
  protected
    FClient: TElMenuItem;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpContextLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsShortCutLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: TElFString);
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHint(const Value: TElFString);
    procedure SetImageIndex(Value: Integer); override;
    procedure SetShortCut(Value: TShortCut); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

  TMenuActionLinkClass = class of TMenuActionLink;
  {$endif}

  { !!! This class only for internal use.  !!! }
  TElMenu = class(TMenu)
  end;

  TDrawStyle = (tdsNormal, tdsOfficeXP, tdsWindowsXP);

  { TElMenuItem }

  TElMenuItem = class(TMenuItem)
  private
    FImageChangeLink: TChangeLink;
    FSubMenuImages: TCustomImageList;
    FCaption: TElFString;
    FHandle: HMENU;
    FChecked: Boolean;
    FEnabled: Boolean;
    FDefault: Boolean;
    FRadioItem: Boolean;
    FVisible: Boolean;
    FGroupIndex: Byte;
    FImageIndex: Integer;
    {$ifdef VCL_4_USED}
    FActionLink: TMenuActionLink;
    {$endif}
    FBreak: TMenuBreak;
    FBitmap: TBitmap;
    FDisBitmap: TBitmap;
    FCommand: Word;
    FHelpContext: THelpContext;
    FHint: TElFString;
    FItems: TElList;
    FShortCut: TShortCut;
    FParent: TElMenuItem;
    FMerged: TElMenuItem;
    FMergedWith: TElMenuItem;
    FMenu: TElMenu;
    FStreamedRebuild: Boolean;
    FOnChange: TMenuChangeEvent;
    FOnClick: TNotifyEvent;
    FOnDrawItem: TMenuDrawItemEvent;
    FOnMeasureItem: TMenuMeasureItemEvent;
    procedure AppendTo(Menu: HMENU; ARightToLeft: Boolean);
    {$ifdef VCL_4_USED}
    procedure DoActionChange(Sender: TObject);
    {$endif}
    procedure ReadShortCutText(Reader: TReader);
    procedure MergeWith(Menu: TElMenuItem);
    procedure RebuildHandle;
    procedure PopulateMenu;
    procedure SubItemChanged(Sender: TObject; Source: TElMenuItem; Rebuild: Boolean);
    procedure TurnSiblingsOff;
    procedure WriteShortCutText(Writer: TWriter);
    procedure VerifyGroupIndex(Position: Integer; Value: Byte);
    {$ifdef VCL_4_USED}
    function GetAction: TBasicAction;
    procedure SetAction(Value: TBasicAction);
    {$endif}
    procedure SetSubMenuImages(Value: TCustomImageList);

    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);

    {$ifdef VCL_4_USED}
    procedure InitiateActions;
    function IsCaptionStored: Boolean;
    function IsCheckedStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsOnClickStored: Boolean;
    function IsShortCutStored: Boolean;
    function IsVisibleStored: Boolean;
    {$endif}
  protected
    procedure DefineProperties(Filer: TFiler); override;

    {$ifdef VCL_4_USED}
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    {$endif}
    procedure ImageListChange(Sender: TObject);

    procedure AssignTo(Dest: TPersistent); override;
    procedure DoDrawText(ACanvas: TCanvas; const ACaption: TElFString;
      var Rect: TRect; Selected: Boolean; Flags: Longint);
    procedure DrawItem(ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    {$ifdef VCL_4_USED}
    override;
    function GetActionLinkClass: TMenuActionLinkClass;
    {$endif}
    function GetHandle: HMENU;
    function GetCount: Integer;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetItem(Index: Integer): TElMenuItem;
    function GetMenuIndex: Integer;
    procedure MeasureItem(ACanvas: TCanvas; var Width, Height: Integer);
    {$ifdef D_6_UP}
    override;
    {$endif}
    procedure MenuChanged(Rebuild: Boolean); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetBreak(Value: TMenuBreak);
    procedure SetCaption(const Value: TElFString);
    procedure SetChecked(Value: Boolean);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetDefault(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetGroupIndex(Value: Byte);
    procedure SetImageIndex(Value: Integer);
    procedure SetMenuIndex(Value: Integer);
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetRadioItem(Value: Boolean);
    procedure SetShortCut(Value: TShortCut);
    procedure SetVisible(Value: Boolean);
    {$ifdef VCL_4_USED}
    property ActionLink: TMenuActionLink read FActionLink write FActionLink;
    {$endif}
    procedure UpdateItems;
    function GetImageWidth: Integer;
    procedure SetHint(Value: TElFString);
    procedure UpdateCommand;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef VCL_4_USED}
    procedure InitiateAction; override;
    {$endif}
    procedure DesignRebuild;
    procedure Insert(Index: Integer; Item: TElMenuItem);
    procedure Delete(Index: Integer);
    procedure Click; override;
    function Find(ACaption: TElFString): TElMenuItem;
    function IndexOf(Item: TElMenuItem): Integer;
    function GetParentComponent: TComponent; override;
    function GetParentMenu: TElMenu;
    function HasParent: Boolean; override;
    procedure Add(Item: TElMenuItem);
    procedure Remove(Item: TElMenuItem);
    procedure Clear;
    //property Command: Word read FCommand;

    property Handle: HMENU read GetHandle;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TElMenuItem read GetItem; default;
    property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
    property Parent: TElMenuItem read FParent;
  published
    {$ifdef VCL_4_USED}
    property Action: TBasicAction read GetAction write SetAction;
    {$endif}
    property SubMenuImages: TCustomImageList read FSubMenuImages write SetSubMenuImages;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Break: TMenuBreak read FBreak write SetBreak default mbNone;
    property Caption: TElFString read FCaption write SetCaption {$ifdef VCL_4_USED}stored IsCaptionStored{$endif};
    property Checked: Boolean read FChecked write SetChecked {$ifdef VCL_4_USED}stored IsCheckedStored{$endif} default False;
    property Default: Boolean read FDefault write SetDefault default False;
    property Enabled: Boolean read FEnabled write SetEnabled {$ifdef VCL_4_USED}stored IsEnabledStored{$endif} default True;
    property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
    property HelpContext: THelpContext read FHelpContext write FHelpContext {$ifdef VCL_4_USED}stored IsHelpContextStored{$endif} default 0;
    property Hint: TElFString read FHint write SetHint {$ifdef VCL_4_USED}stored IsHintStored{$endif};
    property ImageIndex: Integer read FImageIndex write SetImageIndex {$ifdef VCL_4_USED}stored IsImageIndexStored{$endif} default -1;
    property RadioItem: Boolean read FRadioItem write SetRadioItem default False;
    property ShortCut: TShortCut read FShortCut write SetShortCut {$ifdef VCL_4_USED}stored IsShortCutStored{$endif} default 0;
    property Visible: Boolean read FVisible write SetVisible {$ifdef VCL_4_USED}stored IsVisibleStored{$endif} default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick {$ifdef VCL_4_USED}stored IsOnClickStored{$endif};
    property OnDrawItem: TMenuDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMenuMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property OnChange: TMenuChangeEvent read FOnChange write FOnChange;
  end;

  TElMainMenu = class(TMainMenu)
  private
    FOle2Menu: HMENU;
    FMenuImage: TElFString;
    FUnicodeItems: TElMenuItem;
    FHook: TElHook;
    FDrawStyle: TDrawStyle;
    FFont: TFont;
    FForm: TForm;
    FImageChangeLink: TChangeLink;
    FImages: TImageList;
    FOwnerDraw: boolean;
    FRightToLeft : boolean; //GTP

    {$ifdef HAS_HTML_RENDER}
    FIsHTML  : Boolean;
    FOnImageNeeded: TElHTMLImageNeededEvent;
    FRender  : TElHTMLRender;
    procedure SetIsHTML(Value: Boolean);
    {$endif}

    procedure SetOwnerDraw(Value: Boolean);
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(Value: TImageList);

    procedure ItemChanged;
    procedure OnBeforeHook(Sender : TObject; var Message : TMessage; var Handled :
        boolean);
    procedure SetDrawStyle(Value: TDrawStyle);
    procedure SetFont(const Value: TFont);
    function UpdateImage: Boolean;
  protected
    FSystemFont: Boolean;
    function IsOwnerDraw: Boolean;
    procedure ProcessMenuChar(var Message: TWMMenuChar);
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
    {$ifdef ELPACK_UNICODE}
    function DoGetMenuString(Menu: HMENU; ItemID: UINT; Str: PWideChar;
                             MaxCount: Integer; Flag: UINT): Integer;
    {$else}
    function DoGetMenuString(Menu: HMENU; ItemID: UINT; Str: PChar;
                             MaxCount: Integer; Flag: UINT): Integer;
    {$endif}
    procedure MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
    {$ifdef VCL_4_USED}
    override;
    {$endif}
    function GetHandle: HMENU; override;
    function DispatchCommand(ACommand: Word): Boolean;
    procedure Loaded; override;
    procedure SetRightToLeft(Value: Boolean); //GTP
    procedure SetSystemFont(Value: Boolean);
    procedure GetFont;
    procedure FontChange(Sender : TObject);

    {$ifdef HAS_HTML_RENDER}
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString;
      var Image: TBitmap);
    {$endif}
    procedure UpdateCommands;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindItem(Value: Integer; Kind: TFindItemKind): TElMenuItem;
    function IsShortCut(var Message: TWMKey): Boolean;
    {$ifdef VCL_4_USED}
    override;
    {$endif}
    procedure UpdateItems;
    function DispatchPopup(AHandle: HMENU): Boolean;
  published
    property Items: TElMenuItem read FUniCodeItems;
    property Font: TFont read FFont write SetFont;
    property DrawStyle: TDrawStyle read FDrawStyle write SetDrawStyle default tdsNormal;
    property Images: TImageList read FImages write SetImages;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property RightToLeft: Boolean read FRightToLeft write SetRightToLeft default false; //GTP
    property SystemFont: Boolean read FSystemFont write SetSystemFont default true;
    {$ifdef HAS_HTML_RENDER}
    property IsHTML : Boolean read FIsHTML write SetIsHTML default false;
    property OnImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write
        FOnImageNeeded;
    {$endif}
  end;

  TElPopupMenu = class(TPopupMenu)
  private
    {$ifdef HAS_HTML_RENDER}
    FIsHTML  : Boolean;
    FRender  : TElHTMLRender;
    FOnImageNeeded: TElHTMLImageNeededEvent;
    {$endif}
    FHook: TElHook;
    FDrawStyle: TDrawStyle;
    FUnicodeItems: TElMenuItem;
    FPopupPoint: TPoint;
    FForm: TForm;
    FFont: TFont;
    {$ifndef VCL_5_USED}
    FMenuAnimation: TMenuAnimation;
    {$endif}
    {$ifndef VCL_4_USED}
    FTrackButton: TTrackButton;
    FImageChangeLink: TChangeLink;
    FImages: TImageList;
    FOwnerDraw: boolean;

    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(Value: TImageList);
    procedure SetOwnerDraw(Value: Boolean);
    {$endif}
    procedure SetDrawStyle(Value: TDrawStyle);
    procedure OnBeforeHook(Sender : TObject; var Message : TMessage; var Handled : boolean);
    procedure SetFont(const Value: TFont);
    {$ifdef HAS_HTML_RENDER}
    procedure SetIsHTML(Value: Boolean);
    {$endif}
  protected
    FSystemFont: Boolean;
    procedure DoPopup(Sender: TObject);
    {$ifdef VCL_5_USED}
    override;
    {$endif}
    function IsOwnerDraw: Boolean;
    function DispatchCommand(ACommand: Word): Boolean;
    function GetHandle: HMENU; virtual;
    procedure GetFont;
    procedure SetSystemFont(Value: Boolean);
    procedure FontChange(Sender : TObject);
    procedure Loaded; override;
    {$ifdef HAS_HTML_RENDER}
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString;
      var Image: TBitmap);
    {$endif}
    procedure UpdateCommands;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup(X, Y: Integer); override;
    function FindItem(Value: Integer; Kind: TFindItemKind): TElMenuItem;
    function IsShortCut(var Message: TWMKey): Boolean;
    {$ifdef VCL_4_USED}
    override;
    {$endif}
    procedure ProcessMenuChar(var Message: TWMMenuChar);
    procedure UpdateItems;
    function DispatchPopup(AHandle: HMENU): Boolean;
    property PopupPoint: TPoint read FPopupPoint;
    property Handle read GetHandle;
  published
    property Items: TElMenuItem read FUniCodeItems;
    property Font: TFont read FFont write SetFont;
    property DrawStyle: TDrawStyle read FDrawStyle write SetDrawStyle default tdsNormal;
    {$ifndef VCL_5_USED}
    property MenuAnimation: TMenuAnimation read FMenuAnimation write FMenuAnimation default [];
    {$endif}
    {$ifndef VCL_4_USED}
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property Images: TImageList read FImages write SetImages;
    property TrackButton: TTrackButton read FTrackButton write FTrackButton default tbRightButton;
    {$endif}
    property SystemFont: Boolean read FSystemFont write SetSystemFont default true;
    {$ifdef HAS_HTML_RENDER}
    property IsHTML : Boolean read FIsHTML write SetIsHTML default false;
    property OnImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write
        FOnImageNeeded;
    {$endif}
  end;
{$warnings on}


procedure InsertItems(var AMenu: TMenu; MainItem: TElMenuItem; Index: Integer; Items: array of TElMenuItem);
procedure InsertMenuItems(var AMenu: TMenu; Index: Integer; Items: array of TElMenuItem);

function ElNewMenu(Owner: TComponent; const AName: TElFString;
                 Items: array of TElMenuItem): TElMainMenu;

function ElNewSubMenu(const ACaption: TElFString; hCtx: Word;
                    const AName: TElFString; Items: array of TElMenuItem;
                    AEnabled: Boolean): TElMenuItem;

function ElNewItem(const ACaption: TElFString; AShortCut: TShortCut;
                 AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent;
                 hCtx: Word; const AName: TElFString): TElMenuItem;

function ElNewLine: TElMenuItem;

function ElStripHotKey(const Text: TElFString): TElFString;
function ElGetHotkey(const Text: TElFString): TElFString;
procedure ElInitMenuItems(AMenu: TMenu; Items: array of TElMenuItem);

procedure CopyMenuItems(Dest : TElMenuItem; Source : TElMenuItem);

function GetMenuFont: HFont;

var
  FTheme: HTheme;

implementation

{$ifdef ELPACK_UNICODE}
type
  GetMenuStringWProc = function(hMenu: HMENU; uIDItem: UINT; lpString: PWideChar;
                                nMaxCount: Integer; uFlag: UINT): Integer; stdcall;
var
  GetMenuStringW : GetMenuStringWProc;
{$endif}

const
  {$ifndef VCL_5_USED}
  cLineCaption = '-';
  cHotkeyPrefix = '&';
  ODS_HOTLIGHT = $40;
  {$endif}
  RightToLeftMenuFlag = MFT_RIGHTORDER or MFT_RIGHTJUSTIFY;
  {$ifdef VCL_5_USED}
  cMenuAutoFlagToItem: array [TMenuAutoFlag] of TMenuItemAutoFlag = (maAutomatic, maManual);
  cItemAutoFlagToMenu: array [TMenuItemAutoFlag] of TMenuAutoFlag = (maAutomatic, maManual, maAutomatic);
  cBooleanToItemAutoFlag: array [Boolean] of TMenuItemAutoFlag = (maManual, maAutomatic);
  cItemAutoFlagToBoolean: array [TMenuItemAutoFlag] of Boolean = (True, False, True);
  {$endif}

//var
//  CommandPool: TBits;

function PatternBitmap(FgColor, BkColor: TColor): TBitmap;
var
  X, Y: Integer;
begin
  Result := TBitmap.Create;
  try
    with Result do
    begin
      Width := 8;
      Height := 8;
      with Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := BkColor;
        FillRect(Rect(0, 0, Width, Height));
        for Y := 0 to 8 do
          for X := 0 to 8 do
            if (Y mod 2) = (X mod 2) then
              Pixels[X, Y] := FgColor;
      end;
    end;
  except
    Result.Free;
  end;
end;

procedure DrawGrayBitmap(Canvas : TCanvas; X, Y : integer; ABitmap : TBitmap);
var
  R: TRect;
  DestDC, SrcDC: HDC;
const
  ROP_DSPDxax = $00E20746;

begin
  R := Rect(X, Y, X + ABitmap.Width, Y + ABitmap.Height);
  SrcDC := ABitmap.Canvas.Handle;
  { Convert Black to clBtnHighlight }
  Canvas.Brush.Color := clBtnHighlight;
  DestDC := Canvas.Handle;
  Windows.SetTextColor(DestDC, clWhite);
  Windows.SetBkColor(DestDC, clBlack);
  BitBlt(DestDC, X+1, Y+1, ABitmap.Width, ABitmap.Height, SrcDC, 0, 0, ROP_DSPDxax);
  { Convert Black to clBtnShadow }
  Canvas.Brush.Color := clBtnShadow;
  DestDC := Canvas.Handle;
  Windows.SetTextColor(DestDC, clWhite);
  Windows.SetBkColor(DestDC, clBlack);
  BitBlt(DestDC, X, Y, ABitmap.Width, ABitmap.Height, SrcDC, 0, 0, ROP_DSPDxax);
end;

procedure GrayBitmap(ImageList : TCustomImageList; ImageIndex : integer; var 
    DisBitmap : TBitmap);
begin
  if DisBitmap = nil then
  begin
    DisBitmap := TBitmap.Create;
    with DisBitmap do
    begin
      Monochrome := True;
      Width := TImageList(ImageList).Width;
      Height := TImageList(ImageList).Height;
    end;
    { Store masked version of image temporarily in FBitmap }
    DisBitmap.Canvas.Brush.Color := clWhite;
    DisBitmap.Canvas.FillRect(Rect(0, 0, TImageList(ImageList).Width, TImageList(ImageList).Height));
    ImageList_DrawEx(ImageList.Handle, ImageIndex, DisBitmap.Canvas.Handle, 0,0,0,0,
      CLR_DEFAULT, 0, ILD_NORMAL);
  end;
  (*
  lc := 0;
  nc := 0;
  for i := 0 to ABitmap.Height do
    for j := 0 to ABitmap.Width do
    begin
      if nc <> ABitmap.TransparentColor then
      begin
        if nc <> ABitmap.Canvas.Pixels[j, i] then
        begin
          lc := ColorToGray(ABitmap.Canvas.Pixels[j, i]);
          ABitmap.Canvas.Pixels[j, i] := lc;
        end
        else
          ABitmap.Canvas.Pixels[j, i] := lc;
      end
    end;
  *)
end;

procedure ShadowBitmap(ABitmap: TBitmap);
var
  i, j: integer;
begin
  for i := 0 to ABitmap.Height - 1 do
    for j := 0 to ABitmap.Width - 1 do
    begin
      if ABitmap.Canvas.Pixels[j, i] <> Integer(RGB(255, 255, 255)) then
        ABitmap.Canvas.Pixels[j, i] := clBtnShadow;
    end;
end;

(*
function UniqueCommand: Word;
begin
  Result := CommandPool.OpenBit;
  CommandPool[Result] := True;
end;
*)

function GetMenuFont: HFont;
var
  NCM: TNonClientMetrics;
begin
  NCM.cbSize := SizeOf(TNonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCM, 0) then
    Result := CreateFontIndirect(NCM.lfMenuFont)
  else
    Result := GetStockObject(SYSTEM_FONT);
end;

{ Used to populate or merge menus }

type
    PointerMenuItemIterateFunction = function(MenuItem: TMenuItem): Boolean;

procedure IterateMenus(Func: Pointer; Menu1, Menu2: TElMenuItem);
var
  I, J: Integer;
  IIndex, JIndex: Byte;
  Menu1Size, Menu2Size: Integer;
  Done: Boolean;

  function Iterate(var I: Integer; MenuItem: TElMenuItem; AFunc: PointerMenuItemIterateFunction): Boolean;
  var
    Item: TMenuItem;
  begin
    if MenuItem = nil then Exit;
    Result := False;
    while not Result and (I < MenuItem.Count) do
    begin
      Item := MenuItem[I];
      if Item.GroupIndex > IIndex then Break;
       begin
         Result := AFunc(Item);
       end;
      Inc(I);
    end;
  end;

begin
  I := 0;
  J := 0;
  Menu1Size := 0;
  Menu2Size := 0;
  if Menu1 <> nil then 
    Menu1Size := Menu1.Count;
  if Menu2 <> nil then 
    Menu2Size := Menu2.Count;
  Done := False;
  while not Done and ((I < Menu1Size) or (J < Menu2Size)) do
  begin
    IIndex := High(Byte);
    JIndex := High(Byte);
    if (I < Menu1Size) then 
      IIndex := Menu1[I].GroupIndex;
    if (J < Menu2Size) then 
      JIndex := Menu2[J].GroupIndex;
    if IIndex <= JIndex then 
      Done := Iterate(I, Menu1, Func)
    else
    begin
      IIndex := JIndex;
      Done := Iterate(J, Menu2, Func);
    end;
    while (I < Menu1Size) and (Menu1[I].GroupIndex <= IIndex) do 
      Inc(I);
    while (J < Menu2Size) and (Menu2[J].GroupIndex <= IIndex) do 
      Inc(J);
  end;
end;

procedure CopyMenuItems(Dest : TElMenuItem; Source : TElMenuItem);
var i : integer;
    SourceItem,
    DestItem : TElMenuItem;

begin
  for i := 0 to Source.Count - 1 do
  begin
    SourceItem := TElMenuItem(Source.Items[i]);
    DestItem := ElNewItem(SourceItem.Caption, SourceItem.ShortCut, SourceItem.Checked,
                          SourceItem.Enabled, SourceItem.OnClick,
                          SourceItem.HelpContext, SourceItem.Name);
    Dest.Add(DestItem);

    DestItem.Break := SourceItem.Break;
    DestItem.Default := SourceItem.Default;
    DestItem.Enabled := SourceItem.Enabled;
    DestItem.GroupIndex := SourceItem.GroupIndex;
    DestItem.Hint := SourceItem.Hint;
    DestItem.RadioItem := SourceItem.RadioItem;
    DestItem.SubMenuImages := SourceItem.SubMenuImages;
    DestItem.Visible := SourceItem.Visible;
    
    {$ifdef VCL_5_USED}
    DestItem.AutoHotKeys := SourceItem.AutoHotkeys;
    DestItem.AutoLineReduction := SourceItem.AutoLineReduction;
    DestItem.OnAdvancedDrawItem := SourceItem.OnAdvancedDrawItem;
        {$endif}
        {$ifdef VCL_4_USED}
    DestItem.Bitmap := SourceItem.Bitmap;
    DestItem.ImageIndex := SourceItem.ImageIndex;
    DestItem.Action := SourceItem.Action;
    DestItem.OnDrawItem := SourceItem.OnDrawItem;
    DestItem.OnMeasureItem := SourceItem.OnMeasureItem;
    {$endif}
    if SourceItem.Count <> 0 then
      CopyMenuItems(DestItem, SourceItem);
  end;
end;

{$ifdef VCL_4_USED}
procedure TMenuActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TElMenuItem;
end;

function TMenuActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TMenuActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Checked = (Action as TCustomAction).Checked);
end;

function TMenuActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TMenuActionLink.IsHelpContextLinked: Boolean;
begin
  Result := inherited IsHelpContextLinked and
    (FClient.HelpContext = (Action as TCustomAction).HelpContext);
end;

function TMenuActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TMenuActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TMenuActionLink.IsShortCutLinked: Boolean;
begin
  Result := inherited IsShortCutLinked and
    (FClient.ShortCut = (Action as TCustomAction).ShortCut);
end;

function TMenuActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

function TMenuActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    (@FClient.OnClick = @Action.OnExecute);
end;

procedure TMenuActionLink.SetCaption(const Value: TElFString);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TMenuActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then FClient.Checked := Value;
end;

procedure TMenuActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TMenuActionLink.SetHelpContext(Value: THelpContext);
begin
  if IsHelpContextLinked then FClient.HelpContext := Value;
end;

procedure TMenuActionLink.SetHint(const Value: TElFString);
begin
  if IsHintLinked then FClient.Hint := Value;
end;

procedure TMenuActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then FClient.ImageIndex := Value;
end;

procedure TMenuActionLink.SetShortCut(Value: TShortCut);
begin
  if IsShortCutLinked then FClient.ShortCut := Value;
end;

procedure TMenuActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

procedure TMenuActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then FClient.OnClick := Value;
end;
{$endif}

{ TElMenuItem }

constructor TElMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVisible := True;
  FEnabled := True;
  // FCommand := UniqueCommand;
  FCommand := inherited Command;
  FImageIndex := -1;
  {$ifndef VCL_4_USED}
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  {$endif}
end;

destructor TElMenuItem.Destroy;
begin
  if FParent <> nil then
  begin
    FParent.Remove(Self);
    FParent := nil;
  end;
  while Count > 0 do Items[0].Free;
  if FHandle <> 0 then
  begin
    MergeWith(nil);
    DestroyMenu(FHandle);
  end;
  FItems.Free;
  {$ifdef VCL_4_USED}
  FActionLink.Free;
  FActionLink := nil;
  {$else}
  FImageChangeLink.Free;
  FImageChangeLink := nil;
  {$endif}
  //if FCommand <> 0 then CommandPool[FCommand] := False;
  if Assigned(FBitmap) then FBitmap.Free;
  if Assigned(FDisBitMap) then FDisBitMap.Free; 
  inherited Destroy;
end;

procedure TElMenuItem.SetSubMenuImages(Value: TCustomImageList);
begin
  if FSubMenuImages <> nil then
    FSubMenuImages.UnRegisterChanges(FImageChangeLink);
  FSubMenuImages := Value;
  if FSubMenuImages <> nil then
  begin
    FSubMenuImages.RegisterChanges(FImageChangeLink);
    FSubMenuImages.FreeNotification(Self);
  end;
  UpdateItems;
end;

procedure TElMenuItem.ImageListChange(Sender: TObject);
begin
  if Sender = SubMenuImages then
    UpdateItems;
end;

procedure TElMenuItem.UpdateItems;

  function UpdateItem(MenuItem: TElMenuItem): Boolean;
  begin
    Result := False;
    IterateMenus(@UpdateItem, MenuItem.FMerged, MenuItem);
    MenuItem.SubItemChanged(MenuItem, MenuItem, True);
  end;

begin
  IterateMenus(@UpdateItem, FMerged, Self);
end;

const
  Checks: array[Boolean] of DWORD = (MF_UNCHECKED, MF_CHECKED);
  Enables: array[Boolean] of DWORD = (MF_DISABLED or MF_GRAYED, MF_ENABLED);
  Breaks: array[TMenuBreak] of DWORD = (0, MF_MENUBREAK, MF_MENUBARBREAK);
  Separators: array[Boolean] of DWORD = (MF_STRING, MF_SEPARATOR);

procedure TElMenuItem.AppendTo(Menu: HMENU; ARightToLeft: Boolean);
const
  IBreaks: array[TMenuBreak] of DWORD = (MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK);
  IChecks: array[Boolean] of DWORD = (MFS_UNCHECKED, MFS_CHECKED);
  IDefaults: array[Boolean] of DWORD = (0, MFS_DEFAULT);
  IEnables: array[Boolean] of DWORD = (MFS_DISABLED or MFS_GRAYED, MFS_ENABLED);
  IRadios: array[Boolean] of DWORD = (MFT_STRING, MFT_RADIOCHECK);
  ISeparators: array[Boolean] of DWORD = (MFT_STRING, MFT_SEPARATOR);
  IRTL: array[Boolean] of DWORD = (0, RightToLeftMenuFlag);
  IOwnerDraw: array[Boolean] of DWORD = (MFT_STRING, MFT_OWNERDRAW);

var
//  {$ifdef ELPACK_UNICODE}
//  MenuItemInfo : TMenuItemInfoW;
//  {$else}
  MenuItemInfo : TMenuItemInfoA;
//  {$endif}
  Caption: TElFString;
  //NewFlags: Integer;
  IsOwnerDraw: Boolean;
  ParentMenu: TMenu;
begin
  if FVisible then
  begin
    Caption := FCaption;
    if GetCount > 0 then
      MenuItemInfo.hSubMenu := GetHandle
    else
    if (FShortCut <> scNone) and ((Parent = nil) or
      (Parent.Parent <> nil) or not (Parent.Owner is TElMainMenu)) then
      Caption := Caption + #9 + ShortCutToText(FShortCut);
    //if Lo(GetVersion) >= 4 then
    begin
      with MenuItemInfo do
      begin
        cbSize := 44; // Required for Windows 95
        fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or
          MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE;
        ParentMenu := GetParentMenu;
        if ParentMenu <> nil then
        begin
          if ParentMenu is TElPopupMenu then
            IsOwnerDraw := TElPopupMenu(ParentMenu).IsOwnerDraw
          else
            IsOwnerDraw := TElMainMenu(ParentMenu).IsOwnerDraw;
        end
        else
          exit;

        // IsOwnerDraw := Assigned(ParentMenu) and IsOwnerDraw or
        //  Assigned(FBitmap) and not FBitmap.Empty;
        fType := IRadios[FRadioItem] or IBreaks[FBreak] or
          ISeparators[Caption = cLineCaption] or IRTL[ARightToLeft] or
          IOwnerDraw[IsOwnerDraw];
        fState := IChecks[FChecked] or IEnables[FEnabled]
          or IDefaults[FDefault];
        wID := Command;
        hSubMenu := 0;
        hbmpChecked := 0;
        hbmpUnchecked := 0;
//      {$ifdef ELPACK_UNICODE}
//      dwTypeData := PWideChar(Caption);
//      {$else}
        dwTypeData := PAnsiChar(String(Caption));
//      {$endif}
        if GetCount > 0 then
          hSubMenu := GetHandle;
        //{$ifdef ELPACK_UNICODE}
        //InsertMenuItemW(Menu, DWORD(-1), True, MenuItemInfo);
        //{$else}
        //InsertMenuItemA(Menu, DWORD(-1), True, MenuItemInfo);
        //{$endif}
        InsertMenuItemA(Menu, DWORD(-1), True, MenuItemInfo);
      end;
    (*end
    else
    begin
      NewFlags := Breaks[FBreak] or Checks[FChecked] or Enables[FEnabled] or
        Separators[FCaption = '-'] or MF_BYPOSITION;
      if GetCount > 0 then
        {$ifdef ELPACK_UNICODE}
        InsertMenuW(Menu, DWORD(-1), MF_POPUP or NewFlags, GetHandle, PWideChar(FCaption))
        {$else}
        InsertMenuA(Menu, DWORD(-1), MF_POPUP or NewFlags, GetHandle, PChar(FCaption))
        {$endif}
      else
        {$ifdef ELPACK_UNICODE}
        InsertMenuW(Menu, DWORD(-1), NewFlags, Command, PWideChar(Caption));
        {$else}
        InsertMenuA(Menu, DWORD(-1), NewFlags, Command, PChar(Caption));
        {$endif}
    *)
    end;
  end;
end;

procedure TElMenuItem.PopulateMenu;
var
  MenuRightToLeft: Boolean;

  function AddIn(MenuItem: TElMenuItem): Boolean;
  begin
    MenuItem.AppendTo(FHandle, MenuRightToLeft);
    Result := False;
  end;

begin    // all menu items use BiDiMode of their root menu
  {$ifdef VCL_4_USED}
  MenuRightToLeft := (FMenu <> nil) and FMenu.IsRightToLeft;
  {$else}
  MenuRightToLeft := false;
  {$endif}
  IterateMenus(@AddIn, FMerged, Self);
end;

procedure TElMenuItem.ReadShortCutText(Reader: TReader);
begin
  ShortCut := TextToShortCut(Reader.ReadString);
end;

procedure TElMenuItem.MergeWith(Menu: TElMenuItem);
begin
  if FMerged <> Menu then
  begin
    if FMerged <> nil then FMerged.FMergedWith := nil;
    FMerged := Menu;
    if FMerged <> nil then FMerged.FMergedWith := Self;
    RebuildHandle;
  end;
end;

procedure TElMenuItem.Loaded;
begin
  inherited Loaded;
  {$ifdef VCL_4_USED}
  if Action <> nil then ActionChange(Action, True);
  {$endif}
  if FStreamedRebuild then RebuildHandle;
end;

procedure TElMenuItem.RebuildHandle;
begin
  if csDestroying in ComponentState then Exit;
  if csReading in ComponentState then
    FStreamedRebuild := True
  else
  begin
    if FMergedWith <> nil then
      FMergedWith.RebuildHandle
    else
    begin
      while GetMenuItemCount(Handle) > 0 do RemoveMenu(Handle, 0, MF_BYPOSITION);
      PopulateMenu;
      MenuChanged(False);
    end;
  end;
end;

procedure TElMenuItem.VerifyGroupIndex(Position: Integer; Value: Byte);
var
  I: Integer;
begin
  for I := 0 to GetCount - 1 do
    if I < Position then
    begin
      if Items[I].GroupIndex > Value then 
        EMenuError.CreateFmt('Group Index Too Low', [Name]);
    end
    else
      { Ripple change to menu items at Position and after }
      if Items[I].GroupIndex < Value then 
        Items[I].FGroupIndex := Value;
end;

procedure TElMenuItem.WriteShortCutText(Writer: TWriter);
begin
  Writer.WriteString(ShortCutToText(ShortCut));
end;

function TElMenuItem.GetHandle: HMENU;
begin
  if FHandle = 0 then
  begin
    if Owner is TPopupMenu then
      FHandle := CreatePopupMenu
    else
      FHandle := CreateMenu;
    if FHandle = 0 then
      raise EMenuError.CreateFmt('Out Of Resources', [Name]);
    PopulateMenu;
  end;
  Result := FHandle;
end;

procedure TElMenuItem.DoDrawText(ACanvas: TCanvas; const ACaption: TElFString;
  var Rect: TRect; Selected: Boolean; Flags: Longint);
var
  Text: TElFString;
  R : TRect;
  ParentMenu: TElMenu;
  DrawStyle: TDrawStyle;
  {$ifdef HAS_HTML_RENDER}
  IsHTML: boolean;
  Render: TElHTMLRender;
  APos: integer;
  {$endif}
begin
  ParentMenu := GetParentMenu;
  {$ifdef VCL_4_USED}
  if (ParentMenu <> nil) and (ParentMenu.IsRightToLeft) then
  begin
    if Flags and DT_LEFT = DT_LEFT then
      Flags := Flags and (not DT_LEFT) or DT_RIGHT
    else if Flags and DT_RIGHT = DT_RIGHT then
      Flags := Flags and (not DT_RIGHT) or DT_LEFT;
    Flags := Flags or DT_RTLREADING;
  end;
  {$endif}
  if TMenu(ParentMenu) is TElPopupMenu then
  begin
    DrawStyle := TElPopupMenu(ParentMenu).DrawStyle;
    {$ifdef HAS_HTML_RENDER}
    IsHTML := TElPopupMenu(ParentMenu).IsHTML;
    Render := TElPopupMenu(ParentMenu).FRender;
    {$endif}
  end
  else
  begin
    DrawStyle := TElMainMenu(ParentMenu).DrawStyle;
    {$ifdef HAS_HTML_RENDER}
    IsHTML := TElMainMenu(ParentMenu).IsHTML;
    Render := TElMainMenu(ParentMenu).FRender;
    {$endif}
  end;

  Text := ACaption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or
    (Text[1] = '&') and (Text[2] = #0)) then
      Text := Text + ' ';
  with ACanvas do
  begin
    if Text = cLineCaption then
    begin
      if Flags and DT_CALCRECT = 0 then
      begin
        R := Rect;
        if DrawStyle <> tdsNormal then
          Inc(R.Top, 2)
        else
          Inc(R.Top, 4);
        if DrawStyle = tdsOfficeXP then
        begin
          Inc(R.Left, 25);
          Inc(R.Right, 5);
        end;
        DrawEdge(Handle, R, EDGE_ETCHED, BF_TOP);
      end;
    end else
    begin
      Brush.Style := bsClear;
      if Default then
        Font.Style := Font.Style + [fsBold];

      {$ifdef HAS_HTML_RENDER}
      APos := pos('&',Text);
      if (IsHTML)and(APos<>0)and(APos<Length(Text)) then
      begin
        System.Delete(Text, APos, 1);
        System.Insert('</u>',Text,APos+1);
        System.Insert('<u>',Text,APos);
      end;
      {$endif}

      if not Enabled then
      begin
        if (not Selected) and (DrawStyle <> tdsOfficeXP) then
        begin
          OffsetRect(Rect, 1, 1);
          Font.Color := clBtnHighlight;
          {$ifdef HAS_HTML_RENDER}
          if IsHTML then
          begin
            Render.Data.DefaultStyle := Font.Style;
            Render.Data.DefaultHeight := Font.Height;
            Render.Data.DefaultFont := Font.Name;
            Render.Data.Charset := Font.Charset;

	    Render.Data.DefaultColor := Font.Color;
	    Render.PrepareText(Text, 0, false);
            R := Rect;
            if (TMenu(ParentMenu)is TElMainMenu)and(Parent.Parent=nil) then
              inc(R.Left,(R.Right-R.Left-Render.Data.TextSize.cx) div 2);
            inc(R.Top,(R.Bottom-R.Top-Render.Data.TextSize.cy) div 2);
          if Flags and DT_RIGHT <> 0 then
            R.Left := R.Right - Render.Data.TextSize.cx
          else
            R.Right := R.Left + Render.Data.TextSize.cx;
          R.Bottom := R.Top + Render.Data.TextSize.cy;
          if (Flags and DT_CALCRECT = 0) then
            Render.DrawText(ACanvas, Point(0, 0), R, clNone)
          else
            Rect := R;
        end
        else
          {$endif}
            {$ifdef ELPACK_UNICODE}
            ElVCLUtils.DrawTextW(Handle, PWideChar(Text), Length(Text), Rect, Flags);
            {$else}
            Windows.DrawText(Handle, PChar(Text), Length(Text), Rect, Flags);
            {$endif}
          OffsetRect(Rect, -1, -1);
        end;
        if Selected and (ColorToRGB(clHighlight) = ColorToRGB(clBtnShadow)) then
          Font.Color := clBtnHighlight
        else
          Font.Color := clBtnShadow;
      end;
      {$ifdef HAS_HTML_RENDER}
      if IsHTML then
      begin
        Render.Data.DefaultColor := Font.Color;
        Render.Data.DefaultStyle := Font.Style;
        Render.Data.DefaultHeight := Font.Height;
        Render.Data.DefaultFont := Font.Name;
        Render.Data.Charset := Font.Charset;
        Render.PrepareText(Text, 0, false);
        R := Rect;
        if (TMenu(ParentMenu)is TElMainMenu)and(Parent.Parent=nil) then
          inc(R.Left,(R.Right-R.Left-Render.Data.TextSize.cx) div 2);
        inc(R.Top,(R.Bottom-R.Top-Render.Data.TextSize.cy) div 2);
        if Flags and DT_RIGHT <> 0 then
          R.Left := R.Right - Render.Data.TextSize.cx
        else
          R.Right := R.Left + Render.Data.TextSize.cx;
        R.Bottom := R.Top + Render.Data.TextSize.cy;
        if (Flags and DT_CALCRECT = 0) then
          Render.DrawText(ACanvas, Point(0, 0), R, clNone)
        else
          Rect := R;
      end
      else
      {$endif}
        {$ifdef ELPACK_UNICODE}
        ElVCLUtils.DrawTextW(Handle, PWideChar(Text), Length(Text), Rect, Flags);
        {$else}
        Windows.DrawText(Handle, PChar(Text), Length(Text), Rect, Flags);
        {$endif}
    end;
  end;
end;

procedure TElMenuItem.DrawItem(ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
const
  Alignments: array[TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  EdgeStyle: array[Boolean] of Longint = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
var
  TopLevel: Boolean;
  ImageList: {$ifdef VCL_4_USED}TCustomImageList{$else}TImageList{$endif};
  ParentMenu: TMenu;
  Alignment : TPopupAlignment;
  DrawImage : Boolean;
  GlyphRect, SaveRect: TRect;
  DrawStyle: Longint;
  DStyle: TDrawStyle;
  BarWidth: integer;
  BarRect: TRect;
  Glyph: TBitmap;
  OldBrushColor,
  OldPenColor,
  OldFontColor: TColor;
  //State : integer;
  D3FOwnerDraw : boolean;
  Bmp: TBitmap;
  {$ifdef HAS_HTML_RENDER}
  IsHTML: boolean;
  {$endif}
  S : string;
  Height, Width : integer;

  //GTP variables for Right To Left Operation
  MenuItemInfo : TMenuItemInfo;
  RightToLeftMenuItem : boolean;
  RightGlyphRect : TRect;
  RightHotKeyRect : TRect;
  w1, w2, w3 : integer;

  procedure GetMenuSize;
  var
    NonClientMetrics: TNonClientMetrics;
  begin
    NonClientMetrics.cbSize := sizeof(NonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    begin
      Width := NonClientMetrics.iMenuWidth;
      Height := NonClientMetrics.iMenuHeight;
    end;
  end;


begin
(*
  if (IsWinXPUp) and (FTheme <> 0) then
  begin
    if Caption <> '-' then
    begin
      if Selected then
        State := MS_SELECTED
      else
        State := MS_NORMAL;
      DrawThemeBackground(FTheme, ACanvas.Handle, MP_MENUITEM, State, ARect, nil);
      {$ifdef ELPACK_UNICODE}
      DrawThemeText(FTheme, ACanvas.Handle, MP_MENUITEM, State, PWideChar(Caption), Length(Caption), 0, 0, ARect);
      {$else}
      DrawThemeText(FTheme, ACanvas.Handle, MP_MENUITEM, State, PWideChar(WideString(Caption)), Length(Caption), 0, 0, ARect);
      {$endif}
    end
    else
      DrawThemeBackground(FTheme, ACanvas.Handle, MP_SEPARATOR, MS_NORMAL, ARect, nil);
  end
  else
*)
  begin
    ParentMenu := GetParentMenu;
//GTP
    FillChar(MenuItemInfo, sizeof(MenuItemInfo), 0); 
    MenuItemInfo.cbSize := SizeOf (MenuItemInfo);
    MenuItemInfo.fMask := MIIM_TYPE;
    GetMenuItemInfo(ParentMenu.Handle, 0, true, MenuItemInfo);
    RightToLeftMenuItem := (MenuItemInfo.fType and MFT_RIGHTORDER) <> 0;
//GTP

    RightHotKeyRect := ARect;

    with RightGlyphRect do
    begin
      Top := 0;
      Left := 0;
      Right := 0;
      Bottom := 0;
    end; //with

    if ParentMenu is TElPopupMenu then
    begin
      D3FOwnerDraw := TElPopupMenu(ParentMenu).IsOwnerDraw;
      {$ifdef HAS_HTML_RENDER}
      IsHTML := TElPopupMenu(ParentMenu).IsHTML;
//      Render := TElPopupMenu(ParentMenu).FRender;
      {$endif}
    end else
    begin
      D3FOwnerDraw := TElMainMenu(ParentMenu).IsOwnerDraw;
      {$ifdef HAS_HTML_RENDER}
      IsHTML := TElMainMenu(ParentMenu).IsHTML;
//      Render := TElMainMenu(ParentMenu).FRender;
      {$endif}
    end;

    if (ParentMenu <> nil) and D3FOwnerDraw and Assigned(FOnDrawItem) then
      FOnDrawItem(Self, ACanvas, ARect, Selected)
    else
    begin
      Bmp := TBitmap.Create;
      TopLevel := GetParentComponent is TElMainMenu;
      with ACanvas do
      begin
        if (ParentMenu is TElPopupMenu) then
          ImageList := TElPopupMenu(ParentMenu).Images
        else
          ImageList := TElMainMenu(ParentMenu).Images;

        if TMenu(ParentMenu) is TPopupMenu then
          DStyle := TElPopupMenu(ParentMenu).DrawStyle
        else
          DStyle := TElMainMenu(ParentMenu).DrawStyle;
{
        if IsHTML and (Caption<>'-') then
        begin
          Render.Data.DefaultColor := Font.Color;
          Render.Data.DefaultStyle := Font.Style;
          Render.Data.DefaultHeight := Font.Height;
          Render.Data.DefaultFont := Font.Name;
          Render.Data.Charset := Font.Charset;
          Render.PrepareText(Caption, 0, false);
          ARect.Right := ARect.Left + Render.Data.TextSize.cx;
          ARect.Bottom := ARect.Top + Render.Data.TextSize.cy;
        end;
}
        if not Selected then FillRect(ARect);
        if TMenu(ParentMenu) is TElMainMenu then
          Alignment := paLeft
        else
        if TMenu(ParentMenu) is TElPopupMenu then
          Alignment := TElPopupMenu(ParentMenu).Alignment
        else
        Alignment := paLeft;

        {if not TopLevel then
        begin
          HBmp := GetCurrentObject(ACanvas.Handle, OBJ_BITMAP);
          Bytes := GetObject(HBmp, SizeOf(DS), @DS);
        end;}

        BarWidth := GetImageWidth;
        if BarWidth = -1 then
        begin
          if Caption <> cLineCaption then
            BarWidth := ARect.Bottom - ARect.Top
          else
          begin
            SetRectEmpty(SaveRect);
            DrawStyle := DT_CALCRECT;
            DoDrawText(ACanvas, 'W', SaveRect, False, DrawStyle);
            GetMenuSize;
            inc(Height, 3 + 1);
            BarWidth := Max(Height, SaveRect.Bottom - SaveRect.Top);
          end;
        end;
        GlyphRect.Left := ARect.Left + 1;
        GlyphRect.Top := ARect.Top + 1;
        GlyphRect.Right := GlyphRect.Left;
        GlyphRect.Bottom := GlyphRect.Top;

        BarRect.Left := ARect.Left;
        BarRect.Top := ARect.Top;
        BarRect.Bottom := ARect.Bottom;
        BarRect.Right := BarRect.Left + BarWidth;

        if Caption = cLineCaption then
        begin
          FillRect(ARect);
          if Dstyle = tdsOfficeXP then
          begin
            OldBrushColor := Brush.Color;
            Brush.Color := clBtnFace;
            FillRect(BarRect);
            Brush.Color := OldBrushColor;
          end;
          GlyphRect.Left := 0;
          GlyphRect.Right := -2; //was -4; GTP
        end
        else
        begin
          DrawImage := ((ImageList <> nil) and ((ImageIndex > -1) and
            (ImageIndex < ImageList.Count)) or (Checked and ((FBitmap = nil) or
            FBitmap.Empty)));

          if Dstyle = tdsOfficeXP then
          begin
            if (TopLevel) and (Enabled) then
            begin
              // draw frame around selected item
              OldPenColor := Pen.Color;
              if Selected then
              Pen.Color := clBtnShadow;
              {$ifndef VCL_5_USED}
              Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
              {$else}
              Rectangle(ARect);
              {$endif}
              Pen.Color := OldPencolor;
            end
            else
            begin
              // Fill the left bar
              OldBrushColor := Brush.Color;
              Brush.Color := clBtnFace;
              FillRect(BarRect);
              Brush.Color := OldBrushColor;
            end;
          end;

          if ((Selected) and (not TopLevel) and (Enabled)) or ((Selected) and (not TopLevel) and (DStyle = tdsNormal)) then
          begin
            if (DStyle <> tdsNormal) or (((ImageIndex < 0) or (not assigned(ImageList))) {and (not Checked) }and not Assigned(FBitmap)) then
              // ARect.Left := 0
            else
              ARect.Left := BarRect.Right;

            FillRect(ARect);
            if DStyle = tdsOfficeXP then
              {$ifndef VCL_5_USED}
              Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom)
              {$else}
              Rectangle(ARect)
              {$endif}
          end;
          if (DStyle = tdsOfficeXP) and (not TopLevel) then
          begin
            GlyphRect.Right := GlyphRect.Left + ARect.Bottom - ARect.Top - 1;
            GlyphRect.Bottom := GlyphRect.Top + ARect.Bottom - ARect.Top - 1;
          end;

          if DrawImage or (Assigned(FBitmap) and not FBitmap.Empty) or Checked then
          begin
            if DrawImage then
            begin
              if ((DStyle <> tdsOfficeXP) or TopLevel) then
              begin
                if (ImageList <> nil) and ((ImageIndex > -1) and
                   (ImageIndex < ImageList.Count)) then
                begin
                  GlyphRect.Right := GlyphRect.Left + ImageList.Width;
                  GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
                end
                else
                begin
                  if ImageList <> nil then
                  begin
                    GlyphRect.Right := GlyphRect.Left + Max(ImageList.Width, GetSystemMetrics(SM_CXMENUCHECK));
                    GlyphRect.Bottom := GlyphRect.Top + Max(ImageList.Height, GetSystemMetrics(SM_CYMENUCHECK));
                  end
                  else
                  begin
                    GlyphRect.Right := GlyphRect.Left + GetsystemMetrics(SM_CXMENUCHECK);
                    GlyphRect.Bottom := GlyphRect.Top + GetsystemMetrics(SM_CYMENUCHECK);
                  end;
                end;
              end;
            end
            else
            begin
              GlyphRect.Right := GlyphRect.Left + FBitmap.Width;
              GlyphRect.Bottom := GlyphRect.Top + FBitmap.Height;
            end;

            OffsetRect(GlyphRect, 1, -GlyphRect.Top);
            OffsetRect(GlyphRect, 0, ARect.Top + (ARect.Bottom - ARect.Top - GlyphRect.Bottom) div 2);

            if Checked then
            begin
              //Inc(GlyphRect.Right);
              //Inc(GlyphRect.Bottom);
              OldBrushColor := Brush.Color;
              if DStyle <> tdsOfficeXP then
              begin
                if not Selected then
                begin
                  OldBrushColor := Brush.Color;
                  if DStyle = tdsWindowsXP then
                    Brush.Color := clWindow
                  else
                    Brush.Color := clMenu;
                  FillRect(GlyphRect);
                end
                else
                if not Checked then
                begin
                  Brush.Color := clHighlight;
                  FillRect(GlyphRect);
                end;
              end;
              Brush.Color := OldBrushColor;
              Inc(GlyphRect.Left);
              Inc(GlyphRect.Top);

              (*
              if DStyle = tdsOfficeXP then
              begin
                OldBrushColor := Brush.Color;
                if Selected then
                  Brush.Color := BrightColor(clHighLight, 55)
                else
                  Brush.Color := BrightColor(clHighLight, 85);

                OldPenColor := Pen.Color;
                Pen.Color := clHighlight;
                OffsetRect(GlyphRect, -1, -1);
                InflateRect(GlyphRect, 1, 1);
                Inc(GlyphRect.Bottom, 3);
                Inc(GlyphRect.Right);
                FillRect(GlyphRect);
                Rectangle(GlyphRect.Left, GlyphRect.Top, GlyphRect.Right, GlyphRect.Bottom);
                InflateRect(GlyphRect, -1, -1);
                OffsetRect(GlyphRect, 1, 1);
                Brush.Color := OldBrushColor;
                Pen.Color := OldPenColor;
              end;
              *)
            end;

            if DrawImage or (not Assigned(FBitmap)) then
            begin
              if (ImageIndex > -1) and (ImageIndex < ImageList.Count) then
              begin
                if (DStyle = tdsOfficeXP) and (Selected) and (Enabled) and (not Checked) then
                begin
                  FillRect(GlyphRect);
                end;
                if (DStyle = tdsNormal) and (Selected) then
                  DrawEdge(ACanvas.Handle, BarRect, BDR_RAISEDINNER, BF_RECT);
                if (not Enabled) then
                begin
                  if (FDisBitmap = nil)  then
                    GrayBitmap(ImageList, ImageIndex, FDisBitmap);

                  DrawGRayBitmap(ACanvas, GlyphRect.Left, GlyphRect.Top, FDisBitmap);
                  //ElVCLUtils.DrawTransparentBitmapEx(ACanvas.Handle, FDisBitmap, GlyphRect.Left,
                  //  GlyphRect.Top, Rect(0, 0, FDisBitmap.Width, FDisBitmap.Height), RGB(255, 255, 255))
                end
                else
                if (Selected) and (not Checked) then
                begin
                  if DStyle = tdsNormal then
                    DrawEdge(ACanvas.Handle, BarRect, BDR_RAISEDINNER, BF_RECT);

                  if DStyle = tdsOfficeXP then
                  begin
                    ImageList.GetBitmap(ImageIndex, Bmp);
                    ShadowBitmap(Bmp);
                    ElVCLUtils.DrawTransparentBitmapEx(ACanvas.Handle, Bmp, GlyphRect.Left + 1,
                      GlyphRect.Top + 1, Rect(0, 0, Bmp.Width, Bmp.Height), RGB(255, 255, 255));
                    ImageList.Draw(ACanvas, GlyphRect.Left - 1, GlyphRect.Top - 1, ImageIndex{$ifdef VCL_4_USED}, true{$endif});
                  end
                  else
                    ImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex{$ifdef VCL_4_USED}, true{$endif});
                end
                else
                  ImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex{$ifdef VCL_4_USED}, true{$endif})
              end
              else
              begin
                { Draw a menu check }
                Glyph := TBitmap.Create;
                try
                  Glyph.Transparent := True;
                  OldFontColor := Font.Color;

                  if (Selected) {and ((IsWinXPUp) or (DStyle = tdsWindowsXP))} then
                  begin
                    Font.Color := clHighlightText
                  end
                  else
                    Font.Color := clMenuText;

                  if FRadioItem then
                  begin
                    Glyph.TransparentColor := RGB(255, 0, 255);
                    Glyph.Canvas.Brush.Color := RGB(255, 0, 255);
                    Glyph.Canvas.FillRect(Rect(0, 0, 13, 13));
                    Glyph.Height := 13;
                    Glyph.Width := 13;
                    if (DStyle = tdsWindowsXP) and (Selected) then
                    begin
                      Glyph.Canvas.Brush.Color := clHighlightText;
                      Glyph.Canvas.Pen.Color := clHighlightText;
                    end
                    else
                      Glyph.Canvas.Brush.Color := clBlack;

                    if DStyle <> tdsOfficeXP then
                      Glyph.Canvas.Ellipse(2, 2, 11, 11)
                    else
                      Glyph.Canvas.Ellipse(2, 2, 9, 9)
                  end
                  else
                    Glyph.Handle := LoadBitmap(0, PChar(OBM_CHECK));

                  if DStyle = tdsOfficeXP then
                  begin
                    OldBrushColor := Brush.Color;
                    if Selected then
                      Brush.Color := BrightColor(clHighLight, 55)
                    else
                      Brush.Color := BrightColor(clHighLight, 85);

                    OldPenColor := Pen.Color;
                    Pen.Color := clHighlight;
                    OffsetRect(GlyphRect, -2, 0);
                    //InflateRect(GlyphRect, 1, 1);
                    //FillRect(GlyphRect);
                    Rectangle(GlyphRect.Left, GlyphRect.Top, GlyphRect.Right, GlyphRect.Bottom);
                    InflateRect(GlyphRect, -1, -1);
                    OffsetRect(GlyphRect, 1, 1);
                    Brush.Color := OldBrushColor;
                    Pen.Color := OldPenColor;
                  end;

                  (*
                  if DStyle = tdsNormal then
                  begin
                    OldBrushColor := Brush.Color;
                    Brush.Bitmap := PatternBitmap(clBtnFace, clBtnHighlight);
                    FillRect(BarRect);
                    DrawEdge(ACanvas.Handle, BarRect, BDR_SUNKENINNER, BF_RECT);
                    Brush.Color := OldBrushColor;
                  end;
                  *)
                  //Brush.Style := bsClear;

                  if (DStyle <> tdsOfficeXP) or (ImageIndex < 0) then
                  begin
                    if RightToLeftMenuItem then //GTP
                    begin
                      RightGlyphRect := GlyphRect;

                      w1 := ARect.Right - ARect.Left;
                      w2 := GlyphRect.Right - GlyphRect.Left;
                      w3 := (GlyphRect.Left - ARect.Left) * 2;
                      OffsetRect (RightGlyphRect, w1-w2-w3, 0);
                      RightGlyphRect.Left := RightGlyphRect.Left + (RightGlyphRect.Right - RightGlyphRect.Left - Glyph.Width) div 2;
                      RightGlyphRect.Top := RightGlyphRect.Top - 2 + (RightGlyphRect.Bottom - RightGlyphRect.Top - Glyph.Height) div 2;

                      Draw(RightGlyphRect.Left, RightGlyphRect.Top, Glyph);
                    end
                    else
                    begin
                      Draw(GlyphRect.Left + (GlyphRect.Right - GlyphRect.Left - Glyph.Width) div 2,
                        GlyphRect.Top + (GlyphRect.Bottom - GlyphRect.Top - Glyph.Height) div 2, Glyph);
                    end;
                  end;
                  Font.Color := OldFontColor;
                  //Dec(GlyphRect.Right, 2);

                  //Brush.Style := bsSolid;
                finally
                  Glyph.Free;
                end;
              end;
            end
            else
            if (Assigned(FBitmap) and not FBitmap.Empty) then
            begin
              SaveRect := GlyphRect;
              if FBitmap.Width < GlyphRect.Right - GlyphRect.Left then
                with GlyphRect do
                begin
                  Left := Left + ((Right - Left) - FBitmap.Width) div 2 + 1;
                  Right := Left + FBitmap.Width;
                end;
              if FBitmap.Height < GlyphRect.Bottom - GlyphRect.Top then
                with GlyphRect do
                begin
                  Top := Top + ((Bottom - Top) - FBitmap.Height) div 2 + 1;
                  Bottom := Top + FBitmap.Height;
                end;
              if RightToLeftMenuItem then //GTP
              begin
                RightGlyphRect := GlyphRect;
                w1 := ARect.Right - ARect.Left;
                w2 := GlyphRect.Right - GlyphRect.Left;
                w3 := (GlyphRect.Left - ARect.Left) * 2;
                OffsetRect (RightGlyphRect, w1-w2-w3, 0);
                RightGlyphRect.Left := RightGlyphRect.Left + (RightGlyphRect.Right - RightGlyphRect.Left - FBitmap.Width) div 2;
                RightGlyphRect.Top := RightGlyphRect.Top + (RightGlyphRect.Bottom - RightGlyphRect.Top - FBitmap.Height) div 2;
                StretchDraw(RightGlyphRect, FBitmap);
              end
              else
              begin
                StretchDraw(GlyphRect, FBitmap);
              end;
              GlyphRect := SaveRect;
            end;

//            if Checked then
//            begin
//              Dec(GlyphRect.Right);
//              Dec(GlyphRect.Bottom);
//            end;
          end
          else
          begin
            if (not TopLevel) then
            begin
              if ((DStyle <> tdsOfficeXP) or TopLevel) then
              begin
                if ImageList <> nil then
                  GlyphRect.Right := GlyphRect.Left + Max(GetSystemMetrics(SM_CXMENUCHECK), ImageList.Width) + 1
                else
                  GlyphRect.Right := GlyphRect.Left + GetSystemMetrics(SM_CXMENUCHECK) + 1;
              end;
            end
            else
            begin
              if (DStyle <> tdsOfficeXP) then
              begin
                GlyphRect.Right := GlyphRect.Left;
                GlyphRect.Bottom := GlyphRect.Top;
              end;
            end;
            (*
            Checks := false;
            for i := 0 to FParent.Count - 1 do
            begin
              if FParent.Items[i].Checked then
              begin
                Checks := true;
                System.break;
              end;
            end;
            if Checks then
            begin
              GlyphRect.Right := GetSystemMetrics(SM_CXMENUCHECK) + 2;
              GlyphRect.Bottom := GetSystemMetrics(SM_CYMENUCHECK);
            end;
            *)
          end;
        end;
        InflateRect(GlyphRect, 1, 1);
        dec(GlyphRect.Right, 3);

        if (Selected) and (TopLevel) and Enabled then
        begin
          if (DStyle = tdsOfficeXP) then
          begin
            ARect.Left := GlyphRect.Right + 1;
            InflateRect(ARect, -1, -1);
            Inc(ARect.Bottom, 1);
          end;
          FillRect(ARect);
          if (DStyle = tdsOfficeXP) then
          begin
            Dec(ARect.Bottom, 1);
            InflateRect(ARect, 1, 1);
          end;
        end;

        ARect.Left := GlyphRect.Right + 1;
        if not TopLevel then
          Inc(ARect.Left, 4);

        Dec(ARect.Right, 1);

        DrawStyle := DT_EXPANDTABS or DT_SINGLELINE;
        if TopLevel then
          DrawStyle := DrawStyle or DT_CENTER
        else
          DrawStyle := DrawStyle or Alignments[Alignment];

        SaveRect := ARect;

        if (RightToLeftMenuItem) and (not TopLevel) then //GTP
        begin
          DrawStyle := DrawStyle and (not DT_LEFT) or DT_RIGHT;
          DoDrawText(ACanvas, Caption, ARect, Selected, DrawStyle or DT_CALCRECT or DT_NOCLIP);
          if Caption = cLineCaption then
            OffsetRect(SaveRect, 0, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2)
           else
            OffsetRect(SaveRect, -16, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2);

          DoDrawText(ACanvas, Caption, SaveRect, Selected, DrawStyle);
        end
        else
        begin
          DoDrawText(ACanvas, Caption, ARect, Selected, DrawStyle or DT_CALCRECT or DT_NOCLIP);
          OffsetRect(SaveRect, 0, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2);
          DoDrawText(ACanvas, Caption, SaveRect, Selected, DrawStyle);
        end;

        if (ShortCut <> 0) and (not TopLevel) and (Caption <> cLineCaption) then
        begin
          if RightToLeftMenuItem then //GTP
          begin
            RightHotKeyRect.Top := SaveRect.Top;
            RightHotKeyRect.Bottom := SaveRect.Bottom;
            RightHotKeyRect.Left := RightHotKeyRect.Left + 4;
            RightHotKeyRect.Right := SaveRect.Right + 32;
            S := ShortCutToText(ShortCut);
            DoDrawText(ACanvas, S, RightHotKeyRect, Selected, DT_LEFT);
          end
          else
          begin
            ARect.Left := ARect.Right;
            ARect.Right := SaveRect.Right - GetSystemMetrics(SM_CXMENUCHECK);
            ARect.Top := SaveRect.Top;
            ARect.Bottom := SaveRect.Bottom;
            S := ShortCutToText(ShortCut);
            {$ifdef HAS_HTML_RENDER}
            if IsHTML then
              S := '<p align="right">' + S;
            {$endif}
            DoDrawText(ACanvas, S, ARect, Selected, DT_RIGHT);
          end;
        end;
      end;
      Bmp.Free;
    end;
  end;
end;

procedure TElMenuItem.MeasureItem(ACanvas: TCanvas; var Width, Height: Integer);
const
  Alignments: array[TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Alignment: TPopupAlignment;
  ImageList: {$ifdef VCL_4_USED}TCustomImageList{$else}TImageList{$endif};
  ParentMenu: TMenu;
  DrawGlyph: Boolean;
  TopLevel: Boolean;
  DrawStyle: Integer;
  DStyle: TDrawStyle;
  Text: TElFString;
  R: TRect;
  HO: integer;
  (*
  {$ifdef HAS_HTML_RENDER}
  IsHTML: boolean;
  Render: TElHTMLRender;
  APos: integer;
  {$endif}
  *)
  
  procedure GetMenuSize;
  var
    NonClientMetrics: TNonClientMetrics;
  begin
    NonClientMetrics.cbSize := sizeof(NonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    begin
      Width := NonClientMetrics.iMenuWidth;
      Height := NonClientMetrics.iMenuHeight;
    end;
  end;

begin
  if GetParentComponent is TElMainMenu then
  begin
    TopLevel := True;
    GetMenuSize;
  end
  else
    TopLevel := False;
  (*
  {$ifdef HAS_HTML_RENDER}
  IsHTML := false;
  Render := nil;
  {$endif}
  *)
  DStyle := tdsNormal;
  ImageList := nil;
  ParentMenu := GetParentMenu;
  if TMenu(ParentMenu) is TElPopupMenu then
  begin
    DStyle := TElPopupMenu(ParentMenu).DrawStyle;
    ACanvas.Font := TElPopupMenu(ParentMenu).Font;
    ImageList := TElPopupMenu(ParentMenu).Images;
    (*{$ifdef HAS_HTML_RENDER}
    IsHTML := TElPopupMenu(ParentMenu).IsHTML;
    Render := TElPopupMenu(ParentMenu).FRender;
    {$endif}
    *)
  end
  else
  if TMenu(ParentMenu) is TElMainMenu then
  begin
    DStyle := TElMainMenu(ParentMenu).DrawStyle;
    ACanvas.Font := TElMainMenu(ParentMenu).Font;
    ImageList := TElMainMenu(ParentMenu).Images;
    (*{$ifdef HAS_HTML_RENDER}
    IsHTML := TElMainMenu(ParentMenu).IsHTML;
    Render := TElMainMenu(ParentMenu).FRender;
    {$endif}
    *)
  end;

  if Caption = cLineCaption then
  begin
    if DStyle <> tdsNormal then
      Height := 2
    else
      Height := 7;

    Width := 0; // -2
    DrawGlyph := False;
  end
  else
  if Assigned(ImageList) and ((ImageIndex > -1) or not TopLevel) then
  begin
    Width := ImageList.Width;

    if not TopLevel then
      Height := ImageList.Height;

    DrawGlyph := True;
  end
  else
  if Assigned(FBitmap) and not FBitmap.Empty then
  begin
    Width := FBitmap.Width;
    if not TopLevel then
      Height := FBitmap.Height;
    DrawGlyph := True;
  end
  else
  if (DStyle = tdsOfficeXP) and (not TopLevel) then
  begin
    Width := 0;
    DrawGlyph := true;
  end
  else
  begin
    Width := 0;
    DrawGlyph := False;

    if not TopLevel then
    begin
      Width := GetsystemMetrics(SM_CXMENUCHECK);
      Height := GetSystemMetrics(SM_CYMENUCHECK);
    end;
  end;

  HO := Width;

  if not TopLevel and (not DrawGlyph) and Checked then
  begin
    // this is used for the case when the item doesn't have image but is checked.
    // we must take check size into account
    Inc(Width, 16);
    Inc(HO, 16);
  end;

  if not TopLevel then
    Inc(Height, 3);

  FillChar(R, SizeOf(R), 0);
  if ParentMenu is TElMenu then
    Alignment := paLeft
  else
  if TMenu(ParentMenu) is TElPopupMenu then
    Alignment := TPopupMenu(ParentMenu).Alignment
  else
    Alignment := paLeft;
  if ShortCut <> 0 then
    Text := Caption + ' ' + ShortCutToText(ShortCut)
  else
    Text := Caption;
  DrawStyle := Alignments[Alignment] or DT_EXPANDTABS or DT_SINGLELINE or
    DT_NOCLIP or DT_CALCRECT;

  //ACanvas.Font.Color := clMenuText;
  (*
  {$ifdef HAS_HTML_RENDER}
  if IsHTML then
  begin
    APos := pos('&',Text);
    if (APos<>0)and(APos<Length(Text)) then
    begin
      System.Delete(Text, APos, 1);
      System.Insert('</u>',Text,APos+1);
      System.Insert('<u>',Text,APos);
    end;
    Render.Data.DefaultColor := ACanvas.Font.Color;
    Render.Data.DefaultStyle := ACanvas.Font.Style;
    Render.Data.DefaultHeight := ACanvas.Font.Height;
    Render.Data.DefaultFont := ACanvas.Font.Name;
    Render.Data.Charset := ACanvas.Font.Charset;
    Render.PrepareText(Text, 0, false);
    R.Right := R.Left + Render.Data.TextSize.cx;
    R.Bottom := R.Top + Render.Data.TextSize.cy;
  end else
  {$endif}
  *)
  if FCaption <> cLineCaption then
  begin
    DoDrawText(ACanvas, Text, R, False, DrawStyle);
    Inc(Width, R.Right - R.Left {+ 7});
    Height := Max(Height, R.Bottom - R.Top);
  end;

  //if Shortcut <> 0 then
  if not TopLevel then
    inc(Width, GetsystemMetrics(SM_CXMENUCHECK));
  (*if Shortcut <> 0 then
  begin
    DoDrawText(ACanvas, ShortcutToText(ShortCut), R, False, DrawStyle);
    Inc(Width, R.Right - R.Left + 7);
  end;
  *)
  //Inc(Height, 2);

  if (not TopLevel) and (Assigned(ImageList) or Checked) and (not DrawGlyph) then
  begin
    Inc(Width, 16);
    HO := HO + 16;
  end;
  if (DStyle = tdsOfficeXP) and (not TopLevel) then
  begin
    //if FCaption <> cLineCaption then Inc(Height, 2);
    Inc(Width, Height - HO);
  end;
  if Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, ACanvas, Width, Height);
end;

function TElMenuItem.HasParent: Boolean;
begin
  Result := True;
end;

procedure TElMenuItem.SetBreak(Value: TMenuBreak);
begin
  if FBreak <> Value then
  begin
    FBreak := Value;
    MenuChanged(True);
  end;
end;

procedure TElMenuItem.SetCaption(const Value: TElFString);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    inherited Caption := Value; 
    MenuChanged(True);
  end;
end;

procedure TElMenuItem.TurnSiblingsOff;
var
  I: Integer;
  Item: TElMenuItem;
begin
  if FParent <> nil then
    for I := 0 to FParent.Count - 1 do
    begin
      Item := FParent[I];
      if (Item <> Self) and Item.FRadioItem and (Item.GroupIndex = GroupIndex) then
        Item.SetChecked(False);
    end;
end;
  
procedure TElMenuItem.SetChecked(Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    if (FParent <> nil) and not (csReading in ComponentState) then
      CheckMenuItem(FParent.Handle, FCommand, MF_BYCOMMAND or Checks[Value]);
    if Value and FRadioItem then
      TurnSiblingsOff;
  end;
end;

procedure TElMenuItem.SetEnabled(Value: Boolean);
var
  ImageList: TCustomImageList;
  ParentMenu: TMenu;
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Count <> 0)) or
      ((Parent <> nil) and Assigned(Parent.FMergedWith)) then
      MenuChanged(True)
    else
    begin
      if (FParent <> nil) and not (csReading in ComponentState) then
        EnableMenuItem(FParent.Handle, FCommand, MF_BYCOMMAND or Enables[Value]);
      MenuChanged(False);
    end;

    ParentMenu := GetParentMenu;
    if ParentMenu is TElPopupMenu then
      ImageList := TElPopupMenu(ParentMenu).Images
    else
      ImageList := TElMainMenu(ParentMenu).Images;

    if (FEnabled) or not ((ImageList <> nil) and (ImageIndex > 0)
                  and (ImageIndex < ImageList.Count)) then
    begin
      if not Assigned(FDisBitmap) then
      begin
        FDisBitmap.Free;
        FDisBitmap := nil;
      end;
    end
    else
    begin
      if not Assigned(FDisBitmap) then
        GrayBitmap(ImageList, ImageIndex, FDisBitmap);
    end;
  end;
end;

procedure TElMenuItem.SetGroupIndex(Value: Byte);
begin
  if FGroupIndex <> Value then
  begin
    if Parent <> nil then Parent.VerifyGroupIndex(Parent.IndexOf(Self), Value);
    FGroupIndex := Value;
    if FChecked and FRadioItem then
      TurnSiblingsOff;
  end;
end;

{$ifdef VCL_4_USED}
function TElMenuItem.GetAction: TBasicAction;
begin
  if FActionLink <> nil then
    Result := FActionLink.Action
  else
    Result := nil;
end;

function TElMenuItem.GetActionLinkClass: TMenuActionLinkClass;
begin
  Result := TMenuActionLink;
end;
{$endif}

function TElMenuItem.GetCount: Integer;
begin
  if FItems = nil then 
    Result := 0
  else 
    Result := FItems.Count;
end;

function TElMenuItem.GetItem(Index: Integer): TElMenuItem;
begin
  if FItems = nil then EMenuError.CreateFmt('Index Error', [Name]);
  Result := FItems[Index];
end;

procedure TElMenuItem.SetShortCut(Value: TShortCut);
begin
  if FShortCut <> Value then
  begin
    FShortCut := Value;
    MenuChanged(True);
  end;
end;

procedure TElMenuItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    MenuChanged(True);
  end;
end;

procedure TElMenuItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    MenuChanged(True);
  end;
end;

function TElMenuItem.GetMenuIndex: Integer;
begin
  Result := -1;
  if FParent <> nil then Result := FParent.IndexOf(Self);
end;

procedure TElMenuItem.SetMenuIndex(Value: Integer);
var
  Parent: TElMenuItem;
  Count: Integer;
begin
  if FParent <> nil then
  begin
    Count := FParent.Count;
    if Value < 0 then Value := 0;
    if Value >= Count then Value := Count - 1;
    if Value <> MenuIndex then
    begin
      Parent := FParent;
      Parent.Remove(Self);
      Parent.Insert(Value, Self);
    end;
  end;
end;

procedure TElMenuItem.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Proc(Items[I]);
end;

procedure TElMenuItem.SetChildOrder(Child: TComponent; Order: Integer);
begin
  (Child as TElMenuItem).MenuIndex := Order;
end;

procedure TElMenuItem.SetDefault(Value: Boolean);
var
  I: Integer;
begin
  if FDefault <> Value then
  begin
    if Value and (FParent <> nil) then
      for I := 0 to FParent.Count - 1 do
        if FParent[I].Default then FParent[I].FDefault := False;
    FDefault := Value;
    MenuChanged(True);
  end;
end;

{$ifdef VCL_4_USED}
procedure TElMenuItem.InitiateAction;
begin
  if FActionLink <> nil then
    FActionLink.Update;
end;
{$endif}

procedure TElMenuItem.DesignRebuild;
begin
  if (csDesigning in ComponentState) then
    RebuildHandle;
end;

procedure TElMenuItem.Insert(Index: Integer; Item: TElMenuItem);
begin
  if Item.FParent <> nil then
    raise EMenuError.CreateFmt('Menu Reinserted', [Name]);
  if FItems = nil then FItems := TElList.Create;
  if (Index - 1 >= 0) and (Index - 1 < FItems.Count) then
    if Item.GroupIndex < TElMenuItem(FItems[Index - 1]).GroupIndex then
      Item.GroupIndex := TElMenuItem(FItems[Index - 1]).GroupIndex;
  VerifyGroupIndex(Index, Item.GroupIndex);
  FItems.Insert(Index, Item);
  Item.FParent := Self;
  Item.FOnChange := SubItemChanged;
  if FHandle <> 0 then
    RebuildHandle;
  MenuChanged(Count = 1);
end;

procedure TElMenuItem.Delete(Index: Integer);
var
  Cur: TElMenuItem;
begin
  if (Index < 0) or (FItems = nil) or (Index >= GetCount) then
    EMenuError.CreateFmt('Index Error', [Name]);
  Cur := FItems[Index];
  FItems.Delete(Index);
  Cur.FParent := nil;
  Cur.FOnChange := nil;
  if FHandle <> 0 then
    RebuildHandle;
  MenuChanged(Count = 0);
end;

procedure TElMenuItem.Click;
begin
  if Enabled then
  begin
    {$ifdef VCL_4_USED}
    if Assigned(FOnClick) and (Action <> nil) and (@FOnClick <> @Action.OnExecute) then
      FOnClick(Self)
    else
    if (not (csDesigning in ComponentState)) and (ActionLink <> nil) then
       FActionLink.Execute
    else
    {$endif}
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;

function TElMenuItem.Find(ACaption: TElFString): TElMenuItem;
var
  I: Integer;
begin
  Result := nil;
  ACaption := ElStripHotkey(ACaption);
  for I := 0 to Count - 1 do
{$ifdef ELPACK_UNICODE}
    if WideSameText(ACaption, ElStripHotkey(Items[I].Caption)) then
{$else}
    if AnsiSameText(ACaption, ElStripHotkey(Items[I].Caption)) then
{$endif}
    begin
      Result := Items[I];
      System.Break;
    end;
end;

function TElMenuItem.IndexOf(Item: TElMenuItem): Integer;
begin
  Result := -1;
  if FItems <> nil then Result := FItems.IndexOf(Item);
end;

procedure TElMenuItem.Add(Item: TElMenuItem);
begin
  Insert(GetCount, Item);
end;

procedure TElMenuItem.Remove(Item: TElMenuItem);
var
  I: Integer;
begin
  I := IndexOf(Item);
  if I = -1 then raise EMenuError.CreateFmt('Menu Not Found', [Name]);
  Delete(I);
end;

procedure TElMenuItem.MenuChanged(Rebuild: Boolean);
var
  Source: TElMenuItem;
begin
  if (Parent = nil) and ((Owner is TElMainMenu) or (Owner is TElPopupMenu)) then
    Source := nil
  else
    Source := Self;
  if Assigned(FOnChange) then FOnChange(Self, Source, Rebuild);
end;

procedure TElMenuItem.SubItemChanged(Sender: TObject; Source: TElMenuItem; Rebuild: Boolean);
begin
  if Rebuild and ((FHandle <> 0) or Assigned(FMergedWith)) then
    RebuildHandle;
  if Parent <> nil then
    Parent.SubItemChanged(Self, Source, False)
  else
    if Owner is TElMainMenu then 
      TElMainMenu(Owner).ItemChanged;
end;

function TElMenuItem.GetBitmap: TBitmap;
begin
  if FBitmap = nil then FBitmap := TBitmap.Create;
  FBitmap.Transparent := True;
  Result := FBitmap;
end;

{$ifdef VCL_4_USED}
procedure TElMenuItem.SetAction(Value: TBasicAction);
begin
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end
  else
  begin
    if FActionLink = nil then
      FActionLink := GetActionLinkClass.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(Self);
  end;
end;
{$endif}

procedure TElMenuItem.SetBitmap(Value: TBitmap);
begin
  if FBitmap = nil then FBitmap := TBitmap.Create;
  FBitmap.Assign(Value);
  MenuChanged(False);
end;

{$ifdef VCL_4_USED}
procedure TElMenuItem.InitiateActions;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].InitiateAction;
end;
{$endif}

function TElMenuItem.GetParentComponent: TComponent;
begin
  if (FParent <> nil) and (FParent.FMenu <> nil) then
    Result := FParent.FMenu else
    Result := FParent;
end;

procedure TElMenuItem.SetParentComponent(Value: TComponent);
begin
  if FParent <> nil then FParent.Remove(Self);
  if Value <> nil then
    if Value is TElMainMenu then
      TElMainMenu(Value).Items.Add(Self)
    else
      if Value is TElPopupMenu then
        TElPopupMenu(Value).Items.Add(Self)
      else
       if Value is TElMenuItem then
         TElMenuItem(Value).Add(Self);
end;

function TElMenuItem.GetParentMenu: TElMenu;
var
  MenuItem: TElMenuItem;
begin
  MenuItem := Self;
  while Assigned(MenuItem.FParent) do 
    MenuItem := MenuItem.FParent;
  Result := MenuItem.FMenu;
end;

procedure TElMenuItem.SetRadioItem(Value: Boolean);
begin
  if FRadioItem <> Value then
  begin
    FRadioItem := Value;
    if FChecked and FRadioItem then
      TurnSiblingsOff;
    MenuChanged(True);
  end;
end;

//procedure TElMenuItem.ReadCaption(Reader : TReader);
//begin
//  Caption := Reader.ReadWideString;
//end;
//
//procedure TElMenuItem.WriteCaption(Writer : TWriter);
//begin
//  Writer.WriteWideString(Caption);
//end;
//
//procedure TElMenuItem.ReadHint(Reader : TReader);
//begin
//  Hint := Reader.ReadWideString;
//end;
//
//procedure TElMenuItem.WriteHint(Writer : TWriter);
//begin
//  Writer.WriteWideString(Hint);
//end;
//
procedure TElMenuItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ShortCutText', ReadShortCutText, WriteShortCutText, False);
//  Filer.DefineProperty('Caption', ReadCaption, WriteCaption, true);
//  Filer.DefineProperty('Hint', ReadHint, WriteHint, true);
end;

{$ifdef VCL_4_USED}
procedure TElMenuItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Action is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') then
        Self.Caption := Caption;
      if not CheckDefaults or (Self.Checked = False) then
        Self.Checked := Checked;
      if not CheckDefaults or (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.HelpContext = 0) then
        Self.HelpContext := HelpContext;
      if not CheckDefaults or (Self.Hint = '') then
        Self.Hint := Hint;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
      if not CheckDefaults or (Self.ShortCut = scNone) then
        Self.ShortCut := ShortCut;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
        Self.OnClick := OnExecute;
    end;
end;

procedure TElMenuItem.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

function TElMenuItem.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCaptionLinked;
end;

function TElMenuItem.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsCheckedLinked;
end;

function TElMenuItem.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsEnabledLinked;
end;

function TElMenuItem.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHintLinked;
end;

function TElMenuItem.IsHelpContextStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsHelpContextLinked;
end;

function TElMenuItem.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsImageIndexLinked;
end;

function TElMenuItem.IsShortCutStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsShortCutLinked;
end;

function TElMenuItem.IsVisibleStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsVisibleLinked;
end;

function TElMenuItem.IsOnClickStored: Boolean;
begin
  Result := (ActionLink = nil) or not FActionLink.IsOnExecuteLinked;
end;
{$endif}

procedure TElMenuItem.AssignTo(Dest: TPersistent);
begin
  {$ifdef VCL_4_USED}
  if Dest is TCustomAction then
    with TCustomAction(Dest) do
    begin
      Enabled := Self.Enabled;
      HelpContext := Self.HelpContext;
      Hint := Self.Hint;
      ImageIndex := Self.ImageIndex;
      Caption := Self.Caption;
      Visible := Self.Visible;
      OnExecute := Self.OnClick;
    end
  else
  {$endif}
  inherited AssignTo(Dest);
end;

procedure TElMenuItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  {$ifdef VCL_4_USED}
  if (Operation = opRemove) and (AComponent = Action) then Action := nil;
  {$endif}
end;

function TElMenuItem.GetImageWidth: Integer;
var ImageList : TCustomImageList;
    ParentMenu: TMenu;
    IsOfficeXP: boolean;
begin
  ParentMenu := GetParentMenu;
  if ParentMenu is TElPopupMenu then
  begin
    ImageList := TElPopupMenu(ParentMenu).Images;
    IsOfficeXP := TElPopupMenu(ParentMenu).DrawStyle = tdsOfficeXP;
  end
  else
  begin
    ImageList := TElMainMenu(ParentMenu).Images;
    IsOfficeXP := TElMainMenu(ParentMenu).DrawStyle = tdsOfficeXP;
  end;

  if ImageList <> nil then
   result := TImageList(ImageList).Width
  else
  if FBitmap <> nil then
    result := FBitmap.Width
  else
  if not (GetParentComponent is TElMainMenu) then
    result := GetSystemMetrics(SM_CXMENUCHECK)
  else
    result := 0;
  inc(result, 3);
  if IsOfficeXP then
    result := -1; 
end;

procedure TElMenuItem.SetHint(Value: TElFString);
var S : String;
    {$ifdef ELPACK_UNICODE}
    i,
    l : integer;
    T : WideChar;
    {$endif}
begin
  FHint := Value;

  S := FHint;
  {$ifdef ELPACK_UNICODE}
  i := Length(S);
  l := Length(S) + 1 + Length(FHint) * 2;
  SetLength(S, l + 4);

  Move(FHint[1], S[i + 2], Length(FHint) * 2);
  T := #0;
  Move(T, S[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, S[l + 3], sizeof(T));
  {$endif}
  inherited Hint := S;
end;

procedure TElMenuItem.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
end;

procedure TElMenuItem.UpdateCommand;
begin
  FCommand := inherited Command;
end;

{ TElMainMenu }

constructor TElMainMenu.Create(AOwner: TComponent);
begin
  FUnicodeItems := TElMenuItem.Create(Self);
  FUnicodeItems.FMenu := TElMenu(Self);
  inherited Create(AOwner);
  FHook := TElHook.Create(nil);
  FForm := GetOwnerForm(Self);
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  GetFont;
  FSystemFont := true;

  THackClass(Self).FItems.Free;
  THackClass(Self).FItems := FUnicodeItems;
  {$ifdef VCL_4_USED}
  THackClass(Self).FBiDiMode := BiDiMode;
  {$endif}

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  with FHook do
  begin
    Control := FForm;
    Active := true;
    DesignActive := true;
    OnBeforeProcess := OnBeforeHook;
    OnAfterProcess := nil;
  end;
end;

destructor TElMainMenu.Destroy;
begin
  FHook.Control := nil;
//  FUnicodeItems.Free;
  FImageChangeLink.Free;
  FFont.Free;
  FHook.Free;
  inherited;
end;

procedure TElMainMenu.UpdateItems;

  function UpdateItem(MenuItem: TElMenuItem): Boolean;
  begin
    Result := False;
    IterateMenus(@UpdateItem, MenuItem.FMerged, MenuItem);
    MenuItem.SubItemChanged(MenuItem, MenuItem, True);
  end;

begin
  IterateMenus(@UpdateItem, Items.FMerged, Items);
end;

procedure TElMainMenu.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  UpdateItems;
  if WindowHandle <> 0 then
  begin
    Windows.SetMenu(WindowHandle, 0);
    Windows.SetMenu(WindowHandle, Handle);
  end;
end;

procedure TElMainMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Images) and (Operation = opRemove) then Images := nil;
end;

procedure TElMainMenu.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
  begin
    UpdateItems;
    if WindowHandle <> 0 then
    begin
      Windows.SetMenu(WindowHandle, 0);
      Windows.SetMenu(WindowHandle, Handle);
    end;
  end;
end;

procedure TElMainMenu.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    UpdateItems;
  end;
end;

procedure TElMainMenu.SetImages(Value: TImageList);
begin
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FImageChangeLink);
    //{$ifdef VCL_5_USED}
    //FImages.RemoveFreeNotification(Self);
    //{$endif}
  end;
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  if Images <> nil then
    ImageListChange(Images)
  else
    UpdateItems;
  if (Owner <> nil) and (not (csDestroying in ComponentState)) then
    DrawMenuBar(TWinControl(Owner).Handle);
end;


procedure TElMainMenu.SetDrawStyle(Value: TDrawStyle);
begin
  if FDrawStyle <> Value then
  begin
    FDrawStyle := Value;
    OwnerDraw := true;
    Items.DesignRebuild;
  end;
end;


function TElMainMenu.GetHandle: HMENU;
begin
  if FOle2Menu <> 0 then
    Result := FOle2Menu
  else
  begin
    Result := Items.GetHandle;
  end;
end;

function TElMainMenu.IsOwnerDraw: Boolean;
begin
  Result := true;//OwnerDraw or (Images <> nil) or (DrawStyle <> tdsNormal);
end;

procedure TElMainMenu.ProcessMenuChar(var Message: TWMMenuChar);
var
  C, I, First, Hilite, Next: Integer;
  State: Word;

  function IsAccelChar(Menu: HMENU; State: Word; I: Integer; C: Char): Boolean;
  var
    Item: TElMenuItem;
    Id: UINT;
  begin
    Item := nil;
    if State and MF_POPUP <> 0 then
    begin
      Menu := GetSubMenu(Menu, I);
      Item := FindItem(Menu, fkHandle);
    end
    else
    begin
      Id := GetMenuItemID(Menu, I);
      if Id <> $FFFFFFFF then
        Item := FindItem(Id, fkCommand);
    end;
    if Item <> nil then
      Result := IsAccel(Ord(C), Item.Caption) else
      Result := False;
  end;

  function IsInitialChar(Menu: HMENU; State: Word; I: Integer; C: Char): Boolean;
  var
    Item: TElMenuItem;
  begin
    if State and MF_POPUP <> 0 then
    begin
      Menu := GetSubMenu(Menu, I);
      Item := FindItem(Menu, fkHandle);
    end
    else
    begin
      Item := FindItem(Menu, fkHandle);
      if (Item <> nil) and (I < Item.Count) then
        Item := Item.Items[I];
    end;
    // First char is a valid accelerator only if the caption does not
    // contain an explicit accelerator
    if (Item <> nil) and (Item.Caption <> '') then
      Result := (AnsiCompareText(Item.Caption[1], C) = 0) and
        (ElGetHotkey(Item.Caption) = '')
    else
      Result := False;
  end;

begin
  with Message do
  begin
    Result := MNC_IGNORE; { No item found: beep }
    First := -1;
    Hilite := -1;
    Next := -1;
    C := GetMenuItemCount(Menu);
    for I := 0 to C - 1 do
    begin
      State := GetMenuState(Menu, I, MF_BYPOSITION);
      if IsAccelChar(Menu, State, I, User) then
      begin
        if State and MF_DISABLED <> 0 then
        begin
          { Close the menu if this is the only disabled item to choose from.
            Otherwise, ignore the item. }
          if First < 0 then First := -2;
          Continue;
        end;
        if First < 0 then
        begin
          First := I;
          Result := MNC_EXECUTE;
        end
        else
          Result := MNC_SELECT;
        if State and MF_HILITE <> 0 then
          Hilite := I
        else if Hilite >= 0 then
          Next := I;
      end;
    end;
    { We found a single disabled item. End the selection. }
    if First < -1 then
    begin
      Result := MNC_CLOSE shl 16;
      Exit;
    end;

    { If we can't find accelerators, then look for initial letters }
    if First < 0 then
    for I := 0 to C - 1 do
      begin
        State := GetMenuState(Menu, I, MF_BYPOSITION);
        if IsInitialChar(Menu, State, I, User) then
        begin
          if State and MF_DISABLED <> 0 then
          begin
            Result := MNC_CLOSE shl 16;
            Exit;
          end;
          if First < 0 then
          begin
            First := I;
            Result := MNC_EXECUTE;
          end
          else
            Result := MNC_SELECT;
          if State and MF_HILITE <> 0 then
            Hilite := I
          else if Hilite >= 0 then
            Next := I;
        end;
      end;

    if (Result = MNC_EXECUTE) then
      Result := Result shl 16 or First
    else if Result = MNC_SELECT then
    begin
      if Next < 0 then
        Next := First;
      Result := Result shl 16 or Next;
    end;
  end;
end;


function TElMainMenu.FindItem(Value: Integer; Kind: TFindItemKind): TElMenuItem;
var
  FoundItem: TElMenuItem;

  function Find(Item: TElMenuItem): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if ((Kind = fkCommand) and (Value = Item.Command)) or
      ((Kind = fkHandle) and (Value = Integer(Item.FHandle))) or
      ((Kind = fkShortCut) and (Value = Item.ShortCut)) then
    begin
      FoundItem := Item;
      Result := True;
      Exit;
    end
    else
      for I := 0 to Item.GetCount - 1 do
        if Find(Item[I]) then
        begin
          Result := True;
          Exit;
        end;
  end;

begin
  FoundItem := nil;
  IterateMenus(@Find, Items.FMerged, Items);
  Result := FoundItem;
end;

function TElMainMenu.IsShortCut(var Message: TWMKey): Boolean;
type
  TClickResult = (crDisabled, crClicked, crShortCutMoved);
const
  AltMask = $20000000;
var
  ShortCut: TShortCut;
  ShortCutItem: TElMenuItem;
  ClickResult: TClickResult;

  function DoClick(Item: TElMenuItem): TClickResult;
  begin
    Result := crClicked;
    if Item.Parent <> nil then Result := DoClick(Item.Parent);
    if Result = crClicked then
      if Item.Enabled then
        try
          {$ifdef VCL_4_USED}
          if not (csDesigning in ComponentState) then Item.InitiateActions;
          {$endif}
          Item.Click;
          if (Item <> ShortCutItem) and (ShortCutItem.ShortCut <> ShortCut) then
            Result := crShortCutMoved;
        except
          Application.HandleException(Self);
        end
      else Result := crDisabled;
  end;

begin
  ShortCut := Byte(Message.CharCode);
  if GetKeyState(VK_SHIFT) < 0 then Inc(ShortCut, scShift);
  if GetKeyState(VK_CONTROL) < 0 then Inc(ShortCut, scCtrl);
  if Message.KeyData and AltMask <> 0 then Inc(ShortCut, scAlt);
  repeat
    ClickResult := crDisabled;
    ShortCutItem := FindItem(ShortCut, fkShortCut);
    if ShortCutItem <> nil then
      ClickResult := DoClick(ShortCutItem);
  until ClickResult <> crShortCutMoved;
  Result := ShortCutItem <> nil;
end;

{$ifdef ELPACK_UNICODE}
function TElMainMenu.DoGetMenuString(Menu: HMENU; ItemID: UINT; Str: PWideChar;
                                     MaxCount: Integer; Flag: UINT): Integer;
{$else}
function TElMainMenu.DoGetMenuString(Menu: HMENU; ItemID: UINT; Str: PChar;
                                     MaxCount: Integer; Flag: UINT): Integer;
{$endif}
var
  Item: TElMenuItem;
  State: Word;
begin
  if IsOwnerDraw then
  begin
    Item := nil;
    State := GetMenuState(Menu, ItemID, Flag);
    if State and MF_POPUP <> 0 then
    begin
      Menu := GetSubMenu(Menu, ItemID);
      Item := TElMenuItem(FindItem(Menu, fkHandle));
    end
    else
    begin
      ItemID := GetMenuItemID(Menu, ItemID);
      if ItemID <> $FFFFFFFF then
        Item := TElMenuItem(FindItem(ItemID, fkCommand));
    end;
    if Item <> nil then
    begin
      Str[0] := #0;
      {$ifdef ELPACK_UNICODE}
      WideStrPCopy(Str, WideCopy(Item.Caption, 1, MaxCount));
      {$else}
      StrPLCopy(Str, Item.Caption, MaxCount);
      {$endif}
      Result := Length(Str);
    end
    else
      Result := 0;
  end
  else
    {$ifdef ELPACK_UNICODE}
    Result := GetMenuStringW(Menu, ItemID, Str, MaxCount, Flag);
    {$else}
    Result := GetMenuStringA(Menu, ItemID, Str, MaxCount, Flag);
    {$endif}
end;

function TElMainMenu.UpdateImage: Boolean;
var
  {$ifdef ELPACK_UNICODE}
  Image: array[0..511] of WideChar;
  {$else}
  Image: array[0..511] of Char;
  {$endif}

  procedure BuildImage(Menu: HMENU);
  var
    {$ifdef ELPACK_UNICODE}
    P : PWideChar;
    ImageEnd: PWideChar;
    {$else}
    P : PChar;
    ImageEnd: PChar;
    {$endif}
    I, C: Integer;
    State: Word;
  begin
    C := GetMenuItemCount(Menu);
    P := Image;
    ImageEnd := @Image[(SizeOf(Image) div 2)- 5];
    I := 0;
    while (I < C) and (P < ImageEnd) do
    begin
      DoGetMenuString(Menu, I, P, ImageEnd - P, MF_BYPOSITION);
      {$ifdef ELPACK_UNICODE}
      P := WideStrEnd(P);
      {$else}
      P := StrEnd(P);
      {$endif}
      State := GetMenuState(Menu, I, MF_BYPOSITION);
      {$ifdef ELPACK_UNICODE}
      if State and MF_DISABLED <> 0 then P := WideStrECopy(P, '$');
      if State and MF_MENUBREAK <> 0 then P := WideStrECopy(P, '@');
      if State and MF_GRAYED <> 0 then P := WideStrECopy(P, '#');
      P := WideStrECopy(P, ';');
      {$else}
      if State and MF_DISABLED <> 0 then P := StrECopy(P, '$');
      if State and MF_MENUBREAK <> 0 then P := StrECopy(P, '@');
      if State and MF_GRAYED <> 0 then P := StrECopy(P, '#');
      P := StrECopy(P, ';');
      {$endif}
      Inc(I);
    end;
  end;

begin
  Result := False;
  Image[0] := #0;
  if WindowHandle <> 0 then BuildImage(Handle);
  {$ifdef ELPACK_UNICODE}
  if (FMenuImage = '') or (WideStrComp(PWideChar(FMenuImage), Image) <> 0) then
  {$else}
  if (FMenuImage = '') or (StrComp(PChar(FMenuImage), Image) <> 0) then
  {$endif}
  begin
    Result := True;
    FMenuImage := Image;
  end;
end;

procedure TElMainMenu.ItemChanged;
begin
  MenuChanged(nil, nil, False);
  if WindowHandle <> 0 then
    SendMessage(WindowHandle, CM_MENUCHANGED, 0, 0);
end;

function TElMainMenu.DispatchCommand(ACommand: Word): Boolean;
var
  Item: TElMenuItem;
begin
  Result := False;
  Item := FindItem(ACommand, fkCommand);
  if Item <> nil then
  begin
    Item.Click;
    Result := True;
  end;
end;

procedure TElMainMenu.OnBeforeHook(Sender : TObject; var Message : TMessage;
    var Handled : boolean);
var
  SaveIndex: Integer;
  MenuItem: TMenuItem;
  Canvas: TCanvas;
  DC: HDC;
  TopLevel: boolean;
  Selected: boolean;

begin
  Handled := false;
  with Message do
  begin
    case Msg of
    WM_SETTINGCHANGE:
      begin
        GetFont;
        DrawMenuBar(WindowHandle);
        UpdateItems;
      end;
    WM_MENUCHAR:
    begin
      ProcessMenuChar(TWMMenuChar(Message));
      if TWMMenuChar(Message).Result <> MNC_IGNORE then
        Handled := true;
    end;
    WM_COMMAND:
      DispatchCommand(WParam);
    {$ifndef VCL_4_USED}
    WM_KEYDOWN:
      IsShortCut(TWMKey(Message));
    {$endif}
    WM_INITMENUPOPUP:
      DispatchPopup(WParam);
    WM_DRAWITEM:
      with PDrawItemStruct(LParam)^ do
        if (CtlType = ODT_MENU) then
        begin
          MenuItem := FindItem(itemID, fkCommand);
          if MenuItem <> nil then
          begin
            Canvas := TControlCanvas.Create;
            TopLevel := MenuItem.GetParentComponent is TElMainMenu;
            try
            with Canvas do
            begin
              SaveIndex := SaveDC(hDC);
              try
                Handle := hDC;
                if Assigned(Self.Font) then
                  Canvas.Font.Assign(Self.Font)
                else
                  Canvas.Font.Handle := GetMenuFont;
                case DrawStyle of
                tdsNormal:
                  begin
                    Brush.Style := bsSolid;
                    if itemState and ODS_SELECTED <> 0 then
                    begin
                      if (IsWin98Up or IsWin2000Up) and TopLevel and ((not IsWinXPUp) or (not IsAppThemed)) then
                      begin
                        DrawButtonFrameEx(Canvas.Handle, rcItem, false, true, clBtnFace, true);
                        Brush.Style := bsClear;
                      end
                      else
                      begin
                        Brush.Color := clHighlight;
                        Font.Color := clHighlightText;
                      end;
                    end
                    else
                    begin
                      if ((IsWinXPUp) and (TopLevel)) then
                        Brush.Color := clBtnFace
                      else
                        Brush.Color := clMenu;
                      Font.Color := clMenuText;
                    end;
                    if itemState and ODS_HOTLIGHT <> 0 then
                    begin
                      if (IsWin98Up or IsWin2000Up) and (not (IsWinXPUp) or not IsAppThemed) and TopLevel and ((not IsWinXPUp) or (not IsAppThemed)) then
                      begin
                        DrawButtonFrameEx(Canvas.Handle, rcItem, false, false, clBtnFace, true);
                        Brush.Style := bsClear;
                      end
                      else
                      begin
                        Brush.Color := clHighlight;
                        Font.Color := clHighlightText
                      end;
                    end;
                  end;
                tdsOfficeXP:
                  begin
                    if itemState and ODS_SELECTED <> 0 then
                    begin
                      if TopLevel then
                      begin
                        Brush.Color := BrightColor(clBtnFace, 10);
                        Font.Color := clMenuText;
                        Pen.Color := DarkColor(clBtnFace, 200);
                      end
                      else
                      begin
                        Brush.Color := BrightColor(clHighlight, 70);
                        Font.Color := clMenuText;
                        Pen.Color := clHighlight;
                      end
                    end
                    else
                    begin
                      if (TopLevel) then
                      begin
                        Brush.Color := clBtnFace;
                        Pen.Color := clBtnFace;
                      end
                      else
                      begin
                        Brush.Color := BrightColor(clBtnFace, 80);
                        Pen.Color := BrightColor(clBtnFace, 80);
                      end;
                       Font.Color := clMenuText;
                    end;
                    if itemState and ODS_HOTLIGHT <> 0 then
                    begin
                      Brush.Color := BrightColor(clHighLIght, 70);
                      Font.Color := clMenuText;
                      Pen.Color := clhighLight;
                    end;
                  end;
                tdsWindowsXP:
                  begin
                    if itemState and ODS_SELECTED <> 0 then
                    begin
                      Brush.Color := clHighlight;
                      Font.Color := clHighlightText
                    end
                    else
                    begin
                      if (TopLevel) then
                        Brush.Color := clBtnFace
                      else
                        Brush.Color := clWindow;

                      Font.Color := clMenuText;
                    end;
                    if itemState and ODS_HOTLIGHT <> 0 then
                    begin
                      Brush.Color := clHighlight;
                      Font.Color := clHighlightText
                    end;
                  end;
                end;
                Selected := (itemState and ODS_SELECTED <> 0);
                TElMenuItem(MenuItem).DrawItem(Canvas, rcItem, Selected);
              finally
                Handle := 0;
                RestoreDC(hDC, SaveIndex)
              end;
            end;
            finally
              Canvas.Free;
            end;
            Handled := true;
            Exit;
          end;
          Handled := false;
        end;
    WM_MEASUREITEM:
    begin
      with PMeasureItemStruct(LParam)^ do
        if (CtlType = ODT_MENU) then
        begin
          MenuItem := FindItem(itemID, fkCommand);
          if MenuItem <> nil then
          begin
            DC := GetWindowDC(TWinControl(Owner).Handle);
            try
              Canvas := TControlCanvas.Create;
              with Canvas do
              try
                SaveIndex := SaveDC(DC);
                try
                  Handle := DC;
                  if Assigned(Self.Font) then
                    Canvas.Font.Assign(Self.Font)
                  else
                    Canvas.Font.Handle := GetMenuFont;
                  TElMenuItem(MenuItem).MeasureItem(Canvas, Integer(itemWidth),
                                       Integer(itemHeight));
                finally
                  Handle := 0;
                  RestoreDC(DC, SaveIndex);
                end;
              finally
                Canvas.Free;
              end;
            finally
              ReleaseDC(TWinControl(Owner).Handle, DC);
            end;
            Handled := true;
            Exit;
          end;
        end;
       Handled := false;
    end
    end;
    end;
end;

procedure TElMainMenu.MenuChanged(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
var
  NeedUpdate: Boolean;
begin
  if (WindowHandle <> 0) then
  begin
    NeedUpdate := UpdateImage;  // check for changes before CM_MENUCHANGED does
    if Source = nil then
      SendMessage(WindowHandle, CM_MENUCHANGED, 0, 0);
    if NeedUpdate then
      DrawMenuBar(WindowHandle);
  end;
  {$ifdef VCL_4_USED}
  if ComponentState * [csLoading, csDestroying] = [] then DoChange(Source, Rebuild);
  {$endif}
end;

procedure TElMainMenu.Loaded;
var u : boolean;
begin
  inherited;
  UpdateCommands;
  u := false;
  if Images <> nil then
  begin
    u := true;
  end;
  if SystemFont then
  begin
    GetFont;
    u := true;
  end;
  if u then
  begin
    //if Items.FHandle <> 0 then
    //  DestroyMenu(Items.FHandle);
    //Items.FHandle := 0;
    UpdateItems;
    if WindowHandle <> 0 then
    begin
      Windows.SetMenu(WindowHandle, 0);
      Windows.SetMenu(WindowHandle, Handle);
    end;
  end;
end;

//GTP Procedure
procedure TElMainMenu.SetRightToLeft(Value: Boolean);
var
  MenuItemInfo : TMenuItemInfo;

begin
  if FRightToLeft <> Value then
  begin
    FRightToLeft := Value;
    MenuItemInfo.cbSize := SizeOf (MenuItemInfo);
    MenuItemInfo.fMask := MIIM_TYPE;
    GetMenuItemInfo(Handle, 0, true, MenuItemInfo);

    if FRightToLeft then
      MenuItemInfo.fType := MenuItemInfo.fType or MFT_RIGHTORDER or MFT_RIGHTJUSTIFY
    else
      MenuItemInfo.fType := MenuItemInfo.fType and (not MFT_RIGHTORDER) and (not MFT_RIGHTJUSTIFY);

    SetMenuItemInfo(Handle, 0, true, MenuItemInfo);
  end;
end;

procedure TElMainMenu.SetSystemFont(Value: Boolean);
begin
  if FSystemFont <> Value then
  begin
    FSystemFont := Value;
    if FSystemFont then
      GetFont;
    DrawMenuBar(WindowHandle);
  end;
end;

procedure TElMainMenu.GetFont;
var AFont : HFont;
    LFont : TLogFont;
begin
  AFont := GetMenuFont;
  GetObject(AFont, sizeof(LFont), @LFont);
  Font.Name := LFont.lfFaceName;
  Font.Height := LFont.lfHeight;
  Font.Charset := LFont.lfCharSet;
  Font.Style := [];
  if LFont.lfHeight > 400 then
    Font.Style := Font.Style + [fsBold];
  if LFont.lfItalic <> 0 then
    Font.Style := Font.Style + [fsItalic];
  if LFont.lfUnderline <> 0 then
    Font.Style := Font.Style + [fsUnderline];
  if LFont.lfStrikeOut <> 0 then
    Font.Style := Font.Style + [fsStrikeOut];
  DeleteObject(AFont);
end;

procedure TElMainMenu.FontChange(Sender : TObject);
begin
  if not FSystemFont then
  begin
    if WindowHandle <> 0 then
    begin
      Windows.SetMenu(WindowHandle, 0);
      Windows.SetMenu(WindowHandle, Handle);
    end;
  end;
end;

{$ifdef HAS_HTML_RENDER}
procedure TElMainMenu.SetIsHTML(Value: Boolean);
begin
  if FIsHTML <> Value then
  begin
    FIsHTML := Value;
    if FIsHTML then
    begin
      FRender := TElHTMLRender.Create;
      FRender.OnImageNeeded := TriggerImageNeededEvent;
    end
    else
    begin
      FRender.Free;
      FRender := nil;
    end;
  end;
end;

procedure TElMainMenu.TriggerImageNeededEvent(Sender : TObject; Src : TElFString;
  var Image: TBitmap);
begin
  Image := nil;
  if (assigned(FOnImageNeeded)) then
      FOnImageNeeded(Self, Src, Image);
end;
{$endif}

function TElMainMenu.DispatchPopup(AHandle: HMENU): Boolean;
var
  Item: TElMenuItem;
begin
  Result := False;
  Item := FindItem(AHandle, fkHandle);
  if Item <> nil then
  begin
    {$ifdef VCL_4_USED}
    if not (csDesigning in Item.ComponentState) then Item.InitiateActions;
    {$endif}
    Item.Click;
    Result := True;
  end
  {$ifdef VCL_4_USED}
  else
  if not (csDesigning in ComponentState) and (Self is TMainMenu) then
    Items.InitiateActions;
  {$endif}
end;

procedure TElMainMenu.UpdateCommands;
var i : integer;
begin
  for i := 0 to Items.Count - 1 do
    with Items[i] do
      UpdateCommand;
end;

{ Menu building functions }

procedure InsertItems(var AMenu: TMenu; MainItem: TElMenuItem; Index: Integer; Items: array of TElMenuItem);
var
  I: Integer;

  procedure SetOwner(Item: TElMenuItem);
  var
    I: Integer;
  begin
    if Item <> nil then
    begin
      if Item.Owner = nil then
      begin
        if AMenu is TElPopupMenu then
          TElPopupMenu(AMenu).FForm.InsertComponent(Item)
        else
          TElMainMenu(AMenu).FForm.InsertComponent(Item);
      end;
      for I := 0 to Item.Count - 1 do
        SetOwner(Item[I]);
    end;
  end;

begin
  for I := Low(Items) to High(Items) do
  begin
    SetOwner(Items[I]);
    MainItem.Insert(Index, Items[I]);
  end;
end;

procedure InsertMenuItems(var AMenu: TMenu; Index: Integer; Items: array of TElMenuItem);
var
  I: Integer;

  procedure SetOwner(Item: TElMenuItem);
  var
    I: Integer;
  begin
    if Item <> nil then
    begin
    if Item.Owner = nil then
    begin
      if AMenu is TElPopupMenu then  
        TElPopupMenu(AMenu).Owner.InsertComponent(Item)
      else
        TElMainMenu(AMenu).Owner.InsertComponent(Item);
    end;
    for I := 0 to Item.Count - 1 do
      SetOwner(Item[I]);
  end;
  end;

begin
  for I := Low(Items) to High(Items) do
  begin
    SetOwner(Items[I]);
    if (Index = -1) then
    begin
      if AMenu is TElPopupMenu then
        TElPopupMenu(AMenu).Items.Add(Items[I])
      else
        TElMainMenu(AMenu).Items.Add(Items[I]);
    end
    else
    begin
      if AMenu is TElPopupMenu then
        TElPopupMenu(AMenu).FUniCodeItems.Insert(Index, Items[I])
      else
        TElMainMenu(AMenu).FUniCodeItems.Insert(Index, Items[I]);
    end;
  end;
end;

procedure ElInitMenuItems(AMenu: TMenu; Items: array of TElMenuItem);
var
  I: Integer;

  procedure SetOwner(Item: TElMenuItem);
  var
    I: Integer;
  begin
    if Item.Owner = nil then
      if AMenu is TElPopupMenu then  
        TElPopupMenu(AMenu).Owner.InsertComponent(Item)
      else
        TElMainMenu(AMenu).Owner.InsertComponent(Item);
    for I := 0 to Item.Count - 1 do
      SetOwner(Item[I]);
  end;

begin
  for I := Low(Items) to High(Items) do
  begin
    SetOwner(Items[I]);
    if (AMenu is TElPopupMenu) then
      TElPopupMenu(AMenu).Items.Add(Items[I])
    else
    if (AMenu is TElMainMenu) then
      TElMainMenu(AMenu).Items.Add(Items[I]);
  end;
end;

function ElNewMenu(Owner: TComponent; const AName: TElFString; Items:
                 array of TElMenuItem): TElMainMenu;
begin
  Result := TElMainMenu.Create(Owner);
  Result.Name := AName;
  ElInitMenuItems(Result, Items);
end;

function ElNewSubMenu(const ACaption: TElFString; hCtx: Word;
                    const AName: TElFString; Items: array of TElMenuItem;
                    AEnabled: Boolean): TElMenuItem;
var
  I: Integer;
begin
  Result := TElMenuItem.Create(nil);
  for I := Low(Items) to High(Items) do
    Result.Add(Items[I]);
  Result.Caption := ACaption;
  Result.HelpContext := hCtx;
  Result.Name := AName;
  Result.Enabled := AEnabled;
end;

function ElNewItem(const ACaption: TElFString; AShortCut: TShortCut;
                 AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent; hCtx: Word;
                 const AName: TElFString): TElMenuItem;
begin
  Result := TElMenuItem.Create(nil);
  with Result do
  begin
    Caption := ACaption;
    ShortCut := AShortCut;
    OnClick := AOnClick;
    HelpContext := hCtx;
    Checked := AChecked;
    Enabled := AEnabled;
    Name := AName;
  end;
end;

function ElNewLine: TElMenuItem;
begin
  Result := TElMenuItem.Create(nil);
  Result.Caption := cLineCaption;
end;

function ElGetHotkey(const Text: TElFString): TElFString;
var
  i : Integer;
  Len : Integer;
begin
  Result := '';
  Len := Length(Text);
  for i :=1 to Len do
  begin
    {$ifdef ELPACK_UNICODE}
    if ((Text[i] = cHotKeyPrefix) and (Len - i >= 1)) then
      Result := Text[i + 1];
    {$else}
    if not (Text[i] in LeadBytes) then
      if ((Text[i] = cHotKeyPrefix) and (Len - i >= 1)) then
        Result := Text[i + 1];
    {$endif}
  end;
end;

function ElStripHotKey(const Text: TElFString): TElFString;
var
  i : Integer;
  Len : Integer;
  {$ifdef ELPACK_UNICODE}
  s1 : WideString;
  {$endif}
begin
  Result := Text;
  Len := Length(Result);
  for i := 1 to Len do
  begin
    {$ifdef ELPACK_UNICODE}
    if Result[i] = cHotKeyPrefix then
    begin
      s1 := Result;
      WideDelete(s1, i, 1);
      Result := s1;
    end;
    {$else}
    if not (Result[i] in LeadBytes) then
    begin
      if Result[i] = cHotKeyPrefix then
        if ((i > 1) and (Len - i >= 2) and (Result[i - 1] = '(') and
            (Result[i + 2] = ')') and Syslocale.FarEast) then
          Delete(Result, i - 1, 4)
        else
          Delete(Result, i, 1);
    end;
    {$endif}
  end;
end;

{ TElPopupMenu }

constructor TElPopupMenu.Create(AOwner: TComponent);
begin
  FUnicodeItems := TElMenuItem.Create(Self);
  FUnicodeItems.FMenu := TElMenu(Self);
  inherited Create(AOwner);
  FHook := TElHook.Create(nil);
  FForm := GetOwnerForm(Self);
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  GetFont;
  FSystemFont := true;

  THackClass(Self).FItems.Free;
  THackClass(Self).FItems := FUnicodeItems;

  {$ifndef VCL_4_USED}
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  {$else}
  THackClass(Self).FBiDiMode := BiDiMode;
  {$endif}

  with FHook do
  begin
    Control := FForm;
    Active := true;
    DesignActive := true;
    OnBeforeProcess := OnBeforeHook;
    OnAfterProcess := nil;
  end;
end;

destructor TElPopupMenu.Destroy;
begin
  FHook.Control := nil;
  {$ifndef VCL_4_USED}
  FImageChangeLink.Free;
  {$endif}
  FFont.Free;
  FHook.Free;
  inherited;
end;

procedure TElPopupMenu.UpdateItems;

  function UpdateItem(MenuItem: TElMenuItem): Boolean;
  begin
    Result := False;
    IterateMenus(@UpdateItem, MenuItem.FMerged, MenuItem);
    MenuItem.SubItemChanged(MenuItem, MenuItem, True);
  end;

begin
  IterateMenus(@UpdateItem, Items.FMerged, Items);
end;

procedure TElPopupMenu.DoPopup(Sender: TObject);
begin
  inherited;
end;

function TElPopupMenu.FindItem(Value: Integer; Kind: TFindItemKind): TElMenuItem;
var
  FoundItem: TElMenuItem;

  function Find(Item: TElMenuItem): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if ((Kind = fkCommand) and (Value = Item.Command)) or
      ((Kind = fkHandle) and (Value = Integer(Item.FHandle))) or
      ((Kind = fkShortCut) and (Value = Item.ShortCut)) then
    begin
      FoundItem := Item;
      Result := True;
      Exit;
    end
    else
      for I := 0 to Item.GetCount - 1 do
        if Find(Item[I]) then
        begin
          Result := True;
          Exit;
        end;
  end;

begin
  FoundItem := nil;
  IterateMenus(@Find, Items.FMerged, Items);
  Result := FoundItem;
end;

{$ifndef VCL_4_USED}
procedure TElPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Images) and (Operation = opRemove) then Images := nil;
end;

procedure TElPopupMenu.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    UpdateItems;
  end;
end;

procedure TElPopupMenu.ImageListChange(Sender: TObject);
begin
  if Sender = Images then UpdateItems;
end;

procedure TElPopupMenu.SetImages(Value: TImageList);
begin
  if FImages <> nil then FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  UpdateItems;
end;
{$endif}

function TElPopupMenu.IsShortCut(var Message: TWMKey): Boolean;
type
  TClickResult = (crDisabled, crClicked, crShortCutMoved);
const
  AltMask = $20000000;
var
  ShortCut: TShortCut;
  ShortCutItem: TElMenuItem;
  ClickResult: TClickResult;

  function DoClick(Item: TElMenuItem): TClickResult;
  begin
    Result := crClicked;
    if Item.Parent <> nil then Result := DoClick(Item.Parent);
    if Result = crClicked then
      if Item.Enabled then
        try
          {$ifdef VCL_4_USED}
          if not (csDesigning in ComponentState) then Item.InitiateActions;
          {$endif}
          Item.Click;
          if (Item <> ShortCutItem) and (ShortCutItem.ShortCut <> ShortCut) then
            Result := crShortCutMoved;
        except
          Application.HandleException(Self);
        end
      else Result := crDisabled;
  end;

begin
  ShortCut := Byte(Message.CharCode);
  if GetKeyState(VK_SHIFT) < 0 then Inc(ShortCut, scShift);
  if GetKeyState(VK_CONTROL) < 0 then Inc(ShortCut, scCtrl);
  if Message.KeyData and AltMask <> 0 then Inc(ShortCut, scAlt);
  repeat
    ClickResult := crDisabled;
    ShortCutItem := FindItem(ShortCut, fkShortCut);
    if ShortCutItem <> nil then
      ClickResult := DoClick(ShortCutItem);
  until ClickResult <> crShortCutMoved;
  Result := ShortCutItem <> nil;
end;

function TElPopupMenu.IsOwnerDraw: Boolean;
begin
  Result := true;//OwnerDraw or (Images <> nil) or (DrawStyle <> tdsNormal);
end;

procedure TElPopupMenu.ProcessMenuChar(var Message: TWMMenuChar);
var
  C, I, First, Hilite, Next: Integer;
  State: Word;

  function IsAccelChar(Menu: HMENU; State: Word; I: Integer; C: Char): Boolean;
  var
    Item: TElMenuItem;
    Id: UINT;
  begin
    Item := nil;
    if State and MF_POPUP <> 0 then
    begin
      Menu := GetSubMenu(Menu, I);
      Item := FindItem(Menu, fkHandle);
    end
    else
    begin
      Id := GetMenuItemID(Menu, I);
      if Id <> $FFFFFFFF then
        Item := FindItem(Id, fkCommand);
    end;
    if Item <> nil then
      Result := IsAccel(Ord(C), Item.Caption) else
      Result := False;
  end;

  function IsInitialChar(Menu: HMENU; State: Word; I: Integer; C: Char): Boolean;
  var
    Item: TElMenuItem;
  begin
    if State and MF_POPUP <> 0 then
    begin
      Menu := GetSubMenu(Menu, I);
      Item := FindItem(Menu, fkHandle);
    end
    else
    begin
      Item := FindItem(Menu, fkHandle);
      if (Item <> nil) and (I < Item.Count) then
        Item := Item.Items[I];
    end;
    // First char is a valid accelerator only if the caption does not
    // contain an explicit accelerator
    if (Item <> nil) and (Item.Caption <> '') then
      Result := (AnsiCompareText(Item.Caption[1], C) = 0) and
        (ElGetHotkey(Item.Caption) = '')
    else
      Result := False;
  end;

begin
  with Message do
  begin
    Result := MNC_IGNORE; { No item found: beep }
    First := -1;
    Hilite := -1;
    Next := -1;
    C := GetMenuItemCount(Menu);
    for I := 0 to C - 1 do
    begin
      State := GetMenuState(Menu, I, MF_BYPOSITION);
      if IsAccelChar(Menu, State, I, User) then
      begin
        if State and MF_DISABLED <> 0 then
        begin
          { Close the menu if this is the only disabled item to choose from.
            Otherwise, ignore the item. }
          if First < 0 then First := -2;
          Continue;
        end;
        if First < 0 then
        begin
          First := I;
          Result := MNC_EXECUTE;
        end
        else
          Result := MNC_SELECT;
        if State and MF_HILITE <> 0 then
          Hilite := I
        else if Hilite >= 0 then
          Next := I;
      end;
    end;
    { We found a single disabled item. End the selection. }
    if First < -1 then
    begin
      Result := MNC_CLOSE shl 16;
      Exit;
    end;

    { If we can't find accelerators, then look for initial letters }
    if First < 0 then
    for I := 0 to C - 1 do
      begin
        State := GetMenuState(Menu, I, MF_BYPOSITION);
        if IsInitialChar(Menu, State, I, User) then
        begin
          if State and MF_DISABLED <> 0 then
          begin
            Result := MNC_CLOSE shl 16;
            Exit;
          end;
          if First < 0 then
          begin
            First := I;
            Result := MNC_EXECUTE;
          end
          else
            Result := MNC_SELECT;
          if State and MF_HILITE <> 0 then
            Hilite := I
          else if Hilite >= 0 then
            Next := I;
        end;
      end;

    if (Result = MNC_EXECUTE) then
      Result := Result shl 16 or First
    else if Result = MNC_SELECT then
    begin
      if Next < 0 then
        Next := First;
      Result := Result shl 16 or Next;
    end;
  end;
end;

procedure TElPopupMenu.SetDrawStyle(Value: TDrawStyle);
begin
  if FDrawStyle <> Value then
  begin
    FDrawStyle := Value;
    OwnerDraw := true;
    UpdateItems;
  end;
end;

function TElPopupMenu.DispatchCommand(ACommand: Word): Boolean;
var
  Item: TElMenuItem;
begin
  Result := False;
  Item := FindItem(ACommand, fkCommand);
  if Item <> nil then
  begin
    Item.Click;
    if Item.Caption <> cLineCaption then
      Result := True;
  end;
end;

procedure TElPopupMenu.OnBeforeHook(Sender: TObject; var Message: TMessage;
  var Handled: boolean);
var
  SaveIndex: Integer;
  MenuItem: TMenuItem;
  Canvas: TCanvas;
  DC: HDC;
begin
  Handled := false;
  with Message do
    case Msg of
    WM_MENUCHAR:
    begin
      ProcessMenuChar(TWMMenuChar(Message));
      if TWMMenuChar(Message).Result <> MNC_IGNORE then
        Handled := true;
    end;
    WM_INITMENUPOPUP:
      with TWMInitMenuPopup(Message) do
        if DispatchPopup(MenuPopup) then Exit;
    WM_COMMAND:
      DispatchCommand(WParam);
    {$ifndef VCL_4_USED}
    WM_KEYDOWN:
      IsShortCut(TWMKey(Message));
    {$endif}
    WM_DRAWITEM:
      with PDrawItemStruct(LParam)^ do
        if (CtlType = ODT_MENU) then
        begin
          MenuItem := FindItem(itemID, fkCommand);
          if MenuItem <> nil then
          begin
            Canvas := TControlCanvas.Create;
            with Canvas do
            try
              SaveIndex := SaveDC(hDC);
              try
                Handle := hDC;
                if Assigned(Self.Font) then
                  Canvas.Font.Assign(Self.Font)
                else
                  Canvas.Font.Handle := GetMenuFont;
                case DrawStyle of
                tdsNormal:
                begin
                  if itemState and ODS_SELECTED <> 0 then
                  begin
                    Brush.Color := clHighlight;
                    Font.Color := clHighlightText
                  end
                  else
                  begin
                    Brush.Color := clMenu;
                    Font.Color := clMenuText;
                  end;
                  if itemState and ODS_HOTLIGHT <> 0 then
                  begin
                    Brush.Color := clHighlight;
                    Font.Color := clHighlightText
                  end;
                end;
                tdsOfficeXP:
                begin
                  if itemState and ODS_SELECTED <> 0 then
                  begin
                    Brush.Color := BrightColor(clHighlight, 70);
                    Font.Color := clMenuText;
                    Pen.Color := clHighlight;
                  end
                  else
                  begin
                    Brush.Color := BrightColor(clBtnFace, 80);
                    Pen.Color := BrightColor(clBtnFace, 80);
                    Font.Color := clMenuText;
                  end;
                end;
                tdsWindowsXP:
                begin
                  if itemState and ODS_SELECTED <> 0 then
                  begin
                    Brush.Color := clHighlight;
                    Font.Color := clHighlightText
                  end
                  else
                  begin
                    Brush.Color := clWindow;
                    Font.Color := clMenuText;
                  end;
                  if itemState and ODS_HOTLIGHT <> 0 then
                  begin
                    Brush.Color := clHighlight;
                    Font.Color := clHighlightText
                  end;
                end;
                end;
                TElMenuItem(MenuItem).DrawItem(Canvas, rcItem, (itemState and ODS_SELECTED <> 0));
              finally
                Handle := 0;
                RestoreDC(hDC, SaveIndex)
              end;
            finally
              Free;
            end;
            Handled := true;
            Exit;
          end;
          Handled := false;
        end;
    WM_MEASUREITEM:
    begin
      with PMeasureItemStruct(LParam)^ do
        if (CtlType = ODT_MENU) then
        begin
          MenuItem := FindItem(itemID, fkCommand);
          if MenuItem <> nil then
          begin
            DC := GetWindowDC(TWinControl(Owner).Handle);
            try
              Canvas := TControlCanvas.Create;
              with Canvas do
              try
                SaveIndex := SaveDC(DC);
                try
                  Handle := DC;
                  if Assigned(Self.Font) then
                    Canvas.Font.Assign(Self.Font)
                  else
                    Canvas.Font.Handle := GetMenuFont;
                  TElMenuItem(MenuItem).MeasureItem(Canvas, Integer(itemWidth),
                                       Integer(itemHeight));
                finally
                  Handle := 0;
                  RestoreDC(DC, SaveIndex);
                end;
              finally
                Canvas.Free;
              end;
            finally
              ReleaseDC(TWinControl(Owner).Handle, DC);
            end;
            Handled := true;
            Exit;
          end;
        end;
       Handled := false;
    end
    end;
end;

procedure TElPopupMenu.Popup(X, Y: Integer);
const
  Flags: array[{$ifdef VCL_4_USED}Boolean, {$endif}TPopupAlignment] of Word =
    {$ifdef VCL_4_USED}((TPM_LEFTALIGN, TPM_RIGHTALIGN, TPM_CENTERALIGN),{$endif}
     (TPM_RIGHTALIGN, TPM_LEFTALIGN, TPM_CENTERALIGN){$ifdef VCL_4_USED}){$endif};
  Buttons: array[TTrackButton] of Word = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
var
  AFlags: Integer;
begin
  // inherited;
  FPopupPoint := Point(X, Y);
  DoPopup(Self);
  FUnicodeItems.RebuildHandle;
  {$ifdef VCL_4_USED}
  AdjustBiDiBehavior;
  {$endif}
  AFlags := Flags[{$ifdef VCL_4_USED}UseRightToLeftAlignment, {$endif}Alignment] or Buttons[TrackButton] or
    (Byte(MenuAnimation) shl 10);
  TrackPopupMenu(FUnicodeItems.Handle, AFlags, X, Y, 0 { reserved }, TWinControl(Owner).Handle, nil);
end;

procedure TElPopupMenu.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  UpdateItems;
end;

function TElPopupMenu.GetHandle: HMENU;
begin
  Result := FUnicodeItems.Handle;
end;

procedure TElPopupMenu.GetFont;
var AFont : HFont;
    LFont : TLogFont;
begin
  AFont := GetMenuFont;
  GetObject(AFont, sizeof(LFont), @LFont);
  Font.Name := LFont.lfFaceName;
  Font.Height := LFont.lfHeight;
  Font.Charset := LFont.lfCharSet;
  Font.Style := [];
  if LFont.lfHeight > 400 then
    Font.Style := Font.Style + [fsBold];
  if LFont.lfItalic <> 0 then
    Font.Style := Font.Style + [fsItalic];
  if LFont.lfUnderline <> 0 then
    Font.Style := Font.Style + [fsUnderline];
  if LFont.lfStrikeOut <> 0 then
    Font.Style := Font.Style + [fsStrikeOut];
  DeleteObject(AFont);
end;

procedure TElPopupMenu.SetSystemFont(Value: Boolean);
begin
  if FSystemFont <> Value then
  begin
    FSystemFont := Value;
    if FSystemFont then
      GetFont;
  end;
end;

procedure TElPopupMenu.FontChange(Sender : TObject);
begin
  if not FSystemFont then
    DrawMenuBar(WindowHandle);
end;

procedure TElPopupMenu.Loaded;
begin
  inherited;
  UpdateCommands;
  if SystemFont then
  begin
    GetFont;
    if Items.FHandle <> 0 then
      DestroyMenu(Items.FHandle);
    Items.FHandle := 0;
    UpdateItems;
  end;
end;

{$ifdef HAS_HTML_RENDER}
procedure TElPopupMenu.SetIsHTML(Value: Boolean);
begin
  if FIsHTML <> Value then
  begin
    FIsHTML := Value;
    if FIsHTML then
    begin
      FRender := TElHTMLRender.Create;
      FRender.OnImageNeeded := TriggerImageNeededEvent;
    end
    else
    begin
      FRender.Free;
      FRender := nil;
    end;
  end;
end;

procedure TElPopupMenu.TriggerImageNeededEvent(Sender : TObject; Src : TElFString;
  var Image: TBitmap);
begin
  Image := nil;
  if (assigned(FOnImageNeeded)) then
      FOnImageNeeded(Self, Src, Image);
end;

{$endif}

function TElPopupMenu.DispatchPopup(AHandle: HMENU): Boolean;
var
  Item: TElMenuItem;
begin
  Result := False;
  Item := FindItem(AHandle, fkHandle);
  if Item <> nil then
  begin
    {$ifdef VCL_4_USED}
    if not (csDesigning in Item.ComponentState) then Item.InitiateActions;
    {$endif}
    Item.Click;
    Result := True;
  end
  {$ifdef VCL_4_USED}
  else
  if not (csDesigning in ComponentState) and (Self is TPopupMenu) then
    Items.InitiateActions;
  {$endif}
end;

procedure TElPopupMenu.UpdateCommands;
var i : integer;
begin
  for i := 0 to Items.Count - 1 do
    with Items[i] do
      UpdateCommand;
end;


initialization
  //CommandPool := TBits.Create;
  {$ifdef ELPACK_UNICODE}
  GetMenuStringW := GetProcAddress(GetModuleHandle('USER32'), 'GetMenuStringW');
  {$endif}
  if IsWinXPUp then
    FTheme := OpenThemeData(Application.Handle, 'MENU');

finalization
  if (IsWinXPUp) and (FTheme <> 0) then
    CloseThemeData(FTheme);
  //CommandPool.Free;

end.
