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

06/27/2002

  Fixed the problem with assignment of items in non-Unicode mode

05/27/2002

  Added UseSelectedFont and SelectedFontColor properties.

04/19/2002

  Line hint fixed

03/22/2002

  The images were not updated when items list is changed. Fixed.

03/11/2002

  Lots of properties made published

03/06/2002

  Added unicode hint

*)

unit ElListBox;

interface

uses
  StdCtrls,
  Windows,
  SysUtils,
  Messages,
  Classes,
  Controls,
  Graphics,
  Consts,
  Forms,
  ElUxTheme,
  ImgList,
  RTLConsts,
  ElImgFrm,
  Menus,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElTools,
  ExtCtrls,
  ElTmSchema,
  ElHintWnd,
  ElXPThemedControl,
  {$ifdef ELPACK_UNICODE}
  ElUnicodeStrings,
  {$endif}
  ElStrUtils,
  ElVCLUtils;

type


    TDrawTextEvent = procedure (ACanvas: TCanvas; Index: integer;
      var Rect: TRect; Flags: LongInt) of object;

    TIntArray = array[0..MaxInt div sizeof(integer) -1] of integer;
    PIntArray = ^TIntArray;

    TCustomElListBox = class;

    {$ifdef ELPACK_UNICODE}
    TElListBoxStrings = class(TElWideStringList)
    {$else}
    TElListBoxStrings = class(TStringList)
    {$endif}
    private
      ListBox : TCustomElListBox;
    protected
      {$ifdef ELPACK_UNICODE}
      function Get(Index: Integer): WideString; override;
      {$else}
      function Get(Index: Integer): string; override;
      {$endif}
      function GetCount: Integer; override;
      function GetObject(Index: Integer): TObject; override;
      {$ifdef ELPACK_UNICODE}
      procedure Put(Index: Integer; const S: WideString); override;
      {$else}
      procedure Put(Index: Integer; const S: String); override;
      {$endif}
      procedure PutObject(Index: Integer; AObject: TObject); override;
      procedure SetUpdateState(Updating: Boolean); override;
      procedure ResetBox;
      {$ifndef ELPACK_UNICODE}
      {$ifdef VCL_6_USED}
      procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
      {$endif}
      {$endif}
    public
      {$ifdef ELPACK_UNICODE}
      function Add(const S: WideString): Integer; override;
      {$else}
      function Add(const S: String): Integer; override;
      {$endif}
      procedure Clear; override;
      procedure Delete(Index: Integer); override;
      procedure Exchange(Index1, Index2: Integer); override;
      {$ifdef ELPACK_UNICODE}
      function IndexOf(const S: WideString): Integer; override;
      procedure Insert(Index: Integer; const S: WideString); override;
      {$else}
      function IndexOf(const S: String): Integer; override;
      procedure Insert(Index: Integer; const S: String); override;
      {$endif}
      procedure Move(CurIndex, NewIndex: Integer); override;
    end;

    TCustomElListBox = class(TWinControl)
    protected
      FImageIndex : PIntArray;
      FImagesSize : integer;
      FStates : PChar;
      FStatesSize : integer;
      FBorderStyle: TBorderStyle;
      FCanvas     : TCanvas;
      FColumns: Integer;
      FExtendedSelect: Boolean;
      FIntegralHeight: Boolean;
      FItemHeight: Integer;
      FMultiSelect: Boolean;
      FSorted: Boolean;
      FTabWidth: Integer;
      FCurHintItem : Integer;
      FLastTopIndex : integer;
      FImgFormChLink : TImgFormChangeLink;
      FListBoxStrings : TElFStrings;
      FActiveBorderType: TElFlatBorderType;
      FBackground: TBitmap;
      FBorderSides: TElBorderSides;
      FFlat: Boolean;
      FFlatFocusedScrollBars: Boolean;
      FHorizontalScroll: Boolean;
      FInactiveBorderType: TElFlatBorderType;
      FInvertSelection: Boolean;
      FSelectedColor: TColor;
      FSelectedFont: TFont;
      FShowLineHint: Boolean;
      FTheme: HTheme;
      FTransparent: Boolean;
      FTransparentSelection: Boolean;
      FUseBackground: Boolean;
      FImgForm : TElImageForm;
      FMouseOver: boolean;
      FHintTimer: TTimer;
      FHintWnd     : TElHintWindow;
      FHintWndProc : TWndMethod;
      FMaxWidth    : integer;
      FInVScroll,
      FInHScroll     : boolean;
      FLineBorderActiveColor: TColor;
      FLineBorderInactiveColor: TColor;
      FUseXPThemes: Boolean;
      FMoving : boolean;
      FShowCheckBox: Boolean;
      FAllowGrayed: Boolean;
      FImages: TImageList;
      FImageChangeLink : TChangeLink;
      {$ifndef ELPACK_UNICODE}
      FSaveItems: TStringList;
      {$endif}
      FSaveTopIndex: Integer;
      FSaveItemIndex: Integer;
      {$ifdef ELPACK_UNICODE}
      FHint: WideString;
      {$endif}
      FOnDrawText: TDrawTextEvent;
      FStyle: TListBoxStyle;
      FOnDrawItem: TDrawItemEvent;
      FOnMeasureItem: TMeasureItemEvent;
      FUseSelectedFont: Boolean;
      FSelectedFontColor: TColor;
      procedure SetStyle(Value: TListBoxStyle);
      
      procedure SetActiveBorderType(const Value: TElFlatBorderType);
      procedure SetBackground(const Value: TBitmap);
      procedure SetBorderSides(Value: TElBorderSides);
      procedure SetFlat(const Value: boolean);
      procedure SetFlatFocusedScrollBars(const Value: boolean);
      procedure SetHorizontalScroll(Value: Boolean);
      procedure SetImageForm(newValue : TElImageForm);
      procedure SetInactiveBorderType(const Value: TElFlatBorderType);
      procedure SetInvertSelection(const Value: boolean);
      procedure SetSelectedColor(const Value: TColor);
      procedure SetSelectedFont(const Value: TFont);
      procedure SetTransparent(const Value: boolean);
      procedure SetTransparentSelection(Value: Boolean);
      procedure SetUseBackground(const Value: boolean);
      procedure BackgroundChanged(Sender: TObject);
      procedure CancelLineHint;
      procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
      procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
      procedure CMParentColorChanged(var Msg: TMessage); message
          CM_PARENTCOLORCHANGED;
      procedure CMParentFontChanged(var Msg: TMessage); message CM_PARENTFONTCHANGED;
      procedure DrawBackground(DC: HDC; R: TRect);
      procedure DrawBackgroundEx(DC: HDC; R, SubR: TRect);
      procedure DrawFlatBorder(DC: HDC; HDragging, VDragging : boolean);
      procedure DrawParentControl(DC: HDC);
      procedure DrawParentControlEx(DC: HDC; R: TRect);
      procedure HintWndProc(var Message: TMessage);
      procedure ImageFormChange(Sender : TObject);
      procedure IntMouseMove(XPos, YPos : SmallInt);
      procedure LBGetTopIndex(var Msg: TMessage); message LB_GETTOPINDEX;
      procedure OnLineHintTimer(Sender : TObject);
      procedure ResetHorizontalExtent; virtual;
      procedure SelectedFontChanged(Sender: TObject);
      procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
      procedure WMHScroll(var Message: TMessage); message WM_HSCROLL;
      procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
      procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
      procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
      procedure WMNCMouseMove(var Message: TMessage); message WM_NCMOUSEMOVE;
      procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
      procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
      procedure WMVScroll(var Message: TMessage); message WM_VSCROLL;
      procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
          WM_WINDOWPOSCHANGED;
      procedure ResetHorizontalExtent1;
      procedure SetHorizontalExtent; virtual;
      procedure SetColumnWidth;
      procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
      procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
      procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
      procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
      procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
      procedure WMSize(var Message: TWMSize); message WM_SIZE;
      function GetItemHeight: Integer;
      function GetItemIndex: Integer;
      function GetSelCount: Integer;
      function GetSelected(Index: Integer): Boolean;
      function GetTopIndex: Integer;
      procedure SetBorderStyle(Value: TBorderStyle);
      procedure SetColumns(Value: Integer);
      procedure SetExtendedSelect(Value: Boolean);
      procedure SetIntegralHeight(Value: Boolean);
      procedure SetItemHeight(Value: Integer);
      procedure SetItemIndex(Value: Integer);
      procedure SetItems(Value: TElFStrings);
      procedure SetMultiSelect(Value: Boolean);
      procedure SetSelected(Index: Integer; Value: Boolean);
      procedure SetSorted(Value: Boolean);
      procedure SetTabWidth(Value: Integer);
      procedure SetTopIndex(Value: Integer);

      function GetBackground: TBitmap; virtual;
      procedure SetLineBorderActiveColor(Value: TColor);
      procedure SetLineBorderInactiveColor(Value: TColor);
      procedure SetUseXPThemes(Value: Boolean);
      function CreateHintWindow: TElHintWindow; virtual;
      procedure CreateParams(var Params: TCreateParams); override;
      procedure CreateThemeHandle; virtual;
      procedure CreateWnd; override;
      procedure DestroyWnd; override;
      {$ifndef VCL_5_USED}
      procedure DrawItem(Index: Integer; R: TRect; State: TOwnerDrawState); virtual;
      {$else}
      procedure DrawItem(Index: Integer; R: TRect; State: Windows.TOwnerDrawState); virtual;
      {$endif}
      procedure FreeThemeHandle; virtual;
      function GetItemWidth(Index: Integer): Integer; virtual;
      function GetParentCtlHeight: Integer; virtual;
      function GetParentCtlWidth: Integer; virtual;
      function GetThemedClassName: WideString;
      procedure IFMRepaintChildren(var Message: TMessage); message
          IFM_REPAINTCHILDREN;
      function IsThemeApplied: Boolean;
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      function RealScreenToClient(APoint : TPoint): TPoint; virtual;
      procedure WMNCPaint(var Msg : TMessage); message WM_NCPAINT;
      procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
      procedure WndProc(var Message: TMessage); override;
      function InternalGetItemData(Index: Integer): LongInt; dynamic;
      procedure InternalSetItemData(Index: Integer; AData: Longint); dynamic;
      function GetItemData(Index: Integer): LongInt; dynamic;
      procedure SetItemData(Index: Integer; AData: LongInt); dynamic;
      procedure ResetContent; dynamic;
      procedure DeleteString(Index: Integer); dynamic;
      procedure SetShowCheckBox(Value: Boolean);
      function GetState(Index: Integer): TCheckBoxState;
      procedure SetState(Index: Integer; Value: TCheckBoxState);
      function GetCheckBoxSize: TSize;
      procedure SetAllowGrayed(Value: Boolean);
      procedure DrawFlatFrame(Canvas : TCanvas; R : TRect);
      procedure OnImageListChange(Sender : TObject);
      procedure SetImages(newValue : TImageList);
      procedure AdjustItemHeight; virtual;
      function GetImageIndex(Index: Integer): Integer;
      procedure SetImageIndex(Index: Integer; Value: Integer);
      procedure SetStatesSize(aSize : integer);
      procedure SetImagesSize(aSize : integer);

      {$ifdef ELPACK_UNICODE}
      {$ifndef CLX_USED}
      procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
      {$else}
      function HintShow(var HintInfo : THintInfo): Boolean; override;
      {$endif}
      {$endif}

      {$ifdef ELPACK_UNICODE}
      procedure SetHint(Value: WideString);
      {$endif}
      procedure ItemsChange(Sender : TObject);

      procedure DoDrawText(ACanvas: TCanvas; const ACaption: TElFString;{index: integer;}
        var Rect: TRect; Flags: LongInt); virtual;

      procedure MeasureItem(Index: Integer; var Height: Integer); virtual;
      procedure SetUseSelectedFont(Value: Boolean);
      procedure SetSelectedFontColor(Value: TColor);

      property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
          default bsSingle;
      property Columns: Integer read FColumns write SetColumns default 0;
      property ExtendedSelect: Boolean read FExtendedSelect write SetExtendedSelect
          default True;
      property IntegralHeight: Boolean read FIntegralHeight write SetIntegralHeight
          default False;
      property ItemHeight: Integer read GetItemHeight write SetItemHeight;
      property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default
          False;
      property ParentColor default False;
      property Sorted: Boolean read FSorted write SetSorted default False;
      property TabWidth: Integer read FTabWidth write SetTabWidth default 0;
      property ItemIndex: Integer read GetItemIndex write SetItemIndex;
      {$ifdef ELPACK_UNICODE}
      property Items: TElWideStrings read FListBoxStrings write SetItems;
      {$else}
      property Items: TStrings read FListBoxStrings write SetItems;
      {$endif}
      property SelCount: Integer read GetSelCount;
      property TopIndex: Integer read GetTopIndex write SetTopIndex;

      property ActiveBorderType: TElFlatBorderType read FActiveBorderType write
          SetActiveBorderType default fbtSunken;
      property Background: TBitmap read GetBackground write SetBackground;
      property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
      property Flat: Boolean read FFlat write SetFlat default False;
      property FlatFocusedScrollBars: Boolean read FFlatFocusedScrollBars write
          SetFlatFocusedScrollBars default False;
      property HorizontalScroll: Boolean read FHorizontalScroll write
          SetHorizontalScroll;
      property ImageForm: TElImageForm read FImgForm write SetImageForm;
      property InactiveBorderType: TElFlatBorderType read FInactiveBorderType write
          SetInactiveBorderType default fbtSunkenOuter;
      property InvertSelection: Boolean read FInvertSelection write
          SetInvertSelection default False;
      property LineBorderActiveColor: TColor read FLineBorderActiveColor write
          SetLineBorderActiveColor;
      property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write
          SetLineBorderInactiveColor;
      property SelectedColor: TColor read FSelectedColor write SetSelectedColor
          default clHighlight;
      property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
      property ShowLineHint: Boolean read FShowLineHint write FShowLineHint default
          false;
      property Transparent: Boolean read FTransparent write SetTransparent default
          False;
      property TransparentSelection: Boolean read FTransparentSelection write
          SetTransparentSelection default false;
      property UseBackground: Boolean read FUseBackground write SetUseBackground
          default False;
      property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default
          true;
      property TabStop default True;
      property ShowCheckBox: Boolean read FShowCheckBox write SetShowCheckBox default
          false;
      property AllowGrayed: Boolean read FAllowGrayed write SetAllowGrayed default
          true;
      property Images: TImageList read FImages write SetImages;
      property Style: TListBoxStyle read FStyle write SetStyle default lbStandard;
      property UseSelectedFont: Boolean read FUseSelectedFont write 
          SetUseSelectedFont default false;
      property SelectedFontColor: TColor read FSelectedFontColor write 
          SetSelectedFontColor default clHighlightText;
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
      function ItemRect(Index: Integer): TRect;
      property Theme: HTheme read FTheme;
      property Canvas: TCanvas read FCanvas;
      property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
      property State[Index: Integer]: TCheckBoxState read GetState write SetState;
      property ImageIndex[Index: Integer]: Integer read GetImageIndex write
          SetImageIndex;
      property OnDrawText: TDrawTextEvent read FOnDrawText write
          FOnDrawText;
      property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
      property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    published
      {$ifdef ELPACK_UNICODE}
      property Hint: WideString read FHint write SetHint;
      {$endif}
    end;

    TElListBox = class(TCustomElListBox)

      property AllowGrayed;
      property BorderStyle;
      property Columns;
      property ExtendedSelect;
      property IntegralHeight;
      property ItemHeight;
      property MultiSelect;
      property ParentColor;
      property Sorted;
      property TabWidth;
      property ItemIndex;
      property Items;
      property SelCount;
      property TopIndex;
      property ShowCheckBox;

      property ActiveBorderType;
      property Background;
      property BorderSides;
      property Flat;
      property Ctl3D;
      property ParentCtl3D;
      property Font;
      property FlatFocusedScrollBars;
      property HorizontalScroll;
      property Images;
      property ImageForm;
      property InactiveBorderType;
      property InvertSelection;
      property LineBorderActiveColor;
      property LineBorderInactiveColor;
      property SelectedColor;
      property SelectedFont;
      property ShowLineHint;
      property Transparent;
      property TransparentSelection;
      property UseBackground;
      property UseSelectedFont;
      property UseXPThemes;
      property TabStop;
      property ParentFont;

      property OnClick;
      {$ifdef VCL_5_USED}
      property OnContextPopup;
      {$endif}
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      {$ifdef VCL_4_USED}
      property OnEndDock;
      {$endif}
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      {$ifdef VCL_4_USED}
      property OnStartDock;
      {$endif}
      property OnStartDrag;


      property Align;
      {$ifdef VCL_4_USED}
      property Anchors;
      property BiDiMode;
      {$endif}
      property Color;
      {$ifdef VCL_4_USED}
      property Constraints;
      {$endif}
      property DragCursor;
      {$ifdef VCL_4_USED}
      property DragKind;
      {$endif}
      property DragMode;
      property Enabled;
      {$ifdef VCL_4_USED}
      property ImeMode;
      property ImeName;
      property ParentBiDiMode;
      {$endif}
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property TabOrder;
      property Visible;
      property Style;
      property OnDrawItem;
      property OnMeasureItem;
    end;

implementation

const
  CheckMargin = 5;
  {$ifdef MSWINDOWS}
  CheckBoxSize = 13;
  {$else}
  CheckBoxSize = 0;
  {$endif}

type
  THackWinControl = class(TWinControl);

  TElListBoxHintWindow = class(TElHintWindow)
  protected
  {$ifdef CLX_USED}
    procedure VisibleChanged; override;
  {$endif}
  end;

{$ifdef CLX_USED}
procedure TElListBoxHintWindow.VisibleChanged;
begin
  inherited;
  TElListBox(Owner).CancelLineHint;
end;
{$endif}

constructor TCustomElListBox.Create(AOwner : TComponent);
begin
  inherited;
  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  FListBoxStrings := TElListBoxStrings.Create;
  TElListBoxStrings(FListBoxStrings).ListBox := Self;
  TElListBoxStrings(FListBoxStrings).OnChange := ItemsChange;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FItemHeight := 16;
  FBorderStyle := bsSingle;
  FExtendedSelect := True;
  {$ifndef CLX_USED}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}
  FActiveBorderType := fbtSunken;
  FBackground := TBitmap.Create;
  FBackground.OnChange := BackgroundChanged;
  FFlat := False;
  FFlatFocusedScrollBars := False;
  FInactiveBorderType := fbtSunkenOuter;
  FInvertSelection := False;
  FLastTopIndex := 0;
  FSelectedColor := clHighlight;
  FSelectedFontColor := clHighlightText;
  FSelectedFont := TFont.Create;
  FSelectedFont.Color := clHighlightText;
  FSelectedFont.OnChange := SelectedFontChanged;
  FTransparent := False;
  {$ifndef CLX_USED}
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := OnImageListChange;
  FCurHintItem := -1;
  FUseXPThemes := true;
end;

destructor TCustomElListBox.Destroy;
begin
  inherited;
  SetStatesSize(0);
  SetImagesSize(0);
  FImageChangeLink.Free;
  {$ifndef CLX_USED}
  FImgFormChLink.Free;
  {$endif}
  FSelectedFont.Free;
  FCanvas.Free;
  FListBoxStrings.Free;
  FBackground.Free;
end;

function TCustomElListBox.GetBackground: TBitmap;
begin
  Result := FBackground;
end;

procedure TCustomElListBox.SetActiveBorderType(const Value: TElFlatBorderType);
begin
  if FActiveBorderType <> Value then
  begin
    FActiveBorderType := Value;
    {$ifndef CLX_USED}
    if Focused or FMouseOver then DrawFlatBorder(0, false, false);
    {$endif}
  end;
end;

procedure TCustomElListBox.SetBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
end;

procedure TCustomElListBox.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    {$ifndef CLX_USED}
    if HandleAllocated then
      RecreateWnd;
    {$endif}
  end;
end;

procedure TCustomElListBox.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    {$endif}
  end;
end;

procedure TCustomElListBox.SetFlatFocusedScrollBars(const Value: boolean);
begin
  if FFlatFocusedScrollBars <> Value then
  begin
    FFlatFocusedScrollBars := Value;
    {$ifndef CLX_USED}
    if Focused then DrawFlatBorder(0, false, false);
    {$endif}
  end;
end;

procedure TCustomElListBox.SetHorizontalScroll(Value: Boolean);
begin
  if FHorizontalScroll <> Value then
  begin
    FHorizontalScroll := Value;
    {$ifndef CLX_USED}
    if HandleAllocated then
      RecreateWnd;
    {$endif}
  end;
end;

procedure TCustomElListBox.SetImageForm(newValue : TElImageForm);
begin
  if FImgForm <> newValue then
  begin
    if FImgForm <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImgForm.RemoveFreeNotification(Self);
      {$endif}
      FImgForm.UnregisterChanges(FImgFormChLink);
    end;
    if newValue <> nil then
       newValue.FreeNotification(Self);
    FImgForm := newValue;
    if FImgForm <> nil then FImgForm.RegisterChanges(FImgFormChLink);
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;

procedure TCustomElListBox.SetInactiveBorderType(const Value: TElFlatBorderType);
begin
  if FInactiveBorderType <> Value then
  begin
    FInactiveBorderType := Value;
    {$ifndef CLX_USED}
    if not Focused and not FMouseOver then DrawFlatBorder(0, false, false);
    {$endif}
  end;
end;

procedure TCustomElListBox.SetInvertSelection(const Value: boolean);
begin
  if FInvertSelection <> Value then
  begin
    FInvertSelection := Value;
    if (MultiSelect and (SelCount > 0)) or (ItemIndex > -1) then Invalidate;
  end;
end;

procedure TCustomElListBox.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then Invalidate;
  end;
end;

procedure TCustomElListBox.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then Invalidate;
  end;
end;

procedure TCustomElListBox.SetSelectedColor(const Value: TColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    if (MultiSelect and (SelCount > 0)) or (ItemIndex > -1) then Invalidate;
  end;
end;

procedure TCustomElListBox.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
end;

procedure TCustomElListBox.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    Perform(CM_COLORCHANGED, 0, 0);
    {$endif}
  end;
end;

procedure TCustomElListBox.SetTransparentSelection(Value: Boolean);
begin
  if FTransparentSelection <> Value then
  begin
    FTransparentSelection := Value;
    if ItemIndex > -1 then Invalidate;
  end;
end;

procedure TCustomElListBox.SetUseBackground(const Value: boolean);
begin
  if FUseBackground <> Value then
  begin
    FUseBackground := Value;
    Invalidate;
  end;
end;

procedure TCustomElListBox.SetUseXPThemes(Value: Boolean);
begin
  if FUseXPThemes <> Value then
  begin
    FUseXPThemes := Value;
    {$ifdef MSWINDOWS}
    {$ifndef CLX_USED}
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$else}
    RedrawWindow(QWidget_winID(Handle), nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
    {$endif}
    {$endif}
  end;
end;

procedure TCustomElListBox.BackgroundChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomElListBox.CancelLineHint;
begin
  if FHintTimer <> nil then
    FHintTimer.Enabled := false;
  if FHintWnd <> nil then
  begin
    FHintWnd.Free;
    FHintWnd := nil;
  end;
end;

procedure TCustomElListBox.CMFontChanged(var Msg: TMessage);
var tm : TTextMetric;
begin
  inherited;
  Canvas.Font.Assign(Font);
  GetTextMetrics(Canvas.Handle, tm);
  Perform(LB_SETITEMHEIGHT, 0, tm.tmHeight);
end;

procedure TCustomElListBox.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FMouseOver := True;
  if IsThemeApplied or (Flat and not Focused) then DrawFlatBorder(0, false, false);
  if Flat and ShowCheckBox then Invalidate;
end;

procedure TCustomElListBox.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FMouseOver := False;
  if IsThemeApplied or (Flat and not Focused) then DrawFlatBorder(0, false, false);
  if Flat and ShowCheckBox then Invalidate;
end;

procedure TCustomElListBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if FTransparent then Invalidate;
end;

procedure TCustomElListBox.CMParentFontChanged(var Msg: TMessage);
begin
  inherited;
  if ParentFont then
  begin
    FSelectedFont.OnChange := nil;
    try
      if Msg.WParam <> 0 then
        FSelectedFont.Assign(TFont(Msg.LParam))
      else
        FSelectedFont.Assign(Font);
      FSelectedFont.Color := clHighlightText;
    finally
      FSelectedFont.OnChange := SelectedFontChanged;
    end;
  end;
end;

function TCustomElListBox.CreateHintWindow: TElHintWindow;
begin
  Result := TElListBoxHintWindow.Create(nil);
end;

procedure TCustomElListBox.CreateParams(var Params: TCreateParams);
type
  PSelects = ^TSelects;
  TSelects = array[Boolean] of DWORD;
const
  MultiSelects: array[Boolean] of DWORD = (0, LBS_MULTIPLESEL);
  ExtendSelects: array[Boolean] of DWORD = (0, LBS_EXTENDEDSEL);
  IntegralHeights: array[Boolean] of DWORD = (LBS_NOINTEGRALHEIGHT, 0);
  MultiColumns: array[Boolean] of DWORD = (0, LBS_MULTICOLUMN);
  TabStops: array[Boolean] of DWORD = (0, LBS_USETABSTOPS);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  CSHREDRAW: array[Boolean] of DWORD = (CS_HREDRAW, 0);

var
  Selects: PSelects;
begin
  inherited;
  CreateSubClass(Params, 'LISTBOX');
  with Params do
  begin
    Selects := @MultiSelects;
    if FExtendedSelect then Selects := @ExtendSelects;
    Style := Style or (WS_HSCROLL or WS_VSCROLL or LBS_NOTIFY ) or
      {$ifndef ELACK_UNICODE} LBS_HASSTRINGS or {$endif}
      Selects^[FMultiSelect] or IntegralHeights[FIntegralHeight] or
      MultiColumns[FColumns <> 0] or BorderStyles[FBorderStyle] or TabStops[FTabWidth <> 0];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    if FStyle = lbOwnerDrawVariable then
      Style := Style or LBS_OWNERDRAWVARIABLE
    else
      Style := Style or LBS_OWNERDRAWFIXED;
    WindowClass.style := WindowClass.style and not (CSHREDRAW[{$ifdef VCL_4_USED}UseRightToLeftAlignment{$else}false{$endif}] or CS_VREDRAW);
  end;

  if not (csLoading in ComponentState) then
    if (Params.Style and LBS_OWNERDRAWFIXED = 0) and
      (Params.Style and LBS_OWNERDRAWVARIABLE = 0) then
      Params.Style := Params.Style or LBS_OWNERDRAWFIXED;
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    Params.Style := Params.Style and (not WS_BORDER);
    Params.ExStyle := Params.ExStyle and (not WS_EX_CLIENTEDGE);
  end;
end;

procedure TCustomElListBox.CreateThemeHandle;
begin
  if (ThemesAvailable and IsThemeActive) then
    {$ifndef CLX_USED}
    FTheme := OpenThemeData(Handle, PWideChar(GetThemedClassName()))
    {$else}
    {$ifdef MSWINDOWS}
    FTheme := OpenThemeData(QWidget_winID(Handle), PWideChar(GetThemedClassName()))
    {$endif}
    {$endif}
  else
    FTheme := 0;
end;

procedure TCustomElListBox.CreateWnd;
var
  W, H: Integer;
begin
  W := Width;
  H := Height;
  inherited;
  {$ifndef CLX_USED}
  Perform(CM_FONTCHANGED, 0, 0);
  {$endif}
  if UseXPThemes and not IsThemeApplied then
  begin
    {$ifdef MSWINDOWS}
    CreateThemeHandle;
    {$endif}
  end;
  SetWindowPos(Handle, 0, Left, Top, W, H, SWP_NOZORDER or SWP_NOACTIVATE);
  if FTabWidth <> 0 then
    SendMessage(Handle, LB_SETTABSTOPS, 1, Longint(@FTabWidth));
  SetColumnWidth;
  {$ifndef ELPACK_UNICODE}
  if FSaveItems <> nil then
  begin
    FListBoxStrings.Assign(FSaveItems);
    SetTopIndex(FSaveTopIndex);
    SetItemIndex(FSaveItemIndex);
    FSaveItems.Free;
    FSaveItems := nil;
  end;
  {$else}
  TElListBoxStrings(FListBoxStrings).ResetBox;
  {$endif}
end;

procedure TCustomElListBox.DestroyWnd;
begin
  {$ifndef ELPACK_UNICODE}
  if FListBoxStrings.Count > 0 then
  begin
    FSaveItems := TStringList.Create;
    FSaveItems.Assign(FListBoxStrings);
    FSaveTopIndex := GetTopIndex;
    FSaveItemIndex := GetItemIndex;
  end;
  {$endif}
  if UseXPThemes and IsThemeApplied then
    FreeThemeHandle;
  inherited;
end;

procedure TCustomElListBox.DrawBackground(DC: HDC; R: TRect);
var
  X, Y: integer;
begin
  if FUseBackground and not Background.Empty then
  begin
    X := R.Left; Y := R.Top;
    while Y < R.Bottom do
    begin
      while X < R.Right do
      begin
        BitBlt(DC, X, Y, R.Right - X, R.Bottom - Y,
          Background.Canvas.Handle, 0, 0, SRCCOPY);
        Inc(X, Background.Width);
      end;
      X := R.Left;
      Inc(Y, Background.Height);
    end;
  end;
end;

procedure TCustomElListBox.DrawBackgroundEx(DC: HDC; R, SubR: TRect);
var
  Rgn: HRGN;
  SavedDC: integer;
begin
  SavedDC := SaveDC(DC);
  try
    Rgn := CreateRectRgnIndirect(SubR);
    try
      SelectClipRgn(DC, Rgn);
      DrawBackground(DC, R);
    finally
      DeleteObject(Rgn);
    end;
  finally
    RestoreDC(DC, SavedDC);
  end;
end;

procedure TCustomElListBox.DrawFlatBorder(DC: HDC; HDragging, VDragging : boolean);
var
  R : TRect;
  BS: TElFlatBorderType;
  MustRelease: boolean;
  AColor : TColor;

const BordersFlat : array[boolean] of Integer = (0, WS_BORDER);
      Borders3D : array[boolean] of Integer = (0, WS_EX_CLIENTEDGE);

begin
  if (BorderStyle = bsNone) or (not HandleAllocated) then exit;
  if IsThemeApplied then
  begin
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_UPDATENOW);
    exit;
  end;
  
  MustRelease := (DC = 0);
  if MustRelease then DC := GetWindowDC(Handle);
  R := Rect(0, 0, Width, Height);
  try
    if (BorderStyle = bsSingle) then
    begin
      if Focused or FMouseOver then
        BS := FActiveBorderType
      else
        BS := FInactiveBorderType;
      if Focused or FMouseOver then
        AColor := LineBorderActiveColor
      else
        AColor := LineBorderInactiveColor;
      DrawFlatFrameEx2(DC, R, AColor, Color, Focused or FMouseOver, Enabled, FBorderSides, BS);
    end;
    if FFlatFocusedScrollBars or not (Focused or FMouseOver) then
      DrawFlatScrollbars(Handle, DC, R,
        (Focused or FMouseOver) and not FFlatFocusedScrollBars,
        ssBoth, HDragging, VDragging, False,
        GetWindowLong(Handle, GWL_STYLE) or BordersFlat[(not Ctl3D) and (BorderStyle = bsSingle)],
        GetWindowLong(Handle, GWL_EXSTYLE) or Borders3D[Ctl3D and (BorderStyle = bsSingle)]);
  finally
    if MustRelease then ReleaseDC(Handle, DC);
  end;
end;

procedure TCustomElListBox.DoDrawText(ACanvas: TCanvas; const ACaption: TElFString;
  {index: integer;} var Rect: TRect; Flags: LongInt);
begin
  {$ifndef CLX_USED}
  SetBkMode(ACanvas.Handle, Windows.TRANSPARENT);
  {$ifdef ELPACK_UNICODE}
  DrawTextW(ACanvas.Handle, PWideChar(ACaption), Length(ACaption), Rect, Flags);
  {$else}
  DrawText(ACanvas.Handle, PChar(ACaption), Length(ACaption), Rect, Flags);
  {$endif}
  {$else}
  ACanvas.TextRect(Rect, Rect.Left, Rect.Top, ACaption, Flags);
  {$endif}
end;

procedure TCustomElListBox.SetStyle(Value: TListBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomElListBox.DrawItem(Index: Integer; R: TRect; State:
{$ifndef VCL_5_USED}
    TOwnerDrawState);
{$else}
    Windows.TOwnerDrawState);
{$endif}
var
  Flags, TextColor, BackColor: longint;
  TextRect: TRect;
  R1,
  BgRect  : TRect;
  P       : TPoint;
  ax, ay  : integer;
  S       : TSize;
  sid,
  pid     : integer;
  i       : integer;
begin
  if (FStyle <> lbStandard)and Assigned(FOnDrawItem) then
  begin
    FOnDrawItem(Self, Index, R, State);
    exit;
  end;
  {$ifndef CLX_USED}
  TextColor := GetTextColor(Canvas.Handle);
  BackColor := GetBkColor(Canvas.Handle);
  {$else}
  TextColor := Canvas.Font.Color;
  BackColor := Canvas.Brush.Color;
  {$endif}

  if (odSelected in State) and (not FInvertSelection) and FTransparentSelection then
    Canvas.Font.Color := Font.Color;

  if (odSelected in State) and (not FInvertSelection) and (not FTransparentSelection) then
  begin
    Canvas.Brush.Color := FSelectedColor;
    Canvas.FillRect(R);
  end
  else
  {$ifndef CLX_USED}
  if (FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate)) then
  begin
    if (FImgForm.Control <> Self) then
    begin
      R1 := R;
      BgRect := R;
      BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
      P := ClientToScreen(ClientRect.TopLeft);
      ax := BgRect.Left - P.x;
      ay := BgRect.Top - P.y;

      BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);

      BgRect.TopLeft := RealScreenToClient(BgRect.TopLeft);
      BgRect.BottomRight := RealScreenToClient(BgRect.BottomRight);
      FImgForm.PaintBkgnd(Canvas.Handle, R1, Point(BgRect.Left - ax, BgRect.Top - ay), false);
    end;
    if (BgRect.Bottom > GetParentCtlHeight) or
       (BgRect.Right  > GetParentCtlWidth) or
       (BgRect.Left < 0) or (BgRect.Top < 0) then
      Canvas.FillRect(R);
  end
  else
  if FTransparent then
    DrawParentControlEx(Canvas.Handle, R)
  else
  if FUseBackground and not Background.Empty then
    DrawBackgroundEx(Canvas.Handle, ClientRect, R)
  else
  {$endif}
  if not FTransparentSelection then
    Canvas.FillRect(R);
  {$ifndef CLX_USED}
  if (odSelected in State) and FInvertSelection then
    InvertRect(Canvas.Handle, R);
  {$endif}

  if (odSelected in State) then
  begin
    if (not FTransparentSelection) and (UseSelectedFont) then
      Canvas.Font.Assign(FSelectedFont)
    else
      Canvas.Font.Color := FSelectedFontColor;
  end;

  if Index < Items.Count then
  begin
    if Images <> nil then
    begin
      i := ImageIndex[Index];
      if i <> -1 then
      begin
        FImages.Draw(Canvas, R.Left + 1, R.Top + ((R.Bottom - R.Top) - Images.Height) div 2, i);
      end;
      inc(R.Left, Images.Width + 3);
    end;
    if ShowCheckBox then
    begin
      R1 := R;
      s := GetCheckBoxSize;
      inc(R1.Left, 2);
      R1.Right := R1.Left + S.cx;
      R1.Top := R1.Top + (R1.Bottom - R1.Top - S.cy) div 2;
      R1.Bottom := R1.Top + S.cy;
      if IsThemeApplied then
      begin
        pid := BP_CHECKBOX;
        sid := 0;
        case GetState(Index) of
          cbUnchecked :
            if not Enabled then
              sid := CBS_UNCHECKEDDISABLED
            else
              sid := CBS_UNCHECKEDNORMAL;

          cbChecked :
            if not Enabled then
              sid := CBS_CHECKEDDISABLED
            else
              sid := CBS_CHECKEDNORMAL;

          cbGrayed :
            if not Enabled then
              sid := CBS_MIXEDDISABLED
            else
              sid := CBS_MIXEDNORMAL;
        end;
        DrawThemeBackground(Theme, Canvas.Handle, pid, sid, R1, @R);
      end
      else
      begin
        i := 0;
        case GetState(Index) of
          cbChecked : i := DFCS_BUTTONCHECK or DFCS_CHECKED;
          cbUnchecked : i := DFCS_BUTTONCHECK;
          cbGrayed : i := DFCS_BUTTON3STATE or DFCS_CHECKED;
        end; // case
        DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, i);
        if Flat then
          DrawFlatFrame(Canvas, R1);
      end;
      R.Left := R1.Right + 1;
    end;
    TextRect := R;
    {$ifndef CLX_USED}
    Flags := DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX;
    {$else}
    Flags := Integer(AlignmentFlags_SingleLine) or
             Integer(AlignmentFlags_AlignVCenter) or
             Integer(AlignmentFlags_AlignLeft);
    {$endif}
    {$IFDEF VCL_4_USED}
    {$ifndef CLX_USED}
    Flags := DrawTextBiDiModeFlags(Flags);
    if not UseRightToLeftAlignment then
      Inc(TextRect.Left, 2)
    else
      Dec(TextRect.Right, 2);
    {$endif}
    {$ELSE}
    Inc(TextRect.Left, 2);
    {$ENDIF}
    if Assigned(FOnDrawText) then
      FOnDrawText(Canvas, Index, TextRect, Flags)
    else
      DoDrawText(Canvas, Items[Index], TextRect, Flags);
  end;
  {$ifndef CLX_USED}
  SetTextColor(Canvas.Handle, TextColor);
  SetBkColor(Canvas.Handle, BackColor);
  {$else}
  Canvas.Pen.Color := TextColor;
  Canvas.Brush.Color := BackColor;
  {$endif}
end;

procedure TCustomElListBox.DrawParentControl(DC: HDC);
var
  SavedDC: integer;
  P: TPoint;
begin
  if Assigned(Parent) then
  begin
    SavedDC := SaveDC(DC);
    try
      P := Parent.ScreenToClient(ClientOrigin);
      MoveWindowOrg(DC, -P.X, -P.Y);
      Parent.Perform(WM_ERASEBKGND, DC, 0);
      Parent.Perform(WM_PAINT, DC, 0);
      THackWinControl(Parent).PaintControls(DC, nil);
    finally
      RestoreDC(DC, SavedDC);
    end;
  end;
end;

procedure TCustomElListBox.DrawParentControlEx(DC: HDC; R: TRect);
var
  Rgn: HRGN;
  SavedDC: integer;
begin
  SavedDC := SaveDC(DC);
  try
    Rgn := CreateRectRgnIndirect(R);
    try
      SelectClipRgn(DC, Rgn);
      DrawParentControl(DC);
    finally
      DeleteObject(Rgn);
    end;
  finally
    RestoreDC(DC, SavedDC);
  end;
end;

procedure TCustomElListBox.FreeThemeHandle;
begin
  {$ifdef MSWINDOWS}
  if ThemesAvailable then
    CloseThemeData(FTheme);
  {$endif}
  FTheme := 0;
end;

function TCustomElListBox.GetItemWidth(Index: Integer): Integer;
var
  ATabWidth: Longint;
  S: string;
begin
  S := Items[Index] + 'W';
  {$ifndef CLX_USED}
  if TabWidth > 0 then
  begin
    ATabWidth := Round((TabWidth * Canvas.TextWidth('0')) * 0.25);
    Result := LoWord(GetTabbedTextExtent(Canvas.Handle, @S[1], Length(S),
      1, ATabWidth));
  end
  else
  {$endif}
    Result := Canvas.TextWidth(S);
end;

function TCustomElListBox.GetParentCtlHeight: Integer;
var ACtl : TWinControl;
begin
{$ifndef CLX_USED}
  ACtl := FImgForm.GetRealControl;
{$else}
  ACtl := Self;
{$endif}
  result := ACtl.ClientHeight;
end;

function TCustomElListBox.GetParentCtlWidth: Integer;
var ACtl : TWinControl;
begin
{$ifndef CLX_USED}
  ACtl := FImgForm.GetRealControl;
{$else}
  ACtl := Self;
{$endif}
  result := ACtl.ClientWidth;
end;

function TCustomElListBox.GetThemedClassName: WideString;
begin
  Result := 'BUTTON';
end;

procedure TCustomElListBox.HintWndProc(var Message: TMessage);
begin
  FHintWndProc(Message);
  if (Message.Msg = WM_SHOWWINDOW) and (Message.wParam = 0) then
  begin
    FHintWnd.WindowProc := FHintWndProc;
    CancelLineHint;
  end;
end;

procedure TCustomElListBox.IFMRepaintChildren(var Message: TMessage);
var i : integer;
begin
  inherited;
  Invalidate;
  for i := 0 to ControlCount -1 do
  begin
    if Controls[i] is TWinControl then
      PostMessage(TWinControl(Controls[i]).Handle, Message.Msg, Message.wParam, Message.lParam);
  end;
end;

procedure TCustomElListBox.ImageFormChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TCustomElListBox.IntMouseMove(XPos, YPos : SmallInt);
var i : integer;
begin
  if (TopIndex <> -1) then
  begin
    i := TopIndex + (YPos div ItemHeight - 1);
    if YPos mod ItemHeight > 0 then
      inc(i);
    if i <> FCurHintItem then
    begin
      if FHintTimer <> nil then
        CancelLineHint;
      if ShowLineHint and (i < Items.Count) then
      begin
        FCurHintItem := i;
        if FHintTimer = nil then
          FHintTimer := TTimer.Create(nil);
        FHintTimer.Enabled := false;
        FHintTimer.OnTimer := OnLineHintTimer;
        FHintTimer.Interval:= Application.HintPause; 
        FHintTimer.Enabled := True;
      end
      else
        FCurHintItem := -1;
    end;
  end;
end;

function TCustomElListBox.IsThemeApplied: Boolean;
begin
  Result := UseXPThemes and (FTheme <> 0);
end;

procedure TCustomElListBox.LBGetTopIndex(var Msg: TMessage);
var
  R: TRect;
begin
  inherited;
  if (FLastTopIndex <> Msg.Result) and (FUseBackground and not Background.Empty) then
  begin
    R := ClientRect;
    InvalidateRect(Handle, @R, False);
    FLastTopIndex := Msg.Result;
  end;
end;

procedure TCustomElListBox.Loaded;
begin
  inherited;
  {$ifndef CLX_USED}
  RecreateWnd;
  {$endif}
end;

procedure TCustomElListBox.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
    {$ifndef CLX_USED}
    if AComponent = FImgForm then
    begin
      FImgForm := nil;
      Invalidate;
    end;
    {$endif}
  end;
end;

procedure TCustomElListBox.OnLineHintTimer(Sender : TObject);
var i : integer;
    P : TPoint;
    R,
    R1: TRect;
begin
  FHintTimer.Enabled := false;
  if FHintWnd = nil then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    {$ifndef CLX_USED}
    i := ItemAtPos(P, true);//SendMessage(Handle, LB_ITEMFROMPOINT, 0, MakeLParam(P.X, P.Y));
    {$else}
    i := ItemAtPos(P, true);
    {$endif}
    if (i < 0) or (i >= Items.Count) then exit;
    
    if GetItemWidth(i) < ClientWidth then
      FCurHintItem := -1
    else
    if (i <> FCurHintItem) or (i < 0) or (i >= Items.Count) then
    begin
      if InRange(0, Items.Count - 1, i) then
      begin
        FCurHintItem := i;
        FHintTimer.Enabled := true;
      end
      else
        FCurHintItem := -1;
    end
    else
    begin
      FHintWnd := CreateHintWindow;
      {$ifdef ELPACK_UNICODE}
      R := FHintWnd.CalcHintRectW(10000, Items[i], nil);
      {$else}
      R := FHintWnd.CalcHintRect(10000, Items[i], nil);
      {$endif}

      R1 := ItemRect(i);
      OffsetRect(R, R1.Left - 3, R1.Top - 3);
      R.TopLeft := ClientToScreen(R.TopLeft);
      R.BottomRight := ClientToScreen(R.BottomRight);

      {$ifdef ELPACK_UNICODE}
      FHintWnd.ActivateHintW(R, Items[i]);
      {$else}
      FHintWnd.ActivateHint(R, Items[i]);
      {$endif}
      {$ifndef CLX_USED}
      FHintWndProc := FHintWnd.WindowProc;
      FHintWnd.WindowProc := HintWndProc;
      {$endif}
      FHintTimer.Interval := Application.HintHidePause;
      FHintTimer.Enabled := true;
    end;
  end
  else
  begin
    FHintWnd.Free;
    FHintWnd := nil;
  end;
end;

function TCustomElListBox.RealScreenToClient(APoint : TPoint): TPoint;
var ACtl : TWinControl;
begin
{$ifndef CLX_USED}
  ACtl := FImgForm.GetRealControl;
{$else}
  ACtl := Self;
{$endif}
  result := ACtl.ScreenToClient(APoint);
end;

procedure TCustomElListBox.ResetHorizontalExtent;
var
  I: Integer;
begin
  FMaxWidth := 0;
  for I := 0 to Items.Count - 1 do
    FMaxWidth := Max(FMaxWidth, GetItemWidth(I));
  SetHorizontalExtent;
end;

procedure TCustomElListBox.SelectedFontChanged(Sender: TObject);
begin
  ParentFont := False;
end;

procedure TCustomElListBox.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  R1, BgRect : TRect;
begin
  if (FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate)) then
  begin
    R1 := ClientRect;
    BgRect := ClientRect;
    BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);

    BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
    BgRect.TopLeft := RealScreenToClient(BgRect.TopLeft);
    BgRect.BottomRight := RealScreenToClient(BgRect.BottomRight);

    FImgForm.PaintBkgnd(Msg.DC, R1, Point(BgRect.Left{ - ax}, BgRect.Top{ - ay}), false);
  end else
  if FTransparent then
    DrawParentControl(Msg.DC)
  else
  if FUseBackground and not Background.Empty then
    DrawBackground(Msg.DC, ClientRect)
  else inherited;
end;

procedure TCustomElListBox.WMHScroll(var Message: TMessage);
begin
  inherited;
  if TWMScroll(Message).ScrollCode = SB_THUMBTRACK then
    FInHSCroll := true
  else
  if TWMScroll(Message).ScrollCode = SB_THUMBPOSITION then
    FInHSCroll := false;

  if (Flat and (((not FMouseOver) and (not Focused)) or (FlatFocusedScrollBars))) then
    DrawFlatBorder(0, FInHScroll, FInVScroll);
end;

procedure TCustomElListBox.WMKillFocus(var Msg: TMessage);
begin
  inherited;
  if not HandleAllocated then exit;
  if Flat and not FMouseOver then DrawFlatBorder(0, false, false);
  if Flat and ShowCheckBox then Invalidate;
end;

procedure TCustomElListBox.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  if FInVScroll or FInHScroll then
  begin
    if (Flat and
       (((not FMouseOver) and
         (not Focused)) or
       (FlatFocusedScrollBars))) then
      DrawFlatBorder(0, FInHScroll, FInVScroll);
  end;
  IntMouseMove(Message.XPos, Message.YPos);
end;

procedure TCustomElListBox.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if (BorderStyle = bsSingle) and Flat and (not (ThemesAvailable and UseXPThemes)) then
  begin
    if (ebsLeft in BorderSides) then
      inc(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
    if (ebsTop in BorderSides) then
      inc(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
    if (ebsRight in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
    if (ebsBottom in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  end
  else
  if (BorderStyle = bsSingle) then
  begin
    if not (ebsLeft in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(SM_CYEDGE));
    if not (ebsTop in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(SM_CXEDGE));
    if not (ebsRight in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(SM_CYEDGE));
    if not (ebsBottom in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CXEDGE));
  end;
  // Message.Result := WVR_REDRAW;
end;

procedure TCustomElListBox.WMNCMouseMove(var Message: TMessage);
begin
  inherited;
  if FInVScroll or FInHScroll then
  begin
    if (Flat and
       (((not FMouseOver) and
         (not Focused)) or
       (FlatFocusedScrollBars)))
     then DrawFlatBorder(0, FInHScroll, FInVScroll);
  end;
end;

procedure TCustomElListBox.WMNCPaint(var Msg : TMessage);
var DC : HDC;
    RW,
    RC : TRect;
begin
  
  inherited;
  if IsThemeApplied and (BorderStyle = bsSingle) then
  begin
    DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    if DC = 0 then
    begin
      DC := GetWindowDC(Handle);
    end;

    Windows.GetClientRect(Handle, RC);
    if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
      inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
    if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
      inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
    GetWindowRect(Handle, RW);
    MapWindowPoints(0, Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    OffsetRect(RW, -RW.Left, -RW.Top);

    DrawThemeBackgroundTo('EDIT', DC, 0, 0, RW, nil);
    ReleaseDC(Handle, DC);
  end;
end;

procedure TCustomElListBox.WMPaint(var Msg: TWMPaint);
var
  DC: HDC;
  R1, BgRect : TRect;
  P          : TPoint;
  ax, ay     : integer;
begin
  if FTransparent or (FUseBackground and not Background.Empty) or
     ((FImgForm <> nil) and (FImgForm.Backgroundtype <> bgtColorFill) and (not (csDesigning in Componentstate))) then
  begin
    Perform(LB_GETTOPINDEX, 0, 0);
    inherited;
    if Items.Count = 0 then
    begin
      DC := GetDC(Handle);
      try
        if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.Componentstate)) then
        begin
          if (FImgForm.Control <> Self) then
          begin
            R1 := ClientRect;
            BgRect := ClientRect;
            BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
            P := ClientToScreen(ClientRect.TopLeft);
            ax := BgRect.Left - P.x;
            ay := BgRect.Top - P.y;

            BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
            BgRect.TopLeft := RealScreenToClient(BgRect.TopLeft);
            BgRect.BottomRight := RealScreenToClient(BgRect.BottomRight);

            FImgForm.PaintBkgnd(DC, R1, Point(BgRect.Left - ax, BgRect.Top - ay), false);
          end
        end
        else
        if FTransparent then
          DrawParentControl(DC)
        else
          DrawBackground(DC, ClientRect);
      finally
        ReleaseDC(Handle, DC);
      end;
    end;  
  end
  else
    inherited;
  if FFlat then
  begin
    DC := GetWindowDC(Handle);
    try
      DrawFlatBorder(DC, FInHScroll, FInVScroll);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;

procedure TCustomElListBox.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  if Flat and not FMouseOver then DrawFlatBorder(0, false, false);
  if Flat and ShowCheckBox then Invalidate;
end;

procedure TCustomElListBox.WMThemeChanged(var Message: TMessage);
begin
  if ThemesAvailable and UseXPThemes then
  begin
    FreeThemeHandle;
    CreateThemeHandle;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  end;
  Message.Result := 1;
end;

procedure TCustomElListBox.WMVScroll(var Message: TMessage);
begin
  inherited;
  if TWMScroll(Message).ScrollCode = SB_THUMBTRACK then
    FInVSCroll := true
  else
  if TWMScroll(Message).ScrollCode = SB_THUMBPOSITION then
    FInVSCroll := false;

  if (Flat and (((not FMouseOver) and (not Focused)) or (FlatFocusedScrollBars))) then
    DrawFlatBorder(0, FInHScroll, FInVScroll);
end;

procedure TCustomElListBox.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Perform(IFM_REPAINTCHILDREN, 0, 0);
end;

procedure TCustomElListBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) and ((Message.Msg = WM_LBUTTONDOWN) or
    (Message.Msg = WM_LBUTTONDBLCLK)) and not Dragging then
  begin
    if DragMode = dmAutomatic then
    begin
      if IsControlMouseMsg(TWMMouse(Message)) then
        Exit;
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Message);  {overrides TControl's BeginDrag}
      Exit;
    end;
  end;
  if not HorizontalScroll then
    inherited WndProc(Message)
  else
  case Message.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        inherited WndProc(Message);
        FMaxWidth := Max(FMaxWidth, GetItemWidth(Message.Result));
        SetHorizontalExtent;
      end;
    LB_DELETESTRING:
      begin
        if GetItemWidth(Message.wParam) >= FMaxWidth then
        begin
          Perform(WM_HSCROLL, SB_TOP, 0);
          inherited WndProc(Message);
          ResetHorizontalExtent;
        end
        else
          inherited WndProc(Message);
      end;
    LB_RESETCONTENT:
      begin
        FMaxWidth := 0;
        SetHorizontalExtent;
        Perform(WM_HSCROLL, SB_TOP, 0);
        inherited WndProc(Message);
      end;
    WM_SETFONT:
      begin
        inherited WndProc(Message);
        Canvas.Font.Assign(Self.Font);
        ResetHorizontalExtent;
        Exit;
      end;
    else
      inherited WndProc(Message);
  end;
end;

function TCustomElListBox.GetItemHeight: Integer;
var
  R: TRect;
begin
  Result := FItemHeight;
  if HandleAllocated then
  begin
    Perform(LB_GETITEMRECT, 0, Longint(@R));
    Result := R.Bottom - R.Top;
  end;
end;

function TCustomElListBox.GetItemIndex: Integer;
begin
  if MultiSelect then
    Result := SendMessage(Handle, LB_GETCARETINDEX, 0, 0)
  else
    Result := SendMessage(Handle, LB_GETCURSEL, 0, 0);
end;

function TCustomElListBox.GetSelCount: Integer;
begin
  Result := SendMessage(Handle, LB_GETSELCOUNT, 0, 0);
end;

function TCustomElListBox.GetSelected(Index: Integer): Boolean;
var
  R: Longint;
begin
  R := SendMessage(Handle, LB_GETSEL, Index, 0);
  if R = LB_ERR then
  {$ifdef VCL_5_USED}
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
  {$else}
    raise EListError.CreateFmt(SListIndexError, [Index]);
  {$endif}
  Result := LongBool(R);
end;

function TCustomElListBox.GetTopIndex: Integer;
begin
  Result := SendMessage(Handle, LB_GETTOPINDEX, 0, 0);
end;

procedure TCustomElListBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomElListBox.SetColumns(Value: Integer);
begin
  if FColumns <> Value then
    if (FColumns = 0) or (Value = 0) then
    begin
      FColumns := Value;
      RecreateWnd;
    end else
    begin
      FColumns := Value;
      if HandleAllocated then SetColumnWidth;
    end;
end;

procedure TCustomElListBox.SetExtendedSelect(Value: Boolean);
begin
  if Value <> FExtendedSelect then
  begin
    FExtendedSelect := Value;
    RecreateWnd;
  end;
end;

procedure TCustomElListBox.SetIntegralHeight(Value: Boolean);
begin
  if Value <> FIntegralHeight then
  begin
    FIntegralHeight := Value;
    RecreateWnd;
    RequestAlign;
  end;
end;

procedure TCustomElListBox.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) and (Value > 0) then
  begin
    FItemHeight := Value;
    RecreateWnd;
  end;
end;

procedure TCustomElListBox.SetItemIndex(Value: Integer);
begin
  if GetItemIndex <> Value then
    if MultiSelect then SendMessage(Handle, LB_SETCARETINDEX, Value, 0)
    else SendMessage(Handle, LB_SETCURSEL, Value, 0);
end;

procedure TCustomElListBox.SetItems(Value: TElFStrings);
begin
  Items.Assign(Value);
end;

procedure TCustomElListBox.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

procedure TCustomElListBox.SetSelected(Index: Integer; Value: Boolean);
begin
  if SendMessage(Handle, LB_SETSEL, Longint(Value), Index) = LB_ERR then
  {$ifdef VCL_5_USED}
    raise EListError.CreateResFmt(@SListIndexError, [Index]);
  {$else}
    raise EListError.CreateFmt(SListIndexError, [Index]);
  {$endif}
end;

procedure TCustomElListBox.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    TElFStringList(FListBoxStrings).Sorted := true;
  end;
end;

procedure TCustomElListBox.SetTabWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FTabWidth <> Value then
  begin
    FTabWidth := Value;
    RecreateWnd;
  end;
end;

procedure TCustomElListBox.SetTopIndex(Value: Integer);
begin
  if GetTopIndex <> Value then
    SendMessage(Handle, LB_SETTOPINDEX, Value, 0);
end;

function TCustomElListBox.InternalGetItemData(Index: Integer): LongInt;
begin
  Result := GetItemData(Index);
end;

procedure TCustomElListBox.InternalSetItemData(Index: Integer; AData: LongInt);
begin
  SetItemData(Index, AData);
end;

function TCustomElListBox.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
var
  Count: Integer;
  ItemRect: TRect;
begin
  if PtInRect(ClientRect, Pos) then
  begin
    Result := TopIndex;
    Count := Items.Count;
    while Result < Count do
    begin
      Perform(LB_GETITEMRECT, Result, Longint(@ItemRect));
      if PtInRect(ItemRect, Pos) then Exit;
      Inc(Result);
    end;
    if not Existing then Exit;
  end;
  Result := -1;
end;

function TCustomElListBox.ItemRect(Index: Integer): TRect;
var
  Count: Integer;
begin
  Count := Items.Count;
  if (Index = 0) or (Index < Count) then
    Perform(LB_GETITEMRECT, Index, Longint(@Result))
  else if Index = Count then
  begin
    Perform(LB_GETITEMRECT, Index - 1, Longint(@Result));
    OffsetRect(Result, 0, Result.Bottom - Result.Top);
  end else FillChar(Result, SizeOf(Result), 0);
end;

procedure TCustomElListBox.ResetHorizontalExtent1;
var
  I: Integer;
begin
  FMaxWidth := 0;
  for I := 0 to Items.Count - 1 do
    FMaxWidth := Max(FMaxWidth, GetItemWidth(I));
  SetHorizontalExtent;
end;

procedure TCustomElListBox.SetHorizontalExtent;
begin
  SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
end;

procedure TCustomElListBox.SetColumnWidth;
var
  ColWidth: Integer;
begin
  if (FColumns > 0) and (Width > 0) then
  begin
    ColWidth := (Width + FColumns - 3) div FColumns;
    if ColWidth < 1 then
      ColWidth := 1;
    SendMessage(Handle, LB_SETCOLUMNWIDTH, ColWidth, 0);
  end;
end;

function TCustomElListBox.GetItemData(Index: Integer): LongInt;
begin
  Result := SendMessage(Handle, LB_GETITEMDATA, Index, 0);
end;

procedure TCustomElListBox.SetItemData(Index: Integer; AData: LongInt);
begin
  SendMessage(Handle, LB_SETITEMDATA, Index, AData);
end;

procedure TCustomElListBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TCustomElListBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    LBN_SELCHANGE:
      begin
        inherited Changed;
        Click;
      end;
    LBN_DBLCLK: DblClick;
  end;
end;

procedure TCustomElListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := Windows.TOwnerDrawState(LongRec(itemState).Lo);
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      FCanvas.Brush.Color := clHighlight;
      FCanvas.Font.Color := clHighlightText
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      FCanvas.FillRect(rcItem);
    if odFocused in State then
      DrawFocusRect(hDC, rcItem);
    FCanvas.Handle := 0;
  end;
end;

procedure TCustomElListBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
    if FStyle = lbOwnerDrawVariable then
      MeasureItem(itemID, Integer(itemHeight));
end;

procedure TCustomElListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Assigned(FOnMeasureItem) then FOnMeasureItem(Self, Index, Height);
end;

procedure TCustomElListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  ItemNo : Integer;
  ShiftState: TShiftState;
  p         : TPoint;
  R         : TRect;
  ImgOffs   : integer;
begin
  p := SmallPointToPoint(Message.Pos);
  if Images <> nil then
    ImgOffs := Images.Width + 2
  else
    ImgOffs := 0;
  (*
  if ImgOffs <> 0 then
  begin
    if InRange(1, ImgOffs + 2, P.x - R.Left) then
      exit;
  end;
  *)
  if ShowCheckBox then
  begin
    ItemNo := ItemAtPos(p, True);
    if ItemNo > -1 then
    begin
      R := ItemRect(ItemNo);
      if InRange(ImgOffs + 1, ImgOffs + GetCheckBoxSize.cx + 2, P.x - R.Left) then
      begin
        case GetState(ItemNo) of
          cbUnchecked:
            if AllowGrayed then
              SetState(ItemNo, cbGrayed)
            else
              SetState(ItemNo, cbChecked);
          cbGrayed:
            SetState(ItemNo, cbChecked);
          cbChecked:
            SetState(ItemNo, cbUnchecked);
        end;
        exit;
      end;
    end;
  end;
  ShiftState := KeysToShiftState(Message.Keys);
  if (DragMode = dmAutomatic) and FMultiSelect then
  begin
    if not (ssShift in ShiftState) or (ssCtrl in ShiftState) then
    begin
      ItemNo := ItemAtPos(p, True);
      if (ItemNo >= 0) and (Selected[ItemNo]) then
      begin
        BeginDrag (False);
        Exit;
      end;
    end;
  end;
  inherited;
  if (DragMode = dmAutomatic) and not (FMultiSelect and
    ((ssCtrl in ShiftState) or (ssShift in ShiftState))) then
    BeginDrag(False);
end;

procedure TCustomElListBox.WMSize(var Message: TWMSize);
begin
  inherited;
  SetColumnWidth;
end;

procedure TCustomElListBox.ResetContent;
begin
  SendMessage(Handle, LB_RESETCONTENT, 0, 0);
end;

procedure TCustomElListBox.DeleteString(Index: Integer);
begin
  SendMessage(Handle, LB_DELETESTRING, Index, 0);
end;

procedure TCustomElListBox.SetShowCheckBox(Value: Boolean);
begin
  if FShowCheckBox <> Value then
  begin
    FShowCheckBox := Value;
    Invalidate;
  end;
end;

function TCustomElListBox.GetState(Index: Integer): TCheckBoxState;
begin
  if FStatesSize <= Index then
    SetStatesSize(Index + 1);
  result := TCheckBoxState(FStates[Index]);
end;

procedure TCustomElListBox.SetState(Index: Integer; Value: TCheckBoxState);
var R : TRect;
begin
  if FStatesSize <= Index then
    SetStatesSize(Index + 1);
  FStates[Index] := Char(Value);
  if HandleAllocated and (Index < Items.Count) then
  begin
    R := ItemRect(Index);
    if not IsRectEmpty(R) then
      InvalidateRect(Handle, @R, false);
  end;
end;

procedure TCustomElListBox.SetStatesSize(aSize : integer);
var P : PChar;
begin
  if aSize = 0 then
  begin
    if FStatesSize > 0 then
    begin
      FStatesSize := 0;
      FreeMem(FStates);
    end;
  end
  else
  begin
    GetMem(P, aSize);
    if (P <> nil) then
    begin
      FillMemory(P, aSize, byte(cbUnchecked));
      MoveMemory(P, FStates, Min(aSize, FStatesSize));
      if FStatesSize > 0 then
      begin
        FStatesSize := 0;
        FreeMem(FStates);
      end;
      FStates := P;
      FStatesSize := aSize;
    end;
  end;
end;

function TCustomElListBox.GetCheckBoxSize: TSize;
begin
  Result.cx := CheckBoxSize;
  Result.cy := CheckBoxSize;
{$ifndef CLX_USED}
  if IsThemeApplied() then
  begin
    GetThemePartSize(Theme, Canvas.Handle, BP_CHECKBOX, 1, nil, TS_TRUE, Result);
  end;
{$endif}
end;

procedure TCustomElListBox.SetAllowGrayed(Value: Boolean);
var i : integer;
begin
  if FAllowGrayed <> Value then
  begin
    FAllowGrayed := Value;
    if not FAllowGrayed then
    for i := 0 to Items.Count - 1 do
    begin
      if TCheckBoxState(FStates[i]) = cbGrayed then
        FStates[i] := Char(cbUnchecked);
    end;
  end;
end;

procedure TCustomElListBox.DrawFlatFrame(Canvas : TCanvas; R : TRect);
var AColor, Color : TColor;
begin
  if ((Focused or FMouseOver) and (ActiveBorderType = fbtColorLineBorder)) or
     ((not (Focused or FMouseOver)) and (InactiveBorderType = fbtColorLineBorder)) then
  begin
    if Enabled then
      Color := clWindow
    else
      Color := clBtnFace;
    if Focused or FMouseOver then
      AColor := LineBorderActiveColor
    else
      AColor := LineBorderInactiveColor;
    ELVCLUtils.DrawFlatFrameEx2(Canvas.Handle, R, AColor, Color, false, true, AllBorderSides, fbtColorLineBorder);
  end
  else
    ElVCLUtils.DrawFlatFrame(Canvas.Handle, R, clWindow, false);
end;

procedure TCustomElListBox.OnImageListChange(Sender : TObject);
begin
  AdjustItemHeight;
  if (HandleAllocated) then
    Invalidate;//Repaint;
end; { OnImageListChange }

procedure TCustomElListBox.SetImages(newValue : TImageList);
{ Sets data member FImages to newValue. }
begin
  if (FImages <> newValue) then
  begin
    if FImages <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImages.RemoveFreeNotification(Self);
      {$endif}
      FImages.UnRegisterChanges(FImageChangeLink);
    end;
    FImages := newValue;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    AdjustItemHeight;
    if (HandleAllocated) then
      Invalidate;
  end; { if }
end; { SetImages }

procedure TCustomElListBox.AdjustItemHeight;
var r : integer;
    TM: TTextMetric;
begin
  if not HandleAllocated then
    r := Abs(Font.Height) + 2
  else
  begin
    GetTextMetrics(Canvas.Handle, TM);
    r := TM.tmHeight + 2;
  end;
  if Assigned(Images) then
    r := Max(r, Images.Height);
  ItemHeight := r;
end;

procedure TCustomElListBox.SetImagesSize(aSize : integer);
var P : PIntArray;
    i : integer;
begin
  if aSize = 0 then
  begin
    if FImagesSize > 0 then
    begin
      FImagesSize := 0;
      FreeMem(FImageIndex);
    end;
  end
  else
  begin
    GetMem(P, aSize * sizeof(Integer));
    if (P <> nil) then
    begin
      for i := 0 to aSize -1 do
        P[i] := -1;
      MoveMemory(P, FImageIndex, Min(aSize, FImagesSize) * sizeof(integer));
      if FStatesSize > 0 then
      begin
        FImagesSize := 0;
        FreeMem(FImageIndex);
      end;
      FImageIndex := P;
      FImagesSize := aSize;
    end;
  end;
end;

function TCustomElListBox.GetImageIndex(Index: Integer): Integer;
begin
  if FImagesSize <= Index then
    SetImagesSize(Index + 1);
  Result := FImageIndex[Index];
end;

procedure TCustomElListBox.SetImageIndex(Index: Integer; Value: Integer);
var R : TRect;
begin
  if FImagesSize <= Index then
    SetImagesSize(Index + 1);
  FImageIndex[Index] := Value;
  if HandleAllocated and (Index < Items.Count) then
  begin
    R := ItemRect(Index);
    if not IsRectEmpty(R) then
      InvalidateRect(Handle, @R, false);
  end;
end;

{$ifdef ELPACK_UNICODE}
{$ifndef CLX_USED}
procedure TCustomElListBox.CMHintShow(var Message: TMessage);
{$else}
function TCustomElListBox.HintShow(var HintInfo : THintInfo): Boolean;
{$endif}
{$ifndef CLX_USED}
var HintInfo : PHintInfo;
{$endif}
{$ifdef ELPACK_UNICODE}
var T: WideChar;
    l : integer;
    S : String;
    WS: WideString;
{$endif}
begin
  {$ifndef CLX_USED}
  inherited;
  HintInfo := PHintInfo(Message.lParam);
  {$else}
  result := inherited HintShow(HintInfo);
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
{$endif}

{$ifdef ELPACK_UNICODE}
procedure TCustomElListBox.SetHint(Value: WideString);
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

procedure TCustomElListBox.ItemsChange(Sender : TObject);
begin
  Invalidate;
end;

procedure TCustomElListBox.SetUseSelectedFont(Value: Boolean);
begin
  if FUseSelectedFont <> Value then
  begin
    FUseSelectedFont := Value;
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TCustomElListBox.SetSelectedFontColor(Value: TColor);
begin
  if FSelectedFontColor <> Value then
  begin
    FSelectedFontColor := Value;
    if HandleAllocated then
      Invalidate;
  end;
end;

{$ifdef ELPACK_UNICODE}
function TElListBoxStrings.Add(const S: WideString): Integer;
{$else}
function TElListBoxStrings.Add(const S: String): Integer;
{$endif}
begin
{$ifdef ELPACK_UNICODE}
  Result := inherited Add(S);
  SendMessage(ListBox.Handle, LB_INSERTSTRING, Result, Integer(PChar('')));
{$else}
  Result := SendMessage(ListBox.Handle, LB_ADDSTRING, 0, Longint(PChar(S)));
{$endif}
  if Result < 0 then raise EOutOfResources.Create(SInsertLineError);
end;

procedure TElListBoxStrings.Clear;
begin
{$ifdef ELPACK_UNICODE}
  inherited;
{$endif}
  ListBox.ResetContent;
end;

procedure TElListBoxStrings.Delete(Index: Integer);
begin
{$ifdef ELPACK_UNICODE}
  inherited;
{$endif}
  ListBox.DeleteString(Index);
end;

procedure TElListBoxStrings.Exchange(Index1, Index2: Integer);
{$ifndef ELPACK_UNICODE}
var
  TempData: Longint;
  TempString: string;
{$endif}
begin
{$ifdef ELPACK_UNICODE}
  inherited;
  BeginUpdate;
  try
    (*
    TempString := Strings[Index1];
    Strings[Index1] := Strings[Index2];
    Strings[Index2] := TempString;
    *)
    if ListBox.ItemIndex = Index1 then
      ListBox.ItemIndex := Index2
    else
    if ListBox.ItemIndex = Index2 then
      ListBox.ItemIndex := Index1;
  finally
    EndUpdate;
  end;
{$else}
  BeginUpdate;
  try
    TempString := Strings[Index1];

    TempData := ListBox.InternalGetItemData(Index1);
    Strings[Index1] := Strings[Index2];
    ListBox.InternalSetItemData(Index1, ListBox.InternalGetItemData(Index2));
    Strings[Index2] := TempString;
    ListBox.InternalSetItemData(Index2, TempData);
    if ListBox.ItemIndex = Index1 then
      ListBox.ItemIndex := Index2
    else
    if ListBox.ItemIndex = Index2 then
      ListBox.ItemIndex := Index1;
  finally
    EndUpdate;
  end;
{$endif}
end;

{$ifdef ELPACK_UNICODE}
function TElListBoxStrings.Get(Index: Integer): WideString;
{$else}
function TElListBoxStrings.Get(Index: Integer): string;
{$endif}
{$ifndef ELPACK_UNICODE}
var
  Len: Integer;
  Text: array[0..4095] of Char;
{$endif}
begin
{$ifdef ELPACK_UNICODE}
  result := inherited Get(Index);
{$else}
  Len := SendMessage(ListBox.Handle, LB_GETTEXT, Index, Longint(@Text));
  if Len < 0 then Error(SListIndexError, Index);
  SetString(Result, Text, Len);
{$endif}
end;

function TElListBoxStrings.GetCount: Integer;
begin
{$ifdef ELPACK_UNICODE}
  result := inherited GetCount;
{$else}
  Result := SendMessage(ListBox.Handle, LB_GETCOUNT, 0, 0);
{$endif}
end;

function TElListBoxStrings.GetObject(Index: Integer): TObject;
begin
{$ifdef ELPACK_UNICODE}
  Result := inherited GetObject(Index);
{$else}
  Result := TObject(ListBox.GetItemData(Index));
  if Longint(Result) = LB_ERR then Error(SListIndexError, Index);
{$endif}
end;

{$ifdef ELPACK_UNICODE}
function TElListBoxStrings.IndexOf(const S: WideString): Integer;
{$else}
function TElListBoxStrings.IndexOf(const S: String): Integer;
{$endif}
begin
{$ifdef ELPACK_UNICODE}
  Result := inherited IndexOf(S);
{$else}
  Result := SendMessage(ListBox.Handle, LB_FINDSTRINGEXACT, -1, LongInt(PChar(S)));
{$endif}
end;

{$ifdef ELPACK_UNICODE}
procedure TElListBoxStrings.Insert(Index: Integer; const S: WideString);
{$else}
procedure TElListBoxStrings.Insert(Index: Integer; const S: String);
{$endif}
begin
{$ifdef ELPACK_UNICODE}
  inherited Insert(Index, S);
  if SendMessage(ListBox.Handle, LB_INSERTSTRING, Index, Integer(PChar(''))) < 0 then
    raise EOutOfResources.Create(SInsertLineError);
{$else}
  if SendMessage(ListBox.Handle, LB_INSERTSTRING, Index,
    Longint(PChar(S))) < 0 then
    raise EOutOfResources.Create(SInsertLineError);
{$endif}
end;

procedure TElListBoxStrings.Move(CurIndex, NewIndex: Integer);
{$ifndef ELPACK_UNICODE}
var
  TempData: Longint;
  TempString: string;
  {$endif}
begin
{$ifdef ELPACK_UNICODE}
  inherited;
{$endif}
  BeginUpdate;
  ListBox.FMoving := True;
  try
    {$ifndef ELPACK_UNICODE}
    if CurIndex <> NewIndex then
    begin
      TempString := Get(CurIndex);
      {$ifndef ELPACK_UNICODE}
      TempData := ListBox.InternalGetItemData(CurIndex);
      ListBox.InternalSetItemData(CurIndex, 0);
      {$endif}
      Delete(CurIndex);
      Insert(NewIndex, TempString);
      {$ifndef ELPACK_UNICODE}
      ListBox.InternalSetItemData(NewIndex, TempData);
      {$endif}
    end;
    {$endif}
  finally
    ListBox.FMoving := False;
    EndUpdate;
  end;
end;

{$ifdef ELPACK_UNICODE}
procedure TElListBoxStrings.Put(Index: Integer; const S: WideString);
{$else}
procedure TElListBoxStrings.Put(Index: Integer; const S: String);
{$endif}
{$ifndef ELPACK_UNICODE}
var
  I: Integer;
  TempData: Longint;
{$endif}
begin
{$ifdef ELPACK_UNICODE}
  inherited;
{$else}
  I := ListBox.ItemIndex;
  TempData := ListBox.InternalGetItemData(Index);
  // Set the Item to 0 in case it is an object that gets freed during Delete
  ListBox.InternalSetItemData(Index, 0);
  Delete(Index);
  InsertObject(Index, S, nil);
  ListBox.InternalSetItemData(Index, TempData);
  ListBox.ItemIndex := I;
{$endif}
end;

procedure TElListBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
{$ifdef ELPACK_UNICODE}
  inherited;
{$else}
  ListBox.SetItemData(Index, LongInt(AObject));
{$endif}
end;

{$ifndef ELPACK_UNICODE}
{$ifdef VCL_6_USED}
procedure TElListBoxStrings.InsertItem(Index: Integer; const S: string; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;
{$endif}
{$endif}

procedure TElListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  SendMessage(ListBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then ListBox.Refresh;
end;

procedure TElListBoxStrings.ResetBox;
var i : integer;
begin
  for i :=0 to Count -1 do
    SendMessage(ListBox.Handle, LB_INSERTSTRING, i, Integer(PChar('')));
end;

end.

