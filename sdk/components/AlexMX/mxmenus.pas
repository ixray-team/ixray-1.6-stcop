unit MxMenus;

{$I mx.INC}
{$S-,W-,R-}

interface

uses Windows, SysUtils,
  Classes, Controls, Messages, Graphics, ImgList,
  {$IFDEF RX_D16}System.UITypes,{$ENDIF}
  Menus, MxHook {$IFDEF RX_D6}, Types{$ENDIF};

type
  TMxMenuStyle = (msStandard, msOwnerDraw, msBtnLowered, msBtnRaised);
  TMenuOwnerDrawState = set of (mdSelected, mdGrayed, mdDisabled, mdChecked,
    mdFocused, mdDefault);

  TDrawMenuItemEvent = procedure(Sender: TMenu; Item: TMenuItem; Rect: TRect;
    State: TMenuOwnerDrawState) of object;
  TMeasureMenuItemEvent = procedure(Sender: TMenu; Item: TMenuItem; var Width,
    Height: Integer) of object;
  TDrawMarginEvent = procedure(Sender: TMenu; Rect: TRect) of object;
  TItemParamsEvent = procedure(Sender: TMenu; Item: TMenuItem;
    State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
    var Graphic: TGraphic; var NumGlyphs: Integer) of object;
  TItemImageEvent = procedure(Sender: TMenu; Item: TMenuItem;
    State: TMenuOwnerDrawState; var ImageIndex: Integer) of object;

{ TMxPopupMenu }
  TMxPopupMenu = class(TPopupMenu)
  private
  	FMarginStartColor: TColor;
  	FMarginEndColor: TColor;
  	FSepHColor: TColor;
  	FSepLColor: TColor;
  	FBKColor: TColor;
  	FSelColor: TColor;
  	FSelFontColor: TColor;
  	FFontColor: TColor;
    FStyle: TMxMenuStyle;
    FCanvas: TCanvas;
    FShowCheckMarks: Boolean;
    FMinTextOffset: Cardinal;
    FLeftMargin: Cardinal;
    FCursor: TCursor;
    FOnDrawItem: TDrawMenuItemEvent;
    FOnMeasureItem: TMeasureMenuItemEvent;
    FOnDrawMargin: TDrawMarginEvent;
    FOnGetItemParams: TItemParamsEvent;
    FPopupPoint: TPoint;
    FParentBiDiMode: Boolean;
    FImages: TImageList;
    FImageChangeLink: TChangeLink;
    FOnGetImageIndex: TItemImageEvent;
    procedure SetImages(Value: TImageList);
    procedure ImageListChange(Sender: TObject);
    procedure SetStyle(Value: TMxMenuStyle);
    procedure WndMessage(Sender: TObject; var AMsg: TMessage;
      var Handled: Boolean);
    procedure WMDrawItem(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMMeasureItem(var Message: TWMMeasureItem); message WM_MEASUREITEM;
    procedure SetBiDiModeFromPopupControl;
  protected
    procedure Loaded; override;
    function UseRightToLeftAlignment: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState;
      var ImageIndex: Integer); dynamic;
    procedure DrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState); virtual;
    procedure DrawMargin(ARect: TRect); virtual;
    procedure GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState;
      AFont: TFont; var Color: TColor; var Graphic: TGraphic;
      var NumGlyphs: Integer); dynamic;
    procedure MeasureItem(Item: TMenuItem; var Width, Height: Integer); dynamic;
    procedure RefreshMenu(AOwnerDraw: Boolean); virtual;
    function IsOwnerDrawMenu: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh;
    procedure Popup(X, Y: Integer); override;
    procedure DefaultDrawItem(Item: TMenuItem; Rect: TRect;
      State: TMenuOwnerDrawState);
    procedure DefaultDrawMargin(ARect: TRect; StartColor, EndColor: TColor);
    property Canvas: TCanvas read FCanvas;
  published
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property MarginStartColor: TColor read FMarginStartColor write FMarginStartColor default clBlue;
    property MarginEndColor: TColor read FMarginEndColor write FMarginEndColor default clBlue;
    property BKColor: TColor read FBKColor write FBKColor default clBtnFace;
    property SelColor: TColor read FSelColor write FSelColor default clBtnShadow;
    property SelFontColor: TColor read FSelFontColor write FSelFontColor default clHighlightText;
    property FontColor: TColor read FFontColor write FFontColor default clMenuText;
    property SepHColor: TColor read FSepHColor write FSepHColor default clBtnShadow;
    property SepLColor: TColor read FSepLColor write FSepLColor default clBtnHighlight;
//    property Font: TFont read FCanvas.Font write FCanvas.Font;
    property LeftMargin: Cardinal read FLeftMargin write FLeftMargin default 0;
    property MinTextOffset: Cardinal read FMinTextOffset write FMinTextOffset default 0;
    property Style: TMxMenuStyle read FStyle write SetStyle default msStandard;
    property ShowCheckMarks: Boolean read FShowCheckMarks write FShowCheckMarks default True;
    property OwnerDraw stored False;
    property Images: TImageList read FImages write SetImages;
    property OnGetImageIndex: TItemImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnDrawItem: TDrawMenuItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDrawMargin: TDrawMarginEvent read FOnDrawMargin write FOnDrawMargin;
    property OnGetItemParams: TItemParamsEvent read FOnGetItemParams write FOnGetItemParams;
    property OnMeasureItem: TMeasureMenuItemEvent read FOnMeasureItem write FOnMeasureItem;
  end;

{ Utility routines }

procedure SetDefaultMenuFont(AFont: TFont);
function IsItemPopup(Item: TMenuItem): Boolean;

implementation

uses {$IFNDEF VER80} CommCtrl, {$ENDIF} Forms, ExtCtrls, Consts, mxConst,
  mxMaxMin, MxVCLUtils, MxClipIcon, MxStrUtils; // Polaris

const
  DefMarginColor: TColor = clBlue;
  AddWidth = 2;
  AddHeight = 4;
  Tab = #9#9;
  Separator = '-';

type
  TBtnStyle = (bsNone, bsLowered, bsRaised, bsOffice);

function BtnStyle(MenuStyle: TMxMenuStyle): TBtnStyle;
begin
{$IFNDEF VER80}
  case MenuStyle of
    msBtnLowered: Result := bsLowered;
    msBtnRaised: Result := bsRaised;
    else Result := bsNone;
  end;
{$ELSE}
  Result := bsNone;
{$ENDIF}
end;

function IsItemPopup(Item: TMenuItem): Boolean;
begin
  Result := (Item.Parent = nil) or (Item.Parent.Parent <> nil) or
    not (Item.Parent.Owner is TMainMenu);
end;

{$IFDEF VER80}
const
  { return codes for WM_MENUCHAR (not defined in Delphi 1.0) }
  MNC_IGNORE = 0;
  MNC_CLOSE = 1;
  MNC_EXECUTE = 2;
  MNC_SELECT = 3;
{$ENDIF}

{$IFNDEF RX_D4}
procedure ProcessMenuChar(AMenu: TMenu; var Message: TWMMenuChar);
var
  C, I, First, Hilite, Next: Integer;
  State: Word;

  function IsAccelChar(Menu: HMENU; State: Word; I: Integer; C: Char): Boolean;
  var
    Item: TMenuItem;
    Id: Cardinal;
  begin
    Item := nil;
    if State and MF_POPUP <> 0 then begin
      Menu := GetSubMenu(Menu, I);
      Item := AMenu.FindItem(Menu, fkHandle);
    end
    else begin
      Id := GetMenuItemID(Menu, I);
      if Id <> {$IFNDEF VER80} $FFFFFFFF {$ELSE} $FFFF {$ENDIF} then
        Item := AMenu.FindItem(Id, fkCommand);
    end;
    if Item <> nil then Result := IsAccel(Ord(C), Item.Caption)
    else Result := False;
  end;

  function IsInitialChar(Menu: HMENU; State: Word; I: Integer; C: Char): Boolean;
  var
    Item: TMenuItem;
  begin
    if State and MF_POPUP <> 0 then begin
      Menu := GetSubMenu(Menu, I);
      Item := AMenu.FindItem(Menu, fkHandle);
    end
    else begin
      Item := AMenu.FindItem(Menu, fkHandle);
      if Item <> nil then Item := Item.Items[I];
    end;
    if (Item <> nil) and (Item.Caption <> '') then
      Result := AnsiCompareText(Item.Caption[1], C) = 0
    else Result := False;
  end;

begin
  with Message do begin
    Result := MNC_IGNORE; { No item found: beep }
    First := -1;
    Hilite := -1;
    Next := -1;
    C := GetMenuItemCount(Menu);
    for I := 0 to C - 1 do begin
      State := GetMenuState(Menu, I, MF_BYPOSITION);
      if IsAccelChar(Menu, State, I, User) then begin
        if State and MF_DISABLED <> 0 then begin
          { Close the menu if this is the only disabled item to choose from.
            Otherwise, ignore the item. }
          if First < 0 then First := -2;
          Continue;
        end;
        if First < 0 then begin
          First := I;
          Result := MNC_EXECUTE;
        end
        else Result := MNC_SELECT;
        if State and MF_HILITE <> 0 then Hilite := I
        else if Hilite >= 0 then Next := I;
      end;
    end;
    { We found a single disabled item. End the selection. }
    if First < -1 then begin
      Result := MNC_CLOSE shl 16;
      Exit;
    end;

    { If we can't find accelerators, then look for initial letters }
    if First < 0 then
      for I := 0 to C - 1 do begin
        State := GetMenuState(Menu, I, MF_BYPOSITION);
        if IsInitialChar(Menu, State, I, User) then begin
          if State and MF_DISABLED <> 0 then begin
            Result := MNC_CLOSE shl 16;
            Exit;
          end;
          if First < 0 then begin
            First := I;
            Result := MNC_EXECUTE;
          end
          else Result := MNC_SELECT;
          if State and MF_HILITE <> 0 then Hilite := I
          else if Hilite >= 0 then Next := I;
        end;
      end;

    if (Result = MNC_EXECUTE) then Result := Result shl 16 or First
    else if Result = MNC_SELECT then begin
      if Next < 0 then Next := First;
      Result := Result shl 16 or Next;
    end;
  end;
end;
{$ENDIF RX_D4}

procedure MenuWndMessage(Menu: TMenu; var AMsg: TMessage; var Handled: Boolean);
var
  Message: TMessage;
  Item: Pointer;
begin
  with AMsg do
    case Msg of
      WM_MEASUREITEM:
        if (TWMMeasureItem(AMsg).MeasureItemStruct^.CtlType = ODT_MENU) then
        begin
          Item := Menu.FindItem(TWMMeasureItem(AMsg).MeasureItemStruct^.itemID, fkCommand);
          if Item <> nil then begin
            Message := AMsg;
            TWMMeasureItem(Message).MeasureItemStruct^.ItemData := Longint(Item);
            Menu.Dispatch(Message);
            Result := 1;
            Handled := True;
          end;
        end;
      WM_DRAWITEM:
        if (TWMDrawItem(AMsg).DrawItemStruct^.CtlType = ODT_MENU) then
        begin
          Item := Menu.FindItem(TWMDrawItem(AMsg).DrawItemStruct^.itemID, fkCommand);
          if Item <> nil then begin
            Message := AMsg;
            TWMDrawItem(Message).DrawItemStruct^.ItemData := Longint(Item);
            Menu.Dispatch(Message);
            Result := 1;
            Handled := True;
          end;
        end;
      WM_MENUSELECT: Menu.Dispatch(AMsg);
      CM_MENUCHANGED: Menu.Dispatch(AMsg);
      WM_MENUCHAR:
        begin
{$IFDEF RX_D4}
          Menu.ProcessMenuChar(TWMMenuChar(AMsg));
{$ELSE}
          ProcessMenuChar(Menu, TWMMenuChar(AMsg));
{$ENDIF}
        end;
    end;
end;

{$IFNDEF RX_D4}
procedure RefreshMenuItem(MenuItem: TMenuItem; OwnerDraw: Boolean);
const
  Breaks: array[TMenuBreak] of Longint = (0, MF_MENUBREAK, MF_MENUBARBREAK);
  Checks: array[Boolean] of LongInt = (MF_UNCHECKED, MF_CHECKED);
  Enables: array[Boolean] of LongInt = (MF_DISABLED or MF_GRAYED, MF_ENABLED);
  Separators: array[Boolean] of LongInt = (MF_STRING, MF_SEPARATOR);
{$IFNDEF VER80}
  IBreaks: array[TMenuBreak] of DWORD = (MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK);
  IRadios: array[Boolean] of DWORD = (MFT_STRING, MFT_RADIOCHECK);
  ISeparators: array[Boolean] of DWORD = (MFT_STRING, MFT_SEPARATOR);
  IOwnerDraw: array[Boolean] of DWORD = (MFT_STRING, MFT_OWNERDRAW);
{$ENDIF}
var
{$IFNDEF VER80}
  MenuItemInfo: TMenuItemInfo;
{$ENDIF}
  CCaption: array[0..255] of Char;
  NewFlags: Integer;
  ItemID, I, C: Integer;
  MenuHandle: THandle;
  Item: TMenuItem;

{$IFNDEF VER80}
  procedure PrepareItemInfo;
  begin
    FillChar(MenuItemInfo, SizeOf(TMenuItemInfo), 0);
    with MenuItemInfo do begin
      cbSize := SizeOf(TMenuItemInfo);
      fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or
        MIIM_SUBMENU or MIIM_TYPE;
      cch := SizeOf(CCaption) - 1;
    end;
  end;
{$ENDIF}

begin
  if (MenuItem <> nil) then begin
    StrPCopy(CCaption, MenuItem.Caption);
    NewFlags := Breaks[MenuItem.Break] or Checks[MenuItem.Checked] or
      Enables[MenuItem.Enabled] or Separators[MenuItem.Caption = Separator] or
      MF_BYCOMMAND;
    ItemID := MenuItem.Command;
    if MenuItem.Count > 0 then begin
      NewFlags := NewFlags or MF_POPUP;
      ItemID := MenuItem.Handle;
    end
    else begin
      if (MenuItem.ShortCut <> scNone) and ((MenuItem.Parent = nil) or
        (MenuItem.Parent.Parent <> nil) or
        not (MenuItem.Parent.Owner is TMainMenu)) then
          StrPCopy(StrECopy(StrEnd(CCaption), Tab),
            ShortCutToText(MenuItem.ShortCut));
    end;
    Item := MenuItem;
    while Item.Parent <> nil do Item := Item.Parent;
    if (Item.Owner <> nil) and (Item.Owner is TMenu) then
      MenuHandle := TMenu(Item.Owner).Handle
    else
      MenuHandle := Item.Handle;
{$IFNDEF VER80}
    if Lo(GetVersion) >= 4 then begin
      FillChar(MenuItemInfo, SizeOf(TMenuItemInfo), 0);
      MenuItemInfo.cbSize := SizeOf(TMenuItemInfo);
      if MenuItem.Count > 0 then begin
        MenuItemInfo.fMask := MIIM_DATA or MIIM_TYPE;
        with MenuItem do
          MenuItemInfo.fType := IRadios[RadioItem] or IBreaks[Break] or
            ISeparators[Caption = Separator] or IOwnerDraw[OwnerDraw];
        MenuItemInfo.dwTypeData := CCaption;
        SetMenuItemInfo(MenuHandle, MenuItem.Command, False, MenuItemInfo);
      end
      else begin
        C := GetMenuItemCount(MenuHandle);
        ItemID := -1;
        for I := 0 to C - 1 do begin
          PrepareItemInfo;
          MenuItemInfo.dwTypeData := CCaption;
          GetMenuItemInfo(MenuHandle, I, True, MenuItemInfo);
          if MenuItemInfo.wID = MenuItem.Command then begin
            ItemID := I;
            Break;
        end;
    end;
        if (ItemID < 0) and (MenuItem.Parent <> nil) then begin
          MenuHandle := MenuItem.Parent.Handle;
          C := GetMenuItemCount(MenuHandle);
          for I := 0 to C - 1 do begin
            PrepareItemInfo;
            MenuItemInfo.dwTypeData := CCaption;
            GetMenuItemInfo(MenuHandle, I, True, MenuItemInfo);
            if MenuItemInfo.wID = MenuItem.Command then begin
              ItemID := I;
              Break;
            end;
          end;
        end;
        if ItemID < 0 then Exit;
        with MenuItem do
          MenuItemInfo.fType := IRadios[RadioItem] or IBreaks[Break] or
            ISeparators[Caption = Separator] or IOwnerDraw[OwnerDraw];
        MenuItemInfo.dwTypeData := CCaption;
        DeleteMenu(MenuHandle, MenuItem.Command, MF_BYCOMMAND);
        InsertMenuItem(MenuHandle, ItemID, True, MenuItemInfo);
      end;
    end
    else
{$ENDIF}
    begin
      if OwnerDraw then begin
        ModifyMenu(MenuHandle, MenuItem.Command, NewFlags or MF_OWNERDRAW and
          not MF_STRING, ItemID, PChar(MenuItem));
      end
      else begin
        ModifyMenu(MenuHandle, MenuItem.Command, NewFlags, ItemID, CCaption);
      end;
    end;
    for I := 0 to MenuItem.Count - 1 do
      RefreshMenuItem(MenuItem.Items[I], OwnerDraw);
  end;
end;
{$ENDIF RX_D4}

procedure SetDefaultMenuFont(AFont: TFont);
{$IFNDEF VER80}
var
  NCMetrics: TNonCLientMetrics;
{$ENDIF}
begin
{$IFNDEF VER80}
  if NewStyleControls then begin
    NCMetrics.cbSize := SizeOf(TNonCLientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCMetrics, 0) then
    begin
      AFont.Handle := CreateFontIndirect(NCMetrics.lfMenuFont);
      Exit;
    end;
  end;
{$ENDIF}
  with AFont do begin
    if NewStyleControls then Name := {$IFDEF RX_D6}'Tahoma'{$ELSE}'MS Sans Serif'{$ENDIF}
    else Name := 'System';
    Size := 8;
    Color := clMenuText;
    Style := [];
  end;
  AFont.Color := clMenuText;
end;

function GetDefItemHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYMENU);
  if NewStyleControls then Dec(Result, 2);
end;

function GetMarginOffset: Integer;
begin
  Result := Round(LoWord(GetMenuCheckMarkDimensions) * 0.3);
end;

procedure MenuLine(Canvas: TCanvas; C: TColor; X1, Y1, X2, Y2: Integer);
begin
  with Canvas do begin
    Pen.Color := C;
    MoveTo(X1, Y1);
    LineTo(X2, Y2);
  end;
end;

procedure DrawDisabledBitmap(Canvas: TCanvas; X, Y: Integer; Bitmap: TBitmap;
  State: TMenuOwnerDrawState);
const
  ROP_DSPDxax = $00E20746;
var
  Bmp: TBitmap;
  GrayColor, SaveColor: TColor;
  IsHighlight: Boolean;
begin
  if (mdSelected in State) then GrayColor := clGrayText
  else GrayColor := clBtnShadow;
  IsHighlight := NewStyleControls and ((not (mdSelected in State)) or
    (GetNearestColor(Canvas.Handle, ColorToRGB(clGrayText)) =
    GetNearestColor(Canvas.Handle, ColorToRGB(clHighlight))));
  if Bitmap.Monochrome then begin
    SaveColor := Canvas.Brush.Color;
    try
      if IsHighlight then begin
        Canvas.Brush.Color := clBtnHighlight;
        SetTextColor(Canvas.Handle, clWhite);
        SetBkColor(Canvas.Handle, clBlack);
        BitBlt(Canvas.Handle, X + 1, Y + 1, Bitmap.Width, Bitmap.Height,
          Bitmap.Canvas.Handle, 0, 0, ROP_DSPDxax);
      end;
      Canvas.Brush.Color := GrayColor;
      SetTextColor(Canvas.Handle, clWhite);
      SetBkColor(Canvas.Handle, clBlack);
      BitBlt(Canvas.Handle, X, Y, Bitmap.Width, Bitmap.Height,
        Bitmap.Canvas.Handle, 0, 0, ROP_DSPDxax);
    finally
      Canvas.Brush.Color := SaveColor;
    end;
  end
  else begin
    Bmp := CreateDisabledBitmapEx(Bitmap, clBlack, clMenu,
      clBtnHighlight, GrayColor, IsHighlight);
    try
      DrawBitmapTransparent(Canvas, X, Y, Bmp, clMenu);
    finally
      Bmp.Free;
    end;
  end;
end;

procedure DrawMenuBitmap(Canvas: TCanvas; X, Y: Integer; Bitmap: TBitmap;
  IsColor: Boolean; State: TMenuOwnerDrawState);
begin
  if (mdDisabled in State) then
    DrawDisabledBitmap(Canvas, X, Y, Bitmap, State)
  else begin
    if Bitmap.Monochrome and not IsColor then
      BitBlt(Canvas.Handle, X, Y, Bitmap.Width, Bitmap.Height,
        Bitmap.Canvas.Handle, 0, 0, SRCCOPY)
    else
      DrawBitmapTransparent(Canvas, X, Y, Bitmap, Bitmap.TransparentColor
        and not PaletteMask);
  end;
end;

procedure DrawMenuItem(AMenu: TMenu; Item: TMenuItem; Glyph: TGraphic;
  NumGlyphs: Integer; Canvas: TCanvas; ShowCheck: Boolean; Buttons: TBtnStyle;
  Rect: TRect; MinOffset: {$IFDEF RX_D4} Integer {$ELSE} Cardinal {$ENDIF};
  State: TMenuOwnerDrawState {$IFNDEF VER80}; Images: TImageList;
  ImageIndex: Integer {$ENDIF});
var
  Left, LineTop, MaxWidth, I, W: Integer;
  CheckSize: Longint;
  BtnRect: TRect;
  IsPopup, DrawHighlight, DrawLowered: Boolean;
  GrayColor: TColor;
  Bmp: TBitmap;
{$IFNDEF VER80}
  Ico: HIcon;
  H: Integer;
{$ENDIF}
{$IFDEF RX_D4}
  ParentMenu: TMenu;
{$ENDIF}

  procedure MenuTextOut(X, Y: Integer; const Text: string; Flags: Longint);
  var
    R: TRect;
  begin
    if Length(Text) = 0 then Exit;
{$IFDEF RX_D4}
    if (ParentMenu <> nil) and (ParentMenu.IsRightToLeft) then begin
      if Flags and DT_LEFT = DT_LEFT then
        Flags := Flags and (not DT_LEFT) or DT_RIGHT
      else if Flags and DT_RIGHT = DT_RIGHT then
        Flags := Flags and (not DT_RIGHT) or DT_LEFT;
      Flags := Flags or DT_RTLREADING;
    end;
{$ENDIF}
    R := Rect; R.Left := X; R.Top := Y;
    if (mdDisabled in State) then begin
      if DrawHighlight then begin
        Canvas.Font.Color := clBtnHighlight;
        OffsetRect(R, 1, 1);
        DrawText(Canvas.Handle, @Text[1], Length(Text), R, Flags);
        OffsetRect(R, -1, -1);
      end;
      Canvas.Font.Color := GrayColor;
    end;
    DrawText(Canvas.Handle, @Text[1], Length(Text), R, Flags)
  end;

  procedure DrawCheckImage(X, Y: Integer);
  begin
    Bmp := TBitmap.Create;
    try
{$IFNDEF VER80}
      with Bmp do begin
        Width := LoWord(CheckSize);
        Height := HiWord(CheckSize);
      end;
      if Item.RadioItem then begin
        with Bmp do begin
          DrawFrameControl(Canvas.Handle, Bounds(0, 0, Width, Height),
            DFC_MENU, DFCS_MENUBULLET);
          Monochrome := True;
        end;
      end
      else begin
        with Bmp do begin
          DrawFrameControl(Canvas.Handle, Bounds(0, 0, Width, Height),
            DFC_MENU, DFCS_MENUCHECK);
          Monochrome := True;
        end;
      end;
{$ELSE}
      Bmp.Handle := LoadBitmap(0, PChar(32760));
{$ENDIF}
      DrawMenuBitmap(Canvas, X, Y, Bmp, DrawLowered, State);
    finally
      Bmp.Free;
    end;
  end;

  procedure DrawGlyphCheck(ARect: TRect);
  var
    SaveColor: TColor;
    Bmp: TBitmap;
  begin
    InflateRect(ARect, 0, -1);
    SaveColor := Canvas.Brush.Color;
    try
      if not (mdSelected in State) then
{$IFDEF RX_D4}
        Bmp := AllocPatternBitmap(clMenu, clBtnHighlight)
{$ELSE}
        Bmp := CreateTwoColorsBrushPattern(clMenu, clBtnHighlight)
{$ENDIF}
      else Bmp := nil;
      try
        if Bmp <> nil then Canvas.Brush.Bitmap := Bmp
        else Canvas.Brush.Color := clMenu;
        Canvas.FillRect(ARect);
      finally
        Canvas.Brush.Bitmap := nil;
{$IFNDEF RX_D4}
        Bmp.Free;
{$ENDIF}
      end;
    finally
      Canvas.Brush.Color := SaveColor;
    end;
    Frame3D(Canvas, ARect, GrayColor, clBtnHighlight, 1);
  end;

{$IFNDEF VER80}
  function UseImages: Boolean;
  begin
    Result := Assigned(Images) and (ImageIndex >= 0) and
      (ImageIndex < Images.Count) and Images.HandleAllocated;
  end;
{$ENDIF}

begin
    IsPopup := IsItemPopup(Item);

    DrawLowered   := Item.Checked and IsPopup and not (ShowCheck or
                    (Buttons in [bsLowered, bsRaised]));
    DrawHighlight := NewStyleControls and (not (mdSelected in State) or
                    (Buttons in [bsLowered, bsRaised]) or (not IsPopup and
                    (Buttons = bsOffice)) or
                    (GetNearestColor(Canvas.Handle, ColorToRGB(clGrayText)) = GetNearestColor(Canvas.Handle, ColorToRGB(clHighlight))));
    if (mdSelected in State) and not (Buttons in [bsLowered, bsRaised]) then GrayColor := clGrayText
    else GrayColor := clBtnShadow;
    if IsPopup then begin
        if ShowCheck then CheckSize := GetMenuCheckMarkDimensions
        else              CheckSize := 2;
        Left := 2 * GetMarginOffset + LoWord(CheckSize);
    end else begin
        MinOffset := 0;
        CheckSize := 0;
        Left := GetMarginOffset + 2;
    end;
    if (Buttons <> bsNone) and (mdSelected in State) then begin
        case Buttons of
        bsLowered:  Frame3D(Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
        bsRaised:   Frame3D(Canvas, Rect, clBtnHighlight, clBtnShadow, 1);
        bsOffice:   if not IsPopup then Frame3D(Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
        end;
    end;
    if Assigned(Item) then begin
{$IFDEF RX_D4}
        ParentMenu := Item.GetParentMenu;
{$ENDIF}
        if Item.Checked and ShowCheck and IsPopup then begin
            DrawCheckImage(Rect.Left + (Left - LoWord(CheckSize)) div 2,
                            (Rect.Bottom + Rect.Top - HiWord(CheckSize)) div 2);
        end;
{$IFNDEF VER80}
        if Assigned(Images) and IsPopup then
            MinOffset := Max(MinOffset, Images.Width + AddWidth);
{$ENDIF}
        if not ShowCheck and (Assigned(Glyph) or (MinOffset > 0)) then
            if Buttons = bsOffice then Left := 1
            else Left := GetMarginOffset;
{$IFNDEF VER80}
        if UseImages then begin
            W := Images.Width + AddWidth;
            if W < Integer(MinOffset) then W := MinOffset;
            BtnRect := Bounds(Rect.Left + Left - 1, Rect.Top, W + 2,
            Rect.Bottom - Rect.Top);
            if DrawLowered then DrawGlyphCheck(BtnRect)
            else if (mdSelected in State) and IsPopup and (Buttons = bsOffice) and not ShowCheck then begin
                Frame3D(Canvas, BtnRect, clBtnHighlight, GrayColor, 1);
            end;
            if (mdDisabled in State) then
                ImageListDrawDisabled(Images, Canvas, Rect.Left + Left + (W - Images.Width) div 2,
                                      (Rect.Bottom + Rect.Top - Images.Height) div 2, ImageIndex,
                                      clBtnHighlight, GrayColor, DrawHighlight)
            else ImageList_Draw(Images.Handle, ImageIndex, Canvas.Handle, Rect.Left + Left + (W - Images.Width) div 2,
                                (Rect.Bottom + Rect.Top - Images.Height) div 2, ILD_NORMAL);
            Inc(Left, W + GetMarginOffset);
        end else
{$ENDIF}
            if Assigned(Glyph) and not Glyph.Empty and (Item.Caption <> Separator) then begin
                W := Glyph.Width;
                if (Glyph is TBitmap) and (NumGlyphs in [2..5]) then W := W div NumGlyphs;
                W := Max(W + AddWidth, MinOffset);
{$IFNDEF VER80}
                if not (Glyph is TIcon) then
{$ENDIF}
                begin
                    BtnRect := Bounds(Rect.Left + Left - 1, Rect.Top, W + 2,
                    Rect.Bottom - Rect.Top);
                    if DrawLowered then DrawGlyphCheck(BtnRect)
                    else if (mdSelected in State) and IsPopup and (Buttons = bsOffice) and not ShowCheck then begin
                        Frame3D(Canvas, BtnRect, clBtnHighlight, GrayColor, 1);
                    end;
                end;
                if Glyph is TBitmap then begin
                    if (NumGlyphs in [2..5]) then begin
                        I := 0;
                        if (mdDisabled in State) then I := 1
                        else if (mdChecked in State) then I := 3
                        else if (mdSelected in State) then I := 2;
                        if I > NumGlyphs - 1 then I := 0;
                        Bmp := TBitmap.Create;
                        try
                            AssignBitmapCell(Glyph, Bmp, NumGlyphs, 1, I);
                            DrawMenuBitmap(Canvas, Rect.Left + Left + (W - Bmp.Width) div 2,
                                           (Rect.Bottom + Rect.Top - Bmp.Height) div 2, Bmp, DrawLowered,
                                           State - [mdDisabled]);
                        finally
                            Bmp.Free;
                        end;
                    end else DrawMenuBitmap(Canvas, Rect.Left + Left + (W - Glyph.Width) div 2,
                                            (Rect.Bottom + Rect.Top - Glyph.Height) div 2, TBitmap(Glyph),
                                            DrawLowered, State);
                    Inc(Left, W + GetMarginOffset);
                end
{$IFNDEF VER80}
                else if Glyph is TIcon then begin
                    Ico := CreateRealSizeIcon(TIcon(Glyph));
                    try
                        GetIconSize(Ico, W, H);
                        I := Max(W + AddWidth, MinOffset);
                        BtnRect := Bounds(Rect.Left + Left - 1, Rect.Top, I + 2, Rect.Bottom - Rect.Top);
                        if DrawLowered then DrawGlyphCheck(BtnRect)
                        else if (mdSelected in State) and IsPopup and (Buttons = bsOffice) and not ShowCheck then begin
                            Frame3D(Canvas, BtnRect, clBtnHighlight, GrayColor, 1);
                        end;
                        DrawIconEx(Canvas.Handle, Rect.Left + Left + (I - W) div 2,
                                    (Rect.Top + Rect.Bottom - H) div 2, Ico, W, H, 0, 0, DI_NORMAL);
                                    Inc(Left, I + GetMarginOffset);
                    finally
                        DestroyIcon(Ico);
                    end;
                end
{$ENDIF}
                else begin
                    Canvas.Draw(Rect.Left + Left + (W - Glyph.Width) div 2,
                                (Rect.Bottom + Rect.Top - Glyph.Height) div 2, Glyph);
                    Inc(Left, W + GetMarginOffset);
                end;
            end else if (MinOffset > 0) then begin
                BtnRect := Bounds(Rect.Left + Left - 1, Rect.Top, MinOffset + 2, Rect.Bottom - Rect.Top);
                if DrawLowered then begin
                    DrawGlyphCheck(BtnRect);
                    CheckSize := GetMenuCheckMarkDimensions;
                    DrawCheckImage(BtnRect.Left + 2 + (MinOffset - LoWord(CheckSize)) div 2,
                                    (Rect.Bottom + Rect.Top - HiWord(CheckSize)) div 2 + 1);
                end else if (mdSelected in State) and IsPopup and (Buttons = bsOffice) and not ShowCheck then begin
                    Frame3D(Canvas, BtnRect, clBtnHighlight, GrayColor, 1);
                end;
                Inc(Left, MinOffset + GetMarginOffset);
            end;
            if Item.Caption = Separator then begin
                LineTop := (Rect.Top + Rect.Bottom) div 2 - 1;
                if NewStyleControls then begin
                    Canvas.Pen.Width := 1;
        MenuLine(Canvas, clBtnShadow, Rect.Left, LineTop, Rect.Right, LineTop);
        MenuLine(Canvas, clBtnHighlight, Rect.Left, LineTop + 1, Rect.Right, LineTop + 1);
                end
            else begin
                Canvas.Pen.Width := 2;
                MenuLine(Canvas, clMenuText, Rect.Left, LineTop + 1, Rect.Right, LineTop + 1);
            end;
        end else begin
            MaxWidth := Canvas.TextWidth(DelChars(Item.Caption, '&') + Tab);
            if (Item.Parent <> nil) and (Item.ShortCut <> scNone) then begin
                for I := 0 to Item.Parent.Count - 1 do
                MaxWidth := Max(Canvas.TextWidth(DelChars(Item.Parent.Items[I].Caption, '&') + Tab), MaxWidth);
            end;
            Canvas.Brush.Style := bsClear;
            LineTop := (Rect.Bottom + Rect.Top - Canvas.TextHeight('Ay')) div 2;
            MenuTextOut(Rect.Left + Left, LineTop, Item.Caption, DT_EXPANDTABS or DT_LEFT or DT_SINGLELINE);
            if (Item.ShortCut <> scNone) and (Item.Count = 0) and IsPopup then begin
                MenuTextOut(Rect.Left + Left + MaxWidth, LineTop, ShortCutToText(Item.ShortCut), DT_EXPANDTABS or DT_LEFT or DT_SINGLELINE);
            end;
        end;
    end;
end;

procedure MenuMeasureItem(AMenu: TMenu; Item: TMenuItem; Canvas: TCanvas;
  ShowCheck: Boolean; Glyph: TGraphic; NumGlyphs: Integer; var ItemWidth,
  ItemHeight: Integer; MinOffset: Cardinal;
  Images: TImageList;
  ImageIndex: Integer);
var
  IsPopup: Boolean;
  W, H: Integer;
  Ico: HIcon;

  function GetTextWidth(Item: TMenuItem): Integer;
  var
    I, MaxW: Integer;
  begin
    if IsPopup then begin
      Result := Canvas.TextWidth(DelChars(Item.Caption, '&') + Tab);
      MaxW := Canvas.TextWidth(ShortCutToText(Item.ShortCut) + ' ');
      if (Item.Parent <> nil) and (Item.ShortCut <> scNone) then begin
        for I := 0 to Item.Parent.Count - 1 do
          with Item.Parent.Items[I] do begin
            Result := Max(Result, Canvas.TextWidth(DelChars(Caption, '&') + Tab));
            MaxW := Max(MaxW, Canvas.TextWidth(ShortCutToText(ShortCut) + ' '));
          end;
      end;
      Result := Result + MaxW;
      if Item.Count > 0 then Inc(Result, Canvas.TextWidth(Tab));
    end
    else Result := Canvas.TextWidth(DelChars(Item.Caption, '&'));
  end;

begin
  IsPopup := IsItemPopup(Item);
  ItemHeight := GetDefItemHeight;
  if IsPopup then begin
    ItemWidth := GetMarginOffset * 2;
    if Assigned(Images) then
      MinOffset := Max(MinOffset, Images.Width + AddWidth);
  end
  else begin
    ItemWidth := 0;
    MinOffset := 0;
  end;
  Inc(ItemWidth, GetTextWidth(Item));
  if IsPopup and ShowCheck then
    Inc(ItemWidth, LoWord(GetMenuCheckMarkDimensions));
  if Item.Caption = Separator then begin
    ItemHeight := Max(Canvas.TextHeight(Separator) div 2, 9);
  end
  else begin
    ItemHeight := Max(ItemHeight, Canvas.TextHeight(Item.Caption));
    if Assigned(Images) and (IsPopup or ((ImageIndex >= 0) and
      (ImageIndex < Images.Count))) then
    begin
      Inc(ItemWidth, Max(Images.Width + AddWidth, MinOffset));
      if not IsPopup then Inc(ItemWidth, GetMarginOffset);
      if (ImageIndex >= 0) and (ImageIndex < Images.Count) then
        ItemHeight := Max(ItemHeight, Images.Height + AddHeight);
    end else
    if Assigned(Glyph) and not Glyph.Empty then begin
      W := Glyph.Width;
      if (Glyph is TBitmap) and (NumGlyphs in [2..5]) then
        W := W div NumGlyphs;
      H := Glyph.Height;
      if Glyph is TIcon then begin
        Ico := CreateRealSizeIcon(TIcon(Glyph));
        try
          GetIconSize(Ico, W, H);
        finally
          DestroyIcon(Ico);
        end;
      end;
      W := Max(W + AddWidth, MinOffset);
      Inc(ItemWidth, W);
      if not IsPopup then Inc(ItemWidth, GetMarginOffset);
      ItemHeight := Max(ItemHeight, H + AddHeight);
    end
    else if MinOffset > 0 then begin
      Inc(ItemWidth, MinOffset);
      if not IsPopup then Inc(ItemWidth, GetMarginOffset);
    end;
  end;
end;

{ TPopupList }
type
  TPopupList = class(TList)
  private
    procedure WndProc(var Message: TMessage);
  public
    Window: HWND;
    procedure Add(Popup: TPopupMenu);
    procedure Remove(Popup: TPopupMenu);
  end;

var
  PopupList: TPopupList = nil;

procedure TPopupList.WndProc(var Message: TMessage);
var
  I: Integer;
  MenuItem: TMenuItem;
  FindKind: TFindItemKind;
  ContextID: Integer;
  Handled: Boolean;
begin
  try
    case Message.Msg of
      WM_MEASUREITEM, WM_DRAWITEM:
        for I := 0 to Count - 1 do begin
          Handled := False;
          TMxPopupMenu(Items[I]).WndMessage(nil, Message, Handled);
          if Handled then Exit;
        end;
      WM_COMMAND:
        for I := 0 to Count - 1 do
          if TMxPopupMenu(Items[I]).DispatchCommand(Message.wParam) then Exit;
      WM_INITMENUPOPUP:
        for I := 0 to Count - 1 do
          with TWMInitMenuPopup(Message) do
            if TMxPopupMenu(Items[I]).DispatchPopup(MenuPopup) then Exit;
      WM_MENUSELECT:
        with TWMMenuSelect(Message) do begin
          FindKind := fkCommand;
          if MenuFlag and MF_POPUP <> 0 then begin
            FindKind := fkHandle;
            ContextId := GetSubMenu(Menu, IDItem);
          end
          else ContextId := IDItem;
          for I := 0 to Count - 1 do begin
            MenuItem := TMxPopupMenu(Items[I]).FindItem(ContextId, FindKind);
            if MenuItem <> nil then begin
              Application.Hint := MenuItem.Hint;
              with TMxPopupMenu(Items[I]) do
                if FCursor <> crDefault then begin
                  if (MenuFlag and MF_HILITE <> 0) then
                    SetCursor(Screen.Cursors[FCursor])
                  else SetCursor(Screen.Cursors[crDefault]);
                end;
              Exit;
            end;
          end;
          Application.Hint := '';
        end;
      WM_MENUCHAR:
        for I := 0 to Count - 1 do
          with TMxPopupMenu(Items[I]) do
            if (Handle = HMenu(Message.LParam)) or
              (FindItem(Message.LParam, fkHandle) <> nil) then
            begin
              ProcessMenuChar(TWMMenuChar(Message));
              Exit;
            end;
      WM_HELP:
        with PHelpInfo(Message.LParam)^ do begin
          for I := 0 to Count - 1 do
            if TMxPopupMenu(Items[I]).Handle = hItemHandle then begin
              ContextID := TMenu(Items[I]).GetHelpContext(iCtrlID, True);
              if ContextID = 0 then
                ContextID := TMenu(Items[I]).GetHelpContext(hItemHandle, False);
              if Screen.ActiveForm = nil then Exit;
              if (biHelp in Screen.ActiveForm.BorderIcons) then
                Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID)
              else
                Application.HelpContext(ContextID);
              Exit;
            end;
        end;
    end;
    with Message do Result := DefWindowProc(Window, Msg, wParam, lParam);
  except
    Application.HandleException(Self);
  end;
end;

procedure TPopupList.Add(Popup: TPopupMenu);
begin
  if Count = 0 then Window := AllocateHWnd(WndProc);
  inherited Add(Popup);
end;

procedure TPopupList.Remove(Popup: TPopupMenu);
begin
  inherited Remove(Popup);
  if Count = 0 then DeallocateHWnd(Window);
end;

{ TMxPopupMenu }
constructor TMxPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if PopupList = nil then
    PopupList := TPopupList.Create;
  FMarginStartColor := clBlue;
  FMarginEndColor := clBlack;
  FBKColor := clBtnFace;
  FSelColor := clBtnShadow;
  FSelFontColor := clHighlightText;
  FFontColor := clMenuText;
  FSepHColor := clBtnShadow;
  FSepLColor := clBtnHighlight;
  FShowCheckMarks := True;
  FCanvas := TControlCanvas.Create;
  FCursor := crDefault;
  PopupList.Add(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FPopupPoint := Point(-1, -1);
end;

destructor TMxPopupMenu.Destroy;
begin
  FImageChangeLink.Free;
  SetStyle(msStandard);
  PopupList.Remove(Self);
  FCanvas.Free;
  inherited Destroy;
end;

procedure TMxPopupMenu.Loaded;
begin
  inherited Loaded;
  if IsOwnerDrawMenu then RefreshMenu(True);
end;

procedure TMxPopupMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = FImages then SetImages(nil);
  end;
end;

procedure TMxPopupMenu.ImageListChange(Sender: TObject);
begin
  if Sender = FImages then RefreshMenu(IsOwnerDrawMenu);
end;

procedure TMxPopupMenu.SetImages(Value: TImageList);
var
  OldOwnerDraw: Boolean;
begin
  OldOwnerDraw := IsOwnerDrawMenu;
  if FImages <> nil then FImages.UnregisterChanges(FImageChangeLink);
  FImages := Value;
  if Value <> nil then begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  if IsOwnerDrawMenu <> OldOwnerDraw then RefreshMenu(not OldOwnerDraw);
end;

function FindPopupControl(const Pos: TPoint): TControl;
var
  Window: TWinControl;
begin
  Result := nil;
  Window := FindVCLWindow(Pos);
  if Window <> nil then begin
    Result := Window.ControlAtPos(Pos, False);
    if Result = nil then Result := Window;
  end;
end;

procedure TMxPopupMenu.SetBiDiModeFromPopupControl;
var
  AControl: TControl;
begin
  if not SysLocale.MiddleEast then Exit;
  if FParentBiDiMode then begin
    AControl := FindPopupControl(FPopupPoint);
    if AControl <> nil then
      BiDiMode := AControl.BiDiMode
    else
      BiDiMode := Application.BiDiMode;
  end;
end;

function TMxPopupMenu.UseRightToLeftAlignment: Boolean;
var
  AControl: TControl;
begin
  Result := False;
  if not SysLocale.MiddleEast then Exit;
  if FParentBiDiMode then begin
    AControl := FindPopupControl(FPopupPoint);
    if AControl <> nil then
      Result := AControl.UseRightToLeftAlignment
    else
      Result := Application.UseRightToLeftAlignment;
  end
  else Result := (BiDiMode = bdRightToLeft);
end;

procedure TMxPopupMenu.Popup(X, Y: Integer);
const
  Flags: array[Boolean, TPopupAlignment] of Word =
    ((TPM_LEFTALIGN, TPM_RIGHTALIGN, TPM_CENTERALIGN),
     (TPM_RIGHTALIGN, TPM_LEFTALIGN, TPM_CENTERALIGN));
  Buttons: array[TTrackButton] of Word = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
var
  FOnPopup: TNotifyEvent;
begin
  FPopupPoint := Point(X, Y);
  FParentBiDiMode := ParentBiDiMode;
  try
    TrackPopupMenu(Items.Handle, Flags[UseRightToLeftAlignment, Alignment] or Buttons[TrackButton],
                    X, Y, 0, PopupList.Window, nil);
  finally
    ParentBiDiMode := FParentBiDiMode;
  end;
end;

procedure TMxPopupMenu.Refresh;
begin
  RefreshMenu(IsOwnerDrawMenu);
end;

function TMxPopupMenu.IsOwnerDrawMenu: Boolean;
begin
  Result := (FStyle <> msStandard)
    or (Assigned(FImages) and (FImages.Count > 0));
end;

procedure TMxPopupMenu.RefreshMenu(AOwnerDraw: Boolean);
begin
  Self.OwnerDraw := AOwnerDraw and not (csDesigning in ComponentState);
end;

procedure TMxPopupMenu.SetStyle(Value: TMxMenuStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    RefreshMenu(IsOwnerDrawMenu);
  end;
end;

procedure TMxPopupMenu.DefaultDrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
var
  Graphic: TGraphic;
  BackColor: TColor;
  NumGlyphs {$IFNDEF VER80}, ImageIndex {$ENDIF}: Integer;
begin
  if Canvas.Handle <> 0 then begin
    Graphic := nil;
    BackColor := Canvas.Brush.Color;
    NumGlyphs := 1;
    GetItemParams(Item, State, Canvas.Font, BackColor, Graphic, NumGlyphs);
{$IFNDEF VER80}
{$IFDEF RX_D4}
    ImageIndex := Item.ImageIndex;
{$ELSE}
    ImageIndex := -1;
{$ENDIF}
    GetImageIndex(Item, State, ImageIndex);
{$ENDIF}
    DrawMenuItem(Self, Item, Graphic, NumGlyphs, Canvas, FShowCheckMarks,
      BtnStyle(Style), Rect, FMinTextOffset, State
      {$IFNDEF VER80}, FImages, ImageIndex {$ENDIF});
  end;
end;

procedure TMxPopupMenu.DrawItem(Item: TMenuItem; Rect: TRect;
  State: TMenuOwnerDrawState);
var
  Graphic: TGraphic;
  BackColor: TColor;
  NumGlyphs {$IFNDEF VER80}, ImageIndex {$ENDIF}: Integer;
begin
  if Canvas.Handle <> 0 then begin
    Graphic := nil;
    BackColor := Canvas.Brush.Color;
    NumGlyphs := 1;
    GetItemParams(Item, State, Canvas.Font, BackColor, Graphic, NumGlyphs);
    if BackColor <> clNone then begin
      Canvas.Brush.Color := BackColor;
      Canvas.FillRect(Rect);
    end;
    if Assigned(FOnDrawItem) then FOnDrawItem(Self, Item, Rect, State)
    else begin
{$IFNDEF VER80}
{$IFDEF RX_D4}
      ImageIndex := Item.ImageIndex;
{$ELSE}
      ImageIndex := -1;
{$ENDIF}
      GetImageIndex(Item, State, ImageIndex);
{$ENDIF}
      DrawMenuItem(Self, Item, Graphic, NumGlyphs, Canvas, FShowCheckMarks,
        BtnStyle(Style), Rect, FMinTextOffset, State
        {$IFNDEF VER80}, FImages, ImageIndex {$ENDIF});
    end;
  end;
end;

procedure TMxPopupMenu.MeasureItem(Item: TMenuItem; var Width, Height: Integer);
begin
  if Assigned(FOnMeasureItem) then FOnMeasureItem(Self, Item, Width, Height)
end;

procedure TMxPopupMenu.WndMessage(Sender: TObject; var AMsg: TMessage;
  var Handled: Boolean);
begin
  if IsOwnerDrawMenu then MenuWndMessage(Self, AMsg, Handled);
end;

procedure TMxPopupMenu.GetItemParams(Item: TMenuItem; State: TMenuOwnerDrawState;
  AFont: TFont; var Color: TColor; var Graphic: TGraphic; var NumGlyphs: Integer);
begin
  if Assigned(FOnGetItemParams) then
    FOnGetItemParams(Self, Item, State, AFont, Color, Graphic, NumGlyphs)
  else
  begin
    Color := FBKColor;
  end;
  if (Item <> nil) and (Item.Caption = Separator) then Graphic := nil;
end;

procedure TMxPopupMenu.GetImageIndex(Item: TMenuItem; State: TMenuOwnerDrawState;
  var ImageIndex: Integer);
begin
  if Assigned(FImages) and (Item <> nil) and (Item.Caption <> Separator) and
    Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, Item, State, ImageIndex);
end;

procedure TMxPopupMenu.DefaultDrawMargin(ARect: TRect; StartColor,
  EndColor: TColor);
var
  R: Integer;
begin
  with ARect do begin
    if NewStyleControls then R := Right// - 3
    else R := Right;
    GradientFillRect(Canvas, Rect(Left, Top, R, Bottom), StartColor,
      EndColor, fdTopToBottom, 32);
    if NewStyleControls then begin
//      MenuLine(Canvas, FSepHColor, Right-1, Top, Right-1, Bottom);
      MenuLine(Canvas, FSepLColor, Right, Top, Right, Bottom);
    end;
  end;
end;

procedure TMxPopupMenu.DrawMargin(ARect: TRect);
begin
  if Assigned(FOnDrawMargin) then FOnDrawMargin(Self, ARect)
  else begin
    DefaultDrawMargin(ARect, FMarginStartColor, FMarginEndColor);
  end;
end;

procedure TMxPopupMenu.WMDrawItem(var Message: TWMDrawItem);
var
  State: TMenuOwnerDrawState;
  SaveIndex: Integer;
  Item: TMenuItem;
  MarginRect: TRect;
begin
  with Message.DrawItemStruct^ do begin
    State := TMenuOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
    Item := TMenuItem(Pointer(itemData));
    if Assigned(Item) and
      (FindItem(Item.Command, fkCommand) = Item) then
    begin
      SaveIndex := SaveDC(hDC);
      try
        FCanvas.Handle := hDC;
        if (Item.Parent = Self.Items) and (FLeftMargin > 0) then
          if (itemAction = ODA_DRAWENTIRE) then begin
            MarginRect := FCanvas.ClipRect;
            MarginRect.Left := 0;
            MarginRect.Right := FLeftMargin;
            DrawMargin(MarginRect);
          end;
        SetDefaultMenuFont(FCanvas.Font);
        FCanvas.Font.Color := FFontColor;
        FCanvas.Brush.Color := clMenu;
        if mdDefault in State then
          FCanvas.Font.Style := FCanvas.Font.Style + [fsBold];
        if (mdSelected in State) and
          not (Style in [msBtnLowered, msBtnRaised]) then
        begin
          FCanvas.Brush.Color := clHighlight;
          FCanvas.Font.Color := FSelFontColor;
        end;
        if (Item.Parent = Self.Items) then
          Inc(rcItem.Left, LeftMargin + 1);
        with rcItem do
          IntersectClipRect(FCanvas.Handle, Left, Top, Right, Bottom);
        DrawItem(Item, rcItem, State);
        FCanvas.Handle := 0;
      finally
        RestoreDC(hDC, SaveIndex);
      end;
    end;
  end;
end;

procedure TMxPopupMenu.WMMeasureItem(var Message: TWMMeasureItem);
var
  Item: TMenuItem;
  Graphic: TGraphic;
  BackColor: TColor;
  NumGlyphs, ImageIndex: Integer;
begin
  with Message.MeasureItemStruct^ do begin
    Item := TMenuItem(Pointer(itemData));
    if Assigned(Item) and (FindItem(Item.Command, fkCommand) = Item) then
    begin
      FCanvas.Handle := GetDC(0);
      try
        SetDefaultMenuFont(FCanvas.Font);
        if Item.Default then
          FCanvas.Font.Style := FCanvas.Font.Style + [fsBold];
        Graphic := nil;
        BackColor := FBKColor;//Canvas.Brush.Color;
        NumGlyphs := 1;
        GetItemParams(Item, [], FCanvas.Font, BackColor, Graphic, NumGlyphs);
        ImageIndex := Item.ImageIndex;
        GetImageIndex(Item, [], ImageIndex);
        MenuMeasureItem(Self, Item, FCanvas, FShowCheckMarks, Graphic,
          NumGlyphs, Integer(itemWidth), Integer(itemHeight), FMinTextOffset,
          FImages, ImageIndex);
        MeasureItem(Item, Integer(itemWidth), Integer(itemHeight));
        if (Item.Parent = Self.Items) then
          Inc(itemWidth, LeftMargin + 1);
      finally
        ReleaseDC(0, FCanvas.Handle);
        FCanvas.Handle := 0;
      end;
    end;
  end;
end;

initialization
  PopupList := nil;
finalization
  if PopupList <> nil then PopupList.Free;
end.
