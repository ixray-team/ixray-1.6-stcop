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

05/01/2002

  Fixed design-time setting of Style property

07/14/2001

  IsHTML property added.
  Now these controls act as ancestors for ElDBListBox and ElDBComboBox

============================== Version 2.78 ====================================

03/23/2001

  Images were not drawn. Fixed. 

============================== Version 2.75 ====================================

10/26/2000

  Fixed item height when adding a combo box to the form in design-time

*)

unit HTMLLbx;  { TElHTMLListBox component. }

{ HTML-enabled listbox }

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  Graphics,
  Controls,
  Forms,
  Messages,
  Dialogs,
  Menus,
  StdCtrls,
  ExtCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  {$else}
  Types,
  QControls,
  QGraphics,
  QStdCtrls,
  Qt,
  QForms,
  {$endif}
  ElACtrls,

  SysUtils,
  Classes,
  HTMLRender,
  ElHintWnd,
  ElStrUtils,
  ElImgFrm;

type

  TCustomElHTMLListBox = class(TElAdvancedListBox)
  private
    { Private declarations }
    FDummyEvent : TNotifyEvent;  { Dummy field for hiding events. }

    FRender : TElHTMLRender;
    FOnLinkClick : TElHTMLLinkClickEvent;
    FOnImageNeeded : TElHTMLImageNeededEvent;
    FIsHTML: Boolean;
    FCursor: TCursor;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
    // function GetItemHeight(Index: Integer): Integer; virtual;
  protected
    FStyle: TListBoxStyle;
    function GetItemWidth(Index: Integer): Integer; override;
    {$ifndef CLX_USED}
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;
    {$else}
    procedure MeasureItem(Control: TWinControl; Index: Integer;
      var Height, Width: Integer); override;
    function DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState): Boolean; override;
    {$endif}        
    procedure TriggerLinkClickEvent(Sender : TObject; HRef : TElFString); virtual;
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image :
        TBitmap); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetCursor(newValue : TCursor); virtual;
    function CreateHintWindow: THintWindow; override;
    procedure SetStyle(Value: TListBoxStyle);
    procedure SetIsHTML(Value: Boolean); virtual;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    procedure Loaded; override;
    destructor Destroy; override;
  published
    { Published properties and events }
    property OnLinkClick : TElHTMLLinkClickEvent read FOnLinkClick write FOnLinkClick;
    property OnImageNeeded : TElHTMLImageNeededEvent read FOnImageNeeded write FOnImageNeeded;
    property OnDrawItem : TNotifyEvent read FDummyEvent;  { Hidden Event }
    property OnMeasureItem : TNotifyEvent read FDummyEvent;  { Hidden Event }
    property IsHTML: Boolean read FIsHTML write SetIsHTML;
    property Cursor: TCursor read FCursor write SetCursor;
    property Style: TListBoxStyle read FStyle write SetStyle default 
        lbOwnerDrawVariable;
  end;  { TElHTMLListBox }

  TCustomElHTMLComboBox = class(TElAdvancedComboBox)
  private
    { Private declarations }
    FDummyEvent : TNotifyEvent;  { Dummy field for hiding events. }

    FRender : TElHTMLRender;
    FOnLinkClick : TElHTMLLinkClickEvent;
    FOnImageNeeded : TElHTMLImageNeededEvent;
    FIsHTML: Boolean;
  protected
    {$ifndef CLX_USED}
    FStyle: TComboBoxStyle;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;
    {$else}
    procedure MeasureItem(Control: TWinControl; Index: Integer;
      var Height, Width: Integer); override;
    function DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState): Boolean; override;
    {$endif}

    procedure TriggerLinkClickEvent(Sender : TObject; HRef : TElFString); virtual;
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image : TBitmap); virtual;
    procedure SetStyle(Value: TComboBoxStyle); {$ifdef VCL_4_USEd}reintroduce;{$endif}
    procedure SetIsHTML(Value: Boolean); virtual;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    { Published properties and events }
    property OnLinkClick : TElHTMLLinkClickEvent read FOnLinkClick write FOnLinkClick;
    property OnImageNeeded : TElHTMLImageNeededEvent read FOnImageNeeded write FOnImageNeeded;
    property OnDrawItem : TNotifyEvent read FDummyEvent;  { Hidden Event }
    property OnMeasureItem : TNotifyEvent read FDummyEvent;  { Hidden Event }
    property IsHTML: Boolean read FIsHTML write SetIsHTML;
    property Style: TComboBoxStyle read FStyle write SetStyle default 
        csOwnerDrawVariable;
  end;

  TElHTMLListBox = class(TCustomElHTMLListBox)
  end;

  TElHTMLComboBox = class(TCustomElHTMLComboBox)
  end;

implementation

const SideOffset = 2;

{$ifndef CLX_USED}
procedure TCustomElHTMLListBox.MeasureItem(Index: Integer; var Height: Integer);
{$else}
procedure TCustomElHTMLListBox.MeasureItem(Control: TWinControl; Index: Integer;
    var Height, Width: Integer);
{$endif}
{$ifdef MSWINDOWS}
var R : TRect;
{$endif}
{$ifndef MSWINDOWS}
var PX : TSize;
    AL : Integer;
{$endif}
begin
  if IsHTML then
  begin
    if Index >= Items.Count then
    begin
      Height := ItemHeight;
      {$ifndef CLX_USED}
      Width := 0;
      {$endif}
    end
    else
    begin
      FRender.Data.DefaultStyle := Font.Style;
      FRender.Data.DefaultHeight:= Font.Height;
      FRender.Data.DefaultFont := Font.Name;
      FRender.Data.Charset := Font.Charset;
      FRender.PrepareText(Items[Index], 0, false);
      Height := FRender.Data.TextSize.cy;
      {$ifdef CLX_USED}
      Width := FRender.Data.TextSize.cx + SideOffset * 2;
      {$endif}
    end;
  end
  else
  begin
    {$ifdef MSWINDOWS}
    R := Rect(0, 0, MaxInt, MaxInt);
    DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), R,
             DT_LEFT or DT_TOP or DT_NOPREFIX or DT_CALCRECT);
    Height := R.Bottom;
    {$ifdef CLX_USED}
    Width := R.Right + 4;
    {$endif}
    {$else}
    if not FMultiline then
      AL := Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft)
    else
      AL := Integer(AlignmentFlags_AlignLeft);
    R := Rect(0, 0, MaxInt, MaxInt);
    PX := Canvas.TextExtent(Caption, Al);
    Width := PX.cx;
    Height:= PX.cy;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElHTMLListBox.DrawItem(Index: Integer; Rect: TRect; State: 
    {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
{$else}
function TCustomElHTMLListBox.DrawItem(Index: Integer; Rect: TRect; State: 
    TOwnerDrawState): Boolean;
{$endif}
var C : TColor;
begin
  if IsHTML then
  begin
    if ([odFocused, odSelected] * State) <> [] then
    begin
      FRender.Data.DefaultBgColor := clHighlight;
      FRender.Data.DefaultColor := clHighlightText;
    end else
    if ([odGrayed, odDisabled] * State) <> [] then
    begin
      FRender.Data.DefaultBgColor := clBtnFace;
      FRender.Data.DefaultColor := clBtnShadow;
    end
    else
    begin
      FRender.Data.DefaultBgColor := Color;
      FRender.Data.DefaultColor := Font.Color;
    end;
    C := Canvas.Brush.Color;
    Canvas.Brush.Color := FRender.Data.DefaultBgColor;
    Canvas.FillRect(Rect);
    Canvas.Brush.Color := C;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.DefaultHeight:= Font.Height;
    FRender.Data.DefaultFont := Font.Name;
    FRender.Data.Charset := Font.Charset;
    {$ifdef CLX_USED}
    Inc(Rect.Left);
    Dec(Rect.Right);
    {$endif}
    FRender.PrepareText(Items[Index], 0, false);
    FRender.DrawText(Canvas, Point(0, 0), Rect, clNone);
    {$ifdef CLX_USED}
    Dec(Rect.Left);
    Inc(Rect.Right);
    {$endif}
    if odFocused in State then
    begin
      {$ifdef MSWINDOWS}
      Canvas.DrawFocusRect(Rect);
      {$else}
      QStyle_DrawFocusRect(Application.Style.Handle,
                                      Canvas.Handle,
                                      @Rect,
                                      QWidget_colorGroup(Handle),
                                      nil, false);
      {$endif}
    end;
  end
  else
  begin
    Canvas.Font.Assign(Font);
    if odSelected in State then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end
    else
      Canvas.Brush.Color := Color;
    Canvas.TextRect(Rect, Rect.Left, Rect.Top, Items[Index]);
    if odFocused in State then
    begin
      {$ifdef MSWINDOWS}
      Canvas.DrawFocusRect(Rect);
      {$else}
      QStyle_DrawFocusRect(Application.Style.Handle,
                                      Canvas.Handle,
                                      @Rect,
                                      QWidget_colorGroup(Handle),
                                      nil, false);
      {$endif}
    end;
  end;
  {$ifdef CLX_USED}
  result := true;
  {$endif}
end;

{ Event triggers: }
procedure TCustomElHTMLListBox.TriggerLinkClickEvent(Sender : TObject; HRef : 
    TElFString);
begin
  if (assigned(FOnLinkClick)) then
    FOnLinkClick(Self, HRef );
end;  { TriggerLinkClickEvent }

procedure TCustomElHTMLListBox.TriggerImageNeededEvent(Sender : TObject; Src :
    TElFString; var Image : TBitmap);
begin
  if (assigned(FOnImageNeeded)) then
    FOnImageNeeded(Self, Src , Image );
end;  { TriggerImageNeededEvent }

procedure TCustomElHTMLListBox.Loaded;
begin
  inherited;
  {$ifdef MSWINDOWS}
  if HandleAllocated then
    RecreateWnd;
  {$endif}
end;

destructor TCustomElHTMLListBox.Destroy;
begin
  FRender.Free;
  inherited Destroy;
end;  { Destroy }

constructor TCustomElHTMLListBox.Create(AOwner : TComponent);
{ Creates an object of type TElHTMLListBox, and initializes properties. }
begin
  inherited Create(AOwner);
  inherited Style := lbOwnerDrawVariable;  { New default. }
  FRender := TElHTMLRender.Create;
  FRender.OnImageNeeded := TriggerImageNeededEvent;
end;  { Create }

procedure TCustomElHTMLListBox.SetIsHTML(Value: Boolean);
begin
  if FIsHTML <> Value then
  begin
    FIsHTML := Value;
    if FIsHTML then
      inherited Style := lbOwnerDrawVariable
    else
      inherited Style := FStyle;
    RecreateWnd;    
  end;
end;

procedure TCustomElHTMLListBox.CMMouseLeave(var Msg : TMessage);
begin
  inherited Cursor := FCursor;
  inherited;
end;  { CMMouseLeave }
procedure TCustomElHTMLListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var href : TElFString;
    i : integer;
    R : TRect;
begin
  begin
    i := SendMessage(Handle, LB_ITEMFROMPOINT, 0, MakeLParam(X,Y));
    SendMessage(Handle, LB_GETITEMRECT, i, Integer(@R));
    if PtInRect(R, Point(X,Y)) and (i >= 0) and (i < Items.Count) then
    begin
      FRender.Data.DefaultStyle := Font.Style;
      FRender.Data.DefaultHeight := Font.Height;
      FRender.Data.DefaultFont := Font.Name;
      FRender.Data.Charset := Font.Charset;
      FRender.PrepareText(Items[i], 0, false);

      if FRender.IsCursorOverLink(Point(X, Y - R.Top), Point(0, 0), Rect(0, 0, R.Right - R.Left, R.Bottom - R.Top), href) then
      begin
        TriggerLinkClickEvent(Self, href);
        exit;
      end;
    end;
  end;
  inherited;
end;

procedure TCustomElHTMLListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var href : TElFString;
    i : integer;
    R : TRect;
begin
  inherited;
  begin
    i := SendMessage(Handle, LB_ITEMFROMPOINT, 0, MakeLParam(X,Y));
    SendMessage(Handle, LB_GETITEMRECT, i, Integer(@R));
    if PtInRect(R, Point(X,Y)) and (i >= 0) and (i < Items.Count) then
    begin
      FRender.Data.DefaultStyle := Font.Style;
      FRender.Data.DefaultHeight := Font.Height;
      FRender.Data.DefaultFont := Font.Name;
      FRender.Data.Charset := Font.Charset;
      FRender.PrepareText(Items[i], 0, false);

      if FRender.IsCursorOverLink(Point(X, Y - R.Top), Point(0, 0), Rect(0, 0, R.Right - R.Left, R.Bottom - R.Top), href) then
         inherited Cursor := crHandPoint
      else
         inherited Cursor := FCursor;
    end;
  end;
end;  { MouseMove }

procedure TCustomElHTMLListBox.SetCursor(newValue : TCursor);
var P : TPoint;
begin
  if (FCursor <> newValue) then
  begin
    FCursor := newValue;
    GetCursorPos(P);
    P := ScreenToClient(P);
    SendMessage(Handle, WM_MOUSEMOVE, 0, MakeLParam(P.X, P.Y));
  end;  { if }
end;  { SetCursor }

function TCustomElHTMLListBox.GetItemWidth(Index: Integer): Integer;
begin
  if not IsHTML then
    result := inherited GetItemWidth(Index)
  else
  begin
    if Index >= Items.Count then
      Result := 0
    else
    begin
      FRender.Data.DefaultStyle := Font.Style;
      FRender.Data.DefaultHeight := Font.Height;
      FRender.Data.DefaultFont := Font.Name;
      FRender.Data.Charset := Font.Charset;
      FRender.PrepareText(Items[Index], 0, false);

      Result := FRender.Data.TextSize.cx + 4;
    end;
  end;
end;

function TCustomElHTMLListBox.CreateHintWindow: THintWindow;
begin
  Result := TElHintWindow.Create(nil);
{$ifdef HAS_HTML_RENDER}
  TElHintWindow(Result).IsHTML := IsHTML;
  TElHintWindow(Result).OnLinkClick := TriggerLinkClickEvent;
{$endif}
end;

procedure TCustomElHTMLListBox.SetStyle(Value: TListBoxStyle);
begin
  if (FStyle <> Value) and (Value in [Low(TListBoxStyle)..High(TListBoxStyle)]) then
  begin
    FStyle := Value;
    if not IsHTML then
    begin
      inherited Style := Value;
    end;
  end;
end;

(*
function TCustomElHTMLListBox.GetItemHeight(Index: Integer): Integer;
begin
  if not IsHTML then
    Result := ItemHeight
  else
  begin
    if Index >= Items.Count then
      Result := ItemHeight
    else
    begin
      FRender.Data.DefaultStyle := Font.Style;
      FRender.Data.DefaultSize := Font.Size;
      FRender.Data.DefaultFont := Font.Name;
      FRender.Data.Charset := Font.Charset;
      FRender.PrepareText(Items[Index], 0, false);
      Result := FRender.Data.TextSize.cy;
    end;
  end;
end;
*)

{$ifndef CLX_USED}
procedure TCustomElHTMLComboBox.MeasureItem(Index: Integer; var Height:
    Integer);
{$else}
procedure TCustomElHTMLComboBox.MeasureItem(Control: TWinControl; Index:
    Integer; var Height, Width: Integer);
{$endif}
{$ifdef MSWINDOWS}
var R : TRect;
{$endif}
{$ifndef MSWINDOWS}
var PX : TSize;
    AL : Integer;
{$endif}
begin
  if Index = -1 then
    Height := ItemHeight
  else
  if IsHTML then
  begin
    if Index >= Items.Count then
    begin
      Height := ItemHeight;
      {$ifndef CLX_USED}
      Width := 0;
      {$endif}
    end
    else
    begin
      FRender.Data.DefaultStyle := Font.Style;
      FRender.Data.DefaultHeight := Font.Height;
      FRender.Data.DefaultFont := Font.Name;
      FRender.Data.Charset := Font.Charset;
      FRender.PrepareText(Items[Index], 0, false);
      Height := FRender.Data.TextSize.cy;
      {$ifdef CLX_USED}
      Width := FRender.Data.TextSize.cx + SideOffset * 2;
      {$endif}
    end;
  end
  else
  begin
    {$ifdef MSWINDOWS}
    R := Rect(0, 0, MaxInt, MaxInt);
    DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), R,
             DT_LEFT or DT_TOP or DT_NOPREFIX or DT_CALCRECT);
    Height := R.Bottom;
    {$ifdef CLX_USED}
    Width := R.Right + 4;
    {$endif}
    {$else}
    if not FMultiline then
      AL := Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft)
    else
      AL := Integer(AlignmentFlags_AlignLeft);
    R := Rect(0, 0, MaxInt, MaxInt);
    PX := Canvas.TextExtent(Caption, Al);
    Width := PX.cx;
    Height:= PX.cy;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElHTMLComboBox.DrawItem(Index: Integer; Rect: TRect; State: 
    {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
{$else}
function TCustomElHTMLComboBox.DrawItem(Index: Integer; Rect: TRect; State: 
    TOwnerDrawState): Boolean;
{$endif}
var C : TColor;
begin
  if IsHTML then
  begin
    if ([odFocused, odSelected] * State) <> [] then
    begin
      FRender.Data.DefaultBgColor := clHighlight;
      FRender.Data.DefaultColor := clHighlightText;
    end else
    if ([odGrayed, odDisabled] * State) <> [] then
    begin
      FRender.Data.DefaultBgColor := clBtnFace;
      FRender.Data.DefaultColor := clBtnShadow;
    end
    else
    begin
      FRender.Data.DefaultBgColor := Color;
      FRender.Data.DefaultColor := Font.Color;
    end;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultFont := Font.Name;
    FRender.Data.Charset := Font.Charset;
    C := Canvas.Brush.Color;
    Canvas.Brush.Color := FRender.Data.DefaultBgColor;
    Canvas.FillRect(Rect);
    Canvas.Brush.Color := C;
    InflateRect(Rect, -SideOffset div 2, 0);
    {$ifdef CLX_USED}
    Inc(Rect.Left);
    Dec(Rect.Right);
    {$endif}
    FRender.PrepareText(Items[Index], 0, false);
    FRender.DrawText(Canvas, Point(0, 0), Rect, clNone);
    {$ifdef CLX_USED}
    Dec(Rect.Left);
    Inc(Rect.Right);
    {$endif}
  end
  else
  begin
    Canvas.Font.Assign(Font);
    if odSelected in State then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end
    else
      Canvas.Brush.Color := Color;
    Canvas.TextRect(Rect, Rect.Left, Rect.Top, Items[Index]);
    if odFocused in State then
    begin
      {$ifdef MSWINDOWS}
      Canvas.DrawFocusRect(Rect);
      {$else}
      QStyle_DrawFocusRect(Application.Style.Handle,
                                      Canvas.Handle,
                                      @Rect,
                                      QWidget_colorGroup(Handle),
                                      nil, false);
      {$endif}
    end;
  end;
  {$ifdef CLX_USED}
  result := true;
  {$endif}
end;

{ Event triggers: }
procedure TCustomElHTMLComboBox.TriggerLinkClickEvent(Sender : TObject; HRef :
    TElFString);
begin
  if (assigned(FOnLinkClick)) then
    FOnLinkClick(Self, HRef );
end;  { TriggerLinkClickEvent }

procedure TCustomElHTMLComboBox.TriggerImageNeededEvent(Sender : TObject; Src :
    TElFString; var Image : TBitmap);
begin
  if (assigned(FOnImageNeeded)) then
    FOnImageNeeded(Self, Src , Image );
end;  { TriggerImageNeededEvent }

procedure TCustomElHTMLComboBox.Loaded;
begin
  inherited;
  if IsHTML then
    inherited Style := csOwnerDrawVariable
  else
    inherited Style := Style;
  {$ifdef MSWINDOWS}
  if HandleAllocated then
    RecreateWnd;
  {$endif}
end;

destructor TCustomElHTMLComboBox.Destroy;
begin
  FRender.Free;
  inherited Destroy;
end;  { Destroy }

constructor TCustomElHTMLComboBox.Create(AOwner : TComponent);
{ Creates an object of type TElHTMLComboBox, and initializes properties. }
begin
  inherited Create(AOwner);
  //inherited Style := csOwnerDrawVariable;  { New default. }
  FRender := TElHTMLRender.Create;
  FRender.OnImageNeeded := TriggerImageNeededEvent;
end;  { Create }

procedure TCustomElHTMLComboBox.SetIsHTML(Value: Boolean);
begin
  if FIsHTML <> Value then
  begin
    FIsHTML := Value;
    if FIsHTML then
      inherited Style := csOwnerDrawVariable
    else
      inherited Style := FStyle;
    if ComponentState * [csLoading, csReading, csDestroying] = [] then
      RecreateWnd;
  end;
end;

procedure TCustomElHTMLComboBox.SetStyle(Value: TComboBoxStyle);
begin
  if (FStyle <> Value) and (Value in [Low(TComboBoxStyle)..High(TComboBoxStyle)]) then
  begin
    FStyle := Value;
    if not IsHTML then
    begin
      inherited Style := Value;
      if ComponentState * [csLoading, csReading, csDestroying] = [] then
        RecreateWnd;
    end;
  end;
end;



end.
