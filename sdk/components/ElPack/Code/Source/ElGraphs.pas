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

10/10/2001

  CLX adaptation complete

05/27/2001 (c) Akzhan Abdulin

  Fixed Color property design-time storing issue (clWindow not stored)

*)

unit ElGraphs;

{ Data graphs displaying }

interface

uses
  Classes,
  SysUtils,

  {$ifndef CLX_USED}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  extctrls,
  Printers,
{$ifdef VCL_6_USED}
Types,
{$endif}
  {$else}
  QGraphics,
  QControls,
  QForms,
  QExtCtrls,
  Qt,
  Types,
  QTypes,
  {$endif}
  ElVCLUtils,
  ElTools,
  ElQueue,
  ElImgFrm,
  ElCGControl;

type

  TElGraph = class;

  TDataEntry = class
  private
    FValues : TElQueue;
    FColor : TColor;
    FName : string; // for legend
    FMinGrid,
      FMaxGrid : integer; // for legend and fraph
    FOwner : TElGraph;
    FVisible : boolean;
    FAutoGrid : boolean;
    FFaults : integer;

    function GetLimit : integer;
    procedure SetLimit(newValue : Integer);
    procedure SetColor(value : TColor);
    procedure SetName(value : string);
    procedure SetMinGrid(newValue : Integer);
    procedure SetMaxGrid(newValue : Integer);
    procedure SetVisible(value : boolean);
    function GetValueCount : integer;
     { Removes the oldest values, that are beyond the limit }

  public
     { Calculates Min, Max and Average values for the data array }
    procedure CalcMinMax(var Min, Max, Avg : integer);
    constructor Create;
    destructor Destroy; override;
    procedure AddValue(value : integer);
    function GetValue(index : integer) : integer;

    procedure Reset;

    property Name : string read FName write SetName; { Public }
    property Owner : TElGraph read FOwner;
    property Values : TElQueue read FValues;
    property Value[index : integer] : integer read GetValue; default;
    property ValueCount : integer read GetValueCount;
    property Color : TColor read FColor write SetColor;
    property MinGrid : Integer read FMinGrid write SetMinGrid default 0; { Public }
    property MaxGrid : Integer read FMaxGrid write SetMaxGrid default 100; { Public }
    property Visible : boolean read FVisible write SetVisible default true;
    property Limit : Integer read GetLimit write SetLimit default 1000; { Public }
    property AutoGrid : boolean read FAutoGrid write FAutoGrid;
    property Faults : integer read FFaults;
  end;

//  TGraphType = (gtMoveCurrent, gtMoveAll);

  TElGraph = class(TElCustomGraphicControl)
  private
    FShowTimeouts : Boolean;
    FStatus : string;
    FColumnEntry : TDataEntry;
    FHGridLines : Integer;
    FVGridLines : Integer;
    FOnResize : TNotifyEvent;
    FLegendBkColor : TColor;
    FLegendWidth : Integer;
    FLegendAtRight : Boolean;

    FEntryList : TList;
    FShowLegend : Boolean;
    FShowMinMax : Boolean;
    FShowGrid : Boolean;
    FMinMaxEntry : TDataEntry;
    {$ifndef CLX_USED}
    FImgForm : TElImageForm;
    procedure SetImageForm(newValue : TElImageForm);
    {$endif}
    function GetDataList(index : integer) : TDataEntry;
    procedure SetShowLegend(newValue : Boolean);
    procedure SetShowMinMax(newValue : Boolean);
    function GetEntriesCount : Integer;
    procedure SetShowGrid(newValue : Boolean);
//    procedure SetGraphType( newValue : TGraphType ) ;
    procedure SetLegendAtRight(newValue : Boolean);
    procedure SetLegendWidth(newValue : Integer);
    procedure SetLegendBkColor(newValue : TColor);
    procedure SetMinMaxEntry(newValue : TDataEntry);
    procedure SetHGridLines(newValue : Integer);
    procedure SetVGridLines(newValue : Integer);
    function GetColumnEntry : TDataEntry;
    procedure SetColumnEntry(newValue : TDataEntry);
    {$ifndef CLX_USED}
    procedure WMEraseBkgnd(var Msg : TWMEraseBkgnd); message WM_ERASEBKGND;
    {$endif}
    procedure SetShowTimeouts(newValue : Boolean);
    function GetTransparent : Boolean;
    procedure SetTransparent(newValue : Boolean);
  protected
    { Protected declarations }
    function CreateEntry : TDataEntry; virtual;
    { Performs actual drawing }
    procedure DoDrawGraph(Surface : TCanvas); virtual;
    procedure Paint; override;

    { Returns the rectangle for legend }
    function GetLegendRect : TRect; virtual;
    { Returns rectangle for MinMaxInfo }
    function GetMinMaxRect : TRect; virtual;
    { Returns the rectangle for the graph }
    function GetMainRect : TRect; virtual;
    procedure TriggerResizeEvent; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$ifndef CLX_USED}
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    {$endif}
  public
    { Public declarations }
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : Integer); override;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    { Prints the graph }
    {$ifndef CLX_USED}
    procedure Print;
    {$endif}
    { Adds the entry to the list }
    function AddEntry : TDataEntry;
    function InsertEntry(index : integer) : TDataEntry;
    procedure DeleteEntry(index : integer);
    { Refreshes the graph }
    procedure Refresh;
    { Arrays of integer values for graph entry }
    property DataList[index : integer] : TDataEntry read GetDataList; { Public }
    { Ammount of entries }
    property EntriesCount : Integer read GetEntriesCount; { Public }
    property MinMaxEntry : TDataEntry read FMinMaxEntry write SetMinMaxEntry; { Public }
    { Entry, that has attribute "Column" }
    property ColumnEntry : TDataEntry read GetColumnEntry write SetColumnEntry; { Public }
  published
    { Does the graph show the legend? }
    property ShowLegend : Boolean read FShowLegend write SetShowLegend; { Published }
    { Show min, max and average values of the selected entry? }
    property ShowMinMax : Boolean read FShowMinMax write SetShowMinMax; { Published }
    { Show grid lines? }
    property ShowGrid : Boolean read FShowGrid write SetShowGrid; { Published }
//    property GraphType : TGraphType read FGraphType write SetGraphType ;  { Published }
    { Inherited properties: }
    property Align;
    property Canvas;
    property Color;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    { Inherited events: }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    { Legend is positioned at the right side of the graph }
    property LegendAtRight : Boolean read FLegendAtRight write SetLegendAtRight default true; { Published }
    { Legend panel width }
    property LegendWidth : Integer read FLegendWidth write SetLegendWidth default 100; { Published }
    { Background color for legend and MinMax Info }
    property LegendBkColor : TColor read FLegendBkColor write SetLegendBkColor; { Published }
    { Occurs when the graph is resized }
    property OnResize : TNotifyEvent read FOnResize write FOnResize;
    { Ammount of horizontal lines of the grid }
    property HGridLines : Integer read FHGridLines write SetHGridLines default 5; { Published }
    { Ammount of vertical lines of the grid }
    property VGridLines : Integer read FVGridLines write SetVGridLines default 0; { Public }
    property Status : string read FStatus write FStatus; { Published }
    property ShowTimeouts : Boolean read FShowTimeouts write SetShowTimeouts default True; { Published }
    property Transparent : Boolean read GetTransparent write SetTransparent;  { Published }
    {$ifndef CLX_USED}
    property ImageForm   : TElImageForm read FImgForm write SetImageForm;
    {$endif}
{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    {$ifndef CLX_USED}
    property DockOrientation;
    property Floating;
    property DragKind;
    {$endif}
{$ENDIF}
  end; { TElGraph }

implementation

procedure TDataEntry.SetVisible(value : boolean);
begin
  if FVisible = value then exit;
  FVisible := value;
  if FOwner <> nil then FOwner.Repaint;
end;

procedure TDataEntry.SetColor(value : TColor);
begin
  if FColor <> value then
  begin
    FColor := value;
    if (FOwner <> nil) and (FVisible) then FOwner.Repaint;
  end;
end;

procedure TDataEntry.SetName(value : string);
begin
  if FName <> value then
  begin
    FName := value;
    if (FOwner <> nil) and (FOwner.ShowLegend) and (FVisible) then FOwner.Repaint;
  end;
end;

constructor TDataEntry.Create;
begin
  inherited;
  FValues := TElQueue.Create;
  FVisible := true;
  FValues.Capacity := 1000;
  FMaxGrid := 100;
  FMinGrid := 0;
end;

destructor TDataEntry.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TDataEntry.AddValue(value : integer);
begin
  FValues.Add(pointer(value));
  if AutoGrid then
  begin
    if (Value > MaxGrid) and (Value <> -1) and (Value <> -2) then MaxGrid := value + (value div 10);
    if (Value < MinGrid) and (Value <> -1) and (Value <> -2) then MinGrid := value - (value div 10);
  end;
  if value = -1 then inc(FFaults);
end;

function TDataEntry.GetValue(index : integer) : integer;
begin
  result := integer(FValues[index]);
end;

procedure TDataEntry.SetMinGrid(newValue : Integer);
{ Sets data member FMinGrid to newValue. }
begin
  if FMinGrid <> newValue then
  begin
    FMinGrid := newValue;
    if (FOwner <> nil) and (FVisible) then FOwner.Repaint;
  end; { if }
end; { SetMinGrid }

procedure TDataEntry.SetMaxGrid(newValue : Integer);
{ Sets data member FMaxGrid to newValue. }
begin
  if FMaxGrid <> newValue then
  begin
    FMaxGrid := newValue;
    if (FOwner <> nil) and (FVisible) then FOwner.Repaint;
  end; { if }
end; { SetMaxGrid }

{ Calculates Min, Max and Average values for the data array }

procedure TDataEntry.CalcMinMax(var Min, Max, Avg : integer); { public }
var
  I, C, J : Integer;
begin
  Min := $7FFFFFFF;
  Max := 0;
  Avg := 0;
  J := 0;
  for I := 0 to ValueCount - 1 do
  begin
    c := Integer(FValues[i]);
    if (C <> -1) and (c <> -2) then
    begin
      if C > Max then Max := C;
      if C < Min then Min := C;
      Avg := AVG + C;
      inc(J);
    end;
  end;
  if J > 0 then
    Avg := Avg div J
  else
    Avg := -1;
  if Min = $7FFFFFFF then Min := 0;
end; { CalcMinMax }

function TDataEntry.GetValueCount : integer;
begin
  result := FValues.Count;
end;

function TDataEntry.GetLimit : integer;
begin
  result := FValues.Capacity;
end;

procedure TDataEntry.SetLimit(newValue : Integer);
begin
  if FValues.Capacity <> newValue then
  begin
    FValues.Capacity := newValue;
    //FOwner.Invalidate;
  end; { if }
end; { SetLimit }

procedure TDataEntry.Reset; { public }
begin
  FValues.Clear;
  FFaults := 0;
end; { Reset }

function TElGraph.GetDataList(index : integer) : TDataEntry;
{ Returns the value of data member FDataList[]. }
begin
  result := FEntryList[index];
end; { GetDataList }

{$ifndef CLX_USED}
procedure TElGraph.SetImageForm(newValue : TElImageForm);
begin
  if FImgForm <> newValue then
  begin
    {$ifdef VCL_5_USED}
    if FImgForm <> nil then
      FImgForm.RemoveFreeNotification(Self);
    {$endif}
    if newValue <> nil then
       newValue.FreeNotification(Self);
    FImgForm := newValue;
  end;
end;
{$endif}

procedure TElGraph.SetShowLegend(newValue : Boolean);
{ Sets data member FShowLegend to newValue. }
begin
  if FShowLegend <> newValue then
  begin
    FShowLegend := newValue;
    Repaint;
  end; { if }
end; { SetShowLegend }

procedure TElGraph.SetShowMinMax(newValue : Boolean);
{ Sets data member FShowMinMax to newValue. }
begin
  if FShowMinMax <> newValue then
  begin
    FShowMinMax := newValue;
    Repaint;
  end; { if }
end; { SetShowMinMax }

function TElGraph.GetEntriesCount : Integer;
{ Returns the value of data member FEntriesCount. }
begin
  result := FEntryList.Count;
end; { GetEntriesCount }

procedure TElGraph.SetShowGrid(newValue : Boolean);
{ Sets data member FShowGrid to newValue. }
begin
  if FShowGrid <> newValue then
  begin
    FShowGrid := newValue;
    Repaint;
  end; { if }
end; { SetShowGrid }

{ Prints the graph }

{$ifndef CLX_USED}
procedure TElGraph.Print; { public }
begin
  with Printer do
  begin
    BeginDoc; { start printing }
    DoDrawGraph(Canvas);
    EndDoc; { finish printing }
  end;
end; { Print }
{$endif}

function TElGraph.CreateEntry : TDataEntry; { protected }
begin
  result := TDataEntry.Create;
  Result.FOwner := self;
end; { CreateEntry }

{ Adds the entry to the list }

function TElGraph.AddEntry : TDataEntry; { public }
begin
  result := InsertEntry(FEntryList.Count);
end; { AddEntry }

function TElGraph.InsertEntry(index : integer) : TDataEntry; { public }
begin
  result := CreateEntry;
  FEntryList.Add(result);
end; { InsertEntry }

procedure TElGraph.DeleteEntry(index : integer); { public }
var
  T : TDataEntry;
begin
  try
    T := FEntryList[index];
  except
    T := nil;
  end;
  if T = nil then exit;
  if FColumnEntry = T then FColumnEntry := nil;
  if FMinMaxEntry = T then FMinMaxEntry := nil;
  FEntryList.Delete(index);
  Refresh;
end; { DeleteEntry }

{ Refreshes the graph }

procedure TElGraph.Refresh; { public }
begin
  Invalidate;
  Update;
end; { Refresh }

{ Performs actual drawing }

procedure TElGraph.DoDrawGraph(Surface : TCanvas); { protected }
var
  MinMaxRect : TRect;
  LegendRect : TRect;
  MainRect
  {$ifndef CLX_USED}
  ,R1, BgRect
  {$endif}
   : TRect;
  {$ifndef CLX_USED}
  ACtl       : TWinControl;
  {$endif}
  i, i1, i2,
    x, y, cp : integer;
  Entry : TDataEntry;
  Min, Max,
    Avg : integer;

begin
  MainRect := GetMainRect;
  MinMaxRect := GetMinMaxRect;
  LegendRect := GetLegendRect;
  {$ifndef CLX_USED}
  Surface.Brush.Color := clBtnText;
  Surface.FrameRect(BoundsRect);
  {$else}
  Surface.Brush.Style := bsClear;
  Surface.Pen.Color := clBtnText;
  Surface.Rectangle(BoundsRect);
  {$endif}
  if not Transparent then
  begin
    {$ifndef CLX_USED}
    if (FImgForm <> nil) and (not (csDesigning in FImgForm.ComponentState)) then
    begin
      ACtl := FImgForm.GetRealControl;
      R1 := MainRect;
      BgRect := MainRect;
      BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
      BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
      BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
      BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);

      FImgForm.PaintBkgnd(Surface.Handle, R1, Point(Left, Top), false);
    end
    else
    {$endif}
    begin
      Surface.Brush.Color := Color;
      Surface.FillRect(MainRect);
    end;
  end;
  // Draw MinMaxInfo
  if FShowMinMax then
  begin
    Surface.Brush.Color := FLegendBkColor;
    Surface.FillRect(MinMaxRect);
    begin
      Entry := FMinMaxEntry;
      if FMinMaxEntry <> nil then
        Entry.CalcMinMax(Min, Max, Avg)
      else
      begin
        Min := 0;
        Max := 0;
        Avg := 0;
      end;
      X := 2;
      // min value
      Surface.TextOut(MinMaxRect.Left + X, MinMaxRect.Top + 1, 'Min:');
      inc(X, ElTools.Max(20, Surface.TextWidth('Min:')) + 3);
      Surface.Brush.Color := clBtnText;
      {$ifndef CLX_USED}
      Surface.FrameRect(Rect(MinMaxRect.Left + X, MinMaxRect.Top + 1,
        MinMaxRect.Left + X + ElTools.Max(20, Surface.TextWidth(IntToStr(Min))), MinMaxRect.Bottom - 1));
      {$else}
      Surface.Brush.Style := bsClear;
      Surface.Pen.Color := clBtnText;
      Surface.Rectangle(Rect(MinMaxRect.Left + X, MinMaxRect.Top + 1,
        MinMaxRect.Left + X + ElTools.Max(20, Surface.TextWidth(IntToStr(Min))), MinMaxRect.Bottom - 1));
      {$endif}
      Surface.Brush.Color := clWindow;
      Surface.TextRect(Rect(MinMaxRect.Left + X + 1, MinMaxRect.Top + 2,
        MinMaxRect.Left + X + ElTools.Max(20, Surface.TextWidth(IntToStr(Min))) - 1, MinMaxRect.Bottom - 2),
        MinMaxRect.Left + X + 1, MinMaxRect.Top + 1, IntToStr(Min));
      Inc(X, ElTools.Max(20, Surface.TextWidth(IntToStr(Min))) + 3);

      // avg value
      Surface.Brush.Color := FLegendBkColor;
      Surface.TextOut(MinMaxRect.Left + X, MinMaxRect.Top + 1, 'Avg:');
      inc(X, ElTools.Max(20, Surface.TextWidth('Avg:')) + 3);
      Surface.Brush.Color := clBtnText;
      {$ifndef CLX_USED}
      Surface.FrameRect(Rect(MinMaxRect.Left + X, MinMaxRect.Top + 1,
        MinMaxRect.Left + X + ElTools.Max(20, Surface.TextWidth(IntToStr(Avg))), MinMaxRect.Bottom - 1));
      {$else}
      Surface.Brush.Style := bsClear;
      Surface.Pen.Color := clBtnText;
      Surface.Rectangle(Rect(MinMaxRect.Left + X, MinMaxRect.Top + 1,
        MinMaxRect.Left + X + ElTools.Max(20, Surface.TextWidth(IntToStr(Avg))), MinMaxRect.Bottom - 1));
      {$endif}
      Surface.Brush.Color := clWindow;
      Surface.TextRect(Rect(MinMaxRect.Left + X + 1, MinMaxRect.Top + 2,
        MinMaxRect.Left + X + ElTools.Max(20, Surface.TextWidth(IntToStr(Avg))) - 1, MinMaxRect.Bottom - 2),
        MinMaxRect.Left + X + 1, MinMaxRect.Top + 1, IntToStr(Avg));
      Inc(X, ElTools.Max(20, Surface.TextWidth(IntToStr(Avg))) + 3);

      // max value
      Surface.Brush.Color := FLegendBkColor;
      Surface.TextOut(MinMaxRect.Left + X, MinMaxRect.Top + 1, 'Max:');
      inc(X, ElTools.Max(20, Surface.TextWidth('Max:')) + 3);
      Surface.Brush.Color := clBtnText;
      {$ifndef CLX_USED}
      Surface.FrameRect(Rect(MinMaxRect.Left + X, MinMaxRect.Top + 1,
        MinMaxRect.Left + X + ElTools.Max(20, Surface.TextWidth(IntToStr(Max))), MinMaxRect.Bottom - 1));
      {$else}
      Surface.Brush.Style := bsClear;
      Surface.Pen.Color := clBtnText;
      Surface.Rectangle(Rect(MinMaxRect.Left + X, MinMaxRect.Top + 1,
        MinMaxRect.Left + X + ElTools.Max(20, Surface.TextWidth(IntToStr(Max))), MinMaxRect.Bottom - 1));
      {$endif}

      Surface.Brush.Color := clWindow;
      Surface.TextRect(Rect(MinMaxRect.Left + X + 1, MinMaxRect.Top + 2,
        MinMaxRect.Left + X + ElTools.Max(20, Surface.TextWidth(IntToStr(Max))) - 1, MinMaxRect.Bottom - 2),
        MinMaxRect.Left + X + 1, MinMaxRect.Top + 1, IntToStr(Max));
      Inc(X, ElTools.Max(20, Surface.TextWidth(IntToStr(Max))) + 3);

      if (MinMaxEntry <> nil) and (FShowTimeouts) then
      begin
        // faults value
        Surface.Brush.Color := FLegendBkColor;
        Surface.TextOut(MinMaxRect.Left + X, MinMaxRect.Top + 1, 'T/O:');
        inc(X, ElTools.Max(20, Surface.TextWidth('T/O:')) + 3);
        Surface.Brush.Color := clBtnText;
        {$ifndef CLX_USED}
        Surface.FrameRect(Rect(MinMaxRect.Left + X, MinMaxRect.Top + 1,
          MinMaxRect.Left + X + ElTools.Max(30, Surface.TextWidth(IntToStr(MinMaxEntry.Faults))), MinMaxRect.Bottom - 1));
        {$else}
        Surface.Brush.Style := bsClear;
        Surface.Pen.Color := clBtnText;
        Surface.Rectangle(Rect(MinMaxRect.Left + X, MinMaxRect.Top + 1,
          MinMaxRect.Left + X + ElTools.Max(30, Surface.TextWidth(IntToStr(MinMaxEntry.Faults))), MinMaxRect.Bottom - 1));
        {$endif}
        Surface.Brush.Color := clWindow;
        Surface.TextRect(Rect(MinMaxRect.Left + X + 1, MinMaxRect.Top + 2,
          MinMaxRect.Left + X + ElTools.Max(30, Surface.TextWidth(IntToStr(MinMaxEntry.Faults))) - 1, MinMaxRect.Bottom - 2),
          MinMaxRect.Left + X + 1, MinMaxRect.Top + 1, IntToStr(MinMaxEntry.Faults));
      end;
    end;
  end;

  // Draw the legend
  if FShowLegend then
  begin
    Surface.Brush.Color := FLegendBkColor;
    Surface.FillRect(LegendRect);
    Y := 5;
    for i := 0 to EntriesCount - 1 do
    begin
      Entry := DataList[i];
      Surface.Brush.Color := Entry.FColor;
      Surface.FillRect(Rect(LegendRect.Left + 5, Y + 5, LegendRect.Left + 10, Y + 10));
      Surface.Brush.Color := FLegendBkColor;
      Surface.TextOut(LegendRect.Left + 12, Y, Entry.FName);
      Y := Y + ABS(Surface.Font.Height) + 2;
      if Y > LegendRect.Bottom then break;
    end;
  end;

    // draw the grid
  if FShowGrid then
  begin
    if FHGridLines > 0 then
    begin
      Surface.Pen.Color := clCaptionText;
      Y := (MainRect.Bottom - 1) - (MainRect.Bottom div FHGridLines) + (MainRect.Bottom mod FHGridLines);
      for i := 1 to FHGridLines do
      begin
        Surface.MoveTo(MainRect.Left, Y);
        Surface.LineTo(MainRect.Right, Y);
        dec(Y, (MainRect.Bottom div FHGridLines));
      end;
    end;

    if FVGridLines > 0 then
    begin
      Surface.Pen.Color := clCaptionText;
      X := MainRect.Left + ((MainRect.Right - MainRect.Left) div FVGridLines);
      for i := 1 to FVGridLines do
      begin
        Surface.MoveTo(X, MainRect.Top + 1);
        Surface.LineTo(X, MainRect.Bottom - 1);
        inc(X, ((MainRect.Right - MainRect.Left) div FVGridLines));
      end;
    end;
  end;

  // Draw the graph
  Entry := FColumnEntry;
  if (Entry <> nil) and Entry.Visible then
  begin
    i1 := MainRect.Left;
    cp := 0;
    if Entry.ValueCount < (MainRect.Right - MainRect.Left + 1) then
      i1 := MainRect.Right - Entry.ValueCount + 1;
    if Entry.ValueCount > (MainRect.Right - MainRect.Left + 1) then
      cp := Entry.ValueCount - (MainRect.Right - MainRect.Left + 1);
    Surface.Pen.Color := Entry.FColor;
    while i1 <= MainRect.Right do
    begin
      X := Entry.Value[cp];
      Y := (MainRect.Bottom - 1) - Trunc(X * ((MainRect.Bottom - 1) / (Entry.FMaxGrid - Entry.FMinGrid)));
      if X = -2 then
        Y := 0
      else if X = -1 then
        Y := (MainRect.Bottom);
      if Y < (MainRect.Bottom) then
      begin
        Surface.MoveTo(i1, MainRect.Bottom);
        Surface.LineTo(i1, Y);
      end;
      inc(i1);
      inc(cp);
    end;
  end;

  if EntriesCount > 0 then
    for i2 := 0 to EntriesCount - 1 do
    begin
      Entry := DataList[i2];
      if (Entry <> FColumnEntry) and (Entry.Visible) then
      begin
        i1 := MainRect.Left;
        cp := 0;
        if Entry.ValueCount < (MainRect.Right - MainRect.Left + 1) then
          i1 := MainRect.Right - Entry.ValueCount + 1;
        if Entry.ValueCount > (MainRect.Right - MainRect.Left + 1) then
          cp := Entry.ValueCount - (MainRect.Right - MainRect.Left + 1);
        Surface.Pen.Color := Entry.FColor;
        Canvas.MoveTo(i1, MainRect.Bottom);
        while i1 <= MainRect.Right do
        begin
          X := Entry[cp];
          Y := (MainRect.Bottom - 1) - Trunc(X * ((MainRect.Bottom - 1) / (Entry.FMaxGrid - Entry.FMinGrid)));
          if X = -2 then
            Y := 0
          else if X = -1 then
            Y := (MainRect.Bottom);
          if Y < MainRect.Bottom then
            Surface.LineTo(i1, Y);
          inc(i1);
          inc(cp);
        end;
      end;
    end;

end; { DoDrawGraph }

procedure TElGraph.Paint; { protected }
begin
  DoDrawGraph(Canvas);
end; { Paint }

{ Returns the rectangle for legend }

function TElGraph.GetLegendRect : TRect; { protected }
begin
  if not ShowLegend then
    SetRectEmpty(Result)
  else
  begin
    if FLegendAtRight then
      Result.Left := ClientWidth - FLegendWidth
    else
      Result.Left := 1;
    if FLegendAtRight then
      Result.Right := ClientWidth - 1
    else
      Result.Right := FLegendWidth - 1;
    Result.Top := 0;
    Result.Bottom := ClientHeight;
  end;
end; { GetLegendRect }

{ Returns rectangle for MinMaxInfo }

function TElGraph.GetMinMaxRect : TRect; { protected }
begin
  if not ShowMinMax then
    SetRectEmpty(Result)
  else
  begin
    Result.Left := 0;
    Result.Right := ClientWidth;
    if FShowLegend then
    begin
      if FLegendAtRight then
        Result.Right := ClientWidth - FLegendWidth
      else
        Result.Left := FLegendWidth;
    end;
    Result.Top := ClientHeight - Abs(Font.Height) - 6;
    Result.Bottom := ClientHeight;
  end;
end; { GetMinMaxRect }

procedure TElGraph.SetLegendAtRight(newValue : Boolean);
{ Sets data member FLegendAtRight to newValue. }
begin
  if FLegendAtRight <> newValue then
  begin
    FLegendAtRight := newValue;
    if Visible then Repaint;
  end; { if }
end; { SetLegendAtRight }

procedure TElGraph.SetLegendWidth(newValue : Integer);
{ Sets data member FLegendWidth to newValue. }
begin
  if FLegendWidth <> newValue then
  begin
    FLegendWidth := newValue;
    if Visible then Repaint;
  end; { if }
end; { SetLegendWidth }

function TElGraph.GetMainRect : TRect; { protected }
var
  R1 : TRect;

begin
  Result := ClientRect;
  if FShowMinMax then Result.Bottom := GetMinMaxRect.Top;
  R1 := GetLegendRect;
  if FShowLegend then
  begin
    if FLegendAtRight then
      Result.Right := ClientWidth - FLegendWidth
    else
      Result.Left := FLegendWidth;
  end;
end; { GetMainRect }

procedure TElGraph.SetLegendBkColor(newValue : TColor);
{ Sets data member FLegendBkColor to newValue. }
begin
  if FLegendBkColor <> newValue then
  begin
    FLegendBkColor := newValue;
    if FShowLegend or FShowMinMax then Repaint;
  end; { if }
end; { SetLegendBkColor }

procedure TElGraph.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$ifndef CLX_USED}
    if AComponent = FImgForm then
    begin
      FImgForm := nil;
      Invalidate;
    end;
    {$endif}
  end;
end;

procedure TElGraph.TriggerResizeEvent;
begin
  if assigned(FOnResize) then
    FOnResize(Self);
end; { TriggerResizeEvent }

procedure TElGraph.SetMinMaxEntry(newValue : TDataEntry);
{ Sets data member FMinMaxEntry to newValue. }
begin
  if FMinMaxEntry <> newValue then
  begin
    FMinMaxEntry := newValue;
    if Visible then Repaint;
  end; { if }
end; { SetMinMaxEntry }

procedure TElGraph.SetHGridLines(newValue : Integer);
{ Sets data member FHGridLines to newValue. }
begin
  if FHGridLines <> newValue then
  begin
    FHGridLines := newValue;
    Repaint;
  end; { if }
end; { SetHGridLines }

procedure TElGraph.SetVGridLines(newValue : Integer);
{ Sets data member FVGridLines to newValue. }
begin
  if FVGridLines <> newValue then
  begin
    FVGridLines := newValue;
    Repaint;
  end; { if }
end; { SetVGridLines }

function TElGraph.GetColumnEntry : TDataEntry;
begin
  result := FColumnEntry;
end; { GetColumnEntry }

procedure TElGraph.SetColumnEntry(newValue : TDataEntry);
begin
  if FColumnEntry <> newValue then
  begin
    FColumnEntry := newValue;
    Repaint;
  end; { if }
end; { SetColumnEntry }

{$ifndef CLX_USED}
procedure TElGraph.WMEraseBkgnd(var Msg : TWMEraseBkgnd); { private }
begin
  Msg.Result := 1;
end; { WMEraseBkgnd }
{$endif}

procedure TElGraph.SetShowTimeouts(newValue : Boolean);
begin
  if (FShowTimeouts <> newValue) then
  begin
    FShowTimeouts := newValue;
    Repaint;
  end; { if }
end; { SetShowTimeouts }

procedure TElGraph.SetBounds(ALeft, ATop, AWidth, AHeight : Integer); { protected }
var
  i : Integer;
  Entry : TDataEntry;
  w : integer;
  R : TRect;
begin
  inherited;
  if not (csLoading in ComponentState) then TriggerResizeEvent;
  R := GetMainRect;
  w := R.Right - R.Left;
  for i := 0 to FEntryList.Count - 1 do
  begin
    Entry := TDataEntry(FEntryList[i]);
    Entry.Limit := w;
  end;
end; { SetBounds }

function TElGraph.GetTransparent : Boolean;
{ Returns the value of data member FTransparent. }
begin
  result := not (csOpaque in ControlStyle);
end;  { GetTransparent }

procedure TElGraph.SetTransparent(newValue : Boolean);
{ Sets data member FTransparent to newValue. }
begin
  if (Transparent <> newValue) then
  begin
    if newValue then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;  { if }
end;  { SetTransparent }

destructor TElGraph.Destroy;
begin
  FEntryList.Free;
  inherited Destroy;
end; { Destroy }

constructor TElGraph.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csDoubleClicks, csCaptureMouse,
    csDesignInteractive, csReplicatable, csOpaque{$ifndef CLX_USED}, csReflector{$endif}];
  FEntryList := TList.Create;
  FLegendAtRight := true;
  FLegendWidth := 100;
  FHGridLines := 5;
  FVGridLines := 0;
  FShowTimeouts := True;
  Width := 100;
  Height := 50;
end; { Create }
{$ifndef CLX_USED}
procedure TElGraph.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
{$endif}

end.

