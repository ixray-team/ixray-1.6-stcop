
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

unit ElPrinter;

interface

uses

    Windows,
    Printers,
    Graphics,
{$ifdef VCL_6_USED}
Types,
{$endif}

    Classes,
    SysUtils,

    ElTools,
    ElList;

type

    EPrinterError = class(Exception);

    TPageEvent = procedure(Sender : TObject; PageNumber : integer) of object;

    TElPrinter = class(TComponent)
    private
      FActive : boolean;
      FCanvas   : TElList;
      FDC     : HDC;
      FOnAfterPage: TPageEvent;
      FOnBeforePage: TPageEvent;
      FPageIndex: Integer;
      FPages    : TElList;
      FPageWidth,
      FPageHeight: integer;
      FPrintOffsetX: Integer;
      FPrintOffsetY: Integer;
      FRightMargin: Integer;
      FBottomMargin: Integer;
      FLeftMargin: Integer;
      FTopMargin: Integer;
      FTitle: string;
      function GetCanvas(Index: Integer): TCanvas;
      function GetPage(Index: Integer): TMetafile;
      function GetPageCount: Integer;
      function GetPageHeight: Integer;
      function GetPageWidth: Integer;
      function GetPrintOffsetX: Integer;
      function GetPrintOffsetY: Integer;
      procedure SetBottomMargin(Value: Integer);
      procedure SetCanvas(Index: Integer; Value: TCanvas);
      procedure SetLeftMargin(Value: Integer);
      procedure SetPage(Index: Integer; Value: TMetafile);
      procedure SetPageIndex(Value: Integer);
      procedure SetRightMargin(Value: Integer);
      procedure SetTopMargin(Value: Integer);
    protected
      procedure TriggerAfterPage(PageNumber : integer); virtual;
      procedure TriggerBeforePage(PageNumber : integer); virtual;
    public

      constructor Create(AOwner :TComponent); override;
      destructor Destroy; override;
      procedure Abort;
      function  AddPage: Integer;
      procedure BeginDoc;
      procedure Clear;
      procedure DeletePage(Index : Integer);
      procedure EndDoc;
      procedure InsertPage(Index : integer);
      procedure Loaded; override;
      procedure NewPage;
      function Preview: Integer;
      procedure PrintPages(StartIndex, EndIndex : integer);
      procedure SavePage(FileName : String; Index : integer);
      function  HorzMMToPixel(MM100s : Integer) : integer;
      function  VertMMToPixel(MM100s : Integer) : integer;

      property Active : boolean read FActive;
      property Canvas[Index: Integer]: TCanvas read GetCanvas write SetCanvas;
      property Page[Index: Integer]: TMetafile read GetPage write SetPage;
      property PageCount: Integer read GetPageCount;
      property PageHeight: Integer read FPageHeight;
      property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
      property PageWidth: Integer read FPageWidth;
      property PrintOffsetX: Integer read FPrintOffsetX;
      property PrintOffsetY: Integer read FPrintOffsetY;
      property PrinterDC : HDC read FDC;
    published
      property BottomMargin: Integer read FBottomMargin write SetBottomMargin default 2000;
      property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 2000;
      property RightMargin: Integer read FRightMargin write SetRightMargin default 2000;
      property TopMargin: Integer read FTopMargin write SetTopMargin default 2000;

      property Title: string read FTitle write FTitle;

      property OnAfterPage: TPageEvent read FOnAfterPage write FOnAfterPage;
      property OnBeforePage: TPageEvent read FOnBeforePage write FOnBeforePage;
    end;

    TElControlPrinter = class(TComponent)
    private
      FPrinter: TElPrinter;
      procedure SetPrinter(Value: TElPrinter);
    protected
      procedure Notification(AComponent : TComponent; Operation : TOperation); 
          override;
    public
    published
      property Printer: TElPrinter read FPrinter write SetPrinter;
    end;

implementation

uses ElPrinterPreview;

type
  TPrinterDevice = class
    FDriver,
    FDevice,
    FPort: String;
  end;

procedure TElPrinter.Abort;
begin
  if FActive then
  begin
    if Printer.Printing then
      Printer.Abort;
  end;
end;

function TElPrinter.AddPage: Integer;
var AMetafile : TMetafile;
begin
  AMetafile := TMetafile.Create;
  if Printer.Orientation = poPortrait then
  begin
    AMetafile.MMWidth  := MulDiv(GetDeviceCaps(FDC, PHYSICALWIDTH), 2540, GetDeviceCaps(FDC, LOGPIXELSX)) - LeftMargin - RightMargin;
    AMetafile.MMHeight := MulDiv(GetDeviceCaps(FDC, PHYSICALHEIGHT), 2540, GetDeviceCaps(FDC, LOGPIXELSY)) - TopMargin - BottomMargin;
  end
  else
  begin
    AMetafile.MMWidth  := MulDiv(GetDeviceCaps(FDC, PHYSICALHEIGHT), 2540, GetDeviceCaps(FDC, LOGPIXELSY)) - TopMargin - BottomMargin;
    AMetafile.MMHeight := MulDiv(GetDeviceCaps(FDC, PHYSICALWIDTH), 2540, GetDeviceCaps(FDC, LOGPIXELSX)) - LeftMargin - RightMargin;
  end;
  FPages.Add(AMetafile);
  FCanvas.Add(TMetafileCanvas.Create(AMetafile, FDC));
  Result := FPages.Count - 1;
end;

function TElPrinter.HorzMMToPixel(MM100s : Integer) : integer;
begin
  if FDC <> 0 then
  begin
    result := MulDiv(MM100s, GetDeviceCaps(FDC, HORZRES), 100 * GetDeviceCaps(FDC, HORZSIZE));
  end
  else
    result := 0;
end;

function TElPrinter.VertMMToPixel(MM100s : Integer) : integer;
begin
  if FDC <> 0 then
  begin
    result := MulDiv(MM100s, GetDeviceCaps(FDC, VERTRES), 100 * GetDeviceCaps(FDC, VERTSIZE));
  end
  else
    result := 0;
end;

procedure TElPrinter.BeginDoc;
var Device : TPrinterDevice;
begin
  Clear;
  Device := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
  FDC := CreateDC(PChar(Device.FDriver), PChar(Device.FDevice), PChar(Device.FPort), nil);
  FActive := true;
  FPageIndex := AddPage;
  FPageHeight := GetPageHeight;
  FPageWidth  := GetPageWidth;
  FPrintOffsetX := GetPrintOffsetX;
  FPrintOffsetY := GetPrintOffsetY;
  TriggerBeforePage(FPageIndex);
end;

procedure TElPrinter.Clear;
begin
  while FPages.Count > 0 do
  begin
    TMetaFile(FPages[0]).Free;
    FPages.Delete(0);
  end;
  FPageIndex := -1;
end;

constructor TElPrinter.Create(AOwner :TComponent);
begin
  inherited;
  FPageIndex := -1;
  FPages := TElList.Create;
  FCanvas := TElList.Create;

  FRightMargin := 2000;
  FBottomMargin := 2000;
  FLeftMargin := 2000;
  FTopMargin := 2000;
end;

procedure TElPrinter.DeletePage(Index : Integer);
begin
  // delete canvas
  if FActive then
  begin
    TCanvas(FCanvas[Index]).Free;
    FCanvas.Delete(Index);
  end;
  // delete page
  TMetaFile(FPages[Index]).Free;
  FPages.Delete(Index);
  // optionally move index
  if (FPageIndex = Index) and (Index > FPages.Count - 1) then
    Dec(FPageIndex);
end;

destructor TElPrinter.Destroy;
begin
  if FActive then
    EndDoc;
  Clear;
  FPages.Free;
  FCanvas.Free;
  inherited;
end;

procedure TElPrinter.EndDoc;
begin
  TriggerAfterPage(FPageIndex);
  while FCanvas.Count > 0 do
  begin
    TCanvas(FCanvas[0]).Free;
    FCanvas.Delete(0);
  end;
  DeleteDC(FDC);
  FDC := 0;
  FActive := false;
end;

function TElPrinter.GetCanvas(Index: Integer): TCanvas;
begin
  Result := TCanvas(FCanvas[Index]);
end;

function TElPrinter.GetPage(Index: Integer): TMetafile;
begin
  Result := TMetaFile(FPages[Index]);
end;

function TElPrinter.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TElPrinter.GetPageHeight: Integer;
begin
  if FActive then
    Result := TMetafile(Page[FPageIndex]).MMHeight
  else
    Result := 0;
end;

function TElPrinter.GetPageWidth: Integer;
begin
  if FActive then
    Result := TMetafile(Page[FPageIndex]).MMWidth
  else
    Result := 0;
end;

function TElPrinter.GetPrintOffsetX: Integer;
begin
  if Active then
  begin
    Result := MulDiv(GetDeviceCaps(FDC, PHYSICALOFFSETX), 2540, GetDeviceCaps(FDC, LOGPIXELSX)) - FLeftMargin;
  end
  else
  begin
    Result := 0;
  end;
end;

function TElPrinter.GetPrintOffsetY: Integer;
begin
  if Active then
  begin
    Result := MulDiv(GetDeviceCaps(FDC, PHYSICALOFFSETY), 2540, GetDeviceCaps(FDC, LOGPIXELSY)) - FTopMargin;
  end
  else
  begin
    Result := 0;
  end;
end;

procedure TElPrinter.InsertPage(Index : integer);
begin
  assert(false);
end;

procedure TElPrinter.Loaded;
begin
end;

procedure TElPrinter.NewPage;
begin
  TriggerAfterPage(FPageIndex);
  if FPageIndex = FPages.COunt - 1 then
    AddPage;
  Inc(FPageIndex);
  TriggerBeforePage(FPageIndex);
end;

function TElPrinter.Preview: Integer;
begin
  with TElPrinterPreviewDlg.Create(Self) do
  begin
    try
      SetData(Self);
      Result := ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TElPrinter.PrintPages(StartIndex, EndIndex : integer);
var ul,
    i  : integer;
    b  : boolean;

    procedure DoPrintPage(Index : integer);
    var r  : TRect;
        FDC: HDC;
    begin
      FDC := Printer.Canvas.Handle;
      R := Rect(MulDiv(FLeftMargin, GetDeviceCaps(FDC, LOGPIXELSX), 2540),
                MulDiv(FTopMargin,  GetDeviceCaps(FDC, LOGPIXELSY), 2540),
                GetDeviceCaps(FDC, PHYSICALWIDTH) - MulDiv(FBottomMargin, GetDeviceCaps(FDC, LOGPIXELSX), 2540),
                GetDeviceCaps(FDC, PHYSICALHEIGHT)- MulDiv(FTopMargin, GetDeviceCaps(FDC, LOGPIXELSY), 2540));
      Printer.Canvas.StretchDraw(R, GetPage(Index - 1));
    end;

begin
  if (FPages.Count > 0) then
  begin
    b := false;
    Printer.Title := FTitle;
    if not Printer.Printing then
    begin
      Printer.BeginDoc;
      b := true;
    end;
    ul := Min(FPages.Count, EndIndex);
    for i := Max(1, StartIndex) to ul do
    begin
      TriggerBeforePage(i);
      DoPrintPage(i);
      TriggerAfterPage(i);
      if i < ul then
        Printer.NewPage;
    end;
    if b then
      Printer.EndDoc;
  end;
end;

procedure TElPrinter.SavePage(FileName : String; Index : integer);
begin
  TMetafile(Page[Index]).SaveToFile(FileName);
end;

procedure TElPrinter.SetBottomMargin(Value: Integer);
begin
  if FActive then
    raise EPrinterError.Create('Can''t change margins while printing');
  if FBottomMargin <> Value then
  begin
    FBottomMargin := Value;
  end;
end;

procedure TElPrinter.SetCanvas(Index: Integer; Value: TCanvas);
begin
  TCanvas(FCanvas[Index]).Assign(Value);
end;

procedure TElPrinter.SetLeftMargin(Value: Integer);
begin
  if FActive then
    raise EPrinterError.Create('Can''t change margins while printing');
  if FLeftMargin <> Value then
  begin
    FLeftMargin := Value;
  end;
end;

procedure TElPrinter.SetPage(Index: Integer; Value: TMetafile);
begin
  TMetaFile(FPages[Index]).Assign(Value);
end;

procedure TElPrinter.SetPageIndex(Value: Integer);
begin
  if FPageIndex <> Value then
  begin
    while FPageIndex >= FPages.Count do
      AddPage;
    FPageIndex := Value;
  end;
end;

procedure TElPrinter.SetRightMargin(Value: Integer);
begin
  if FActive then
    raise EPrinterError.Create('Can''t change margins while printing');
  if FRightMargin <> Value then
  begin
    FRightMargin := Value;
  end;
end;

procedure TElPrinter.SetTopMargin(Value: Integer);
begin
  if FActive then
    raise EPrinterError.Create('Can''t change margins while printing');
  if FTopMargin <> Value then
  begin
    FTopMargin := Value;
  end;
end;

procedure TElPrinter.TriggerAfterPage(PageNumber : integer);
begin
  if Assigned(FOnAfterPage) then FOnAfterPage(Self, PageNumber);
end;

procedure TElPrinter.TriggerBeforePage(PageNumber : integer);
begin
  if Assigned(FOnBeforePage) then FOnBeforePage(Self, PageNumber);
end;

procedure TElControlPrinter.SetPrinter(Value: TElPrinter);
begin
  if FPrinter <> Value then
  begin
    {$ifdef VCL_5_USED}
    if FPrinter <> nil then
      FPrinter.RemoveFreeNotification(Self);
    {$endif}
    FPrinter := Value;
    if FPrinter <> nil then
      FPrinter.FreeNotification(Self);
  end;
end;

procedure TElControlPrinter.Notification(AComponent : TComponent; operation : 
    TOperation);
// var i : integer;
begin
  inherited Notification(AComponent, operation);
  if (operation = opRemove) and (AComponent = Printer) then
    Printer := nil;
end; { Notification }



end.
