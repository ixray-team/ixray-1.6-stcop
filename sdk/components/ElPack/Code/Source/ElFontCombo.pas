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

{$r ElFontCombo.res}

(*

Version History

04/02/2002

  Added fsmNoSample sample mode to prevent the control from drawing text using the selected font
  Increased speed of component loading

09/26/2001

  Made SampleText property Unicode (optionally)
  Improved size calculation
  Added XP styles when drawing text

*)

unit ElFontCombo;

interface

uses Windows,
     Graphics,
     Printers,
     StdCtrls,

     SysUtils,
     Classes,

{$ifdef VCL_6_USED}
Types,
{$endif}
     ElTools,
     ElStrUtils,
     ElVCLUtils,
     ElTmSchema,
     ElUxTheme,
     ElACtrls;

type

    TElFontSampleMode = (fsmFontName, fsmFontSample, fsmBoth, fsmNoSample);

    TElFontDevice = (efdScreen, efdPrinter, efdBoth);

    TElFontComboOption = (foAnsiOnly, foTrueTypeOnly, foIncludeOEMFonts,
                          foIncludeSymbolFonts, foOEMFontsOnly,
                          foScalableOnly);

    TElFontComboOptions = set of TElFontComboOption;

    TElFontComboBox = class(TElAdvancedComboBox)
    protected
      FOptions: TElFontComboOptions;
      FSampleText: TElFString;
      FFontPitch : TFontPitch;
      FSampleMode: TElFontSampleMode;
      FFontDevice: TElFontDevice;
      FFontName  : string;
      FFakeInt   : Integer;
      
      procedure SetFontName(Value: TFontName);
      procedure SetOptions(Value: TElFontComboOptions);
      procedure SetSampleText(Value: TElFString);
      procedure SetFontPitch(Value: TFontPitch);
      procedure SetSampleMode(Value: TElFontSampleMode);
      procedure CreateWnd; override;
      function GetItemText(index : integer): TElFString;
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
      procedure SetFontDevice(Value: TElFontDevice);

      procedure AddFont(Font : TFontName; FontType : integer);
      function GetFontName: TFontName;
      procedure Loaded; override;
    public
      procedure RebuildFontList;
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

    published
      property Items : integer read FFakeInt;
      property Style : integer read FFakeInt;
      property Text : integer read FFakeInt;

      property FontName: TFontName read GetFontName write SetFontName;
      property Options: TElFontComboOptions read FOptions write SetOptions;
      property SampleText: TElFString read FSampleText write SetSampleText;
      property FontPitch: TFontPitch read FFontPitch write SetFontPitch;
      property SampleMode: TElFontSampleMode read FSampleMode write SetSampleMode;
      property FontDevice: TElFontDevice read FFontDevice write SetFontDevice;
    end;

implementation

var FDeviceBmp,
    FTrueTypeBmp: TBitmap;


procedure TElFontComboBox.SetFontName(Value: TFontName);
var i : integer;
    s : string;
begin
  s := Uppercase(Value);
  if csLoading in ComponentState then
    FFontName := Value
  else
  for i := 0 to inherited Items.Count - 1 do
  begin
    if Uppercase(inherited Items[i]) = s then
    begin
      ItemIndex := i;
      exit;
    end;
  end;
end;

procedure TElFontComboBox.SetOptions(Value: TElFontComboOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if ComponentState * [csReading, csLoading, csDestroying] = [] then 
      RebuildFontList;
  end
end;

procedure TElFontComboBox.SetSampleText(Value: TElFString);
begin
  if FSampleText <> Value then
  begin
    FSampleText := Value;
    if SampleMode <> fsmFontName then
      Invalidate;
  end;
end;

function IsValidFont(Box: TElFontComboBox; LogFont: TLogFont;
  FontType: Integer): Boolean;
begin
  Result := True;
  if (foAnsiOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = ANSI_CHARSET);
  if (foTrueTypeOnly in Box.Options) then
    Result := Result and (FontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE);

  if (Box.FontPitch = fpFixed) then
    Result := Result and (LogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH)
  else
  if (Box.FontPitch = fpVariable) then
    Result := Result and (LogFont.lfPitchAndFamily and VARIABLE_PITCH = VARIABLE_PITCH);

  if (foOEMFontsOnly in Box.Options) then
    Result := Result and (LogFont.lfCharSet = OEM_CHARSET);
  if (not (foIncludeOEMFonts in Box.Options)) then
    Result := Result and (LogFont.lfCharSet <> OEM_CHARSET);
  if (not (foIncludeSymbolFonts in Box.Options)) then
    Result := Result and (LogFont.lfCharSet <> SYMBOL_CHARSET);
  if (foScalableOnly in Box.Options) then
    Result := Result and (FontType and RASTER_FONTTYPE = 0);
end;

function EnumFontsProc(var EnumLogFont: TEnumLogFont;
  var TextMetric: TNewTextMetric; FontType: Integer; Data: LPARAM): Integer; stdcall;
var FontName : string;

begin
  FontName := StrPas(PAnsiChar(AnsiString(EnumLogFont.elfLogFont.lfFaceName)));
  if IsValidFont(TElFontComboBox(Pointer(Data)), EnumLogFont.elfLogFont, FontType) then
    TElFontComboBox(Pointer(Data)).AddFont(FontName, FontType);
  Result := 1;  
end;

procedure TElFontComboBox.RebuildFontList;
var
  DC: HDC;
  S : string;
begin
  if not HandleAllocated then Exit;
  DC := GetDC(0);
  S := FontName;
  inherited Items.Clear;
  inherited Items.BeginUpdate;
  try
    if (FFontDevice = efdScreen) or (FFontDevice = efdBoth) then
        EnumFontFamilies(DC, nil, @EnumFontsProc, Longint(Self));
    if (FFontDevice = efdPrinter) or (FFontDevice = efdBoth) then
    try
      EnumFontFamilies(Printer.Handle, nil, @EnumFontsProc, Longint(Self));
    except
      { skip any errors }
    end;
  finally
    inherited Items.EndUpdate;
    ReleaseDC(0, DC);
  end;
  FontName := S;
end;

procedure TElFontComboBox.SetFontPitch(Value: TFontPitch);
begin
  if FFontPitch <> Value then
  begin
    FFontPitch := Value;
    if ComponentState * [csReading, csLoading, csDestroying] = [] then 
      RebuildFontList;
  end;
end;

procedure TElFontComboBox.SetSampleMode(Value: TElFontSampleMode);
begin
  if FSampleMode <> Value then
  begin
    FSampleMode := Value;
    Invalidate;
  end;
end;

function TElFontComboBox.GetItemText(index : integer): TElFString;
begin
  if SampleMode = fsmFontName then
    result := inherited Items[Index]
  else
  if SampleMode = fsmFontSample then
    result := SampleText
  else
    result := TElFString(inherited Items[Index] + ' - ') + SampleText;
end;

procedure TElFontComboBox.CreateWnd;
begin
  inherited;
  RebuildFontList;
end;

function TElFontComboBox.GetItemWidth(Index: Integer): Integer;
var S   : TElFString;
    R   : TRect;
    ATheme : HTheme;
    sid : integer;
begin
  if Index = -1 then
    Canvas.Font.Name := Font.Name
  else
    Canvas.Font.Name := inherited Items[Index];

  S := GetItemText(Index);
  SetRectEmpty(R);

  if ThemesAvailable then
  begin
    Atheme := OpenThemeData(Handle, 'EDIT');
    if ATheme <> 0 then
    begin
      if not Enabled then
        sid := ETS_DISABLED
      else
        sid := ETS_NORMAL;

      GetThemeTextExtent(ATheme, Canvas.Handle, EP_EDITTEXT, sid, PWideChar(WideString(S)), Length(WideString(S)), DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_CALCRECT, nil, R);
      Result := R.Right - R.Left + 24;
      CloseThemeData(ATheme);
      exit;
    end;
  end;
  {$ifdef ELPACK_UNICODE}
  ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER or DT_LEFT or DT_CALCRECT);
  {$else}
  DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER or DT_LEFT or DT_CALCRECT);
  {$endif}
  Result := R.Right - R.Left + 24;
end;

{$ifndef CLX_USED}
procedure TElFontComboBox.MeasureItem(Index: Integer; var Height:
    Integer);
{$else}
procedure TElFontComboBox.MeasureItem(Control: TWinControl; Index:
    Integer; var Height, Width: Integer);
{$endif}
var S : TElFString;
    R : TRect;
    Metrics : TTextMetric;
    //ATheme  : HTheme;
    //sid     : integer;
begin
  if (Index = -1) or (SampleMode = fsmNoSample) then
    Canvas.Font.Name := Font.Name
  else
    Canvas.Font.Name := inherited Items[Index];

  if (Index <> -1) and (Index < inherited Items.Count) then
  begin
    S := GetItemText(Index);
    SetRectEmpty(R);

    (*
    if ThemesAvailable then
    begin
      Atheme := OpenThemeData(Handle, 'EDIT');
      if ATheme <> 0 then
      begin
        if not Enabled then
          sid := ETS_DISABLED
        else
          sid := ETS_NORMAL;

        GetThemeTextExtent(ATheme, Canvas.Handle, EP_EDITTEXT, sid, PWideChar(WideString(S)), Length(WideString(S)), DT_SINGLELINE or DT_LEFT or DT_VCENTER, nil, R);
        {$ifdef CLX_USED}
        Width := R.Right - R.Left + 24;
        {$endif}
        Height := R.Bottom - R.Top;
        CloseThemeData(ATheme);
        exit;
      end;
    end;
    *)
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER or DT_LEFT or DT_CALCRECT);
    {$else}
    DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER or DT_LEFT or DT_CALCRECT);
    {$endif}
    {$ifdef CLX_USED}
    Width := R.Right - R.Left + 24;
    {$endif}
    Height := R.Bottom - R.Top;
  end
  else
  begin
    GetTextMetrics(Canvas.Handle, Metrics);
    Height := Abs(Metrics.tmHeight) + 2;
    {$ifdef CLX_USED}
    Width := Canvas.TextWidth(GetItemText(Index)) + 4;
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElFontComboBox.DrawItem(Index: Integer; Rect: TRect; State:
    {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
{$else}
function TElFontComboBox.DrawItem(Index: Integer; Rect: TRect; State:
    TOwnerDrawState): Boolean;
{$endif}
var Bitmap : TBitmap;
    R      : TRect;
    //sid    : integer;
    S      : TElFString;
begin
  Canvas.Font.Assign(Font);

  if (Index = -1) or (SampleMode = fsmNoSample) then
    Canvas.Font.Name := Font.Name
  else
    Canvas.Font.Name := inherited Items[Index];

  if odSelected in State then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Font.Color := clHighlightText;
  end
  else
    Canvas.Brush.Color := Color;

  Canvas.FillRect(Rect);
  
  if (Integer(inherited Items.Objects[Index]) and TRUETYPE_FONTTYPE) <> 0 then
    Bitmap := FTrueTypeBmp
  else
  if (Integer(inherited Items.Objects[Index]) and DEVICE_FONTTYPE) <> 0 then
    Bitmap := FDeviceBmp
  else
    Bitmap := nil;
  if Bitmap <> nil then
  begin
    R.Left := Rect.Left;
    R.Right := R.Left + 20;
    R.Top := Rect.Top + (Rect.Bottom - Rect.Top - 12) div 2;
    R.Bottom := R.Top + 12;
    Canvas.BrushCopy(R, Bitmap, Classes.Rect(0, 0, 20, 12), clFuchsia);
  end;
  s := GetItemText(Index);

  Inc(Rect.Left, 20);
  (*
  if ThemesAvailable then
  begin
    if not Enabled then
      sid := ETS_DISABLED
    else
    if odSelected in State then
      sid := ETS_SELECTED
    else
    if FMouseOver then
      sid := ETS_HOT
    else
      sid := ETS_NORMAL;
    DrawThemeTextTo('EDIT', Canvas.Handle, EP_EDITTEXT, sid, PWideChar(WideString(s)), Length(WideString(s)), DT_VCENTER or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE, 0, Rect);
  end
  else
    *)
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(S), Length(S), Rect, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
    {$else}
    DrawText(Canvas.Handle, PChar(String(S)), Length(S), Rect, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
    {$endif}
  (*
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
  *)
end;

procedure TElFontComboBox.SetFontDevice(Value: TElFontDevice);
begin
  if FFontDevice <> Value then
  begin
    FFontDevice := Value;
    if ComponentState * [csReading, csLoading, csDestroying] = [] then 
      RebuildFontList;
  end;
end;

{$O-}
procedure TElFontComboBox.AddFont(Font : TFontName; FontType : integer);
var i : integer;
begin
  if inherited Items.IndexOf(Font) = -1 then
  begin
    for i := 0 to inherited Items.Count do
    begin
      if (i = inherited Items.Count) or (AnsiCompareStr(Font, inherited Items[i]) < 0) then
      begin
        inherited Items.InsertObject(i, Font, TObject(Pointer(FontType)));
        exit;
      end;
    end;
  end
end;

function TElFontComboBox.GetFontName: TFontName;
begin
  if ItemIndex = -1 then
    Result := ''
  else
    Result := inherited Items[ItemIndex]; 
end;

constructor TElFontComboBox.Create(AOwner : TComponent);
{ Creates an object of type TElHTMLComboBox, and initializes properties. }
begin
  inherited Create(AOwner);
  inherited Style := csOwnerDrawVariable;
  FFontDevice := efdBoth;
  FOptions := [foIncludeOEMFonts, foIncludeSymbolFonts];
end;  { Create }

destructor TElFontComboBox.Destroy;
begin
  inherited Destroy;
end;  { Destroy }

procedure TElFontComboBox.Loaded;
begin
  inherited;
  FontName := FFontName;
end;

initialization

  FTrueTypeBmp := TBitmap.Create;
  FDeviceBmp := TBitmap.Create;
  FTrueTypeBmp.LoadFromResourceName(HInstance, 'ELFONTCOMBOTRUETYPEBITMAP');
  FDeviceBmp.LoadFromResourceName(HInstance, 'ELFONTCOMBODEVICEBITMAP');

finalization

  FTrueTypeBmp.Free;
  FDeviceBmp.Free;

end.
