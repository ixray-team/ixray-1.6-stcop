{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Parts:                                           }
{   copyright (c) 2001 Akzhan Abdulin                }
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

04/30/2002

  Fixed alignment problem with word-wrapped and <br>-ed paragraphs

04/18/2002

  Fixed comment skipping routine that could cause an infinite loop

01/20/2002

  Changed font size handling to work with font.height, not font.size.
  This is internal change, font sizes specified in HTML text remain unchanged

01/18/2002

  Added possibility to adjust font color based on some supposedly background color

11/28/2001

  Improved speed of rendering when wordwrap is on

11/01/2001

  Fixed WordWrapping in case of width = 0. 

10/08/2001

  Fixed assignment of break item properties in WordWrap mode -- before the fix
  not last break item's properties where taken, but first item's.

09/10/2001

  Fixed rendering of LI items with WordWrap on.

07/27/2001

  SUB and SUP tags added for subscript and superscript text 

04/21/2001

  Fixed incorrect wrapping of HTML links and other elements that start a text block
  (i.e. elements whose attribute is different from previous text)

============================== Version 2.78 ====================================

03/27/2001

  Fixed detection of the element at given position with right and center alignment

03/15/2001

  Now <font color="value"> understands simple color definitions like red, white).


03/14/2001

  Added <strikeout> tag that applied to text font style.

  Fixed '&amp;' and '&apos;' handling.

  Minor code optimizations and readness improvements.
  Unnecessary variables, 'if' statements excluded, some 'while' statements
  replaced with 'for' statements. or'ed comparisons somewhere replaced with
  inside const set test etc.

============================== Version 2.75 ====================================

11/07/2000

  Added unordered (bullet) lists

  Added indent parameter for paragraphs. Can be positive or negative

*)

unit HTMLRender;

interface

uses Classes,
     SysUtils,
{$ifndef CLX_USED}
     Forms,
     Windows,
     Graphics,
{$ifdef VCL_6_USED}
Types,
{$endif}
{$else}
{$ifdef MSWINDOWS}
     Windows,
{$endif}
     QTypes,
     Types,
     Qt,
     QForms,
     QGraphics,
{$endif}
     ElList,
     ElStack,
     ElTools,
     ElVCLUtils,
{$ifdef ELPACK_UNICODE}
     ElUnicodeStrings,
{$endif}
     ElStrUtils;

const

     flIsLink = 1;
     flSub    = 2;
     flSuper  = 4;

type
  { event handlers support }
   TElFString = ElStrUtils.TElFString;

type
  TElHTMLImageNeededEvent = procedure(Sender : TObject; Src : TElFString; var Image : TBitmap) of object;
  TElHTMLLinkClickEvent = procedure(Sender : TObject; HRef : TElFString) of object;

  THTMLItemType = (hitChar, hitSoftBreak, hitBreak, hitPara, hitBitmap, hitHR, hitLI, hitUL);

  THTMLItemTypes = set of THTMLItemType;

  TElHTMLRender = class;
  TElHTMLData = class;

  TElHTMLItem = class
  private
    FOwner    : TElHTMLData;
    ItemType  : THTMLItemType;
    FText     : TElFString;
    FontStyle : TFontStyles;
    FontHeight: integer;
    FontBgColor,
    FontColor : TColor;
    FLinkRef  : TElFString;
    FFontRef  : TElFString;
    FFntCnt   : integer;
    TWidth,
    Width,
    Height    : Word;
    Indent    : integer;
    FBoolState: integer;
    
    function GetWidth  : integer; virtual;
    function GetHeight(var BaseLine : integer) : integer; virtual;
    function GetIsLink: Boolean;
    procedure SetIsLink(Value: Boolean);
    function GetIsSub: Boolean;
    procedure SetIsSub(Value: Boolean);
    function GetIsSuper: Boolean;
    procedure SetIsSuper(Value: Boolean);

  public
    procedure Assign(Source : TElHTMLItem);
    constructor Create(Owner : TElHTMLData);
    destructor Destroy; override;
    
    property Text: TElFString read FText write FText;
    property LinkRef: TElFString read FLinkRef write FLinkRef;
    property IsLink: Boolean read GetIsLink write SetIsLink;
    property IsSub: Boolean read GetIsSub write SetIsSub;
    property IsSuper: Boolean read GetIsSuper write SetIsSuper;
  end;

  TElHTMLBreakItem = class(TElHTMLItem)
  private
    FParams   : integer;
    ListLevel : integer;
    ListItemN : integer;

    function GetWidth : integer; override;
    function GetHeight(var BaseLine : integer) : integer; override;
  public
    procedure Assign(Source : TElHTMLItem);
    procedure AssignBreakProps(Source : TElHTMLBreakItem);
  end;

  TElHTMLData = class
    protected
      FRender       : TElHTMLRender;
      FLinkColor    : TColor;
      FDefaultColor : TColor;
      FLinkStyle    : TFontStyles;
      FDefaultStyle : TFontStyles;
      FDefaultHeight: Integer;
      FCharset      : TFontCharset;
      FDefaultFont  : String;
      FArray        : TElList;
      FDefaultBgColor  : TColor;
      FHighlightBgColor: TColor;
      FHighlightColor  : TColor;
      FSelectedItem: TElHTMLItem;
      FRect        : TRect;

      FTextSize    : TSize;
    public
  {$ifdef CLX_USED}
      TextOffset : TPoint;
  {$endif}
      constructor Create;
      destructor Destroy; override;
      procedure ClearArray;
      function  LineCount : integer;

      property TextSize     : TSize read FTextSize;
      property LinkColor    : TColor read FLinkColor write FLinkColor;
      property DefaultBgColor : TColor read FDefaultBgColor write FDefaultBgColor;
      property DefaultColor : TColor read FDefaultColor write FDefaultColor;
      property LinkStyle    : TFontStyles read FLinkStyle write FLinkStyle;
      property DefaultStyle : TFontStyles read FDefaultStyle write FDefaultStyle;
      property DefaultHeight: Integer read FDefaultHeight write FDefaultHeight;
      property DefaultFont  : String read FDefaultFont write FDefaultFont;
      property Charset      : TFontCharset read FCharset write FCharset;
      property HighlightBgColor : TColor read FHighlightBgColor write FHighlightBgColor;
      property HighlightColor : TColor read FHighlightColor write FHighlightColor;
      property SelectedItem: TElHTMLItem read FSelectedItem write FSelectedItem;
      property Rect: TRect read FRect write FRect;
  end;

  TElHTMLRender = class
  private
    FOnImageNeeded : TElHTMLImageNeededEvent;
    FIntData,
    FData        : TElHTMLData;
    Canvas       : TCanvas;
    Bitmap       : TBitmap;
    {$ifndef CLX_USED}
    FSaveObj     : HGDIOBJ;
    {$endif}
  protected
    procedure TriggerImageNeededEvent(Src : TElFString; var Image : TBitmap); virtual;
    procedure CalcTokenSizes(FCurData : TElHTMLData);
    function GetTextSize : TSize;
    function FindItemAt(Point : TPoint; SrcPoint : TPoint; R : TRect) : TElHTMLItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DestroyData(Data : TElHTMLData);
    procedure SetData(NewData : TElHTMLData);
    function  CreateData : TElHTMLData;
    procedure DrawText(Canvas : TCanvas; SrcPoint : TPoint; R : TRect; AdjustFromColor : TColor);
    procedure DrawTextEx(Canvas : TCanvas; SrcPoint : TPoint; R : TRect; UseOverColors : boolean; Color, BkColor, SelColor, SelBkColor : TColor; AdjustFromColor : TColor);
    function  IsCursorOverLink(Point : TPoint; SrcPoint : TPoint; R : TRect; var href : TElFString) : Boolean;
    procedure SelectLinkAt(Point : TPoint; SrcPoint : TPoint; R : TRect);
    procedure SelectPrevLink;
    procedure SelectNextLink;

    procedure PrepareToData(Text : TElFString; MaxWidth :
        integer; AutoWrap : boolean; CurData : TElHTMLData);
    procedure PrepareText(Text : TElFString; MaxWidth : integer; AutoWrap : boolean);

    property Data : TElHTMLData read FData;
    property OnImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write FOnImageNeeded;
  end;

implementation

{.$L-,O+,D-}

var ScreenPixelsPerInch : integer;

Function ColorLighter(Const Color: TColor; Const Percent: Byte):TColor;
Var
  R, G, B: Byte;
  FColor: TColorRef;
begin
  FColor := ColorToRGB(Color);

  R := GetRValue(FColor);
  G := GetGValue(FColor);
  B := GetBValue(FColor);

  R := R + muldiv(255-r, Percent, 100);
  G := G + muldiv(255-g, Percent, 100);
  B := B + muldiv(255-b, Percent, 100);

  Result := RGB(R, G, B);
end;

Function ColorDarker(Const OriginalColor: TColor; Const Percent: Byte): TColor;
Var
  R, G, B: Integer;
  WinColor: Integer;
begin
  WinColor := ColorToRGB(OriginalColor);
  R := GetRValue(WinColor);
  G := GetGValue(WinColor);
  B := GetBValue(WinColor);

  R := MulDiv(R, Percent, 100);
  G := MulDiv(G, Percent, 100);
  B := MulDiv(B, Percent, 100);

  If R < 0 Then R := 0;
  If G < 0 Then G := 0;
  If B < 0 Then B := 0;

  Result := RGB(R, G, B);
end;

function AdjustColor(Color, BkColor : TColor) : TColor;
var HLS : TColor;
begin
  HLS := RGBToHLS(ColorToRGB(BkColor));
  if ((HLS and ($00FF00)) shr 8) > 128 then
  begin
    result := ColorDarker(Color, 50);
  end
  else
  begin
    result := ColorLighter(Color, 50);
  end;
end;

constructor TElHTMLItem.Create(Owner : TElHTMLData);
begin
  inherited Create;
  FOwner := Owner;
  TWidth := Word(-1);
end;

destructor TElHTMLItem.Destroy;
begin
  inherited;
end;

function TElHTMLItem.GetHeight(var BaseLine : integer) : integer;
{$ifndef CLX_USED}
{$IFDEF VCL_4_USED}
var TM : tagTextMetricA;
{$ELSE}
var TM : TTextMetricA;
{$ENDIF}
{$else}
var Metrics : QFontMetricsH;
{$endif}
begin
  BaseLine := 0;
  case ItemType of
    hitChar:
      begin
        if Assigned(FOwner.FRender) then
        begin
          with FOwner.FRender.Canvas.Font do
          begin
            if IsLink then
              Style := FOwner.FLinkStyle
            else
              Style := FontStyle;
            Charset := FOwner.Charset;
            Name  := FFontRef;
            if IsSub or IsSuper then
              Height := FontHeight - 2
            else
              Height := FontHeight;
          end;
          {$ifndef CLX_USED}
          GetTextMetricsA(FOwner.FRender.Canvas.Handle, TM);
          baseLine := TM.tmDescent;
          Result := Abs(TM.tmHeight);
          {$else}
          Metrics := QFontMetrics_create(FOwner.FRender.Canvas.Font.Handle);
          result := QFontMetrics_height(Metrics);
          baseLine := QFontMetrics_descent(Metrics);
          QFontMetrics_destroy(Metrics);
          {$endif}
        end
        else
          Result := 0;
      end;
    hitBitmap:
      result := Height;
    else
      result := 0;
  end;
end;

function TElHTMLItem.GetWidth : integer;
{$ifndef CLX_USED}
var R : TRect;
{$IFDEF VCL_4_USED}
var TM : tagTextMetricA;
{$ELSE}
var TM : TTextMetricA;
{$ENDIF}
{$else}
var Metrics : QFontMetricsH;
{$endif}
begin
  case Self.ItemType of
    hitUL:
      begin
        if FOwner.FRender <> nil then
        begin
          with FOwner.FRender.Canvas.Font do
          begin
            if IsLink then
              Style := FOwner.FLinkStyle
            else
              Style := FontStyle;
            Charset := FOwner.Charset;
            Name    := FFontRef;
            Height  := FontHeight;
          end;
          {$ifndef CLX_USED}
          GetTextMetricsA(FOwner.FRender.Canvas.Handle, TM);
          result := TM.tmMaxCharWidth;
          {$else}
          Metrics := QFontMetrics_create(FOwner.FRender.Canvas.Font.Handle);
          result := QFontMetrics_maxWidth(Metrics);
          QFontMetrics_destroy(Metrics);
          {$endif}
        end else
          result := 0;
      end;
    hitChar:
      begin
        if TWidth <> Word(-1) then
          result := TWidth
        else
        begin
          if FOwner.FRender <> nil then
          begin
            with FOwner.FRender.Canvas.Font do
            begin
              if IsLink then
              begin
                Style := FOwner.FLinkStyle;
              end else
              begin
                Style := FontStyle;
              end;
              Name  := FFontRef;
              if IsSub or IsSuper then
                Height := FontHeight - 2
              else
                Height := FontHeight;
            end;
            {$ifndef CLX_USED}
            SetRectEmpty(R);
            {$ifdef ELPACK_UNICODE}
            ElVCLUtils.DrawTextW(FOwner.FRender.Canvas.Handle, PWideChar(Text), Length(Text), R, DT_SINGLELINE or DT_BOTTOM or DT_NOPREFIX or DT_CALCRECT);
            {$else}
            Windows.DrawText(FOwner.FRender.Canvas.Handle, Pchar(Text), Length(Text), R, DT_SINGLELINE or DT_BOTTOM or DT_NOPREFIX or DT_CALCRECT);
            {$endif}
            result := R.Right - R.Left + 1;
            {$else}
            result := FOwner.FRender.Canvas.TextWidth(WideString(Text));
            {$endif}
          end
          else
            result := 0;
          TWidth := Result;
        end;
      end;
    hitBitmap: result := Width;
    hitHR: result := Width;
    else result := 0;
  end;
end;

procedure TElHTMLItem.Assign(Source : TElHTMLItem);
begin
  if Source <> nil then
  begin
    FontStyle := Source.FontStyle;
    FontHeight := Source.FontHeight;
    FontBgColor := Source.FontBgColor;
    FontColor := Source.FontColor;
    FLinkRef := Source.FLinkRef;
    FFontRef := Source.FFontRef;
    Indent := Source.Indent;
    FFntCnt := Source.FFntCnt;
    FBoolState := Source.FBoolState;
  end;
end;

function TElHTMLItem.GetIsLink: Boolean;
begin
  Result := (FBoolState and flIsLink) = flIsLink;
end;

procedure TElHTMLItem.SetIsLink(Value: Boolean);
begin
  if Value then
    FBoolState := FBoolState or flIsLink
  else
    FBoolState := FBoolState and not flIsLink;
end;

function TElHTMLItem.GetIsSub: Boolean;
begin
  Result := (FBoolState and flSub) = flSub;
end;

procedure TElHTMLItem.SetIsSub(Value: Boolean);
begin
  if Value then
    FBoolState := FBoolState or flSub
  else
    FBoolState := FBoolState and not flSub;
end;

function TElHTMLItem.GetIsSuper: Boolean;
begin
  Result := (FBoolState and flSuper) = flSuper;
end;

procedure TElHTMLItem.SetIsSuper(Value: Boolean);
begin
  if Value then
    FBoolState := FBoolState or flSuper
  else
    FBoolState := FBoolState and not flSuper;
end;

function TElHTMLBreakItem.GetHeight(var BaseLine : integer) : integer;
begin
  BaseLine := 0;
  case Self.ItemType of
    hitLI,
    hitSoftBreak,
    hitBreak:
      begin
        if FOwner.FRender <> nil then
        with FOwner.FRender.Canvas.Font do
        begin
          Charset := FOwner.Charset;
          Name    := FOwner.DefaultFont;
          Height := FOwner.FDefaultHeight;
          result := Abs(Height);
        end else
          result := 0;
      end;
    hitPara:
      begin
        if FOwner.FRender <> nil then
        with FOwner.FRender.Canvas.Font do
        begin
          Charset := FOwner.Charset;
          Name    := FOwner.DefaultFont;
          Height  := FOwner.FDefaultHeight;
          if FOwner.FArray.IndexOf(Self) = 0 then
             result := Abs(Height)
          else
             result := MulDiv(Abs(Height), 3, 2);
        end else
          result := 0;
      end;
    else
      result := 0;
  end;
end;

function TElHTMLBreakItem.GetWidth : integer;
{$ifndef CLX_USED}
{$IFDEF VCL_4_USED}
var TM : tagTextMetricA;
{$ELSE}
var TM : TTextMetricA;
{$ENDIF}
{$else}
var Metrics : QFontMetricsH;
{$endif}
begin
  case Self.ItemType of
    hitLI:
      begin
        if Assigned(FOwner.FRender) then
        begin
          with FOwner.FRender.Canvas.Font do
          begin
            if IsLink then
              Style := FOwner.FLinkStyle
            else
              Style := FontStyle;
            Charset := FOwner.Charset;
            Name  := FFontRef;
            Height  := FontHeight;
          end;
          {$ifndef CLX_USED}
          GetTextMetricsA(FOwner.FRender.Canvas.Handle, TM);
          result := TM.tmAveCharWidth;
          {$else}
          Metrics := QFontMetrics_create(FOwner.FRender.Canvas.Font.Handle);
          result := QFontMetrics_maxWidth(Metrics);
          QFontMetrics_destroy(Metrics);
          {$endif}
        end else
          result := 0;
      end;
    hitSoftBreak,
    hitBreak,
    hitPara: result := 0;
    else result := 0;
  end;
end;

procedure TElHTMLBreakItem.Assign(Source : TElHTMLItem);
begin
  inherited;
  if Source is TElHTMLBreakItem then
  begin
    with TElHTMLBreakItem(Source) do
    begin
      Self.ListLevel := ListLevel;
      Self.ListItemN := ListItemN;
      {$ifdef MSWINDOWS}
      Self.FParams   := FParams and not (DT_LEFT or DT_CENTER or DT_RIGHT);
      {$else}
      Self.FParams   := FParams and not (Integer(AlignmentFlags_AlignLeft) or
                                         Integer(AlignmentFlags_AlignHCenter) or
                                         Integer(AlignmentFlags_AlignRight));
      {$endif}
    end;
  end;
end;

procedure TElHTMLBreakItem.AssignBreakProps(Source : TElHTMLBreakItem);
begin
  if Source <> nil then 
  with Source do
  begin
    Self.ListLevel := ListLevel;
    Self.ListItemN := ListItemN;
    Self.FParams   := FParams;
  end;
end;

constructor TElHTMLData.Create;
begin
  inherited;
  FArray := TElList.Create;
  FLinkColor := clBlue;
  FLinkStyle := [fsUnderline];

  FDefaultColor := clWindowText;
  FDefaultBgColor := clNone;
  FDefaultStyle := [];
  FDefaultHeight := -11;
  FDefaultFont := 'MS Sans Serif';
end;

destructor TElHTMLData.Destroy;
begin
  ClearArray;
  FArray.Free;
  inherited;
end;

procedure TElHTMLData.ClearArray;  { protected }
var i : integer;
    Item : TElHTMLItem;
begin
  for i := 0 to Pred(FArray.Count) do
  begin
    Item := TElHTMLItem(FArray[i]);
    Item.Free;
  end;
  FArray.Clear;
end;  { ClearArray }

function TElHTMLData.LineCount : integer;
var i, j : integer;
begin
  result := 0;
  j := FArray.Count - 1;
  if j > 0 then
  begin
    for i := 0 to j do
      if TElHTMLItem(FArray[i]) is TElHTMLBreakItem then
        inc(result);
  end;
end;

procedure TElHTMLRender.DestroyData(Data : TElHTMLData);
begin
  Data.Free;
  if Data = FData then FData := nil;
end;

function TElHTMLRender.CreateData : TElHTMLData;
begin
  Result := TElHTMLData.Create;
  result.FRender := Self;
end;

procedure TElHTMLRender.SetData(NewData : TElHTMLData);
begin
  FData := newData;
  if FData = nil then
    FData := FIntData;
  FData.FRender := Self;
end;

procedure TElHTMLRender.PrepareText(Text : TElFString; MaxWidth : integer; AutoWrap : boolean);
begin
  PrepareToData(Text, MaxWidth, AutoWrap, FData);
end;

procedure TElHTMLRender.PrepareToData(Text : TElFString;
    MaxWidth : integer; AutoWrap : boolean; CurData : TElHTMLData);

type TFontSettings = record
       FontName : TFontName;
       Height   : integer;
       Color    : TColor;
       BgColor  : TColor;
     end;
     PFontSettings = ^TFontSettings;

const

  TElFQuote1 = TElFChar('''');
  TElFQuote2 = TElFChar('"');

var
    p        : PElFChar;
    BPrp     : TElHTMlBreakItem;
    DefPrp,
    CurPrp   : TElHTMLItem;
    CurC     : TElFChar;
    s        : TElFString;
    b        : boolean;
    i        : integer;
    FStack   : TElStack;
    //FSaveFont,
    FCurFont : PFontSettings;
    FSaveData: TElHTMLData;
    ListLevel: integer;
    LSPos    : integer;

    function GetLastBreakItem(AArray : TElList): TElHTMLBreakItem;
    var i : integer;
    begin
      Result := nil;
      for i := AArray.Count - 1 downto 0 do
      begin
        if TObject(AArray[i]) is TElHTMLBreakItem then
        begin
          result := TElHTMLBreakItem(AArray[i]);
          break;
        end;
      end;
    end;

    procedure SetItemFont(Item : TElHTMLItem; Font : PFontSettings);
    begin
      with Item, Font^ do
      begin
        FFontRef := FontName;
        FontColor := Color;
        FontBgColor := BgColor;
        FontHeight := Height;
      end;
    end;

    procedure SetFontFromItem(Item : TElHTMLItem; Font : PFontSettings);
    begin
      with Item, Font^ do
      begin
        FontName := FFontRef;
        Color    := FontColor;
        BgColor  := FontBgColor;
        Height   := FontHeight;
      end;
    end;

    procedure SkipComment;
    var s : string;
        p1: PElFChar;
    begin
      s := '';
      while (P^ <> #0) do
      begin
        if (P^ = '-') then
        begin
          p1 := P;
          Inc(p1);
          if P1^ = '-' then
          begin
            inc(p1);
            if P1^ = '>' then
            begin
              inc(p, 2);
              exit;
            end
            else
              inc(p);
          end
          else
            inc(p);
        end
        else
          inc(p);
      end;
      dec(p);
    end;

    function FindParamName(SL : TStringList; Name : string) : integer;
    var i : integer;
    begin
      for i := 0 to Pred(SL.Count) do
      begin
        if SL.Names[i] = Name then
        begin
          result := i;
          exit;
        end;
      end;
      result := -1;
    end;

    procedure CollectParameters(SL : TElFStringList);
    var
        S, S1,
        S2    : TElFString;
        quote : TElFChar;
        CurC  : TElFChar;
        quoted: boolean;
    begin
      S := '';
      quote := #0;
      quoted := False;
      while (P^ <> #0) do
      begin
        CurC := P^;
        if quoted then
        begin
          if CurC = quote then
          begin
            quoted := False;
          end;
        end
        else
        begin
          if CurC in [TElFQuote1, TElFQuote2] then
          begin
            quote := CurC;
            quoted := True;
          end;
        end;
        if (not quoted) and (CurC in [TElFChar(#32), TElFChar('>')]) then
        begin
{$ifdef ELPACK_UNICODE}
          if Assigned(WideStrScan(PWideChar(S), TElFChar('='))) then
{$else}
          if (Pos('=', S) > 0) then
{$endif}
          begin
{$ifdef ELPACK_UNICODE}
            S1 := WideCopy(S, 1, Pos('=', S) - 1);
            S2 := WideCopy(S, Pos('=', S), Length(S));
{$else}
            S1 := Copy(S, 1, Pos('=', S) - 1);
            S2 := Copy(S, Pos('=', S), Length(S));
{$endif}
            SL.Add(LowerCase(S1) + S2);
          end;
          S := '';
          if CurC = '>' then break;
        end
        else
          if (CurC <> #32) or (S = '') or (S[Length(S)] <> #32) then
             S := S + CurC;
        Inc(P);
      end;
    end;

    procedure SkipToTagEnd;
    begin
      while not (P^ in [TElFChar(#0), TElFChar('>')]) do
        inc(p);
      if P^ = #0 then Dec(p);
    end;

    function RemoveQuotes(const S : TElFString) : TElFString;
    var
      i : integer;
    begin
      i := Length(S);
      if i = 0 then
        result := ''
      else
      begin
        if (S[1] in [TElFQuote1, TElFQuote2]) then
        begin
          result := Copy(S, 2, i - 1);
          if S[i] = S[1] then
            SetLength(result, i - 2);
        end else
          result := S;
      end;
    end;

    procedure ProcessTag;
    var S    : TElFString;
        BPrp : TElHTMlBreakItem;
        APrp : TElHTMLItem;
        SL   : TElFStringList;
        {$ifndef CLX_USED}
        Bmp  : Graphics.TBitmap;
        {$else}
        Bmp  : QGraphics.TBitmap;
        {$endif}
        b    : boolean;
        c    : Integer;

      procedure ProcessFontStyleTag(Style: TFontStyle);
      begin
        if b then CurData.FArray.Add(CurPrp);
        APrp := TElHTMLItem.Create(CurData);
        APrp.Assign(CurPrp);
        if S[1] <> TElFChar('/') then
           Include(APrp.FontStyle, Style)
        else
           Exclude(APrp.FontStyle, Style);
        if not b then CurPrp.Free;
        CurPrp := APrp;
        if SL <> nil then SkipToTagEnd;
      end;

      procedure ProcessSubStyleTag(Style: integer);
      begin
        if b then
          CurData.FArray.Add(CurPrp);
        APrp := TElHTMLItem.Create(CurData);
        APrp.Assign(CurPrp);

        if S[1] <> TElFChar('/') then
          APrp.FBoolState := APrp.FBoolState or Style
        else
          APrp.FBoolState := APrp.FBoolState and not Style;

        if not b then CurPrp.Free;
        CurPrp := APrp;
        if SL <> nil then
          SkipToTagEnd;
      end;

    begin
      inc(p);
      S := '';
      while not (P^ in [TElFChar(#0), TElFChar('>'), TElFChar(' ')]) do
      begin
        s := s + P^;
        if S = TElFString('!--') then
        begin
          SkipComment;
          exit;
        end;
        inc(p);
      end;
      if P^ = #0 then
      begin
        dec(p);
        exit;
      end;
      SL := nil;
      try
        if P^ = ' ' then
        begin
          SL := TElFStringList.Create;
          CollectParameters(SL);
        end;
        s := lowercase(s);
        b := (CurPrp.ItemType <> hitChar) or (Length(CurPrp.Text) > 0);
        if (S = TElFString('sub')) or (S = TElFString('/sub')) then
        begin
          ProcessSubStyleTag(flSub);
        end
        else
        if (S = TElFString('sup')) or (S = TElFString('/sup')) then
        begin
          ProcessSubStyleTag(flSuper);
        end
        else
        if (S = TElFString('b')) or (S = TElFString('/b')) then
        begin
          ProcessFontStyleTag(fsBold);
        end
        else
        if (S = TElFString('i')) or (S = TElFString('/i')) then
        begin
          ProcessFontStyleTag(fsItalic);
        end
        else
        if (S = TElFString('strikeout')) or (S = TElFString('/strikeout')) then
        begin
          ProcessFontStyleTag(fsStrikeOut);
        end else
        if (S = TElFString('u')) or (S = TElFString('/u')) then
        begin
          ProcessFontStyleTag(fsUnderline);
        end else
        if s = TElFString('ul') then
        begin
          if b then CurData.FArray.Add(CurPrp);
          APrp := TElHTMLItem.Create(CurData);
          APrp.Assign(CurPrp);
          if not b then CurPrp.Free;
          Inc(ListLevel);
          APrp.ItemType := hitUL;
          APrp.Indent := APrp.Indent + APrp.GetWidth;
          APrp.ItemType := hitChar;
          CurPrp := APrp;
        end else
        if s = TElFString('/ul') then
        begin
          if b and (CurPrp.ItemType <> hitBreak) then CurData.FArray.Add(CurPrp);
          APrp := TElHTMLItem.Create(CurData);
          APrp.Assign(CurPrp);
          if (not b) or (CurPrp.ItemType = hitBreak) then CurPrp.Free;
          Dec(ListLevel);
          APrp.ItemType := hitUL;
          APrp.Indent := APrp.Indent - APrp.GetWidth;
          if TElHTMLItem(CurData.FArray.Last).ItemType = hitBreak then
             CurData.FArray.Delete(CurData.FArray.Count - 1);
          CurPrp := TElHTMLBreakItem.Create(CurData);
          CurPrp.Assign(APrp);
          CurPrp.ItemType := hitBreak;
          CurData.FArray.Add(CurPrp);
          APrp.ItemType := hitChar;
          CurPrp := APrp;
        end else
        if s = TElFString('li') then
        begin
          if b and (CurPrp.ItemType <> hitBreak) then CurData.FArray.Add(CurPrp);
          APrp := TElHTMLItem.Create(CurData);
          APrp.Assign(CurPrp);
          if (not b) or (CurPrp.ItemType = hitBreak) then CurPrp.Free;
          if TElHTMLItem(CurData.FArray.Last).ItemType = hitBreak then
             CurData.FArray.Delete(CurData.FArray.Count - 1);
          CurPrp := TElHTMLBreakItem.Create(CurData);
          CurPrp.ItemType := hitLI;
          TElHTMLBreakItem(CurPrp).ListItemN := -1;
          TElHTMLBreakItem(CurPrp).ListLevel := ListLevel;

          CurPrp.Assign(APrp);
          CurData.FArray.Add(CurPrp);

          APrp.ItemType := hitChar;
          CurPrp := APrp;
        end;
        if s = TElFString('p') then
        begin
          if b then CurData.FArray.Add(CurPrp);
          APrp := TElHTMLItem.Create(CurData);
          APrp.Assign(CurPrp);
          if not b then CurPrp.Free;
          CurPrp := TElHTMLBreakItem.Create(CurData);
          CurPrp.ItemType := hitPara;
          CurPrp.Assign(APrp);
          {$ifdef MSWINDOWS}
          TElHTMLBreakItem(CurPrp).FParams := 0;
          {$else}
          TElHTMLBreakItem(CurPrp).FParams := Integer(AlignmentFlags_AlignLeft);
          {$endif}
          if SL <> nil then
          begin
            s := lowercase(RemoveQuotes(SL.Values[TElFString('align')]));
            {$ifdef MSWINDOWS}
            TElHTMLBreakItem(CurPrp).FParams := DT_LEFT;
            {$else}
            TElHTMLBreakItem(CurPrp).FParams := Integer(AlignmentFlags_AlignLeft);
            {$endif}
            if s <> '' then
            begin
              {$ifdef MSWINDOWS}
              if s = TElFString('left') then
                 TElHTMLBreakItem(CurPrp).FParams := DT_LEFT
              else
              if s = TElFString('center') then
                 TElHTMLBreakItem(CurPrp).FParams := DT_CENTER
              else
              if s = TElFString('right') then
                 TElHTMLBreakItem(CurPrp).FParams := DT_RIGHT;
              {$else}
              if s = TElFString('left') then
                 TElHTMLBreakItem(CurPrp).FParams := Integer(AlignmentFlags_AlignLeft)
              else
              if s = TElFString('center') then
                 TElHTMLBreakItem(CurPrp).FParams := Integer(AlignmentFlags_AlignHCenter)
              else
              if s = TElFString('right') then
                 TElHTMLBreakItem(CurPrp).FParams := Integer(AlignmentFlags_AlignRight);
              {$endif}
            end;

            s := RemoveQuotes(SL.Values[TElFString('indent')]);
            if (Length(s) > 0) then
              TElHTMLBreakItem(CurPrp).Indent := StrToIntDef(s, 0);
          end;
          APrp.Assign(CurPrp);
          CurData.FArray.Add(CurPrp);
          APrp.ItemType := hitChar;
          CurPrp := APrp;
        end else
        if s = TElFString('br') then
        begin
          if b then CurData.FArray.Add(CurPrp);
          APrp := TElHTMLItem.Create(CurData);
          APrp.Assign(CurPrp);
          if not b then CurPrp.Free;

          CurPrp := TElHTMLBreakItem.Create(CurData);
          CurPrp.Assign(APrp);
          CurPrp.ItemType := hitBreak;

          BPrp := GetLastBreakItem(CurData.FArray);
          if BPrp = nil then
          begin
            {$ifdef MSWINDOWS}
            TElHTMLBreakItem(CurPrp).FParams := 0;
            {$else}
            TElHTMLBreakItem(CurPrp).FParams := Integer(AlignmentFlags_AlignLeft);
            {$endif}
          end
          else
            TElHTMLBreakItem(CurPrp).FParams := BPrp.FParams;

          CurData.FArray.Add(CurPrp);
          CurPrp := APrp;
        end else
        if s = TElFString('img') then
        begin
          if SL <> nil then
          begin
            s := RemoveQuotes(SL.Values[TElFString('src')]);
            if Length(S) > 0 then
            begin
              if b then CurData.FArray.Add(CurPrp);
              APrp := TElHTMLItem.Create(CurData);
              APrp.Assign(CurPrp);
              APrp.ItemType := hitBitmap;
              APrp.Text := s;
              TriggerImageNeededEvent(S, Bmp);
              s := RemoveQuotes(SL.Values[TElFString('width')]);
              if Bmp <> nil then
                 APrp.Width := StrToIntDef(s, Bmp.Width)
              else
                 APrp.Width := StrToIntDef(s, 0);
              s := RemoveQuotes(SL.Values[TElFString('height')]);
              if Bmp <> nil then
                 APrp.Height := StrToIntDef(s, Bmp.height)
              else
                 APrp.Height := StrToIntDef(s, 0);
              CurData.FArray.Add(APrp);
              APrp := TElHTMLItem.Create(CurData);
              APrp.Assign(CurPrp);
              APrp.ItemType := hitChar;
              if not b then CurPrp.Free;
              CurPrp := APrp;
            end;
          end;
        end else
        if s = TElFString('a') then
        begin
          if SL <> nil then
          begin
            s := RemoveQuotes(SL.Values[TElFString('href')]);
            if b then CurData.FArray.Add(CurPrp);
            APrp := TElHTMLItem.Create(CurData);
            APrp.Assign(CurPrp);
            APrp.IsLink  := true;
            APrp.FLinkRef := s;
            if not b then
               CurPrp.Free;
            CurPrp := APrp;
          end;
        end else
        if s = TElFString('/a') then
        begin
          if b then CurData.FArray.Add(CurPrp);
          APrp := TElHTMLItem.Create(CurData);
          APrp.Assign(CurPrp);
          APrp.FLinkRef := '';
          APrp.IsLink := false;
          if not b then CurPrp.Free;
          CurPrp := APrp;
        end else
        if s = TElFString('/p') then
        begin
          // intentionally left blank
        end else
        if s = TElFString('html') then
        begin
          // intentionally left blank
        end else
        if s = TElFString('hr') then
        begin
          if b then CurData.FArray.Add(CurPrp);
          // add line break
          APrp := TElHTMLBreakItem.Create(CurData);
          APrp.Assign(CurPrp);
          APrp.ItemType := hitBreak;
          CurData.FArray.Add(APrp);
          // add HR
          APrp := TElHTMLItem.Create(CurData);
          APrp.Assign(CurPrp);
          CurData.FArray.Add(APrp);
          // create new token
          CurPrp := TElHTMLBreakItem.Create(CurData);
          CurPrp.Assign(APrp);
          APrp.ItemType := hitHR;
          if (SL <> nil) then
          begin
            s := RemoveQuotes(SL.Values[TElFString('width')]);
            APrp.Width := StrToIntDef(s, 0);
          end;
          APrp := TElHTMLBreakItem.Create(CurData);
          APrp.Assign(CurPrp);
          APrp.ItemType := hitBreak;
          CurData.FArray.Add(APrp);
        end;
        if s = TElFString('/font') then
        begin
          if b then CurData.FArray.Add(CurPrp);
          APrp := TElHTMLItem.Create(CurData);
          APrp.Assign(CurPrp);
          if FStack.Count > 0 then
          begin
            if (PFontSettings(FStack[FStack.Count - 1]).Color <> FCurFont.Color) or
               (PFontSettings(FStack[FStack.Count - 1]).BgColor <> FCurFont.BgColor) then
              Dec(APrp.FFntCnt);

            Dispose(FCurFont);
            FCurFont := FStack.Pop;
          end;
          SetItemFont(APrp, FCurFont);
          if not b then CurPrp.Free;
          CurPrp := APrp;
          if SL <> nil then SkipToTagEnd;
        end else
        if s = TElFString('font') then
        begin
          if b then CurData.FArray.Add(CurPrp);
          APrp := TElHTMLItem.Create(CurData);
          APrp.Assign(CurPrp);
          FStack.Push(FCurFont);
          New(FCurFont);
          SetFontFromItem(CurPrp, FCurFont);
          if SL <> nil then
          begin
            s := RemoveQuotes(SL.Values[TElFString('face')]);
            if s <> '' then
            begin
              FCurFont.FontName := S;
            end;
            s := RemoveQuotes(SL.Values[TElFString('name')]);
            if s <> '' then
            begin
              FCurFont.FontName := S;
            end;
            s := RemoveQuotes(SL.Values[TElFString('size')]);
            if Length(s) > 0 then
            begin
              if (s[1] = TElFChar('+')) or (s[1] = TElFChar('-')) then
              begin
                FCurFont.Height := CurData.FDefaultHeight - MulDiv(StrToIntDef(S, 0), ScreenPixelsPerInch, 72);
              end
              else
              begin
                FCurFont.Height := StrToIntDef(S, CurData.FDefaultHeight);
              end;
            end;
            s := RemoveQuotes(SL.Values[TElFString('color')]);
            if Length(s) > 0 then
            begin
              if s[1] = TElFChar('#') then
              begin
                Delete(s, 1, 1);
                FCurFont.Color := swapInt32(H2DDef(S, CurData.FDefaultColor) shl 8);
              end else
              begin
                if not ContainsAt(s, 1, 'cl') then
                begin
                  s := TElFString('cl') + s;
                end;
                if IdentToColor(s, c) then
                   FCurFont.Color := c;
              end;
            end;
            s := RemoveQuotes(SL.Values[TElFString('bgcolor')]);
            if Length(s) > 0 then
            begin
              if s[1] = TElFChar('#') then
              begin
                Delete(s, 1, 1);
                FCurFont.BgColor := swapInt32(H2DDef(S, CurData.FDefaultBgColor) shl 8);
              end else
              begin
                if not ContainsAt(s, 1, 'cl') then
                begin
                  s := TElFString('cl') + s;
                end;
                if IdentToColor(s, c) then
                   FCurFont.BgColor := c;
              end;
            end;
            if ((Length(SL.Values[TElFString('color')]) > 0)) or
               ((Length(SL.Values['bgcolor'])) > 0) then
             Inc(APrp.FFntCnt);
          end;
          SetItemFont(APrp, FCurFont);
          if not b then CurPrp.Free;
          CurPrp := APrp;
        end;
      finally
        SL.Free;
      end;
    end;

    function GetCurWidth : integer;
    var i : integer;
        Item : TElHTMLItem;
    begin
      Result := CurPrp.Width;
      for i := Pred(CurData.FArray.Count) downto 0 do
      begin
        Item := TElHTMLItem(CurData.FArray[i]);
        if Item is TElHTMLBreakItem then
        begin
          Inc(Result, Item.Indent);
          break;
        end
        else
          Inc(Result, Item.GetWidth);
      end;
    end;

begin

  FSaveData := FData;
  FData := CurData;
  FData.ClearArray;
  //FSaveFont := nil;
  with CurData do
  begin
    try
      FSelectedItem := nil;
      p := PElFChar(Text);
      DefPrp := TElHTMLItem.Create(CurData);
      try
        FStack := TElStack.Create;
        try
          New(FCurFont);
          //FSaveFont := FCurFont;
          FCurFont.FontName := '';
          DefPrp.FontStyle := FDefaultStyle;
          FCurFont.FontName := DefaultFont;
          FCurFont.Color := FDefaultColor;
          FCurFont.BgColor := FDefaultBgColor;
          FCurFont.BgColor := FDefaultBgColor;
          FCurFont.Height := FDefaultHeight;
          SetItemFont(DefPrp, FCurFont);

          CurPrp := TElHTMLItem.Create(CurData);
          CurPrp.Assign(DefPrp);
          while P ^ <> #0 do
          begin
            CurC := #0;
            if P^ = TElFChar('<') then
              ProcessTag
            else
            begin
              if P^ = TElFChar(#13) then
              begin
                inc(P);
                if P^ <> TElFChar(#10) then dec(p);
              end
              else
              begin
                if P^ = TElFChar('&') then
                begin
                  inc(P);
                  if P^ = TElFChar('#') then
                  begin
                    inc(p);
                    s := '';
                    while (p^<>TElFChar(#0)) and (p^<>TElFChar(';')) do
                    begin
                      s := s + P^;
                      inc(p);
                    end;
                    i := StrToIntDef(S, 32);
                    if i <> 32 then
                       CurC := TElFChar(i)
                    else
                      if (Length(CurPrp.Text) = 0) or (CurPrp.Text[Length(CurPrp.Text)] <> TElFChar(#32)) then
                         CurC := TElFChar(#32);
                    if (p^) = TElFChar(#0) then dec(p);
                  end
                  else
                  begin
                    s := '';
                    while (p^ <> TElFChar(#0)) and (p^ <> TElFChar(';')) do
                    begin
                      s := s + P^;
                      inc(p);
                    end;
                    s := Lowercase(s);
                    if s = TElFString('nbsp') then
                       curC := TElFChar(#32)
                    else
                    if s = TElFString('amp') then
                       curC := TElFChar('&')
                    else
                    {if s = TElFString('apos') then
                       curC := ''''
                    else
                    }
                    if s = TElFString('quot') then
                       curC := TElFChar('"')
                    else
                    if s = TElFString('gt') then
                       CurC := TElFChar('>')
                    else
                    if s = TElFString('lt') then
                       CurC := TElFChar('<')
                    else
                    if s = TElFString('euro') then
                       CurC := 'À'
                    else
                    if s = TElFString('sect') then
                       CurC := #$A7
                    else
                    if s = TElFString('reg') then
                       CurC := #$AE
                    else
                    if s = TElFString('copy') then
                       CurC := #$A9
                    else
                    if s = TElFString('para') then
                       CurC := #$B6
                    else
                    if s = TElFString('trade') then
                       CurC := #$99;

                    if (p^) = #0 then dec(p);
                  end;
                end else
                if P^ = #32 then
                begin
                  if (Length(CurPrp.Text) = 0) or (CurPrp.Text[Length(CurPrp.Text)] <> #32) then
                     CurC := P^;
                end
                else
                  CurC := P^;
              end;

              if CurC <> #0 then
              begin
                CurPrp.Text := CurPrp.Text + CurC;
                CurPrp.TWidth := Word(-1);
                if AutoWrap and (GetCurWidth + CurPrp.GetWidth > MaxWidth) then
                begin
                  CurPrp.TWidth := WORD(-1);
                  b := false;
                  {$ifdef ELPACK_UNICODE}
                  LSPos := WideLastPos(' ', CurPrp.Text);
                  {$else}
                  LSPos := LastPos(' ', CurPrp.Text);
                  {$endif}
                  if (LSPos = 0) and (TElHTMLItem(FArray.Last) is TElHTMLBreakItem) then
                  begin
                    s := CurC;
                    SetLength(CurPrp.FText, Length(CurPrp.FText) - 1);
                  end
                  else
                    S := TStrExtractStr(CurPrp.FText, LSPos + 1, Length(CurPrp.Text));

                  if CurPrp.Text <> '' then
                    b := true;
                  FArray.Add(CurPrp);

                  CurPrp := TElHTMLBreakItem.Create(CurData);
                  CurPrp.ItemType := hitSoftBreak;
                  CurPrp.Assign(TElHTMLItem(FArray.Last));
                  BPrp := GetLastBreakItem(FArray);
                  TElHTMLBreakItem(CurPrp).AssignBreakProps(BPrp);
                  CurData.FArray.Add(CurPrp);
                  CurPrp := TElHTMLItem.Create(CurData);
                  CurPrp.ItemType := hitChar;
                  CurPrp.Assign(TElHTMLItem(CurData.FArray.Last));
                  CurPrp.Text := S;
                  CurPrp.TWidth := Word(-1);
                  with CurData do
                    if not b then
                      FArray.Delete(FArray.Count - 2);
                end;
              end;
            end;
            inc(p);
          end;
          if (CurPrp.ItemType <> hitChar) or (Length(CurPrp.Text) > 0) then
             FArray.Add(CurPrp)
          else
             CurPrp.Free;
          if FArray.Count > 0 then
          begin
            CurPrp := TElHTMLItem(FArray[0]);
            if CurPrp.ItemType <> hitPara then
            begin
              CurPrp := TElHTMLBreakItem.Create(CurData);
              CurPrp.ItemType := hitPara;
              {$ifdef MSWINDOWS}
              TElHTMLBreakItem(CurPrp).FParams := DT_LEFT;
              {$else}
              TElHTMLBreakItem(CurPrp).FParams := Integer(AlignmentFlags_AlignLeft);
              {$endif}
              FArray.Insert(0, CurPrp);
            end;
          end;
          CalcTokenSizes(CurData);
        finally
          Dispose(FCurFont);
          while FStack.Count > 0 do
          begin
            FCurFont := FStack.Pop;
            Dispose(FCurFont);
          end;
          FStack.Free;
        end;
      finally
        DefPrp.Free;
      end;
    except
      on E : Exception do
      begin
        ClearArray;
        Raise Exception.Create('ElPack HTML engine error - the text is not valid HTML');
      end;
    end;
  end;
  FData := FSaveData;
end;  { PrepareText }

procedure TElHTMLRender.DrawText(Canvas : TCanvas; SrcPoint : TPoint; R : TRect; AdjustFromColor : TColor);  { public }
begin
  DrawTextEx(Canvas, SrcPoint, R, false, 0, 0, 0, 0, AdjustFromColor);
end;

procedure TElHTMLRender.DrawTextEx(Canvas : TCanvas; SrcPoint : TPoint; R : TRect; UseOverColors : boolean; Color, BkColor, SelColor, SelBkColor : TColor; AdjustFromColor : TColor);
var i, j,
    CurX,
    CurY      : integer;
    RealX,
    RealY     : integer;
    BreakItem : TElHTMLBreakItem;
    Item      : TElHTMLItem;
    bmp       : TBitmap;
    R1        : TRect;
    c         : TColor;
    fx        : integer;
    //bladj     : Integer;
begin
  if FData = nil then exit;
  // prepare a temp canvas for painting
  Bitmap.Height := R.Bottom - R.Top + 1;
  Bitmap.Width := R.Right - R.Left + 1;
  Bitmap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
  {$ifndef CLX_USED}
  bitblt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Canvas.Handle, R.Left, R.Top, SRCCOPY);
  {$else}
  bitblt(Bitmap.Handle, 0, 0, QPainter_device(Canvas.Handle), R.Left, R.Top, Bitmap.Width, Bitmap.Height, RasterOp_CopyROP, true);
  {$endif}
  CurX := 0;
  CurY := 0;
  BreakItem := nil;
  j := FData.FArray.Count - 1;
  {$ifndef CLX_USED}
  SetTextAlign(Bitmap.Canvas.Handle, TA_BASELINE or TA_LEFT or TA_NOUPDATECP);
  //bladj := 0;
  {$else}
  Bitmap.Canvas.TextAlign := taBottom;
  {$endif}
  Bitmap.Canvas.Font.Charset := FData.Charset;
  FData.Rect := R;
  for i := 0 to j do
  begin
    Item := TElHTMLItem(FData.FArray[i]);
    if Item is TElHTMLBreakItem then
    begin
      if BreakItem <> nil then
         curY := curY + BreakItem.Height
      else
         BreakItem := TElHTMLBreakItem(Item);
      if CurY > Bitmap.Height + SrcPoint.Y then break;

      {$ifdef MSWINDOWS}
      case LoWord(TElHTMLBreakItem(Item).FParams) of
        DT_LEFT:
          curX := TElHTMLBreakItem(Item).Indent;
        DT_RIGHT:
          curX := Max(BreakItem.Width, Bitmap.Width) - Item.Width - 1;
        DT_CENTER:
          curX := (Max(BreakItem.Width, Bitmap.Width) - 1 - Item.Width) div 2;
      {$else}
      case TElHTMLBreakItem(Item).FParams and $0000FFFF of
        Integer(AlignmentFlags_AlignLeft):
          curX := TElHTMLBreakItem(Item).Indent;
        Integer(AlignmentFlags_AlignRight):
          curX := Max(BreakItem.Width, Bitmap.Width) - Item.Width - 1;
        Integer(AlignmentFlags_AlignHCenter):
          curX := (Max(BreakItem.Width, Bitmap.Width) - 1 - Item.Width) div 2;
      {$endif}

      end;

      BreakItem := TElHTMLBreakItem(Item);

      if Item.ItemType = hitLI then
      begin
        R1.Left := CurX - SrcPoint.x - Item.GetWidth - 1;
        R1.Right := CurX - SrcPoint.x - 1;
        R1.Top := CurY - SrcPoint.Y + (BreakItem.Height - BreakItem.FParams shr 16 - (R1.Right - R1.Left)) div 2
                  + BreakItem.FParams shr 16;
        R1.Bottom := R1.Top + (R1.Right - R1.Left);

        if UseOverColors and (Item.FFntCnt = 0) then
          c := Color
        else
          c := Item.FontColor;

        Bitmap.Canvas.Brush.Color := c;
        with R1 do
          Bitmap.Canvas.Ellipse(Left, Top, Right, Bottom);
      end;

      if CurY + BreakItem.Height < SrcPoint.Y then
        Continue;
    end
    else
    begin
      if Item.ItemType = hitChar then
      begin
        with Bitmap.Canvas do
        begin
          if (FData.FSelectedItem = Item) then
          begin
            if UseOverColors then
               c := SelBkColor
            else
               c := FData.FHighlightBgColor;
            if c = clNone then
               Brush.Style := bsClear
            else
              Brush.Color := c;
          end
          else
          begin
            if UseOverColors and (Item.FFntCnt = 0) then
               c := BkColor
            else
               c := Item.FontBgColor;
            begin
              if c = clNone then
                 Brush.Style := bsClear
              else
                Brush.Color := c;
            end;
          end;
          if Item = FData.FSelectedItem then
          begin
            if UseOverColors then
               c := SelColor
            else
               c := FData.FHighlightColor;

            if AdjustFromColor <> clNone then
              Font.Color := AdjustColor(c, AdjustFromColor)
            else
              Font.Color := c;

            if Item.IsLink then
               Font.Style := FData.FLinkStyle
            else
               Font.Style := Item.FontStyle;
          end
          else
          if Item.IsLink then
          begin
            if AdjustFromColor <> clNone then
              Font.Color := AdjustColor(FData.FLinkColor, AdjustFromColor)
            else
              Font.Color := FData.FLinkColor;
            Font.Style := FData.FLinkStyle;
          end
          else
          begin
            if UseOverColors and (Item.FFntCnt = 0) then
            begin
              if AdjustFromColor <> clNone then
                Font.Color := AdjustColor(Color, AdjustFromColor)
              else
                Font.Color := Color;
            end
            else
            begin
              if AdjustFromColor <> clNone then
                Font.Color := AdjustColor(Item.FontColor, AdjustFromColor)
              else
                Font.Color := Item.FontColor;
            end;
            Font.Style := Item.FontStyle;
          end;
          Font.Name := Item.FFontRef;
          if Item.IsSub or Item.IsSuper then
            Font.Height := Item.FontHeight - 2 * Sign(Item.FontHeight)
          else
            Font.Height := Item.FontHeight;

          RealX := CurX - SrcPoint.x;
          RealY := CurY - SrcPoint.Y;
          if Item.IsSuper then
            RealY := RealY - Item.GetHeight(fx) div 2
          else
          if Item.IsSub then
            RealY := RealY + Item.GetHeight(fx) div 4;

          {$ifndef CLX_USED}
          {$ifdef ELPACK_UNICODE}
          Windows.TextOutW(Bitmap.Canvas.Handle,
                           RealX,
                           RealY + BreakItem.Height -
                           HIWORD(BreakItem.FParams),
                           PWideChar(Item.Text),
                           Length(Item.Text));
          {$else}
          Windows.TextOut (Bitmap.Canvas.Handle,
                           RealX,
                           RealY + BreakItem.Height -
                           HIWORD(BreakItem.FParams),
                           Pchar(Item.Text),
                           Length(Item.Text));
          {$endif}
          {$else}
          Bitmap.Canvas.TextOut(RealX,
                  RealY + BreakItem.Height -
                  BreakItem.FParams shr 16 * 2,
                  WideString(Item.Text));
          {$endif}
        end;
        inc(CurX, Item.Width);
      end
      else
      if Item.ItemType = hitBitmap then
      begin
        TriggerImageNeededEvent(Item.Text, Bmp);
        if Bmp <> nil then
        begin
          DrawTransparentBitmapEx(Bitmap.Canvas.Handle, Bmp,
                                  CurX - SrcPoint.X,
                                  CurY - SrcPoint.Y,
                                  Rect(0, 0, Item.Width, Item.Height),
{$ifndef CLX_USED}
                                  Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
{$else}
                                  Bmp.TransparentColor);
{$endif}
          {Bitmap.Canvas.StretchDraw(Rect(CurX - SrcPoint.x, CurY + BreakItem.Height - Item.Height - SrcPoint.y,
                                         CurX - SrcPoint.x + Item.Width, CurY - SrcPoint.y + Item.Height), Bmp);
          }
        end;
        inc(CurX, Item.Width);
      end
      else
      if Item.ItemType = hitHR then
      begin
        if Item.Width = 0 then
           R1 := Rect(CurX - SrcPoint.X, CurY  + BreakItem.Height div 2 - SrcPoint.Y, Max(BreakItem.Width, Bitmap.Width), CurY  + BreakItem.Height div 2 + 2 - SrcPoint.Y)
        else
           R1 := Rect(CurX - SrcPoint.X, CurY  + BreakItem.Height div 2 - SrcPoint.Y, CurX - SrcPoint.X + Item.Width, CurY  + BreakItem.Height div 2 + 2 - SrcPoint.Y);
        ElVCLUtils.DrawBevel(Bitmap.Canvas.Handle, R1, clBtnShadow, clBtnFace, [ebsTop, ebsBottom]);
      end;
    end;
  end;
{$ifndef CLX_USED}
  bitblt(Canvas.Handle, R.Left, R.Top, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  {$else}
  //Canvas.CopyRect(R, Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.height));
  bitblt(QPainter_device(Canvas.Handle), R.Left + FData.TextOffset.x, R.Top + FData.TextOffset.y, Bitmap.Handle, 0, 0, Bitmap.Width, Bitmap.Height, RasterOp_CopyROP, true);
  {$endif}
end;  { DrawText }

function TElHTMLRender.IsCursorOverLink(Point : TPoint; SrcPoint : TPoint; R : TRect; var href : TElFString) : Boolean;  { public }
var Item : TElHTMLItem;
begin
  href := '';
  Item := Self.FindItemAt(Point, SrcPoint, R);
  if Assigned(Item) then
  begin
    Result := Item.IsLink;
    if Result then
      href := Item.FLinkRef;
  end
  else
    Result := false;
end;  { IsCursorOverLink }

procedure TElHTMLRender.TriggerImageNeededEvent(Src : TElFString; var Image : TBitmap);
begin
  Image := nil;
  if (assigned(FOnImageNeeded)) then
    FOnImageNeeded(Self, Src , Image );
end;  { TriggerImageNeededEvent }

function TElHTMLRender.GetTextSize : TSize;
var i : integer;
    Item : TElHTMLItem;
begin
  Result.cx := 0;
  Result.cy := 0;
  if FData = nil then exit;
  for i := 0 to Pred(FData.FArray.Count) do
  begin
    Item := TElHTMLItem(FData.FArray[i]);
    if (Item is TElHTMLBreakItem) then
    begin
      Result.cx := Max(Result.cx, Item.Width);
      Inc(Result.cy, Item.Height);
    end;
  end;
end;

procedure TElHTMLRender.CalcTokenSizes(FCurData : TElHTMLData);
var i, j,
    cury  : integer;

    function ProcessBlock(StartI : integer; var StartY : integer) : integer;
    var Item : TElHTMLItem;
        {
         TxtH,
         CurX,
         CurY,
        }
        cbl,
        mbl,
        curW,
        curH : integer;
    begin
      result := StartI;
      CurW := 0;
      CurH := 0;
      mbl  := 0;
      //TxtH := 0;
      Item := TElHTMLItem(FCurData.FArray[StartI]);
      Assert(Item <> nil);
      repeat
        Item.Width := Item.GetWidth;
        inc(CurW, Item.Width);
        Item.Height := Item.GetHeight(cbl);
        if Item.ItemType = hitChar then
          if Item.IsSub then
          begin
            inc(cbl, Item.Height div 4);
            inc(Item.Height, Item.Height div 4);
          end
          else
          if Item.IsSuper then
          begin
            dec(cbl, Item.Height div 2);
            inc(Item.Height, Item.Height * 2 div 3);
          end;

        mbl := max(mbl, cbl);
        CurH := Max(CurH, Item.Height);
        {
        if Item.TexType = hitChar then
           TxtH := Max(TxtH, Item.Height);
        }
        inc(result);
        if result = FCurData.FArray.Count then break;
        Item := TElHTMLItem(FCurData.FArray[result]);
      until (Item is TElHTMLBreakItem);
      Item := TElHTMLItem(FCurData.FArray[StartI]);
      Item.Height := CurH;
      Item.Width  := CurW + TElHTMLBreakItem(Item).Indent;
      TElHTMLBreakItem(Item).FParams := (mbl shl 16) or (TElHTMLBreakItem(Item).FParams and $0000FFFF);
      StartY := StartY + CurH;
    end;

begin
  i := 0;
  cury := 0;
  j := FCurData.FArray.Count - 1;
  while i <= j do
    i := ProcessBlock(i, CurY);

  FcurData.FTextSize := GetTextSize;
end;  { ParseText }

procedure TElHTMLRender.SelectLinkAt(Point : TPoint; SrcPoint : TPoint; R : TRect);  { public }
var Item : TElHTMLItem;
begin
  if FData = nil then exit;
  Item := FindItemAt(Point, SrcPoint, R);
  FData.FSelectedItem := Item;
end;  { HighlightItemAt }

function TElHTMLRender.FindItemAt;
var i, j,
    k,
    CurX,
    CurY : integer;
    fx,
    RH   : integer;
    BreakItem,
    Item : TElHTMLItem;
begin
  result := nil;
  if FData = nil then exit;
  j := FData.FArray.Count - 1;
  CurX := 0;
  CurY := 0;
  for i := 0 to j do
  begin
    Item := TElHTMLItem(FData.FArray[i]);
    if (Item is TElHTMLBreakItem) then
    begin
      BreakItem := Item;
      if CurY + Item.Height > Point.Y + SrcPoint.Y then
      begin
        {$ifdef MSWINDOWS}
        case LoWord(TElHTMLBreakItem(Item).FParams) of
          DT_LEFT:
            CurX := BreakItem.Indent;
          DT_RIGHT:
            CurX := Max(R.Right - R.Left, BreakItem.Width) - Item.Width - 1;
          DT_CENTER:
            CurX := (Max(R.Right - R.Left, BreakItem.Width) - Item.Width - 1) div 2;
        {$else}
        case TElHTMLBreakItem(Item).FParams and $0000FFFF of
          Integer(AlignmentFlags_AlignLeft):
            CurX := 0;
          Integer(AlignmentFlags_AlignRight):
            CurX := Max(R.Right - R.Left, BreakItem.Width) - Item.Width - 1;
          Integer(AlignmentFlags_AlignHCenter):
            CurX := (Max(R.Right - R.Left, BreakItem.Width) - Item.Width - 1) div 2;
        {$endif}
        end;
        k := i + 1;
        if k <= j then
        begin
          Item := TElHTMLItem(FData.FArray[k]);
          while (not (Item is TElHTMLBreakItem)) and (k <= j) do
          begin
            if Item.IsSub or Item.IsSuper then
              RH := Item.GetHeight(fx)
            else
              rh := Item.Height;
            fx := 0;
            if Item.IsSuper then
            begin
              fx := RH div 2;
            end;
            if (CurX + Item.Width >= Point.X + SrcPoint.X) and (CurX <= Point.X + SrcPoint.X) and
               (Point.Y + SrcPoint.Y >= CurY + BreakItem.Height - RH - fx) and
               (Point.Y + SrcPoint.Y <= CurY + BreakItem.Height - fx) then
            begin
              result := Item;
              exit;
            end;
            Inc(CurX, Item.Width);
            inc(k);
            if k <= j then
               Item := TElHTMLItem(FData.FArray[k]);
          end;
        end;
        exit;
      end;
      inc(CurY, Item.Height);
    end;
  end;
end;  { FindItemAt }

procedure TElHTMLRender.SelectPrevLink;  { public }
var Item : TElHTMLItem;
    i    : integer;
begin
  if FData = nil then exit;
  i := FData.FArray.IndexOf(FData.FSelectedItem) - 1;
  while i >= 0 do
  begin
    Item := TElHTMLItem(FData.FArray[i]);
    if Item.IsLink then
    begin
      FData.FSelectedItem := Item;
      break;
    end;
    dec(i);
  end;
end;  { SelectPrevLink }

procedure TElHTMLRender.SelectNextLink;  { public }
var Item : TElHTMLItem;
    i    : integer;
begin
  if FData = nil then exit;
  for i := Succ(FData.FArray.IndexOf(FData.FSelectedItem)) to Pred(FData.FArray.Count) do
  begin
    Item := TElHTMLItem(FData.FArray[i]);
    if Item.IsLink then
    begin
      FData.FSelectedItem := Item;
      break;
    end;
  end;
end;  { SelectNextLink }

constructor TElHTMLRender.Create;  { public }
{$ifndef CLX_USED}
var ADC : HDC;
{$endif}
begin
  inherited;
  FData := TElHTMLData.Create;
  FData.FRender := Self;
  FIntData := FData;

  Bitmap := TBitmap.Create;
  {$ifndef CLX_USED}
  Canvas := TCanvas.Create;
  {$else}
  Bitmap.Width := 1;
  Bitmap.Height := 1;
  Canvas := Bitmap.Canvas;
  {$endif}
  //Canvas.Handle := QPainter_Create(Bitmap.Handle);

  {$ifndef CLX_USED}
  ADC := GetDC(Application.Handle);
  Canvas.Handle := CreateCompatibleDC(ADC);
  ReleaseDC(Application.Handle, ADC);
  FSaveObj := SelectObject(Canvas.Handle, Bitmap.Handle);
  {$endif}
end;  { Create }

destructor TElHTMLRender.Destroy;  { public }
{$ifndef CLX_USED}
var ADC : HDC;
{$endif}
begin
  {$ifndef CLX_USED}
  ADC := Canvas.Handle;
  SelectObject(ADC, FSaveObj);
  Canvas.Free;
  DeleteDC(ADC);
  {$endif}
  Bitmap.Free;
  FIntData.Free;
  inherited;
end;  { Destroy }

{$ifndef CLX_USED}
var
  DC: HDC;

initialization
  DC := GetDC(0);
  ScreenPixelsPerInch := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0,DC);

{$else}
var
  Metrics: QPaintDeviceMetricsH;

initialization
  Metrics := QPaintDeviceMetrics_create(QWidget_to_QPaintDevice(QApplication_desktop));
  ScreenPixelsPerInch := QPaintDeviceMetrics_logicalDpiY(Metrics);
  QPaintDeviceMetrics_destroy(Metrics);
{$endif}

end.




