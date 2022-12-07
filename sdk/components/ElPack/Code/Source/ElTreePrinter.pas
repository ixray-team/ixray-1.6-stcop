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

Version history

07/12/2002

  Fixed an AV that could happen when the item has no style assigned and FillBackground is true

11/05/2001

  Added Notification method to prevent AVs on tree deletion

10/22/2001

  Fixed vertical position of item checkboxes

*)

unit ElTreePrinter;

interface

uses ElTree,
     ElPrinter,
     ElHeader,
     ElStack,
{$ifdef HAS_HTML_RENDER}
     HTMLRender,
{$endif}
     ElStrUtils,
     ElVCLUtils,
     ElTools,
{$ifdef VCL_4_USED}
     ImgList,
{$endif}
     Windows,
     Graphics,
     StdCtrls,
     Controls,
{$ifdef VCL_6_USED}
Types,
{$endif}

     SysUtils,
     Classes;

type

    TPrintTreeItemEvent = procedure(Sender : TObject; Item : TElTreeItem; var Print : boolean) of object;
    TPrintHeaderSectionEvent = procedure(Sender : TObject; Section : TElHEaderSection; var Print : boolean) of object;

  EElTreePrinterError = class(Exception);

  TDrawPageNumberEvent = procedure(Sender : TObject; var Text: TElFString; PageNumber : integer) of object;
  TDrawPageCaptionEvent = procedure(Sender : TObject; var Text: TElFString; PageNumber : integer; var Rec: TRect) of object;

  TPNLayout = (plTop, plBottom);

  TElTreePrinter = class(TElControlPrinter)
  private
      FOnAfterPage: TPageEvent;
      FOnBeforePage: TPageEvent;
  protected
      FShowPageNumbers: Boolean;
      FPageNambersLayout: TPNLayout;
      FPageNumbersText: TElFString;
      FPageNumbersAlignment: TAlignment;
      FCaption: TElFString;
      FOnDrawPageNumber: TDrawPageNumberEvent;
      FOnDrawCaption: TDrawPageCaptionEvent;

      FPrinting : boolean;
{$ifdef HAS_HTML_RENDER}
      FRender : TElHTMLRender;
{$endif}
      FBkColor: TColor;
      FShowButtons: Boolean;
      FShowCheckboxes: Boolean;
      FShowColumns: Boolean;
      FShowEmptyImages: Boolean;
      FShowEmptyImages2: Boolean;
      FShowHeader: Boolean;
      FshowHeaderExpandMarks: Boolean;
      FShowHeaderImages: Boolean;
      FShowHeaderSortMarks: Boolean;
      FShowHiddenItems: Boolean;
      FShowHiddenSections: Boolean;
      FShowImages: Boolean;
      FShowInvisibleItems: Boolean;
      FShowInvisibleSections: Boolean;
      FShowLines: Boolean;
      FShowRoot: Boolean;
      FShowRootButtons: Boolean;
      FFillBackground: Boolean;
      FFont: TFont;
      FHeaderOnEveryPage: Boolean;
      FOnItemPrinting: TPrintTreeItemEvent;
      FOnSectionPrinting : TPrintHeaderSectionEvent;
      FScale: Integer;
      FStripedEvenColor: TColor;
      FStripedItems: Boolean;
      FStripedOddColor: TColor;
      FTextColor: TColor;
      FTree: TCustomElTree;
      VirtStyle : TElCellStyle;
      FShowLeafButton: Boolean;
      FVerticalLines: Boolean;
      FHorizontalLines: Boolean;
      FHorzDivLinesColor: TColor;
      FVertDivLinesColor: TColor;

      procedure DoDrawHeader(Canvas : TCanvas; ARect : TRect);
      procedure DoDrawHeaderSection(Canvas : TCanvas; Section : TElHeaderSection;
          ARect : TRect);
      procedure DoDrawItem(Canvas : TCanvas; ItemIndex : integer; Item : TElTreeItem;
          ARect : TRect);
      procedure DoDrawItemCellContents(Canvas : TCanvas; Item : TElTreeItem; Section
          : TElHeaderSection; ARect : TRect; TextColor, TextBkColor, ItemBkColor :
          TColor; FontStyle : TFontStyles);
      procedure DoDrawItemTree(Canvas : TCanvas; Item : TElTreeItem; Section :
          TElHeaderSection; var ARect : TRect);
      procedure SetBkColor(Value: TColor);
      procedure SetShowButtons(Value: Boolean);
      procedure SetShowCheckboxes(newValue: Boolean);
      procedure SetShowColumns(Value: Boolean);
      procedure SetShowEmptyImages(newValue : boolean);
      procedure SetShowEmptyImages2(newValue : boolean);
      procedure SetShowHeader(Value: Boolean);
      procedure SetshowHeaderExpandMarks(Value: Boolean);
      procedure SetShowHeaderImages(Value: Boolean);
      procedure SetShowHeaderSortMarks(Value: Boolean);
      procedure SetShowHiddenItems(Value: Boolean);
      procedure SetShowHiddenSections(Value: Boolean);
      procedure SetShowImages(Value: Boolean);
      procedure SetShowInvisibleItems(Value: Boolean);
      procedure SetShowInvisibleSections(Value: Boolean);
      procedure SetShowLines(Value: Boolean);
      procedure SetShowRoot(Value: Boolean);
      procedure SetShowRootButtons(newValue: Boolean);
      procedure SetFillBackground(Value: Boolean);
      procedure SetHeaderOnEveryPage(Value: Boolean);
      procedure SetScale(Value: Integer);
      procedure SetTree(Value: TCustomElTree);
      procedure TriggerItemPrintingEvent(Item : TElTreeItem; var Print : boolean);
          virtual;
      procedure TriggerSectionPrintingEvent(Section : TElHeaderSection; var Print : boolean);
          virtual;
      procedure DrawButtons(ACanvas : TCanvas; Item : TElTreeItem; IsNode : boolean;
          var R : TRect);
      procedure DrawCheckBoxes(ACanvas : TCanvas; Item : TElTreeItem; var R : TRect);
      procedure DrawImages(ACanvas : TCanvas; Item : TElTreeItem; var R : TRect);
      procedure DrawItemLines(ACanvas : TCanvas; Item : TElTreeItem; var R : TRect);
      procedure SetVerticalLines(Value: Boolean);
      procedure SetHorizontalLines(Value: Boolean);
      procedure SetShowLeafButton(Value: Boolean);
      procedure SetHorzDivLinesColor(Value: TColor);
      procedure SetVertDivLinesColor(Value: TColor);
      procedure TriggerAfterPage(PageNumber : integer); virtual;
      procedure TriggerBeforePage(PageNumber : integer); virtual;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      procedure DoDrawPageNumber(PageNumber : integer); virtual;
      procedure DoDrawCaption(PageNumber : integer; var Rec: TRect); virtual;
  public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure Print;
  published
      property BkColor: TColor read FBkColor write SetBkColor;
      property ShowButtons: Boolean read FShowButtons write SetShowButtons default
          false;
      property ShowCheckboxes: Boolean read FShowCheckboxes write SetShowCheckboxes
          default false;
      property ShowColumns: Boolean read FShowColumns write SetShowColumns;
      property ShowEmptyImages: Boolean read FShowEmptyImages write
          SetShowEmptyImages default false;
      property ShowEmptyImages2: Boolean read FShowEmptyImages2 write
          SetShowEmptyImages2 default false;
      property ShowHeader: Boolean read FShowHeader write SetShowHeader;
      property showHeaderExpandMarks: Boolean read FshowHeaderExpandMarks write
          SetshowHeaderExpandMarks;
      property ShowHeaderImages: Boolean read FShowHeaderImages write
          SetShowHeaderImages;
      property ShowHeaderSortMarks: Boolean read FShowHeaderSortMarks write
          SetShowHeaderSortMarks;
      property ShowHiddenItems: Boolean read FShowHiddenItems write
          SetShowHiddenItems;
      property ShowHiddenSections: Boolean read FShowHiddenSections write
          SetShowHiddenSections;
      property ShowImages: Boolean read FShowImages write SetShowImages default true;
      property ShowInvisibleItems: Boolean read FShowInvisibleItems write
          SetShowInvisibleItems default true;
      property ShowInvisibleSections: Boolean read FShowInvisibleSections write
          SetShowInvisibleSections;
      property ShowLines: Boolean read FShowLines write SetShowLines default true;
      property ShowRoot: Boolean read FShowRoot write SetShowRoot default false;
      property ShowRootButtons: Boolean read FShowRootButtons write
          SetShowRootButtons default false;
      property FillBackground: Boolean read FFillBackground write SetFillBackground;
      property Font: TFont read FFont write FFont;
      property HeaderOnEveryPage: Boolean read FHeaderOnEveryPage write
          SetHeaderOnEveryPage;
      property OnItemPrinting: TPrintTreeItemEvent read FOnItemPrinting write
          FOnItemPrinting;
      property OnSectionPrinting : TPrintHeaderSectionEvent read FOnSectionPrinting
                                                            write FOnSectionPrinting;
      property Scale: Integer read FScale write SetScale default 100;
      property StripedEvenColor: TColor read FStripedEvenColor write
          FStripedEvenColor;
      property StripedItems: Boolean read FStripedItems write FStripedItems default
          false;
      property StripedOddColor: TColor read FStripedOddColor write FStripedOddColor;
      property TextColor: TColor read FTextColor write FTextColor;
      property Tree: TCustomElTree read FTree write SetTree;
      property ShowLeafButton: Boolean read FShowLeafButton write SetShowLeafButton
          default false;
      property VerticalLines: Boolean read FVerticalLines write SetVerticalLines
          default false;
      property HorizontalLines: Boolean read FHorizontalLines write
          SetHorizontalLines;
      property HorzDivLinesColor: TColor read FHorzDivLinesColor write
          SetHorzDivLinesColor;
      property VertDivLinesColor: TColor read FVertDivLinesColor write
          SetVertDivLinesColor;
      property OnAfterPage: TPageEvent read FOnAfterPage write FOnAfterPage;
      property OnBeforePage: TPageEvent read FOnBeforePage write FOnBeforePage;

      property ShowPageNumbers: Boolean read FShowPageNumbers write FShowPageNumbers;
      property PageNambersLayout: TPNLayout read FPageNambersLayout write FPageNambersLayout default plTop;
      property PageNumbersText: TElFString read FPageNumbersText write FPageNumbersText;
      property PageNumbersAlignment: TAlignment read FPageNumbersAlignment write FPageNumbersAlignment default taCenter;
      property Caption: TElFString read FCaption write FCaption;
      property OnDrawPageNumber: TDrawPageNumberEvent read FOnDrawPageNumber write FOnDrawPageNumber;
      property OnDrawCaption: TDrawPageCaptionEvent read FOnDrawCaption write FOnDrawCaption;
  end;

implementation

type

    THackElTree = class(TCustomElTree);
    THackElHeader= class(TElHeader);
    THackElHeaderSection = class(TElHeaderSection);
{$ifdef ELTREE_USE_STYLES}
    THackElCellControl = class(TElCellControl);
{$endif}
    THackElTreeItem = class(TElTreeItem);

procedure TElTreePrinter.DoDrawHeader(Canvas : TCanvas; ARect : TRect);
var i : integer;
    SR : TRect;
    Paint : boolean;
    Section : TElHeaderSection;
begin
  SR := ARect;

  for i := 0 to THackElTree(Tree).HeaderSections.Count - 1 do
  begin
    Section := THackElTree(Tree).HeaderSections.ItemByPos[i];
    if ((not ShowHiddenSections) and (not THackElHeaderSection(Section).FVisible)) or
    ((not ShowInvisibleSections) and (not Section.Visible) and (THackElHeaderSection(Section).FVisible)) then
      Continue;
    TriggerSectionPrintingEvent(Section, Paint);
    if Paint then
    begin
      SR.Right := SR.Left + Section.Width;

      if FillBackground then
      begin
        if (not Section.ParentColor)  then
          Canvas.Brush.Color := Section.Color
        else
          Canvas.Brush.Color := THackElTree(Tree).HeaderColor;

        Canvas.FillRect(SR);
      end;

      DoDrawHeaderSection(Canvas, Section, SR);
      SR.Left := SR.Right;
    end;
  end;
end;

procedure TElTreePrinter.DoDrawHeaderSection(Canvas : TCanvas; Section :
    TElHeaderSection; ARect : TRect);
var
  TS          : TElHeaderSection;
  R1,
  R2          : TRect;
  w           : integer;
  {$ifdef MSWINDOWS}
  DC          : THandle;
  {$endif}
  s           : TElFString;
  SaveCol,
    SaveColor : TColor;
  StImIndex   : integer;
  BMP         : TBitmap;
  AL          : integer;
  C           : TColor;
  AHeader     : THackElHeader;
begin
  TS := Section;

  AHeader := THackElHeader(THackElTree(Tree).HeaderSections.Owner);

  if TS.ParentColor then
    C := THackElHeader(THackElTree(Tree).HeaderSections.Owner).Color
  else
    C := TS.Color;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := C;
  Canvas.FillRect(ARect);

  DC := Canvas.Handle;
  begin
    if AHeader.Flat then
    begin
      DrawEdge(DC, ARect, BDR_RAISEDINNER, BF_RECT);
    end
    else
    begin
      DrawEdge(DC, ARect, BDR_RAISEDOUTER, BF_BOTTOMRIGHT); { black }
      Dec(ARect.Bottom);
      Dec(ARect.Right);
      DrawEdge(DC, ARect, BDR_RAISEDINNER, BF_TOPLEFT); { btnhilite }
      Inc(ARect.Top);
      Inc(ARect.Left);
      DrawEdge(DC, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT); { btnshadow }
      Dec(ARect.Bottom);
      Dec(ARect.Right);
    end;
  end;

  inc(ARect.Top);

  inc(ARect.Left);
  dec(ARect.Bottom);
  dec(ARect.Right, 4);

  if ((TS.Expandable and ShowHeaderExpandMarks) or TS.LookupEnabled or TS.FilterEnabled) and
     (ARect.Left + 6 + (ResizeWidth - 4) < ARect.Right) then
  begin
    if (TS.Expandable and ShowHeaderExpandMarks) then
    begin
      R2 := Rect(ARect.Right - 5, ARect.Top, ARect.Right, ARect.Top + 7);
      if TS.Expanded then
        DrawArrow(Canvas, eadLeft, R2, clBtnText, true)
      else
        DrawArrow(Canvas, eadRight, R2, clBtnText, true);
    end;
    if TS.LookupEnabled then
    begin
      BMP := ElHeaderPointBmp;
      R2 := Rect(ARect.Right - 5, ARect.Top + 10, ARect.Right, Min(ARect.Top + 14, ARect.Bottom));
      Canvas.BrushCopy(R2, Bmp, Rect(0, 0, R2.Right - R2.Left, R2.Bottom - R2.Top), BMP.TransparentColor)
    end
    else
    if TS.FilterEnabled then
    begin
      SaveColor := Canvas.Brush.Color;
      SaveCol := Canvas.Pen.Color;
      if TS.FilterIsActive then
        Canvas.Brush.Color := AHeader.ActiveFilterColor
      else
        Canvas.Brush.Color := AHeader.FilterColor;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Polygon([Point(ARect.Right - 4, ARect.Top + 10), Point(ARect.Right, ARect.Top + 10), Point(ARect.Right - 2, Min(ARect.Top + 14, ARect.Bottom))]);
      Canvas.Brush.Color := SaveColor;
      Canvas.Pen.Color := SaveCol;
    end;
    Dec(ARect.Right, 8);
  end;
  R1 := ARect;
  if (TS.SortMode <> hsmNone) and (TS.ShowSortMark) then
  begin
    if R1.Right - 8 < R1.Left + ResizeWidth then
    begin
      w := 0;
      R1.Right := R1.Left + ResizeWidth;
    end
    else
    begin
      w := 9;
      dec(R1.Right, 9);
    end;
    R2 := Rect(R1.Right, ((R1.Bottom - ARect.Top) div 2 + ARect.Top) - 3, R1.Right + W, ((R1.Bottom - ARect.Top) div 2 + ARect.Top) + 3);
    Canvas.Brush.Color := C;

    if w > 0 then
    begin
      if (TS.SortMode = hsmAscend) xor (AHeader.InvertSortArrows) then
        BMP := ElHeaderAscBmp
      else
        BMP := ElHeaderDescBmp;

      Canvas.BrushCopy(R2, Bmp, Rect(0, 0, 9, 6), Bmp.Canvas.Pixels[0, 5]);
      dec(R1.Right, 3);
    end;
  end;
  {if TS.FResizable then }
  inc(R1.Left, ResizeWidth);
  if R1.Right < R1.Left then exit;
  if TS.Style = ElhsOwnerDraw then
  begin
    AHeader.DoSectionDraw(Canvas, TS, R1, false);
  end
  else
  begin
    if TS.Style = elhsPictureOnly then
    begin
      if (AHeader.Images <> nil) then
      begin
        StImIndex := TS.ImageIndex;
        if StImIndex = -1 then
          StImIndex := AHeader.DoGetPicture(TS);
        if InRange(0, AHeader.Images.Count - 1, StImIndex) then
        begin
          R2 := Rect(Max(R1.Left + ((R1.Right - R1.Left + 1) div 2) - (AHeader.Images.Width div 2), R1.Left),
              R1.Top + ((R1.Bottom - R1.Top + 1) div 2) - (AHeader.Images.Height div 2),
              Min(R1.Right, R1.Left + ((R1.Right - R1.Left + 1) div 2) - (AHeader.Images.Width div 2) + AHeader.Images.Width),
              R1.Top + AHeader.Images.Height);

          if ARect.Left < ARect.Right then
          begin
            AHeader.Images.Draw(Canvas, R2.Left, R2.Top, StImIndex);
            // Canvas.CopyRect(R2, BMP.Canvas, Rect(0, 0, min(FImages.Width, Min(R1.Right, R1.Left + FImages.Width - 1) - R1.Left + 1), FImages.Height));
          end;
          inc(R1.Left, AHeader.Images.Width);
        end;
      end;
      exit;
    end;
    if (AHeader.Images <> nil) and (TS.PictureAlign = hsaLeft) then
    begin
      StImIndex := TS.ImageIndex;
      if StImIndex = -1 then
        StImIndex := AHeader.DoGetPicture(TS);
      if InRange(0, AHeader.Images.Count - 1, StImIndex) then
      begin
        R2 := Rect(R1.Left, (R1.Bottom + R1.Top + 1) div 2 - (AHeader.Images.Height div 2), Min(R1.Right, R1.Left + AHeader.Images.Width), 0);
        R2.Bottom := R2.Top + AHeader.Images.Height;

        if ARect.Left < ARect.Right then
          AHeader.Images.Draw(Canvas, R2.Left, R2.Top, StImIndex);

        inc(R1.Left, AHeader.Images.Width);
      end;
    end;
    if R1.Right < R1.Left then exit;
    if TS.Style = elhsText then
    begin
      S := TS.Text;
{$ifdef HAS_HTML_RENDER}
      if Pos('<html>', s) = 1 then
      begin
        FRender.Data.DefaultBgColor := C;
        FRender.Data.DefaultColor := Font.Color;
        FRender.Data.DefaultStyle := Font.Style;
        FRender.Data.DefaultHeight := Font.Height;
        FRender.Data.DefaultFont := Font.Name;
        FRender.Data.Charset := Font.Charset;
        FRender.PrepareText(S, 0, false);
        FRender.DrawText(Canvas, Point(0, 0), R1, clNone);
      end
      else
{$endif}
      begin
        if AHeader.RightAlignedText then
        begin
          Al := DT_RIGHT;
          case TS.Alignment of
            hsaCenter : Al := DT_CENTER;
            hsaRight : Al := DT_LEFT;
          end; // case
        end
        else
        begin
          Al := DT_LEFT;
          case TS.Alignment of
            hsaCenter : Al := DT_CENTER;
            hsaRight : Al := DT_RIGHT;
          end; // case
        end;
        case TS.TextLayout of
          tlTop : Al := AL or DT_TOP;
          tlCenter : Al := Al or DT_VCENTER;
          tlBottom : Al := Al or DT_BOTTOM;
        end;
        if AHeader.RightAlignedText then
           AL := AL or DT_RTLREADING;
  {$IFNDEF LITE}
        if AHeader.WrapCaptions then
        begin
          AL := AL or DT_WORDBREAK;
          if TS.TextLayout = tlCenter then
             AL := AL or DT_TOP;
        end
        else
        if (Pos(#13#10, TS.Text) = 0) then
  {$ENDIF}
           AL := AL or DT_SINGLELINE or DT_END_ELLIPSIS;

        Canvas.Brush.Style := bsClear;
        if TS.ParentColor then
          Canvas.Font.Color := Font.Color
        else
          Canvas.Font.Color := TS.FontColor;

        {$ifdef ELPACK_UNICODE}
        ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(S), -1, R1,
          DT_NOPREFIX or AL);
        {$else}
        DrawText(Canvas.Handle, PChar(S), -1, R1,
          DT_NOPREFIX or AL);
        {$endif}
      end;
      Canvas.Brush.Style := bsSolid;
    end;
    inc(R1.Left, Canvas.TextWidth(S) + 3);
    if R1.Right < R1.Left then exit;
  end;
  if (AHeader.Images <> nil) and (TS.PictureAlign = hsaRight) then
  begin
    StImIndex := TS.ImageIndex;
    if StImIndex = -1 then
      StImIndex := AHeader.DoGetPicture(TS);
    if InRange(0, AHeader.Images.Count - 1, StImIndex) then
    begin
      R2 := Rect(R1.Left, (R1.Bottom + R1.Top + 1) div 2 - (AHeader.Images.Height div 2), Min(R1.Right, R1.Left + AHeader.Images.Width), 0);
      R2.Bottom := R2.Top + AHeader.Images.Height;

      if ARect.Left < ARect.Right then
        AHeader.Images.Draw(Canvas, R2.Left, R2.Top, StImIndex);

      inc(R1.Left, AHeader.Images.Width);
    end;
  end;
end;

procedure TElTreePrinter.DoDrawItem(Canvas : TCanvas; ItemIndex : integer; Item
    : TElTreeItem; ARect : TRect);
var i, j    : integer;
    SR, R   : TRect;
    Paint   : boolean;
    Section : TElHeaderSection;
{$ifdef ELTREE_USE_STYLES}
    SectionIndex : integer;
{$endif}
    SText   : TElFString;
{$ifdef ELTREE_USE_STYLES}
    CurStyle  : TElCellStyle;
{$endif}
    FontColor,
    BkColor,
    TextBkColor : TColor;
    FontStyle   : TFontStyles;
begin
  SR := ARect;
  if StripedItems then
  begin
    if ItemIndex mod 2 = 0 then
      BkColor := StripedEvenColor
    else
      BkColor := StripedOddColor;

    FontColor := Self.TextColor;
    TextBkColor := BkColor;
  end
  else
  begin
    BkColor := Self.BkColor;
    FontColor := Self.TextColor;
    TextBkColor := Self.BkColor;
  end;
  if not Item.ParentColors then
  begin
    FontColor := Item.Color;
    if Item.UseBkColor then
    begin
      BkColor := Item.RowBkColor;
      TextBkColor := Item.BkColor;
    end;
  end;
  if not Item.ParentStyle then
  begin
    FontStyle := [];
    if Item.Bold then
      Include(FontStyle, fsBold);
    if Item.Italic then
      Include(FontStyle, fsItalic);
    if Item.Underlined then
      Include(FontStyle, fsUnderline);
    if Item.Strikeout then
      Include(FontStyle, fsStrikeout);
  end
  else
  begin
    FontStyle := THackElTree(Tree).Font.Style;
  end;
  R := ARect;

  if not ShowColumns then
  begin
    Section := nil;
{$ifdef ELTREE_USE_STYLES}
    CurStyle := nil;
    if Item.UseStyles then
    begin
      if THackElTree(Tree).VirtualityLevel = vlNone then
        CurStyle := Item.MainStyle
      else
      begin
        CurStyle := VirtStyle;
        THackElTree(Tree).TriggerVirtualStyleNeeded(Item, -1, CurStyle);
      end;
    end;
{$endif}
    if FillBackground then
    begin
{$ifdef ELTREE_USE_STYLES}
      if (not CurStyle.OwnerProps) and CurStyle.UseBkColor then
        Canvas.Brush.Color := CurStyle.CellBkColor
      else
{$endif}
        Canvas.Brush.Color := BkColor;

      Canvas.FillRect(R);
    end;
    DoDrawItemTree(Canvas, Item, Section, R);
    DoDrawItemCellContents(Canvas, Item, Section, R, FontColor, TextBkColor, BkColor, FontStyle);
  end
  else
  for i := 0 to THackElTree(Tree).HeaderSections.Count - 1 do
  begin
    Section := THackElTree(Tree).HeaderSections.ItemByPos[i];
    {$ifdef ELTREE_USE_STYLES}
    SectionIndex := Section.Index;
    {$endif}
    if ((not ShowHiddenSections) and (not THackElHeaderSection(Section).FVisible)) or
     ((not ShowInvisibleSections) and (not Section.Visible) and (THackElHeaderSection(Section).FVisible)) then
       Continue;
    TriggerSectionPrintingEvent(Section, Paint);
    if Paint then
    begin
{$ifdef ELTREE_USE_STYLES}
      CurStyle := nil;
      if Item.UseStyles then
      begin
        if THackElTree(Tree).VirtualityLevel = vlNone then
        begin
          CurStyle := Item.MainStyle;
          j := SectionIndex;
          if SectionIndex <> THackElTree(Tree).MainTreeColumn then
          begin
            if SectionIndex > THackElTree(Tree).MainTreeColumn then
              dec(j);
            if (Item.StylesCount > j) and (not Section.UseMainStyle) then
              CurStyle := Item.Styles[j];
          end;
        end
        else
        begin
          CurStyle := VirtStyle;
          THackElTree(Tree).TriggerVirtualStyleNeeded(Item, SectionIndex, CurStyle);
        end;
      end;
{$endif}
      R := ARect;
      R.Left := Section.Left;
      R.Right := R.Left + Section.Width;
      if FillBackground then
      begin
{$ifdef ELTREE_USE_STYLES}
        if (CurStyle <> nil) and (not CurStyle.OwnerProps) and CurStyle.UseBkColor then
          Canvas.Brush.Color := CurStyle.CellBkColor
        else
{$endif}
          Canvas.Brush.Color := bkColor;

        Canvas.FillRect(R);
      end;
      if Section.Index = THackElTree(Tree).MainTreeColumn then
      begin
        DoDrawItemTree(Canvas, Item, Section, R);
      end;
      if R.Left < R.Right then
      begin
        if (THackElTree(Tree).OwnerDrawByColumn and (Section.Style = ElhsOwnerDraw)) or ((not THackElTree(Tree).OwnerDrawByColumn) and (SText = THackElTree(Tree).OwnerDrawMask))
{$ifdef ELTREE_USE_STYLES}
          or (Item.UseStyles and (CurStyle.Style = elhsOwnerDraw))
{$endif}
        then
        begin
          THackElTree(Tree).DoItemDraw(Item, Canvas, R, Section.Index);
        end
        else
          DoDrawItemCellContents(Canvas, Item, Section, R, FontColor, TextBkColor, BkColor, FontStyle);
      end;
      if VerticalLines then
      begin
        Canvas.Pen.Color := VertDivLinesColor;
        Canvas.MoveTo(R.Right - 1, SR.Top);
        Canvas.LineTo(R.Right - 1, SR.Bottom);
      end;
    end;
    if HorizontalLines then
    begin
      Canvas.Pen.Color := HorzDivLinesColor;
      Canvas.MoveTo(SR.Left, SR.Bottom);
      Canvas.LineTo(R.Right, SR.Bottom);
    end;
  end;
end;

procedure TElTreePrinter.DoDrawItemCellContents(Canvas : TCanvas; Item : 
    TElTreeItem; Section : TElHeaderSection; ARect : TRect; TextColor, 
    TextBkColor, ItemBkColor : TColor; FontStyle : TFontStyles);
var i, j,
    h, w      : integer;
    R2, R, R3 : TRect;
    SectionIndex : integer;
    SText     : TElFString;
{$ifdef ELTREE_USE_STYLES}
    CurStyle  : TElCellStyle;
{$endif}
    TextFlags : integer;
    FontSize  : integer;
    FontName  : TFontName;
{$ifdef HAS_HTML_RENDER}
    FData     : TElHTMLData;
{$endif}
begin
  if Section = nil then
    SectionIndex := -1
  else
    SectionIndex := Section.Index;

  if (SectionIndex = -1) or (SectionIndex = THackElTree(Tree).MainTreeColumn) then
  begin
    if THackElTree(Tree).RightAlignedTree then
       dec(ARect.Right, THackElTree(Tree).ItemIndent div 3)
    else
       inc(ARect.Left, THackElTree(Tree).ItemIndent div 3);
  end;

{$ifdef ELTREE_USE_STYLES}
  CurStyle := nil;
  if Item.UseStyles then
  begin
    if THackElTree(Tree).VirtualityLevel = vlNone then
    begin
      CurStyle := Item.MainStyle;
      if Section <> nil then
      begin
        j := SectionIndex;
        if SectionIndex <> THackElTree(Tree).MainTreeColumn then
        begin
          if SectionIndex > THackElTree(Tree).MainTreeColumn then
            dec(j);
          if (Item.StylesCount > j) and (not Section.UseMainStyle) then
            CurStyle := Item.Styles[j];
        end;
      end;
    end
    else
    begin
      CurStyle := VirtStyle;
      THackElTree(Tree).TriggerVirtualStyleNeeded(Item, SectionIndex, CurStyle);
    end;
  end;
  if Item.UseStyles and (CurStyle.Style = ElhsPictureOnly) then
  begin
    if Assigned(CurStyle.Picture) then
    begin
      // draw picture
      h := CurStyle.Picture.Height;
      w := CurStyle.Picture.Width;
      R := ARect;
      if THackElTree(Tree).RightAlignedTree then
      begin
        R2 := Rect(((R.Left + R.Right) div 2)-(w div 2), R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2), Min(R.Right, ((R.Left + R.Right) div 2)-(w div 2) + w), R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2) + h);
        // R2 := Rect(Max(R.Left, R.Right - w), R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2), R.Right, R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2) + h);

        if CurStyle.Picture.Transparent then
          DrawTransparentBitmapEx(Canvas.Handle, CurStyle.Picture, R2.Left, R2.Top, Rect(0, 0, R.Right - min(w, Max(R.Left, R.Right - w + 1) + 1), h), CurStyle.Picture.TransparentColor)
        else
          Canvas.CopyRect(R2, CurStyle.Picture.Canvas, Rect(0, 0, R.Right - min(w, Max(R.Left, R.Right - w + 1) + 1), h));
      end
      else
      begin
        R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2), Min(R.Right, R.Left + w), R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2) + h);

        if CurStyle.Picture.Transparent then
          DrawTransparentBitmapEx(Canvas.Handle, CurStyle.Picture, R2.Left, R2.Top, Rect(0, 0, min(w, Min(R.Right, R.Left + w - 1) - R.Left + 1), h), CurStyle.Picture.TransparentColor)
        else
          Canvas.CopyRect(R2, CurStyle.Picture.Canvas, Rect(0, 0, min(w, Min(R.Right, R.Left + w - 1) - R.Left + 1), h));
      end;
    end;
  end
  else
{$endif}
  begin
    // draw text
{$ifdef ELTREE_USE_STYLES}
    if Item.UseStyles and (CurStyle.Control <> nil) and
      (SectionIndex <> -1) and (SectionIndex <> THackElTree(Tree).MainTreeColumn) then
    begin
      CurStyle.Control.Paint(Canvas, R);
    end
    else
    if (not Item.UseStyles) or
       ((Item.UseStyles and (CurStyle.Style = elhsText))) then
{$endif}
    begin
      if THackElTree(Tree).RightAlignedTree then
        TextFlags := DT_RIGHT
      else
        TextFlags := DT_LEFT;

      TextFlags := TextFlags or DT_NOPREFIX or MultiLineFlags[Item.Multiline] or
                   MultiLineEllipseFlags[Item.Multiline];

      if THackElTree(Tree).RightAlignedText then
        TextFlags := TextFlags or DT_RTLREADING;

	  if THackElTree(Tree).HeaderSections[SectionIndex].Alignment = hsaRight then
        TextFlags := TextFlags or DT_RIGHT
	  else 
	  if THackElTree(Tree).HeaderSections[SectionIndex].Alignment = hsaCenter then
        TextFlags := TextFlags or DT_CENTER;

      FontName  := Font.Name;
      FontSize  := Font.Size;
{$ifdef ELTREE_USE_STYLES}
      if Item.UseStyles then
      begin
        if not CurStyle.OwnerProps then
        begin
          TextColor := CurStyle.TextColor;
          TextBkColor := CurStyle.TextBkColor;
          // ItemBkColor := CurStyle.TextBkColor;
          FontStyle := CurStyle.FontStyles;
          TextFlags := CurStyle.TextFlags;
          FontName  := CurStyle.FontName;
          FontSize  := CurStyle.FontSize;
        end;
      end;
{$endif}
      i := SectionIndex;
      if THackElTree(Tree).VirtualityLevel = vlNone then
      begin
        if i = -1 then
          SText := Item.Text
        else
        begin
          if i = THackElTree(Tree).MainTreeColumn then
          begin
            if Section.Password then
              SText := '******'
            else
              SText := Item.Text;
          end
          else
          begin
            if i > THackElTree(Tree).MainTreeColumn then
              dec(i);
            if i >= Item.ColumnText.Count then
              SText := ''
            else
            if Section.Password then
              SText := '******'
            else
              SText := Item.ColumnText[i];
          end;
        end;
      end
      else
      begin
        if Section.Password then
          SText := '******'
        else
          THackElTree(Tree).TriggerVirtualTextNeeded(Item, i, SText);
      end;
      Canvas.Font.Size  := FontSize;
      Canvas.Font.Name  := FontName;
      Canvas.Font.Style := FontStyle;
      Canvas.Font.Color := TextColor;

      if FillBackground then
      begin
        Canvas.Brush.Style := bsSolid;
        if ItemBkColor <> clNone then
          Canvas.FillRect(ARect);
        Canvas.Brush.Color := TextBkColor;
      end
      else
        Canvas.Brush.Style := bsClear;

{$IFDEF HAS_HTML_RENDER}
      if Item.IsHTML and (Copy(SText, 1, 6) = '<html>') then
      begin
        R3.Left := 0;
        R3.Top := 0;
        if THackElTree(Tree).VirtualityLevel = vlNone then
        begin
          FData := THackElTreeItem(Item).FHTMLData;
          FRender.SetData(FData);
        end
        else
        begin
          with FRender do
          begin
            Data.DefaultStyle := Canvas.Font.Style;
            Data.DefaultFont  := Canvas.Font.Name;
            Data.DefaultColor := Canvas.Font.Color;
            Data.DefaultHeight:= Canvas.Font.Height;
            Data.Charset      := Canvas.Font.Charset;
            Data.DefaultBgColor := TextBkColor;

            PrepareText(SText, 0, false);
          end;
        end;
        FRender.DrawText(Canvas, Point(0, 0), ARect, clNone);
        if THackElTree(Tree).VirtualityLevel = vlNone then
          FRender.SetData(nil);
      end
      else
{$ENDIF}
      begin
        if Item.Multiline and (Pos(#13#10, SText) > 0) then
          TextFlags := (TextFlags and (not (DT_VCENTER or DT_BOTTOM))) or DT_TOP
        else
          TextFlags := (TextFlags and (not (DT_TOP or DT_BOTTOM))) or DT_VCENTER; 
      {$ifdef ELPACK_UNICODE}
        ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(SText), Length(SText), ARect, TextFlags);
      {$else}
        DrawText(Canvas.Handle, PChar(SText), Length(SText), ARect, TextFlags);
      {$endif}
      end;
    end;
  end;
end;

procedure TElTreePrinter.DoDrawItemTree(Canvas : TCanvas; Item : TElTreeItem;
    Section : TElHeaderSection; var ARect : TRect);
var ANode : boolean;
begin
  if Item.IndentAdjust <> 0 then
  begin
    if THackElTree(Tree).RightAlignedTree then
      dec(ARect.Right, Item.IndentAdjust)
    else
      inc(ARect.Left, Item.IndentAdjust);
  end;
  if THackElTree(Tree).RightAlignedTree then
  begin
    if (ShowRoot and ShowLines) or (ShowButtons and ShowRootButtons) then
      dec(ARect.Right, THackElTree(Tree).ItemIndent)
    else
      dec(ARect.Right, THackElTree(Tree).ItemIndent div 5);
  end
  else
  begin
    if (ShowRoot and ShowLines) or (ShowButtons and ShowRootButtons) then
      Inc(ARect.Left, THackElTree(Tree).ItemIndent)
    else
      Inc(ARect.Left, THackElTree(Tree).ItemIndent div 5);
  end;
  if (FShowLines) and (not Item.SuppressLines) then
    DrawItemLines(Canvas, Item, ARect)
  else
  begin
    if THackElTree(Tree).RightAlignedTree then
      dec(ARect.Right, (Item.Level - 1) * THackElTree(Tree).ItemIndent)
    else
      inc(ARect.Left, (Item.Level - 1) * THackElTree(Tree).ItemIndent);
  end;

  if Item.Ancestor.SuppressButtons then
  begin //Eyal
    if THackElTree(Tree).RightAlignedTree then
    begin
      inc(ARect.Right, THackElTree(Tree).ItemIndent);
    end else
    begin
      dec(ARect.Left, THackElTree(Tree).ItemIndent);
    end;
  end;


  ANode := ((Item.HasVisibleChildren) or Item.ForceButtons);
  if ShowButtons  and (not Item.SuppressButtons) and
     (ShowRootButtons or (Item.Parent <> nil)) and
     (ANode or ShowLeafButton) then
    DrawButtons(Canvas, Item, ANode, ARect);

  if THackElTree(Tree).RightAlignedTree then
    dec(ARect.Right, THackElTree(Tree).ItemIndent)
  else
    inc(ARect.Left, THackElTree(Tree).ItemIndent);

  if ShowCheckBoxes then
    DrawCheckBoxes(Canvas, Item, ARect);

  if (ShowImages) then
    DrawImages(Canvas, Item, ARect);
end;

procedure TElTreePrinter.SetBkColor(Value: TColor);
begin
  if FBkColor <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FBkColor := Value;
  end;
end;

procedure TElTreePrinter.SetShowButtons(Value: Boolean);
begin
  if FShowButtons <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowButtons := Value;
  end;
end;

procedure TElTreePrinter.SetShowCheckboxes(newValue: Boolean);
begin
  if (FShowCheckboxes <> newValue) then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowCheckboxes := newValue;
  end; {if}
end; {SetDrawCheckboxes}

procedure TElTreePrinter.SetShowColumns(Value: Boolean);
begin
  if FShowColumns <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowColumns := Value;
  end;
end;

procedure TElTreePrinter.SetShowEmptyImages(newValue : boolean);
begin
  if FShowEmptyImages <> newValue then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowEmptyImages := newValue;
  end;
end;

procedure TElTreePrinter.SetShowEmptyImages2(newValue : boolean);
begin
  if FShowEmptyImages2 <> newValue then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowEmptyImages2 := newValue;
  end;
end;

procedure TElTreePrinter.SetShowHeader(Value: Boolean);
begin
  if FShowHeader <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowHeader := Value;
  end;
end;

procedure TElTreePrinter.SetshowHeaderExpandMarks(Value: Boolean);
begin
  if FShowHeaderExpandMarks <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowHeaderExpandMarks := Value;
  end;
end;

procedure TElTreePrinter.SetShowHeaderImages(Value: Boolean);
begin
  if FShowHeaderImages <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowHeaderImages := Value;
  end;
end;

procedure TElTreePrinter.SetShowHeaderSortMarks(Value: Boolean);
begin
  if FShowHeaderSortMarks <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowHeaderSortMarks := Value;
  end;
end;

procedure TElTreePrinter.SetShowHiddenItems(Value: Boolean);
begin
  if FShowHiddenItems <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowHiddenItems := Value;
  end;
end;

procedure TElTreePrinter.SetShowHiddenSections(Value: Boolean);
begin
  if FShowHiddenSections <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowHiddenSections := Value;
  end;
end;

procedure TElTreePrinter.SetShowImages(Value: Boolean);
begin
  if FShowImages <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowImages := Value;
  end;
end;

procedure TElTreePrinter.SetShowInvisibleItems(Value: Boolean);
begin
  if FShowInvisibleItems <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowInvisibleItems := Value;
  end;
end;

procedure TElTreePrinter.SetShowInvisibleSections(Value: Boolean);
begin
  if FShowInvisibleSections <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowInvisibleSections := Value;
  end;
end;

procedure TElTreePrinter.SetShowLines(Value: Boolean);
begin
  if FShowLines <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowLines := Value;
  end;
end;

procedure TElTreePrinter.SetShowRoot(Value: Boolean);
begin
  if FShowRoot <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowRoot := Value;
  end;
end;

procedure TElTreePrinter.SetShowRootButtons(newValue: Boolean);
begin
  if FShowRootButtons <> newValue then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowRootButtons := newValue;
  end;
end;

procedure TElTreePrinter.SetFillBackground(Value: Boolean);
begin
  if FFillBackground <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FFillBackground := Value;
  end;
end;

procedure TElTreePrinter.SetHeaderOnEveryPage(Value: Boolean);
begin
  if FHeaderOnEveryPage <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FHeaderOnEveryPage := Value;
  end;
end;

procedure TElTreePrinter.SetScale(Value: Integer);
begin
  if FScale <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FScale := Value;
  end;
end;

procedure TElTreePrinter.SetTree(Value: TCustomElTree);
begin
  if FTree <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change tree object while printing');
    FTree := Value;
  end;
end;

procedure TElTreePrinter.TriggerItemPrintingEvent(Item : TElTreeItem; var Print
    : boolean);
begin
  Print := true;
  if assigned(FOnItemPrinting) then
    FOnItemPrinting(Self, Item, Print);
end;

procedure TElTreePrinter.TriggerSectionPrintingEvent(Section : TElHeaderSection; var Print : boolean);
begin
  Print := true;
  if assigned(FOnSectionPrinting) then
    FOnSectionPrinting(Self, Section, Print);
end;

constructor TElTreePrinter.Create(AOwner : TComponent);
begin
  inherited;
  FShowLines  := true;
  FShowImages := true;
  FScale := 100;
  Font := TFont.Create;
{$ifdef ELTREE_USE_STYLE}
  VirtStyle := TElCellStyle.Create(nil);
{$endif}
{$ifdef HAS_HTML_RENDER}
  FRender := TElHTMLRender.Create;
{$endif}
  FPageNambersLayout := plTop;
  FPageNumbersText := 'Page ';
  FPageNumbersAlignment := taCenter;
end;

destructor TElTreePrinter.Destroy;
begin
{$ifdef HAS_HTML_RENDER}
  FRender.Free;
{$endif}
{$ifdef ELTREE_USE_STYLE}
  VirtStyle.Free;
{$endif}
  Font.Free;
  inherited;
end;

procedure TElTreePrinter.Print;
var i : integer;
    pa:  boolean;
    R1: TRect;
    DC: HDC;
    Item : TElTreeItem;
    Print : boolean;
    SaveMapMode : integer;
    SaveLogExtEx,
    SavePhExtEx : TSize;

    PhExtX,
    PhExtY,
    LogExtX,
    LogExtY  : integer;

begin
  i  := 0;

  if Tree = nil then raise EElTreePrinterError.Create('No tree assigned to ElTreePrinter');
  if Printer = nil then exit;
  pa := Printer.Active;
  if not pa then
    Printer.BeginDoc;
  FPrinting := true;

  DC := GetDC(0);
  LogExtX := MulDiv(Printer.PageWidth, GetDeviceCaps(DC, LOGPIXELSX) * 100, Scale * 2540);
  LogExtY := MulDiv(Printer.PageHeight, GetDeviceCaps(DC, LOGPIXELSY) * 100, Scale * 2540);
  ReleaseDC(0, DC);

  PhExtX := MulDiv(Printer.PageWidth, GetDeviceCaps(Printer.PrinterDC, LOGPIXELSX), 2540);
  PhExtY := MulDiv(Printer.PageHeight, GetDeviceCaps(Printer.PrinterDC, LOGPIXELSY), 2540);

  try
    with Printer.Canvas[Printer.PageIndex] do
    begin
      SaveMapMode := GetMapMode(Handle);
      SetMapMode(Handle, MM_ANISOTROPIC);
      SetWindowExtEx(Handle, LogExtX, LogExtY, @SaveLogExtEx);
      SetViewPortExtEx(Handle, PhExtX, PhExtY, @SavePhExtEx);
    end;

    SetRectEmpty(R1);

    R1.Right := LogExtX;

    TriggerBeforePage(Printer.PageIndex);
//    DoDrawCaption(Printer.PageIndex, R1);


    while true do
    begin
      SetRectEmpty(R1);

      R1.Right := LogExtX;

      DoDrawCaption(Printer.PageIndex, R1);

      if ShowColumns and ShowHeader and ((Printer.PageIndex = 0) or (HeaderOnEveryPage)) then
      begin
        R1.Bottom := R1.Bottom + THackElTree(Tree).HeaderSections.Owner.CalcHeaderHeight;
        DoDrawHeader(Printer.Canvas[Printer.PageIndex], R1);
      end
      else
        R1.Bottom := R1.Bottom + R1.Top;
      while true do
      begin
        R1.Top := R1.Bottom;
        if i >= Integer(THackElTree(Tree).Items.Count) then
          break;
        Item := THackElTree(Tree).Items[i];
        if item = nil then
          break;
        if ((not ShowHiddenItems) and Item.Hidden and THackElTree(Tree).FilteredVisibility) or
           ((not ShowInvisibleItems) and not Item.FullyExpanded) then
        begin
          inc(i);
          Continue;
        end;
        TriggerItemPrintingEvent(Item, Print);
        if not Print then
        begin
          Continue;
          inc(i);
        end;
        if LogExtY - R1.Top < Item.Height then
        begin

          TriggerAfterPage(Printer.PageIndex);

          with Printer.Canvas[Printer.PageIndex] do
          begin
            SetWindowExtEx(Handle, SaveLogExtEx.CX, SaveLogExtEx.CY, nil);
            SetViewPortExtEx(Handle, SavePhExtEx.cx, SavePhExtEx.CY, nil);
            SetMapMode(Handle, SaveMapMode);
          end;

          Printer.NewPage;

          with Printer.Canvas[Printer.PageIndex] do
          begin
            SaveMapMode := GetMapMode(Handle);
            SetMapMode(Handle, MM_ANISOTROPIC);
            SetWindowExtEx(Handle, LogExtX, LogExtY, @SaveLogExtEx);
            SetViewPortExtEx(Handle, PhExtX, PhExtY, @SavePhExtEx);
          end;

          TriggerBeforePage(Printer.PageIndex);

          break;
        end
        else
        begin
          R1.Bottom := R1.Top + Item.Height;
          DoDrawItem(Printer.Canvas[Printer.PageIndex], i, Item, R1);
          inc(i);
        end;
      end;
      if i = Integer(THackElTree(Tree).Items.Count) then
        break;
    end;
    TriggerAfterPage(Printer.PageIndex);
    with Printer.Canvas[Printer.PageIndex] do
    begin
      SetWindowExtEx(Handle, SaveLogExtEx.cx, SaveLogExtEx.cy, nil);
      SetViewPortExtEx(Handle, SavePhExtEx.cx, SavePhExtEx.cy, nil);
      SetMapMode(Handle, SaveMapMode);
    end;

  finally
    if not pa then
      Printer.EndDoc;
    FPrinting := false;
    if DC <> 0 then
      ReleaseDC(0, DC);
  end;
end;

procedure TElTreePrinter.DrawButtons(ACanvas : TCanvas; Item : TElTreeItem;
    IsNode : boolean; var R : TRect);
var FCCanvas : TCanvas;
    //FCBitmap : HBitmap;
    // FCBitmap : TBitmap;
    R1 : TRect;
    w,
    h  : integer;
    // TC : TColor;
begin
  if R.Left >= R.Right then exit;
  if THackElTree(Tree).CustomPlusMinus then
  begin
    w := THackElTree(Tree).PlusPicture.Width;
    h := THackElTree(Tree).PlusPicture.Height;

    CenterRects(w, THackElTree(Tree).ItemIndent, h, R.Bottom - R.Top + 1, R1);

    if not IsNode then
    begin
      FCCanvas := THackElTree(Tree).LeafPicture.Canvas;
      //FCBitmap := FPlusPicture.Handle;
      // TC := THackElTree(Tree).LeafPicture.Canvas.Pixels[0, h-1];
      //FCBitmap := FPlusPicture.Handle;
      // FCBitmap := THackElTree(Tree).LeafPicture;
    end
    else
    if not Item.Expanded then
    begin
      FCCanvas := THackElTree(Tree).PlusPicture.Canvas;
      //FCBitmap := FPlusPicture.Handle;
      // TC := THackElTree(Tree).PlusPicture.Canvas.Pixels[0, h-1];
      //FCBitmap := FPlusPicture.Handle;
      // FCBitmap := THackElTree(Tree).PlusPicture;
    end else
    begin
      FCCanvas := THackElTree(Tree).MinusPicture.Canvas;
      //FCBitmap := FMinusPicture.Handle;
      // TC := THackElTree(Tree).MinusPicture.Canvas.Pixels[0, h-1];
      //FCBitmap := FMinusPicture.Handle;
      // FCBitmap := THackElTree(Tree).MinusPicture;
    end;
  end
  else
  begin
    w := 10;
    CenterRects(w, THackElTree(Tree).ItemIndent, w, R.Bottom - R.Top + 1, R1);
    if not IsNode then
    begin
      FCCanvas := LeafBmp.Canvas;
      // TC := LeafBmp.Canvas.Pixels[1, LeafBmp.Height-2];
      //FCBitmap := PlusBmp.Handle;
      //FCBitmap := LeafBmp;
    end
    else
    if not Item.Expanded then
    begin
      FCCanvas := PlusBmp.Canvas;
      // TC := PlusBmp.Canvas.Pixels[1, PlusBmp.Height-2];
      //FCBitmap := PlusBmp.Handle;
      // FCBitmap := PlusBmp;
    end
    else
    begin
      FCCanvas := MinusBmp.Canvas;
      // TC := MinusBmp.Canvas.Pixels[1, MinusBmp.Height-2];
      // FCBitmap := MinusBmp.Handle;
      // FCBitmap := MinusBmp;
    end;
  end;
  if THackElTree(Tree).RightAlignedTree then
  begin
    R1 := Rect(max(R.Right - R1.Right, R.Left), R1.Top + R.Top, R.Right - (R1.Right - w), R1.Bottom + R.Top);
    BitBlt(ACanvas.Handle, R1.Left, R1.Top, R1.Right - R1.Left + 1, R1.Bottom - R1.Top + 1, FCCanvas.Handle, w - (R1.Right - R1.Left), 0, SRCCOPY);
  end else
  begin
    OffsetRect(R1, R.Left, R.Top);
    BitBlt(ACanvas.Handle, R1.Left, R1.Top, min(R1.Right - R1.Left + 1, R.Right - R.Left + 1), R1.Bottom - R1.Top + 1, FCCanvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TElTreePrinter.DrawCheckBoxes(ACanvas : TCanvas; Item : TElTreeItem; 
    var R : TRect);
var
  cbh,
  cbw,
  i    : integer;
  R2,
  R3   : TRect;

begin
  if Item.ShowCheckbox then
  begin
    cbw := 0;
    try
      if THackElTree(Tree).RightAlignedTree then
        dec(R.Right, 2)
      else
        inc(R.Left, 2);

      if THackElTree(Tree).CustomCheckboxes then
      begin
        if Item.CheckBoxType = ectRadioButton then
        begin
          cbh := THackElTree(Tree).RadioButtonGlyph.Height;
          cbw := THackElTree(Tree).RadioButtonGlyph.Width div 6;

          if not Item.Checked then
          begin
            if Item.CheckBoxEnabled then
              R3 := Rect(0, 0, cbw, cbh)
            else
              R3 := Rect(cbw, 0, cbw * 2, cbh);
          end else
          begin
            if Item.CheckBoxEnabled then
              R3 := Rect(cbw * 2, 0, cbw * 3, cbh)
            else
              R3 := Rect(cbw * 3, 0, cbw * 6, cbh);
          end;
        end else
        if Item.CheckBoxType = ectCheckBox then
        begin
          cbh := THackElTree(Tree).CheckBoxGlyph.Height;
          cbw := THackElTree(Tree).CheckBoxGlyph.Width div 6;

          if not Item.Checked then
          begin
            if Item.CheckBoxEnabled then
              R3 := Rect(0, 0, cbw, cbh)
            else
              R3 := Rect(cbw, 0, cbw * 2, cbh);
          end else
          begin
            if Item.CheckBoxEnabled then
              R3 := Rect(cbw * 2, 0, cbw * 3, cbh)
            else
              R3 := Rect(cbw * 3, 0, cbw * 4, cbh);
          end;
        end else
        begin
          cbh := THackElTree(Tree).CheckBoxGlyph.Height;
          cbw := THackElTree(Tree).CheckBoxGlyph.Width div 6;

          case Item.CheckBoxState of
            cbUnchecked:
              if Item.CheckBoxEnabled then
                R3 := Rect(0, 0, cbw, cbh)
              else
                R3 := Rect(cbw, 0, cbw * 2, cbh);
            cbChecked:
              if Item.CheckBoxEnabled then
                R3 := Rect(cbw * 2, 0, cbw * 3, cbh)
              else
                R3 := Rect(cbw * 3, 0, cbw * 4, cbh);
            cbGrayed:
              if Item.CheckBoxEnabled then
                R3 := Rect(cbw * 4, 0, cbw * 5, cbh)
              else
                R3 := Rect(cbw * 5, 0, cbw * 6, cbh);
          end;
        end;
        if THackElTree(Tree).RightAlignedTree then
        begin
          R2 := Rect(R.Right - cbw + 1, R.Top + ((R.Bottom - R.Top + 1) div 2) - cbh div 2, R.Right, R.Top + ((R.Bottom - R.Top + 1) div 2) + cbh div 2);

          if R.Left >= R2.Right then exit;
          if R.Left >= R.Right - cbw then
          begin
            R2.Left := R.Left;
            R3.Left := r3.Right - (R.Right - R.Left + 1);
            // HelperBitmap.Width := R.Right - R2.Left;
          end
          else ;
            // HelperBitmap.Width := cbw;
        end
        else
        begin
          R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) div 2) - cbh div 2, R.Left + cbw, R.Top + ((R.Bottom - R.Top + 1) div 2) + cbh div 2);

          if R2.Left >= R.Right then exit;
          if R2.Left + cbw >= R.Right then
          begin
            R2.Right := R.Right;
            R3.Right := R3.Left + (R.Right - R.Left + 1);
            // HelperBitmap.Width := R.Right - R.Left + 1;
          end
          else ;
            // HelperBitmap.Width := cbw;
        end;
        // HelperBitmap.Height := R2.Bottom - R2.Top + 1;

        if Item.CheckBoxType = ectRadioButton then
        begin
          THackElTree(Tree).RadioButtonGlyph.Pixelformat := pfDevice;
          DrawTransparentBitmapEx(ACanvas.Handle, THackElTree(Tree).RadioButtonGlyph, 0, 0, R3, THackElTree(Tree).RadioButtonGlyph.TransparentColor);
        end
        else
        begin
          THackElTree(Tree).CheckBoxGlyph.Pixelformat := pfDevice;
          DrawTransparentBitmapEx(ACanvas.Handle, THackElTree(Tree).CheckBoxGlyph, 0, 0, R3, THackElTree(Tree).CheckBoxGlyph.TransparentColor);
        end;

        // bitblt(ACanvas.Handle, r2.Left, R2.Top, HelperBitmap.Width, HelperBitmap.Height, HelperBitmap.Canvas.Handle, 0, 0, srccopy);
      end
      else
      begin
        //cbw := ItemExt - 2;
        cbw := THackElTree(Tree).CheckBoxSize;
        cbh := cbw;
        //cbh := ItemExt - 2;
        i := DFCS_BUTTONCHECK or DFCS_CHECKED;
        if Item.CheckBoxType = ectRadioButton then
        begin
          if Item.Checked then i := DFCS_BUTTONRADIO or DFCS_CHECKED
          else i := DFCS_BUTTONRADIO;
        end else
          if Item.CheckBoxType = ectCheckBox then
          begin
            if Item.Checked then i := DFCS_BUTTONCHECK or DFCS_CHECKED
            else i := DFCS_BUTTONCHECK;
          end else
          begin
            case Item.CheckBoxState of //
              cbChecked: i := DFCS_BUTTONCHECK or DFCS_CHECKED;
              cbUnchecked: i := DFCS_BUTTONCHECK;
              cbGrayed: i := DFCS_BUTTON3STATE or DFCS_CHECKED;
            end; // case
          end;
        if not Item.CheckBoxEnabled then
          i := i or DFCS_INACTIVE;

        if THackElTree(Tree).RightAlignedTree then
        begin     
          R2 := Rect(Max(R.Left, R.Right - (cbw - 2)), R.Top + ((R.Bottom - R.Top + 1) div 2) - cbh div 2, R.Right, R.Top + ((R.Bottom - R.Top + 1) div 2) + cbh div 2);

          R3 := Rect(0, 0, cbw -2, cbw -2);
          if R.Left >= R2.Right then exit;
          if R.Left >= R.Right - cbw then
          begin
            R2.Left := R.Left;
            OffsetRect(R3, -(R.Left - (R.Right - cbw)), 0);
            // HelperBitmap.Width := R.Right - R2.Left;
          end
          else
            R2.Left := R2.Right - cbw;
            // HelperBitmap.Width := cbw;
        end else
        begin
          R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) div 2) - cbh div 2, Min(R.Right, R.Left + (THackElTree(Tree).ItemIndent - 2)), R.Top + ((R.Bottom - R.Top + 1) div 2) + cbh div 2);

          R3 := Rect(0, 0, cbw - 2, cbw -2);
          if R2.Left >= R.Right then exit;
          if R2.Left + cbw >= R.Right then
          begin
            R2.Right := R.Right;
            // HelperBitmap.Width := R.Right - R2.Left;
          end
          else
            R2.Right := R2.Left + cbw;
            // HelperBitmap.Width := cbw;
        end;
        // HelperBitmap.Height := cbw - 1;

        R3 := R2;
        R3.Bottom := R3.Top + cbw;

        DrawFrameControl(ACanvas.Handle, R3, DFC_BUTTON, i);
      end;
    finally
      if THackElTree(Tree).RightAlignedTree then
         dec(R.Right, cbw)
      else
         inc(R.Left, cbw);
    end;
  end;
end;

procedure TElTreePrinter.DrawImages(ACanvas : TCanvas; Item : TElTreeItem; var 
    R : TRect);
var
  FTImages : TImageList;
  ImDrawStyle: TDrawingStyle;
  StImIndex: integer;
  h, w     : integer;
  R2       : TRect;
  ImageDrawn2 : boolean;

  procedure DoDrawImage;
  begin
    h := FTImages.Height;
    w := FTImages.Width;

    if THackElTree(Tree).RightAlignedTree then
      R2 := Rect(Max(R.Left, R.Right - w), R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2), R.Right, R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2) + h)
    else
      R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2), Min(R.Right, R.Left + w), R.Top + ((R.Bottom - R.Top + 1) div 2) - (h div 2) + h);

    if R2.Right >= R2.Left then
      FTImages.Draw(ACanvas, R2.Left, R2.Top, StImIndex);

    if THackElTree(Tree).RightAlignedTree then
      dec(R.Right, w)
    else
      inc(R.Left, w);
  end;

begin
  if R.Left >= R.Right then exit;
  ImageDrawn2 := false;

  if THackElTree(Tree).Images2 <> nil then
    FTImages := THackElTree(Tree).Images2
  else
    FTImages := THackElTree(Tree).Images;
  if FTImages <> nil then
  begin
    ImDrawStyle := FTImages.DrawingStyle;
    if THackElTree(Tree).ChangeStateImage and THackElTree(Tree).RowSelect then
    begin
      if Item.Selected or Item.Cut then
        FTImages.DrawingStyle := dsSelected
      else
      if Item.Focused then
        FTImages.DrawingStyle := dsFocus
      else
        FTImages.DrawingStyle := dsNormal;
    end
    else
    if THackElTree(Tree).ChangeStateImage and Item.Cut then
      FTImages.DrawingStyle := dsSelected;

    // draw 2nd image
    if (Item.Focused or Item.Selected or (Item.Expanded {and Item.HasVisibleChildren})) then
      StImIndex := Item.StateImageIndex2
    else
      StImIndex := Item.ImageIndex2;
    if StImIndex = -1 then
       StImIndex := THackElTree(Tree).DoGetPicture2(Item);
    if InRange(0, FTImages.Count - 1, StImIndex) then
    begin
      DoDrawImage;
      ImageDrawn2 := true;
    end else
    begin
      if FShowEmptyImages2 then
      begin
        if THackElTree(Tree).RightAlignedTree then
          dec(R.Right, FTImages.Width)
        else
          inc(R.Left, FTImages.Width);
      end;
    end;
    FTImages.DrawingStyle := ImDrawStyle;
  end;
  if (THackElTree(Tree).Images <> nil) then
  begin
    FTImages := THackElTree(Tree).Images;
    ImDrawStyle := THackElTree(Tree).Images.DrawingStyle;
    if THackElTree(Tree).ChangeStateImage and THackElTree(Tree).RowSelect then
    begin
      if Item.Selected or Item.Cut then
         FTImages.DrawingStyle := dsSelected
      else
         if Item.Focused then
           FTImages.DrawingStyle := dsFocus
         else
           FTImages.DrawingStyle := dsNormal;
    end
    else
      if THackElTree(Tree).ChangeStateImage and Item.Cut then
        FTImages.DrawingStyle := dsSelected;

    if (Item.Focused or Item.Selected or (Item.Expanded {and Item.HasVisibleChildren})) then
      StImIndex := Item.StateImageIndex
    else
      StImIndex := Item.ImageIndex;
    if StImIndex = -1 then
      StImIndex := THackElTree(Tree).DoGetPicture(Item);
    if InRange(0, FTImages.Count - 1, StImIndex) then
    begin
      if THackElTree(Tree).RightAlignedTree then
      begin
        if ImageDrawn2 then
          dec(R.Right, THackElTree(Tree).ItemIndent div 3); // make the space between images
      end else
      begin
        if ImageDrawn2 then
          inc(R.Left, THackElTree(Tree).ItemIndent div 3); // make the space between images
      end;
      DoDrawImage;
    end
    else
    if FShowEmptyImages then
    begin
      if THackElTree(Tree).RightAlignedTree then
      begin
        if ImageDrawn2 then
          dec(R.Right, THackElTree(Tree).ItemIndent div 3); // make the space between images
      end
      else
      begin
        if ImageDrawn2 then
          inc(R.Left, THackElTree(Tree).ItemIndent div 3); // make the space between images
      end;
      if THackElTree(Tree).RightAlignedTree then
        dec(R.Right, FTImages.Width)
      else
        inc(R.Left, FTImages.Width);
    end;
    FTImages.DrawingStyle := ImDrawStyle;
  end;
end;

procedure TElTreePrinter.DrawItemLines(ACanvas : TCanvas; Item : TElTreeItem; 
    var R : TRect);

var
  Stack : TElStack;
  //ItemRoot,
  TSI,
    TSI1: TElTreeItem;
  SavePen    : TPenStyle;
  SavePenCol : TColor;

function GetPrevVisChild(Parent, Item: TElTreeItem; NoRoot : boolean): TElTreeItem;
  begin
    if NoRoot and (Item.Parent = nil) then
    begin
      result := nil;
      exit;
    end;
    Result := Parent.GetPrevChild(Item);
    if THackElTree(Tree).FilteredVisibility then
      while Assigned(Result) and (Result.Hidden) do
        Result := Parent.GetPrevChild(Result);
  end;

  function GetNextVisChild(Parent, Item: TElTreeItem; NoRoot : boolean): TElTreeItem;
  begin
    if NoRoot and (Item.Parent = nil) then
    begin
      Result := nil;
      exit;
    end;
    Result := Parent.GetNextChild(Item);
    if THackElTree(Tree).FilteredVisibility then
      while Assigned(Result) and (Result.Hidden) do
        Result := Parent.GetNextChild(Result);
  end;

  procedure DrawLine(Canvas : TCanvas; StartX, StartY, EndX, EndY : integer);
  var
    Coord: Integer;
    // CRef: COLORREF;
    // DC: HDC;
  begin
    if THackElTree(Tree).LinesStyle = psDot then
    begin
      // CRef := ColorToRGB(Canvas.Pen.Color);
      // skip a pixel if not in grid
      Coord := (StartX and 1) xor (StartY and 1);
      if StartX = EndX then
      begin
        // draw vertical line
        Inc(Coord, StartY);
        // DC := Canvas.Handle;
        while Coord < EndY do
        begin
          Canvas.MoveTo(StartX, Coord);
          Inc(Coord, 1);
          Canvas.LineTo(StartX, Coord);
          Inc(Coord, 1);
          (*
          SetPixel(DC, StartX, Coord, CRef);
          Inc(Coord, 2);
          *)
        end;
      end
      else
      begin
        // draw horizontal line
        Inc(Coord, StartX);
        //DC := Canvas.Handle;
        while Coord < EndX do
        begin
          Canvas.MoveTo(Coord, StartY);
          Inc(Coord, 1);
          Canvas.LineTo(Coord, StartY);
          Inc(Coord, 1);
          (*
          SetPixel(DC, Coord, StartY, CRef);
          Inc(Coord, 2);
          *)
        end;
      end;
    end
    else
    begin
      Canvas.MoveTo(StartX, StartY);
      Canvas.LineTo(EndX, EndY);

      {MoveToEx(Canvas.Handle, StartX, StartY, nil);
      LineTo(Canvas.Handle, EndX, EndY);}
    end;
  end;

  function GetNextNotLineSuppressedSibling(Item: TElTreeItem): TElTreeItem;
  var
    NextSibling: TElTreeItem;
  begin
    Result := nil;

    NextSibling := Item.GetNextSibling;
    while (NextSibling <> nil) and (Result = nil) do begin
      if not NextSibling.SuppressLines then Result := NextSibling;

      NextSibling := NextSibling.GetNextSibling;
    end;
  end;

var ItemExt : integer;

begin
  ItemExt := THackElTree(Tree).ItemIndent;
  inc(R.Bottom);
  try
    Stack := TElStack.Create;
    TSI := Item.Parent;
    SavePen := ACanvas.Pen.Style;
    SavePenCol := ACanvas.Pen.Color;
    ACanvas.Pen.Style := THackElTree(Tree).LinesStyle;
    ACanvas.Pen.Color := THackElTree(Tree).LinesColor;
    while TSI <> nil do
    begin
      Stack.Push(TSI);
      TSI := TSI.Parent;
    end;
    //DC := ACanvas.Handle;

    if Item.Parent <> nil then
    begin
      TSI := Item;
      while TSI.Parent <> nil do TSI := TSI.Parent;
      if ShowRoot and (GetNextVisChild(THackElTreeItem(TSI).FParent, TSI, false) <> nil) then
      begin
        if THackElTree(Tree).RightAlignedTree then
        begin
          Inc(R.Right, ItemExt);
          if (R.Right - (ItemExt div 2 {+ 4}) > R.Left) then
            DrawLine(ACanvas, R.Right - (ItemExt div 2 {+ 4}), R.Top{ + ((R.Bottom - R.Top + 1) div 2)}, R.Right - (ItemExt div 2 {+ 4}), R.Top + (R.Bottom - R.Top + 1));
          Dec(R.Right, ItemExt);
        end else
        begin
          Dec(R.Left, ItemExt);
          if (R.Left + (ItemExt div 2 {- 4}) < R.Right) then
            DrawLine(ACanvas, R.Left + (ItemExt div 2 {- 4}), R.Top, R.Left + (ItemExt div 2 {- 4}), R.Top + (R.Bottom - R.Top + 1));
          Inc(R.Left, ItemExt);
        end;
      end;
    end;

    if Stack.Count > 0 then
    begin
      TSI := TElTreeItem(Stack.Pop);
      while Stack.Count > 0 do
      begin
        TSI1 := TSI;

        if Stack.Count > 0 then
        begin
          TSI := TElTreeItem(Stack.Pop);

          if (GetNextVisChild(TSI1, TSI, true) <> nil) and ((R.Left + ItemExt div 2) < R.Right) and
             (GetNextNotLineSuppressedSibling(TSI) <> nil) then
            if THackElTree(Tree).RightAlignedTree then
            begin
              if (GetNextVisChild(TSI1, TSI, true) <> nil) and ((R.Right - ItemExt div 2) > R.Left) then
                DrawLine(ACanvas, R.Right - ItemExt div 2, R.Top, R.Right - ItemExt div 2, R.Bottom + 1);
            end
            else
            begin
              if (GetNextVisChild(TSI1, TSI, true) <> nil) and ((R.Left + ItemExt div 2) < R.Right) then
                DrawLine(ACanvas, R.Left + ItemExt div 2, R.Top,  R.Left + ItemExt div 2, R.Bottom + 1);
            end;
        end;
        if THackElTree(Tree).RightAlignedTree then
          dec(R.Right, ItemExt)
        else
          inc(R.Left, ItemExt);
      end;
      if THackElTree(Tree).RightAlignedTree then
        dec(R.Right, ItemExt)
      else
        inc(R.Left, ItemExt);
    end;
    Stack.Free;

    if (Item.Parent <> nil) then
    begin
      if THackElTree(Tree).RightAlignedTree then
      begin
        inc(R.Right, ItemExt);
        if ((R.Right - ItemExt div 2) > R.Left) then
        begin
          DrawLine(ACanvas, R.Right - ItemExt div 2, R.Top, R.Right - ItemExt div 2, R.Top + ((R.Bottom - R.Top + 1) div 2));
          if not Item.SuppressLines then
            DrawLine(ACanvas, R.Right - ItemExt div 2, R.Top + ((R.Bottom - R.Top + 1) div 2), Max(R.Right - ItemExt, R.Left), R.Top + ((R.Bottom - R.Top + 1) div 2));
        end;
      end else
      begin
        dec(R.Left, ItemExt);
        if ((R.Left + ItemExt div 2) < R.Right) then
        begin
          if not Item.SuppressLines or (GetNextNotLineSuppressedSibling(Item) <> nil) then
            DrawLine(ACanvas, R.Left + ItemExt div 2, R.Top, R.Left + ItemExt div 2, R.Top + ((R.Bottom - R.Top + 1) div 2));
          if not Item.SuppressLines then
            DrawLine(ACanvas, R.Left + ItemExt div 2, R.Top + ((R.Bottom - R.Top + 1) div 2), Min(R.Left + ItemExt, R.Right), R.Top + ((R.Bottom - R.Top + 1) div 2));
        end;
      end;

      if (GetNextVisChild(Item.Parent, Item, true) <> nil) and
         (GetNextNotLineSuppressedSibling(Item) <> nil) then
      begin
        if THackElTree(Tree).RightAlignedTree then
        begin
          if ((R.Right - ItemExt div 2) > R.Left) then
            DrawLine(ACanvas, R.Right - ItemExt div 2, R.Top + ((R.Bottom - R.Top + 1) div 2), R.Right - ItemExt div 2, R.Bottom + 1);
        end else
        begin
          if ((R.Left + ItemExt div 2) < R.Right) then
            DrawLine(ACanvas, R.Left + ItemExt div 2, R.Top + ((R.Bottom - R.Top + 1) div 2), R.Left + ItemExt div 2, R.Bottom + 1);
        end;
      end; //if
    end // if
    else
    begin
      if THackElTree(Tree).RightAlignedTree then
        inc(R.Right, ItemExt)
      else
        dec(R.Left, ItemExt);

      if FShowRoot then
      begin
        if THackElTree(Tree).RightAlignedTree then
        begin
          if (R.Right - (ItemExt div 2{ + 4}) > R.Left) then
          begin
            if not Item.SuppressLines then
              DrawLine(ACanvas, R.Right - (ItemExt div 2{ + 4}), R.Top + ((R.Bottom - R.Top + 1) div 2), Max(R.Right - ItemExt, R.Left), R.Top + ((R.Bottom - R.Top + 1) div 2));
            if GetPrevVisChild(THackElTreeItem(Item).FParent, Item, false) <> nil then
              DrawLine(ACanvas, R.Right - (ItemExt div 2 {+ 4}), R.Top, R.Right - (ItemExt div 2 {+ 4}), R.Top + ((R.Bottom - R.Top + 1) div 2));
            if not Item.SuppressLines then
              if GetNextVisChild(THackElTreeItem(Item).FParent, Item, false) <> nil then
                 DrawLine(ACanvas, R.Right - (ItemExt div 2 {+ 4}), R.Top + ((R.Bottom - R.Top + 1) div 2), R.Right - (ItemExt div 2 {+ 4}), R.Top + (R.Bottom - R.Top + 1));
          end;
        end else
        begin
          if (R.Left + (ItemExt div 2 {- 4}) < R.Right) then
          begin
            if not Item.SuppressLines then
              DrawLine(ACanvas, R.Left + (ItemExt div 2 {- 4}), R.Top + ((R.Bottom - R.Top + 1) div 2), Min(R.Left + ItemExt, R.Right), R.Top + ((R.Bottom - R.Top + 1) div 2));
            if GetPrevVisChild(THackElTreeItem(Item).FParent, Item, false) <> nil then
              DrawLine(ACanvas, R.Left + (ItemExt div 2 {- 4}), R.Top, R.Left + (ItemExt div 2 {- 4}), R.Top + ((R.Bottom - R.Top + 1) div 2));
            if (GetNextVisChild(THackElTreeItem(Item).FParent, Item, false) <> nil) then
              if not Item.SuppressLines then
                DrawLine(ACanvas, R.Left + (ItemExt div 2 {- 4}), R.Top + ((R.Bottom - R.Top + 1) div 2), R.Left + (ItemExt div 2 {- 4}), R.Top + (R.Bottom - R.Top + 1));
          end;
        end;
      end;
    end;
    ACanvas.Pen.Style := SavePen;
    ACanvas.Pen.Color := SavePenCol;
  finally
    dec(R.Bottom);
  end;
end;

procedure TElTreePrinter.SetVerticalLines(Value: Boolean);
begin
  if FVerticalLines <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FVerticalLines := Value;
  end;
end;

procedure TElTreePrinter.SetHorizontalLines(Value: Boolean);
begin
  if FHorizontalLines <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FHorizontalLines := Value;
  end;
end;

procedure TElTreePrinter.SetShowLeafButton(Value: Boolean);
begin
  if FShowLeafButton <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FShowLeafButton := Value;
  end;
end;

procedure TElTreePrinter.SetHorzDivLinesColor(Value: TColor);
begin
  if FHorzDivLinesColor <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FHorzDivLinesColor := Value;
  end;
end;

procedure TElTreePrinter.SetVertDivLinesColor(Value: TColor);
begin
  if FVertDivLinesColor <> Value then
  begin
    if (Printer <> nil) and Printer.Active then
      raise EPrinterError.Create('Can''t change properties while printing');
    FVertDivLinesColor := Value;
  end;
end;

procedure TElTreePrinter.TriggerAfterPage(PageNumber : integer);
begin
  if FShowPageNumbers and (FPageNambersLayout = plBottom) then DoDrawPageNumber(PageNumber);
  if Assigned(FOnAfterPage) then FOnAfterPage(Self, PageNumber);
end;

procedure TElTreePrinter.TriggerBeforePage(PageNumber : integer);
begin
  if Assigned(FOnBeforePage) then FOnBeforePage(Self, PageNumber);
  if FShowPageNumbers and (FPageNambersLayout = plTop) then DoDrawPageNumber(PageNumber);
end;

procedure TElTreePrinter.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FTree then
      Tree := nil;
  end;
end;

procedure TElTreePrinter.DoDrawPageNumber(PageNumber : integer);
var S : TElFString;
    R : TRect;
    PageHeight,
    HPP,
    WP,
    HP: integer;
    FDC : THandle;
begin
  S  := FPageNumbersText + IntToStr(PageNumber + 1);
  if Assigned(FOnDrawPageNumber) then
    FOnDrawPageNumber(Self, S, PageNumber);
  FDC:= GetDC(0);
  PageHeight := MulDiv(Printer.PageHeight, GetDeviceCaps(FDC, LOGPIXELSY) * 100, Scale * 2540);
  WP := MulDiv(Printer.PageWidth, GetDeviceCaps(FDC, LOGPIXELSX), 2540);
  HP := MulDiv(Printer.TopMargin, GetDeviceCaps(FDC, LOGPIXELSY), 2540);
  HPP:= MulDiv(Printer.BottomMargin, GetDeviceCaps(FDC, LOGPIXELSY), 2540);
  ReleaseDC(0, FDC);
  if FPageNambersLayout = plTop then
    R.Top  := -(HP - Printer.Canvas[PageNumber].TextHeight(S)) div 2
  else
    R.Top := PageHeight + abs(HPP-Printer.Canvas[PageNumber].TextHeight(S)) div 2;

  if FPageNumbersAlignment <> taLeftJustify then
  begin
    R.Left := (WP - Printer.Canvas[PageNumber].TextWidth(S)){ div 2};
    if FPageNumbersAlignment = taCenter then
      R.Left := R.Left div 2;
  end
  else
    R.Left := 0;

  {$ifdef HAS_HTML_RENDER}
  if Pos('<html>', s) = 1 then
  begin
    FRender.Data.DefaultBgColor := Printer.Canvas[PageNumber].Brush.Color;
    FRender.Data.DefaultColor := Font.Color;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultFont := Font.Name;
    FRender.Data.Charset := Font.Charset;
    FRender.PrepareText(S, 0, false);
    R.Right := R.Left + FRender.Data.TextSize.cx;
    R.Bottom := R.Top + FRender.Data.TextSize.cy;

    FRender.DrawText(Printer.Canvas[PageNumber], Point(0, 0), R, clNone);
  end
  else
  {$endif}
    Printer.Canvas[PageNumber].TextOut(R.Left, R.Top, S);
end;

procedure TElTreePrinter.DoDrawCaption(PageNumber : integer; var Rec: TRect);
begin
  {$ifdef HAS_HTML_RENDER}
  if Pos('<html>', FCaption) = 1 then
  begin
    FRender.Data.DefaultBgColor := Printer.Canvas[PageNumber].Brush.Color;
    FRender.Data.DefaultColor := Font.Color;
    FRender.Data.DefaultStyle := Font.Style;
    FRender.Data.DefaultHeight := Font.Height;
    FRender.Data.DefaultFont := Font.Name;
    FRender.Data.Charset := Font.Charset;
    FRender.PrepareText(FCaption, 0, false);
    Rec.Bottom := Rec.Top + FRender.Data.TextSize.cy;
  end
  else
  {$endif}
    Rec.Bottom:= Rec.Top+Printer.Canvas[PageNumber].TextHeight(FCaption);

  if Assigned(FOnDrawCaption) then
    FOnDrawCaption(Self, FCaption, PageNumber, Rec);

  {$ifdef HAS_HTML_RENDER}
  if Pos('<html>', FCaption) = 1 then
    FRender.DrawText(Printer.Canvas[PageNumber], Point(0, 0), Rec, clNone)
  else
  {$endif}
    Printer.Canvas[PageNumber].TextOut(Rec.Left, Rec.Top, FCaption);
//    Printer.Canvas[PageNumber].
end;

end.
