{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

{$R ..\Design\ElTreeMLGen.dcr}

(*

Template description:

Loop:

ITEMS
    - this loop defines the part of template, which is processed for every
      item
SECTIONS
    - this loop defines the part of template, which is processed for every
      section in the header
CELLS
    - this loop defines the part of template, which is processed for every cell
      of the item
      This loop must only be used when generating multicolumn tree.

Tags:

<HEADER></HEADER>
    - defines the part of text that belongs to the header. Header-related
      macros and tags can be used only within this tag.
<SECTION></SECTION>
    - defines the part of the text that belongs to the header section.

<ITEM></ITEM>
    - defines the part of text that belongs to the tree item. Item-related macros
      and tags can be used only within this tag.
<CELL></CELL>
    - defines the part of text that belongs to the cell of the tree item.
      Cell-related macros can be used only within this tag.

Macros:

Section-related macros:

SECTIONINDEX
    - replaced by section index (it's "ID", as section's position can be changed
      in runtime, but index remains unchanged). 
SECTIONDATA
    - replaced by section contents, be it a text or owner-drawn picture.
SECTIONWIDTH [adjustment="+/-N"]
    - current width of the section as it is drawn on the screen
      Adjustment is added to the value before replacement
OPTSECTIONWIDTH [adjustment="+/-N"]
    - optimal width of the section as it is drawn on the screen
      Adjustment is added to the value before replacement

Item-related macros:
ABSOLUTEINDEX [adjustment="+/-N"]
    - replaced by the absolute index of the item in the tree. This index is the
      number of current item in the list of all items including visible, invisible,
      hidden and any other item. Adjustment is added to the value before replacement
RELATIVEINDEX [adjustment="+/-N"]
    - replaced by the index of the item in the list of the printable items. Can
      be treated as the number of the item in the export sequence.
      Adjustment is added to the value before replacement
CHILDINDEX  [adjustment="+/-N"]
    - replaced by the number of the item in the list of children of the item's
      parent. Adjustment is added to the value before replacement
ITEMLEVEL  [adjustment="+/-N"]
    - replaced by the level of the item. Adjustment is added to the value
      before replacement
INDENT indentchars="..."
    - replaced by indentchars sequence repeated Indent times. For exanple, when
      indentchars is "-", INDENT macro for the item with level 3 will be replaced
      with "---"

cell-related macros:

CELLDATA
    - replaced by cell contents, be it a text or owner-drawn picture.
CELLHEIGHT [adjustment="+/-N"]
    - current height of the cell as it is drawn on the screen
      Adjustment is added to the value before replacement
CELLWIDTH [adjustment="+/-N"]
    - current width of the cell as it is drawn on the screen
      Adjustment is added to the value before replacement
OPTCELLHEIGHT [adjustment="+/-N"]
    - optimal height of the cell as it is drawn on the screen
      Adjustment is added to the value before replacement
OPTCELLWIDTH [adjustment="+/-N"]
    - optimal width of the cell as it is drawn on the screen
      Adjustment is added to the value before replacement

FONTSIZE [adjustment="+/-N"]
    - replaced by font size with adjustment specified by "adjustment" parameter.
      For example, if font size is 8, macro FONTSIZE adjustment="+2" wil be replaced with
      10.
FONTNAME
    - replaced by font name

CELLBKCOLOR (format="dec/hex")
    - replaced by (hexa)decimal value of the cell background color. The value is
      NOT prefixed by any symbol. "Format" parameter specifies the format of the
      color value.
TEXTBKCOLOR (format="dec/hex")
    - replaced by (hexa)decimal value of the text background color. The value is
      NOT prefixed by any symbol. "Format" parameter specifies the format of the
      color value.
TEXTCOLOR (format="dec/hex")
    - replaced by (hexa)decimal value of the text color. The value is NOT
      prefixed by any symbol. "Format" parameter specifies the format of the
      color value.

Predefined conditions:

SectionIndexIs {eq="number"|lt="number"|gt="number"}
    - evaluates to true if current section index is equal, lower than or
      greater than specified number

ItemAbsIndexIs {eq="number"|lt="number"|gt="number"}
    - evaluates to true if current item absolute index is equal, lower than or
      greater than specified number

ItemChildIndexIs {eq="number"|lt="number"|gt="number"}
    - evaluates to true if current item child index is equal, lower than or
      greater than specified number

ItemLevelIs {eq="number"|lt="number"|gt="number"}
    - evaluates to true if current item level is equal, lower than or
      greater than specified number

*)

unit ElTreeMLGen;

interface

uses Classes,
     SysUtils,

     Graphics,
     Windows,
     {$ifndef VCL_4_USED}
     ComCtrls,
     {$endif}
     Controls,

     ElTools,
     ElTree,
     ElHeader,
     ElStrUtils,

     ElMLGen;

type

  TMLTreeGenOwnerDrawEvent = procedure (Sender : TObject; Item : TElTreeItem; 
          SectionIndex : integer; MaxRect : TRect; var ReplacementString : 
          string) of object;
  TMLTreeGenImageEvent = procedure (Sender : TObject; ImageList : TImageList; 
          ImageIndex : integer; var ReplacementString : string) of object;
  TMlTreeGenItemGenerateEvent = procedure (Sender : TObject; Item : TElTreeItem;
          var Generate : boolean) of object;
  TMlTreeGenItemGeneratedEvent = procedure (Sender : TObject; Item : 
          TElTreeItem) of object;
  TElTreeMLGenerator = class (TCustomElMLGen)
  private
    FOnImage: TMLTreeGenImageEvent;
  protected
    FBusy: Boolean;
    FCurItem: TElTreeItem;
    FCurItemIdx: Integer;
    FCurSection: TElHeaderSection;
    FCurSectionIdx: Integer;
    FGenerateColumns: Boolean;
    FGenerateHeader: Boolean;
    FGenerateHiddenItems: Boolean;
    FGenerateImages: Boolean;
    FGenerateInvisibleItems: Boolean;
    FHeaderOnEveryPage: Boolean;
    FItemsPerPage: Integer;
    FLastItemIdx: Integer;
    FLastSectionIdx: Integer;
    FOnItemGenerate: TMlTreeGenItemGenerateEvent;
    FOnItemGenerated: TMlTreeGenItemGeneratedEvent;
    FOnOwnerDraw: TMLTreeGenOwnerDrawEvent;
    FRelItemIdx: Integer;
    FRelSectionIdx: Integer;
    FTree: TCustomElTree;
    VirtStyle: TElCellStyle;
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    function BuildTree(Item : TElTreeItem): string;
    function DrawCell(Item : TElTreeItem; SectionIndex : integer): string;
    function DrawHeaderSection(SectionIndex : integer): string;
    procedure IfFound(IfName : string; TagParameters : TStringParameters; var 
            ResultValue : boolean); override;
    procedure IsTag(TagName : string; var IsTag : boolean); override;
    procedure LoopIteration(LoopNumb: integer; LoopName: string; TagParameters 
            : TStringParameters; var LoopDone : boolean); override;
    procedure MacroFound(MacroName : string; TagParameters : TStringParameters; 
            var MacroResult : string; var UseTranslationTable : boolean); 
            override;
    procedure SetGenerateColumns(Value: Boolean);
    procedure SetGenerateHeader(Value: Boolean);
    procedure SetGenerateHiddenItems(Value: Boolean);
    procedure SetGenerateInvisibleItems(Value: Boolean);
    procedure SetHeaderOnEveryPage(Value: Boolean);
    procedure SetItemsPerPage(Value: Integer);
    procedure SetTree(Value: TCustomElTree);
    procedure TagFound(Tag : string; const TagClosed : boolean; TagParameters : 
            TStringParameters); override;
    procedure TriggerImageEvent(ImageList : TImageList; ImageIndex : integer; 
            var ReplacementString : string); virtual;
    procedure TriggerItemGeneratedEvent(Item : TElTreeItem); virtual;
    procedure TriggerItemGenerateEvent(Item : TElTreeItem; var Generate : 
            boolean); virtual;
    procedure TriggerOwnerDrawEvent(Item : TElTreeItem; SectionIndex : integer; 
            MaxRect : TRect; var ReplacementString : string); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentItem: TElTreeItem read FCurItem;
    property CurrentSection: TElHeaderSection read FCurSection;
  published
    property GenerateColumns: Boolean read FGenerateColumns write 
            SetGenerateColumns;
    property GenerateHeader: Boolean read FGenerateHeader write 
            SetGenerateHeader default true;
    property GenerateHiddenItems: Boolean read FGenerateHiddenItems write 
            SetGenerateHiddenItems;
    property GenerateInvisibleItems: Boolean read FGenerateInvisibleItems write 
            SetGenerateInvisibleItems;
    property HeaderOnEveryPage: Boolean read FHeaderOnEveryPage write 
            SetHeaderOnEveryPage;
    property ItemsPerPage: Integer read FItemsPerPage write SetItemsPerPage 
            default 0;
    property OnAfterExecute;
    property OnBeforeExecute;
    property OnIfFound;
    property OnImage: TMLTreeGenImageEvent read FOnImage write FOnImage;
    property OnItemGenerate: TMlTreeGenItemGenerateEvent read FOnItemGenerate 
            write FOnItemGenerate;
    property OnItemGenerated: TMlTreeGenItemGeneratedEvent read 
            FOnItemGenerated write FOnItemGenerated;
    property OnLoopIteration;
    property OnMacroFound;
    property OnOwnerDraw: TMLTreeGenOwnerDrawEvent read FOnOwnerDraw write 
            FOnOwnerDraw;
    property OnPageBegin;
    property OnPageEnd;
    property OnTagFound;
    property OnWriteString;
    property TagPrefix;
    property Template;
    property Tree: TCustomElTree read FTree write SetTree;
  end;
  
procedure Register;
  
implementation

type

  THackElTree = class (TCustomElTree)
  end;
  
  THackElHeaderSection = class (TElHeaderSection)
  end;


{
****************************** TElTreeMLGenerator ******************************
}
constructor TElTreeMLGenerator.Create(AOwner: TComponent);
begin
  inherited;
  {$ifdef ELTREE_USE_STYLE}
  VirtStyle := TElCellStyle.Create(nil);
  {$endif}
end;{TElTreeMLGenerator.Create cat:Undefined}

destructor TElTreeMLGenerator.Destroy;
begin
  {$ifdef ELTREE_USE_STYLE}
  VirtStyle.Free;
  {$endif}
  inherited;
end;{TElTreeMLGenerator.Destroy cat:Undefined}

procedure TElTreeMLGenerator.AfterExecute;
begin
  FBusy := false;
  inherited;
end;{TElTreeMLGenerator.AfterExecute cat:Undefined}

procedure TElTreeMLGenerator.BeforeExecute;
begin
  inherited;
  FBusy := true;
  
  FLastItemIdx   := -1;
  FCurItemIdx    := -1;
  FCurSectionIdx := -1;
  FLastSectionIdx:= -1;
  FRelItemIdx    := -1;
  FRelSectionIdx := -1;
  
  FCurItem       := nil;
  FCurSection    := nil;
end;{TElTreeMLGenerator.BeforeExecute cat:Undefined}

function TElTreeMLGenerator.BuildTree(Item : TElTreeItem): string;
var
  S: string;
begin
  result := '';
  if Item.ImageIndex2 <> -1 then
  begin
    if THackElTree(Tree).Images2 <> nil then
      TriggerImageEvent(THackElTree(Tree).Images2, Item.ImageIndex2, S)
    else
    if THackElTree(Tree).Images <> nil then
      TriggerImageEvent(THackElTree(Tree).Images, Item.ImageIndex2, S);
    Result := Result + S;
  end;
  if Item.ImageIndex <> -1 then
  begin
    if THackElTree(Tree).Images <> nil then
      TriggerImageEvent(THackElTree(Tree).Images, Item.ImageIndex, S);
    Result := Result + S;
  end;
end;{TElTreeMLGenerator.BuildTree cat:Undefined}

function TElTreeMLGenerator.DrawCell(Item : TElTreeItem; SectionIndex : 
        integer): string;
var
  Rect: TRect;
  Size: TPoint;
begin
  THackElTree(Tree).TriggerMeasureItemPartEvent(Item, SectionIndex, Size);
  Rect.Left := 0;
  Rect.Top  := 0;
  Rect.Right := Size.x;
  Rect.Bottom := Item.Height;
  Result := '';
  Self.TriggerOwnerDrawEvent(Item, SectionIndex, Rect, Result);
end;{TElTreeMLGenerator.DrawCell cat:Undefined}

function TElTreeMLGenerator.DrawHeaderSection(SectionIndex : integer): string;
var
  Rect: TRect;
  Size: TPoint;
begin
  Size.x := THackElTree(Tree).HeaderSections.Owner.MeasureSectionWidth(
             THackElTree(Tree).HeaderSections.Item[SectionIndex], nil, @Size.y);
  Rect.Left := 0;
  Rect.Top  := 0;
  Rect.BottomRight := Size;
  Result := '';
  TriggerOwnerDrawEvent(nil, SectionIndex, Rect, Result);
end;{TElTreeMLGenerator.DrawHeaderSection cat:Undefined}

procedure TElTreeMLGenerator.IfFound(IfName : string; TagParameters : 
        TStringParameters; var ResultValue : boolean);
var S : string;
begin
  if IfName = 'GenerateHeader' then
  begin
    if GenerateHeader then
      ResultValue := HeaderOnEveryPage or (PageCount = 1)
    else
      ResultValue := false;
  end
  else
  if IfName = 'GenerateColumns' then
  begin
    ResultValue := GenerateColumns;
  end
  else
  if IfName = 'SectionIndexIs' then
  begin
    S := TagParameters.GetValueByNameEx('eq', '');
    if S <> '' then
    begin
      ResultValue := (CurrentSection <> nil) and
                     (CurrentSection.Index = StrToIntDef(S, -2));
      exit;
    end;
    S := TagParameters.GetValueByNameEx('lt', '');
    if S <> '' then
    begin
      ResultValue := (CurrentSection <> nil) and
                     (CurrentSection.Index < StrToIntDef(S, -2));
      exit;
    end;
    S := TagParameters.GetValueByNameEx('gt', '');
    if S <> '' then
    begin
      ResultValue := (CurrentSection <> nil) and
                     (CurrentSection.Index > StrToIntDef(S, -2));
      exit;
    end;
    inherited IfFound(IfName, TagParameters, ResultValue);
  end
  else
  if IfName = 'ItemLevelIs' then
  begin
    S := TagParameters.GetValueByNameEx('eq', '');
    if S <> '' then
    begin
      ResultValue := (CurrentItem <> nil) and
                     (CurrentItem.Level = StrToIntDef(S, -2));
      exit;
    end;
    S := TagParameters.GetValueByNameEx('lt', '');
    if S <> '' then
    begin
      ResultValue := (CurrentItem <> nil) and
                     (CurrentItem.Level < StrToIntDef(S, -2));
      exit;
    end;
    S := TagParameters.GetValueByNameEx('gt', '');
    if S <> '' then
    begin
      ResultValue := (CurrentItem <> nil) and
                     (CurrentItem.Level > StrToIntDef(S, -2));
      exit;
    end;
    inherited IfFound(IfName, TagParameters, ResultValue);
  end
  else
  if IfName = 'ItemAbsIndexIs' then
  begin
    S := TagParameters.GetValueByNameEx('eq', '');
    if S <> '' then
    begin
      ResultValue := (CurrentItem <> nil) and
                     (CurrentItem.AbsoluteIndex = StrToIntDef(S, -2));
      exit;
    end;
    S := TagParameters.GetValueByNameEx('lt', '');
    if S <> '' then
    begin
      ResultValue := (CurrentItem <> nil) and
                     (CurrentItem.AbsoluteIndex < StrToIntDef(S, -2));
      exit;
    end;
    S := TagParameters.GetValueByNameEx('gt', '');
    if S <> '' then
    begin
      ResultValue := (CurrentItem <> nil) and
                     (CurrentItem.AbsoluteIndex > StrToIntDef(S, -2));
      exit;
    end;
    inherited IfFound(IfName, TagParameters, ResultValue);
  end
  else
  if IfName = 'ItemChildIndexIs' then
  begin
    S := TagParameters.GetValueByNameEx('eq', '');
    if S <> '' then
    begin
      ResultValue := (CurrentItem <> nil) and
                     (CurrentItem.Index = StrToIntDef(S, -2));
      exit;
    end;
    S := TagParameters.GetValueByNameEx('lt', '');
    if S <> '' then
    begin
      ResultValue := (CurrentItem <> nil) and
                     (CurrentItem.Index < StrToIntDef(S, -2));
      exit;
    end;
    S := TagParameters.GetValueByNameEx('gt', '');
    if S <> '' then
    begin
      ResultValue := (CurrentItem <> nil) and
                     (CurrentItem.Index > StrToIntDef(S, -2));
      exit;
    end;
    inherited IfFound(IfName, TagParameters, ResultValue);
  end
  else
    inherited IfFound(IfName, TagParameters, ResultValue);
end;{TElTreeMLGenerator.IfFound cat:Undefined}

procedure TElTreeMLGenerator.IsTag(TagName : string; var IsTag : boolean);
begin
  if (TagName = 'HEADER') or
     (TagName = '/HEADER') or
     (TagName = 'SECTION') or
     (TagName = '/SECTION') or
     (TagName = 'ITEM') or
     (TagName = '/ITEM') or
     (TagName = 'CELL') or
     (TagName = '/CELL') then
      IsTag := true
  else
    inherited
end;{TElTreeMLGenerator.IsTag cat:Undefined}

procedure TElTreeMLGenerator.LoopIteration(LoopNumb: integer; LoopName: string; 
        TagParameters : TStringParameters; var LoopDone : boolean);
begin
  LoopDone := false;
end;{TElTreeMLGenerator.LoopIteration cat:Undefined}

procedure TElTreeMLGenerator.MacroFound(MacroName : string; TagParameters :
        TStringParameters; var MacroResult : string; var UseTranslationTable :
        boolean);
var
  SText: TElFString;
  si: Integer;
  Size: TPoint;

  {$ifdef ELTREE_USE_STYLES}
  CurStyle : TElCellStyle;
  {$endif}

  function StyleResult(MacroName : string; TagParameters :
    TStringParameters) : string;
  var ns : boolean;
  begin
    ns := true;
    {$ifdef ELTREE_USE_STYLES}
    CurStyle := nil;
    if FCurItem.UseStyles then
    begin
      if not GenerateHeader then
      begin
        if THackElTree(Tree).VirtualityLevel = vlNone then
        begin
          CurStyle := FCurItem.MainStyle;
        end
        else
        begin
          CurStyle := VirtStyle;
          THackElTree(Tree).TriggerVirtualStyleNeeded(FCurItem, FCurSection.Index, CurStyle);
        end
      end
      else
      begin
        if THackElTree(Tree).VirtualityLevel = vlNone then
        begin
          si := FCurSection.Index;
          if si > THackElTree(Tree).MainTreeColumn then
            dec(si);
          if (FCurItem.StylesCount > si)  and (not FCurSection.UseMainStyle) then
            CurStyle := FCurItem.Styles[si];
        end
        else
        begin
          CurStyle := VirtStyle;
          THackElTree(Tree).TriggerVirtualStyleNeeded(FCurItem, FCurSection.Index, CurStyle);
        end;
      end;
      if (CurStyle <> nil) and (not CurStyle.OwnerProps) then
        ns := false;
      if not ns then
      begin
        if MacroName = 'CELLBKCOLOR' then
        begin
          if CurStyle.UseBkColor then
            result := IntToHex(ColorToRGB(CurStyle.CellBkColor), 6)
          else
          if (not FCurItem.ParentColors) and (FCurItem.UseBkColor) then
            result := IntToHex(ColorToRGB(FCurItem.RowBkColor), 6)
          else
            result := IntToHex(ColorToRGB(THackElTree(Tree).BkColor), 6);
        end
        else
        if MacroName = 'TEXTBKCOLOR' then
        begin
          if CurStyle.UseBkColor then
            result := IntToHex(ColorToRGB(CurStyle.TextBkColor), 6)
          else
          if (not FCurItem.ParentColors) and (FCurItem.UseBkColor) then
            result := IntToHex(ColorToRGB(FCurItem.BkColor), 6)
          else
            result := IntToHex(ColorToRGB(THackElTree(Tree).BkColor), 6);
        end
        else
        if MacroName = 'TEXTCOLOR' then
        begin
          result := IntToHex(ColorToRGB(CurStyle.TextColor), 6);
        end
        else
        if MacroName = 'FONTSIZE' then
        begin
          result := IntToStr(CurStyle.FontSize + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0));
        end
        else
        if MacroName = 'FONTNAME' then
        begin
          result := CurStyle.FontName
        end
        else
          result := '';
      end;
    end;
    {$endif}
    if ns then
    begin
      if (MacroName = 'CELLBKCOLOR') then
      begin
        if (not FCurItem.ParentColors) and (FCurItem.UseBkColor) then
          result := IntToHex(ColorToRGB(FCurItem.RowBkColor), 6)
        else
        if THackElTree(Tree).StripedItems then
          if FCurItemIdx mod 2 = 0 then
            result := IntToHex(ColorToRGB(THackElTree(Tree).StripedEvenColor), 6)
          else
            result := IntToHex(ColorToRGB(THackElTree(Tree).StripedOddColor), 6)
        else
          result := IntToHex(ColorToRGB(THackElTree(Tree).BkColor), 6);
      end
      else
      if (MacroName = 'TEXTBKCOLOR') then
      begin
        if (not FCurItem.ParentColors) and (FCurItem.UseBkColor) then
          result := IntToHex(ColorToRGB(FCurItem.BkColor), 6)
        else
        if THackElTree(Tree).StripedItems then
          if FCurItemIdx mod 2 = 0 then
            result := IntToHex(ColorToRGB(THackElTree(Tree).StripedEvenColor), 6)
          else
            result := IntToHex(ColorToRGB(THackElTree(Tree).StripedOddColor), 6)
        else
          result := IntToHex(ColorToRGB(THackElTree(Tree).BkColor), 6);
      end
      else
      if MacroName = 'TEXTCOLOR' then
      begin
        if (not FCurItem.ParentColors) then
          result := IntToHex(ColorToRGB(FCurItem.Color), 6)
        else
          result := IntToHex(ColorToRGB(THackElTree(Tree).TextColor), 6);
      end
      else
      if MacroName = 'FONTSIZE' then
        result := IntToStr(THackElTree(Tree).Font.Size + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0))
      else
      if MacroName = 'FONTNAME' then
        result := THackElTree(Tree).Font.Name
      else
        result := '';
    end;
  end;
  
  var FSaveUse : boolean;
  
begin
  FSaveUse := UseTranslationTable;
  UseTranslationTable := true;
  if MacroName = 'ABSOLUTEINDEX' then
  begin
    if FCurItem = nil then
      raise Exception.Create('ABSOLUTEINDEX macro can be used only in <ITEM> tag');
    MacroResult := IntToStr(FCurItemIdx + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0));
  end
  else
  if MacroName = 'RELATIVEINDEX' then
  begin
    if FCurItem = nil then
      raise Exception.Create('RELATIVEINDEX macro can be used only in <ITEM> tag');
    MacroResult := IntToStr(FRelItemIdx + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0));
  end
  else
  if MacroName = 'CHILDINDEX' then
  begin
    if FCurItem = nil then
      raise Exception.Create('CHILDINDEX macro can be used only in <ITEM> tag');
    MacroResult := IntToStr(FCurItem.Index + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0));
  end
  else
  if MacroName = 'INDENT' then
  begin
    if FCurItem = nil then
      raise Exception.Create('INDENT macro can be used only in <ITEM> tag');
    if TagParameters.FindItemByName('indentchars') = -1 then
      raise Exception.Create('Indentation characters must be specified in INDENT macro');

    MacroResult := MakeString(FCurItem.Level, TagParameters.GetValueByNameEx('indentchars', ' '));
  end
  else
  if MacroName = 'LEVEL' then
  begin
    if FCurItem = nil then
      raise Exception.Create('LEVEL macro can be used only in <ITEM> tag');
    MacroResult := IntToStr(FCurItem.Level + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0));
  end
  else
  if MacroName = 'CELLDATA' then
  begin
    if (FCurItem = nil) or (GenerateColumns and (FCurSection = nil)) then
      raise Exception.Create('CELLDATA macro can be used only in <ITEM> or <CELL> tag');
  
    SI := FCurSection.Index;
    {$ifdef ELTREE_USE_STYLES}
    if FCurItem.UseStyles then
    begin
      if THackElTree(Tree).VirtualityLevel = vlNone then
      begin
        if (not GenerateColumns) or (SI = THackElTree(Tree).MainTreeColumn) then
          CurStyle := FCurItem.MainStyle
        else
        begin
          si := FCurSection.Index;
          if si > THackElTree(Tree).MainTreeColumn then
            dec(si);
  
          if (FCurItem.StylesCount > si) and (not FCurSection.UseMainStyle) then
            CurStyle := FCurItem.Styles[si]
          else
            CurStyle := FCurItem.MainStyle;
        end;
      end
      else
      begin
        CurStyle := VirtStyle;
        THackElTree(Tree).TriggerVirtualStyleNeeded(FCurItem, FCurSection.Index, CurStyle);
      end;
      MacroResult := '';
      if (not GenerateColumns) or (SI = THackElTree(Tree).MainTreeColumn) then
        MacroResult := BuildTree(FCurItem);
      if CurStyle.Style = elhsOwnerDraw then
      begin
        MacroResult := MacroResult + DrawCell(FCurItem, FCurSection.Index);
        exit;
      end;
    end;
    {$endif}
    if (not GenerateColumns) or (SI = THackElTree(Tree).MainTreeColumn) then
    begin
      SText := FCurItem.Text;
      if ( (not THackElTree(Tree).OwnerDrawByColumn) and
           (SText = THackElTree(Tree).OwnerDrawMask)) then
        MacroResult := BuildTree(FCurItem) + DrawCell(FCurItem, -1)
      else
        MacroResult := BuildTree(FCurItem) + SText;
    end
    else
    begin
      if THackElTree(Tree).VirtualityLevel = vlNone then
      begin
        if si > THackElTree(Tree).MainTreeColumn then dec(si);
        if FCurItem.ColumnText.Count > si then
          SText := FCurItem.ColumnText[si]
        else
          SText := '';
      end
      else
        THackElTree(Tree).TriggerVirtualTextNeeded(FCurItem, SI, SText);
  
      if ( (not THackElTree(Tree).OwnerDrawByColumn) and
           (SText = THackElTree(Tree).OwnerDrawMask)) then
        MacroResult := BuildTree(FCurItem) + DrawCell(FCurItem, -1)
      else
      if (THackElTree(Tree).OwnerDrawByColumn) and
         (FCurSection.Style = elhsOwnerDraw) then
        MacroResult := DrawCell(FCurItem, FCurSection.Index)
      else
        MacroResult := SText;
    end;
  end
  else
  if MacroName = 'CELLHEIGHT' then
  begin
    if (FCurItem = nil) or (GenerateColumns and (FCurSection = nil)) then
      raise Exception.Create('CELLHEIGHT macro can be used only in <ITEM> or <CELL> tag');
    if (not FCurItem.Multiline) or (FCurItem.OwnerHeight and THackElTree(Tree).AdjustMultilineHeight) then
      MacroResult := IntToStr(FCurItem.Height + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0))
    else
      MacroResult := IntToStr(THackElTree(Tree).LineHeight + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0))
  end
  else
  if MacroName = 'OPTCELLHEIGHT' then
  begin
    if (FCurItem = nil) or (GenerateColumns and (FCurSection = nil)) then
      raise Exception.Create('OPTCELLHEIGHT macro can be used only in <ITEM> or <CELL> tag');

    if (not FCurItem.Multiline) or (FCurItem.OwnerHeight and THackElTree(Tree).AdjustMultilineHeight) then
      MacroResult := IntToStr(FCurItem.Height + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0))
    else
    begin
      THackElTree(Tree).MeasureCell(FCurItem, FCurSection.Index, Size);
      MacroResult := IntToStr(Size.y + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0))
    end;
  end
  else
  if MacroName = 'CELLWIDTH' then
  begin
    if (FCurItem = nil) or (GenerateColumns and (FCurSection = nil)) then
      raise Exception.Create('CELLWIDTH macro can be used only in <ITEM> or <CELL> tag');
    if GenerateColumns then
      MacroResult := IntToStr(FCurSection.Width + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0))
    else
      MacroResult := IntToStr(Tree.ClientWidth + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0))
  end
  else
  if MacroName = 'OPTCELLWIDTH' then
  begin
    if (FCurItem = nil) or (GenerateColumns and (FCurSection = nil)) then
      raise Exception.Create('OPTCELLWIDTH macro can be used only in <ITEM> or <CELL> tag');
  
    if GenerateColumns then
    begin
      THackElTree(Tree).MeasureCell(FCurItem, FCurSection.Index, Size);
      MacroResult := IntToStr(Size.x + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0))
    end
    else
      MacroResult := IntToStr(FCurItem.GetWidth + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0))
  end
  else
  if (MacroName = 'FONTSIZE') or
     (MacroName = 'FONTNAME') or
     (MacroName = 'CELLBKCOLOR') or
     (MacroName = 'TEXTBKCOLOR') or
     (MacroName = 'TEXTCOLOR') then
  begin
    MacroResult := StyleResult(MacroName, TagParameters);
  end
  else
  if MacroName = 'SECTIONINDEX' then
  begin
    if (not GenerateColumns) or (GenerateColumns and (FCurSection = nil)) then
      raise Exception.Create('SECTIONDATA macro can be used only in <SECTION> tag');
    MacroResult := IntToStr(FCurSection.Index);
  end
  else
  if MacroName = 'SECTIONDATA' then
  begin
    if (not GenerateColumns) or (GenerateColumns and (FCurSection = nil)) then
      raise Exception.Create('SECTIONDATA macro can be used only in <SECTION> tag');
    if FCurSection.Style = elhsOwnerDraw then
      MacroResult := Self.DrawHeaderSection(FCurSection.Index)
    else
      MacroResult := FCurSection.Text;
  end
  else
  if MacroName = 'SECTIONWIDTH' then
  begin
    if (not GenerateColumns) or (GenerateColumns and (FCurSection = nil)) then
      raise Exception.Create('SECTIONWIDTH macro can be used only in <SECTION> tag');
    MacroResult := IntToStr(FCurSection.Width + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0));
  end
  else
  if MacroName = 'OPTSECTIONWIDTH' then
  begin
    if (not GenerateColumns) or (GenerateColumns and (FCurSection = nil)) then
      raise Exception.Create('OPTSECTIONWIDTH macro can be used only in <SECTION> tag');
    MacroResult := IntToStr(
              Max(THackElTree(Tree).FHeader.MeasureSectionWidth(FCurSection, nil, nil),
                  THackElTree(Tree).MeasureColumnWidth(FCurSection.Index, false))
              + StrToIntDef(TagParameters.GetValueByNameEx('adjustment', '0'), 0));
  end
  else
  begin
    UseTranslationTable := FSaveUse;
    inherited MacroFound(MacroName, TagParameters, MacroResult, UseTranslationTable);
  end;
end;{TElTreeMLGenerator.MacroFound cat:Undefined}

procedure TElTreeMLGenerator.SetGenerateColumns(Value: Boolean);
begin
  if FGenerateColumns <> Value then
  begin
    if FBusy then
      raise Exception.Create('Can''t change properties during template processing.');
    FGenerateColumns := Value;
  end;
end;{TElTreeMLGenerator.SetGenerateColumns cat:Undefined}

procedure TElTreeMLGenerator.SetGenerateHeader(Value: Boolean);
begin
  if FGenerateHeader <> Value then
  begin
    if FBusy then
      raise Exception.Create('Can''t change properties during template processing.');
    FGenerateHeader := Value;
  end;
end;{TElTreeMLGenerator.SetGenerateHeader cat:Undefined}

procedure TElTreeMLGenerator.SetGenerateHiddenItems(Value: Boolean);
begin
  if FGenerateHiddenItems <> Value then
  begin
    if FBusy then
      raise Exception.Create('Can''t change properties during template processing.');
    FGenerateHiddenItems := Value;
  end;
end;{TElTreeMLGenerator.SetGenerateHiddenItems cat:Undefined}

procedure TElTreeMLGenerator.SetGenerateInvisibleItems(Value: Boolean);
begin
  if FGenerateInvisibleItems <> Value then
  begin
    if FBusy then
      raise Exception.Create('Can''t change properties during template processing.');
    FGenerateInvisibleItems := Value;
  end;
end;{TElTreeMLGenerator.SetGenerateInvisibleItems cat:Undefined}

procedure TElTreeMLGenerator.SetHeaderOnEveryPage(Value: Boolean);
begin
  if FHeaderOnEveryPage <> Value then
  begin
    if FBusy then
      raise Exception.Create('Can''t change properties during template processing.');
    FHeaderOnEveryPage := Value;
  end;
end;{TElTreeMLGenerator.SetHeaderOnEveryPage cat:Undefined}

procedure TElTreeMLGenerator.SetItemsPerPage(Value: Integer);
begin
  if FItemsPerPage <> Value then
  begin
    if FBusy then
      raise Exception.Create('Can''t change properties during template processing.');
    FItemsPerPage := Value;
  end;
end;{TElTreeMLGenerator.SetItemsPerPage cat:Undefined}

procedure TElTreeMLGenerator.SetTree(Value: TCustomElTree);
begin
  if FTree <> Value then
  begin
    if FBusy then
      raise Exception.Create('Can''t change Tree during template processing.');
    FTree := Value;
  end;
end;{TElTreeMLGenerator.SetTree cat:Undefined}

procedure TElTreeMLGenerator.TagFound(Tag : string; const TagClosed : boolean; 
        TagParameters : TStringParameters);
var
  Generate: Boolean;
begin
  if (Tag = 'HEADER') then
  begin
    FCurSectionIdx := -1;
    FLastSectionIdx := -1;
  end
  else
  if (Tag = '/HEADER') then
  begin
    FCurSectionIdx := -1;
    FLastSectionIdx := -1;
    FCurSection := nil;
  end
  else
  if (Tag = 'SECTION') then
  begin
    if GetLoopIndex('SECTIONS') = -1 then
      raise Exception.Create('<SECTION> tag must be used inside of SECTIONS loop');
  
    FCurSectionIdx := FLastSectionIdx + 1;
    if FCurSectionIdx >= THackElTree(Tree).HeaderSections.Count then
    begin
      FCurSection := nil;
      LoopBreak('SECTIONS');
    end
    else
      FCurSection := THackElTree(Tree).HeaderSections.ItemByPos[FCurSectionIdx];
  end
  else
  if (Tag = '/SECTION') then
  begin
    if GetLoopIndex('SECTIONS') = -1 then
      raise Exception.Create('<SECTION> tag must be used inside of SECTIONS loop');
  
    FLastSectionIdx := FCurSectionIdx;
    FCurSectionIdx  := -1;
    FCurSection     := nil;
  end
  else
  if (Tag = 'ITEM') then
  begin
    FCurSectionIdx := -1;
    FLastSectionIdx := -1;
  
    if GetLoopIndex('ITEMS') = -1 then
      raise Exception.Create('<ITEM> tag must be used inside of ITEMS loop');
  
    FCurItemIdx := FLastItemIdx + 1;
    repeat
      if FCurItemIdx >= THackElTree(Tree).Items.Count then
      begin
        FCurItem := nil;
        LoopBreak('ITEMS');
        break;
      end;
      FCurItem := THackElTree(Tree).Items[FCurItemIdx];
      if ((not GenerateHiddenItems) and FCurItem.Hidden and THackElTree(Tree).FilteredVisibility) or
         ((not GenerateInvisibleItems) and not FCurItem.FullyExpanded) then
        Continue;
      Generate := true;
      TriggerItemGenerateEvent(FCurItem, Generate);
      if Generate then
      begin
        Inc(FRelItemIdx);
        break;
      end;
    until false;
  end
  else
  if (Tag = '/ITEM') then
  begin
    if GetLoopIndex('ITEMS') = -1 then
      raise Exception.Create('</ITEM> tag must be used inside of ITEMS loop');
  
    TriggerItemGeneratedEvent(FCurItem);
    FLastItemIdx := FCurItemIdx;
    FCurItemIdx  := -1;
    FCurItem := nil;
    if (ItemsPerPage <> 0) and (LoopCountersCurrentPageStr['ITEMS'] >= ItemsPerPage) then
      NextPage;
    if FLastItemIdx = THackElTree(Tree).Items.Count then
      LoopBreak('ITEMS');
  end
  else
  if (Tag = 'CELL') then
  begin
    FCurSectionIdx := FLastSectionIdx + 1;
    if FCurSectionIdx >= THackElTree(Tree).HeaderSections.Count then
    begin
      FCurSection := nil;
      LoopBreak('CELLS');
    end
    else
      FCurSection := THackElTree(Tree).HeaderSections.ItemByPos[FCurSectionIdx];
  end
  else
  if (Tag = '/CELL') then
  begin
    FLastSectionIdx := FCurSectionIdx;
    FCurSectionIdx  := -1;
    FCurSection     := nil;
  end
  else
    inherited;
end;{TElTreeMLGenerator.TagFound cat:Undefined}

procedure TElTreeMLGenerator.TriggerImageEvent(ImageList : TImageList; 
        ImageIndex : integer; var ReplacementString : string);
begin
  if assigned(FOnImage) then
    FOnImage(Self, ImageList, ImageIndex, ReplacementString);
end;{TElTreeMLGenerator.TriggerImageEvent cat:Undefined}

procedure TElTreeMLGenerator.TriggerItemGeneratedEvent(Item : TElTreeItem);
begin
  if assigned(FOnItemGenerated) then
    FOnItemGenerated(Self, Item);
end;{TElTreeMLGenerator.TriggerItemGeneratedEvent cat:Undefined}

procedure TElTreeMLGenerator.TriggerItemGenerateEvent(Item : TElTreeItem; var 
        Generate : boolean);
begin
  if assigned(FOnItemGenerate) then
    FOnItemGenerate(Self, Item, Generate);
end;{TElTreeMLGenerator.TriggerItemGenerateEvent cat:Undefined}

procedure TElTreeMLGenerator.TriggerOwnerDrawEvent(Item : TElTreeItem; 
        SectionIndex : integer; MaxRect : TRect; var ReplacementString : 
        string);
begin
  ReplacementString := '';
  if assigned(FOnOwnerDraw) then
    FOnOwnerDraw(Self, Item, SectionIndex, MaxRect, ReplacementString);
end;{TElTreeMLGenerator.TriggerOwnerDrawEvent cat:Undefined}

procedure TElTreeMLGenerator.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FTree then
      Tree := nil;
  end;
end;

procedure Register;
begin
  RegisterComponents('EldoS', [TElTreeMLGenerator]);
end;

end.

