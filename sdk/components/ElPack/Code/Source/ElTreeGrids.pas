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

07/23/2002

  Fixed focus change using Tab key 

07/09/2002

  ExplorerEditMode property was ignored when edit operation was finished by pressing arrows/tab. Fixed
  Cell controls didn't receive clicks on left mouse button. Fixed. 

06/26/2002

  Pressing the keys which are the hotkeys for the form in default editor caused activation of the hot-key control. Fixed.

06/20/2002

  OnAfterSelectionChange was not fired when moving the focus up/down with
  keyboard or when clicking cell with mouse. Fixed.

05/13/2002

  Fixed the problem with ignoring ExplorerEditMode when initiating edit
  operation in the item and the pending edit operation is already active

05/06/2002

  It is now possible to set RowCount to 0. 

04/30/2002

  Fixed inplace editor invokation in RowSelect mode

04/01/2002

  Inplace editing was triggered by any button (instead of left button only). fixed.

03/12/2002

  Built-in editor always has the same font as a grid itself
  AV appeared whn trying to read Rows array. Fixed.

02/15/2002

  Fixed Tab key handling

02/01/2002

  Changed Cols, Rows types to conditionally unicode

11/05/2001

  goRowSelect was not working properly. Fixed. 

*)

unit ElTreeGrids;

interface

uses

    Windows,
    Messages,
    Controls,
    Forms,
    StdCtrls,

    Classes,
{$ifdef VCL_6_USED}
Types,
{$endif}

    ElStrArray,
    ElTree,
    ElHeader,
    ElStrUtils,
    {$ifdef ELPACK_UNICODE}
    ElUnicodeStrings,
    {$endif}
    {$ifdef ELPACK_COMPLETE}
    ElTreeBtnEdit;
    {$else}
    ElTreeStdEditors;
    {$endif}

type

{$ifdef ELPACK_UNICODE}
  TElFStringArray = TElWideStringArray;
{$else}
  TElFStringArray = TElStringArray;
{$endif}

     TCustomElTreeGrid = class(TCustomElTree)
     private
       FgoAlwaysShowEditor: Boolean;
       FgoRowSelect: Boolean;
       FgoColMoving: Boolean;
       FgoTabs: Boolean;

       procedure SetgoRowSelect(Value: Boolean);
       procedure SetgoColMoving(Value: Boolean);
       function GetCol: Integer;
       procedure SetCol(Value: Integer);
       function GetRow: Integer;
       procedure SetRow(Value: Integer);
       function GetLeftCol: Integer;
       procedure SetLeftCol(Value: Integer);
       function GetColCount: Integer;
       procedure SetColCount(Value: Integer);
       function GetColWidths(Index: Integer): Integer;
       procedure SetColWidths(Index: Integer; Value: Integer);
       function GetDefaultColWidth: Integer;
       procedure SetDefaultColWidth(Value: Integer);
       function GetDefaultRowHeight: Integer;
       procedure SetDefaultRowHeight(Value: Integer);
       function GetEditorMode: Boolean;
       procedure SetEditorMode(Value: Boolean);
       function GetRowCount: Integer;
       procedure SetRowCount(Value: Integer);
       function GetTopRow: Integer;
       procedure SetTopRow(Value: Integer);
       function GetgoEditing: Boolean;
       procedure SetgoEditing(Value: Boolean);
     protected
       FgoTabSkipNonEditable: Boolean;
       procedure KeyDownTransfer(Sender : TObject; var Key : Word; Shift :
           TShiftState); override;
       procedure WMChar(var Message: TMessage); message WM_CHAR;
       function CreateView: TElTreeView; override;
       function GetAsCell(ACol, ARow : Integer): TElTreeItem;
       procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;

       procedure MouseDownTransfer(Sender : TObject; Button : TMouseButton; Shift :
           TShiftState; X : Integer; Y : Integer); override;
       procedure EnsureColumnVisible(SectionNumber : integer);

       property goAlwaysShowEditor: Boolean read FgoAlwaysShowEditor write
           FgoAlwaysShowEditor;
       property goRowSelect: Boolean read FgoRowSelect write SetgoRowSelect;
       property goColMoving: Boolean read FgoColMoving write SetgoColMoving default true;
       property goTabs: Boolean read FgoTabs write FgoTabs;
       property ColCount: Integer read GetColCount write SetColCount default 5;
       property DefaultColWidth: Integer read GetDefaultColWidth write
           SetDefaultColWidth default 64;
       property DefaultRowHeight: Integer read GetDefaultRowHeight write
           SetDefaultRowHeight default 24;
       property EditorMode: Boolean read GetEditorMode write SetEditorMode;
       property RowCount: Integer read GetRowCount write SetRowCount default 5;
       property goEditing: Boolean read GetgoEditing write SetgoEditing default true;
       property goTabSkipNonEditable: Boolean read FgoTabSkipNonEditable write
           FgoTabSkipNonEditable;
     public
       function CellRect(ACol, ARow: Longint): TRect;
       procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
       constructor Create(Owner : TComponent); override;
       function GetNextEditableSection(Section : TElHeaderSection; GoForward :
           boolean): TElHeaderSection;
       property Col: Integer read GetCol write SetCol;
       property ColWidths[Index: Integer]: Integer read GetColWidths write
           SetColWidths;
       property LeftCol: Integer read GetLeftCol write SetLeftCol;
       property Row: Integer read GetRow write SetRow;
       property TopRow: Integer read GetTopRow write SetTopRow;
     published
       property VerticalLines default true;
       property HorizontalLines default true;
     end;

     {$ifdef ELPACK_COMPLETE}
     TElTreeTrickyInplaceEdit = class(TElTreeInplaceButtonEdit)
     {$else}
     TElTreeTrickyInplaceEdit = class(TElTreeInplaceEdit)
     {$endif}
     private
       FDummyStr : string;
     published
       property Name : string read FDummyStr;
       property Tree : string read FDummyStr;
     end;

     TElTreeStringGrid = class(TCustomElTreeGrid)
     private
       FCols : TElFStringArray;
       FRows: TElFStringArray;
       {$ifdef ELPACK_COMPLETE}
       FEditor : TElTreeInplaceButtonEdit;
       {$else}
       FEditor : TElTreeInplaceEdit;
       {$endif}
       FSavedEditWndProc: TWndMethod;
       function GetCols(Index: Integer): TElFStrings;
       procedure SetCols(Index: Integer; Value: TElFStrings);
       function GetRows(Index: Integer): TElFStrings;
       procedure SetRows(Index: Integer; Value: TElFStrings);
       function GetCells(ACol, ARow: Integer): TElFString;
       procedure SetCells(ACol, ARow: Integer; Value: TElFString);
       function GetObjects(ACol, ARow: Integer): TObject;
       procedure SetObjects(ACol, ARow: Integer; Value: TObject);
       {$ifdef ELPACK_COMPLETE}
       procedure SetEditor(Value : TElTreeInplaceButtonEdit);
       {$else}
       procedure SetEditor(Value : TElTreeInplaceEdit);
       {$endif}
     protected
       FUseDefaultEditor: Boolean;
       procedure TriggerInplaceEditorNeeded(Item : TElTreeItem; SectionIndex : Integer;
        SupposedFieldType : TElFieldType; var Editor : TElTreeInplaceEditor); override;
       procedure OnFontChange(Sender: TObject); override;

       procedure KeyDownTransfer(Sender : TObject; var Key : Word; Shift :
           TShiftState); override;
       procedure EditorKeyDown(Sender  : TObject; var Key : Word; Shift : TShiftState);
       procedure EditWndProc(var Message: TMessage);
     public
       constructor Create(Owner : TComponent); override;
       destructor Destroy; override;
       property Cols[Index: Integer]: TElFStrings read GetCols write SetCols;
       property Rows[Index: Integer]: TElFStrings read GetRows write SetRows;
       property Cells[ACol, ARow: Integer]: TElFString read GetCells write SetCells;
           default;
       property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;

       {$ifdef ELPACK_COMPLETE}
       property Editor : TElTreeInplaceButtonEdit read FEditor
                                                  write SetEditor;
       {$else}
       property Editor : TElTreeInplaceEdit read FEditor
                                            write SetEditor;
       {$endif}
     published
       property UseDefaultEditor: Boolean read FUseDefaultEditor write 
           FUseDefaultEditor default false;

       property ColCount default 5;
       property RowCount default 5;
       property goAlwaysShowEditor  default false;
       property goRowSelect default false;
       property goColMoving default true;
       property goEditing default true;
       property goTabs default false;
       property goTabSkipNonEditable default false;
       property DefaultColWidth default 64;
       property DefaultRowHeight default 24;

       property ActiveBorderType;
       property Align;
       property AutoLookup;
       property AutoResizeColumns;
   {$IFDEF VCL_4_USED}
       property Anchors;
       property Action;
       property Constraints;
       property DockOrientation;
       property Floating;
       property BevelKind;
       property DoubleBuffered;
       property DragKind;
   {$ENDIF}
   {$IFNDEF LITE}
       property AdjustMultilineHeight;
       property Background;
       property BackgroundType;
   {$ENDIF}
       property BorderStyle;
       property ChangeDelay;
       property ChangeStateImage;
       property CheckBoxGlyph;
       property Ctl3D;
       property Color;
       property Cursor;
       property CustomCheckboxes;
       property CustomPlusMinus;
       property VertDivLinesColor;
       property HorzDivLinesColor;
       property DragCursor;
       property DragAllowed;
       property DragTrgDrawMode;
       property DragType;
       property DragExpandDelay;
       property DragImageMode;
       property DrawFocusRect;
       property DragRectAcceptColor;
       property DragRectDenyColor;

       property Enabled;
       property ExpandOnDragOver;
       property ExplorerEditMode;
       property FilteredVisibility;
       property Flat;
       property FlatFocusedScrollbars;
       property FocusedSelectColor;
       property FocusedSelectTextColor;
       property ForcedScrollBars;
       property Font stored true;
   {$IFNDEF LITE}
       property GradientStartColor;
       property GradientEndColor;
       property GradientSteps;
   {$ENDIF LITE}
       property HeaderActiveFilterColor;
       property HeaderColor;
       property HeaderHeight default 0;
       property HeaderHotTrack;
       property HeaderInvertSortArrows;
       property HeaderSections;
       property HeaderFilterColor;
       property HeaderFlat;
       property HeaderImages;
   {$IFNDEF LITE}
       property HeaderWrapCaptions;
   {$ENDIF}
       property HideFocusRect;
       property HideHintOnTimer;
       property HideHintOnMove;
       property HideSelectColor;
       property HideSelectTextColor;
       property HideSelection;
       property HorizontalLines default true;
       property HideHorzScrollBar;
       property HideVertScrollBar;
       property Hint;
       property HorzScrollBarStyles;
   {$IFDEF ELPACK_COMPLETE}
       property HeaderImageForm;
       property ImageForm;
   {$ENDIF}
       property Images;
       property Images2;
       property InactiveBorderType;
       property InplaceEditorDelay;
       property ItemIndent;
       property Items;
       property LineBorderActiveColor;
       property LineBorderInactiveColor;
       property LinesColor;
       property LinesStyle;
       property LineHintColor;
       property LineHintMode default shmLong;
       property LineHintTimeout;
       property LockHeaderHeight default true;
       property MainTreeColumn;
       property MinusPicture;
       property MoveFocusOnCollapse;
       property ParentCtl3D;
       property ParentFont;
       property ParentShowHint;
       property PlusMinusTransparent;
       property PlusPicture;
       property PopupMenu;
       property RadioButtonGlyph;
       property RightAlignedText;
       property RightAlignedTree;
       property RightClickSelect;
       property ScrollbarOpposite;
       property ScrollTracking;
       property ShowButtons default false;
       property ShowCheckboxes;
       property ShowEmptyImages;
       property ShowEmptyImages2;
       property ShowHint;
       property ShowImages default false;
       property ShowLines default false;
       property ShowRoot default false;
       property ShowRootButtons;
       property SortDir;
       property SortMode;
       property SortSection;
       property SortType;
   {$IFDEF ELPACK_COMPLETE}
       property Storage;
       property StoragePath;
   {$ENDIF}
       property StickyHeaderSections;
       property TabOrder;
       property TabStop;
       property UseCustomScrollBars;

       property VerticalLines default true;
       property VerticalLinesLong default false;
       property VertScrollBarStyles;
       property Visible;
       property UseSystemHintColors;
       property TextColor;
       property BkColor;

       property OnScroll;
       property OnHeaderColumnClick;
       property OnHeaderColumnDraw;
       property OnHeaderColumnResize;
       property OnHeaderColumnMove;
       property OnHeaderLookup;
       property OnHeaderLookupDone;
       property OnHeaderResize;
       property OnHeaderSectionExpand;
       property OnHeaderSectionCollapse;
       property OnHeaderSectionFilterCall;
       property OnHeaderSectionAutoSize;
       property OnHeaderSectionMeasure;
       property OnHorzScrollDrawPart;
       property OnHorzScrollHintNeeded;

       property OnAfterSelectionChange;
       property OnChanging;
       property OnDragTargetChange;
       property OnItemChange;
       property OnItemPreDraw;
       property OnItemDraw;
       property OnResize;
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
       property OnTryEdit;
       property OnEditRequest;
       property OnComboEditShow;
       property OnValidateCombo;
       property OnValidateInplaceEdit;
       property OnTuneUpInplaceEdit;
       property OnEditKeyDown;
{$endif}
{$else}
       property OnInplaceEditorNeeded;
{$endif}
       property OnItemChecked;
       property OnItemExpand;
       property OnItemCollapse;
       property OnItemExpanding;
       property OnItemCollapsing;
       property OnItemDeletion;
       property OnItemFocused;
       property OnShowLineHint;
       property OnCompareItems;
       property OnItemPicDraw;
       property OnItemPicDraw2;
       property OnItemPostDraw;
       property OnHotTrack;
       property OnSortBegin;
       property OnSortEnd;
       property OnItemSave;
       property OnItemLoad;
       property OnItemSelectedChange;
       property OnCellStyleSave;
       property OnCellStyleLoad;
       property OnVertScrollDrawPart;
       property OnVertScrollHintNeeded;
   {$IFDEF HAS_HTML_RENDER}
       property OnHTMLImageNeeded;
   {$ENDIF}

       property OnHeaderMouseDown;
       property OnClick;
       property OnEnter;
       property OnExit;
       property OnDragDrop;
       property OnDragOver;
       property OnStartDrag;
       property OnEndDrag;
       property OnMouseDown;
       property OnMouseMove;
       property OnMouseUp;
       property OnDblClick;
       property OnKeyDown;
       property OnKeyPress;
       property OnKeyUp;
   {$IFDEF VCL_4_USED}
       property OnStartDock;

       property OnEndDock;
   {$ENDIF}
   {$IFDEF VCL_5_USED}
       property OnContextPopup;
   {$ENDIF}
   {$ifdef ELTREE_USE_OLE_DRAGDROP}
   {$IFNDEF VER90}
   {$IFNDEF LITE}
       property OnOleTargetDrag;
       property OnOleTargetDrop;
       property OnOleDragStart;
       property OnOleDragFinish;
   {$ENDIF}
   {$ENDIF}
   {$endif}
   end;

     TElTreeGridView = class(TElTreeView)
     protected
       procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
       procedure WMLButtonDblClick(var Message: TMessage); message WM_LBUTTONDBLCLK;
       procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
     end;

     EElTreeGridError = class(EElTreeError);

implementation

procedure TCustomElTreeGrid.KeyDownTransfer(Sender : TObject; var Key : Word;
    Shift : TShiftState);
var i, j : integer;
    Item : TElTreeItem;
    b : boolean;
    Section : TElHeaderSection;
begin
  inherited;
  if goTabs and (Key = VK_TAB) and (not goRowSelect) and (ItemFocused <> nil) then
  begin
    if Shift = [] then
    begin
      if FGoAlwaysShowEditor and (not fGoRowSelect) and IsEditing then
      begin
        Key := 0;
        EndEdit(not ExplorerEditMode);
      end;
      Section := FHeader.Sections[SelectColumn];
      j := Section.Position;

      if j = FHeader.Sections.Count - 1 then
      begin
        i := ItemFocused.AbsoluteIndex;

        if i = Integer(Items.Count) - 1 then
          i := -1;
        ItemFocused := Items[i + 1];
        EnsureVisibleBottom(ItemFocused);

        Section := FHeader.Sections.ItemByPos[0];

        if goEditing and goTabSkipNonEditable then
        begin
          Section := GetNextEditableSection(Section, false);
        end;

        //if ((Section.Right - LeftPosition) > ClientWidth) or (Section.Left < LeftPosition) then
        //  LeftPosition := Section.Left;
        SelectColumn := Section.Index;
        EnsureColumnVisible(SelectColumn);
      end
      else
      begin
        Section := FHeader.Sections.ItemByPos[j + 1];
        if goEditing and goTabSkipNonEditable then
        begin
          Section := GetNextEditableSection(Section, false);
        end;
        SelectColumn := Section.Index;
        //if ((Section.Right - LeftPosition) > ClientWidth) or (Section.Left < LeftPosition) then
        //  LeftPosition := Section.Left;
        EnsureColumnVisible(SelectColumn);
      end;
      DoItemFocused;
      DoAfterSelectionChange;

      if FGoAlwaysShowEditor and (not fGoRowSelect) and HeaderSections[SelectColumn].Editable then
         EditItem(ItemFocused, SelectColumn);

      Key := 0;
    end
    else
    if Shift = [ssShift] then
    begin
      if FGoAlwaysShowEditor and (not fGoRowSelect) and IsEditing then
        EndEdit(not ExplorerEditMode);

      Section := FHeader.Sections[SelectColumn];
      j := Section.Position;

      if j = 0 then
      begin
        i := ItemFocused.AbsoluteIndex;

        if i = 0 then
          i := Items.Count;
        ItemFocused := Items[i - 1];
        EnsureVisible(ItemFocused);

        Section := FHeader.Sections.ItemByPos[FHeader.Sections.Count - 1];

        if goEditing and goTabSkipNonEditable then
          Section := GetNextEditableSection(Section, true);

        //if ((Section.Right - LeftPosition) > ClientWidth) or (Section.Left < LeftPosition) then
        //  LeftPosition := Section.Left;
        SelectColumn := Section.Index;
        EnsureColumnVisible(SelectColumn);
      end
      else
      begin
        Section := FHeader.Sections.ItemByPos[j - 1];
        if goEditing and goTabSkipNonEditable then
          Section := GetNextEditableSection(Section, true);
      //if ((Section.Right - LeftPosition) > ClientWidth) or (Section.Left < LeftPosition) then
        //  LeftPosition := Section.Left;
        SelectColumn := Section.Index;
        EnsureColumnVisible(SelectColumn);
      end;
      DoItemFocused;
      DoAfterSelectionChange;

      if FGoAlwaysShowEditor and (not fGoRowSelect) and HeaderSections[SelectColumn].Editable then
        EditItem(ItemFocused, SelectColumn);

      Key := 0;
    end;
  end;
  if (Shift = []) then
  begin
    if (ItemFocused <> nil) then
    begin
      if (Key = VK_PRIOR) or (Key = VK_NEXT) or
         (Key = VK_HOME) or (Key = VK_END) or
         (Key = VK_UP) or (Key = VK_DOWN) then
      begin
        if FGoAlwaysShowEditor and (not fGoRowSelect) and IsEditing then
           EndEdit(not ExplorerEditMode);
        Item := TElTreeGridView(FView).FindNewFocused(Key, @j, b);
        if Item <> nil then
        begin
          ItemFocused := Item;
          if (Key = VK_NEXT) or (Key = VK_END) or (Key = VK_DOWN) then
            EnsureVisibleBottom(ItemFocused)
          else
            EnsureVisible(ItemFocused);
        end;
        DoAfterSelectionChange;
        
        if FGoAlwaysShowEditor and (not fGoRowSelect) and HeaderSections[SelectColumn].Editable then
          EditItem(ItemFocused, SelectColumn);
        Key := 0;
      end
      else
      if Key = VK_LEFT then
      begin
        if FGoAlwaysShowEditor and (not fGoRowSelect) and IsEditing then
           EndEdit(not ExplorerEditMode);

        Section := FHeader.Sections[SelectColumn];
        j := Section.Position;

        if j > 0 then
        begin
          Section := FHeader.Sections.ItemByPos[j - 1];
          if goEditing and goTabSkipNonEditable then
            Section := GetNextEditableSection(Section, true);
          SelectColumn := Section.Index;

          DoItemFocused;
          DoAfterSelectionChange;
          EnsureColumnVisible(SelectColumn);
          if FGoAlwaysShowEditor and (not fGoRowSelect) and HeaderSections[SelectColumn].Editable then
           EditItem(ItemFocused, SelectColumn);
        end;
        Key := 0;
      end
      else
      if Key = VK_RIGHT then
      begin
        Section := FHeader.Sections[SelectColumn];
        j := Section.Position;

        if j < FHeader.Sections.Count - 1 then
        begin
          Section := FHeader.Sections.ItemByPos[j + 1];
          if goEditing and goTabSkipNonEditable then
            Section := GetNextEditableSection(Section, true);
          SelectColumn := Section.Index;

          DoItemFocused;
          DoAfterSelectionChange;
          EnsureColumnVisible(SelectColumn);
          if FGoAlwaysShowEditor and (not fGoRowSelect) and HeaderSections[SelectColumn].Editable then
            EditItem(ItemFocused, SelectColumn);
        end;
        Key := 0;
      end
    end;
    if Key = VK_INSERT then
    begin
      if goEditing then
      begin
        Item := Items.AddItem(nil);
        EnsureVisibleBottom(Item);
        if ItemFocused = nil then
        begin
          ItemFocused := Item;
          DoAfterSelectionChange;
          if FGoAlwaysShowEditor and (not fGoRowSelect) and HeaderSections[SelectColumn].Editable then
             EditItem(Item, SelectColumn);
        end;
      end;
      Key := 0;
    end
    else
    if Key = VK_DELETE then
    begin
      if goEditing then
        Items.DeleteItem(ItemFocused);
      DoAfterSelectionChange;
      Key := 0;
    end;
  end;
end;

procedure TCustomElTreeGrid.WMChar(var Message: TMessage);
begin
  if (not FGoAlwaysShowEditor) and (not fGoRowSelect) then
  begin
    if (ItemFocused <> nil)  and HeaderSections[SelectColumn].Editable then
      EditItem(ItemFocused, SelectColumn);
  end;
  inherited;
end;

procedure TCustomElTreeGrid.SetgoRowSelect(Value: Boolean);
begin
  if FgoRowSelect <> Value then
  begin
    FgoRowSelect := Value;
    RowSelect := Value;
    if Value then
      SelectColumn := -1
    else
      SelectColumn := 0;
  end;
end;

procedure TCustomElTreeGrid.SetgoColMoving(Value: Boolean);
begin
  if FgoColMoving <> Value then
  begin
    FgoColMoving := Value;
    DraggableSections := Value;
  end;
end;

function TCustomElTreeGrid.CreateView: TElTreeView;
begin
  result := TElTreeGridView.Create(Self);
end;  { CreateView }

function TCustomElTreeGrid.CellRect(ACol, ARow: Longint): TRect;
var Item : TElTreeItem;
begin
  Item := GetAsCell(ACol, ARow);
  if Item <> nil then
  begin
    Result := GetItemRect(ARow);
    Result.Left := FHeader.Sections[ACol].Left;
    Result.Right := Result.Left + FHeader.Sections[ACol].Width;
    OffsetRect(Result, -FHeader.Left, 0);
  end
  else
  begin
    SetRectEmpty(Result);
  end;
end;

function TCustomElTreeGrid.GetAsCell(ACol, ARow : Integer): TElTreeItem;
begin
  if (Integer(Items.Count) > ARow) and (FHeader.Sections.Count > ACol) then
    Result := Items[ARow]
  else
    Result := nil;
end;

procedure TCustomElTreeGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var Item : TElTreeItem;
    hc   : integer;
    ip   : TSTItemPart;
begin
  Item := GetItemAt(X, Y, ip, hc);
  if ip = ipOutside then
  begin
    ACol := -1;
    ARow := -1;
  end
  else
  begin
    ARow := Item.AbsoluteIndex;
    ACol := hc;
  end;
end;

function TCustomElTreeGrid.GetCol: Integer;
begin
  result := SelectColumn;
end;

procedure TCustomElTreeGrid.SetCol(Value: Integer);
begin
  if (Value <> -1) or (not FgoRowSelect) then
    SelectColumn := Value
  else
    SelectColumn := -1;
end;

function TCustomElTreeGrid.GetRow: Integer;
begin
  if ItemFocused <> nil then
    Result := ItemFocused.AbsoluteIndex
  else
    Result := -1;
end;

procedure TCustomElTreeGrid.SetRow(Value: Integer);
begin
  if (Value >= 0) and (Value < Integer(Items.Count)) then
    ItemFocused := Items[Value]
  else
    raise EElTreeGridError.Create('Row index out of bounds');
end;

function TCustomElTreeGrid.GetLeftCol: Integer;
var HS : TElHeaderSection;
begin
  if Left = 0 then
    Result := 0
  else
  begin
    HS := FHeader.GetSectionAt(Left, 0);
    if HS <> nil then
      result := HS.Index
    else
      result := -1;
  end;
end;

procedure TCustomElTreeGrid.SetLeftCol(Value: Integer);
begin
  if Value < FHeader.Sections.Count then
    Left := FHeader.Sections[Value].Left;
end;

function TCustomElTreeGrid.GetColCount: Integer;
begin
  Result := FHeader.Sections.Count;
end;

procedure TCustomElTreeGrid.SetColCount(Value: Integer);
begin
  if Value < 1 then exit;
  IsUpdating := true;
  try
    if Value < FHeader.Sections.Count then
      with FHeader.Sections do
        while Count > Value do
          DeleteSection(Item[Count - 1]);
    if Value > FHeader.Sections.Count then
    begin
      with FHeader.Sections do
        while Count < Value do
          AddSection.Editable := goEditing;
    end;
  finally
    IsUpdating := false;
  end;
end;

function TCustomElTreeGrid.GetColWidths(Index: Integer): Integer;
begin
  if Index >= FHeader.Sections.Count then
    raise EElTreeGridError.Create('Column index out of bounds');
  Result := FHeader.Sections[Index].Width;
end;

procedure TCustomElTreeGrid.SetColWidths(Index: Integer; Value: Integer);
begin
  if Index >= FHeader.Sections.Count then
    raise EElTreeGridError.Create('Column index out of bounds');
  FHeader.Sections[Index].Width := Value;
end;

function TCustomElTreeGrid.GetDefaultColWidth: Integer;
begin
  Result := FHeader.DefaultWidth;
end;

procedure TCustomElTreeGrid.SetDefaultColWidth(Value: Integer);
begin
  FHeader.DefaultWidth := Value;
end;

function TCustomElTreeGrid.GetDefaultRowHeight: Integer;
begin
  if AutoLineHeight then
    Result := -1
  else
    Result := LineHeight;
end;

procedure TCustomElTreeGrid.SetDefaultRowHeight(Value: Integer);
begin
  if Value = -1 then
    AutoLineHeight := true
  else
  begin
    AutoLineHeight := false;
    LineHeight := Value;
  end;
end;

function TCustomElTreeGrid.GetEditorMode: Boolean;
begin
  Result := IsEditing;
end;

procedure TCustomElTreeGrid.SetEditorMode(Value: Boolean);
begin
  if Value <> EditorMode then
  begin
    if Value then
    begin
      if (ItemFocused <> nil) {and (not goRowSelect) }and HeaderSections[SelectColumn].Editable then
        EditItem(ItemFocused, SelectColumn);
    end
    else
      EndEdit(true);
  end;
end;

function TCustomElTreeGrid.GetRowCount: Integer;
begin
  Result := Items.Count;
end;

procedure TCustomElTreeGrid.SetRowCount(Value: Integer);
begin
  if Value < 0 then exit;
  IsUpdating := true;
  try
  while Value < Integer(Items.Count) do
    with Items do
      DeleteItem(FAllList.Last);

  if Value > Integer(Items.Count) then
  begin
    Items.AllocateStorage(Value);
    while Value > Integer(Items.Count) do
      Items.AddLastItem(nil);
  end;
  finally
    IsUpdating := false;
  end;

end;

function TCustomElTreeGrid.GetTopRow: Integer;
begin
  Result := TopIndex;
end;

procedure TCustomElTreeGrid.SetTopRow(Value: Integer);
begin
  TopIndex := Value;
end;

constructor TCustomElTreeGrid.Create(Owner : TComponent);
begin
  inherited;
  DefaultColWidth := 64;
  DefaultRowHeight := 24;
  ColCount := 5;
  RowCount := 5;
  ShowColumns := true;
  LockHeaderHeight := true;
  HeaderHeight := 0;
  MultiSelect := false;
  RowSelect := false;
  SelectColumn := 0;
  Tracking := false;
  ShowRoot := false;
  ShowButtons := false;
  ShowLines := false;
  ShowImages := false;
  goEditing := true;
  goColMoving := true;
  VerticalLines := true;
  HorizontalLines := true;
  AlwaysKeepSelection := true;
  AlwaysKeepFocus := true;
  VerticalLinesLong := false;
end;

function TCustomElTreeGrid.GetgoEditing: Boolean;
begin
  Result := DoInplaceEdit;
end;

procedure TCustomElTreeGrid.SetgoEditing(Value: Boolean);
var i : integer;
begin
  DoInplaceEdit := Value;
  for i := 0 to FHeader.Sections.Count - 1 do
    FHeader.Sections[i].Editable := Value;
end;

procedure TCustomElTreeGrid.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  if goTabs then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TCustomElTreeGrid.MouseDownTransfer(Sender : TObject; Button :
    TMouseButton; Shift : TShiftState; X : Integer; Y : Integer);
var ARow, ACol : integer;
begin
  Y := Y ;
  X := X + FView.Left;
  MouseToCell(X+ FView.Left, Y+ FView.Top, ACol, ARow);
  if (ARow <> -1) and (ACol <> -1) and (Button = mbLeft) then
  begin
    if FGoAlwaysShowEditor {and (not fGoRowSelect)} and IsEditing then
      EndEdit(not ExplorerEditMode);
    if (ARow = Row) and (ACol = Col) and
       (not FGoAlwaysShowEditor) {and (not fGoRowSelect) } then
      Self.EditorMode := true
    else
    begin
      Row := ARow;
      Col := ACol;
      DoAfterSelectionChange;
      if FGoAlwaysShowEditor and {(not fGoRowSelect) and} HeaderSections[SelectColumn].Editable then
        EditItem(ItemFocused, SelectColumn);
    end;
  end;
  inherited;
end;  { MouseDownTransfer }

procedure TCustomElTreeGrid.EnsureColumnVisible(SectionNumber : integer);
var Section : TElHeaderSection;
begin
  if (SectionNumber >= 0) and (SectionNumber < FHeader.Sections.Count) then
  begin
    Section := FHeader.Sections[SectionNumber];
    if LeftPosition + FView.Width < Section.Right then
    begin
      LeftPosition := Section.Right - FView.Width;
    end
    else
    if LeftPosition > Section.Left then
    begin
      LeftPosition := Section.Left;
    end;
  end;
end;

function TCustomElTreeGrid.GetNextEditableSection(Section : TElHeaderSection;
    GoForward : boolean): TElHeaderSection;
var i : integer;
begin
  result := Section;
  if not Result.Editable then
    repeat
      if GoForward then
      begin
        i := Section.Position + 1;
        if i >= HeaderSections.Count then
          i := 0;
      end
      else
      begin
        i := Section.Position - 1;
        if i = -1 then
          i := HeaderSections.Count - 1;
      end;
      result := FHeader.Sections.ItemByPos[i];
    until (result = Section) or (result.Editable);
end;

procedure TElTreeGridView.WMLButtonDown(var Message: TWMLButtonDown);
var
  ItemPart: TSTItemPart;
  HCol    : Integer;
  XPos,
  YPos    : integer;
  Item    : TElTreeItem; 
begin
  DoHideLineHint;
  FIgnoreClick2 := false;
  if (not (csDesigning in ComponentState)) and (not Focused) and CanFocus then
    SetFocus;
  XPos := Message.XPos;
  YPos := Message.YPos;
  FPressCoord := Point(XPos, YPos);
  FPressed := true;
  Item := GetItemAt(XPos, YPos, ItemPart, HCol);
  if ItemPart <> ipOutside then
  begin
    if (ItemPart in [ipButton, ipPicture, ipPicture2]) or ((ItemPart = ipColumn) and IsControlCell(Item, HCol)) then
      inherited
    else
    begin
      TCustomElTreeGrid(FOwner).MouseDownTransfer(Self, mbLeft, [], XPos, YPos);
    end;
  end;
end;

procedure TElTreeGridView.WMLButtonDblClick(var Message: TMessage);
begin
  inherited;
end;

procedure TElTreeGridView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  if (Owner as TCustomElTreeGrid).goTabs then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

function TElTreeStringGrid.GetCols(Index: Integer): TElFStrings;
var i    : integer;
    Item : TElTreeItem;
    hs   : integer;
begin
  if FCols = nil then
    FCols := TElFStringArray.Create;
  Result := FCols;
  // filling the cols
  FCols.Clear;
  hs := Index;
  if Index > MainTreeColumn then
    dec(hs);
  for i := 0 to Items.Count - 1 do
  begin
    Item := Items[i];
    if hs = MainTreeColumn then
      FCols.AddObject(Item.Text, Item.AnObject)
    else
    begin
      if Item.ColumnText.count <= hs then
        FCols.AddObject('', nil)
      else
        FCols.AddObject(Item.ColumnText[hs], Item.ColumnText.Objects[hs]);
    end;
  end;
end;

procedure TElTreeStringGrid.SetCols(Index: Integer; Value: TElFStrings);
var i : integer;
begin
  IsUpdating := true;
  try
    while Index >= FHeader.Sections.Count do
      FHeader.Sections.AddSection.Editable := goEditing;

    Items.AllocateStorage(Value.Count);
    while Value.Count > Integer(Items.Count) do
      Items.AddLastItem(nil);
    for i := 0 to Value.Count - 1 do
    begin
      Cells[Index, i] := Value[i];
      Objects[Index, i] := Value.Objects[i];
    end;
  finally
    IsUpdating := false;
  end;
end;

function TElTreeStringGrid.GetRows(Index: Integer): TElFStrings;
var i    : integer;
    Item : TElTreeItem;
begin
  if FRows = nil then
    FRows := TElFStringArray.Create;
  Result := FRows;
  // filling the rows
  FRows.Clear;
  if Index > Integer(Items.Count) - 1 then
    exit;
  Item := Items[Index];
  
  for i := 0 to FHeader.Sections.Count - 1 do
  begin
    if i < MainTreeColumn then
    begin
      if Item.ColumnText.count <= i then
        FRows.AddObject('', nil)
      else
        FRows.AddObject(Item.ColumnText[i], Item.ColumnText.Objects[i]);
    end
    else
    if i = MainTreeColumn then
      FRows.AddObject(Item.Text, Item.AnObject)
    else
    begin
      if Item.ColumnText.count <= i - 1 then
        FRows.AddObject('', nil)
      else
        FRows.AddObject(Item.ColumnText[i - 1], Item.ColumnText.Objects[i - 1]);
    end;
  end;
end;

procedure TElTreeStringGrid.SetRows(Index: Integer; Value: TElFStrings);
var i : integer;
begin
  IsUpdating := true;
  try
    Items.AllocateStorage(Index + 1);
    while Index >= Integer(Items.Count) do
      Items.AddLastItem(nil);
    while Value.Count >= FHeader.Sections.Count do
      FHeader.Sections.AddSection;
    for i := 0 to Value.Count - 1 do
    begin
      Cells[i, Index] := Value[i];
      Objects[i, Index] := Value.Objects[i];
    end;
  finally
    IsUpdating := false;
  end;
end;

constructor TElTreeStringGrid.Create(Owner : TComponent);
begin
  inherited;
  FEditor := TElTreeTrickyInplaceEdit.Create(Self);
  {$ifdef ELPACK_COMPLETE}
  FEditor.Editor.ButtonVisible := false;
  FEditor.Editor.AltButtonVisible := false;
  {$endif}
  FEditor.Editor.OnKeyDown := EditorKeyDown;
  // FEditor.Tree := Self;
  FSavedEditWndProc := FEditor.Editor.WindowProc;
  FEditor.Editor.WindowProc := EditWndProc;
end;

destructor TElTreeStringGrid.Destroy;
begin
  if FRows <> nil then
    FRows.Free;
  if FCols <> nil then
    FCols.Free;
  FCols := nil;
  FRows := nil;
  FEditor.Free;
  inherited;
end;

procedure TElTreeStringGrid.TriggerInplaceEditorNeeded(Item : TElTreeItem;
          SectionIndex : Integer; SupposedFieldType : TElFieldType;
          var Editor : TElTreeInplaceEditor);
begin
  inherited;
  if UseDefaultEditor {or (Editor = nil) }then
    Editor := FEditor;
end;

{$ifdef ELPACK_COMPLETE}
procedure TElTreeStringGrid.SetEditor(Value : TElTreeInplaceButtonEdit);
{$else}
procedure TElTreeStringGrid.SetEditor(Value : TElTreeInplaceEdit);
{$endif}
begin
  FEditor.Assign(Value);
end;

function TElTreeStringGrid.GetCells(ACol, ARow: Integer): TElFString;
begin
  if (ARow >= Integer(Items.Count)) or
    (Items[ARow].ColumnText.Count + 1 <= ACol) then
    Result := ''
  else
  begin
    if ACol = MainTreeColumn then
      Result := Items[ARow].Text
    else
    if ACol < MainTreeColumn then
      Result := Items[ARow].ColumnText[ACol]
    else
      Result := Items[ARow].ColumnText[ACol - 1];
  end;
end;

procedure TElTreeStringGrid.SetCells(ACol, ARow: Integer; Value: TElFString);
var Item : TElTreeItem;
    hs : integer;
begin
  IsUpdating := true;
  try
    Items.AllocateStorage(ARow + 1);
    while ARow >= Integer(Items.Count) do
      Items.AddItem(nil);
    while ACol >= FHeader.Sections.Count do
      FHeader.Sections.AddSection;
    Item := Items[ARow];
    hs := ACol;
    if ACol > MainTreeColumn then
      dec(hs);
    if ACol = MainTreeColumn then
      Item.Text := Value
    else
    begin
      while Item.ColumnText.Count <= hs do
        Item.ColumnText.Add('');
      Item.ColumnText[hs] := Value;
    end;
  finally
    IsUpdating := false;
  end;
end;

function TElTreeStringGrid.GetObjects(ACol, ARow: Integer): TObject;
begin
  if (ARow >= Integer(Items.Count)) or
    (Items[ARow].ColumnText.Count + 1 <= ACol) then
    Result := nil
  else
  begin
    if ACol = MainTreeColumn then
      Result := Items[ARow].AnObject
    else
    if ACol < MainTreeColumn then
      Result := Items[ARow].ColumnText.Objects[ACol]
    else
      Result := Items[ARow].ColumnText.Objects[ACol - 1];
  end;
end;

procedure TElTreeStringGrid.SetObjects(ACol, ARow: Integer; Value: TObject);
var Item : TElTreeItem;
    hs : integer;
begin
  IsUpdating := true;
  try
    Items.AllocateStorage(ARow + 1);
    while ARow >= Integer(Items.Count) do
      Items.AddItem(nil);
    while ACol >= FHeader.Sections.Count do
      FHeader.Sections.AddSection;
    Item := Items[ARow];
    hs := ACol;
    if ACol > MainTreeColumn then
      dec(hs);
    if ACol = MainTreeColumn then
      Item.AnObject := Value
    else
    begin
      while Item.ColumnText.Count <= hs do
        Item.ColumnText.Add('');
      Item.ColumnText.Objects[hs] := Value;
    end;
  finally
    IsUpdating := false;
  end;
end;

procedure TElTreeStringGrid.KeyDownTransfer(Sender : TObject; var Key : Word; 
    Shift : TShiftState);
begin
  if (Key = VK_F2) and (Shift = []) and (not IsEditing) then
  begin
    EditorMode := true;
  end;
  if (Key = VK_RETURN) and (Shift = []) and (not fgoRowSelect) then
  begin
    if (not IsEditing) then
      EditorMode := true;
  end;
  inherited;
end;

procedure TElTreeStringGrid.OnFontChange(Sender: TObject);
begin
  inherited;
  Editor.Editor.Font.Assign(Font);
end;

procedure TElTreeStringGrid.EditorKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  if (Shift = []) then
  begin
    (*
    if (Key in [VK_NEXT, VK_PRIOR, VK_HOME, VK_END, VK_UP, VK_DOWN]) then
    begin
      EndEdit(false);
    end;
    else
    *)
    case Key of
      VK_DOWN, VK_UP:
        begin
          Key := 0;
          EndEdit(not ExplorerEditMode);
          KeyDownTransfer( Self, Key, Shift );
        end;
      VK_RETURN:
        if not goAlwaysShowEditor then
        begin
          Key := 0;
          EndEdit(false);
        end;
      VK_RIGHT:
        if FEditor.Editor.SelStart >= Length(FEditor.Editor.Text) then
        begin
          Key := 0;
          EndEdit(not ExplorerEditMode);
          if (Col+1) < ColCount then
            Col := Col + 1
          else
            if (Row+1) < RowCount then
            begin
              Col := 0;
              Row := Row + 1;
            end;
        end;
      VK_LEFT:
        if FEditor.Editor.SelStart <= 0 then
        begin
          Key := 0;
          EndEdit(not ExplorerEditMode);
          if Col > 0 then
            Col := Col - 1
          else
            if Row > 0 then
            begin
              Col := ColCount - 1;
              Row := Row - 1;
            end;
        end;
      VK_TAB:
        begin
          Key := 0;
          EndEdit(not ExplorerEditMode);
          if (Col+1) < ColCount then
            Col := Col + 1
          else
            if (Row+1) < RowCount then
            begin
              Col := 0;
              Row := Row + 1;
            end;
        end;
    end;

//    if Key in [VK_TAB, VK_RIGHT] then
//      if FEditor.Editor.SelStart > Length(FEditor.Editor.Text)

    (*
    if (Key = VK_RETURN) and (not goAlwaysShowEditor) then
      EndEdit(false)
    else
    if Key = VK_LEFT then
    begin
      if FEditor.Editor.SelStart = 0 then
      begin
        EndEdit(False);
      end;
    end
    else
    if Key = VK_RIGHT then
    begin
      if FEditor.Editor.SelStart = Length(FEditor.Editor.Text) then
      begin
        EndEdit(False);
      end;
    end;
    *)
  end;
  inherited;
end;

procedure TElTreeStringGrid.EditWndProc(var Message: TMessage);
begin
  case Message.Msg of
    // WM_KILLFOCUS: if FEditor.Editor.Visible then EndEdit( false );
    WM_GETDLGCODE: Message.Result := DLGC_WANTCHARS or DLGC_WANTTAB or DLGC_WANTARROWS or DLGC_WANTALLKEYS;
  end;
  if Message.Msg <> WM_GETDLGCODE then
    FSavedEditWndProc( Message )
end;

end.

