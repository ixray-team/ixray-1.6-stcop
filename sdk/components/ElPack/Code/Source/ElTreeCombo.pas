{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

(*

Version History

05/12/2002

  The tree didn't work with standard scrollbars. Fixed.
  Enter/Escape did not close the dropdown. Fixed.
  TreeParentFont didn't work as it should. Fixed.

04/02/2002

  Fixed the problem when drop-down tree didn't appear in responce to the first click

02/26/2002

  Fixed WM_CHAR handling to correctly pass incremental search  
  The combobox fialed to drop down for some reason. Fixed.

12/19/2001

  Fixed problems with positioning the owner form when TreeCombo is dropped on NT 4.

07/15/2001

  Added recent properties and events of ElTree

03/14/2001

  Types used by event handlers defined inside unit. 

*)
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

unit ElTreeCombo;

interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  ExtCtrls,
  ElDragDrop,
  {$ifdef HAS_HTML_RENDER}
  HTMLRender,
  {$endif}
  ElTree,
  ElHeader,
  ElList,
  ElTmSchema,
  ElUxTheme,
  ActiveX,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElImgLst,
  ElTools,
  ElPopBtn,
  ElVCLUtils,
  ElStrUtils,
  ElBtnEdit,
  ElScrollBar,
  ElIni;

{$ifndef BUILDER_USED}

type
  TElHeaderSection = ElHeader.TElHeaderSection;
  TCustomElHeader = ElHeader.TCustomElHeader;
  TElFieldType = ElHeader.TElFieldType;
  TElScrollBarPart = ElScrollBar.TElScrollBarPart;
  TElCellStyle = ElTree.TElCellStyle;
  TElTreeItem = ElTree.TElTreeItem;
  TCustomElTree = ElTree.TCustomElTree;
{$endif}

{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
{$ifndef BUILDER_USED}
  TDragType = ElDragDrop.TDragType;
  IDataObject = ActiveX.IDataObject;
  IDropSource = ActiveX.IDropSource;
  TDragTypes = ElDragDrop.TDragTypes;
  TOleDragObject = ElDragDrop.TOleDragObject;
{$endif}
{$ENDIF}
{$ENDIF}
{$endif}

type
  TElComboDropEvent = procedure(Sender : TObject; Dropped : boolean) of object;

  TElTreeCombo = class;

  TDropdownElTree = class(TCustomElTree)
  private
    FOwner : TElTreeCombo;
    procedure WMMouseActivate(var Msg: TMessage); message WM_MOUSEACTIVATE;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Msg : TWMNCHitTest); message WM_NCHITTEST;
    procedure WMExitSizeMove(var Message: TMessage); message WM_EXITSIZEMOVE;
    procedure WMEnterSizeMove(var Message: TMessage); message WM_ENTERSIZEMOVE;
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure UpdateScrollBars; override;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
  public
    function CanFocus: Boolean;override;
  end;

  TElTreeCombo = class(TCustomElButtonEdit)
  private
    FOnEditRequest : TEditRequestEvent;
    FOnTreeKeyDown : TKeyEvent;
    FOnTreeKeyPress : TKeyPressEvent;
    FOnTreeKeyUp : TKeyEvent;
    FOnTreeMouseDown : TMouseEvent;
    FOnTreeMouseMove : TMouseMoveEvent;
    FOnTreeMouseUp : TMouseEvent;
    FAutoProcessSelection : Boolean;
    FOnNewTextSelection : TNotifyEvent;
    FDropWidth : Integer;
    FDropHeight : Integer;
    FOnValidateCombo_FTree : TValidateComboEvent;
    FOnCellStyleLoad_FTree : TCellStyleSaveEvent;
    FOnCellStyleSave_FTree : TCellStyleSaveEvent;
    FOnComboEditShow_FTree : TComboEditShowEvent;
    FOnHeaderLookup_FTree : TElHeaderLookupEvent;
    FOnHeaderLookupDone_FTree : TElHeaderLookupDoneEvent;
    FOnHeaderResize_FTree : TNotifyEvent;
    FOnHeaderSectionAutoSize_FTree: TColumnNotifyEvent;
    FOnHeaderSectionCollapse_FTree : THeaderSectionEvent;
    FOnHeaderSectionExpand_FTree : THeaderSectionEvent;
    FOnHeaderSectionFilterCall_FTree: TColumnNotifyEvent;
    FOnHorzScrollDrawPart_FTree: TElScrollDrawPartEvent;
    FOnHorzScrollHintNeeded_FTree: TElScrollHintNeededEvent;
    FOnHotTrack_FTree : THotTrackEvent;
    FOnItemLoad_FTree : TItemSaveEvent;
    FOnItemPicDraw2_FTree : TOnPicDrawEvent;
    FOnItemSave_FTree : TItemSaveEvent;
    FOnItemSelectedChange_FTree : TItemSelChangeEvent;
{$IFDEF ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF B_3}
{$IFNDEF VER90}
    FOnOleDragFinish_FTree : TOleDragFinishEvent;
    FOnOleDragStart_FTree : TOleDragStartEvent;
    FOnOleTargetDrag_FTree : TTargetDragEvent;
    FOnOleTargetDrop_FTree : TTargetDropEvent;
{$ENDIF}
{$ENDIF}
{$ENDIF}
    FOnResize_FTree: TNotifyEvent;
    FOnScroll_FTree: ElTree.TElScrollEvent;
    FOnTryEdit_FTree : TTryEditEvent;
    FOnValidateInplaceEdit_FTree : TOnValidateEvent;
    FOnVertScrollDrawPart_FTree: TElScrollDrawPartEvent;
    FOnVertScrollHintNeeded_FTree: TElScrollHintNeededEvent;
    //FEdit : TEdit;
    FSaveCursor,
    FSaveCursor1 : TCursor;
    FReadOnly : Boolean;
    FDropped : Boolean;
    FOnDrop : TElComboDropEvent;
    FTree : TDropdownElTree;
    FSelection : TElTreeItem;
    FSelectionList : TElList;
    IgnoreFocus : boolean;
    IgnoreDrop  : boolean;
    FMouseDown  : boolean;
    FSaveCapture: HWND;
    { Exposed Events: }
    FOnCompareItems : TOnCompareItems;
    FOnHeaderColumnClick : TColumnNotifyEvent;
    FOnHeaderColumnDraw : TElSectionRedrawEvent;
    FOnHeaderColumnMove : TElColumnMoveEvent;
    FOnHeaderColumnResize : TColumnNotifyEvent;
    FOnItemChange : TOnItemChangeEvent;
    FOnItemCollapse : TOnItemExpandEvent;
    FOnItemCollapsing : TOnItemExpanding;
    FOnItemDeletion : TOnItemExpandEvent;
    FOnItemDraw : TOnItemDrawEvent;
    FOnItemExpand : TOnItemExpandEvent;
    FOnItemExpanding : TOnItemExpanding;
    FOnItemFocused : TNotifyEvent;
    FOnItemPicDraw : TOnPicDrawEvent;
    FOnShowLineHint : TOnShowHintEvent;
    FOnStartDrag_FTree : TStartDragEvent;
    procedure ProcessSelect;
    function GetSelection : TElTreeItem;
    procedure SetSelection(newValue : TElTreeItem);
    function GetSelectionList : TElList;
    { Inherited properties' Read/Write methods: }
    procedure SetAutoLineHeight_FTree(newValue : Boolean);
    function GetAutoLineHeight_FTree : Boolean;
    procedure SetBkColor_FTree(newValue : TColor);
    function GetBkColor_FTree : TColor;
    procedure SetChangeStateImage_FTree(newValue : Boolean);
    function GetChangeStateImage_FTree : Boolean;
    procedure SetCtl3D_FTree(newValue : Boolean);
    function GetCtl3D_FTree : Boolean;
    procedure SetCursor(newValue : TCursor);
    function GetCursor : TCursor;
    procedure SetDraggableSections(newValue : Boolean);
    function GetDraggableSections : Boolean;
    procedure SetFont_FTree(newValue : TFont);
    function GetFont_FTree : TFont;
    procedure SetHeaderHotTrack_FTree(newValue : Boolean);
    function GetHeaderHotTrack_FTree : Boolean;
    procedure SetHeaderImages_FTree(newValue : TImageList);
    function GetHeaderImages_FTree : TImageList;
    procedure SetHeaderSections_FTree(newValue : TElHeaderSections);
    function GetHeaderSections_FTree : TElHeaderSections;
    procedure SetHideHintOnMove_FTree(newValue : Boolean);
    function GetHideHintOnMove_FTree : Boolean;
    procedure SetHideHintOnTimer_FTree(newValue : Boolean);
    function GetHideHintOnTimer_FTree : Boolean;
    procedure SetHideHorzScrollBar_FTree(newValue : Boolean);
    function GetHideHorzScrollBar_FTree : Boolean;
    procedure SetHideVertScrollBar_FTree(newValue : Boolean);
    function GetHideVertScrollBar_FTree : Boolean;
    procedure SetHorizontalLines_FTree(newValue : Boolean);
    function GetHorizontalLines_FTree : Boolean;
    procedure SetImages_FTree(newValue : TImageList);
    function GetImages_FTree : TImageList;
    procedure SetItems_FTree(newValue : TElTreeItems);
    function GetItems_FTree : TElTreeItems;
    procedure SetLineHintMode(newValue : THintModes);
    function GetLineHintMode : THintModes;
    procedure SetMainTreeColumn(newValue : Integer);
    function GetMainTreeColumn : Integer;
    procedure SetMultiSelect(newValue : Boolean);
    function GetMultiSelect : Boolean;
    procedure SetOwnerDrawByColumn(newValue : Boolean);
    function GetOwnerDrawByColumn : Boolean;
    procedure SetOwnerDrawMask(newValue : string);
    function GetOwnerDrawMask : string;
    procedure SetParentCtl3D(newValue : Boolean);
    function GetParentCtl3D : Boolean;
    procedure SetParentFont(newValue : Boolean);
    function GetParentFont : Boolean;
    procedure SetParentShowHint(newValue : Boolean);
    function GetParentShowHint : Boolean;
    procedure SetRowSelect(newValue : Boolean);
    function GetRowSelect : Boolean;
    procedure SetScrollTracking(newValue : Boolean);
    function GetScrollTracking : Boolean;
    procedure SetSelectionMode(newValue : TSTSelModes);
    function GetSelectionMode : TSTSelModes;
    procedure SetShowButtons(newValue : Boolean);
    function GetShowButtons : Boolean;
    procedure SetShowColumns(newValue : Boolean);
    function GetShowColumns : Boolean;
    procedure SetShowHint(newValue : Boolean);
    function GetShowHint : Boolean;
    procedure SetShowImages(newValue : Boolean);
    function GetShowImages : Boolean;
    procedure SetShowLines(newValue : Boolean);
    function GetShowLines : Boolean;
    procedure SetShowRoot(newValue : Boolean);
    function GetShowRoot : Boolean;
    procedure SetSortDir(newValue : TSortDirs);
    function GetSortDir : TSortDirs;
    procedure SetSortMode(newValue : TSortModes);
    function GetSortMode : TSortModes;
    procedure SetSortSection(newValue : Integer);
    function GetSortSection : Integer;
    procedure SetSortType(newValue : TSortTypes);
    function GetSortType : TSortTypes;
    procedure SetTextColor(newValue : TColor);
    function GetTextColor : TColor;
    procedure SetTracking(newValue : Boolean);
    function GetTracking : Boolean;
    procedure SetVerticalLines(newValue : Boolean);
    function GetVerticalLines : Boolean;
    procedure ButtonClick(Sender : TObject);
    procedure CompareItemsTransfer(Sender : TObject; Item1 : TElTreeItem; Item2 : TElTreeItem; var res : Integer); { TOnCompareItems }
    procedure ClickHandler(Sender : TObject); { TNotifyEvent }
    procedure DblClickHandler(Sender : TObject); { TNotifyEvent }
    procedure HeaderColumnClickTransfer(Sender : TObject; SectionIndex : Integer); { TColumnNotifyEvent }
    procedure HeaderColumnDrawTransfer(Sender : TCustomElHeader; Canvas : TCanvas; Section : TElHeaderSection; R : TRect; Pressed : Boolean); { TElSectionRedrawEvent }
    procedure HeaderColumnMoveTransfer(Sender : TCustomElTree; Section : TElHeaderSection; OldPos : Integer; NewPos : Integer); { TElColumnMoveEvent }
    procedure HeaderColumnResizeTransfer(Sender : TObject; SectionIndex : Integer); { TColumnNotifyEvent }
    procedure ItemChangeTransfer(Sender : TObject; Item : TElTreeItem; ItemChangeMode : TItemChangeMode); { TOnItemChangeEvent }
    procedure ItemCollapseTransfer(Sender : TObject; Item : TElTreeItem); { TOnItemExpandEvent }
    procedure ItemCollapsingTransfer(Sender : TObject; Item : TElTreeItem; var CanProcess : Boolean); { TOnItemExpanding }
    procedure ItemDeletionTransfer(Sender : TObject; Item : TElTreeItem); { TOnItemExpandEvent }
    procedure ItemDrawTransfer(Sender : TObject; Item : TElTreeItem; Surface : TCanvas; R : TRect; SectionIndex : Integer); { TOnItemDrawEvent }
    procedure ItemExpandTransfer(Sender : TObject; Item : TElTreeItem); { TOnItemExpandEvent }
    procedure ItemExpandingTransfer(Sender : TObject; Item : TElTreeItem; var CanProcess : Boolean); { TOnItemExpanding }
    procedure ItemFocusedTransfer(Sender : TObject); { TNotifyEvent }
    procedure ItemPicDrawTransfer(Sender : TObject; Item : TElTreeItem; var ImageIndex : Integer); { TOnPicDrawEvent }
    procedure KeyDownHandler(Sender : TObject; var Key : Word; Shift : TShiftState); { TKeyEvent }
    procedure ShowLineHintTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; var Text : TElFString; HintWindow : THintWindow; MousePos : TPoint; var DoShowHint : Boolean); { TOnShowHintEvent }
    procedure TreeResizeTransfer(Sender: TObject);
    procedure ScrollTransfer(Sender: TObject; ScrollBarKind: TScrollBarKind; ScrollCode: integer);
    procedure SetDropped(newValue : Boolean);
    procedure SetLineHeight_FTree(newValue : Integer);
    function GetLineHeight_FTree : Integer;
    //function GetTextHeight: Integer;
    function GetReadOnly : Boolean;
    procedure SetReadOnly(newValue : Boolean);
    procedure CMEnter(var Msg : TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg : TCMExit); message CM_EXIT;
    procedure SetAlwaysKeepSelection_FTree(newValue : Boolean);
    function GetAlwaysKeepSelection_FTree : Boolean;
    procedure SetAutoExpand_FTree(newValue : Boolean);
    function GetAutoExpand_FTree : Boolean;
    procedure SetAutoLookup_FTree(newValue : Boolean);
    function GetAutoLookup_FTree : Boolean;
    procedure SetBarStyle_FTree(newValue : Boolean);
    function GetBarStyle_FTree : Boolean;
    procedure SetCustomPlusMinus_FTree(newValue : Boolean);
    function GetCustomPlusMinus_FTree : Boolean;
    procedure SetDeselectChildrenOnCollapse_FTree(newValue : Boolean);
    function GetDeselectChildrenOnCollapse_FTree : Boolean;
    procedure SetTreeDragImageMode(newValue : TDragImgMode);
    function GetTreeDragImageMode : TDragImgMode;
    procedure SetTreeDrawFocusRect(newValue : Boolean);
    function GetTreeDrawFocusRect : Boolean;
    procedure SetTreeFullRowSelect(newValue : Boolean);
    function GetTreeFullRowSelect : Boolean;
    procedure SetTreeImages2(newValue : TImageList);
    function GetTreeImages2 : TImageList;
    procedure SetMinusPicture_FTree(newValue : TBitmap);
    function GetMinusPicture_FTree : TBitmap;
    procedure SetPlusPicture_FTree(newValue : TBitmap);
    function GetPlusPicture_FTree : TBitmap;
    procedure SetShowCheckboxes_FTree(newValue : Boolean);
    function GetShowCheckboxes_FTree : Boolean;
    procedure SetStickyHeaderSections_FTree(newValue : Boolean);
    function GetStickyHeaderSections_FTree : Boolean;
    {$ifdef SUPPORT_STORAGE}
    procedure SetStoragePath_FTree(newValue : string);
    function GetStoragePath_FTree : string;
    {$endif}
    procedure CellStyleLoad_FTreeTransfer(Sender : TObject; Stream : TStream; Style : TElCellStyle); { TCellStyleSaveEvent }
    procedure CellStyleSave_FTreeTransfer(Sender : TObject; Stream : TStream; Style : TElCellStyle); { TCellStyleSaveEvent }
    procedure ComboEditShow_FTreeTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; Combobox : TComboBox); { TComboEditShowEvent }
    procedure HeaderLookup_FTreeTransfer(Sender : TObject; Section : TElHeaderSection; var Text : string); { TElHeaderLookupEvent }
    procedure HeaderLookupDone_FTreeTransfer(Sender : TObject; Section : TElHeaderSection; Text : string; Accepted : Boolean); { TElHeaderLookupDoneEvent }
    procedure HeaderResize_FTreeTransfer(Sender : TObject); { TNotifyEvent }
    procedure HeaderSectionAutoSize_FTreeTransfer(Sender: TObject; SectionIndex: integer);
    procedure HeaderSectionCollapse_FTreeTransfer(Sender : TObject; Section : TElHeaderSection); { THeaderSectionEvent }
    procedure HeaderSectionExpand_FTreeTransfer(Sender : TObject; Section : TElHeaderSection); { THeaderSectionEvent }
    procedure HeaderSectionFilterCall_FTreeTransfer(Sender: TObject; SectionIndex: integer);
    procedure HorzScrollDrawPart_FTreeTransfer(Sender : TObject; Canvas : TCanvas;
      R : TRect; Part : TElScrollBarPart; Enabled, Focused, Pressed : boolean;
      var DefaultDraw : boolean);
    procedure HorzScrollHintNeeded_FTreeTransfer(Sender : TObject; TrackPosition : integer; var Hint : TElFString);
    procedure HotTrack_FTreeTransfer(Sender : TObject; OldItem : TElTreeItem; NewItem : TElTreeItem); { THotTrackEvent }
    procedure ItemLoad_FTreeTransfer(Sender : TObject; Stream : TStream; Item : TElTreeItem); { TItemSaveEvent }
    procedure ItemPicDraw2_FTreeTransfer(Sender : TObject; Item : TElTreeItem; var ImageIndex : Integer); { TOnPicDrawEvent }
    procedure ItemSave_FTreeTransfer(Sender : TObject; Stream : TStream; Item : TElTreeItem); { TItemSaveEvent }
    procedure ItemSelectedChange_FTreeTransfer(Sender : TObject; Item : TElTreeItem); { TItemSelChangeEvent }
{$IFDEF ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF B_3}
{$IFNDEF VER90}
    procedure OleDragFinish_FTreeTransfer(Sender : TObject; dwEffect : TDragType; Result : HRESULT); { TOleDragFinishEvent }
    procedure OleDragStart_FTreeTransfer(Sender : TObject; var dataObj : IDataObject; var dropSource : IDropSource; var dwOKEffects : TDragTypes); { TOleDragStartEvent }
    procedure OleTargetDrag_FTreeTransfer(Sender : TObject; State : TDragState; Source : TOleDragObject; Shift : TShiftState; X : Integer; Y : Integer; var DragType : TDragType); { TTargetDragEvent }
    procedure OleTargetDrop_FTreeTransfer(Sender : TObject; Source : TOleDragObject; Shift : TShiftState; X : Integer; Y : Integer; var DragType : TDragType); { TTargetDropEvent }
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$ifdef VER3_EDITORS}
    procedure TryEdit_FTreeTransfer(Sender : TObject; Item : TElTreeItem; SectionIndex : integer; var CellType : TElFieldType; var CanEdit : Boolean);
{$else}
    procedure TryEdit_FTreeTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; var CellType : TElFieldType; var CanEdit : Boolean);
{$endif}
    procedure ValidateCombo_FTreeTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; Combo : TComboBox; var Accept : Boolean); { TValidateComboEvent }
    procedure ValidateInplaceEdit_FTreeTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; var Text : string; var Accept : Boolean); { TOnValidateEvent }
    procedure VertScrollDrawPart_FTreeTransfer(Sender : TObject; Canvas : TCanvas;
      R : TRect; Part : TElScrollBarPart; Enabled, Focused, Pressed : boolean;
      var DefaultDraw : boolean);
    procedure VertScrollHintNeeded_FTreeTransfer(Sender : TObject; TrackPosition : integer; var Hint : TElFString);
    procedure PrepareSelection;
    procedure SetDropWidth(newValue : Integer);
    procedure SetDropHeight(newValue : Integer);
    procedure SetPathSeparator(newValue : Char);
    function GetPathSeparator : Char;
    function GetDrawFocusRect_FTree : Boolean;
    procedure SetFilteredVisibility_FTree(newValue : Boolean);
    function GetFilteredVisibility_FTree : Boolean;
    procedure SetRightAlignedText_FTree(newValue : Boolean);
    function GetRightAlignedText_FTree : Boolean;
    procedure SetRightAlignedTree_FTree(newValue : Boolean);
    function GetRightAlignedTree_FTree : Boolean;
    procedure SetCheckBoxGlyph_FTree(newValue : TBitmap);
    function GetCheckBoxGlyph_FTree : TBitmap;
    procedure SetCustomCheckboxes_FTree(newValue : Boolean);
    function GetCustomCheckboxes_FTree : Boolean;
    procedure SetRadioButtonGlyph_FTree(newValue : TBitmap);
    function GetRadioButtonGlyph_FTree : TBitmap;
    procedure SetUnderlineTracked_FTree(newValue : Boolean);
    function GetUnderlineTracked_FTree : Boolean;
    procedure SetDoInplaceEdit(newValue : Boolean);
    function GetDoInplaceEdit : Boolean;
    procedure TreeKeyDownTransfer(Sender : TObject; var Key : Word; Shift : TShiftState); { TKeyEvent }
    procedure TreeKeyPressTransfer(Sender : TObject; var Key : Char); { TKeyPressEvent }
    procedure TreeKeyUpTransfer(Sender : TObject; var Key : Word; Shift : TShiftState); { TKeyEvent }
    procedure TreeMouseDownTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer); { TMouseEvent }
    procedure TreeMouseMoveTransfer(Sender : TObject; Shift : TShiftState; X : Integer; Y : Integer); { TMouseMoveEvent }
    procedure TreeMouseUpTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer); { TMouseEvent }
    procedure SetShowRootButtons(newValue : Boolean);
    function GetShowRootButtons : Boolean;
    procedure EditRequestTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection); { TEditRequestEvent }
    procedure SetTreeFlat(newValue : Boolean);
    function GetTreeFlat : Boolean;
    procedure SetTreeHeaderActiveFilterColor(newValue : TColor);
    function GetTreeHeaderActiveFilterColor : TColor;
    procedure SetTreeHeaderFilterColor(newValue : TColor);
    function GetTreeHeaderFilterColor : TColor;
    procedure SetTreeHeaderFlat(newValue : Boolean);
    function GetTreeHeaderFlat : Boolean;
    procedure SetAutoResizeColumns(newValue : Boolean);
    function GetAutoResizeColumns : Boolean;
    procedure SetTreeBackground(newValue : TBitmap);
    function GetTreeBackground : TBitmap;
    procedure SetTreeBackgroundType(newValue : TElBkGndType);
    function GetTreeBackgroundType : TElBkGndType;
    procedure SetFlatFocusedScrollbars(newValue : Boolean);
    function GetFlatFocusedScrollbars : Boolean;
    procedure SetGradientEndColor(newValue : TColor);
    function GetGradientEndColor : TColor;
    procedure SetGradientStartColor(newValue : TColor);
    function GetGradientStartColor : TColor;
    procedure SetGradientSteps(newValue : Integer);
    function GetGradientSteps : Integer;
    procedure SetHorzScrollBarStyles(newValue : TElScrollBarStyles);
    function GetHorzScrollBarStyles : TElScrollBarStyles;
    procedure SetVertScrollBarStyles(newValue : TElScrollBarStyles);
    function GetVertScrollBarStyles : TElScrollBarStyles;
    function GetForcedScrollBars: TScrollStyle;
    procedure SetForcedScrollBars(const Value: TScrollStyle);
    function GetFocusedSelectColor_Tree: TColor;
    procedure SetFocusedSelectColor_Tree(const Value: TColor);
    function GetTreeHeaderHeight: integer;
    procedure SetTreeHeaderHeight(const Value: integer);
    function GetHeaderInvertSortArrows_FTree: boolean;
    procedure SetHeaderInvertSortArrows_FTree(const Value: boolean);
    function GetLeftPosition_FTree: integer;
    procedure SetLeftPosition_FTree(const Value: integer);
    function GetLinesColor_FTree: TColor;
    function GetLinesStyle_FTree: TPenStyle;
    procedure SetLinesColor_FTree(const Value: TColor);
    procedure SetLinesStyle_FTree(const Value: TPenStyle);
    function GetLockHeaderHeight_FTree: boolean;
    procedure SetLockHeaderHeight_FTree(const Value: boolean);
    function GetMoveColumnOnDrag_FTree: boolean;
    function GetMoveFocusOnCollapse_FTree: boolean;
    procedure SetMoveColumnOnDrag_FTree(const Value: boolean);
    procedure SetMoveFocusOnCollapse_FTree(const Value: boolean);
    function GetNoBlendSelected: boolean;
    procedure SetNoBlendSelected(const Value: boolean);
    function GetScrollBackground: boolean;
    procedure SetScrollBackground(const Value: boolean);
    function GetSelectColumn: integer;
    procedure SetSelectColumn(const Value: integer);
    procedure StartDrag_FTreeTransfer(Sender: TObject;
      var DragObject: TDragObject);
    function GetTreeActiveBorderType: TElFlatBorderType;
    function GetTreeInactiveBorderType: TElFlatBorderType;
    procedure SetTreeActiveBorderType(const Value: TElFlatBorderType);
    procedure SetTreeInactiveBorderType(const Value: TElFlatBorderType);
    procedure SetStorage_FTree(newValue : TElIniFile);
    function  GetStorage_FTree : TElIniFile;
    function GetAlwaysKeepFocus: Boolean;
    procedure SetAlwaysKeepFocus(Value: Boolean);
    function GetAdjustMultilineHeight: Boolean;
    procedure SetAdjustMultilineHeight(Value: Boolean);
    procedure SetBarStyleVerticalLines(Value: Boolean);
    function GetBarStyleVerticalLines: Boolean;
    function GetChangeDelay: Integer;
    procedure SetChangeDelay(Value: Integer);
    function GetHorzDivLinesColor: TColor;
    procedure SetHorzDivLinesColor(Value: TColor);
    function GetDragTrgDrawMode: TDragTargetDraw;
    procedure SetDragTrgDrawMode(Value: TDragTargetDraw);
    function GetDragExpandDelay: Integer;
    procedure SetDragExpandDelay(Value: Integer);
    function GetDragRectAcceptColor: TColor;
    procedure SetDragRectAcceptColor(Value: TColor);
    function GetDragRectDenyColor: TColor;
    procedure SetDragRectDenyColor(Value: TColor);
    function GetExpandOnDragOver: Boolean;
    procedure SetExpandOnDragOver(Value: Boolean);
    function GetFocusedSelectTextColor: TColor;
    procedure SetFocusedSelectTextColor(Value: TColor);
    function GetHeaderColor: TColor;
    procedure SetHeaderColor(Value: TColor);
    function GetHeaderWrapCaptions: Boolean;
    procedure SetHeaderWrapCaptions(Value: Boolean);
    function GetHideFocusRect: Boolean;
    procedure SetHideFocusRect(Value: Boolean);
    function GetHideSelectColor: TColor;
    procedure SetHideSelectColor(Value: TColor);
    function GetHideSelectTextColor: TColor;
    procedure SetHideSelectTextColor(Value: TColor);
    function GetHideSelection: Boolean;
    procedure SetHideSelection(Value: Boolean);
    function GetIncrementalSearch: Boolean;
    procedure SetIncrementalSearch(Value: Boolean);
    function GetItemIndent: Integer;
    procedure SetItemIndent(Value: Integer);
    function GetLineHintColor: TColor;
    procedure SetLineHintColor(Value: TColor);
    function GetLineHintTimeout: Integer;
    procedure SetLineHintTimeout(Value: Integer);
    function GetLineHintType: TLineHintType;
    procedure SetLineHintType(Value: TLineHintType);
    function GetPlusMinusTransparent: Boolean;
    procedure SetPlusMinusTransparent(Value: Boolean);
    function GetRightClickSelect: Boolean;
    procedure SetRightClickSelect(Value: Boolean);
    function GetRowHotTrack: Boolean;
    procedure SetRowHotTrack(Value: Boolean);
    function GetScrollbarOpposite: Boolean;
    procedure SetScrollbarOpposite(Value: Boolean);
    function GetTrackColor: TColor;
    procedure SetTrackColor(Value: TColor);
    function GetUseCustomScrollBars: Boolean;
    procedure SetUseCustomScrollBars(Value: Boolean);
    function GetVerticalLinesLong: Boolean;
    procedure SetVerticalLinesLong(Value: Boolean);
    function GetUseSystemHintColors: Boolean;
    procedure SetUseSystemHintColors(Value: Boolean);
    function GetOnHeaderSectionMeasure: TMeasureSectionEVent;
    procedure SetOnHeaderSectionMeasure(Value: TMeasureSectionEVent);
    function GetOnApplyVisFilter: TApplyVisFilterEvent;
    procedure SetOnApplyVisFilter(Value: TApplyVisFilterEvent);
    function GetOnAfterSelectionChange: TNotifyEvent;
    procedure SetOnAfterSelectionChange(Value: TNotifyEvent);
    function GetOnItemChecked: TOnItemCheckedEvent;
    procedure SetOnItemChecked(Value: TOnItemCheckedEvent);
    function GetOnSortBegin: TNotifyEvent;
    procedure SetOnSortBegin(Value: TNotifyEvent);
    function GetOnSortEnd: TNotifyEvent;
    procedure SetOnSortEnd(Value: TNotifyEvent);
    {$ifdef HAS_HTML_RENDER}
    function GetOnHTMLImageNeeded: TElHTMLImageNeededEvent;
    procedure SetOnHTMLImageNeeded(Value: TElHTMLImageNeededEvent);
    {$endif}
    function GetStripedOddColor: TColor;
    procedure SetStripedOddColor(Value: TColor);
    function GetStripedEvenColor: TColor;
    procedure SetStripedEvenColor(Value: TColor);
    function GetStripedItems: Boolean;
    procedure SetStripedItems(Value: Boolean);

    procedure WMChar(var Message: TMessage); message WM_CHAR;
    procedure WMKeyDown(var Message: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TMessage); message WM_KEYUP;
    procedure WMSysKeyDown(var Message: TMessage); message WM_SYSKEYDOWN;
    procedure WMSysKeyUp(var Message: TMessage); message WM_SYSKEYUP;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    { Protected declarations }
    FCloseOnClick: Boolean;
    FSizeableTree: Boolean;
    FAdjustDropDownPos: Boolean;
    procedure TriggerDropEvent(Dropped : boolean); virtual;
    procedure KeyPress(var Key : Char); override;
    procedure TriggerNewTextSelectionEvent; virtual;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    function GetHintType: TElHintType;
    procedure SetHintType(Value: TElHintType);
    procedure WMActivateApp(var Msg : TWMActivateApp); message WM_ACTIVATEAPP;
    procedure WMKillFocus(var Msg: TMessage); message WM_KILLFOCUS;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk); message 
        WM_LBUTTONDBLCLK;
    procedure EMSetReadOnly(var Msg : TMessage); message EM_SETREADONLY;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMContextMenu(var Message: TMessage); message WM_CONTEXTMENU;
    procedure WMSetFocus(var Msg: TMessage); message WM_SETFOCUS;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    function GetVertDivLinesColor: TColor;
    procedure SetVertDivLinesColor(Value: TColor);
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    function GetHeaderSections: TElHeaderSections;
    procedure SetHeaderSections(Value: TElHeaderSections);
    function GetHeaderImages: TImageList;
    procedure SetHeaderImages(Value: TImageList);
    function GetMultiSelectLevel: Integer;
    procedure SetMultiSelectLevel(Value: Integer);
    function GetDragScrollInterval: Integer;
    procedure SetDragScrollInterval(Value: Integer);
    function GetMouseFrameSelect: Boolean;
    procedure SetMouseFrameSelect(Value: Boolean);
    function CreateTree: TCustomElTree; virtual;
    function GetShowLeafButton: Boolean;
    procedure SetShowLeafButton(Value: Boolean);
    function GetLeafPicture: TBitmap;
    procedure SetLeafPicture(Value: TBitmap);
    //function GetExplorerEditMode: Boolean;
    // procedure SetExplorerEditMode(Value: Boolean);
    function GetHeaderFont: TFont;
    procedure SetHeaderFont(Value: TFont);
    function GetHeaderUseTreeFont: Boolean;
    procedure SetHeaderUseTreeFont(Value: Boolean);
    procedure SetSizeableTree(Value: Boolean);
    procedure SetUseXPThemes(const Value : boolean); override;
    procedure Loaded; override;
    function IsButtonWidthStored: Boolean;
    property OnApplyVisFilter: TApplyVisFilterEvent read GetOnApplyVisFilter write
        SetOnApplyVisFilter;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    function GetTree : TElTree;

    property SelectionList : TElList read GetSelectionList; { Public }
    property Dropped : Boolean read FDropped write SetDropped; { Public }
    property Selection : TElTreeItem read GetSelection write SetSelection; { Published }
  published
    property ButtonWidth stored IsButtonWidthStored default 15;

    property TopMargin;
    property LeftMargin;
    property RightMargin;
    property AutoSize;
    property RTLContent;
    property BorderSides;
    property PasswordChar;
    property MaxLength;
    property Transparent;
    property WantTabs;
    property HandleDialogKeys;
    property HideSelection;
    property TabSpaces;
    {$ifdef ELPACK_COMPLETE}
    property ImageForm;
    {$endif}
    property WordWrap;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnChange;
    property OnSelectionChange;

    property ActiveBorderType;
    property ButtonFlat default false;
    property ButtonColor default clBtnFace;
    property Flat;
    property InactiveBorderType;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;

    property Align;
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Text;
    property TabStop;
    property TabOrder;
    property Visible;

    { Exposed subcomponent properties: }
    property AutoLineHeight : Boolean read GetAutoLineHeight_FTree write SetAutoLineHeight_FTree;
    property TreeBkColor : TColor read GetBkColor_FTree write SetBkColor_FTree;
    property TreeChangeStateImage : Boolean read GetChangeStateImage_FTree write SetChangeStateImage_FTree;
    property TreeCtl3D : Boolean read GetCtl3D_FTree write SetCtl3D_FTree;
    property TreeCursor : TCursor read GetCursor write SetCursor;
    property DraggableSections : Boolean read GetDraggableSections write SetDraggableSections;
    property TreeFocusedSelectColor: TColor read GetFocusedSelectColor_Tree write SetFocusedSelectColor_Tree;
    property TreeFont : TFont read GetFont_FTree write SetFont_FTree;
    property TreeHeaderHotTrack : Boolean read GetHeaderHotTrack_FTree write SetHeaderHotTrack_FTree;
    property TreeHeaderImages : TImageList read GetHeaderImages_FTree write SetHeaderImages_FTree;
    property TreeHeaderInvertSortArrows: boolean read GetHeaderInvertSortArrows_FTree write SetHeaderInvertSortArrows_FTree;
    property TreeHeaderSections : TElHeaderSections read GetHeaderSections_FTree write SetHeaderSections_FTree;
    property TreeHideHintOnMove : Boolean read GetHideHintOnMove_FTree write SetHideHintOnMove_FTree;
    property TreeHideHintOnTimer : Boolean read GetHideHintOnTimer_FTree write SetHideHintOnTimer_FTree;
    property TreeHideHorzScrollBar : Boolean read GetHideHorzScrollBar_FTree write SetHideHorzScrollBar_FTree;
    property TreeHideVertScrollBar : Boolean read GetHideVertScrollBar_FTree write SetHideVertScrollBar_FTree;
    property TreeHorizontalLines : Boolean read GetHorizontalLines_FTree write SetHorizontalLines_FTree;
    property TreeLeftPosition: integer read GetLeftPosition_FTree write SetLeftPosition_FTree;
    property Images : TImageList read GetImages_FTree write SetImages_FTree;
    property Images2 : TImageList read GetTreeImages2 write SetTreeImages2;
    property Items : TElTreeItems read GetItems_FTree write SetItems_FTree;
    property LineHintMode : THintModes read GetLineHintMode write SetLineHintMode;
    property MainTreeColumn : Integer read GetMainTreeColumn write SetMainTreeColumn;
    property MultiSelect : Boolean read GetMultiSelect write SetMultiSelect;
    property NoBlendSelected: boolean read GetNoBlendSelected write SetNoBlendSelected;
    property OwnerDrawByColumn : Boolean read GetOwnerDrawByColumn write SetOwnerDrawByColumn;
    property OwnerDrawMask : string read GetOwnerDrawMask write SetOwnerDrawMask;
    property TreeParentCtl3D : Boolean read GetParentCtl3D write SetParentCtl3D;
    property TreeParentFont : Boolean read GetParentFont write SetParentFont;
    property TreeParentShowHint : Boolean read GetParentShowHint write SetParentShowHint;
    property TreeScrollBackground: boolean read GetScrollBackground write SetScrollBackground;
    property RowSelect : Boolean read GetRowSelect write SetRowSelect;
    property ScrollTracking : Boolean read GetScrollTracking write SetScrollTracking;
    property SelectColumn: integer read GetSelectColumn write SetSelectColumn;
    property SelectionMode : TSTSelModes read GetSelectionMode write SetSelectionMode;
    property ShowButtons : Boolean read GetShowButtons write SetShowButtons;
    property ShowColumns : Boolean read GetShowColumns write SetShowColumns;
    property TreeShowHint : Boolean read GetShowHint write SetShowHint;
    property ShowImages : Boolean read GetShowImages write SetShowImages;
    property ShowLines : Boolean read GetShowLines write SetShowLines;
    property ShowRoot : Boolean read GetShowRoot write SetShowRoot;
    property SortDir : TSortDirs read GetSortDir write SetSortDir;
    property SortMode : TSortModes read GetSortMode write SetSortMode;
    property SortSection : Integer read GetSortSection write SetSortSection;
    property SortType : TSortTypes read GetSortType write SetSortType;
    property TextColor : TColor read GetTextColor write SetTextColor;
    property Tracking : Boolean read GetTracking write SetTracking;
    property VerticalLines : Boolean read GetVerticalLines write SetVerticalLines;
    property AlwaysKeepSelection : Boolean read GetAlwaysKeepSelection_FTree write SetAlwaysKeepSelection_FTree;
    
    property AutoExpand : Boolean read GetAutoExpand_FTree write SetAutoExpand_FTree;
    property AutoLookup : Boolean read GetAutoLookup_FTree write SetAutoLookup_FTree;
    property BarStyle : Boolean read GetBarStyle_FTree write SetBarStyle_FTree;
    property CustomPlusMinus : Boolean read GetCustomPlusMinus_FTree write SetCustomPlusMinus_FTree;
    property DeselectChildrenOnCollapse : Boolean read GetDeselectChildrenOnCollapse_FTree write SetDeselectChildrenOnCollapse_FTree;
    property DragImageMode : TDragImgMode read GetTreeDragImageMode write SetTreeDragImageMode;
    property DrawFocusRect : Boolean read GetTreeDrawFocusRect write SetTreeDrawFocusRect;
    property ForcedScrollBars: TScrollStyle read GetForcedScrollBars write SetForcedScrollBars;
    property FullRowSelect : Boolean read GetTreeFullRowSelect write SetTreeFullRowSelect;
    property MinusPicture : TBitmap read GetMinusPicture_FTree write SetMinusPicture_FTree;
    property MoveColumnOnDrag: boolean read GetMoveColumnOnDrag_FTree write SetMoveColumnOnDrag_FTree;
    property MoveFocusOnCollapse: boolean read GetMoveFocusOnCollapse_FTree write SetMoveFocusOnCollapse_FTree;
    property PlusPicture : TBitmap read GetPlusPicture_FTree write SetPlusPicture_FTree;
    property ShowCheckboxes : Boolean read GetShowCheckboxes_FTree write SetShowCheckboxes_FTree;
    property StickyHeaderSections : Boolean read GetStickyHeaderSections_FTree write SetStickyHeaderSections_FTree;
    property Storage : TElIniFile read GetStorage_FTree write SetStorage_FTree;
    {$ifdef SUPPORT_STORAGE}
    property StoragePath : string read GetStoragePath_FTree write SetStoragePath_FTree;
    {$endif}
    property DropWidth : Integer read FDropWidth write SetDropWidth;
    property DropHeight : Integer read FDropHeight write SetDropHeight default 80;

    property LineHeight : Integer read GetLineHeight_FTree write SetLineHeight_FTree;
    property LinesColor: TColor read GetLinesColor_FTree write SetLinesColor_FTree;
    property LinesStyle: TPenStyle read GetLinesStyle_FTree write SetLinesStyle_FTree;
    property LockHeaderHeight: boolean read GetLockHeaderHeight_FTree write SetLockHeaderHeight_FTree;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly default False; { Published }
    property PathSeparator : Char read GetPathSeparator write SetPathSeparator;
    property FilteredVisibility : Boolean read GetFilteredVisibility_FTree write SetFilteredVisibility_FTree;
    property RightAlignedText : Boolean read GetRightAlignedText_FTree write SetRightAlignedText_FTree;
    property RightAlignedTree : Boolean read GetRightAlignedTree_FTree write SetRightAlignedTree_FTree;

    { Inherited events: }
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    { Exposed subcomponent events: }
    property OnCompareItems : TOnCompareItems read FOnCompareItems write FOnCompareItems;
    property OnHeaderColumnClick : TColumnNotifyEvent read FOnHeaderColumnClick write FOnHeaderColumnClick;
    property OnHeaderColumnDraw : TElSectionRedrawEvent read FOnHeaderColumnDraw write FOnHeaderColumnDraw;
    property OnHeaderColumnMove : TElColumnMoveEvent read FOnHeaderColumnMove write FOnHeaderColumnMove;
    property OnHeaderColumnResize : TColumnNotifyEvent read FOnHeaderColumnResize write FOnHeaderColumnResize;
    property OnItemChange : TOnItemChangeEvent read FOnItemChange write FOnItemChange;
    property OnItemCollapse : TOnItemExpandEvent read FOnItemCollapse write FOnItemCollapse;
    property OnItemCollapsing : TOnItemExpanding read FOnItemCollapsing write FOnItemCollapsing;
    property OnItemDeletion : TOnItemExpandEvent read FOnItemDeletion write FOnItemDeletion;
    property OnItemDraw : TOnItemDrawEvent read FOnItemDraw write FOnItemDraw;
    property OnItemExpand : TOnItemExpandEvent read FOnItemExpand write FOnItemExpand;
    property OnItemExpanding : TOnItemExpanding read FOnItemExpanding write FOnItemExpanding;
    property OnItemFocused : TNotifyEvent read FOnItemFocused write FOnItemFocused;
    property OnItemPicDraw : TOnPicDrawEvent read FOnItemPicDraw write FOnItemPicDraw;
    property OnScroll: ElTree.TElScrollEvent read FOnScroll_FTree write FOnScroll_FTree;
    property OnShowLineHint : TOnShowHintEvent read FOnShowLineHint write FOnShowLineHint;
    property OnTreeResize: TNotifyEvent read FOnResize_FTree write FOnResize_FTree;


    property OnCellStyleLoad : TCellStyleSaveEvent read FOnCellStyleLoad_FTree write FOnCellStyleLoad_FTree;
    property OnCellStyleSave : TCellStyleSaveEvent read FOnCellStyleSave_FTree write FOnCellStyleSave_FTree;
    property OnComboEditShow : TComboEditShowEvent read FOnComboEditShow_FTree write FOnComboEditShow_FTree;
    property OnHeaderLookup : TElHeaderLookupEvent read FOnHeaderLookup_FTree write FOnHeaderLookup_FTree;
    property OnHeaderLookupDone : TElHeaderLookupDoneEvent read FOnHeaderLookupDone_FTree write FOnHeaderLookupDone_FTree;
    property OnHeaderResize : TNotifyEvent read FOnHeaderResize_FTree write FOnHeaderResize_FTree;
    property OnHeaderSectionAutoSize: TColumnNotifyEvent read FOnHeaderSectionAutoSize_FTree write FOnHeaderSectionAutoSize_FTree;
    property OnHeaderSectionCollapse : THeaderSectionEvent read FOnHeaderSectionCollapse_FTree write FOnHeaderSectionCollapse_FTree;
    property OnHeaderSectionExpand : THeaderSectionEvent read FOnHeaderSectionExpand_FTree write FOnHeaderSectionExpand_FTree;
    property OnHeaderSectionFilterCall: TColumnNotifyEvent read FOnHeaderSectionFilterCall_FTree write FOnHeaderSectionFilterCall_FTree;
    property OnHorzScrollDrawPart: TElScrollDrawPartEvent read FOnHorzScrollDrawPart_FTree write FOnHorzScrollDrawPart_FTree;
    property OnHorzScrollHintNeeded: TElScrollHintNeededEvent read FOnHorzScrollHintNeeded_FTree write FOnHorzScrollHintNeeded_FTree;
    property OnHotTrack : THotTrackEvent read FOnHotTrack_FTree write FOnHotTrack_FTree;
    property OnItemLoad : TItemSaveEvent read FOnItemLoad_FTree write FOnItemLoad_FTree;
    property OnItemPicDraw2 : TOnPicDrawEvent read FOnItemPicDraw2_FTree write FOnItemPicDraw2_FTree;
    property OnItemSave : TItemSaveEvent read FOnItemSave_FTree write FOnItemSave_FTree;
    property OnItemSelectedChange : TItemSelChangeEvent read FOnItemSelectedChange_FTree write FOnItemSelectedChange_FTree;
{$IFDEF ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF B_3}
{$IFNDEF VER90}
    property OnOleDragFinish : TOleDragFinishEvent read FOnOleDragFinish_FTree write FOnOleDragFinish_FTree;
    property OnOleDragStart : TOleDragStartEvent read FOnOleDragStart_FTree write FOnOleDragStart_FTree;
    property OnOleTargetDrag : TTargetDragEvent read FOnOleTargetDrag_FTree write FOnOleTargetDrag_FTree;
    property OnOleTargetDrop : TTargetDropEvent read FOnOleTargetDrop_FTree write FOnOleTargetDrop_FTree;
{$ENDIF}
{$ENDIF}
{$ENDIF}
    property OnTryEdit : TTryEditEvent read FOnTryEdit_FTree write FOnTryEdit_FTree;
    property OnValidateCombo : TValidateComboEvent read FOnValidateCombo_FTree write FOnValidateCombo_FTree;
    property OnValidateInplaceEdit : TOnValidateEvent read FOnValidateInplaceEdit_FTree write FOnValidateInplaceEdit_FTree;
    property OnVertScrollDrawPart: TElScrollDrawPartEvent read FOnVertScrollDrawPart_FTree write FOnVertScrollDrawPart_FTree;
    property OnVertScrollHintNeeded: TElScrollHintNeededEvent read FOnVertScrollHintNeeded_FTree write FOnVertScrollHintNeeded_FTree;
    property OnNewTextSelection : TNotifyEvent read FOnNewTextSelection write FOnNewTextSelection;
    property OnTreeKeyDown : TKeyEvent read FOnTreeKeyDown write FOnTreeKeyDown;
    property OnTreeKeyPress : TKeyPressEvent read FOnTreeKeyPress write FOnTreeKeyPress;
    property OnTreeKeyUp : TKeyEvent read FOnTreeKeyUp write FOnTreeKeyUp;
    property OnTreeMouseDown : TMouseEvent read FOnTreeMouseDown write FOnTreeMouseDown;
    property OnTreeMouseMove : TMouseMoveEvent read FOnTreeMouseMove write FOnTreeMouseMove;
    property OnTreeMouseUp : TMouseEvent read FOnTreeMouseUp write FOnTreeMouseUp;

    property OnDrop : TElComboDropEvent read FOnDrop write FOnDrop;

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
    property CheckBoxGlyph : TBitmap read GetCheckBoxGlyph_FTree write SetCheckBoxGlyph_FTree;
    property CustomCheckboxes : Boolean read GetCustomCheckboxes_FTree write SetCustomCheckboxes_FTree;
    property RadioButtonGlyph : TBitmap read GetRadioButtonGlyph_FTree write SetRadioButtonGlyph_FTree;
    property UnderlineTracked : Boolean read GetUnderlineTracked_FTree write SetUnderlineTracked_FTree;
    property AutoProcessSelection : Boolean read FAutoProcessSelection write FAutoProcessSelection default true; { Published }
    property DoInplaceEdit : Boolean read GetDoInplaceEdit write SetDoInplaceEdit;
    property ShowRootButtons : Boolean read GetShowRootButtons write SetShowRootButtons;
    property OnEditRequest : TEditRequestEvent read FOnEditRequest write FOnEditRequest;
    property TreeActiveBorderType: TElFlatBorderType read GetTreeActiveBorderType write SetTreeActiveBorderType;
    property TreeFlat : Boolean read GetTreeFlat write SetTreeFlat;
    property TreeHeaderActiveFilterColor : TColor read GetTreeHeaderActiveFilterColor write SetTreeHeaderActiveFilterColor;
    property TreeHeaderFilterColor : TColor read GetTreeHeaderFilterColor write SetTreeHeaderFilterColor;
    property TreeHeaderFlat : Boolean read GetTreeHeaderFlat write SetTreeHeaderFlat;
    property TreeHeaderHeight: integer read GetTreeHeaderHeight write SetTreeHeaderHeight;
    property TreeInactiveBorderType: TElFlatBorderType read GetTreeInactiveBorderType write SetTreeInactiveBorderType;
    property AutoResizeColumns : Boolean read GetAutoResizeColumns write SetAutoResizeColumns;
    property TreeBackground : TBitmap read GetTreeBackground write SetTreeBackground;
    property TreeBackgroundType : TElBkGndType read GetTreeBackgroundType write SetTreeBackgroundType;
    property FlatFocusedScrollbars : Boolean read GetFlatFocusedScrollbars write SetFlatFocusedScrollbars;
    property GradientEndColor : TColor read GetGradientEndColor write SetGradientEndColor;
    property GradientStartColor : TColor read GetGradientStartColor write SetGradientStartColor;
    property GradientSteps : Integer read GetGradientSteps write SetGradientSteps;
    property HorzScrollBarStyles : TElScrollBarStyles read GetHorzScrollBarStyles write SetHorzScrollBarStyles;
    property VertScrollBarStyles : TElScrollBarStyles read GetVertScrollBarStyles write SetVertScrollBarStyles;
    property AlwaysKeepFocus: Boolean read GetAlwaysKeepFocus write 
        SetAlwaysKeepFocus;
    property AdjustMultilineHeight: Boolean read GetAdjustMultilineHeight write
        SetAdjustMultilineHeight;
    property BarStyleVerticalLines: Boolean read GetBarStyleVerticalLines write
        SetBarStyleVerticalLines;
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay;
    property HorzDivLinesColor: TColor read GetHorzDivLinesColor write
        SetHorzDivLinesColor;
    property DragTrgDrawMode: TDragTargetDraw read GetDragTrgDrawMode write
        SetDragTrgDrawMode;
    property DragExpandDelay: Integer read GetDragExpandDelay write 
        SetDragExpandDelay;
    property DragRectAcceptColor: TColor read GetDragRectAcceptColor write 
        SetDragRectAcceptColor;
    property DragRectDenyColor: TColor read GetDragRectDenyColor write 
        SetDragRectDenyColor;
    property ExpandOnDragOver: Boolean read GetExpandOnDragOver write 
        SetExpandOnDragOver;
    property FocusedSelectTextColor: TColor read GetFocusedSelectTextColor write 
        SetFocusedSelectTextColor;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
    property HeaderWrapCaptions: Boolean read GetHeaderWrapCaptions write 
        SetHeaderWrapCaptions;
    property HideFocusRect: Boolean read GetHideFocusRect write SetHideFocusRect;
    property HideSelectColor: TColor read GetHideSelectColor write 
        SetHideSelectColor;
    property HideSelectTextColor: TColor read GetHideSelectTextColor write 
        SetHideSelectTextColor;
    property TreeHideSelection: Boolean read GetHideSelection write SetHideSelection;
    property IncrementalSearch: Boolean read GetIncrementalSearch write 
        SetIncrementalSearch;
    property ItemIndent: Integer read GetItemIndent write SetItemIndent;
    property LineHintColor: TColor read GetLineHintColor write SetLineHintColor;
    property LineHintTimeout: Integer read GetLineHintTimeout write 
        SetLineHintTimeout;
    property LineHintType: TLineHintType read GetLineHintType write SetLineHintType;
    property PlusMinusTransparent: Boolean read GetPlusMinusTransparent write 
        SetPlusMinusTransparent;
    property RightClickSelect: Boolean read GetRightClickSelect write 
        SetRightClickSelect;
    property RowHotTrack: Boolean read GetRowHotTrack write SetRowHotTrack;
    property ScrollbarOpposite: Boolean read GetScrollbarOpposite write 
        SetScrollbarOpposite;
    property TrackColor: TColor read GetTrackColor write SetTrackColor;
    property UseCustomScrollBars: Boolean read GetUseCustomScrollBars write 
        SetUseCustomScrollBars;
    property VerticalLinesLong: Boolean read GetVerticalLinesLong write 
        SetVerticalLinesLong;
    property UseSystemHintColors: Boolean read GetUseSystemHintColors write 
        SetUseSystemHintColors;
    property OnHeaderSectionMeasure: TMeasureSectionEVent read
        GetOnHeaderSectionMeasure write SetOnHeaderSectionMeasure;
    property OnAfterSelectionChange: TNotifyEvent read GetOnAfterSelectionChange 
        write SetOnAfterSelectionChange;
    property OnItemChecked: TOnItemCheckedEvent read GetOnItemChecked write
        SetOnItemChecked;
    property OnSortBegin: TNotifyEvent read GetOnSortBegin write SetOnSortBegin;
    property OnSortEnd: TNotifyEvent read GetOnSortEnd write SetOnSortEnd;
    {$ifdef HAS_HTML_RENDER}
    property OnHTMLImageNeeded: TElHTMLImageNeededEvent read GetOnHTMLImageNeeded
        write SetOnHTMLImageNeeded;
    {$endif}
    property StripedOddColor: TColor read GetStripedOddColor write
        SetStripedOddColor;
    property StripedEvenColor: TColor read GetStripedEvenColor write
        SetStripedEvenColor;
    property StripedItems: Boolean read GetStripedItems write SetStripedItems;
    property HintType: TElHintType read GetHintType write SetHintType;
    property VertDivLinesColor: TColor read GetVertDivLinesColor write
        SetVertDivLinesColor;
    property CloseOnClick: Boolean read FCloseOnClick write FCloseOnClick default
        false;
    property SizeableTree: Boolean read FSizeableTree write SetSizeableTree default
        true;
    property HeaderSections: TElHeaderSections read GetHeaderSections write
        SetHeaderSections;
    property HeaderImages: TImageList read GetHeaderImages write SetHeaderImages;
    property MultiSelectLevel: Integer read GetMultiSelectLevel write
        SetMultiSelectLevel;
    property DragScrollInterval: Integer read GetDragScrollInterval write
        SetDragScrollInterval;
    property MouseFrameSelect: Boolean read GetMouseFrameSelect write
        SetMouseFrameSelect;
    property ShowLeafButton: Boolean read GetShowLeafButton write SetShowLeafButton;
    property LeafPicture: TBitmap read GetLeafPicture write SetLeafPicture;
    //property ExplorerEditMode: Boolean read GetExplorerEditMode write
    //     SetExplorerEditMode;
    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
    property HeaderUseTreeFont: Boolean read GetHeaderUseTreeFont write 
        SetHeaderUseTreeFont;
    property AdjustDropDownPos: Boolean read FAdjustDropDownPos write
        FAdjustDropDownPos default true;
  end; { TElTreeCombo }

implementation

type TElTreeComboButton = class(TCustomElGraphicButton)
     protected
       function GetThemedClassName: WideString; override;
       function GetThemePartID: Integer; override;
       function GetThemeStateID: Integer; override;
     end;

procedure TDropdownElTree.WMNCHitTest(var Msg : TWMNCHitTest);
var R : TRect;
begin

  R := BoundsRect;
  OffsetRect(R, -Left, -Top);
  if TElTreeCombo(FOwner).FSizeableTree then
  begin
    Msg.Result := HitTest(R, ScreenToClient(SmallPointToPoint(Msg.Pos)), (R.Bottom - R.Top) div 4, GetSystemMetrics(SM_CYEDGE));
    if (Msg.Result = HTBOTTOMLEFT) or
       (Msg.Result = HTLEFT) or
       (Msg.Result = HTTOPLEFT) or
       (Msg.Result = HTTOPRIGHT) or
       (Msg.Result = HTTOP) then
      Msg.Result := HTNOWHERE
    else
      inherited;
    InflateRect(R, -GetSystemMetrics(SM_CXEDGE), 0);
    Dec(R.Bottom, GetSystemMetrics(SM_CYEDGE));
    R.Top := R.Bottom - GetSystemMetrics(SM_CYHSCROLL);
    R.Left := R.Right - GetSystemMetrics(SM_CXVSCROLL);
    if PtInRect(R, ScreenToClient(SmallPointToPoint(Msg.Pos))) then
      Msg.Result := HTBOTTOMRIGHT;
  end
  else
    inherited;
end;

procedure TDropdownElTree.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := (Params.Style and not WS_CHILD) or WS_POPUP or WS_BORDER;
  if TElTreeCombo(FOwner).FSizeableTree then
    Params.ExStyle := WS_EX_TOOLWINDOW or WS_EX_STATICEDGE
  else
    Params.ExStyle := WS_EX_TOOLWINDOW;
  Params.WndParent := GetDesktopWindow;
end;

function TDropdownElTree.CanFocus: Boolean;
begin
  result := false;
end;

procedure TDropdownElTree.WMMouseActivate(var Msg: TMessage);
begin
  Msg.Result := MA_NOACTIVATE;
end;
    
procedure TDropdownElTree.WMExitSizeMove(var Message: TMessage);
var AForm : TCustomForm;
begin
  AForm := GetParentForm(FOwner);
  if (AForm <> nil) and (AForm.CanFocus) then
    AForm.SetFocus;

  Windows.SetFocus(FOwner.Handle);
  FOwner.IgnoreFocus := false;
  inherited;
end;

procedure TDropdownElTree.WMEnterSizeMove(var Message: TMessage);
begin
  inherited;
  FOwner.IgnoreFocus := true;
end;

procedure TDropdownElTree.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  inherited;
  if TElTreeCombo(FOwner).FSizeableTree then
    Dec(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(SM_CYHSCROLL));
end;

procedure TDropdownElTree.WMNCPaint(var Message: TMessage);
var DC : HDC;
    R  : TRect;
begin
  inherited;
  if TElTreeCombo(FOwner).FSizeableTree then
  begin
    R := BoundsRect;
    OffsetRect(R, -R.Left, -R.Top);
    InflateRect(R, -GetSystemMetrics(SM_CXEDGE), 0);
    Dec(R.Bottom, GetSystemMetrics(SM_CYEDGE));
    R.Top := R.Bottom - GetSystemMetrics(SM_CYHSCROLL);
    DC := GetWindowDC(Handle);
    if IsThemeApplied then
    begin
      FillRect(DC, R, HBRUSH(GetThemeSysColor(Theme, clBtnFace) + 1));
      R.Left := R.Right - GetSystemMetrics(SM_CXVSCROLL);
      DrawThemeBackgroundTo('STATUS', DC, SP_GRIPPER, 0, R, nil);
    end
    else
      DrawFrameControl(DC, R, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);

    ReleaseDC(Handle, DC);
  end;  
end;

procedure TDropdownElTree.UpdateScrollBars;
begin
  inherited;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TDropdownElTree.WMSetFocus(var Message: TMessage);
begin
  Windows.SetFocus(Message.wParam);
end;

procedure TDropdownElTree.CMParentFontChanged(var Message: TMessage);
begin
  // intentionally left blank
end;

function TElTreeCombo.GetSelection : TElTreeItem;
begin
  result := FSelection;
  // result := FTree.ItemFocused;
end; { GetSelection }

procedure TElTreeCombo.SetSelection(newValue : TElTreeItem);
{ Sets data member FSelection to newValue. }
begin
  if (Selection <> newValue) then
  begin
    FSelection := newValue;
    FTree.ItemFocused := FSelection;
    if (Selection <> nil) and AutoProcessSelection then
      Text := Selection.GetFullName(PathSeparator);
  end; { if }
end; { SetSelection }

function TElTreeCombo.GetSelectionList : TElList;
begin
  result := FSelectionList;
end; { GetSelectionList }

{ Inherited properties' Read/Write methods: }

procedure TElTreeCombo.SetAutoLineHeight_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's AutoLineHeight property to newValue. }
begin
  FTree.AutoLineHeight := newValue;
end; { SetAutoLineHeight_FTree }

function TElTreeCombo.GetAutoLineHeight_FTree : Boolean;
begin
  GetAutoLineHeight_FTree := FTree.AutoLineHeight;
end; { GetAutoLineHeight_FTree }

procedure TElTreeCombo.SetBkColor_FTree(newValue : TColor);
{ Sets the FTree subcomponent's BkColor property to newValue. }
begin
  FTree.BkColor := newValue;
end; { SetBkColor_FTree }

function TElTreeCombo.GetBkColor_FTree : TColor;
begin
  GetBkColor_FTree := FTree.BkColor;
end; { GetBkColor_FTree }

procedure TElTreeCombo.SetChangeStateImage_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's ChangeStateImage property to newValue. }
begin
  FTree.ChangeStateImage := newValue;
end; { SetChangeStateImage_FTree }

function TElTreeCombo.GetChangeStateImage_FTree : Boolean;
begin
  GetChangeStateImage_FTree := FTree.ChangeStateImage;
end; { GetChangeStateImage_FTree }

procedure TElTreeCombo.SetCtl3D_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's Ctl3D property to newValue. }
begin
  FTree.Ctl3D := newValue;
end; { SetCtl3D_FTree }

function TElTreeCombo.GetCtl3D_FTree : Boolean;
begin
  GetCtl3D_FTree := FTree.Ctl3D;
end; { GetCtl3D_FTree }

procedure TElTreeCombo.SetCursor(newValue : TCursor);
{ Sets the FTree subcomponent's Cursor property to newValue. }
begin
  FTree.Cursor := newValue;
end; { SetCursor }

function TElTreeCombo.GetCursor : TCursor;
begin
  GetCursor := FTree.Cursor;
end; { GetCursor }

procedure TElTreeCombo.SetDraggableSections(newValue : Boolean);
{ Sets the FTree subcomponent's DraggableSections property to newValue. }
begin
  FTree.DraggableSections := newValue;
end; { SetDraggableSections }

function TElTreeCombo.GetDraggableSections : Boolean;
begin
  GetDraggableSections := FTree.DraggableSections;
end; { GetDraggableSections }

procedure TElTreeCombo.SetFont_FTree(newValue : TFont);
{ Sets the FTree subcomponent's Font property to newValue. }
begin
  FTree.Font := newValue;
end; { SetFont_FTree }

function TElTreeCombo.GetFont_FTree : TFont;
begin
  GetFont_FTree := FTree.Font;
end; { GetFont_FTree }

procedure TElTreeCombo.SetHeaderHotTrack_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's HeaderHotTrack property to newValue. }
begin
  FTree.HeaderHotTrack := newValue;
end; { SetHeaderHotTrack_FTree }

function TElTreeCombo.GetHeaderHotTrack_FTree : Boolean;
begin
  GetHeaderHotTrack_FTree := FTree.HeaderHotTrack;
end; { GetHeaderHotTrack_FTree }

procedure TElTreeCombo.SetHeaderImages_FTree(newValue : TImageList);
{ Sets the FTree subcomponent's HeaderImages property to newValue. }
begin
  FTree.HeaderImages := newValue;
end; { SetHeaderImages_FTree }

function TElTreeCombo.GetHeaderImages_FTree : TImageList;
begin
  GetHeaderImages_FTree := FTree.HeaderImages;
end; { GetHeaderImages_FTree }

procedure TElTreeCombo.SetHeaderSections_FTree(newValue : TElHeaderSections);
{ Sets the FTree subcomponent's HeaderSections property to newValue. }
begin
  FTree.HeaderSections := newValue;
end; { SetHeaderSections_FTree }

function TElTreeCombo.GetHeaderSections_FTree : TElHeaderSections;
begin
  GetHeaderSections_FTree := FTree.HeaderSections;
end; { GetHeaderSections_FTree }

procedure TElTreeCombo.SetHideHintOnMove_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's HideHintOnMove property to newValue. }
begin
  FTree.HideHintOnMove := newValue;
end; { SetHideHintOnMove_FTree }

function TElTreeCombo.GetHideHintOnMove_FTree : Boolean;
begin
  GetHideHintOnMove_FTree := FTree.HideHintOnMove;
end; { GetHideHintOnMove_FTree }

procedure TElTreeCombo.SetHideHintOnTimer_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's HideHintOnTimer property to newValue. }
begin
  FTree.HideHintOnTimer := newValue;
end; { SetHideHintOnTimer_FTree }

function TElTreeCombo.GetHideHintOnTimer_FTree : Boolean;
begin
  GetHideHintOnTimer_FTree := FTree.HideHintOnTimer;
end; { GetHideHintOnTimer_FTree }

procedure TElTreeCombo.SetHideHorzScrollBar_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's HideHorzScrollBar property to newValue. }
begin
  FTree.HideHorzScrollBar := newValue;
end; { SetHideHorzScrollBar_FTree }

function TElTreeCombo.GetHideHorzScrollBar_FTree : Boolean;
begin
  GetHideHorzScrollBar_FTree := FTree.HideHorzScrollBar;
end; { GetHideHorzScrollBar_FTree }

procedure TElTreeCombo.SetHideVertScrollBar_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's HideVertScrollBar property to newValue. }
begin
  FTree.HideVertScrollBar := newValue;
end; { SetHideVertScrollBar_FTree }

function TElTreeCombo.GetHideVertScrollBar_FTree : Boolean;
begin
  GetHideVertScrollBar_FTree := FTree.HideVertScrollBar;
end; { GetHideVertScrollBar_FTree }

procedure TElTreeCombo.SetHorizontalLines_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's HorizontalLines property to newValue. }
begin
  FTree.HorizontalLines := newValue;
end; { SetHorizontalLines_FTree }

function TElTreeCombo.GetHorizontalLines_FTree : Boolean;
begin
  GetHorizontalLines_FTree := FTree.HorizontalLines;
end; { GetHorizontalLines_FTree }

procedure TElTreeCombo.SetImages_FTree(newValue : TImageList);
{ Sets the FTree subcomponent's Images property to newValue. }
begin
  FTree.Images := newValue;
end; { SetImages_FTree }

function TElTreeCombo.GetImages_FTree : TImageList;
begin
  GetImages_FTree := FTree.Images;
end; { GetImages_FTree }

procedure TElTreeCombo.SetItems_FTree(newValue : TElTreeItems);
{ Sets the FTree subcomponent's Items property to newValue. }
begin
  FTree.Items := newValue;
end; { SetItems_FTree }

function TElTreeCombo.GetItems_FTree : TElTreeItems;
begin
  GetItems_FTree := FTree.Items;
end; { GetItems_FTree }

procedure TElTreeCombo.SetLineHintMode(newValue : THintModes);
{ Sets the FTree subcomponent's LineHintMode property to newValue. }
begin
  FTree.LineHintMode := newValue;
end; { SetLineHintMode }

function TElTreeCombo.GetLineHintMode : THintModes;
begin
  GetLineHintMode := FTree.LineHintMode;
end; { GetLineHintMode }

procedure TElTreeCombo.SetMainTreeColumn(newValue : Integer);
{ Sets the FTree subcomponent's MainTreeColumn property to newValue. }
begin
  FTree.MainTreeColumn := newValue;
end; { SetMainTreeColumn }

function TElTreeCombo.GetMainTreeColumn : Integer;
begin
  GetMainTreeColumn := FTree.MainTreeColumn;
end; { GetMainTreeColumn }

procedure TElTreeCombo.SetMultiSelect(newValue : Boolean);
{ Sets the FTree subcomponent's MultiSelect property to newValue. }
begin
  FTree.MultiSelect := newValue;
end; { SetMultiSelect }

function TElTreeCombo.GetMultiSelect : Boolean;
begin
  GetMultiSelect := FTree.MultiSelect;
end; { GetMultiSelect }

procedure TElTreeCombo.SetOwnerDrawByColumn(newValue : Boolean);
{ Sets the FTree subcomponent's OwnerDrawByColumn property to newValue. }
begin
  FTree.OwnerDrawByColumn := newValue;
end; { SetOwnerDrawByColumn }

function TElTreeCombo.GetOwnerDrawByColumn : Boolean;
begin
  GetOwnerDrawByColumn := FTree.OwnerDrawByColumn;
end; { GetOwnerDrawByColumn }

procedure TElTreeCombo.SetOwnerDrawMask(newValue : string);
{ Sets the FTree subcomponent's OwnerDrawMask property to newValue. }
begin
  FTree.OwnerDrawMask := newValue;
end; { SetOwnerDrawMask }

function TElTreeCombo.GetOwnerDrawMask : string;
begin
  GetOwnerDrawMask := FTree.OwnerDrawMask;
end; { GetOwnerDrawMask }

procedure TElTreeCombo.SetParentCtl3D(newValue : Boolean);
{ Sets the FTree subcomponent's ParentCtl3D property to newValue. }
begin
  FTree.ParentCtl3D := newValue;
end; { SetParentCtl3D }

function TElTreeCombo.GetParentCtl3D : Boolean;
begin
  GetParentCtl3D := FTree.ParentCtl3D;
end; { GetParentCtl3D }

procedure TElTreeCombo.SetParentFont(newValue : Boolean);
{ Sets the FTree subcomponent's ParentFont property to newValue. }
begin
  FTree.ParentFont := newValue;
end; { SetParentFont }

function TElTreeCombo.GetParentFont : Boolean;
begin
  GetParentFont := FTree.ParentFont;
end; { GetParentFont }

procedure TElTreeCombo.SetParentShowHint(newValue : Boolean);
{ Sets the FTree subcomponent's ParentShowHint property to newValue. }
begin
  FTree.ParentShowHint := newValue;
end; { SetParentShowHint }

function TElTreeCombo.GetParentShowHint : Boolean;
begin
  GetParentShowHint := FTree.ParentShowHint;
end; { GetParentShowHint }

procedure TElTreeCombo.SetRowSelect(newValue : Boolean);
{ Sets the FTree subcomponent's RowSelect property to newValue. }
begin
  FTree.RowSelect := newValue;
end; { SetRowSelect }

function TElTreeCombo.GetRowSelect : Boolean;
begin
  GetRowSelect := FTree.RowSelect;
end; { GetRowSelect }

procedure TElTreeCombo.SetScrollTracking(newValue : Boolean);
{ Sets the FTree subcomponent's ScrollTracking property to newValue. }
begin
  FTree.ScrollTracking := newValue;
end; { SetScrollTracking }

function TElTreeCombo.GetScrollTracking : Boolean;
begin
  GetScrollTracking := FTree.ScrollTracking;
end; { GetScrollTracking }

procedure TElTreeCombo.SetSelectionMode(newValue : TSTSelModes);
{ Sets the FTree subcomponent's SelectionMode property to newValue. }
begin
  FTree.SelectionMode := newValue;
end; { SetSelectionMode }

function TElTreeCombo.GetSelectionMode : TSTSelModes;
begin
  GetSelectionMode := FTree.SelectionMode;
end; { GetSelectionMode }

procedure TElTreeCombo.SetShowButtons(newValue : Boolean);
{ Sets the FTree subcomponent's ShowButtons property to newValue. }
begin
  FTree.ShowButtons := newValue;
end; { SetShowButtons }

function TElTreeCombo.GetShowButtons : Boolean;
begin
  GetShowButtons := FTree.ShowButtons;
end; { GetShowButtons }

procedure TElTreeCombo.SetShowColumns(newValue : Boolean);
{ Sets the FTree subcomponent's ShowColumns property to newValue. }
begin
  FTree.ShowColumns := newValue;
end; { SetShowColumns }

function TElTreeCombo.GetShowColumns : Boolean;
begin
  GetShowColumns := FTree.ShowColumns;
end; { GetShowColumns }

procedure TElTreeCombo.SetShowHint(newValue : Boolean);
{ Sets the FTree subcomponent's ShowHint property to newValue. }
begin
  FTree.ShowHint := newValue;
end; { SetShowHint }

function TElTreeCombo.GetShowHint : Boolean;
begin
  GetShowHint := FTree.ShowHint;
end; { GetShowHint }

procedure TElTreeCombo.SetShowImages(newValue : Boolean);
{ Sets the FTree subcomponent's ShowImages property to newValue. }
begin
  FTree.ShowImages := newValue;
end; { SetShowImages }

function TElTreeCombo.GetShowImages : Boolean;
begin
  GetShowImages := FTree.ShowImages;
end; { GetShowImages }

procedure TElTreeCombo.SetShowLines(newValue : Boolean);
{ Sets the FTree subcomponent's ShowLines property to newValue. }
begin
  FTree.ShowLines := newValue;
end; { SetShowLines }

function TElTreeCombo.GetShowLines : Boolean;
begin
  GetShowLines := FTree.ShowLines;
end; { GetShowLines }

procedure TElTreeCombo.SetShowRoot(newValue : Boolean);
{ Sets the FTree subcomponent's ShowRoot property to newValue. }
begin
  FTree.ShowRoot := newValue;
end; { SetShowRoot }

function TElTreeCombo.GetShowRoot : Boolean;
begin
  GetShowRoot := FTree.ShowRoot;
end; { GetShowRoot }

procedure TElTreeCombo.SetSortDir(newValue : TSortDirs);
{ Sets the FTree subcomponent's SortDir property to newValue. }
begin
  FTree.SortDir := newValue;
end; { SetSortDir }

function TElTreeCombo.GetSortDir : TSortDirs;
begin
  GetSortDir := FTree.SortDir;
end; { GetSortDir }

procedure TElTreeCombo.SetSortMode(newValue : TSortModes);
{ Sets the FTree subcomponent's SortMode property to newValue. }
begin
  FTree.SortMode := newValue;
end; { SetSortMode }

function TElTreeCombo.GetSortMode : TSortModes;
begin
  GetSortMode := FTree.SortMode;
end; { GetSortMode }

procedure TElTreeCombo.SetSortSection(newValue : Integer);
{ Sets the FTree subcomponent's SortSection property to newValue. }
begin
  FTree.SortSection := newValue;
end; { SetSortSection }

function TElTreeCombo.GetSortSection : Integer;
begin
  GetSortSection := FTree.SortSection;
end; { GetSortSection }

procedure TElTreeCombo.SetSortType(newValue : TSortTypes);
{ Sets the FTree subcomponent's SortType property to newValue. }
begin
  FTree.SortType := newValue;
end; { SetSortType }

function TElTreeCombo.GetSortType : TSortTypes;
begin
  GetSortType := FTree.SortType;
end; { GetSortType }

procedure TElTreeCombo.SetTextColor(newValue : TColor);
{ Sets the FTree subcomponent's TextColor property to newValue. }
begin
  FTree.TextColor := newValue;
end; { SetTextColor }

function TElTreeCombo.GetTextColor : TColor;
begin
  GetTextColor := FTree.TextColor;
end; { GetTextColor }

procedure TElTreeCombo.SetTracking(newValue : Boolean);
{ Sets the FTree subcomponent's Tracking property to newValue. }
begin
  FTree.Tracking := newValue;
end; { SetTracking }

function TElTreeCombo.GetTracking : Boolean;
begin
  GetTracking := FTree.Tracking;
end; { GetTracking }

procedure TElTreeCombo.SetVerticalLines(newValue : Boolean);
{ Sets the FTree subcomponent's VerticalLines property to newValue. }
begin
  FTree.VerticalLines := newValue;
end; { SetVerticalLines }

function TElTreeCombo.GetVerticalLines : Boolean;
begin
  GetVerticalLines := FTree.VerticalLines;
end; { GetVerticalLines }

procedure TElTreeCombo.CompareItemsTransfer(Sender : TObject; Item1 : TElTreeItem; Item2 : TElTreeItem; var res : Integer);
begin
  if (assigned(FOnCompareItems)) then
    FOnCompareItems(Self, Item1, Item2, res);
end; { CompareItemsTransfer }

procedure TElTreeCombo.ClickHandler(Sender : TObject);
begin
  if FCloseOnClick then
  begin
    Dropped := false;
    ProcessSelect;
    TriggerNewTextSelectionEvent;
    SetFocus;
  end;
end;

procedure TElTreeCombo.DblClickHandler(Sender : TObject);
begin
  Dropped := false;
  ProcessSelect;
  TriggerNewTextSelectionEvent;
  SetFocus;
end; { DblClickHandler }

procedure TElTreeCombo.HeaderColumnClickTransfer(Sender : TObject; SectionIndex : Integer);
begin
  if (assigned(FOnHeaderColumnClick)) then
    FOnHeaderColumnClick(Self, SectionIndex);
end; { HeaderColumnClickTransfer }

procedure TElTreeCombo.HeaderColumnDrawTransfer(Sender : TCustomElHeader; Canvas : TCanvas; Section : TElHeaderSection; R : TRect; Pressed : Boolean);
begin
  if (assigned(FOnHeaderColumnDraw)) then
    FOnHeaderColumnDraw(FTree.HeaderSections.Owner, Canvas, Section, R, Pressed);
end; { HeaderColumnDrawTransfer }

procedure TElTreeCombo.HeaderColumnMoveTransfer(Sender : TCustomElTree; Section : TElHeaderSection; OldPos : Integer; NewPos : Integer);
begin
  if (assigned(FOnHeaderColumnMove)) then
    FOnHeaderColumnMove(FTree, Section, OldPos, NewPos);
end; { HeaderColumnMoveTransfer }

procedure TElTreeCombo.HeaderColumnResizeTransfer(Sender : TObject; SectionIndex : Integer);
begin
  if (assigned(FOnHeaderColumnResize)) then
    FOnHeaderColumnResize(FTree, SectionIndex);
end; { HeaderColumnResizeTransfer }

procedure TElTreeCombo.ItemChangeTransfer(Sender : TObject; Item : TElTreeItem; ItemChangeMode : TItemChangeMode);
begin
  if (assigned(FOnItemChange)) then
    FOnItemChange(Self, Item, ItemChangeMode);
end; { ItemChangeTransfer }

procedure TElTreeCombo.ItemCollapseTransfer(Sender : TObject; Item : TElTreeItem);
begin
  if (assigned(FOnItemCollapse)) then
    FOnItemCollapse(Self, Item);
end; { ItemCollapseTransfer }

procedure TElTreeCombo.ItemCollapsingTransfer(Sender : TObject; Item : TElTreeItem; var CanProcess : Boolean);
begin
  if (assigned(FOnItemCollapsing)) then
    FOnItemCollapsing(Self, Item, CanProcess);
end; { ItemCollapsingTransfer }

procedure TElTreeCombo.ItemDeletionTransfer(Sender : TObject; Item : TElTreeItem);
begin
  SelectionList.Remove(Item);
  if FSelection = Item then
    FSelection := nil;
  if (assigned(FOnItemDeletion)) then
    FOnItemDeletion(Self, Item);
end; { ItemDeletionTransfer }

procedure TElTreeCombo.ItemDrawTransfer(Sender : TObject; Item : TElTreeItem; Surface : TCanvas; R : TRect; SectionIndex : Integer);
begin
  if (assigned(FOnItemDraw)) then
    FOnItemDraw(Self, Item, Surface, R, SectionIndex);
end; { ItemDrawTransfer }

procedure TElTreeCombo.ItemExpandTransfer(Sender : TObject; Item : TElTreeItem);
begin
  if (assigned(FOnItemExpand)) then
    FOnItemExpand(Self, Item);
end; { ItemExpandTransfer }

procedure TElTreeCombo.ItemExpandingTransfer(Sender : TObject; Item : TElTreeItem; var CanProcess : Boolean);
begin
  if (assigned(FOnItemExpanding)) then
    FOnItemExpanding(Self, Item, CanProcess);
end; { ItemExpandingTransfer }

procedure TElTreeCombo.ItemFocusedTransfer(Sender : TObject);
begin
  if (assigned(FOnItemFocused)) then
    FOnItemFocused(Self);
end; { ItemFocusedTransfer }

procedure TElTreeCombo.ItemPicDrawTransfer(Sender : TObject; Item : TElTreeItem; var ImageIndex : Integer);
begin
  if (assigned(FOnItemPicDraw)) then
    FOnItemPicDraw(Self, Item, ImageIndex);
end; { ItemPicDrawTransfer }

procedure TElTreeCombo.KeyDownHandler(Sender : TObject; var Key : Word; Shift : TShiftState);
{ Handles the FTree OnKeyDown event. }
begin
  if Dropped and ((Key = VK_DOWN) or (Key = VK_UP)) and (ssAlt in Shift) then
  begin
    Dropped := false;
    exit;
  end;
  if (Key = VK_RETURN) and (Shift = []) then
  begin
    Key := 0;
    Dropped := false;
    ProcessSelect;
    exit;
  end;
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    Key := 0;
    Dropped := false;
    exit;
  end;
  TreeKeyDownTransfer(Self, Key, Shift);
end; { KeyDownHandler }

procedure TElTreeCombo.ShowLineHintTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; var Text : TElFString; HintWindow : THintWindow; MousePos : TPoint; var DoShowHint : Boolean);
begin
  if (assigned(FOnShowLineHint)) then
    FOnShowLineHint(Self, Item, Section, Text, HintWindow, MousePos, DoShowHint);
end; { ShowLineHintTransfer }

procedure TElTreeCombo.StartDrag_FTreeTransfer(Sender : TObject; var DragObject : TDragObject);
begin
  if (assigned(FOnStartDrag_FTree)) then
    FOnStartDrag_FTree(Self, DragObject);
end; { StartDrag_FTreeTransfer }

procedure TElTreeCombo.SetDropped(newValue : Boolean);
{ Sets data member FDropped to newValue. }
var
  P, P1: TPoint;
  PF   : TCustomForm;
begin
  if (FDropped <> newValue) then
  begin
    FDropped := newValue;
    TElGraphicButton(FButton).Down := FDropped;
    TriggerDropEvent(FDropped);
    if FDropped then
    begin
      //FTree.Parent := Self;
      if ReadOnly then
      begin
        SendMessage(Handle, EM_SETSEL, -1, -1);
        HideCaret(Handle);
      end;

      FSaveCursor := Cursor;
      Cursor := crArrow;
      PrepareSelection;
      //FTree.ParentWindow := GetDesktopWindow;
      if TreeParentFont then
        TreeFont.Assign(Font);

      FTree.Height := FDropHeight;

      if FDropWidth > 0 then
        FTree.Width := FDropWidth
      else
        FTree.Width := Width;

      P := Point(Left, Top + Height);
      P := Parent.ClientToScreen(P);
      IgnoreFocus := true;
      if P.Y + FTree.Height > Screen.Height then
        P.Y := P.Y - FTree.Height - Height;
      if AdjustDropDownPos then
      begin
        PF := GetParentForm(Self);
        P1 := Point(0, PF.ClientHeight);
        P1 := PF.ClientToScreen(P1);
        if P.Y + FTree.Height > P1.y then
        begin
          P1 := Point(0, 0);
          P1 := PF.ClientToScreen(P1);
          if P.Y - Height - FTree.Height >= P1.y then
          P.Y := P.Y - Height - FTree.Height;
        end;
      end;

      IgnoreDrop := true;
      SetWindowPos(FTree.Handle, HWND_TOPMOST, P.x, P.y, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW );
      SetFocus;

      inherited ReadOnly := True;
      IgnoreFocus := false;
      IgnoreDrop := false;
    end
    else
    begin
      Cursor := FSaveCursor;
      IgnoreDrop := true;
      FDropHeight := FTree.Height;
      FDropWidth := FTree.Width;
      inherited ReadOnly := ReadOnly;
      IgnoreFocus := true;
      //FTree.Visible := false;

      if FTree.HandleAllocated then
        SetWindowPos(FTree.Handle, HWND_NOTOPMOST, 0,0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_HIDEWINDOW or SWP_NOACTIVATE);
                          
      IgnoreFocus := false;
      IgnoreDrop := false;
    end;
  end; { if }
end; { SetDropped }

procedure TElTreeCombo.TriggerDropEvent(Dropped : boolean);
begin
  if (assigned(FOnDrop)) then FOnDrop(Self, Dropped);
end; { TriggerDropEvent }

procedure TElTreeCombo.KeyPress(var Key : Char); { protected }
begin
  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
  begin
    if Dropped then
    begin
      Dropped := false;
      Key := #0;
      exit;
    end;
  end;
  inherited KeyPress(Key);
end; { KeyPress }

procedure TElTreeCombo.ButtonClick(Sender : TObject);
begin
  if not (csDesigning in ComponentState) then
    Dropped := not Dropped;
end;

{ Exposed properties' Read/Write methods: }

procedure TElTreeCombo.SetLineHeight_FTree(newValue : Integer);
{ Sets the FTree subcomponent's LineHeight property to newValue. }
begin
  FTree.LineHeight := newValue;
end; { SetLineHeight_FTree }

function TElTreeCombo.GetLineHeight_FTree : Integer;
begin
  GetLineHeight_FTree := FTree.LineHeight;
end; { GetLineHeight_FTree }

function TElTreeCombo.GetReadOnly : Boolean;
begin
  result := FReadOnly;
end; { GetReadOnly }

procedure TElTreeCombo.SetReadOnly(newValue : Boolean);
{ Sets data member FReadOnly to newValue. }
begin
  if (FReadOnly <> newValue) then
  begin
    if NewValue then
    begin
      FSaveCursor1 := Cursor;
      if Dropped then
        FSaveCursor := crArrow
      else
        Cursor := crArrow;
    end
    else
    begin
      if Dropped then
        FSaveCursor := FSaveCursor1
      else
        Cursor := FSaveCursor1;
    end;
    if not NewValue then
    begin
      if FMouseDown then
      begin
        if FSaveCapture <> 0 then
          SetCapture(FSaveCapture)
        else
          ReleaseCapture;
      end;
      FMouseDown := false;
    end;
    FReadOnly := newValue;
    inherited ReadOnly := FReadOnly;
  end; { if }
end; { SetReadOnly }

procedure TElTreeCombo.CMEnter(var Msg : TCMEnter); { private }
begin
  inherited;
  if not IgnoreFocus then
    Dropped := false;
end; { CMEnter }

procedure TElTreeCombo.CMExit(var Msg : TCMExit); { private }
begin
  inherited;
  if not IgnoreFocus then
    Dropped := false;
end; { CMExit }

procedure TElTreeCombo.ProcessSelect;
begin
  if (not MultiSelect) then
    Selection := FTree.ItemFocused
  else
    FTree.AllSelected(FSelectionList);
end;

{ Exposed properties' Read/Write methods: }

procedure TElTreeCombo.SetAlwaysKeepSelection_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's AlwaysKeepSelection property to newValue. }
begin
  FTree.AlwaysKeepSelection := newValue;
end; { SetAlwaysKeepSelection_FTree }

function TElTreeCombo.GetAlwaysKeepSelection_FTree : Boolean;
begin
  GetAlwaysKeepSelection_FTree := FTree.AlwaysKeepSelection;
end; { GetAlwaysKeepSelection_FTree }

procedure TElTreeCombo.SetAutoExpand_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's AutoExpand property to newValue. }
begin
  FTree.AutoExpand := newValue;
end; { SetAutoExpand_FTree }

function TElTreeCombo.GetAutoExpand_FTree : Boolean;
begin
  GetAutoExpand_FTree := FTree.AutoExpand;
end; { GetAutoExpand_FTree }

procedure TElTreeCombo.SetAutoLookup_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's AutoLookup property to newValue. }
begin
  FTree.AutoLookup := newValue;
end; { SetAutoLookup_FTree }

function TElTreeCombo.GetAutoLookup_FTree : Boolean;
begin
  GetAutoLookup_FTree := FTree.AutoLookup;
end; { GetAutoLookup_FTree }

procedure TElTreeCombo.SetBarStyle_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's BarStyle property to newValue. }
begin
  FTree.BarStyle := newValue;
end; { SetBarStyle_FTree }

function TElTreeCombo.GetBarStyle_FTree : Boolean;
begin
  GetBarStyle_FTree := FTree.BarStyle;
end; { GetBarStyle_FTree }

procedure TElTreeCombo.SetCustomPlusMinus_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's CustomPlusMinus property to newValue. }
begin
  FTree.CustomPlusMinus := newValue;
end; { SetCustomPlusMinus_FTree }

function TElTreeCombo.GetCustomPlusMinus_FTree : Boolean;
begin
  GetCustomPlusMinus_FTree := FTree.CustomPlusMinus;
end; { GetCustomPlusMinus_FTree }

procedure TElTreeCombo.SetDeselectChildrenOnCollapse_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's DeselectChildrenOnCollapse property to newValue. }
begin
  FTree.DeselectChildrenOnCollapse := newValue;
end; { SetDeselectChildrenOnCollapse_FTree }

function TElTreeCombo.GetDeselectChildrenOnCollapse_FTree : Boolean;
begin
  GetDeselectChildrenOnCollapse_FTree := FTree.DeselectChildrenOnCollapse;
end; { GetDeselectChildrenOnCollapse_FTree }

procedure TElTreeCombo.SetTreeDragImageMode(newValue : TDragImgMode);
{ Sets the FTree subcomponent's DragImageMode property to newValue. }
begin
  FTree.DragImageMode := newValue;
end; { SetTreeDragImageMode }

function TElTreeCombo.GetTreeDragImageMode : TDragImgMode;
begin
  GetTreeDragImageMode := FTree.DragImageMode;
end; { GetTreeDragImageMode }

procedure TElTreeCombo.SetTreeDrawFocusRect(newValue : Boolean);
{ Sets the FTree subcomponent's DrawFocusRect property to newValue. }
begin
  FTree.DrawFocusRect := newValue;
end; { SetTreeDrawFocusRect }

function TElTreeCombo.GetTreeDrawFocusRect : Boolean;
begin
  GetTreeDrawFocusRect := FTree.DrawFocusRect;
end; { GetTreeDrawFocusRect }

procedure TElTreeCombo.SetTreeFullRowSelect(newValue : Boolean);
{ Sets the FTree subcomponent's FullRowSelect property to newValue. }
begin
  FTree.FullRowSelect := newValue;
end; { SetTreeFullRowSelect }

function TElTreeCombo.GetTreeFullRowSelect : Boolean;
begin
  GetTreeFullRowSelect := FTree.FullRowSelect;
end; { GetTreeFullRowSelect }

procedure TElTreeCombo.SetTreeImages2(newValue : TImageList);
{ Sets the FTree subcomponent's Images2 property to newValue. }
begin
  FTree.Images2 := newValue;
end; { SetTreeImages2 }

function TElTreeCombo.GetTreeImages2 : TImageList;
begin
  GetTreeImages2 := FTree.Images2;
end; { GetTreeImages2 }

procedure TElTreeCombo.SetMinusPicture_FTree(newValue : TBitmap);
{ Sets the FTree subcomponent's MinusPicture property to newValue. }
begin
  FTree.MinusPicture := newValue;
end; { SetMinusPicture_FTree }

function TElTreeCombo.GetMinusPicture_FTree : TBitmap;
begin
  GetMinusPicture_FTree := FTree.MinusPicture;
end; { GetMinusPicture_FTree }

procedure TElTreeCombo.SetPlusPicture_FTree(newValue : TBitmap);
{ Sets the FTree subcomponent's PlusPicture property to newValue. }
begin
  FTree.PlusPicture := newValue;
end; { SetPlusPicture_FTree }

function TElTreeCombo.GetPlusPicture_FTree : TBitmap;
begin
  GetPlusPicture_FTree := FTree.PlusPicture;
end; { GetPlusPicture_FTree }

procedure TElTreeCombo.SetShowCheckboxes_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's ShowCheckboxes property to newValue. }
begin
  FTree.ShowCheckboxes := newValue;
end; { SetShowCheckboxes_FTree }

function TElTreeCombo.GetShowCheckboxes_FTree : Boolean;
begin
  GetShowCheckboxes_FTree := FTree.ShowCheckboxes;
end; { GetShowCheckboxes_FTree }

procedure TElTreeCombo.SetStickyHeaderSections_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's StickyHeaderSections property to newValue. }
begin
  FTree.StickyHeaderSections := newValue;
end; { SetStickyHeaderSections_FTree }

function TElTreeCombo.GetStickyHeaderSections_FTree : Boolean;
begin
  GetStickyHeaderSections_FTree := FTree.StickyHeaderSections;
end; { GetStickyHeaderSections_FTree }

{$ifdef SUPPORT_STORAGE}
procedure TElTreeCombo.SetStoragePath_FTree(newValue : string);
{ Sets the FTree subcomponent's StoragePath property to newValue. }
begin
  FTree.StoragePath := newValue;
end; { SetStoragePath_FTree }

function TElTreeCombo.GetStoragePath_FTree : string;
begin
  GetStoragePath_FTree := FTree.StoragePath;
end; { GetStoragePath_FTree }
{$endif}

procedure TElTreeCombo.CellStyleLoad_FTreeTransfer(Sender : TObject; Stream : TStream; Style : TElCellStyle);
begin
  if (assigned(FOnCellStyleLoad_FTree)) then
    FOnCellStyleLoad_FTree(Self, Stream, Style);
end; { CellStyleLoad_FTreeTransfer }

procedure TElTreeCombo.CellStyleSave_FTreeTransfer(Sender : TObject; Stream : TStream; Style : TElCellStyle);
begin
  if (assigned(FOnCellStyleSave_FTree)) then
    FOnCellStyleSave_FTree(Self, Stream, Style);
end; { CellStyleSave_FTreeTransfer }

procedure TElTreeCombo.ComboEditShow_FTreeTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; Combobox : TComboBox);
begin
  if (assigned(FOnComboEditShow_FTree)) then
    FOnComboEditShow_FTree(Self, Item, Section, Combobox);
end; { ComboEditShow_FTreeTransfer }

procedure TElTreeCombo.HeaderLookup_FTreeTransfer(Sender : TObject; Section : TElHeaderSection; var Text : string);
begin
  if (assigned(FOnHeaderLookup_FTree)) then
    FOnHeaderLookup_FTree(Self, Section, Text);
end; { HeaderLookup_FTreeTransfer }

procedure TElTreeCombo.HeaderLookupDone_FTreeTransfer(Sender : TObject; Section : TElHeaderSection; Text : string; Accepted : Boolean);
begin
  if (assigned(FOnHeaderLookupDone_FTree)) then
    FOnHeaderLookupDone_FTree(Self, Section, Text, Accepted);
end; { HeaderLookupDone_FTreeTransfer }

procedure TElTreeCombo.HeaderResize_FTreeTransfer(Sender : TObject);
begin
  if (assigned(FOnHeaderResize_FTree)) then
    FOnHeaderResize_FTree(Self);
end; { HeaderResize_FTreeTransfer }

procedure TElTreeCombo.HeaderSectionCollapse_FTreeTransfer(Sender : TObject; Section : TElHeaderSection);
begin
  if (assigned(FOnHeaderSectionCollapse_FTree)) then
    FOnHeaderSectionCollapse_FTree(Self, Section);
end; { HeaderSectionCollapse_FTreeTransfer }

procedure TElTreeCombo.HeaderSectionExpand_FTreeTransfer(Sender : TObject; Section : TElHeaderSection);
begin
  if (assigned(FOnHeaderSectionExpand_FTree)) then
    FOnHeaderSectionExpand_FTree(Self, Section);
end; { HeaderSectionExpand_FTreeTransfer }

procedure TElTreeCombo.HotTrack_FTreeTransfer(Sender : TObject; OldItem : TElTreeItem; NewItem : TElTreeItem);
begin
  if (assigned(FOnHotTrack_FTree)) then
    FOnHotTrack_FTree(Self, OldItem, NewItem);
end; { HotTrack_FTreeTransfer }

procedure TElTreeCombo.ItemLoad_FTreeTransfer(Sender : TObject; Stream : TStream; Item : TElTreeItem);
begin
  if (assigned(FOnItemLoad_FTree)) then
    FOnItemLoad_FTree(Self, Stream, Item);
end; { ItemLoad_FTreeTransfer }

procedure TElTreeCombo.ItemPicDraw2_FTreeTransfer(Sender : TObject; Item : TElTreeItem; var ImageIndex : Integer);
begin
  if (assigned(FOnItemPicDraw2_FTree)) then
    FOnItemPicDraw2_FTree(Self, Item, ImageIndex);
end; { ItemPicDraw2_FTreeTransfer }

procedure TElTreeCombo.ItemSave_FTreeTransfer(Sender : TObject; Stream : TStream; Item : TElTreeItem);
begin
  if (assigned(FOnItemSave_FTree)) then
    FOnItemSave_FTree(Self, Stream, Item);
end; { ItemSave_FTreeTransfer }

procedure TElTreeCombo.ItemSelectedChange_FTreeTransfer(Sender : TObject; Item : TElTreeItem);
begin
  if (assigned(FOnItemSelectedChange_FTree)) then
    FOnItemSelectedChange_FTree(Self, Item);
end; { ItemSelectedChange_FTreeTransfer }

{$IFDEF ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF B_3}
{$IFNDEF VER90}
procedure TElTreeCombo.OleDragFinish_FTreeTransfer(Sender : TObject; dwEffect : TDragType; Result : HResult);
begin
  if (assigned(FOnOleDragFinish_FTree)) then
    FOnOleDragFinish_FTree(Self, dwEffect, Result);
end; { OleDragFinish_FTreeTransfer }

procedure TElTreeCombo.OleDragStart_FTreeTransfer(Sender : TObject; var dataObj : IDataObject; var dropSource : IDropSource; var dwOKEffects : TDragTypes);
begin
  if (assigned(FOnOleDragStart_FTree)) then
    FOnOleDragStart_FTree(Self, dataObj, dropSource, dwOKEffects);
end; { OleDragStart_FTreeTransfer }

procedure TElTreeCombo.OleTargetDrag_FTreeTransfer(Sender : TObject; State : TDragState; Source : TOleDragObject; Shift : TShiftState; X : Integer; Y : Integer; var DragType : TDragType);
begin
  if (assigned(FOnOleTargetDrag_FTree)) then
    FOnOleTargetDrag_FTree(Self, State, Source, Shift, X, Y, DragType);
end; { OleTargetDrag_FTreeTransfer }

procedure TElTreeCombo.OleTargetDrop_FTreeTransfer(Sender : TObject; Source : TOleDragObject; Shift : TShiftState; X : Integer; Y : Integer; var DragType : TDragType);
begin
  if (assigned(FOnOleTargetDrop_FTree)) then
    FOnOleTargetDrop_FTree(Self, Source, Shift, X, Y, DragType);
end; { OleTargetDrop_FTreeTransfer }
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$ifdef VER3_EDITORS}
procedure TElTreeCombo.TryEdit_FTreeTransfer(Sender : TObject; Item : TElTreeItem; SectionIndex : integer; var CellType : TElFieldType; var CanEdit : Boolean);
{$else}
procedure TElTreeCombo.TryEdit_FTreeTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; var CellType : TElFieldType; var CanEdit : Boolean);
{$endif}
begin
  if (assigned(FOnTryEdit_FTree)) then
{$ifndef VER3_EDITORS}
    FOnTryEdit_FTree(Self, Item, Section, CellType, CanEdit);
{$else}
    FOnTryEdit_FTree(Self, Item, SectionIndex, CellType, CanEdit);
{$endif}
end; { TryEdit_FTreeTransfer }

procedure TElTreeCombo.ValidateCombo_FTreeTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; Combo : TComboBox; var Accept : Boolean);
begin
  if (assigned(FOnValidateCombo_FTree)) then
    FOnValidateCombo_FTree(Self, Item, Section, Combo, Accept);
end; { ValidateCombo_FTreeTransfer }

procedure TElTreeCombo.ValidateInplaceEdit_FTreeTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection; var Text : string; var Accept : Boolean);
begin
  if (assigned(FOnValidateInplaceEdit_FTree)) then
    FOnValidateInplaceEdit_FTree(Self, Item, Section, Text, Accept);
end; { ValidateInplaceEdit_FTreeTransfer }

destructor TElTreeCombo.Destroy;
begin
  FSelectionList.Free;
  FTree.Free;
  inherited Destroy;
end; { Destroy }

constructor TElTreeCombo.Create(AOwner : TComponent);
begin
  ButtonClass := TElTreeComboButton;
  inherited Create(AOwner);
  FDropHeight := 80;
  FSelectionList := TElList.Create;
  FAdjustDropDownPos := true;

  OnButtonClick := ButtonClick;

  TElTreeComboButton(FButton).Glyph.LoadFromResourceName(HInstance, 'DROPDOWNGLYPH');
  TElTreeComboButton(FButton).ShowText := false;

  // TElTreeComboButton(FButton).Glyph.SaveToFile('e:\arrow.bmp');

  FAutoProcessSelection := true;

  AltButtonVisible := false;
  ButtonFlat := false;
  ButtonWidth := 15;
  ButtonColor := clBtnFace;
  FDropWidth := Width;
  FSizeableTree := true;
  TElGraphicButton(FButton).Cursor := crArrow;
  TElGraphicButton(FButton).IsSwitch := true;
  FTree := TDropDownElTree(CreateTree);
  with FTree do
  begin
    FOwner := Self;
    ControlStyle := ControlStyle - [csCaptureMouse];
    //ParentWindow := GetDesktopWindow;
    //
    // Parent := Self;
    Visible := false;

    BorderStyle := bsSingle;
    Align := alNone;
    Tracking := false;
    FTree.DoInplaceEdit := false;
    MultiSelect := false;
    TreeHideSelection := false;
    Flat := false;

    OnCompareItems := CompareItemsTransfer;
    OnDblClick := DblClickHandler;
    OnClick    := ClickHandler;
    OnHeaderColumnClick := HeaderColumnClickTransfer;
    OnHeaderColumnDraw := HeaderColumnDrawTransfer;
    OnHeaderColumnMove := HeaderColumnMoveTransfer;
    OnHeaderColumnResize := HeaderColumnResizeTransfer;
    OnItemChange := ItemChangeTransfer;
    OnItemCollapse := ItemCollapseTransfer;
    OnItemCollapsing := ItemCollapsingTransfer;
    OnItemDeletion := ItemDeletionTransfer;
    OnItemDraw := ItemDrawTransfer;
    OnItemExpand := ItemExpandTransfer;
    OnItemExpanding := ItemExpandingTransfer;
    OnItemFocused := ItemFocusedTransfer;
    OnItemPicDraw := ItemPicDrawTransfer;

    OnKeyDown := KeyDownHandler;
    OnKeyPress := TreeKeyPressTransfer;
    OnKeyUp := TreeKeyUpTransfer;
    OnMouseDown := TreeMouseDownTransfer;
    OnMouseMove := TreeMouseMoveTransfer;
    OnMouseUp := TreeMouseUpTransfer;
    OnEditRequest := EditRequestTransfer;

    OnShowLineHint := ShowLineHintTransfer;
    OnStartDrag := StartDrag_FTreeTransfer;
    OnCellStyleLoad := CellStyleLoad_FTreeTransfer;
    OnCellStyleSave := CellStyleSave_FTreeTransfer;
    OnComboEditShow := ComboEditShow_FTreeTransfer;
    OnHeaderLookup := HeaderLookup_FTreeTransfer;
    OnHeaderLookupDone := HeaderLookupDone_FTreeTransfer;
    OnHeaderResize := HeaderResize_FTreeTransfer;
    OnHeaderSectionAutoSize := HeaderSectionAutoSize_FTreeTransfer;
    OnHeaderSectionCollapse := HeaderSectionCollapse_FTreeTransfer;
    OnHeaderSectionExpand := HeaderSectionExpand_FTreeTransfer;
    OnHeaderSectionFilterCall := HeaderSectionFilterCall_FTreeTransfer;
    OnHorzScrollDrawPart := HorzScrollDrawPart_FTreeTransfer;
    OnHorzScrollHintNeeded := HorzScrollHintNeeded_FTreeTransfer;
    OnHotTrack := HotTrack_FTreeTransfer;
    OnItemLoad := ItemLoad_FTreeTransfer;
    OnItemPicDraw2 := ItemPicDraw2_FTreeTransfer;
    OnItemSave := ItemSave_FTreeTransfer;
    OnItemSelectedChange := ItemSelectedChange_FTreeTransfer;

{$IFDEF ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF B_3}
{$IFNDEF VER90}
    OnOleDragFinish := OleDragFinish_FTreeTransfer;
    OnOleDragStart := OleDragStart_FTreeTransfer;
    OnOleTargetDrag := OleTargetDrag_FTreeTransfer;
    OnOleTargetDrop := OleTargetDrop_FTreeTransfer;
{$ENDIF}
{$ENDIF}
{$ENDIF}
    OnResize := TreeResizeTransfer;
    OnScroll := ScrollTransfer;
    OnTryEdit := TryEdit_FTreeTransfer;
    OnValidateCombo := ValidateCombo_FTreeTransfer;
    OnValidateInplaceEdit := ValidateInplaceEdit_FTreeTransfer;
    OnVertScrollDrawPart := VertScrollDrawPart_FTreeTransfer;
    OnVertScrollHintNeeded := VertScrollHintNeeded_FTreeTransfer;

  end; { FTree }
  FReadOnly := False;
end; { Create }

procedure TElTreeCombo.Assign(Source : TPersistent); { public }
begin
  if Source is TCustomElTree then
    FTree.Assign(Source as TCustomElTree)
  else
    inherited Assign(Source);
end; { Assign }

procedure TElTreeCombo.PrepareSelection; { private }

type TSearchRec = record
       Text : String;
       Separator : string;
     end;
     PSearchRec = ^TSearchRec;

var
  i : integer;
  SRec : TSearchRec;

  procedure IntSelect(Item : TElTreeItem; Index : integer; var ContinueIterate : boolean;
    IterateData : pointer; Tree : TCustomElTree);
  begin
    if Item.GetFullName(PSearchRec(IterateData).Separator) = PSearchRec(IterateData).Text then
    begin
      ContinueIterate := false;
      Tree.ItemFocused := Item;
      Item.Selected := true;
    end;
  end;

begin
  if MultiSelect then
  begin
    FTree.Items.BeginUpdate;
    FTree.DeselectAll;
    for i := 0 to FSelectionList.Count - 1 do // Iterate
      TElTreeItem(FSelectionList[i]).Selected := true;
    FTree.Items.EndUpdate;
  end
  else
  begin
    SRec.Separator := PathSeparator;
    SRec.Text := Text;
    FTree.ItemFocused := FSelection;
    if FTree.ItemFocused = nil then
      FTree.Items.Iterate(false, false, @IntSelect, @SRec);
  end;
end; { PrepareSelection }

procedure TElTreeCombo.SetDropWidth(newValue : Integer);
begin
  if (FDropWidth <> newValue) then
  begin
    FDropWidth := newValue;
    if FTree.Visible then
      FTree.Width := newValue;
  end; {if}
end;

procedure TElTreeCombo.SetDropHeight(newValue : Integer);
begin
  if (FDropHeight <> newValue) then
  begin
    FDropHeight := newValue;
    if FTree.Visible then
      FTree.Height := newValue;
  end; {if}
end;

procedure TElTreeCombo.SetPathSeparator(newValue : Char);
begin
  FTree.PathSeparator := newValue;
end;

function TElTreeCombo.GetPathSeparator : Char;
begin
  GetPathSeparator := FTree.PathSeparator;
end;

procedure TElTreeCombo.TriggerNewTextSelectionEvent;
{ Triggers the OnNewTextSelection event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnNewTextSelection)) then
    FOnNewTextSelection(Self);
end; { TriggerNewTextSelectionEvent }

procedure TElTreeCombo.KeyDown(var Key : Word; Shift : TShiftState); { protected }
begin
  if (Key = VK_DOWN) and (ssAlt in Shift) then
    Dropped := not Dropped
  else
    inherited;
end; { KeyDown }

function TElTreeCombo.GetDrawFocusRect_FTree : Boolean;
begin
  GetDrawFocusRect_FTree := FTree.DrawFocusRect;
end; { GetDrawFocusRect_FTree }

procedure TElTreeCombo.SetFilteredVisibility_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's FilteredVisibility property to newValue. }
begin
  FTree.FilteredVisibility := newValue;
end; { SetFilteredVisibility_FTree }

function TElTreeCombo.GetFilteredVisibility_FTree : Boolean;
begin
  GetFilteredVisibility_FTree := FTree.FilteredVisibility;
end; { GetFilteredVisibility_FTree }

procedure TElTreeCombo.SetRightAlignedText_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's RightAlignedText property to newValue. }
begin
  FTree.RightAlignedText := newValue;
end; { SetRightAlignedText_FTree }

function TElTreeCombo.GetRightAlignedText_FTree : Boolean;
begin
  GetRightAlignedText_FTree := FTree.RightAlignedText;
end; { GetRightAlignedText_FTree }

procedure TElTreeCombo.SetRightAlignedTree_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's RightAlignedTree property to newValue. }
begin
  FTree.RightAlignedTree := newValue;
end; { SetRightAlignedTree_FTree }

function TElTreeCombo.GetRightAlignedTree_FTree : Boolean;
begin
  GetRightAlignedTree_FTree := FTree.RightAlignedTree;
end; { GetRightAlignedTree_FTree }

{ Exposed properties' Read/Write methods: }

procedure TElTreeCombo.SetCheckBoxGlyph_FTree(newValue : TBitmap);
{ Sets the FTree subcomponent's CheckBoxGlyph property to newValue. }
begin
  FTree.CheckBoxGlyph := newValue;
end; { SetCheckBoxGlyph_FTree }

function TElTreeCombo.GetCheckBoxGlyph_FTree : TBitmap;
begin
  GetCheckBoxGlyph_FTree := FTree.CheckBoxGlyph;
end; { GetCheckBoxGlyph_FTree }

procedure TElTreeCombo.SetCustomCheckboxes_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's CustomCheckboxes property to newValue. }
begin
  FTree.CustomCheckboxes := newValue;
end; { SetCustomCheckboxes_FTree }

function TElTreeCombo.GetCustomCheckboxes_FTree : Boolean;
begin
  GetCustomCheckboxes_FTree := FTree.CustomCheckboxes;
end; { GetCustomCheckboxes_FTree }

procedure TElTreeCombo.SetRadioButtonGlyph_FTree(newValue : TBitmap);
{ Sets the FTree subcomponent's RadioButtonGlyph property to newValue. }
begin
  FTree.RadioButtonGlyph := newValue;
end; { SetRadioButtonGlyph_FTree }

function TElTreeCombo.GetRadioButtonGlyph_FTree : TBitmap;
begin
  GetRadioButtonGlyph_FTree := FTree.RadioButtonGlyph;
end; { GetRadioButtonGlyph_FTree }

procedure TElTreeCombo.SetUnderlineTracked_FTree(newValue : Boolean);
{ Sets the FTree subcomponent's UnderlineTracked property to newValue. }
begin
  FTree.UnderlineTracked := newValue;
end; { SetUnderlineTracked_FTree }

function TElTreeCombo.GetUnderlineTracked_FTree : Boolean;
begin
  GetUnderlineTracked_FTree := FTree.UnderlineTracked;
end; { GetUnderlineTracked_FTree }

procedure TElTreeCombo.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); { protected }
begin
  inherited;
  if (Button = mbLeft) and (ReadOnly) then
    Dropped := not Dropped;
end; { MouseDown }

{ Exposed properties' Read/Write methods: }

procedure TElTreeCombo.SetDoInplaceEdit(newValue : Boolean);
{ Sets the FTree subcomponent's DoInplaceEdit property to newValue. }
begin
  FTree.DoInplaceEdit := newValue;
end; { SetDoInplaceEdit }

function TElTreeCombo.GetDoInplaceEdit : Boolean;
begin
  GetDoInplaceEdit := FTree.DoInplaceEdit;
end; { GetDoInplaceEdit }

procedure TElTreeCombo.TreeKeyDownTransfer(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  if (assigned(FOnTreeKeyDown)) then
    FOnTreeKeyDown(Self, Key, Shift);
end; { TreeKeyDownTransfer }

procedure TElTreeCombo.TreeKeyPressTransfer(Sender : TObject; var Key : Char);
begin
  if (assigned(FOnTreeKeyPress)) then
    FOnTreeKeyPress(Self, Key);
end; { TreeKeyPressTransfer }

procedure TElTreeCombo.TreeKeyUpTransfer(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  if (assigned(FOnTreeKeyUp)) then
    FOnTreeKeyUp(Self, Key, Shift);
end; { TreeKeyUpTransfer }

procedure TElTreeCombo.TreeMouseDownTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer);
begin
  if (assigned(FOnTreeMouseDown)) then
    FOnTreeMouseDown(Self, Button, Shift, X, Y);
end; { TreeMouseDownTransfer }

procedure TElTreeCombo.TreeMouseMoveTransfer(Sender : TObject; Shift : TShiftState; X : Integer; Y : Integer);
begin
  if (assigned(FOnTreeMouseMove)) then
    FOnTreeMouseMove(Self, Shift, X, Y);
end; { TreeMouseMoveTransfer }

procedure TElTreeCombo.TreeMouseUpTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer);
begin
  if (assigned(FOnTreeMouseUp)) then
    FOnTreeMouseUp(Self, Button, Shift, X, Y);
end; { TreeMouseUpTransfer }

procedure TElTreeCombo.SetShowRootButtons(newValue : Boolean);
begin
  FTree.ShowRootButtons := newValue;
end; { SetShowRootButtons }

function TElTreeCombo.GetShowRootButtons : Boolean;
begin
  GetShowRootButtons := FTree.ShowRootButtons;
end; { GetShowRootButtons }

function TElTreeCombo.GetTree : TElTree; { public }
begin
  result := TElTree(FTree);
end; { GetTree }

procedure TElTreeCombo.EditRequestTransfer(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection);
begin
  if (assigned(FOnEditRequest)) then
    FOnEditRequest(Self, Item, Section);
end; { EditRequestTransfer }

procedure TElTreeCombo.SetTreeFlat(newValue : Boolean);
begin
  FTree.Flat := newValue;
end; { SetTreeFlat }

function TElTreeCombo.GetTreeFlat : Boolean;
begin
  GetTreeFlat := FTree.Flat;
end; { GetTreeFlat }

procedure TElTreeCombo.SetTreeHeaderActiveFilterColor(newValue : TColor);
begin
  FTree.HeaderActiveFilterColor := newValue;
end; { SetTreeHeaderActiveFilterColor }

function TElTreeCombo.GetTreeHeaderActiveFilterColor : TColor;
begin
  GetTreeHeaderActiveFilterColor := FTree.HeaderActiveFilterColor;
end; { GetTreeHeaderActiveFilterColor }

procedure TElTreeCombo.SetTreeHeaderFilterColor(newValue : TColor);
begin
  FTree.HeaderFilterColor := newValue;
end; { SetTreeHeaderFilterColor }

function TElTreeCombo.GetTreeHeaderFilterColor : TColor;
begin
  GetTreeHeaderFilterColor := FTree.HeaderFilterColor;
end; { GetTreeHeaderFilterColor }

procedure TElTreeCombo.SetTreeHeaderFlat(newValue : Boolean);
begin
  FTree.HeaderFlat := newValue;
end; { SetTreeHeaderFlat }

function TElTreeCombo.GetTreeHeaderFlat : Boolean;
begin
  GetTreeHeaderFlat := FTree.HeaderFlat;
end; { GetTreeHeaderFlat }

procedure TElTreeCombo.SetAutoResizeColumns(newValue : Boolean);
begin
  FTree.AutoResizeColumns := newValue;
end;  { SetAutoResizeColumns }

function TElTreeCombo.GetAutoResizeColumns : Boolean;
begin
  GetAutoResizeColumns := FTree.AutoResizeColumns;
end;  { GetAutoResizeColumns }

procedure TElTreeCombo.SetTreeBackground(newValue : TBitmap);
begin
  FTree.Background := newValue;
end;  { SetTreeBackground }

function TElTreeCombo.GetTreeBackground : TBitmap;
begin
  GetTreeBackground := FTree.Background;
end;  { GetTreeBackground }

procedure TElTreeCombo.SetTreeBackgroundType(newValue : TElBkGndType);
begin
  FTree.BackgroundType := newValue;
end;  { SetTreeBackgroundType }

function TElTreeCombo.GetTreeBackgroundType : TElBkGndType;
begin
  GetTreeBackgroundType := FTree.BackgroundType;
end;  { GetTreeBackgroundType }

procedure TElTreeCombo.SetFlatFocusedScrollbars(newValue : Boolean);
begin
  FTree.FlatFocusedScrollbars := newValue;
end;  { SetFlatFocusedScrollbars }

function TElTreeCombo.GetFlatFocusedScrollbars : Boolean;
begin
  GetFlatFocusedScrollbars := FTree.FlatFocusedScrollbars;
end;  { GetFlatFocusedScrollbars }

procedure TElTreeCombo.SetGradientEndColor(newValue : TColor);
begin
  FTree.GradientEndColor := newValue;
end;  { SetGradientEndColor }

function TElTreeCombo.GetGradientEndColor : TColor;
begin
  GetGradientEndColor := FTree.GradientEndColor;
end;  { GetGradientEndColor }

procedure TElTreeCombo.SetGradientStartColor(newValue : TColor);
begin
  FTree.GradientStartColor := newValue;
end;  { SetGradientStartColor }

function TElTreeCombo.GetGradientStartColor : TColor;
begin
  GetGradientStartColor := FTree.GradientStartColor;
end;  { GetGradientStartColor }

procedure TElTreeCombo.SetGradientSteps(newValue : Integer);
begin
  FTree.GradientSteps := newValue;
end;  { SetGradientSteps }

function TElTreeCombo.GetGradientSteps : Integer;
begin
  GetGradientSteps := FTree.GradientSteps;
end;  { GetGradientSteps }

procedure TElTreeCombo.SetHorzScrollBarStyles(newValue : TElScrollBarStyles);
begin
  FTree.HorzScrollBarStyles := newValue;
end;  { SetHorzScrollBarStyles }

function TElTreeCombo.GetHorzScrollBarStyles : TElScrollBarStyles;
begin
  GetHorzScrollBarStyles := FTree.HorzScrollBarStyles;
end;  { GetHorzScrollBarStyles }

procedure TElTreeCombo.SetVertScrollBarStyles(newValue : TElScrollBarStyles);
begin
  FTree.VertScrollBarStyles := newValue;
end;  { SetVertScrollBarStyles }

function TElTreeCombo.GetVertScrollBarStyles : TElScrollBarStyles;
begin
  GetVertScrollBarStyles := FTree.VertScrollBarStyles;
end;  { GetVertScrollBarStyles }

function TElTreeCombo.GetForcedScrollBars: TScrollStyle;
begin
  Result := FTree.ForcedScrollBars;
end;

procedure TElTreeCombo.SetForcedScrollBars(const Value: TScrollStyle);
begin
  FTree.ForcedScrollBars := Value;
end;

function TElTreeCombo.GetFocusedSelectColor_Tree: TColor;
begin
  Result := FTree.FocusedSelectColor;
end;

procedure TElTreeCombo.SetFocusedSelectColor_Tree(const Value: TColor);
begin
  FTree.FocusedSelectColor := Value;
end;

function TElTreeCombo.GetTreeHeaderHeight: integer;
begin
  Result := FTree.HeaderHeight;
end;

procedure TElTreeCombo.SetTreeHeaderHeight(const Value: integer);
begin
  FTree.HeaderHeight := Value;
end;

function TElTreeCombo.GetHeaderInvertSortArrows_FTree: boolean;
begin
  Result := FTree.HeaderInvertSortArrows;
end;

procedure TElTreeCombo.SetHeaderInvertSortArrows_FTree(
  const Value: boolean);
begin
  FTree.HeaderInvertSortArrows := Value;
end;

function TElTreeCombo.GetLeftPosition_FTree: integer;
begin
  Result := FTree.LeftPosition;
end;

procedure TElTreeCombo.SetLeftPosition_FTree(const Value: integer);
begin
  if FTree.HandleAllocated then
    FTree.LeftPosition := Value;
end;

function TElTreeCombo.GetLinesColor_FTree: TColor;
begin
  Result := FTree.LinesColor;
end;

function TElTreeCombo.GetLinesStyle_FTree: TPenStyle;
begin
  Result := FTree.LinesStyle;
end;

procedure TElTreeCombo.SetLinesColor_FTree(const Value: TColor);
begin
  FTree.LinesColor := Value;
end;

procedure TElTreeCombo.SetLinesStyle_FTree(const Value: TPenStyle);
begin
  FTree.LinesStyle := Value;
end;

function TElTreeCombo.GetLockHeaderHeight_FTree: boolean;
begin
  Result := FTree.LockHeaderHeight;
end;

procedure TElTreeCombo.SetLockHeaderHeight_FTree(const Value: boolean);
begin
  FTree.LockHeaderHeight := Value;
end;

function TElTreeCombo.GetMoveColumnOnDrag_FTree: boolean;
begin
  Result := FTree.MoveColumnOnDrag;
end;

function TElTreeCombo.GetMoveFocusOnCollapse_FTree: boolean;
begin
  Result := FTree.MoveFocusOnCollapse;
end;

procedure TElTreeCombo.SetMoveColumnOnDrag_FTree(const Value: boolean);
begin
  FTree.MoveColumnOnDrag := Value;
end;

procedure TElTreeCombo.SetMoveFocusOnCollapse_FTree(const Value: boolean);
begin
  FTree.MoveFocusOnCollapse := Value;
end;

function TElTreeCombo.GetNoBlendSelected: boolean;
begin
  Result := FTree.NoBlendSelected;
end;

procedure TElTreeCombo.SetNoBlendSelected(const Value: boolean);
begin
  FTree.NoBlendSelected := Value;
end;

function TElTreeCombo.GetScrollBackground: boolean;
begin
  Result := FTree.ScrollBackground;
end;

procedure TElTreeCombo.SetScrollBackground(const Value: boolean);
begin
  FTree.ScrollBackground := Value;
end;

function TElTreeCombo.GetSelectColumn: integer;
begin
  Result := FTree.SelectColumn;
end;

procedure TElTreeCombo.SetSelectColumn(const Value: integer);
begin
  FTree.SelectColumn := Value;
end;

procedure TElTreeCombo.HeaderSectionAutoSize_FTreeTransfer(Sender: TObject;
  SectionIndex: integer);
begin
  if Assigned(FOnHeaderSectionAutoSize_FTree) then
    FOnHeaderSectionAutoSize_FTree(Sender, SectionIndex);
end;

procedure TElTreeCombo.HeaderSectionFilterCall_FTreeTransfer(
  Sender: TObject; SectionIndex: integer);
begin
  if Assigned(FOnHeaderSectionFilterCall_FTree) then
    FOnHeaderSectionFilterCall_FTree(Sender, SectionIndex);
end;

procedure TElTreeCombo.HorzScrollDrawPart_FTreeTransfer(Sender: TObject;
  Canvas: TCanvas; R: TRect; Part: TElScrollBarPart; Enabled, Focused,
  Pressed: boolean; var DefaultDraw: boolean);
begin
  if Assigned(FOnHorzScrollDrawPart_FTree) then
    FOnHorzScrollDrawPart_FTree(Sender, Canvas, R, Part, Enabled, Focused, Pressed, DefaultDraw);
end;

procedure TElTreeCombo.HorzScrollHintNeeded_FTreeTransfer(Sender: TObject;
  TrackPosition: integer; var Hint: TElFString);
begin
  if Assigned(FOnHorzScrollHintNeeded_FTree) then
    FOnHorzScrollHintNeeded_FTree(Sender, TrackPosition, Hint);
end;

procedure TElTreeCombo.VertScrollDrawPart_FTreeTransfer(Sender: TObject;
  Canvas: TCanvas; R: TRect; Part: TElScrollBarPart; Enabled, Focused,
  Pressed: boolean; var DefaultDraw: boolean);
begin
  if Assigned(FOnHorzScrollDrawPart_FTree) then
    FOnHorzScrollDrawPart_FTree(Sender, Canvas, R, Part, Enabled, Focused, Pressed, DefaultDraw);
end;

procedure TElTreeCombo.VertScrollHintNeeded_FTreeTransfer(Sender: TObject;
  TrackPosition: integer; var Hint: TElFString);
begin
  if Assigned(FOnHorzScrollHintNeeded_FTree) then
    FOnHorzScrollHintNeeded_FTree(Sender, TrackPosition, Hint);
end;

procedure TElTreeCombo.TreeResizeTransfer(Sender: TObject);
begin
  if Assigned(FOnResize_FTree) then FOnResize_FTree(Sender);
end;

procedure TElTreeCombo.ScrollTransfer(Sender: TObject;
  ScrollBarKind: TScrollBarKind; ScrollCode: integer);
begin
  if Assigned(FOnScroll_FTree) then FOnScroll_FTree(Sender, ScrollBarKind, ScrollCode);
end;

function TElTreeCombo.GetTreeActiveBorderType: TElFlatBorderType;
begin
  Result := FTree.ActiveBorderType;
end;

function TElTreeCombo.GetTreeInactiveBorderType: TElFlatBorderType;
begin
  Result := FTree.InactiveBorderType;
end;

procedure TElTreeCombo.SetTreeActiveBorderType(
  const Value: TElFlatBorderType);
begin
  FTree.ActiveBorderType := Value;
end;

procedure TElTreeCombo.SetTreeInactiveBorderType(
  const Value: TElFlatBorderType);
begin
  FTree.InactiveBorderType := Value;
end;

procedure TElTreeCombo.SetStorage_FTree(newValue : TElIniFile);
begin
{$IFDEF SUPPORT_STORAGE}
  FTree.Storage := newValue;
{$ENDIF}
end;

function  TElTreeCombo.GetStorage_FTree : TElIniFile;
begin
{$IFDEF SUPPORT_STORAGE}
  result := FTree.Storage;
{$ELSE}
  result := nil;
{$ENDIF}
end;

function TElTreeCombo.GetAlwaysKeepFocus: Boolean;
begin
  Result := FTree.AlwaysKeepFocus;
end;

procedure TElTreeCombo.SetAlwaysKeepFocus(Value: Boolean);
begin
  FTree.AlwaysKeepFocus := Value;
end;

function TElTreeCombo.GetAdjustMultilineHeight: Boolean;
begin
  Result := FTree.AdjustMultilineHeight;
end;

procedure TElTreeCombo.SetAdjustMultilineHeight(Value: Boolean);
begin
  FTree.AdjustMultilineHeight := Value;
end;

procedure TElTreeCombo.SetBarStyleVerticalLines(Value: Boolean);
begin
  FTree.BarStyleVerticalLines := Value;
end;

function TElTreeCombo.GetBarStyleVerticalLines: Boolean;
begin
  Result := FTree.BarStyleVerticalLines;
end;

function TElTreeCombo.GetChangeDelay: Integer;
begin
  Result := FTree.ChangeDelay;
end;

procedure TElTreeCombo.SetChangeDelay(Value: Integer);
begin
  FTree.ChangeDelay := Value;
end;

function TElTreeCombo.GetHorzDivLinesColor: TColor;
begin
  Result := FTree.HorzDivLinesColor;
end;

procedure TElTreeCombo.SetHorzDivLinesColor(Value: TColor);
begin
  FTree.HorzDivLinesColor := Value;
end;

function TElTreeCombo.GetDragTrgDrawMode: TDragTargetDraw;
begin
  Result := FTree.DragTrgDrawMode;
end;

procedure TElTreeCombo.SetDragTrgDrawMode(Value: TDragTargetDraw);
begin
  FTree.DragTrgDrawMode := Value;
end;

function TElTreeCombo.GetDragExpandDelay: Integer;
begin
  Result := FTree.DragExpandDelay;
end;

procedure TElTreeCombo.SetDragExpandDelay(Value: Integer);
begin
  FTree.DragExpandDelay := Value;
end;

function TElTreeCombo.GetDragRectAcceptColor: TColor;
begin
  Result := FTree.DragRectAcceptColor;
end;

procedure TElTreeCombo.SetDragRectAcceptColor(Value: TColor);
begin
  FTree.DragRectAcceptColor := Value;
end;

function TElTreeCombo.GetDragRectDenyColor: TColor;
begin
  Result := FTree.DragRectDenyColor;
end;

procedure TElTreeCombo.SetDragRectDenyColor(Value: TColor);
begin
  FTree.DragRectDenyColor := Value;
end;

function TElTreeCombo.GetExpandOnDragOver: Boolean;
begin
  Result := FTree.ExpandOnDragOver;
end;

procedure TElTreeCombo.SetExpandOnDragOver(Value: Boolean);
begin
  FTree.ExpandOnDragOver := Value;
end;

function TElTreeCombo.GetFocusedSelectTextColor: TColor;
begin
  Result := FTree.FocusedSelectTextColor;
end;

procedure TElTreeCombo.SetFocusedSelectTextColor(Value: TColor);
begin
  FTree.FocusedSelectTextColor := Value;
end;

function TElTreeCombo.GetHeaderColor: TColor;
begin
  Result := FTree.HeaderColor;
end;

procedure TElTreeCombo.SetHeaderColor(Value: TColor);
begin
  FTree.HeaderColor := Value;
end;

function TElTreeCombo.GetHeaderWrapCaptions: Boolean;
begin
  Result := FTree.HeaderWrapCaptions;
end;

procedure TElTreeCombo.SetHeaderWrapCaptions(Value: Boolean);
begin
  FTree.HeaderWrapCaptions := Value;
end;

function TElTreeCombo.GetHideFocusRect: Boolean;
begin
  Result := FTree.HideFocusRect;
end;

procedure TElTreeCombo.SetHideFocusRect(Value: Boolean);
begin
  FTree.HideFocusRect := Value;
end;

function TElTreeCombo.GetHideSelectColor: TColor;
begin
  Result := FTree.HideSelectColor;
end;

procedure TElTreeCombo.SetHideSelectColor(Value: TColor);
begin
  FTree.HideSelectColor := Value;
end;

function TElTreeCombo.GetHideSelectTextColor: TColor;
begin
  Result := FTree.HideSelectTextColor;
end;

procedure TElTreeCombo.SetHideSelectTextColor(Value: TColor);
begin
  FTree.HideSelectTextColor := Value;
end;

function TElTreeCombo.GetHideSelection: Boolean;
begin
  Result := FTree.HideSelection;
end;

procedure TElTreeCombo.SetHideSelection(Value: Boolean);
begin
  FTree.HideSelection := Value;
end;

function TElTreeCombo.GetIncrementalSearch: Boolean;
begin
  Result := FTree.IncrementalSearch;
end;

procedure TElTreeCombo.SetIncrementalSearch(Value: Boolean);
begin
  FTree.IncrementalSearch := Value;
end;

function TElTreeCombo.GetItemIndent: Integer;
begin
  Result := FTree.ItemIndent;
end;

procedure TElTreeCombo.SetItemIndent(Value: Integer);
begin
  FTree.ItemIndent := Value;
end;

function TElTreeCombo.GetLineHintColor: TColor;
begin
  Result := FTree.LineHintColor;
end;

procedure TElTreeCombo.SetLineHintColor(Value: TColor);
begin
  FTree.LineHintColor := Value;
end;

function TElTreeCombo.GetLineHintTimeout: Integer;
begin
  Result := FTree.LineHintTimeout;
end;

procedure TElTreeCombo.SetLineHintTimeout(Value: Integer);
begin
  FTree.LineHintTimeout := Value;
end;

function TElTreeCombo.GetLineHintType: TLineHintType;
begin
  Result := FTree.LineHintType;
end;

procedure TElTreeCombo.SetLineHintType(Value: TLineHintType);
begin
  FTree.LineHintType := Value;
end;

function TElTreeCombo.GetPlusMinusTransparent: Boolean;
begin
  Result := FTree.PlusMinusTransparent;
end;

procedure TElTreeCombo.SetPlusMinusTransparent(Value: Boolean);
begin
  FTree.PlusMinusTransparent := Value;
end;

function TElTreeCombo.GetRightClickSelect: Boolean;
begin
  Result := FTree.RightClickSelect;
end;

procedure TElTreeCombo.SetRightClickSelect(Value: Boolean);
begin
  FTree.RightClickSelect := Value;
end;

function TElTreeCombo.GetRowHotTrack: Boolean;
begin
  Result := FTree.RowHotTrack;
end;

procedure TElTreeCombo.SetRowHotTrack(Value: Boolean);
begin
  FTree.RowHotTrack := Value; 
end;

function TElTreeCombo.GetScrollbarOpposite: Boolean;
begin
  Result := FTree.ScrollbarOpposite;
end;

procedure TElTreeCombo.SetScrollbarOpposite(Value: Boolean);
begin
  FTree.ScrollbarOpposite := Value;
end;

function TElTreeCombo.GetTrackColor: TColor;
begin
  Result := FTree.TrackColor;
end;

procedure TElTreeCombo.SetTrackColor(Value: TColor);
begin
  FTree.TrackColor := Value;
end;

function TElTreeCombo.GetUseCustomScrollBars: Boolean;
begin
  Result := FTree.UseCustomScrollBars;
end;

procedure TElTreeCombo.SetUseCustomScrollBars(Value: Boolean);
begin
  FTree.UseCustomScrollBars := Value;
end;

function TElTreeCombo.GetVerticalLinesLong: Boolean;
begin
  Result := FTree.VerticalLinesLong;
end;

procedure TElTreeCombo.SetVerticalLinesLong(Value: Boolean);
begin
  FTree.VerticalLinesLong := Value;
end;

function TElTreeCombo.GetUseSystemHintColors: Boolean;
begin
  Result := FTree.UseSystemHintColors;
end;

procedure TElTreeCombo.SetUseSystemHintColors(Value: Boolean);
begin
  FTree.UseSystemHintColors := Value;
end;

function TElTreeCombo.GetOnHeaderSectionMeasure: TMeasureSectionEVent;
begin
  Result := FTree.OnHeaderSectionMeasure;
end;

procedure TElTreeCombo.SetOnHeaderSectionMeasure(Value: TMeasureSectionEVent);
begin
  FTree.OnHeaderSectionMeasure := Value;
end;

function TElTreeCombo.GetOnApplyVisFilter: TApplyVisFilterEvent;
begin
//  Result := FTree.OnApplyVisFilter;
end;

procedure TElTreeCombo.SetOnApplyVisFilter(Value: TApplyVisFilterEvent);
begin
//  FTree.OnApplyVisFilter := Value;
end;

function TElTreeCombo.GetOnAfterSelectionChange: TNotifyEvent;
begin
  Result := FTree.OnAfterSelectionChange;
end;

procedure TElTreeCombo.SetOnAfterSelectionChange(Value: TNotifyEvent);
begin
  FTree.OnAfterSelectionChange := Value;
end;

function TElTreeCombo.GetOnItemChecked: TOnItemCheckedEvent;
begin
  Result := FTree.OnItemChecked;
end;

procedure TElTreeCombo.SetOnItemChecked(Value: TOnItemCheckedEvent);
begin
  FTree.OnItemChecked := Value;
end;

function TElTreeCombo.GetOnSortBegin: TNotifyEvent;
begin
  Result := FTree.OnSortBegin;
end;

procedure TElTreeCombo.SetOnSortBegin(Value: TNotifyEvent);
begin
  FTree.OnSortBegin := Value;
end;

function TElTreeCombo.GetOnSortEnd: TNotifyEvent;
begin
  Result := FTree.OnSortEnd;
end;

procedure TElTreeCombo.SetOnSortEnd(Value: TNotifyEvent);
begin
  FTree.OnSortEnd := Value;
end;

{$ifdef HAS_HTML_RENDER}
function TElTreeCombo.GetOnHTMLImageNeeded: TElHTMLImageNeededEvent;
begin
  Result := FTree.OnHTMLImageNeeded;
end;

procedure TElTreeCombo.SetOnHTMLImageNeeded(Value: TElHTMLImageNeededEvent);
begin
  FTree.OnHTMLImageNeeded := Value;
end;
{$endif}

function TElTreeCombo.GetStripedOddColor: TColor;
begin
  Result := FTree.StripedOddColor;
end;

procedure TElTreeCombo.SetStripedOddColor(Value: TColor);
begin
  FTree.StripedOddColor := Value;
end;

function TElTreeCombo.GetStripedEvenColor: TColor;
begin
  Result := FTree.StripedEvenColor;
end;

procedure TElTreeCombo.SetStripedEvenColor(Value: TColor);
begin
  FTree.StripedEvenColor := Value;
end;

function TElTreeCombo.GetStripedItems: Boolean;
begin
  Result := FTree.StripedItems;
end;

procedure TElTreeCombo.SetStripedItems(Value: Boolean);
begin
  FTree.StripedItems := Value;
end;

function TElTreeCombo.GetHintType: TElHintType;
begin
  Result := FTree.HintType;
end;

procedure TElTreeCombo.SetHintType(Value: TElHintType);
begin
  FTree.HintType := Value;
end;

type THackView = class(TElTreeView);

procedure TElTreeCombo.WMChar;
begin
  if not FDropped then
    inherited
  else
    THackView(FTree.FView).WndProc(Message);
end;

procedure TElTreeCombo.WMKeyDown;
begin
  if not FDropped then
  begin
    if ReadOnly then
    begin
      inherited;
      HideCaret(Handle);
      SendMessage(Handle, EM_SETSEL, -1, -1);
    end
    else
      inherited
  end
  else
    FTree.FView.WindowProc(Message);
end;

procedure TElTreeCombo.WMKeyUp;
begin
  if not FDropped then
    inherited
  else
    FTree.FView.WindowProc(Message);
end;

procedure TElTreeCombo.WMSysKeyDown;
begin
  if not FDropped then
    inherited
  else
    FTree.FView.WindowProc(Message);
end;

procedure TElTreeCombo.WMSysKeyUp;
begin
  if not FDropped then
    inherited
  else
    FTree.FView.WindowProc(Message);
end;

procedure TElTreeCombo.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if not FDropped then
    inherited
  else
    Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTALLKEYS;
end;

procedure TElTreeCombo.WMActivateApp(var Msg : TWMActivateApp);
begin
  inherited;
  if (Msg.Active = false) then
    Self.Dropped := false;
(*
  if (Msg.Active = WA_ACTIVE) or (Msg.Active = WA_CLICKACTIVE) then
  begin
    if TElTreeCombo(Owner).CanFocus then
      TElTreeCombo(Owner).SetFocus;
  end;
*)
end;

procedure TElTreeCombo.WMKillFocus(var Msg: TMessage);
begin
  if (not IgnoreDrop) and (not IgnoreFocus) then
    Dropped := false;
  inherited;
end;

procedure TElTreeCombo.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = EN_UPDATE) and ReadOnly then
  begin
    HideCaret(Handle);
  end
  else
    inherited;
end;

procedure TElTreeCombo.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if ReadOnly then
  begin
    if FMouseDown then
    begin
      if (FSaveCapture <> 0) then
        SetCapture(FSaveCapture)
      else
        ReleaseCapture;
    end;

    FMouseDown := false;
    inherited;
    SendMessage(Handle, EM_SETSEL, -1, -1);
    HideCaret(Handle);
  end
  else
    inherited;
end;

procedure TElTreeCombo.WMMButtonDblClk(var Message: TWMMButtonDblClk);
begin
  if ReadOnly then
  begin
    Message.Result := 0;
    with Message do
      MouseDown(mbMiddle, KeysToShiftState(Keys) + [ssDouble], XPos, YPos);
    SendMessage(Handle, EM_SETSEL, -1, -1);
    HideCaret(Handle);
  end else
    inherited;
end;

procedure TElTreeCombo.EMSetReadOnly(var Msg : TMessage);
begin
  inherited;
  (*
  if Boolean(Msg.wParam) then
  begin
    if GetFocus = Handle then
       HideCaret(Handle);
  end;
  *)
end;

procedure TElTreeCombo.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if ReadOnly then
  begin
    inherited;
    if GetCapture <> Handle then
    begin
      FSaveCapture := GetCapture;
      SetCapture(Handle);
    end;

    FMouseDown := true;
    SendMessage(Handle, EM_SETSEL, -1, -1);
    HideCaret(Handle);
  end else
    inherited;
end;

procedure TElTreeCombo.WMContextMenu(var Message: TMessage);
var
  Pt, Temp: TPoint;
  {$IFDEF VCL_5_USED}
  Handled: Boolean;
  {$ENDIF}
  PopupMenu: TPopupMenu;
begin
  if ReadOnly then
  begin
    Pt.x := LOWORD(Message.lParam);
    Pt.y := HIWORD(Message.lParam);
    if Pt.X < 0 then
      Temp := Pt
    else
    begin
      Temp := ScreenToClient(Pt);
      if not PtInRect(ClientRect, Temp) then
      begin
        inherited;
        Exit;
      end;
    end;

    {$IFDEF VCL_5_USED}
    Handled := False;
    DoContextPopup(Temp, Handled);
    Message.Result := Ord(Handled);
    if Handled then Exit;
    {$ENDIF}

    PopupMenu := GetPopupMenu;
    if (PopupMenu <> nil) and PopupMenu.AutoPopup then
    begin
      SendCancelMode(nil);
      PopupMenu.PopupComponent := Self;
      if Pt.X < 0 then
        Pt := ClientToScreen(Point(0,0));
      PopupMenu.Popup(Pt.X, Pt.Y);
    end;
    Message.Result := 1;
  end else
    inherited;
end;

procedure TElTreeCombo.WMSetFocus(var Msg: TMessage);
begin
  inherited;
  if ReadOnly then
     HideCaret(Handle);
end;

procedure TElTreeCombo.WMMouseMove(var Message: TMessage);
begin
  if ReadOnly and FMouseDown then
  begin
    if WindowFromPoint(SmallPointToPoint(TWMMouse(Message).Pos)) <> Handle then
      WMLButtonUp(TWMMouse(Message));
  end
  else
    inherited;
end;

procedure TElTreeCombo.WMCancelMode(var Message: TMessage);
begin
  inherited;
  if FMouseDown then
  begin
    if FSaveCapture <> 0 then
      SetCapture(FSaveCapture)
    else
      ReleaseCapture;
  end;
end;

function TElTreeCombo.GetVertDivLinesColor: TColor;
begin
  Result := FTree.VertDivLinesColor;
end;

procedure TElTreeCombo.SetVertDivLinesColor(Value: TColor);
begin
  FTree.VertDivLinesColor := Value;
end;

procedure TElTreeCombo.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FButton) and (Msg.Sender <> FTree)
     and (Msg.Sender <> FTree.View) and (Msg.Sender <> FTree.HScrollBar)
     and (Msg.Sender <> FTree.VScrollBar) and (Msg.Sender <> FTree.FHeader) then Dropped := false;
end;

function TElTreeCombo.GetHeaderSections: TElHeaderSections;
begin
  Result := FTree.HeaderSections;
end;

procedure TElTreeCombo.SetHeaderSections(Value: TElHeaderSections);
begin
  FTree.HeaderSections := Value;
end;

function TElTreeCombo.GetHeaderImages: TImageList;
begin
  Result := FTree.HeaderImages;
end;

procedure TElTreeCombo.SetHeaderImages(Value: TImageList);
begin
  FTree.HeaderImages := Value;
end;

function TElTreeCombo.GetMultiSelectLevel: Integer;
begin
  Result := FTree.MultiSelectLevel;
end;

procedure TElTreeCombo.SetMultiSelectLevel(Value: Integer);
begin
  FTree.MultiSelectLevel := Value;
end;

function TElTreeCombo.GetDragScrollInterval: Integer;
begin
  Result := FTree.DragScrollInterval;
end;

procedure TElTreeCombo.SetDragScrollInterval(Value: Integer);
begin
  FTree.DragScrollInterval := Value;
end;

function TElTreeCombo.GetMouseFrameSelect: Boolean;
begin
  Result := FTree.MouseFrameSelect;
end;

procedure TElTreeCombo.SetMouseFrameSelect(Value: Boolean);
begin
  FTree.MouseFrameSelect := Value;
end;

function TElTreeCombo.CreateTree: TCustomElTree;
begin
  Result := TDropDownElTree.Create(nil);
end;

function TElTreeCombo.GetShowLeafButton: Boolean;
begin
  Result := FTree.ShowLeafButton;
end;

procedure TElTreeCombo.SetShowLeafButton(Value: Boolean);
begin
  FTree.ShowLeafButton := Value;
end;

function TElTreeCombo.GetLeafPicture: TBitmap;
begin
  Result := FTree.LeafPicture;
end;

procedure TElTreeCombo.SetLeafPicture(Value: TBitmap);
begin
  FTree.LeafPicture := Value;
end;

{
function TElTreeCombo.GetExplorerEditMode: Boolean;
begin
  Result := FTree.ExplorerEditMode;
end;

procedure TElTreeCombo.SetExplorerEditMode(Value: Boolean);
begin
  FTree.ExplorerEditMode := Value;
end;
}
function TElTreeCombo.GetHeaderFont: TFont;
begin
  Result := FTree.HeaderFont;
end;

procedure TElTreeCombo.SetHeaderFont(Value: TFont);
begin
  FTree.HeaderFont := Value;
end;

function TElTreeCombo.GetHeaderUseTreeFont: Boolean;
begin
  Result := FTree.HeaderUseTreeFont;
end;

procedure TElTreeCombo.SetHeaderUseTreeFont(Value: Boolean);
begin
  FTree.HeaderUseTreeFont := Value;
end;

procedure TElTreeCombo.SetSizeableTree(Value: Boolean);
begin
  if FSizeableTree <> Value then
  begin
    FSizeableTree := Value;
    FTree.RecreateWnd;
  end;
end;

procedure TElTreeCombo.SetUseXPThemes(const Value : boolean);
begin
  inherited;
  TDropDownElTree(FTree).UseXPThemes := Value;
end;

procedure TElTreeCombo.Loaded;
begin
  inherited;
  FTree.Loaded;
  if TreeParentFont then
    TreeFont.Assign(Font);
//  FTree.Parent := Self;
end;

function TElTreeCombo.IsButtonWidthStored: Boolean;
begin
  Result := ButtonWidth <> GetSystemMetrics(SM_CXHSCROLL);
end;

procedure TElTreeCombo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if TreeParentFont then
    TreeFont.Assign(Font);
end;

function TElTreeComboButton.GetThemedClassName: WideString;
begin
  Result := 'COMBOBOX';
end;

function TElTreeComboButton.GetThemePartID: Integer;
begin
  result := CP_DROPDOWNBUTTON;
  ShowGlyph := not IsThemeApplied;
end;

function TElTreeComboButton.GetThemeStateID: Integer;
begin
  if not Enabled then
    result := CBXS_DISABLED
  else
  if FState in [ebsDown, ebsExclusive] then
    result := CBXS_PRESSED
  else
  if MouseInControl then
    result := CBXS_HOT
  else
    result := CBXS_NORMAL;
end;

end.

