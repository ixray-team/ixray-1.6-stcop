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

07/14/2002


  When RowSelect is false, drag frame is now drawn around main text. 

06/28/2002

  Added RemoveItem/AddExistingItem methods. These methods let you detach/attach items and whole trees.

  Fixed the bug with incorrect repainting of main text when tracking is on and mouse is moved over items.
  The problem was introduced

06/20/2002

  Fixed counting items of different height (when using HTML-enabled items). This affected scrollbar updating and navigation.

06/03/2002

  Fixed problems when the control is being destroyed while in editing mode.

05/29/2002

  Fixed sorting for main column (spoiled recently)

05/24/2002

  Fixed drawing of text that is larger than the height of the item (this issue was introduced 05/05/2002)
  Improved header size adjustment when the sections are rebuilt
  Improved font changing -- header font is also updated now.

05/22/2002

  Writing the items to string list worked only for the tree with columns turned on. Fixed.
  Fixed the problem with ItemFocused that contained the pointer to FRoot after Clear method.

05/20/2002

  Changed the order of moving selection and focus when the item is
  deleted

05/10/2002

  HeaderHotTrack was not saved and reverted to false. Fixed.
  SortUseCase was not used when VirtualityLevel is vlNone. Fixed.
  Thanks to Steve Moss for reports

05/08/2002

  Fixed the AV that happened when virtual mode was used with item style enabled
  and no columns in header present 

05/06/2002

  Fixed the EOutOfBounds error that could happen when trying to edit the last style-enabled cell
  Fixed item height calculation for better performance and to remove possible side-effects

05/05/2002

  Cell Styles revisited. Now text in the cell with TextFlags that includes
  DT_VCENTER or DT_BOTTOM is justified vertically.

05/02/2002

  Fixed the AV that could happen when attempting to edit the cell controlled by cell style

  Fixed the AV that happened in some cases when DoubleClickMode = dcmEdit and
  the click happened on empty space

04/23/2002

  OnItemChange is now fired with icmColumnText parameter when column text changes

04/15/2002

  ElCellStyle.UseBkColor was not saved to stream. Fixed.
  Clicking on Empty space didn't deselect items even when AlwaysKeepSelection was false. Fixed.

04/10/2002

  Fixed inplace editing in DblClickMode = dcmEdit

04/09/2002

  Fixed MoveToItem to insert the item above the target when the item belonged to 

04/08/2002

  Fixed painting of the control when IsUpdating = true

04/07/2002

  Improved vertical scrolling of items with own height (once more)

04/05/2002

  Improved vertical scrolling of items with own height

04/04/2002

  Now the item that is a drop target is expanded even if it doesn't have visible
  children. This helps to expand items which are built on-the-fly  

03/28/2002

  Overlay images support added 

03/26/2002

  Items can be assigned to and from TStringList/TElWideStringList.

  Scrolling to the end of items when there are items of different height
  doesn't move the last item to the very top now and leaves at least screen
  of items visible.

03/10/2002

  Optimized painting of the tree when the parent form is resized
  Optimized font change handling

03/06/2002

  Scrollbar colors are correctly updated now

03/03/2002

  Fixed selection of item colors
  Improved selection of items with mouse in multi-select mode
  Added space style with customizable color for item border.

02/23/2002

  Fixed the problem with custom checkboxes/radiobuttons that was introduced in 3.02u9.

02/21/2002

  Double-click handler is not called now when double-click happens on checkbox

  Fixed handling of vertical scrollbar when there are > 32767 items and custom scrollbars are not used

02/16/2002

  Optimized resource use on Win9x (reduced the number of bitmaps being created
    per component)

02/14/2002

  Fixed possible AV when using inplace editors and C++Builder codeguard. 

02/04/2002

  improved vertical scrollbar handling in case of items of different height

02/03/2002

  DoubleBuffered property added.
  Currently, when the form is resized, tree doesn't use double-buffering. This
  significantly speeds up repainting during resizing.

02/02/2002

  Case-insensitive sorting in Unicode significantly optimized

02/01/2002

  AddExistingItem, InsertExistingItem, RemoveItem methods removed.

01/29/2002

  Fixed style saving when OnStyleSave/OnStyleLoad event handler is assigned.

  Added DblClickMode property, ExpandOnDblClick made obsolete.

01/23/2002

  Fixed the terrific bug when Unicode value from inplace editor was converted
  to string after editing.

01/22/2002

  Images were not displayed in HTML hint for items. Fixed.
  After using Save method the path was not restored. Fixed.

01/18/2002

  When the item is HTML-enabled, text color is adjusted when the item is selected.

12/24/2001

  OnItemFocused events were not fired when focus was moved due to calls to item deletion events. Fixed

12/21/2001

  Changed colors in default tree buttons to be the same as standard ones 

12/12/2001

  Fixed background painting in Kylix
  Fixed CalcPageUpPos/CalcPageDownPos methods to return proper value 

12/06/2001

  Changed time of OnItemExpand/OnItemCollapse

11/28/2001

  LineBorderActiveColor and LineBorderInactiveColor properties added.

11/21/2001

  Changed selected items drawing when the form is activated/deactivated.

11/17/2001

  Added HintIsHTML flag to TElTreeItem. 

11/16/2001

  Mouse movement was processed during vertical scrolling. Fixed. 

11/09/2001

  Added SortUseCase property. 

  Now inplace editing is invoked only when the cursor is over main text or column
  (i.e. not on checkbox or image)

11/08/2001

  Item was not aligned correctly when Item.SuppressButtons was true and
  Tree.ShowRootButtons was false

11/03/2001

  Page setting for vertical scrollbar in mode with items of variable height is
  now always 1 to prevent confusion
  Drag code didn't always correctly process Accept parameter when setting colors 

10/30/2001

  Helloween release.
  Added some compatibility properties and methods for easier migration between
  TTreeView and TElTree 

10/29/2001

  Added TextLeft and TextRight properties to TElTreeItem

10/27/2001

  FullRowSelect is not needed anymore - RowSelect does the same

10/22/2001

  Now it's possible to set AutoExpand and MoveFocusOnCollapse to true at the same time
  Removed spacing before checkbox
  Added spacing before image if checkbox is drawn
  Fixed vertical position of item checkboxes

10/16/2001

  When the tree is disabled, scrollbars (only when UseCustomScrollbars is true)
  are shown as disabled in Windows XP with styles enabled.
  Borders were drawn in XP style even when BorderStyle = bsNone. Fixed.
  UseCustomCheckBoxes property was not taken in effect when XP styles were used. Fixed
  CheckBoxSize behaved incorrectly when application was designed under Windows XP and
  then run on previous Windows versions. Fixed

10/09/2001

  AutoCollapse property added. When AutoCollapse is false and AutoExpand is true,
  focused item is expanded but other branches are not collapsed

10/07/2001

  KeepSelectionWithinLevel property added. When it is true and multiselect is on,
  several items can be selected only when all items are on the same level as the
  main item is

  ElCellProgressBar and ElCellCheckBox now draw their captions using
  *SelectTextColor when the item is selected.

  HeaderFont property was not taken into account when loading the form from
  resource in run-time

10/06/2001

  Clicking on the right side of the item now deselects items  
  If the tree has Align = alClient, it's parts were not updated after form
  loading. Fixed.

10/05/2001

  Fixed default for ShowRootButtons
  Fixed key clicks processing for inplace editors - some inplace editors didn't
  catch Enter/Esc keys

10/03/2001

  Changed positioning of inplace editor for main tree column in multi-column mode.
  Now editor rectangle is calculated not for the whole cell, but for the rectangle
  between left side of the text bounding rect and cell's right side. 
  
10/02/2001

  Clicks on CellProgressBar select an item now

09/29/2001

  Added TElCellProgressBar.Caption property  

09/28/2001

  HTML items set in design time were not painted correctly. Fixed.
  HTML cells whose column alignment is taRightJustify were not painted. Fixed. 

09/26/2001

  Visible flag for cell controls was not taken into account. Fixed.
  TElCellControl.BorderWidth added. This property adds some margin between
  cell bounds and control.  

09/22/2001

  Tracking is now shown even when control has no focus
  HTML text is correctly aligned vertically
  Own height is set correctly in design-time
  IsHTML flag is restored correctly after saving the form in design-time  
  Improved alignment of scrollbars when no columns are shown

09/16/2001

  Fixed the problem with mouse clicks and movements to the right of the last column

09/09/2001

  Fixed the AV that happened when QuickEditMode is set and click is done
  outside of the tree  

09/07/2001

  Fixed minor problems with scrolling the view after the item was manually expanded

09/06/2001

  Fixed drawing of the strike line in no_columns mode

08/30/2001

  UpdateDiffItems method fixed so that it doesn't go to the infinite loop now

  ExplorerEditMode property added. In this mode when inplace editor looses focus,
  changes are accepted. When ExplorerEditMode is off, the only way to accept
  changes is to press Enter

08/22/2001

  Fixed some problems with max.width not updated when ShowColumns = false and
  the widest item's width is changed due to property change (extremely rare, but
  happens). 

08/19/2001

  Changed Iterate* methods to include CheckCollapsed parameter

08/18/2001     

  Added MultiSelectLevel property.
  Added DragScrollInterval property.
  Added more informative drag cursors

08/09/2001

  Reduced size of item instance by 36 bytes (it was 164 + 20, now it's 128 + 20).
  +20 is the size of static data, which can be absent in case of virtual mode.

  Added Item.IndentAdjust property. This value is ADDED to item's position when drawing
  or calculating item part. this way you can move the item to the left or to the right
  of it's original location. 

08/08/2001

  DivLinesColor property replaced by HorzDivLinesColor and VertDivLinesColor.
  Published OnHitTest events of custom scrollbars 

08/07/2001

  Fixed the problem with default values for MainStyle object

08/03/2001

  MouseFrameSelect property added. Now you can select a range of items by
  pressing left button and moving the mouse cursor

07/26/2001

  Added Unicode support

07/25/2001

  HintType property made published
  OnMeasureItemPart event made published

07/20/2001

  Inplace editing remade.

07/18/2001

  StripedEvenColor, StripedOddColor, StripedItems properties added.

07/15/2001

  Virtuality Level 1 added. 

  MultiSort added:
  1) AddSortSection and RemoveSortSection -- add and remove additional sections
     to the list of sorting sections
  2) Ctrl+Click on header column changes sort mode of the header section and thus
     adds the section to the list of sorting sections or removes it from there

  Added LinHintType property to control, what exactly is displayed in line hint

07/14/2001

  Changed OnHeaderItemDraw event to include canvas

  Fixed Item.MoveTo(Item.Parent) (this statement caused corruption of internal
  tree structure before).

07/12/2001

  BorderSides property added.

07/06/2001

  Significantly improved speed of Clear operations.

07/03/2001

  Fixed the problem with cell background when the item is selected and
  SelectColumn <> -1.

06/29/2001

  A bug with multiline items fixed.

06/28/2001

  ipOutside part type added. Now ipInside is set only when the click happened in
  main column in multicolumn mode.
  DefaultSectionWidth property added.

  Fixed format for Date and Time inplace editors.

============================== Version 2.80 ====================================

06/21/2001

  Added ElCellStyle.UseBkColor. This property defines whether CellBkColor and
  TextBkColor properties of ElCellStyle object are used.

06/06/2001

  Now empty space between image and text belongs to text and GetItemAt
  returns ipText. As result, one can select the item by clicking this empty space

06/05/2001

  HideSelection property was reset (and so lost) under certain conditions. Fixed.

06/03/2001

  Horizontal scroll thumb size changed

06/02/2001

  Default for LineHintTimeout was not set correctly. It is 3000 ms now and is
  set fine.

05/29/2001

  Fixed the problem that appears in treecombo, when MouseDown is called and tree
  is already not visible (tree combo has been closed).  

05/21/2001

  OnAfterSelectionChange was triggered when no selection change happened during
  keyboard operation.
  Such behaviour has been improved (although this event doesn't guarantee, that
  actual selection has been changed during keyboard operation).

  FullExpand and FullCollapse methods could corrupt update counter if no children
  were available for some items. Fixed.

05/12/2001

  LineHintTimeout property added. 

04/26/2001

  DrawLine function fixed -- worked incorrectly on Riva TNT 2 video cards (and
  maybe some others)

04/18/2001

  Vertical scrollbar behaviour changed to reflect the total/on-screen ratio
  (before the change scrollbar reflected total/1 ratio) 

  Fixed the problem with select when clicking on expand button

  Fixed the problem of redrawing items when some window is slowly dragged over
  the tree 

04/13/2001

  Added ScrollBarOpposite to allow left-aligned scrollbar. Works with custom
    scrollbars only.

  Fixed the bug with incorrect drag image when multiple items were dragged while
    DragImageKind was set to dimOne or dimNever.
  BevelKind property is ignored now (this is better than drawing garbage) 
  Sorting improved

04/10/2001

  Fixed AV that happened when cancelling the inplace editor

04/04/2001

  Fixed the incorrect default for RightClickSelect property

============================== Version 2.78 ====================================

03/30/2001

  Added ipPicture2 to hit states

03/26/2001 (c) Akzhan Abdulin

  Currency inplace editor setup fixed.

  Inplace editors placement fixed.

  Sorting preparation step modified, currency sorting fixed.

03/15/2001

  Shift+down pressed twice in the end of the list in mutliselect mode
  selected all items. Fixed.

03/14/2001 (c) Akzhan Abdulin

  Incremental search now works correctly with international
  and shifted characters.

  Item comparison and insertion methods now correctly compares
  international strings using Windows locale.

  Types used by event handlers defined inside unit.

  Minor code optimizations and readness improvements.
  Unnecessary variables, 'if' statements excluded, some 'while' statements
  replaced with 'for' statements. DrawLine dot line drawing rewritten.

03/10/2001

  Fixed possible AVs that could happen when image list is removed.

  Minor optimizations and readness improvements.

  Added automatic scrolling of the item, which is expanded and it's children
  don't fint into view.

03/01/2001

  RootItem  property  added  to  ElTreeItems.  Lets application randomly access
  0-level items

  Fixed the bug with incorrect section width when ImageIndex2 is set in code
  before the tree is shown for the first time

  Added indentation to the cell contents

02/23/2001

  Added ELTREE_USE_INPLACE_EDITORS directive
  Added ELTREE_USE_STYLES directive
  Added ELTREE_USE_OLE_DRAGDROP directive
  Added ELTREE_USE_EXT_EDITORS directive

  Added new drag target drawing styles

  VisibleRowCount property added.

  DragExpandDelay property added.

  Incremental Search added.

  Currency inplace editor and currency field type added.

02/15/2001

  OnAfterSelectionChange event added. It is fired after user changes selection
  using mouse or keyboard. This event is especially useful when you need to
  enable/disable some controls to reflect possibility to perform some action with
  selected items (Delete button/menu item is a perfect sample).

  DragRectAcceptColor and DragRectDenyColor properties added.

02/08/2001                                         

  Fixed the problem with Header Height in case of change of tree's
  (and header's) font

01/31/2001

  Shift-Tab navigation fixed.
  Default for ExpandOnDoubleClick set to true.

============================== Version 2.77 ====================================

01/20/2001

  ChangeDelay property added. It sets delay time between focus
is moved with keyboard and OnItemFocused event is fired.

01/16/2001

  MoveFocusOnCollapse property didn't work. Fixed.

12/30/2000

  OnItemChecked event added
  Fixed possible AVs that could happen when drag'n'drop is started

12/25/2000

  Christmas gift -- sorting and other operations improved. Sorting is about 100
  times faster now.

12/21/2000

  Item hints added. Now every item can have its own hint and you don't need to implement
  OnItemHintNeeded event handler to provide a hint for the item

12/16/2000

  Centering of item cell text was incorrect under some circumstances. Fixed.

  Opening a dialog form in responce to MouseDown caused further problems with
  selecting items. Fixed.

12/14/2000

  IterateBranch method added.

  Fixed focus movement in AutoExpand mode.

============================== Version 2.76 ====================================

12/06/2000

  Items text, that contains "&" was drawing with end-ellipse at the end. Fixed.
  Disabled items now don't react on clicks and can't be selected with keyboard.

============================== Version 2.75 ====================================

11/04/2000

  Improved vertical alignment of text when drawing multiline and HTML items

10/27/2000

  Fixed the bug, when the height for multiline items with no multiline main text
  (i.e. the item where only column text is multiline) was defined incorrectly.

10/26/2000

  Fixed the bug, that slows down scrolling after 2.74.

10/25/2000

  Added edition of multiline items

============================== Version 2.74 ====================================

10/21/2000

  Property UseCustomScrollBars added. Now you can use standard Windows or custom
  scrollbars.
  Support for mouse wheel in Windows 95 added (Delphi 4/5 and C++Builder 4/5 only).
  Support for Genius and Microsoft mouse drivers (autoscroll feature) added.

10/17/2000

  Added Item.DataInterface property to keep interface reference in it

10/12/2000

  Selecting multiple items with keyboard or mouse worked not always properly.
  Fixed.

10/10/2000

  Divider lines were not painted after 10/06/2000 (v2.73). Fixed.
  Items with multiline main text are supported now. WARNING: enabling multiline
    significantly slows down the tree

10/06/2000

  Fixed drawing of cell text when styles are defined

10/04/2000

  HeaderColor property added
  Vertical alignment of the item main text is the same as of the cell text
  Tree font style is not spoiled when the item that is dragged has
  ParentStyle = false
  RowBkColor is not used if UseBkColor is false

10/01/2000

  Backspace key moves focus to the parent item now.

09/30/2000

  UseSystemHintColors property added. Allows to show item hints using system
  colors instead of tree colors
  Speed optimized by changing IndexOf to IndexOfFrom in some important iteration
  methods.

09/25/2000

  Added ElTreeItem.RowBkColor property. Allows to define a color for the whole
  item row

09/20/2000

  Automatical resizing of columns worked incorrectly for columns after the main
  one. Fixed

09/05/2000

  DoubleBuffered made obsolete.

09/01/2000

  OnEndDrag event didn't work. Fixed.

08/15/2000.

  Added ElTreeItems.AddLastItem method.
  Made ElCellStyle.Assign method public.
  ItemExt constant now is the tree's property

07/31/2000.

  Fixed the LockedHeaderHeight property loading.
  Fixed the cell button text color.

07/29/2000.

  Checkbox clicks moved from MouseDown to MouseUp to correctly process
  double-clicks.

  Selecting items with mouse in simple SelectionMode fixed.

07/19/2000.

  AlwaysKeepFocus property added. This property prevents the tree from setting
  ItemFocused to nil when the user clicks empty space.

  LineHintColor property added. This property defines the color of the line hint. 

  ElTreeItem.UseBkColor property added. If item uses own colors, background of
  the text is filled with BkColor. To prevent this (for example, when background
  image is used in the tree), set UseBkColor property to false.

07/09/2000.

  Added TElTreeItem.AnObject property. This property can be used as a simple
  placeholder for the object references, as Data is used to hold pointers.

07/02/2000.
  Different routines changed to improve speed of scrolling in keyboard operations.
  FillVisFwd fixed to ensure correct scrolling when FilteredVisibility enabled.

*)

unit ElTree;

{$B-}

{$ALIGN ON}

{$define PaintBackground}

interface

uses
  SysUtils,
  Classes,
{$ifndef CLX_USED}
  Controls,
  Messages,
  Windows,
  Graphics,
  Forms,
  StdCtrls,
  ExtCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ImgList,
{$ifdef ELPACK_COMPLETE}
  Buttons,
{$endif}
  Menus,
{$else}
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  QForms,
  QStdCtrls,
  QControls,
  QGraphics,
  Qt,
  Types,
  QImgList,
  QExtCtrls,
  QButtons,
  QMenus,
  QDialogs,
  ElExtBkgnd,
{$endif}

  ElTools,
  ElHook,
  ElXPThemedControl,
  ElUxTheme,
  ElTmSchema,
  ElHeader,
  ElList,
  ElScrollBar,
  ElStrUtils,
  ElHintWnd,
  Variants,
{$ifdef ELPACK_UNICODE}
  ElUnicodeStrings,
{$endif}
{$IFDEF ELPACK_COMPLETE}
{$ifdef SUPPORT_STORAGE}
  ElIni,
{$endif}
{$ifdef ELTREE_USE_STYLES}
  ElPopBtn,
{$endif}

{$ifndef VER3_EDITORS}
{$IFDEF ELTREE_USE_INPLACE_EDITORS}
{$ifdef ELTREE_USE_EXT_EDITORS}
  ElBtnEdit,
  ElACtrls,
  ElDTPick,
  ElCurrEdit,
  ElCheckCtl,
{$endif}
{$endif}
{$ENDIF}

  ElImgFrm,
{$ENDIF}
{$IFDEF HAS_HTML_RENDER}
  ElArray,
  HTMLRender,
{$ENDIF}

{$ifndef CLX_USED}
{$IFNDEF LITE}
{$ifdef ELTREE_USE_OLE_DRAGDROP}
  ElDragDrop,
{$endif}
{$endif}
  ElExtBkgnd,
{$ENDIF}
  ElVCLUtils
  {$ifdef CLX_USED}
  , ElCLXUtils
  {$endif}
{$ifndef CLX_USED}
{$IFNDEF VER90}
  , ActiveX
{$ENDIF}
{$endif}
  ;

{$R ElTree.res}

{$ifndef BUILDER_USED}
type

  TElHeaderSection = ElHeader.TElHeaderSection;
  TCustomElHeader = ElHeader.TCustomElHeader;
  TElFieldType = ElHeader.TElFieldType;
  TElFieldTypes = ElHeader.TElFieldTypes;
  TElScrollBarPart = ElScrollBar.TElScrollBarPart;
{$endif}

{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
{$IFNDEF BUILDER_USED}
  TDragType = ElDragDrop.TDragType;
  IDataObject = ActiveX.IDataObject;
  IDropSource = ActiveX.IDropSource;
  TDragTypes = ElDragDrop.TDragTypes;
  TOleDragObject = ElDragDrop.TOleDragObject;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$endif}

const
  stsFocused = 1; // TElTreeItem states
  stsSelected = 2;
  stsCut = 3;
  stsUnderlined = 4;
  stsBold = 5;
  stsItalic = 6;
  stsExpanded = 7;
  stsStrikeOut = 8;
  stiMaxState = 8;

  tisFocused    = 1;
  tisSelected   = 2;
  tisCut        = 4;
  tisExpanded   = 8;
  tisBold       = 16;
  tisItalic     = 32;
  tisUnderlined = 64;
  tisStrikeout  = 128;

  ibfParentColors   = $000001;
  ibfParentStyle    = $000002;
  ibfSuppressLines  = $000004;
  ibfImageDrawn     = $000008;
  ibfImageDrawn2    = $000010;
  ibfForceButtons   = $000020;
  ibfStrikedOutLine = $000040;
  ibfDrawHLine      = $000080;
  ibfAllowSelection = $000100;
  ibfAllowEdit      = $000200;
  ibfUseBkColor     = $000400;
  ibfDeleting       = $000800;
  ibfUseStyles      = $001000;
  ibfMultiline      = $002000;
  ibfHidden         = $004000;
  ibfEnabled        = $008000;
  ibfSuppressButtons= $010000;
  ibfCheckBoxEnabled= $020000;
  ibfShowCheckBox   = $040000;
  ibfIsHTML         = $080000;
  ibfOwnerHeight    = $100000;
  ibfRec            = $200000;
  ibfHintIsHTML     = $400000;
  
  {$ifndef CLX_USED}
  CM_MOUSEWHEEL     = CM_BASE + WM_MOUSEWHEEL;
  WM_UPDATESBFRAME  = WM_USER + 1298;
  {$endif}
  
type
  TSTIStates = set of 1..stiMaxState;

type
  EElTreeError = class(Exception)
  end;

resourcestring
  STExOutOfBounds = 'ElTree item index out of bounds.';
  STexInvItem = 'ElTree item not specified';
  STexRecursiveMove = 'Can''t move Item to one of its subitems.';

type
  TItemChangeMode = (icmText, icmState, icmCheckState, icmColumnText);

type
  TSTItemPart  = (ipButton, ipMainText, ipColumn, ipInside, ipPicture, ipPicture2, ipCheckBox, ipOutside);
  TSTSelModes  = (smSimple, smUsual);
  TSortDirs    = (sdAscend, sdDescend);
  TSortModes   = (smNone, smAdd, smClick, smAddClick);
  TSortTypes   = (stCustom, stText, stNumber, stFloating, stDateTime, stDate, stTime, stBoolean, stCurrency);
  THintModes   = (shmNone, shmLong, shmAll);
  TLineHintType= (lhtMainTextOnly, lhtCellTextOnly, lhtSmart);
  TElHintType  = (shtMainText, shtHintOnly, shtHintOrText);
  TDragImgMode = (dimNever, dimOne, dimAll);
  TNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert);
  TElCheckBoxType = (ectCheckBox, ect3SCheckBox, ectRadioButton);
  TVirtualityLevel= (vlNone, vlTextAndStyles);
  TElItemBorderStyle  = (ibsNone, ibsRaised, ibsFlat, ibsSunken, ibsSpace);
  TElDragType = (dtOLE, dtDelphi, dtBoth);
  TElDblClickMode = (dcmNone, dcmExpand, dcmEdit);

  TDragTargetDraw = (ColorFrame,
                     ColorRect,
                     SelColorRect,
                     dtdNone,
                     dtdUpColorLine,
                     dtdDownColorLine,
                     dtdUpSelColorLine,
                     dtdDownSelColorLine);
     // ColorFrame - usual background, color frame
     // ColorRect  - green rect (drop allowed) or red rect (drop not allowed)
     // SelColorRect - usual selected color rect

// *****************************************************************************

{$ifndef CLX_USED}
const TM_CLOSEINPLACEEDITOR = WM_USER + 2835;
{$endif}

type
  TCustomElTree = class;
  TElTreeItem = class;
  TElTreeItems = class;
{$ifdef ELTREE_USE_STYLES}
  TElCellStyle = class;
{$else}
  TElCEllStyle = class end;
{$endif}
  TElTreeView = class;

{$ifdef VER3_EDITORS}

{$ifdef ELTREE_USE_INPLACE_EDITORS}
  TElTreeInplaceEditor = class;
  TInplaceEditorNeededEvent   = procedure (Sender : TObject; Item : TElTreeItem; SectionIndex : Integer; SupposedFieldType : TElFieldType; var Editor : TElTreeInplaceEditor) of object;
  TInplaceOperationEvent      = procedure (Sender : TObject; var DefaultConversion : boolean) of object;
  TInplaceAfterOperationEvent = procedure (Sender : TObject; var Accepted   : boolean; var DefaultConversion : boolean) of object;
  TInplaceValidationEvent     = procedure (Sender : TObject; var InputValid : boolean) of object;
{$endif}
{$endif}

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifdef VER3_EDITORS}
  TElTreeInplaceEditor = class(TComponent)
  protected
    FDefaultValueAsText: TElFString;
    FEditing : boolean;
    FTree    : TCustomElTree;
    FTypes   : TElFieldTypes;
    FItem    : TElTreeItem;
    FValueAsText: TElFString;
    FSectionIndex: Integer;
    FDataType: TElFieldType;
    FCellRect: TRect;
    CanReFocus: boolean;
    FOnBeforeOperation : TInplaceOperationEvent;
    FOnAfterOperation  : TInplaceAfterOperationEvent;
    FOnValidateResult  : TInplaceValidationEvent;
    procedure SetTree(Value: TCustomElTree);
  protected
    function GetVisible: Boolean; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetEditorParent; virtual;
    procedure StartOperation; virtual;
    procedure CompleteOperation(Accepted : boolean); virtual;

    procedure TriggerBeforeOperation(var DefaultConversion : boolean); virtual;
    procedure TriggerAfterOperation(var Accepted : boolean; var DefaultConversion : boolean); virtual;
    procedure TriggerValidateResult(var InputValid : boolean); virtual;
    procedure DoStartOperation; virtual; abstract;
    procedure DoStopOperation(Accepted : boolean); virtual;
  public
    property Item: TElTreeItem read FItem;
    property ValueAsText: TElFString read FValueAsText write FValueAsText;
    property SectionIndex: Integer read FSectionIndex;
    property DataType: TElFieldType read FDataType;
    property CellRect: TRect read FCellRect;
    property Visible: Boolean read GetVisible;
  published
    property Tree: TCustomElTree read FTree write SetTree;
    property Types: TElFieldTypes read FTypes write FTypes default [];
    property DefaultValueAsText: TElFString read FDefaultValueAsText write
        FDefaultValueAsText;

    property OnBeforeOperation : TInplaceOperationEvent read FOnBeforeOperation
                                                        write FOnBeforeOperation;
    property OnAfterOperation : TInplaceAfterOperationEvent read FOnAfterOperation
                                                            write FOnAfterOperation;
    property OnValidateResult : TInplaceValidationEvent read FOnValidateResult
                                                        write FOnValidateResult;
  end;

  TElTreeInplaceManager = class(TComponent)
  private
    FEditorsList: TElList;
  protected
    procedure RegisterEditor(Editor : TElTreeInplaceEditor);
    procedure UnregisterEditor(Editor : TElTreeInplaceEditor);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetSuitableEditor(SupposedFieldType : TElFieldType): 
        TElTreeInplaceEditor;
  published
  end;
{$endif}
{$endif}

{$ifdef ELTREE_USE_STYLES}
{$warnings off}
  TElCellControl = class(TComponent)
  private
    FPopupMenu: TPopupMenu;
    FOwner: TElCellStyle;
    FCaption: TElFString;
       //FBoundsRect : TRect;
    FVisible: Boolean;
    FEnabled: boolean;

    FOnClick: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnDblClick: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FFont: TFont;

    procedure SetPopupMenu(newValue: TPopupMenu);
    procedure FontChanged(Sender: TObject);
    procedure SetFont(newValue: TFont);
  protected
       //procedure SetBoundsRect(newValue : TRect); virtual;
    FBorderWidth: Integer;
    procedure SetCaption(newValue: TElFString); virtual;
    procedure SetVisible(newValue: Boolean); virtual;
    procedure SetEnabled(newValue: Boolean); virtual;

    procedure TriggerClickEvent; virtual;
    procedure TriggerMouseDownEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure TriggerMouseUpEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure TriggerDblClickEvent; virtual;
    procedure TriggerMouseMoveEvent(Shift: TShiftState; X, Y: Integer); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetBorderWidth(Value: Integer);
    function PassClicks: Boolean; virtual;
  public
    procedure Update; virtual;
    procedure Assign(Source: TElCellControl); virtual; abstract;
    procedure Paint(Canvas: TCanvas; Rect: TRect); virtual; abstract;
    constructor Create; virtual;
    destructor Destroy; override;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property Font: TFont read FFont write SetFont;
  published
    property Caption: TElFString read FCaption write SetCaption;
    property Owner : TElCellStyle read FOwner;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;
{$warnings on}

{$IFDEF ELPACK_COMPLETE}
  TElCellCheckBox = class (TElCellControl)
  private
    FAlignment : TAlignment;
    FState     : TCheckBoxState;
    FAllowGrayed : boolean;
    procedure SetState(newValue : TCheckBoxState);
    procedure SetAllowGrayed(newValue : Boolean);
    function GetChecked : Boolean;
    procedure SetChecked(newValue : Boolean);
    procedure SetAlignment(newValue : TAlignment);
  protected
    procedure TriggerClickEvent; override;
    procedure TriggerMouseDownEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure TriggerMouseUpEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure Assign(Source: TElCellControl); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas; R: TRect); override;

    property State : TCheckBoxState read FState write SetState;
    property Checked : boolean read GetChecked write SetChecked;
    property AllowGrayed : Boolean read FAllowGrayed write SetAllowGrayed; { Published }
    property Alignment : TAlignment read FAlignment write SetAlignment default taRightJustify; { Protected }
  end;

  TElCellButtonGlyph = class(TElButtonGlyph)
    property ImageList;
    property ImageIndex;
    property UseImageList;
  end;

  TElCellButton = class(TElCellControl)
  private
    FGlyph: TElCellButtonGlyph;
    FLayout: TButtonLayout;
    FFixClick: Boolean;
    FDown: Boolean;
    function GetGlyph: TBitmap;
    procedure SetGlyph(newValue: TBitmap);
    procedure GlyphChanged(Sender: TObject);
    procedure SetDown(newValue: Boolean);
    procedure SetLayout(newValue: TButtonLayout);
    function GetUseImageList : Boolean;
    procedure SetUseImageList(newValue : Boolean);
    function GetImageList : TImageList;
    procedure SetImageList(newValue : TImageList);
    function GetImageIndex : Integer;
    procedure SetImageIndex(newValue : Integer);
  protected
    procedure TriggerMouseDownEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure TriggerMouseUpEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure Assign(Source: TElCellControl); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas; R: TRect); override;

    property UseImageList : Boolean read GetUseImageList write SetUseImageList;  { Public }
    property ImageList : TImageList read GetImageList write SetImageList;  { Public }
    property ImageIndex : Integer read GetImageIndex write SetImageIndex;  { Public }

    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property FixClick: Boolean read FFixClick write FFixClick;
    property Down: Boolean read FDown write SetDown default False;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
  end;

{$ENDIF}

  TElCellProgressBar = class(TElCellControl)
  private
  protected
    FMinValue: Integer;
    FMaxValue: Integer;
    FValue: Integer;
    FBarColor: TColor;
    FShowProgressText: Boolean;
    FTextAlignment: TAlignment;
    FFrameColor: TColor;
    FColor: TColor;
    procedure SetMinValue(Value: Integer);
    procedure SetMaxValue(Value: Integer);
    procedure SetValue(Value: Integer);
    procedure SetBarColor(Value: TColor);
    procedure SetShowProgressText(Value: Boolean);
    procedure SetTextAlignment(Value: TAlignment);
    procedure SetFrameColor(Value: TColor);
    procedure SetColor(Value: TColor);
    function PassClicks: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TElCellControl); override;
    procedure Paint(Canvas: TCanvas; R: TRect); override;

    property MinValue: Integer read FMinValue write SetMinValue;
    property MaxValue: Integer read FMaxValue write SetMaxValue default 100;
    property Value: Integer read FValue write SetValue;
    property BarColor: TColor read FBarColor write SetBarColor default clHighlight;
    property TextAlignment: TAlignment read FTextAlignment write SetTextAlignment default taCenter;
    property ShowProgressText: Boolean read FShowProgressText write SetShowProgressText default true;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clWindowText;
    property Color: TColor read FColor write SetColor default clWindow;
  end;

  TElCellStyle = class(TOBject)
  private
    FTag: Integer;
    FOwner: TElTreeItem;
    FCellBkColor: TColor;
    FTextBkColor: TColor;
    FTextColor: TColor;
    FTextFlags: DWORD;
    FPicture: TBitmap;
    FCellType: TElFieldType;
    FStyle: TElSectionStyle;
    FOwnerProps: Boolean;
    FFontSize: integer;
    FFontStyles: TFontStyles;
    FFontName: TFontName;
    FControl: TElCellControl;
    FUseBkColor: Boolean;
    procedure SetControl(newValue: TElCellControl);
    procedure SetFontSize(newValue: integer);
    procedure SetFontStyles(newValue: TFontStyles);
    procedure SetFontName(newValue: TFontName);
    procedure SetOwnerColors(newValue: Boolean);
    procedure SetStyle(newValue: TElSectionStyle);
    procedure SetCellBkColor(newValue: TColor);
    procedure SetTextBkColor(newValue: TColor);
    procedure SetTextColor(newValue: TColor);
    procedure SetTextFlags(newValue: DWORD);
    procedure SetPicture(newValue: TBitmap);
    procedure SetCellType(newValue: TElFieldType);
    procedure SetUseBkColor(Value: Boolean);
  public
    constructor Create(Owner: TElTreeItem);
    destructor Destroy; override;
    procedure Assign(Source: TElCellStyle);
    procedure Update;

    property Tag: Integer read FTag write FTag;
    property Control: TElCellControl read FControl write SetControl; { Published }
    property CellBkColor: TColor read FCellBkColor write SetCellBkColor;
    property TextBkColor: TColor read FTextBkColor write SetTextBkColor;
    property TextColor: TColor read FTextColor write SetTextColor;
    property TextFlags: DWORD read FTextFlags write SetTextFlags;
    property Picture: TBitmap read FPicture write SetPicture;
    property CellType: TElFieldType read FCellType write SetCellType default sftText;
    property Style: TElSectionStyle read FStyle write SetStyle;
    property OwnerProps: Boolean read FOwnerProps write SetOwnerColors;
    property FontSize: integer read FFontSize write SetFontSize;
    property FontStyles: TFontStyles read FFontStyles write SetFontStyles;
    property FontName: TFontName read FFontName write SetFontName;
    property Owner   : TElTreeItem read FOwner;
    property UseBkColor: Boolean read FUseBkColor write SetUseBkColor;
  end;
{$endif}

  TElTreeItemStaticData = record
    FText    : TElFString;
    FColText : TElFStringList;
    FHint    : TElFString;
{$ifdef ELTREE_USE_STYLES}
    FMainStyle: TElCellStyle;
    FStyles  : TElList;
{$endif}
  end;
  PElTreeItemStaticData = ^TElTreeItemStaticData;

  TElTreeItem = class(TPersistent)
  protected
{$IFDEF HAS_HTML_RENDER}
    FHTMLData      : TElHTMLData;
    FHTMLDataArray : TElArray;
{$ENDIF}
    FTag           : Longint;
    FObject        : TObject;
    FDataInterface : IUnknown;
{$IFDEF USE_VARIANT}
    FSortData      : Variant;
{$ELSE}
    FSortData      : Pointer;
    FSortType      : integer;
{$ENDIF}
    FSortRef       : integer;

    FStaticData    : PElTreeItemStaticData;
    FBorderStyle   : TElItemBorderStyle;

{$IFNDEF LITE}
    FComplexHeight : Cardinal;
    (* record
         FHeight : Word;
         FRealHeight: Word;
       end;  
    *)
{$ENDIF LITE}

    FCheckBoxState: TCheckBoxState;
    FCheckBoxType: TElCheckBoxType;

    FState: TSTIStates;
    FIState: integer;

    FChildren: TElList;
    FOwner: TCustomElTree;
    FList : TElTreeItems;
    FData : pointer; // generic data

    FRowBkColor : TColor;
    FColor,
      FBkColor: TColor;
    FStrikedLineColor: TColor;

    FBoolData1   : integer;

    FTextLeft,
      FTextRight: integer;
    FImageIndex,
      FStImageIndex: integer;
    FImageIndex2,
      FStImageIndex2: integer;

    FParent: TElTreeItem;
    FRoot  : TElTreeItem;
    FIndentAdjust: Integer;
    FBorderSpaceColor: TColor;
    FOverlayIndex: ShortInt;
    FOverlayIndex2: ShortInt;

    function GetText: TElFString;
    function GetHint: TElFString; virtual;
    procedure SetHint(Value: TElFString);
    procedure SetBorderStyle(Value: TElItemBorderStyle);
    function GetParent: TElTreeItem;
    function GetLevel: integer;
    procedure SetColor(index: integer; value: TColor);
    procedure SetUseBkColor(newValue: Boolean);
    function GetHasChildren: boolean;
    function GetHasVisibleChildren: Boolean;
    procedure SetExpanded(value: boolean);
    procedure SetParentColors(value: Boolean);
    procedure SetParentStyle(value: Boolean);
    function GetIndex: integer;
    function GetAbsIndex: integer;
    function GetVisIndex: integer;
    function GetChildIndex(Child: TElTreeItem): integer;
    function IsExpanded: boolean;
    function GetFullExpand: boolean;
    procedure MakeFullyExpanded(value: boolean);
    procedure OnColTextChange(Sender: TObject);
    procedure SetImageIndex(value: integer);
    procedure SetStImageIndex(value: integer);
    procedure SetImageIndex2(value: integer);
    procedure SetStImageIndex2(value: integer);
    procedure SetForceButtons(newValue: Boolean);
    function GetChildrenCount: Integer;
    function GetCount: Integer;
    function GetItems(Index: integer): TElTreeItem;
{$ifdef ELTREE_USE_STYLES}
    procedure SetUseStyles(newValue: Boolean);
{$endif}
    procedure OnStyleDelete(Sender: TObject; Item: pointer);
{$ifdef ELTREE_USE_STYLES}
    function GetStyles(index: integer): TElCellStyle;
    procedure SetStyles(index: integer; newValue: TElCellStyle);
    function GetStylesCount: Integer;
{$endif}
    procedure SetCheckBoxState(newValue: TCheckBoxState);
    procedure SetChecked(newValue: Boolean);
    function GetChecked: boolean;
    procedure SetShowCheckBox(newValue: Boolean);
    procedure SetCheckBoxType(newValue: TElCheckBoxType);
    procedure SetCheckBoxEnabled(newValue: Boolean);
    procedure SetSuppressButtons(newValue: Boolean);
    procedure SetEnabled(newValue: Boolean);
    procedure SetHidden(newValue: Boolean);
    function GetFullyVisible: Boolean;
    procedure SetFullyVisible(newValue: Boolean);
    function GetSelected : boolean;
    procedure SetSelected(newValue : boolean);
{$ifdef ELTREE_USE_STYLES}    
    procedure CreateStyles;
{$endif}
{$IFNDEF LITE}
    procedure SetOwnerHeight(newValue : Boolean);
    procedure SetHeight(newValue : Integer);
{$ENDIF LITE}
    function  GetHeight : Integer;
    procedure SetSuppressLines(newValue: Boolean);
    procedure UpdateItem;
    procedure SetText(Value: TElFString); virtual;
    function GetState(index: integer): boolean;
    procedure SetState(index: integer; value: boolean);
    procedure RemoveChild(Child: TElTreeItem);
    procedure RemoveSubChild(Child: TElTreeItem);
    procedure DeleteChild(Child: TElTreeItem);
    function  AddChild(Child: TElTreeItem): integer;

    procedure  AddExistingChild(Child: TElTreeItem);

    function  AddLastChild(Child: TElTreeItem): integer;
    function InsertChild(index: integer; Child: TElTreeItem): integer;
    procedure ExchangeItems(I, J: integer);

    // Sorting-related methods
    procedure QuickSort(recursive: boolean;
                        L, R: Integer;
                        SM : TElSSortMode;
                        SortType: TSortTypes;
                        FSortSection : integer);
    procedure AddSortData(SortType: TSortTypes;
                          FSortSection : integer);
    procedure ReleaseSortData;
    procedure NormalizeSorts(StartIdx : integer);
                        
    procedure SetRowBkColor(newValue : TColor); virtual;
    {$IFNDEF LITE}
    function  GetOwnerHeight : boolean;
    procedure SetMultiline(newValue: Boolean); virtual;
    {$ENDIF}
{$IFDEF HAS_HTML_RENDER}
    procedure SetIsHTML(newValue: Boolean);
    procedure OnHTMLDataDestroy(Sender :TObject; Item : Pointer);
    procedure ReRenderMainText;
    procedure ReRenderAllTexts;
{$ENDIF}
    function GetAncestor: TElTreeItem;

    procedure SetStrikedOutLine(const Value: boolean);
    procedure SetStrikedLineColor(const Value: TColor);
    function GetStrikedOutLine: boolean;
    procedure SetDrawHLine(const Value: Boolean);

    procedure SetAllowEdit(const Value: Boolean);
    function CalcSubItemsHeight: Integer;
    procedure NewStaticData;
    procedure DisposeStaticData;
    procedure FillStaticData;
    function GetColText: TElFStrings;
    function GetParentStyle: Boolean;

    {$ifdef ELTREE_USE_STYLES}
    function GetMainStyle: TElCellStyle;
    function GetUseStyles: Boolean;
    {$endif}
    function GetUseBkColor: Boolean;
    function GetParentColors: Boolean;
    function GetDrawHLine: Boolean;
    (*
    function GetAllowSelection: Boolean;
    procedure SetAllowSelection(Value: Boolean);
    *)
    function GetAllowEdit: Boolean;
    function GetForceButtons: Boolean;
    function GetSuppressButtons: Boolean;
    function GetSuppressLines: Boolean;
    function GetIsHTML: Boolean;
    function GetMultiline: Boolean;
    function GetShowCheckBox: Boolean;
    function GetCheckBoxEnabled: Boolean;
    function GetEnabled: Boolean;
    function GetHidden: Boolean;
    procedure SetIndentAdjust(Value: Integer);
    function GetDropTarget: Boolean;
    function GetHintIsHTML: Boolean;
    procedure SetHintIsHTML(Value: Boolean);
    procedure SetBorderSpaceColor(Value: TColor);
    procedure SetOverlayIndex(value: ShortInt);
    procedure SetOverlayIndex2(value: ShortInt);
    procedure ClearSubChild;
  public
    constructor Create(AOwner: TCustomElTree); virtual;
    destructor Destroy; override;
    function GetWidth: Integer; virtual;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;

    function IsUnder(Item: TElTreeItem): boolean;
    function GetFullName(separator: TElFString): TElFString;
    function GetFullNameEx(separator: TElFString; AddRoot: boolean): TElFString;
    procedure Expand(recursive: boolean);
    procedure Collapse(recursive: boolean);
    procedure Sort(recursive: boolean);
    procedure MoveTo(NewParent: TElTreeItem);
       // Moves the item to the new parent, adding it to the NewParent's
       // children list.
    procedure MoveToIns(NewParent: TElTreeItem; AnIndex: integer);
       // Moves the item to the new parent, inserting it to the NewParent's
       // children list at Index position

    procedure Clear;
    function GetFirstVisibleChild: TElTreeItem;
    function GetFirstChild: TElTreeItem;
    function GetLastChild: TElTreeItem;
    function GetNextChild(Child: TElTreeItem): TElTreeItem;
    function GetPrevChild(Child: TElTreeItem): TElTreeItem;
    function GetFirstSibling: TElTreeItem;
    function GetLastSibling: TElTreeItem;
    function GetNextSibling: TElTreeItem;
    function GetPrevSibling: TElTreeItem;
    function GetLastSubItem : TElTreeItem;
      // Get the last item, that has current item as one of its "parents"
    function GetChildByIndex(index: integer): TElTreeItem;
       // Get child item, which is on "index" position in the children list
{$IFDEF ELTREE_USE_INPLACE_EDITORS}
    procedure EditText;
{$ENDIF}
    procedure Assign(Source: TPersistent); override;

    procedure Delete; virtual;
    property TreeView: TCustomElTree read FOwner;

    function IsVisible: Boolean;
    function GetNextVisible: TElTreeItem;
    function GetPrevVisible: TElTreeItem;
    function GetPrev: TElTreeItem; virtual;
    function GetNext: TElTreeItem; virtual;
    procedure MoveToItem(Item: TElTreeItem; Mode: TNodeAttachMode); virtual;
{$ifdef ELTREE_USE_STYLES}
    function AddStyle: TElCellStyle; virtual;
    procedure RemoveStyle(Style: TElCellStyle); virtual;
{$endif}                    
    procedure RedrawItem(DoCheck: boolean);
    procedure RedrawItemPart(DoCheck: boolean; Left, Right: integer);
    function DisplayRect(TextOnly : boolean): TRect;
    {$ifdef ELTREE_USE_INPLACE_EDITORS}
    procedure EndEdit(ByCancel : boolean);
    {$endif}
    function HasAsParent(Item : TElTreeItem): Boolean;
    function IndexOf(Item : TElTreeItem): Integer;
    procedure MakeVisible;

    property TextLeft  : integer read FTextLeft;
    property TextRight : integer read FTextRight;

    property Data: pointer read FData write FData;
       // User defined data. The ElTree doesn't free this memory when
       // the item is deleted

    property AnObject: TObject read FObject write FObject;
       // User defined data. The ElTree doesn't free this memory when
       // the item is deleted

    property DataInterface: IUnknown read FDataInterface write FDataInterface;
    // User defined interface. Will be set to NIL before destruction to
    // decrease the reference counter.

    property Owner: TCustomElTree read FOwner;
       // Points to the item's owner ElTree component

    property Parent: TElTreeItem read GetParent write MoveTo;
       // Points to the item's parent item

    property HasVisibleChildren: boolean read GetHasVisibleChildren;
       // Shows, if the item has child items, that are not hidden

    property HasChildren: boolean read GetHasChildren;
       // Shows, if the item has child items
    property Index: integer read GetIndex;
       // This is the index of the item in the parent's children list

    property AbsoluteIndex: integer read GetAbsIndex;
       // This is the index of the item among all items in the tree

    property VisIndex: integer read GetVisIndex;
       // This is the index of the item among all expanded items in the tree

    property Count: Integer read GetCount;
    // This is the number of elements in Children array. Direct children only!!!

    property ChildrenCount: integer read GetChildrenCount;
    // This is the number of children on all levels below this item.

    property Children[Index: integer]: TElTreeItem read GetItems; { Public }
    // The array of children elements. Direct children only!!!
    // To find out all subitems, use Iterate* methods

    property Item[Index: integer]: TElTreeItem read GetItems; { Public }
    // the same but for TTreeView compatibility
    
    property Level: integer read GetLevel;
       // Tells, on which level the item is
       // Root items have Level 0 (zero)

    property Tag : Longint read FTag write FTag;

{$ifdef ELTREE_USE_STYLES}
    property Styles[index: integer]: TElCellStyle read GetStyles write SetStyles;
{$endif}

    property Ancestor: TElTreeItem read GetAncestor;

    property StrikedOutLine: boolean read GetStrikedOutLine write SetStrikedOutLine;

    property StrikedLineColor: TColor read FStrikedLineColor write SetStrikedLineColor;

    property DrawHLine: Boolean read GetDrawHLine write SetDrawHLine;

    // property AllowSelection: Boolean read GetAllowSelection write SetAllowSelection;

    property AllowEdit: Boolean read GetAllowEdit write SetAllowEdit;

    property Focused: boolean index 1 read GetState write SetState;
       // The item currently has focus
    property Selected: boolean read GetSelected write SetSelected;
       // The item is marked as selected
    property Cut: boolean index 3 read GetState write SetState;
       // The item is marked as cut
    property Underlined: boolean index 4 read GetState write SetState;
       // This property sets the item's main text font style
       // This doesn't affect columns text style
    property Bold: boolean index 5 read GetState write SetState;
       // This property sets the item's main text font style
       // This doesn't affect columns text style
    property Italic: boolean index 6 read GetState write SetState;
       // This property sets the item's main text font style
       // This doesn't affect columns text style
    property StrikeOut: boolean index 8 read GetState write SetState;
       // This property sets the item's main text font style
       // This doesn't affect columns text style
    property ParentStyle: Boolean read GetParentStyle write SetParentStyle;
       // if ParentStyle=true, then item's font styles are ignored, and tree's
       // are used
    property Text: TElFString read GetText write SetText;
       // Main tree text
    property ColumnText: TElFStrings read GetColText;

    // the alias for ColumnText
    property SubItems  : TElFStrings read GetColText;
       // Text for additional columns
    property Expanded: boolean read IsExpanded write SetExpanded;
       // The item is expanded

    property FullyExpanded: boolean read GetFullExpand write MakeFullyExpanded;
       // The item and all its parent are expanded

    property Color: TColor index 1 read FColor write SetColor;
       // Color of item's text

    property BkColor: TColor index 2 read FBkColor write SetColor;
       // Color of item's text background

    property UseBkColor: Boolean read GetUseBkColor write SetUseBkColor;

    property ParentColors: Boolean read GetParentColors write SetParentColors;
       // if ParentColors=True, then Color and BkColor properties are ignored

    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property StateImageIndex: integer read FStImageIndex write SetStImageIndex default -1;

    property ImageIndex2: integer read FImageIndex2 write SetImageIndex2 default -1;
    property StateImageIndex2: integer read FStImageIndex2 write SetStImageIndex2 default -1;

    property ForceButtons: Boolean read GetForceButtons write SetForceButtons
        default False;
    property SuppressButtons: Boolean read GetSuppressButtons write
        SetSuppressButtons default False;
    property SuppressLines: Boolean read GetSuppressLines write SetSuppressLines;

    property Hint: TElFString read GetHint write SetHint;

{$ifdef ELTREE_USE_STYLES}
    property UseStyles: Boolean read GetUseStyles write SetUseStyles;
    property MainStyle  : TElCellStyle read GetMainStyle;
    property StylesCount: Integer read GetStylesCount;
{$endif}
    property CheckBoxState: TCheckBoxState read FCheckBoxState write SetCheckBoxState default cbUnchecked;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property ShowCheckBox: Boolean read GetShowCheckBox write SetShowCheckBox
        default True;
    property CheckBoxType: TElCheckBoxType read FCheckBoxType write SetCheckBoxType default ectCheckBox;
    property CheckBoxEnabled: Boolean read GetCheckBoxEnabled write
        SetCheckBoxEnabled;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Hidden: Boolean read GetHidden write SetHidden;
    property FullyVisible: Boolean read GetFullyVisible write SetFullyVisible;
    property Height      : Integer read GetHeight {$ifndef LITE}write SetHeight{$endif};  { Public }

{$IFNDEF LITE}
    property OwnerHeight : Boolean read GetOwnerHeight write SetOwnerHeight;  { Public }
    property Multiline: Boolean read GetMultiline write SetMultiline;
{$ENDIF LITE}
    property RowBkColor : TColor read FRowBkColor write SetRowBkColor;  { Public }
    // The color of the item background (complete, not under text
{$IFDEF HAS_HTML_RENDER}
    property IsHTML: Boolean read GetIsHTML write SetIsHTML;
{$ENDIF}
    property BorderStyle: TElItemBorderStyle read FBorderStyle write SetBorderStyle;
    property IndentAdjust: Integer read FIndentAdjust write SetIndentAdjust;
    property DropTarget: Boolean read GetDropTarget;
    property HintIsHTML: Boolean read GetHintIsHTML write SetHintIsHTML;
    property BorderSpaceColor: TColor read FBorderSpaceColor write
        SetBorderSpaceColor default clWindow;
    property OverlayIndex: ShortInt read FOverlayIndex write SetOverlayIndex default -1;
    property OverlayIndex2: ShortInt read FOverlayIndex2 write SetOverlayIndex2 default -1;
  end;

// *****************************************************************************

  TElTreeItemClass = class of TElTreeItem;

  TElLookupCompareProc = function(Item: TElTreeItem; SearchDetails: Pointer): boolean;

  TIterateProc = procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);

   TIterateProcAnonymusMethod = reference to procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);

  TElTreeItems = class(TPersistent)
  protected
    FOwner: TCustomElTree;
    //FCount: integer;

    FRoot: TElTreeItem;
    FItemClass : TElTreeItemClass;
    function GetVisCount: integer;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function CreateItem(FOwner: TCustomElTree): TElTreeItem; virtual;
    function GetCount: Integer;
    function GetRootCount: Integer;
    function GetRootItem(Index: Integer): TElTreeItem;
    function GetItem(index: integer): TElTreeItem;
    function GetVisItem(index: integer): TElTreeItem;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TCustomElTree); virtual;
    constructor CreateClass(AOwner: TCustomElTree; ItemClass : TElTreeItemClass);
    destructor Destroy; override;

    procedure AddExistingItem(Item, Parent: TElTreeItem);
    procedure InsertExistingItem(Item, Parent: TElTreeItem; Index: integer);
    procedure RemoveItem(Child: TElTreeItem);
    
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);

    procedure SaveToStringList(AStrings : TStrings);
    procedure LoadFromStringList(Strings : TStrings);
    {$ifdef ELPACK_UNICODE}
    procedure LoadFromWideStringList(Strings : TElWideStrings);
    procedure SaveToWideStringList(AStrings : TElWideStrings);
    {$endif}

    procedure DeleteItem(Child: TElTreeItem);
    function GetAbsIndex(Child: TElTreeItem) : integer;
    function GetVisIndex(Child: TElTreeItem) : integer;
    function AddItem(Parent: TElTreeItem)    : TElTreeItem;
    function AddLastItem(Parent: TElTreeItem): TElTreeItem;
    procedure SetItem(Index: Integer; Value: TElTreeItem);
    function InsertItem(Index: integer; Parent: TElTreeItem): TElTreeItem;

    // AllocateStorage increases the size of the list, thus speeding up items adding
    procedure AllocateStorage(MaxItems : integer);
    function Add(Item: TElTreeItem; Text: TElFString): TElTreeItem; virtual;
    function AddChild(Item: TElTreeItem; Text: TElFString): TElTreeItem; virtual;
    function AddChildFirst(Item: TElTreeItem; Text: TElFString): TElTreeItem; virtual;
    function AddChildObject(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; virtual;
    function AddChildObjectFirst(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; virtual;
    function AddFirst(Item: TElTreeItem; Text: TElFString): TElTreeItem; virtual;
    function AddObject(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; virtual;
    function AddObjectFirst(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; virtual;
    function Insert(Item: TElTreeItem; Text: TElFString): TElTreeItem; virtual;
    function InsertObject(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; virtual;
    function InsertAfter(Item: TElTreeItem; Text: TElFString): TElTreeItem; virtual;
    function InsertAfterObject(Item: TElTreeItem; Text: TElFString; Ptr: Pointer): TElTreeItem; virtual;
    procedure InsertItemFromString(Index : integer; AString : TElFString);

    procedure Delete(Item: TElTreeItem); virtual;
    function GetFirstNode: TElTreeItem;

    procedure Clear;
    procedure IterateBranch(VisibleOnly: boolean; IterateProc: TIterateProc; IterateData: pointer; BranchParent: TElTreeItem);

    procedure IterateFrom(VisibleOnly, CheckCollapsed: boolean; IterateProc: TIterateProcAnonymusMethod; IterateData: pointer; StartFrom: TElTreeItem); (*<+>*)
    procedure IterateBackFrom(VisibleOnly, CheckCollapsed: boolean; IterateProc: TIterateProcAnonymusMethod; IterateData: pointer; StartFrom: TElTreeItem); (*<+>*)
    procedure Iterate(VisibleOnly, CheckCollapsed: boolean; IterateProc: TIterateProcAnonymusMethod; IterateData: pointer); (*<+>*)
    procedure IterateBack(VisibleOnly, CheckCollapsed: boolean; IterateProc: TIterateProcAnonymusMethod; IterateData: pointer); (*<+>*)

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    function LookForItem(StartItem: TElTreeItem;
      TextToFind: TElFString;
      DataToFind: pointer;
      ColumnNum : integer;
      LookForData,
      CheckStartItem,
      SubItemsOnly,
      VisibleOnly,
      NoCase: boolean): TElTreeItem;
        (*<+>*)
    function LookForItem2(StartItem: TElTreeItem;
      TextToFind: TElFString;
      WholeTextOnly: boolean;
      DataToFind: pointer;
      ColumnNum : integer;
      LookForData,
      CheckStartItem,
      SubItemsOnly,
      VisibleOnly,
      CheckCollapsed,
      NoCase: boolean): TElTreeItem;

    function LookForItemEx(StartItem: TElTreeItem; ColumnNum: integer;
      CheckStartItem, SubItemsOnly, VisibleOnly: boolean;
      SearchDetails: pointer;
      CompareProc: TElLookupCompareProc): TElTreeItem;

    function LookBackForItemEx2(StartItem: TElTreeItem; ColumnNum: integer;
      CheckStartItem, SubItemsOnly, VisibleOnly, CheckCollapsed: boolean;
      SearchDetails: pointer;
      CompareProc: TElLookupCompareProc): TElTreeItem;

    (*<+>*)
    function LookForItemEx2(StartItem: TElTreeItem; ColumnNum: integer;
      CheckStartItem, SubItemsOnly, VisibleOnly, CheckCollapsed: boolean;
      SearchDetails: pointer;
      CompareProc: TElLookupCompareProc): TElTreeItem;

    property ItemClass : TElTreeItemClass read FItemClass write FItemClass;
    property Owner: TCustomElTree read FOwner;
    property Item[Index: integer]: TElTreeItem read GetItem; default;
    property ItemAsVis[Index: integer]: TElTreeItem read GetVisItem;
    property Count: Integer read GetCount;
    property VisCount: integer read GetVisCount;
    property RootCount: Integer read GetRootCount;
    property RootItem[Index: Integer]: TElTreeItem read GetRootItem;
  end;

// *****************************************************************************

{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
  TOleDragStartEvent = procedure(Sender: TObject; var dataObj: IDataObject;
    var dropSource: IDropSource; var dwOKEffects: TDragTypes) of object;
  TOleDragFinishEvent = procedure(Sender: TObject; dwEffect: TDragType; Result: HResult) of object;
{$ENDIF}
{$ENDIF}
{$endif}

  TMeasureItemPartEvent = procedure(Sender: TObject; Item: TElTreeItem; PartIndex: integer; var Size: TPoint) of object;
  THeaderSectionEvent = procedure(Sender: TObject; Section: TElHeaderSection) of object;
  TColumnNotifyEvent = procedure(Sender: TObject; SectionIndex: integer) of object;
  TOnItemDrawEvent = procedure(Sender: TObject; Item: TElTreeItem; Surface: TCanvas;
    R: TRect; SectionIndex: integer) of object;
  TOnShowHintEvent = procedure(Sender: TObject;
    Item: TElTreeItem;
    Section : TElHeaderSection;
    var Text: TElFString;
    HintWindow: THintWindow;
    MousePos: TPoint;
    var DoShowHint: boolean) of object;
  TApplyVisFilterEvent = procedure(Sender: TObject; Item: TElTreeItem; var Hidden: boolean) of object;

  TTuneUpInplaceEditEvent = procedure(Sender : TObject; Item : TElTreeItem; SectionIndex : integer; Editor : TCustomEdit) of object;
  TOnItemExpandEvent = procedure(Sender: TObject; Item: TElTreeItem) of object;
  TOnItemCheckedEvent = Procedure (Sender: TObject; Item: TelTreeItem) of Object;
  TItemSelChangeEvent = procedure(Sender: TObject; Item: TElTreeItem) of object;
  TOnItemChangeEvent = procedure(Sender: TObject; Item: TElTreeItem;
    ItemChangeMode: TItemChangeMode) of object;

  TOnCompareItems = procedure(Sender: TObject; Item1, Item2: TElTreeItem;
    var res: integer) of object;

  TOnItemExpanding = procedure(Sender: TObject; Item: TElTreeItem;
    var CanProcess: boolean) of object;

  TOnPicDrawEvent = procedure(Sender: TObject; Item: TElTreeItem;
    var ImageIndex: integer) of object;

  THotTrackEvent = procedure(Sender: TObject; OldItem, NewItem: TElTreeItem) of object;

  TOnValidateEvent = procedure(Sender: TObject; Item: TElTreeItem;
    Section: TElHeaderSection;
    var Text: string; var Accept: boolean) of object;

  {$ifndef VER3_EDITORS}
  TTryEditEvent = procedure(Sender: TObject; Item: TElTreeItem;
    Section: TElHeaderSection; var CellType: TElFieldType; var CanEdit: boolean) of object;
  {$else}
  TTryEditEvent = procedure(Sender: TObject; Item: TElTreeItem;
    SectionIndex: integer; var CellType: TElFieldType; var CanEdit: boolean) of object;
  {$endif}
  TEditRequestEvent = procedure(Sender: TObject; Item: TElTreeItem;
    Section: TElHeaderSection) of object;

  TComboEditShowEvent = procedure(Sender: TObject; Item: TElTreeItem;
    Section: TElHeaderSection;
    Combobox: TCombobox) of object;

  TValidateComboEvent = procedure(Sender: TObject; Item: TElTreeItem;
    Section: TElHeaderSection;
    Combo: TComboBox;
    var Accept: boolean) of object;

  TElScrollEvent = procedure(Sender: TObject;
    ScrollBarKind: TScrollBarKind;
    ScrollCode: integer) of object;

  TElColumnMoveEvent = procedure(Sender: TCustomElTree;
    Section: TElHeaderSection;
    OldPos, NewPos: integer) of object;

  TItemSaveEvent = procedure(Sender: TObject; Stream: TStream;
    Item: TElTreeItem) of object;

  TCellStyleSaveEvent = procedure(Sender: TObject; Stream: TStream;
    Style: TElCellStyle) of object;

  TElTreeChangingEvent = procedure (Sender : TObject; Item : TElTreeItem;
    var AllowChange: Boolean) of object;

  TElTreeItemPostDrawEvent = procedure(Sender : TObject; Canvas : TCanvas; Item : TElTreeItem;
    ItemRect : TRect; var DrawFocusRect : boolean) of object;

  TElTreeItemDragTargetEvent = procedure (Sender : TObject; Item : TElTreeItem;
    ItemRect : TRect; X, Y : integer) of object;

  TVirtualTextNeededEvent = procedure (Sender : TObject; Item : TElTreeItem; SectionIndex : Integer; var Text : TElFString) of object;
  TVirtualHintNeededEvent = procedure (Sender : TObject; Item : TElTreeItem; var Hint : TElFString) of object;
  TVirtualValueNeededEvent= procedure (Sender : TObject; Item : TElTreeItem; SectionIndex : Integer; VarType : integer; var Value : Variant) of object;
{$ifdef ELTREE_USE_STYLES}
  TVirtualStyleNeededEvent= procedure (Sender : TObject; Item : TElTreeItem; SectionIndex : Integer; Style : TElCellStyle) of object;
{$endif}

  TElTreeView = class(TCustomControl)
  protected
    FHeader   : TElHeader;
    FOwner    : TCustomElTree;
    FItems    : TElTreeItems;
    {$ifdef ELTREE_USE_STYLES}
    VirtStyle : TElCellStyle;
    {$endif}
    // Hint fields
    FHintTimer: TTimer;
    FHintWnd  : TElHintWindow;
    FHintCoord: TPoint;
    FHintItem,
    FHintItemEx : TElTreeItem;

    // Update fields
    FPainting, // already in Paint
    FClearVis, // do update visibles list
    FClearAll, // clear the whole ClientRect
    FVisUpdated, // visibles list updated
    FRangeUpdate // all items should be updated
               : boolean;

    // Positioning fields
    FHRange    : integer;

    // Mouse action fields
    FPressCoord: TPoint;
    FPressed   : boolean;
    FMouseSel  : boolean;
    FClickCoord: TPoint;
    FClicked   : boolean;
{$ifdef ELTREE_USE_STYLES}
    FClickControl: TElCellControl;
{$endif}
    FIgnoreClick,
    FIgnoreClick2: boolean;
    FClickPassed : boolean;
    FPassedItem  : TElTreeItem;
    FPassedShift : TShiftState;

    FClickSection: integer;
    // Current items
    FClickItem,
    FTrackItem,
{$IFDEF ELTREE_USE_INPLACE_EDITORS}
    FEditingItem,
{$endif}
    FFocused,
    FSelected,
    FDropTrg  : TElTreeItem;

    // mouse frame selection
    FMFSStartItem  : TElTreeItem;
    FMFSStartCoord : TPoint;
    FMFSEndItem    : TElTreeItem;
    FMFSendCoord   : TPoint;
    FMFSList       : TElList;

    // Painting helper fields
    FVisible  : TElList;
    FOverColors,
    FRowOvColors: boolean;

    // Drag'n'drop fields
    FDragScrollTimer,
    FDragExpandTimer : TTimer;
    FDropAcc   : boolean;
    FInDragging: boolean;
    FDDY       : integer;
{$ifndef CLX_USED}
{$IFDEF VCL_4_USED}
    FDragImages: TImageList;
{$ELSE}
    FDragImages: TImageList;
{$ENDIF}
{$endif}
    // Edit fields
{$ifndef VER3_EDITORS}
{$IFDEF ELTREE_USE_INPLACE_EDITORS}
    FInpEdit   : TWinControl;
    FEditing   : boolean;
    FEditType  : TElFieldType;
    FEditSect  : integer;
    FEditTimer : TTimer;
    FItemToEdit: TElTreeItem;
{$ENDIF}
{$else}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    FInpEdit   : TElTreeInplaceEditor;
    FEditing   : boolean;
    FEditType  : TElFieldType;
    FEditSect  : integer;
    FEditTimer : TTimer;
    FItemToEdit: TElTreeItem;
{$endif}
{$endif}
    FOldHide   : boolean;
    FFakePopup : TPopupMenu;
{$IFDEF HAS_HTML_RENDER}
    FRender    : TElHTMLRender;
{$ENDIF}
{$IFNDEF LITE}
    FTmpBmp   : TBitmap;
{$ENDIF}
    SearchText: string;
    SearchTextTimeoutThread : TThread;

    FScrollFirstClick: boolean;

    FHasFocus : boolean;

    procedure StartClearSearchTimeoutThread ;
    procedure StopClearSearchTimeoutThread ;
    procedure SearchTextTimeout (Sender : TObject) ;

    function ProcessSearch(Key : Char): Boolean;
    {$ifndef CLX_USED}
    procedure WMSize(var Msg : TWMSize); message WM_SIZE;
    {$else}
    procedure Resize; override;
    {$endif}

{$IFNDEF LITE}
    // Custom background routines
    procedure RedoTmpBmp;
{$ENDIF}

    // Painting routines
    procedure RedrawTree(ACanvas : TCanvas; RealLeftPos : integer; ItemsList : TElList);
    procedure DrawImages(ACanvas : TCanvas; Item : TElTreeItem; HelperBitmap : TBitmap; var R : TRect; var ItemRect : TRect);
    procedure DrawButtons(ACanvas : TCanvas; Item : TElTreeItem; IsNode : boolean; HelperBitmap : TBitmap; var R : TRect; var ItemRect : TRect);
    procedure DrawCheckBoxes(ACanvas : TCanvas; Item : TElTreeItem; HelperBitmap : TBitmap; var R : TRect; var ItemRect : TRect);
    procedure DrawItemLines(ACanvas : TCanvas; Item : TElTreeItem; var R : TRect; var ItemRect : TRect);
    procedure DoRedrawItem(ACanvas : TCanvas; Item: TElTreeItem; ItemRect, SurfRect: TRect);
    procedure DoRedrawItemTree(ACanvas : TCanvas; Item: TElTreeItem; ItemRect, SurfRect: TRect);
    procedure Paint; override;
    {$ifndef CLX_USED}
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    {$endif}
    procedure DoPaintBkgnd(DC : HDC; ClipRect : TRect);
    procedure UpdateView;

    // Hint routines
    procedure TryStartHint(XPos, YPos : Integer);
    procedure OnHintTimer(Sender: TObject);
    procedure DoHideLineHint;
    procedure DoShowLineHint(Item: TElTreeItem; Section : TElHeaderSection);
    function  GetHintText(Item: TElTreeItem; var Section : TElHeaderSection) : TElFString;

    // Dynamic height helper routines
    function CalcPageUpPos(CurIdx : integer) : integer;
    function CalcPageDownPos(CurIdx : integer) : integer;

    // Windows Message handling
    {$ifndef CLX_USED}
    procedure WndProc(var Message: TMessage); override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LButtonDblClk;
    procedure WMRButtonDblClk(var Msg: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Msg: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    {$else}
    function WidgetFlags: Integer; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    {$endif}
    procedure IntLButtonDown(X, Y : integer; Shift : TShiftState);
    function IntLButtonUp(X, Y : integer; Shift : TShiftState): Boolean;
    procedure IntRButtonDown(X, Y : integer; Shift : TShiftState);
    function IntRButtonUp(X, Y : integer; Shift : TShiftState): Boolean;
    function IntLButtonDblClick(X, Y : integer; Shift : TShiftState): Boolean;
    function IntRButtonDblClick(X, Y : integer; Shift : TShiftState): Boolean;
    procedure IntMouseMove(X, Y : integer; Shift : TShiftState);

    // VCL notification handlers
    {$ifndef CLX_USED}
    procedure CMMouseWheel(var Msg : TMessage); message CM_MOUSEWHEEL;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMHintShow(var Msg : TMessage); message CM_HINTSHOW;
    {$else}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      const MousePos: TPoint): Boolean; override;
    procedure PaletteChanged(Sender: TObject); override;
    function HintShow(var HintInfo : THintInfo): Boolean; override;
    {$endif}

    // Scrolling routines
    procedure SetHPosition(value: integer);
    procedure SetVPosition(value: integer);
    procedure DoSetTopIndex(Value: integer);

    procedure OnHScroll(Sender: TObject; ScrollCode: TElScrollCode; var ScrollPos: Integer; var DoChange : boolean);
    procedure OnVScroll(Sender: TObject; ScrollCode: TElScrollCode; var ScrollPos: Integer; var DoChange : boolean);

    // Painting helper routines
    procedure FillVisFwd(StartIndex: integer);
    procedure DefineHRange;
    function GetVisCount: integer; virtual;
    function GetVisiblesHeight : integer;

    // editing routines

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
    function  DoEndEdit(ByCancel: boolean) : boolean; virtual;
    procedure OnEditExit(Sender: TObject);
    procedure DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function  SetupEditControl(Item : TElTreeItem; Section : TElHeaderSection; FT : TElFieldType) : boolean; virtual;
    function  SetupNumericEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupFloatEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupEnumEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupTextEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupDateTimeEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupDateEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupTimeEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupCustomEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupBoolEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupBlobEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function  SetupPictureEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean; virtual;
    function SetupCurrencyEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect): Boolean; virtual;

    procedure ProcessEditResults(ByCancel : boolean); virtual;
    procedure ProcessFloatEditResults(ByCancel : boolean); virtual;
    procedure ProcessNumericEditResults(ByCancel : boolean); virtual;
    procedure ProcessDateTimeEditResults(ByCancel : boolean); virtual;
    procedure ProcessDateEditResults(ByCancel : boolean); virtual;
    procedure ProcessTimeEditResults(ByCancel : boolean); virtual;
    procedure ProcessEnumEditResults(ByCancel : boolean); virtual;
    procedure ProcessBoolEditResults(ByCancel : boolean); virtual;
    procedure ProcessTextEditResults(ByCancel : boolean); virtual;
    procedure ProcessBlobEditResults(ByCancel : boolean); virtual;
    procedure ProcessPictureEditResults(ByCancel : boolean); virtual;
    procedure ProcessCustomEditResults(ByCancel : boolean); virtual;
    procedure ProcessCurrencyEditResults(ByCancel : boolean); virtual;

    procedure DoEditItem(Item: TElTreeItem; SectionNum: integer); virtual;
    procedure OnEditTimer(Sender : TObject);
{$else}
    procedure OnEditTimer(Sender : TObject);
    procedure DoEditItem(Item: TElTreeItem; SectionNum: integer); virtual;
    procedure DoEndEdit(ByCancel: boolean); virtual;
    procedure EditOperationCancelled; virtual;
    procedure EditOperationAccepted; virtual;
{$endif}
{$endif}
    // drag'n'drop routines
    {$ifndef CLX_USED}
    procedure FillDragImage;
    {$endif}
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean); virtual;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    {$ifndef CLX_USED}
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    {$else}
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    {$endif}
    function DragScroll(Source: TDragObject; X, Y: integer): boolean; virtual;
    procedure OnScrollTimer(Sender : TObject);
    procedure OnDragExpandTimer(Sender : TObject);

{$ifndef CLX_USED}
    function GetDragImages: TDragImageList; override;
{$endif}
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
    procedure OnDropTargetDrag(Sender: TObject; State: TDragState; Source: TOleDragObject; Shift: TShiftState; X: Integer; Y: Integer; var DragType: TDragType);
    procedure OnDropTargetDrop(Sender: TObject; Source: TOleDragObject; Shift: TShiftState; X: Integer; Y: Integer; var DragType: TDragType);
{$ENDIF}
{$ENDIF}
{$endif}
    // coords routines
    function GetItemRect(ItemIndex: integer): TRect; virtual;
    function GetItemAtY(Y: integer): TElTreeItem; virtual;
    function GetItemAt(X, Y: Integer; var ItemPart: TSTItemPart; var HitColumn: integer): TElTreeItem; virtual;

    // user input routines
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {$ifndef CLX_USED}
    procedure CMMouseLeave(var Message: TMessage); message CM_MouseLeave;
    {$else}
    procedure MouseLeave(Control : TControl); override;
    {$endif}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure ProcessPassedClick;
    procedure FitMostChildren(Item : TElTreeItem);

    procedure DoEnter; override;
    procedure DoExit; override;

    {$ifndef CLX_USED}
    procedure DestroyWnd; override;
    {$else}
    procedure DestroyWidget; override;
    {$endif}

    // selection routines
    procedure DoSetSelected(value: TElTreeItem);
    function GetVisCount2: Integer; virtual;
    function FindNewFocused(Key : word; PVal1 : PInteger; var Sel : boolean): TElTreeItem; virtual;
    procedure DrawMouseSelectFrame;
    procedure AllocateMouseSelectFrame;
    procedure DeallocateMouseSelectFrame;
    procedure SelectMouseSelectItems;
    {$ifndef CLX_USED}
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    {$endif}
    procedure CancelMouseSel;
    {$ifndef CLX_USED}
    procedure CMDeactivate(var Message: TMessage); message CM_DEACTIVATE;

    {$ifdef ELPACK_COMPLETE}
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    {$endif}
    {$endif}
    procedure InitiateEditOp(Item : TElTreeItem; HCol : integer; Immediate :
        boolean); virtual;
    function IsControlCell(Item : TElTreeItem; SectionIndex : integer): Boolean;
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;

    property Owner : TCustomElTree read FOwner;
  end;

  TCustomElTree = class(TElXPThemedControl)
  protected
    {$ifdef ELTREE_USE_INPLACE_EDITORS}
    {$ifdef VER3_EDITORS}
    FEditorManager: TElTreeInplaceManager;
    {$endif}
    {$endif}
    FStripedOddColor: TColor;
    FStripedEvenColor: TColor;
    FStripedItems: Boolean;
    {$ifdef ELTREE_USE_STYLES}
    FOnVirtualStyleNeeded: TVirtualStyleNeededEvent;
    {$endif}
    FSortSections        : TElList;
    FOnVirtualTextNeeded : TVirtualTextNeededEvent;
    FVirtualityLevel     : TVirtualityLevel;
    FOnVirtualHintNeeded : TVirtualHintNeededEvent;
    FOnVirtualValueNeeded: TVirtualValueNeededEvent;
    FLineHintType        : TLineHintType;
    FLineHintTimeout     : Integer;
    FFireFocusEvents     : integer;
    FTransButtons        : boolean;
    FTransCheckBoxes     : boolean;
    FTrackColor          : TColor;
    FExpandOnDragOver    : Boolean;
    FForcedScrollBars    : TScrollStyle;
    FMoveFocusOnCollapse : Boolean;
    FHeaderHeight        : integer;
    FOnVertScrollHintNeeded : TElScrollHintNeededEvent;
    FOnHorzScrollDrawPart   : TElScrollDrawPartEvent;
    FOnHorzScrollHintNeeded : TElScrollHintNeededEvent;
    FOnVertScrollDrawPart   : TElScrollDrawPartEvent;
    FOnVertScrollHitTest    : TElScrollHitTestEvent;
    FOnChanging             : TElTreeChangingEvent;

{$IFDEF VCL_4_USED}
{$ifndef CLX_USED}
    FBevelKindDummy : TBevelKind;
{$endif}
{$ENDIF}
    FHintType : TElHintType;
    FOnClick : TNotifyEvent;
    FOnDblClick : TNotifyEvent;
    //FOnEndDrag : TDragDropEvent;
    FOnDrop : TDragDropEvent;
    FOnOver : TDragOverEvent;
    FOnDrag : TEndDragEvent;
    FOnEnter : TNotifyEvent;
    FOnExit : TNotifyEvent;
    FOnKeyDown : TKeyEvent;
    FOnKeyPress : TKeyPressEvent;
    FOnKeyUp : TKeyEvent;
    FOnMouseDown : TMouseEvent;
    FOnMouseMove : TMouseMoveEvent;
    FOnMouseUp : TMouseEvent;
    FOnStartDrag : TStartDragEvent;
    FOnItemPreDraw: TOnItemExpandEvent;
    FOnDragTargetChange: TElTreeItemDragTargetEvent;

{$IFNDEF LITE}
    FGradientStartColor : TColor;
    FGradientEndColor : TColor;
    FGradientSteps : Integer;
{$ENDIF LITE}
    FActiveBorderType : TElFlatBorderType;
    FInactiveBorderType : TElFlatBorderType;
    FRowHotTrack : Boolean;
    FFocusedSelectColor: TColor;
    FHideSelectColor: TColor;
    FFocusedSelectTextColor: TColor;
    FHideSelectTextColor: TColor;
{$IFNDEF LITE}
    FNoBlendSelected : Boolean;
    FScrollBackground : Boolean;
    FBackground : TBitmap;
    FBackgroundType : TElBkGndType;
    FAdjustMultilineHeight : Boolean;
{$ENDIF}
    FFlatFocusedScrollbars : Boolean;
    FAutoResizeColumns : Boolean;
    FHideFocusRect: Boolean;
    FShowEmptyImages : Boolean;
    FShowEmptyImages2: Boolean;
    FShowRootButtons : Boolean;
    FUnderlineTracked: Boolean;
    FCustomCheckboxes: Boolean;
    FCheckBoxGlyph: TBitmap;
    FRadioButtonGlyph: TBitmap;

    FFilteredVisibility: Boolean;
    FOnApplyVisFilter: TApplyVisFilterEvent;
    FRightAlignedText: Boolean;
    FFlat: Boolean;
    FRightAlignedTree: Boolean;
    FPathSeparator: Char;
    FLinesStyle: TPenStyle;
    FLinesColor: TColor;
    FDeselectChildrenOnCollapse: Boolean;
    FDrawFocusRect: Boolean;
    FBarStyle: Boolean;
    FAlwaysKeepFocus : boolean;
    FAlwaysKeepSelection: Boolean;
    FFullRowSelect: Boolean;
    FDragType  : TElDragType;
    FMouseOver : boolean;
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
    FDropTarget: TElDropTarget;
{$ENDIF}
{$ENDIF}
{$endif}

    FDragObject : TDragObject;
    FAutoLookup: Boolean;
    FSelectColumn: Integer;
    FAutoExpand: Boolean;
    FLeafPicture,
    FPlusPicture,
    FMinusPicture: TBitmap;
    FCustomPlusMinus: Boolean;
    FShowHeader,
    FShowCheckboxes: Boolean;

{$IFDEF ELPACK_COMPLETE}
    FImgForm : TElImageForm;
    FImgFormChLink : TImgFormChangeLink;
    FStorage: TElIniFile;
    FStoragePath: string;
{$ENDIF}
    FDragImageMode: TDragImgMode;
    FHideHorzScrollBar: Boolean;
    FHideVertScrollBar: Boolean;
    FExpandOnDblClick: Boolean;
    FHideHintOnMove: Boolean;
    FSortSection: Integer;
    FSortMode: TSortModes;
    FSortType: TSortTypes;
    FDragAllowed: Boolean;
    FBkColor  : TColor;
    FTextColor: TColor;
    FShowButtons: boolean;
    FShowLines: boolean;
    FShowImages: boolean;
    FShowRoot: boolean;
    FLineHintColor: TColor;
    FShowHintMode: THintModes;
    // BMP: TBitmap;
    FBorderStyle: TBorderStyle;

    FCanEdit: boolean;

    FIgnoreSBChange : boolean;
    FScrollbarsInitialized : boolean;

    FSortRequired,
    FProcUpdate, // already in SetIsUpdating
    FUpdated : boolean;
    FInSorting : integer;
    FBSVLines,
    FHLines,
      FVLines: boolean;

    FAllList,
      FSelectedList : TElList;

    FScrollTracking,
      FTracking: boolean;
    FHeaderHotTrack: boolean;
    FODFollowCol: boolean;
    FODMask: TElFString;

    FImages: TImageList;
    FImages2: TImageList;

    FImageChangeLink: TChangeLink;

    FTopIndex,
      FBottomIndex: integer; // visible items
    FChStateImage: boolean;
    FRealHint, 
    FHint       : TElFString;
    FMainTreeCol: integer;
    FMultiSelect: boolean;
    FMultiSelectLevel: Integer; // CNS: -1 any, 0,1,2... limit multiseletion to this level
    FRowSelect  : boolean;
    FHideSelect : boolean;
    FLineHeight : integer;
    FAutoLineHeight: boolean;
    ItemExt        : integer;  
    FUseCustomBars : boolean;

    FTreeIsFocused : boolean;

    FHPos: integer;
    FVScrollVisible,
    FHScrollVisible: boolean;
    FSelMode: TSTSelModes;
    FSortDir: TSortDirs;

    FSelChange: boolean;
    FColSizeUpdate,
    FUpdating : // UpdateCount > 0
                 boolean;
    FUpdateCount: integer;

    FHintHide: boolean;
    FUseSystemHintColors : boolean;
    IgnoreResize: boolean;
    FCurBkColor: TColor;
    FCurTextColor: TColor;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    FDelOnEdit: boolean;
{$endif}
    FAutoSizingColumns: boolean;
    FItems: TElTreeItems;

    FOnColumnResize: TColumnNotifyEvent;
    FOnColumnClick: TColumnNotifyEvent;
    FOnColumnDraw: TElSectionRedrawEvent;
{$IFNDEF VCL_4_USED}
    FOnResize: TNotifyEvent;
{$ENDIF}
    FOnItemChange: TOnItemChangeEvent;
    FOnItemDraw: TOnItemDrawEvent;
    FOnItemChecked : TOnItemCheckedEvent;
    FOnItemExpand: TOnItemExpandEvent;
    FOnItemCollapse: TOnItemExpandEvent;
    FOnItemExpanding: TOnItemExpanding;
    FOnItemCollapsing: TOnItemExpanding;
    FOnItemDelete: TOnItemExpandEvent;
    FOnItemFocused: TNotifyEvent;
    FOnItemPostDraw : TElTreeItemPostDrawEvent;
    FOnShowHint: TOnShowHintEvent;
    FOnCompareItems: TOnCompareItems;
    FOnItemPicDraw: TOnPicDrawEvent;
    FOnItemPicDraw2: TOnPicDrawEvent;
    FOnHotTrack: THotTrackEvent;

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    FOnComboEditShow: TComboEditShowEvent;
    FOnTuneUpInplaceEdit : TTuneUpInplaceEditEvent;
    FOnEditRequest: TEditRequestEvent;
    FOnValidateCombo: TValidateComboEvent;
    FOnValidate: TOnValidateEvent;
{$endif}
{$endif}

    FOnScroll: TElScrollEvent;
    FOnItemSave: TItemSaveEvent;
    FOnItemLoad: TItemSaveEvent;
    FOnTryEdit: TTryEditEvent;
    FOnHeaderColumnMove: TElColumnMoveEvent;
    FOnSave: TCellStyleSaveEvent;
    FOnLoad: TCellStyleSaveEvent;
    FOnItemSelectedChange: TItemSelChangeEvent;
    FOnHeaderLookup: TElHeaderLookupEvent;
    FOnHeaderLookupDone: TElHeaderLookupDoneEvent;
    FOnHeaderResize: TNotifyEvent;
    FOnHeaderSectionExpand: THeaderSectionEvent;
    FOnHeaderSectionCollapse: THeaderSectionEvent;
    FOnHeaderSectionMeasure : TMeasureSectionEvent;
    FOnSectionAutoSize : TColumnNotifyEvent;
    FOnSectionFilterCall : TColumnNotifyEvent;
    FOnMeasureItemPart   : TMeasureItemPartEvent;
    FOnSortBegin,
    FOnSortEnd           : TNotifyEvent;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    FOnEditKeyDown       : TKeyEvent;
{$endif}

{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
    FOnOleDragFinish: TOleDragFinishEvent;
    FOnOleDragStart: TOleDragStartEvent;
    FOnOleTargetDrag: TTargetDragEvent;
    FOnOleTargetDrop: TTargetDropEvent;
{$ENDIF}
{$ENDIF}
{$endif}

    TotalHiddenCount,
    TotalVisCount: integer;
    TotalVarHeightCount : integer;

    FView  : TElTreeView;
    FHeader: TElHeader;
    FHScrollBar : TElScrollBar;
    FVScrollBar : TElScrollBar;
    FHorzScrollBarStyle,
    FVertScrollBarStyle : TElScrollBarStyles;
    FFakeBool : boolean;
    SavedHH   : integer;
    FDelayTimer: TTimer;

    FDelayedItem: TElTreeItem;
    FDragExpandDelay,
    FChangeDelay: Integer;
    FDragTrgDrawMode: TDragTargetDraw;

    FOnHeaderMouseDown: TMouseEvent;
    FOnAfterSelectionChange: TNotifyEvent;
    FDragRectAcceptColor: TColor;
    FDragRectDenyColor: TColor;
    FIncrementalSearch: Boolean;
    FRightClickSelect: Boolean;
    FScrollbarOpposite: Boolean;
    FVerticalLinesLong: Boolean;
    FBorderSides: TElBorderSides;
    {$ifdef ELTREE_USE_INPLACE_EDITORS}
    {$ifdef VER3_EDITORS}
    FOnInplaceEditorNeeded: TInplaceEditorNeededEvent;
    {$endif}
    {$endif}
    
    FCursor : TCursor;
{$IFDEF HAS_HTML_RENDER}
    FOnImageNeeded : TElHTMLImageNeededEvent;
    FOnLinkClick : TElHTMLLinkClickEvent;
    FLinkCursor: TCursor;
    FLinkColor : TColor;
    FLinkStyle : TFontStyles;
{$ENDIF}

    FQuickEditMode: Boolean;
    FMainTextType: TElFieldType;
    FOnHorzScrollHitTest: TElScrollHitTestEvent;
    FMouseFrameSelect : Boolean;
    FVertDivLinesColor: TColor;
    FHorzDivLinesColor: TColor;
    FDragScrollInterval: Integer;
    FShowLeafButton: Boolean;
    FExplorerEditMode: Boolean;
    FCheckBoxSize: Integer;
    FIgnoreEnabled: Boolean;
    FInplaceEditorDelay: Integer;
    FHeaderFont: TFont;
    FHeaderUseTreeFont: Boolean;
    FKeepSelectionWithinLevel: Boolean;
    FAutoCollapse: Boolean;
    FIgnoreResizes: Boolean;
    FSortUseCase: Boolean;
    FLineBorderActiveColor: TColor;
    FLineBorderInactiveColor: TColor;
    FDblClickMode: TElDblClickMode;
    FDoubleBuffered: Boolean;
    {$ifndef CLX_USED}
    InSizeMove : boolean;
    FHook : TElHook;
    {$endif}

{$IFDEF HOTTRACK_CURSOR}
    FTrackingCursor : TCursor;
{$ENDIF}
    procedure SetStripedOddColor(Value: TColor);
    procedure SetStripedEvenColor(Value: TColor);
    procedure SetStripedItems(Value: Boolean);
{$IFDEF HAS_HTML_RENDER}
    procedure TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image : TBitmap); virtual;
    procedure TriggerLinkClickEvent(HRef : string; X, Y: integer); virtual;
    procedure SetLinkColor(newValue : TColor); virtual;
    procedure SetLinkStyle(newValue : TFontStyles); virtual;
{$ENDIF}
    {$ifndef CLX_USED}
    procedure OnBeforeHook(Sender : TObject; var Message : TMessage; var Handled : boolean);
    procedure SetParent(AParent: TWinControl); override;
    {$endif}
    procedure SetVirtualityLevel(Value: TVirtualityLevel);
    procedure SetBorderSides(Value: TElBorderSides);
    function GetDefaultSectionWidth: Integer;
    procedure SetDefaultSectionWidth(Value: Integer);
    procedure OnHeaderSectionResize(Header: TCustomElHeader; Section: TElHeaderSection);
    procedure OnHeaderSectionClick(Header: TCustomElHeader; Section: TElHeaderSection);
    procedure OnHeaderSectionDelete(Header: TCustomElHeader; Section: TElHeaderSection);
    procedure DoHeaderMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure OnHeaderSectionLookup(Sender: TObject; Section: TElHeaderSection; var Text: string);
    procedure OnHeaderSectionLookupDone(Sender: TObject; Section: TElHeaderSection; Text: string; Accepted: boolean);
    procedure OnHeaderExpand(Sender: TCustomElHeader; Section: TElHeaderSection);
    procedure OnHeaderCollapse(Sender: TCustomElHeader; Section: TElHeaderSection);
    procedure OnHeaderSectionVisChange(Sender: TCustomElHeader; Section: TElHeaderSection);
    procedure HeaderSectionAutoSizeHandler(Sender : TCustomElHeader; Section : TElHeaderSection);  { TElHeaderSectionEvent }
    procedure SectionAutoSizeTransfer(Sender : TCustomElHeader; Section : TElHeaderSection);  { TElHeaderSectionEvent }
    procedure SectionFilterCallTransfer(Sender : TCustomElHeader; Section : TElHeaderSection);  { TElHeaderSectionEvent }

    procedure DoHeaderResize(Sender: TObject);
    procedure OnFontChange(Sender: TObject); virtual;
    procedure OnSignChange(Sender: TObject);
    procedure ImageListChange(Sender: TObject);

    function GetDropTarget: TElTreeItem;

    procedure SetTextColor(value: TColor);
    procedure SetBkColor(value: TColor);
{$IFNDEF LITE}
    function  GetHeaderWrapCaptions : boolean;
    procedure SetHeaderWrapCaptions(Value : boolean);
{$ENDIF}
    procedure SetHeaderHotTrack(value: boolean);
    procedure SetHeaderHeight(value: integer);
    procedure SetShowEmptyImages(newValue : boolean);
    procedure SetShowEmptyImages2(newValue : boolean);
    procedure SetImages(Value: TImageList);
    procedure SetImages2(newValue: TImageList);
    procedure SetLineHintTimeout(Value: Integer);

    procedure SetLineStyle(Value: Boolean);
    procedure SetRootStyle(Value: Boolean);
    procedure SetImagesStyle(Value: Boolean);
    {$ifndef CLX_USED}
    procedure SetBorderStyle(Value: TBorderStyle);
    {$endif}
    procedure SetButtonStyle(Value: Boolean);
    procedure SetUpdating(value: boolean);
    function  GetUpdating : boolean;
    procedure SetHLines(value: boolean);
    procedure SetVLines(value: boolean);
    procedure SetBSVLines(value: boolean);
    procedure SetRowSelect(value: boolean);
    procedure SetMultiSelectLevel(Value : integer);
    procedure SetMultiSelect(value: boolean);
    procedure SetFocused(value: TElTreeItem);
    procedure SetHideSelect(value: boolean);
    procedure SetAutoExpand(value: boolean);
    procedure SetMoveFocusOnCollapse(value: boolean);
    function GetHeaderSections: TElHeaderSections;
    procedure SetHeaderSections(value: TElHeaderSections);
    procedure SetChStateImage(value: boolean);
    procedure SetUseStdBars(value : boolean);

    procedure SetItemIndent(value: integer);
    procedure SetLineHeight(value: integer);
    procedure SetAutoLineHeight(value: boolean);
    function GetHeaderHeight: integer;
    procedure SetMainTreeCol(value: integer);
    procedure SetItems(value: TElTreeItems);
    function GetTotalVisCount: integer;
    function GetDraggableSections: Boolean;
    procedure SetDraggableSections(newValue: Boolean);
    procedure SetSortMode(newValue: TSortModes);
    procedure SetSortSection(newValue: Integer);

    function GetMoveColumnOnDrag: Boolean;
    procedure SetMoveColumnOnDrag(newValue: Boolean);
    procedure SetHideHorzScrollBar(newValue: Boolean);
    procedure SetHideVertScrollBar(newValue: Boolean);
    function GetHeaderImages: TImageList;
    procedure SetHeaderImages(newValue: TImageList);

    function GetFireFocusEvents: boolean;
    procedure SetFireFocusEvents(Value: boolean);

    procedure SetScrollbarOpposite(Value: Boolean);
    procedure SetVerticalLinesLong(Value: Boolean);

    function GetSelCount: integer;
    function GetSelected: TElTreeItem;
    function GetFocused : TElTreeItem;
    procedure SetSelected(newValue: TElTreeItem);
{$ifdef SUPPORT_STORAGE}
    procedure SetStorage(newValue: TElIniFile);
{$endif}
{$ifndef CLX_USED}
{$IFDEF ELPACK_COMPLETE}
    procedure SetImageForm(newValue : TElImageForm);
    procedure ImageFormChange(Sender: TObject);
    procedure SetHeaderImageForm(newValue : TElImageForm);
    function GetHeaderImageForm : TElImageForm;
{$ENDIF}
{$endif}
    procedure SetShowCheckboxes(newValue: Boolean);
    procedure SetPlusPicture(newValue: TBitmap);
    procedure SetMinusPicture(newValue: TBitmap);
    procedure SetCustomPlusMinus(newValue: Boolean);
    procedure SetSelectColumn(newValue: Integer);
    procedure SetDragType(newValue: TElDragType);

    procedure HeaderResizeTransfer(Sender: TObject);
    procedure HeaderResizeHandler(Sender: TObject);

    function GetStickyHeaderSections: Boolean;
    procedure SetStickyHeaderSections(newValue: Boolean);
    procedure SetBarStyle(newValue: Boolean);
    procedure SetDrawFocusRect(newValue: Boolean);
    procedure SetLinesColor(newValue: TColor);
    procedure SetHorzDivLinesColor(newValue: TColor);
    procedure SetLinesStyle(newValue: TPenStyle);
    procedure SetRightAlignedTree(newValue: Boolean);
    procedure SetFlat(newValue: Boolean);
    procedure SetRightAlignedText(newValue: Boolean);
    procedure SetFilteredVisibility(newValue: Boolean);
    procedure SetUnderlineTracked(newValue: Boolean);
    procedure SetCustomCheckboxes(newValue: Boolean);
    procedure SetCheckBoxGlyph(newValue: TBitmap);
    procedure SetRadioButtonGlyph(newValue: TBitmap);
    procedure SetShowRootButtons(newValue: Boolean);
    procedure SetHideFocusRect(newValue: Boolean);
    function GetLockHeaderHeight: Boolean;
    procedure SetLockHeaderHeight(newValue: Boolean);
    procedure SetTransButtons(newValue : boolean);

    {$ifndef CLX_USED}
    procedure UpdateFrame;
    {$endif}
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    function GetInEditing : boolean;
{$endif}
{$endif}
    procedure SetHeaderActiveFilterColor(newValue : TColor);
    function GetHeaderActiveFilterColor : TColor;
    procedure SetHeaderFilterColor(newValue : TColor);
    function GetHeaderFilterColor : TColor;
    procedure SetHeaderFlat(newValue : Boolean);
    function GetHeaderFlat : Boolean;

    {$ifdef MSWINDOWS}
    procedure DrawFlatBorder(HorzTracking, VertTracking : boolean);
    procedure DrawFlatBorderEx(DC : Windows.HDC; HorzTracking, VertTracking : boolean);
    {$endif}

{$ifdef HAS_HTML_RENDER}
    procedure ReRenderAllHTMLItems;
{$endif}
    procedure SetFlatFocusedScrollbars(newValue : Boolean);
{$IFNDEF LITE}
    procedure SetBackground(newValue : TBitmap);
    procedure SetBackgroundType(newValue : TElBkGndType);
    procedure BackgroundChange(Sender : TObject);
{$ENDIF}
    {$ifndef CLX_USED}
    procedure WMNCHITTEST(var Msg : TMessage); message WM_NCHITTEST;
    procedure WMVScroll(var Msg : TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg : TWMHScroll); message WM_HSCROLL;
    procedure WMEnable(var Msg : TMessage); message WM_ENABLE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure CMMouseEnter( var Msg: TMessage ); message CM_MouseEnter;
    procedure CMMouseLeave(var Message: TMessage); message CM_MouseLeave;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMSysColorChange(var Msg: TMessage); message WM_SYSCOLORCHANGE;

    {$ifdef ELPACK_COMPLETE}
    procedure IFMRepaintChildren(var Message: TMessage); message
        IFM_REPAINTCHILDREN;
    {$endif}
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message
        WM_WINDOWPOSCHANGED;
    {$ifdef ELPACK_COMPLETE}
    procedure IFMCanPaintBkgnd(var Message: TMessage); message IFM_CANPAINTBKGND;
    {$endif}
    {$else}
    procedure EnabledChanged; override;
    {$endif}

    procedure SetHideSelectColor(newValue: TColor);
    procedure SetFocusedSelectColor(newValue: TColor);

    procedure SetHideSelectTextColor(newValue: TColor);
    procedure SetFocusedSelectTextColor(newValue: TColor);

    procedure SetRowHotTrack(newValue : Boolean);
    procedure SetActiveBorderType(newValue : TElFlatBorderType);
    procedure SetInactiveBorderType(newValue : TElFlatBorderType);
{$IFNDEF LITE}
    procedure SetGradientStartColor(newValue : TColor);
    procedure SetGradientEndColor(newValue : TColor);
    procedure SetGradientSteps(newValue : Integer);
{$ENDIF LITE}
    procedure SetHPosition(value: integer);
    procedure SetVPosition(value: integer);

    procedure ClickTransfer(Sender : TObject);  virtual;
    procedure DblClickTransfer(Sender : TObject); virtual;
    procedure DropTransfer(Sender : TObject; Source : TObject; X : Integer; Y : Integer);  virtual;
    //procedure EndDragTransfer(Sender : TObject; Source : TObject; X : Integer; Y : Integer);
    procedure OverTransfer(Sender : TObject; Source : TObject; X : Integer; Y : Integer; State : TDragState; var Accept : Boolean); virtual;
    procedure DragTransfer(Sender : TObject; Target : TObject; X : Integer; Y : Integer); virtual;
    procedure EnterTransfer(Sender : TObject); virtual;
    procedure ExitTransfer(Sender : TObject); virtual;
    procedure KeyDownTransfer(Sender : TObject; var Key : Word; Shift : TShiftState); virtual;
    procedure KeyPressTransfer(Sender : TObject; var Key : Char); virtual;
    procedure KeyUpTransfer(Sender : TObject; var Key : Word; Shift : TShiftState); virtual;
    procedure MouseDownTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer); virtual;
    procedure MouseMoveTransfer(Sender : TObject; Shift : TShiftState; X : Integer; Y : Integer); virtual;
    procedure MouseUpTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer); virtual;
    procedure StartDragTransfer(Sender : TObject; var DragObject : TDragObject); virtual;
    procedure MeasureSectionTransfer(Sender : TObject; Section : TElHeaderSection; var Size: TPoint); virtual;
    procedure SetCursor(newValue : TCursor);
    function  GetCursor : TCursor;
    {$ifndef CLX_USED}
    function  SetScrollInfo(hWnd: HWND; BarFlag: Integer; const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer;
    function  GetScrollInfo(hWnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): BOOL;
    {$else}
    function  SetScrollInfo(Wnd: TCustomElScrollBar; BarFlag: Integer; const ScrollInfo: TScrollInfo; Redraw: boolean): Integer;
    function  GetScrollInfo(Wnd: TCustomElScrollBar; BarFlag: Integer; var ScrollInfo: TScrollInfo): boolean;
    {$endif}
    procedure SetHorzScrollBarStyle(newValue : TElScrollBarStyles);
    procedure SetVertScrollBarStyle(newValue : TElScrollBarStyles);
    procedure HorzScrollDrawPartTransfer(Sender : TObject; Canvas : TCanvas; R : TRect; Part : TElScrollBarPart; Enabled : Boolean; Focused : Boolean; Pressed : Boolean; var DefaultDraw : Boolean);  { TElScrollDrawPartEvent }
    procedure HorzScrollHintNeededTransfer(Sender : TObject; TrackPosition : Integer; var Hint : TElFString);  { TElScrollHintNeededEvent }
    procedure VertScrollDrawPartTransfer(Sender : TObject; Canvas : TCanvas; R : TRect; Part : TElScrollBarPart; Enabled : Boolean; Focused : Boolean; Pressed : Boolean; var DefaultDraw : Boolean);  { TElScrollDrawPartEvent }
    procedure VertScrollHintNeededHandler(Sender : TObject; TrackPosition : Integer; var Hint  : TElFString);  { TElScrollHintNeededEvent }
    procedure VertScrollHintNeededTransfer(Sender : TObject; TrackPosition : Integer; var Hint : TElFString);  { TElScrollHintNeededEvent }
    function GetHeaderInvertSortArrows : Boolean;
    procedure SetHeaderInvertSortArrows(newValue : Boolean);
    // procedure OnFontChanged(Sender: TObject); virtual;
    procedure SBChanged(Sender: TObject);
    procedure ScrollBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetForcedScrollBars(newValue : TScrollStyle);
    {$ifndef CLX_USED}
    function GetDragCursor : TCursor;
    procedure SetDragCursor(Value : TCursor);
    {$endif}
    procedure SetTrackColor(value : TColor);
{$IFNDEF LITE}
    procedure SetNoBlendSelected(newValue : Boolean);
    function GetLockedHeaderSection : TElHeaderSection;
    procedure SetLockedHeaderSection(newValue : TElHeaderSection);
    procedure SetAdjustMultilineHeight(newValue : Boolean); virtual;
{$ENDIF LITE}
{$IFDEF VCL_4_USED}
    procedure ActionChange(Sender : TObject; CheckDefaults : Boolean); override;
{$ENDIF}
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure AlignPieces;
    function GetRoot: TElTreeItem; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  CompareItems(Item1, Item2: TElTreeItem; SM : TElSSortMode; ST : TSortTypes; FSortSection : integer): integer; virtual;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    procedure SetCanEdit(value: boolean); virtual;
{$endif}
    procedure SetShowHeader(value: boolean); virtual;
    {$ifndef CLX_USED}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    {$else}
    procedure FontChanged; override;
    procedure ColorChanged; override;
    {$endif}
{$IFDEF VCL_4_USED}
    procedure Resize; override;
{$ELSE}
    procedure Resize; dynamic;
{$ENDIF}
    function  DoGetPicture(Item: TElTreeItem): integer; virtual;
    function  DoGetPicture2(Item: TElTreeItem): integer; virtual;
    function  DefineLineHeight: integer; virtual;
    procedure UpdateScrollBars; virtual;
    {$ifndef CLX_USED}
    procedure CreateParams(var Params: TCreateParams); override;
    {$endif}
    function  CreateItems: TElTreeItems; virtual;
    function  CreateItemsExt(ItemClass : TElTreeItemClass) : TElTreeItems; virtual;
    function  CreateHeader: TElHeader; virtual;
    function DoSetFocused(value: TElTreeItem; Forced : boolean): Boolean;
    function DoSetFocusedEx(value: TElTreeItem; Forced, Delayed : boolean): Boolean;
    procedure SetHeaderColor(newValue : TColor); virtual;
    function  GetHeaderColor : TColor; virtual;
    function GetHint: TElFString;
    procedure SetHint(newValue: TElFString);

    procedure DoChanging(Item : TElTreeItem; var AllowChange: Boolean); virtual;
    procedure DoOnColumnResize(SectionIndex: integer); virtual;
    procedure DoColumnClick(SectionIndex: integer); virtual;
    procedure DoItemFocused; virtual;
    procedure DoItemDraw(Item: TElTreeItem; Surface: TCanvas; R: TRect; SectionIndex: integer); virtual;
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    procedure DoValidate(Item: TElTreeItem; Section: TElHeaderSection; var Text: string; var Accept: boolean); virtual;
    procedure NotifyOnEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
{$endif}
{$endif}

    procedure DoItemChange(Item: TElTreeItem; ItemChangeMode: TItemChangeMode); virtual;
    procedure DoItemExpanding(Item: TElTreeItem; var CanProcess: boolean); virtual;
    procedure DoItemCollapsing(Item: TElTreeItem; var CanProcess: boolean); virtual;
    procedure DoItemChecked(Item : TElTreeItem); virtual;
    procedure DoItemExpand(Item: TElTreeItem); virtual;
    procedure DoItemCollapse(Item: TElTreeItem); virtual;
    procedure DoItemDelete(Item: TElTreeItem); virtual;
    procedure DoCompareItems(Item1, Item2: TElTreeItem; var res: integer); virtual;
    procedure DoHeaderDraw(Header: TCustomElHeader; Canvas : TCanvas; Section: TElHeaderSection;
      Rect: TRect; Pressed: Boolean); virtual;
    procedure OnHeaderSectionChange(Sender: TCustomElHeader; Section: TElHeaderSection; Change: TSectionChangeMode); virtual;
    procedure OnHeaderSectionMove(Sender: TCustomElHeader; Section: TElHeaderSection; OldPos, NewPos: integer); virtual;
    procedure TriggerHotTrackEvent(OldItem, NewItem: TElTreeItem); virtual;
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    procedure DoValidateCombo(Item: TElTreeItem; Section: TElHeaderSection; Combo: TComboBox; var Accept: boolean); virtual;
    procedure TriggerEditRequestEvent(Item: TElTreeItem; Section: TElHeaderSection); virtual;
    procedure TriggerComboEditShowEvent(Item: TElTreeItem; Section: TElHeaderSection; Combobox: TCombobox); virtual;
    procedure TriggerTryEditEvent(Item: TElTreeItem; Section: TElHeaderSection;
      var CellType: TElFieldType; var CanEdit: boolean); virtual;
    procedure TriggerTuneUpInplaceEditEvent(Item : TElTreeItem; SectionIndex : integer; Editor : TCustomEdit); virtual;
{$endif}
{$endif}
    procedure TriggerScrollEvent(ScrollBarKind: TScrollBarKind; ScrollCode: integer); virtual;
    procedure TriggerHeaderColumnMoveEvent(Section: TElHeaderSection; OldPos, NewPos: integer); virtual;
    procedure TriggerItemSaveEvent(Stream: TStream; Item: TElTreeItem); virtual;
    procedure TriggerItemLoadEvent(Stream: TStream; Item: TElTreeItem); virtual;
    procedure TriggerItemSelectedChangeEvent(Item: TElTreeItem); virtual;
    procedure DoShowHint(Item: TElTreeItem; Section : TElHeaderSection; var Text: TElFString; HintWindow: THintWindow; MousePos: TPoint; var DoShowHint: boolean); virtual;

    procedure Paint; override;
    procedure OnHeaderSectionCreate(Header: TCustomElHeader; Section: TElHeaderSection); virtual;

    procedure TriggerHeaderLookupEvent(Section: TElHeaderSection; var Text: string); virtual;
    procedure TriggerHeaderLookupDoneEvent(Section: TElHeaderSection; Text: string; Accepted: boolean); virtual;
    procedure TriggerHeaderSectionExpandEvent(Section: TElHeaderSection); virtual;
    procedure TriggerHeaderSectionCollapseEvent(Section: TElHeaderSection); virtual;
    procedure TriggerMeasureItemPartEvent(Item: TElTreeItem; PartIndex: integer; var Size: TPoint); virtual;
    procedure TriggerApplyVisFilterEvent(Item: TElTreeItem; var Hidden: boolean); virtual;
    procedure TriggerItemPostDrawEvent(Canvas : TCanvas; Item : TElTreeItem; ItemRect : TRect; var DrawFocusRect : boolean); virtual;
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
    procedure TriggerOleTargetDragEvent(State: TDragState; Source: TOleDragObject;
      Shift: TShiftState; X, Y: integer; var DragType: TDragType); virtual;
    procedure TriggerOleTargetDropEvent(Source: TOleDragObject; Shift: TShiftState;
      X, Y: integer; var DragType: TDragType); virtual;
    procedure TriggerOleDragStartEvent(var dataObj: IDataObject; var dropSource: IDropSource;
      var dwOKEffects: TDragTypes); virtual;
    procedure TriggerOleDragFinishEvent(dwEffect: TDragType; Result: HResult); virtual;
{$ENDIF}
{$ENDIF}
{$endif}

{$ifndef CLX_USED}
    function GetDragImages: TDragImageList; override;
{$endif}
    procedure AutoSizeAllColumns;
    procedure AutoSizeColumn(SectionIndex : integer);
    function GetTopItem: TElTreeItem; virtual;
    procedure SetTopItem(Item: TElTreeItem); virtual;
    procedure Loaded; override;
    function SectionTypeToSortType(SectionType: TElFieldType): TSortTypes;
    procedure TriggerSortBegin; virtual;
    procedure TriggerSortEnd; virtual;
    function CreateView : TElTreeView; virtual;
    {$ifndef CLX_USED}
    procedure CreateWnd; override;
    {$else}
    procedure CreateWidget; override;
    {$endif}
    procedure StartDelayedFocus(FocusItemToReport : TElTreeItem);
    procedure StopDelayedFocus;
    procedure OnDelayTimer(Sender : TObject);
    procedure DoAfterSelectionChange; virtual;
    procedure SetDragRectAcceptColor(const Value: TColor);
    procedure SetDragRectDenyColor(Value: TColor);
    procedure SetDragTrgDrawMode(Value: TDragTargetDraw);
    function GetVisibleRowCount: Integer;
    procedure DoSetDragTrgDrawMode(Value: TDragTargetDraw; RedrawItem : boolean);
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure UpdateDiffItems;
    procedure SlowCompareItems(Item1, Item2: TElTreeItem; Section : 
        TElHeaderSection; var Result : integer);
    procedure TriggerVirtualTextNeeded(Item : TElTreeItem; SectionIndex : Integer;
        var Text : TElFString); virtual;
    procedure TriggerVirtualHintNeeded(Item : TElTreeItem; var Hint : TElFString); 
        virtual;
    procedure TriggerVirtualValueNeeded(Item : TElTreeItem; SectionIndex : Integer; 
        VarType : integer; var Value : Variant); virtual;
    {$ifdef ELTREE_USE_STYLES}
    procedure TriggerVirtualStyleNeeded(Item : TElTreeItem; SectionIndex : Integer; 
        Style : TElCellStyle); virtual;
    {$endif}

    {$ifdef VER3_EDITORS}
    {$ifdef ELTREE_USE_INPLACE_EDITORS}
    procedure TriggerTryEditEvent(Item: TElTreeItem; SectionIndex : integer;
      var CellType: TElFieldType; var CanEdit: boolean); virtual;
    procedure TriggerInplaceEditorNeeded(Item : TElTreeItem; SectionIndex : Integer;
        SupposedFieldType : TElFieldType; var Editor : TElTreeInplaceEditor);
        virtual;
    {$endif}
    {$endif}
    procedure VertScrollHitTestTransfer(Sender : TObject; X, Y : integer; var Part
        : TElScrollBarPart; var DefaultTest : boolean); virtual;
    procedure HorzScrollHitTestTransfer(Sender : TObject; X, Y : integer; var Part
        : TElScrollBarPart; var DefaultTest : boolean); virtual;
    procedure SetVertDivLinesColor(Value: TColor);
    procedure SetCheckBoxSize(Value: Integer);
    function GetTrackItem: TElTreeItem;
    function GetDragging: Boolean;
    procedure SetShowLeafButton(Value: Boolean);
    procedure SetLeafPicture(Value: TBitmap);
    {$ifdef VCL_4_USED}
    procedure MouseWheelTransfer(Sender : TObject; Shift: TShiftState; WheelDelta:
        Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelDownTransfer(Sender : TObject; Shift: TShiftState;
        MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelUpTransfer(Sender : TObject; Shift: TShiftState; MousePos:
        TPoint; var Handled: Boolean);
    {$endif}
    procedure FitMostChildren(Item : TElTreeItem);
    function GetThemedClassName: WideString; override;
    procedure SetUseXPThemes(const Value: Boolean); override;
    function GetCheckBoxSize: Integer;
    function GetHeaderPopupMenu: TPopupMenu;
    procedure SetHeaderPopupMenu(Value: TPopupMenu);
    procedure SetHeaderFont(Value: TFont);
    procedure SetHeaderUseTreeFont(Value: Boolean);
    procedure HeaderFontChanged(Sender: TObject);
    function IsStripedColorStored: Boolean;
    {$ifndef CLX_USED}
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    {$endif}
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetSortUseCase(Value: Boolean);
    procedure SetLineBorderActiveColor(Value: TColor);
    procedure SetLineBorderInactiveColor(Value: TColor);
    procedure SetDblClickMode(Value: TElDblClickMode);
    procedure SetExpandOnDblClick(Value: Boolean);
    function GetPlusPicture: TBitmap;
    function GetLeafPicture: TBitmap;
    function GetMinusPicture: TBitmap;
    function GetCheckBoxGlyph: TBitmap;
    function GetRadioButtonGlyph: TBitmap;
    procedure OnCheckSignChange(Sender: TObject);
    {$ifndef CLX_USED}
    procedure WMUpdateSBFrame(var Message: TMessage); message WM_UPDATESBFRAME;
    {$endif}
    procedure SetDoubleBuffered(Value: Boolean);

    property TextColor: TColor read FTextColor write SetTextColor default clWindowText;
    property BkColor: TColor read FBkColor write SetBkColor default clWindow;
    property ShowButtons: Boolean read FShowButtons write SetButtonStyle default true;
    {$ifndef CLX_USED}
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    {$endif}
    property ShowLines: Boolean read FShowLines write SetLineStyle default true;
    property ShowImages: Boolean read FShowImages write SetImagesStyle default true;
    property ShowRoot: boolean read FShowRoot write SetRootStyle default false;
    property LineHintMode: THintModes read FShowHintMode write FShowHintMode default shmLong;
    property LineHintColor: TColor read FLineHintColor write FLineHintColor default clWindow;
    property HideSelection: Boolean read FHideSelect write SetHideSelect default false;
    property HideHintOnTimer: boolean read FHintHide write FHintHide default false;
    property Images: TImageList read FImages write SetImages;
    property Images2: TImageList read FImages2 write SetImages2;

    property ChangeStateImage: boolean read FChStateImage write SetChStateImage default false;
    property ShowColumns: Boolean read FShowHeader write SetShowHeader default false;
    property DragTrgDrawMode: TDragTargetDraw read FDragTrgDrawMode write
        SetDragTrgDrawMode default SelColorRect;
    property DraggableSections: Boolean read GetDraggableSections write SetDraggableSections default false; { Published }
    property SelectionMode: TSTSelModes read FSelMode write FSelMode default smUsual;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    property DoInplaceEdit: boolean read FCanEdit write SetCanEdit default true;
{$else}
    property DoInplaceEdit: boolean read FCanEdit write FCanEdit default true;
{$endif}
    property VerticalLines: boolean read FVLines write SetVLines default false;
    property BarStyleVerticalLines : boolean read FBSVLines write SetBSVLines default false;
    property HorizontalLines: boolean read FHLines write SetHLines default false;
    property ScrollTracking: boolean read FScrollTracking write FScrollTracking default false;
    property HotTrack: Boolean read FTracking write FTracking default true;
    property Tracking: boolean read FTracking write FTracking default true;
    property RowSelect: boolean read FRowSelect write SetRowSelect default true;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default true;
    property MultiSelectLevel: Integer read FMultiSelectLevel write SetMultiSelectLevel default -1; // CNS
    property LineHeight: integer read FLineHeight write SetLineHeight nodefault;
    property AutoLineHeight: boolean read FAutoLineHeight write SetAutoLineHeight default true;
    property HeaderHotTrack: boolean read FHeaderHotTrack write SetHeaderHotTrack default true;
    property HeaderSections: TElHeaderSections read GetHeaderSections write SetHeaderSections;
    property HeaderHeight: integer read GetHeaderHeight write SetHeaderHeight nodefault;
    property MainTreeColumn: integer read FMainTreeCol write SetMainTreeCol default 0;
    property OwnerDrawByColumn: boolean read FODFollowCol write FODFollowCol default true;
    property OwnerDrawMask: TElFString read FODMask write FODMask;
    property DragAllowed: Boolean read FDragAllowed write FDragAllowed default false;
    property SortDir: TSortDirs read FSortDir write FSortDir default sdAscend;
    property SortMode: TSortModes read FSortMode write SetSortMode default smNone; { Published }
    property SortSection: Integer read FSortSection write SetSortSection default 0; { Published }
    property SortType: TSortTypes read FSortType write FSortType default stText;
    property HideHintOnMove: Boolean read FHideHintOnMove write FHideHintOnMove default true; { Protected }
    property ExpandOnDblClick: Boolean read FExpandOnDblClick write
        SetExpandOnDblClick default true;
    property MoveColumnOnDrag: Boolean read GetMoveColumnOnDrag write SetMoveColumnOnDrag default false; { Published }
    property HideHorzScrollBar: Boolean read FHideHorzScrollBar write SetHideHorzScrollBar default false; { Published }
    property HideVertScrollBar: Boolean read FHideVertScrollBar write SetHideVertScrollBar default false; { Published }
    property HorzScrollBarStyles : TElScrollBarStyles read FHorzScrollBarStyle write SetHorzScrollBarStyle stored true;
    property VertScrollBarStyles : TElScrollBarStyles read FVertScrollBarStyle write SetVertScrollBarStyle stored true;

    {$IFNDEF LITE}
    property NoBlendSelected : Boolean read FNoBlendSelected write SetNoBlendSelected default false;  { Protected }
    property Background : TBitmap read FBackground write SetBackground;  { Protected }
    property BackgroundType : TElBkGndType read FBackgroundType write SetBackgroundType default bgtColorFill;  { Protected }
    property ScrollBackground : Boolean read FScrollBackground write FScrollBackground default false;  { Protected }
    {$ENDIF}

    property HeaderImages: TImageList read GetHeaderImages write SetHeaderImages; { Protected }
    property DragImageMode: TDragImgMode read FDragImageMode write FDragImageMode default dimNever; { Protected }
{$IFDEF ELPACK_COMPLETE}
    property StoragePath     : string read FStoragePath write FStoragePath;
{$IFDEF SUPPORT_STORAGE}
    property Storage         : TElIniFile read FStorage write SetStorage;
{$ELSE}
    property Storage         : TElIniFile read FStorage write FStorage;
{$ENDIF}
{$ENDIF}

{$ifndef CLX_USED}
{$IFDEF ELPACK_COMPLETE}
    property ImageForm       : TElImageForm read FImgForm write SetImageForm;
    property HeaderImageForm : TElImageForm read GetHeaderImageForm write SetHeaderImageForm;
{$ENDIF}
{$endif}
    property ShowCheckboxes: Boolean read FShowCheckboxes write SetShowCheckboxes default false;
    property PlusPicture: TBitmap read GetPlusPicture write SetPlusPicture;
    property MinusPicture: TBitmap read GetMinusPicture write SetMinusPicture;
    property CustomPlusMinus: Boolean read FCustomPlusMinus write SetCustomPlusMinus default false;
    property SelectColumn: Integer read FSelectColumn write SetSelectColumn default -1;
    property AutoExpand: Boolean read FAutoExpand write SetAutoExpand default false;
    property AutoLookup: Boolean read FAutoLookup write FAutoLookup default false;
    property DragType: TElDragType read FDragType write SetDragType default dtDelphi;
    property FullRowSelect: Boolean read FFullRowSelect write FFullRowSelect default true;
    property AlwaysKeepSelection: Boolean read FAlwaysKeepSelection write FAlwaysKeepSelection default true;
    property AlwaysKeepFocus: Boolean read FAlwaysKeepFocus write FAlwaysKeepFocus default false;
    property StickyHeaderSections: Boolean read GetStickyHeaderSections write SetStickyHeaderSections default false;
    property BarStyle: Boolean read FBarStyle write SetBarStyle default false;
    property DrawFocusRect: Boolean read FDrawFocusRect write SetDrawFocusRect default true;
    property DeselectChildrenOnCollapse: Boolean read FDeselectChildrenOnCollapse write FDeselectChildrenOnCollapse default false;
    property HorzDivLinesColor: TColor read FHorzDivLinesColor write
        SetHorzDivLinesColor default clBtnFace;
    property LinesColor: TColor read FLinesColor write SetLinesColor default clBtnFace;
    property LinesStyle: TPenStyle read FLinesStyle write SetLinesStyle default psDot;
    property PathSeparator: Char read FPathSeparator write FPathSeparator default '\'; { Protected }
    property RightAlignedTree: Boolean read FRightAlignedTree write SetRightAlignedTree default false;
    property Flat: Boolean read FFlat write SetFlat default false; { Protected }
    property RightAlignedText: Boolean read FRightAlignedText write SetRightAlignedText default false;
    property FilteredVisibility: Boolean read FFilteredVisibility write SetFilteredVisibility default false;
    property UnderlineTracked: Boolean read FUnderlineTracked write SetUnderlineTracked default true; { Published }
    property CustomCheckboxes: Boolean read FCustomCheckboxes write SetCustomCheckboxes default false; { Published }
    property CheckBoxGlyph: TBitmap read GetCheckBoxGlyph write SetCheckBoxGlyph;
    property RadioButtonGlyph: TBitmap read GetRadioButtonGlyph write
        SetRadioButtonGlyph;
    property ScrollbarOpposite: Boolean read FScrollbarOpposite write SetScrollbarOpposite;
    property ShowRootButtons: Boolean read FShowRootButtons write SetShowRootButtons default true;
    property ShowEmptyImages : Boolean read FShowEmptyImages write SetShowEmptyImages default false;
    property ShowEmptyImages2: Boolean read FShowEmptyImages2 write SetShowEmptyImages2 default false;

    property HideFocusRect: Boolean read FHideFocusRect write SetHideFocusRect default false;
    property LockHeaderHeight: Boolean read GetLockHeaderHeight write SetLockHeaderHeight default false; { Protected }
    property AutoResizeColumns : Boolean read FAutoResizeColumns write FAutoResizeColumns default True;  { Protected }
    property HeaderActiveFilterColor : TColor read GetHeaderActiveFilterColor write SetHeaderActiveFilterColor default clBlack;
    property HeaderFilterColor : TColor read GetHeaderFilterColor write SetHeaderFilterColor default clBtnText;
    property HeaderFlat : Boolean read GetHeaderFlat write SetHeaderFlat default false;
{$IFNDEF LITE}
    property HeaderWrapCaptions : Boolean read GetHeaderWrapCaptions write SetHeaderWrapCaptions default false;
{$ENDIF}
    property FlatFocusedScrollbars : Boolean read FFlatFocusedScrollbars write SetFlatFocusedScrollbars default true;  { Protected }
    property HideSelectColor: TColor read FHideSelectColor write SetHideSelectColor default clBtnFace;
    property FocusedSelectColor: TColor read FFocusedSelectColor write SetFocusedSelectColor default clHighlight;
    property HideSelectTextColor: TColor read FHideSelectTextColor write SetHideSelectTextColor default clBtnShadow;
    property FocusedSelectTextColor: TColor read FFocusedSelectTextColor write SetFocusedSelectTextColor default clHighlightText;

    property UseCustomScrollBars : boolean read FUseCustomBars write SetUseStdBars default true;

    property RowHotTrack : Boolean read FRowHotTrack write SetRowHotTrack default false;  { Protected }
    property ActiveBorderType : TElFlatBorderType read FActiveBorderType write SetActiveBorderType default fbtSunken;  { Protected }
    property InactiveBorderType : TElFlatBorderType read FInactiveBorderType write SetInactiveBorderType default fbtSunkenOuter;  { Protected }
    property ItemIndent : integer read ItemExt write SetItemIndent default 17;
{$IFNDEF LITE}
    property GradientStartColor : TColor read FGradientStartColor write SetGradientStartColor default clBlack;  { Protected }
    property GradientEndColor : TColor read FGradientEndColor write SetGradientEndColor  default clBlack;  { Protected }
    property GradientSteps : Integer read FGradientSteps write SetGradientSteps default 64;  { Protected }
{$ENDIF LITE}
    property Cursor : TCursor read GetCursor write SetCursor default crArrow;
    property HeaderInvertSortArrows : Boolean read GetHeaderInvertSortArrows write SetHeaderInvertSortArrows default false;  { Protected }
    property MoveFocusOnCollapse : Boolean read FMoveFocusOnCollapse write SetMoveFocusOnCollapse default false;  { Protected }
    property ForcedScrollBars : TScrollStyle read FForcedScrollBars write SetForcedScrollBars default ssNone;  { Protected }
    property PlusMinusTransparent : boolean read FTransButtons write SetTransButtons default false;
    property Hint: TElFString read GetHint write SetHint;
    property DragRectAcceptColor: TColor read FDragRectAcceptColor write
        SetDragRectAcceptColor default clGreen;
    property DragRectDenyColor: TColor read FDragRectDenyColor write
        SetDragRectDenyColor default clRed;
    property DragExpandDelay: Integer read FDragExpandDelay write FDragExpandDelay
        default 500;
    property IncrementalSearch: Boolean read FIncrementalSearch write
        FIncrementalSearch;
    {$IFNDEF LITE}
    property AdjustMultilineHeight : Boolean read FAdjustMultilineHeight write SetAdjustMultilineHeight default true;  { Protected }
    {$ENDIF}
    property ExpandOnDragOver : Boolean read FExpandOnDragOver write FExpandOnDragOver default false;  { Protected }
    {$ifndef CLX_USED}
    property DragCursor : TCursor read GetDragCursor write SetDragCursor;
    {$endif}
    property TrackColor : TColor read FTrackColor write SetTrackColor default clHighlight;
    property UseSystemHintColors : Boolean read FUseSystemHintColors write FUseSystemHintColors default false;  { Protected }
    property HeaderColor : TColor read GetHeaderColor write SetHeaderColor default clBtnFace;
    property ChangeDelay: Integer read FChangeDelay write FChangeDelay default 500;
    property RightClickSelect: Boolean read FRightClickSelect write
        FRightClickSelect default true;
    property StripedOddColor: TColor read FStripedOddColor write SetStripedOddColor stored IsStripedColorStored;
    property StripedEvenColor: TColor read FStripedEvenColor write SetStripedEvenColor stored IsStripedColorStored;
    property StripedItems: Boolean read FStripedItems write SetStripedItems default false;
    {$ifdef ELTREE_USE_INPLACE_EDITORS}
    {$ifdef VER3_EDITORS}
    property OnInplaceEditorNeeded: TInplaceEditorNeededEvent read
        FOnInplaceEditorNeeded write FOnInplaceEditorNeeded;
    {$endif}
    {$endif}
    property QuickEditMode: Boolean read FQuickEditMode write FQuickEditMode
        default false;
    property MainTextType: TElFieldType read FMainTextType write FMainTextType
        default sftText;
    property HintType: TElHintType read FHintType write FHintType
        default shtHintOrText;
    property OnVertScrollHitTest: TElScrollHitTestEvent read FOnVertScrollHitTest
        write FOnVertScrollHitTest;
    property OnHorzScrollHitTest: TElScrollHitTestEvent read FOnHorzScrollHitTest
        write FOnHorzScrollHitTest;
    property MouseFrameSelect: Boolean read FMouseFrameSelect write
        FMouseFrameSelect default true;
    property VertDivLinesColor: TColor read FVertDivLinesColor write
        SetVertDivLinesColor default clBtnFace;

{$IFNDEF VCL_4_USED}
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
{$ENDIF}
    property OnItemChange: TOnItemChangeEvent read FOnItemChange write FOnItemChange;
    property OnItemDraw: TOnItemDrawEvent read FOnItemDraw write FOnItemDraw;
    property OnItemChecked  : TOnItemCheckedEvent read FOnItemChecked write FOnItemChecked;
    property OnItemExpand: TOnItemExpandEvent read FOnItemExpand write FOnItemExpand;
    property OnItemCollapse: TOnItemExpandEvent read FOnItemCollapse write FOnItemCollapse;
    property OnItemExpanding: TOnItemExpanding read FOnItemExpanding write FOnItemExpanding;
    property OnItemCollapsing: TOnItemExpanding read FOnItemCollapsing write FOnItemCollapsing;
    property OnScroll: TElScrollEvent read FOnScroll write FOnScroll;
    property OnItemDeletion: TOnItemExpandEvent read FOnItemDelete write FOnItemDelete;
    property OnChanging: TElTreeChangingEvent read FOnChanging write FOnChanging;
    property OnItemFocused: TNotifyEvent read FOnItemFocused write FOnItemFocused;
    property OnShowLineHint: TOnShowHintEvent read FOnShowHint write FOnShowHint;
    property OnCompareItems: TOnCompareItems read FOnCompareItems write FOnCompareItems;
    property OnItemPicDraw: TOnPicDrawEvent read FOnItemPicDraw write FOnItemPicDraw;
    property OnItemPicDraw2: TOnPicDrawEvent read FOnItemPicDraw2 write FOnItemPicDraw2;
    property OnHotTrack: THotTrackEvent read FOnHotTrack write FOnHotTrack;

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    property OnTuneUpInplaceEdit : TTuneUpInplaceEditEvent read FOnTuneUpInplaceEdit write FOnTuneUpInplaceEdit;
    property OnComboEditShow: TComboEditShowEvent read FOnComboEditShow write FOnComboEditShow;
    property OnValidateCombo: TValidateComboEvent read FOnValidateCombo write FOnValidateCombo;
    property OnValidateInplaceEdit: TOnValidateEvent read FOnValidate write FOnValidate;
    property OnEditKeyDown: TKeyEvent read FOnEditKeyDown write FOnEditKeyDown;
    property OnEditRequest: TEditRequestEvent read FOnEditRequest write FOnEditRequest;
    property OnTryEdit: TTryEditEvent read FOnTryEdit write FOnTryEdit;
{$endif}
{$else}
    property OnTryEdit: TTryEditEvent read FOnTryEdit write FOnTryEdit;
{$endif}
    property OnItemSave: TItemSaveEvent read FOnItemSave write FOnItemSave;
    property OnItemLoad: TItemSaveEvent read FOnItemLoad write FOnItemLoad;
    property OnItemSelectedChange: TItemSelChangeEvent read FOnItemSelectedChange write FOnItemSelectedChange;
    property OnCellStyleSave: TCellStyleSaveEvent read FOnSave write FOnSave;
    property OnCellStyleLoad: TCellStyleSaveEvent read FOnLoad write FOnLoad;
    property OnSortBegin : TNotifyEvent read FOnSortBegin write FOnSortBegin;
    property OnSortEnd : TNotifyEvent read FOnSortEnd write FOnSortEnd;
    
    property OnHeaderResize: TNotifyEvent read FOnHeaderResize write FOnHeaderResize;
    property OnHeaderLookup: TElHeaderLookupEvent read FOnHeaderLookup write FOnHeaderLookup;
    property OnHeaderLookupDone: TElHeaderLookupDoneEvent read FOnHeaderLookupDone write FOnHeaderLookupDone;
    property OnHeaderSectionExpand: THeaderSectionEvent read FOnHeaderSectionExpand write FOnHeaderSectionExpand;
    property OnHeaderSectionCollapse : THeaderSectionEvent read FOnHeaderSectionCollapse write FOnHeaderSectionCollapse;
    property OnHeaderSectionAutoSize : TColumnNotifyEvent read FOnSectionAutoSize write FOnSectionAutoSize;
    property OnHeaderColumnResize: TColumnNotifyEvent read FOnColumnResize write FOnColumnResize;
    property OnHeaderColumnClick: TColumnNotifyEvent read FOnColumnClick write FOnColumnClick;
    property OnHeaderColumnMove: TElColumnMoveEvent read FOnHeaderColumnMove write FOnHeaderColumnMove;
    property OnHeaderColumnDraw: TElSectionRedrawEvent read FOnColumnDraw write FOnColumnDraw;
    property OnHeaderSectionFilterCall : TColumnNotifyEvent read FOnSectionFilterCall write FOnSectionFilterCall;
    property OnHeaderSectionMeasure : TMeasureSectionEvent read FOnHeaderSectionMeasure write FOnHeaderSectionMeasure;
    property OnApplyVisFilter: TApplyVisFilterEvent read FOnApplyVisFilter write FOnApplyVisFilter;
    property OnItemPostDraw : TElTreeItemPostDrawEvent read FOnItemPostDraw write FOnItemPostDraw;
    property OnMeasureItemPart: TMeasureItemPartEvent read FOnMeasureItemPart write FOnMeasureItemPart;
{$IFDEF HAS_HTML_RENDER}
    property OnHTMLImageNeeded : TElHTMLImageNeededEvent read FOnImageNeeded write FOnImageNeeded;
    property OnLinkClick : TElHTMLLinkClickEvent read FOnLinkClick write FOnLinkClick;
    property LinkCursor: TCursor read FLinkCursor write FLinkCursor default crHandPoint;
    property LinkColor : TColor read FLinkColor write SetLinkColor default clBlue;  { Published }
    property LinkStyle : TFontStyles read FLinkStyle write SetLinkStyle;  { Published }
{$ENDIF}

    property OnClick : TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick : TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop : TDragDropEvent read FOnDrop write FOnDrop;
    property OnDragOver : TDragOverEvent read FOnOver write FOnOver;
    //property OnEndDrag : TEndDragEvent read FOnDrag write FOnDrag;
    property OnEnter : TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit : TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown : TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress : Tkeypressevent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp : TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnMouseDown : TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp : TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnStartDrag : TStartDragEvent read FOnStartDrag write FOnStartDrag;
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
    property OnOleTargetDrag: TTargetDragEvent read FOnOleTargetDrag write FOnOleTargetDrag;
    property OnOleTargetDrop: TTargetDropEvent read FOnOleTargetDrop write FOnOleTargetDrop;
    property OnOleDragStart: TOleDragStartEvent read FOnOleDragStart write FOnOleDragStart;
    property OnOleDragFinish: TOleDragFinishEvent read FOnOleDragFinish write FOnOleDragFinish;
{$ENDIF}
{$ENDIF}
{$endif}
    property OnHorzScrollDrawPart : TElScrollDrawPartEvent read FOnHorzScrollDrawPart write FOnHorzScrollDrawPart;
    property OnHorzScrollHintNeeded : TElScrollHintNeededEvent read FOnHorzScrollHintNeeded write FOnHorzScrollHintNeeded;
    property OnVertScrollDrawPart : TElScrollDrawPartEvent read FOnVertScrollDrawPart write FOnVertScrollDrawPart;
    property OnVertScrollHintNeeded : TElScrollHintNeededEvent read FOnVertScrollHintNeeded write FOnVertScrollHintNeeded;

    property OnHeaderMouseDown: TMouseEvent read FOnHeaderMouseDown write
        FOnHeaderMouseDown;
    property OnAfterSelectionChange: TNotifyEvent read FOnAfterSelectionChange
        write FOnAfterSelectionChange;
    property OnItemPreDraw: TOnItemExpandEvent read FOnItemPreDraw write
        FOnItemPreDraw;
    property OnDragTargetChange: TElTreeItemDragTargetEvent read
        FOnDragTargetChange write FOnDragTargetChange;
    property LineHintTimeout: Integer read FLineHintTimeout write
        SetLineHintTimeout default 3000;
    property VerticalLinesLong: Boolean read FVerticalLinesLong write
        SetVerticalLinesLong default true;
    property DefaultSectionWidth: Integer read GetDefaultSectionWidth write 
        SetDefaultSectionWidth;
    property BorderSides: TElBorderSides read FBorderSides write SetBorderSides;
    property LineHintType: TLineHintType read FLineHintType write FLineHintType
        default lhtSmart;
    property OnVirtualTextNeeded: TVirtualTextNeededEvent read FOnVirtualTextNeeded 
        write FOnVirtualTextNeeded;
    property VirtualityLevel: TVirtualityLevel read FVirtualityLevel write 
        SetVirtualityLevel;
    property OnVirtualHintNeeded: TVirtualHintNeededEvent read FOnVirtualHintNeeded
        write FOnVirtualHintNeeded;
    property OnVirtualValueNeeded: TVirtualValueNeededEvent read
        FOnVirtualValueNeeded write FOnVirtualValueNeeded;
    {$ifdef ELTREE_USE_STYLES}
    property OnVirtualStyleNeeded: TVirtualStyleNeededEvent read
        FOnVirtualStyleNeeded write FOnVirtualStyleNeeded;
    {$endif}
    property CheckBoxSize: Integer read GetCheckBoxSize write SetCheckBoxSize
        default 15;
    property DragScrollInterval: Integer read FDragScrollInterval write
        FDragScrollInterval default 100;
    property ShowLeafButton: Boolean read FShowLeafButton write SetShowLeafButton;
    property LeafPicture: TBitmap read GetLeafPicture write SetLeafPicture;
    property ExplorerEditMode: Boolean read FExplorerEditMode write
        FExplorerEditMode;
    property IgnoreEnabled: Boolean read FIgnoreEnabled write FIgnoreEnabled;

    property InplaceEditorDelay: Integer read FInplaceEditorDelay write
        FInplaceEditorDelay default 500;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property HeaderUseTreeFont: Boolean read FHeaderUseTreeFont write
        SetHeaderUseTreeFont default true;
    property KeepSelectionWithinLevel: Boolean read FKeepSelectionWithinLevel write
        FKeepSelectionWithinLevel;
    property AutoCollapse: Boolean read FAutoCollapse write FAutoCollapse default
        false;
    property SortUseCase: Boolean read FSortUseCase write SetSortUseCase default
        true;
    property LineBorderActiveColor: TColor read FLineBorderActiveColor write 
        SetLineBorderActiveColor;
    property LineBorderInactiveColor: TColor read FLineBorderInactiveColor write 
        SetLineBorderInactiveColor;
    property DblClickMode: TElDblClickMode read FDblClickMode write SetDblClickMode
        default dcmExpand;
{$IFDEF HOTTRACK_CURSOR}
    property TrackingCursor: TCursor read FTrackingCursor write FTrackingCursor default crDefault;
{$ENDIF}
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    constructor CreateClass(AOwner: TComponent; ItemClass : TElTreeItemClass);
    destructor Destroy; override;
    procedure Update; override;
    procedure Assign(Source: TPersistent); override;
    procedure FullCollapse; virtual;
    procedure FullExpand; virtual;
{$warnings off}
    function CanFocus : boolean; {$ifndef VCL_5_USED}virtual;{$else}override;{$endif}
{$warnings on}

    function Focused: Boolean; {$IFDEF VCL_4_USED} override; {$ENDIF VCL_4_USED}
    function GetItemRect(ItemIndex: integer): TRect; virtual;
    function GetItemAtY(Y: integer): TElTreeItem; virtual;
    function GetItemAt(X, Y: Integer; var ItemPart: TSTItemPart;
      var HitColumn: integer): TElTreeItem; virtual;
    procedure MeasureCell(Item : TElTreeItem; ColumnNum : integer; var Size : TPoint); virtual;
    function GetNextSelected(Prev: TElTreeItem): TElTreeItem; virtual;
    procedure AllSelected(SelectedItems: TElList); virtual;
    procedure SelectAll; virtual;
    procedure InvertSelection; virtual;
    procedure SelectAllEx(IncludeHidden: boolean); virtual;
    procedure InvertSelectionEx(IncludeHidden: boolean); virtual;
    procedure DeselectAll; virtual;
    procedure DeselectAllEx(IncludeHidden: boolean); virtual;
    procedure SelectRange(FromItem, ToItem: TElTreeItem); virtual;
    procedure SelectRange2(FromItem, ToItem: TElTreeItem; SelectDisabled : boolean); virtual;
    procedure SelectRangeEx(FromItem, ToItem: TElTreeItem; IncludeHidden : boolean); virtual;
    procedure SelectRangeEx2(FromItem, ToItem: TElTreeItem; IncludeHidden, SelectDisabled : boolean); virtual;
    procedure Sort(recursive: boolean); virtual;
{$IFDEF SUPPORT_STORAGE}
    procedure Save; virtual;
    procedure Restore; virtual;
{$ENDIF}
    procedure EnsureVisible(Item: TElTreeItem);
    procedure EnsureVisibleBottom(Item: TElTreeItem);
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    procedure EditItem(Item: TElTreeItem; SectionNum: integer); virtual;
    procedure EndEdit(ByCancel: boolean);
    function IsEditing: boolean;
{$endif}
{$else}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    function IsEditing: boolean;
    procedure EditItem(Item: TElTreeItem; SectionNum: integer); virtual;
    procedure EndEdit(ByCancel: boolean);
{$endif}
{$endif}

    procedure SaveStringsToStream(Stream: TStream); virtual;
    function GetNodeAt(X, Y: integer): TElTreeItem;
    {$ifndef CLX_USED}
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    {$endif}
    function IsInView(Item: TElTreeItem): Boolean; virtual;
    function MeasureColumnWidth(ColumnNum: integer; VisibleOnly : boolean): integer;
    function IndexInView(Item : TElTreeItem): Integer;
    procedure AllSelectedEx(SelectedItems : TElList; Order : boolean);
    procedure AddSortSection(Index : Integer; ReSort : boolean);
    procedure RemoveSortSection(Index : Integer; ReSort : boolean);
    procedure ClearSortList(ReSort : boolean);
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    property InEditState: Boolean read GetInEditing;
{$endif}
{$endif}
    property TopIndex: integer read FTopIndex write SetVPosition;

    property BottomIndex: integer read FBottomIndex;
    property IsUpdating: boolean read GetUpdating write SetUpdating;
    property Items: TElTreeItems read FItems write SetItems;
    property ItemFocused: TElTreeItem read GetFocused write SetFocused;
    property SelectedCount: integer read GetSelCount;
    property FireFocusEvents: boolean read GetFireFocusEvents write SetFireFocusEvents default true;

    property Selected: TElTreeItem read GetSelected write SetSelected;
    property TopItem: TElTreeitem read GetTopItem write SetTopItem;
    property DragObject : TDragObject read FDragObject;
    property View : TElTreeView read FView;
    property HorzScrollBarVisible : boolean read FHScrollVisible;
    property VertScrollBarVisible : boolean read FVScrollVisible;
{$IFNDEF LITE}
    property LockedHeaderSection : TElHeaderSection read GetLockedHeaderSection write SetLockedHeaderSection;  { Public }
    property VisibleRowCount: Integer read GetVisibleRowCount;
{$ENDIF}
    property DropTarget: TElTreeItem read GetDropTarget;
    property HScrollBar: TElScrollBar read FHScrollBar;  (*<+>*)
    property VScrollBar : TElScrollBar read FVScrollBar;  (*<+>*)
    property TrackItem: TElTreeItem read GetTrackItem;
  published
    property LeftPosition: integer read FHPos write SetHPosition;
    {$ifndef CLX_USED}
{$IFDEF VCL_4_USED}
    property BevelKind : TBevelKind read FBevelKindDummy write FBevelKindDummy stored false default bkNone;
{$ENDIF}
    {$endif}
    property HeaderPopupMenu: TPopupMenu read GetHeaderPopupMenu write
        SetHeaderPopupMenu;
    property DoubleBuffered: Boolean read FDoubleBuffered write SetDoubleBuffered
        default true;
  end;

  TElTree = class(TCustomElTree)
  published
    property ActiveBorderType;
    {$ifndef CLX_USED}
    property DragCursor;
    {$endif}
  
    property Align;
    property AlwaysKeepFocus;
    property AlwaysKeepSelection;
    property AutoCollapse;
    property AutoExpand;
    property AutoLineHeight;
    property AutoLookup;
    property AutoResizeColumns;
    property Anchors;
    {$ifndef CLX_USED}
    property Action;
    {$endif}
    property Constraints;
    {$ifndef CLX_USED}
    property DockOrientation;
    property Floating;
    property BevelKind;
    property DoubleBuffered;
    property DragKind;
    {$endif}
    property DefaultSectionWidth;
{$IFNDEF LITE}
    property AdjustMultilineHeight;
    property Background;
    property BackgroundType;
{$ENDIF}
    property BarStyle;
    property BarStyleVerticalLines;
    {$ifndef CLX_USED}
    property BorderStyle;
    {$endif}
    property BorderSides;
    property ChangeDelay;
    property ChangeStateImage;
    property CheckBoxGlyph;
    property CheckBoxSize;
    {$ifndef CLX_USED}
    property Ctl3D;
    {$endif}
// here is fine
    property Color;
    property Cursor;
    property CustomCheckboxes;
    property CustomPlusMinus;
    property DeselectChildrenOnCollapse;
    property DblClickMode;
    property DoInplaceEdit;
    property DragAllowed;

    property DragExpandDelay;
    property DraggableSections;
    property DrawFocusRect;
    property DragImageMode;
    property DragRectAcceptColor;
    property DragRectDenyColor;
    property DragScrollInterval;
    property DragTrgDrawMode;
    property DragType;

    property Enabled;
    property ExpandOnDblClick;
    property ExpandOnDragOver;
    property ExplorerEditMode;
    property FilteredVisibility;
    property Flat;
    property FlatFocusedScrollbars;
    property FocusedSelectColor;
    property FocusedSelectTextColor;
    property ForcedScrollBars;
    property Font stored true;
    property FullRowSelect;

{$IFNDEF LITE}
    property GradientStartColor;
    property GradientEndColor;
    property GradientSteps;
{$ENDIF LITE}
    property HeaderActiveFilterColor;
    property HeaderColor;
    property HeaderHeight;
    property HeaderHotTrack;
    property HeaderInvertSortArrows;
    property HeaderSections;
    property HeaderFilterColor;
    property HeaderFlat;
    property HeaderFont;
    property HeaderUseTreeFont;
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
    property HorizontalLines;
    property HideHorzScrollBar;
    property HideVertScrollBar;
    property Hint;
    property HintType;
    property HorzDivLinesColor;
    property HorzScrollBarStyles;
    property IgnoreEnabled;
{$ifndef CLX_USED}
{$IFDEF ELPACK_COMPLETE}
    property HeaderImageForm;
    property ImageForm;
{$ENDIF}
{$endif}
    property Images;
    property Images2;
    property InactiveBorderType;
    property IncrementalSearch;
    property InplaceEditorDelay;
    property ItemIndent;
    property Items;
    property KeepSelectionWithinLevel;
    property LeafPicture;
    property LineBorderActiveColor;
    property LineBorderInactiveColor;

    property LineHeight;
    property LinesColor;
    property LinesStyle;
    property LineHintColor;
    property LineHintMode default shmLong;
    property LineHintTimeout;
    property LineHintType;
    property LockHeaderHeight;
    property MainTextType;
    property MainTreeColumn;
    property MinusPicture;
    property MoveColumnOnDrag;
    property MoveFocusOnCollapse;
    property MouseFrameSelect;
    property MultiSelect;
    property MultiSelectLevel;
    property OwnerDrawByColumn default true;
    property OwnerDrawMask;
    {$ifndef CLX_USED}
    property ParentCtl3D;
    {$endif}
    property ParentFont;
    property ParentShowHint;
    {$ifndef CLX_USED}
    property PathSeparator;
    {$endif}
    property PlusMinusTransparent;
    property PlusPicture;
    property PopupMenu;
    property QuickEditMode;

    property RadioButtonGlyph;
    property RightAlignedText;
    property RightAlignedTree;
    property RightClickSelect;
    property RowHotTrack;
    property RowSelect;
{$IFNDEF LITE}
    property NoBlendSelected;
    property ScrollBackground;
{$ENDIF}
    property ScrollbarOpposite;
    property ScrollTracking;
    property SelectColumn;
    property ShowButtons;
    property ShowColumns;
    property ShowCheckboxes;
    property ShowEmptyImages;
    property ShowEmptyImages2;
    property ShowHint;
    property ShowImages;
    property ShowLeafButton;
    property ShowLines;
    property ShowRoot;
    property ShowRootButtons;
    property SelectionMode;
    property SortDir;
    property SortMode;
    property SortSection;
    property SortType;
{$IFDEF ELPACK_COMPLETE}
    property Storage;
    property StoragePath;
{$ENDIF}
    property SortUseCase;
    property StickyHeaderSections;
    property StripedOddColor;
    property StripedEvenColor;
    property StripedItems;

    property TabOrder;
    property TabStop;
    property Tracking;
    property TrackColor;
    property UnderlineTracked;
    property UseCustomScrollBars;

    property VertDivLinesColor;
    property VerticalLines;
    property VerticalLinesLong;
    property VertScrollBarStyles;
    property VirtualityLevel;
    property Visible;
    property UseSystemHintColors;
    property UseXPThemes;
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
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    property OnTryEdit;
    property OnInplaceEditorNeeded;
{$endif}
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
    property OnMeasureItemPart;
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
    property OnLinkClick;
    property LinkCursor;
    property LinkColor;
    property LinkStyle;
{$ENDIF}
    property OnVirtualTextNeeded;
    property OnVirtualHintNeeded;
    property OnVirtualValueNeeded;
    {$ifdef ELTREE_USE_STYLES}
    property OnVirtualStyleNeeded;
    {$endif}

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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    {$ifndef CLX_USED}
    property OnStartDock;
    property OnEndDock;
    {$endif}
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
{$IFDEF HOTTRACK_CURSOR}
    property TrackingCursor;
{$ENDIF}
  end;

type

  TElTreeDragObject = class(TDragControlObject)
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
    {$ifndef CLX_USED}
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    {$endif}
    destructor Destroy; override;
  end;

const
  FDivLineWidth = 1;
  CheckMargin   = 2;
  CheckBoxSize  = 15;

  // CNS
  crDragSingleNo    = 20001;
  crDragSingleMove  = 20002;
  crDragSingleCopy  = 20003;
  {
  crDragMultiNo     = 20004;
  crDragMultiMove   = 20005;
  crDragMultiCopy   = 20006;
  }

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
type

{$IFDEF ELPACK_COMPLETE}
{$ifdef ELTREE_USE_EXT_EDITORS}
     TElHackAdvancedComboBox = class(TElAdvancedComboBox)
     protected                  
       procedure WMGetDlgCode(var Message : TMessage); message WM_GETDLGCODE;
     end;
{$endif}
{$endif}

{$IFDEF ELPACK_COMPLETE}
{$ifdef ELTREE_USE_EXT_EDITORS}
     EditBoxClass  = TElButtonEdit;
     ComboBoxClass = TElHackAdvancedComboBox;
     CheckBoxClass = TElCheckBox;
{$ELSE}
     EditBoxClass  = TEdit;
     ComboBoxClass = TComboBox;
     CheckBoxClass = TCheckBox;
{$endif}
{$ELSE}
     EditBoxClass  = TEdit;
     ComboBoxClass = TComboBox;
     CheckBoxClass = TCheckBox;
{$ENDIF}
{$endif}
{$endif}

{$ifndef CLX_USED}
const MultiLineFlags : array[boolean] of integer = (DT_SINGLELINE, 0);
      MultiLineEllipseFlags : array[boolean] of integer = (DT_END_ELLIPSIS, 0);
{$else}
const MultiLineFlags : array[boolean] of integer = (Integer(AlignmentFlags_SingleLine), 0);
      MultiLineEllipseFlags : array[boolean] of integer = (0, 0);

      VK_RETURN  = KEY_RETURN;
      VK_ESCAPE  = KEY_ESCAPE;
      VK_BACK    = KEY_BACKSPACE;
      VK_ADD     = KEY_PLUS;
      VK_SUBTRACT= KEY_MINUS;
      VK_MULTIPLY= Key_Asterisk;
      VK_SPACE   = KEY_SPACE;
      VK_DIVIDE  = KEY_SLASH;
      VK_HOME    = KEY_HOME;
      VK_END     = KEY_END;
      VK_PRIOR   = KEY_PRIOR;
      VK_NEXT    = KEY_NEXT;
      VK_LEFT    = KEY_LEFT;
      VK_RIGHT   = KEY_RIGHT;
      VK_UP      = KEY_UP;
      VK_DOWN    = KEY_DOWN;
{$endif}


var LeafBmp,
    PlusBmp,
    MinusBmp : TBitmap;

implementation

{$define NEW_GETITEMRECT}
{.$DEFINE SPEED_TEST}

uses
{$IFDEF SPEED_TEST}
  dbugintf,
{$ENDIF}
{$ifdef CLX_USED}
  SyncObjs,
{$endif}
  ElStack;

type THackElList = class(TElList)
     end;

     TElHeaderHack = class (TCustomElHeader)
     public
       property IsDesigning;
     end;

     TElScrollBarHack = class(TElScrollBar)
     public
       property IsDesigning;
     end;

    TSearchTextTimeoutThread = class (TThread)
      private
        {$ifndef CLX_USED}
        CriticalSection : TRTLCriticalSection;
        {$else}
        CriticalSection : TCriticalSection;
        {$endif}
        fKeepAlive : Boolean;
        procedure SetKeepAlive (KeepAlive : Boolean);
        function GetKeepAlive : Boolean;
      protected
        procedure Execute; override;
      public
        constructor Create;
        destructor Destroy ;override;

        property KeepAlive : Boolean read GetKeepAlive write SetKeepAlive default False;
    end ;

(*
function GetErrorMsg(ID: integer): string;
var
  p: pchar;
begin
  GetMem(p, 260);
  LoadString(HInstance, ID, p, 260);
  result := StrPas(p);
  FreeMem(p, 260);
end;
*)

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef ELPACK_COMPLETE}
type
  TElIntEdit=class(TCustomEdit)  // CNS
  private
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure KeyPress(var Key: Char); override;
  end;

procedure TElIntEdit.WMGetDlgCode(var Message: TWMGetDlgCode); // CNS
begin
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS; // we want the RETURN key
end;

procedure TElIntEdit.KeyPress(var Key: Char); // CNS
begin                                      // get rid of Windows beep
  if Key = Chr(VK_RETURN) then
    key := Chr(0);
  if Key = Chr(VK_ESCAPE) then
    key := Chr(0);
  if Key <> #0 then
    inherited KeyPress(Key);
end;

{$endif}
{$endif}
{$endif}

{$ifndef CLX_USED}
procedure ChangeButtonColors(AButton : TBitmap);
var i, j : integer;
begin
  for i := 0 to AButton.Width -1 do
  begin
    for j := 0 to AButton.Height -1 do
    begin
      case AButton.Canvas.Pixels[i, j] of
        clDkGray: AButton.Canvas.Pixels[i, j]:= GetSysColor(COLOR_3DSHADOW);//GetSysColor(COLOR_BTNSHADOW);
        clBlack: AButton.Canvas.Pixels[i, j] := GetSysColor(COLOR_WINDOWTEXT);
        clWhite: AButton.Canvas.Pixels[i, j] := GetSysColor(COLOR_WINDOW);
      end;
    end;
  end;
end;
{$endif}

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$IFDEF ELPACK_COMPLETE}
{$ifdef ELTREE_USE_EXT_EDITORS}

procedure TElHackAdvancedComboBox.WMGetDlgCode(var Message : TMessage);
begin
  Message.Result := DLGC_WANTALLKEYS;
end;
{$endif}
{$endif}
{$endif}
{$endif}

procedure TSearchTextTimeoutThread.SetKeepAlive (KeepAlive : Boolean) ;
begin
  {$ifndef CLX_USED}
  EnterCriticalSection(CriticalSection);
  {$else}
  CriticalSection.Enter;
  {$endif}
  try
    fKeepAlive := KeepAlive;
  finally
    {$ifndef CLX_USED}
    LeaveCriticalSection(CriticalSection);
    {$else}
    CriticalSection.Leave;
    {$endif}
  end
end ;

function TSearchTextTimeoutThread.GetKeepAlive : Boolean ;
begin
  {$ifndef CLX_USED}
  EnterCriticalSection(CriticalSection);
  {$else}
  CriticalSection.Enter;
  {$endif}
  try
    Result := fKeepAlive;
  finally
    {$ifndef CLX_USED}
    LeaveCriticalSection(CriticalSection);
    {$else}
    CriticalSection.Leave;
    {$endif}
  end
end ;

constructor TSearchTextTimeoutThread.Create ;
begin
  inherited Create (True) ;
  {$ifndef CLX_USED}
  InitializeCriticalSection(CriticalSection);
  {$else}
  CriticalSection := TCriticalSection.Create;
  {$endif}
  fKeepAlive := False;
end ;

destructor TSearchTextTimeoutThread.Destroy ;
begin
  {$ifndef CLX_USED}
  DeleteCriticalSection(CriticalSection);
  {$else}
  CriticalSection.Free;
  {$endif}
  inherited Destroy;
end ;

procedure TSearchTextTimeoutThread.Execute ;
begin
  while not Terminated do
  begin
    Sleep (500);
    if not KeepAlive then
      break ;
    KeepAlive := False
  end
end ;

{$ifndef CLX_USED}
function TElTreeDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
var {
     FTree      : TCustomElTree;
     IsDragMulti,
    }
    IsDragCopy : Boolean;
begin
  if (Control is TCustomElTree) then
  begin
    IsDragCopy  := ((GetKeyState(VK_CONTROL) and $8000) > 0);
    {
    FTree := TCustomElTree(Control);
    IsDragMulti := (FTree.SelectedCount > 1);
    if IsDragMulti then
    begin
      if not Accepted then
        result := crDragMultiNo
      else
      if IsDragCopy then
        result := crDragMultiCopy // Ctrl key down
      else
        result := crDragMultiMove;
    end
    else
    }
    begin
      if not Accepted then
        result := crDragSingleNo
      else
      if IsDragCopy then
        result := crDragSingleCopy // Ctrl key down
      else
        result := crDragSingleMove;
    end;
  end
  else
    result := inherited GetDragCursor(Accepted, X, Y);
end;
{$endif}

procedure TElTreeDragObject.Finished;
begin
  inherited;
  Free;
end;

destructor TElTreeDragObject.Destroy;
begin
  inherited;
end;

// ****************************************************************************
//                            TElCellControl
// ****************************************************************************

{$ifdef ELTREE_USE_STYLES}
procedure TElCellControl.Update;
begin
  if Visible and (Owner <> nil) then Owner.Update;
end;

constructor TElCellControl.Create;
begin
  inherited Create(nil);
  FVisible := true;
  FEnabled := true;
  FFont := TFont.Create;
  FFont.Color := clBtnText;
  FFont.OnChange := FontChanged;
end;

destructor TElCellControl.Destroy;
begin
  FFont.Free;
  if Owner <> nil then
  begin
    Owner.FControl := nil;
    Owner.Update;
  end;
  inherited;
end;

procedure TElCellControl.SetCaption(newValue: TElFString);
{ Sets data member FCaption to newValue. }
begin
  if (FCaption <> newValue) then
  begin
    FCaption := newValue;
    if Visible then Update;
  end; { if }
end; { SetCaption }

procedure TElCellControl.TriggerClickEvent;
{ Triggers the OnClick event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnClick)) then FOnClick(Self);
end; { TriggerClickEvent }

procedure TElCellControl.TriggerMouseDownEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{ Triggers the onMousedown event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnMouseDown)) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end; { TriggerMouseDownEvent }

procedure TElCellControl.TriggerMouseUpEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{ Triggers the OnMouseUp event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnMouseUp)) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end; { TriggerMouseUpEvent }

procedure TElCellControl.TriggerDblClickEvent;
{ Triggers the OnDblClick event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnDblClick)) then
    FOnDblClick(Self);
end; { TriggerDblClickEvent }

procedure TElCellControl.TriggerMouseMoveEvent(Shift: TShiftState; X, Y: Integer);
{ Triggers the OnMouseMove event. This is a virtual method (descendants of this component can override it). }
begin
  if (assigned(FOnMouseMove)) then
    FOnMouseMove(Self, Shift, X, Y);
end; { TriggerMouseMoveEvent }
(*
procedure TElCellControl.SetBoundsRect(newValue : TRect);
begin
  if not EqualRect(FBoundsRect, newValue) then
  begin
    FBoundsRect := newValue;
    if Visible then Update;
  end;  { if }
end;  { SetBoundsRect }
*)

procedure TElCellControl.SetVisible(newValue: Boolean);
begin
  if (FVisible <> newValue) then
  begin
    FVisible := newValue;
    //if not newValue then FBoundsRect := Rect(0, 0, 0, 0);
    Update;
  end; {if}
end;

procedure TElCellControl.SetEnabled(newValue: Boolean);
begin
  if (FEnabled <> newValue) then
  begin
    FEnabled := newValue;
    Update;
  end; {if}
end; {SetEnabled}

procedure TElCellControl.SetPopupMenu(newValue: TPopupMenu);
begin
  if (FPopupMenu <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FPopupMenu <> nil then
      FPopupMenu.RemoveFreeNotification(Self);
    {$endif}
    FPopupMenu := newValue;
    if FPopupMenu <> nil then FPopupMenu.FreeNotification(Self);
  end;  {if}
end;

procedure TElCellControl.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPopupMenu) then FPopupMenu := nil;
end;

procedure TElCellControl.SetBorderWidth(Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Update;
  end;
end;

function TElCellControl.PassClicks: Boolean;
begin
  Result := false;
end;

procedure TElCellControl.FontChanged(Sender: TObject);
begin
  Update;
end;

procedure TElCellControl.SetFont(newValue: TFont);
begin
  FFont.Assign(newValue);
end;



// ****************************************************************************
//                              TElCellCheckBox  
// ****************************************************************************

{$IFDEF ELPACK_COMPLETE}

procedure TElCellCheckBox.SetState(newValue : TCheckBoxState);
begin
  if FState <> newValue then
  begin
    if (newValue = cbGrayed) and not FAllowGrayed then exit;
    FState := newValue;
    Update;
  end;
end;

procedure TElCellCheckBox.SetAllowGrayed(newValue : Boolean);
begin
  if (not newValue) and (FState = cbGrayed) then State := cbUnchecked;
  FAllowGrayed := newValue;
end;

function TElCellCheckBox.GetChecked : Boolean;
begin
  result := FState = cbChecked;
end;

procedure TElCellCheckBox.SetChecked(newValue : Boolean);
begin
  if newValue
     then State := cbChecked
     else State := cbUnchecked;
end;

procedure TElCellCheckBox.SetAlignment(newValue : TAlignment);
begin
 if (FAlignment <> newValue) then
  begin
    FAlignment := newValue;
    Update;
  end; { if }
end;

procedure TElCellCheckBox.Paint(Canvas: TCanvas; R: TRect);
var
  ARect : TRect;
  SrcRect,
    CheckRect : TRect;
  cbh, ch,
  cw, cbw : integer;
  i : integer;
  FTree : TCustomElTree;
  AFont : TFont;
  FGlyph: TBitmap;
  {$ifdef MSWINDOWS}
  FTheme   : HTheme;
  sid      : integer;
  {$endif}
  {$ifdef CLX_USED}
  PX       : TSize;
  {$endif}

begin
  {Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  }
  inc(R.Right);
  inc(R.Bottom);

  ARect  := R;
  FTree  := FOwner.FOwner.FOwner;
  FGlyph := FTree.CheckBoxGlyph;

  {$ifdef MSWINDOWS}
  if (FTree <> nil) and FTree.IsThemeApplied then
  begin
    FTheme := OpenThemeData(0, 'BUTTON');
    if FTheme <> 0 then
    begin
      (*
      {$ifndef CLX_USED}
      DrawThemeBackground(FTheme, Canvas.Handle, 0, 0, R, @R);
      {$else}
      Canvas.Start;
      DrawThemeBackground(FTheme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), 0, 0, R, @R);
      Canvas.Stop;
      {$endif}
      *)
      case State of
        cbUnchecked:
          sid := CBS_UNCHECKEDNORMAL;
        cbChecked:
          sid := CBS_CHECKEDNORMAL;
        else
          sid := CBS_MIXEDNORMAL;
      end;

      cbw := FTree.CheckBoxSize;
      cbh := cbw;

      ch := R.Bottom - R.Top + 1;
      cw := R.Right {- R.Left} + 1;

      if (Alignment = taRightJustify) xor FTree.RightAlignedText then
      begin
        CheckRect := Rect(R.Left + CheckMargin, R.Top + (ch shr 1 - cbh shr 1), R.Left + CheckMargin + cbw, R.Top + (ch shr 1 + cbh - cbh shr 1));
      end
      else
      if (Alignment = taRightJustify) or FTree.RightAlignedText then (*<+>*)
      begin
        CheckRect := Rect(cw - CheckMargin - cbw, R.Top + (ch shr 1 - cbh shr 1), cw - CheckMargin, R.Top + (ch shr 1 + cbh - cbh shr 1));
      end
      else
      if Alignment = taCenter then
      begin
        CheckRect := Rect((R.Left + R.Right - cbw) shr 1 - 1 , R.Top + (ch shr 1 - cbh shr 1), (R.Left + R.Right - cbw) shr 1 + cbw, R.Top + (ch shr 1 + cbh - cbh shr 1));
      end;

      {$ifndef CLX_USED}
      DrawThemeBackground(FTheme, Canvas.Handle, BP_CHECKBOX, sid, CheckRect, @R);
      {$else}
      Canvas.Start;
      DrawThemeBackground(FTheme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_CHECKBOX, sid, CheckRect, @R);
      {$endif}

      if (Alignment = taRightJustify) xor FTree.RightAlignedText then
      begin
        ARect.Left := ARect.Left + cbw + CheckMargin * 2;
      end else
      begin
        ARect.Right := ARect.Right - cbw - CheckMargin * 2;
      end;
      Canvas.Brush.Style := bsClear;
      InflateRect(ARect, -1, -1);

      {$ifndef CLX_USED}
      DrawThemeText(FTheme, Canvas.Handle, BP_CHECKBOX, sid, PWideChar(WideString(Caption)), Length(WideString(Caption)),DT_SINGLELINE or DT_LEFT or DT_VCENTER, 0, ARect);
      {$else}
      DrawThemeText(FTheme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_CHECKBOX, sid, PWideChar(WideString(Caption)), Length(WideString(Caption)), DT_SINGLELINE or DT_LEFT or DT_VCENTER, 0, ARect);
      Canvas.Stop;
      {$endif}
      CloseThemeData(FTheme);
      exit;
    end;
  end;
  {$endif}
  
  if FTree.FCustomCheckboxes and (not FGlyph.Empty) then
  begin
    cbh := FGlyph.Height;
    cbw := FGlyph.Width div 6;
    case State of
      cbUnchecked :
        if Enabled then SrcRect := Rect(0, 0, cbw, cbh) else SrcRect := Rect(cbw, 0, cbw * 2, cbh);
      cbChecked :
        if Enabled then SrcRect := Rect(cbw * 2, 0, cbw * 3, cbh) else SrcRect := Rect(cbw * 3, 0, cbw * 4, cbh);
      cbGrayed :
        if Enabled then SrcRect := Rect(cbw * 4, 0, cbw * 5, cbh) else SrcRect := Rect(cbw * 5, 0, cbw * 6, cbh);
    end;
    i := 0;
  end
  else
  {$ifndef CLX_USED}
  begin
    cbw := CheckBoxSize;
    cbh := CheckBoxSize;
    i := DFCS_BUTTONCHECK or DFCS_CHECKED;
    begin
      case State of
        cbChecked : i := DFCS_BUTTONCHECK or DFCS_CHECKED;
        cbUnchecked : i := DFCS_BUTTONCHECK;
        cbGrayed : i := DFCS_BUTTON3STATE or DFCS_CHECKED;
      end; // case
    end;
    if (not Enabled) {or (FMouseInControl and FPressed) }then i := i or DFCS_INACTIVE;
  end;
  {$else}
  begin
    i := 0;
    QStyle_indicatorSize(Application.Style.Handle, @PX);
    cbw := PX.cx;
    cbh := PX.cy;

    case State of
      cbChecked : i := integer(QButtonToggleState_On);
      cbUnchecked : i := integer(QButtonToggleState_Off);
      cbGrayed : i := integer(QButtonToggleState_NoChange);
    end; // case

    // if (not Enabled) then i := i or DFCS_INACTIVE;
  end;
  {$endif}
  ch := R.Bottom - R.Top + 1;
  cw := R.Right {- R.Left} + 1;
  if (Alignment = taRightJustify) xor FTree.RightAlignedText then
  begin
    CheckRect := Rect(R.Left + CheckMargin, R.Top + (ch shr 1 - cbh shr 1), R.Left + CheckMargin + cbw, R.Top + (ch shr 1 + cbh - cbh shr 1));
  end
  else
  if (Alignment = taRightJustify) or FTree.RightAlignedText then (*<+>*)
  begin
    CheckRect := Rect(cw - CheckMargin - cbw, R.Top + (ch shr 1 - cbh shr 1), cw - CheckMargin, R.Top + (ch shr 1 + cbh - cbh shr 1));
  end
  else
  if Alignment = taCenter then
  begin
    CheckRect := Rect((R.Left + R.Right - cbw) shr 1 - 1 , R.Top + (ch shr 1 - cbh shr 1), (R.Left + R.Right - cbw) shr 1 + cbw, R.Top + (ch shr 1 + cbh - cbh shr 1));
  end;
  {$ifdef CLX_USED}
  Canvas.Start;
  {$endif}
  if FTree.FCustomCheckboxes and (not FGlyph.Empty) then
  begin
    {$ifndef CLX_USED}
    Canvas.CopyRect(CheckRect, FGlyph.Canvas, SrcRect);
    {$else}
    FGlyph.Canvas.Start;
    with SrcRect do
      bitblt(QPainter_Device(Canvas.Handle), CheckRect.Left, CheckRect.Top, QPainter_Device(FGlyph.Canvas.Handle), Left, Top, Right - Left, Bottom - Top, RasterOp_CopyRop, true);
    FGlyph.Canvas.Stop;
    {$endif}
  end
  else
  begin
    // Canvas.Brush.Style := bsClear;
    {$ifndef CLX_USED}
    DrawFrameControl(Canvas.Handle, CheckRect, DFC_BUTTON, i);
    {$else}
    Canvas.Brush.Style := bsClear;
    Canvas.Brush.Color := clBtnFace;
    // this is a dirty trick
    Canvas.FillRect(checkRect);

    QStyle_DrawIndicator(Application.Style.Handle,
                         Canvas.Handle,
                         CheckRect.Left,
                         CheckRect.Top,
                         CheckRect.Right - CheckRect.Left,
                         CheckRect.Bottom - CheckRect.Top,
                         QWidget_colorGroup(FTree.Handle),
                         i, false, Enabled);
    Canvas.Brush.Color := clBtnFace;
    Canvas.Brush.Style := bsSolid;
    {$endif}
    // if Flat and Enabled then DrawFlatFrame(CheckRect);
  end;
  if (Alignment = taRightJustify) xor FTree.RightAlignedText then
  begin
    ARect.Left := ARect.Left + cbw + CheckMargin * 2;
  end else
  begin
    ARect.Right := ARect.Right - cbw - CheckMargin * 2;
  end;
  if Length(Caption) > 0 then
  begin
    Canvas.Brush.Style := bsClear;
    InflateRect(ARect, -1, -1);
    AFont := TFont.Create;
    AFont.Assign(Canvas.Font);
    Canvas.Font.Assign(Font);

    if not Enabled then
    begin
      OffsetRect(ARect, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      {$ifndef CLX_USED}
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Caption), Length(Caption), ARect, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
      {$else}
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), ARect, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
      {$endif}
      {$else}
      Canvas.TextRect(ARect, ARect.Left, ARect.Top, WideString(Caption), Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter));
      {$endif}
      Canvas.Font.Color := clBtnShadow;
      OffsetRect(ARect, -1, -1);
      {$ifndef CLX_USED}
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Caption), Length(Caption), ARect, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
      {$else}
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), ARect, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
      {$endif}
      {$else}
      Canvas.TextRect(ARect, ARect.Left, ARect.Top, WideString(Caption), Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter));
      {$endif}
    end
    else
    begin
      Canvas.Font.Color := FTree.TextColor;

      if FOwner.FOwner.Selected then
      begin
        if ((not FTree.FBarStyle) and (FOwner.FOwner.BorderStyle = ibsNone)) and
           (((FTree.FSelectColumn = FTree.FMainTreeCol) and (FTree.FShowHeader)) or
             FTree.FRowSelect) then
        begin
          if ({((GetParentForm(FTree) <> nil) and (GetParentForm(FTree).ActiveControl = FTree))} FTree.FView.FHasFocus or (not FTree.FHideSelect)) then
            Canvas.Font.Color := FTree.FFocusedSelectTextColor
          else
            Canvas.Font.Color := FTree.FHideSelectTextColor;
        end;
      end;

      {$ifndef CLX_USED}
      {$ifdef ELPACK_UNICODE}
      DrawTypedTextW(Canvas, ARect, Caption, DT_SINGLELINE or DT_LEFT or DT_VCENTER, tdtNormal);
      {$else}
      DrawTypedText(Canvas, ARect, Caption, DT_SINGLELINE or DT_LEFT or DT_VCENTER, tdtNormal);
      {$endif}
      {$else}
      Canvas.TextRect(ARect, ARect.Left, ARect.Top, WideString(Caption), Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter));
      {$endif}
    end;
    Canvas.Brush.Style := bsSolid;
    Canvas.Font := AFont;
    AFont.Free;
  end;
  {$ifdef CLX_USED}
  Canvas.Stop;
  {$endif}
end; { Paint }

procedure TElCellCheckBox.TriggerClickEvent;
var
  State : TCheckBoxState;
begin
  State := Self.State;
  case State of
    cbChecked : Checked := false;
    cbGrayed : Checked := true;
    cbUnchecked :
      if AllowGrayed then Self.State := cbGrayed else Checked := true;
  end;
  inherited;
end;

procedure TElCellCheckBox.TriggerMouseDownEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TElCellCheckBox.TriggerMouseUpEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TElCellCheckBox.Assign(Source: TElCellControl);
begin
  inherited;
end;

constructor TElCellCheckBox.Create;
begin
  inherited;
  Alignment := taRightJustify;
end;

destructor TElCellCheckBox.Destroy;
begin
  inherited;
end;

// ****************************************************************************
//                                   TElCellButton       
// ****************************************************************************

function TElCellButton.GetGlyph: TBitmap;
begin
  Result := FGlyph.Glyph;
end; {GetGlyph}

procedure TElCellButton.GlyphChanged(Sender: TObject);
begin
  Update;
end;

procedure TElCellButton.SetGlyph(newValue: TBitmap);
begin
  FGlyph.Glyph := newValue;
  Update;
end; {SetGlyph}

procedure TElCellButton.SetDown(newValue: Boolean);
begin
  if (FDown <> newValue) then
  begin
    FDown := newValue;
    Update;
  end; {if}
end; {SetDown}

procedure TElCellButton.Assign;
begin
  if Source is TElCellButton then
    with Source as TElCellButton do
    begin
      Self.Glyph := Glyph;
      Self.FFixClick := FixClick;
      Self.FDown := Down;
    end else inherited;
end;

procedure TElCellButton.TriggerMouseDownEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FFixClick then SetDown(not Down) else SetDown(true);
  inherited;
end;

procedure TElCellButton.TriggerMouseUpEvent(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FFixClick then SetDown(false);
  inherited;
end;

procedure TElCellButton.Paint(Canvas: TCanvas; R: TRect);
var
  {$ifndef CLX_USED}
  DrawFlags: Integer;
  {$endif}
  FState   : TElButtonState;
  FBrush   : TBrush;
  FPen     : TPen;
  FSFont   : TFont;
  FTree    : TCustomElTree;
  {$ifdef MSWINDOWS}
  FTheme   : HTheme;
  sid      : integer;
  {$endif}
begin
  inc(R.Right);
  inc(R.Bottom);

  FTree := FOwner.FOwner.FOwner;

  {$ifdef MSWINDOWS}
  if (FTree <> nil) and FTree.IsThemeApplied then
  begin
    FTheme := OpenThemeData(0, 'BUTTON');
    if FTheme <> 0 then
    begin
      (*
      {$ifndef CLX_USED}
      DrawThemeBackground(FTheme, Canvas.Handle, 0, 0, R, @R);
      {$else}
      Canvas.Start;
      DrawThemeBackground(FTheme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), 0, 0, R, @R);
      Canvas.Stop;
      {$endif}
      *)
      if FDown then
        sid := PBS_PRESSED
      else
        sid := PBS_NORMAL;
      {$ifndef CLX_USED}
      DrawThemeBackground(FTheme, Canvas.Handle, BP_PUSHBUTTON, sid, R, @R);
      {$else}
      Canvas.Start;
      DrawThemeBackground(FTheme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_PUSHBUTTON, sid, R, @R);
      Canvas.Stop;
      {$endif}

      if not FEnabled then
        FState := ebsDisabled
      else
      if FDown then
      begin
        FState := ebsDown;
        OffsetRect(R, 1, 1);
      end
      else
        FState := ebsUp;

      FBrush   := TBrush.Create;
      FPen     := TPen.Create;
      FSFont   := TFont.Create;
      FBrush.Assign(Canvas.Brush);
      FPen.Assign(Canvas.Pen);
      FSFont.Assign(Canvas.Font);
      Canvas.Font.Assign(FFont);
      FGlyph.Draw(Canvas, R, Point(0, 0), Caption, FLayout, 1, 1, FState, FState, taCenter, false,
        Pos(#13#10, FCaption) > 0, false, true, true, 0, tdtNormal, clBtnFace, true, FTheme, BP_PUSHBUTTON, sid, false, false{$ifdef HAS_HTML_RENDER}, false, nil{$endif}, true);
      Canvas.Font.Assign(FSFont);
      Canvas.Pen.Assign(FPen);
      Canvas.Brush.Assign(FBrush);
      FBrush.Free;
      FPen.Free;
      FSFont.Free;

      CloseThemeData(FTheme);
      exit;
    end;
  end;
  {$endif}

  {$ifndef CLX_USED}
  DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
  if (FDown) then DrawFlags := DrawFlags or DFCS_PUSHED;
  DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DrawFlags);
  InflateRect(R, 1, 1);
  {$else}
  Canvas.Start;
  Canvas.Brush.Color := clBtnFace;
  //Canvas.FillRect(R);
  with R do
    QStyle_drawButton(Application.Style.Handle,
                      Canvas.Handle,
                      Left,
                      Top,
                      Right - Left,
                      Bottom - Top,
                      FTree.Palette.ColorGroup(GetColorGroup(FTree)),
                      FDown,
                      nil);
  Canvas.Stop;
  {$endif}
  if not FEnabled then
    FState := ebsDisabled
  else
  if FDown then
  begin
    FState := ebsDown;
    OffsetRect(R, 1, 1);
  end
  else
    FState := ebsUp;

  FBrush   := TBrush.Create;
  FPen     := TPen.Create;
  FSFont   := TFont.Create;
  FBrush.Assign(Canvas.Brush);
  FPen.Assign(Canvas.Pen);
  FSFont.Assign(Canvas.Font);
  Canvas.Font.Assign(FFont);
  FGlyph.Draw(Canvas, R, Point(0, 0), Caption, FLayout, 1, 1, FState, FState, taCenter, false,
    Pos(#13#10, FCaption) > 0, false, true, true, 0, tdtNormal, clBtnFace, false, 0, 0, 0, false, false{$ifdef HAS_HTML_RENDER}, false, nil{$endif}, true);
  Canvas.Font.Assign(FSFont);
  Canvas.Pen.Assign(FPen);
  Canvas.Brush.Assign(FBrush);
  FBrush.Free;
  FPen.Free;
  FSFont.Free;
end; {Paint}

procedure TElCellButton.SetLayout(newValue: TButtonLayout);
begin
  if (FLayout <> newValue) then
  begin
    FLayout := newValue;
    Update;
  end; {if}
end;

function TElCellButton.GetUseImageList : Boolean;
{ Returns the value of data member FUseImageList. }
begin
  result := FGlyph.UseImageList; //FUseImageList;
end;  { GetUseImageList }

procedure TElCellButton.SetUseImageList(newValue : Boolean);
{ Sets data member FUseImageList to newValue. }
begin
  if (FGlyph.UseImageList <> newValue) then
  begin
    FGlyph.UseImageList := newValue;
    Update;
  end;  { if }
end;  { SetUseImageList }

function TElCellButton.GetImageList : TImageList;
{ Returns the value of data member FImageList. }
begin
  result := FGlyph.ImageList;
end;  { GetImageList }

procedure TElCellButton.SetImageList(newValue : TImageList);
{ Sets data member FImageList to newValue. }
begin
  if (FGlyph.ImageList <> newValue) then
  begin
    {$ifdef VCL_5_USED}
    if FGlyph.ImageList <> nil then FGlyph.ImageList.RemoveFreeNotification(Self);
    {$endif}
    FGlyph.ImageList := newValue;
    if FGlyph.ImageList <> nil then FGlyph.ImageList.FreeNotification(Self);
    if UseImageList then Update;
  end;  { if }
end;  { SetImageList }

function TElCellButton.GetImageIndex : Integer;
{ Returns the value of data member FImageIndex. }
begin
  result := FGlyph.ImageIndex;
end;  { GetImageIndex }

procedure TElCellButton.SetImageIndex(newValue : Integer);
{ Sets data member FImageIndex to newValue. }
begin
  if (FGlyph.ImageIndex <> newValue) then
  begin
    FGlyph.ImageIndex := newValue;
    if UseImageList then Update;
  end;  { if }
end;  { SetImageIndex }

destructor TElCellButton.Destroy;
begin
  Dec(ButtonCount);
  if ButtonCount <= 0 then
  begin
    Pattern.Free;
    Pattern := nil;
  end;
  FGlyph.Free;
  inherited;
end;

constructor TElCellButton.Create;
begin
  FGlyph := TElCellButtonGlyph.Create;
  FGlyph.OnChange := GlyphChanged;
  inherited;
  Inc(ButtonCount);
  FDown := False;
  FLayout := blGlyphLeft;
end; {Create}

{$ENDIF}

constructor TElCellProgressBar.Create;
begin
  inherited;
  FMaxValue := 100;
  FColor := clWindow;
  FBarColor := clHighlight;
  FFrameColor := clWindowText;
  FShowProgressText := true;
  FTextAlignment := taCenter;
end; {Create}

destructor TElCellProgressBar.Destroy;
begin
  inherited;
end;

procedure TElCellProgressBar.Assign;
begin
  if Source is TElCellProgressBar then
    with Source as TElCellProgressBar do
    begin
      Self.FBarColor := BarColor;
      Self.FMaxValue := MaxValue;
      Self.FMinValue := MinValue;
      Self.FShowProgressText := ShowProgressText;
      Self.FTextAlignment := TextAlignment;
      Self.FValue := Value;
      Self.FFrameColor := FFrameColor;
      Self.FColor := Color;
    end else
      inherited;
end;

procedure TElCellProgressBar.Paint(Canvas: TCanvas; R: TRect);
var
  BrushColor,
  PenColor : TColor;
  FFont    : TFont;
  FillRect,
  BarRect,
  TextRect : TRect;
  MCaption,
  ACaption  : TElFString;
  FTree    : TCustomElTree;
begin
  //inc(R.Right);
  inc(R.Bottom);
  BarRect := R;
  FTree := FOwner.FOwner.FOwner;

  if (ShowProgressText or (Caption <> '')) and (TextAlignment in [taLeftJustify, taRightJustify]) then
  begin
    SetRectEmpty(TextRect);

    if ShowProgressText then
    begin
      ACaption := IntToStr(MulDiv(Value , 100, (MaxValue - MinValue))) + '%';
      MCaption := '100%';
    end
    else
    begin
      ACaption := Caption;
      MCaption := Caption;
    end;

    FFont := TFont.Create;
    FFont.Assign(Canvas.Font);
    Canvas.Font.Assign(FTree.Font);
    if FOwner.FOwner.Selected then
    begin                    
      if ((not FTree.FBarStyle) and (FOwner.FOwner.BorderStyle = ibsNone)) and
         (((FTree.FSelectColumn = FTree.FMainTreeCol) and (FTree.FShowHeader)) or
           FTree.FRowSelect) then
      begin
        if ({((GetParentForm(FTree) <> nil) and (GetParentForm(FTree).ActiveControl = FTree))} FTree.FView.FHasFocus or (not FTree.FHideSelect)) then
          Canvas.Font.Color := FTree.FFocusedSelectTextColor
        else
          Canvas.Font.Color := FTree.FHideSelectTextColor;
      end;
    end;
    {$ifndef CLX_USED}
    {$ifdef ELPACK_UNICODE}
    ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(MCaption), Length(MCaption), TextRect, DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_CALCRECT);
    {$else}
    DrawText(Canvas.Handle, PChar(MCaption), Length(MCaption), TextRect, DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_CALCRECT);
    {$endif}
    {$else}
    Canvas.TextExtent(MCaption, TextRect, Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter));
    {$endif}
    if TextRect.Right - TextRect.Left < R.Right - R.Left then
    begin
      if (TextAlignment = taRightJustify) xor FTree.FRightAlignedTree then
      begin
        BarRect.Right  := R.Right - (TextRect.Right - TextRect.Left) - 2;
        TextRect.Left  := R.Right - (TextRect.Right - TextRect.Left);
        TextRect.Right := R.Right;
      end
      else
      begin
        BarRect.Left   := R.Left + (TextRect.Right - TextRect.Left) + 2;
        TextRect.Right := R.Left + (TextRect.Right - TextRect.Left);
        TextRect.Left  := R.Left;
      end;
      TextRect.Top := BarRect.Top;
      TextRect.Bottom := BarRect.Bottom;
      Canvas.Brush.Style := bsClear;
      {$ifndef CLX_USED}
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(ACaption), Length(ACaption), TextRect, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
      {$else}
      DrawText(Canvas.Handle, PChar(ACaption), Length(ACaption), TextRect, DT_SINGLELINE or DT_LEFT or DT_VCENTER);
      {$endif}
      {$else}
      Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top, ACaption, Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter));
      {$endif}
    end;
    Canvas.Font.Assign(FFont);
    FFont.Free;
  end;

  if BarRect.Right > BarRect.Left then
  begin
    BrushColor := Canvas.Brush.Color;
    PenColor := Canvas.Pen.Color;
    Canvas.Pen.Color := FrameColor;
    Canvas.Brush.Color := Color;
    with BarRect do
      Canvas.Rectangle(Left, Top, Right, Bottom);

    FillRect := BarRect;

    if Value > MinValue then
    begin
      if not FTree.FRightAlignedTree then
      begin
        FillRect.Left := BarRect.Left;
        FillRect.Right := min(BarRect.Right - 1, FillRect.Left + MulDiv(BarRect.Right - BarRect.Left, Value, MaxValue - MinValue));
        InflateRect(FillRect, -1, -1);
        inc(FillRect.Right);
      end
      else
      begin
        FillRect.Right := BarRect.Right;
        FillRect.Left := max(BarRect.Left - 1, FillRect.Right - MulDiv(BarRect.Right - BarRect.Left, Value, MaxValue - MinValue));
        InflateRect(FillRect, -1, -1);
        dec(FillRect.Left);
      end;
      Canvas.Brush.Color := BarColor;
      Canvas.FillRect(FillRect);
    end;
    Canvas.Brush.Color := BrushColor;
    Canvas.Pen.Color := PenColor;

    if (ShowProgressText or (Caption <> '')) and (TextAlignment = taCenter) then
    begin
      SetRectEmpty(TextRect);
      FFont := TFont.Create;
      FFont.Assign(Canvas.Font);
      Canvas.Font.Assign(FTree.Font);

      if FOwner.FOwner.Selected then
      begin
        if ((not FTree.FBarStyle) and (FOwner.FOwner.BorderStyle = ibsNone)) and
           (((FTree.FSelectColumn = FTree.FMainTreeCol) and (FTree.FShowHeader)) or
             FTree.FRowSelect) then
        begin
          if ({((GetParentForm(FTree) <> nil) and (GetParentForm(FTree).ActiveControl = FTree))} FTree.FView.FHasFocus or (not FTree.FHideSelect)) then
            Canvas.Font.Color := FTree.FFocusedSelectTextColor
          else
            Canvas.Font.Color := FTree.FHideSelectTextColor;
        end;
      end;

      if ShowProgressText then
        ACaption := IntToStr(MulDiv(Value , 100, (MaxValue - MinValue))) + '%'
      else
        ACaption := Caption;

      {$ifndef CLX_USED}
      {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(ACaption), Length(ACaption), TextRect, DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_CALCRECT);
      {$else}
      DrawText(Canvas.Handle, PChar(ACaption), Length(ACaption), TextRect, DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_CALCRECT);
      {$endif}
      {$else}
      Canvas.TextExtent(ACaption, Textrect, Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter));
      {$endif}
      if TextRect.Right - TextRect.Left < R.Right - R.Left then
      begin
        Canvas.Brush.Style := bsClear;
        TextRect := R;
        {$ifndef CLX_USED}
        {$ifdef ELPACK_UNICODE}
        ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(ACaption), Length(ACaption), TextRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        {$else}
        DrawText(Canvas.Handle, PChar(ACaption), Length(ACaption), TextRect, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        {$endif}
        {$else}
        Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top, ACaption, Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter));  
        {$endif}
      end;
      Canvas.Font.Assign(FFont);
      FFont.Free;
    end;
  end;
end; {Paint}

procedure TElCellProgressBar.SetMinValue(Value: Integer);
begin
  if FMinValue <> Value then
  begin
    FMinValue := Value;
    Update;
  end;
end;

procedure TElCellProgressBar.SetMaxValue(Value: Integer);
begin
  if FMaxValue <> Value then
  begin
    FMaxValue := Value;
    Update;
  end;
end;

procedure TElCellProgressBar.SetValue(Value: Integer);
begin
  if FValue <> Value then
  begin
    FValue := Min(Max(MinValue, Value), MaxValue);
    Update;
  end;
end;

procedure TElCellProgressBar.SetBarColor(Value: TColor);
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    Update;
  end;
end;

procedure TElCellProgressBar.SetShowProgressText(Value: Boolean);
begin
  if FShowProgressText <> Value then
  begin
    FShowProgressText := Value;
    Update;
  end;
end;

procedure TElCellProgressBar.SetTextAlignment(Value: TAlignment);
begin
  if FTextAlignment <> Value then
  begin
    FTextAlignment := Value;
    Update;
  end;
end;

procedure TElCellProgressBar.SetFrameColor(Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Update;
  end;
end;

procedure TElCellProgressBar.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Update;
  end;
end;

function TElCellProgressBar.PassClicks: Boolean;
begin
  Result := true;
end;


{$endif ELTREE_USE_STYLES}

// ****************************************************************************
//                                TElTreeView
// ****************************************************************************

constructor TElTreeView.Create(Owner : TComponent);
begin
  FOwner := Owner as TCustomElTree;
  inherited;
  ControlStyle := ControlStyle + [csOpaque, csDisplayDragImage];
{$IFNDEF LITE}
  FTmpBmp := TBitmap.Create;
{$ENDIF}
  FVisible := TElList.Create;
  FHRange := -1;
  {$ifdef CLX_USED}
  InputKeys := [ikNav];
  {$endif}
end;

destructor TElTreeView.Destroy;
begin
  StopClearSearchTimeoutThread;

  if FDragExpandTimer <> nil then
  begin
    FDragExpandTimer.Free;
    FDragExpandTimer := nil;
  end;
  if FHintItem <> nil then
     DoHideLineHint;
  FHintItemEx := nil;
  If FHintTimer <> nil then
  begin
    FHintTimer.Free;
    FHintTimer := nil;
  end;
  if FHintWnd <> nil then
  begin
    FHintWnd.Free;
    FHintWnd := nil;
  end;
  if FDragScrollTimer <> nil then
  begin
    FDragScrollTimer.Free;
    FDragScrollTimer := nil;
  end;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
  if FInpEdit <> nil then
  begin
    DoEndEdit(true);
    if FFakePopup <> nil then FFakePopup.Free;
    FFakePopup := nil;
  end;
  if FEditTimer <> nil then FEditTimer.Free;
{$else}
  if FInpEdit <> nil then
    DoEndEdit(true);
  if FEditTimer <> nil then FEditTimer.Free;
{$endif}
{$endif}
  FVisible.Free;
{$IFNDEF LITE}
  FTmpBmp.Free;
{$ENDIF}
  {$ifdef ELTREE_USE_STYLES}
  if VirtStyle <> nil then
    VirtStyle.Free;
  {$endif}
  inherited;
end;

{$ifndef CLX_USED}
procedure TElTreeView.DestroyWnd;  { protected }
{$else}
procedure TElTreeView.DestroyWidget;  { protected }
{$endif}
begin
  DoEndEdit(true);
  inherited;
end;  { DestroyWnd }

{$IFNDEF LITE}

procedure TElTreeView.RedoTmpBmp;
var BgRect,
    BgRect1,
    BgRect2,
    BgRect4    : TRect;

begin
  if FOwner.BackgroundType in [bgtColorFill, bgtCenterBitmap] then
  begin
    FTmpBmp.FreeImage;
    {FTmpBmp.Height := 0;
    FTmpBmp.Width := 0;}
  end else
  if FOwner.BackgroundType in [bgtHorzGradient, bgtVertGradient] then
  begin
    if (ClientWidth <> 0) and (ClientHeight <> 0) then
    begin
      FTmpBmp.Height := ClientHeight + 1;
      FTmpBmp.Width := ClientWidth + 1;
      BgRect := ClientRect;
      {$ifdef CLX_USED}
      FTmpBmp.Canvas.Start;
      {$endif}
      with FOwner do
        GradientFill(FTmpBmp.Canvas.Handle, BgRect, GradientStartColor, GradientEndColor, GradientSteps, BackgroundType = bgtVertGradient);
      {$ifdef CLX_USED}
      FTmpBmp.Canvas.Stop;
      {$endif}
    end;
  end else
  begin
    if (ClientWidth <> 0) and (ClientHeight <> 0) then
    begin
      FTmpBmp.Height := ClientHeight + 1;
      FTmpBmp.Width := ClientWidth + 1;

      BgRect := ClientRect;
      BgRect4 := BgRect;
      BgRect1 := ClientRect;
      BgRect2 := BgRect;
      OffsetRect(BgRect2, BgRect2.Left, BgRect2.Top);
      {$ifdef CLX_USED}
      FTmpBmp.Canvas.Start;
      {$endif}
      with FOwner do
        ExtDrawBkgnd(FTmpBmp.Canvas.Handle, Handle, BgRect1, BgRect, BgRect, BgRect2, false, BkColor, BkColor, false, Background, backgroUndtype);
      {$ifdef CLX_USED}
      FTmpBmp.Canvas.Stop;
      {$endif}
    end;
  end;
end;

{$ENDIF}

procedure TElTreeView.DoHideLineHint;
begin
  FHintItem := nil;
  if FHintTimer <> nil then
  begin
    FHintTimer.Free;
    FHintTimer := nil;
  end;
  if FHintWnd <> nil then
  begin
    FHintWnd.ReleaseHandle;
    FHintWnd.Hide;
    //FHintWnd.Free;
    //FHintWnd := nil;
  end;
end;

procedure TElTreeView.TryStartHint(XPos, YPos : Integer);
var Item  : TElTreeItem;
    R3    : TRect;
    xoffs : integer;
    Section : TElHeaderSection;
    hc    : integer;
    ip    : TSTItemPart;
    IT    : TElFString;
    Show1 : boolean;
{$ifdef HAS_HTML_RENDER}
    HTMLData : TElHTMLData;
{$endif}
begin
  Item := GetItemAt(XPos, YPos, ip, hc);
  if ip <> ipOutside then
  begin
    if hc = -1 then
      Section := nil
    else
      Section := FOwner.FHeader.Sections[hc];
  end
  else
    exit;

  if Item = nil then exit;
  if FOwner.FHintType = shtHintOnly then
  begin
    IT := Item.Hint;
    Show1 := true;
  end
  else
  begin
    IT := GetHintText(Item, Section);
    if FOwner.FShowHintMode = shmAll then
      Show1 := true
    else
    if (FOwner.FShowHintMode = shmLong) and (Item.Hint = '') then
    begin
      SetRectEmpty(R3);
{$IFDEF HAS_HTML_RENDER}
      if Item.IsHTML and (Copy(IT, 1, 6) = '<html>') then
      begin
        if (not FOwner.ShowColumns) or (Section.Index = FOwner.MainTreeColumn) then
          HTMLData := Item.FHTMLData
        else
        begin
          hc := Section.Index;
          if hc > FOwner.MainTreeColumn then
            dec(hc);
          HTMLData := Item.FHTMLDataArray[hc];
        end;
        if HTMLData <> nil then
        begin
          R3.Right := HTMLData.TextSize.cx;
          R3.Bottom := HTMLData.TextSize.cy;
        end;
      end
      else
{$ENDIF}
        {$ifndef CLX_USED}
        {$ifdef ELPACK_UNICODE}
        ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(IT), Length(IT), R3, DT_NOPREFIX or DT_CALCRECT);
        {$else}
        DrawText(Canvas.Handle, PChar(IT), Length(IT), R3, DT_NOPREFIX or DT_CALCRECT);
        {$endif}
        {$else}
        Canvas.TextExtent(IT, R3, Integer(AlignmentFlags_SingleLine) or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter));
        {$endif}
      with FOwner do
        if FHeader.Visible and (FMainTreeCol <> - 1) and
           (FHeader.Sections.Count > FMainTreeCol)
    {$IFNDEF LITE}
           and FHeader.Sections[FMainTreeCol].Locked
    {$ENDIF}
        then
          xOffs := - FHPos
        else
          xOffs := 0;

      if not FOwner.ShowColumns then
      begin
        Show1 := ((R3.Right - R3.Left) > (Item.FTextRight - Item.FTextLeft)) or
                 (Item.FTextRight > ClientWidth + FOwner.FHPos + xOffs) or (Item.FTextLeft < FOwner.FHPos + xOffs);
      end
      else
      begin
        if Section.Index = FOwner.FMainTreeCol then
          Show1 := ((R3.Right - R3.Left) > (Item.FTextRight - Item.FTextLeft)) or
                   ((Item.FTextRight > ClientWidth + FOwner.FHPos + xOffs) or
                   (Item.FTextLeft < FOwner.FHPos + xOffs))
        else
          Show1 := ((R3.Right - R3.Left) > (Section.Width - (FOwner.ItemExt div 3) shl 1)) or
                   ((Section.Right - (FOwner.ItemExt div 3) > ClientWidth + FOwner.FHPos + xOffs) or
                   (Section.Left + (FOwner.ItemExt div 3) < FOwner.FHPos + xOffs));
      end;
    end
    else
      Show1 := true;
  end;

  if (FHintItem = nil) and Show1 then
    DoShowLineHint(Item, Section);
end;

function TElTreeView.GetHintText;
var si : integer;
    {$ifdef ELTREE_USE_STYLES}
    AStyle : TElCEllStyle;
    {$endif}
    b : boolean;

    function GetShortHint(AHint : TElFString) : TElFString;
    begin
      {$ifdef ELPACK_UNICODE}
      if WidePos('|', AHint) > 0 then
        result := WideCopy(AHint, 1, WidePos('|', AHint) - 1)
      else
        result := AHint;
      {$else}
      if Pos('|', AHint) > 0 then
        result := Copy(AHint, 1, Pos('|', AHint) - 1)
      else
        result := AHint;
      {$endif}
    end;

begin
  b := true;
  if FOwner.FHintType = shtHintOnly then
  begin
    result := GetShortHint(Item.Hint);
  end
  else
  begin
    if FOwner.FHintType = shtHintOrText then
    begin
      result := GetShortHint(Item.Hint);
      if Length(Result) <> 0 then
        exit;
    end;
    if (FOwner.LineHintType <> lhtMainTextOnly) and
      FOwner.ShowColumns then
    begin
      si := Section.Index;
      if FOwner.VirtualityLevel = vlNone then
      begin
        if si > FOwner.MainTreeColumn then
          dec(si);
      end;

      if FOwner.VirtualityLevel = vlNone then
      begin
        {$ifdef ELTREE_USE_STYLES}
        AStyle := nil;
        
        if Item.UseStyles then
        begin
          AStyle := Item.MainStyle;
          if (Section.Index <> FOwner.MainTreeColumn) and
             (Item.StylesCount > si) and (not AStyle.FOwnerProps) then
            AStyle := Item.Styles[si];
        end;
        {$endif}
      end
      else
      begin
        {$ifdef ELTREE_USE_STYLES}
        AStyle := nil;
        if Item.UseStyles then
        begin
          AStyle := VirtStyle;
          FOwner.TriggerVirtualStyleNeeded(Item, si, AStyle);
        end;
        {$endif}
      end;

      {$ifdef ELTREE_USE_STYLES}
      if (AStyle = nil) or (AStyle.FStyle = ElhsText) then
      {$endif}
      begin
        if Item.FStaticData <> nil then
        begin
          if Section.Index = FOwner.MainTreeColumn then
            b := true
          else
          if Item.ColumnText.Count > si then
          begin
            result := Item.ColumnText[si];
            if (Length(Result) <> 0) or
               (FOwner.LineHintType <> lhtSmart) then
              b := false
            else
            if (FOwner.LineHintType = lhtSmart) then
              Section := FOwner.FHeader.Sections[FOwner.MainTreeColumn];
          end
          else
          if FOwner.LineHintType = lhtCellTextOnly then
            b := false;
        end
        else
        begin
          if Section.Index = FOwner.MainTreeColumn then
            b := true
          else
          FOwner.TriggerVirtualTextNeeded(Item, Section.Index, result);
          if (Length(Result) <> 0) or
             (FOwner.LineHintType <> lhtSmart) then
            b := false
          else
          if (FOwner.LineHintType = lhtSmart) then
            Section := FOwner.FHeader.Sections[FOwner.MainTreeColumn]
          else
          if FOwner.LineHintType = lhtCellTextOnly then
            b := false;
        end;
      end

      {$ifndef ELTREE_USE_STYLES}
      ;
      {$else}
      else
      if FOwner.LineHintType = lhtCellTextOnly then
        b := false;
    {$endif}
    end;

    if b = true then // we are showing long text
      result := Item.Text;
  end;
end;

procedure TElTreeView.DoShowLineHint(Item: TElTreeItem; Section : TElHeaderSection);
var
  S, S1: TElFString;
  R, R1, R2: TRect;
  dx, dy: integer;
  b: boolean;
  P: TPoint;
  xOffs : integer;
begin
  if {$ifdef ELTREE_USE_INPLACE_EDITORS}FOwner.IsEditing or{$endif} FMouseSel then
    exit;
  R1 := GetItemRect(FVisible.IndexOf(Item));
  if IsRectEmpty(R1) then exit;
  if FHintWnd = nil then
     FHintWnd := TElHintWindow.Create(self);
  b := true;
  S := GetHintText(Item, Section);
  if S = '' then
    exit;

  FHintWnd.Font := Font;
  if not FOwner.UseSystemHintColors then
  begin
    FHintWnd.Color        := FOwner.FLineHintColor;
    FHintWnd.Font.Color   := FOwner.FTextColor;
  end
  else
  begin
    FHintWnd.Color := clInfoBk;
    FHintWnd.Font.Color := clInfoText;
  end;
  FHintWnd.Font.Charset := Font.Charset;

{$IFDEF HAS_HTML_RENDER}
  FHintWnd.IsHTML := Item.HintIsHTML;
  FHintWnd.OnImageNeeded := FOwner.TriggerImageNeededEvent;
{$ENDIF}

  with FOwner do
    if FHeader.Visible and (FMainTreeCol <> - 1) and
         (FHeader.Sections.Count > FMainTreeCol)
{$IFNDEF LITE}
         and FHeader.Sections[FMainTreeCol].Locked
{$ENDIF}
         then
      xOffs := - FHPos
    else
      xOffs := 0;

  if not FOwner.ShowColumns then
    R1.Left := Max(0, Item.FTextLeft - FOwner.FHPos - xOffs)
  else
  begin
    if Section.Index = FOwner.FMainTreeCol then
      R1.Left := Max(0, Item.FTextLeft + FOwner.FHPos - xOffs)
    else
      R1.Left := Max(0, Section.Left + FOwner.ItemExt div 5 - FOwner.FHPos - xOffs);
  end;
{$IFNDEF VER90}
  {$ifdef ELPACK_UNICODE}
  R := FHintWnd.CalcHintRectW(10000, S, nil);
  {$else}
  R := FHintWnd.CalcHintRect(10000, S, nil);
  {$endif}
{$ELSE}
  R.Left := 0;
  R.Right := FHintWnd.Canvas.TextWidth(S) + 6;
  R.Top := 0;
  R.Bottom := FHintWnd.Canvas.TextHeight(S) + 2;
{$ENDIF}

  dec(R.Bottom, 2);
  dx := R.Right - R.Left + 1;
  dy := R.Bottom - R.Top + 1;
  R.Left := R1.Left;
  R.Top := R1.Top;
  R.Right := R.Left + dx;
  R.Bottom := R.Top + dy;
  R.TopLeft := ClientToScreen(R.TopLeft);
  R.BottomRight := ClientToScreen(R.BottomRight);
  FHintWnd.BoundsRect := R;
  b := true;
  P := FHintCoord;
  if FOwner.ShowColumns then
    Inc(P.Y, FOwner.HeaderHeight);
  S1 := S;
  FOwner.DoShowHint(Item, Section, S, FHintWnd, P, b);
  if (b and (S <> S1)) then
  begin
    R := FHintWnd.BoundsRect;
{$IFNDEF VER90}
    {$ifdef ELPACK_UNICODE}
    R2 := FHintWnd.CalcHintRectW(10000, S, nil);
    {$else}
    R2 := FHintWnd.CalcHintRect(10000, S, nil);
    {$endif}
{$ELSE}
    R2.Left := 0;
    R2.Right := FHintWnd.Canvas.TextWidth(S) + 6;
    R2.Top := 0;
    R2.Bottom := FHintWnd.Canvas.TextHeight(S) + 2;
{$ENDIF}
    dec(R2.Bottom, 2);
    dx := R2.Right - R2.Left + 1;
    dy := R2.Bottom - R2.Top + 1;
    R.Right := R.Left + dx;
    R.Bottom := R.Top + dy;
  end;
  if b then
  begin
    FHintItem := Item;
    {$ifdef ELPACK_UNICODE}
    FHintWnd.ActivateHintW(R, S);
    {$else}
    FHintWnd.ActivateHint(R, S);
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TElTreeView.WndProc(var Message: TMessage);
var P1 : TPoint;
    Item : TElTreeItem;
    HCol : Integer;
    IP : TSTItemPart;
begin
  if (FHintItem <> nil) and (FOwner.FHideHintOnMove) then
  begin
    if ((Message.Msg >= WM_MOUSEMOVE) and (Message.Msg <= WM_MOUSELAST)) or (Message.Msg = WM_NCMOUSEMOVE) then
    begin
      GetCursorPos(P1);
      P1 := ScreenToClient(P1);
      Item := GetItemAt(P1.X, P1.Y, IP, HCol);
      if Item <> FHintItem then
         DoHideLineHint;
      inherited;
      exit;
    end
    else
    if
      ((Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST)) or
      ((Message.Msg = CM_ACTIVATE) or (Message.Msg = CM_DEACTIVATE)) or
      (Message.Msg = CM_APPKEYDOWN) or (Message.Msg = CM_APPSYSCOMMAND) or
      (Message.Msg = WM_COMMAND) or
      ((Message.Msg > WM_MOUSEMOVE) and (Message.Msg <= WM_MOUSELAST))
      or (Message.Msg = WM_NCMOUSEMOVE) then
      DoHideLineHint;
  end;
  if (FHintItem <> nil) and ((Message.Msg = CM_ACTIVATE) or (Message.Msg = CM_DEACTIVATE))
    or (Message.Msg = WM_NCMOUSEMOVE) then
    DoHideLineHint;
  inherited;
end;
{$endif}

{$ifdef CLX_USED}
function TElTreeView.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
var P1 : TPoint;
    TSI,
    Item : TElTreeItem;
    HCol : Integer;
    IP : TSTItemPart;
    Button: QT.ButtonState;
    MouseButton: TMouseButton;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$define EL_COMMON_EDITORS}
{$endif}
begin
  if (FHintItem <> nil) and (FOwner.FHideHintOnMove) then
  begin
    case QEvent_type(Event) of
      QEventType_MouseButtonPress,
      QEventType_MouseButtonRelease,
      QEventType_MouseMove,
      QEventType_MouseButtonDblClick:
        begin
          GetCursorPos(P1);
          P1 := ScreenToClient(P1);
          Item := GetItemAt(P1.X, P1.Y, IP, HCol);
          if Item <> FHintItem then
             DoHideLineHint;
          result := inherited EventFilter(Sender, Event);
          exit;
        end;
      QEventType_KeyPress,
      QEventType_KeyRelease,
      QEventType_FocusOut,
      QEventType_WindowActivate,
      QEventType_WindowDeactivate:
        DoHideLineHint;
    end;
  end;
  if (FHintItem <> nil) and (QEvent_type(Event) in [QEventType_WindowActivate, QEventType_WindowDeactivate]) then
    DoHideLineHint;
  result := false;
  case QEvent_type(Event) of
    QEventType_MouseButtonPress:
      begin
        Button := QMouseEvent_button(QMouseEventH(Event));
        if Integer(ButtonState_MidButton) and Integer(Button) <> 0 then
          MouseButton := mbMiddle
        else
        if Integer(ButtonState_RightButton) and Integer(Button) <> 0 then
          MouseButton := mbRight
        else
          MouseButton := mbLeft;

        if MouseButton = mbLeft then
        begin
          IntLButtonDown(QMouseEvent_x(QMouseEventH(Event)),
                         QMouseEvent_y(QMouseEventH(Event)),
                         ButtonStateToShiftState(QMouseEvent_state(QMouseEventH(Event))));
          result := inherited EventFilter(Sender, Event);
        end
        else
        if MouseButton = mbRight then
        begin
          // this is also in WMRButtonDown
          {$ifdef EL_COMMON_EDITORS}
          if FEditTimer <> nil then
             FEditTimer.Enabled := false;
          {$endif}
          DoHideLineHint;
          if FMouseSel then
            CancelMouseSel;

          IntRButtonDown(QMouseEvent_x(QMouseEventH(Event)),
                         QMouseEvent_y(QMouseEventH(Event)),
                         ButtonStateToShiftState(QMouseEvent_state(QMouseEventH(Event))));
          result := inherited EventFilter(Sender, Event);
        end
        else
          result := false;
      end;
    QEventType_MouseButtonRelease:
      begin
        Button := QMouseEvent_button(QMouseEventH(Event));
        if Integer(ButtonState_MidButton) and Integer(Button) <> 0 then
          MouseButton := mbMiddle
        else
        if Integer(ButtonState_RightButton) and Integer(Button) <> 0 then
          MouseButton := mbRight
        else
          MouseButton := mbLeft;

        if MouseButton = mbLeft then
        begin
          if IntLButtonUp(QMouseEvent_x(QMouseEventH(Event)),
                         QMouseEvent_y(QMouseEventH(Event)),
                         ButtonStateToShiftState(QMouseEvent_state(QMouseEventH(Event)))) then
            result := inherited EventFilter(Sender, Event)
          else
            result := true;
        end
        else
        if MouseButton = mbRight then
        begin
          if IntRButtonUp(QMouseEvent_x(QMouseEventH(Event)),
                          QMouseEvent_y(QMouseEventH(Event)),
                          ButtonStateToShiftState(QMouseEvent_state(QMouseEventH(Event)))) then
            result := inherited EventFilter(Sender, Event)
          else
            result := true;
        end
        else
          result := false;
      end;
    QEventType_MouseMove:
      begin
        result := inherited EventFilter(Sender, Event);
        IntMouseMove(QMouseEvent_x(QMouseEventH(Event)),
                          QMouseEvent_y(QMouseEventH(Event)),
                          ButtonStateToShiftState(QMouseEvent_state(QMouseEventH(Event))));
      end;
    QEventType_MouseButtonDblClick:
      begin
        Button := QMouseEvent_button(QMouseEventH(Event));
        if Integer(ButtonState_MidButton) and Integer(Button) <> 0 then
          MouseButton := mbMiddle
        else
        if Integer(ButtonState_RightButton) and Integer(Button) <> 0 then
          MouseButton := mbRight
        else
          MouseButton := mbLeft;

        if MouseButton = mbLeft then
        begin
          if IntLButtonDblClick(QMouseEvent_x(QMouseEventH(Event)),
                                QMouseEvent_y(QMouseEventH(Event)),
                                ButtonStateToShiftState(QMouseEvent_state(QMouseEventH(Event)))) then
            result := inherited EventFilter(Sender, Event)
          else
            result := true;
        end
        else
        if MouseButton = mbRight then
        begin
          if IntRButtonDblClick(QMouseEvent_x(QMouseEventH(Event)),
                                QMouseEvent_y(QMouseEventH(Event)),
                                ButtonStateToShiftState(QMouseEvent_state(QMouseEventH(Event)))) then
            result := inherited EventFilter(Sender, Event)
          else
            result := true;
        end;
      end;
    QEventType_DragEnter:
      begin
        result := inherited EventFilter(Sender, Event);
        FInDragging := true;
      end;
    {$ifdef MWSINDOWS}
    QEventType_DragMove:
      begin
        result := inherited EventFilter(Sender, Event);
        QApplication_processEvents(Application.Handle);
      end;
    {$endif}
    (*
    QEventType_DragMove:
      begin
        result := inherited EventFilter(Sender, Event);
        P1 := Point(QMouseEvent_x(QMouseEventH(Event)),
                    QMouseEvent_y(QMouseEventH(Event)));

        if not (QControls.DragObject is TBaseDragControlObject) then
          SendDragObject := QControls.DragObject
        else
          SendDragObject := DragControl;
        b := false;
        if FOwner.DragAllowed then
            with ScreenToClient(P1) do
              DoDragOver(Source, X, Y, b);
        QDropEvent_accept(Event, b);
      end;
    *)
    QEventType_DragLeave:
      begin
        result := inherited EventFilter(Sender, Event);
        FInDragging := false;
        FOwner.FDragObject := nil;
        if FDragExpandTimer <> nil then
        begin
          FDragExpandTimer.Enabled := false;
        end;

        if FOwner.DragAllowed then
        begin
          if FDragScrollTimer <> nil then
          begin
            FDragScrollTimer.Free;
            FDragScrollTimer := nil;
          end;
          TSI := FDropTrg;
          FDropTrg := nil;
          if TSI <> nil then
          begin
            TSI.RedrawItem(false);
            Update;
          end;
        end;
      end;
    // QEventType_DragResponse,
    QEventType_Drop:
      begin
        result := inherited EventFilter(Sender, Event);
        FInDragging := false;
        FOwner.FDragObject := nil;
        if FDragExpandTimer <> nil then
        begin
          FDragExpandTimer.Enabled := false;
          FDragExpandTimer.Free;
          FDragExpandTimer := nil;
        end;

        if FOwner.DragAllowed then
        begin
          if FDragScrollTimer <> nil then
          begin
            FDragScrollTimer.Free;
            FDragScrollTimer := nil;
          end;
          TSI := FDropTrg;
          FDropTrg := nil;
          if TSI <> nil then
          begin
            TSI.RedrawItem(false);
            Update;
          end;
        end;
      end;
    else
      result := inherited EventFilter(Sender, Event);
  end;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.CMMouseWheel(var Msg : TMessage);  { private }
{$else}
function TElTreeView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      const MousePos: TPoint): Boolean;
{$endif}
var
  Dy : integer;
  sl : integer;
  TSI: TElTreeItem;
  R  : TRect;
  Pos: TPoint;
  {$ifndef CLX_USED}
  Message: TWMMouseMove;
  {$endif}
begin
  {$ifndef CLX_USED}
  if IsWinNTUp or IsWin98Up then
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @sl, SPIF_SENDCHANGE)
  else
    sl := 3;
  {$else}
    sl := 3;
  {$endif}
  if sl = 0 then sl := 1;
  {$ifndef CLX_USED}
  Dy := TWMMouseWheel(Msg).WheelDelta div (MOUSE_WHEEL_DELTA div sl);
  {$else}
  Dy := WheelDelta div (MOUSE_WHEEL_DELTA div sl);
  {$endif}
  if Dy <> 0 then
  begin
    if FMouseSel then
    begin
      DrawMouseSelectFrame;
      SetVPosition(FOwner.FTopIndex - Dy);
      Update;
      {$ifndef CLX_USED}
      Pos := ScreenToClient(SmallPointToPoint(TWMMouseWheel(Msg).Pos));
      {$else}
      Pos := ScreenToClient(MousePos);
      {$endif}
      if PtInRect(ClientRect, Pos) then
      begin
        TSI := GetItemAtY(Pos.y);
        if TSI = nil then
          TSI := TElTreeItem(FVisible.Last);
        begin
          R := GetItemRect(FVisible.IndexOf(TSI));
          FMFSEndItem := TSI;
          FMFSEndCoord.x := Pos.x + FOwner.FHPos;
          FMFSEndCoord.y := Pos.y - R.Top;
        end;
        SelectMouseSelectItems;
        FOwner.DoAfterSelectionChange;
      end;
      DrawMouseSelectFrame;
    end
    else
      SetVPosition(FOwner.FTopIndex - Dy);

    {$ifndef CLX_USED}
    Message.Msg := WM_MOUSEMOVE;
    Message.Keys := 0;
    Message.Pos := TWMMousewheel(Msg).Pos;
    Message.Result := 0;
    WMMouseMove(Message);
    {$else}
    IntMouseMove(MousePos.X, MousePos.Y, Shift);
    {$endif}
  end;
  {$ifndef CLX_USED}
  inherited;
  {$else}
  result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  {$endif}
end;  { CMMouseWheel }

{$ifndef CLX_USED}
procedure TElTreeView.WMMouseWheel(var Msg: TMessage); { private }
begin
  inherited;
  CMMouseWheel(Msg);
end; { WMMouseWheel }
{$endif}

procedure TElTreeView.OnHintTimer;
begin
  if FHintItem <> nil then
  begin
    DoHideLineHint;
    exit;
  end;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
  if FInpEdit <> nil then exit;
{$endif}
  FHintTimer.Enabled := false;
  if not FOwner.HandleAllocated then exit;

  if (not (csDesigning in ComponentState)) then
  begin
    TryStartHint(FHintCoord.X, FHintCoord.Y);

    if FOwner.FHintHide then
    begin
      if FOwner.FLineHintTimeout <= 0 then
      begin
        FHintTimer.Interval := Application.HintHidePause;
        if FHintTimer.Interval = 0 then
          FHintTimer.Interval := 1000;
      end
      else
        FHintTimer.Interval := FOwner.FLineHintTimeout;
      FHintTimer.Enabled := true;
    end;
  end;
end;

procedure TElTreeView.SetHPosition(value: integer);
{$ifndef CLX_USED}
var
  Code: Word;
{$IFNDEF VER90}
  Form: TCustomForm;
{$ELSE}
  Form: TForm;
{$ENDIF}
{$endif}
begin
  FOwner.IsUpdating := true;
  if csReading in ComponentState then
    FOwner.FHPos := Value
  else
  begin
    if Value > FHRange - Width then
      Value := FHRange - Width;
    //if Value > FHRange then Value := FHRange;
    if Value < 0 then Value := 0;
    if Value <> FOwner.FHPos then
    begin
      FOwner.FHPos := Value;
      with FOwner do
      begin
        FHeader.LeftPos := FHPos;
        {//LockedColumn, update
         FHeader.Left := -FHPos;
        FHeader.Width := FHPos + FView.Width;}
      end;
      FClearAll := true;
      if csDesigning in ComponentState then
      begin
        {$ifndef CLX_USED}
        Form := GetParentForm(self);
        if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
        {$endif}
      end;
      FOwner.FUpdated := true;
      FRangeUpdate := true;
    end;
    {$ifndef CLX_USED}
    Code := SB_CTL;
    with FOwner do
      if Windows.GetScrollPos(FHScrollBar.Handle, Code) <> FHPos then
        SetScrollPos(FHScrollBar.Handle, Code, FHPos, true);
    {$else}
    with FOwner do
      if FHPos <> FHScrollBar.Position then
        FHScrollBar.Position := FHPos;
    {$endif}
  end;
  FOwner.IsUpdating := false;
end;

procedure TElTreeView.SetVPosition(value: integer);
var
  MaxV: integer;
{$ifndef CLX_USED}
  Code: Word;
{$endif}
{$IFNDEF VER90}
  Form: TCustomForm;
{$ELSE}
  Form: TForm;
{$ENDIF}
begin
  FOwner.IsUpdating := true;
  FTrackItem := nil;
  if csReading in ComponentState then
    FOwner.FTopIndex := Value
  else
  begin
    with FOwner do
    begin
      if TotalVarHeightCount > 0 then
      begin
        if TotalVisCount = 0 then
          GetTotalVisCount;
        maxV := CalcPageUpPos(TotalVisCount - 1);//TotalVisCount - CalcPageUpPos(TotalVisCount - 1);
      end
      else
      begin
        if TotalVisCount = 0 then
          GetTotalVisCount;
        maxV := TotalVisCount - GetVisCount + 1;
      end;
      if Value > maxV then Value := maxV;
    end;
    if Value < 0 then Value := 0;
    if Value <> FOwner.FTopIndex then
    begin
      FOwner.FUpdated := true;
      FClearVis := true;
      FClearAll := true;
      DoSetTopIndex(Value);
    end;
    if csDesigning in ComponentState then
    begin
      Form := GetParentForm(self);
      {$ifndef CLX_USED}
      if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
      {$else}
      if (Form <> nil) and (Form.DesignerHook <> nil) then Form.DesignerHook.Modified;
      {$endif}
    end;
    {$ifndef CLX_USED}
    Code := SB_CTL;
    with FOwner do
      SetScrollPos(FVScrollBar.Handle, Code, FTopIndex, True);
    {$else}
    with FOwner do
      FVScrollBar.Position := FTopIndex;
    {$endif}
  end;
  FOwner.IsUpdating := false;
end;

procedure TElTreeView.DoSetTopIndex;
var MaxV : integer;
  //  b : boolean;
begin
  (*
  b := false;
  if ((value >= Integer(FItems.Count)) and (value > 0)) then
    b := true
  else
  if (value < 0) then
    // Raise EElTreeError.Create(GetErrorMsg(STExOutOfBounds));
    value := 0;
  *)
  with FOwner do
  begin
    if TotalVisCount = 0 then
      GetTotalVisCount;
    if TotalVarHeightCount > 0 then
      maxV := CalcPageUpPos(TotalVisCount - 1)//TotalVisCount - //1// CalcPageUpPos(TotalVisCount - 1) + 1
    else
      maxV := TotalVisCount - View.GetVisCount + 1;
    if (Value < 0) or (MaxV < 0) then
      Value := 0
    else
    if Value > MaxV then
      Value := MaxV;
    FTopIndex := value;
    if (FTopIndex = 0) and (FItems.Count = 0) then FBottomIndex := 0
    else
    begin
      FillVisFwd(FTopIndex);
      FBottomIndex := FTopIndex + FVisible.Count - 1;
    end;
  end;
end;

procedure TElTreeView.FillVisFwd;
var i, j, k : integer;
    Item : TElTreeItem;
    ah, th, CurHeight : integer;
begin
  FVisible.Clear;
  th := Height;
  i := 0;
  j := FOwner.FAllList.Count;
  k := 0;
  CurHeight := 0;
  while k < j do
  begin
    Item := TElTreeItem(FOwner.FAllList[k]);
    if (FOwner.FilteredVisibility and Item.Hidden) then
      k := FOwner.FAllList.IndexOfFrom(k, Item.GetLastSubItem)
    else
    begin
      if i >= StartIndex then
      begin
        if (CurHeight > th) then break;
        if FOwner.TotalVarHeightCount > 0 then
           ah := Item.GetHeight
        else
           ah := FOwner.LineHeight;
        FVisible.Add(Item);
        inc(CurHeight, ah);
        if (CurHeight > th) then break;
      end;
      if (Item.FChildren <> nil) and (Item.FChildren.Count > 0) then
         if not Item.Expanded then
           k := FOwner.FAllList.IndexOfFrom(k, Item.GetLastSubItem);
      inc(i);
    end;
    inc(k);
  end;
end;

{$ifndef CLX_USED}
procedure TElTreeView.WMSize(var Msg : TWMSize);  { private }
{$else}
procedure TElTreeView.Resize;
{$endif}
begin
  inherited;
  begin
    FOwner.IsUpdating := true;
    FOwner.FUpdated := true;
    FClearVis := true;
    FClearAll := true;
    FVisUpdated := true;
    DoSetTopIndex(FOwner.FTopIndex);
    FOwner.IsUpdating := false;
  end;
{$IFNDEF LITE}
  if not (FOwner.BackgroundType in [bgtColorFill, bgtCenterBitmap]) then RedoTmpBmp;
{$ENDIF}
  //Invalidate;
end;  { WMSize }

const ScrollCodesToInt : array [TElScrollCode] of Integer = (SB_LINEUP, SB_LINEDOWN, SB_PAGEUP, SB_PAGEDOWN, SB_THUMBPOSITION, SB_THUMBTRACK, SB_TOP, SB_BOTTOM, SB_ENDSCROLL, SB_ENDSCROLL + 1, SB_ENDSCROLL + 2);

procedure TElTreeView.OnHScroll(Sender: TObject; ScrollCode: TElScrollCode; var ScrollPos: Integer; var DoChange : boolean);
var AScrollCode : TElScrollCode;
begin
  AScrollCode := ScrollCode;
  if AScrollCode = escSndLineDown then
  begin
    if FOwner.FVScrollBar.SecondBtnKind = sbkOpposite then AscrollCode := escLineUp else
    if FOwner.FVscrollBar.SecondBtnKind = sbkPage then AScrollCode := escPageDown;
  end else
  if AScrollCode = escSndLineUp then
  begin
    if FOwner.FVscrollBar.SecondBtnKind = sbkOpposite then AscrollCode := escLineDown else
    if FOwner.FVscrollBar.SecondBtnKind = sbkPage then AScrollCode := escPageUp;
  end;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
  if FInpEdit <> nil then DoEndEdit(false);
{$else}
  if FInpEdit <> nil then DoEndEdit(false);
{$endif}
{$endif}
  with FOwner do
  case AScrollCode of
    escLineUp : SetHPosition(FHPos - 4);
    escLineDown: SetHPosition(FHPos + 4);
    escPageUp: SetHPosition(FHPos - Self.Width shr 1);
    escPageDown: SetHPosition(FHPos + Self.Width shr 1);
    escPosition: SetHPosition(ScrollPos);
    escTrack: if FScrollTracking then SetHPosition(ScrollPos);
    escTop: SetHPosition(0);
    escBottom: SetHPosition(FHRange);
  end; // case
  FOwner.TriggerScrollEvent(sbHorizontal, ScrollCodesToInt[ScrollCode]);
  {with FOwner do
    if Flat or FUseCustomBars then DrawFlatBorder(false, false);
  }
  DoChange := false;
end;

procedure TElTreeView.OnVScroll(Sender: TObject; ScrollCode: TElScrollCode; var ScrollPos: Integer; var DoChange : boolean);
var AScrollCode : TElScrollCode;
    aNewPos     : integer;
    {$ifndef CLX_USED}
    Message     : TWMMouseMove;
    {$endif}
    P           : TPoint;
begin
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
  if (FInpEdit <> nil) then
  begin
    DoChange := false;
    FScrollFirstClick := not FScrollFirstClick;
    if FScrollFirstClick then
    begin
      FOwner.FVScrollbar.EndScroll;
      exit
    end
    else
    if ScrollCode <> escEndScroll then
      if not DoEndEdit(false) then
        exit;
  end
  else
{$endif}
{$else}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
  if (FInpEdit <> nil) then
  begin
    DoChange := false;
    FScrollFirstClick := not FScrollFirstClick;
    if FScrollFirstClick then
    begin
      FOwner.FVScrollbar.EndScroll;
      exit
    end
    else
    if ScrollCode <> escEndScroll then
      DoEndEdit(false);
  end
  else
{$endif}
{$endif}
    FScrollFirstClick := false;

  AScrollCode := ScrollCode;
  if AScrollCode = escSndLineDown then
  begin
    if FOwner.FVscrollBar.SecondBtnKind = sbkOpposite then
      AscrollCode := escLineUp
    else
    if FOwner.FVscrollBar.SecondBtnKind = sbkPage then
      AScrollCode := escPageDown;
  end else
  if AScrollCode = escSndLineUp then
  begin
    if FOwner.FVscrollBar.SecondBtnKind = sbkOpposite then
      AscrollCode := escLineDown
    else
    if FOwner.FVscrollBar.SecondBtnKind = sbkPage then
      AScrollCode := escPageUp;
  end;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
  if FInpEdit <> nil then
    DoEndEdit(false);
{$else}
  if FInpEdit <> nil then
    DoEndEdit(false);
{$endif}
{$endif}
  with FOwner do
  case AScrollCode of
    escLineUp:   SetVPosition(FTopIndex - 1);
    escLineDown: SetVPosition(FTopIndex + 1);
    escPageUp  :
      begin
        {aNewPos := CalcPageUpPos(FTopIndex);
        if (aNewPos = FTopIndex) and (FVisible.Count = 1) and (TotalVisCount <> 1) then dec(aNewPos);}
        if FOwner.TotalVarHeightCount > 0 then
           aNewPos := CalcPageUpPos(FTopIndex)
        else
           aNewPos := FTopIndex - (GetVisCount - 1);
        if aNewPos < 0 then aNewPos := 0;
        SetVPosition(aNewPos);
      end;
    escPageDown:
      begin
        if FOwner.TotalVarHeightCount > 0 then
           aNewPos := CalcPageDownPos(FTopIndex)
        else
           aNewPos := FTopIndex + (GetVisCount - 1);
        if (aNewPos = FTopIndex) and (FVisible.Count = 1) and (TotalVisCount <> 1) then inc(aNewPos);
        SetVPosition(aNewPos);
      end;
    escPosition: SetVPosition(ScrollPos);
    escTop:      SetVPosition(0);
    escBottom:   SetVPosition(TotalVisCount);
    escTrack:    if FScrollTracking then SetVPosition(ScrollPos);
  end; // case
  FOwner.TriggerScrollEvent(sbVertical, ScrollCodesToInt[ScrollCode]);
  {with FOwner do
    if Flat or FUseCustomBars then DrawFlatBorder(false, false);
  }
  if FMouseSel then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    {$ifndef CLX_USED}
    Message.Msg := WM_MOUSEMOVE;
    Message.Keys := 0;
    Message.XPos := P.X;
    Message.YPos := P.Y;
    Message.Result := 0;
    WMMouseMove(Message);
    {$else}
    IntMouseMove(P.X, P.Y, []);
    {$endif}
  end;
  DoChange := false;
end;

function TElTreeView.CalcPageUpPos;
var i : integer;
IntPrevVis : TIterateProcAnonymusMethod;

type TSRec = record
       MaxHeight : integer;
       CurHeight : integer;
       LastItem  : TElTreeItem;
       // CurIdx    : integer;
     end;
     PSRec = ^TSRec;

var SRec : TSRec;

begin
  IntPrevVis :=  procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
               IterateData: pointer; Tree: TCustomElTree)
     begin
       with PSRec(IterateData)^ do
       begin
         Inc(CurHeight, Item.GetHeight);
         if CurHeight > MaxHeight then
           ContinueIterate := false
         else
           LastItem := Item;
       end;
     end;
  i := CurIdx;
  if (i = 0) or (FVisible.Count = 0) then
  begin
    result := 0;
    exit;
  end;
  SRec.MaxHeight := ClientHeight;
  SRec.CurHeight := 0;
  SRec.LastItem  := FOwner.FItems.GetVisItem(i);
  FOwner.FItems.IterateBackFrom(true, false, IntPrevVis, @SRec, SRec.LastItem); (*<+>*)
  result := SRec.LastItem.VisIndex;
end;

function TElTreeView.CalcPageDownPos;
var i : integer;
IntPrevVis: TIterateProcAnonymusMethod;

type TSRec = record
       MaxHeight : integer;
       CurHeight : integer;
       LastItem  : TElTreeItem;
     end;
     PSRec = ^TSRec;

var SRec : TSRec;

begin
  IntPrevVis :=  procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
               IterateData: pointer; Tree: TCustomElTree)
     begin
       with PSRec(IterateData)^ do
       begin
         Inc(CurHeight, Item.GetHeight);
         if CurHeight > MaxHeight then
           ContinueIterate := false
         else
           LastItem := Item;
       end;
     end;
  i := CurIdx;
  if (FVisible.Count = 0) then
  begin
    result := 0;
    exit;
  end;
  SRec.MaxHeight := ClientHeight;
  SRec.CurHeight := 0;
  SRec.LastItem  := FOwner.FItems.GetVisItem(i);
  FOwner.FItems.IterateFrom(true, false, IntPrevVis, @SRec, SRec.LastItem); (*<+>*)
  result := SRec.LastItem.VisIndex;
end;

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
procedure TElTreeView.ProcessFloatEditResults(ByCancel : boolean);
begin
  ProcessTextEditResults(ByCancel);
end;

procedure TElTreeView.ProcessBlobEditResults(ByCancel : boolean);
begin
  // Intentionally left blank
end;

procedure TElTreeView.ProcessPictureEditResults(ByCancel : boolean);
begin
  // Intentionally left blank
end;

procedure TElTreeView.ProcessCustomEditResults(ByCancel : boolean);
begin
  // Intentionally left blank
end;

procedure TElTreeView.ProcessDateTimeEditResults(ByCancel : boolean);
{$ifdef ELTREE_USE_EXT_EDITORS}
{$ifdef ELPACK_COMPLETE}
var b : boolean;
    s : string;
    sn: integer;
    ASection : TElHeaderSection;
{$endif}
{$endif}
begin
{$ifdef ELTREE_USE_EXT_EDITORS}
{$ifdef ELPACK_COMPLETE}
  b := not ByCancel;
  TElDateTimePicker(FInpEdit).OnExit := nil;
  if b then
    S := DateTimeToStr(TElDateTimePicker(FInpEdit).DateTime)
  else
    s := '';
  if (Fowner.FShowHeader) and (FEditSect <> -1) then
    ASection := FHeader.Sections[FEditSect]
  else
    ASection := nil;
  FOwner.DoValidate(FEditingItem, ASection, s, b);
  if (FEditingItem <> nil) and b then
  begin
    if (FEditSect = FOwner.FMainTreeCol) or (FEditSect = -1) then
    begin
      FEditingItem.Text := s;
    end else
    begin
      sn := FEditSect;
      if sn > FOwner.FMainTreeCol then
        dec(sn);
      if FOwner.VirtualityLevel = vlNone then
      begin
        while FEditingItem.FStaticData.FColText.Count <= sn do
          FEditingItem.FStaticData.FColText.Add('');
        FEditingItem.FStaticData.FColText[sn] := s;
      end;
    end;
  end;
{$else}
  ProcessTextEditResults(ByCancel);
{$endif}
{$else}
  ProcessTextEditResults(ByCancel);
{$endif}
end;

procedure TElTreeView.ProcessNumericEditResults(ByCancel : boolean);
begin
  ProcessTextEditResults(ByCancel);
end;

procedure TElTreeView.ProcessTextEditResults(ByCancel : boolean);
var b : boolean;
    s : string;
    sn: integer;
    ASection : TElHeaderSection;
begin
    b := not ByCancel;
    if b then
{$IFDEF LITE}
      s := (FInpEdit as EditBoxClass).Text
    else
      s := '';
    (FInpEdit as EditBoxClass).OnExit := nil;
{$ELSE}
    {$IFNDEF ELPACK_COMPLETE}
    if FInpEdit is TMemo then
    begin
       s := TMemo(FInpEdit).Text;
       TMemo(FInpEdit).OnExit := nil;
    end else
    begin
      s := TEdit(FInpEdit).Text;
      TEdit(FInpEdit).OnExit := nil;
    end
    else
    if FInpEdit is TMemo then
    begin
       S := '';
       TMemo(FInpEdit).OnExit := nil;
    end else
    begin
      S := '';
      TEdit(FInpEdit).OnExit := nil;
    end;
  {$ELSE}
    begin
      s := (FInpEdit as EditBoxClass).Text;
      (FInpEdit as EditBoxClass).OnExit := nil;
    end
    else
    begin
      s := '';
      (FInpEdit as EditBoxClass).OnExit := nil;
    end;
  {$ENDIF}
{$ENDIF}
    if (FOwner.FShowHeader) and (FEditSect <> -1) then
       ASection := FHeader.Sections[FEditSect]
    else
       ASection := nil;
    FOwner.DoValidate(FEditingItem, ASection, s, b);
    if (FEditingItem <> nil) and (b) then
    begin
      if (FEditSect = FOwner.FMainTreeCol) or (FEditSect = -1) then
        FEditingItem.Text := s
      else
      begin
        sn := FEditSect;
        if sn > FOwner.FMainTreeCol then dec(sn);
        if FOwner.VirtualityLevel = vlNone then
        begin
          while FEditingItem.FStaticData.FColText.Count <= sn do
            FEditingItem.FStaticData.FColText.Add('');
          FEditingItem.FStaticData.FColText[sn] := s;
        end;
      end;
    end;
end;

procedure TElTreeView.ProcessBoolEditResults(ByCancel : boolean);
var b : boolean;
    s : string;
    sn: integer;
    ASection : TElHeaderSection;
    
begin
  b := not ByCancel;
  if b then
  begin
    if CheckBoxClass(FInpEdit).Checked then
       S := '1'
    else
       S := '';
  end
  else
    s := '';
  if (FOwner.FShowHeader) and (FEditSect <> -1)
     then ASection := FOwner.FHeader.Sections[FEditSect]
     else ASection := nil;
  FOwner.DoValidate(FEditingItem, ASection, s, b);
  if (FEditingItem <> nil) and b then
  begin
    if (FEditSect = FOwner.FMainTreeCol) or (FEditSect = -1) then
    begin
      FEditingItem.Text := s;
    end else
    begin
      sn := FEditSect;
      if sn > FOwner.FMainTreeCol then
        dec(sn);
      if FOwner.VirtualityLevel = vlNone then
      begin
        while FEditingItem.FStaticData.FColText.Count <= sn do
          FEditingItem.FStaticData.FColText.Add('');
        FEditingItem.FStaticData.FColText[sn] := s;
      end;
    end;
  end;
  CheckBoxClass(FInpEdit).OnExit := nil;
end;

procedure TElTreeView.ProcessCurrencyEditResults(ByCancel : boolean);
{$ifdef ELTREE_USE_EXT_EDITORS}
{$ifdef ELPACK_COMPLETE}
var b : boolean;
    s : string;
    sn: integer;
    ASection : TElHeaderSection;
{$endif}
{$endif}
begin
{$ifdef ELTREE_USE_EXT_EDITORS}
  b := not ByCancel;
  if b then
    S := CurrToPrettyStr(TElCurrencyEdit(FInpEdit).Value)
  else
    s := '';
  if (Fowner.FShowHeader) and (FEditSect <> -1) then
    ASection := FHeader.Sections[FEditSect]
  else
    ASection := nil;
  FOwner.DoValidate(FEditingItem, ASection, s, b);
  if (FEditingItem <> nil) and b then
  begin
    if (FEditSect = FOwner.FMainTreeCol) or (FEditSect = -1) then
    begin
      FEditingItem.Text := s;
    end else
    begin
      sn := FEditSect;
      if sn > FOwner.FMainTreeCol then
        dec(sn);
      if FOwner.VirtualityLevel = vlNone then
      begin
        while FEditingItem.FStaticData.FColText.Count <= sn do
          FEditingItem.FStaticData.FColText.Add('');
        FEditingItem.FStaticData.FColText[sn] := s;
      end;
    end;
  end;
  TElCurrencyEdit(FInpEdit).OnExit := nil;
{$else}
  ProcessTextEditResults(ByCancel);
{$endif}
end;

procedure TElTreeView.ProcessDateEditResults(ByCancel : boolean);
var b : boolean;
    s : string;
    sn: integer;
    ASection : TElHeaderSection;
begin
  b := not ByCancel;
{$ifdef ELTREE_USE_EXT_EDITORS}
  TElDateTimePicker(FInpEdit).OnExit := nil;
  if b then
    S := DateToStr(TElDateTimePicker(FInpEdit).Date)
  else
{$else}
  TDateTimePicker(FInpEdit).OnExit := nil;
  if b then
    S := DateToStr(TDateTimePicker(FInpEdit).Date)
  else
{$endif}
    s := '';
  if (Fowner.FShowHeader) and (FEditSect <> -1)
     then ASection := FHeader.Sections[FEditSect]
     else ASection := nil;
  FOwner.DoValidate(FEditingItem, ASection, s, b);
  if (FEditingItem <> nil) and b then
  begin
    if (FEditSect = FOwner.FMainTreeCol) or (FEditSect = -1) then
    begin
      FEditingItem.Text := s;
    end else
    begin
      sn := FEditSect;
      if sn > FOwner.FMainTreeCol then
        dec(sn);
      if FOwner.VirtualityLevel = vlNone then
      begin
        while FEditingItem.FStaticData.FColText.Count <= sn do
          FEditingItem.FStaticData.FColText.Add('');
        FEditingItem.FStaticData.FColText[sn] := s;
      end;
    end;
  end;
end;

procedure TElTreeView.ProcessTimeEditResults(ByCancel : boolean);
var b : boolean;
    s : string;
    sn: integer;
    ASection : TElHeaderSection;

begin
  b := not ByCancel;
{$ifdef ELTREE_USE_EXT_EDITORS}
  TElDateTimePicker(FInpEdit).OnExit := nil;
  if b then
    S := TimeToStr(TElDateTimePicker(FInpEdit).Time)
  else
{$else}
  TDateTimePicker(FInpEdit).OnExit := nil;
  if b then
    S := TimeToStr(TDateTimePicker(FInpEdit).Time)
  else
{$endif}
    s := '';

  if (FOwner.FShowHeader) and (FEditSect <> -1)
     then ASection := FHeader.Sections[FEditSect]
     else ASection := nil;

  FOwner.DoValidate(FEditingItem, ASection, s, b);
  if (FEditingItem <> nil) and b then
  begin
    if (FEditSect = FOwner.FMainTreeCol) or (FEditSect = -1) then
    begin
      FEditingItem.Text := s;
    end else
    begin
      sn := FEditSect;
      if sn > FOwner.FMainTreeCol then
        dec(sn);
      if FOwner.VirtualityLevel = vlNone then
      begin
        while FEditingItem.FStaticData.FColText.Count <= sn do
          FEditingItem.FStaticData.FColText.Add('');
        FEditingItem.FStaticData.FColText[sn] := s;
      end;
    end;
  end;
end;

procedure TElTreeView.ProcessEnumEditResults(ByCancel : boolean);
var b: boolean;
    ASection : TElHeaderSection;
begin
  b := not ByCancel;
  if (FEditSect = FOwner.FMainTreeCol) or (FEditSect = -1) then
    ASection := nil
  else
    ASection := FHeader.Sections[FEditSect];
  FOwner.DoValidateCombo(FEditingItem, ASection, TComboBox(FInpEdit), b);
  ComboBoxClass(FInpEdit).OnExit := nil;
end;

procedure TElTreeView.ProcessEditResults(ByCancel : boolean);
begin
  case FEditType of
    sftPicture : ProcessPictureEditResults(ByCancel);
    sftCustom  : ProcessCustomEditResults(ByCancel);
    sftBLOB    : ProcessBlobEditResults(ByCancel);
    sftEnum    : ProcessEnumEditResults(ByCancel);
    sftDate    : ProcessDateEditResults(ByCancel);
    sftTime    : ProcessTimeEditResults(ByCancel);
    sftDateTime: ProcessDateTimeEditResults(ByCancel);
    sftText    : ProcessTextEditResults(ByCancel);
    sftFloating: ProcessFloatEditResults(ByCancel);
    sftNumber  : ProcessNumericEditResults(ByCancel);
    sftBool    : ProcessBoolEditResults(ByCancel);
{$ifdef ELPACK_COMPLETE}
    sftCurrency: ProcessCurrencyEditResults(ByCancel);
{$else}
    sftCurrency: ProcessTextEditResults(ByCancel);
{$endif}
  end;

end;

function TElTreeView.DoEndEdit;
begin
  result := true;
  if FEditing then
    FOwner.HideSelection := FOldHide;
  if (FInpEdit = nil) or (FEditingItem = nil) or (not FInpEdit.Visible) then
  begin
    if assigned(FInpEdit) then
    begin
      FInpEdit.Parent := nil;
      FInpEdit.Free;
      if Assigned(FFakePopup) then FFakePopup.Free;
      FFakePopup := nil;
      FInpEdit := nil;
    end;
    FEditingItem := nil;
    try
      if (not (csDestroying in ComponentState)) and CanFocus then
        SetFocus;
    except
    end;
    FEditing := false;
    exit;
  end;
  //if not ByCancel then
  ProcessEditResults(ByCancel);

  if FInpEdit <> nil then
    FInpEdit.Visible := false;
  if (FEditingItem <> nil) and (not (csDestroying in ComponentState)) then
  begin
    FEditingItem.RedrawItem(false);
    FOwner.FUpdated := true;
    if not FOwner.IsUpdating then Update;
  end;
  FEditingItem := nil;
  try         
    if (not (csDestroying in ComponentState)) and CanFocus then
      SetFocus;
  except
  end;
  FEditing := false;
end;

procedure TElTreeView.OnEditExit;
begin
  if FEditing then
     DoEndEdit(False);
end;
{$endif}
{$endif}
procedure TElTreeView.DefineHRange;

  procedure CalcWidth(Item: TElTreeItem; {Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; }Tree: TCustomElTree);
  var
    w: integer;

  begin
    w := Item.GetWidth;
    if w > FHRange then
      FHRange := w;
  end;
                                                       
var i, j : integer;
    Item : TElTreeItem;

begin
  FHRange := 0;
  j := FOwner.FAllList.Count;
  i := 0;
  while i < j do
  begin
    Item := TElTreeItem(FOwner.FAllList[i]);
    if (FOwner.FilteredVisibility and Item.Hidden) then
    begin
      i := FOwner.FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end
    else
    begin
      CalcWidth(Item, FOwner);
      if not Item.Expanded then
         i := FOwner.FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end;
    inc(i);
  end;
  exit;

  //FItems.Iterate(true, @CalcWidth, nil);
end;

procedure TElTreeView.Paint;
{$ifdef CLX_USED}
var Bmp : TBitmap;
    R   : TRect;
{$endif}
begin
  FPainting := true;
  {$ifdef CLX_USED}
  Canvas.Start;
  (*
  if FOwner.ShowCheckBoxes and (not FOwner.CustomCheckBoxes) then
  begin
    RedrawTree(Canvas, FOwner.FHPos, FVisible);
  end
  else
  *)
  begin
    Bmp := TBitmap.Create;
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;
    Bmp.Canvas.Start;
    DoPaintBkgnd(Bmp.Canvas.Handle, Canvas.ClipRect);
    R := Canvas.ClipRect;
    if IsRectEmpty(R) then
      R := ClientRect;
    Bmp.Canvas.SetClipRect(R);
    RedrawTree(Bmp.Canvas, FOwner.FHPos, FVisible);
    Canvas.CopyRect(R, Bmp.Canvas, R);
    Bmp.Canvas.Stop;
    Bmp.Free;
  end;
  Canvas.Stop;
  {$else}
  RedrawTree(Canvas, FOwner.FHPos, FVisible);
  {$endif}
  FPainting := false;
end;

{$ifndef CLX_USED}
procedure TElTreeView.FillDragImage;
var
  ABmp: TBitmap;
  TIL : TImageList;
  i, j: integer;
  b: boolean;
  tt,
    bb: integer;
  R   : TRect;

  procedure IntRedrawItemTree(Index: integer; Surface: TBitmap);
  var
    R: TRect;
    SText: TElFString;
    Item: TElTreeItem;
    R1, R3: TRect;
    //SaveStyle: TFontStyles;
    ImDrawStyle: TDrawingStyle;
    StImIndex: integer;

  begin
    Item := FVisible[index];
    SText := Item.Text;

    with FOwner do
    begin

      R := Self.GetItemRect(index);
      dec(R.Top, FDDY);
      dec(R.Bottom, FDDY);
      if RightAlignedTree then
      begin
        if (FShowRoot and FShowLines) or (FShowButtons and FShowRootButtons) then dec(R.Right, ItemExt);
      end else
      begin
        if (FShowRoot and FShowLines) or (FShowButtons and FShowRootButtons) then Inc(R.Left, ItemExt);
      end;

      if RightAlignedTree then
      begin
        dec(R.Right, Item.Level * ItemExt);
      end else
      begin
        inc(R.Left, Item.Level * ItemExt);
      end;

      if FShowImages then
      begin
        if (FImages <> nil) then
        begin
          ImDrawStyle := FImages.DrawingStyle;
          FImages.DrawingStyle := dsFocus;
          if Item.FState = [] then StImIndex := Item.FImageIndex else StImIndex := Item.FStImageIndex;
          if StImIndex = -1 then
            StImIndex := DoGetPicture(Item);
          if InRange(0, FImages.Count - 1, StImIndex) then
          begin
            if RightAlignedTree then
            begin
              FImages.Draw(Surface.Canvas, R.Right - FImages.Width, (R.Top + (R.Bottom - R.Top + 1) shr 1) - (FImages.Height shr 1), StImIndex);
              dec(R.Right, FImages.Width);
            end else
            begin
              FImages.Draw(Surface.Canvas, R.Left, (R.Top + (R.Bottom - R.Top + 1) shr 1) - (FImages.Height shr 1), StImIndex);
              inc(R.Left, FImages.Width);
            end;
          end;
          FImages.DrawingStyle := ImDrawStyle;
        end;
      end; // Show images
      if RightAlignedTree then
      begin
        dec(R.Right, (ItemExt div 3));
      end else
      begin
        inc(R.Left, (ItemExt div 3));
      end;

      if (FODFollowCol and (FHeader.Sections.Count > 0) and (FHeader.Sections[FMainTreeCol].Style = ElhsOwnerDraw))
        or ((not (FODFollowCol)) and (SText = FODMask)) then
      begin
        DoItemDraw(Item, Canvas, R, FMainTreeCol);
      end
      else
      begin
        with Surface.Canvas do
        begin
          {$ifdef ELTREE_USE_STYLES}
          if not Item.ParentStyle then
          {$endif}
          begin
            with Surface.Canvas.Font do
            begin
              Style := [];
              if Item.Bold then Style := [fsBold];
              if Item.Italic then Style := Style + [fsItalic];
              if Item.Underlined then Style := Style + [fsUnderline];
              if Item.StrikeOut then Style := Style + [fsStrikeOut];
            end;
          end;
        end;
        R1 := R;
        SetRectEmpty(R3);
{$IFDEF HAS_HTML_RENDER}
        if Item.IsHTML and (Copy(SText, 1, 6) = '<html>') then
        begin
          R3.Left := 0;
          R3.Top := 0;
          with FRender do
          begin
            Data.Charset := Surface.Canvas.Font.Charset;
            PrepareText(SText, 0, false);
            R3.Right := Data.TextSize.cx;
            R3.Bottom := Data.TextSize.cy;
          end;
        end else
{$ENDIF}
        {$ifdef ELPACK_UNICODE}
          ElVCLUtils.DrawTextW(Surface.Canvas.Handle, PWideChar(SText), Length(SText), R3, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}DT_CALCRECT);
        {$else}
          DrawText(Surface.Canvas.Handle, PChar(SText), Length(SText), R3, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}DT_CALCRECT);
        {$endif}
        if RightAlignedTree then
        begin
          R1.Left := R1.Right - (R3.Right - R3.Left);
        end else
        begin
          R1.Right := R1.Left + (R3.Right - R3.Left);
        end;
{$IFDEF HAS_HTML_RENDER}
        if Item.IsHTML and (Copy(SText, 1, 6) = '<html>') then
        begin
          FRender.Data.DefaultStyle := Surface.Canvas.Font.Style;

          with FRender do
          begin
            //Charset := Surface.Canvas.Font.Charset;
            //PrepareText(SText, 0, false);
            DrawText(Surface.Canvas, Point(0, 0), R1, clNone);
          end;
        end else
{$ENDIF}
        //SaveStyle := Surface.Canvas.Font.Style;
        begin
          if RightAlignedText then
          begin
            {$ifdef ELPACK_UNICODE}
            ElVCLUtils.DrawTextW(Surface.Canvas.Handle, PWideChar(SText), Length(SText), R1, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}DT_VCENTER or DT_RIGHT or DT_RTLREADING);
            {$else}
            DrawText(Surface.Canvas.Handle, PChar(SText), Length(SText), R1, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}DT_VCENTER or DT_RIGHT or DT_RTLREADING);
            {$endif}
          end else
          begin
            {$ifdef ELPACK_UNICODE}
            ElVCLUtils.DrawTextW(Surface.Canvas.Handle, PWideChar(SText), Length(SText), R1, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}DT_VCENTER);
            {$else}
            DrawText(Surface.Canvas.Handle, PChar(SText), Length(SText), R1, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}DT_VCENTER);
            {$endif}
          end;
        end;
        //Surface.Canvas.Font.Style := SaveStyle;
      end; // if ... else
    end;
  end;

  procedure DrawDragImages;
  var
    i: integer;
    R,
      R1: TRect;

  begin
    with FOwner do
    begin
      j := FVisible.Count;
      for i := 0 to j - 1 do
        if (TElTreeItem(FVisible[i]).Selected) then
        begin
          R := Self.GetItemRect(i);
          dec(R.Top, FDDY);
          dec(R.Bottom, FDDY);
          if FHLines then dec(R.Bottom, FDivLineWidth);
          R.Left := 0;
          r1 := r;
          if FShowHeader then
             r1.right := FHeader.Sections[FMainTreeCol].Width
          else
             r1.right := Max(FHRange, FHPos + ClientWidth);
          ABmp.Canvas.Font := Canvas.Font;
          ABmp.Canvas.Font.Color := FTextColor;
          ABmp.Canvas.Pen.Color := FTextColor;
          IntRedrawItemTree(i, ABmp);
        end;
    end;
  end;

begin
  FDragImages := nil;
  if FOwner.DragImageMode = dimNever then exit;
  b := false;
  tt := -1;
  bb := -1;

  with FOwner do
    if DragImageMode = dimOne then
      for i := 0 to FVisible.Count - 1 do
      begin
        if (TElTreeItem(FVisible[i]).Selected) then
        begin
          if b then
          begin
            FDragImages := nil;
            exit;
          end
          else
          begin
            b := true;
            R := Self.GetItemRect(i);
            tt := R.Top;//i * LineHeight;
            bb := R.Bottom;
          end;
        end;
      end
    else
    begin
      for i := 0 to FVisible.Count - 1 do
      begin
        if (TElTreeItem(FVisible[i]).Selected) then
        begin
          R := Self.GetItemRect(i);
          bb := R.Bottom;
          if not b then
          begin
            tt := R.Top;
            b := true;
          end;
        end;
      end;
    end;
  TIL := TImageList.Create(self);
  TIL.Height := bb - tt + 1;
  FDDY := tt;

  with FOwner do
    if FMainTreeCol >= FHeader.Sections.Count then
      FMainTreeCol := 0;
  if FOwner.FShowHeader then
    TIL.Width := FHeader.Sections[FOwner.FMainTreeCol].Width
  else
    TIL.Width := Max(FHRange, FOwner.FHPos + Width);

  ABmp := TBitmap.Create;
  try
    ABmp.Width := TIL.Width;
    ABmp.Height := TIL.Height;
  {$IFNDEF VER90}
    ABmp.TransparentMode := tmFixed;
    ABmp.TransparentColor := FOwner.FBkColor;
  {$ENDIF}
    ABmp.Canvas.FillRect(Rect(0, 0, ABMP.Width, ABMP.Height));
  {$IFNDEF VER90}
    ABmp.Canvas.Lock;
  {$ENDIF}
    try
      DrawDragImages;
    finally
  {$IFNDEF VER90}
      ABmp.Canvas.UnLock;
  {$ENDIF}
    end;
    TIL.AddMasked(ABmp, clDefault);
  finally
    ABmp.Free;
  end;
  FDragImages := TIL;
end;
{$endif}


procedure TElTreeView.DrawImages(ACanvas : TCanvas; Item : TElTreeItem; HelperBitmap : TBitmap; var R : TRect; var ItemRect : TRect);
var
  FTImages : TImageList;
  {$ifndef CLX_USED}
  ImDrawStyle: TDrawingStyle;
  {$endif}
  StImIndex: integer;
  OvIndex  : integer;
  h, w     : integer;
  R2       : TRect;

  procedure DoDrawImage;
  var xp : integer;
  begin
    h := FTImages.Height;
    w := FTImages.Width;

    if FOwner.RightAlignedTree then
      R2 := Rect(Max(R.Left, R.Right - w), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1), R.Right, R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1) + h)
    else
      R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1), Min(R.Right, R.Left + w), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1) + h);

    if R2.Right >= R2.Left then
    begin
      {$ifndef CLX_USED}
      HelperBitmap.PixelFormat := pfDevice;
      {$endif}

      HelperBitmap.Height := R2.Bottom - R2.Top;
      HelperBitmap.Width  := R2.Right - R2.Left;

      {$ifndef CLX_USED}
      bitblt(HelperBitmap.Canvas.Handle, 0, 0, HelperBitmap.Width, HelperBitmap.Height, ACanvas.Handle, R2.Left, R2.Top, srccopy);
      {$else}
      ACanvas.Start;
      HelperBitmap.Canvas.Start;
      with HelperBitmap do
        bitblt(QPainter_device(HelperBitmap.Canvas.Handle), 0, 0, QPainter_device(ACanvas.Handle), R2.Left, R2.Top, Width, Height, Rasterop_CopyRop, true);

      //HelperBitmap.Canvas.Stop;
      //ACanvas.Stop;
      {$endif}

      if FOwner.RightAlignedTree then
        xp := HelperBitmap.Width - w
      else
        xp := 0;
      {$ifndef CLX_USED}
      if OvIndex <> -1 then
        FTImages.DrawOverlay(HelperBitmap.Canvas, xp, 0, StImIndex, OvIndex)
      else
      {$endif}
        FTImages.Draw(HelperBitmap.Canvas, xp, 0, StImIndex);

      {$ifndef CLX_USED}
      bitblt(ACanvas.Handle, r2.Left, R2.Top, HelperBitmap.Width, HelperBitmap.Height, HelperBitmap.Canvas.Handle, 0, 0, srccopy);
      {$else}
      //HelperBitmap.Canvas.Start;
      //ACanvas.Start;
      with HelperBitmap do
        bitblt(QPainter_device(ACanvas.Handle), R2.Left, R2.Top, QPainter_device(HelperBitmap.Canvas.Handle), 0, 0, Width, Height, Rasterop_CopyRop, true);

      HelperBitmap.Canvas.Stop;
      ACanvas.Stop;

      {$endif}
    end;

    if FOwner.RightAlignedTree then dec(R.Right, w) else inc(R.Left, w);
  end;

begin
  if R.Left >= R.Right then exit;

  with FOwner do
  begin
    if FImages2 <> nil then
      FTImages := FImages2
    else
      FTImages := FImages;
    if FTImages <> nil then
    begin
      {$ifndef CLX_USED}
      ImDrawStyle := FTImages.DrawingStyle;
      if FChStateImage and RowSelect then
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
      if FChStateImage and Item.Cut then
        FTImages.DrawingStyle := dsSelected;
      {$endif}
      // draw 2nd image
      if (Item.Focused or Item.Selected or (Item.Expanded {and Item.HasVisibleChildren})) then
        StImIndex := Item.FStImageIndex2
      else
        StImIndex := Item.FImageIndex2;
      if StImIndex = -1 then
         StImIndex := DoGetPicture2(Item);
      OvIndex := Item.OverlayIndex2;
      if InRange(0, FTImages.Count - 1, StImIndex) then
      begin
        if Item.ShowCheckBox and FOwner.ShowCheckboxes then
          if RightAlignedTree then
            dec(R.Right, ItemExt div 3)
          else
            inc(R.Left, ItemExt div 3);

        DoDrawImage;
        Item.FBoolData1 := Item.FBoolData1 or ibfImageDrawn2;
      end else
      begin
        if FShowEmptyImages2 then
        begin
          if FOwner.RightAlignedTree then
            dec(R.Right, FTImages.Width) else inc(R.Left, FTImages.Width);
          Item.FBoolData1 := Item.FBoolData1 or ibfImageDrawn2;
        end else
        Item.FBoolData1 := Item.FBoolData1 and not ibfImageDrawn2;
      end;
      {$ifndef CLX_USED}
      FTImages.DrawingStyle := ImDrawStyle;
      {$endif}
    end;

    if (FImages <> nil) then
    begin
      FTImages := FImages;
      {$ifndef CLX_USED}
      ImDrawStyle := FImages.DrawingStyle;
      if FChStateImage and RowSelect then
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
        if FChStateImage and Item.Cut then
          FTImages.DrawingStyle := dsSelected;
      {$endif}
      if (Item.Focused or Item.Selected or (Item.Expanded {and Item.HasVisibleChildren})) then
        StImIndex := Item.FStImageIndex
      else
        StImIndex := Item.FImageIndex;
      if StImIndex = -1 then
        StImIndex := DoGetPicture(Item);

      {$ifndef CLX_USED}
      OvIndex := Item.OverlayIndex;
      {$endif}
      if InRange(0, FTImages.Count - 1, StImIndex) then
      begin
        if RightAlignedTree then
        begin
          if ((Item.FBoolData1 and ibfImageDrawn2) = ibfImageDrawn2) or
             (Item.ShowCheckBox and FOwner.ShowCheckboxes) then
            dec(R.Right, ItemExt div 3); // make the space between images
        end
        else
        begin
          if ((Item.FBoolData1 and ibfImageDrawn2) = ibfImageDrawn2) or
             (Item.ShowCheckBox and FOwner.ShowCheckboxes) then
            inc(R.Left, ItemExt div 3); // make the space between images
        end;
        DoDrawImage;
        Item.FBoolData1 := Item.FBoolData1 or ibfImageDrawn;
      end
      else
        if FShowEmptyImages then
        begin
          if RightAlignedTree then
          begin
            if (Item.FBoolData1 and ibfImageDrawn2) = ibfImageDrawn2 then
              dec(R.Right, ItemExt div 3); // make the space between images
          end
          else
          begin
            if (Item.FBoolData1 and ibfImageDrawn2) = ibfImageDrawn2 then
              inc(R.Left, ItemExt div 3); // make the space between images
          end;
          if FOwner.RightAlignedTree then
            dec(R.Right, FTImages.Width)
          else
            inc(R.Left, FTImages.Width);
            
          Item.FBoolData1 := Item.FBoolData1 or ibfImageDrawn;
        end
        else
          Item.FBoolData1 := Item.FBoolData1 and not ibfImageDrawn;
      {$ifndef CLX_USED}
      FTImages.DrawingStyle := ImDrawStyle;
      {$endif}
    end;
  end;
end;
{.$HINTS OFF}
{$WARNINGS OFF}
procedure TElTreeView.DrawButtons(ACanvas : TCanvas; Item : TElTreeItem; IsNode : boolean; HelperBitmap : TBitmap; var R : TRect; var ItemRect : TRect);
var FCCanvas : TCanvas;
    //FCBitmap : HBitmap;
    FCBitmap : TBitmap;
    R1 : TRect;
    w,
    h  : integer;
    TC : TColor;
    {$ifdef MSWINDOWS}
    sid: integer;
    PX : TSize;
    {$endif}
    //HR : HRESULT;
begin
  if R.Left >= R.Right then exit;
  with FOwner do
  begin
    {$ifdef MSWINDOWS}
    sid := 0;
    h := 0;
    {$endif}
    // w := 0;
    {$ifdef MSWINDOWS}
    if IsThemeApplied then
    begin
      if Item.Expanded then
        sid := GLPS_OPENED
      else
        sid := GLPS_CLOSED;
      //PX.cx := 0;
      //PX.cy := 0;
      SetRectEmpty(R1);
      {$ifndef CLX_USED}
      GetThemePartSize(Theme, ACanvas.Handle, TVP_GLYPH, sid, nil, 1, PX);
      {$else}
      GetThemePartSize(Theme, QPaintDevice_handle(QPainter_device(ACanvas.Handle)), TVP_GLYPH, sid, nil, 1, PX);
      {$endif}
      w := Px.cx;
      h := Px.cy;
      //w := 10;
      //h := 10;
      CenterRects(w + 1, ItemExt, h + 1, R.Bottom - R.Top + 1, R1);
    end
    else
    {$endif}
    begin
      if FCustomPlusMinus then
      begin
        w := PlusPicture.Width;
        h := PlusPicture.Height;

        CenterRects(w, ItemExt, h, R.Bottom - R.Top + 1, R1);

        if not IsNode then
        begin
          FCCanvas := FLeafPicture.Canvas;
          //FCBitmap := FPlusPicture.Handle;
          {$ifndef CLX_USED}
          TC := FLeafPicture.Canvas.Pixels[0, h-1];
          {$else}
          TC := GetPixel(FLeafPicture.Canvas, 0, h - 1);
          {$endif}
          //FCBitmap := FPlusPicture.Handle;
          FCBitmap := FLeafPicture;
        end
        else
        if not Item.Expanded then
        begin
          FCCanvas := FPlusPicture.Canvas;
          {$ifndef CLX_USED}
          TC := FPlusPicture.Canvas.Pixels[0, h-1];
          {$else}
          TC := GetPixel(FPlusPicture.Canvas, 0, h - 1);
          {$endif}
          FCBitmap := FPlusPicture;
        end else
        begin
          FCCanvas := FMinusPicture.Canvas;
          {$ifndef CLX_USED}
          TC := FMinusPicture.Canvas.Pixels[0, h-1];
          {$else}
          TC := GetPixel(FMinusPicture.Canvas, 0, h - 1);
          {$endif}
          FCBitmap := FMinusPicture;
        end;
      end
      else
      begin
        w := 10;
        CenterRects(w, ItemExt, w, R.Bottom - R.Top + 1, R1);
        if not IsNode then
        begin
          FCCanvas := LeafBmp.Canvas;
          {$ifndef CLX_USED}
          TC := LeafBmp.Canvas.Pixels[1, LeafBmp.Height-2];
          {$else}
          TC := GetPixel(LeafBmp.Canvas, 1, LeafBmp.Height-2);
          {$endif}
          //FCBitmap := PlusBmp.Handle;
          FCBitmap := LeafBmp;
        end
        else
        if not Item.Expanded then
        begin
          FCCanvas := PlusBmp.Canvas;
          {$ifndef CLX_USED}
          TC := PlusBmp.Canvas.Pixels[1, PlusBmp.Height-2];
          {$else}
          TC := GetPixel(PlusBmp.Canvas, 1, PlusBmp.Height-2);
          {$endif}
          FCBitmap := PlusBmp;
        end else
        begin
          FCCanvas := MinusBmp.Canvas;
          {$ifndef CLX_USED}
          TC := MinusBmp.Canvas.Pixels[1, MinusBmp.Height - 2];
          {$else}
          TC := GetPixel(MinusBmp.Canvas, 1, MinusBmp.Height - 2);
          {$endif}
          FCBitmap := MinusBmp;
        end;
      end;
    end;

    if RightAlignedTree then
    begin
      R1 := Rect(max(R.Right - R1.Right, R.Left), R1.Top + R.Top, R.Right - (R1.Right - w), R1.Bottom + R.Top);
      {$ifdef MSWINDOWS}
      if IsThemeApplied then
      begin
        {$ifndef CLX_USED}
        DrawThemeBackground(Theme, ACanvas.Handle, TVP_GLYPH, sid, Rect(R.Right - w, R1.top, R.Right, R1.Top + h), @R1);
        {$else}
        ACanvas.Start;
        DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(ACanvas.Handle)), TVP_GLYPH, sid, Rect(R.Right - w, R1.top, R.Right, R1.Top + h), @R1);
        ACanvas.Stop;
        {$endif}
      end
      else
      {$endif}
      begin
        {$ifndef CLX_USED}
        if FOwner.FTransButtons then
          DrawTransparentBitmapEx(ACanvas.Handle, FCBitmap, R1.Left, R1.Top, Rect(w - (R1.Right - R1.Left), 0, w, R1.Bottom - R1.Top), TC)
        else
          BitBlt(ACanvas.Handle, R1.Left, R1.Top, R1.Right - R1.Left + 1, R1.Bottom - R1.Top + 1, FCCanvas.Handle, w - (R1.Right - R1.Left), 0, SRCCOPY);
        {$else}
        if FOwner.FTransButtons then
        begin
          ACanvas.Start;
          FCCanvas.Start;
          DrawTransparentBitmapEx(ACanvas.Handle, FCBitmap, R1.Left, R1.Top, Rect(w - (R1.Right - R1.Left), 0, w, R1.Bottom - R1.Top), TC);
          FCCanvas.Stop;
          ACanvas.Stop;
        end
        else
          ACanvas.Start;
          FCCanvas.Start;
          //ACanvas.CopyRect(R1, FCCanvas, Rect(0, 0, R1.Right - R1.Left + 1, R1.Bottom - R1.Top + 1));
          BitBlt(QPainter_device(ACanvas.Handle), R1.Left, R1.Top, QPainter_device(FCCanvas.Handle), w - (R1.Right - R1.Left), 0, R1.Right - R1.Left + 1, R1.Bottom - R1.Top + 1, RasterOp_CopyROP, true);
          ACanvas.Stop;
          FCCanvas.Stop;
        {$endif}
      end;
    end
    else
    begin
      OffsetRect(R1, R.Left, R.Top);
      {$ifdef MSWINDOWS}
      if IsThemeApplied then
      begin
        {$ifndef CLX_USED}
        DrawThemeBackground(Theme, ACanvas.Handle, TVP_GLYPH, sid, Rect(R1.Left, R1.top, R1.Left + w, R1.Top + h), @R1);
        {$else}
        ACanvas.Start;
        DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(ACanvas.Handle)), TVP_GLYPH, sid, Rect(R1.Left, R1.top, R1.Left + w, R1.Top + h), @R1);
        ACanvas.Stop;
        {$endif}
      end
      else
      {$endif}
      begin
        {$ifndef CLX_USED}
        if FOwner.FTransButtons then
          DrawTransparentBitmapEx(ACanvas.Handle, FCBitmap, R1.Left, R1.Top,
                                   Rect(0, 0, min(R1.Right - R1.Left, R.Right - R1.Left), R1.Bottom - R1.Top), TC)
        else
          BitBlt(ACanvas.Handle, R1.Left, R1.Top, min(R1.Right - R1.Left + 1, R.Right - R.Left), R1.Bottom - R1.Top + 1, FCCanvas.Handle, 0, 0, SRCCOPY);
        {$else}
        if FOwner.FTransButtons then
        begin
          ACanvas.Start;
          FCCanvas.Start;
          DrawTransparentBitmapEx(ACanvas.Handle, FCBitmap, R1.Left, R1.Top,
                                   Rect(0, 0, min(R1.Right - R1.Left, R.Right - R1.Left), R1.Bottom - R1.Top), TC);
          FCCanvas.Stop;
          ACanvas.Stop;
        end
        else
          //ACanvas.CopyRect(Rect(R1.Left, R1.Top, R1.Left + min(R1.Right - R1.Left + 1, R.Right - R.Left), R1.Bottom), FCCanvas, Rect(0, 0, min(R1.Right - R1.Left + 1, R.Right - R.Left), R1.Bottom - R1.Top + 1));
          ACanvas.Start;
          FCCanvas.Start;
          BitBlt(QPainter_device(ACanvas.Handle), R1.Left, R1.Top, QPainter_device(FCCanvas.Handle), 0, 0, min(R1.Right - R1.Left + 1, R.Right - R.Left), R1.Bottom - R1.Top + 1, RasterOp_CopyROp, false);
          FCCanvas.Stop;
          ACanvas.Stop;
        {$endif}
      end;
    end;
  end;
end;
{$WARNINGS ON}
procedure TElTreeView.DrawCheckBoxes(ACanvas : TCanvas; Item : TElTreeItem; HelperBitmap : TBitmap; var R : TRect; var ItemRect : TRect);
var
  cbh,
  cbw,
  i    : integer;
  {$ifdef MSWINDOWS}
  R1,
  {$endif}
  R2,
  R3   : TRect;
  {$ifndef CLX_USED}
  ADC   : HDC;
  HObj  : HGDIObj;
  {$endif}
  {$ifdef MSWINDOWS}
  PX    : TSize;
  pid,
  {$endif}
  {$ifdef MSWINDOWS}
  sid   : integer;
  ATheme: HTheme;
  {$endif}

begin
  with FOwner do
  begin
    if (Item.FBoolData1 and ibfShowCheckBox) = ibfShowCheckBox then
    begin
      cbw := 0;
      {$ifdef MSWINDOWS}
      sid := 0;
      {$endif}
      try
        {
        if RightAlignedTree then
          dec(R.Right, 2)
        else
          inc(R.Left, 2);
        }
        {$ifdef MSWINDOWS}
        if IsThemeApplied and (not FCustomCheckBoxes) then
        begin
          if Item.FCheckBoxType = ectRadioButton then
          begin
            pid := BP_RADIOBUTTON;
          end
          else
          begin
            pid := BP_CHECKBOX;
          end;
          SetRectEmpty(R1);
          {$ifndef CLX_USED}
          ATheme := OpenThemeData(Handle, 'BUTTON');
          {$else}
          ATheme := OpenThemeData(QWidget_winID(Handle), 'BUTTON');
          {$endif}
          if ATheme <> 0 then
          begin
            SetRectEmpty(R1);
            {$ifndef CLX_USED}
            GetThemePartSize(ATheme, Canvas.Handle, pid, 0, nil, 1, PX);
            {$else}
            Canvas.Start;
            GetThemePartSize(ATheme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), pid, 0, nil, 1, PX);
            Canvas.Stop;
            {$endif}
            cbw := Px.cx;
            cbh := Px.cy;
            CloseThemeData(ATheme);
          end
          else
          begin
            cbw := FCheckBoxSize;
            cbh := FCheckBoxSize;
          end;
          if Item.FCheckBoxType = ectRadioButton then
          begin
            if not Item.Checked then
            begin
              if not Item.CheckBoxEnabled then
                sid := RBS_UNCHECKEDDISABLED
              else
                sid := RBS_UNCHECKEDNORMAL;
            end
            else
            begin
              if not Item.CheckBoxEnabled then
                sid := RBS_CHECKEDDISABLED
              else
                sid := RBS_CHECKEDNORMAL;
            end;
          end
          else
          begin
            case Item.CheckBoxState of
              cbUnchecked :
                if not Item.CheckboxEnabled then
                  sid := CBS_UNCHECKEDDISABLED
                else
                  sid := CBS_UNCHECKEDNORMAL;

              cbChecked :
                if not Item.CheckboxEnabled then
                  sid := CBS_CHECKEDDISABLED
                else
                  sid := CBS_CHECKEDNORMAL;

              cbGrayed :
                if not Item.CheckboxEnabled then
                  sid := CBS_MIXEDDISABLED
                else
                  sid := CBS_MIXEDNORMAL;
            end;
          end;
          if RightAlignedTree then
          begin
            R2 := Rect(Max(R.Left, R.Right - cbw), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (cbh shr 1), R.Right, R.Top + ((R.Bottom - R.Top + 1) shr 1) + cbh shr 1 + 1);
            R3 := Rect(R2.Right - cbw, r2.top, R2.Right, r2.Top + cbh);
          end
          else
          begin
            R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) shr 1) - (cbh shr 1), Min(R.Right, R.Left + cbw), R.Top + ((R.Bottom - R.Top + 1) shr 1) + cbh shr 1 + 1);
            R3 := Rect(R2.Left, R2.Top, R2.Left + cbw, R2.Top + cbh);
          end;
          if R.Left >= R2.Right then exit;
          {$ifndef CLX_USED}
          DrawThemeBackgroundTo('BUTTON', ACanvas.Handle, pid, sid, R3, @R2);
          {$else}
          ACanvas.Start;
          DrawThemeBackgroundTo('BUTTON', QPaintDevice_handle(QPainter_device(ACanvas.Handle)), pid, sid, R3, @R2);
          ACanvas.Stop;
          {$endif}
        end
        else
        {$endif}
        if FCustomCheckboxes then
        begin
          if Item.FCheckBoxType = ectRadioButton then
          begin
            cbh := FRadioButtonGlyph.Height;
            cbw := FRadioButtonGlyph.Width div 6;

            if not Item.Checked then
            begin
              if Item.CheckBoxEnabled then
                R3 := Rect(0, 0, cbw, cbh)
              else
                R3 := Rect(cbw, 0, cbw * 2, cbh);
            end
            else
            begin
              if Item.CheckBoxEnabled then
                R3 := Rect(cbw * 2, 0, cbw * 3, cbh)
              else
                R3 := Rect(cbw * 3, 0, cbw * 6, cbh);
            end;
          end
          else
          if Item.FCheckBoxType = ectCheckBox then
          begin
            cbh := FCheckBoxGlyph.Height;
            cbw := FCheckBoxGlyph.Width div 6;

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
            cbh := FCheckBoxGlyph.Height;
            cbw := FCheckBoxGlyph.Width div 6;

            case Item.CheckBoxState of
              cbUnchecked:
                if Item.CheckBoxEnabled then R3 := Rect(0, 0, cbw, cbh)
                else R3 := Rect(cbw, 0, cbw * 2, cbh);
              cbChecked:
                if Item.CheckBoxEnabled then R3 := Rect(cbw * 2, 0, cbw * 3, cbh)
                else R3 := Rect(cbw * 3, 0, cbw * 4, cbh);
              cbGrayed:
                if Item.CheckBoxEnabled then R3 := Rect(cbw * 4, 0, cbw * 5, cbh)
                else R3 := Rect(cbw * 5, 0, cbw * 6, cbh);
            end;
          end;

          if RightAlignedTree then
          begin
            R2 := Rect(R.Right - cbw + 1, R.Top + ((R.Bottom - R.Top + 1) shr 1) - cbh shr 1, R.Right, R.Top + ((R.Bottom - R.Top + 1) shr 1) + cbw shr 1);

            if R.Left >= R2.Right then exit;
            if R.Left >= R.Right - cbw then
            begin
              R2.Left := R.Left;
              R3.Left := r3.Right - (R.Right - R.Left + 1);
              HelperBitmap.Width := R.Right - R2.Left;
            end
            else
              HelperBitmap.Width := cbw;
          end
          else
          begin
            R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) shr 1) - cbh shr 1, R.Left + cbw, R.Top + ((R.Bottom - R.Top + 1) shr 1) + cbw shr 1);

            if R2.Left >= R.Right then exit;
            if R2.Left + cbw >= R.Right then
            begin
              R2.Right := R.Right;
              R3.Right := R3.Left + (R.Right - R.Left + 1);
              HelperBitmap.Width := R.Right - R.Left + 1;
            end
            else
              HelperBitmap.Width := cbw;
          end;
          HelperBitmap.Height := R2.Bottom - R2.Top + 1;

          {$ifndef CLX_USED}
          bitblt(HelperBitmap.Canvas.Handle, 0, 0, HelperBitmap.Width, HelperBitmap.Height, ACanvas.Handle, r2.Left, R2.Top, srccopy);
          if Item.FCheckBoxType = ectRadioButton then
          begin
            FRadioButtonGlyph.Pixelformat := pfDevice;
            DrawTransparentBitmapEx(HelperBitmap.Canvas.Handle, FRadioButtonGlyph, 0, 0, R3, FRadioButtonGlyph.traNsparentcolor);
          end
          else
          begin
            FCheckBoxGlyph.Pixelformat := pfDevice;
            DrawTransparentBitmapEx(HelperBitmap.Canvas.Handle, FCheckBoxGlyph, 0, 0, R3, FCheckBoxGlyph.TransparentColor);
          end;
          bitblt(ACanvas.Handle, r2.Left, R2.Top, HelperBitmap.Width, HelperBitmap.Height, HelperBitmap.Canvas.Handle, 0, 0, srccopy);
          {$else}
          ACanvas.Start;
          HelperBitmap.Canvas.Start;
          bitblt(QPainter_device(HelperBitmap.Canvas.Handle), 0, 0, QPainter_device(ACanvas.Handle), r2.Left, R2.Top, HelperBitmap.Width, HelperBitmap.Height, RasterOp_CopyROP, true);
          if Item.FCheckBoxType = ectRadioButton then
          begin
            DrawTransparentBitmapEx(HelperBitmap.Canvas.Handle, FRadioButtonGlyph, 0, 0, R3, FRadioButtonGlyph.traNsparentcolor);
          end
          else
          begin
            DrawTransparentBitmapEx(HelperBitmap.Canvas.Handle, FCheckBoxGlyph, 0, 0, R3, FCheckBoxGlyph.TransparentColor);
          end;
          bitblt(QPainter_device(ACanvas.Handle), r2.Left, R2.Top, QPainter_device(HelperBitmap.Canvas.Handle), 0, 0, HelperBitmap.Width, HelperBitmap.Height, RasterOp_CopyROP, true);
          HelperBitmap.Canvas.Stop;
          ACanvas.Stop;
          {$endif}
        end
        else
        begin
          //cbw := ItemExt - 2;
          cbw := FOwner.CheckBoxSize;
          cbh := cbw;
          //cbh := ItemExt - 2;

          {$ifndef CLX_USED}
          i := DFCS_BUTTONCHECK or DFCS_CHECKED;
          if Item.FCheckBoxType = ectRadioButton then
          begin
            if Item.Checked then i := DFCS_BUTTONRADIO or DFCS_CHECKED
            else i := DFCS_BUTTONRADIO;
          end else
            if Item.FCheckBoxType = ectCheckBox then
            begin
              if Item.Checked then i := DFCS_BUTTONCHECK or DFCS_CHECKED
              else i := DFCS_BUTTONCHECK;
            end else
            begin
              case Item.FCheckBoxState of //
                cbChecked: i := DFCS_BUTTONCHECK or DFCS_CHECKED;
                cbUnchecked: i := DFCS_BUTTONCHECK;
                cbGrayed: i := DFCS_BUTTON3STATE or DFCS_CHECKED;
              end; // case
            end;
          if (Item.FBoolData1 and ibfCheckBoxEnabled) <> ibfCheckBoxEnabled then
            i := i or DFCS_INACTIVE;
          {$endif}

          if RightAlignedTree then
          begin
            R2 := Rect(Max(R.Left, R.Right - (cbw - 2)), R.Top + ((R.Bottom - R.Top + 1) shr 1) - cbh shr 1, R.Right, R.Top + ((R.Bottom - R.Top + 1) shr 1) + cbh shr 1);

            R3 := Rect(0, 0, cbw - 1, cbw - 1);
            if R.Left >= R2.Right then exit;
            if R.Left >= R.Right - cbw then
            begin
              R2.Left := R.Left;
              OffsetRect(R3, -(R.Left - (R.Right - cbw)), 0);
              HelperBitmap.Width := R.Right - R2.Left;
            end
            else
              HelperBitmap.Width := cbw;
          end
          else
          begin
            R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) shr 1) - cbh shr 1, Min(R.Right, R.Left + cbw), R.Top + ((R.Bottom - R.Top + 1) shr 1) + cbh shr 1);

            R3 := Rect(0, 0, cbw -1, cbw -1);
            if R2.Left >= R.Right then exit;
            if R2.Left + cbw >= R.Right then
            begin
              R2.Right := R.Right;
              HelperBitmap.Width := R.Right - R2.Left;
            end
            else
              HelperBitmap.Width := cbw;
          end;
          HelperBitmap.Height := cbw - 1;

          {$ifndef CLX_USED}
          ADC := CreateCompatibleDC(ACanvas.Handle);
          HObj := SelectObject(ADC, HelperBitmap.Handle);
          bitblt(ADC, 0, 0, HelperBitmap.Width, HelperBitmap.Height, ACanvas.Handle, r2.Left, R2.Top, srccopy);

          //SetBkMode(ADC, OPAQUE);
          DrawFrameControl(ADC, R3, DFC_BUTTON, i);

          bitblt(ACanvas.Handle, r2.Left, R2.Top, HelperBitmap.Width, HelperBitmap.Height, ADC, 0, 0, srccopy);
          SelectObject(ADC, HObj);
          DeleteDC(ADC);
          {$else}
          ACanvas.Brush.Style := bsClear;
          ACanvas.Brush.Color := clBtnFace;
          ACanvas.Start;
          // this is a dirty trick
          ACanvas.FillRect(Rect(R2.Left, R2.Top, R2.Left + HelperBitmap.Width - 1, R2.Top + HelperBitmap.Height - 1));
          i := 0;
          if Item.FCheckBoxType = ectCheckBox then
            case Item.FCheckBoxState of
              cbChecked : i := integer(QButtonToggleState_On);
              cbGrayed :  i := integer(QButtonToggleState_NoChange);
              cbUnchecked:i := integer(QButtonToggleState_Off);
            end; // case

          if Item.FCheckBoxType = ectRadioButton then
            QStyle_DrawExclusiveIndicator(Application.Style.Handle,
                 ACanvas.Handle,
                 R2.Left,
                 R2.Top,
                 HelperBitmap.Width,
                 HelperBitmap.Height,
                 QWidget_colorGroup(Handle),
                 Item.Checked,
                 false,
                 (Item.FBoolData1 and ibfCheckBoxEnabled) = ibfCheckBoxEnabled)
          else
            QStyle_DrawIndicator(Application.Style.Handle,
                 ACanvas.Handle,
                 R2.Left,
                 R2.Top,
                 HelperBitmap.Width,
                 HelperBitmap.Height,
                 QWidget_colorGroup(Handle),
                 i,
                 false,
                 (Item.FBoolData1 and ibfCheckBoxEnabled) = ibfCheckBoxEnabled);
          ACanvas.Stop;
          {$endif}
        end;
      finally
        if RightAlignedTree then
          dec(R.Right, cbw)
        else
          inc(R.Left, cbw);
      end;
    end;
  end;
end;

procedure TElTreeView.DrawItemLines(ACanvas : TCanvas; Item : TElTreeItem; var R : TRect; var ItemRect : TRect);

var
  Stack : TElStack;
  //ItemRoot,
  TSI,
    TSI1: TElTreeItem;
  SavePen    : TPenStyle;
  SavePenCol : TColor;

function GetPrevVisChild(Parent, Item: TElTreeItem; NoRoot : boolean): TElTreeItem;
  begin
    if NoRoot and (Item.FParent = FItems.FRoot) then
    begin
      result := nil;
      exit;
    end;
    Result := Parent.GetPrevChild(Item);
    if FOwner.FilteredVisibility then
      while Assigned(Result) and (Result.Hidden) do Result := Parent.GetPrevChild(Result);
  end;

  function GetNextVisChild(Parent, Item: TElTreeItem; NoRoot : boolean): TElTreeItem;
  begin
    if NoRoot and (Item.FParent = FItems.FRoot) then
    begin
      Result := nil;
      exit;
    end;
    Result := Parent.GetNextChild(Item);
    if FOwner.FilteredVisibility then
      while Assigned(Result) and (Result.Hidden) do Result := Parent.GetNextChild(Result);
  end;

  procedure DrawLine(Canvas : TCanvas; StartX, StartY, EndX, EndY : integer);
  var
    Coord: Integer;
    // CRef: COLORREF;
    // DC: HDC;
  begin
    Canvas.Pen.Style := psSolid;
    if FOwner.LinesStyle = psDot then
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
          {$ifdef CLX_USED}
          Canvas.LineTo(StartX, Coord-1);
          {$else}
          Canvas.LineTo(StartX, Coord);
          {$endif}
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
          {$ifdef CLX_USED}
          Canvas.LineTo(Coord - 1, StartY);
          {$else}
          Canvas.LineTo(Coord, StartY);
          {$endif}
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

begin
  inc(R.Bottom);
  try
    with FOwner do
    begin
      Stack := TElStack.Create;
      TSI := Item.Parent;
      SavePen := ACanvas.Pen.Style;
      SavePenCol := ACanvas.Pen.Color;
      ACanvas.Pen.Style := LinesStyle;
      ACanvas.Pen.Color := LinesColor;
      while TSI <> nil do
      begin
        Stack.Push(TSI);
        TSI := TSI.Parent;
      end;
      //DC := ACanvas.Handle;

      if Item.FParent <> FItems.FRoot then
      begin
        TSI := Item;

        while TSI.FParent <> FItems.FRoot do
          TSI := TSI.FParent;
        if ShowRoot and (GetNextVisChild(TSI.FParent, TSI, false) <> nil) then
        begin
          if RightAlignedTree then
          begin
            Inc(R.Right, ItemExt);
            if (R.Right - (ItemExt shr 1 {+ 4}) > R.Left) then
              DrawLine(ACanvas, R.Right - (ItemExt shr 1 {+ 4}), R.Top{ + ((R.Bottom - R.Top + 1) shr 1)}, R.Right - (ItemExt shr 1 {+ 4}), R.Top + (R.Bottom - R.Top + 1));
            Dec(R.Right, ItemExt);
          end else
          begin
            Dec(R.Left, ItemExt);
            if (R.Left + (ItemExt shr 1 {- 4}) < R.Right) then
              DrawLine(ACanvas, R.Left + (ItemExt shr 1 {- 4}), R.Top, R.Left + (ItemExt shr 1 {- 4}), R.Top + (R.Bottom - R.Top + 1));
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

            if (GetNextVisChild(TSI1, TSI, true) <> nil) and ((R.Left + ItemExt shr 1) < R.Right) and
               (GetNextNotLineSuppressedSibling(TSI) <> nil) then
              if RightAlignedTree then
              begin
                if (GetNextVisChild(TSI1, TSI, true) <> nil) and ((R.Right - ItemExt shr 1) > R.Left) then
                  DrawLine(ACanvas, R.Right - ItemExt shr 1, R.Top, R.Right - ItemExt shr 1, ItemRect.Bottom + 1);
              end
              else
              begin
                if (GetNextVisChild(TSI1, TSI, true) <> nil) and ((R.Left + ItemExt shr 1) < R.Right) then
                  DrawLine(ACanvas, R.Left + ItemExt shr 1, R.Top,  R.Left + ItemExt shr 1, ItemRect.Bottom + 1);
              end;
          end;
          if RightAlignedTree then
            dec(R.Right, ItemExt)
          else
            inc(R.Left, ItemExt);
        end;
        if RightAlignedTree then
          dec(R.Right, ItemExt)
        else
          inc(R.Left, ItemExt);
      end;
      Stack.Free;

      if (Item.FParent <> Item.FRoot) then
      begin
        if RightAlignedTree then
        begin
          inc(R.Right, ItemExt);
          if ((R.Right - ItemExt shr 1) > R.Left) then
          begin
            DrawLine(ACanvas, R.Right - ItemExt shr 1, R.Top, R.Right - ItemExt shr 1, R.Top + ((R.Bottom - R.Top + 1) shr 1));
            if not Item.SuppressLines then
              DrawLine(ACanvas, R.Right - ItemExt shr 1, R.Top + ((R.Bottom - R.Top + 1) shr 1), Max(R.Right - ItemExt, R.Left), R.Top + ((R.Bottom - R.Top + 1) shr 1));
          end;
        end
        else
        begin
          dec(R.Left, ItemExt);
          if ((R.Left + ItemExt shr 1) < R.Right) then
          begin
            if not Item.SuppressLines or (GetNextNotLineSuppressedSibling(Item) <> nil) then
              DrawLine(ACanvas, R.Left + ItemExt shr 1, R.Top, R.Left + ItemExt shr 1, R.Top + ((R.Bottom - R.Top + 1) shr 1));
            if not Item.SuppressLines then
              DrawLine(ACanvas, R.Left + ItemExt shr 1, R.Top + ((R.Bottom - R.Top + 1) shr 1), Min(R.Left + ItemExt, R.Right), R.Top + ((R.Bottom - R.Top + 1) shr 1));
          end;
        end;

        if (GetNextVisChild(Item.Parent, Item, true) <> nil) and
           (GetNextNotLineSuppressedSibling(Item) <> nil) then
        begin
          if RightAlignedTree then
          begin
            if ((R.Right - ItemExt shr 1) > R.Left) then
              DrawLine(ACanvas, R.Right - ItemExt shr 1, R.Top + ((R.Bottom - R.Top + 1) shr 1), R.Right - ItemExt shr 1, R.Bottom + 1);
          end else
          begin
            if ((R.Left + ItemExt shr 1) < R.Right) then
              DrawLine(ACanvas, R.Left + ItemExt shr 1, R.Top + ((R.Bottom - R.Top + 1) shr 1), R.Left + ItemExt shr 1, R.Bottom + 1);
          end;
        end; //if
      end // if
      else
      begin
        if RightAlignedTree then
          inc(R.Right, ItemExt)
        else
          dec(R.Left, ItemExt);

        if FShowRoot then
        begin
          if RightAlignedTree then
          begin
            if (R.Right - (ItemExt shr 1{ + 4}) > R.Left) then
            begin
              if not Item.SuppressLines then
                DrawLine(ACanvas, R.Right - (ItemExt shr 1{ + 4}), R.Top + ((R.Bottom - R.Top + 1) shr 1), Max(R.Right - ItemExt, R.Left), R.Top + ((R.Bottom - R.Top + 1) shr 1));
              if GetPrevVisChild(Item.FParent, Item, false) <> nil then
                DrawLine(ACanvas, R.Right - (ItemExt shr 1 {+ 4}), R.Top, R.Right - (ItemExt shr 1 {+ 4}), R.Top + ((R.Bottom - R.Top + 1) shr 1));
              if not Item.SuppressLines then
                if GetNextVisChild(Item.FParent, Item, false) <> nil then
                   DrawLine(ACanvas, R.Right - (ItemExt shr 1 {+ 4}), R.Top + ((R.Bottom - R.Top + 1) shr 1), R.Right - (ItemExt shr 1 {+ 4}), R.Top + (R.Bottom - R.Top + 1));
            end;
          end else
          begin
            if (R.Left + (ItemExt shr 1 {- 4}) < R.Right) then
            begin
              if not Item.SuppressLines then
                DrawLine(ACanvas, R.Left + (ItemExt shr 1 {- 4}), R.Top + ((R.Bottom - R.Top + 1) shr 1), Min(R.Left + ItemExt, R.Right), R.Top + ((R.Bottom - R.Top + 1) shr 1));
              if GetPrevVisChild(Item.FParent, Item, false) <> nil then
                DrawLine(ACanvas, R.Left + (ItemExt shr 1 {- 4}), R.Top, R.Left + (ItemExt shr 1 {- 4}), R.Top + ((R.Bottom - R.Top + 1) shr 1));
              if (GetNextVisChild(Item.FParent, Item, false) <> nil) then
                if not Item.SuppressLines then
                  DrawLine(ACanvas, R.Left + (ItemExt shr 1 {- 4}), R.Top + ((R.Bottom - R.Top + 1) shr 1), R.Left + (ItemExt shr 1 {- 4}), R.Top + (R.Bottom - R.Top + 1));
            end;
          end;
        end;
      end;
      ACanvas.Pen.Style := SavePen;
      ACanvas.Pen.Color := SavePenCol;
    end;
  finally
    dec(R.Bottom);
  end;
end;

{.$HINTS OFF}
procedure TElTreeView.DoRedrawItemTree(ACanvas : TCanvas; Item: TElTreeItem; ItemRect, SurfRect: TRect);
var
  SText: TElFString;
{$ifdef CLX_USED}
  AForm : TCustomForm;
{$endif}
  R, R1,
{$ifdef ELTREE_USE_STYLES}
  R2,
{$endif}
  R3: TRect;
{$IFNDEF PaintBackground}
  BgRect,
  BgRect1,
  BgRect2,
  BgRect4    : TRect;
{$ENDIF}
  AL, VAL    : integer;
{$ifdef ELTREE_USE_STYLES}
  W,
  H     : integer;
  CurStyle : TElCellStyle;
{$endif}
  xxx      : TFontStyles;
{$IFNDEF PaintBackground}
  Blend    : boolean;
{$ENDIF}
  TransBk  : boolean;
  OwnBk    : boolean;
  xOffs    : integer;
  HelperBmp: TBitmap;
{$IFDEF HAS_HTML_RENDER}
  FData    : TElHTMLData;
{$ENDIF}
  ANode    : boolean;
  {$ifdef HAS_HTML_RENDER}
  AdjColor : TColor;
  {$endif}
begin
  OwnBk := FOverColors;
  TransBk := false;
  HelperBmp := TBitmap.Create;
{$IFNDEF PaintBackground}
  Blend := false;
{$ENDIF}
  SText := Item.Text;
  xOffs := 0;

  with FOwner do
  begin
    if FShowHeader then
    begin
{$IFNDEF LITE}
      if FHeader.Sections[FMainTreeCol].Locked then
         xOffs := - FHPos;
{$ENDIF}
      R := Rect(FHeader.Sections[FMainTreeCol].Left - FHPos - xOffs, ItemRect.Top, FHeader.Sections[FOwner.FMainTreeCol].Right - FHPos  - xOffs, ItemRect.Bottom);

      if FVLines and ((not FBarStyle) or FBSVLines) then Dec(R.Right, FDivLineWidth);
    end
    else
      R := Rect(SurfRect.Left - FHPos - xOffs, ItemRect.Top, SurfRect.Right - FHPos - xOffs, ItemRect.Bottom);
    //if not FHLines then
         //inc(R.Bottom, FDivLineWidth);
{$ifdef ELTREE_USE_STYLES}
    if (Item.FBoolData1 and ibfUseStyles) = ibfUseStyles then
      if (FOwner.VirtualityLevel <> vlNone) then
      begin
        if FOwner.ShowColumns then
          al := MainTreeColumn
        else
          al := -1;
        FOwner.TriggerVirtualStyleNeeded(Item, al, VirtStyle);
      end;
{$endif}
{$ifdef ELTREE_USE_STYLES}
    if (Item.FBoolData1 and ibfUseStyles) = ibfUseStyles then
    begin
      if Item.FStaticData <> nil then
      begin
        CurStyle := Item.MainStyle;
      end
      else
        CurStyle := VirtStyle;

      if FOverColors or CurStyle.FOwnerProps then
      begin
        ACanvas.Brush.Color := FCurBkColor;
        ACanvas.Font.Color := FCurTextColor;
        if FOverColors then OwnBk := true;
      end
      else
      begin
        ACanvas.Brush.Color := CurStyle.FCellBkColor;
        {if FOverColors then
          OwnBk := false
        else       }
          OwnBk := CurStyle.UseBkColor;
      end;
{$IFNDEF PaintBackground}
      Blend := true;
{$ENDIF}
    end
    else
{$endif}
    begin
      ACanvas.Brush.Color := FCurBkColor;
      ACanvas.Font.Color := FCurTextColor;
    end;
    if Item.Selected and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) and (((FSelectColumn = FMainTreeCol) and (FShowHeader)) or FRowSelect) then
    begin
      if ({((GetParentForm(self) <> nil) and (GetParentForm(self).ActiveControl = self))} FHasFocus or (not FHideSelect)) then
        ACanvas.Brush.Color := FFocusedSelectColor
      else
        ACanvas.Brush.Color := FHideSelectColor;
      OwnBk := true;
{$IFNDEF PaintBackground}
      Blend := true;
{$ENDIF}
    end;
    if (Item = FDropTrg) and (FDragTrgDrawMode in [ColorRect, SelColorRect])
       and (((FSelectColumn = FMainTreeCol) and (FShowHeader)) or FRowSelect) then
    begin
      case FDragTrgDrawMode of
        ColorRect:
          begin
            if FDropAcc then
              ACanvas.Brush.Color := FOwner.FDragRectAcceptColor
            else
              ACanvas.Brush.Color := FOwner.FDragRectDenyColor;
          end;
        SelColorRect:
          ACanvas.Brush.Color := clHighlight;
      end; // case
{$IFNDEF PaintBackground}
      Blend := true;
{$ENDIF}
      OwnBk := true;
    end;
    // now fill the background
    //if (not FHLines) or FBarStyle then
    //inc(R.Bottom);

{$IFNDEF LITE}
{$IFNDEF PaintBackground}
    if (BackgroundType <> bgtColorFill) and ((Blend and (not FNoBlendSelected)) or (not Blend)) then
    begin
      BgRect4 := R;
      BgRect := ClientRect;
      BgRect1 := ClientRect;
      BgRect2 := ItemRect;
      OffsetRect(BgRect2, -BgRect2.Left, -BgRect2.Top);

      if BackgroundType <> bgtCenterBitmap then
        ExtDrawBkgnd(ACanvas.Handle, Handle, BgRect1, BgRect, BgRect2, BgRect4, false, ACanvas.Brush.Color, ACanvas.Brush.Color, Blend, FTmpBmp, bgtCenterBitmap)
      else
        ExtDrawBkgnd(ACanvas.Handle, Handle, BgRect1, BgRect, BgRect2, BgRect4, false, ACanvas.Brush.Color, ACanvas.Brush.Color, Blend, Background, BackgroundType);
    end
    else
{$ENDIF}

{$ENDIF}
    {$ifndef CLX_USED}
    if (InSizeMove) {$ifndef LITE}and (BackgroundType = bgtColorFill){$endif}{$ifdef ELPACK_COMPLETE}{$ifndef CLX_USED}and ((csDesigning in ComponentState) or (ImageForm = nil)){$endif}{$endif} then
    begin
      if not OwnBk then
        ACanvas.Brush.Color := BkColor;
      ACanvas.FillRect(R);
    end
    else
    {$endif}
    if OwnBk then
    begin
      ACanvas.FillRect(R);
    end;
    //if (not FHLines) or FBarStyle then dec(R.Bottom);

    if Item.IndentAdjust <> 0 then
    begin
      if RightAlignedTree then
        dec(R.Right, Item.IndentAdjust)
      else
        inc(R.Left, Item.IndentAdjust);
    end;

    if RightAlignedTree then
    begin
      if (FShowRoot and FShowLines) or (FShowButtons and (FShowRootButtons )) then
        dec(R.Right, ItemExt)
      else
        dec(R.Right, ItemExt div 5);
    end else
    begin
      if (FShowRoot and FShowLines) or (FShowButtons and (FShowRootButtons )) then
        Inc(R.Left, ItemExt)
      else
        Inc(R.Left, ItemExt div 5);
    end;

    // draw tree and images
    if (FShowLines) and (not Item.SuppressLines) then
      DrawItemLines(ACanvas, Item, r, ItemRect)
    else
    begin
      if RightAlignedTree then
        dec(R.Right, (Item.Level - 1) * ItemExt)
      else
        inc(R.Left, (Item.Level - 1) * ItemExt);
    end;

    if Item.Ancestor.SuppressButtons then
    begin //Eyal
      if RightAlignedTree then
      begin
        inc(R.Right, ItemExt);
      end else
      begin
        dec(R.Left, ItemExt);
      end;
    end;

    ANode := ((Item.HasVisibleChildren) or Item.ForceButtons);
    if FShowButtons  and (not Item.SuppressButtons) and
       (FShowRootButtons or (Item.FParent <> Items.FRoot)) and
       (ANode or ShowLeafButton) then
      DrawButtons(ACanvas, Item, ANode, HelperBmp, R, ItemRect);

    if RightAlignedTree then
      dec(R.Right, ItemExt)
    else
      inc(R.Left, ItemExt);

    if FShowCheckBoxes then
      DrawCheckBoxes(ACanvas, Item, HelperBmp, R, ItemRect);

    Item.FBoolData1 := (Item.FBoolData1 and not ibfImageDrawn) and not ibfImageDrawn2;

    if (FShowImages) then
      DrawImages(ACanvas, Item, HelperBmp, R, ItemRect);

{$WARNINGS off}
    {$ifdef ELTREE_USE_STYLES}
    if Item.FStaticData <> nil then
      CurStyle := Item.MainStyle
    else
      CurStyle := VirtStyle;
    {$endif}
    if (FODFollowCol and (FHeader.Sections.Count > 0) and
        (FHeader.Sections[FMainTreeCol].Style = ElhsOwnerDraw)) or
       ((SText = FODMask) and (not (FODFollowCol)))
{$ifdef ELTREE_USE_STYLES}
      or (((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) and (CurStyle.FStyle = elhsOwnerDraw))
{$endif}
      then
    begin
      DoItemDraw(Item, ACanvas, R, FMainTreeCol);
      if RightAlignedTree then
      begin
        dec(R.Right, ItemExt div 3);
        R1 := R;
        Item.FTextLeft := R1.Left + FHPos - xOffs;
        Item.FTextRight := R1.Right + FHPos - xOffs;
        with FOwner do
          if not (ShowButtons or ShowLines or ShowImages or ShowCheckBoxes) then
            Inc(Item.FTextRight, ItemExt div 3);
        inc(R1.Right);
      end else
      begin
        inc(R.Left, ItemExt div 3);
        R1 := R;
        Item.FTextLeft := R1.Left + FHPos + xOffs;
        Item.FTextRight := R1.Right + FHPos + xOffs;

        with FOwner do
          if not (ShowButtons or ShowLines or ShowImages or ShowCheckBoxes) then
            Dec(Item.FTextLeft, ItemExt div 3);

        dec(R1.Left);
      end;
    end
    else
    begin
      // make the empty space, that relates to the text
      if RightAlignedTree then
        dec(R.Right, ItemExt div 3)
      else
        inc(R.Left, ItemExt div 3);

{$ifdef ELTREE_USE_STYLES}
      (*
      if Item.UseStyles and (CurStyle.Control <> nil) then
      begin
        CurStyle.Control.Paint(ACanvas, R);
      end
      else
      *)
      if (Item.UseStyles and (CurStyle.FStyle = elhsText)) or
      ((Item.FBoolData1 and ibfUseStyles) <> ibfUseStyles) then
{$endif}
      begin
        xxx := [];
{$ifdef ELTREE_USE_STYLES}
        if ((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
        begin
          if FOverColors or CurStyle.FOwnerProps then
          begin
            ACanvas.Font.Color := FCurTextColor;
            ACanvas.Brush.Color := FCurBkColor;
            TransBk := false;
          end
          else
          begin
            ACanvas.Brush.Color := CurStyle.FTextBkColor;
            ACanvas.Font.Color := CurStyle.FTextColor;
            if not CurStyle.UseBkColor then
              TransBk := true;
{$IFNDEF PaintBackground}
            Blend := true;
{$ENDIF}
          end;
          if not CurStyle.FOwnerProps then
          with CurStyle do
          begin
            //if ACanvas.Font.Name <> FontName then
            ACanvas.Font.Name := FontName;
            xxx := FontStyles; //if ACanvas.Font.Style <> FontStyles then
            //if ACanvas.Font.Size <> FontSize then
            ACanvas.Font.Size := FontSize;
            AL := CurStyle.FTextFlags;

            {$ifndef CLX_USED}
            {$IFNDEF LITE}
            VAL:= AL and (DT_TOP or DT_BOTTOM or DT_VCENTER);
            {$ENDIF}
            {$else}
            VAL := AL and (Integer(AlignmentFlags_AlignTop) or Integer(AlignmentFlags_AlignBottom) or Integer(AlignmentFlags_AlignVCenter));
            {$endif}

          end
          else
          begin
            if ACanvas.Font.Name <> Font.Name then
              ACanvas.Font.Name := Font.Name;
            if ((Item.FBoolData1 and ibfParentStyle) = ibfParentStyle) then
              xxx := Font.Style
            else
            begin
              if stsBold in Item.FState then Include(xxx, fsBold);
              if stsItalic in Item.FState then Include(xxx, fsItalic);
              if stsUnderlined in Item.FState then Include(xxx, fsUnderline);
              if stsStrikeOut in Item.FState then Include(xxx, fsStrikeout);
            end;
            //if ACanvas.Font.Size <> Font.Size then
              ACanvas.Font.Size := Font.Size;

            {$ifndef CLX_USED}
            Al := DT_LEFT;
            if Assigned(FHeader) and (FShowHeader) and (FHeader.Sections.Count > FMainTreeCol) then
              case FHeader.Sections[FMainTreeCol].Alignment of
                hsaCenter: Al := DT_CENTER;
                hsaRight: Al := DT_RIGHT;
              end; // case

            AL := DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}AL or {$IFNDEF LITE}MultiLineEllipseFlags[Item.Multiline]{$ELSE}DT_END_ELLIPSIS{$ENDIF};
            {$IFNDEF LITE}
            VAL := 0;
            {$ENDIF}
            {$else}
            Al := Integer(AlignmentFlags_AlignLeft);
            if Assigned(FHeader) and (FShowHeader) and (FHeader.Sections.Count > FMainTreeCol) then
              case FHeader.Sections[FMainTreeCol].Alignment of
                hsaCenter: Al := Integer(AlignmentFlags_AlignHCenter);
                hsaRight: Al := Integer(AlignmentFlags_AlignRight);
              end; // case

            AL := MultiLineFlags[Item.Multiline] or AL or MultiLineEllipseFlags[Item.Multiline];
            VAL := Integer(AlignmentFlags_AlignLeft);
            {$endif}
          end;
        end
        else
{$endif ELTREE_USE_STYLES}
        begin
          ACanvas.Font.Color := FCurTextColor;
          ACanvas.Brush.Color := FCurBkColor;
          if ACanvas.Font.Name <> Font.Name then ACanvas.Font.Name := Font.Name;
          if ((Item.FBoolData1 and ibfParentStyle) = ibfParentStyle) then
            xxx := Font.Style
          else
          begin
            if stsBold in Item.FState then Include(xxx, fsBold);
            if stsItalic in Item.FState then Include(xxx, fsItalic);
            if stsUnderlined in Item.FState then Include(xxx, fsUnderline);
            if stsStrikeOut in Item.FState then Include(xxx, fsStrikeout);
          end;
          if not ((Item.FBoolData1 and ibfParentColors) = ibfParentColors) then
          begin
            ACanvas.Font.Color := Item.Color;
            if ((Item.FBoolData1 and ibfUseBkColor) = ibfUseBkColor) then
              ACanvas.Brush.Color := Item.BkColor
            else
               TransBk := true;
            OwnBk := (not FOverColors) or FRowOvColors;
          end;
          if ACanvas.Font.Size <> Font.Size then
            ACanvas.Font.Size := Font.Size;

          {$ifndef CLX_USED}
          if FRightAlignedTree then
            Al := DT_RIGHT
          else
            Al := DT_LEFT;

          AL := DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}AL or {$IFNDEF LITE}MultiLineEllipseFlags[Item.Multiline]{$ELSE}DT_END_ELLIPSIS{$ENDIF};
          if RightAlignedText then AL := AL or DT_RTLREADING;
          {$IFNDEF LITE}
          VAL := 0;
          {$ENDIF}
          {$else}
          if FRightAlignedTree then
            AL := Integer(AlignmentFlags_AlignRight)
          else
            AL := Integer(AlignmentFlags_AlignLeft);

          AL := MultiLineFlags[Item.Multiline] or AL or MultiLineEllipseFlags[Item.Multiline];
          VAL := Integer(AlignmentFlags_AlignLeft);
          {$endif}
        end;
        if (FTracking and (Item = FTrackItem)) and FUnderlineTracked then Include(xxx, fsUnderline);
        ACanvas.Font.Style := xxx;
        if Item.Selected and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) and ((FSelectColumn = -1) or (FSelectColumn = FMainTreeCol) or FRowSelect or (not FShowHeader)) then
        begin
          {$ifdef CLX_USED}
          AForm := GetParentForm(Self);
          FHasFocus := ((AForm <> nil) and (AForm.ActiveControl = Self));
          {$endif}
          if FHasFocus or (not FHideSelect) then
          begin
            ACanvas.Brush.Color := FFocusedSelectColor;
            ACanvas.Font.Color := FFocusedSelectTextColor;
            TransBk := false;
          end else
          begin
            ACanvas.Brush.Color := FHideSelectColor;
            ACanvas.Font.Color := FHideSelectTextColor;
          end;
{$IFNDEF PaintBackground}
          Blend := true;
{$ENDIF}
          OwnBk := true;
        end;
        if (FTracking and (Item = FTrackItem)) then
           if Item.Selected and ((not FBarStyle) and (Item.FBorderStyle = ibsNone)) and
             (FRowSelect or (FSelectColumn = -1) or
             (FShowHeader and (FMainTreeCol = FSelectColumn))) then
             begin
               {$ifdef CLX_USED}
               AForm := GetParentForm(Self);
               FHasFocus := ((AForm <> nil) and (AForm.ActiveControl = Self));
               {$endif}
               if FHasFocus or (not FHideSelect) then
               begin
                 ACanvas.Brush.Color := FFocusedSelectColor;
                 ACanvas.Font.Color := FFocusedSelectTextColor;
               end
               else
               begin
                 ACanvas.Brush.Color := FHideSelectColor;
                 ACanvas.Font.Color := FHideSelectTextColor;
               end;
             end
           else
              ACanvas.Font.Color := FOwner.TrackColor;

        if (Item = FDropTrg)
           and (FDragTrgDrawMode in [ColorRect, SelColorRect])
           and ((FSelectColumn = -1)
             or (FSelectColumn = FMainTreeCol)
             or FRowSelect
             or (not FShowHeader)) then
        begin
          case FDragTrgDrawMode of
            ColorRect:
              begin
                if FDropAcc then
                  ACanvas.Brush.Color := FDragRectAcceptColor
                else
                  ACanvas.Brush.Color := FDragRectDenyColor;
                ACanvas.Font.Color := clBtnText;
              end;
            SelColorRect:
              begin
                ACanvas.Brush.Color := clHighlight;
                ACanvas.Font.Color := clHighlightText;
              end;
          end; // case
{$IFNDEF PaintBackground}
          Blend := true;
{$ENDIF}
          OwnBk := true;
        end;

        SetRectEmpty(R3);
        ACanvas.Font.Charset := Font.Charset;
{$IFDEF HAS_HTML_RENDER}
        if Item.IsHTML and (Copy(SText, 1, 6) = '<html>') then
        begin
          R3.Left := 0;
          R3.Top := 0;
          if VirtualityLevel = vlNone then
          begin
            FData := Item.FHTMLData;
            if FData = nil then
            begin
              R3.Right := 0;
              R3.Bottom := 0;
            end else
            begin
              R3.Right := FData.TextSize.cx;
              R3.Bottom := FData.TextSize.cy;
            end;
          end
          else
          begin
            with FRender do
            begin
              Data.DefaultStyle := ACanvas.Font.Style;
              Data.DefaultFont  := ACanvas.Font.Name;
              Data.DefaultColor := ACanvas.Font.Color;
              Data.DefaultHeight:= ACanvas.Font.Height;
              Data.Charset      := ACanvas.Font.Charset;

              PrepareText(SText, 0, false);
              R3.Right := Data.TextSize.cx;
              R3.Bottom := Data.TextSize.cy;
            end;
          end;
        end
        else
{$ENDIF}
        {$ifndef CLX_USED}
        {$ifdef ELPACK_UNICODE}
          ElVCLUtils.DrawTextW(ACanvas.Handle, PWideChar(SText), Length(SText), R3, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF} DT_LEFT or DT_CALCRECT);
        {$else}
          DrawText(ACanvas.Handle, PChar(SText), Length(SText), R3, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF} DT_LEFT or DT_CALCRECT);
        {$endif}
        {$else}
          ACanvas.TextExtent(SText, R3, MultiLineFlags[Item.Multiline] or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignTop));
        {$endif}
        //InflateRect(R, 2, 0);

        if RightAlignedTree then
        begin
          R1 := R;
          inc(R1.Right, 2);
          R1.Left := Max(R1.Left, R1.Right - ((R3.Right - R3.Left + 1) + 4) + FDivLineWidth);
        end else
        begin
          R1 := R;
          dec(R1.Left);
          R1.Right := Min(R1.Left + (R3.Right - R3.Left + 1) + 4 - FDivLineWidth, R.Right);
        end;
        //InflateRect(R1, -1, -1);
        if R1.Left < R1.Right then
        begin
{$IFNDEF LITE}
{$IFNDEF PaintBackground}
          if (BackgroundType <> bgtColorFill) and ((Blend and (not FNoBlendSelected)) or (not Blend)) then
          begin
            BgRect4 := R1;
            BgRect := ClientRect;
            BgRect1 := ClientRect;

            BgRect2 := ItemRect;
            OffsetRect(BgRect2, -BgRect2.Left, -BgRect2.Top);
            if BackgroundType <> bgtCenterBitmap then
            begin
              ExtDrawBkgnd(ACanvas.Handle, Handle, BgRect1, BgRect, BgRect2, BgRect4, false, ACanvas.Brush.Color, ACanvas.Brush.Color, Blend, FTmpBmp, bgtCenterBitmap);
            end else
            begin
              ExtDrawBkgnd(ACanvas.Handle, Handle, BgRect1, BgRect, BgRect2, BgRect4, false, ACanvas.Brush.Color, ACanvas.Brush.Color, Blend, Background, BackgroundType);
            end;
          end
          else
{$ENDIF}
{$ENDIF}
          if OwnBk and (not TransBk) then
          begin
            //ACanvas.Pen.Style := psClear;
            ACanvas.FillRect(R1);
            {$ifdef HAS_HTML_RENDER}
            AdjColor := ACanvas.Brush.Color;
            {$endif}
          end
          {$ifdef HAS_HTML_RENDER}
          else
            AdjColor := clNone
          {$endif}
          ;

          if RightAlignedTree then
            dec(R1.Right)
          else
            inc(R1.Left);
          ACanvas.Brush.Style := bsClear;
          {$IFNDEF LITE}
          if (Item.Multiline
          {$IFDEF HAS_HTML_RENDER}or Item.IsHTML{$ENDIF})
          {$ifndef CLX_USED}
          and (VAL = 0)
          {$else}
          and (VAL = Integer(AlignmentFlags_AlignLeft))
          {$endif}
          and (LineHeight < (R1.Bottom - R1.Top)) then
          {$ifndef CLX_USED}
              VAL := DT_TOP
          {$else}
              VAL := Integer(AlignmentFlags_AlignTop)
          {$endif}
          else
          {$ENDIF}
          {$ifndef CLX_USED}
            VAL := DT_VCENTER;
          {$else}
            VAL := Integer(AlignmentFlags_AlignVCenter);
          {$endif}

{$IFDEF HAS_HTML_RENDER}
          if Item.IsHTML and (Copy(SText, 1, 6) = '<html>') then
          begin
            if VirtualityLevel = vlNone then
              FData := Item.FHTMLData
            else
              FData := FRender.Data;

            if FData <> nil then
            begin
              if RightAlignedText then
              begin
                R3.Left := R1.Right - Min(R1.Right - R.Left, R3.Right - R3.Left);
                R3.Right := R1.Right;
              end else
              begin
                R3.Right := Min(R1.Right - R.Left, R3.Right - R3.Left) + R1.Left;
                R3.Left := R1.Left;
              end;
              R3.Bottom := Min(R3.Bottom, R3.Top + (R1.Bottom - R1.Top));

              {$ifndef CLX_USED}
              if (VAL and DT_BOTTOM) = DT_BOTTOM then
              {$else}
              if (VAL and Integer(AlignmentFlags_AlignBottom)) = Integer(AlignmentFlags_AlignBottom) then
              {$endif}
                OffsetRect(R3, 0, R1.Top + (R1.Bottom - r1.Top) - (R3.Bottom - R3.Top))
              else
              {$ifndef CLX_USED}
              if (VAL and DT_VCENTER) = DT_VCENTER then
              {$else}
              if (VAL and Integer(AlignmentFlags_AlignVCenter)) = Integer(AlignmentFlags_AlignVCenter) then
              {$endif}
                OffsetRect(R3, 0, R1.Top + (((R1.Bottom - r1.Top) - (R3.Bottom - R3.Top)) shr 1) - 1)
              else
                OffsetRect(R3, 0, R1.Top);

              with FRender do
              begin
                //PrepareText(SText, 0, false);
                SetData(FData);
                DrawTextEx(ACanvas, Point(0, 0), R3, true, ACanvas.Font.Color, FData.DefaultBgColor, FData.HighlightColor, FData.HighlightBgColor, AdjColor);
                if VirtualityLevel = vlNone then SetData(nil);
              end;
            end;
          end
          else
{$ENDIF}
          begin
            if RightAlignedText then
            begin
              R3.Left := R1.Right - Min(R1.Right - R.Left, R3.Right - R3.Left);
              R3.Right := R1.Right;
            end else
            begin
              R3.Right := Min(R1.Right - R.Left, R3.Right - R3.Left) + R1.Left;
              R3.Left := R1.Left;
            end;
            R3.Bottom := Min(R3.Bottom, R3.Top + (R1.Bottom - R1.Top));

            {$ifndef CLX_USED}
            if (VAL and DT_BOTTOM) = DT_BOTTOM then
            {$else}
            if (VAL and Integer(AlignmentFlags_AlignBottom)) = Integer(AlignmentFlags_AlignBottom) then
            {$endif}
              OffsetRect(R3, 0, R1.Top + (R1.Bottom - r1.Top) - (R3.Bottom - R3.Top))
            else
            {$ifndef CLX_USED}
            if (VAL and DT_VCENTER) = DT_VCENTER then
            {$else}
            if (VAL and Integer(AlignmentFlags_AlignVCenter)) = Integer(AlignmentFlags_AlignVCenter) then
            {$endif}
              OffsetRect(R3, 0, R1.Top + (((R1.Bottom - r1.Top) - (R3.Bottom - R3.Top)) shr 1) - 1)
            else
              OffsetRect(R3, 0, R1.Top);

            {$ifndef CLX_USED}
            {$ifdef ELPACK_UNICODE}
              ElVCLUtils.DrawTextW(ACanvas.Handle, PWideChar(SText), Length(SText), R3, VAL or DT_NOPREFIX or AL or DT_EXTERNALLEADING);
            {$else}
              DrawText(ACanvas.Handle, PChar(SText), Length(SText), R3, VAL or DT_NOPREFIX or AL or DT_EXTERNALLEADING);
            {$endif}
            {$else}
            ACanvas.TextRect(R3, R3.Left, R3.Top, SText, VAL or AL);
            {$endif}
          end;
          if RightAlignedTree then
            inc(R1.Right)
          else
            dec(R1.Left);

          Item.FTextLeft := R1.Left + FHPos + xOffs -1;
          Item.FTextRight := R1.Right + FHPos + xOffs + 1;

          if RightAlignedTree then
          begin
            with FOwner do
              if not (ShowButtons or ShowLines or ShowImages or ShowCheckBoxes) then
                Inc(Item.FTextRight, ItemExt div 3);
          end else
          begin
            with FOwner do
              if not (ShowButtons or ShowLines or ShowImages or ShowCheckBoxes) then
                Dec(Item.FTextLeft, ItemExt div 3);
          end;
        end
        else
        begin
          Item.FTextLeft := R1.Left + FHPos + xOffs -1;
          Item.FTextRight := Item.FTextLeft;
        end;
        ACanvas.Brush.Style := bsSolid;
      end
{$ifdef ELTREE_USE_STYLES}
      else
        if Item.UseStyles and (CurStyle.FStyle = elhsPictureOnly) and Assigned(CurStyle.FPicture) then
        begin
          h := CurStyle.FPicture.Height;
          w := CurStyle.FPicture.Width;

          if RightAlignedTree then
          begin
            R2 := Rect(((R.Left + R.Right) shr 1)-(w shr 1), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1), Min(R.Right, ((R.Left + R.Right) shr 1)-(w shr 1) + w), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1) + h);
            // R2 := Rect(Max(R.Left, R.Right - w), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1), R.Right, R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1) + h);
            Item.FTextLeft := R.Left + FHPos + xOffs;
            Item.FTextRight := R.Right + FHPos + xOffs;
  
            if CurStyle.FPicture.Transparent then
            begin
            //R2 := Rect(0, 0, Min(CurStyle.FPicture.Width, R.Right - R.Left), Min(CurStyle.FPicture.Height, ((R.Bottom-R.Top+1) shr 1)- (h shr 1) + h));
            //DrawTransparentBitmapEx(ACanvas.Handle, CurStyle.FPicture.Handle, R.Left, R.Top + ((R.Bottom-R.Top + 1) shr 1)-(h shr 1), R2, CurStyle.FPicture.TransparentColor)
              {$ifdef CLX_USED}
              ACanvas.Start;
              CurStyle.FPicture.Canvas.Start;
              {$endif}
              DrawTransparentBitmapEx(ACanvas.Handle, CurStyle.FPicture, R2.Left, R2.Top, Rect(0, 0, R.Right - min(w, Max(R.Left, R.Right - w + 1) + 1), h), CurStyle.FPicture.TransparentColor);
              {$ifdef CLX_USED}
              ACanvas.Stop;
              CurStyle.FPicture.Canvas.Stop;
              {$endif}
            end
            else
            begin
              {$ifdef CLX_USED}
              ACanvas.Start;
              CurStyle.FPicture.Canvas.Start;
              {$endif}
              ACanvas.CopyRect(R2, CurStyle.FPicture.Canvas, Rect(0, 0, R.Right - min(w, Max(R.Left, R.Right - w + 1) + 1), h));
              {$ifdef CLX_USED}
              ACanvas.Stop;
              CurStyle.FPicture.Canvas.Stop;
              {$endif}
            end;
          end
          else
          begin
            R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1), Min(R.Right, R.Left + w), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1) + h);
            Item.FTextLeft := R.Left + FHPos + xOffs;
            Item.FTextRight := R.Right + FHPos + xOffs;
            if CurStyle.FPicture.Transparent then
            begin
            //R2 := Rect(0, 0, Min(CurStyle.FPicture.Width, R.Right - R.Left), Min(CurStyle.FPicture.Height, ((R.Bottom-R.Top+1) shr 1)- (h shr 1) + h));
            //DrawTransparentBitmapEx(ACanvas.Handle, CurStyle.FPicture.Handle, R.Left, R.Top + ((R.Bottom-R.Top + 1) shr 1)-(h shr 1), R2, CurStyle.FPicture.TransparentColor)
              {$ifdef CLX_USED}
              ACanvas.Start;
              CurStyle.FPicture.Canvas.Start;
              {$endif}
              DrawTransparentBitmapEx(ACanvas.Handle, CurStyle.FPicture, R2.Left, R2.Top, Rect(0, 0, min(w, Min(R.Right, R.Left + w - 1) - R.Left + 1), h), CurStyle.FPicture.TransparentColor);
              {$ifdef CLX_USED}
              ACanvas.Stop;
              CurStyle.FPicture.Canvas.Stop;
              {$endif}
            end
            else
            begin
              {$ifdef CLX_USED}
              ACanvas.Start;
              CurStyle.FPicture.Canvas.Start;
              {$endif}
              ACanvas.CopyRect(R2, CurStyle.FPicture.Canvas, Rect(0, 0, min(w, Min(R.Right, R.Left + w - 1) - R.Left + 1), h));
              {$ifdef CLX_USED}
              ACanvas.Stop;
              CurStyle.FPicture.Canvas.Stop;
              {$endif}
            end;
          end;
        end;
{$endif}
    end; // if ... else
  end;
  HelperBmp.Free;
end;

procedure TElTreeView.DoRedrawItem;
var
  TSI: TElTreeItem;
{$IFNDEF PaintBackground}
  BgRect,
  BgRect1,
  BgRect2,
  BgRect4    : TRect;
{$ENDIF}
  R, R1,
  RD, R2,
  R3: TRect;
{$IFNDEF PaintBackground}
  Blend    : boolean;
{$ENDIF}
  SText: TElFString;
  i, j, k,
{$ifdef ELTREE_USE_STYLES}
  h,
  w,
{$endif}
  al,
  val,
  hi,
  tw: integer;
  dfr : boolean;
  HS: TElHeaderSection;
  PrevPenColor : TColor;
  FTransBk     : boolean;
  flv: integer;
{$ifdef ELTREE_USE_STYLES}
  CurStyle: TElCellStyle;
{$endif}
{$ifdef CLX_USED}
  AForm : TCustomForm;
{$endif}
{$IFNDEF LITE}
  OwnBk : boolean;
{$ENDIF}
  xOffs : integer;
  aLockBmp : TBitmap;
{$IFDEF HAS_HTML_RENDER}
  FData    : TElHTMLData;
{$ENDIF}
{$ifdef HAS_HTML_RENDER}
  AdjColor : TColor;
{$endif}
begin
  aLockBmp := nil;
  TSI      := Item;
  xOffs := 0;
  with FOwner do                   
  begin
    r1 := Rect(SurfRect.Left - FHPos, ItemRect.Top, SurfRect.Right - 1 - FHPos, ItemRect.Bottom);
    flv := FDivLineWidth;
    if (not VerticalLines) or (FBarStyle and (not FBSVLines)) or
        (Item.BorderStyle <> ibsNone) then
      flv := 0;
    FOverColors := false;
    FRowOvColors:= false;         
    {$ifdef CLX_USED}
    AForm := GetParentForm(Self);
    FHasFocus := ((AForm <> nil) and (AForm.ActiveControl = Self));
    {$endif}
    if (not Item.ParentColors) and (Item.UseBkColor) then
    begin
      ACanvas.Brush.Color := Item.RowBkColor;
      FCurBkColor := Item.RowBkColor;
      FOverColors := true;
      FRowOvColors:= true;
    end;

    if TSI.Selected and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) and FRowSelect then
    begin
      if FHasFocus or (not FHideSelect) then
      begin
        ACanvas.Brush.Color := FFocusedSelectColor;
        ACanvas.Font.Color := FFocusedSelectTextColor;
        FCurBkColor := FFocusedSelectColor;
        FCurTextColor := clHighlightText;
      end else
      begin
        FCurBkColor := FHideSelectColor;
        ACanvas.Brush.Color := FHideSelectColor;
        ACanvas.Font.Color := FHideSelectTextColor;
        FCurTextColor := FHideSelectTextColor;
      end;
      FOverColors := true;
{$IFNDEF PaintBackground}
      Blend := true;
{$ENDIF}
    end
    else
    if StripedItems then
    begin
      if (FVisible.IndexOf(Item) + TopIndex) mod 2 = 0 then
        FCurBkColor := FStripedEvenColor
      else
        FCurBkColor := FStripedOddColor;
      FOverColors := true;
    end;
    if FRowSelect then
    begin
      if (TSI = FDropTrg) and (FDragTrgDrawMode in [ColorRect, SelColorRect]) then
      begin
        case FDragTrgDrawMode of
          ColorRect:
            begin
              if FDropAcc then
                ACanvas.Brush.Color := FOwner.FDragRectAcceptColor
              else
                ACanvas.Brush.Color := FOwner.FDragRectDenyColor;
              FCurBkColor := ACanvas.Brush.Color;
              ACanvas.Font.Color := clBtnText;
              FCurTextColor := clBtnText;
              FOverColors := true;
            end;
          SelColorRect:
            begin
              ACanvas.Brush.Color := clHighlight;
              ACanvas.Font.Color := clHighlightText;
              FCurBkColor := clHighlight;
              FCurTextColor := clHighlightText;
              FOverColors := true;
            end;
        end; // case
{$IFNDEF PaintBackground}
        Blend := true;
{$ENDIF}
      end;
    end;
    k := FHeader.Sections.Count;
    if FShowHeader then
    begin
{$IFNDEF LITE}
      if (FHeader.LockedSection <> nil) and (FHeader.LockedSection.Visible) then
      begin
        ALockBmp := TBitmap.Create;
        HS := FHeader.LockedSection;
        xOffs := 0; if HS.Locked then xOffs := - FHPos;
        R := Rect(HS.Left - FHPos - xOffs, ItemRect.Top, HS.Right - FHPos - flv - xOffs, ItemRect.Bottom);
        inc(R.Bottom, FDivLineWidth);
        ALockBmp.Width := R.Right - R.Left;
        ALockBmp.Height := R.Bottom - R.Top;
        {$ifndef CLX_USED}
        ALockBmp.Handle := CreateCompatibleBitmap(ACanvas.Handle, ALockBmp.Width, ALockBmp.Height);
        BitBlt(ALockBmp.Canvas.Handle, 0, 0, ALockBmp.Width, ALockBmp.Height, ACanvas.Handle, R.Left, R.Top, SRCCOPY);
        {$else}
        ALockBmp.Canvas.Start;
        ACanvas.Start;
        BitBlt(QPainter_device(ALockBmp.Canvas.Handle), 0, 0, QPainter_device(ACanvas.Handle), R.Left, R.Top, ALockBmp.Width, ALockBmp.Height, RasterOp_CopyROP, true);
        ALockBmp.Canvas.Stop;
        ACanvas.Stop;
        {$endif}
      end;
{$ENDIF}
      for j := 0 to k do
      begin
        FTransBk := false;
        if (j = k) then
        begin
{$IFNDEF LITE}
          if (FHeader.LockedSection = nil) then
             break
          else
             HS := FHeader.LockedSection;
{$ELSE}
 	        break;
{$ENDIF LITE}
        end
        else
        begin
          HS := FHeader.Sections[j];
{$IFNDEF LITE}
          if HS.Locked then Continue;
{$ENDIF LITE}
        end;
        if not HS.Visible then Continue;
        xOffs := 0;
{$IFNDEF LITE}
        if HS.Locked then xOffs := - FHPos;
{$ENDIF LITE}
        hi := HS.Index;
{$IFNDEF LITE}
        OwnBk := FOverColors;
{$ENDIF}
{$IFNDEF PaintBackground}
        Blend := false;
{$ENDIF}
        if HS.Visible then
        begin
{$IFNDEF LITE}
          if HS.Locked then
          begin
            R := Rect(HS.Left - FHPos - xOffs, ItemRect.Top, HS.Right - FHPos - flv - xOffs, ItemRect.Bottom);
            inc(R.Bottom, FDivLineWidth);
            {$ifndef CLX_USED}
            BitBlt(ACanvas.Handle, R.Left, R.Top, ALockBmp.Width, ALockBmp.Height, ALockBmp.Canvas.Handle, 0, 0, SRCCOPY);
            {$else}
            ALockBmp.Canvas.Start;
            ACanvas.Start;
            BitBlt(QPainter_device(ACanvas.Handle), R.Left, R.Top, QPainter_device(ALockBmp.Canvas.Handle), 0, 0, ALockBmp.Width, ALockBmp.Height, RasterOp_CopyROP, true);
            ALockBmp.Canvas.Stop;
            ACanvas.Stop;
            {$endif}
          end;
{$ENDIF LITE}
          if HS.Index = FMainTreeCol then
            DoRedrawItemTree(ACanvas, Item, ItemRect, ItemRect)
          else
          begin
          // check the proper section and subtext index
            i := hi;
            if VirtualityLevel = vlNone then
            begin
              if i > FMainTreeCol then
                dec(i);
              if i >= Item.ColumnText.Count then
                SText := ''
              else
                if HS.Password then
                  SText := '******'
                else
                  SText := Item.ColumnText[i];
            end
            else
            begin
              if HS.Password then
                SText := '******'
              else
                FOwner.TriggerVirtualTextNeeded(Item, i, SText);
            end;
{$ifdef ELTREE_USE_STYLES}
            if ((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
            begin
              if VirtualityLevel = vlNone then
              begin
                if (Item.FStaticData.FStyles = nil) or (i >= Item.FStaticData.FStyles.Count) or (HS.UseMainStyle) then
                   CurStyle := Item.MainStyle
                else
                   CurStyle := Item.FStaticData.FStyles[i];
              end
              else
              begin
                Fowner.TriggerVirtualStyleNeeded(Item, i, VirtStyle);
                CurStyle := VirtStyle;
              end;
              if FOverColors or CurStyle.FOwnerProps then
              begin
                ACanvas.Brush.Color := FCurBkColor;
                ACanvas.Font.Color := FCurTextColor;
              end
              else
              begin
                ACanvas.Brush.Color := CurStyle.FCellBkColor;
                if not (Item.Selected and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) and ((FSelectColumn = HS.Index) or FRowSelect)) then
                begin
                  if not CurStyle.UseBkColor then
                    FTransBk := true
                  else
{$IFNDEF LITE}
                    OwnBk := true
{$ENDIF}
                  ;  
                end;
              end;
            end
            else
{$endif}
            begin
              ACanvas.Brush.Color := FCurBkColor;
              ACanvas.Font.Color := FCurTextColor;
            end;
            if Item.Selected and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) and ((FSelectColumn = HS.Index) or FRowSelect) then
            begin
              if ({((GetParentForm(self) <> nil) and
                  (GetParentForm(self).ActiveControl = self))}FHasFocus or
                 (not FHideSelect)) then
                 ACanvas.Brush.Color := FFocusedSelectColor
              else
                 ACanvas.Brush.Color := FHideSelectColor;
{$IFNDEF PaintBackground}
              Blend := true;
{$ENDIF}
{$IFNDEF LITE}
              OwnBk := true;
{$ENDIF}
            end;
            if (TSI = FDropTrg) and (FDragTrgDrawMode in [ColorRect, SelColorRect])
               and (((not FRowSelect) and (FSelectColumn = hi)) or FRowSelect) then
            begin
{$IFNDEF LITE}
              OwnBk := true;
{$ENDIF}
              case FDragTrgDrawMode of
                ColorRect:
                  begin
                    if FDropAcc then
                      ACanvas.Brush.Color := FDragRectAcceptColor
                    else
                      ACanvas.Brush.Color := FDragRectDenyColor;
                  end;
                SelColorRect:
                  ACanvas.Brush.Color := clHighlight;
              end; // case
            end;
            // now fill the background

            R := Rect(HS.Left - FHPos - xOffs, ItemRect.Top, HS.Right - FHPos - flv - xOffs, ItemRect.Bottom);
            //if (not FHLines) or FBarStyle then
            {$ifndef NEW_GETITEMRECT}
            inc(R.Bottom, FDivLineWidth);
            {$endif}

{$IFNDEF LITE}
{$IFNDEF PaintBackground}
            if (BackgroundType <> bgtColorFill) and ((Blend and (not FNoBlendSelected)) or (not Blend)) then
            begin
              BgRect4 := R;
              BgRect := ClientRect;
              BgRect1 := ClientRect;

              BgRect2 := ItemRect;
              OffsetRect(BgRect2, -BgRect2.Left, -BgRect2.Top);
              if BackgroundType <> bgtCenterBitmap then
                ExtDrawBkgnd(ACanvas.Handle, Handle, BgRect1, BgRect, BgRect2, BgRect4, false, ACanvas.Brush.Color, ACanvas.Brush.Color, Blend, FTmpBmp, bgtCenterBitmap)
              else
                ExtDrawBkgnd(ACanvas.Handle, Handle, BgRect1, BgRect, BgRect2, BgRect4, false, ACanvas.Brush.Color, ACanvas.Brush.Color, Blend, Background, BackgroundType);
            end else
{$ENDIF}
            {$ifndef CLX_USED}
            if (InSizeMove) and (BackgroundType = bgtColorFill) {$ifdef ELPACK_COMPLETE}{$ifndef CLX_USED}and ((csDesigning in ComponentState) or (ImageForm = nil)){$endif}{$endif} then
            begin
              if not OwnBk then
                ACanvas.Brush.Color := BkColor;
              ACanvas.FillRect(R);
            end
            else
            {$endif}
            if OwnBk then
{$ENDIF}
            if not FTransBk then
              ACanvas.FillRect(R);

            if (FODFollowCol and (HS.Style = ElhsOwnerDraw)) or ((not (FODFollowCol)) and (SText = FODMask))
{$ifdef ELTREE_USE_STYLES}
              or (((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) and (CurStyle.FStyle = elhsOwnerDraw))
{$endif}
              then
            begin
              R := Rect(HS.Left + 1 - FHPos - xOffs, ItemRect.Top + 1, HS.Right - (FHPos + flv + 1) - xOffs, ItemRect.Bottom - 1);
              DoItemDraw(Item, ACanvas, R, hi);
            end
            else
            begin
{$ifdef ELTREE_USE_STYLES}
              if Item.UseStyles and (not CurStyle.OwnerProps) and (CurStyle.Control <> nil) then
              begin
                if CurStyle.Control.Visible then
                begin
                  R := Rect(HS.Left - FHPos - xOffs, ItemRect.Top, HS.Right - (FHPos + flv) - xOffs, ItemRect.Bottom);
                  InflateRect(R, -CurStyle.Control.BorderWidth, -CurStyle.Control.BorderWidth);
                  CurStyle.Control.Paint(ACanvas, R);
                end;
              end
              else
              if (Item.UseStyles and (CurStyle.FStyle = elhsText)) or (not ((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles)) then
{$endif}
              begin
{$ifdef ELTREE_USE_STYLES}
                if ((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) and (not CurStyle.OwnerProps) then
                begin
                  AL := CurStyle.FTextFlags;
                  {$ifndef CLX_USED}
{$IFNDEF LITE}
                  VAL := AL and (DT_TOP or DT_BOTTOM or DT_VCENTER);
{$ENDIF}
                  {$else}
                  VAL := AL and (Integer(AlignmentFlags_AlignTop) or Integer(AlignmentFlags_AlignBottom) or Integer(AlignmentFlags_AlignVCenter));
                  {$endif}
                  if FOverColors or CurStyle.FOwnerProps then
                  begin
                    ACanvas.Font.Color := FCurTextColor;
                    ACanvas.Brush.Color := FCurBkColor;
                  end
                  else
                  begin
                    ACanvas.Brush.Color := CurStyle.FTextBkColor;
                    ACanvas.Font.Color := CurStyle.FTextColor;
                    if not CurStyle.UseBkColor then
                      FTransBk := true;
{$IFNDEF PaintBackground}
                    Blend := true;
{$ENDIF}
{$IFNDEF LITE}
                    OwnBk := true;
{$ENDIF}
                  end;
                  if not CurStyle.FOwnerProps then with CurStyle do
                  begin
                    if ACanvas.Font.Name <> FontName then ACanvas.Font.Name := FontName;
                    if ACanvas.Font.Style <> FontStyles then ACanvas.Font.Style := FontStyles;
                    if ACanvas.Font.Size <> FontSize then ACanvas.Font.Size := FontSize;
                  end
                  else
                  begin
                    if ACanvas.Font.Name <> Font.Name then ACanvas.Font.Name := Font.Name;
                    if ACanvas.Font.Style <> Font.Style then ACanvas.Font.Style := Font.Style;
                    if ACanvas.Font.Size <> Font.Size then ACanvas.Font.Size := Font.Size;
                  end;
                end
                else
{$endif}
                begin
                  if ACanvas.Font.Name <> Font.Name then ACanvas.Font.Name := Font.Name;
                  if ACanvas.Font.Style <> Font.Style then ACanvas.Font.Style := Font.Style;
                  if ACanvas.Font.Size <> Font.Size then ACanvas.Font.Size := Font.Size;

                  {$ifndef CLX_USED}
                  if FRightAlignedText then AL := DT_RIGHT else Al := DT_LEFT;
                  if FRightAlignedText then
                  begin
                    case HS.Alignment of
                      hsaCenter: Al := DT_CENTER;
                      hsaRight: Al := DT_LEFT;
                    end; // case
                  end
                  else
                  begin
                    case HS.Alignment of
                      hsaCenter: Al := DT_CENTER;
                      hsaRight: Al := DT_RIGHT;
                    end; // case
                  end;
                  AL := AL or DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF} {$IFNDEF LITE}MultiLineEllipseFlags[Item.Multiline]{$ELSE}DT_END_ELLIPSIS{$ENDIF};
{$IFNDEF LITE}
                  VAL := 0;
{$ENDIF}
                  {$else}
                  if FRightAlignedText then
                    AL := Integer(AlignmentFlags_AlignRight)
                  else
                    Al := Integer(AlignmentFlags_AlignLeft);
                  if FRightAlignedText then
                  begin
                    case HS.Alignment of
                      hsaCenter: Al := Integer(AlignmentFlags_AlignHCenter);
                      hsaRight: Al := Integer(AlignmentFlags_AlignLeft);
                    end; // case
                  end else
                  begin
                    case HS.Alignment of
                      hsaCenter: Al := Integer(AlignmentFlags_AlignHCenter);
                      hsaRight: Al := Integer(AlignmentFlags_AlignRight);
                    end; // case
                  end;
                  AL := AL or MultiLineFlags[Item.Multiline] or MultiLineEllipseFlags[Item.Multiline];
{$IFNDEF LITE}
                  VAL := Integer(AlignmentFlags_AlignLeft);
{$ENDIF}
                  {$endif}
                end;
                if (FTracking and (Item = FTrackItem)) and FUnderlineTracked and FRowHotTrack then ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];

                if Item.Selected and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) and ((FSelectColumn = hi) or FRowSelect) then
                begin
                  if ({((GetParentForm(self) <> nil) and (GetParentForm(self).ActiveControl = self))}FHasFocus or (not FHideSelect)) then
                  begin
                    ACanvas.Brush.Color := FFocusedSelectColor;
                    ACanvas.Font.Color := FFocusedSelectTextColor;
                  end else
                  begin
                    ACanvas.Brush.Color := FHideSelectColor;
                    ACanvas.Font.Color := FHideSelectTextColor;
                  end;
{$IFNDEF PaintBackground}
                  Blend := true;
{$ENDIF}
{$IFNDEF LITE}
                  OwnBk := true;
{$ENDIF}
                end;
                if (TSI = FDropTrg) and (FDragTrgDrawMode in [ColorRect, SelColorRect])
                   and (((not FRowSelect) and (FSelectColumn = hi)) or FRowSelect) then
                begin
                  case FDragTrgDrawMode of
                    ColorRect:
                      begin
                        if FDropAcc then
                          ACanvas.Brush.Color := FDragRectAcceptColor
                        else
                          ACanvas.Brush.Color := FDragRectDenyColor;
                        ACanvas.Font.Color := clBtnText;
                      end;
                    SelColorRect:
                      begin
                        ACanvas.Brush.Color := clHighlight;
                        ACanvas.Font.Color := clHighlightText;
                      end;
                  end; // case
                  {$IFNDEF LITE}
                  OwnBk := true;
                  {$ENDIF}
{$IFNDEF PaintBackground}
                  Blend := true;
{$ENDIF}
                end;
                SetRectEmpty(R3);
                ACanvas.Font.Charset := Font.Charset;
{$IFDEF HAS_HTML_RENDER}
                if Item.IsHTML and (Copy(SText, 1, 6) = '<html>') then
                begin
                  R3.Left := 0;
                  R3.Top := 0;
                  if VirtualityLevel = vlNone then
                  begin
                    FData := TElHTMLData(Item.FHTMLDataArray[i]);

                    if FData = nil then
                    begin
                      R3.Right  := 0;
                      R3.Bottom := 0;
                    end
                    else
                    with FRender do
                    begin
                      R3.Right := FData.TextSize.cx;
                      R3.Bottom := FData.TextSize.cy;
                    end;
                  end
                  else
                  begin
                    with FRender do
                    begin
                      Data.DefaultStyle := ACanvas.Font.Style;
                      Data.DefaultFont  := ACanvas.Font.Name;
                      Data.DefaultColor := ACanvas.Font.Color;
                      Data.DefaultHeight:= ACanvas.Font.Height;
                      Data.Charset      := ACanvas.Font.Charset;

                      PrepareText(SText, 0, false);
                      R3.Right := Data.TextSize.cx;
                      R3.Bottom := Data.TextSize.cy;
                    end;
                  end;
                end
                else
{$ENDIF}
                begin
                  if FTransBk then
                    ACanvas.Brush.Style := bsClear;
                  {$ifndef CLX_USED}
                  {$ifdef ELPACK_UNICODE}
                  ElVCLUtils.DrawTextW(ACanvas.Handle, PWideChar(SText), Length(SText), R3, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF} DT_CALCRECT);
                  {$else}
                  DrawText(ACanvas.Handle, PChar(SText), Length(SText), R3, DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF} DT_CALCRECT);
                  {$endif}
                  {$else}
                  ACanvas.TextExtent(SText, R3, MultiLineFlags[Item.Multiline] or Integer(AlignmentFlags_AlignLeft));
                  {$endif}
                end;
                tw := R3.Right - R3.Left + FDivLineWidth * 2;
                // define the rectangle for the text
                R := Rect(HS.Left - (FHPos + xOffs) {- FDivLineWidth}, ItemRect.Top, HS.Right - (FHPos {+ flv}) - xOffs {- FDivLineWidth}, ItemRect.Bottom);
                R2 := R;
                {$ifndef CLX_USED}
                case (AL and (DT_LEFT or DT_RIGHT or DT_CENTER)) of
                {$else}
                case (AL and (Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignRight) or Integer(AlignmentFlags_AlignHCenter))) of
                {$endif}
                {$ifndef CLX_USED}
                  DT_LEFT:
                {$else}
                  Integer(AlignmentFlags_AlignLeft):
                {$endif}
                    begin
                      R.Right := Min(R.Left + tw + ItemExt div 3, R.Right);
                      R.Left := Min(R.Left + ItemExt div 5, R.Right);
                    end;
                {$ifndef CLX_USED}
                  DT_RIGHT:
                {$else}
                  Integer(AlignmentFlags_AlignRight):
                {$endif}
                    begin
                      R.Left := Max(R.Right - (tw + ItemExt div 3), R.Left);
                      R.Right := Max(R.Right - ItemExt div 5, R.Left);
                    end;
                {$ifndef CLX_USED}
                  DT_CENTER:
                {$else}
                  Integer(AlignmentFlags_AlignHCenter):
                {$endif}
                    begin
                      //InflateRect(R, -FDivLineWidth, 0);
                      R.Left := R.Left + ((R.Right - R.Left) shr 1 - Min(R.Right - R.Left, tw + 3) shr 1);
                      R.Right := R.Left + Min(R.Right - R.Left, tw + 3);
                    end;
                end;

                if (R.Left > R2.Right) then
                  R.Left := R2.Right;
                if R.Right < R2.Left then
                  R.Right := R2.Left;

                //InflateRect(R, 1, 1);
                //if FHLines then inc(R.Bottom, FDivLineWidth);
                {$ifdef HAS_HTML_RENDER}
                AdjColor := clNone;
                {$endif}
{$IFNDEF LITE}
{$IFNDEF PaintBackground}
                if (BackgroundType <> bgtColorFill) and ((Blend and (not FNoBlendSelected)) or (not Blend)) then
                begin
                  BgRect4 := R;
                  BgRect := ClientRect;
                  BgRect1 := ClientRect;

                  BgRect2 := ItemRect;
                  OffsetRect(BgRect2, -BgRect2.Left, -BgRect2.Top);
                  if BackgroundType <> bgtCenterBitmap then
                    ExtDrawBkgnd(ACanvas.Handle, Handle, BgRect1, BgRect, BgRect2, BgRect4, false, ACanvas.Brush.Color, ACanvas.Brush.Color, Blend, FTmpBmp, bgtCenterBitmap)
                  else
                    ExtDrawBkgnd(ACanvas.Handle, Handle, BgRect1, BgRect, BgRect2, BgRect4, false, ACanvas.Brush.Color, ACanvas.Brush.Color, Blend, Background, BackgroundType);
                end else
{$ENDIF}
                if OwnBk then
{$ENDIF}
                begin
                  ACanvas.FillRect(R);
                  {$ifdef HAS_HTML_RENDER}
                  AdjColor := ACanvas.Brush.Color;
                  {$endif}
                end;
                ACanvas.Brush.Style := bsClear;

                {$IFNDEF LITE}
                if (Item.Multiline
                {$IFDEF HAS_HTML_RENDER}or Item.IsHTML{$ENDIF})
                {$ifndef CLX_USED}
                and (VAL = 0)
                {$else}
                and (VAL = Integer(AlignmentFlags_AlignLeft))
                {$endif}
                and (LineHeight < (R1.Bottom - R1.Top)) then
                {$ifndef CLX_USED}
                    VAL := DT_TOP
                {$else}
                    VAL := Integer(AlignmentFlags_AlignTop)
                {$endif}
                else
                {$ENDIF}
                {$ifndef CLX_USED}
                    VAL := DT_VCENTER;
                {$else}
                    VAL := Integer(AlignmentFlags_AlignVCenter);
                {$endif}

{$IFDEF HAS_HTML_RENDER}
                if Item.IsHTML and (Copy(SText, 1, 6) = '<html>') then
                begin
                  if RightAlignedText then
                  begin
                    R3.Left := R.Right - Min(R.Right - R.Left, R3.Right - R3.Left);
                    R3.Right := R.Right;
                  end else
                  begin
                    R3.Right := Min(R.Right - R.Left, R3.Right - R3.Left) + R.Left;
                    R3.Left := R.Left;
                  end;

                  R3.Bottom := Min(R3.Bottom, R3.Top + (R1.Bottom - R1.Top));

                  {$ifndef CLX_USED}
                  if (VAL and DT_BOTTOM) = DT_BOTTOM then
                  {$else}
                  if (VAL and Integer(AlignmentFlags_AlignBottom)) = Integer(AlignmentFlags_AlignBottom) then
                  {$endif}
                  begin
                    OffsetRect(R3, 0, R.Top + (R.Bottom - R.Top) - (R3.Bottom - R3.Top));
                  end else
                  {$ifndef CLX_USED}
                  if (VAL and DT_VCENTER) = DT_VCENTER then
                  {$else}
                  if (VAL and Integer(AlignmentFlags_AlignVCenter)) = Integer(AlignmentFlags_AlignVCenter) then
                  {$endif}
                  begin
                    OffsetRect(R3, 0, R.Top + (((R.Bottom - R.Top) - (R3.Bottom - R3.Top)) shr 1) - 1);
                  end else
                  begin
                    OffsetRect(R3, 0, R.Top);
                  end;
                  if VirtualityLevel = vlNone then
                    FData := TElHTMLData(Item.FHTMLDataArray[i])
                  else
                    FData := FRender.Data;

                  if FData <> nil then
                  with FRender do
                  begin
                    SetData(FData);
                    DrawTextEx(ACanvas, Point(0, 0), R3, true, ACanvas.Font.Color, FData.DefaultBgColor, FData.HighlightColor, FData.HighlightBgColor, AdjColor);
                    if VirtualityLevel = vlNone then
                      SetData(nil);
                  end;
                end
                else
{$ENDIF}
                begin
                  if RightAlignedText then
                  begin
                    R3.Left := R.Right - Min(R.Right - R.Left, R3.Right - R3.Left);
                    R3.Right := R.Right;
                  end else
                  begin
                    R3.Right := Min(R.Right - R.Left, R3.Right - R3.Left) + R.Left;
                    R3.Left := R.Left;
                  end;
                  R3.Bottom := Min(R3.Bottom, R3.Top + (R1.Bottom - R1.Top));
                  
                  {$ifndef CLX_USED}
                  if (VAL and DT_BOTTOM) = DT_BOTTOM then
                  {$else}
                  if (VAL and Integer(AlignmentFlags_AlignBottom)) = Integer(AlignmentFlags_AlignBottom) then
                  {$endif}
                  begin
                    OffsetRect(R3, 0, R.Top + (R.Bottom - R.Top) - (R3.Bottom - R3.Top));
                  end else
                  {$ifndef CLX_USED}
                  if (VAL and DT_VCENTER) = DT_VCENTER then
                  {$else}
                  if (VAL and Integer(AlignmentFlags_AlignVCenter)) = Integer(AlignmentFlags_AlignVCenter) then
                  {$endif}
                  begin
                    OffsetRect(R3, 0, R.Top + (((R.Bottom - R.Top) - (R3.Bottom - R3.Top)) shr 1) - 1);
                  end else
                  begin
                    OffsetRect(R3, 0, R.Top);
                  end;
                  {$ifndef CLX_USED}
                  {$ifdef ELPACK_UNICODE}
                    ElVCLUtils.DrawTextW(ACanvas.Handle, PWideChar(SText), Length(SText), R3, DT_NOPREFIX or VAL or AL or DT_EXTERNALLEADING);
                  {$else}
                    DrawText(ACanvas.Handle, PChar(SText), Length(SText), R3, DT_NOPREFIX or VAL or AL or DT_EXTERNALLEADING);
                  {$endif}
                  {$else}
                  ACanvas.TextRect(R3, R3.Left, R3.Top, SText, VAL or AL);
                  {$endif}
                end;
                ACanvas.Brush.Style := bsSolid;
              end
{$ifdef ELTREE_USE_STYLES}
              else
              if Item.UseStyles and (CurStyle.FStyle = elhsPictureOnly) and Assigned(CurStyle.FPicture) then
              begin
                R := Rect(HS.Left - FHPos - xOffs, ItemRect.Top, HS.Right - (FHPos + flv) - xOffs, ItemRect.Bottom);
                h := CurStyle.FPicture.Height;
                w := CurStyle.FPicture.Width;
                R2 := Rect(((R.Left + R.Right) shr 1)-(w shr 1), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1), Min(R.Right, ((R.Left + R.Right) shr 1)-(w shr 1) + w), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1) + h);
                //R2 := Rect(R.Left, R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1), Min(R.Right, R.Left + w), R.Top + ((R.Bottom - R.Top + 1) shr 1) - (h shr 1) + h);
                if CurStyle.FPicture.Transparent then
                begin
                  //R2 := Rect(0, 0, Min(CurStyle.FPicture.Width, R.Right - R.Left), Min(CurStyle.FPicture.Height, ((R.Bottom-R.Top+1) shr 1)- (h shr 1) + h));
                  //DrawTransparentBitmapEx(ACanvas.Handle, CurStyle.FPicture.Handle, R.Left, R.Top + ((R.Bottom-R.Top + 1) shr 1)-(h shr 1), R2, CurStyle.FPicture.TransparentColor)
                  DrawTransparentBitmapEx(ACanvas.Handle, CurStyle.FPicture, R2.Left, R2.Top, Rect(0, 0, min(w, Min(R.Right, R.Left + w - 1) - R.Left + 1), h), CurStyle.FPicture.TransparentColor);
                end
                else
                  ACanvas.CopyRect(R2, CurStyle.FPicture.Canvas, Rect(0, 0, min(w, Min(R.Right, R.Left + w - 1) - R.Left + 1), h));
              end;
{$endif}
            end; // if/else
          end;
          if FVLines and (((not FBarStyle) and (Item.BorderStyle = ibsNone)) or FBSVLines) then
          begin
            xOffs := 0;
{$IFNDEF LITE}
            if HS.Locked then
              xOffs := - FHPos;
{$ENDIF LITE}
            i := HS.Right - FHPos - xOffs - 1;
            ACanvas.Pen.Color := VertDivLinesColor;
            ACanvas.MoveTo(i, ItemRect.Top);
            ACanvas.LineTo(i, ItemRect.Bottom + 1);
          end;
        end;
      end; //for

      if Item.StrikedOutLine then
      begin
        PrevPenColor := ACanvas.Pen.Color;
        if Item.Selected and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) and FRowSelect then
        begin
          if ({((AForm <> nil) and (AForm.ActiveControl = self))}FHasFocus or (not FHideSelect)) then
            ACanvas.Pen.Color := FFocusedSelectTextColor
          else
            ACanvas.Pen.Color := FHideSelectTextColor;
        end
        else
          ACanvas.Pen.Color := Item.StrikedLineColor;
        xOffs := 0;
{$IFNDEF LITE}
            if (FHeader.Sections.Count = 1) and FHeader.Sections[0].Locked then
              xOffs := - FHPos;
{$ENDIF LITE}
        {$ifndef CLX_USED}
        MoveToEx(ACanvas.Handle,Item.FTextLeft - 2 - (FHPos - xOffs), (ItemRect.Top + ItemRect.Bottom) shr 1 + 1 , nil);
        LineTo(ACanvas.Handle, FHeader.SectionsWidth - (FHPos - xOffs), (ItemRect.Top + ItemRect.Bottom) shr 1 + 1);
        {$else}
        ACanvas.MoveTo(Item.FTextLeft - 2 - (FHPos - xOffs), (ItemRect.Top + ItemRect.Bottom) shr 1 + 1);
        ACanvas.LineTo(FHeader.SectionsWidth - (FHPos - xOffs), (ItemRect.Top + ItemRect.Bottom) shr 1 + 1);
        {$endif}
        ACanvas.Pen.Color := PrevPenColor;
      end;
    end
    else
    begin
      DoRedrawItemTree(ACanvas, Item, ItemRect, ItemRect);
      if Item.StrikedOutLine then
      begin
        PrevPenColor := ACanvas.Pen.Color;
        if Item.Selected and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) and FRowSelect then
        begin
          if ({((AForm <> nil) and (AForm.ActiveControl = self))}FHasFocus or (not FHideSelect)) then
            ACanvas.Pen.Color := FFocusedSelectTextColor
          else
            ACanvas.Pen.Color := FHideSelectTextColor;
        end
        else
          ACanvas.Pen.Color := Item.StrikedLineColor;

        {$ifndef CLX_USED}
        MoveToEx(ACanvas.Handle, Item.FTextLeft - 2 - FHPos, (ItemRect.Top + ItemRect.Bottom) shr 1 + 1 , nil);
        LineTo(ACanvas.Handle, Item.FTextRight + FHPos, (ItemRect.Top + ItemRect.Bottom) shr 1 + 1);
        {$else}
        ACanvas.MoveTo(Item.FTextLeft - 2 - FHPos, (ItemRect.Top + ItemRect.Bottom) shr 1 + 1);
        ACanvas.LineTo(Item.FTextRight + FHPos, (ItemRect.Top + ItemRect.Bottom) shr 1 + 1);
        {$endif}
        ACanvas.Pen.Color := PrevPenColor;
      end;
    end;
    if ALockBmp <> nil then ALockBmp.Free;

    if not (FRowSelect) then
    begin
      if (FSelectColumn = -1) or (not FShowHeader) or (FHeader.Sections.Count <= FSelectColumn) then
      begin
        R1.left := TSI.FTextLeft - FHPos - xOffs + 1;
        R1.Right := TSI.FTextRight - FHPos - xOffs - 1;
      end else
      begin
        HS := FHeader.Sections[FSelectColumn];
        R1.Left := HS.Left - FHPos - xOffs;
        R1.Right := R1.Left + HS.Width - xOffs;
        if FVLines then
           dec(R1.Right, FDivLineWidth);
      end;
    end;

    begin
      RD := R1;

      if RowSelect then
      begin
        RD.Left := ItemRect.Left - FHPos;
        if FShowHeader then
        begin
    {$IFNDEF LITE}
          if LockedHeaderSection <> nil then RD.Left := 0;
    {$ENDIF LITE}
          RD.Right := ClientWidth;
          if (FBarStyle or (Item.BorderStyle <> ibsNone)) then
          begin
            dec(RD.Right);
    {$IFNDEF LITE}
            if LockedHeaderSection <> nil then inc(RD.Left);
    {$ENDIF LITE}
          end;
        end
        else
           RD.Right := ItemRect.Right + 1;
      end;

      Inc(RD.Bottom);

      if RowSelect then
      begin
        R1.Left := ItemRect.Left - FHPos;
        if FShowHeader then
        begin
  {$IFNDEF LITE}
          if LockedHeaderSection <> nil then R1.Left := 0;
  {$ENDIF LITE}
          R1.Right := FHeader.SectionsWidth - FHPos - flv; //ItemRect.Right {- FHPos }- flv;
          if (FBarStyle or (Item.BorderStyle <> ibsNone)) then
          begin
            dec(R1.Right);
  {$IFNDEF LITE}
            if LockedHeaderSection <> nil then
              inc(R1.Left);
  {$ENDIF LITE}
          end;
        end
        else
           R1.Right := ItemRect.Right(* - 1*);
      end
    end;
    //if not FHLines then
    dfr := FTreeIsFocused or (not FHideFocusRect);
    FOwner.TriggerItemPostDrawEvent(ACanvas, Item, R1, dfr);
    {$ifndef NEW_GETITEMRECT}
    inc(R1.Bottom);
    {$endif}
    if TSI.Focused and dfr and FDrawFocusRect then
    begin
      {$ifdef CLX_USED}
      ACanvas.Start;
      {$endif}
      ACanvas.Brush.Color := FBkColor;
      ACanvas.Pen.Color := FTextColor;
      ACanvas.Font.Color := FTextColor;
      // this one is made to fix the colors
      {$ifndef CLX_USED}
      ACanvas.TextOut(R1.Left, R1.Top, '');
      {$endif}
      // otherwise FocusRect won't draw right
      {$ifndef CLX_USED}
      ACanvas.DrawFocusRect(R1);
      {$else}
      QStyle_drawFocusRect(Application.Style.Handle, ACanvas.Handle, @R1, QWidget_colorGroup(Parent.Handle), nil, false);
      ACanvas.Stop;
      {$endif}
    end;
    if (TSI = FDropTrg) then
    begin
      case FDragTrgDrawMode of
      ColorFrame:
        begin
          if FDropAcc then
            ACanvas.Brush.Color := FDragRectAcceptColor
          else
            ACanvas.Brush.Color := FDragRectDenyColor;
          ACanvas.TextOut(RD.Left, RD.Top, '');
          {$ifndef CLX_USED}
          ACanvas.FrameRect(RD);
          {$else}
          ACanvas.Pen.Color := ACanvas.Brush.Color;
          ACanvas.Brush.Style := bsClear;
          ACanvas.Rectangle(RD);
          {$endif}
        end;
      dtdUpColorLine:
        begin
          if FDropAcc then
            ACanvas.Pen.Color := FDragRectAcceptColor
          else
            ACanvas.Pen.Color := FDragRectDenyColor;
          ACanvas.MoveTo(RD.Left, RD.Top);
          ACanvas.LineTo(RD.Right, RD.Top);

          ACanvas.MoveTo(RD.Left, RD.Top + 3);
          ACanvas.LineTo(RD.Left, RD.Top);
          ACanvas.MoveTo(RD.Left + 1, RD.Top + 1);
          ACanvas.LineTo(RD.Left + 1, RD.Top);

          ACanvas.MoveTo(RD.Right - 1, RD.Top + 3);
          ACanvas.LineTo(RD.Right - 1, RD.Top);
          ACanvas.MoveTo(RD.Right - 2, RD.Top + 1);
          ACanvas.LineTo(RD.Right - 2, RD.Top);
        end;
      dtdDownColorLine:
        begin
          dec(RD.Bottom);
          if FDropAcc then
            ACanvas.Pen.Color := FDragRectAcceptColor
          else
            ACanvas.Pen.Color := FDragRectDenyColor;
          ACanvas.MoveTo(RD.Left, RD.Bottom);
          ACanvas.LineTo(RD.Right, RD.Bottom);

          ACanvas.MoveTo(RD.Left, RD.Bottom - 3);
          ACanvas.LineTo(RD.Left, RD.Bottom);
          ACanvas.MoveTo(RD.Left + 1, RD.Bottom - 1);
          ACanvas.LineTo(RD.Left + 1, RD.Bottom);

          ACanvas.MoveTo(RD.Right - 1, RD.Bottom - 3);
          ACanvas.LineTo(RD.Right - 1, RD.Bottom);
          ACanvas.MoveTo(RD.Right - 2, RD.Bottom - 1);
          ACanvas.LineTo(RD.Right - 2, RD.Bottom);
        end;
      dtdUpSelColorLine:
        begin
          ACanvas.Pen.Color := clHighlight;
          ACanvas.MoveTo(RD.Left, RD.Top);
          ACanvas.LineTo(RD.Right, RD.Top);

          ACanvas.MoveTo(RD.Left, RD.Top + 3);
          ACanvas.LineTo(RD.Left, RD.Top);
          ACanvas.MoveTo(RD.Left + 1, RD.Top + 1);
          ACanvas.LineTo(RD.Left + 1, RD.Top);

          ACanvas.MoveTo(RD.Right - 1, RD.Top + 3);
          ACanvas.LineTo(RD.Right - 1, RD.Top);
          ACanvas.MoveTo(RD.Right - 2, RD.Top + 1);
          ACanvas.LineTo(RD.Right - 2, RD.Top);
        end;
      dtdDownSelColorLine:
        begin
          dec(RD.Bottom);
          ACanvas.Pen.Color := clHighlight;
          ACanvas.MoveTo(RD.Left, RD.Bottom);
          ACanvas.LineTo(RD.Right, RD.Bottom);

          ACanvas.MoveTo(RD.Left, RD.Bottom - 3);
          ACanvas.LineTo(RD.Left, RD.Bottom);
          ACanvas.MoveTo(RD.Left + 1, RD.Bottom - 1);
          ACanvas.LineTo(RD.Left + 1, RD.Bottom);

          ACanvas.MoveTo(RD.Right - 1, RD.Bottom - 3);
          ACanvas.LineTo(RD.Right - 1, RD.Bottom);
          ACanvas.MoveTo(RD.Right - 2, RD.Bottom - 1);
          ACanvas.LineTo(RD.Right - 2, RD.Bottom);
        end;
      end;
    end;
  end;
end;
{$WARNINGS on}
{$hints on}

function TElTreeView.GetVisiblesHeight : integer;
var i : integer;
begin
  result := 0;
  for i := 0 to FVisible.Count - 1 do
      inc(Result, TelTreeItem(FVisible[i]).GetHeight);
end;

procedure TElTreeView.RedrawTree(ACanvas : TCanvas; RealLeftPos : integer; ItemsList : TElList);
var
  i, j,
  y, h,
  oy  : integer;
  R, r1, R2, r4: TRect;
  Item: TElTreeItem;
  Sect: TElHeaderSection;
  fSaveStyle : TBrushStyle;
  FSaveLeftPos : integer;
  mb : integer;
begin
  with FOwner do
  begin
    if FShowHeader then
    begin
      with FHeader do
      if (Left <> FHPos) {or (Width <> FHPos + FView.Width) }then
      begin
        LeftPos := FHPos;
        //Width := FHPos + FView.Width;
      end;
    end;
    FSaveLeftPos := FHPos;
    FHPos := RealLeftPos;
    FHeader.LeftPos := FHPos;

    if FHeader.Sections.Count <= MainTreeColumn then
       MainTreeColumn := 0;

    fSaveStyle := ACanvas.Brush.Style;
    // redraw items
    j := ItemsList.Count - 1;
    for i := 0 to j do
    begin
      R := Self.GetItemRect(i);
      Item := TElTreeItem(ItemsList[i]);
      if FHLines and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) then
          dec(R.Bottom, FDivLineWidth);
      if IntersectRect(R4, R, ACanvas.ClipRect) then
      begin
        r1 := r4;
        R1.Bottom := r1.Top + Item.Height;
        if FHLines and ((not FBarStyle) and (Item.BorderStyle = ibsNone)) then
           dec(R1.Bottom, FDivLineWidth);
        if FShowHeader then
           r1.right := FHeader.SectionsWidth
        else
        begin
          R.Right := Max(FHRange, FHPos + Self.Width);
          r1.right := R.Right;
        end;


{$ifdef ELTREE_USE_STYLES}
        if not ((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
{$endif}
        begin
          ACanvas.Font.Color := FTextColor;
          ACanvas.Pen.Color := FTextColor;
        end;

        // TODO: FCurBkColor := BkColor;
        FCurTextColor := FTextColor;
        if FBarStyle or (Item.BorderStyle <> ibsNone) then
        begin
          InflateRect(R, -1, -1);
          InflateRect(R1, -1, -1);
        end;
        DoRedrawItem(ACanvas, Item, R, R1);
        if FBarStyle or (Item.BorderStyle <> ibsNone) then
        begin
          InflateRect(R, 1, 1);
          //inc(R.Bottom);
          if FShowHeader then
            R.Right := -FHPos + FHeader.SectionsWidth;
          if Item.BorderStyle <> ibsNone then
          begin
            {$ifndef CLX_USED}
            case Item.BorderStyle of
              ibsRaised: DrawEdge(ACanvas.Handle, R, BDR_RAISEDINNER, BF_RECT);
              ibsFlat: DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_FLAT or BF_RECT);
              ibsSunken: DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_RECT);
              ibsSpace:
                begin
                  Windows.SetBkColor(ACanvas.Handle, ColorToRGB(Item.BorderSpaceColor));
                  FrameRect(ACanvas.Handle, R, ACanvas.Brush.Handle);
                end;
            end;
            {$else}
            case Item.BorderStyle of
              ibsRaised: DrawEdge(ACanvas, R, esNone, esRaised, [ebTop, ebLeft, ebRight, ebBottom]);
              // ibsFlat: DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_FLAT or BF_RECT);
              ibsSunken: DrawEdge(ACanvas, R, esLowered, esNone, [ebTop, ebLeft, ebRight, ebBottom]);
              ibsSpace:
                begin
                  ACanvas.Pen.Color := Item.BorderSpaceColor;
                  ACanvas.Brush.Style := bsClear;
                  with R do
                    ACanvas.Rectangle(Left, Top, Right, Bottom);
                  ACanvas.Brush.Style := bsSolid;
                end;
            end;
            {$endif}
          end
          else
          begin
            {$ifndef CLX_USED}
            if not Item.Selected then
              DrawEdge(ACanvas.Handle, R, BDR_RAISEDINNER, BF_RECT)
            else
              DrawEdge(ACanvas.Handle, R, BDR_SUNKENOUTER, BF_RECT);
            {$else}
            if not Item.Selected then
              DrawEdge(ACanvas, R, esNone, esRaised, [ebTop, ebLeft, ebRight, ebBottom])
            else
              DrawEdge(ACanvas, R, esLowered, esNone, [ebTop, ebLeft, ebRight, ebBottom]);
            {$endif}
          end;
        end;
      end;
    end;
    ACanvas.Brush.Style := fSaveStyle;
    // fill the rest of the window
    r2 := Self.ClientRect;
    Inc(R2.Right);
    Inc(R2.Bottom);
    if IntersectRect(R4, R2, ACanvas.ClipRect) {$ifndef LITE}and (BackgroundType = bgtColorFill){$endif} {$ifdef ELPACK_COMPLETE}{$ifndef CLX_USED}and ((csDesigning in ComponentState) or (ImageForm = nil)){$endif}{$endif} then
    begin
      // Draw bottom empty part
      ACanvas.Brush.Color := FBkColor;
      oy := R4.top - 1;
      Y := GetVisiblesHeight;
      //Changed for flexible heights FVisible.Count * FLineHeight;
      if (R4.Top < Y) then
        R4.Top := Y;
      {$ifndef CLX_USED}
      if InSizeMove then
      {$endif}
        ACanvas.FillRect(R4);
      // Draw side empty part
      if FShowHeader then
      begin
        if FHeader.Sections.Count > 0 then
          r4.Left := FHeader.SectionsWidth
        else
          r4.left := 0;
        dec(R4.Left, FHPos);
        R4.Top := oy;
        {$ifndef CLX_USED}
        if InSizeMove then
        {$endif}
          ACanvas.FillRect(R4);
      end;
    end;

    ACanvas.Pen.Color := FOwner.HorzDivLinesColor;

    // Draw horizontal lines
    if (not FBarStyle) or FBSVLines then
    begin
      if FHLines and (not BarStyle) then
      begin
        y := 0;
        if FShowHeader then
          h := FHeader.SectionsWidth - FHPos
        else
          h := Self.ClientWidth;

        i := 0;
        if ItemsList.Count > 0 then
        begin
          Inc(Y, TElTreeItem(ItemsList[i]).GetHeight - 1);

          while Y < Self.ClientHeight do
          begin
            if TElTreeItem(ItemsList[i]).DrawHLine then
              if IntersectRect(R, Rect(0, Y, h, Y + FDivLineWidth), ACanvas.ClipRect) then
              begin
                ACanvas.MoveTo(R.Left, R.Top);
                ACanvas.LineTo(R.Right, R.Top);
              end;
            inc(i);
            if i >= FVisible.Count then break;
            Inc(Y, TElTreeItem(FVisible[i]).GetHeight);
          end;
        end;
      end;

      ACanvas.Pen.Color := FOwner.VertDivLinesColor;

      // Draw vertical lines
      if FVLines and (FShowHeader) then
      begin
        if (FBottomIndex >= 0) then
          mb := GetItemRect(ItemsList.Count - 1).Bottom - FHeader.Height
        else
          mb := ClientHeight - FHeader.Height;

        for i := 0 to FHeader.Sections.Count - 1 do
        begin
          Sect := FHeader.Sections[i];
          if (not Sect.Visible) or (Sect.Width <= 0) then continue;
{$IFNDEF LITE}
          if Sect.Locked then
             j := Sect.Right
          else
{$ENDIF LITE}
          begin
            j := Sect.Right - FHPos;
{$IFNDEF LITE}
            if FHeader.LockedSection <> nil then
            begin
              if j < FHeader.LockedSection.Width then Continue;
            end;
{$ENDIF}
          end;

          if FVerticalLinesLong and IntersectRect(R, Rect(j - FDivLineWidth, 0, j, Self.ClientHeight), ACanvas.ClipRect) then
          begin
            R.Top := mb;
            ACanvas.MoveTo(R.Left, R.Top);
            ACanvas.LineTo(R.Left, R.Bottom);
          end;
        end;
      end;
    end;
    FHPos := FSaveLeftPos;
    FHeader.LeftPos := FHPos;
  end;
end;

procedure TElTreeView.IntMouseMove(X, Y : integer; Shift : TShiftState);
var
  TSI  : TElTreeItem;
  R    : Trect;
  IP   : TSTItemPart;
{$IFNDEF VCL_5_USED}
  AMsg : TMessage;
{$ENDIF}
  HCol : integer;
  P    : TPoint;
  b    : boolean;
  
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
  dataObj: IDataObject;
  dropSource: IDropSource;
  OKEffects: TDragTypes;
  Effect: TDragType;
  dwOKEffects,
    dwEffect: integer;
  HRes: HResult;
{$ENDIF}
{$ENDIF}
{$endif}
{$IFDEF HAS_HTML_RENDER}
  href: TElFString;
    od: TElHTMLData;
{$ENDIF}
begin

{$IFDEF HAS_HTML_RENDER}
  TSI := GetItemAt(X, Y, IP, HCol);
  if (TSI<>nil) and (TSI.FHTMLData <> nil) then
  begin
    od := FRender.Data;
    FRender.SetData(TSI.FHTMLData);
    p := point(x-TSI.FHTMLData.Rect.Left, y-TSI.FHTMLData.Rect.Top);
    if FRender.IsCursorOverLink(P, Point(0,0), TSI.FHTMLData.Rect, href) then
      Cursor := FOwner.FLinkCursor
    else
      Cursor := FOwner.FCursor;
    FRender.SetData(od);
  end
  else Cursor := FOwner.FCursor;
{$ENDIF}

  if not Dragging then
  begin
    if FPressed then
    begin
      GetItemAt(X, Y, IP, HCol);
      b := (ip <> ipOutside) and (not FMouseSel) and FOwner.DragAllowed and
           ((FOwner.MultiSelect and (FOwner.FSelectedList <> nil) and (FOwner.FSelectedList.Count > 0)) or
           (not FOwner.MultiSelect and (FSelected <> nil))) and
           (sqrt(sqr(FPressCoord.X - X) + sqr(FPressCoord.Y - Y)) >= 5);
      if b then
      begin
{$ifdef ELTREE_USE_INPLACE_EDITORS}
        if FEditTimer <> nil then
           FEditTimer.Enabled := false;
{$endif}
        FClickPassed := false;
        FPassedItem := nil;
        FIgnoreClick2 := true;

        if FOwner.DragType <> dtOLE then
        begin
          BeginDrag(true);
        end
{$IFDEF VER90}
          ;
{$ELSE}
        else
        begin
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF LITE}
          dataObj := nil;
          dropSource := nil;
          OKEffects := [];
          FOwner.TriggerOleDragStartEvent(dataObj, dropSource, OKEffects);
          dwOkEffects := 0;
          if dtCopy in OKEffects then dwOkEffects := dwOkEffects or DROPEFFECT_COPY;
          if dtMove in OKEffects then dwOkEffects := dwOkEffects or DROPEFFECT_MOVE;
          if dtLink in OKEffects then dwOkEffects := dwOkEffects or DROPEFFECT_LINK;
          FPressed := false;

          if Assigned(DataObj) and Assigned(dropSource) then
          begin
            dwEffect := DROPEFFECT_NONE;
            Effect := dtNone;
            HRes := DoDragDrop(dataObj, dropSource, dwOKEffects, dwEffect);
            if (dwEffect and DROPEFFECT_COPY) <> 0 then Effect := dtCopy;
            if (dwEffect and DROPEFFECT_LINK) <> 0 then Effect := dtLink;
            if (dwEffect and DROPEFFECT_Move) <> 0 then Effect := dtMove;
            FOwner.TriggerOleDragFinishEvent(Effect, HRes);
          end;
{$ENDIF}
{$endif}
        end;
{$ENDIF}
      end
      else
      if FOwner.FMouseFrameSelect and (FVisible.Count > 0) and (FOwner.MultiSelect) then
      begin
        if (not FMouseSel) then
        begin
          if (sqrt(sqr(FPressCoord.X - X) + sqr(FPressCoord.Y - Y)) >= 5) then
          begin
            TSI := GetItemAtY(FPressCoord.Y);
            if TSI = nil then
              TSI := TElTreeItem(FVisible.Last);
            begin
              R := GetItemRect(FVisible.IndexOf(TSI));
              FMFSStartItem := TSI;
              FMFSStartCoord.x := FPressCoord.X + FOwner.FHPos;
              FMFSStartCoord.y := FPressCoord.Y - R.Top;
              FMFSEndItem := TSI;
              FMFSEndCoord := FMFSStartCoord;
              if not (ssCtrl in GetShiftState) then
              begin
                FOwner.DeselectAll;
                FOwner.DoAfterSelectionChange;
              end;
              {$ifndef CLX_USED}
              SetCapture(Handle);
              {$else}
              SetMouseGrabControl(Self);
              {$endif}
              AllocateMouseSelectFrame;
              FMouseSel := true;
              FMFSList  := TElList.Create;
            end;
          end;
        end
        else
        begin
          DrawMouseSelectFrame;
          if PtInRect(ClientRect, Point(X, Y)) then
          begin
            TSI := GetItemAtY(Y);
            if TSI = nil then
              TSI := TElTreeItem(FVisible.Last);
            begin
              R := GetItemRect(FVisible.IndexOf(TSI));
              FMFSEndItem := TSI;
              FMFSEndCoord.x := X + FOwner.FHPos;
              FMFSEndCoord.y := Y - R.Top;
            end;
            SelectMouseSelectItems;
            FOwner.DoAfterSelectionChange;
          end
          else
          begin
            if Y >= ClientHeight then
            begin
              SetVPosition(FOwner.FTopIndex + 1);
            end
            else
            if Y < 0 then
            begin
              SetVPosition(FOwner.FTopIndex - 1);
            end
            else
            if X >= ClientWidth then
            begin
              SetHPosition(FOwner.FHPos + 4);
            end
            else
            if X < 0 then
            begin
              SetHPosition(FOwner.FHPos - 4);
            end;
            Update;
          end;
          DrawMouseSelectFrame;
        end;
      end;
    end;
    if ((FHintItemEx <> nil) or (not ShowHint)) and (not FMouseSel) then
    begin
      Application.Hint := FOwner.FRealHint;
      Hint := FOwner.FRealHint;

      TSI := GetItemAt(X, Y, IP, HCol);
      if (TSI <> nil) then
      begin
        if (FHintItemEx <> TSI) then
        begin
          if Length(TSI.Hint) > 0 then
          begin
             Application.Hint := TSI.Hint;
             Hint := TSI.Hint;
          end;
          if ShowHint and (not FInDragging) then
          begin
            P := ClientToScreen(Point(X, Y));
            {$ifndef CLX_USED}
            {$IFDEF VCL_5_USED}
            Application.ActivateHint(P);
            {$ELSE}
            ZeroMemory(@AMsg, sizeof(AMsg));
            TWMMouse(AMsg).XPos := -10000;
            TWMMouse(AMsg).YPos := -10000;
            AMsg.Msg := WM_MOUSEMOVE;
            Application.HintMouseMessage(Self, AMsg);
            TWMMouse(AMsg).Pos := SmallPoint(X, Y);
            Application.HintMouseMessage(Self, AMsg);
            {$ENDIF}
            {$else}
            Application.HintMouseMessage(Self, Shift, -1, -1);
            Application.HintMouseMessage(Self, Shift, X, Y);
            {$endif}
          end;
        end;
      end;
    end;

    if (FOwner.FShowHintMode <> shmNone) and (not FMouseSel) then
    begin
      if (FHintCoord.X <> X) or (FHintCoord.Y <> Y) then
      begin
        TSI := GetItemAtY(Y);
        if ((FOwner.FHideHintOnMove) or (TSI <> nil)) and (FHintItem <> TSI) then
        begin
          DoHideLineHint;
          FHintItem := nil;
          FHintCoord := Point(X, Y);
          FHintTimer := TTimer.Create(self);
          FHintTimer.Enabled  := false;
          FHintTimer.OnTimer  := OnHintTimer;
          FHintTimer.Interval := Application.HintShortPause;
          if FHintTimer.Interval = 0 then
             FHintTimer.Interval := 1000;
          FHintTimer.Enabled := true;
        end;
      end;
    end;
    if FOwner.FTracking {and Focused }and (not FMouseSel) then
    begin
      TSI := FTrackItem;
      if FOwner.IsEditing then
        FTrackItem := nil
      else
        FTrackItem := GetItemAtY(Y);
{$IFDEF HOTTRACK_CURSOR}
      if (FTrackItem = nil) and (Cursor <> crDefault) then
        Cursor := crDefault;
{$ENDIF}
      if TSI = FTrackItem then exit;
      if (not FOwner.IgnoreEnabled) and (FTrackItem <> nil) and (not FTrackItem.Enabled) then
        FTrackItem := nil;

      if TSI <> nil then
      begin
        if (not FOwner.FShowHeader) or FOwner.FRowHotTrack then TSI.RedrawItem(false)
        else
        begin
          R := GetItemRect(FVisible.IndexOf(TSI));
          with FOwner do
          begin
            R.Left := FHeader.Sections[FMainTreeCol].Left - FHPos;
            R.Right := FHeader.Sections[FMainTreeCol].Right - FHPos;
          end;
          {$ifndef CLX_USED}
          InvalidateRect(Handle, @R, true);
          {$else}
          Inc(R.Bottom); Inc(R.Right);
          QWidget_update(Handle, @R);
          Dec(R.Bottom); Dec(R.Right);
          {$endif}
        end;
      end;
      if FTrackItem <> nil then
      begin
        if (not FOwner.FShowHeader) or FOwner.FRowHotTrack then FTrackItem.RedrawItem(false)
        else
        begin
          R := GetItemRect(FVisible.IndexOf(FTrackItem));
          with FOwner do
          begin
            R.Left := FHeader.Sections[FMainTreeCol].Left - FHPos;
            R.Right := FHeader.Sections[FMainTreeCol].Right - FHPos;
          end;
          {$ifndef CLX_USED}
          InvalidateRect(Handle, @R, true);
          {$else}
          Inc(R.Bottom); Inc(R.Right);
          QWidget_update(Handle, @R);
          Dec(R.Bottom); Dec(R.Right);
          {$endif}
        end;
      end;
      FOwner.TriggerHotTrackEvent(TSI, FTrackItem);
{$IFDEF HOTTRACK_CURSOR}
      if not (Owner = nil) and (Cursor <> Owner.TrackingCursor) then
        Cursor := Owner.TrackingCursor;
{$ENDIF}
      //Update;
    end; // if (FTracking and Focused)
  end; //if (not Dragging)
end;

procedure TElTreeView.IntLButtonDown(X, Y : integer; Shift : TShiftState);
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$define EL_COMMON_EDITORS}
{$endif}
begin
{$ifdef EL_COMMON_EDITORS}
  if FEditTimer <> nil then
     FEditTimer.Enabled := false;
{$endif}
  DoHideLineHint;
  FIgnoreClick2 := false;
  // this has been moved to MouseDown
  if (not (csDesigning in ComponentState)) and (not Focused) and CanFocus then
    SetFocus;
  FPressCoord := Point(X, Y);
  FPressed := true;
  (*
  Item := GetItemAt(XPos, YPos, ItemPart, HCol);
  if (Item = nil) or (Item.Enabled) or (FOwner.IgnoreEnabled)  then
  begin
    //if (ItemPart = ipMainText) then
    begin
{$ifdef EL_COMMON_EDITORS}
      if (Item <> nil) and ((Item = FClickItem) or FOwner.QuickEditMode) and Item.AllowEdit and FOwner.FCanEdit and (FClicked or FOwner.QuickEditMode) then
      begin
        with FOwner do
        if (FShowHeader) then
        begin
          if ((FClickSection = HCol) or (FOwner.QuickEditMode)) and FHeader.Sections[HCol].Editable and
              not (csDesigning in FOwner.ComponentState) then
          begin
            if FEditTimer = nil then
              FEditTimer := TTimer.Create(nil);
            FEditTimer.Enabled := false;
            FEditTimer.Interval := 500;
            FEditTimer.OnTimer := OnEditTimer;
            FItemToEdit := Item;
            FEditSect   := HCol;
            FEditTimer.Enabled := true;
          end;
        end
        else
        if not (csDesigning in FOwner.ComponentState) then
        begin
          if FEditTimer = nil then
            FEditTimer := TTimer.Create(nil);
          FEditTimer.Enabled := false;
          FEditTimer.Interval := 500;
          FEditTimer.OnTimer := OnEditTimer;
          FItemToEdit := Item;
          FEditSect   := -1;
          FEditTimer.Enabled := true;
        end;
        FClicked := false;
      end;
{$endif}
    end;
  end;
  *)
end;

function TElTreeView.IntLButtonUp(X, Y : integer; Shift : TShiftState): Boolean;
var
  Item: TElTreeItem;
  ItemPart: TSTItemPart;
  HCol    : Integer;
{$ifdef ELTREE_USE_STYLES}
  i, j: integer;
  Ctrl: TElCellControl;
  b: boolean;
  AStyle : TElCEllStyle;
{$endif}

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$define EL_COMMON_EDITORS}
{$endif}
{$IFDEF HAS_HTML_RENDER}
  href: TElFString;
     p: tpoint;
    od: TElHTMLData;
{$ENDIF}
begin
  result := false;
{$ifdef EL_COMMON_EDITORS}
  if FEditTimer <> nil then
     FEditTimer.Enabled := false;
{$endif}
  DoHideLineHint;
  FPressed := false;
  if FMouseSel then
  begin
    CancelMouseSel;
    FIgnoreClick2 := false;
    exit;
  end;

  Item := GetItemAt(X, Y, ItemPart, HCol);

  if (Item = nil) or (Item.Enabled) or (FOwner.IgnoreEnabled) then
  begin
    if (FPressCoord.Y = Y) and (FPressCoord.X = X)
    // if the mouse was moved when the button was pressed, we don't check for editing
    and (ItemPart <> ipInside) and (ItemPart <> ipOutside) and (ItemPart <> ipButton) then
    begin
      if not FIgnoreClick2 then
      begin
{$ifdef EL_COMMON_EDITORS}
        if (ItemPart in [ipMainText, ipColumn]) and
           (Item <> nil) and ((Item = FClickItem) or FOwner.QuickEditMode) and Item.AllowEdit
            and FOwner.FCanEdit and ((FClicked or FOwner.QuickEditMode) and (FOwner.FDblClickMode <> dcmEdit)) then
          InitiateEditOp(Item, HCol, false);
{$endif}
        FClicked := true;
        FClickCoord := Point(X, Y);
        FClickItem := Item;
        FClickSection := HCol;
      end;
    end;
    if Item = nil then
      FClickItem := nil;
    FIgnoreClick2 := false;

    result := true;

{$IFDEF HAS_HTML_RENDER}
    if (Item<>nil) and (Item.FHTMLData <> nil) then
    begin
      od := FRender.Data;
      FRender.SetData(Item.FHTMLData);
      p := point(x-Item.FHTMLData.Rect.Left, y-Item.FHTMLData.Rect.Top);
      if FRender.IsCursorOverLink(P, Point(0,0), Item.FHTMLData.Rect, href) then
        FOwner.TriggerLinkClickEvent(href, X - Left, Y - Top);
      FRender.SetData(od);
    end;
{$ENDIF}

{$ifdef ELTREE_USE_STYLES}
    // now check the CellControl

    b := (FClickControl <> nil);
    Item := GetItemAt(X, Y, ItemPart, HCol);
    if ItemPart = ipColumn then
    begin
      if Item.UseStyles then
      begin
        if FOwner.VirtualityLevel = vlNone then
        begin
          i := HCol;
          j := i;
          if i = FOwner.FMainTreeCol then
            AStyle := Item.MainStyle
          else
          begin
            if i > FOwner.FMainTreeCol then Dec(i);
            if Item.StylesCount > i then
              AStyle := Item.Styles[i]
            else
              AStyle := Item.MainStyle;
          end;
        end
        else
        begin
          j := HCol;
          AStyle := VirtStyle;
          FOwner.TriggerVirtualStyleNeeded(Item, HCol, AStyle);
        end;
        if (AStyle <> nil) and (not AStyle.OwnerProps) and (AStyle.Control <> nil)  and (AStyle.Control.Visible) then
        begin
          with FOwner do
          begin
            Ctrl := AStyle.Control;
            if Ctrl <> nil then
            begin
              Ctrl.TriggerMouseUpEvent(mbLeft, Shift, X - FOwner.FHPos - FHeader.Sections[j].Left, Y);
              if Ctrl = FClickControl then Ctrl.TriggerClickEvent;
              b := false;
            end;
          end;
        end;
      end;
    end;
    if b then
      FClickControl.TriggerMouseUpEvent(mbRight, GetShiftState, -1, -1);
    FClickControl := nil;
{$endif}
  end;
end;

{$IFDEF HAS_HTML_RENDER}
procedure TCustomElTree.TriggerLinkClickEvent(HRef : string; X, Y: integer);
begin
  if Assigned(FOnLinkClick) then FOnLinkClick(Self, HRef);
end;
{$ENDIF}

procedure TElTreeView.IntRButtonDown(X, Y : integer; Shift : TShiftState);
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$define EL_COMMON_EDITORS}
{$endif}
begin
  if not (csDesigning in ComponentState) and
    (CanFocus or (GetParentForm(Self) = nil)) then
  begin
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    DoEndEdit(false);
{$endif}
{$endif}
    SetFocus;
  end;
end;

function TElTreeView.IntRButtonUp(X, Y : integer; Shift : TShiftState): Boolean;
var
  ItemPart: TSTItemPart;
  HCol: Integer;
  Item: TElTreeItem;
{$ifdef ELTREE_USE_STYLES}
  Ctrl: TElCellControl;
  AStyle: TElCellStyle;
  b: boolean;
  APopupMenu : TPopupMenu;
  i, j : integer;
{$endif}

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$define EL_COMMON_EDITORS}
{$endif}
begin
  result := false;
{$ifdef EL_COMMON_EDITORS}
  if FEditTimer <> nil then
     FEditTimer.Enabled := false;
{$endif}
  DoHideLineHint;

{$ifdef ELTREE_USE_STYLES}
  b := (FClickControl <> nil);
{$endif}
  Item := GetItemAt(X, Y, ItemPart, HCol);
  if (Item = nil) or (Item.Enabled) or (FOwner.IgnoreEnabled) then
  begin
{$ifdef ELTREE_USE_STYLES}
    if ItemPart = ipColumn then
    begin
      if Item.UseStyles then
      begin
        if FOwner.VirtualityLevel = vlNone then
        begin
          i := HCol;
          j := i;
          if i = FOwner.FMainTreeCol then
            AStyle := Item.MainStyle
          else
          begin
            if i > FOwner.FMainTreeCol then Dec(i);
            if Item.StylesCount > i then
              AStyle := Item.Styles[i]
            else
              AStyle := Item.MainStyle;
          end;
        end
        else
        begin
          j := HCol;
          AStyle := VirtStyle;
          FOwner.TriggerVirtualStyleNeeded(Item, HCol, AStyle);
        end;
        if (AStyle <> nil) and (not AStyle.OwnerProps) and (AStyle.Control <> nil) and (AStyle.Control.Visible) then
        begin
          with FOwner do
          begin
            Ctrl := AStyle.Control;
            // if Ctrl <> nil then
            begin
              Ctrl.TriggerMouseUpEvent(mbRight, Shift, X - FOwner.FHPos - FHeader.Sections[j].Left, Y);
              if Ctrl = FClickControl then Ctrl.TriggerClickEvent;
              APopupMenu := Ctrl.PopupMenu;
              if (APopupMenu <> nil) and APopupMenu.AutoPopup then
              begin
                {$ifndef CLX_USED}
                SendCancelMode(nil);
                {$endif}
                APopupMenu.PopupComponent := Ctrl;
                with ClientToScreen(Point(X, Y)) do
                  APopupMenu.Popup(X, Y);
                Exit;
              end
              else
                result := true;
            end;
          end;
        end;
      end;
    end;
{$endif}
{$ifdef ELTREE_USE_STYLES}
    if b then
       FClickControl.TriggerMouseUpEvent(mbRight, GetShiftState, -1, -1);
    FClickControl := nil;
    if (not b) then
{$endif}
       result := true;
  end;
end;

function TElTreeView.IntLButtonDblClick(X, Y : integer; Shift : TShiftState): 
    Boolean;
var
  ItemPart: TSTItemPart;
  HCol: Integer;
  Item: TElTreeItem;
{$ifdef ELTREE_USE_STYLES}
  Ctrl: TElCellControl;
  i: integer;
{$endif}

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$define EL_COMMON_EDITORS}
{$endif}
begin
  result := false;
{$ifdef EL_COMMON_EDITORS}
  if FEditTimer <> nil then
     FEditTimer.Enabled := false;
{$endif}
  HCol := 0;
  Item := GetItemAt(X, Y, ItemPart, HCol);
  if (Item = nil) or (Item.Enabled) or (FOwner.IgnoreEnabled) then
  begin
    if (FOwner.FDblClickMode = dcmEdit) and FOwner.FCanEdit and (Item <> nil) and Item.AllowEdit and
       (ItemPart in [ipMainText, ipColumn]) and
       ((Item = FClickItem) or FOwner.QuickEditMode) then
      begin
        InitiateEditOp(Item, HCol, true);
      end
    else
    if ((ItemPart = ipMainText) or ((Item <> nil) and (FOwner.FFullRowSelect) and (ItemPart <> ipCheckBox))) and (FOwner.FDblClickMode = dcmExpand) then
    begin
      if Item.Expanded then
        Item.Expanded := false
      else
      begin
        Item.Expanded := true;
        FitMostChildren(Item);
      end;
    end;
    if (ItemPart = ipColumn) then
    begin
{$ifdef ELTREE_USE_STYLES}
      i := HCol;
      if i > FOwner.FMainTreeCol then Dec(i);
      if Item.UseStyles and (Item.StylesCount > HCol) then
      begin
        if not Item.Styles[i].OwnerProps then
        begin
          Ctrl := Item.Styles[i].Control;
          if Ctrl <> nil then
          begin
            Ctrl.TriggerDblClickEvent;
            exit;
          end;
        end;
      end;
{$endif}
{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
      if (FOwner.FCanEdit) and (FHeader.Sections[HCol].Editable) and
         (Item.Enabled or (FOwner.IgnoreEnabled)) and
         not (csDesigning in FOwner.ComponentState) then
         begin
           DoEditItem(Item, HCol);
         end;
{$endif}
{$endif}
    end;
    FClicked := false;
    FClickItem := nil;
    FClickSection := -2;
    FIgnoreClick2 := true;
    if (ItemPart <> ipButton) and (ItemPart <> ipCheckBox) then
      result := true;
    FIgnoreClick := false;
  end;
end;

function TElTreeView.IntRButtonDblClick(X, Y : integer; Shift : TShiftState): 
    Boolean;
var
  ItemPart: TSTItemPart;
  HCol: Integer;
  Item: TElTreeItem;
{$ifdef ELTREE_USE_STYLES}
  Ctrl: TElCellControl;
  i: integer;
{$endif}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$define EL_COMMON_EDITORS}
{$endif}
begin
  result := false;
{$ifdef EL_COMMON_EDITORS}
  if FEditTimer <> nil then
     FEditTimer.Enabled := false;
{$endif}
  HCol := 0;
  Item := GetItemAt(X, Y, ItemPart, HCol);
  if (Item = nil) or (Item.Enabled) or (FOwner.IgnoreEnabled) then
  begin
    if (ItemPart = ipColumn) then
    begin
{$ifdef ELTREE_USE_STYLES}
      i := HCol;
      if i > FOwner.FMainTreeCol then Dec(i);
      if Item.UseStyles and (Item.StylesCount > HCol) then
      begin
        if not Item.Styles[i].OwnerProps then
        begin
          Ctrl := Item.Styles[i].Control;
          if Ctrl <> nil then
          begin
            Ctrl.TriggerDblClickEvent;
            exit;
          end;
        end;
      end;
{$endif}
    end;
    result := true;
  end;
end;

{$ifdef CLX_USED}
function TElTreeView.WidgetFlags : Integer;
begin
  result := Integer(WidgetFlags_WRepaintNoErase);
end;
{$else}
procedure TElTreeView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;
{$endif}

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TElTreeView.OnEditTimer(Sender : TObject);
begin
  FEditTimer.Enabled := false;
  if FMouseSel or (FItemToEdit = nil) then exit;
  DoEditItem(FItemToEdit, FEditSect);
end;

function TElTreeView.SetupPictureEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
begin
  FEditing := true;
  FOwner.TriggerEditRequestEvent(Item, Section);
  FEditing := false;
  FEditingItem := nil;
  result := false;
end;

function TElTreeView.SetupCustomEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
begin
  FEditing := true;
  FOwner.TriggerEditRequestEvent(Item, Section);
  FEditing := false;
  FEditingItem := nil;
  result := false;
end;

function TElTreeView.SetupBlobEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
begin
  FEditing := true;
  FOwner.TriggerEditRequestEvent(Item, Section);
  FEditing := false;
  FEditingItem := nil;
  result := false;
end;

function TElTreeView.SetupFloatEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
begin
  result := SetupTextEditControl(Item, Section, Text, EditRect);
end;

function TElTreeView.SetupDateTimeEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
var SectionIndex : integer;
begin
{$ifdef ELTREE_USE_EXT_EDITORS}
  FInpEdit  := TElDateTimePicker.Create(Self);
  FFakePopup:= TPopupMenu.Create(self);
  FFakePopup.AutoPopup := false;
  FInpEdit.Parent := self;
  TElDateTimePicker(FInpEdit).Format := edfShortDateLongTime;
  TElDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  FInpEdit.Visible := false;
  try
    TElDateTimePicker(FInpEdit).DateTime := StrToDateTime(Text);
  except
    on E : EConvertError do
    begin
      TElDateTimePicker(FInpEdit).DateTime := Now;
    end;
  end;
  FFakePopup := TPopupMenu.Create(nil);
  FFakePopup.AutoPopup := false;
  TElDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  TElDateTimePicker(FInpEdit).OnExit := OnEditExit;
  TElDateTimePicker(FInpEdit).OnKeyDown := DoEditKeyDown;
  TElDateTimePicker(FInpEdit).Font.Assign(Font);
  Inc(EditRect.Bottom, 4);

  TElDateTimePicker(FInpEdit).BoundsRect := EditRect;
  if Section <> nil then
    SectionIndex := Section.Index
  else
    SectionIndex := -1;
  FOwner.TriggerTuneUpInplaceEditEvent(Item, SectionIndex, TCustomEdit(FInpEdit));
  result := true;
{$else}
  result := SetupTextEditControl(Item, Section, Text, EditRect);
{$endif}
end;

function TElTreeView.SetupDateEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
var SectionIndex : integer;
begin
{$ifdef ELTREE_USE_EXT_EDITORS}
  FInpEdit  := TElDateTimePicker.Create(Self);
  FFakePopup:= TPopupMenu.Create(self);
  FFakePopup.AutoPopup := false;
  FInpEdit.Parent := self;
  TElDateTimePicker(FInpEdit).Format := edfLongDate;
  TElDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  FInpEdit.Visible := false;
  try
    TElDateTimePicker(FInpEdit).DateTime := StrToDateTime(Text);
  except
    on E : EConvertError do
    begin
      TElDateTimePicker(FInpEdit).DateTime := Now;
    end;
  end;
  FFakePopup := TPopupMenu.Create(nil);
  FFakePopup.AutoPopup := false;
  TElDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  TElDateTimePicker(FInpEdit).OnExit := OnEditExit;
  TElDateTimePicker(FInpEdit).OnKeyDown := DoEditKeyDown;
  TElDateTimePicker(FInpEdit).Font.Assign(Font);
  Inc(EditRect.Bottom, 4);

  TElDateTimePicker(FInpEdit).BoundsRect := EditRect;
  result := true;
{$else}
  FInpEdit  := TDateTimePicker.Create(Self);
  TDateTimePicker(FInpEdit).Kind := dtkDate;
  FFakePopup:= TPopupMenu.Create(self);
  FFakePopup.AutoPopup := false;
  FInpEdit.Parent := self;
  TDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  FInpEdit.Visible := false;
  try
    TDateTimePicker(FInpEdit).Date := StrToDate(Text);
  except
    on E : EConvertError do
    begin
      TDateTimePicker(FInpEdit).Date := Now;
    end;
  end;
  FFakePopup := TPopupMenu.Create(nil);
  FFakePopup.AutoPopup := false;
  TDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  TDateTimePicker(FInpEdit).OnExit := OnEditExit;
  TDateTimePicker(FInpEdit).OnKeyDown := DoEditKeyDown;
  TDateTimePicker(FInpEdit).Font.Assign(Font);
  Inc(EditRect.Bottom, 4);

  TDateTimePicker(FInpEdit).BoundsRect := EditRect;
  result := true;
{$endif}
  if Section <> nil then
    SectionIndex := Section.Index
  else
    SectionIndex := -1;
  FOwner.TriggerTuneUpInplaceEditEvent(Item, SectionIndex, TCustomEdit(FInpEdit));
end;

function TElTreeView.SetupTimeEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
var SectionIndex : integer;
begin
{$ifdef ELTREE_USE_EXT_EDITORS}
  FInpEdit  := TElDateTimePicker.Create(Self);
  FFakePopup:= TPopupMenu.Create(self);
  FFakePopup.AutoPopup := false;
  FInpEdit.Parent := self;
  TElDateTimePicker(FInpEdit).Format := edfLongTime;
  TElDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  FInpEdit.Visible := false;
  try
    TElDateTimePicker(FInpEdit).DateTime := StrToDateTime(Text);
  except
    on E : EConvertError do
    begin
      TElDateTimePicker(FInpEdit).DateTime := Now;
    end;
  end;
  FFakePopup := TPopupMenu.Create(nil);
  FFakePopup.AutoPopup := false;
  TElDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  TElDateTimePicker(FInpEdit).OnExit := OnEditExit;
  TElDateTimePicker(FInpEdit).OnKeyDown := DoEditKeyDown;
  TElDateTimePicker(FInpEdit).Font.Assign(Font);
  Inc(EditRect.Bottom, 4);

  TElDateTimePicker(FInpEdit).BoundsRect := EditRect;
  result := true;
{$else}
  FInpEdit  := TDateTimePicker.Create(Self);
  TDateTimePicker(FInpEdit).Kind := dtkTime;
  FFakePopup:= TPopupMenu.Create(Self);
  FFakePopup.AutoPopup := false;
  FInpEdit.Parent := self;
  TDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  FInpEdit.Visible := false;
  try
    TDateTimePicker(FInpEdit).Time := StrToTime(Text);
  except
    on E : EConvertError do
    begin
      TDateTimePicker(FInpEdit).Time := Now;
    end;
  end;
  FFakePopup := TPopupMenu.Create(nil);
  FFakePopup.AutoPopup := false;
  TDateTimePicker(FInpEdit).PopupMenu := FFakePopup;
  TDateTimePicker(FInpEdit).OnExit := OnEditExit;
  TDateTimePicker(FInpEdit).OnKeyDown := DoEditKeyDown;
  TDateTimePicker(FInpEdit).Font.Assign(Font);
  Inc(EditRect.Bottom, 4);

  TDateTimePicker(FInpEdit).BoundsRect := EditRect;

  result := true;
{$endif}
  if Section <> nil then
    SectionIndex := Section.Index
  else
    SectionIndex := -1;
  FOwner.TriggerTuneUpInplaceEditEvent(Item, SectionIndex, TCustomEdit(FInpEdit));
end;

function TElTreeView.SetupNumericEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
begin
  result := SetupTextEditControl(Item, Section, Text, EditRect);
end;

function TElTreeView.SetupTextEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
var b : boolean;
var SectionIndex : integer;
begin
  FFakePopup := TPopupMenu.Create(nil);
  FFakePopup.AutoPopup := false;

{$ifdef ELTREE_USE_EXT_EDITORS}
  FInpEdit := EditBoxClass.Create(self);
  FInpEdit.Parent := self;
  FInpEdit.Visible := false;

  EditBoxClass(FInpEdit).HandleDialogKeys := true;
  EditBoxClass(FInpEdit).ButtonVisible    := false;
  EditBoxClass(FInpEdit).AltButtonVisible := false;
  EditBoxClass(FInpEdit).ActiveBorderType := FOwner.ActiveBorderType;
  if Item.Multiline then
     EditBoxClass(FInpEdit).AutoSize      := false;
  EditBoxClass(FInpEdit).Multiline        := Item.Multiline;

  EditBoxClass(FInpEdit).PopupMenu := FFakePopup;
  EditBoxClass(FInpEdit).BorderStyle := bsSingle;
  EditBoxClass(FInpEdit).ParentCtl3D := false;
  EditBoxClass(FInpEdit).Ctl3D := false;
  EditBoxClass(FInpEdit).OnExit := OnEditExit;
  EditBoxClass(FInpEdit).OnKeyDown := DoEditKeyDown;
  EditBoxClass(FInpEdit).AutoSize := false;
  EditBoxClass(FInpEdit).HandleDialogKeys := true;
  EditBoxClass(FInpEdit).Font.Assign(Font);

  b := EditBoxClass(FInpEdit).Font.Height < 0;
  if b then
     EditBoxClass(FInpEdit).Height := -Font.Height + 2
  else
     EditBoxClass(FInpEdit).Height := Font.Height + 2;
  EditBoxClass(FInpEdit).Text := Text;
  with EditBoxClass(FInpEdit) do
  begin
    Modified := false;
    SelStart := 0;
    SelLength := Length(Text);
    Height := Max(Height, EditRect.Bottom - EditRect.Top);
  end;
{$ELSE}
{$IFNDEF LITE}
  if Item.Multiline and (Pos(#13#10, Text) > 0) then
  begin
    FInpEdit := TMemo.Create(Self);
    FInpEdit.Parent := self;
    FInpEdit.Visible := false;

    TMemo(FInpEdit).PopupMenu := FFakePopup;
    TMemo(FInpEdit).BorderStyle := bsSingle;
    TMemo(FInpEdit).ParentCtl3D := false;
    TMemo(FInpEdit).Ctl3D := false;
    TMemo(FInpEdit).OnExit := OnEditExit;
    TMemo(FInpEdit).OnKeyDown := DoEditKeyDown;
    TMemo(FInpEdit).Font.Assign(Font);

    b := TMemo(FInpEdit).Font.Height < 0;
    if b then
       TMemo(FInpEdit).Height := -Font.Height + 2
    else
       TMemo(FInpEdit).Height := Font.Height + 2;
    TMemo(FInpEdit).Text := Text;
    with TMemo(FInpEdit) do
    begin
      Modified := false;
      SelStart := 0;
      SelLength := Length(Text);
      Height := Max(Height, EditRect.Bottom - EditRect.Top);
    end;
  end else
{$ENDIF}
  begin
//  FInpEdit := TEdit.Create(Self);
    FInpEdit := TElIntEdit.Create(Self); // CNS this one gives RETURN key
    FInpEdit.Parent := self;
    FInpEdit.Visible := false;

    TEdit(FInpEdit).PopupMenu := FFakePopup;
    TEdit(FInpEdit).BorderStyle := bsSingle;
    TEdit(FInpEdit).ParentCtl3D := false;
    TEdit(FInpEdit).Ctl3D := false;
    TEdit(FInpEdit).OnExit := OnEditExit;
    TEdit(FInpEdit).OnKeyDown := DoEditKeyDown;
    TEdit(FInpEdit).AutoSize := false;
    TEdit(FInpEdit).Font.Assign(Font);

    b := TEdit(FInpEdit).Font.Height < 0;
    if b then
       TEdit(FInpEdit).Height := -Font.Height + 2
    else
       TEdit(FInpEdit).Height := Font.Height + 2;
    TEdit(FInpEdit).Text := Text;
    with TEdit(FInpEdit) do
    begin
      Modified := false;
      SelStart := 0;
      SelLength := Length(Text);
      Height := Max(Height, EditRect.Bottom - EditRect.Top);
    end;
  end;
{$ENDIF}

  if Section <> nil then
    SectionIndex := Section.Index
  else
    SectionIndex := -1;
  FOwner.TriggerTuneUpInplaceEditEvent(Item, SectionIndex, TCustomEdit(FInpEdit));

  FInpEdit.BoundsRect := EditRect;
  result := true;
end;

function TElTreeView.SetupBoolEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;
var SectionIndex : integer;
begin
  FInpEdit := CheckBoxClass.Create(Self);

  FInpEdit.Parent := self;
  FInpEdit.Visible := false;

  CheckBoxClass(FInpEdit).AllowGrayed := false;
  CheckBoxClass(FInpEdit).Caption := '';
  CheckBoxClass(FInpEdit).BoundsRect := EditRect;
  CheckBoxClass(FInpEdit).OnExit := OnEditExit;
  CheckBoxClass(FInpEdit).OnKeyDown := DoEditKeyDown;
  CheckBoxClass(FInpEdit).Checked := Text <> '';
{$ifdef ELTREE_USE_EXT_EDITORS}
  CheckBoxClass(FInpEdit).Flat := FOwner.Flat;
  CheckBoxClass(FInpEdit).UseCustomGlyphs := FOwner.CustomCheckboxes;
  if FOwner.CustomCheckboxes then
     CheckBoxClass(FInpEdit).Glyph := FOwner.CheckBoxGlyph;
{$ENDIF}
  if Section <> nil then
    SectionIndex := Section.Index
  else
    SectionIndex := -1;
  FOwner.TriggerTuneUpInplaceEditEvent(Item, SectionIndex, TCustomEdit(FInpEdit));

  result := true;
end;

function TElTreeView.SetupCurrencyEditControl(Item : TElTreeItem; Section :
    TElHeaderSection; Text : string; EditRect : TRect): Boolean;
var SectionIndex : integer;
begin
{$ifdef ELTREE_USE_EXT_EDITORS}
  FFakePopup := TPopupMenu.Create(nil);
  FFakePopup.AutoPopup := false;
  FInpEdit := TElCurrencyEdit.Create(Self);
  FInpEdit.Parent := self;
  FInpEdit.Visible := false;

  TElCurrencyEdit(FInpEdit).ActiveBorderType := FOwner.ActiveBorderType;

  TElCurrencyEdit(FInpEdit).PopupMenu := FFakePopup;
  TElCurrencyEdit(FInpEdit).BorderStyle := bsSingle;
  TElCurrencyEdit(FInpEdit).ParentCtl3D := false;
  TElCurrencyEdit(FInpEdit).Ctl3D := false;
  TElCurrencyEdit(FInpEdit).OnExit := OnEditExit;
  TElCurrencyEdit(FInpEdit).OnKeyDown := DoEditKeyDown;
  TElCurrencyEdit(FInpEdit).Font.Assign(Font);

  if Section <> nil then
     FOwner.TriggerTuneUpInplaceEditEvent(Item, Section.Index, TCustomEdit(FInpEdit));

  TElCurrencyEdit(FInpEdit).Value := PrettyStrToCurr(Text);
  FInpEdit.BoundsRect := EditRect;
  result := true;

  if Section <> nil then
    SectionIndex := Section.Index
  else
    SectionIndex := -1;
  FOwner.TriggerTuneUpInplaceEditEvent(Item, SectionIndex, TCustomEdit(FInpEdit));
{$ELSE}
  result := SetupTextEditControl(Item, Section, Text, EditRect);
{$ENDIF}
end;

function TElTreeView.SetupEnumEditControl(Item : TElTreeItem; Section : TElHeaderSection; Text : string; EditRect : TRect) : boolean;

begin
  FInpEdit := ComboBoxClass.Create(self);
{$ifdef ELTREE_USE_EXT_EDITORS}
  ComboBoxClass(FInpEdit).ActiveBorderType := FOwner.ActiveBorderType;
{$ENDIF}
  FFakePopup := TPopupMenu.Create(nil);
  FFakePopup.AutoPopup := false;
  FInpEdit.Parent := self;
  ComboBoxClass(FInpEdit).PopupMenu := FFakePopup;
  FInpEdit.Visible := false;
  ComboBoxClass(FInpEdit).Style := csDropDownList;
  ComboBoxClass(FInpEdit).ParentCtl3D := false;
  ComboBoxClass(FInpEdit).Ctl3D := false;
  ComboBoxClass(FInpEdit).OnExit := OnEditExit;
  ComboBoxClass(FInpEdit).OnKeyDown := DoEditKeyDown;
  ComboBoxClass(FInpEdit).ItemIndex := 0;
  ComboBoxClass(FInpEdit).Text := Text;
  ComboBoxClass(FInpEdit).BoundsRect := EditRect;
  FOwner.TriggerComboEditShowEvent(Item, Section, ComboBoxClass(FInpEdit));
  ComboBoxClass(FInpEdit).Font.Assign(Font);

  if ComboBoxClass(FInpEdit).Font.Height < 0 then
     ComboBoxClass(FInpEdit).Font.Height := -(-Font.Height - 1)
  else
     ComboBoxClass(FInpEdit).Font.Height := Font.Height - 1;

  result := true;
end;

function TElTreeView.SetupEditControl(Item : TElTreeItem; Section : TElHeaderSection; FT : TElFieldType) : boolean;
var s  : string;
    sn : integer;
    R  : TRect;
    Left : integer;
begin
  R := GetItemRect(FVisible.IndexOf(Item));

  if (Section = nil) or (Section.Index = FOwner.FMainTreeCol) then
  begin
    s := Item.Text;
    with FOwner do
    begin
      if (Item.FTextLeft < FHPos) or (Item.FTextLeft > FHPos + Self.Width) then
         SetHPosition(Item.FTextLeft);
      R.Left := Item.FTextLeft - FHPos;
      if Section <> nil then
         R.Right := Section.Right
      else
         R.Right := ClientWidth - 1;
      if R.Right >= ClientWidth then
         R.Right := ClientWidth - 1;
    end;
  end
  else
  begin
    sn := Section.Index;
    if sn > FOwner.MainTreeColumn then dec(sn);
    if Item.ColumnText.Count <= sn then
       s := ''
    else
       s := Item.ColumnText[sn];
    Left := Section.Left;
    if (Left < FOwner.FHPos) or (Left > FOwner.FHPos + ClientWidth) then
       FOwner.SetHPosition(Left);
    R.Left := Left - FOwner.FHPos;
    R.Right := R.Left + Section.Width;
    if R.Right >= ClientWidth then
       R.Right := ClientWidth - 1;
  end;

  case FT of
    sftPicture: result := SetupPictureEditControl(Item, Section, s, R);
    sftCustom : result := SetupCustomEditControl(Item, Section, s, R);
    sftBLOB   : result := SetupBlobEditControl(Item, Section, s, R);
    sftEnum   : result := SetupEnumEditControl(Item, Section, s, R);
    sftDate   : result := SetupDateEditControl(Item, Section, s, R);
    sftTime   : result := SetupTimeEditControl(Item, Section, s, R);
    sftDateTime: result := SetupDateTimeEditControl(Item, Section, s, R);
    sftText   : result := SetupTextEditControl(Item, Section, s, R);
    sftFloating,
    sftNumber : result := SetupNumericEditControl(Item, Section, s, R);
    sftBool   : result := SetupBoolEditControl(Item, Section, s, R);
    sftCurrency: result := SetupCurrencyEditControl(Item, Section, s, R);
    else
      result := false;
  end; // case
end;

procedure TElTreeView.DoEditItem;
var
  Section: TElHeaderSection;
  b: boolean;
{$ifdef ELTREE_USE_STYLES}
  Style: TElCellStyle;
  i: integer;
{$endif}
  FT: TElFieldType;

begin
  if Item = nil then
    raise EElTreeError.Create(STexInvItem);
  if FVisible.IndexOf(Item) = -1 then
    FOwner.EnsureVisible(Item);
  if FInpEdit <> nil then
  begin
    if FInpEdit.Visible then
      DoEndEdit(true);
    FInpEdit.Parent := nil;
    if FFakePopup <> nil then
      FFakePopup.Free;
    FFakePopup := nil;
    FInpEdit.Free;
    FInpEdit := nil;
  end;
  if FOwner.ShowColumns and (SectionNum = -1) then
     SectionNum := FOwner.MainTreeColumn;
  FEditingItem := Item;
  FEditSect := SectionNum;
  if (SectionNum <> -1) then
  begin
    Section := FHeader.Sections[SectionNum];
    if (not Section.Visible) then
    begin
      FEditingItem := nil;
      exit;
    end;
  end
  else
    Section := nil;
{$ifdef ELTREE_USE_STYLES}
  if Item.UseStyles then
  begin
    if FOwner.VirtualityLevel = vlNone then
    begin
      if (SectionNum = FOwner.MainTreeColumn) or (SectionNum = -1) then
         Style := Item.MainStyle
      else
      begin
        i := SectionNum;
        if i > FOwner.MainTreeColumn then dec(i);
        if (Item.FStaticData.FStyles = nil) or (Item.FStaticData.FStyles.Count <= i) then
          Style := Item.MainStyle
        else
          Style := Item.FStaticData.FStyles[i];
      end;
    end
    else
    begin
      FOwner.TriggerVirtualStyleNeeded(Item, SectionNum, VirtStyle);
      Style := VirtStyle;
    end;
    FT := Style.CellType;
  end
  else
{$endif}
  begin
    if Section <> nil then
      FT := Section.FieldType
    else
      FT := FOwner.MainTextType;
  end;
  b := true;
  FOwner.TriggerTryEditEvent(Item, Section, FT, b);
  if not b then
  begin
    FEditingItem := nil;
    exit;
  end;
  FEditType := FT;
  if not SetupEditControl(Item, Section, FT) then
     exit;

  FOldHide := FOwner.HideSelection;
  FOwner.FHideSelect := false;

  FInpEdit.Visible := true;
  try (*<+>*)
    if FInpEdit.CanFocus then (*<+>*)
      FInpEdit.SetFocus;
  except (*<+>*)
  end; (*<+>*)
  FEditing := true;
end;
{$endif}
{$endif}

{$ifdef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TElTreeView.OnEditTimer(Sender : TObject);
begin
  FEditTimer.Enabled := false;
  if FMouseSel or (FItemToEdit = nil) then exit;
  DoEditItem(FItemToEdit, FEditSect);
end;

procedure TElTreeView.DoEditItem(Item: TElTreeItem; SectionNum: integer);
var
  Section: TElHeaderSection;
  R: TRect;
  Left : integer;
{$ifdef ELTREE_USE_STYLES}
  Style: TElCellStyle;
  i: integer;
{$endif}
  FT: TElFieldType;
  Editor : TElTreeInplaceEditor;
  s : TElFString;
  sn : integer;
  b  : boolean;                
begin
  if Item = nil then
    raise EElTreeError.Create(STexInvItem);
  if FVisible.IndexOf(Item) = -1 then
    FOwner.EnsureVisible(Item);
  if FInpEdit <> nil then
    DoEndEdit(true);
  
  if FOwner.ShowColumns and (SectionNum = -1) then
     SectionNum := FOwner.MainTreeColumn;
  FEditingItem := Item;
  if (SectionNum <> -1) then
  begin
    Section := FHeader.Sections[SectionNum];
    if (not Section.Visible) then
    begin
      FEditingItem := nil;
      exit;
    end;
  end
  else
    Section := nil;
{$ifdef ELTREE_USE_STYLES}
  if Item.UseStyles then
  begin
    if FOwner.VirtualityLevel = vlNone then
    begin
      if (SectionNum = FOwner.MainTreeColumn) or (SectionNum = -1) then
         Style := Item.MainStyle
      else
      begin
        i := SectionNum;
        if i > FOwner.MainTreeColumn then dec(i);
        if (Item.FStaticData.FStyles = nil) or (Item.FStaticData.FStyles.Count <= i) then
          Style := Item.MainStyle
	else
          Style := Item.FStaticData.FStyles[i];
      end;
    end
    else
    begin
      FOwner.TriggerVirtualStyleNeeded(Item, SectionNum, VirtStyle);
      Style := VirtStyle;
    end;
    FT := Style.CellType;
  end else
{$endif}
  begin
    if Section <> nil then
      FT := Section.FieldType
    else
      FT := FOwner.MainTextType;
  end;

  b := true;
  FOwner.TriggerTryEditEvent(Item, SectionNum, FT, b);
  if not b then exit;
  FOwner.TriggerInplaceEditorNeeded(Item, SectionNum, FT, Editor);

  if Editor = nil then
    exit
  else
  begin
    if (Section = nil) or (Section.Index = FOwner.FMainTreeCol) then
    begin
      s := Item.Text;
    end
    else
    begin
      if Item.FStaticData <> nil then
      begin
        sn := Section.Index;
        if sn > FOwner.MainTreeColumn then
          dec(sn);
        if Item.ColumnText.Count <= sn then
           s := ''
        else
           s := Item.ColumnText[sn];
      end
      else
        FOwner.TriggerVirtualTextNeeded(Item, Section.Index, s);
    end;
    R := GetItemRect(FVisible.IndexOf(Item));
    if (Section = nil) or (not FOwner.ShowColumns) then
    begin
      if (Item.FTextLeft < FOwner.FHPos) or (Item.FTextLeft > FOwner.FHPos + Self.Width) then
         SetHPosition(Item.FTextLeft);
      if FOwner.RightAlignedTree then
      begin
        R.Right := Item.FTextRight - FOwner.FHPos;
        R.Left := 0;
      end
      else
      begin
        R.Left := Item.FTextLeft - FOwner.FHPos;
        R.Right := ClientWidth - 1;
      end;
    end
    else
    begin
      if FOwner.RightAlignedTree then
      begin
        if SectionNum = FOwner.FMainTreeCol then
          Left  := Item.FTextRight - ClientWidth
        else
          Left := Section.Right - ClientWidth;
        if (Left < FOwner.FHPos) or (Left > FOwner.FHPos + ClientWidth) then
           FOwner.SetHPosition(Left);
        R.Right := Left + ClientWidth - FOwner.FHPos;
        R.Left := Section.Left - FOwner.FHPos;
        if R.Left < 0 then
          R.Left := 0;
      end
      else
      begin
        if SectionNum = FOwner.FMainTreeCol then
          Left := Item.FTextLeft
        else
          Left := Section.Left;
        if (Left < FOwner.FHPos) or (Left > FOwner.FHPos + ClientWidth) then
           FOwner.SetHPosition(Left);
        R.Left := Left - FOwner.FHPos;
        R.Right := Section.Right - FOwner.FHPos;
        if R.Right >= ClientWidth then
           R.Right := ClientWidth - 1;
      end;
    end;

    FInpEdit := Editor;
    Editor.Tree := FOwner;
    Editor.FValueAsText := S;
    Editor.FItem := Item;
    Editor.FSectionIndex := SectionNum;
    Editor.FDatatype := FT;
    Editor.FCellRect := R;
    Editor.StartOperation;
  end;
end;

procedure TElTreeView.DoEndEdit;
begin
  if FInpEdit <> nil then
  begin
    FInpEdit.CompleteOperation(not ByCancel);
  end;
end;

procedure TElTreeView.EditOperationCancelled;
begin
  FInpEdit := nil;
end;

procedure TElTreeView.EditOperationAccepted;
var si : integer;
begin
  if FOwner.VirtualityLevel = vlNone then
  begin
    si := FInpEdit.FSectionIndex;
    if (si = FOwner.MainTreeColumn) or (si = -1) then
      FEditingItem.Text := FInpEdit.FValueAsText
    else
    begin
      if si > FOwner.MainTreeColumn then
        dec(si);
      while FEditingItem.FStaticData.FColText.Count <= si do
        FEditingItem.FStaticData.FColText.Add('');
      FEditingItem.FStaticData.FColText[si] := FInpEdit.FValueAsText;
    end;
  end;
  FInpEdit := nil;
end;

procedure TCustomElTree.EditItem(Item: TElTreeItem; SectionNum: integer);
begin
  if not FCanEdit then exit;
  try
    with FView do
      if FInpEdit <> nil then
        EndEdit(true);
  except
  end;
  FView.DoEditItem(Item, SectionNum);
end;

procedure TCustomElTree.EndEdit(ByCancel: boolean);
begin
  FView.DoEndEdit(ByCancel);
end;
{$endif}
{$endif}

procedure TElTreeView.DoEndDrag(Target: TObject; X, Y: Integer);
var
  TSI: TElTreeItem;
begin
  inherited DoEndDrag(Target, X, Y);
  {$ifndef CLX_USED}
  FDragImages.Free;
  FDragImages := nil;
  {$endif}
  TSI := FDropTrg;
  FDropTrg := nil;
  if TSI <> nil then
    TSI.RedrawItem(true);
end;

{$ifndef CLX_USED}
procedure TElTreeView.WMRButtonUp(var Msg: TWMRButtonUp);
begin
  if IntRButtonUp(Msg.XPos, Msg.YPos, KeyDataToShiftState(Msg.Keys)) then
    inherited;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if IntLButtonUp(Message.XPos, Message.YPos, KeyDataToShiftState(Message.Keys)) then
    inherited;
end;
{$endif}

procedure TElTreeView.MouseMove(Shift: TShiftState; X, Y: Integer); { protected }
{$ifdef ELTREE_USE_STYLES}
var
  ItemPart: TSTItemPart;
  HCol: Integer;
  Item: TElTreeItem;
  Ctrl: TElCellControl;
  CX, CY: integer;
  i, j: integer;
{$endif}
begin
{$ifdef ELTREE_USE_STYLES}
  HCol := 0;
  Item := GetItemAt(X, Y, ItemPart, HCol);
  if (ItemPart = ipColumn) then
  begin
    i := HCol;
    j := i;
    if i > FOwner.FMainTreeCol then Dec(i);
    if Item.UseStyles and (Item.StylesCount > HCol) then
    begin
      CY := X;
      CX := Y;
      with FOwner do
        cy := (cy div LineHeight) * LineHeight;
      if not Item.Styles[i].OwnerProps then
      begin
        Ctrl := Item.Styles[i].Control;
        if Ctrl <> nil then
        begin
          Ctrl.TriggerMouseMoveEvent(GetShiftState, CX - FOwner.FHPos - FHeader.Sections[j].Left, CY);
          exit;
        end;
      end;
    end;
  end;
{$endif}
  inherited;
end; { MouseMove }

{$ifndef CLX_USED}
procedure TElTreeView.WMRButtonDblClk(var Msg: TWMRButtonDblClk); { private }
begin
  if IntRButtonDblClick(Msg.XPos, Msg.YPos, KeyDataToShiftState(Msg.Keys)) then
    inherited;
end; { WMRButtonDblClk }
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if IntLButtonDblClick(Message.XPos, Message.YPos, KeyDataToShiftState(Message.Keys)) then
    inherited;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.WMLButtonDown(var Message: TWMLButtonDown);
begin
  IntLButtonDown(Message.XPos, Message.YPos, KeyDataToShiftState(Message.Keys));
  inherited;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.WMRButtonDown(var Message: TWMRButtonDown);
begin
  // this is also in EventFilter
{$ifdef EL_COMMON_EDITORS}
  if FEditTimer <> nil then
     FEditTimer.Enabled := false;
{$endif}
  DoHideLineHint;
  if FMouseSel then
    CancelMouseSel;

  IntRButtonDown(Message.XPos, Message.YPos, KeyDataToShiftState(Message.Keys));
  inherited;
end;
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  IntMouseMove(Message.XPos, Message.YPos, KeyDataToShiftState(Message.Keys));
end;
{$endif}

procedure TElTreeView.DoStartDrag(var DragObject: TDragObject);
begin
  {$ifndef CLX_USED}
  DragCursor := FOwner.DragCursor;
  {$endif}
  
  FDropTrg := nil;
  {$ifndef CLX_USED}
  if (FSelected <> nil) or ((FOwner.FSelectedList <> nil) and (FOwner.FSelectedList.Count > 0)) then
     FillDragImage;
  if (FDragImages <> nil) then
  begin
    with FOwner do
    if FShowHeader then
      FDragImages.SetDragImage(0, FPressCoord.X + FHPos - FHeader.Sections[FMainTreeCol].Left, FPressCoord.y - fddy)
    else
      FDragImages.SetDragImage(0, FPressCoord.X + FHPos, FPressCoord.y - fddy);
  end;
  {$endif}
  inherited DoStartDrag(DragObject);
end;

procedure TElTreeView.OnScrollTimer(Sender : TObject);
var P : TPoint;
    Y : integer;
begin
  FDragScrollTimer.Enabled := false;
  FDragScrollTimer.Interval := FOwner.DragScrollInterval;
  if FVisible.Count > 0 then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);
    y := P.Y;
    if ((FOwner.TopIndex > 0) and (Y <= TElTreeItem(FVisible[0]).GetHeight)) then // CNS div by 2 removed
    // if ((FOwner.TopIndex > 0) and (Y <= TElTreeItem(FVisible[0]).GetHeight shr 1)) then
    begin
      if FOwner.FDragObject <> nil then FOwner.FDragObject.HideDragImage;
      SetVPosition(FOwner.TopIndex - 1);
      if FOwner.FDragObject <> nil then FOwner.FDragObject.ShowDragImage;
      FDragScrollTimer.Enabled := true;
    end else
    if ((FOwner.BottomIndex <= FOwner.TotalVisCount) and (Y >= Height - TElTreeItem(FVisible.Last).GetHeight)) then // CNS div by 2 removed
    // if ((FOwner.BottomIndex <= FOwner.TotalVisCount) and (Y >= Height - TElTreeItem(FVisible.Last).GetHeight shr 1)) then
    begin
      if FOwner.FDragObject <> nil then FOwner.FDragObject.HideDragImage;
      SetVPosition(FOwner.TopIndex + 1);
      if FOwner.FDragObject <> nil then FOwner.FDragObject.ShowDragImage;
      FDragScrollTimer.Enabled := true;
    end;
  end;
end;

function TElTreeView.DragScroll; { protected }
var
  NewY: integer;
begin
  result := false;
  NewY := 0;
  if (Y < NewY + (FOwner.FLineHeight shr 1)) and (FOwner.TopIndex > 0) then
  begin
    Source.HideDragImage;
    result := true;
    SetVPosition(FOwner.TopIndex - 1);
  end else
    if (Y > Height - (FOwner.FLineHeight shr 1)) and (FOwner.BottomIndex <= FOwner.TotalVisCount) then
    begin
      Source.HideDragImage;
      result := true;
      SetVPosition(FOwner.TopIndex + 1);
    end;
end; { DragScroll }

{$ifdef CLX_USED}
procedure TElTreeView.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
begin
  inherited;
  DoDragOver(nil, X, Y, Accept);
end;
{$endif}

procedure TElTreeView.DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
var
  TSI: TElTreeItem;
  R  : TRect;
begin
  //TDragObject(Source).HideDragImage;
  if (FHintTimer <> nil) or (FHintItem <> nil) then
    DoHideLineHint;

  if FVisible.Count > 0 then
    if ((FOwner.TopIndex > 0) and
        (Y <= TElTreeItem(FVisible[0]).GetHeight)) or
        // (Y <= TElTreeItem(FVisible[0]).GetHeight shr 1)) or
       ((FOwner.BottomIndex <= FOwner.TotalVisCount) and
        (Y >= Height - TElTreeItem(FVisible.Last).GetHeight)) then
        // (Y >= Height - TElTreeItem(FVisible.Last).GetHeight shr 1)) then
    begin
      if FDragScrollTimer = nil then
      begin
        FDragScrollTimer := TTimer.Create(self);
        FDragScrollTimer.Enabled := false;
      end;
      if not FDragScrollTimer.Enabled then
      begin
        FDragScrollTimer.OnTimer := OnScrollTimer;
        FDragScrollTimer.Interval := 1000;//FOwner.DragScrollInterval * 2;
        FDragScrollTimer.Enabled := true;
      end;
    end;
  //fh := DragScroll(Source, X, Y);

  FDropAcc := CanDrop;
  TSI := FDropTrg;

  FDropTrg := GetItemAtY(Y);
  if Assigned(FDropTrg) and not (FOwner.IgnoreEnabled or FDropTrg.Enabled) then
    FDropTrg := nil;
  if TSI = FDropTrg then
  begin
    // if there is no code that might change DragImgDrawMode,
    // we have nothing more to do so we can leave the method
    if not Assigned(FOwner.OnDragTargetChange) then
      exit;
  end;
  {$ifndef CLX_USED}
  Source.HideDragImage;
  try
  {$endif}
    if TSI <> nil then
      TSI.RedrawItem(false);

    if Assigned(FOwner.OnDragTargetChange) then
    begin
      R := FOwner.GetItemRect(FVisible.IndexOf(FDropTrg));
      FOwner.OnDragTargetChange(FOwner, FDropTrg, R, X, Y + Top);
    end;

    if FDropTrg <> nil then
    begin
      if FOwner.FExpandOnDragOver then
      begin
        if FOwner.FDragExpandDelay = 0 then
        begin
          if FOwner.FDragObject <> nil then
            FOwner.FDragObject.HideDragImage;
          FDropTrg.Expand(false);
          Update;
          if FOwner.FDragObject <> nil then
            FOwner.FDragObject.ShowDragImage;
        end
        else
        begin
          if FDragExpandTimer = nil then
            FDragExpandTimer := TTimer.Create(self);
          with FDragExpandTimer do
          begin
            Enabled := false;
            OnTimer := OnDragExpandTimer;
            Interval:= FOwner.FDragExpandDelay;
            Enabled := true;
          end;
        end;
      end;
      FDropTrg.RedrawItem(false);
    end;
    Update;
    {$ifndef CLX_USED}
  finally
    Source.ShowDragImage;
  end;
    {$endif}
end;

function TElTreeView.GetItemAt;
var
  dX, i: integer;
  odx  : integer;
  io   : integer;
begin
  result := GetItemAtY(Y);
  ItemPart := ipOutside;
  if Result = nil then exit;
  with FOwner do
  begin
    if RightAlignedTree then
    begin
      dX := X + FHPos;
      odx := dX;
      if FShowHeader then
        for i := 0 to FHeader.Sections.Count - 1 do
        begin
{$IFNDEF LITE}
          if FHeader.Sections[i] = FHeader.LockedSection then
          begin
            odx := dx;
            dx := dx - FHeader.LeftPos;
          end else
{$ENDIF}
          begin
            dx := odx;
          end;
          if (dX >= FHeader.Sections[i].Left) and (dX < FHeader.Sections[i].Right) then
          begin
            HitColumn := i;
            if HitColumn <> FMainTreeCol then
            begin
              ItemPart := ipColumn;
              exit;
            end else
            begin
              ItemPart := ipInside;
              break;
            end;
          end; // if
        end; // for
      if (dX >= Result.FTextLeft) and (dX <= Result.FTextRight + ItemExt div 3 - 1) then
      begin
        ItemPart := ipMainText;
        HitColumn := MainTreeColumn;
        exit;
      end; // else ItemPart := ipInside;
      if ((Result.FBoolData1 and ibfImageDrawn) = ibfImageDrawn) then
      begin
        i := Result.FTextRight + FImages.Width + ItemExt div 3 - 1;
        if (dX >= i - FImages.Width) and (dX < i) then
        begin
          ItemPart := ipPicture;
          HitColumn := MainTreeColumn;
          exit;
        end;
        if ((Result.FBoolData1 and ibfImageDrawn2) = ibfImageDrawn2) or
           (Result.ShowCheckBox and FOwner.ShowCheckboxes) then
          inc(i, ItemExt div 3);
      end
      else
        i := Result.FTextRight + ItemExt div 3 - 1;
      if ((Result.FBoolData1 and ibfImageDrawn2) = ibfImageDrawn2) then
      begin
        io := 0;
        if FImages2 <> nil then
        begin
          inc(i, FImages2.Width);
          if (dX >= i - FImages2.Width) and (dX < i + io) then
          begin
            ItemPart := ipPicture2;
            HitColumn := MainTreeColumn;
            exit;
          end;
        end else
        begin
          inc(i, FImages.Width);
          if (dX >= i - FImages.Width) and (dX < i) then
          begin
            ItemPart := ipPicture2;
            HitColumn := MainTreeColumn;
            exit;
          end;
        end;
      end;
      if FShowCheckBoxes and ((Result.FBoolData1 and ibfShowCheckBox) = ibfShowCheckBox) then
      begin
        inc(i, {ItemExt div 3} - 1);
        if FCustomCheckboxes then
        begin
          if Result.FCheckBoxType = ectRadioButton then
          begin
            if (dX >= i) and (dX < i + FRadioButtonGlyph.Width div 6) then
            begin
              ItemPart := ipCheckBox;
              HitColumn := MainTreeColumn;
              exit;
            end;
            inc(i, FRadioButtonGlyph.Width div 6);
          end
          else
          begin
            if (dX >= i) and (dX < i + FCheckBoxGlyph.Width div 6) then
            begin
              ItemPart := ipCheckBox;
              HitColumn := MainTreeColumn;
              exit;
            end;
            inc(i, FCheckBoxGlyph.Width div 6);
          end;
        end
        else
        begin
          if (dX >= i) and (dX < i + CheckBoxSize) then
          begin
            ItemPart := ipCheckBox;
            HitColumn := MainTreeColumn;
            exit;
          end;
          inc(i, CheckBoxSize);
        end;
      end;
      if not (ShowButtons) or (not (Result.HasVisibleChildren or Result.ForceButtons or ShowLeafButton)) then exit;
      if (dX >= i) and (dX < i + ItemExt) then
      begin
        if Result.SuppressButtons then
          ItemPart := ipInside
        else
          ItemPart := ipButton;
        HitColumn := MainTreeColumn;
        exit;
      end;
    end
    else
    begin
      dX := X + FHPos;
      odx := dx;

      if FShowHeader then
        for i := 0 to FHeader.Sections.Count - 1 do
        begin
{$IFNDEF LITE}
          if FHeader.Sections[i] = FHeader.LockedSection then
          begin
            odx := dx;
            dx := dx - FHeader.LeftPos;
          end else
{$ENDIF}
          begin
            dx := odx;
          end;
          if (dX >= FHeader.Sections[i].Left) and (dX < FHeader.Sections[i].Right) then
          begin
            HitColumn := i;
            if HitColumn <> FMainTreeCol then
            begin
              ItemPart := ipColumn;
              exit;
            end
            else
            begin
              ItemPart := ipInside;
              break;
            end;
          end; // if
        end; // for
      if (dX >= Result.FTextLeft - (ItemExt div 3) + 1) and (dX <= Result.FTextRight) then
      begin
        ItemPart := ipMainText;
        HitColumn := MainTreeColumn;
        exit;
      end; // else ItemPart := ipInside;
      if ((Result.FBoolData1 and ibfImageDrawn) = ibfImageDrawn) then
      begin
        i := Result.FTextLeft - FImages.Width - (ItemExt div 3) + 1;
        if (dX < i + FImages.Width) and (dX >= i) then
        begin
          ItemPart := ipPicture;
          HitColumn := MainTreeColumn;
          exit;
        end;
        if ((Result.FBoolData1 and ibfImageDrawn2) = ibfImageDrawn2) or
           (Result.ShowCheckBox and FOwner.ShowCheckboxes) then
          dec(i, ItemExt div 3);
      end
      else
        i := Result.FTextLeft - (ItemExt div 3) + 1;
      if ((Result.FBoolData1 and ibfImageDrawn2) = ibfImageDrawn2) then
      begin
        io := 0;
        if FImages2 <> nil then
        begin
          dec(i, FImages2.Width);
          if (dX < i + FImages2.Width + io) and (dX >= i) then
          begin
            ItemPart := ipPicture2;
            HitColumn := MainTreeColumn;
            exit;
          end;
        end
        else
        begin
          dec(i, FImages.Width);
          if (dX < i + FImages.Width) and (dX >= i) then
          begin
            ItemPart := ipPicture2;
            HitColumn := MainTreeColumn;
            exit;
          end;
        end;
      end;
      if FShowCheckBoxes and ((Result.FBoolData1 and ibfShowCheckBox) = ibfShowCheckBox) then
      begin
        dec(i, {ItemExt div 3} - 1);
        if FCustomCheckboxes then
        begin
          if Result.FCheckBoxType = ectRadioButton then
          begin
            if (dX < i) and (dX >= i - FRadioButtonGlyph.Width div 6) then
            begin
              ItemPart := ipCheckBox;
              HitColumn := MainTreeColumn;
              exit;
            end;
            dec(i, FRadioButtonGlyph.Width div 6);
          end else
          begin
            if (dX < i) and (dX >= i - FCheckBoxGlyph.Width div 6) then
            begin
              ItemPart := ipCheckBox;
              HitColumn := MainTreeColumn;
              exit;
            end;
            dec(i, FCheckBoxGlyph.Width div 6);
          end;
        end
        else
        begin
          if (dX < i) and (dX >= i - CheckBoxSize) then
          begin
            ItemPart := ipCheckBox;
            HitColumn := MainTreeColumn;
            exit;
          end;
          dec(i, CheckBoxSize);
        end;
      end;
      if not (ShowButtons) or (not (Result.HasVisibleChildren or Result.ForceButtons or ShowLeafButton)) then exit;
      if (dX < i) and (dX >= i - ItemExt) then
      begin
        if Result.SuppressButtons then
          ItemPart := ipInside
        else
          ItemPart := ipButton;
        HitColumn := MainTreeColumn;
        exit;
      end;
    end;
  end;
end;

function TElTreeView.GetItemAtY(y: integer): TElTreeItem;
var
  i : integer;
  j : integer;
begin
  result := nil;
  if Y < 0 then exit;
  j := 0;
  for i := 0 to FVisible.Count - 1 do
  begin
    Inc(J, TElTreeItem(FVisible[i]).GetHeight);
    if Y < J then
    begin
      result := TElTreeItem(FVisible[i]);
      exit;
    end;
  end;
end;

function TElTreeView.GetItemRect;
var i, j : integer;
begin
  if (not InRange(0, FVisible.Count - 1, ItemIndex)) or (csDestroying in ComponentState) then
  begin
    SetRectEmpty(result);
    exit;
  end;
  j := 0;
  for i := 0 to ItemIndex - 1 do
    Inc(J, TElTreeItem(FVisible[i]).GetHeight);
  Result := Rect(0, j, Self.Width, j + TElTreeItem(FVisible[ItemIndex]).GetHeight);
end;

procedure TElTreeView.ProcessPassedClick;
var Item,
    FS : TElTreeItem;
    FFS  : boolean;
begin
  Item := FPassedItem;
  FClickPassed := false;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
  if FOwner.IsEditing then exit;
{$endif}
  with FOwner do
    if FMultiSelect then IsUpdating := true;
  FFS := false;
  if (FOwner.FSelMode = smUsual) and (FOwner.FMultiSelect) then
  begin
    if (not (ssCtrl in FPassedShift)) and (not (ssShift in FPassedShift)) then
    begin
      with FOwner do
      begin
        if ItemFocused <> nil then FFS := ItemFocused.Selected;
        DeselectAll;
      end;
    end;
    if ssShift in FPassedShift then
    begin
      with FOwner do
      begin
        FS := FSelected;
        DeselectAll;
        if FS <> nil then
        begin
          FSelected := FS;
          FSelected.Selected := true;
        end;
        SelectRange2(Item, FSelected, false);
      end;
    end;
  end;
  with FOwner do
  begin
    if ItemFocused = Item then
    begin
      if (((not FFS) or (FOwner.FAlwaysKeepSelection)) and
         (FOwner.FSelMode = smUsual)) and (not Item.Selected) then
         DoSetSelected(Item)
    end
    else
      ItemFocused := Item;
    if ((FMultiSelect) and (not (ssCtrl in FPassedShift)) and (not (ssShift in FPassedShift))) or
       (not FMultiSelect) {and Item.AllowSelection} then
        FSelected := Item;
  end;
  with FOwner do
    if FMultiSelect then
       IsUpdating := false;
  FOwner.DoAfterSelectionChange;
end;

procedure TElTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ItemPart: TSTItemPart;
  HCol: Integer;
  FOldSelected,
  Item: TElTreeItem;
  SelList: TElList;
  FFS: boolean;
  //Form : TCustomForm;
{$ifdef ELTREE_USE_STYLES}
  AStyle : TElCellStyle;
  Ctrl: TElCellControl;
  CX, CY: integer;
  i, j: integer;
  b : boolean;
{$endif}
  FOldSelcount : integer;
begin
  //Form := GetParentForm(Self);
  if FIgnoreClick or (ssDouble in Shift) then
  begin
    inherited;
    exit;
  end;
  FOwner.FSelChange := false;

  HCol := 0;
  Item := GetItemAt(X, Y, ItemPart, HCol);

  if (ItemPart = ipOutside) and ((FOwner.ShowColumns) or (not FOwner.RowSelect)) then
    Item := nil;
  if (Item = nil) or ((Item.Enabled) or (FOwner.IgnoreEnabled)) {and
     ((not FOwner.ShowColumns) or FOwner.RowSelect))} then
  begin
    //b := true;
    if (ItemPart = ipButton) and ((ssRight in Shift) or (button = mbRight)) then
       ItemPart := ipMainText;
    if (ItemPart = ipButton) then
    begin
      if Item.Expanded then
      begin
        FOldSelected := FFocused;
        Item.Expanded := false;
        if FOwner.MoveFocusOnCollapse and (FOldSelected <> FFocused) then
        begin
          FOwner.DoAfterSelectionChange;
          FOwner.DoItemFocused;
        end;
      end
      else
      begin
        Item.Expanded := true;
        FitMostChildren(Item);
      end;
      //b := false;
    end
    else
    if (ItemPart = ipCheckBox) then
    begin
      // checkboxes are processed in MouseUp now
    end
    else
    begin
      if (ItemPart = ipColumn) then
      begin
{$ifdef ELTREE_USE_STYLES}
        // b := true;
        if Item.UseStyles then
        begin
          if FOwner.VirtualityLevel = vlNone then
          begin
            i := HCol;
            j := i;
            if i = FOwner.FMainTreeCol then
              AStyle := Item.MainStyle
            else
            begin
              if i > FOwner.FMainTreeCol then Dec(i);
              if Item.StylesCount > i then
                AStyle := Item.Styles[i]
              else
                AStyle := Item.MainStyle;
            end;
          end
          else
          begin
            AStyle := VirtStyle;
            FOwner.TriggerVirtualStyleNeeded(Item, HCol, AStyle);
          end;
          if (AStyle <> nil) and (not AStyle.OwnerProps) and (AStyle.Control <> nil) and (AStyle.Control.Visible) then
          begin
            CY := y;
            CX := x;
            with FOwner do
            begin
              cy := (cy div LineHeight) * LineHeight;
              Ctrl := AStyle.Control;
              if Ctrl <> nil then
              begin
                begin
                  Ctrl.TriggerMouseDownEvent(Button, Shift, CX - FOwner.FHPos - FHeader.Sections[j].Left, CY);
                  FClickControl := Ctrl;
                  b := Ctrl.PassClicks;
                end;
                if not b then
                begin
                  inherited;
                  exit;
                end;
              end;
            end;
          end;
        end;
{$endif}
        if not FHeader.Sections[HCol].ClickSelect then exit;
      end;
    end;
    if ((ssLeft in Shift) or (button = mbLeft)) and (ItemPart <> ipButton) and (ItemPart <> ipCheckbox) then
    begin
      with FOwner do
        if FMultiSelect then IsUpdating := true;
      if (Item <> nil) and (((ItemPart <> ipInside) and (ItemPart <> ipOutside)) or FOwner.FRowSelect) then
      begin
        with FOwner do
        begin
          if FMultiSelect then
             FOldSelcount := FSelectedList.Count
          else
          if FSelected <> nil then
             FOldSelCount := 1
          else {Eyal}
          if not FOwner.AlwaysKeepSelection then {Eyal}
             FOldSelCount := 0;
        end;    { with }
         // when uncommented, a weird side-effect happens when selection jumps after
          // a message box is shown in responce to double-click
        if FOwner.MultiSelect and Item.Selected and (not (ssCtrl in Shift)) then
        begin
          FClickPassed := true;
          FPassedItem := Item;
          FPassedShift := Shift;
          FFS := Item.Selected;
          FOwner.ItemFocused := Item;
          if (FFS or (FOwner.AlwaysKeepSelection)) then
          begin
            Item.Selected := (FOwner.FSelMode = smUsual);
          end;
        end
        else
        begin
          //FFS := false;
          if (FOwner.FSelMode = smUsual) and (FOwner.FMultiSelect) then
          begin
            if (not (ssCtrl in shift)) and (not (ssShift in shift)) then
            begin
              with FOwner do
              begin
                //if ItemFocused <> nil then FFS := ItemFocused.Selected;
                (*
                if (Item = nil) or (not Item.Selected) then
                  DeselectAll
                else
                  Item.Selected := false;
                *)
                DeselectAll;
              end;
            end;
            if ssShift in shift then
            begin
              with FOwner do
              begin
                FOldSelected := FSelected;
                DeselectAll;
                FSelected := FOldSelected;
                if FSelected <> nil then
                   FSelected.Selected := true;
                SelectRange2(Item, FSelected, false);
                if ItemFocused <> Item then
                  Item.Selected := false;
              end;
            end
            else
            if ssCtrl in Shift then
            begin
              (*
              if Assigned(FOwner.ItemFocused) then
                 FFS := not FOwner.ItemFocused.Selected
              else
                 FFS := true;
              *)
            end;
          end;
          with FOwner do
          begin
            if ItemFocused = Item then
            begin
              if (not Item.Selected) or ((not FOwner.FAlwaysKeepSelection) or (FOldSelcount > 1)) then
                   DoSetSelected(Item);
            end
            else
              // if Item.AllowSelection then
              ItemFocused := Item;
            if (((FMultiSelect) and (not (ssCtrl in shift)) and (not (ssShift in shift)))
              or (not FMultiSelect)) and Item.Focused then
              FSelected := Item;
            if ((ssCtrl in shift) and (FOldSelCount <= 1))
               and (FMultiSelect) and (FOwner.FSelectedList.Count = 0)
               {and FOwner.FAlwaysKeepSelection} and Item.Focused then
               DoSetSelected(Item);
          end;
        end;
      end
      else // Eyal: (?) if FOwner.FMultiSelect or not FOwner.FAlwaysKeepSelection then {Eyal}
      begin
        if Item = nil then
        with FOwner do
        begin
          if (not FAlwaysKeepFocus) then
            ItemFocused := nil;
          if MultiSelect and (not AlwaysKeepSelection) then
            DeselectAll;
        end;
      end;
      with FOwner do
        if FMultiSelect then IsUpdating := false;
      if FOwner.FSelChange then
      begin
        FOwner.DoAfterSelectionChange;
        FOwner.FSelChange := false;
      end;
    end
    else
    if ((ssRight in Shift) or (button = mbRight)) and FOwner.RightClickSelect then
    begin
      with FOwner do
      if FMultiSelect then
      begin
        IsUpdating := true;
        SelList := TElList.Create;
        AllSelected(SelList);
      end;
  
      if FOwner.MultiSelect and Assigned(Item) and Item.Selected then
      begin
        FFS := Item.Selected;
        FOwner.ItemFocused := Item;
        if FFS or (FOwner.AlwaysKeepSelection) then Item.Selected := true;
      end else
  {$WARNINGS off}
      with FOwner do
      if (not FMultiSelect) or (FMultiSelect and ((SelList.Count < 2) and (FSelMode = smUsual)) or (FSelMode = smSimple)) then
      begin
        if (Item <> nil) and (((ItemPart <> ipInside) and (ItemPart <> ipOutside)) or FRowSelect) then
        begin
          begin
            if (FSelMode = smUsual) and (FMultiSelect) then
            begin
              if (not (ssCtrl in shift)) and (not (ssShift in shift)) then
              begin
            //if ItemFocused <> nil then FFS := ItemFocused.Selected;
                DeselectAll;
              end;
              if ssShift in shift then
              begin
                DeselectAll;
                if FSelected <> nil then FSelected.Selected := true;
                SelectRange2(Item, FSelected, false);
              end;
            end;
            if (ItemFocused = Item) and (not Item.Selected) then
               DoSetSelected(Item)
            else
            if {Item.AllowSelection and }(ItemFocused <> Item) then
              ItemFocused := Item;
            if ((FMultiSelect) and (not (ssCtrl in shift)) and (not (ssShift in shift)))
              or (not FMultiSelect) then
             FSelected := Item;
          end;
        end
        else // Eyal: (?) if FOwner.FMultiSelect or not FOwner.FAlwaysKeepSelection then {Eyal}
          if (Item = nil) and (not FAlwaysKeepFocus) then ItemFocused := nil;
      end;
      with FOwner do
      if FMultiSelect then
      begin
        SelList.Free;
        IsUpdating := false;
      end;
      if FOwner.FSelChange then
      begin
        FOwner.DoAfterSelectionChange;
        FOwner.FSelChange := false;
      end;
    end;
  end;
  inherited;
end;
{$WARNINGS on}

procedure TElTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ItemPart: TSTItemPart;
  HCol: Integer;
  Item: TElTreeItem;
begin
  inherited;
  HCol := 0;
  Item := GetItemAt(X, Y, ItemPart, HCol);
  if (Item = nil) or (Item.Enabled) or (FOwner.IgnoreEnabled) then
  begin
    if (ItemPart = ipCheckBox) then
    begin
      if not (Item.Enabled or (FOwner.IgnoreEnabled)) then exit;
      if Item.CheckBoxEnabled then
      begin
        if Item.CheckBoxType = ect3SCheckBox then
        begin
          if Item.CheckBoxState = cbChecked then
            Item.CheckBoxState := cbUnchecked
          else
          if Item.CheckBoxState = cbUnchecked then
            Item.CheckBoxState := cbGrayed
          else
            Item.CheckBoxState := cbChecked;
        end
        else
        if Item.CheckBoxType = ectCheckBox then
          Item.Checked := not Item.Checked
        else
          Item.Checked := true;
        FOwner.DoItemChecked(Item);
        FOwner.DoItemChange(Item, icmCheckState);
      end;
    end;
    if FClickPassed then
      ProcessPassedClick;
  end;
end;

{$WARNINGS OFF}
procedure TElTreeView.KeyDown(var Key: Word; Shift: TShiftState);
var
  PrevSel,
  TSI, TSI1: TElTreeItem;
  OldSel,
    OldSel1: boolean;
  gr : integer;
  sel: boolean;
  NewFocused: TElTreeItem;

begin
  inherited;
  if not (csNoStdEvents in ControlStyle) then
  begin
    (*
    if FOwner.FIncrementalSearch and (Shift = []) then
    begin
      if ProcessSearch(Char(Key)) then exit;
    end;
    *)

    if FMouseSel then
      exit;

    FOwner.FSelChange := false;

    with FOwner do
      if FMultiSelect then IsUpdating := true;
    TSI := nil;
    TSI1 := nil;
    NewFocused := nil;
    OldSel := false;
    OldSel1 := false;

    if (ssCtrl in shift) and (ssShift in shift) then Shift := Shift - [ssCtrl];
    if (ssShift in shift) and (Key = Ord('8')) then
    begin
      if (FFocused <> nil) and ((FFocused.Enabled) or (FOwner.IgnoreEnabled)) and (FFocused.HasVisibleChildren or FFocused.ForceButtons) then FFocused.Expand(true);
    end
    else
    case Key of //
      VK_BACK:
        if (FFocused <> nil) and (FFocused.Parent <> nil) and ((FFocused.Parent.Enabled) or (FOwner.IgnoreEnabled)) then
        begin
          FOwner.DoSetFocusedEx(FFocused.Parent, false, true);
          FOwner.EnsureVisible(FOwner.ItemFocused);
        end;
      VK_ADD:
        if ssCtrl in Shift then
          FOwner.AutoSizeAllColumns
        else
        if (FFocused <> nil) and (FFocused.HasVisibleChildren or FFocused.ForceButtons) and
           (not (FFocused.Expanded)) and ((FFocused.Enabled) or (FOwner.IgnoreEnabled)) then
        begin
          FFocused.Expanded := true;
          FitMostChildren(FFocused);
        end;
      VK_SUBTRACT:
        if (FFocused <> nil) and ((FFocused.Enabled) or (FOwner.IgnoreEnabled)) and (FFocused.Expanded) then
           FFocused.Expanded := false;
      VK_MULTIPLY:
        if (FFocused <> nil) and ((FFocused.Enabled) or (FOwner.IgnoreEnabled)) and
           (FFocused.HasVisibleChildren or FFocused.ForceButtons) then
           begin
             FFocused.Expand(true);
             FitMostChildren(FFocused);
           end;
      VK_DIVIDE:
        if (FFocused <> nil) and ((FFocused.Enabled) or (FOwner.IgnoreEnabled)) then
           FFocused.Collapse(true);
      VK_SPACE:
        if (FFocused <> nil) and ((FFocused.Enabled) or (FOwner.IgnoreEnabled)) and (FOwner.FMultiSelect) then
        begin
          if (not (ssCtrl in shift)) and (not (ssShift in shift)) and (FOwner.FSelMode = smUsual) then
          begin
            FOwner.DeselectAll;
            FSelected := FFocused;
          end;
          if FOwner.FSelMode = smSimple then
            FSelected := FFocused;
          FFocused.Selected := not (FFocused.Selected);
          FOwner.DoAfterSelectionChange;
        end;
      VK_HOME,
        VK_END,
        VK_PRIOR,
        VK_NEXT,
        VK_LEFT,
        VK_RIGHT,
        VK_UP,
        VK_DOWN:
        begin
          if FOwner.RightAlignedTree then
          begin
            if Key = VK_LEFT then
              Key := VK_RIGHT
            else
              if Key = VK_RIGHT then
                Key := VK_LEFT;
          end;
          if Key = VK_LEFT then
            if (FFocused <> nil) and FFocused.Expanded and ((FFocused.Enabled) or (FOwner.IgnoreEnabled))  then
            begin
              FFocused.Expanded := false;
              with FOwner do
                if FMultiSelect then IsUpdating := false;
              exit;
            end;
          if Key = VK_RIGHT then
            if (FFocused <> nil) and (not (FFocused.Expanded)) and ((FFocused.Enabled) or (FOwner.IgnoreEnabled)) and (FFocused.HasVisibleChildren or FFocused.ForceButtons) then
            begin
              FFocused.Expanded := true;
              FitMostChildren(FFocused);
              with FOwner do
                if FMultiSelect then IsUpdating := false;
              exit;
            end;
          (*
          GIRec.OTSI := nil;
          GIRec.TSI := nil;
          GIRec.i := 0;
          GIRec.NewFocused := nil;
          *)
          with FOwner do
          if FMultiSelect then
          begin
            if ((ssCtrl in Shift) and (FSelMode = smUsual)) or (FSelMode = smSimple) then
            begin
              TSI := FFocused;
              if TSI <> nil then
                 OldSel := TSI.Selected
              else
                 OldSel := false;
            end;
            if (ssShift in Shift) then
            begin
              TSI1 := FSelected;
              if TSI1 <> nil then
                 OldSel1 := TSI1.Selected
              else
                 OldSel1 := false;
            end;
            PrevSel := FSelected;
            if (FSelMode = smUsual) and (not (ssCtrl in shift)) then
               DeselectAll;
          end;
          if Key = VK_UP then
          begin
            NewFocused := FindNewFocused(VK_UP, @gr, sel);
            if (FFocused = nil) and (NewFocused = nil) then
            begin
              with FOwner do
                if FMultiSelect then
                  IsUpdating := false;
              exit;
            end
          end
          else
          if Key = VK_DOWN then
          begin
            NewFocused := FindNewFocused(VK_DOWN, nil, sel);
            if (FFocused = nil) and (NewFocused = nil) then
            begin
              with FOwner do
                if FMultiSelect then
                  IsUpdating := false;
              exit;
            end
          end
          else
          if Key = VK_Left then
          begin
            if (FFocused <> nil) and (FFocused.Parent <> nil) then
              if FFocused.Parent <> nil then
                NewFocused := FFocused.Parent
              else
                NewFocused := FFocused;
          end
          else
          if Key = VK_Right then
          begin
            if (FFocused <> nil) and (FFocused.GetFirstVisibleChild <> nil) then
            begin
              FOwner.IsUpdating := true;
              NewFocused := FFocused.GetFirstVisibleChild;
              if NewFocused = nil then NewFocused := FFocused;
              FOwner.FUpdated := true;
              FRangeUpdate := true;
              FOwner.IsUpdating := false;
            end;
          end else
          if Key = VK_NEXT then
          begin
            NewFocused := FindNewFocused(VK_NEXT, nil, sel);
          end else
          if Key = VK_PRIOR then
          begin
            NewFocused := FindNewFocused(VK_PRIOR, nil, sel);
          end
          else
          if Key = VK_HOME then
          begin
            NewFocused := FindNewFocused(VK_HOME, nil, sel);
          end
          else
          if Key = VK_END then
          begin
            NewFocused := FindNewFocused(VK_END, nil, sel);
          end;
          if FOwner.FMultiSelect then
          begin
            if ((ssCtrl in Shift) and (FOwner.FSelMode = smUsual)) or (FOwner.FSelMode = smSimple) then
            begin
              if TSI <> nil then TSI.Selected := OldSel;
               //if FFocused<>TSI then FFocused.Selected:=GIRec.Sel;
            end;
            if ssShift in shift then
            begin
              FOwner.SelectRangeEx2(TSI1, NewFocused, false, false);
              if TSI1 <> nil then
                TSI1.Selected := OldSel1;
               //if FFocused <> nil then FFocused.Selected:=true;
            end;
          end;
          if NewFocused <> nil then
          begin
            if FOwner.FMultiSelect then
            begin
              if ((ssCtrl in Shift) and (FOwner.FSelMode = smUsual)) or (FOwner.FSelMode = smSimple) then
              begin
                if NewFocused <> TSI then
                   NewFocused.Selected := not sel;
              end;
              if ssShift in shift then
              begin
                if FFocused <> NewFocused then
                   NewFocused.Selected := false;
              end;
            end;
            if FFocused <> NewFocused then
            begin
              FOwner.DoSetFocusedEx(NewFocused, false, true);

              if FOwner.FMultiSelect then
                 FSelected := PrevSel;
            end
            else
            begin
              if FOwner.FMultiSelect and (FSelected = nil) then
                FSelected := PrevSel;
              NewFocused.Selected := true;
            end;
          end;
          if (not ((ssCtrl in shift) or (ssShift in shift))) or
             (not FOwner.FMultiSelect) then
          begin
            if FFocused <> nil then
              FSelected := FFocused;
            if (FFocused <> nil) and (not ((FOwner.FMultiSelect) and (FOwner.FSelMode = smSimple))) and (FOwner.FMultiSelect) then
              FFocused.Selected := true;
          end;
          if (FFocused <> nil) and ((FVisible.IndexOf(FFocused) = -1) {or xx}) then
          begin
            if Key = VK_UP then
            begin
              SetVPosition(gr);
            end else
            if Key = VK_DOWN then
            begin
              //FOwner.EnsureVisibleBottom(FItems.GetVisItem(GIRec.i));
              if FOwner.TotalVarHeightCount > 0 then
                 SetVPosition(CalcPageUpPos(gr))
              else
                 SetVPosition(gr - (GetVisCount - 2));
            end else
            if Key = VK_Left then
            begin
              SetVPosition(FFocused.VisIndex);
            end else
            if Key = VK_Right then
            begin
              FOwner.EnsureVisibleBottom(FItems.GetVisItem(gr));
            end else
            if Key = VK_PRIOR then
            begin
              if FOwner.TotalVarHeightCount > 0 then
                   SetVPosition(CalcPageUpPos(FOwner.FTopIndex))
                else
                   SetVPosition(FOwner.FTopIndex - (GetVisCount - 1));
            end else
            if Key = VK_NEXT then
            begin
              if FOwner.TotalVarHeightCount > 0 then
                 SetVPosition(CalcPageDownPos(FOwner.FTopIndex))
              else
                 SetVPosition(FOwner.FTopIndex + (GetVisCount - 1));
            end
            else
            if KEY = VK_Home then
            begin
              SetVPosition(0);
            end else
            if KEY = VK_End then
            begin
              SetVPosition(FOwner.TotalVisCount - 1);
            end;
            FClearVis := true;
            FClearAll := true;
          end;
        end;
    end; // case
    with FOwner do
      if FMultiSelect then
        IsUpdating := false;
    if FOwner.FSelChange then
    begin
      FOwner.DoAfterSelectionChange;
      FOwner.FSelChange := false;
    end;
  end;
end;

{$WARNINGS ON}
procedure TElTreeView.KeyPress(var Key: Char);
begin
  if not (csNoStdEvents in ControlStyle) then
  begin
    if FOwner.FIncrementalSearch and ProcessSearch(Key) then exit;
  end;
  inherited;
end;

procedure TElTreeView.SetFocus;
begin
  if CanFocus then
  try
    inherited SetFocus;
 except
 end;
end;

procedure TElTreeView.DoSetSelected;
var PreLevel : Integer; // CNS
begin
  // if (value <> nil) and (not value.AllowSelection) then exit;

  if FOwner.FMultiSelect then
  begin
    if Value <> nil then with Value do
    begin
      if FOwner.FSelectedList.Count > 0 then // CNS if existing selection(s), then get previous level started
        PreLevel := TElTreeItem(FOwner.FSelectedList.First).Level
      else
        PreLevel:=FOwner.FMultiSelectLevel;

      if FOwner.FMultiSelectLevel >= 0 then // CNS if either existing or new select has wrong level, deselect all
      begin
        if (Value.Level<>FOwner.FMultiSelectLevel) or
           (PreLevel<>FOwner.FMultiSelectLevel) then
          Owner.DeselectAll;
      end;

      if (FIState and tisSelected) > 0 then
      begin
        FOwner.FSelChange := true;
        Exclude(FState, stsSelected);
        FIState := FIState and (not tisSelected);
        FOwner.FSelectedList.Remove(Value);
        if FSelected = Value then
           FSelected := nil;
      end
      else
      begin
        if FOwner.FKeepSelectionWithinLevel and (FOwner.FSelectedList.Count > 0) and
          (FOwner.FSelectedList[0] <> Value) then
        begin
          if Value.Level <> TElTreeItem(FOwner.FSelectedList[0]).Level then
            exit;
        end;

        FOwner.FSelChange := true;
        Include(FState, stsSelected);
        FIState := FIState or tisSelected;
        FOwner.FSelectedList.Add(Value);
      end;
      FOwner.TriggerItemSelectedChangeEvent(Value);
      Value.UpdateItem;
    end
  end
  else
  begin
    if Self.FSelected <> nil then with FSelected do
    begin
      FOwner.FSelChange := true;
      Exclude(FState, stsSelected);
      FIState := FIState and (not tisSelected);
      FSelected.UpdateItem;
      FOwner.TriggerItemSelectedChangeEvent(Self.FSelected);
      FSelected := nil;
    end;
    if Value <> nil then with value do
    begin
      FOwner.FSelChange := true;
      Include(FState, stsSelected);
      FIState   := FIState or tisSelected;
      FSelected := Value;
      FOwner.TriggerItemSelectedChangeEvent(Value);
      Value.UpdateItem;
    end;
  end;
end;

function TElTreeView.GetVisCount;
var
  j: integer;
  CliHeight: integer;

begin
  CliHeight := Height;
  j := FOwner.FLineHeight;
  result := CliHeight div j;
  if ((CliHeight mod j) > 0) then inc(result);
end;

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TElTreeView.DoEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FOwner.NotifyOnEditKeyDown(Sender,Key,Shift);
  if Shift = [] then
  begin
    if Key = VK_Return then
    begin
      DoEndEdit(false);
      Key := 0;
    end;
    if Key = VK_Escape then
    begin
      DoEndEdit(True);
      Key := 0;
    end;
  end;
end;
{$endif}
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.CMMouseLeave;
{$else}
procedure TElTreeView.MouseLeave;
{$endif}
var
  TSI: TElTreeItem;
begin
{$ifdef ELTREE_USE_INPLACE_EDITORS}
  if assigned(FInpEdit) and FInpEdit.Visible then exit;
{$endif}
  TSI := FTrackItem;
  FTrackItem := nil;
  if TSI <> nil then
  begin
    TSI.RedrawItem(true);
    Update;
  end;
  FHintItemEx := nil;
  // Destroy Hint timer
  {$ifndef CLX_USED}
  DoHideLineHint;
  {$endif}
{$ifdef ELTREE_USE_STYLES}
  if FClickControl <> nil then
  begin
    FClickControl.TriggerMouseUpEvent(mbLeft, GetShiftState, -1, -1);
    FClickControl := nil;
  end;
{$endif}
  inherited;
end;

{$ifndef CLX_USED}
procedure TElTreeView.CMHintShow(var Msg : TMessage);
{$else}
function TElTreeView.HintShow(var HintInfo : THintInfo): Boolean;
{$endif}
var
{$ifndef CLX_USED}
  HintInfo : PHintInfo;
{$endif}
  Item     : TElTreeItem;
  IP       : TSTItemPart;
  HCol     : integer;

  {$ifdef ELPACK_UNICODE}
  T: WideChar;
  WS: WideString;
  l : integer;
  S : String;
  {$endif}

begin
{$ifndef CLX_USED}
  HintInfo := PHintInfo(Msg.lParam);
{$endif}  
  {$ifdef CLX_USED}
  result := inherited HintShow(HintInfo);
  {$endif}
  Item := GetItemAt(HintInfo.CursorPos.X, HintInfo.CursorPos.Y, IP, HCol);
  if (Item <> nil) and (Item.Hint <> '') then
     HintInfo.HintStr := Item.Hint;
  FHintItemEx := Item;
  {$ifdef ELPACK_UNICODE}
  if (Item <> nil) and (Item.Hint <> '') then
    WS := Item.Hint
  else
    WS := Owner.FHint;
  if Length(WS) = 0 then
  begin
    HintInfo.HintStr := '';
    exit;
  end;

  if HintInfo.HintStr = GetShortHint(Owner.Hint) then
  begin
    WS := GetShortHintW(WS);
    S := WS;
  end
  else
    S := WS;

  l := Length(S) + 1 + Length(WS) * 2;
  SetLength(HintInfo.HintStr, l + 4);
  Move(PChar(S)^, HintInfo.HintStr[1], Length(S) + 1);

  Move(WS[1], HintInfo.HintStr[Length(S) + 2], Length(WS) * 2);
  T := #0;
  Move(T, HintInfo.HintStr[l + 1], sizeof(T));
  T := #$FFFE;
  Move(T, HintInfo.HintStr[l + 3], sizeof(T));
  {$endif}
end;

{$ifdef CLX_USED}
procedure TElTreeView.PaletteChanged(Sender: TObject);
{$else}
procedure TElTreeView.CMSysColorChange(var Msg: TMessage);
{$endif}
begin
  inherited;
  Invalidate;
  {$ifndef CLX_USED}
  ChangeButtonColors(PlusBmp);
  ChangeButtonColors(MinusBmp);
  ChangeButtonColors(LeafBmp);
  {$endif}
end;

{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF LITE}
procedure TElTreeView.OnDropTargetDrag(Sender: TObject; State: TDragState; Source: TOleDragObject; Shift: TShiftState; X: Integer; Y: Integer; var DragType: TDragType);
var
  Item: TElTreeItem;

begin
  FOwner.TriggerOleTargetDragEvent(State, Source, Shift, X, Y, DragType);
  DoDragOver(Source, X, Y, DragType in [dtCopy, dtMove, dtLink]);
  if State = dsDragLeave then
  begin
    if Self.FDropTrg <> nil then
    begin
      Item := FDropTrg;
      FDropTrg := nil;
      Item.RedrawItem(true);
    end;
  end;
end;

procedure TElTreeView.OnDropTargetDrop(Sender: TObject; Source: TOleDragObject; Shift: TShiftState; X: Integer; Y: Integer; var DragType: TDragType);
var
  TSI: TElTreeItem;
begin
  TSI := FDropTrg;
  FDropTrg := nil;
  if TSI <> nil then TSI.RedrawItem(true);
  FOwner.TriggerOleTargetDropEvent(Source, Shift, X, Y, DragType);
end;
{$ENDIF LITE}
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.WMSetFocus(var Msg : TWMSetFocus);  { private }
begin
  inherited;
  {$ifndef CLX_USED}
  FHasFocus := true;
  if FOwner.FHideSelect then
    Invalidate;

  with FOwner do
    if Flat or FUseCustomBars or IsThemeApplied then UpdateFrame;
  {$endif}
end;  { WMSetFocus }
{$endif}

{$ifndef CLX_USED}
procedure TElTreeView.WMKillFocus(var Msg : TWMKillFocus);  { private }
{$else}
procedure TElTreeView.DoExit;
{$endif}
begin
  FMouseSel := false;
  FPressed  := false;
  FHasFocus := false;
  inherited;
  FHintItemEx := nil;
  DoHideLineHint;
  FOwner.FTreeIsFocused := false;

  if HandleAllocated then
  begin
    {$ifndef CLX_USED}
    with FOwner do
      if Flat or FUseCustomBars or IsThemeApplied then
      begin
        UpdateFrame;
        DrawFlatBorder(false, false);
      end;
    {$else}
    if FOwner.HideSelection then
      Invalidate;
    {$endif}
  end;
end;  { WMKillFocus }

{$ifndef CLX_USED}
function TElTreeView.GetDragImages: TDragImageList;
begin
  if (FDragImages <> nil) and (FDragImages.Count > 0) then
     Result := FDragImages
  else
     result := nil;
end;
{$endif}

procedure TElTreeView.DoEnter;
begin
  inherited;
  {$ifdef CLX_USEd}
  FHasFocus := true;
  if FOwner.FHideSelect then
    Invalidate;
  {$endif}
end;

{$ifndef CLX_USED}
procedure TElTreeView.DoExit;
begin
  inherited;
  if FOwner.FHideSelect then
    Invalidate;
end;
{$endif}

procedure TElTreeView.UpdateView;
var
  R: TRect;
begin
  if (not HandleAllocated) then
  begin
    FOwner.FUpdated := true;
    exit;
  end;
  if FOwner.FUpdating then FOwner.FUpdated := true else
  begin
    if FRangeUpdate or (FClearAll) then
    begin
      if (FOwner.FVLines and FOwner.FShowHeader) or (FClearAll) then
      begin
        R := ClientRect;
        FClearAll := false;
      end
      else
        R := Rect(0, 0, ClientWidth, GetVisiblesHeight);
      {$ifndef CLX_USED}
      InvalidateRect(Handle, @R, true);
      {$else}
      Inc(R.Bottom); Inc(R.Right);
      QWidget_update(Handle, @R);
      Dec(R.Bottom); Dec(R.Right);
      {$endif}
      FRangeUpdate := false;
    end;
    Update;
  end;
end;

procedure TElTreeView.DoPaintBkgnd(DC : {$ifdef CLX_USED}ElVCLUtils.{$endif}HDC; ClipRect : TRect);
var
  AClipRect: TRect;
{$IFDEF ELPACK_COMPLETE}
  ACtl : TWinControl;
  R1,
{$ENDIF}
  BgRect
{$IFNDEF LITE}
  ,
  BgRect1
{$ENDIF}
          : TRect;
begin
  Canvas.Brush.Color := FOwner.BkColor;
  BgRect := ClientRect;
  Inc(BgRect.Bottom);
  Inc(BgRect.Right);
  AClipRect := ClipRect;
  {$IFDEF ELPACK_COMPLETE}
  if (FOwner.FImgForm <> nil) and (not (csDesigning in FOwner.FImgForm.ComponentState)) then
  begin
    //if (FOwner.FImgForm.Control <> Parent) then
    begin
      ACtl := FOwner.FImgForm.GetRealControl;
      R1 := BgRect;
      BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
      BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
      //BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
      //BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);

      FOwner.FImgForm.PaintBkgnd(DC, R1, BgRect.TopLeft, false);
    end;
  end
  else
  {$ENDIF}
  {$IFNDEF LITE}
  if (FOwner.BackgroundType <> bgtColorFill) then
  begin
    BgRect1 := BgRect;
    {$ifdef CLX_USED}
    FTmpBmp.Canvas.Start;
    {$endif}
    if FOwner.BackgroundType <> bgtCenterBitmap then
    begin
      ExtDrawBkgnd(DC, Handle, BgRect1, BgRect, BgRect, AClipRect, false, Canvas.Brush.Color, Canvas.Brush.Color, false, FTmpBmp, bgtCenterBitmap);
    end
    else
    begin
      ExtDrawBkgnd(DC, Handle, BgRect1, BgRect, BgRect, AClipRect, false, Canvas.Brush.Color, Canvas.Brush.Color, false, FOwner.Background, FOwner.BackgroundType);
    end;
    {$ifdef CLX_USED}
    FTmpBmp.Canvas.Stop;
    {$endif}
  end
  else
{$ENDIF}
  begin
    {$ifndef CLX_USED}
    if not FOwner.InSizeMove then
      FillRect(DC, AClipRect, Canvas.Brush.Handle);
    {$else}
    Canvas.Start;
    QPainter_FillRect(DC, @AClipRect, Canvas.Brush.Handle);
    Canvas.Stop;
    {$endif}
  end;
end;

{$warnings off}

{$ifndef CLX_USED}
procedure TElTreeView.WMPaint(var Msg : TWMPaint);  { private }
var
  ADC,
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  R : TRect;
  ARgn : HRGN;

{$IFDEF SPEED_TEST}
var TickCount : integer;
{$ENDIF}
begin
{$IFDEF SPEED_TEST}
  TickCount := GetTickCount;
{$ENDIF}
  if not FOwner.IsUpdating then
  begin
    if (Msg.DC <> 0) then
      PaintHandler(Msg)
    else
    begin
      if FOwner.DoubleBuffered and (not FOwner.InSizeMove) then
      begin
        DC := GetDC(0);
        MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
        ReleaseDC(0, DC);
        MemDC := CreateCompatibleDC(0);
        OldBitmap := SelectObject(MemDC, MemBitmap);
        ADC := MemDC;
      end
      else
        ADC := 0;

      try
        DC := BeginPaint(Handle, PS);
        if (not FOwner.DoubleBuffered) or FOwner.InSizeMove then
          ADC := DC;
        GetClipBox(DC, R);
        if IsRectEmpty(R) then
          R := ClientRect
        else
        begin
          inc(R.Right);
          inc(R.Bottom);
        end;
        //with R do
        //  BitBlt(MemDC, Left, Top, Right - Left, Bottom - Top, DC, Left, Top, SRCCOPY);

        if FOwner.DoubleBuffered and (not FOwner.InSizeMove) then
        begin
          with R do
            ARgn := CreateRectRgn(Left, Top, Right, Bottom);
          SelectClipRgn(ADC, ARgn);
        end;

        DoPaintBkgnd(ADC, R);

        Msg.DC := ADC;

        PaintHandler(Msg);

        if FOwner.DoubleBuffered and (not FOwner.InSizeMove) then
        begin
          SelectClipRgn(MemDC, 0);
          DeleteObject(ARgn);
        end;
        Msg.DC := 0;
        if FOwner.DoubleBuffered and (not FOwner.InSizeMove) then
          with R do
            BitBlt(DC, Left, Top, Right - Left, Bottom - Top, MemDC, Left, Top, SRCCOPY);
        EndPaint(Handle, PS);
      finally
        if FOwner.DoubleBuffered and (not FOwner.InSizeMove) then
        begin
          SelectObject(MemDC, OldBitmap);
          DeleteDC(MemDC);
          DeleteObject(MemBitmap);
        end;
      end;
    end;
  end
  else
  begin
    DefWindowProc(Handle, WM_PAINT, TMessage(Msg).wParam, TMessage(Msg).lParam);
  end;
{$IFDEF SPEED_TEST}
  TickCount := GetTickCount - TickCount;
  SendDebug('Redrawing time: ' + IntToStr(TickCount) + ' msec');
{$ENDIF}
end;
{$endif}

{$warnings on}

function TElTreeView.GetVisCount2: Integer;
var
  j: integer;
  CliHeight: integer;

begin
  CliHeight := Height;
  j := FOwner.FLineHeight;
  result := CliHeight div j;
end;

procedure TElTreeView.OnDragExpandTimer(Sender : TObject);
begin
  FDragExpandTimer.Enabled := false;
  if (FDropTrg <> nil) and (not FDropTrg.Expanded) {and (FDropTrg.HasVisibleChildren) }then
  begin
    if FOwner.FDragObject <> nil then
      FOwner.FDragObject.HideDragImage;
    FDropTrg.Expand(false);
    Update;
    if FOwner.FDragObject <> nil then
      FOwner.FDragObject.ShowDragImage;
  end;
end;

function TElTreeView.ProcessSearch(Key : Char): Boolean;
var
  liSearchTextLength,
    liIndex : Integer ;
    AnItem  : TElTreeItem;
begin
  Result := false;
  if (Key = #27) then
  begin
    SearchText := '' ;
    FOwner.DeselectAll ;
    Exit
  end
  else
    if (Key = #8) then
    begin
      if (SearchText > '') then
        SetLength(SearchText, Length(SearchText) - 1);
    end
    else
      if (Key > #32) then
        SearchText := SearchText + Key
      else
        Exit ;

  if (SearchText > '') then
  begin
    liSearchTextLength := Length (SearchText) ;

    for liIndex := 0 to (FOwner.Items.Count - 1) do
    begin
      AnItem := FOwner.Items [liIndex];
      if AnsiSameText(SearchText, Copy (AnItem.Text, 1, liSearchTextLength)) then
      begin
        FOwner.EnsureVisible(AnItem) ;
        AnItem.Focused := True ;
        AnItem.Selected := False ;
        break;
      end ;
    end;
    StartClearSearchTimeoutThread;
  end;
end;

procedure TElTreeView.StartClearSearchTimeoutThread ;
begin
  if Assigned (SearchTextTimeoutThread) then
    TSearchTextTimeoutThread(SearchTextTimeoutThread).KeepAlive := True
  else
  begin
    SearchTextTimeoutThread := TSearchTextTimeoutThread.Create ;
    SearchTextTimeoutThread.OnTerminate := SearchTextTimeout ;
    SearchTextTimeoutThread.Resume
  end
end ;

procedure TElTreeView.StopClearSearchTimeoutThread ;
begin
  if Assigned (SearchTextTimeoutThread) then
  begin
    SearchTextTimeoutThread.OnTerminate := nil ;
    SearchTextTimeoutThread.Terminate ;

    SearchTextTimeoutThread := nil
  end
end ;

procedure TElTreeView.SearchTextTimeout (Sender : TObject) ;
begin
  SearchTextTimeoutThread := nil ;
  SearchText := ''
end;

procedure TElTreeView.FitMostChildren(Item : TElTreeItem);
var
  R: TRect;
  i,
  k,
  ch,
  cti : integer;
  oi  : integer;
  TopItem: TElTreeItem;

begin
  if Item.FullyExpanded and Item.FullyVisible and (FOwner.IsInView(Item)) then
  begin
    k := Item.CalcSubItemsHeight;
    TopItem := FOwner.GetTopItem();
    if TopItem <> nil then
    begin
      cti := TopItem.VisIndex;
      oi  := Item.VisIndex;
      R   := GetItemRect(FVisible.IndexOf(Item));
      i   := 0;
      ch := ClientHeight;
      if cti < oi then
      begin
        while (cti < oi) do
        begin
          if FVisible.Count <= i then
            break;
{$ifndef LITE}
          TopItem := FVisible[i];
{$endif}
          if R.Bottom + k >= ch then
          begin
{$ifdef LITE}
            Dec(R.Bottom, FOwner.LineHeight);          
{$else}
            Dec(R.Bottom, TopItem.Height);
{$endif}
            inc(i);
            Inc(cti);
          end
          else
          begin
            break;
          end;
        end;
        FOwner.GetTotalVisCount;
        FOwner.TopIndex := FOwner.TopIndex + i;
      end;
    end;
  end;
end;

{$warnings off}
function TElTreeView.FindNewFocused(Key : word; PVal1 : PInteger; var Sel : boolean): TElTreeItem;
type
  PGIRec = ^TGIRec;
  TGIRec = record
    i, oi,
    ni, PgCnt  : integer;
    TSI, OTSI  : TElTreeItem;
    Sel, fp    : boolean;
    NewFocused : TElTreeItem;
  end;
var
  GIRec: TGIRec;
  IntNextVis: TIterateProcAnonymusMethod;
  IntVis0: TIterateProcAnonymusMethod;
  IntPgVis: TIterateProcAnonymusMethod;
begin
   IntNextVis := procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree)
      begin
        if (PGIRec(IterateData)^.OTSI = Tree.FView.FFocused) and (Tree.IgnoreEnabled or (Item.Enabled)) then
        begin
          PGIRec(IterateData)^.Sel := Item.Selected;
          PGIRec(IterateData)^.NewFocused := Item;
          PGIRec(IterateData)^.i := Index;
          ContinueIterate := false;
        end;
        if Item = Tree.FView.FFocused then
           PGIRec(IterateData)^.OTSI := Item;
      end;

    IntVis0 := procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree)
      begin
        with PGIRec(IterateData)^ do
        begin
          if (Item.Enabled or (Tree.IgnoreEnabled)) then
          begin
            Sel := Item.Selected;
            NewFocused := Item;
            i := 0;
            ContinueIterate := false;
          end;
        end;
      end;

    IntPgVis := procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree)
        begin
          with PGIRec(IterateData)^ do
          begin
            if Item = Tree.FView.FFocused then
               oi := 1;
            if oi = 1 then
            begin
              if Tree.IgnoreEnabled or Item.Enabled then
              begin
                dec(PgCnt);
                NewFocused := Item;
              end;

              TSI := Item;
              Sel := Item.Selected;
              i := Index;
            end;
            if PgCnt < 0 then
            begin
              ContinueIterate := false;
            end;
          end;
        end;

  Result := nil;
  FillChar(GIRec, sizeof(GIRec), 0);
  if Key = VK_UP then
  begin
    if FFocused = nil then
    begin
      if FOwner.TotalVisCount > 0 then
      begin
        FItems.Iterate(true, false, IntVis0, @GIRec);
        Result := GIRec.NewFocused;
      end;
    end
    else
    begin
      FItems.IterateBack(true, false, IntNextVis, @GIRec);
      if GIRec.NewFocused <> nil then
        Result := GIRec.NewFocused
      else
        Result := FFocused;
    end;
    if PVal1 <> nil then
      PVal1^ := GIRec.i;
    Sel := GIRec.Sel;
  end
  else
  if Key = VK_DOWN then
  begin
    if FFocused = nil then
    begin
      if FOwner.TotalVisCount > 0 then
      begin
        FItems.Iterate(true, false, IntVis0, @GIRec);
        result := GIRec.NewFocused;
      end;
    end else
    begin
      FItems.Iterate(true, false, IntNextVis, @GIRec);
      if GIRec.NewFocused <> nil then
        result := GIRec.NewFocused
      else
        result := FFocused;
    end;
    if PVal1 <> nil then
      PVal1^ := GIRec.i;
    Sel := GIRec.Sel;
  end
  else
  if Key = VK_NEXT then
  begin
    if FOwner.TotalVarHeightCount > 0 then
       GIRec.PgCnt := CalcPageDownPos(FOwner.FTopIndex) - FOwner.FTopIndex
    else
       GIRec.PgCnt := GetVisCount - 1;

    GIRec.oi := 0;
    GIRec.NewFocused := nil;
    if FFocused <> nil then
       FItems.IterateFrom(true, false, IntPgVis, @GIRec, FOwner.ItemFocused);
    if GIRec.NewFocused <> nil then
      result := GIRec.NewFocused
    else
      result := FFocused;
    Sel := GIRec.Sel;
  end
  else
  if Key = VK_PRIOR then
  begin
    if FFocused = nil then
    begin
      if FOwner.TotalVarHeightCount > 0 then
         GIRec.PgCnt := FOwner.FTopIndex - CalcPageUpPos(FOwner.FTopIndex)
      else
         GIRec.PgCnt := GetVisCount - 1;
    end else
    begin
      GIRec.oi := FFocused.VisIndex;
      if FOwner.TotalVarHeightCount > 0 then
         GIRec.PgCnt := GIRec.oi - CalcPageUpPos(GIRec.oi)
      else
         GIRec.PgCnt := GetVisCount - 1;
    end;
    GIRec.oi := 0;
    GIRec.NewFocused := nil;

    if FFocused <> nil then
       FItems.IterateBackFrom(true, false, IntPgVis, @GIRec, FOwner.ItemFocused);
    if GIRec.NewFocused <> nil then
      result := GIRec.NewFocused
    else
      result := FFocused;

    Sel := GIRec.Sel;
  end
  else
  if Key = VK_HOME then
  begin
    if FFocused <> nil then
    begin
      FItems.Iterate(true, false, IntVis0, @GIRec);
      result := GIRec.NewFocused;
      GIRec.i := 0;
      if result = nil then
      begin
        with FOwner do
        begin
          result := ItemFocused;
          GIRec.i := FTopIndex;
        end;
      end;
    end;
    Sel := GIRec.Sel;
  end
  else
  if Key = VK_END then
  begin
    if FFocused <> nil then
    with FOwner do
    begin
      GIRec.i := TotalVisCount - 1;
      Result := FItems.GetVisItem(GIRec.i);
      while (Result <> nil) and (not (FOwner.IgnoreEnabled or Result.Enabled)) do
      begin
        dec(GIRec.i);
        if GIRec.i >= 0 then
           Result := FItems.GetVisItem(GIRec.i);
      end;
      if Result = nil then
      begin
        Result := ItemFocused;
        GIRec.i := FTopIndex;
      end;
    end;
    Sel := GIRec.Sel;
  end;
end;

{$warnings on}

procedure TElTreeView.DrawMouseSelectFrame;
var R,
    R1 : TRect;
    si,
    ei : integer;

begin
  R := Rect(Min(FMFSStartCoord.X, FMFSEndCoord.X), 0, Max(FMFSStartCoord.X, FMFSEndCoord.X), 0);
  OffsetRect(R, -FOwner.FHPos, 0);
  si := FVisible.IndexOf(FMFSStartItem);
  if si <> -1 then
  begin
    R1 := GetItemRect(si);
    si := R1.Top + FMFSStartCoord.Y;
  end;
  ei := FVisible.IndexOf(FMFSEndItem);
  if ei <> -1 then
  begin
    R1 := GetItemRect(ei);
    ei := R1.Top + FMFSEndCoord.Y;
  end;
  if (ei <> -1) and (si <> -1) then
  begin
    R.Top := Min(si, ei);
    R.Bottom := Max(si, ei);
  end
  else
  if (si = -1) and (ei = -1) then
  begin
    R.Top := -1;
    R.Bottom := ClientHeight + 1;
  end
  else
  if si = -1 then
  begin
    if FMFSStartItem.AbsoluteIndex > FMFSEndItem.AbsoluteIndex then
    begin
      R.Top := ei;
      R.Bottom := ClientHeight + 1;
    end
    else
    begin
      R.Top := -1;
      R.Bottom := ei;
    end;
  end
  else
  if ei = -1 then
  begin
    R.Top := si;
    R.Bottom := ClientHeight + 1;
  end;
  Canvas.Brush.Color := FOwner.FBkColor;
  Canvas.Pen.Color := FOwner.FTextColor;
  Canvas.Font.Color := FOwner.FTextColor;
  // this one is made to fix the colors
  Canvas.TextOut(R.Left, R.Top, '');
  // otherwise FocusRect won't draw right
  Canvas.DrawFocusRect(R);
end;

procedure TElTreeView.AllocateMouseSelectFrame;
begin
  DrawMouseSelectFrame;
end;

procedure TElTreeView.DeallocateMouseSelectFrame;
begin
  DrawMouseSelectFrame;
end;

procedure TElTreeView.SelectMouseSelectItems;
var si,
    ei   : integer;
    i    : integer;
    al   : TElList;
    Item : TElTreeItem;
    R    : TRect;
    HCol : integer;
    ip   : TSTItemPart;
begin
  i := 0;
  if (FMFSEndItem = FMFSStartItem) then
  begin
    si := FMFSStartItem.AbsoluteIndex;
    ei := si;
    if (FMFSStartItem = FVisible.Last) then
    begin
      R := GetItemRect(FVisible.Count - 1);
      Item := GetItemAt(FMFSStartCoord.X - FOwner.FHPos, FMFSStartCoord.Y + R.Top, ip, HCol);
      if Item = nil then
      begin
        Item := GetItemAt(FMFSEndCoord.X - FOwner.FHPos, FMFSEndCoord.Y + R.Top, ip, HCol);
        if Item = nil then
        begin
          ei := si - 1;
          i := -1;
        end;
      end;
    end;
  end
  else
  begin
    si := FMFSStartItem.AbsoluteIndex;
    ei := FMFSEndItem.AbsoluteIndex;
  end;
  if (si > ei) and (i <> -1) then
  begin
    i := si;
    si := ei;
    ei := i;
  end;

  Al := TElList.Create;
  for i := si to ei do
  begin
    Item := FOwner.Items[i];
    if FOwner.ShowColumns then
    begin
      if Min(FMFSStartCoord.X, FMFSEndCoord.X) < FOwner.FHeader.SectionsWidth then
        Al.Add(Item);
    end
    else
    if Min(FMFSStartCoord.X, FMFSEndCoord.X) < Item.GetWidth then
        Al.Add(Item);
  end;
  FOwner.SetUpdating(true);
  for i := 0 to Al.Count - 1 do
  begin
    Item := TElTreeItem(Al.FastGet(i));
    if FMFSList.IndexOf(Item) = -1 then
    begin
      if FOwner.IgnoreEnabled or Item.Enabled then
        Item.Selected := not Item.Selected;
    end;
  end;
  for i := 0 to FMFSList.Count - 1 do
  begin
    Item := TElTreeItem(FMFSList.FastGet(i));
    if AL.IndexOf(Item) = -1 then
    begin
      if FOwner.IgnoreEnabled or Item.Enabled then
        Item.Selected := not Item.Selected;
    end;
  end;
  FOwner.SetUpdating(false);
  Update;
  FMFSList.Free;
  FMFSList := AL;
end;

{$ifndef CLX_USED}
procedure TElTreeView.WMCancelMode(var Message: TMessage);
begin
  if FMouseSel then
    CancelMouseSel;
  inherited;
end;
{$endif}

procedure TElTreeView.CancelMouseSel;
begin
  FMFSList.Free;
  FMFSList := nil;
  FMouseSel := false;
  DeallocateMouseSelectFrame;
  {$ifndef CLX_USED}
  ReleaseCapture;
  {$else}
  SetMouseGrabControl(nil);
  {$endif}
end;

{$ifndef CLX_USED}
procedure TElTreeView.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  {$ifdef ELPACK_COMPLETE}
  Perform(IFM_REPAINTCHILDREN, 0, 0);
  {$endif}
end;

{$ifdef ELPACK_COMPLETE}
procedure TElTreeView.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;
{$endif}

procedure TElTreeView.CMDeactivate(var Message: TMessage);
begin
  inherited;
  if FOwner.FHideSelect then
    Invalidate;
end;

procedure TElTreeView.CMDrag(var Message: TCMDrag);
var
  TSI: TElTreeItem;
begin
  FOwner.FDragObject := Message.DragRec^.Source;
  inherited;
  with Message, DragRec^ do
  begin
    case DragMessage of
      dmDragEnter:
        FInDragging := true;
      dmDragMove:
        begin
          if FOwner.DragAllowed then
            with ScreenToClient(Pos) do
              DoDragOver(Source, X, Y, Message.Result <> 0);
        end;
      (*
      dmFindTarget:
        begin
          if FOwner.DragAllowed then
            with ScreenToClient(Pos) do
              DoDragOver(Source, X, Y, Message.Result <> 0);
        end;
      *)
      dmDragLeave:
        begin
          FInDragging := false;
          FOwner.FDragObject := nil;
          if FDragExpandTimer <> nil then
          begin
            FDragExpandTimer.Enabled := false;
          end;

          if FOwner.DragAllowed then
          begin
            if FDragScrollTimer <> nil then
            begin
              FDragScrollTimer.Free;
              FDragScrollTimer := nil;
            end;
            TDragObject(Source).HideDragImage;
            TSI := FDropTrg;
            FDropTrg := nil;
            if TSI <> nil then
            begin
              TSI.RedrawItem(false);
              Update;
            end;
            TDragObject(Source).ShowDragImage;
          end;
        end;
      dmDragDrop,
      dmDragCancel:
        begin
          FInDragging := false;
          FOwner.FDragObject := nil;
          if FDragExpandTimer <> nil then
          begin
            FDragExpandTimer.Enabled := false;
            FDragExpandTimer.Free;
            FDragExpandTimer := nil;
          end;

          if FOwner.DragAllowed then
          begin
            if FDragScrollTimer <> nil then
            begin
              FDragScrollTimer.Free;
              FDragScrollTimer := nil;
            end;
            TSI := FDropTrg;
            FDropTrg := nil;
            if TSI <> nil then
            begin
              TSI.RedrawItem(false);
              Update;
            end;
          end;
        end;
    end;
  end;
end;

{$endif}
procedure TElTreeView.InitiateEditOp(Item : TElTreeItem; HCol : integer;
    Immediate : boolean);
begin
  with FOwner do
  if (FShowHeader) then
  begin
    if ((FClickSection = HCol) or (FOwner.QuickEditMode)) and FHeader.Sections[HCol].Editable and
        not (csDesigning in FOwner.ComponentState) then
    begin
      if FEditTimer = nil then
        FEditTimer := TTimer.Create(self);
      FEditTimer.Enabled := false;
      // FEditTimer.Interval := 500;
      if not Immediate then
        FEditTimer.Interval := FOwner.InplaceEditorDelay
      else
        FEditTimer.Interval := 1;
      FEditTimer.OnTimer := OnEditTimer;
      FItemToEdit := Item;
      FEditSect   := HCol;
      FEditTimer.Enabled := true;
    end;
  end
  else
  if not (csDesigning in FOwner.ComponentState) then
  begin
    if FEditTimer = nil then
      FEditTimer := TTimer.Create(self);
    FEditTimer.Enabled := false;
    if not Immediate then
      FEditTimer.Interval := FOwner.InplaceEditorDelay
    else
      FEditTimer.Interval := 1;
    FEditTimer.OnTimer := OnEditTimer;
    FItemToEdit := Item;
    FEditSect   := -1;
    FEditTimer.Enabled := true
  end;
end;

function TElTreeView.IsControlCell(Item : TElTreeItem; SectionIndex : integer): 
    Boolean;
{$ifdef ELTREE_USE_STYLES}
var
  AStyle : TElCellStyle;
  i : integer;
{$endif}
begin
  result := false;
{$ifdef ELTREE_USE_STYLES}
  if Item.UseStyles then
  begin
    if FOwner.VirtualityLevel = vlNone then
    begin
      i := SectionIndex;
      if i = FOwner.FMainTreeCol then
        AStyle := Item.MainStyle
      else
      begin
        if i > FOwner.FMainTreeCol then Dec(i);
        if Item.StylesCount > i then
          AStyle := Item.Styles[i]
        else
          AStyle := Item.MainStyle;
      end;
    end
    else
    begin
      AStyle := VirtStyle;
      FOwner.TriggerVirtualStyleNeeded(Item, SectionIndex, AStyle);
    end;
    if (AStyle <> nil) and (not AStyle.OwnerProps) and (AStyle.Control <> nil) and (AStyle.Control.Visible) then
      result := true;
  end;
  {$endif}
end;

// ****************************************************************************
//                                   TElTreeItem
// ****************************************************************************

constructor TElTreeItem.Create(AOwner: TCustomElTree);
begin
  inherited Create;
  FOwner := AOwner;
  FBoolData1 := ibfParentColors or ibfParentStyle or ibfCheckBoxEnabled or
                ibfEnabled or ibfUseBkColor or ibfOwnerHeight or
                ibfDrawHLine or ibfAllowSelection or ibfAllowEdit;

  FState := [];
  FImageIndex := -1;
  FStImageIndex := -1;
  FImageIndex2 := -1;
  FStImageIndex2 := -1;
  FOverlayIndex := -1;
  FOverlayIndex2 := -1;
  FCheckBoxState := cbUnchecked;
  FCheckBoxType := ectCheckBox;
  FBorderSpaceColor := clWindow;
{$IFNDEF LITE}
  FComplexHeight := (FComplexHeight and $FFFF0000) or WORD(-1);
  //FRealHeight  := -1;
{$ENDIF LITE}
end;

destructor TElTreeItem.Destroy;
begin
  if (FOwner <> nil) and ((FIState and tisSelected) > 0) then
  begin
    with FOwner do
    begin
      if FMultiSelect then
        FSelectedList.Remove(Self);
    end;
  end;
  if FChildren <> nil then
    FChildren.Free;
  FDataInterface := nil;
{$IFDEF HAS_HTML_RENDER}
  if IsHTML then
    inc(FOwner.TotalVarHeightCount);
  IsHTML := false;
{$ENDIF}
  inherited Destroy;
end;

{$ifdef ELTREE_USE_STYLES}
procedure TElTreeItem.CreateStyles;
begin
  if FStaticData <> nil then
  with FStaticData^ do
  begin
    FStyles := TElList.Create;
    FStyles.OnDelete := OnStyleDelete;
  end;
end;
{$endif}

procedure TElTreeItem.OnStyleDelete(Sender: TObject; Item: pointer);
begin
  TElCellStyle(Item).Free;
end;

type

  TItemPersistInfo = record
    FState        : TSTIStates;
    FImageIndex   : integer;
    FStImageIndex : integer;
    FImageIndex2  : integer;
    FStImageIndex2: integer;
    FBoolData1,
    FBoolData2    : integer;
    FColor,
    FRowBkColor,
    FBkColor      : TColor;
    FCheckBoxState: TCheckBoxState;
    FCheckBoxType : TElCheckBoxType;
    FHeight       : Cardinal;
    FBorderStyle  : TElItemBorderStyle;
    FIndentAdjust : integer;
    FStrikedLineColor: TColor;
    FTag          : Integer;
    FOverlayImage : integer;
    FOtherData    : array[0..14] of integer;
  end;

  TS28aItemInfo = record
    FState       : TSTIStates;
    FImageIndex  : integer;
    FStImageIndex: integer;
    ForceButtons : LongBool;
    FColor,
    FRowBkColor,
      FBkColor   : TColor;
    FParentColors: boolean;
    FParentStyle : boolean;
    FCheckBoxEnabled: Boolean;
    FCheckBoxState: TCheckBoxState;
    FShowCheckBox: Boolean;
    FCheckBoxType: TElCheckBoxType;
    FImageIndex2: integer;
    FStImageIndex2: integer;
    FEnabled: boolean;
    FHidden: boolean;
    FUseBkColor : boolean;
{$IFNDEF LITE}
    FMultiline    : boolean;
    FOwnerHeight  : boolean;
    FHeight       : Cardinal;
    FSuppressLines: boolean;
{$ENDIF}
{$IFDEF HAS_HTML_RENDER}
    FIsHTML   : boolean;
{$ENDIF}
  end;
  TS28ItemInfo = record
    FState: TSTIStates;
    FImageIndex: integer;
    FStImageIndex: integer;
    ForceButtons: LongBool;
    FColor,
      FBkColor: TColor;
    FParentColors: boolean;
    FParentStyle: boolean;
    FCheckBoxEnabled: Boolean;
    FCheckBoxState: TCheckBoxState;
    FShowCheckBox: Boolean;
    FCheckBoxType: TElCheckBoxType;
    FImageIndex2: integer;
    FStImageIndex2: integer;
    FEnabled: boolean;
    FHidden: boolean;
{$IFNDEF LITE}
    FMultiline    : boolean;
    FOwnerHeight  : boolean;
    FHeight       : Cardinal;
    FSuppressLines: boolean;
{$ENDIF}
{$IFDEF HAS_HTML_RENDER}
    FIsHTML   : boolean;
{$ENDIF}
  end;

  PStyleInfo = ^TStyleInfo;
  TStyleInfo = record
    FCellBkColor: TColor;
    FTextBkColor: TColor;
    FTextColor: TColor;
    FTextFlags: DWORD;
    FPicture: TBitmap;
    FCellType: TElFieldType;
    FStyle: TElSectionStyle;
    FOwnerProps: Boolean;
    FUseBkColor: boolean;
    FFontSize: integer;
    FFontStyles: TFontStyles;
    FTag: integer;
  end;

procedure StateSetToStateInt(FState : TSTIStates; var FIState : Integer);
begin
  FIState := 0;
  if stsFocused in FState then FIState := FIState or tisFocused else FIState := FIState and (not tisFocused);
  if stsSelected in FState then FIState := FIState or tisSelected else FIState := FIState and (not tisSelected);
  if stsCut in FState then FIState := FIState or tisCut else FIState := FIState and (not tisCut);
  if stsExpanded in FState then FIState := FIState or tisexpaNded else FIState := FIState and (not tisExpanded);
  if stsBold in FState then FIState := FIState or tisBold else FIState := FIState and (not tisBold);
  if stsItalic in FState then FIState := FIState or tisItalic else FIState := FIState and (not tisItalic);
  if stsUnderlined in FState then FIState := FIState or tisUnderlined else FIState := FIState and (not tisUnderlined);
  if stsStrikeOut in FState then FIState := FIState or tisStrikeOut else FIState := FIState and (not tisStrikeOut);
end;

procedure TElTreeItem.ReadData(Stream: TStream);
var
  i, j, k: integer;
  s: AnsiString;
  t28  : TS28ItemInfo;
  t28a : TS28aItemInfo;
  PInfo: TItemPersistInfo;
  ver  : integer;
{$ifdef ELTREE_USE_STYLES}
  SI   : TStyleInfo;
  Style: TElCellStyle;
{$endif}
  {$ifdef ELPACK_UNICODE}
  WS   : WideString;
  {$endif}
  b    : boolean;
  Child: TElTreeItem;
begin
  Stream.ReadBuffer(k, SizeOf(k));
  Ver := 0;
  if k < 0 then
  begin
    ver := k;
    Stream.ReadBuffer(k, SizeOf(k));
  end;
  SetLength(s, k);
  Stream.ReadBuffer(PAnsiChar(s)^, k);
  if FStaticData <> nil then
    FStaticData.FText := s;

  OutputDebugString(PChar(IntToStr(SizeOf(char))));

  if
{$IFDEF ELPACK_COMPLETE}
    (Ver = -7) or
{$ENDIF}
{$IFDEF LITE}
    (Ver = -7) or
{$ENDIF}
    (Ver = -8) or
    (Ver = -9) then
  begin
    Stream.ReadBuffer(T28, SizeOf(T28));
    Stream.ReadBuffer(b, SizeOf(b));
    if b then
      FBoolData1 := FBoolData1 or ibfUseBkColor
    else
      FBoolData1 := FBoolData1 and not ibfUseBkColor;

    FState := T28.FState - [stsFocused, stsSelected];
    StateSetToStateInt(FState, FIState);
    FColor := T28.FColor;
    FBkColor := T28.FBkColor;
    FImageIndex := T28.FImageIndex;
    FStImageIndex := T28.FStImageIndex;

    if T28.FParentColors then
      FBoolData1 := FBoolData1 or ibfParentColors
    else
      FBoolData1 := FBoolData1 and not ibfParentColors;
    if T28.FParentStyle then
      FBoolData1 := FBoolData1 or ibfParentStyle
    else
      FBoolData1 := FBoolData1 and not ibfParentStyle;
    if T28.ForceButtons then
      FBoolData1 := FBoolData1 or ibfForceButtons
    else
      FBoolData1 := FBoolData1 and not ibfForceButtons;
    if T28.FCheckBoxEnabled then
      FBoolData1 := FBoolData1 or ibfCheckBoxEnabled
    else
      FBoolData1 := FBoolData1 and not ibfCheckBoxEnabled;
    if T28.FShowCheckBox then
      FBoolData1 := FBoolData1 or ibfShowCheckBox
    else
      FBoolData1 := FBoolData1 and not ibfShowCheckBox;
    if T28.FEnabled then
      FBoolData1 := FBoolData1 or ibfEnabled
    else
      FBoolData1 := FBoolData1 and not ibfEnabled;
    if T28.FHidden then
      FBoolData1 := FBoolData1 or ibfHidden
    else
      FBoolData1 := FBoolData1 and not ibfHidden;


    FCheckBoxState := T28.FCheckBoxState;
    FCheckBoxType := T28.FCheckBoxType;
    FImageIndex2 := T28.FImageIndex2;
    FStImageIndex2 := T28.FStImageIndex2;

{$IFNDEF LITE}
    if T28.FMultiline then
      FBoolData1 := FBoolData1 or ibfMultiline
    else
      FBoolData1 := FBoolData1 and not ibfMultiline;
    if T28.FOwnerHeight then
      FBoolData1 := FBoolData1 or ibfOwnerHeight
    else
      FBoolData1 := FBoolData1 and not ibfOwnerHeight;
    if T28.FSuppressLines then
      FBoolData1 := FBoolData1 or ibfSuppressLines
    else
      FBoolData1 := FBoolData1 and not ibfSuppressLines;
    {
    FMultiline    := T28.FMultiline;
    FOwnerHeight  := T28.FOwnerHeight;
    FSuppressLines:= T28.FSuppressLines;
    }
    FComplexHeight:= T28.FHeight shl 16 or (FComplexHeight and $0000FFFF);
    //FHeight       := T28.FHeight;

{$ENDIF}
{$IFDEF HAS_HTML_RENDER}
    IsHTML       := T28.FIsHTML;
{$ENDIF}
  end
  else
    if Ver = -10 then
    begin
      Stream.ReadBuffer(T28a, SizeOf(T28a));
      Stream.ReadBuffer(b, SizeOf(b));
      if b then
        FBoolData1 := FBoolData1 or ibfUseBkColor
      else
        FBoolData1 := FBoolData1 and not ibfUseBkColor;

      FState := T28a.FState - [stsFocused, stsSelected];
      StateSetToStateInt(FState, FIState);

      FColor := T28a.FColor;
      FBkColor := T28a.FBkColor;
      FRowBkColor := T28a.FRowBkColor;
      FImageIndex := T28a.FImageIndex;
      FStImageIndex := T28a.FStImageIndex;

      FCheckBoxState := T28a.FCheckBoxState;
      FCheckBoxType := T28a.FCheckBoxType;

      if T28a.FParentColors then
        FBoolData1 := FBoolData1 or ibfParentColors
      else
        FBoolData1 := FBoolData1 and not ibfParentColors;
      if T28a.FParentStyle then
        FBoolData1 := FBoolData1 or ibfParentStyle
      else
        FBoolData1 := FBoolData1 and not ibfParentStyle;
      if T28a.ForceButtons then
        FBoolData1 := FBoolData1 or ibfForceButtons
      else
        FBoolData1 := FBoolData1 and not ibfForceButtons;
      if T28a.FCheckBoxEnabled then
        FBoolData1 := FBoolData1 or ibfCheckBoxEnabled
      else
        FBoolData1 := FBoolData1 and not ibfCheckBoxEnabled;
      if T28a.FShowCheckBox then
        FBoolData1 := FBoolData1 or ibfShowCheckBox
      else
        FBoolData1 := FBoolData1 and not ibfShowCheckBox;
      if T28a.FEnabled then
        FBoolData1 := FBoolData1 or ibfEnabled
      else
        FBoolData1 := FBoolData1 and not ibfEnabled;
      if T28a.FHidden then
        FBoolData1 := FBoolData1 or ibfHidden
      else
        FBoolData1 := FBoolData1 and not ibfHidden;

  {$IFNDEF LITE}
      if T28a.FMultiline then
        FBoolData1 := FBoolData1 or ibfMultiline
      else
        FBoolData1 := FBoolData1 and not ibfMultiline;
      if T28a.FOwnerHeight then
        FBoolData1 := FBoolData1 or ibfOwnerHeight
      else
        FBoolData1 := FBoolData1 and not ibfOwnerHeight;
      if T28a.FSuppressLines then
        FBoolData1 := FBoolData1 or ibfSuppressLines
      else
        FBoolData1 := FBoolData1 and not ibfSuppressLines;

      FComplexHeight:= T28a.FHeight shl 16 or (FComplexHeight and $0000FFFF);

  {$ENDIF}
  {$IFDEF HAS_HTML_RENDER}
      IsHTML       := T28a.FIsHTML;
  {$ENDIF}
  end
  else
  if Ver <= -11 then
  begin
    Stream.ReadBuffer(PInfo, SizeOf(TItemPersistInfo));
    FState := PInfo.FState - [stsFocused, stsSelected];
    StateSetToStateInt(FState, FIState);

    FImageIndex   := PInfo.FImageIndex;
    FStImageIndex := PInfo.FStImageIndex;
    FImageIndex2  := PInfo.FImageIndex2;
    FStImageIndex2:= PInfo.FStImageIndex2;
    FBoolData1    := PInfo.FBoolData1;
    FColor        := PInfo.FColor;
    FRowBkColor   := PInfo.FRowBkColor;
    FBkColor      := PInfo.FBkColor;
    FCheckBoxState:= PInfo.FCheckBoxState;
    FCheckBoxType := PInfo.FCheckBoxType;
    FBorderStyle  := PInfo.FBorderStyle;
    FIndentAdjust := PInfo.FIndentAdjust;
    FStrikedLineColor:= PInfo.FStrikedLineColor;
    FTag          := PInfo.FTag;

{$ifndef LITE}
    FComplexHeight:= PInfo.FHeight shl 16 or (FComplexHeight and $0000FFFF);
{$endif}
  {$IFDEF HAS_HTML_RENDER}
    if IsHTML then
    begin
      FBoolData1 := FBoolData1 and not ibfIsHTML;
      IsHTML := true;
    end;
  {$ENDIF}
    if Ver <= -13 then
    begin
      FOverlayIndex := PInfo.FOverlayImage and $0000FFFF;
      FOVerlayIndex2 := (PInfo.FOverlayImage and $FFFF0000) shr 16;
    end;
  end;
{$ifndef LITE}
  if (not OwnerHeight)
  {$ifdef HAS_HTML_RENDER}
  or IsHTML
  {$endif}
  then
    inc(Owner.TotalVarHeightCount);
{$endif}

  Stream.ReadBuffer(i, SizeOf(i));
  if i = -1 then
  begin
    {$ifdef ELPACK_UNICODE}
    if Ver <= -12 then
    begin
      ReadWideStringFromStream(Stream, WS);
      if FStaticData <> nil then
        FStaticData.FHint := WS;
    end
    else
    {$endif}
    begin
      ReadStringFromStreamA(Stream, s);
      if FStaticData <> nil then
        FStaticData.FHint := s;
    end;
    Stream.ReadBuffer(i, SizeOf(i));
  end;
  for j := 0 to i - 1 do
  begin
    Stream.ReadBuffer(k, SizeOf(k));
    SetLength(s, k);
    Stream.ReadBuffer(pchar(s)^, k);
    if FStaticData <> nil then
      FStaticData.FColText.Add(s);
  end;
  if Ver < 0 then
  begin
{$ifdef ELTREE_USE_STYLES}
    Stream.ReadBuffer(b, sizeof(b));
    if b then
      FBoolData1 := FBoolData1 or ibfUseStyles
    else
      FBoolData1 := FBoolData1 and not ibfUseStyles;
{$else}
    Stream.ReadBuffer(b, sizeof(b));
{$endif}
{$ifdef ELTREE_USE_STYLES}
    if (FBoolData1 and ibfUseStyles) = ibfUseStyles then
    begin
      if Ver < -1 then
        Stream.ReadBuffer(b, sizeof(boolean))
      else
        b := false;
      if b then
      begin
        if Assigned(FOwner.FOnLoad) then FOwner.FOnLoad(Self, Stream, MainStyle);
      end
      else
      begin
        Stream.ReadBuffer(SI, sizeof(TStyleInfo));
        with MainStyle do
        begin
          FCellBkColor := SI.FCellBkColor;
          FTextBkColor := SI.FTextBkColor;
          FTextColor := SI.FTextColor;
          FTextFlags := SI.FTextFlags;
          FPicture := SI.FPicture;
          FCellType := SI.FCellType;
          FStyle := SI.FStyle;
          FOwnerProps := SI.FOwnerProps;
          FFontSize := SI.FFontSize;
          FFontStyles := SI.FFontStyles;
          FTag := SI.FTag;
          FUseBkColor := SI.FUseBkColor;
        end;
        ReadStringFromStreamA(Stream, s);
        MainStyle.FFontName := s;
      end;
      Stream.ReadBuffer(i, SizeOf(i));
      for j := 1 to i do
      begin
        Style := AddStyle;
        if b then
        begin
          if Assigned(FOwner.FOnLoad) then FOwner.FOnLoad(Self, Stream, Style);
        end else
        begin
          Stream.ReadBuffer(SI, sizeof(TStyleInfo));
          with Style do
          begin
            FCellBkColor := SI.FCellBkColor;
            FTextBkColor := SI.FTextBkColor;
            FTextColor := SI.FTextColor;
            FTextFlags := SI.FTextFlags;
            FPicture := SI.FPicture;
            FCellType := SI.FCellType;
            FStyle := SI.FStyle;
            FOwnerProps := SI.FOwnerProps;
            FFontSize := SI.FFontSize;
            FFontStyles := SI.FFontStyles;
            FTag := SI.FTag;
            FUseBkColor := SI.FUseBkColor;
          end;
          ReadStringFromStreamA(Stream, s);
          Style.FFontName := s;
        end;
      end; // for
    end;
{$endif}
  end;
  FOwner.TriggerItemLoadEvent(Stream, Self);
  Stream.ReadBuffer(i, SizeOf(i));
  for j := 0 to i - 1 do
  begin
    Child := FList.AddItem(self);
    Child.ReadData(Stream);
  end;
end;

procedure TElTreeItem.WriteData(Stream: TStream);
var
  i, j, k: integer;
  s: AnsiString;
  sTmp: AnsiString;
  PInfo : TItemPersistInfo;
  p: pchar;
{$ifdef ELTREE_USE_STYLES}
  SI: TStyleInfo;
  Style: TElCellStyle;
{$endif}
  b: boolean;
  {$ifdef ELPACK_UNICODE}
  WS: WideString;
  {$endif}
begin
  // write version
  k := -13;
  Stream.WriteBuffer(k, SizeOf(k));
  // write text data
  if FStaticData <> nil then
    s := FStaticData.FText;
  k := length(s);
  Stream.WriteBuffer(k, SizeOf(k));
  Stream.WriteBuffer(PChar(s)^, k);

  OutputDebugString(PChar(IntToStr(SizeOf(char))));
  // write binary data

  PInfo.FState := FState;
  PInfo.FImageIndex := FImageIndex;
  PInfo.FImageIndex2 := FImageIndex2;
  PInfo.FStImageIndex := FStImageIndex;
  PInfo.FStImageIndex2 := FStImageIndex2;
  PInfo.FBoolData1 := FBoolData1;
  PInfo.FColor := FColor;
  PInfo.FBkColor := FBkColor;
  PInfo.FRowBkColor := FRowBkColor;

  PInfo.FCheckBoxState := FCheckBoxState;
  PInfo.FCheckBoxType := FCheckBoxType;
  PInfo.FBorderStyle := FBorderStyle;
  PInfo.FIndentAdjust := FIndentAdjust;
  PInfo.FStrikedLineColor := FStrikedLineColor;
  PInfo.FTag := FTag;
  PInfo.FOverlayImage := FOverlayIndex or (FOverlayIndex2 shl 16);

{$IFNDEF LITE}
  PInfo.FHeight := FComplexHeight shr 16;
{$ENDIF}

  Stream.WriteBuffer(PInfo, SizeOf(PInfo));
  // write SubStrings
  i := -1;
  Stream.WriteBuffer(i, SizeOf(i));
  {$ifdef ELPACK_UNICODE}
  WS := '';
  if FStaticData <> nil then
    WS := FStaticData.FHint;
  WriteWideStringToStream(Stream, WS);
  {$else}
  S := '';
  if FStaticData <> nil then
    s := FStaticData.FHint;
  WriteStringToStream(Stream, S);
  {$endif}
  if FStaticData <> nil then
    i := FStaticData.FColText.Count
  else
    i := 0;
  Stream.WriteBuffer(i, SizeOf(i));
  for j := 0 to i - 1 do
  begin
    if FStaticData <> nil then
      s := FStaticData.FColText[j]
    else
      s := '';
    k := length(s);
    Stream.WriteBuffer(k, SizeOf(k));
    Stream.WriteBuffer(pchar(s)^, k);
  end;
{$ifdef ELTREE_USE_STYLES}
  b := (FBoolData1 and ibfUseStyles) = ibfUseStyles;
  Stream.WriteBuffer(b, sizeof(b));
{$else}
  b := false;
  Stream.WriteBuffer(b, sizeof(b));
{$endif}
{$ifdef ELTREE_USE_STYLES}
  if (FBoolData1 and ibfUseStyles) = ibfUseStyles then
  begin
    b := Assigned(FOwner.FOnSave);
    Stream.WriteBuffer(b, sizeof(boolean));
    if FStaticData <> nil then
    begin
      if FStaticData.FStyles = nil then
        i := 0
      else
        i := FStaticData.FStyles.Count;
      for j := 0 to i do
      begin
        if j = 0 then
          Style := MainStyle
        else
          Style := TElCellStyle(FStaticData.FStyles[j - 1]);
        if b then
          FOwner.FOnSave(Self, Stream, Style)
        else
        begin
          with Style do
          begin
            SI.FCellBkColor := FCellBkColor;
            SI.FTextBkColor := FTextBkColor;
            SI.FTextColor := FTextColor;
            SI.FTextFlags := FTextFlags;
            SI.FPicture := FPicture;
            SI.FCellType := FCellType;
            SI.FStyle := FStyle;
            SI.FOwnerProps := FOwnerProps;
            SI.FFontSize := FFontSize;
            SI.FFontStyles := FFontStyles;
            SI.FTag := FTag;
            SI.FUseBkColor := FUseBkColor;
          end;
          Stream.WriteBuffer(SI, sizeof(TStyleInfo));
          WriteStringToStream(Stream, Style.FFontName);
        end;
        if j = 0 then Stream.WriteBuffer(i, sizeof(i));
      end;
    end;
  end;
{$endif}
  FOwner.TriggerItemSaveEvent(Stream, Self);
  // write subitems
  if FChildren = nil then i := 0 else i := FChildren.Count;
  Stream.WriteBuffer(i, SizeOf(i));
  for j := 0 to i - 1 do
      TElTreeItem(THackElList(FChildren).FList[j]).WriteData(Stream);
end;

procedure TElTreeItem.Assign(Source: TPersistent);
var
  Item, Child: TElTreeItem;
  i, j : integer;
{$ifdef ELTREE_USE_STYLES}
  Style: TElCellStyle;
{$endif}
begin
  if Source is TElTreeItem then
  begin
    if FOwner <> nil then
      FOwner.IsUpdating := true;
    Item := TElTreeItem(Source);
    if (FStaticData <> nil) then
    begin
      FStaticData.FText := Item.Text;
      if Item.ColumnText <> nil then
        FStaticData.FColText.Assign(Item.ColumnText);
      FStaticData.FHint := Item.Hint;
    end;
    Data := Item.Data;
    FColor := Item.FColor;
    FBkColor := Item.FBkColor;
    FRowBkColor := Item.FRowBkColor;
    FState := Item.FState;
    FState := FState - [stsFocused, stsSelected];
    FBoolData1 := Item.FBoolData1 and not (ibfIsHTML or ibfImageDrawn or ibfImageDrawn2);

{$ifdef HAS_HTML_RENDER}
    IsHTML := Item.IsHTML;
{$endif}
    FImageIndex := Item.FImageIndex;
    FStImageIndex := Item.FStImageIndex;
    FImageIndex2 := Item.FImageIndex2;
    FStImageIndex2 := Item.FStImageIndex2;
    FCheckBoxState := Item.FCheckBoxState;
    FCheckBoxType := Item.FCheckBoxType;
    FBorderStyle := Item.FBorderStyle;
    FIndentAdjust := Item.FIndentAdjust;
    FStrikedLineColor := Item.FStrikedLineColor;
    {$ifndef LITE}
    FComplexHeight := Item.FComplexHeight;
    {$endif}
    FOverlayIndex := Item.FOverlayIndex;
    FOverlayIndex2 := Item.FOverlayIndex2;

    FObject := Item.FObject;
    FDataInterface := Item.FDataInterface;
    FTag := Item.FTag;
{$ifdef ELTREE_USE_STYLES}
    if (FStaticData <> nil) and (Item.FStaticData <> nil) then
    begin
      FStaticData.FMainStyle.Assign(Item.MainStyle);
      if FStaticData.FStyles <> nil then
        FStaticData.FStyles.Clear;
      if Item.FStaticData.FStyles <> nil then
      begin
        if FStaticData.FStyles = nil then
          CreateStyles;
        for i := 0 to Item.FStaticData.FStyles.Count - 1 do
        begin
          Style := AddStyle;
          Style.Assign(Item.Styles[i]);
        end;
      end;
    end;
{$endif}
    Clear;
    if Item.FChildren <> nil then
    begin
      if FChildren = nil then
        FChildren := TElList.Create;
      j := 0;
      for i := 0 to Item.FChildren.Count - 1 do
      begin
        Child := FList.CreateItem(FOwner);
        if FOwner.VirtualityLevel = vlNone then
          Child.NewStaticData;

        if FOwner.FAllList.Count > 0 then
           j := FOwner.FAllList.IndexOfFrom(j, GetLastSubItem) + 1
        else
           j := 0;
        FChildren.Add(Child);
        FOwner.FAllList.Insert(j, Child);
        Child.FRoot := FRoot;
        Child.FList := FList;
        Child.FParent := self;
        Child.FOwner := FOwner;
        Child.Assign(TElTreeItem(THackElList(Item.FChildren).FList[i]));
      end;
    end else
      if FChildren <> nil then
      begin
        FChildren.Free;
        FChildren := nil;
      end;

    if (FOwner <> nil) then
    with FOwner.FView do
    begin
      FOwner.FUpdated := true;
      FVisUpdated := true;
      FClearVis := true;
      FClearAll := true;
      FOwner.IsUpdating := false;
    end;
  end else inherited Assign(Source);
end;

procedure TElTreeItem.SetUseBkColor(newValue: Boolean);
begin
  if ((FBoolData1 and ibfUseBkColor) = ibfUseBkColor) <> newValue then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfUseBkColor
    else
      FBoolData1 := FBoolData1 and not ibfUseBkColor;
    UpdateItem;
  end;
end;

procedure TElTreeItem.SetColor(index: integer; value: TColor);
begin
  if index = 1 then
  begin
    if FColor = value then exit;
    FColor := value;
  end
  else
  begin
    if FBkColor = value then exit;
    FBkColor := value;
  end;
  UpdateItem;
  //REMOVE: RedrawItem(true);
  //REMOVE: if FOwner.IsUpdating then FOwner.FUpdated:=true else FOwner.Update;
end;

function TElTreeItem.GetParent;
begin
  if FParent = FRoot then result := nil else result := FParent;
end;

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
procedure TElTreeItem.EditText;
begin
  FOwner.FView.DoEditItem(self, -1);
end;
{$else}
procedure TElTreeItem.EditText;
begin
  FOwner.FView.DoEditItem(self, -1);
end;
{$endif}
{$endif}

function TElTreeItem.GetFullNameEx;
begin
  if (FParent <> FRoot) or (AddRoot) then
    if Separator <> #0 then
      result := separator + Text
    else
      result := Text
  else
    result := Text;
  if FParent <> FRoot then
    result := FParent.GetFullNameEx(separator, AddRoot) + result;
end;

function TElTreeItem.GetFullName;
begin
  if Separator <> #0 then
    result := separator + Text
  else
    result := Text;
  if FParent <> FRoot then
    result := FParent.GetFullName(separator) + result;
end;

function TElTreeItem.GetChildByIndex;
begin
  if (index < 0) or (FChildren = nil) or (index >= FChildren.Count) then
    raise EElTreeError.Create(STExOutOfBounds);
  result := TElTreeItem(THackElList(FChildren).FList[index]);
end;

function TElTreeItem.GetChildrenCount;
var
  i, j: integer;
begin
  if FChildren = nil then
     Result := 0
  else
     Result := FChildren.Count;
  j := result;
  for i := 0 to j - 1 do
    result := result + TElTreeItem(THackElList(FChildren).FList[i]).ChildrenCount;
end;

function TElTreeItem.GetNextChild;
var
  i: integer;
begin
  if FChildren = nil then
     result := nil
  else
  begin
    i := Child.Index;
    if (i = (FChildren.Count - 1)) then
       result := nil
    else
       result := TElTreeItem(THackElList(FChildren).FList[i + 1]);
  end;
end;

function TElTreeItem.GetPrevChild;
var
  i: integer;
begin
  if FChildren = nil then
     result := nil
  else
  begin
    i := Child.Index;
    if i = 0 then
       result := nil
    else
      result := TElTreeItem(THackElList(FChildren).FList[i - 1]);
  end;
end;

function TElTreeItem.GetFirstVisibleChild;
var
  i: integer;
  ChC : integer;
  Item: TElTreeItem;
begin
  result := nil;
  if (not FOwner.FilteredVisibility) then
  begin
    result := GetFirstChild;
    exit;
  end;
  if (FChildren <> nil) then
  begin
    Chc := FChildren.Count;
    if (Chc > 0) then
    for i := 0 to ChC - 1 do
    begin
      Item := TElTreeItem(THackElList(FChildren).FList[i]);
      if not Item.Hidden then
      begin
        result := Item;
        exit;
      end;
    end;
  end;
end;

function TElTreeItem.GetFirstChild;
begin
  if (FChildren = nil) or (FChildren.Count = 0) then
     result := nil
  else
     result := TElTreeItem(THackElList(FChildren).FList[0]);
end;

function TElTreeItem.GetLastChild;
begin
  if (FChildren = nil) or (FChildren.Count = 0) then
     result := nil
  else
     result := TElTreeItem(FChildren.Last);
end;

function TElTreeItem.GetFirstSibling;
begin
  if FParent = nil then
     result := nil
  else
     result := FParent.GetFirstChild;
end;

function TElTreeItem.GetLastSibling;
begin
  if FParent = nil then
     result := nil
  else
     result := FParent.GetLastChild;
end;

function TElTreeItem.GetNextSibling;
begin
  if FParent = nil then
     result := nil
  else
     result := FParent.GetNextChild(self);
end;

function TElTreeItem.GetPrevSibling;
begin
  if FParent = nil then
     result := nil
  else
    result := FParent.GetPrevChild(self);
end;

function TElTreeItem.GetFullExpand;
begin
  if Parent = nil then
     result := true
  else
     result := (Parent.FullyExpanded and Parent.Expanded);
end;

function TElTreeItem.GetSelected : boolean;
begin
  result := (FIState and tisSelected) > 0;
end;

procedure TElTreeItem.SetSelected(newValue : boolean);
var b : boolean;
begin
   if FOwner.FMultiSelect then
   begin
     b := ((FIState and tisSelected) > 0) xor newValue;
     if b then
     begin
       if newValue = false then
       begin
         FOwner.FSelChange := true;
         Exclude(FState, stsSelected);
         FIState := FIState and (not tisSelected);
         FOwner.FSelectedList.Remove(Self);
         with FOwner.FView do
         if FSelected = Self then FSelected := nil;
       end else
       begin

         if FOwner.FKeepSelectionWithinLevel and (FOwner.FSelectedList.Count > 0) and
            (FOwner.FSelectedList[0] <> Self) then
         begin
           if Level <> TElTreeItem(FOwner.FSelectedList[0]).Level then
             exit;
         end;
         FOwner.FSelChange := true;
         Include(FState, stsSelected);
         FIState := FIState or tisSelected;
         FOwner.FSelectedList.Add(Self);
       end;
       FOwner.TriggerItemSelectedChangeEvent(Self);
     end;
   end;
   UpdateItem;
end;

procedure TElTreeItem.SetState;
var
  FOldState: TSTIStates;
  reqitemch: boolean;
begin
  reqitemch := true;
  FOldState := FState;
  case index of
    stsFocused: begin
        if (value = false) then
        begin
          if FOwner.ItemFocused = Self then
             FOwner.ItemFocused := nil;
        end
        else
           FOwner.ItemFocused := self;
        exit;
      end;
    stsCut:
      begin
        if value = false then Exclude(FState, stsCut)
        else Include(FState, stsCut);
      end;
    stsUnderlined:
      begin
        if value = false then Exclude(FState, stsUnderlined)
        else Include(FState, stsUnderlined);
      end;
    stsBold:
      begin
        if value = false then Exclude(FState, stsBold)
        else Include(FState, stsBold);
      end;
    stsItalic:
      begin
        if value = false then Exclude(FState, stsItalic)
        else Include(FState, stsItalic);
      end;
    stsStrikeOut:
      begin
        if value = false then Exclude(FState, stsStrikeOut)
        else Include(FState, stsStrikeOut);
      end;
  end; // case
  if FOldState <> FState then
  begin
    if (index in [stsBold, stsItalic]) and (not FOwner.FShowHeader) and fullyExpanded then
    begin
      if FOwner.FUpdateCount > 0 then
        FOwner.FView.FHRange := -1
      else
        FOwner.FView.DefineHRange;
    end;
    if reqitemch then FOwner.DoItemChange(self, icmState);
    UpdateItem;
  end;
end;

function TElTreeItem.GetState;
begin
  result := index in FState;
end;

function TElTreeItem.IsExpanded;
begin
  result := stsExpanded in FState;
end;

procedure TElTreeItem.Expand;
var
  i,
  j: integer;
  b: boolean;

begin
  b := true;
  if Self <> FRoot then FOwner.DoItemExpanding(self, b);
  if not (b) then exit;
  FOwner.IsUpdating := true;
  with FOwner.FView do
  begin
    FVisUpdated := true;
    FRangeUpdate := true;
  end;
  FState := FState + [stsExpanded];
  if recursive then
  begin
    if (FChildren = nil) then
       j := 0
    else
       j := FChildren.Count;
    for i := 0 to j - 1 do
        TElTreeItem(THackElList(FChildren).FList[i]).expand(true);
  end;
  if (not FOwner.FShowHeader) and FullyExpanded then
    FOwner.FView.FHRange := -1;
  with FOwner.FView do
  begin
    FClearVis := true;
    FOwner.FUpdated := true;
  end;
  FOwner.IsUpdating := false;
  if Self <> FRoot then FOwner.DoItemExpand(self);
end;

procedure TElTreeItem.Collapse;
var
  i : integer;
  b : boolean;
  AnItem : TElTreeItem;
  IterateProc: TIterateProcAnonymusMethod;
begin
  IterateProc :=  procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
      IterateData: pointer; Tree: TCustomElTree)
    begin
      if not Item.IsUnder(TElTreeItem(IterateData)) then
      begin
        ContinueIterate := false;
        exit;
      end;
      ContinueIterate := true;
      if Item <> IterateData then Item.Selected := false;
    end;
  b := true;
  if Self <> FRoot then FOwner.DoItemCollapsing(self, b);
  if not (b) then exit;
  FOwner.IsUpdating := true;
  with FOwner.FView do
  begin
    FRangeUpdate := true;
    FVisUpdated := true;
  end;
  FState := FState - [stsExpanded];
  with FOwner do
    if (ItemFocused <> nil) and (ItemFocused <> Self) and (ItemFocused.IsUnder(Self)) and (FmovefocusOnCollapse) then
    begin
      if SelectedCount = 1 then
      begin
        AnItem := ItemFocused;
        b := FAutoExpand;
        FAutoExpand := false;
        ItemFocused := Self;
        FAutoExpand := b;
        AnItem.Selected := false;
      end
      else
        ItemFocused := Self;
    end;
  if Recursive then
  begin
    if Assigned(FChildren) then
      for i := 0 to Pred(FChildren.Count) do
        TElTreeItem(THackElList(FChildren).FList[i]).Collapse(True);
  end;

  if FOwner.FDeselectChildrenOnCollapse then
    FOwner.Items.IterateFrom(false, true, IterateProc, Self, Self); (*<+>*)

  if (not FOwner.FShowHeader) and FullyExpanded then
     FOwner.FView.FHRange := -1;
  with FOwner.FView do
  begin
    FClearVis := true;
    FClearAll := true;
  end;
  FOwner.FUpdated := true;
  FOwner.IsUpdating := false;
  if Self <> FRoot then FOwner.DoItemCollapse(self);
end;

function TElTreeItem.GetChildIndex;
begin
  if FChildren = nil then
     result := -1
  else
     result := FChildren.IndexOf(Child);
end;

function TElTreeItem.GetIndex;
begin
  if FParent = nil then
     result := -1
  else
     result := FParent.GetChildIndex(self);
end;

function TElTreeItem.GetVisIndex;
begin
  // no check for root - should never be called for root
  result := FList.GetVisIndex(self);
end;

function TElTreeItem.GetAbsIndex;
begin
  // no check for root - should never be called for root
  result := FList.GetAbsIndex(self);
end;

procedure TElTreeItem.RemoveChild;
var
  i: integer;
begin
  with FOwner.FView do
  begin
    i := FVisible.IndexOf(Child);
    if i >= 0 then
       FVisible.Delete(i);
  end;

  {with FOwner.FView do
    if FSelected = Child then FSelected := nil;
  }

  if FChildren <> nil then
  begin
    i := FChildren.IndexOf(Child);
    if i <> -1 then
       FChildren.Delete(i);
    if FChildren.Count = 0 then
    begin
      FChildren.Free;
      FChildren := nil;
    end;
    //dec(FList.FCount);
  end;
end;

procedure TElTreeItem.RemoveSubChild;
var
  i: integer;
begin
//  Child.FBoolData1 := Child.FBoolData1 or ibfDeleting;
  FOwner.IsUpdating := true;
  Child.ClearSubChild;
  if Child.Hidden then
  begin
    dec(FOwner.TotalHiddenCount);
    Child.FBoolData1 := Child.FBoolData1 and not ibfHidden;
  end;
  if Child = FOwner.FView.FSelected then
  begin
    if csDestroying in FOwner.ComponentState then
      FOwner.SetSelected(nil)
    else
    begin
      FOwner.SetSelected(GetNextChild(Child));
      if FOwner.FView.FSelected = nil then
        FOwner.SetSelected(GetPrevChild(Child));
      if FOwner.FView.FSelected = nil then
        if Self <> FRoot then
          FOwner.SetSelected(Self);
    end;
  end;
  Child.Selected := false;
  // Move focus, if needed
  if Child = FOwner.FView.FFocused then
  begin
    if csDestroying in FOwner.ComponentState then
      FOwner.DoSetFocused(nil, true)
    else
    begin
      FOwner.DoSetFocused(GetNextChild(Child), true);
      if FOwner.ItemFocused = nil then
        FOwner.DoSetFocused(GetPrevChild(Child), true);
      if FOwner.ItemFocused = nil then
        if Self <> FRoot then
          FOwner.DoSetFocused(Self, true);
    end;
  end;
  with FOwner do
    if (FUpdateCount = 0) and (FilteredVisibility) then
       UpdateDiffItems;
  with FOwner.FView do
  begin
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    if FItemToEdit = Child then
       FItemToEdit := nil;
{$endif}
    if FPassedItem = Child then
       FPassedItem := nil;
    if FTrackItem = Child then
       FTrackItem := nil;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
    if (FInpEdit <> nil) then
    begin
      FEditingItem := nil;
      if not FOwner.FDelOnEdit then
         DoEndEdit(true);
    end;
{$else}
    if (FInpEdit <> nil) then
    begin
      FEditingItem := nil;
      if not FOwner.FDelOnEdit then
         DoEndEdit(true);
    end;
{$endif}
{$endif}
  end;
  if (FOwner.FView.FVisible.Count > 0) and not (csDestroying in FOwner.ComponentState) then
  with FOwner.FView do
  begin
    i := FVisible.IndexOfBack(FVisible.Count - 1, Child);
    if (i >= 0) then
       FVisible.Delete(i);
  end;
  FOwner.DoItemDelete(Child);
{$IFNDEF LITE}
  if not Child.OwnerHeight then
     dec(FOwner.TotalVarHeightCount);
  if Child.Multiline and FOwner.FAdjustMultilineHeight then
     dec(FOwner.TotalVarHeightCount);
{$IFDEF HAS_HTML_RENDER}
  if (Child.FBoolData1 and ibfIsHTML) = ibfIsHTML then
     dec(FOwner.TotalVarHeightCount);
{$ENDIF}
{$ENDIF LITE}
  Child.ClearSubChild;
  i := FOwner.FAllList.IndexOfBack(FOwner.FAllList.Count - 1, Child);
  if (i >= 0) then
    FOwner.FAllList.Delete(i);

  with FOwner do
  begin
    FUpdated := true;
    FView.FRangeUpdate := true;
    IsUpdating := false;
  end;
end;

procedure TElTreeItem.DeleteChild;
var
  i: integer;
begin
  Child.FBoolData1 := Child.FBoolData1 or ibfDeleting;
  FOwner.IsUpdating := true;
  Child.Clear;
  if Child.Hidden then
  begin
    dec(FOwner.TotalHiddenCount);
    Child.FBoolData1 := Child.FBoolData1 and not ibfHidden;
  end;
  if Child = FOwner.FView.FSelected then
  begin
    if csDestroying in FOwner.ComponentState then
      FOwner.SetSelected(nil)
    else
    begin
      FOwner.SetSelected(GetNextChild(Child));
      if FOwner.FView.FSelected = nil then
        FOwner.SetSelected(GetPrevChild(Child));
      if FOwner.FView.FSelected = nil then
        if Self <> FRoot then
          FOwner.SetSelected(Self);
    end;
  end;
  Child.Selected := false;
  // Move focus, if needed
  if Child = FOwner.FView.FFocused then
  begin
    if csDestroying in FOwner.ComponentState then
      FOwner.DoSetFocused(nil, true)
    else
    begin
      FOwner.DoSetFocused(GetNextChild(Child), true);
      if FOwner.ItemFocused = nil then
        FOwner.DoSetFocused(GetPrevChild(Child), true);
      if FOwner.ItemFocused = nil then
        if Self <> FRoot then
          FOwner.DoSetFocused(Self, true);
    end;
  end;
  with FOwner do
    if (FUpdateCount = 0) and (FilteredVisibility) then
       UpdateDiffItems;
  with FOwner.FView do
  begin
{$ifdef ELTREE_USE_INPLACE_EDITORS}
    if FItemToEdit = Child then
       FItemToEdit := nil;
{$endif}
    if FPassedItem = Child then
       FPassedItem := nil;
    if FTrackItem = Child then
       FTrackItem := nil;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
    if (FInpEdit <> nil) then
    begin
      FEditingItem := nil;
      if not FOwner.FDelOnEdit then
         DoEndEdit(true);
    end;
{$else}
    if (FInpEdit <> nil) then
    begin
      FEditingItem := nil;
      if not FOwner.FDelOnEdit then
         DoEndEdit(true);
    end;
{$endif}
{$endif}
  end;
  if (FOwner.FView.FVisible.Count > 0) and not (csDestroying in FOwner.ComponentState) then
  with FOwner.FView do
  begin
    i := FVisible.IndexOfBack(FVisible.Count - 1, Child);
    if (i >= 0) then
       FVisible.Delete(i);
  end;
  FOwner.DoItemDelete(Child);
  if FChildren.Count > 0 then
  begin
    i := FChildren.IndexOfBack(FChildren.Count - 1, Child);
    if i <> -1 then
       FChildren.Delete(i);
    if FChildren.Count = 0 then
    begin
      FChildren.Free;
      FChildren := nil;
    end;
  end;
{$IFNDEF LITE}
  if not Child.OwnerHeight then
     dec(FOwner.TotalVarHeightCount);
  if Child.Multiline and FOwner.FAdjustMultilineHeight then
     dec(FOwner.TotalVarHeightCount);
{$IFDEF HAS_HTML_RENDER}
  if (Child.FBoolData1 and ibfIsHTML) = ibfIsHTML then
     dec(FOwner.TotalVarHeightCount);
{$ENDIF}
{$ENDIF LITE}
  Child.Clear;
  i := FOwner.FAllList.IndexOfBack(FOwner.FAllList.Count - 1, Child);
  if (i >= 0) then
    FOwner.FAllList.Delete(i);
  if Child.FStaticData <> nil then
    Child.DisposeStaticData;
  Child.Free;
  with FOwner do
  begin
    FUpdated := true;
    FView.FRangeUpdate := true;
    IsUpdating := false;
  end;
end;

function TElTreeItem.GetLastSubItem : TElTreeItem;
var Item : TElTreeItem;
begin
  result := Self;
  Item := Result.GetLastChild;
  While Item <> nil do
  begin
    result := Item;
    Item := Result.GetLastChild;
  end;
end;

function TElTreeItem.AddChild;
var i : integer;
begin
  if FChildren = nil then
     FChildren := TElList.Create;
  with FOwner do
  begin
    if (FAllList.Count = 0) or (GetLastSubItem = FAllList.Last) then
        FAllList.Add(Child)
    else
    begin
      i := FAllList.IndexOf(GetLastSubItem);
      FAllList.Insert(i + 1, Child);
    end;
  end;
  result := FChildren.Add(Child);
  Child.FRoot := FRoot;

  Child.FParent := Self;
  Child.FList := Self.FList;

  if (FOwner.VirtualityLevel = vlNone) and (Child.FStaticData = nil) then
    Child.NewStaticData;
  FOwner.FUpdated := true;
end;

procedure TElTreeItem.AddExistingChild;
var i : integer;
begin
  with FOwner do
  begin
    if (FAllList.Count = 0) or (GetLastSubItem = FAllList.Last) then
        FAllList.Add(Child)
    else
    begin
      i := FAllList.IndexOf(self);
      FAllList.Insert(i + 1, Child);
    end;
  end;
  Child.FRoot := FRoot;
  Child.FOwner := FOwner;
  Child.FList := FList;
  if Child.FChildren <> nil then
    for i := Child.FChildren.Count - 1 downto 0 do
      Child.AddExistingChild(TElTreeItem(Child.FChildren.Items[i]));
  FOwner.FUpdated := true;
end;

function TElTreeItem.AddLastChild;
begin
  if FChildren = nil then FChildren := TElList.Create;
  with FOwner do
  begin
    FAllList.Add(Child);
  end;
  result := FChildren.Add(Child);
  Child.FRoot := FRoot;
  if FOwner.VirtualityLevel = vlNone then
    Child.NewStaticData;
  FOwner.FUpdated := true;
end;

function TElTreeItem.InsertChild;
var i : integer;
    SI: TElTreeItem;

begin
  if FChildren = nil then FChildren := TElList.Create;
  if Index > FChildren.Count then
    raise EElTreeError.Create(STExOutOfBounds);

  with FOwner do
  begin
    if Count = 0 then
      i := FAllList.IndexOf(Self) + 1
    else
    begin
      if FChildren.Count = Index then
      begin
        SI := TElTreeItem(THackElList(FChildren).FList[Index - 1]).GetLastSubItem;
        if SI = FAllList.Last then
           i := FAllList.Count
        else
           i := FAllList.IndexOf(SI) + 1;
      end
      else
        i := FAllList.IndexOf(THackElList(FChildren).FList[Index]);
    end;
    if i = FAllList.Count then
       FAllList.Add(Child)
    else
       FAllList.Insert(i, Child);
  end;

  FChildren.Insert(Index, Child);
  result := index;
  Child.FParent := self;
  Child.FRoot := FRoot;
  if FOwner.VirtualityLevel = vlNone then
    Child.NewStaticData;
  FOwner.FUpdated := true;
end;

procedure TElTreeItem.MoveTo;
var TSI: TElTreeItem;
begin
  if NewParent = self then exit;
  if NewParent = nil then
     TSI := FRoot
  else
     TSI := NewParent;
  if TSI.FChildren = nil then
     MoveToIns(NewParent, 0)
  else
     MoveToIns(NewParent, TSI.FChildren.Count);
end;

procedure TElTreeItem.MoveToIns(NewParent: TElTreeItem; AnIndex: integer);
var i : integer;
    OldStart,
    OldEnd,
    NewStart : integer;
    aresize  : boolean;
    //OldSel   : boolean;
begin
  if NewParent = self then exit;
  if NewParent = nil then NewParent := FRoot;
  if NewParent.IsUnder(self) then
    raise EElTreeError.Create(STexRecursiveMove);
  FOwner.IsUpdating := true;
  {if FParent.FChildren = nil then
  begin
    i := -1;
    FParent.FChildren := TElList.Create;
  end else }
  i := FParent.FChildren.IndexOf(self);

  with FOwner.FAllList do
  begin
    OldStart := IndexOf(Self);
    OldEnd   := IndexOfFrom(OldStart, GetLastSubItem);
  end;
  aresize := false;
  {if FParent <> nil then }
  //OldSel := FOwner.FView.FSelected = Self;
  FParent.RemoveChild(self);
  if (FParent = NewParent) and (AnIndex > i) and (AnIndex >= FParent.Count) then
  begin
    NewStart := FOwner.FAllList.IndexOf(FParent.GetLastSubItem);
    with FOwner.FAllList do
    if NewStart = Count - 1 then
    begin
      aresize := true;
      Count := Count + 1;
    end;
    inc(NewStart);
    with FParent do
    begin
      if FChildren = nil then
         FChildren := TElList.Create;
      FChildren.Add(self);
    end;
  end
  else
  begin
    with NewParent do
    begin
      if FChildren = nil then
         FChildren := TElList.Create;
      FChildren.Insert(AnIndex, self);
      if AnIndex = 0 then
         NewStart := FOwner.FAllList.IndexOf(NewParent)
      else
         NewStart := FOwner.FAllList.IndexOf(TElTreeItem(THackElList(FChildren).FList[AnIndex - 1]).getlastsuBitem);
    end;
    with FOwner.FAllList do
      if NewStart = Count - 1 then
      begin
        aresize := true;
        Count := Count + 1;
      end;
    inc(NewStart);
  end;
  FOwner.FAllList.MoveRange(OldStart, OldEnd, NewStart);

  //if OldSel then FOwner.FView.FSelected := Self;

  if aresize then
  with FOwner.FAllList do
     Count := Count - 1;
  FParent := NewParent;
  with FOwner do
  begin
    //inc(FItems.FCount);
    if ((FSortMode = smAdd) or (FSortMode = smAddClick)) and (FInSorting = 0) then
    begin
      if (FMainTreeCol = FSortSection) or (FSortSection = -1) then
        FSortRequired := true;
    end;
    with FOwner.FView do
    begin
      FVisUpdated := true;
      FClearVis := true;
      FUpdated := true;
      FClearAll := true;
    end;
    IsUpdating := false;
  end;
end;

procedure TElTreeItem.RedrawItemPart;
var
  R: TRect;
begin
  if (FOwner = nil) or ((FBoolData1 and ibfDeleting) = ibfDeleting) then exit;
  with FOwner do
    R := FView.GetItemRect(FView.FVisible.IndexOf(self));
  inc(R.Bottom);
  if DoCheck and IsRectEmpty(R) then exit;
  R.Left := Left - FOwner.FHPos;
  R.Right := Right - FOwner.FHPos;
  if not (csDestroying in FOwner.ComponentState) then
  {$ifndef CLX_USED}
    InvalidateRect(FOwner.FView.Handle, @R, true);
  {$else}
  begin
    Inc(R.Bottom); Inc(R.Right);
    QWidget_update(FOwner.FView.Handle, @R);
    Dec(R.Bottom); Dec(R.Right);
  end;
  {$endif}
end;

procedure TElTreeItem.RedrawItem;
var
  R: TRect;
begin
  if (FOwner = nil) or ((FBoolData1 and ibfDeleting) = ibfDeleting) then exit;
  if DoCheck and (FOwner.FView.FVisible.IndexOf(Self) = -1) then
    exit;
  with FOwner do
    R := FView.GetItemRect(FView.FVisible.IndexOf(self));
  inc(R.Bottom);
  if DoCheck and IsRectEmpty(R) then exit;
  if not (csDestroying in FOwner.ComponentState) then
  {$ifndef CLX_USED}
    InvalidateRect(FOwner.FView.Handle, @R, true);
  {$else}
  begin
    Inc(R.Bottom); Inc(R.Right);
    QWidget_update(FOwner.FView.Handle, @R);
    Dec(R.Bottom); Dec(R.Right);
  end;
  {$endif}
end;

procedure TElTreeItem.SetText(Value: TElFString);
begin
  if FStaticData <> nil then
  begin
    FStaticData.FText := Value;
    FOwner.DoItemChange(Self, icmText);
    FOwner.IsUpdating := true;
    with FOwner do
    begin
      if (not FShowHeader) and FullyExpanded then
      begin
        if FUpdateCount > 0 then
          FView.FHRange := -1
        else
          FView.DefineHRange;
        FView.FRangeUpdate := true;
        FOwner.FUpdated := true;
      end;
      if (FSortMode = smAdd) or (FSortMode = smAddClick) then
      begin
        if (FMainTreeCol = FSortSection) or (FSortSection = -1) then
        begin
          if FUpdateCount > 0 then
             FSortRequired := true
          else
          begin
            FOwner.TriggerSortBegin;
            FParent.Sort(false);
            FOwner.TriggerSortBegin;
          end;
        end;
      end;
      if FShowHeader and (FUpdateCount = 0) then
         AutoSizeColumn(FMainTreeCol)
      else
         FOwner.FColSizeUpdate := true;
    end;

    {$IFNDEF LITE}
    if (Multiline and FOwner.AdjustMultilineHeight)
        {$IFDEF HAS_HTML_RENDER}
        or IsHTML
        {$ENDIF}
    then
    begin
      {$IFDEF HAS_HTML_RENDER}
      if IsHTML then ReRenderMainText;
      {$ENDIF}

      FComplexHeight := (FComplexHeight and $FFFF0000) or WORD(-1);
      //FRealHeight := -1;

      with FOwner.FView do
      begin
        FVisUpdated := true;
        FClearVis := true;
        FClearAll := true;
        FRangeUpdate := true;
      end;

    end;
    {$ENDIF}
    FOwner.IsUpdating := false;
  end;
  UpdateItem;
end;

function TElTreeItem.GetLevel: integer;
begin
  if Self = FRoot then
    result := -1
  else
    result := FParent.Level + 1;
end;

{Returns true if the item is under then given item in the tree}

function TElTreeItem.IsUnder(Item: TElTreeItem): boolean;
begin
  if Item = nil then
  begin
    result := false;
    exit;
  end;
  if Self = Item then
     result := true
  else
  if (FParent <> nil) and (FParent <> FRoot) then
     result := FParent.IsUnder(Item)
  else
     result := false;
end;

procedure TElTreeItem.SetExpanded;
begin
  if value = Expanded then exit;
  FOwner.IsUpdating := true;
  if value = true then Expand(false) else Collapse(false);
  FOwner.IsUpdating := false;
end;

procedure TElTreeItem.MakeFullyExpanded;
var
  TSI: TElTreeItem;
begin
  if not value then exit;
  FOwner.IsUpdating := true;
  try
    TSI := Self;
    while TSI.FParent <> nil do
    begin
      TSI.FParent.Expand(false);
      TSI := TSI.FParent;
    end; // while
    with FOwner.FView do
    begin
      FClearVis := true;
      FVisUpdated := true;
      FRangeUpdate := true;
    end;
    FOwner.FUpdated := true;
  finally
    FOwner.IsUpdating := false;
  end;
end;

procedure TElTreeItem.SetParentStyle(value: Boolean);
begin
  if value = ((FBoolData1 and ibfParentStyle) = ibfParentStyle) then exit;
  if Value then
    FBoolData1 := FBoolData1 or ibfParentStyle
  else
    FBoolData1 := FBoolData1 and not ibfParentStyle;

  with FOwner do
  begin
    IsUpdating := true;
    if FShowHeader then
      FColSizeUpdate := true;

    if (not FShowHeader) and FullyExpanded then
      FView.FHRange := -1;

    FView.FRangeUpdate := true;
    FUpdated := true;
    IsUpdating := false;
  end;
  UpdateItem;
end;

procedure TElTreeItem.SetParentColors(value: Boolean);
begin
  if value = ((FBoolData1 and ibfParentColors) = ibfParentColors) then exit;
  if Value then
    FBoolData1 := FBoolData1 or ibfParentColors
  else
    FBoolData1 := FBoolData1 and not ibfParentColors;
  UpdateItem;
end;

function TElTreeItem.GetHasChildren: boolean;
begin
  result := (FChildren <> nil) and (FChildren.Count > 0);
end;

procedure TElTreeItem.Clear;
var i : integer;
    OldFocused : TElTreeItem;
begin
  OldFocused := FOwner.FView.FFocused;
  inc(FOwner.FFireFocusEvents);
  try
    if FChildren = nil then exit;
    for i := -FChildren.Count + 1 to 0 do
        DeleteChild(TElTreeItem(FChildren.Last));
    FChildren.Free;
    FChildren := nil;
    if FullyExpanded then
      FOwner.FView.FVisUpdated := true;
  finally
    Dec(FOwner.FFireFocusEvents);
  end;
  if (OldFocused <> FOwner.FView.FFocused) and (FOwner.FFireFocusEvents = 0) then
    FOwner.DoItemFocused;
end;

procedure TElTreeItem.ClearSubChild;
var i : integer;
    OldFocused : TElTreeItem;
begin
  if FChildren = nil then exit;
  OldFocused := FOwner.FView.FFocused;
  inc(FOwner.FFireFocusEvents);
  try
    for i := FChildren.Count-1 downto 0 do
      RemoveSubChild(TElTreeItem(FChildren.Items[i]));
    if FullyExpanded then
      FOwner.FView.FVisUpdated := true;
  finally
    Dec(FOwner.FFireFocusEvents);
  end;
  if (OldFocused <> FOwner.FView.FFocused) and (FOwner.FFireFocusEvents = 0) then
    FOwner.DoItemFocused;
end;

procedure TElTreeItem.ExchangeItems(I, J: integer);
var
  P : Pointer;
begin
  // No check for FChildren and indexes validity. The routine should be called from QuickSort only!
  P := THackElList(FChildren).FList[i];
  THackElList(FChildren).FList[i] := THackElList(FChildren).FList[j];
  THackElList(FChildren).FList[j] := P;
end;

procedure TElTreeItem.NormalizeSorts(StartIdx : integer);

  procedure IntNormalize(List : TElList; Item : TElTreeItem; var CurIdx : integer);
  var i : integer;
      AChild : TElTreeItem;
      ChC    : integer;
  begin
    if (Item.FChildren <> nil) then
    begin
      ChC := Item.FChildren.Count;
      if (ChC > 0) then
      begin
        for i := 0 to ChC - 1 do
        begin
          AChild := TElTreeItem(THackElList(Item.FChildren).FList[i]);
          if CurIdx > List.Count - 1 then
            List.Count := CurIdx + 1; 

          List[CurIdx] := AChild;
          Inc(CurIdx);
          if (AChild.FChildren <> nil) and (AChild.FChildren.Count > 0) then
             IntNormalize(List, AChild, CurIdx);
        end;
      end;
    end;
  end;

begin
  IntNormalize(FOwner.FAllList, Self, StartIdx);
end;

procedure TElTreeItem.Sort(recursive: boolean);
var
  ST       : TSortTypes;
  ASection : TElHeaderSection;
  SM       : TElSSortMode;
  FCol     : integer;
  StartItem: TElTreeItem;
  StartIdx : Integer;
begin
  inc(FOwner.FInSorting);
  if (FChildren <> nil) and (Recursive or (FChildren.Count > 1)) then
  begin
    FOwner.IsUpdating := true;

    with FOwner do
    begin
      if not FShowHeader or (FHeader.Sections.Count <= FSortSection) then
        FCol := -1
      else
        FCol := FSortSection;

      SM   := hsmNone;
      ST   := FSortType;

      if FShowHeader and (FCol >= 0) and (FHeader.Sections.Count > FSortSection) then
      begin
        ASection := FHeader.Sections[FCol];
        if ASection <> nil then
        begin
          SM := ASection.SortMode;
          ST := SectionTypeToSortType(ASection.FieldType);
        end
        else
          FCol := -1;
      end
      else
        FCol := -1;
    end;

    StartItem := FChildren.FastGet(0);
    StartIdx := StartItem.AbsoluteIndex;

    FBoolData1 := FBoolData1 and not ibfRec;

    QuickSort(recursive, 0, FChildren.Count - 1, SM, ST, FCol);

    NormalizeSorts(StartIdx);

    with FOwner.FView do
    begin
      FClearAll := true;
      FClearVis := true;
    end;
    with FOwner do
    begin
      FUpdated := true;
      IsUpdating := false;
    end;
  end;
  dec(FOwner.FInSorting);
end;

procedure TElTreeItem.AddSortData(SortType: TSortTypes;
                                  FSortSection : integer);
var S : TElFString;
    N, I, FCol : integer;
    CC: Currency;
    bb: boolean;
    T : TDateTime;
    F : Extended;
    MTC: integer;
    VA : Variant;
    IntConv : boolean;
    
begin
  MTC := FOwner.FMainTreeCol;
  if FSortRef = 0 then
  begin
    {$IFDEF USE_VARIANT}
    VarClear(FSortData);
    {$ELSE}
    FSortType := -1;
    {$ENDIF}
    IntConv := true;
    if FOwner.VirtualityLevel <> vlNone then
    begin
      //if Assigned(FOwner.FOnVirtualValueNeeded) then
      begin
        IntConv := false;
        try
          if SortType = stText then
          begin
            FOwner.TriggerVirtualTextNeeded(Self, FSortSection, S);
            {$IFDEF USE_VARIANT}
            FSortData := S;
            {$ELSE}
            FSortType := vtString;
            {$ifdef ELPACK_UNICODE}
            GetMem(FSortData, (Length(S) + 1) * sizeof(WideChar));
            if FOwner.SortUseCase then
              WideStrPCopy(PWideChar(FSortData), S)
            else
              WideStrPCopy(PWideChar(FSortData), WideUpperCase(S));
            {$else}
            GetMem(FSortData, Length(S) + 1);
            if FOwner.SortUseCase then
              StrPLCopy(PChar(FSortData), S, Length(S))
            else
              StrPLCopy(PChar(FSortData), Uppercase(S), Length(S));
            {$endif}
            {$ENDIF}
          end
          else
          begin
            case SortType of
              stNumber:
                begin
                  {$IFDEF USE_VARIANT}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtInteger, VA);
                  FSortData := VA;
                  {$ELSE}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtInteger, VA);
                  N := VA;
                  FSortType := vtInteger;
                  Integer(FSortData) := N;
                  {$ENDIF}
                end;
              stCurrency:
                begin
                  {$IFDEF USE_VARIANT}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtCurrency, VA);
                  FSortData := VA;
                  {$ELSE}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtCurrency, VA);
                  CC := VA;
                  FSortType := vtCurrency;
                  GetMem(FSortData, sizeof(Currency));
                  PCurrency(FSortData)^ := CC;
                  {$ENDIF}
                end;
              stFloating:
                begin
                  {$IFDEF USE_VARIANT}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtExtended, VA);
                  FSortData := VA;
                  {$ELSE}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtExtended, VA);
                  F := VA;
                  begin
                    GetMem(FSortData, sizeof(Double));
                    PDouble(FSortData)^ := F;
                  end;
                  {$ENDIF}
                end;
              stDateTime,
              stDate,
              stTime:
                begin
                  {$IFDEF USE_VARIANT}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtExtended, VA);
                  FSortData := VA;
                  {$ELSE}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtExtended, VA);
                  T := VA;
                  FSortType := vtExtended;
                  GetMem(FSortData, sizeof(Double));
                  PDouble(FSortData)^ := T;
                  {$ENDIF}
                end;
              stBoolean:
                begin
                  {$IFDEF USE_VARIANT}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtBoolean, VA);
                  FSortData := VA;
                  {$ELSE}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtBoolean, VA);
                  BB := VA;
                  FSortType := vtBoolean;
                  Integer(FSortData) := Integer(BB);
                  {$ENDIF}
                end;
              else
                begin
                  {$IFDEF USE_VARIANT}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtString, VA);
                  FSortData := VA;
                  {$ELSE}
                  FOwner.TriggerVirtualValueNeeded(Self, FSortSection, vtString, VA);
                  S := VA;
                  FSortType := vtString;
                  GetMem(FSortData, Length(S) + 1);
                  StrPLCopy(PChar(FSortData), S, Length(S));
                  {$ENDIF}
                end;
            end;
          end;
        except
          on E: EVariantError do
            IntConv := true;
        end;
      end;
    end;
    
    if IntConv then
    begin
      if (FSortSection = MTC) or (FSortSection = -1) then
      begin
        S := Text; // NOTE:
      end
      else
      begin
        FCol := FSortSection;
        if FSortSection > MTC then
           dec(FCol);
        if FCol >= FStaticData.FColText.Count then
           S := ''
        else
           S := FStaticData.FColText[FCol];
      end;

      if SortType = stText then
      begin
        {$IFDEF USE_VARIANT}
        FSortData := S;
        {$ELSE}
        FSortType := vtString;

        {$ifdef ELPACK_UNICODE}
        GetMem(FSortData, (Length(S) + 1) * sizeof(WideChar));
        if FOwner.SortUseCase then
          WideStrPCopy(PWideChar(FSortData), S)
        else
          WideStrPCopy(PWideChar(FSortData), WideUpperCase(S));
        {$else}
        GetMem(FSortData, Length(S) + 1);
        if FOwner.SortUseCase then
          StrPLCopy(PChar(FSortData), S, Length(S))
        else
          StrPLCopy(PChar(FSortData), Uppercase(S), Length(S));
        {$endif}
        {$ENDIF}
      end
      else
      if SortType = stNumber then
      begin
        {$IFDEF USE_VARIANT}
        TVarData(FSortData).VType := vtInteger;
        Val(S, N, I);
        if I <> 0 then
          FSortData := NULL
        else
          FSortData := N;
        {$ELSE}
        FSortType := vtInteger;
        Val(S, N, I);
        if I <> 0 then
          FSortType := -1
        else
          Integer(FSortData) := N;
        {$ENDIF}
      end
      else
      case SortType of
        stCurrency:
          begin
            {$IFDEF USE_VARIANT}
            FSortData := PrettyStrToCurr(S);
            {$ELSE}
            FSortType := vtCurrency;
            GetMem(FSortData, sizeof(Currency));
            PCurrency(FSortData)^ := PrettyStrToCurr(S);
            {$ENDIF}
          end;
        stFloating:
          begin
            {$IFDEF USE_VARIANT}
            TVarData(FSortData).VType := vtExtended;
            if (not TextToFloat(Pchar(S), F, fvExtended)) then
               FSortData := NULL
            else
               FSortData := F;
            {$ELSE}
            FSortType := vtExtended;
            if (not TextToFloat(PChar(String(S)), F, fvExtended)) then
               FSortType := -1
            else
            begin
              GetMem(FSortData, sizeof(Double));
              PDouble(FSortData)^ := F;
            end;
            {$ENDIF}
          end;
        stDateTime:
          begin
            {$IFDEF USE_VARIANT}
            TVarData(FSortData).VType := vtExtended;
            try
              T := StrToDateTime(S);
              FSortData := T;
            except
              FSortData := NULL
            end;
            {$ELSE}
            FSortType := vtExtended;
            try
              T := StrToDateTime(S);
              GetMem(FSortData, sizeof(Double));
              PDouble(FSortData)^ := T;
            except
              FSortType := -1;
            end;
            {$ENDIF}
          end;
        stDate:
          begin
            {$IFDEF USE_VARIANT}
            TVarData(FSortData).VType := vtExtended;
            try
              T := StrToDate(S);
              FSortData := T;
            except
              FSortData := NULL
            end;
            {$ELSE}
            FSortType := vtExtended;
            try
              T := StrToDate(S);
              GetMem(FSortData, sizeof(Double));
              PDouble(FSortData)^ := T;
            except
              FSortType := -1;
            end;
            {$ENDIF}
          end;
        stTime:
          begin
            {$IFDEF USE_VARIANT}
            TVarData(FSortData).VType := vtExtended;
            try
              T := StrToTime(S);
              FSortData := T;
            except
              FSortData := NULL
            end;
            {$ELSE}
            FSortType := vtExtended;
            try
              T := StrToTime(S);
              GetMem(FSortData, sizeof(Double));
              PDouble(FSortData)^ := T;
            except
              FSortType := -1;
            end;
            {$ENDIF}
          end;
        stBoolean:
          begin
            {$IFDEF USE_VARIANT}
            TVarData(FSortData).VType := vtBoolean;
            FSortData := boolean(Length(S) > 0);
            {$ELSE}
            FSortType := vtBoolean;
            Integer(FSortData) := Length(S);
            {$ENDIF}
          end;
        else
          begin
            {$IFDEF USE_VARIANT}
            FSortData := S;
            {$ELSE}
            FSortType := vtString;
            GetMem(FSortData, Length(S) + 1);
            StrPLCopy(PChar(FSortData), S, Length(S));
            {$ENDIF}
          end;
      end;
    end;
  end;
  inc(FSortRef);
end;

procedure TElTreeItem.ReleaseSortData;
begin
  dec(FSortRef);
  if (FSortRef = 0) then
  {$IFDEF USE_VARIANT}
     VarClear(FSortData);
  {$ELSE}
  begin
    if ((FSortType = vtExtended) or (FSortType = vtString)) and (FSortData <> nil) then
       FreeMem(FSortData);
    FSortData := nil;
    FSortType := -1;
  end;
  {$ENDIF}
end;

procedure TElTreeItem.QuickSort(recursive: boolean;
                                L, R: Integer;
                                SM : TElSSortMode;
                                SortType: TSortTypes;
                                FSortSection : integer);
var
  I, J       : Integer;
  P          : TElTreeItem;
  EL, ER     : integer;
  ChildCount : integer;
  LocalFRec  : boolean;
  Item1,
  Item2      : TElTreeItem;

begin
  Childcount := FChildren.Count;
  inc(FOwner.FInSorting);
  if ChildCount > 1 then
  begin
    if (FBoolData1 and ibfRec) <> ibfRec then
    begin
      for i := 0 to ChildCount - 1 do
      begin
        p := TElTreeItem(THackElList(FChildren).FList[i]);
        if P.FSortRef = 0 then
           P.AddSortData(SortType, FSortSection)
        else
           inc(P.FSortRef);
      end;
    end;
    El := L;
    Er := R;
    repeat
      I := L;
      J := R;
      P := TElTreeItem(THackElList(FChildren).FList[(L + R) shr 1]);
      repeat
        while (I < ChildCount) do
        begin
          Item1 := TElTreeItem(THackElList(FChildren).FList[I]);
          Item2 := P;
          if Item1 = Item2 then break;
          if (FOwner.CompareItems(Item1, Item2, SM, SortType, FSortSection) >= 0) then break;
          Inc(I);
        end;
        while (J >= 0) do
        begin
          Item1 := TElTreeItem(THackElList(FChildren).FList[J]);
          Item2 := P;
          if Item1 = Item2 then break;
          if (FOwner.CompareItems(Item1, P, SM, SortType, FSortSection) < 0) then break;
          Dec(J);
        end;
        if I <= J then
        begin
          if i <> j then
             ExchangeItems(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if (L < J) and ((EL <> L) or (ER <> J)) then
      begin
        LocalFRec:= (FBoolData1 and ibfRec) = ibfRec;
        FBoolData1 := FBoolData1 or ibfRec;
        QuickSort(recursive, L, J, SM, SortType, FSortSection);
        if LocalFRec then
          FBoolData1 := FBoolData1 or ibfRec
        else
          FBoolData1 := FBoolData1 and not ibfRec;
      end;
      L := I;
    until I >= R;
    if (FBoolData1 and ibfRec) <> ibfRec then
      for i := 0 to ChildCount - 1 do
      begin
        P := TElTreeItem(THackElList(FChildren).FList[i]);
        if P.FSortRef = 1 then
           P.ReleaseSortData
        else
           dec(P.FSortRef);
      end;
  end;

  if Recursive then
  begin
    if (FBoolData1 and ibfRec) <> ibfRec then
    begin
      for i := 0 to ChildCount - 1 do
      begin
        P := TElTreeItem(THackElList(FChildren).FList[i]);
        if (P.FChildren <> nil) and (P.FChildren.Count > 0) then
            P.QuickSort(recursive, 0, P.FChildren.Count - 1, SM, SortType, fsortSection);
      end; // for
    end;
  end;
  dec(FOwner.FInSorting);
end;

procedure TElTreeItem.OnColTextChange(Sender: TObject);
begin
  if FOwner <> nil then
  begin
    FOwner.DoItemChange(Self, icmColumnText);

    with FOwner do
    if (FSortMode = smAdd) or (FSortMode = smAddClick) then
    begin
      if InRange(-1, FHeader.Sections.Count - 1, FSortSection) then
      begin
        if FParent <> nil then
        begin
          if FUpdateCount > 0 then
             FSortRequired := true
          else
          begin
            FOwner.TriggerSortBegin;
            FParent.Sort(false);
            FOwner.TriggerSortBegin;
          end;
        end;
      end;
    end;
    with FOwner do
      if FShowHeader and (FUpdateCount = 0) then
         AutoSizeAllColumns
      else
        FColSizeUpdate := true;
    {$IFNDEF LITE}
    if (Multiline and FOwner.AdjustMultilineHeight)
        {$IFDEF HAS_HTML_RENDER}
        or IsHTML
        {$ENDIF}
    then
    begin
      FComplexHeight := (FComplexHeight and $FFFF0000) or word(-1);
      {$IFDEF HAS_HTML_RENDER}
      if (FBoolData1 and ibfIsHTML) = ibfIsHTML then
        ReRenderAllTexts;
      {$ENDIF}
      FOwner.IsUpdating := true;
      with FOwner.FView do
      begin
        FVisUpdated := true;
        FClearVis := true;
        FClearAll := true;
        FRangeUpdate := true;
      end;
      FOwner.FUpdated := true;
      FOwner.IsUpdating := false;
    end;
    {$ENDIF}

    UpdateItem;
  end;
end;

procedure TElTreeItem.SetImageIndex(value: integer); { private }
begin
  if FImageIndex = value then exit;
  FImageIndex := value;
  FOwner.IsUpdating := true;
  if FOwner.FShowImages then
  begin
    with FOwner do
    begin
      if (not FShowEmptyImages) and FShowHeader then
         AutoSizeColumn(FMainTreeCol)
      else
         FColSizeUpdate := true;
      if (not FShowHeader) and FullyExpanded then
        FView.FHRange := -1;
    end;
    UpdateItem;
  end;
  
  FOwner.IsUpdating := false;
end; { SetImageIndex }

procedure TElTreeItem.SetStImageIndex(value: integer); { private }
begin
  if FStImageIndex = value then exit;
  FStImageIndex := value;
  if FOwner.FShowImages then
  begin
    with FOwner do
    begin
      if (not FShowEmptyImages) and FShowHeader then
         AutoSizeColumn(FMainTreeCol)
      else
         FColSizeUpdate := true;
      if (not FShowHeader) and FullyExpanded then
        if FUpdateCount > 0 then
          FView.FHRange := -1
        else
          FView.DefineHRange;
    end;
    UpdateItem;
  end;
end; { SetStImageIndex }

procedure TElTreeItem.SetImageIndex2(value: integer); { private }
begin
  if FImageIndex2 = value then exit;
  FImageIndex2 := value;
  if FOwner.FShowImages then
  begin
    with FOwner do
    begin
      if (not FShowEmptyImages2) and FShowHeader then
         AutoSizeColumn(FMainTreeCol)
      else
         FColSizeUpdate := true;
      if (not FShowHeader) and FullyExpanded then
        if FUpdateCount > 0 then
          FView.FHRange := -1
        else
          FView.DefineHRange;
    end;
    UpdateItem;
  end;
end; { SetImageIndex }

procedure TElTreeItem.SetStImageIndex2(value: integer); { private }
begin
  if FStImageIndex2 = value then exit;
  FStImageIndex2 := value;
  if FOwner.FShowImages then
  begin
    with FOwner do
    begin
      if (not FShowEmptyImages2) and FShowHeader then
         AutoSizeColumn(FMainTreeCol)
      else
         FOwner.FColSizeUpdate := true;
      if (not FShowHeader) and FullyExpanded then
      if FUpdateCount > 0 then
        FView.FHRange := -1
      else
        FView.DefineHRange;
    end;
    UpdateItem;
  end;
end; { SetStImageIndex }

procedure TElTreeItem.SetForceButtons(newValue: Boolean);
{ Sets data member FForceButtons to newValue. }
begin
  if ((FBoolData1 and ibfForceButtons) = ibfForceButtons) <> newValue then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfForceButtons
    else
      FBoolData1 := FBoolData1 and not ibfForceButtons;

    if FOwner.FShowButtons then
    begin
      with FOwner do
        if FShowHeader and (FOwner.FUpdateCount = 0) then
          AutoSizeColumn(FMainTreeCol)
        else
          FOwner.FColSizeUpdate := true;
      UpdateItem;
    end;
  end;
end; { SetForceButtons }

function TElTreeItem.GetCount: Integer;
begin
  if FChildren = nil then
    result := 0
  else
    result := FChildren.Count;
end; { GetCount }

function TElTreeItem.GetItems(Index: integer): TElTreeItem;
{ Returns the value of data member FItems[Index ]. }
begin
  if FChildren = nil then
     result := nil
  else
     result := FChildren[Index];
end; { GetItems }

procedure TElTreeItem.Delete; { public }
begin
  FList.DeleteItem(Self);
end; { Delete }

function TElTreeItem.IsVisible: Boolean; { public }
begin
  result := FullyExpanded;
  if FOwner.FFilteredVisibility then
  if Self = FList.FRoot then
    result := true
  else
    result := result and (not Hidden) and (FParent.IsVisible);
end; { IsVisible }

function TElTreeItem.GetNextVisible: TElTreeItem; { public }
begin
  Result := GetNext;
  while Assigned(Result) and (not Result.IsVisible) do Result := Result.GetNext;
end;

function TElTreeItem.GetPrevVisible: TElTreeItem; { public }
begin
  Result := GetPrev;
  while Assigned(Result) and (not Result.IsVisible) do Result := Result.GetPrev;
end;

function TElTreeItem.GetPrev: TElTreeItem; { public }
var i : integer;

begin
  i := FOwner.FAllList.IndexOf(Self);
  if i = 0 then
     Result := nil
  else
     Result := FOwner.FAllList[Pred(i)];
end; { GetPrev }

function TElTreeItem.GetNext: TElTreeItem; { public }
var i : integer;

begin
  i := FOwner.FAllList.IndexOf(Self);
  if i = Pred(FOwner.FAllList.Count) then
     Result := nil
  else
     Result := FOwner.FAllList[Succ(i)];
end; { GetNext }

procedure TElTreeItem.MoveToItem(Item: TElTreeItem; Mode: TNodeAttachMode); { public }
begin
  case Mode of
    naAdd:
      begin
        if not Assigned(item) then
           raise EElTreeError.Create(STexInvItem);
        if not Assigned(item) then
           MoveTo(nil)
        else
           MoveToIns(item.FParent, item.FParent.Count);
      end;
    naAddFirst:
      begin
        if not Assigned(item) then
           MoveToIns(nil, 0)
        else
           MoveToIns(item.FParent, 0);
      end;
    naAddChild: MoveTo(item);
    naAddChildFirst: MoveToIns(item, 0);
    naInsert:
      begin
        if not Assigned(item) then
          MoveToIns(nil, 0)
        else
          if (FParent = item.FParent) and (Index < Item.Index) then
            MoveToIns(item.FParent, item.Index-1)
          else
            MoveToIns(item.FParent, item.Index);
      end;
  end;
end; { MoveToItem }

{$ifdef ELTREE_USE_STYLES}
procedure TElTreeItem.SetUseStyles(newValue: Boolean);
begin
  if ((FBoolData1 and ibfUseStyles) = ibfUseStyles) <> newValue then
  begin
    FOwner.IsUpdating := true;
    if newValue then
      FBoolData1 := FBoolData1 or ibfUseStyles
    else
      FBoolData1 := FBoolData1 and not ibfUseStyles;
    if FStaticData <> nil then
      if (FBoolData1 and ibfUseStyles) = ibfUseStyles then
      begin
        if FStaticData.FStyles = nil then
          CreateStyles;
      end
      else
      begin
        if FStaticData.FStyles <> nil then
        begin
          FStaticData.FStyles.Free;
          FStaticData.FStyles := nil;
        end;
      end;
    with FOwner do
    begin
      if FShowHeader then
        FColSizeUpdate := true;

      if (not FShowHeader) and FullyExpanded then
        FView.FHRange := -1;
      FView.FRangeUpdate := true;
      FUpdated := true;
      IsUpdating := false;
    end;
  end; {if}
end;
{$endif}

procedure TElTreeItem.UpdateItem;
begin
  if not (FOwner.FFilteredVisibility and Hidden) {and (FOwner.FUpdateCount = 0) }then RedrawItem(true);
  with FOwner do
    if FUpdateCount > 0 then
      FUpdated := true
    else
      FView.Update;
end;

{$ifdef ELTREE_USE_STYLES}
function TElTreeItem.GetStyles(index: integer): TElCellStyle;
begin
  if FStaticData <> nil then
  begin
    if FStaticData.FStyles = nil then
      result := nil
    else
      result := FStaticData.FStyles[index];
  end
  else
    result := nil;
end;

procedure TElTreeItem.SetStyles(index: integer; newValue: TElCellStyle);
begin
  if FStaticData <> nil then
  begin
    if FStaticData.FStyles = nil then
      CreateStyles;
    if (FStaticData.FStyles[index] <> newValue) then
    begin
      FStaticData.FStyles[index] := newValue;
    end; {if}
  end;
end;

function TElTreeItem.AddStyle: TElCellStyle;
begin
  if FStaticData <> nil then
  begin
    Result := TElCellStyle.Create(Self);
    Result.FOwnerProps := true;
    Result.FFontSize := FOwner.Font.Size;
    Result.FFontStyles := FOwner.Font.Style;
    Result.FFontName := FOwner.Font.Name;
    if Assigned(FStaticData.FMainStyle) then
      Result.Assign(FStaticData.FMainStyle);
    if FStaticData.FStyles = nil then
      CreateStyles;
    FStaticData.FStyles.Add(Result);
  end
  else
    result := nil;
end;

procedure TElTreeItem.RemoveStyle(Style: TElCellStyle);
begin
  if FStaticData <> nil then
  begin
    FStaticData.FStyles.Remove(Style);
    if FStaticData.FStyles.Count = 0 then
    begin
      FStaticData.FStyles.Free;
      FStaticData.FStyles := nil;
    end;
  end;
end;

function TElTreeItem.GetStylesCount: Integer;
begin
  if FStaticData <> nil then
  begin
    if FStaticData.FStyles = nil then
      result := 0
    else
      result := FStaticData.FStyles.Count;
  end
  else
    result := 0;
end;
{$endif}

procedure TElTreeItem.SetCheckBoxState(newValue: TCheckBoxState);
var
  Section: TElHeaderSection;
begin
  if (FCheckBoxState <> newValue) then
  begin
    FCheckBoxState := newValue;
    if ((FBoolData1 and ibfShowCheckBox) = ibfShowCheckBox) and (FOwner.FShowCheckboxes) then
    begin
      if FOwner.FShowHeader then
      begin
        Section := FOwner.FHeader.Sections[FOwner.FMainTreeCol];
        RedrawItemPart(true, Section.Left, Section.Right);
      end else RedrawItem(true);
      with FOwner do
        if IsUpdating then FUpdated := true else FView.Update;
    end;
  end; {if}
end; {SetCheckBoxState}

function TElTreeItem.GetChecked: boolean;
begin
  Result := FCheckBoxState = cbChecked;
end;

procedure TElTreeItem.SetChecked(newValue: Boolean);
var
  Section: TElHeaderSection;
  i: integer;
  Item: TElTreeItem;
begin
  if (Checked <> newValue) then
  begin
    if newValue then
      FCheckBoxState := cbChecked
    else
      FCheckBoxState := cbUnchecked;
    if (FCheckBoxType = ectRadioButton) and (newValue) then
    begin
      //FOwner.IsUpdating := true;
      if Assigned(FParent.FChildren) then
      begin
        for i := 0 to Pred(FParent.FChildren.Count) do
        begin
          Item := FParent.Children[i];
          if (Item.FCheckBoxType = ectRadioButton) and (Item <> Self) then
          begin
            Item.Checked := false;
            //FOwner.FUpdated:=true;
          end;
        end; // for
      end; // if
      //FOwner.IsUpdating := false;
    end;
    if ((FBoolData1 and ibfShowCheckBox) = ibfShowCheckBox) and (FOwner.FShowCheckboxes) then
    begin
      if FOwner.FShowHeader then
      begin
        Section := FOwner.FHeader.Sections[FOwner.FMainTreeCol];
        RedrawItemPart(true, Section.Left, Section.Right);
      end else RedrawItem(true);
      with FOwner do
        if IsUpdating then FUpdated := true else FView.Update;
    end;
  end;
end; {SetChecked}

procedure TElTreeItem.SetShowCheckBox(newValue: Boolean);
begin
  if (((FBoolData1 and ibfShowCheckBox) = ibfShowCheckBox) <> newValue) then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfShowCheckbox
    else
      FBoolData1 := FBoolData1 and not ibfShowCheckbox;

    with FOwner do
    begin
      if (FShowCheckboxes) then
      begin
        if FShowHeader and (FOwner.FUpdateCount = 0) then
          AutoSizeColumn(FMainTreeCol)
        else
          FOwner.FColSizeUpdate := true;
        if (not FShowHeader) and FullyExpanded then
          if FUpdateCount > 0 then
            FView.FHRange := -1
          else
            FView.DefineHRange;
        UpdateItem;
      end;
    end;
  end; {if}
end; {SetShowCheckBox}

procedure TElTreeItem.SetCheckBoxType(newValue: TElCheckBoxType);
var
  Section: TElHeaderSection;
begin
  if (FCheckBoxType <> newValue) then
  begin
    FCheckBoxType := newValue;
    if ((FBoolData1 and ibfShowCheckBox) = ibfShowCheckBox) and (FOwner.FShowCheckboxes) then
    begin
      if FOwner.FShowHeader then
      begin
        Section := FOwner.FHeader.Sections[FOwner.FMainTreeCol];
        RedrawItemPart(true, Section.Left, Section.Right);
      end
      else
        RedrawItem(true);
      with FOwner do
        if IsUpdating then
          FUpdated := true
        else
          FView.Update;
    end;
  end; {if}
end; {SetCheckBoxType}

procedure TElTreeItem.SetCheckBoxEnabled(newValue: Boolean);
var
  Section: TElHeaderSection;
begin
  if (((FBoolData1 and ibfCheckBoxEnabled) = ibfCheckBoxEnabled) <> newValue) then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfCheckBoxEnabled
    else
      FBoolData1 := FBoolData1 and not ibfCheckBoxEnabled;

    if ((FBoolData1 and ibfShowCheckBox) = ibfShowCheckBox) and FOwner.FShowCheckboxes then
    begin
      if FOwner.FShowHeader then
      begin
        Section := FOwner.FHeader.Sections[FOwner.FMainTreeCol];
        RedrawItemPart(true, Section.Left, Section.Right);
      end else RedrawItem(true);
      with FOwner do
        if IsUpdating then FUpdated := true else FView.Update;
    end;
  end; {if}
end; {SetCheckBoxEnabled}

procedure TElTreeItem.SetSuppressButtons(newValue: Boolean);
begin
  if (((FBoolData1 and ibfSuppressButtons) = ibfSuppressButtons) <> newValue) then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfSuppressButtons
    else
      FBoolData1 := FBoolData1 and not ibfSuppressButtons;
    with FOwner do
    begin
      if FShowButtons and FShowHeader and (FOwner.FUpdateCount = 0) then
        AutoSizeColumn(FMainTreeCol)
      else
        FOwner.FColSizeUpdate := true;
      if (not FShowHeader) and FullyExpanded then
        if FUpdateCount > 0 then
          FView.FHRange := -1
        else
          FView.DefineHRange;
    end;
    UpdateItem;
  end; {if}
end;

procedure TElTreeItem.SetEnabled(newValue: Boolean);
begin
  if (((FBoolData1 and ibfEnabled) = ibfEnabled) <> newValue) then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfEnabled
    else
      FBoolData1 := FBoolData1 and not ibfEnabled;
    UpdateItem;
  end; {if}
end; {SetEnabled}

procedure TElTreeItem.SetHidden(newValue: Boolean);
begin
  if (((FBoolData1 and ibfHidden) = ibfHidden) <> newValue) then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfHidden
    else
      FBoolData1 := FBoolData1 and not ibfHidden;
    if FOwner <> nil then
    begin
      FOwner.IsUpdating := true;
      if newValue = true then
      begin
        inc(FOwner.TotalHiddenCount);
      end else
      begin
        dec(FOwner.TotalHiddenCount);
      end;
      if FOwner.FilteredVisibility then
      with FOwner.FView do
      begin
        FVisUpdated := true;
        FClearVis := true;
        FClearAll := true;
        FRangeUpdate := true;
        FOwner.FUpdated := true;
      end;
      FOwner.IsUpdating := false;
    end;
    with FOwner do
      if FShowHeader and (FOwner.FUpdateCount = 0) then
        AutoSizeColumn(FMainTreeCol)
      else
        FOwner.FColSizeUpdate := true;
  end; {if}
end;

function TElTreeItem.GetFullyVisible: Boolean;
begin
  if Parent <> nil then
    result := (not hidden) and Parent.FullyVisible
  else
    result := not hidden;
end;

procedure TElTreeItem.SetFullyVisible(newValue: Boolean);
begin
  if (FullyVisible <> newValue) then
  begin
    if newValue and (Parent <> nil) then
    begin
      Parent.FullyVisible := true;
      Hidden := false;
    end;
  end; {if}
end;

function TElTreeItem.GetHasVisibleChildren: Boolean;
var
  i: integer;
begin
  result := false;
  if FChildren = nil then
    result := false
  else
  begin
    if not FOwner.FilteredVisibility then
      result := FChildren.Count > 0
    else
    begin
      if FChildren.Count <> 0 then
        for i := 0 to FChildren.Count - 1 do
        begin
          if not TElTreeItem(THackElList(FChildren).FList[i]).Hidden then
          begin
            result := true;
            exit;
          end;
        end;
    end;
  end;
end;

{$IFNDEF LITE}
procedure TElTreeItem.SetOwnerHeight(newValue : Boolean);
{ Sets data member FOwnerHeight to newValue. }
begin
  if (((FBoolData1 and ibfOwnerHeight) = ibfOwnerHeight) <> newValue) then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfOwnerHeight
    else
      FBoolData1 := FBoolData1 and not ibfOwnerHeight;
    FOwner.IsUpdating := true;
    FComplexHeight := (FComplexHeight and $FFFF0000) or word(-1);
    if newValue then
      dec(FOwner.TotalVarHeightCount)
    else
      inc(FOwner.TotalVarHeightCount);      
    with FOwner.FView do
    begin
      FVisUpdated := true;
      FClearVis := true;
      FClearAll := true;
      FRangeUpdate := true;
      FOwner.FUpdated := true;
    end;
    FOwner.IsUpdating := false;
  end;  { if }
end;  { SetOwnerHeight }
{$ENDIF LITE}

function TElTreeItem.GetHeight : Integer;
{$IFNDEF LITE}
var Size : TPoint;
    i : integer;
{$ENDIF}

    function TextLinesCount : integer;
    var i, j : integer;
        AText: TElFString;
    begin
      i := 1;
      result := 0;
      AText := Text;
      j := Length(AText);
      while i <= j do
      begin
        if (AText[i] = #13) and (i < j) and (AText[i + 1] = #10) then
        begin
          inc(result);
          inc(i);
        end;
        inc(i);
      end;
      if i > 1 then
        inc(result);
    end;

begin
{$IFDEF LITE}
  result := FOwner.LineHeight;
{$ELSE}
  if ((FBoolData1 and ibfOwnerHeight) <> ibfOwnerHeight) or {$IFDEF HAS_HTMl_RENDER}IsHTML or{$ENDIF} (Multiline and FOwner.adjuStmultilineheight) then
  begin
    if (((FComplexHeight shr 16) > 0) and (not Multiline) {$IFDEF HAS_HTMl_RENDER}and not IsHTML{$ENDIF}) then
      result := FComplexHeight shr 16
    else
    begin
      if SmallInt(FComplexHeight and $0000FFFF) <> -1 then
         result := SmallInt(FComplexHeight and $0000FFFF)
      else
      begin
        if FOwner.ShowColumns then
        begin
          result := 0;
          for i := 0 to Pred(FOwner.FHeader.Sections.Count) do
          begin
            FOwner.MeasureCell(Self, i, Size);
            result := Max(result, Size.Y);
          end;
        end
        else
        begin
          FOwner.MeasureCell(Self, -1, Size);
          result := Size.Y;
        end;
        FComplexHeight := (FComplexHeight and $FFFF0000) or Word(result);
      end;
    end;
  end
  else
    result := FOwner.LineHeight;
{$ENDIF LITE}
end;  { GetHeight }

{$IFNDEF LITE}
procedure TElTreeItem.SetHeight(newValue : Integer);
{ Sets data member FHeight to newValue. }
begin
  if (SmallInt((FComplexHeight shr 16)) <> newValue) then
  begin
    FComplexHeight := (Word(newValue) shl 16) or (FComplexHeight and $0000FFFF);

    if ((FBoolData1 and ibfOwnerHeight) <> ibfOwnerHeight) then
    begin
      FOwner.IsUpdating := true;
      with FOwner.FView do
      begin
        FVisUpdated := true;
        FClearVis := true;
        FClearAll := true;
        FRangeUpdate := true;
        FOwner.FUpdated := true;
      end;
      FOwner.IsUpdating := false;
    end;
  end;  { if }
end;  { SetHeight }
{$ENDIF LITE}

procedure TElTreeItem.SetSuppressLines(newValue: Boolean);
{ Sets data member FSuppressLines to newValue. }

var
InvalidateItemPart: TIterateProcAnonymusMethod;
begin
  InvalidateItemPart :=  procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
           IterateData: pointer; Tree: TCustomElTree)
    var R: TRect;
    begin
      with Item.TreeView do
        R := GetItemRect(Item.AbsoluteIndex);
      Item.RedrawItemPart(true,R.Left, R.Right);

      ContinueIterate := true;
    end;

  if (((FBoolData1 and ibfSuppressLines) = ibfSuppressLines) <> newValue) then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfSuppressLines
    else
      FBoolData1 := FBoolData1 and not ibfSuppressLines;

    if FOwner.FShowLines then
    begin
      FList.IterateFrom(true, false, InvalidateItemPart, nil, FParent); (*<+>*)
      with FOwner do
      begin
        if FShowHeader and (FOwner.FUpdateCount = 0) then
          AutoSizeColumn(FMainTreeCol)
        else
          FColSizeUpdate := true;
        if (not FShowHeader) and FullyExpanded then
          if FUpdateCount > 0 then
            FView.FHRange := -1
          else
            FView.DefineHRange;
      end;
      RedrawItem(true);
    end;
  end;  { if }
end;  { SetSuppressLines }

procedure TElTreeItem.SetRowBkColor(newValue : TColor);
{ Sets data member FRowBkColor to newValue. }
begin
  if (FRowBkColor <> newValue) then
  begin
    FRowBkColor := newValue;
    if ((FBoolData1 and ibfParentColors) <> ibfParentColors) and
       ((FBoolData1 and ibfUseBkColor) <> ibfUseBkColor) then
       RedrawItem(true);
  end;  { if }
end;  { SetRowBkColor }

{$IFNDEF LITE}

function TElTreeItem.GetOwnerHeight : boolean;
begin
  result := not (((FBoolData1 and ibfOwnerHeight) <> ibfOwnerHeight){$IFNDEF LITE} or (Multiline and FOwner.FAdjustMultilineHeight){$ENDIF});
end;

procedure TElTreeItem.SetMultiline(newValue: Boolean);
{ Sets data member FMultiline to newValue. }
begin
  if (((FBoolData1 and ibfMultiline) = ibfMultiline) <> newValue) then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfMultiline
    else
      FBoolData1 := FBoolData1 and not ibfMultiline;

    FComplexHeight := (FComplexHeight and $FFFF0000) or WORD(-1);

    FOwner.IsUpdating := true;
    if FOwner.FAdjustMultilineHeight then
    begin
      if newValue then
        inc(FOwner.TotalVarHeightCount)
      else
        dec(FOwner.TotalVarHeightCount);
    end;
    with FOwner do
      if (not FShowHeader) and FullyExpanded then
        FView.FHRange := -1;

    with FOwner.FView do
    begin
      FVisUpdated := true;
      FClearVis := true;
      FClearAll := true;
      FRangeUpdate := true;
      FOwner.FUpdated := true;
    end;
    FOwner.IsUpdating := false;
  end;  { if }
end;  { SetMultiline }
{$ENDIF}

{$IFDEF HAS_HTML_RENDER}

procedure TElTreeItem.ReRenderMainText;
var AText : TElFString;
begin
  if csLoading in FOwner.ComponentState then exit;
  if ((FBoolData1 and ibfIsHTML) <> ibfIsHTML) then exit;
  AText := Text;
  if FHTMLData <> nil then
     FOwner.FView.FRender.DestroyData(FHTMLData);
  if (AText = '') or (Pos('<html>', AText) <> 1) then
     FHTMLData := nil
  else
  begin
    FHTMLData := FOwner.FView.FRender.CreateData;

    FHTMLData.DefaultStyle := FOwner.Font.Style;
    FHTMLData.DefaultFont  := FOwner.Font.Name;
    FHTMLData.DefaultColor := FOwner.TextColor;
    FHTMLData.DefaultHeight:= FOwner.Font.Height;
    FHTMLData.Charset      := FOwner.Font.Charset;
    FHTMLData.LinkStyle := FOwner.LinkStyle;
    FHTMLData.LinkColor := FOwner.LinkColor;

    FOwner.FView.FRender.PrepareToData(AText, 0, false, FHTMLData);
  end;
end;

procedure TElTreeItem.ReRenderAllTexts;
var i : integer;
    FData : TElHTMLData;
begin
  if csLoading in FOwner.ComponentState then exit;
  if ((FBoolData1 and ibfIsHTML) <> ibfIsHTML) then exit;
  FHTMLDataArray.Clear;
  if FOwner.VirtualityLevel = vlNone then
  begin
    for i := 0 to ColumnText.Count -1 do
    begin
      if (Pos('<html>', PAnsiChar(ColumnText[i])) = 1) then
      begin
        FHTMLDataArray[i] := FOwner.FView.FRender.CreateData;
        FData := FHTMLDataArray[i];

        FData.DefaultStyle := FOwner.Font.Style;
        FData.DefaultFont  := FOwner.Font.Name;
        FData.DefaultColor := FOwner.TextColor;
        FData.DefaultHeight:= FOwner.Font.Height;
        FData.Charset      := FOwner.Font.Charset;
        FData.LinkStyle := FOwner.LinkStyle;
        FData.LinkColor := FOwner.LinkColor;

        FOwner.FView.FRender.prEparetodata(ColumnText[i], 0, false, FData);
      end;
    end;
  end;
end;

procedure TElTreeItem.OnHTMLDataDestroy(Sender :TObject; Item : Pointer);
var FData : TElHTMLData;
begin
  FData := TElHTMLData(Item);
  if FOwner <> nil then
     FOwner.FView.FRender.DestroyData(FData)
  else
     FData.Free;
end;

procedure TElTreeItem.SetIsHTML(newValue: Boolean);
begin
  if (((FBoolData1 and ibfIsHTML) = ibfIsHTML) <> newValue) then
  begin
    if newValue then
      FBoolData1 := FBoolData1 or ibfIsHTML
    else
      FBoolData1 := FBoolData1 and not ibfIsHTML;

    {$ifndef LITE}
    FComplexHeight := (FComplexHeight and $FFFF0000) or WORD(-1);
    {$endif}
    if NewValue then
    begin
      FBoolData1 := FBoolData1 or ibfHintIsHTML;
      FHTMLData      := nil;
      FHTMLDataArray := TElArray.Create;
      FHTMLDataArray.OnDelete := OnHTMLDataDestroy;
      ReRenderMainText;
      ReRenderAllTexts;
    end
    else
    begin
      if FHTMLData <> nil then
      begin
        if FOwner <> nil then
           FOwner.FView.FRender.DestroyData(FHTMLData)
        else
           FHTMLData.Free;
      end;
      FHTMLData := nil;
      FHTMLDataArray.Free;
      FHTMLDataArray := nil;
    end;

    if newValue then
      inc(FOwner.TotalVarHeightCount)
    else
      dec(FOwner.TotalVarHeightCount);

    if not (csDestroying in FOwner.ComponentState) then
    begin
      FOwner.IsUpdating := true;
      with FOwner do
        if (not FShowHeader) and FullyExpanded then
          FView.FHRange := -1;
      with FOwner.FView do
      begin
        FVisUpdated := true;
        FClearVis := true;
        FClearAll := true;
        FRangeUpdate := true;
        FOwner.FUpdated := true;
      end;
      FOwner.IsUpdating := false;
    end;
  end;  { if }
end;

{$ENDIF}

function TElTreeItem.GetAncestor: TElTreeItem;
begin
  Result := Self;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

function TElTreeItem.CalcSubItemsHeight: Integer;
var
  Item : TElTreeItem;
  i    : integer;
begin
  Result := 0;
  if Expanded and (FChildren <> nil) then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      Item := FChildren[i];
      if Item.Hidden then
        Continue;
      inc(Result, Item.Height);
      if Item.Expanded then
        inc(Result, Item.CalcSubItemsHeight);
    end;
  end;
end;

procedure TElTreeItem.SetStrikedOutLine(const Value: boolean);
begin
  if (((FBoolData1 and ibfStrikedOutLine) = ibfStrikedOutLine) <> Value) then
  begin
    if Value then
      FBoolData1 := FBoolData1 or ibfStrikedOutLine
    else
      FBoolData1 := FBoolData1 and not ibfStrikedOutLine;
    UpdateItem;
  end;
end;

procedure TElTreeItem.SetStrikedLineColor(const Value: TColor);
begin
  if FStrikedLineColor <> Value then
  begin
    FStrikedLineColor := Value;
    if ((FBoolData1 and ibfStrikedOutLine) = ibfStrikedOutLine) then
      UpdateItem;
  end;
end;

function TElTreeItem.GetStrikedOutLine: boolean;
begin
  Result := ((FBoolData1 and ibfStrikedOutLine) = ibfStrikedOutLine);
end;

procedure TElTreeItem.SetDrawHLine(const Value: Boolean);
begin
  if (((FBoolData1 and ibfDrawHLine) = ibfDrawHLine) <> Value) then
  begin
    if Value then
      FBoolData1 := FBoolData1 or ibfDrawHLine
    else
      FBoolData1 := FBoolData1 and not ibfDrawHLine;

    UpdateItem;
  end;
end;

procedure TElTreeItem.SetAllowEdit(const Value: Boolean);
begin
  if (((FBoolData1 and ibfAllowEdit) = ibfAllowEdit) <> Value) then
  begin
    if Value then
      FBoolData1 := FBoolData1 or ibfAllowEdit
    else
      FBoolData1 := FBoolData1 and not ibfAllowEdit;
  end;
end;

procedure TElTreeItem.NewStaticData;
begin
  New(FStaticData);
  FillChar(FStaticData^, sizeof(TElTreeItemStaticData), 0);
  with FStaticData^ do
  begin
    FColText := TElFStringList.Create;
    FColText.OnChange := OnColTextChange;
{$ifdef ELTREE_USE_STYLES}
    FMainStyle := TElCellStyle.Create(Self);
    FMainStyle.FFontSize := FOwner.Font.Size;
    FMainStyle.FFontStyles := FOwner.Font.Style;
    FMainStyle.FFontName := FOwner.Font.Name;
    FMainStyle.FOwnerProps := true;
{$endif}
  end;
end;

procedure TElTreeItem.DisposeStaticData;
begin
  with FStaticData^ do
  begin
    if FColText <> nil then
      FColText.Free;
{$ifdef ELTREE_USE_STYLES}
    if FMainStyle <> nil then
    begin
      FMainStyle.Free;
      FMainStyle := nil;
    end;
    if FStyles <> nil then
    begin
      FStyles.Free;
      FStyles := nil;
    end;
{$endif}
  end;
  Dispose(FStaticData);
  FStaticData := nil;
end;

procedure TElTreeItem.FillStaticData;
begin
  // currently empty
end;

function TElTreeItem.GetText: TElFString;
begin
  if FStaticData <> nil then
    Result := FStaticData.FText
  else
    FOwner.TriggerVirtualTextNeeded(Self, -1, result);
end;

function TElTreeItem.GetColText: TElFStrings;
begin
  if FStaticData <> nil then
    Result := FStaticData.FColText
  else
    Result := nil;
end;

function TElTreeItem.GetHint: TElFString;
begin
  if FStaticData <> nil then
    Result := FStaticData.FHint
  else
    FOwner.TriggerVirtualHintNeeded(Self, Result);
end;

procedure TElTreeItem.SetHint(Value: TElFString);
begin
  if FStaticData <> nil then
    FStaticData.FHint := Value;
end;

{$ifdef ELTREE_USE_STYLES}
function TElTreeItem.GetMainStyle: TElCellStyle;
begin
  if FStaticData <> nil then
    Result := FStaticData.FMainStyle
  else
    Result := nil;
end;

{$endif}

procedure TElTreeItem.SetBorderStyle(Value: TElItemBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    UpdateItem; 
  end;
end;

function TElTreeItem.GetWidth: Integer;
var
  Size : TPoint;
begin
  FOwner.MeasureCell(Self, -1, Size);
  result := Size.x;
end;

function TElTreeItem.GetDrawHLine: Boolean;
begin
  Result := (FBoolData1 and ibfDrawHLine) = ibfDrawHLine;
end;

(*
function TElTreeItem.GetAllowSelection: Boolean;
begin
  Result := (FBoolData1 and ibfAllowSelection) = ibfAllowSelection;
end;

procedure TElTreeItem.SetAllowSelection(Value: Boolean);
begin
  if Value then
    FBoolData1 := FBoolData1 or ibfAllowSelection
  else
    FBoolData1 := FBoolData1 and not ibfAllowSelection;
end;
*)

function TElTreeItem.GetAllowEdit: Boolean;
begin
  Result := (FBoolData1 and ibfAllowEdit) = ibfAllowEdit;
end;

function TElTreeItem.GetParentStyle: Boolean;
begin
  Result := (FBoolData1 and ibfParentStyle) = ibfParentStyle;
end;

{$ifdef ELTREE_USE_STYLES}
function TElTreeItem.GetUseStyles: Boolean;
begin
  result := (FBoolData1 and ibfUseStyles) = ibfUseStyles;
end;
{$endif}

function TElTreeItem.GetUseBkColor: Boolean;
begin
  Result := (FBoolData1 and ibfUseBkColor) = ibfUseBkColor;
end;

function TElTreeItem.GetParentColors: Boolean;
begin
  Result := (FBoolData1 and ibfParentColors) = ibfParentColors;
end;

function TElTreeItem.GetForceButtons: Boolean;
begin
  Result := (FBoolData1 and ibfForceButtons) = ibfForceButtons;
end;

function TElTreeItem.GetSuppressButtons: Boolean;
begin
  Result := ((FBoolData1 and ibfSuppressButtons) = ibfSuppressButtons) and
            ((Level > 0) or (FOwner.ShowRootButtons));
end;

function TElTreeItem.GetSuppressLines: Boolean;
begin
  Result := (FBoolData1 and ibfSuppressLines) = ibfSuppressLines;
end;

function TElTreeItem.GetIsHTML: Boolean;
begin
  Result := (FBoolData1 and ibfIsHTML) = ibfIsHTML;
end;

function TElTreeItem.GetMultiline: Boolean;
begin
  Result := (FBoolData1 and ibfMultiline) = ibfMultiline;
end;

function TElTreeItem.GetShowCheckBox: Boolean;
begin
  Result := (FBoolData1 and ibfShowCheckbox) = ibfShowCheckbox;
end;

function TElTreeItem.GetCheckBoxEnabled: Boolean;
begin
  Result := (FBoolData1 and ibfCheckboxEnabled) = ibfCheckboxEnabled;
end;

function TElTreeItem.GetEnabled: Boolean;
begin
  Result := (FBoolData1 and ibfEnabled) = ibfEnabled;
end;

function TElTreeItem.GetHidden: Boolean;
begin
  Result := (FBoolData1 and ibfHidden) = ibfHidden;
end;

procedure TElTreeItem.SetIndentAdjust(Value: Integer);
begin
  if IndentAdjust <> Value then
  begin
    FIndentAdjust := Value;
    with FOwner do
    begin
      if FShowHeader then
         AutoSizeColumn(FMainTreeCol)
      else
         FColSizeUpdate := true;
      if (not FShowHeader) and FullyExpanded then
        if FUpdateCount > 0 then
          FView.FHRange := -1
        else
          FView.DefineHRange;
    end;
    UpdateItem;
  end;
end;

function TElTreeItem.DisplayRect(TextOnly : boolean): TRect;
begin
  if not FOwner.IsInView(Self) then
  begin
    SetRectEmpty(Result);
    exit;
  end;
  result := FOwner.GetItemRect(FOwner.IndexInView(Self));
  if TextOnly then
  begin
    Result.Left := FTextLEft;
    Result.Right := FTextRight;
  end;
end;

{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TElTreeItem.EndEdit(ByCancel : boolean);
begin
  FOwner.EndEdit(ByCancel);
end;
{$endif}

function TElTreeItem.HasAsParent(Item : TElTreeItem): Boolean;
begin
  Result := IsUnder(Item);
end;

function TElTreeItem.IndexOf(Item : TElTreeItem): Integer;
begin
  if Item.IsUnder(Self) then
    result := Item.Index
  else
    result := -1;
end;

procedure TElTreeItem.MakeVisible;
begin
  FOwner.EnsureVisible(Self);
end;

function TElTreeItem.GetDropTarget: Boolean;
begin
  Result := FOwner.DropTarget = Self;
end;

function TElTreeItem.GetHintIsHTML: Boolean;
begin
  Result := (FBoolData1 and ibfHintIsHTML) = ibfHintIsHTML;
end;

procedure TElTreeItem.SetHintIsHTML(Value: Boolean);
begin
  if (((FBoolData1 and ibfHintIsHTML) = ibfHintIsHTML) <> Value) then
  begin
    if Value then
      FBoolData1 := FBoolData1 or ibfHintIsHTML
    else
      FBoolData1 := FBoolData1 and not ibfHintIsHTML;
  end;
end;

procedure TElTreeItem.SetBorderSpaceColor(Value: TColor);
begin
  if FBorderSpaceColor <> Value then
  begin
    FBorderSpaceColor := Value;
    if BorderStyle = ibsSpace then
      UpdateItem;
  end;
end;

procedure TElTreeItem.SetOverlayIndex(value: ShortInt);
begin
  if (FOverlayIndex = value) or (Value < -1) or (Value > 3) then exit;
  FOverlayIndex := value;
  FOwner.IsUpdating := true;
  if FOwner.FShowImages then
  begin
    with FOwner do
    begin
      if (not FShowHeader) and FullyExpanded then
        FView.FHRange := -1;
    end;
    UpdateItem;
  end;

  FOwner.IsUpdating := false;
end; { SetImageIndex }

procedure TElTreeItem.SetOverlayIndex2(value: ShortInt);
begin
  if (FOverlayIndex2 = value) or (Value < -1) or (Value > 3) then exit;
  FOverlayIndex2 := value;
  if FOwner.FShowImages then
  begin
    with FOwner do
    begin
      if (not FShowHeader) and FullyExpanded then
        if FUpdateCount > 0 then
          FView.FHRange := -1
        else
          FView.DefineHRange;
    end;
    UpdateItem;
  end;
end; { SetImageIndex }


// ****************************************************************************
//                               TElTreeItems
// ****************************************************************************

constructor TElTreeItems.CreateClass(AOwner: TCustomElTree; ItemClass : TElTreeItemClass);
begin
  FItemClass := ItemClass;
  Create(AOwner);
end;

constructor TElTreeItems.Create(AOwner: TCustomElTree);
begin
  inherited Create;
  FOwner := AOwner;
  if FItemClass = nil then
    FItemClass := TElTreeItem;
  FRoot := FItemClass.Create(FOwner);
  FRoot.FList := self;
  FRoot.FParent := nil;
  FRoot.FRoot := FRoot;
  FRoot.NewStaticData;
  FRoot.FState := [stsExpanded];
end;

destructor TElTreeItems.Destroy;
begin
  FOwner.IsUpdating := true;
  FRoot.DisposeStaticData;
  FRoot.Clear;
  FRoot.Free;
  inherited Destroy;
end;

function TElTreeItems.GetVisCount: integer;
begin
  result := FOwner.TotalVisCount;
end;

procedure TElTreeItems.DefineProperties(Filer: TFiler);

  function WriteNodes: Boolean;
  begin
    Result := Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, WriteNodes);
end;

procedure TElTreeItems.SetItem(Index: Integer; Value: TElTreeItem);
begin
  GetItem(Index).Assign(Value);
end;

procedure TElTreeItems.Iterate;
var
  j: integer;
  DoContinue: boolean;

procedure IntIterate(VisibleOnly, CheckCollapsed: boolean; Item: TElTreeItem); (*<+>*)
  var
    i, k: integer;
  begin
    inc(j);
    if (j >= 0) and ((not VisibleOnly) or ((not Item.Hidden) or (not FOwner.FilteredVisibility))) then
       IterateProc(Item, j, DoContinue, IterateData, FOwner);
    if not (DoContinue) then exit;
    if (not (VisibleOnly)) or CheckCollapsed or (Item.Expanded) then  (*<+>*)
    begin
      if Item.FChildren = nil then
         k := 0
      else
         k := Item.FChildren.Count;
      for i := 0 to k - 1 do
      begin
        if (not VisibleOnly) or ((not TElTreeItem(THackElList(Item.FChildren).FList[i]).Hidden) or (not FOwner.Filteredvisibility)) then
          IntIterate(VisibleOnly, CheckCollapsed, TElTreeItem(THackElList(Item.FChildren).FList[i])); (*<+>*)
        if not (DoContinue) then exit;
      end;
    end;
  end;

begin
  j := -2;
  DoContinue := true;
  IntIterate(VisibleOnly, CheckCollapsed, FRoot);
end;

procedure TElTreeItems.IterateBranch;
var
  j: integer;
  DoContinue: boolean;
  Stack: TElStack;
  TSI: TElTreeItem;

  procedure IntIterate(VisibleOnly: boolean; Item: TElTreeItem; StartFrom: TElTreeItem; var Stack: TElStack);
  var
    i, k, l: integer;
  begin
    inc(j);
    if (Stack <> nil) and (Stack.Count > 0) then
    begin
      if Item.FChildren = nil then
         k := -1
      else
         k := Item.FChildren.IndexOf(Stack.Pop);
      if Stack.Count = 0 then
      begin
        Stack.Free;
        Stack := nil;
      end;
    end
    else
    begin
      if not Item.IsUnder(StartFrom) then
         DoContinue := false
      else
        if (j >= 0) and ((not VisibleOnly) or ((not Item.Hidden) or (not FOwner.FilteredVisibility))) then
           IterateProc(Item, j, DoContinue, IterateData, FOwner);
      if not (DoContinue) then
         exit;
      k := 0;
    end;
    if (not (VisibleOnly)) or (Item.Expanded) then
    begin
      if Item.FChildren = nil then
         l := 0
      else
         l := Item.FChildren.Count;
      if k <> - 1 then
        for i := k to l - 1 do
        begin
          if (not VisibleOnly) or ((not TElTreeItem(THackElList(Item.FChildren).FList[i]).Hidden) or (not FOwner.FilteredVisibility)) then
              IntIterate(VisibleOnly, TElTreeItem(THackElList(Item.FChildren).FList[i]), StartFrom, Stack);
          if not DoContinue then
             exit;
        end;
    end;
  end;

begin
  j := -2;
  if BranchParent <> nil then
  begin
    Stack := TElStack.Create;
    TSI := BranchParent;
    Stack.Push(TSI);
    while TSI.Parent <> nil do
    begin
      Stack.Push(TSI.Parent);
      TSI := TSI.Parent;
    end; // while
    DoContinue := true;
    IntIterate(VisibleOnly, FRoot, BranchParent, Stack);
    Stack.Free;
  end
  else
    Iterate(VisibleOnly, not VisibleOnly, IterateProc, IterateData); (*<+>*)
end;

procedure TElTreeItems.IterateFrom;
var
  j: integer;
  DoContinue: boolean;
  Stack: TElStack;
  TSI: TElTreeItem;

  procedure IntIterate(VisibleOnly, CheckCollapsed: boolean; Item: TElTreeItem; StartFrom: TElTreeItem; var Stack: TElStack); (*<+>*)
  var
    i, k, l: integer;
  begin
    inc(j);
    if (Stack <> nil) and (Stack.Count > 0) then
    begin
      if Item.FChildren = nil then
        k := -1
      else
        k := Item.FChildren.IndexOf(Stack.Pop);
      if Stack.Count = 0 then
      begin
        Stack.Free;
        Stack := nil;
      end;
    end
    else
    begin
      if (j >= 0) and ((not VisibleOnly) or ((not Item.Hidden) or (not FOwner.FilteredVisibility))) then
        IterateProc(Item, j, DoContinue, IterateData, FOwner);
      if not (DoContinue) then exit;
      k := 0;
    end;
    if (not (VisibleOnly)) or CheckCollapsed or (Item.Expanded) then
    begin
      if Item.FChildren = nil
         then l := 0
      else
         l := Item.FChildren.Count;
      if k <> - 1 then
        for i := k to l - 1 do
        begin
          if (not VisibleOnly) or ((not TElTreeItem(THackElList(Item.FChildren).FList[i]).Hidden) or (not FOwner.FilteredVisibility)) then
            IntIterate(VisibleOnly, CheckCollapsed, TElTreeItem(THackElList(Item.FChildren).FList[i]), StartFrom, Stack);
          if not (DoContinue) then exit;
        end;
    end;
  end;

begin
  j := -2;
  if StartFrom <> nil then
  begin
    Stack := TElStack.Create;
    TSI := StartFrom;
    Stack.Push(TSI);
    while TSI.Parent <> nil do
    begin
      Stack.Push(TSI.Parent);
      TSI := TSI.Parent;
    end; // while
  end
  else
    Stack := nil;
  DoContinue := true;
  IntIterate(VisibleOnly, CheckCollapsed, FRoot, StartFrom, Stack); (*<+>*)
  if Stack <> nil then Stack.Free;
end;

procedure TElTreeItems.IterateBack;
var
  j: integer;
  DoContinue: boolean;

  procedure IntIterate(VisibleOnly, CheckCollapsed: boolean; Item: TElTreeItem);  (*<+>*)
  var
    i, k: integer;
  begin
    if not (VisibleOnly) or CheckCollapsed or (Item.Expanded) then (*<+>*)
    begin
      if Item.FChildren = nil then
        k := 0
      else
        k := Item.FChildren.Count;
      for i := k - 1 downto 0 do
      begin
        if (not VisibleOnly) or ((not TElTreeItem(THackElList(Item.FChildren).FList[i]).Hidden) or (not FOwner.FilteredVisibility)) then
           IntIterate(VisibleOnly, CheckCollapsed, TElTreeItem(THackElList(Item.FChildren).FList[i])); (*<+>*)
        if not (DoContinue) then exit;
      end;
    end;
    dec(j);
    if (j >= 0) and ((not VisibleOnly) or ((not Item.Hidden) or (not FOwner.FilteredVisibility))) then
       IterateProc(Item, j, DoContinue, IterateData, FOwner);
    if not (DoContinue) then exit;
  end;

begin
  if VisibleOnly then
     j := FOwner.TotalVisCount
  else
     j := FOwner.FAllList.Count;
  DoContinue := true;
  IntIterate(VisibleOnly, CheckCollapsed, FRoot); (*<+>*)
end;

// Changed 10/28/98 by EM

procedure TElTreeItems.IterateBackFrom;
var
  j: integer;
  DoContinue: boolean;
  Stack: TElStack;
  TSI: TElTreeItem;

  procedure IntIterate(VisibleOnly, CheckCollapsed: boolean; Item: TElTreeItem; StartFrom: TElTreeItem; var Stack: TElStack); (*<+>*)
  var
    i, k: integer;
  begin
    if (Stack <> nil) and (Stack.Count > 0) then
    begin
      if Item.FChildren = nil then
        k := -1
      else
        k := Item.FChildren.IndexOf(Stack.Pop);
      if Stack.Count = 0 then
      begin
        Stack.Free;
        Stack := nil;
      end;
    end
    else
    begin
      if Item.FChildren = nil then k := -1 else k := Item.FChildren.Count - 1;
    end;
    if (not (VisibleOnly) or CheckCollapsed or (Item.Expanded)) and (Item <> StartFrom) then (*<+>*)
      for i := k downto 0 do
      begin
        if (not VisibleOnly) or
           ((not TElTreeItem(THackElList(Item.FChildren).FList[i]).Hidden) or
           (not FOwner.FilteredVisibility)) then
          IntIterate(VisibleOnly, CheckCollapsed, TElTreeItem(THackElList(Item.FChildren).FList[i]), StartFrom, Stack);
        if not (DoContinue) then exit;
      end;
    dec(j);
    if (j >= 0) and ((not VisibleOnly) or ((not Item.Hidden) or
       (not FOwner.FilteredVisibility))) and (Item <> FRoot) then
      IterateProc(Item, j, DoContinue, IterateData, FOwner);
    if (not DoContinue) then exit;
  end;

begin
  if VisibleOnly then
     j := FOwner.TotalVisCount
  else
     j := FOwner.FAllList.Count;
  if StartFrom <> nil then
  begin
    Stack := TElStack.Create;
    TSI := StartFrom;
    Stack.Push(TSI);
    while TSI.Parent <> nil do
    begin
      Stack.Push(TSI.Parent);
      TSI := TSI.Parent;
    end; // while
  end else Stack := nil;
  DoContinue := true;
  IntIterate(VisibleOnly, CheckCollapsed, FRoot, StartFrom, Stack);
  if Stack <> nil then Stack.Free;
end;

function TElTreeItems.GetVisItem;
var i, j, idx : integer;
    Item : TElTreeItem;
begin
  if (index < 0) or (index >= FOwner.TotalVisCount) then
  begin
    result := nil;
    exit;
  end;

  i := 0;
  idx := 0;
  j := FOwner.FAllList.Count;
  while i < j do
  begin
    Item := TElTreeItem(FOwner.FAllList[i]);
    if (FOwner.FilteredVisibility and Item.Hidden) then
    begin
      i := FOwner.FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end else
    begin
      if idx = index then
      begin
        result := Item;
        exit;
      end;
      inc(idx);
      if not Item.Expanded then
         i := FOwner.FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end;
    inc(i);
  end;
  result := nil;
end;

function TElTreeItems.GetItem;
begin
  if (index < 0) or (index >= Integer(Count)) then
  begin
    result := nil;
    exit;
  end;
  result := TElTreeItem(THackElList(FOwner.FAllList).FList[index]);
end;

function TElTreeItems.GetVisIndex;
var i, j, idx : integer;
    Item : TElTreeItem;

begin
  if Child = nil then
  begin
    result := -1;
    exit;
  end;

  i := 0;
  idx := 0;
  j := FOwner.FAllList.Count;
  while i < j do
  begin
    Item := TElTreeItem(FOwner.FAllList[i]);
    if (FOwner.FilteredVisibility and Item.Hidden) then
    begin
      i := FOwner.FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end else
    begin
      if Child = Item then
      begin
        result := idx;
        exit;
      end;
      inc(idx);
      if not Item.Expanded then
         i := FOwner.FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end;
    inc(i);
  end;
  result := -1;
end;

function TElTreeItems.GetAbsIndex;
begin
  if Child = nil then
    result := -1
  else
    result := FOwner.FAllList.IndexOf(Child);
end;

procedure TElTreeItems.DeleteItem;
var
  TSI: TElTreeItem;
begin
  if (Child = nil) or (child = FRoot) then exit;
  FOwner.IsUpdating := true;
  try
    TSI := Child.FParent;
    TSI.DeleteChild(Child);
    if TSI.FullyExpanded and TSI.Expanded then
    begin
      with FOwner.FView do
      begin
        FClearVis := true;
        FClearAll := true;
        FVisUpdated := true;
      end;
      FOwner.FUpdated := true;
    end;
  finally
    FOwner.IsUpdating := false;
  end;
end;

procedure TElTreeItems.AllocateStorage(MaxItems : integer);
var NewCapacity : longint;
begin
  NewCapacity := (MaxItems + (AlignMem - 1)) and not (AlignMem - 1);
  with FOwner do
    if FAllList.Capacity < NewCapacity then
      FAllList.Capacity := NewCapacity;
end;

procedure TElTreeItems.RemoveItem;
var
  TSI: TElTreeItem;
begin
  if (Child = nil) or (child = FRoot) then exit;
  FOwner.IsUpdating := true;
  try
    TSI := Child.FParent;
    TSI.RemoveSubChild(Child);
    TSI.RemoveChild(Child);
{
    Child.FParent := nil;
    Child.FRoot := nil;
    Child.FOwner := nil;
}
    if TSI.FullyExpanded and TSI.Expanded then
    begin
      with FOwner.FView do
      begin
        FClearVis := true;
        FClearAll := true;
        FVisUpdated := true;
      end;
      FOwner.FUpdated := true;
    end;
  finally
    FOwner.IsUpdating := false;
  end;
end;

procedure TElTreeItems.AddExistingItem(Item, Parent: TElTreeItem); { public }
var
  Child, AParent: TElTreeItem;
  i: integer;
begin
  FOwner.IsUpdating := true;
  Child := Item;
  if Parent = nil then
  begin
    Child.FParent := FRoot;
    Child.FList := self;
    Child.FRoot := FRoot;
  end else
  begin
    Child.FParent := Parent;
    Child.FList := Parent.FList; { := self}
    Child.FRoot := Parent.FRoot; { := FRoot}
  end;
  AParent := Child.FParent;
  Child.FOwner := AParent.FOwner;
  AParent.AddChild(Child);
  //inc(FCount);
  if Child.FChildren <> nil then
    for i := Child.FChildren.Count-1 downto 0 do
      Child.AddExistingChild(TElTreeItem(Child.FChildren.Items[i]));
  with
   FOwner do
  begin
    if AParent.FullyExpanded and AParent.Expanded then
    begin
      with FOwner.FView do
      begin
        FClearVis := true;
        FVisUpdated := true;
      end;
    end;
{$IFNDEF LITE}
    if not Child.OwnerHeight then
       inc(TotalVarHeightCount);
    if Child.Multiline and FAdjustMultilineHeight then
       inc(TotalVarHeightCount);
{$IFDEF HAS_HTML_RENDER}
    if (Child.FBoolData1 and ibfIsHTML) = ibfIsHTML then
       inc(TotalVarHeightCount);
{$ENDIF}
{$ENDIF LITE}
    FUpdated := true;
    FView.FRangeUpdate := true;
    IsUpdating := false;
  end;
end;

procedure TElTreeItems.InsertExistingItem(Item, Parent: TElTreeItem; Index: integer); { public }
var
  Child, AParent: TElTreeItem;
begin
  if parent = nil then AParent := FRoot else AParent := parent;
  if AParent.FChildren = nil then exit;
  if (index > AParent.FChildren.Count) or (index < 0) then
    raise EElTreeError.Create(STExOutOfBounds);
  Child := Item;
  AddExistingItem(Child, AParent);
  Child.MoveToItem(AParent.Children[Index], naInsert);
end;

function TElTreeItems.InsertItem;
var
  Child, AParent: TElTreeItem;
begin
  FOwner.IsUpdating := true;
  Child := CreateItem(FOwner);
  Child.FList := self;
  Child.FParent := Parent;
  if Child.FParent = nil then Child.FParent := FRoot;
  AParent := Child.FParent;
  if (AParent.FChildren = nil) then AParent.FChildren := TElList.Create;
  if (index > AParent.FChildren.Count) or (index < 0) then
    raise EElTreeError.Create(STExOutOfBounds);
  AParent.InsertChild(Index, Child);
  //inc(FCount);

  result := Child;
  with FOwner do
  begin
    if AParent.FullyExpanded and AParent.Expanded then
    begin
      with FOwner.FView do
      begin
        FClearVis := true;
        FClearAll := true;
        FVisUpdated := true;
      end;
    end;
    with FOwner.FView do
    begin
      FUpdated := true;
      FRangeUpdate := true;
    end;
    IsUpdating := false;
  end;
end;

function TElTreeItems.AddLastItem(Parent: TElTreeItem): TElTreeItem;
var
  Child, AParent: TElTreeItem;
begin
  FOwner.IsUpdating := true;
  Child := CreateItem(FOwner);
  Child.FList := self;
  Child.FParent := Parent;
  if Child.FParent = nil then Child.FParent := FRoot;
  AParent := Child.FParent;
  AParent.AddLastChild(Child);
  //inc(FCount);
  result := Child;
  with FOwner do
  begin
    if AParent.Expanded and AParent.FullyExpanded then
    begin
      with FOwner.FView do
      begin
        FClearVis := true;
        FVisUpdated := true;
      end;
    end;
    with FOwner.FView do
    begin
      FUpdated := true;
      FRangeUpdate := true;
    end;
    IsUpdating := false;
  end;
end;

function TElTreeItems.AddItem;
var
  Child, AParent: TElTreeItem;
begin
  FOwner.IsUpdating := true;
  Child := CreateItem(FOwner);
  Child.FList := self;
  Child.FParent := Parent;
  if Child.FParent = nil then
    Child.FParent := FRoot;
  AParent := Child.FParent;
  AParent.AddChild(Child);
  //inc(FCount);
  result := Child;
  with FOwner do
  begin
    if AParent.Expanded and AParent.FullyExpanded then
    begin
      with FOwner.FView do
      begin
        FClearVis := true;
        FVisUpdated := true;
      end;
    end;
    with FOwner.FView do
    begin
      FUpdated := true;
      FRangeUpdate := true;
    end;
    IsUpdating := false;
  end;
end;

procedure TElTreeItems.Assign;
begin
  if Source is TElTreeItems then
  begin
    if FOwner <> nil then
      FOwner.IsUpdating := true;
    FRoot.Assign(TElTreeItems(Source).FRoot);
    //FCount := TElTreeItems(Source).FCount;
    if FOwner <> nil then
      with FOwner.FView do
      begin
        FHRange  := -1;
        FOwner.FUpdated := true;
        FVisUpdated := true;
        FClearVis := true;
        FClearAll := true;
        FOwner.IsUpdating := false;
      end;
  end
  else
  if (Source is TStrings) then
    LoadFromStringList(TStrings(Source))
  {$ifdef ELPACK_UNICODE}
  else
  if (Source is TElWideStrings) then
    LoadFromWideStringList(TElWideStrings(Source))
  {$endif}
  else
    inherited;
end;

procedure TElTreeItems.Clear;
var i : integer;
begin
  i := Count;
  if FOwner <> nil then
    FOwner.IsUpdating := true;
  FOwner.FView.FVisible.Clear;
  FRoot.Clear;
  if FOwner <> nil then
  with FOwner.FView do
  begin
    FHRange := -1;
    if i > 0 then
    begin
      FVisUpdated := true;
      FClearVis := true;
      FClearAll := true;
    end;
    FOwner.FUpdated := true;
    FOwner.IsUpdating := false;
  end;
end;

function TElTreeItems.GetCount: Integer;
begin
  if Self.FOwner <> nil then
    result := FOwner.FAllList.Count
  else
    result := 0;
end;

function TElTreeItems.CreateItem;
begin
  result := FItemClass.Create(FOwner);
end;

procedure TElTreeItems.ReadData;
begin
  if FOwner <> nil then
    FOwner.IsUpdating := true;
  FRoot.Clear;
  FRoot.ReadData(Stream);
  if FOwner <> nil then
  with FOwner.FView do
    begin
      FHRange := -1;
      FVisUpdated := true;
      FClearVis := true;
      FClearAll := true;
      FOwner.FUpdated := true;
      FOwner.IsUpdating := false;
    end;
end;

procedure TElTreeItems.WriteData;
var
  TSI: TElTreeItem;
begin
  TSI := nil;
  if FOwner <> nil then
  begin
    TSI := FOwner.ItemFocused;
    FOwner.ItemFocused := nil;
  end;
  FRoot.WriteData(Stream);
  if FOwner <> nil then
  begin
    FOwner.ItemFocused := TSI;
  end;
end;

procedure TElTreeItems.SaveToStream;
begin
  WriteData(Stream);
end;

procedure TElTreeItems.LoadFromStream;
begin
  ReadData(Stream);
end;

procedure TElTreeItems.SaveToFile;
var
  T: TFileStream;
begin
  T := nil;
  try
    T := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    SaveToStream(T);
  finally
    T.Free;
  end;
end;

procedure TElTreeItems.LoadFromFile;
var
  T: TFileStream;
begin
  T := nil;
  try
    T := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    LoadFromStream(T);
  finally
    T.Free;
  end;
end;

function TElTreeItems.Add(Item: TElTreeItem; Text: TElFString): TElTreeItem; { public }
begin
  if Item = nil
     then result := AddItem(nil)
     else result := AddItem(Item.FParent);
  Result.Text := Text;
end; { Add }

function TElTreeItems.AddChild(Item: TElTreeItem; Text: TElFString): TElTreeItem; { public }
begin
  result := AddItem(Item);
  Result.Text := Text;
end; { AddChild }

function TElTreeItems.AddChildFirst(Item: TElTreeItem; Text: TElFString): TElTreeItem; { public }
begin
  result := InsertItem(0, Item);
  Result.Text := Text;
end; { AddChildFirst }

function TElTreeItems.AddChildObject(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; { public }
begin
  Result := AddItem(Item);
  Result.Text := Text;
  Result.Data := Ptr;
end; { AddChildObject }

function TElTreeItems.AddChildObjectFirst(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; { public }
begin
  result := InsertItem(0, Item);
  Result.Text := Text;
  Result.Data := Ptr;
end; { AddChildObjectFirst }

function TElTreeItems.AddFirst(Item: TElTreeItem; Text: TElFString): TElTreeItem; { public }
begin
  if Item = nil then result := InsertItem(0, nil)
  else result := InsertItem(0, Item.FParent);
  Result.Text := Text;
end; { AddFirst }

function TElTreeItems.AddObject(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; { public }
begin
  if Item = nil then result := AddItem(nil) else result := AddItem(Item.FParent);
  Result.Text := Text;
  Result.Data := Ptr;
end; { AddObject }

function TElTreeItems.AddObjectFirst(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; { public }
begin
  if Item = nil then result := InsertItem(0, nil) else result := InsertItem(0, Item.FParent);
  Result.Text := Text;
  Result.Data := Ptr;
end; { AddObjectFirst }

function TElTreeItems.Insert(Item: TElTreeItem; Text: TElFString): TElTreeItem; { public }
var
  i: integer;
  FParent: TElTreeItem;
begin
  if Item = nil then
  begin
    i := 0;
    FParent := nil;
  end else
  begin
    i := Item.Index;
    FParent := Item.FParent;
  end;
  result := InsertItem(i, FParent);
  Result.Text := Text;
end; { Insert }

// Changed 10/28/98 by EM

function TElTreeItems.InsertObject(Item: TElTreeItem; Text: TElFString; Ptr: pointer): TElTreeItem; { public }
var
  i: integer;
  FParent: TElTreeItem;
begin
  if Item = nil then
  begin
    i := 0;
    FParent := nil;
  end else
  begin
    i := Item.Index;
    FParent := Item.FParent;
  end;
  result := InsertItem(i, FParent);
  Result.Text := Text;
  Result.Data := Ptr;
end; { InsertObject }

procedure TElTreeItems.Delete; { public }
begin
  DeleteItem(Item);
end; { Delete }

function TElTreeItems.GetFirstNode: TElTreeItem; { public }
begin
  Result := Item[0];
end; { GetFirstNode }

// Changed 10/27/98 by EM

procedure TElTreeItems.BeginUpdate; { public }
begin
  FOwner.IsUpdating := true;
end; { BeginUpdate }

// Changed 10/27/98 by EM

procedure TElTreeItems.EndUpdate; { public }
begin
  FOwner.IsUpdating := false;
end; { EndUpdate }


// Changed 10/28/98 by EM

function TElTreeItems.InsertAfterObject(Item: TElTreeItem; Text: TElFString; Ptr: Pointer): TElTreeItem; { public }
var
  i: integer;
  FParent: TElTreeItem;
begin
  if Item = nil then
  begin
    i := 0;
    FParent := nil;
  end else
  begin
    i := Succ(Item.Index);
    FParent := Item.FParent;
  end;
  result := InsertItem(i, FParent);
  Result.Text := Text;
  Result.Data := Ptr;
end; { InsertAfterObject }

// Changed 10/28/98 by EM

function TElTreeItems.InsertAfter(Item: TElTreeItem; Text: TElFString): TElTreeItem; { public }
var
  i: integer;
  FParent: TElTreeItem;
begin
  if Item = nil then
  begin
    i := 0;
    FParent := nil;
  end else
  begin
    i := Item.Index + 1;
    FParent := Item.FParent;
  end;
  result := InsertItem(i, FParent);
  Result.Text := Text;
end; { InsertAfter }

type
  TSRec = record
    TextToFind: TElFString;
    WholeTextOnly: boolean;
    ColumnNum: integer;
    StartItem: TElTreeItem;
    DataToFind: pointer;
    CheckStartItem,
      LookForData,
      SubItemsOnly: boolean;
    NoCase: boolean;
    Result: TElTreeItem;
  end;
  PSRec = ^TSRec;

  TSRecEx = record
    ColumnNum: integer;
    StartItem: TElTreeItem;
    CheckStartItem,
      SubItemsOnly: boolean;
    Result: TElTreeItem;
    CompProc: TElLookupCompareProc;
    SearchDetails: pointer;
  end;
  PSRecEx = ^TSRecEx;

function TElTreeItems.LookBackForItemEx2(StartItem: TElTreeItem; ColumnNum: integer;
        CheckStartItem, SubItemsOnly, VisibleOnly, CheckCollapsed: boolean;
        SearchDetails: pointer;
        CompareProc: TElLookupCompareProc): TElTreeItem;
var
  SRec: TSRecEx;
  IntFind: TIterateProcAnonymusMethod;
begin
  IntFind :=  procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree)
      var
        b: boolean;

      begin
        if (Item = PSRecEx(IterateData).StartItem) and (not PSRecEx(IterateData).CheckStartItem) then exit;
        if ((not PSRecEx(IterateData).CheckStartItem) or (Item <> PSRecEx(IterateData).StartItem)) and
        (PSRecEx(IterateData).SubItemsOnly) and (not Item.IsUnder(PSRecEx(IterateData).StartItem)) then
        begin
          ContinueIterate := false;
          exit;
        end;
        b := PSRecEx(IterateData).CompProc(Item, PSRecEx(IterateData).SearchDetails);
        if b then
        begin
          PSRecEx(IterateData).result := Item;
          ContinueIterate := false;
          exit;
        end;
      end;
  SRec.ColumnNum := ColumnNum;
  SRec.StartItem := StartItem;
  SRec.CheckStartItem := CheckStartItem;
  SRec.SubItemsOnly := SubItemsOnly;
  SRec.CompProc := CompareProc;
  SRec.SearchDetails := SearchDetails;
  SRec.Result := nil;
  try
    IterateBackFrom(VisibleOnly, CheckCollapsed, IntFind, @SRec, StartItem); (*<+>*)
  finally
    Result := SRec.Result;
  end;
end;

  procedure IntFindLookForItem(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
  var
    i: integer;
    s: TElFString;
  begin
    if (Item = PSRec(IterateData).StartItem) and (not PSRec(IterateData).cHeckstartitem) then exit;
    if (PSRec(IterateData).SubItemsOnly) and (not Item.IsUnder(PSRec(IterateData).StartItem)) then
    begin
      ContinueIterate := false;
      exit;
    end;
    if PSRec(IterateData).LookForData then
    begin
      if Item.FData = PSRec(IterateData).DataToFind then
      begin
        PSRec(IterateData).result := Item;
        ContinueIterate := false;
        exit;
      end;
    end else
    begin
      if (PSRec(IterateData).ColumnNum = -1) or (PSRec(IterateData).ColumnNum = Tree.MainTreeColumn) then
        s := Item.Text
      else
      begin
        i := PSRec(IterateData).ColumnNum;
        if Item.FStaticData <> nil then
        begin
          if i > Tree.MainTreeColumn then
            dec(i);
          if Item.ColumnText.Count <= i then
            s := ''
          else
            s := Item.ColumnText[i];
        end
        else
          Tree.TriggerVirtualTextNeeded(Item, i, s);
      end;
      if PSRec(IterateData).NoCase then
      begin
        {$ifdef ELPACK_UNICODE}
        if WideSameText(S, PSRec(IterateData).TextToFind) then
        {$else}
        if AnsiSameText(S, PSRec(IterateData).TextToFind) then
        {$endif}
        begin
          PSRec(IterateData).result := Item;
          ContinueIterate := false;
          exit;
        end;
      end else
      begin
        if S = PSRec(IterateData).TextToFind then
        begin
          PSRec(IterateData).result := Item;
          ContinueIterate := false;
          exit;
        end;
      end;
    end;
  end;

function TElTreeItems.LookForItem(StartItem: TElTreeItem;
  TextToFind: TElFString;
  DataToFind: pointer;
  ColumnNum: integer;
  LookForData,
  CheckStartItem,
  SubItemsOnly,
  VisibleOnly,
  NoCase: boolean): TElTreeItem;

type
  TSRec = record
    TextToFind: TElFString;
    ColumnNum: integer;
    StartItem: TElTreeItem;
    DataToFind: pointer;
    CheckStartItem,
      LookForData,
      SubItemsOnly: boolean;
    NoCase: boolean;
    Result: TElTreeItem;
  end;
  PSRec = ^TSRec;

var
  SRec: TSRec;
begin
  SRec.TextToFind := TextToFind;
  SRec.ColumnNum := ColumnNum;
  SRec.StartItem := StartItem;
  SRec.LookForData := LookForData;
  SRec.DataToFind := DataToFind;
  SRec.NoCase := NoCase;
  SRec.CheckStartItem := CheckStartItem;
  SRec.SubItemsOnly := SubItemsOnly;
  SRec.Result := nil;
  try          
    IterateFrom(VisibleOnly, not VisibleOnly, IntFindLookForItem, @SRec, StartItem); (*<+>*)
  finally
    Result := SRec.Result;
  end;
end;

procedure IntFindLookForItemEx2(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
  var
    b: boolean;

  begin
    if (Item = PSRecEx(IterateData).StartItem) and (not PSRecEx(IterateData).CheckStartItem) then exit;
    if ((not PSRecEx(IterateData).CheckStartItem) or (Item <> PSRecEx(IterateData).StartItem)) and
    (PSRecEx(IterateData).SubItemsOnly) and (not Item.IsUnder(PSRecEx(IterateData).StartItem)) then
    begin
      ContinueIterate := false;
      exit;
    end;
    b := PSRecEx(IterateData).CompProc(Item, PSRecEx(IterateData).SearchDetails);
    if b then
    begin
      PSRecEx(IterateData).result := Item;
      ContinueIterate := false;
      exit;
    end;
  end;

(*<+>*)
function TElTreeItems.LookForItemEx2(StartItem: TElTreeItem; ColumnNum: integer;
      CheckStartItem, SubItemsOnly, VisibleOnly, CheckCollapsed: boolean;
      SearchDetails: pointer;
      CompareProc: TElLookupCompareProc): TElTreeItem;

var
  SRec: TSRecEx;
begin
  SRec.ColumnNum := ColumnNum;
  SRec.StartItem := StartItem;
  SRec.CheckStartItem := CheckStartItem;
  SRec.SubItemsOnly := SubItemsOnly;
  SRec.CompProc := CompareProc;
  SRec.SearchDetails := SearchDetails;
  SRec.Result := nil;
  try
    IterateFrom(VisibleOnly, CheckCollapsed, IntFindLookForItemEx2, @SRec, StartItem); (*<+>*)
  finally
    Result := SRec.Result;
  end;
end;


  procedure IntFindLookForItem2(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
  var
    SubStrPos, i: integer;
    s, CopiedSubStr: TElFString;

  begin
    if (Item = PSRec(IterateData).StartItem) and (not PSRec(IterateData).CheckStartItem) then exit;
    if (PSRec(IterateData).SubItemsOnly) and not (Item = PSRec(IterateData).StartItem) and (not Item.IsUnder(PSRec(IterateData).StartItem)) then
    begin
      ContinueIterate := false;
      exit;
    end;
    if PSRec(IterateData).LookForData then
    begin
      if Item.FData = PSRec(IterateData).DataToFind then
      begin
        PSRec(IterateData).result := Item;
        ContinueIterate := false;
        exit;
      end;
    end else
    begin
      if (PSRec(IterateData).ColumnNum = -1) or (PSRec(IterateData).ColumnNum = Tree.MainTreeColumn) then s := Item.Text else
      begin
        i := PSRec(IterateData).ColumnNum;
        if i > Tree.MainTreeColumn then dec(i);
        if Item.ColumnText.Count <= i then s := ''
        else s := Item.ColumnText[i];
      end;
      if PSRec(IterateData).NoCase then
      begin
        if (PSRec(IterateData).WholeTextOnly and
           {$ifdef ELPACK_UNICODE}
           (WideSameText(S, PSRec(IterateData).TextToFind))) or
           {$else}
           (AnsiSameText(S, PSRec(IterateData).TextToFind))) or
           {$endif}
           (not PSRec(IterateData).WholeTextOnly and
           {$ifdef ELPACK_UNICODE}
           (WidePos(WideUpperCase(PSRec(IterateData).TextToFind), WideUpperCase(S)) > 0))
           {$else}
           (Pos(UpperCase(PSRec(IterateData).TextToFind),UpperCase(S)) > 0))
           {$endif}
           then
        begin
          PSRec(IterateData).result := Item;
          ContinueIterate := false;
          exit;
        end;
      end
      else
      begin
        {$ifdef ELPACK_UNICODE}
        SubStrPos := WidePos(PSRec(IterateData).TextToFind, S);
        {$else}
        SubStrPos := Pos(PSRec(IterateData).TextToFind,S);
        {$endif}
        CopiedSubStr := '';
        if SubStrPos > 0 then
        {$ifdef ELPACK_UNICODE}
          CopiedSubStr := WideCopy(S, SubStrPos, Length(PSRec(IterateData).TextToFind));
        {$else}
          CopiedSubStr := Copy(S, SubStrPos, Length(PSRec(IterateData).TextToFind));
        {$endif}
        if (PSRec(IterateData).WholeTextOnly and (S = PSRec(IterateData).TextToFind)) or
           (not PSRec(IterateData).WholeTextOnly and
           {$ifdef ELPACK_UNICODE}
           (WidePos(PSRec(IterateData).TextToFind,S) > 0))
           {$else}
           (Pos(PSRec(IterateData).TextToFind,S) > 0))
           {$endif}
           then
        begin
          PSRec(IterateData).result := Item;
          ContinueIterate := false;
          exit;
        end;
      end;
    end;
  end;

(*<+>*)
function TElTreeItems.LookForItem2(StartItem: TElTreeItem;
  TextToFind: TElFString;
  WholeTextOnly: boolean;
  DataToFind: pointer;
  ColumnNum: integer;
  LookForData,
  CheckStartItem,
  SubItemsOnly,
  VisibleOnly,
  CheckCollapsed,
  NoCase: boolean): TElTreeItem;

var
  SRec: TSRec;
begin
  SRec.TextToFind := TextToFind;
  SRec.WholeTextOnly := WholeTextOnly;
  SRec.ColumnNum := ColumnNum;
  SRec.StartItem := StartItem;
  SRec.LookForData := LookForData;
  SRec.DataToFind := DataToFind;
  SRec.NoCase := NoCase;
  SRec.CheckStartItem := CheckStartItem;
  SRec.SubItemsOnly := SubItemsOnly;
  SRec.Result := nil;
  try
    IterateFrom(VisibleOnly, CheckCollapsed, IntFindLookForItem2, @SRec, StartItem); (*<+>*)
  finally
    Result := SRec.Result;
  end;
end;

function TElTreeItems.LookForItemEx(StartItem: TElTreeItem; ColumnNum: integer; CheckStartItem, SubItemsOnly,
  VisibleOnly: boolean; SearchDetails: pointer; CompareProc: TElLookupCompareProc): TElTreeItem;

type
  TSRec = record
    ColumnNum: integer;
    StartItem: TElTreeItem;
    CheckStartItem,
      SubItemsOnly: boolean;
    Result: TElTreeItem;
    CompProc: TElLookupCompareProc;
    SearchDetails: pointer;
  end;
  PSRec = ^TSRec;

var
  SRec: TSRec;
  IntFind: TIterateProcAnonymusMethod;
begin
  IntFind := procedure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree)
      var
        b: boolean;

      begin
        if (Item = PSRec(IterateData).StartItem) and (not PSRec(IterateData).CheckStartItem) then exit;
        if (PSRec(IterateData).SubItemsOnly) and (not Item.IsUnder(PSRec(IterateData).StartItem)) then
        begin
          ContinueIterate := false;
          exit;
        end;
        b := PSRec(IterateData).CompProc(Item, PSRec(IterateData).SearchDetails);
        if b then
        begin
          PSRec(IterateData).result := Item;
          ContinueIterate := false;
          exit;
        end;
      end;
  SRec.ColumnNum := ColumnNum;
  SRec.StartItem := StartItem;
  SRec.CheckStartItem := CheckStartItem;
  SRec.SubItemsOnly := SubItemsOnly;
  SRec.CompProc := CompareProc;
  SRec.SearchDetails := SearchDetails;
  SRec.Result := nil;
  try
    IterateFrom(VisibleOnly, not VisibleOnly, IntFind, @SRec, StartItem); (*<+>*)
  finally
    Result := SRec.Result;
  end;
end;

function TElTreeItems.GetRootCount: Integer;
begin
  Result := FRoot.Count;
end;

function TElTreeItems.GetRootItem(Index: Integer): TElTreeItem;
begin
  Result := FRoot.Item[Index];
end;

procedure TElTreeItems.LoadFromStringList(Strings : TStrings);
var i : integer;
begin
  BeginUpdate;
  try
    Clear;
    for i := 0 to Strings.Count - 1 do
      InsertItemFromString(i, Strings[i]);
  finally
    EndUpdate;
  end;
end;

{$ifdef ELPACK_UNICODE}
procedure TElTreeItems.LoadFromWideStringList(Strings : TElWideStrings);
var i : integer;
begin
  BeginUpdate;
  try
    Clear;
    for i := 0 to Strings.Count - 1 do
      InsertItemFromString(i, Strings[i]);
  finally
    EndUpdate;
  end;
end;
{$endif}

procedure TElTreeItems.InsertItemFromString(Index : integer; AString :
    TElFString);
var l,
    cl    : integer;
    p,
    op    : PElFChar;
    CItem,
    NItem : TElTreeItem;
    S     : TElFString;
    i     : integer;
begin
  if Index > 0 then
    CItem := GetItem(Index - 1)
  else
    CItem := nil;
  l := 0;
  p := PElFChar(AString);
  while (p^ = #9) do
  begin
    inc(p);
    inc(l);
  end;
  if CItem <> nil then
    cl := CItem.Level
  else
    cl := 0;
  if l > cl + 1 then
    l := cl + 1;
  while cl > l do
  begin
    CItem := CItem.Parent;
    dec(cl);
  end;
  if (cl = l) and (CItem <> nil) then
    CItem := CItem.Parent;
  NItem := AddItem(CItem);
  i := 0;
  while true do
  begin
    op := p;
    while (p^ <> #9) and (p^ <> #0) do
    begin
      inc(p);
    end;
    SetLength(S, p - op);
    if p - op > 0 then 
      Move(op^, S[1], Length(S) * sizeof(TElFChar));

    if ((i = 0) and (not FOwner.FShowHeader)) or (i = FOwner.FMainTreeCol) then
      NItem.Text := S
    else
      NItem.ColumnText.Add(S);
    inc(i);
    if (p^ = #0) then
      break
    else
      inc(p);
  end;
end;

procedure TElTreeItems.SaveToStringList(AStrings : TStrings);
var i, j,
    k    : integer;
    S    : String;
    AnItem : TElTreeItem;
begin
  AStrings.Clear;
  for i := 0 to Count - 1 do
  begin
    AnItem := GetItem(i);
    S := MakeString(AnItem.Level, #9);
    if FOwner.FShowHeader then
      k := Owner.HeaderSections.Count - 1
    else
      k := 0;
    for j := 0 to k do
    begin
      if j > 0 then S := S + #9;
      if (not FOwner.FShowHeader) or (j = FOwner.FMainTreeCol) then
        S := S + AnItem.Text
      else
      if j < FOwner.FMainTreeCol then
      begin
        if AnItem.ColumnText.Count > j then
          S := S + AnItem.ColumnText[j];
      end
      else
      begin
        if AnItem.ColumnText.Count > j - 1 then
          S := S + AnItem.ColumnText[j - 1];
      end;
    end;
    AStrings.Add(S);
  end;
end;

{$ifdef ELPACK_UNICODE}
procedure TElTreeItems.SaveToWideStringList(AStrings : TElWideStrings);
var i, j,
    k    : integer;
    S    : WideString;
    AnItem : TElTreeItem;
begin
  AStrings.Clear;
  for i := 0 to Count - 1 do
  begin
    AnItem := GetItem(i);
    S := WideMakeString(AnItem.Level, #9);
    if FOwner.FShowHeader then
      k := Owner.HeaderSections.Count - 1
    else
      k := 0;
    for j := 0 to k do
    begin
      if j > 0 then S := S + #9;
      if (not FOwner.FShowHeader) or (j = FOwner.FMainTreeCol) then
        S := S + AnItem.Text
      else
      if j < FOwner.FMainTreeCol then
      begin
        if AnItem.ColumnText.Count > j then
          S := S + AnItem.ColumnText[j];
      end
      else
      begin
        if AnItem.ColumnText.Count > j - 1 then
          S := S + AnItem.ColumnText[j - 1];
      end;
    end;
    AStrings.Add(S);
  end;
end;
{$endif}

// ****************************************************************************
//                                 TCustomElTree          
// ****************************************************************************

procedure TCustomElTree.SetBSVLines;
begin
  if (FBSVLines = value) then exit;
  IsUpdating := true;
  with FView do
  begin
    FBSVLines := value;
    if BarStyle then
    begin
      FClearAll := true;
      FUpdated := true;
    end;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.AlignPieces;
var TreeRect,
    HeaderRect,
    HScrollRect,
    VScrollRect : TRect;
    BlindRect   : TRect;
    hh, hs, wv  : integer;
begin
  if not HandleAllocated then exit;
  FIgnoreSBChange := true;
  try
    TreeRect := ClientRect;
    if FShowHeader then
    begin
      TElHeaderHack(FHeader).AdjustHeaderHeight;
      SavedHH := GetHeaderHeight;
      hh := SavedHH;
      HeaderRect := Rect(0, 0, TreeRect.Right, hh);
      if StickyHeaderSections then inc(HeaderRect.Right);
      TreeRect.Top := hh;
    end;
    SetRectEmpty(BlindRect);
    if FHScrollVisible {$ifndef CLX_USED}and FUseCustomBars{$endif} then
    begin
      hs := FHScrollBar.Height;
      Dec(TreeRect.Bottom, hs);
      HScrollRect := Rect(0, TreeRect.Bottom, TreeRect.Right, TreeRect.Bottom + hs);
    end;
    if FVScrollVisible {$ifndef CLX_USED}and FUseCustomBars{$endif} then
    begin
      wv := FVScrollBar.Width;
      if RightAlignedText xor ScrollBarOpposite then
      begin
        VScrollRect := Rect(TreeRect.Left, 0, TreeRect.Left + wv, TreeRect.Bottom);
        Inc(HeaderRect.Left, wv);
        Inc(TreeRect.Left, wv);
        Inc(HScrollRect.Left, wv);
        if FHScrollVisible then
           BlindRect := Rect(0, VScrollRect.Bottom + 1, VScrollRect.Right, HScrollRect.Bottom);
      end else
      begin
        VScrollRect := Rect(TreeRect.Right - wv, 0, TreeRect.Right, TreeRect.Bottom);
        dec(HeaderRect.Right, wv);
        dec(TreeRect.Right, wv);
        dec(HScrollRect.Right, wv);
        if FHScrollVisible then
           BlindRect := Rect(VScrollRect.Left, VScrollRect.Bottom + 1, VScrollRect.Right, HScrollRect.Bottom);
      end;
    end;
    {$ifdef CLX_USED}
    FIgnoreResizes := true;
    {$endif}
    {$ifndef CLX_USED}
    with HeaderRect do
      if FShowHeader then
        SetWindowPos(FHeader.Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top, SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOOWNERZORDER or SWP_SHOWWINDOW or SWP_NOSENDCHANGING)
      else
        SetWindowPos(FHeader.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_HIDEWINDOW);
    with TreeRect do
        SetWindowPos(FView.Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top, SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER or SWP_SHOWWINDOW or SWP_NOSENDCHANGING);
    with HScrollRect do
      if FHScrollVisible {$ifndef CLX_USED}and FUseCustomBars{$endif} then
        SetWindowPos(FHScrollBar.Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top, SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOZORDER or SWP_SHOWWINDOW or SWP_NOSENDCHANGING)
      else
        SetWindowPos(FHScrollBar.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_HIDEWINDOW);
    with VScrollRect do
      if FVScrollVisible {$ifndef CLX_USED}and FUseCustomBars{$endif} then
        SetWindowPos(FVScrollBar.Handle, HWND_TOP, VScrollRect.Left, VScrollRect.Top, VScrollRect.Right - VScrollRect.Left, VScrollRect.Bottom - VScrollRect.Top, SWP_NOACTIVATE or SWP_NOOWNERZORDER {or SWP_NOZORDER }or SWP_SHOWWINDOW or SWP_NOSENDCHANGING)
      else
        SetWindowPos(FVScrollBar.Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_HIDEWINDOW);
    {$else}
    with HeaderRect do
      if FShowHeader then
      begin
        FHeader.SetBounds(Left, Top, Right - Left, Bottom - Top);
        FHeader.Visible := true;
      end
      else
      begin
        FHeader.Visible := false;
        //FHeader.SetBounds(0,0,0,0);
      end;
    with TreeRect do
      FView.SetBounds(Left, Top, Right - Left, Bottom - Top);
    with HScrollRect do
      if FHScrollVisible {$ifndef CLX_USED}and FUseCustomBars{$endif} then
      begin
        FHScrollBar.SetBounds(Left, Top, Right - Left, Bottom - Top);
        FHScrollBar.Visible := true;
      end
      else
      begin
        FHScrollBar.Visible := false;
        // FHScrollBar.SetBounds(0,0,0,0);
      end;
    with VScrollRect do
      if FVScrollVisible {$ifndef CLX_USED}and FUseCustomBars{$endif} then
      begin
        FVScrollBar.SetBounds(Left, Top, Right - Left, Bottom - Top);
        FVScrollBar.Visible := true;
      end
      else
      begin
        FVScrollBar.Visible := false;
        //FVScrollBar.SetBounds(0,0,0,0);
      end;
    {$endif}
    if not IsRectEmpty(BlindRect) then
    {$ifndef CLX_USED}
      InvalidateRect(Handle, @BlindRect, true);
    {$else}
    begin
      Inc(BlindRect.Bottom); Inc(BlindRect.Right);
      QWidget_update(Handle, @BlindRect);
      Dec(BlindRect.Bottom); Dec(BlindRect.Right);
    end;
    {$endif}
  finally
    {$ifdef CLX_USED}
    FIgnoreResizes := false;
    {$endif}
    FIgnoreSBChange := false;
  end;
end;

function TCustomElTree.GetRoot: TElTreeItem;
begin
  result := FItems.FRoot;
end;

type THackScrollBar = class(TElScrollBar)
       property Font;
       property NoScrollMessages;
     end;

constructor TCustomElTree.CreateClass;
begin
  CreateItemsExt(ItemClass);
  Create(AOwner);
end;

constructor TCustomElTree.Create;
{$ifdef CLX_USED}
var PS : TSize;
{$endif}
begin
  inherited;
  {$ifdef CLX_USED}
  InputKeys := [ikNav];
  {$endif}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
  {$ifdef VER3_EDITORS}
  FEditorManager := TElTreeInplaceManager.Create(Self);
  {$endif}
{$endif}
  FUseCustomBars := true;
  ControlStyle :=
{$IFDEF VER90}
  [csClickEvents, csOpaque, csDoubleClicks, csCaptureMouse,
    csDisplayDragImage, csReplicatable];
{$ELSE}
  [csClickEvents, csOpaque, csDoubleClicks, csCaptureMouse,
    csDisplayDragImage, csReplicatable{$ifndef CLX_USED}, csReflector{$endif}];
{$ENDIF}

  FAllList := TElList.Create;
  FSelectedList := TElList.Create;
  if FItems = nil then
    FItems := CreateItems;

  //BMP := TBitmap.Create;
  Height := 100;
  Width := 200;
  FView := CreateView;
  FMainTextType := sftText;
  FHintType := shtHintOrText;
  FQuickEditMode := false;
{$IFDEF HAS_HTML_RENDER}
  FView.FRender := TElHTMLRender.Create;
  FView.FRender.OnImageNeeded := TriggerImageNeededEvent;
{$ENDIF}
  FView.TabStop := false;
  FView.OnClick := ClickTransfer;
  FView.OnDblClick := DblClickTransfer;
  FView.OnDragDrop := DropTransfer;
  FView.OnDragOver := OverTransfer;
  FView.OnEndDrag := DragTransfer;
  FView.OnEnter := EnterTransfer;
  FView.OnExit := ExitTransfer;
  FView.OnKeyDown := KeyDownTransfer;
  FView.OnKeyPress := KeyPressTransfer;
  FView.OnKeyUp := KeyUpTransfer;
  FView.OnMouseDown := MouseDownTransfer;
  FView.OnMouseMove := MouseMoveTransfer;
  FView.OnMouseUp := MouseUpTransfer;
  FView.OnStartDrag := StartDragTransfer;
  {$ifdef VCL_4_USED}
  FView.OnMouseWheel := MouseWheelTransfer;
  FView.OnMouseWheelDown := MouseWheelDownTransfer;
  FView.OnMouseWheelUp := MouseWheelUpTransfer;
  {$endif}
  {$ifndef CLX_USED}
  FView.Ctl3D := false;
  {$endif}
  FView.ParentShowHint := true;
  FView.TabStop := true;
  FView.TabOrder := 0;
  FView.ParentFont := true;

  FHScrollBar := TElScrollBar.Create(Self);
  THackScrollBar(FHScrollBar).ControlStyle := FHScrollBar.ControlStyle + [csNoDesignVisible];
  THackScrollBar(FHScrollBar).NoScrollMessages := true;
  FHorzScrollBarStyle := TElScrollBarStyles.Create(FHScrollBar, Self);
  FHorzScrollBarStyle.OnChange := SBChanged;
  FHScrollBar.OnDrawPart := HorzScrollDrawPartTransfer;
  FHScrollBar.OnScrollHintNeeded := HorzScrollHintNeededTransfer;

  FHScrollBar.ParentFont := true;
  FHScrollBar.TabStop := false;
  FHScrollBar.OnScroll := FView.OnHScroll;
  {$ifndef CLX_USED}
  FHScrollBar.Ctl3D := false;
  {$endif}
  {$ifndef CLX_USED}
  FHScrollBar.Height := GetSystemMetrics(SM_CYVTHUMB);
  {$else}
  QStyle_scrollBarExtent(Application.Style.Handle, @PS);
  FHScrollBar.Height := PS.cy;
  {$endif}
  FHScrollBar.Visible := false;
  FHScrollBar.OnMouseDown := ScrollBarMouseDown;
  FHScrollBar.AltDisablingArrows := true;
{$IFDEF HAS_HTML_RENDER}
  FHScrollBar.IsHTML := true;
  FLinkColor := clBlue;
  FLinkStyle := [fsUnderline];
{$ENDIF}

  FVScrollBar := TElScrollBar.Create(Self);
  THackScrollBar(FVScrollBar).ControlStyle := FHScrollBar.ControlStyle + [csNoDesignVisible];
  THackScrollBar(FVScrollBar).NoScrollMessages := true;
  FVertScrollBarStyle := TElScrollBarStyles.Create(FVScrollBar, Self);
  FVertScrollBarStyle.OnChange := SBChanged;
  FVScrollBar.OnDrawPart := VertScrollDrawPartTransfer;
  FVScrollBar.OnScrollHintNeeded := VertScrollHintNeededHandler;
  FVScrollBar.ShowTrackHint := true;

  FVScrollBar.ParentFont := true;
  FVScrollBar.Kind := sbVertical;
  FVScrollBar.TabStop := false;
  FVScrollBar.OnScroll := FView.OnVScroll;
  {$ifndef CLX_USED}
  FVScrollBar.Ctl3D := false;
  {$endif}
  {$ifndef CLX_USED}
  FVScrollBar.Width := GetSystemMetrics(SM_CXHTHUMB);
  {$else}
  QStyle_scrollBarExtent(Application.Style.Handle, @PS);
  FVScrollBar.Width := PS.cx;
  {$endif}
  FVScrollBar.Visible := false;
  FVScrollBar.OnMouseDown := ScrollBarMouseDown;
  FVScrollBar.AltDisablingArrows := true;
{$IFDEF HAS_HTML_RENDER}
  FVScrollBar.IsHTML := true;
  FLinkCursor := crHandPoint;
{$ENDIF}

  ItemExt := 17;
  FCheckBoxSize := 15;
  FHeader := CreateHeader;
  FView.FHeader := FHeader;
  FView.FItems := FItems;
  FHeaderHotTrack := true;

  with FHeader do
  begin
    TabStop := false;
    SetDesigning(csDesigning in ComponentState);
    OnSectionDraw := DoHeaderDraw;
    OnResize := HeaderResizeHandler;
    OnSectionResize := OnHeaderSectionResize;
    OnSectionClick := OnHeaderSectionClick;
    OnSectionDelete := OnHeaderSectionDelete;
    OnMouseDown := DoHeaderMouseDown;
    OnSectionMove := OnHeaderSectionMove;
    OnSectionShowHide := OnHeaderSectionVisChange;
    OnSectionChange := OnHeaderSectionChange;
    OnSectionCreate := OnHeaderSectionCreate;
    OnHeaderLookup := OnHeaderSectionLookup;
    OnHeaderLookupDone := OnHeaderSectionLookupDone;
    OnSectionExpand := Self.OnHeaderExpand;
    OnSectionCollapse := Self.OnHeaderCollapse;
    OnSectionAutoSize := HeaderSectionAutoSizeHandler;
    OnFilterCall := SectionFilterCallTransfer;
    Tracking := FHeaderHotTrack;
    Align := alNone;
    Visible := false;
    AllowDrag := false;
    MultiSort := true;
  end; //with
  FHeader.Font.Assign(Font);

  FLineHeight := DefineLineHeight;

  FShowHeader := False;
  Font.OnChange := OnFontChange;

  {$IFNDEF LITE}
  FAdjustMultilineHeight := true;
  {$ENDIF}

  FBorderStyle := bsSingle;
  FBkColor := clWindow;
  FTextColor := clWindowText;
  FCanEdit := true;
  FHLines := false;
  FVLines := false;
  FVerticalLinesLong := true;
  FFullRowSelect := true;
  FSelMode := smUsual;
  FShowHintMode := shmLong;
  FMultiSelect := true;
  FMultiSelectLevel := -1;  // CNS
  FDragScrollInterval := 100;
  FMouseFrameSelect := true;
  FExpandOnDblClick := true;
  FDblClickMode := dcmExpand;
  FHideSelect := false;
  FODFollowCol := true;
  FODMask := '~~@~~';
  FRowSelect := True;
  FTracking := true;
{$IFDEF HOTTRACK_CURSOR}
  FTrackingCursor := crDefault;
{$ENDIF}
  FShowButtons := True;
  FShowImages := True;
  FShowLines := True;
  FDrawFocusRect := true;
  TabStop := true;
  FAutoLineHeight := true;
  FLineHintType := lhtSmart;
  FDragTrgDrawMode := SelColorRect;
  FPathSeparator := '\';
  FSortMode := smNone;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  DragMode := dmManual;
  FHideHintOnMove := true;
  FAlwaysKeepSelection := true;

  //FPlusPicture := TBitmap.Create;
  //FPlusPicture.OnChange := OnSignChange;
  //FMinusPicture := TBitmap.Create;
  //FMinusPicture.OnChange := OnSignChange;
  //FLeafPicture := TBitmap.Create;
  //FLeafPicture.OnChange := OnSignChange;
  //FCheckBoxGlyph := TBitmap.Create;
  //FRadioButtonGlyph := TBitmap.Create;

  FSelectColumn := -1;
  FDragType := dtDelphi;
  FLinesColor := clBtnFace;
  FLineHintColor := FBkColor;
  FLinesStyle := psDot;
  FActiveBorderType := fbtSunken;
  FInactiveBorderType := fbtSunkenOuter;
{$IFDEF ELPACK_COMPLETE}
  FStoragePath := '\Tree';
  {$ifndef CLX_USED}
  FImgFormChLink := TImgFormChangeLink.Create;
  FImgFormChLink.OnChange := ImageFormChange;
  {$endif}
{$ENDIF}
  FUnderlineTracked := true;
  FShowRootButtons := true;
  FAutoResizeColumns := True;
  FFlatFocusedScrollbars := true;
  FHorzDivLinesColor := clBtnFace;
  FVertDivLinesColor := clBtnFace;
{$IFNDEF LITE}
  FBackground := TBitmap.Create;
  FBackground.OnChange := BackgroundChange;
  FBackgroundType := bgtColorFill;
{$ENDIF}
  FFocusedSelectColor := clHighlight;
  FHideSelectColor := clBtnFace;
  FFocusedSelectTextColor := clHighlightText;
  FHideSelectTextColor := clBtnShadow;
  FTrackColor := clHighlight;
  FDragRectAcceptColor := clGreen;
  FDragRectDenyColor   := clRed;
  FRightClickSelect := true;
  FSortType := stText;
  FSortUseCase := true;

  {$ifdef MSWINDOWS}
  FBorderSides := [ebsLeft, ebsTop, ebsRight, ebsBottom];
  {$endif}

  FStripedEvenColor := clBtnHighlight;
  FStripedOddColor := clBtnShadow;

  FHeaderUseTreeFont := true;
  FHeaderFont := TFont.Create;
  FHeaderFont.OnChange := HeaderFontChanged;
  
  FLineHintTimeout := 3000;
  FChangeDelay := 500;
  FDragExpandDelay := 500;
{$IFNDEF LITE}
  FGradientSteps := 64;
{$ENDIF LITE}

  FDelayTimer := TTimer.Create(self);

  FDelayTimer.Enabled := false;
  FDelayTimer.OnTimer := OnDelayTimer;
  FInplaceEditorDelay := 500;

  {$ifndef CLX_USED}
  FHook := TElHook.Create(nil);
  FHook.OnBeforeProcess := OnBeforeHook;
  {$endif}
  FDoubleBuffered := true;

  // for CLX to work, these must be the last
  FView.Parent := Self;
  FHScrollBar.Parent := Self;
  FVScrollBar.Parent := Self;
  FHeader.Parent := Self;
  FCursor := crArrow;
end;

destructor TCustomElTree.Destroy;
begin
  inherited Destroying;
  if IsEditing then EndEdit(true);
{$IFNDEF LITE}
  FBackground.Free;
  {$ifndef CLX_USED}
  {$IFDEF ELPACK_COMPLETE}
  ImageForm := nil;
  FImgFormChLink.Free;
  {$ENDIF}
  {$endif}
{$ENDIF}

  UseXPThemes := false;
  FCheckBoxGlyph.Free;
  FRadioButtonGlyph.Free;

  FPlusPicture.Free;
  FMinusPicture.Free;
  FLeafPicture.Free;

  FItems.Free;
{$IFDEF HAS_HTML_RENDER}
  FView.FRender.Free;
{$ENDIF}
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
  if FDropTarget <> nil then
     FDropTarget.Free;
{$ENDIF}
{$ENDIF}
{$endif}
  FView.Free;
  FVertScrollBarStyle.Free;
  FHorzScrollBarStyle.Free;
  FHScrollBar.Free;
  FVScrollBar.Free;
  FHeader.Free;
  FHeaderFont.Free;
  FDelayedItem := nil;
  FDelayTimer.Free;
  FDelayTimer := nil;
  if FSelectedList <> nil then
    FSelectedList.Free;
  FAllList.Free;
  if Font <> nil then Font.OnChange := nil;
  if FImages <> nil then FImages.UnregisterChanges(FImageChangeLink);
  if FImages2 <> nil then FImages2.UnregisterChanges(FImageChangeLink);
  FImageChangeLink.Free;
  //BMP.Free;
{$ifdef ELTREE_USE_INPLACE_EDITORS}
  {$ifdef VER3_EDITORS}
  FEditorManager.Free;
  {$endif}
{$endif}
  FSortSections.Free;
  FSortSections := nil;
  {$ifndef CLX_USED}
  FHook.Free;
  FHook := nil;
  {$endif}
  inherited Destroy;
end;

{$IFDEF HAS_HTML_RENDER}
procedure TCustomElTree.SetLinkColor(newValue : TColor);
begin
  if (FLinkColor <> newValue) then
  begin
    FLinkColor := newValue;
    ReRenderAllHTMLItems;
    Repaint;
  end;
end;

procedure TCustomElTree.SetLinkStyle(newValue : TFontStyles);
begin
  if (FLinkStyle <> newValue) then
  begin
    FLinkStyle := newValue;
    ReRenderAllHTMLItems;
    Repaint;
  end;
end;
{$ENDIF}

{$ifndef CLX_USED}
procedure TCustomElTree.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
  if Assigned(FDropTarget) then FDropTarget.Target := Self;
{$ENDIF}
{$ENDIF}
{$endif}
  AlignPieces;
end; {CreateWindowHandle}
{$endif}

function TCustomElTree.GetSelCount;
begin
  if MultiSelect then
  begin
    result := FSelectedList.Count;
  end else
  begin
    if ItemFocused = nil then result := 0 else result := 1;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElTree.CreateParams;
const
{$IFNDEF VCL_4_USED}
  BorderStyles: array[TBorderStyle] of Longint = (0, WS_BORDER);
{$ELSE}
  BorderStyles: array[TBorderStyle] of DWord = (0, WS_BORDER);
{$ENDIF}
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or (WS_HSCROLL or WS_VSCROLL);

    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;

    with Params.WindowClass do
      style := style and not (CS_HREDRAW or CS_VREDRAW);
    if RightAlignedText then
      ExStyle := ExStyle or WS_EX_RTLREADING or WS_EX_RIGHT and (not WS_EX_RIGHTSCROLLBAR);
  end;
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElTree.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;
{$endif}

procedure TCustomElTree.SetHorzScrollBarStyle(newValue : TElScrollBarStyles);
begin
  FHorzScrollBarStyle.Assign(newValue);
end;

procedure TCustomElTree.SetVertScrollBarStyle(newValue : TElScrollBarStyles);
begin
  FVertScrollBarStyle.Assign(newValue);
end;

function TCustomElTree.GetItemAt;
begin
  y := Y - FView.Top;
  X := X - FView.Left;
  result := FView.GetItemAt(X, Y, ItemPart, HitColumn);
end;

procedure TCustomElTree.SetImages;
var
  i: integer;
begin
  if FImages = value then exit;
  IsUpdating := true;
  if FImages <> nil then
  begin
    {$ifdef VCL_5_USED}
    FImages.RemoveFreeNotification(Self);
    {$endif}
    FImages.UnRegisterChanges(FImageChangeLink);
  end;
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  if csDestroying in ComponentState then exit;
  i := DefineLineHeight;
  if FAutoLineHeight and (i <> FLineHeight) then
  begin
    FLineHeight := i;
    with FView do
    begin
      FClearVis := true;
      FClearAll := true;
    end;
  end;
  if not FShowHeader then
  begin
    with FView do
    begin
      FHRange := -1;
      DefineHRange;
    end;
    if RightAlignedTree then
    begin
      FRightAlignedTree := false;
      RightAlignedTree := true;
    end;
  end else
  begin
    if FShowHeader and FHeader.Sections[FMainTreeCol].AutoSize then
      if (FUpdateCount = 0) then
        AutoSizeColumn(FMainTreeCol)
      else
        FColSizeUpdate := true;
  end;
  with FView do
  begin
    FRangeUpdate := true;
    FUpdated := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.SetLineStyle;
begin
  if FShowLines = value then exit;
  IsUpdating := true;
  FShowLines := value;
  with FView do
  begin
    if not FShowHeader then FHRange := -1;
    FRangeUpdate := true;
    FUpdated := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.SetShowEmptyImages(newValue : boolean);
begin
  if FShowEmptyImages <> newValue then
  begin
    FShowEmptyImages := newValue;
    if (not ShowImages) or (FImages = nil) then exit;
    IsUpdating := true;
    with FView do
    begin
      if not FShowHeader then FHRange := -1;
      FUpdated := true;
      FRangeUpdate := true;
    end;
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.SetShowEmptyImages2(newValue : boolean);
begin
  if FShowEmptyImages2 <> newValue then
  begin
    FShowEmptyImages2 := newValue;
    if (not ShowImages) or (FImages = nil) then exit;
    IsUpdating := true;
    with FView do
    begin
      if not FShowHeader then FHRange := -1;
      FUpdated := true;
      FRangeUpdate := true;
    end;
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.SetRootStyle;
begin
  if FShowRoot = value then exit;
  IsUpdating := true;
  FShowRoot := value;
  with FView do
  begin
    if not FShowHeader then
      FHRange := -1;
    FUpdated := true;
    FRangeUpdate := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.SetImagesStyle;
var
  i: integer;
begin
  if FShowImages = value then exit;
  IsUpdating := true;
  FShowImages := value;
  if not FShowHeader then FView.FHRange := -1;
  i := DefineLineHeight;
  if FAutoLineHeight and (i <> FLineHeight) then
  begin
    FLineHeight := i;
    with FView do
    begin
      FClearVis := true;
      FClearAll := true;
    end;
  end;
  with FView do
  begin
    FRangeUpdate := true;
    FUpdated := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.ImageListChange;
var
  i: integer;
begin
  if FView.FPainting then exit;
  IsUpdating := true;
  if AutoLineHeight then
  begin
    i := DefineLineHeight;
    if i <> FLineHeight then
    begin
      with FView do
      begin
        FClearVis := true;
        if i < FLineHeight then FClearAll := true;
      end;
      FLineHeight := i;
    end;
  end;
  with FView do
  begin
    FUpdated := true;
    FRangeUpdate := true;
  end;
  IsUpdating := false;
end;

{$ifndef CLX_USED}
procedure TCustomElTree.SetBorderStyle;
begin
  if FBorderStyle = value then exit;
  FBorderStyle := value;
  RecreateWnd;
end;
{$endif}

procedure TCustomElTree.SetButtonStyle;
begin
  if FShowButtons = value then exit;
  IsUpdating := true;
  FShowButtons := value;
  with FView do
  begin
    if not FShowHeader then FHRange := -1;
    FUpdated := true;
    FRangeUpdate := true;
  end;
  IsUpdating := false;
end;

function TCustomElTree.GetUpdating : boolean;
begin
  result := FUpdateCount > 0;
end;

procedure TCustomElTree.SetUpdating;
var
  i: integer;
begin
  if FProcUpdate then exit;

  if value = true then
  begin
    FHeader.BeginUpdate;
    inc(FUpdateCount)
  end
  else
  begin
    dec(FUpdateCount);
    FHeader.EndUpdate;
  end;
  
  if FUpdateCount > 0 then
    exit;
  if (csDestroying in ComponentState) or (csLoading in ComponentState) then
    exit;
  FProcUpdate := true;
  if FilteredVisibility then
    UpdateDiffItems;
  with FView do
  begin
    if FAutoLineHeight then
    begin
      i := DefineLineHeight;
      if i <> FLineHeight then
      begin
        FClearVis := true;
        FLineHeight := i;
      end;
    end;
    if (FHRange = -1) and (not FShowHeader) then FView.DefineHRange;
    if FSortRequired then
    begin
      TriggerSortBegin;
      Sort(true);
      TriggerSortEnd;
      FSortRequired := false;
    end;
    if FUpdated then
    begin
      if FVisUpdated then // recount visibles
      begin
        TotalVisCount := GetTotalVisCount;
        FVisUpdated := false;
      end;
      IgnoreResize := true;
      if (not (csLoading in ComponentState)) and (HandleAllocated) then
      begin
        UpdateScrollBars;
      end;
      IgnoreResize := false;
      if FClearVis and HandleAllocated then // refresh FVisible
      begin
        DoSetTopIndex(FTopIndex);
        FClearVis := false;
      end;
      IgnoreResize := true;
      if FShowHeader and (VirtualityLevel = vlNone) and (FColSizeUpdate) then
      begin
        FColSizeUpdate := false;
        AutoSizeAllColumns;
      end;
      if (not (csLoading in ComponentState)) and (HandleAllocated) then
        UpdateScrollBars;
      IgnoreResize := false;
      UpdateView;
      FUpdated := false;
    end;
  end;
  FProcUpdate := false;
end;

function TCustomElTree.GetTotalVisCount;
var i, j, idx : integer;
    Item : TElTreeItem;
begin
  i := 0;
  idx := 0;
  j := FAllList.Count;
  while i < j do
  begin
    Item := TElTreeItem(FAllList[i]);
    if (FilteredVisibility and Item.Hidden) then
    begin
      i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end else
    begin
      inc(idx);
      if not Item.Expanded then
         i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end;
    inc(i);
  end;
  result := idx;
  TotalVisCount := result;
end;

{
function TCustomElTree.VisToIndex;
type PGIRec = ^TGIRec;
     TGIRec = record
       VisIndex,
       Visibles:integer;
       j:integer;
     end;
var GIRec : TGIRec;

    procedure IntVisToIndex(Item:TElTreeItem; Index: integer; var ContinueIterate:boolean;
                                              IterateData:pointer; Tree:TCustomElTree);
    var i : integer;
    begin
      if Item.FullyExpanded then inc(PGIRec(IterateData)^.Visibles);
      if PGIRec(IterateData)^.Visibles=PGIRec(IterateData)^.VisIndex then
      begin
        PGIRec(IterateData)^.j:=Index;
        ContinueIterate:=false;
      end;
    end;

begin
  GIRec.VisIndex:=Index;
  GIRec.Visibles:=-1;
  GIRec.j:=-1;
  FItems.Iterate(false,true,@IntVisToIndex,@GIRec); (*<+>*)
  result:=GIRec.j;
end;
}
{function TCustomElTree.IndexToVis;
type PGIRec = ^TGIRec;
     TGIRec = record
       VisIndex,
       Visibles:integer;
       j:integer;
     end;
var GIRec : TGIRec;

    procedure IntIndexToVis(Item:TElTreeItem; Index: integer; var ContinueIterate:boolean;
                                              IterateData:pointer; Tree:TCustomElTree);
    var i : integer;
    begin
      if Item.FullyExpanded then inc(PGIRec(IterateData)^.Visibles);
      if Index=PGIRec(IterateData)^.VisIndex then
      begin
        PGIRec(IterateData)^.j:=PGIRec(IterateData)^.Visibles;
        ContinueIterate:=false;
      end;
    end;

begin
  GIRec.VisIndex:=Index;
  GIRec.Visibles:=-1;
  GIRec.j:=-1;
  FItems.Iterate(false,true,@IntIndexToVis,@GIRec); (*<+>*)
  result:=GIRec.j;
end;
}

procedure TCustomElTree.UpdateScrollBars;

  function UpdHBar: boolean;
  var
    b1: boolean;
    ScrollInfo: TScrollInfo;
    CurPos: integer;
    hsw : integer;
  begin
    b1 := FHScrollVisible;
    ScrollInfo.cbSize := Sizeof(ScrollInfo);
    ScrollInfo.fMask := SIF_POS or SIF_RANGE;
    {$ifndef CLX_USED}
    GetScrollInfo(FHScrollBar.Handle, SB_CTL, ScrollInfo);
    {$else}
    Self.GetScrollInfo(FHScrollBar, SB_CTL, ScrollInfo);
    {$endif}
    if FShowHeader then
    begin
      FView.FHRange := 0;
      if FHeader.Sections.Count > 0 then
      begin
        hsw := FHeader.SectionsWidth;
        ScrollInfo.nMax := hsw;
        FView.FHRange := hsw;
      end
         else ScrollInfo.nMax := 0;
    end
    else
      ScrollInfo.nMax := FView.FHRange;
    if ScrollInfo.nMax < 0 then
      ScrollInfo.nMax := 0;
    ScrollInfo.nPage := FView.Width + 1;

    FHScrollVisible := (ScrollInfo.nMax - Integer(ScrollInfo.nPage) > 0) and (not FHideHorzScrollBar);
    if (not FHScrollVisible) then
    begin
      ScrollInfo.nMax := 0;
      FHPos := 0;
      FHeader.LeftPos := 0
      //LockedColumn, update
      //if FHeader.Left < 0 then FHeader.Left := 0;
    end;
    result := (FHScrollVisible <> b1) and (not (ForcedScrollBars in [ssHorizontal, ssBoth]));

    FHPos := Min(FHPos, Max(0, ScrollInfo.nMax));
    CurPos := FHPos;

    ScrollInfo.fMask := SIF_ALL;

    ScrollInfo.nMin := 0;
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.nPos := CurPos;
    ScrollInfo.nTrackPos := CurPos;

    {$ifndef CLX_USED}
    SetScrollInfo(FHScrollBar.Handle, SB_CTL, ScrollInfo, true);
    {$else}
    Self.SetScrollInfo(FHScrollBar, SB_CTL, ScrollInfo, true);
    {$endif}

    {$ifndef CLX_USED}
    // Set info for hidden standard scrollbar
    if {$ifndef CLX_USED}FUseCustomBars or {$endif}(ForcedScrollBars in [ssHorizontal, ssBoth]) then
       ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;
    Windows.SetScrollInfo(Handle, SB_HORZ, ScrollInfo, true);
    {$endif}
    if (FHeader.LeftPos <> FHPos) then
      FHeader.LeftPos := FHPos;
    FHScrollVisible := FHScrollVisible or (ForcedScrollBars in [ssHorizontal, ssBoth])
  end;

  function UpdVBar: boolean;
  var
    b: boolean;
    CHeight   : integer;
    ScrollInfo: TScrollInfo;
    maxV      : integer;
  begin                   
    if TotalVisCount = 0 then
      GetTotalVisCount;
    maxV := TotalVisCount - 1;

    if TotalVarHeightCount > 0 then
      MaxV := FView.CalcPageUpPos(maxV);

    ScrollInfo.nMax := maxV;


    ScrollInfo.fMask := SIF_ALL;
    ScrollInfo.nMin := 0;
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.nPos := FTopIndex;
    ScrollInfo.nTrackPos := FTopIndex;
    B := FVScrollVisible;
    CHeight := FView.Height;
    FVScrollVisible := ((FTopIndex > 0) or (FView.GetVisiblesHeight > CHeight) or ((ScrollInfo.nMax > FView.FVisible.Count) and (FView.FVisible.Count > 0))) and (not FHideVertScrollBar);
    //if (FTopIndex > 0) and (not FHideVertScrollBar) then FVScrollVisible := true;

    if TotalVarHeightCount > 0 then
      ScrollInfo.nPage := 1// min((FView.CalcPageDownPos(TopIndex) - FView.CalcPageUpPos(TopIndex)) shr 1{TopIndex}, View.GetVisCount - 1)
    else
      ScrollInfo.nPage := View.GetVisCount - 1;

    if not (FVScrollVisible) then
    begin
      ScrollInfo.nMax := 0;
      ScrollInfo.nPage := 1;
    end;

    {$ifndef CLX_USED}
    SetScrollInfo(FVScrollBar.Handle, SB_CTL, ScrollInfo, true);
    {$else}
    SetScrollInfo(FVScrollBar, SB_CTL, ScrollInfo, true);
    {$endif}
    // Set info for hidden standard scrollbar

    //ScrollInfo.fMask := SIF_POS or SIF_RANGE or SIF_PAGE;
    {$ifndef CLX_USED}
    if {$ifndef CLX_USED}FUseCustomBars or {$endif}(ForcedScrollBars in [ssVertical, ssBoth]) then
       ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;

    Windows.SetScrollInfo(Handle, SB_VERT, ScrollInfo, true);
    {$endif}

    result := (FVScrollVisible <> b) and (not (ForcedScrollBars in [ssVertical, ssBoth]));
    FVScrollVisible := FVScrollVisible or (ForcedScrollBars in [ssVertical, ssBoth])
  end;

// var FHVis, FVVis : boolean;
var i : integer;
begin
  // FHVis := FHScrollVisible;
  // FVVis := FVScrollVisible;
  i := 0;
  while UpdHBar or UpdVBar do
  begin
    AlignPieces;
    inc(i);
    if i > 3 then break;
  end;
  {
  if (FHVis <> FHScrollVisible) or (FVVis <> FVScrollVisible) then
  begin
    AlignPieces;
  end;
  }
  if not FScrollbarsInitialized then
  begin
    FScrollbarsInitialized := true;
    {$ifndef CLX_USED}
    if UseCustomScrollbars then
      PostMessage(Handle, WM_UPDATESBFRAME, 0, 0);
    {$endif}
  end;
  {$ifndef CLX_USED}
  if {$ifndef CLX_USED}FUseCustomBars and {$endif}(BorderStyle = bsSingle) then
     DrawFlatBorder(false, false);
  {$endif}
end;

procedure TCustomElTree.SetHLines;
var
  i: integer;
begin
  if FHLines = value then exit;
  IsUpdating := true;
  FHLines := value;
  if FAutoLineHeight then
  begin
    i := DefineLineHeight;
    if i <> FLineHeight then
    begin
      FLineHeight := i;
      with FView do
      begin
        FClearVis := true;
        FClearAll := true;
      end;
    end;
  end;
  with FView do
  begin
    FRangeUpdate := true;
    FUpdated := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.SetVLines;
begin
  if FVLines = value then exit;
  IsUpdating := true;
  with FView do
  begin
    FVLines := value;
    FClearAll := true;
    FUpdated := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.OnHeaderSectionResize;
begin
  IsUpdating := true;
  DoOnColumnResize(Section.Index);
  with FView do
  begin
    FUpdated := true;
    FRangeUpdate := true;
    FClearVis := true;
    if FVLines then FClearAll := true else FRangeUpdate := true;
  end;
  IsUpdating := false;
  {if FHeader.SectionsWidth < FView.Width then AlignPieces;}
end;

procedure TCustomElTree.OnHeaderSectionLookup(Sender: TObject; Section: TElHeaderSection; var Text: string);

type
  TSRec = record
    Text: PChar;
    ColNum: integer;
  end;
  PSRec = ^TSRec;

var
  SRec: TSrec;
  TI: TElTreeItem;

  function IntCompare(Item: TElTreeItem; SearchDetails: Pointer): boolean;
  var
    i: integer;
    AT: TElFString;
  begin
    i := PSRec(SearchDetails).ColNum;
    if Item.FOwner.MainTreeColumn = i then
      AT := AnsiUpperCase(Item.Text)
    else
    begin
      if Item.FStaticData <> nil then
      begin
        if Item.ColumnText.Count <= i then
          AT := ''
        else
        begin
          if I > Item.FOwner.MainTreeColumn then
            AT := AnsiUpperCase(Item.ColumnText[i - 1])
          else
            AT := AnsiUpperCase(Item.ColumnText[i]);
        end;
      end
      else
      begin
        Item.FOwner.TriggerVirtualTextNeeded(Item, i, AT);
        AT := AnsiUpperCase(AT);
      end;
    end;
    result := Pos(AnsiUpperCase(StrPas(PSRec(SearchDetails).Text)), AT) = 1;
  end;

begin
  TriggerHeaderLookupEvent(Section, Text);
  if AutoLookup then
  begin
    SRec.Text := PChar(Text);
    SRec.ColNum := Section.Index;
    TI := Items.LookForItemEx2(nil, Section.Index, false, false, false, true,@SRec, @IntCompare);  (*<+>*)
    if TI <> nil then
    begin
      EnsureVisible(TI);
      ItemFocused := TI;
    end;
  end;
end;

procedure TCustomElTree.OnHeaderExpand;
begin
  TriggerHeaderSectionExpandEvent(Section);
  try
    IsUpdating := true;
    with FView do
    begin
      FUpdated := true;
      FClearAll := true;
    end;
    //if FShowHeader then FHeader.Repaint;
  finally
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.OnHeaderCollapse;
begin
  TriggerHeaderSectionCollapseEvent(Section);
  try
    IsUpdating := true;
    with FView do
    begin
      FUpdated := true;
      FClearAll := true;
    end;
    //if FShowHeader then FHeader.Repaint;
  finally
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.OnHeaderSectionLookupDone(Sender: TObject; Section: TElHeaderSection; Text: string; Accepted: boolean);
begin
  TriggerHeaderLookupDoneEvent(Section, Text, Accepted);
end;

procedure TCustomElTree.DoHeaderMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
  if FView.FInpEdit <> nil then FView.DoEndEdit(true);
{$else}
  if FView.FInpEdit <> nil then FView.DoEndEdit(true);
{$endif}
{$endif}
  if Assigned(FOnHeaderMouseDown) then
    FOnHeaderMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TCustomElTree.OnHeaderSectionClick;
begin
  if (FSortMode = smClick) or (FSortMode = smAddClick) then
  begin
    IsUpdating := true;
    if not (ssCtrl in GetShiftState) then
    begin
      FHeader.MultiSort := false;
      if FSortSections <> nil then
        FSortSections.Clear;
      SortSection := Section.Index;
      if Section.SortMode = hsmNone then
        if SortDir = sdAscend then
          Section.SortMode := hsmDescend
        else
          Section.SortMode := hsmAscend;
      if Section.SortMode = hsmAscend then
        Section.SortMode := hsmDescend
      else
        Section.SortMode := hsmAscend;

      case Section.FieldType of
        sftText: SortType := stText;
        sftNumber: SortType := stNumber;
        sftFloating: SortType := stFloating;
        sftDateTime: SortType := stDateTime;
        sftDate: SortType := stDate;
        sftTime: SortType := stTime;
        sftBool: SortType := stBoolean;
        sftCurrency: SortType := stCurrency;
      else
        SortType := stCustom;
      end;
      FHeader.MultiSort := true;
    end
    else
    begin
      if FSortSections = nil then
        FSortSections := TElList.Create;
      if Section.SortMode = hsmNone then
      begin
        Section.SortMode := hsmAscend;
        AddSortSection(Section.Index, false);
      end
      else
      if Section.SortMode = hsmAscend then
      begin
        Section.SortMode := hsmDescend;
        if FSortSections.IndexOf(Section) = -1 then
          FSortSections.Add(Section);
      end
      else
      begin
        Section.SortMode := hsmNone;
        RemoveSortSection(Section.Index, false);
      end;
    end;
    TriggerSortBegin;
    FItems.FRoot.Sort(true);
    TriggerSortEnd;
    with FView do
    begin
      FUpdated := true;
      FRangeUpdate := true;
      FVisUpdated := true;
    end;
    IsUpdating := false;
  end;
  DoColumnClick(Section.Index);
end;

procedure TCustomElTree.OnSignChange(Sender: TObject);
var
  i: integer;
begin
  if not FCustomPlusMinus then exit;
  IsUpdating := true;
  if AutoLineHeight then
  begin
    i := DefineLineHeight;
    if i <> FLineHeight then
    begin
      with FView do
      begin
        FClearVis := true;
        if i < FLineHeight then FClearAll := true;
      end;
      FLineHeight := i;
    end;
    FView.FVisUpdated := true;
  end;
  with FView do
  begin
    FRangeUpdate := true;
    FUpdated := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.OnFontChange;
var
  i : integer;
begin
  if FView.FPainting then exit;
  IsUpdating := true;
  ParentFont := false;
  Canvas.Font.Assign(Font);
  {$ifndef CLX_USED}
  Perform(CM_FONTCHANGED, 0, 0);
  {$else}
  NotifyControls(CM_PARENTFONTCHANGED);
  {$endif}
  if HeaderUseTreeFont then
    FHeader.Font.Assign(Font);
  //TElHeaderHack(FHeader).OnFontChange(FHeader.Font);
  {$ifdef USE_HTML_RENDER}
  ReRenderAllHTMLItems;
  {$endif}
  if AutoLineHeight then
  begin
    i := DefineLineHeight;
    if i <> FLineHeight then
    begin
      with FView do
      begin
        FClearVis := true;
        if i < FLineHeight then FClearAll := true;
      end;
      FLineHeight := i;
    end;
  end;
  with FView do
  begin
    FUpdated := true;
    FRangeUpdate := true;
  end;
  IsUpdating := false;
end;

{$IFNDEF LITE}
function TCustomElTree.GetHeaderWrapCaptions : boolean;
begin
  result := TElHeader(FHeader).WrapCaptions;
end;

procedure TCustomElTree.SetHeaderWrapCaptions(Value : boolean);
begin
  TElHeader(FHeader).WrapCaptions := value;
end;
{$ENDIF}
procedure TCustomElTree.SetHeaderHotTrack;
begin
  FHeaderHotTrack := value;
  FHeader.Tracking := value;
end;

procedure TCustomElTree.SetRowSelect;
begin
  IsUpdating := true;
  FRowSelect := value;
  if FRowSelect = false then FullRowSelect := false;
  with FView do
  begin
    FClearAll := true;
    FClearVis := true;
    FUpdated := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.SetMultiSelect;
var TSI: TElTreeItem;
begin
  if Value <> FMultiSelect then
  begin
    IsUpdating := true;
    FUpdated := true;
    FMultiSelect := value;
    TSI := ItemFocused;
    if Value = true then
    begin
      FSelectedList := TElList.Create;
      with FView do
        if FSelected <> nil then FSelectedList.Add(FSelected);
    end else
    begin
      DeselectAllEx(true);
      if TSI <> nil then
      begin
        TSI.Focused := true;
        TSI.Selected := true;
      end;
      FSelectedList.Free;
      FSelectedList := nil;
    end;
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.SetMultiSelectLevel;    // CNS
var TSI: TElTreeItem;
begin
  if Value<>FMultiSelectLevel then
  begin
    IsUpdating := True;
    FUpdated := true;
    FMultiSelectLevel := value;
    TSI := ItemFocused;
    DeselectAllEx(True);  // just deselect everything on change
    if TSI <> nil then    // and reselected the focused item
    begin
      TSI.Focused := true;
      TSI.Selected := true;
    end;
    IsUpdating := false;
  end;
end;

function TCustomElTree.DoSetFocused(value: TElTreeItem; Forced : boolean):
    Boolean;
begin
  result := DoSetFocusedEx(value, Forced, false);
end;

function TCustomElTree.DoSetFocusedEx(value: TElTreeItem; Forced, Delayed : 
    boolean): Boolean;
var
  AnItem,
  FOld     : TElTreeItem;
  DoChange : boolean;

begin

  result := false;
  (*if (value <> nil) and
     (not value.AllowSelection) then exit;
  *)
  if FView.FFocused <> Value then
  begin
    DoChange := Forced;
    DoChanging(Value, DoChange);

    if (not Forced) and (not DoChange) then
    begin
      if MultiSelect then
        FView.DoSetSelected(FView.FFocused);
      exit;
    end;

    IsUpdating := true;
    try
      AnItem := FView.FFocused;
      if AnItem <> nil then with AnItem do
      begin
        Exclude(FState, stsFocused);
        RedrawItem(true);
        FView.FFocused := nil;
        if FAutoExpand and (value <> nil) then
        begin
          FOld := AnItem;
          if AutoCollapse then
          while (FOld <> nil) and (not Value.IsUnder(FOld)) do
          begin
            FOld.Collapse(false);
            FOld := FOld.Parent;
          end; // while
          FUpdated := true;
        end;
      end;
      result := true;
      FView.FFocused := value;
      FView.DoSetSelected(value);
      if FView.FFocused <> nil then
      begin
        with FView do
        begin
          Include(FFocused.FState, stsFocused);
          FFocused.RedrawItem(true);
          if FAutoExpand then FFocused.Expand(false);
        end;
        if not (csDestroying in ComponentState) then
        begin
          with FView do
            if (FVisible.IndexOf(FFocused) = FVisible.Count - 1) and
               (Getvisiblesheight > Height) and (FVisible.Count > 1) then
              TopIndex := TopIndex + 1;
        end;
      end;
      if (FChangeDelay = 0) or (not Delayed) then
        DoItemFocused
      else
      begin
        StopDelayedFocus;
        StartDelayedFocus(FView.FFocused);
      end;
      FUpdated := true;
    finally
      IsUpdating := false;
    end;
  end;
end;

procedure TCustomElTree.DoChanging(Item : TElTreeItem; var AllowChange: Boolean);
begin
  if Assigned(FOnChanging) and (FFireFocusEvents = 0) then
     FOnChanging(Self, Item, AllowChange)
  else AllowChange := true;
end;

procedure TCustomElTree.SetFocused;
begin
 DoSetFocused(Value, false);
end;

function TCustomElTree.GetNextSelected;
var i : integer;
begin
  if not (FMultiSelect) then
  begin
    if Prev = nil then
       result := ItemFocused
    else
       result := nil;
    exit;
  end;

  if (Prev = nil) and (FSelectedList.Count > 0) then
     result := TElTreeItem(FSelectedList[0])
  else
  begin
    i := FSelectedList.IndexOf(Prev);
    if (i <> -1) and (i < FSelectedList.Count - 1) then
       result := TElTreeItem(FSelectedList[i+1])
    else
       result := nil;
  end;
end;

procedure TCustomElTree.AllSelected(SelectedItems: TElList);
begin
  if FMultiSelect then
    SelectedItems.Assign(FSelectedList)
  else
  begin
    SelectedItems.Clear;
    if (ItemFocused <> nil) then
      SelectedItems.Add(ItemFocused);
  end;
end;

procedure TCustomElTree.EnsureVisible;
begin
  if IsUpdating then exit;
  FView.FillVisFwd(TopIndex);

  if Item = nil then
     exit;
  if FView.FVisible.IndexOf(Item) <> -1 then exit;
  if not Item.FullyExpanded then
     Item.MakeFullyExpanded(true);
  IsUpdating := true;
  SetVPosition(Item.VisIndex);
  FUpdated := true;
  with FView do
  begin
    FRangeUpdate := true;
    FClearAll := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.EnsureVisibleBottom;
begin
  if IsUpdating then exit;
  if Item = nil then exit;
  if FView.FVisible.IndexOf(Item) <> -1 then exit;
  if not Item.FullyExpanded then
     Item.MakeFullyExpanded(true);
  IsUpdating := true;
  SetVPosition(FView.CalcPageUpPos(Item.VisIndex) + 1);
  FUpdated := true;
  with FView do
  begin
    FRangeUpdate := true;
    FClearAll := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.SetAutoLineHeight;
begin
  if FAutoLineHeight = value then
     exit;
  IsUpdating := true;
  FAutoLineHeight := value;
  if value = true then
     FLineHeight := DefineLineHeight;
  FUpdated := true;
  with FView do
  begin
    FClearAll := true;
    FClearVis := true;
  end;
  FUpdated := true;
  IsUpdating := false;
end;

procedure TCustomElTree.SetItemIndent(value: integer);
begin
  if ItemExt <> Value then
  begin
    IsUpdating := true;
    ItemExt := Value;
    FUpdated := true;
    FView.FClearAll := true;
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.SetLineHeight;
begin
  if (FLineHeight = value) or FAutoLineHeight then
     exit;
  IsUpdating := true;
  FUpdated := true;
  FLineHeight := value;
  with FView do
  begin
    FClearVis := true;
    FClearAll := true;
  end;
  if value = 0 then FLineHeight := DefineLineHeight;
  IsUpdating := false;
end;

function TCustomElTree.DefineLineHeight;
var
  m: integer;
begin
  m := 0;
  if FShowImages then
  begin
    if (Images <> nil) then m := Images.Height;
    if (Images2 <> nil) then m := Max(m, Images2.Height);
  end;
  if ItemExt > m then m := ItemExt;
  if Font.Height < 0 then
     m := max(m, Abs(Font.Height) + 4)
  else
     m := max(m, Abs(Font.Height) + 2);
  if ShowCheckboxes then
  begin
    if CustomCheckboxes then
    begin
      m := Max(m, Max(FRadioButtonGlyph.Height, FCheckBoxGlyph.Height));
    end else
    begin
      m := max(m, ItemExt - 2);
    end;
  end;
  result := m;
  if FBarStyle then inc(result, 2);
  if FHLines and not (FBarStyle) then inc(result, FDivLineWidth);
end;

{$ifndef CLX_USED}
procedure TCustomElTree.WMSize;
begin
  inherited;
{$IFNDEF VCL_4_USED}
  if not (csLoading in ComponentState) and (not IgnoreResize) then Resize;
{$ENDIF}
  if Flat or FUseCustomBars or IsThemeApplied then UpdateFrame;
end;
{$endif}

procedure TCustomElTree.Resize;
begin
{$IFNDEF VCL_4_USED}
  if Assigned(FOnResize) then FOnResize(Self);
{$ELSE}
  inherited;
{$ENDIF}
  IsUpdating := true;
  AlignPieces;
  (*
  with FView do
  begin
    FVisUpdated := true;
    FClearAll := true;
    FClearVis := true;
  end;
  FUpdated := true;
  *)
  IsUpdating := false;
end;

function TCustomElTree.GetItemRect;
begin
  result := FView.GetItemRect(ItemIndex);
  OffsetRect(result, 0, FView.Top);
end;

procedure TCustomElTree.OnHeaderSectionVisChange(Sender: TCustomElHeader; Section: TElHeaderSection); { private }
begin
  IsUpdating := true;
  with FView do
  begin
    FClearAll := true;
    FHRange := -1;
  end;
  if FHeader.SectionsWidth < FView.Width then
  begin
    FHeader.Invalidate;
    SetHPosition(0);
  end;
  FUpdated := true;
  IsUpdating := false;
end; { OnHeaderSectionVisChange }

procedure TCustomElTree.DoOnColumnResize(SectionIndex: integer); { protected }
begin
  if Assigned(FOnColumnResize) then FOnColumnResize(self, SectionIndex);
end; { DoOnColumnResize }

procedure TCustomElTree.DoColumnClick(SectionIndex: integer); { protected }
begin
  if Assigned(FOnColumnClick) then FOnColumnClick(self, SectionIndex);
end; { DoColumnClick }

procedure TCustomElTree.DoItemFocused; { protected }
begin
  if Assigned(FOnItemFocused) and
     (not (csDestroying in ComponentState)) and
     (FFireFocusEvents = 0) then
    FOnItemFocused(self);
end; { DoItemFocused }

procedure TCustomElTree.DoItemDraw(Item: TElTreeItem; Surface: TCanvas; R: TRect; SectionIndex: integer); { protected }
begin
  if Assigned(FOnItemDraw) then FOnItemDraw(Self, Item, Surface, R, SectionIndex);
end; { DoItemDraw }

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TCustomElTree.DoValidate(Item: TElTreeItem; Section: TElHeaderSection; var Text: string; var Accept: boolean); { protected }
begin
  FDelOnEdit := true;
  if assigned(FOnValidate) then FOnValidate(self, Item, Section, Text, Accept);
  FDelOnEdit := false;
end; { DoValidate }
{$endif}
{$endif}

procedure TCustomElTree.DoShowHint(Item: TElTreeItem; Section : TElHeaderSection; var Text: TElFString; HintWindow: THintWindow; MousePos: TPoint; var DoShowHint: boolean); { protected }
begin
  if assigned(FOnShowHint) then
    FOnShowHint(Self, Item, Section, Text, HintWindow, MousePos, DoShowHint);
end; { DoShowHint }

procedure TCustomElTree.DoItemChange(Item: TElTreeItem; ItemChangeMode: TItemChangeMode); { protected }
begin
  if Assigned(FOnItemChange) then FOnItemChange(Self, Item, ItemChangeMode);
end; { DoOnItemChange }

procedure TCustomElTree.DoItemExpanding(Item: TElTreeItem; var CanProcess: boolean); { protected }
begin
  if assigned(FOnItemExpanding) then FOnItemExpanding(Self, Item, CanProcess);
end; { DoItemExpanding }

procedure TCustomElTree.DoItemCollapsing(Item: TElTreeItem; var CanProcess: boolean); { protected }
begin
  if assigned(FOnItemCollapsing) then FOnItemCollapsing(Self, Item, CanProcess);
end; { DoItemCollapsing }

procedure TCustomElTree.DoItemChecked(Item : TElTreeItem);  { protected }
begin
  if assigned(FOnItemChecked) then FOnItemChecked(Self, Item);
end;  { DoItemChecked }

procedure TCustomElTree.DoItemExpand(Item: TElTreeItem); { protected }
begin
  if assigned(FOnItemExpand) then FOnItemExpand(Self, Item);
end; { DoItemExpand }

procedure TCustomElTree.DoItemCollapse(Item: TElTreeItem); { protected }
begin
  if assigned(FOnItemCollapse) then FOnItemCollapse(Self, Item);
end; { DoItemCollapse }

procedure TCustomElTree.DoItemDelete(Item: TElTreeItem); { protected }
begin
  if assigned(FOnItemDelete) then FOnItemDelete(Self, Item);
end; { DoItemDelete }


procedure TCustomElTree.DoCompareItems(Item1, Item2: TElTreeItem; var res: integer); { protected }
begin
  if assigned(FOnCompareItems) then FOnCompareItems(Self, Item1, Item2, res) else res := 0;
end; { DoCompareItems }

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TCustomElTree.TriggerEditRequestEvent(Item: TElTreeItem; Section: TElHeaderSection);
begin
  if (assigned(FOnEditRequest)) then FOnEditRequest(Self, Item, Section);
end; { TriggerEditRequestEvent }

procedure TCustomElTree.TriggerComboEditShowEvent(Item: TElTreeItem; Section: TElHeaderSection; Combobox: TCombobox);
begin
  if (assigned(FOnComboEditShow)) then
    FOnComboEditShow(Self, Item, Section, Combobox);
end; { TriggerComboEditShowEvent }

procedure TCustomElTree.DoValidateCombo(Item: TElTreeItem; Section: TElHeaderSection; Combo: TComboBox; var Accept: boolean);
begin
  FDelOnEdit := true;
  if (assigned(FOnValidateCombo)) then
    FOnValidateCombo(Self, Item, Section, Combo, Accept);
  FDelOnEdit := false;
end; { TriggerValidateComboEvent }
{$endif}
{$endif}

procedure TCustomElTree.OnHeaderSectionChange(Sender: TCustomElHeader; Section: TElHeaderSection; Change: TSectionChangeMode); { protected }
begin
  case Change of
    scmAlign:
      if Section.Visible then
      begin
        IsUpdating := true;
        FView.FClearAll := true;
        IsUpdating := false;
      end;
    scmStyle:
      if FODFollowCol and Section.Visible then
      begin
        IsUpdating := true;
        FView.FClearAll := true;
        IsUpdating := false;
      end;
    scmPassword:
      if Section.Visible then
        FView.Invalidate;//Repaint;
  end;
end; { OnHeaderSectionChange }

function TCustomElTree.CreateHeader: TElHeader; { protected }
begin
  result := TElHeader.Create(self);
end; { CreateHeader }

{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TCustomElTree.SetCanEdit(value: boolean);
begin
  FCanEdit := value;
end;
{$endif}

procedure TCustomElTree.OnHeaderSectionCreate(Header: TCustomElHeader; Section: TElHeaderSection); { protected }
begin
  IsUpdating := true;
  FUpdated := true;
  AlignPieces;
  IsUpdating := false;
end; { OnHeaderSectionCreate }

procedure TCustomElTree.Paint; { protected }
var R : TRect;
    {$ifndef CLX_USED}
  {$IFDEF ELPACK_COMPLETE}
    ACtl   : TWinControl;
    ax, ay : integer;
    P      : TPoint;
    BgRect : TRect;
  {$ENDIF}
    {$endif}
begin
  {$ifdef MSWINDOWS}
  if ((Flat {$ifndef CLX_USED}or FUseCustomBars{$endif}) {$ifndef CLX_USED}and (BorderStyle = bsSingle){$endif}) {$ifdef MSWINDOWS}or IsThemeApplied{$endif} then
    DrawFlatBorder(false, false);
  {$endif}
  if (FVScrollVisible and FHScrollVisible) {$ifndef CLX_USED}and FUseCustomBars{$endif} then
  begin
    R := Rect(FVScrollbar.Left, FHScrollBar.Top, FVScrollBar.Left + FVScrollBar.Width, FHScrollBar.Top + FHScrollBar.Height);
    {$ifdef MSWINDOWS}
    if IsThemeApplied then
    begin
      R := Rect(FVScrollbar.Left, FHScrollBar.Top, FVScrollBar.Left + FVScrollBar.Width, FHScrollBar.Top + FHScrollBar.Height);
      InflateRect(R, 1, 1);
      {$ifndef CLX_USED}
      DrawThemeBackground(Theme, Canvas.Handle, 0, 0, R, nil);
      {$else}
      Canvas.Start;
      DrawThemeBackground(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), 0, 0, R, nil);
      Canvas.Stop;
      {$endif}
    end
    else
    {$endif}
    {$ifndef CLX_USED}
{$IFDEF ELPACK_COMPLETE}
    if (FImgForm <> nil) and (not (csDesigning in FImgForm.GetRealControl.ComponentState)) then
    begin
      //if (FImgForm.Control <> Self) then
      begin
        ACtl := FImgForm.GetRealControl;
        BgRect := R;
        BgRect.TopLeft := ClientToScreen(BgRect.TopLeft);
        P := Parent.ClientToScreen(Point(Left, Top));
        ax := BgRect.Left - P.x;
        ay := BgRect.Top - P.y;

        BgRect.BottomRight := ClientToScreen(BgRect.BottomRight);
        BgRect.TopLeft := ACtl.ScreenToClient(BgRect.TopLeft);
        BgRect.BottomRight := ACtl.ScreenToClient(BgRect.BottomRight);
        FImgForm.PaintBkgnd(Canvas.Handle, R, Point(BgRect.Left - ax, BgRect.Top - ay), false);
      end;
    end
    else
{$ENDIF}
    {$endif}
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := FVScrollBar.Color;
      Canvas.FillRect(R);
    end;
  end;
end; { Paint }

function TCustomElTree.GetMoveColumnOnDrag: Boolean;
begin
  result := FHeader.MoveOnDrag;
end; { GetMoveColumnOnDrag }

procedure TCustomElTree.SetMoveColumnOnDrag(newValue: Boolean);
begin
  if (FHeader.MoveOnDrag <> newValue) then FHeader.MoveOnDrag := newValue;
end; { SetMoveColumnOnDrag }

procedure TCustomElTree.SetHideHorzScrollBar(newValue: Boolean);
begin
  if (FHideHorzScrollBar <> newValue) then
  begin
    FHideHorzScrollBar := newValue;
    IsUpdating := true;
    with FView do
    begin
      FVisUpdated := true;
      FClearAll := true;
      FClearVis := true;
    end;
    FUpdated := true;
    AlignPieces;
    IsUpdating := false;
  end; { if }
end; { SetHideHorzScrollBar }

procedure TCustomElTree.SetHideVertScrollBar(newValue: Boolean);
begin
  if (FHideVertScrollBar <> newValue) then
  begin
    FHideVertScrollBar := newValue;
    IsUpdating := true;
    with FView do
    begin
      FVisUpdated := true;
      FClearAll := true;
      FClearVis := true;
    end;
    FUpdated := true;
    AlignPieces;
    IsUpdating := false;
  end; { if }
end; { SetHideVertScrollBar }

procedure TCustomElTree.SetUnderlineTracked(newValue: Boolean);
begin
  if (FUnderlineTracked <> newValue) then
  begin
    FUnderlineTracked := newValue;
    with FView do
      if Tracking and (FTrackItem <> nil) then FTrackItem.RedrawItem(true);
  end; { if }
end; { SetUnderlineTracked }

procedure TCustomElTree.SetCustomCheckboxes(newValue: Boolean);
begin
  if (FCustomCheckboxes <> newValue) then
  begin
    FCustomCheckboxes := newValue;
    if FCustomCheckboxes then
    begin
      GetCheckBoxGlyph;
      GetRadioButtonGlyph;
    end;
    if ShowCheckBoxes then
    begin
      IsUpdating := true;
      FUpdated := true;
      FView.FClearAll := true;
      IsUpdating := false;
    end;
  end; { if }
end; { SetCustomCheckboxes }

procedure TCustomElTree.SetCheckBoxGlyph(newValue: TBitmap);
begin
  CheckBoxGlyph.Assign(newValue);

  if newValue = nil then
    CustomCheckboxes := false;
  if ShowCheckBoxes then
  begin
    IsUpdating := true;
    FUpdated := true;
    FView.FClearAll := true;
    IsUpdating := false;
  end;
end; { SetCheckBoxGlyph }

procedure TCustomElTree.SetRadioButtonGlyph(newValue: TBitmap);
{ Sets data member FRadioButtonGlyph to newValue. }
begin
  RadioButtonGlyph.Assign(newValue);

  if newValue = nil then
    CustomCheckboxes := false;
  if ShowCheckBoxes then
  begin
    IsUpdating := true;
    FUpdated := true;
    FView.FClearAll := true;
    IsUpdating := false;
  end;
end; { SetRadioButtonGlyph }

procedure TCustomElTree.SetShowRootButtons(newValue: Boolean);
begin
  if FShowRootButtons = newValue then exit;
  IsUpdating := true;
  FShowRootButtons := newValue;
  if not FShowHeader then FView.FHRange := -1;
  FUpdated := true;
  FView.FRangeUpdate := true;
  IsUpdating := false;
end;

procedure TCustomElTree.SetHideFocusRect(newValue: Boolean);
begin
  if (FHideFocusRect <> newValue) then
  begin
    FHideFocusRect := newValue;
    if (not Focused) and (HandleAllocated) then
    begin
      IsUpdating := true;
      FView.FRangeUpdate := true;
      FUpdated := true;
      IsUpdating := false;
    end;
  end; {if}
end;

function TCustomElTree.GetLockHeaderHeight: Boolean;
begin
  result := FHeader.LockHeight;
end; { GetLockHeaderHeight }

procedure TCustomElTree.SetLockHeaderHeight(newValue: Boolean);
begin
  if (LockHeaderHeight <> newValue) then
    FHeader.LockHeight := newValue;
end; { SetLockHeaderHeight }


{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
function TCustomElTree.GetInEditing : boolean;
begin
  result := FView.FEditing;
end;
{$endif}
{$endif}

{$ifndef CLX_USED}
procedure TCustomElTree.UpdateFrame;
var R : TRect;
begin
  if not HandleAllocated then exit;
  R := Rect( 0, 0, Width, Height);
  if (BorderStyle = bsSingle) then
     RedrawWindow(Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_Frame );
end;
{$endif}

type TSelHackWinControl = class (TWinControl)
     end;

{$ifndef CLX_USED}
procedure TCustomElTree.WMSetFocus(var Msg : TWMSetFocus);  { private }
begin
  inherited;
  if Msg.FocusedWnd <> FView.Handle then
    FView.SetFocus
  else
    TSelHackWinControl(Parent).SelectNext(Self, false, True);
  if (Flat and (FInactiveBorderType <> FActiveBorderType)) {$ifndef CLX_USED}or FUseCustomBars{$endif} then
    UpdateFrame;
end;  { WMSetFocus }

procedure TCustomElTree.WMKillFocus(var Msg : TWMKillFocus);  { private }
begin
  inherited;
  if not HandleAllocated then exit;
  if (Flat and (FInactiveBorderType <> FActiveBorderType)) {$ifndef CLX_USED}or FUseCustomBars{$endif} then
    UpdateFrame;
end;  { WMKillFocus }
{$endif}

procedure TCustomElTree.AutoSizeColumn(SectionIndex : integer);
var Section : TElHeaderSection;
begin
  if FUpdateCount > 0 then
  begin
    FColSizeUpdate := true;
    FUpdated := true;
    exit;
  end;
  Section := FHeader.Sections[SectionIndex];
  if (section <> nil) and Section.AutoSize then
  begin
    Section.Width := Max(FHeader.MeasureSectionWidth(Section, nil, nil), MeasureColumnWidth(SectionIndex, true));
    OnHeaderSectionResize(FHeader, Section);
  end;
end;

function TCustomElTree.CanFocus : boolean;
begin
  result := inherited CanFocus;
end;

procedure TCustomElTree.AutoSizeAllColumns;
var i : integer;
begin
  if FAutosizingColumns then exit;
  if FUpdateCount > 0 then
  begin
    FUpdated := true;
    FColSizeUpdate := true;
    exit;
  end;
  FAutosizingColumns := true;
  for i := 0 to Pred(FHeader.Sections.Count) do
  begin
    AutoSizeColumn(i);
  end;
  FAutosizingColumns := false;
end;

{$warnings off}
{$hints off}
procedure TCustomElTree.MeasureCell(Item : TElTreeItem; ColumnNum : integer; var Size : TPoint);
var HS : TElHeaderSection;
{$ifdef ELTREE_USE_STYLES}
    CurStyle : TElCellStyle;
{$endif}
    OwnFontSet : boolean;
    xxx,
    SaveFontStyle : TFontStyles;
    SaveFontSize : integer;
    SaveFontName : TFontName;
    cNum, AL : integer;
    {$ifdef CLX_USED}
    R2,
    {$endif}
    R3 : TRect;
    FTImages : TImageList;
    StImIndex : integer;
    FID       : boolean;
    ASize : TPoint;
    SText : TElFString;

begin
  if (ColumnNum = -1) or (ColumnNum = FMainTreeCol) then
  begin
{$ifdef ELTREE_USE_STYLES}
    if VirtualityLevel = vlNone then
    begin
      if Item.UseStyles then
        CurStyle := Item.MainStyle
      else
        CurStyle := nil;
    end
    else
    begin
      CurStyle := FView.VirtStyle;
      TriggerVirtualStyleNeeded(Item, ColumnNum, CurStyle); 
    end;
{$endif}
    Size.Y := FLineHeight;
    Size.X := 0;
    if (FShowRoot and FShowLines) or (FShowButtons and FShowRootButtons) then
      Inc(Size.X, ItemExt)
    else
      Inc(Size.X, ItemExt div 5);

    Inc(Size.X, Item.Level * ItemExt);

    if FShowCheckBoxes then
    begin
      if (Item.FBoolData1 and ibfShowCheckBox) = ibfShowCheckBox then
      begin
        if FCustomCheckboxes then
          inc(Size.X, FRadioButtonGlyph.Width div 6)
        else
          inc(Size.X, CheckBoxSize);
      end;
    end;

    if (FShowImages) then
    begin
      Fid := false;
      if FImages2 <> nil then
        FTImages := FImages2
      else
        FTImages := FImages;
      if FTImages <> nil then
      begin
        if (Item.Focused or Item.Selected or Item.Cut or (Item.Expanded and Item.HasVisibleChildren)) then StImIndex := Item.FStImageIndex2 else StImIndex := Item.FImageIndex2;
        if StImIndex = -1 then StImIndex := DoGetPicture2(Item);
        if InRange(0, FTImages.Count - 1, StImIndex) or FShowEmptyImages2 then
        begin
          inc(Size.X, FTImages.Width);
          if ShowCheckBoxes and Item.ShowCheckBox then
            inc(Size.X, ItemExt div 3);
          FID := true;
        end;
      end;
      if (FImages <> nil) then
      begin
        if (Item.Focused or Item.Selected or Item.Cut or (Item.Expanded and Item.HasVisibleChildren)) then StImIndex := Item.FStImageIndex else StImIndex := Item.FImageIndex;
        if StImIndex = -1 then StImIndex := DoGetPicture(Item);
        if InRange(0, FImages.Count - 1, StImIndex) or FShowEmptyImages then
        begin
          inc(Size.X, FImages.Width);
          if FID or
             (ShowCheckBoxes and Item.ShowCheckBox) then
            inc(Size.X, ItemExt div 3); // make the space between images
        end;
      end;
    end; // Show images

    if (FODFollowCol and ({FShowHeader and }(FHeader.Sections.Count > 0) and (FHeader.Sections[FMainTreeCol].Style = ElhsOwnerDraw)))
      or ((Item.Text = FODMask) and (not (FODFollowCol)))
{$ifdef ELTREE_USE_STYLES}
      or (Item.UseStyles and (CurStyle.FStyle = elhsOwnerDraw))
{$endif}
      then
    begin
      TriggerMeasureItemPartEvent(Item, ColumnNum, ASize);
      inc(Size.X, ASize.X + (ItemExt div 5) * 3);
    end
    else
    begin
      inc(Size.X, (ItemExt div 5) * 3);
{$ifdef ELTREE_USE_STYLES}
      if (((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) and (CurStyle.FStyle = elhsText)) or
         ((Item.FBoolData1 and ibfUseStyles) <> ibfUseStyles) then
{$endif}
      begin
        xxx := [];
        SaveFontStyle := Canvas.Font.Style;
        SaveFontSize := Canvas.Font.Height;
        SaveFontName := Canvas.Font.Name;
        OwnFontSet := false;
{$ifdef ELTREE_USE_STYLES}
        if ((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
        begin
          if not CurStyle.FOwnerProps then with CurStyle do
          begin
            if Canvas.Font.Name <> FontName then
            begin
              Canvas.Font.Name := FontName;
              OwnFontSet := true;
            end;
            xxx := FontStyles;
            if Canvas.Font.Size <> FontSize then
            begin
              Canvas.Font.Size := FontSize;
              OwnFontSet := true;
            end;
            end else
          begin
            if Canvas.Font.Name <> Font.Name then
            begin
              Canvas.Font.Name := Font.Name;
              OwnFontSet := true;
            end;
            if ((Item.FBoolData1 and ibfParentStyle) = ibfParentStyle) then
              xxx := Font.Style
            else
            begin
              if stsBold in Item.FState then Include(xxx, fsBold);
              if stsItalic in Item.FState then Include(xxx, fsItalic);
              if stsUnderlined in Item.FState then Include(xxx, fsUnderline);
              if stsStrikeOut in Item.FState then Include(xxx, fsStrikeout);
            end;
            if Canvas.Font.Size <> Font.Size then
            begin
              Canvas.Font.Size := Font.Size;
              OwnFontSet := true;
            end;
          end;
        end
        else
{$endif}
        begin
          if Canvas.Font.Name <> Font.Name then
          begin
            Canvas.Font.Name := Font.Name;
            OwnFontSet := true;
          end;
          if ((Item.FBoolData1 and ibfParentStyle) = ibfParentStyle) then
            xxx := Font.Style
          else
          begin
            if stsBold in Item.FState then Include(xxx, fsBold);
            if stsItalic in Item.FState then Include(xxx, fsItalic);
            if stsUnderlined in Item.FState then Include(xxx, fsUnderline);
            if stsStrikeOut in Item.FState then Include(xxx, fsStrikeout);
          end;
          if Canvas.Font.Size <> Font.Size then
          begin
            Canvas.Font.Size := Font.Size;
            OwnFontSet := true;
          end;
        end;
        if Canvas.Font.Style <> xxx then
        begin
          Canvas.Font.Style := xxx;
          OwnFontSet := true;
        end;
        {$ifndef CLX_USED}
        AL := DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}DT_VCENTER or DT_LEFT or DT_CALCRECT;
        if RightAlignedText then
          AL := AL or DT_RTLREADING;
        {$else}
        AL := MultiLineFlags[Item.Multiline] or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter);
        {$endif}

        SetRectEmpty(R3);
{$IFDEF HAS_HTML_RENDER}
        if Item.IsHTML and (Copy(Item.Text, 1, 6) = '<html>') then
        begin
          R3.Left := 0;
          R3.Top := 0;
          with FView.FRender do
          begin
            Data.DefaultStyle := Canvas.Font.Style;
            Data.DefaultFont  := Canvas.Font.Name;
            Data.DefaultHeight:= Canvas.Font.Height;
            Data.Charset      := Canvas.Font.Charset;

            PrepareText(Item.Text, 0, false);
            R3.Right := Data.TextSize.cx;
            R3.Bottom := Data.TextSize.cy;
          end;
        end
        else
{$ENDIF}
        {$ifndef CLX_USED}
        {$ifdef ELPACK_UNICODE}
          ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(Item.Text), Length(Item.Text), R3,  DT_NOPREFIX or AL);
        {$else}
          DrawText(Canvas.Handle, PChar(Item.Text), Length(Item.Text), R3,  DT_NOPREFIX or AL);
        {$endif}
        {$else}
          Canvas.TextExtent(Item.Text, R3, AL);
        {$endif}
        Size.Y := Max(Size.Y, R3.Bottom - R3.Top + 1);

        Inc(Size.X, R3.Right - R3.Left);
        if OwnFontSet then
        begin
          Canvas.Font.Style  := SaveFontStyle;
          Canvas.Font.Height := SaveFontSize;
          Canvas.Font.Name   := SaveFontName;
        end;
      end
{$ifdef ELTREE_USE_STYLES}
      else
      if Item.UseStyles and (CurStyle.FStyle = elhsPictureOnly) and Assigned(CurStyle.FPicture) then
        Inc(Size.X, CurStyle.Picture.Width + 3);
{$endif}
    end;
    if ShowColumns then
    begin
      if FVLines then
        Inc(Size.X, 4)
      else
        Inc(Size.X, 2);
    end;
    exit;
  end;
  HS := FHeader.Sections[ColumnNum];

  OwnFontSet := false;
{$ifdef ELTREE_USE_STYLES}
  CurStyle := nil;
  if Item.UseStyles then
  begin
    if Item.FStaticData <> nil then
    begin
      cNum := ColumnNum;
      if cNum > FMainTreeCol then
        dec(cNum);

      if (Item.StylesCount > cNum) and (not HS.UseMainStyle) then
         CurStyle := Item.Styles[cNum];
      if CurStyle = nil then
        CurStyle := Item.MainStyle;
    end
    else
    begin
      CurStyle := FView.VirtStyle;
      TriggerVirtualStyleNeeded(Item, ColumnNum, CurStyle);
    end;
  end;
{$endif}
  if Item.FStaticData <> nil then
  begin
    cNum := ColumnNum;
    if cNum > FMainTreeCol then dec(cNum);

    if Item.ColumnText.Count <= cNum then
      SText := ''
    else
      SText := Item.ColumnText[cNum];
  end
  else
    TriggerVirtualTextNeeded(Item, ColumnNum, SText);

  if (FODFollowCol and (HS.Style = ElhsOwnerDraw)) or ((not (FODFollowCol)) and (SText = FODMask))
{$ifdef ELTREE_USE_STYLES}
    or (((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) and (CurStyle.FStyle = elhsOwnerDraw))
{$endif}
  then
  begin
    TriggerMeasureItemPartEvent(Item, ColumnNum, Size);
    Inc(Size.X, (ItemExt div 5) * 3);
  end else
{$ifdef ELTREE_USE_STYLES}
  if Item.UseStyles and (CurStyle.Control <> nil) then
  begin
    Size.X := HS.Width;
    Size.Y := FLineHeight;
  end
  else
  if Item.UseStyles and (CurStyle.FStyle = elhsPictureOnly) and Assigned(CurStyle.FPicture) then
  begin
    Size.Y := CurStyle.FPicture.Height;
    Size.X := CurStyle.FPicture.Width;
  end
  else
  if (((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) and (CurStyle.FStyle = elhsText)) or ((Item.FBoolData1 and ibfUseStyles) <> ibfUseStyles) then
{$endif}
  begin
    SaveFontStyle := Canvas.Font.Style;
    SaveFontSize := Canvas.Font.Height;
    SaveFontName := Canvas.Font.Name;
{$ifdef ELTREE_USE_STYLES}
    if ((Item.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
    begin
      if ((Item.FBoolData1 and ibfParentStyle) <> ibfPArentStyle) then
      with CurStyle do
      begin
        if Canvas.Font.Name <> FontName then Canvas.Font.Name := FontName;
        if Canvas.Font.Style <> FontStyles then Canvas.Font.Style := FontStyles;
        if Canvas.Font.Size <> FontSize then Canvas.Font.Size := FontSize;
        OwnFontSet := true;
      end;
    end;
{$endif}
    {$ifndef CLX_USED}
    AL := DT_NOPREFIX or {$IFNDEF LITE}MultiLineFlags[Item.Multiline] or {$ELSE} DT_SINGLELINE or{$ENDIF}DT_VCENTER or DT_LEFT or DT_CALCRECT;
    if RightAlignedText then AL := AL or DT_RTLREADING;
    {$else}
    AL := MultiLineFlags[Item.Multiline] or Integer(AlignmentFlags_AlignLeft) or Integer(AlignmentFlags_AlignVCenter);
    {$endif}
    if HS.Password then
      SText := '******'
    else
    if Item.FStaticData <> nil then
    begin
      cNum := ColumnNum;
      if cNum > FMainTreeCol then dec(cNum);
      if Item.ColumnText.Count <= cNum then
        SText := ''
      else
        SText := Item.ColumnText[cNum];
    end
    else
      TriggerVirtualTextNeeded(Item, ColumnNum, SText);
      
    SetRectEmpty(R3);
{$IFDEF HAS_HTML_RENDER}
    if Item.IsHTML and (Copy(SText, 1, 6) = '<html>') then
    begin
      with FView.FRender do
      begin
        Data.DefaultStyle := Canvas.Font.Style;
        Data.DefaultFont  := Canvas.Font.Name;
        Data.DefaultHeight:= Canvas.Font.Height;
        Data.Charset      := Canvas.Font.Charset;

        PrepareText(SText, 0, false);
        R3.Right := Data.TextSize.cx;
        R3.Bottom := Data.TextSize.cy;
      end;
    end
    else
{$ENDIF}
    {$ifndef CLX_USED}
    {$ifdef ELPACK_UNICODE}
      ElVCLUtils.DrawTextW(Canvas.Handle, PWideChar(SText), Length(SText), R3, DT_NOPREFIX or AL);
    {$else}
      DrawText(Canvas.Handle, PChar(SText), Length(SText), R3, DT_NOPREFIX or AL);
    {$endif}
    {$else}
      Canvas.TextExtent(SText, R3, AL);
    {$endif}
    InflateRect(R3, 1, 1);

    Size.X := R3.Right - R3.Left + FDivLineWidth * 2 + ItemExt div 5 * 3;
    Size.Y := Max(FLineHeight, R3.Bottom - R3.Top + 1);

    if OwnFontSet then
    begin
      Canvas.Font.Style  := SaveFontStyle;
      Canvas.Font.Height := SaveFontSize;
      Canvas.Font.Name   := SaveFontName;
    end;
  end
{$ifdef ELTREE_USE_STYLES}
  else
  begin
    Size.X := 0;
    Size.Y := FLineHeight;
  end;
{$endif}
end;
{$warnings on}
{$hints on}
function TCustomElTree.MeasureColumnWidth(ColumnNum: integer; VisibleOnly : boolean): integer;

{type TMeasureProc = procedure(Item : TElTreeItem; ColumnNum : integer; var Size : TPoint) of object;

type TSRec = record
       MeasureProc : TMeasureProc;
       CurWidth,
       ColumnNum    : integer;
     end;
     PSRec = ^TSRec;

         procedure IntMeasure(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean; IterateData: pointer; Tree: TCustomElTree);
         var P : TPoint;
         begin
           PSRec(IterateData).MeasureProc(Item, PSRec(IterateData).ColumnNum, P);
           if P.X > PSRec(IterateData).CurWidth then PSRec(IterateData).CurWidth := P.x;
         end;

var SRec : TSRec;
}
var i, j : integer;
    Item : TElTreeItem;
    CurWidth : integer;
    P    : TPoint;

begin
  if (ColumnNum >= FHeader.Sections.Count) then
  begin
    result := 0;
    exit;
  end;

  CurWidth := 0;
  i := 0;
  j := FAllList.Count;
  while i < j do
  begin
    Item := TElTreeItem(FAllList[i]);

    if (FilteredVisibility and Item.Hidden and VisibleOnly) then
    begin
      i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end else
    begin
      MeasureCell(Item, ColumnNum, P);
      if P.X > CurWidth then CurWidth := P.x;
      if (not Item.Expanded) and VisibleOnly then
         i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end;
    inc(i);
  end;
  result := CurWidth;
  {SRec.MeasureProc := MeasureCell;
  SRec.ColumnNum := ColumnNum;
  Canvas.Font.Assign(Font);
  Items.Iterate(VisibleOnly, @IntMeasure, @SRec);
  result := SRec.CurWidth;}
end;

procedure TCustomElTree.HeaderSectionAutoSizeHandler(Sender : TCustomElHeader; Section : TElHeaderSection);
begin
  if FAutoResizeColumns then
  begin
    if Section.AutoSize then
       AutoSizeColumn(Section.Index)
    else
       Section.Width := Max(FHeader.MeasureSectionWidth(Section, nil, nil), MeasureColumnWidth(Section.Index, true));
    OnHeaderSectionResize(FHeader, Section);
  end else
  begin
    if Section.AutoSize then
       AutoSizeColumn(Section.Index)
    else
      SectionAutoSizeTransfer(Sender, Section);
  end;
end;  { HeaderSectionAutoSizeHandler }

procedure TCustomElTree.SectionAutoSizeTransfer(Sender : TCustomElHeader; Section : TElHeaderSection);
{ Transfers FHeader OnSectionAutoSize event to the outside world. }
begin
  if (assigned(FOnSectionAutoSize)) then
    FOnSectionAutoSize(Self, Section.Index);  { Substitute Self for subcomponent's Sender. }
end;  { SectionAutoSizeTransfer }

procedure TCustomElTree.SectionFilterCallTransfer(Sender : TCustomElHeader; Section : TElHeaderSection);
{ Transfers FHeader OnFilterCall event to the outside world. }
begin
  if (assigned(FOnSectionFilterCall)) then
    FOnSectionFilterCall(Self, Section.Index);  { Substitute Self for subcomponent's Sender. }
end;  { SectionFilterCallTransfer }

{ Exposed properties' Read/Write methods: }
procedure TCustomElTree.SetHeaderActiveFilterColor(newValue : TColor);
{ Sets the FHeader subcomponent's ActiveFilterColor property to newValue. }
begin
  FHeader.ActiveFilterColor := newValue;
end;  { SetHeaderActiveFilterColor }

function TCustomElTree.GetHeaderActiveFilterColor : TColor;
{ Returns the ActiveFilterColor property from the FHeader subcomponent. }
begin
  GetHeaderActiveFilterColor := FHeader.ActiveFilterColor;
end;  { GetHeaderActiveFilterColor }

procedure TCustomElTree.SetHeaderFilterColor(newValue : TColor);
{ Sets the FHeader subcomponent's FilterColor property to newValue. }
begin
  FHeader.FilterColor := newValue;
end;  { SetHeaderFilterColor }

function TCustomElTree.GetHeaderFilterColor : TColor;
{ Returns the FilterColor property from the FHeader subcomponent. }
begin
  GetHeaderFilterColor := FHeader.FilterColor;
end;  { GetHeaderFilterColor }

procedure TCustomElTree.SetHeaderFlat(newValue : Boolean);
{ Sets the FHeader subcomponent's Flat property to newValue. }
begin
  FHeader.Flat := newValue;
end;  { SetHeaderFlat }

function TCustomElTree.GetHeaderFlat : Boolean;
{ Returns the Flat property from the FHeader subcomponent. }
begin
  GetHeaderFlat := FHeader.Flat;
end;  { GetHeaderFlat }

{$ifndef CLX_USED}
procedure TCustomElTree.WMEnable(var Msg : TMessage);  { private }
begin
  inherited;
  if (Flat {$ifndef CLX_USED}or FUseCustomBars{$endif}) and (not IsThemeApplied) then DrawFlatBorder(false, false);
end;  { WMEnable }
{$endif}

procedure TCustomElTree.SetFlatFocusedScrollbars(newValue : Boolean);
{ Sets data member FFlatFocusedScrollbars to newValue. }
begin
  if (FFlatFocusedScrollbars <> newValue) then
  begin
    FFlatFocusedScrollbars := newValue;
    {$ifndef CLX_USED}
    if Focused and (not FUseCustomBars) and (not IsThemeApplied) then
      DrawFlatBorder(false, false);
    {$endif}
  end;  { if }
end;  { SetFlatFocusedScrollbars }

{$IFNDEF LITE}

procedure TCustomElTree.BackgroundChange(Sender : TObject);
begin
  if (Background.Empty) then FBackgroundType := bgtColorFill else
  begin
    with FView do
    begin
      RedoTmpBmp;
      if BackgroundType <> bgtColorFill then
        if not (csLoading in ComponentState) then Invalidate;//Repaint;
    end;
  end;
end;

procedure TCustomElTree.SetBackground(newValue : TBitmap);
{ Sets data member FBackground to newValue. }
begin
  FBackground.Assign(newValue);
end;  { SetBackground }

procedure TCustomElTree.SetBackgroundType(newValue : TElBkGndType);
{ Sets data member FBackgroundType to newValue. }
begin
  if (FBackgroundType <> newValue) then
  begin
    if (Background.Empty) and (FBackGroundType in [bgtTileBitmap, bgtStretchBitmap, bgtCenterBitmap])
       then FBackgroundType := bgtColorFill
       else FBackgroundType := newValue;
    with FView do
    begin
      RedoTmpBmp;
      if not (csLoading in Self.ComponentState) then Invalidate;//Repaint;
    end;
  end;  { if }
end;  { SetBackgroundType }
{$ENDIF}

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TCustomElTree.TriggerTuneUpInplaceEditEvent(Item : TElTreeItem; SectionIndex : integer; Editor : TCustomEdit);
begin
  if (assigned(FOnTuneUpInplaceEdit)) then
    FOnTuneUpInplaceEdit(Self, Item , SectionIndex, Editor);
end;  { TriggerTuneUpInplaceEditEvent }
{$endif}
{$endif}

procedure TCustomElTree.SetHideSelectTextColor(newValue: TColor);
begin
  if (FHideSelectTextColor <> newValue) then
  begin
    FHideSelectTextColor := newValue;
    if ({((GetParentForm(self) = nil) or (GetParentForm(self).ActiveControl <> self))}FView.FHasFocus and FHideSelect) then FView.Invalidate;//Repaint;
  end;  {if}
end;

procedure TCustomElTree.SetFocusedSelectTextColor(newValue: TColor);
begin
  if (FFocusedSelectTextColor <> newValue) then
  begin
    FFocusedSelectTextColor := newValue;
    if ({((GetParentForm(self) <> nil) and (GetParentForm(self).ActiveControl = self))}FView.FHasFocus or (not FHideSelect)) then FView.Invalidate;//Repaint;
  end;  {if}
end;

procedure TCustomElTree.SetHideSelectColor(newValue: TColor);
begin
  if (FHideSelectColor <> newValue) then
  begin
    FHideSelectColor := newValue;
    if ({((GetParentForm(self) = nil) or (GetParentForm(self).ActiveControl <> self))}FView.FHasFocus and FHideSelect) then FView.Invalidate;//Repaint;
  end;  {if}
end;

procedure TCustomElTree.SetFocusedSelectColor(newValue: TColor);
begin
  if (FFocusedSelectColor <> newValue) then
  begin
    FFocusedSelectColor := newValue;
    if ({((GetParentForm(self) <> nil) and (GetParentForm(self).ActiveControl = self))}FView.FHasFocus or (not FHideSelect)) then FView.Invalidate;//Repaint;
  end;  {if}
end;
{$IFNDEF LITE}
procedure TCustomElTree.SetNoBlendSelected(newValue : Boolean);
{ Sets data member FNoBlendSelected to newValue. }
begin
  if (FNoBlendSelected <> newValue) then
  begin
    FNoBlendSelected := newValue;
    if BackGroundType <> bgtColorFill then FView.Invalidate;//Repaint;
  end;  { if }
end;  { SetNoBlendSelected }
{$ENDIF}
procedure TCustomElTree.SetRowHotTrack(newValue : Boolean);
{ Sets data member FRowHotTrack to newValue. }
begin
  if (FRowHotTrack <> newValue) then
  begin
    FRowHotTrack := newValue;
    with FView do
      if Tracking and (FTrackItem <> nil) then FTrackItem.RedrawItem(true);
  end;  { if }
end;  { SetRowHotTrack }

procedure TCustomElTree.SetActiveBorderType(newValue : TElFlatBorderType);
{ Sets data member FActiveBorderStyle to newValue. }
begin
  if (FActiveBorderType <> newValue) then
  begin
    FActiveBorderType := newValue;
    {$ifndef CLX_USED}
    if (FTreeIsFocused or FMouseOver) {$ifndef CLX_USED}or FUseCustomBars{$endif} then UpdateFrame;
    {$endif}
  end;  { if }
end;  { SetActiveBorderStyle }

procedure TCustomElTree.SetInactiveBorderType(newValue : TElFlatBorderType);
{ Sets data member FInactiveBorderStyle to newValue. }
begin
  if (FInactiveBorderType <> newValue) then
  begin
    FInactiveBorderType := newValue;
    {$ifndef CLX_USED}
    if not (FTreeIsFocused or FMouseOver) {$ifndef CLX_USED}or FUseCustomBars{$endif} then UpdateFrame;
    {$endif}
  end;  { if }
end;  { SetInactiveBorderStyle }

{$IFNDEF LITE}
procedure TCustomElTree.SetGradientStartColor(newValue : TColor);
{ Sets data member FGradientStartColor to newValue. }
begin
  if (FGradientStartColor <> newValue) then
  begin
    FGradientStartColor := newValue;
    if (BackgroundType in [bgtHorzGradient, bgtVertGradient]) and (FUpdateCount = 0) then
    with FView do
    begin
      RedoTmpBmp;
      Invalidate;//Repaint;
    end;
  end;  { if }
end;  { SetGradientStartColor }

procedure TCustomElTree.SetGradientEndColor(newValue : TColor);
{ Sets data member FGradientEndColor to newValue. }
begin
  if (FGradientEndColor <> newValue) then
  begin
    FGradientEndColor := newValue;
    if (BackgroundType in [bgtHorzGradient, bgtVertGradient]) and (FUpdateCount = 0) then
    with FView do
    begin
      RedoTmpBmp;
      Invalidate;//Repaint;
    end;
  end;  { if }
end;  { SetGradientEndColor }

procedure TCustomElTree.SetGradientSteps(newValue : Integer);
{ Sets data member FGradientSteps to newValue. }
begin
  if (FGradientSteps <> newValue) and (newValue > 0) then
  begin
    FGradientSteps := newValue;
    if (BackgroundType in [bgtHorzGradient, bgtVertGradient]) and (FUpdateCount = 0) then
    with FView do
    begin
      RedoTmpBmp;
      Invalidate;//Repaint;
    end;
  end;  { if }
end;  { SetGradientSteps }
{$ENDIF LITE}

function TCustomElTree.GetHeaderImages: TImageList;
{ Returns the value of data member FHeaderImages. }
begin
  result := FHeader.Images;
end; { GetHeaderImages }

procedure TCustomElTree.SetHeaderImages(newValue: TImageList);
{ Sets data member FHeaderImages to newValue. }
begin
  if (FHeader.Images <> newValue) then FHeader.Images := newValue;
end; { SetHeaderImages }

procedure TCustomElTree.TriggerScrollEvent(ScrollBarKind: TScrollBarKind; ScrollCode: integer);
begin
  if (assigned(FOnScroll)) then
    FOnScroll(Self, ScrollBarKind, ScrollCode);
end; { TriggerScrollEvent }

procedure TCustomElTree.DeSelectAllEx; { public }

{  procedure IntSelAll(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
  begin
    if (Item.FIState and tisSelected) > 0 then
    begin
      Item.Selected := false;
      if Tree.FSelectedList.Count = 0 then ContinueIterate := false;
    end;
  end;
}
var i : integer;
    TI : TElTreeItem;
begin
  if FMultiSelect = false then exit;
  if not IncludeHidden then DeselectAll else
  begin
    IsUpdating := true;
    for i := 0 to Pred(FSelectedList.Count) do    // Iterate
    begin
      TI := TElTreeItem(FSelectedList[i]);
      FSelChange := true;
      Exclude(TI.FState, stsSelected);
      TI.FIState := TI.FIState and (not tisSelected);
      if FView.FSelected = TI then FView.FSelected := nil;
      TriggerItemSelectedChangeEvent(TI);
      TI.UpdateItem;
    end;    // for
    FSelectedList.Count := 0;
    IsUpdating := false;
  end;
{  exit;

  IsUpdating := true;
  FItems.Iterate(not IncludeHidden, @IntSelAll, nil);
  IsUpdating := false;
}
end; { DeSelectAll }

procedure TCustomElTree.DeSelectAll; { public }

{  procedure IntSelAll(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
  begin
    if (Item.FIState and tisSelected) > 0 then
    begin
      Item.Selected := false;
      if Tree.FSelectedList.Count = 0 then ContinueIterate := false;
    end;
  end;
}

var i  : integer;
    TI : TElTreeItem;
begin
  if FMultiSelect = false then exit;
  IsUpdating := true;
  i := 0;
  while i < FSelectedList.Count do    // Iterate
  begin
    TI := TElTreeItem(FSelectedList[i]);
    if not (FilteredVisibility and TI.Hidden) then
    begin
      FSelChange := true;
      Exclude(TI.FState, stsSelected);
      TI.FIState := TI.FIState and (not tisSelected);
      FSelectedList.Remove(TI);
      if FView.FSelected = TI then FView.FSelected := nil;
      TriggerItemSelectedChangeEvent(TI);
      TI.UpdateItem;
    end else inc(i);
  end;    // for
  //FItems.Iterate(FilteredVisibility, @IntSelAll, nil);
  IsUpdating := false;
end; { DeSelectAll }

procedure TCustomElTree.SelectAllEx; { public }

{  procedure IntSelAll(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
  begin
    if (Item.FIState and tisSelected) = 0 then Item.Selected := true;
  end;
}

var i, j : integer;
    Item : TElTreeItem;

begin
  if FMultiSelect = false then exit;
  i := 0;
  j := FAllList.Count;
  IsUpdating := true;
  while i < j do
  begin
    Item := TElTreeItem(FAllList[i]);

    if (FilteredVisibility and (not IncludeHidden) and Item.Hidden) then
    begin
      i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end else
    begin
      if (Item.FIState and tisSelected) = 0 then Item.Selected := true;
      if (not Item.Expanded) and (not IncludeHidden) then
         i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end;
    inc(i);
  end;
  IsUpdating := false;

  {if FMultiSelect = false then exit;
  IsUpdating := true;
  FItems.Iterate(not IncludeHidden, @IntSelAll, nil);
  IsUpdating := false;}
end; { SelectAll }

procedure TCustomElTree.SelectAll; { public }

{  procedure IntSelAll(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
  begin
    if (Item.FIState and tisSelected) = 0 then Item.Selected := true;
  end;
}

var i, j : integer;
    Item : TElTreeItem;

begin
  if FMultiSelect = false then exit;
  i := 0;
  j := FAllList.Count;
  IsUpdating := true;
  while i < j do
  begin
    Item := TElTreeItem(FAllList[i]);

    if (FilteredVisibility and Item.Hidden) then
    begin
      i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end else
    begin
      if (Item.FIState and tisSelected) = 0 then Item.Selected := true;
      if (not Item.Expanded) and (FilteredVisibility) then
         i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end;
    inc(i);
  end;
  IsUpdating := false;

  {if FMultiSelect = false then exit;
  IsUpdating := true;
  FItems.Iterate(FilteredVisibility, @IntSelAll, nil);
  IsUpdating := false;}
end; { SelectAll }

procedure TCustomElTree.InvertSelectionEx; { public }

{  procedure IntInvSel(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
  begin
    Item.Selected := not Item.Selected;
  end;
}
var i, j : integer;
    Item : TElTreeItem;

begin
  if FMultiSelect = false then exit;
  i := 0;
  j := FAllList.Count;
  IsUpdating := true;
  while i < j do
  begin
    Item := TElTreeItem(FAllList[i]);

    if (FilteredVisibility and (not IncludeHidden) and Item.Hidden) then
    begin
      i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end else
    begin
      Item.Selected := not Item.Selected;
      if (not Item.Expanded) and (not IncludeHidden) then
         i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end;
    inc(i);
  end;
  IsUpdating := false;

  {if FMultiSelect = false then exit;
  IsUpdating := true;
  FItems.Iterate(not IncludeHidden, @IntInvSel, nil);
  IsUpdating := false;}
end;

procedure TCustomElTree.InvertSelection; { public }

{  procedure IntInvSel(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
  begin
    Item.Selected := not Item.Selected;
  end;
}

var i, j : integer;
    Item : TElTreeItem;

begin
  if FMultiSelect = false then exit;
  i := 0;
  j := FAllList.Count;
  IsUpdating := true;
  while i < j do
  begin
    Item := TElTreeItem(FAllList[i]);

    if (FilteredVisibility and Item.Hidden) then
    begin
      i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end else
    begin
      Item.Selected := not Item.Selected;
      if (not Item.Expanded) and (FilteredVisibility) then
         i := FAllList.IndexOfFrom(i, Item.GetLastSubItem);
    end;
    inc(i);
  end;
  IsUpdating := false;

{
  if FMultiSelect = false then exit;
  IsUpdating := true;
  FItems.Iterate(FilteredVisibility, @IntInvSel, nil);
  IsUpdating := false;
}  
end;

function TCustomElTree.GetFocused: TElTreeItem;
begin
  result := FView.FFocused;
end;

function TCustomElTree.GetSelected: TElTreeItem;
begin
  result := FView.FSelected;
end; { GetSelected }  

procedure TCustomElTree.SetSelected(newValue: TElTreeItem);
begin
  if not FMultiSelect then
     SetFocused(newValue)
  else
    if FView <> nil then
       FView.FSelected := newValue;
end; { SetSelected }

function TCustomElTree.GetTopItem: TElTreeItem; { public }
begin
  with FView do
    if FVisible.Count = 0 then
      result := nil
    else
      Result := FVisible[0];
end; { GetTopItem }

procedure TCustomElTree.SetTopItem(Item: TElTreeItem); { public }
begin
  if Item = nil then raise EElTreeError.Create(STexInvItem);
  TopIndex := Item.VisIndex;
end; { SetTopItem }

procedure TCustomElTree.TriggerHeaderColumnMoveEvent;
begin
  if (assigned(FOnHeaderColumnMove)) then
    FOnHeaderColumnMove(Self, Section, OldPos, NewPos);
end; { TriggerHeaderColumnMoveEvent }

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TCustomElTree.TriggerTryEditEvent(Item: TElTreeItem; Section:
  TElHeaderSection; var CellType: TElFieldType; var CanEdit: boolean);
begin
  if (assigned(FOnTryEdit)) and (not (csDestroying in ComponentState)) then
    FOnTryEdit(Self, Item, Section, CellType, CanEdit);
end;
{$endif}
{$else}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TCustomElTree.TriggerTryEditEvent(Item: TElTreeItem; SectionIndex : integer;
  var CellType: TElFieldType; var CanEdit: boolean);
begin
  if (assigned(FOnTryEdit)) and (not (csDestroying in ComponentState)) then
    FOnTryEdit(Self, Item, SectionIndex, CellType, CanEdit);
end;
{$endif}
{$endif}

procedure TCustomElTree.TriggerItemSaveEvent(Stream: TStream; Item: TElTreeItem);
begin
  if (assigned(FOnItemSave)) then FOnItemSave(Self, Stream, Item);
end;

procedure TCustomElTree.TriggerItemLoadEvent(Stream: TStream; Item:
  TElTreeItem);
begin
  if (assigned(FOnItemLoad)) then FOnItemLoad(Self, Stream, Item);
end;

{$ifdef SUPPORT_STORAGE}
procedure TCustomElTree.Save;
var
  FSaveKey: string;
  F: TForm;
  S: string;
  AKey: string;
begin
  if Assigned(FStorage) then
  begin
    FSaveKey := FStorage.CurrentKey; 
    F := GetOwnerForm(Self);
    if (F <> nil) and (F.Name <> '') then
      S := F.Name + FStorage.Delimiter
    else
      S := '';
    AKey := FStorage.Delimiter + S + FStoragePath;
    FHeader.Storage := FStorage;
    FHeader.StoragePath := AKey;
    FHeader.Save;
    if FStorage.OpenKey(AKey, true) then
    begin
      FStorage.WriteInteger('', 'FontSize', Font.Size);
      FStorage.WriteInteger('', 'FontColor', Font.Color);
      FStorage.WriteString('', 'FontName', Font.Name);
      FStorage.WriteBool('', 'FontBold', fsBold in Font.Style);
      FStorage.WriteBool('', 'FontItalic', fsItalic in Font.Style);
      FStorage.WriteBool('', 'FontUnderline', fsUnderline in Font.Style);
      FStorage.WriteBool('', 'FontStrikeout', fsStrikeout in Font.Style);
      FStorage.WriteInteger('', 'SortType', integer(FSortType));
      FStorage.WriteInteger('', 'SortMode', integer(FSortMode));
      FStorage.WriteInteger('', 'SortDir', integer(FSortDir));
      FStorage.WriteInteger('', 'SortSection', integer(FSortSection));
    end;
    FStorage.OpenKey(FSaveKey, false);
  end;
end; {Save}

procedure TCustomElTree.Restore;
var
  FSaveKey: string;
  i: integer;
  b: Boolean;
  s: string;
  F: TForm;
  AKey: string;
begin
  IsUpdating := true;
  if Assigned(FStorage) then
  begin
    F := GetOwnerForm(Self);
    if (F <> nil) and (F.Name <> '') then S := F.Name + FStorage.Delimiter else S := '';
    AKey := FStorage.Delimiter + S + FStoragePath;
    FHeader.Storage := FStorage;
    FHeader.StoragePath := AKey;
    FHeader.Restore;
    FSaveKey := FStorage.CurrentKey;
    if FStorage.OpenKey(AKey, false) then
    begin
      FStorage.ReadInteger('', 'FontSize', Font.Size, i);
      Font.Size := i;
      FStorage.ReadInteger('', 'FontColor', Font.Color, i);
      Font.Color := i;
      FStorage.ReadString('', 'FontName', Font.Name, s);
      Font.Name := s;
      FStorage.ReadBool('', 'FontBold', fsBold in Font.Style, b);
      if b then Font.Style := Font.Style + [fsBold] else Font.Style := Font.Style - [fsBold];
      FStorage.ReadBool('', 'FontItalic', fsItalic in Font.Style, b);
      if b then Font.Style := Font.Style + [fsItalic] else Font.Style := Font.Style - [fsItalic];
      FStorage.ReadBool('', 'FontUnderline', fsUnderline in Font.Style, b);
      if b then Font.Style := Font.Style + [fsUnderline] else Font.Style := Font.Style - [fsUnderline];
      FStorage.ReadBool('', 'FontStrikeout', fsStrikeout in Font.Style, b);
      if b then Font.Style := Font.Style + [fsStrikeout] else Font.Style := Font.Style - [fsStrikeout];
      FStorage.ReadInteger('', 'SortType', integer(FSortType), i);
      FSortType := TSortTypes(i);
      FStorage.ReadInteger('', 'SortMode', integer(FSortMode), i);
      FSortMode := TSortModes(i);
      FStorage.ReadInteger('', 'SortDir', integer(FSortDir), i);
      FSortDir := TSortDirs(i);
      FStorage.ReadInteger('', 'SortSection', FSortSection, i);
      FSortSection := i;
    end;
    FStorage.OpenKey(FSaveKey, false);
  end;
  IsUpdating := false;
end; {Restore}
{$ENDIF}

procedure TCustomElTree.TriggerItemSelectedChangeEvent(Item: TElTreeItem);
begin
  if (assigned(FOnItemSelectedChange)) and (not (csDestroying in ComponentState)) then
    FOnItemSelectedChange(Self, Item);
end; {TriggerItemSelectedChangeEvent}

{$ifndef CLX_USED}
procedure TCustomElTree.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  //inherited;
  Message.Result := 1;
end;
{$endif}

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TCustomElTree.EndEdit(ByCancel: boolean);
begin
  FView.DoEndEdit(ByCancel);
end;

procedure TCustomElTree.EditItem;
begin
  if not FCanEdit then exit;
  try
    with FView do
      if FInpEdit <> nil then EndEdit(true);
  except
  end;
  FView.DoEditItem(Item, SectionNum);
end;
{$endif}
{$endif}

procedure TCustomElTree.SetShowHeader;
begin
  if (FHeader = nil) then exit;
  if not (csReading in ComponentState) and (Value) and (FHeader.Sections.Count = 0) then exit;
  FShowHeader := value;
  if value = false then FView.FHRange := -1;
  //if csReading in ComponentState then exit;
  IsUpdating := true;
  FUpdated := true;

  with FView do
  begin
    FVisUpdated := true;
    FClearAll := true;
    FClearVis := true;
  end;
{$IFNDEF LITE}
  if not (BackgroundType in [bgtColorFill, bgtCenterBitmap]) then FView.RedoTmpBmp;
{$ENDIF}
  AlignPieces;
  IsUpdating := false;
end;

procedure TCustomElTree.SetMainTreeCol;
begin
  if value = FMainTreeCol then exit;
  if value >= FHeader.Sections.Count then FMainTreeCol := 0 else FMainTreeCol := value;
  IsUpdating := true;
  FView.FRangeUpdate := true;
  FUpdated := true;
  IsUpdating := false;
end;

procedure TCustomElTree.DoHeaderResize;
begin
  if FView.FPainting or FIgnoreResizes then exit;
  IsUpdating := true;
  FUpdated := true;
  with FView do
  begin
    FRangeUpdate := true;
    FClearVis := true;
  end;
  if SavedHH <> FHeader.Height then
  begin
    with FView do
    begin
      FClearAll := true;
      FVisUpdated:= true;
    end;
    AlignPieces;
  end;
  if (FHeader.LeftPos <> FHPos) {or (FHeader.Width <> FHPos + FView.Width)} then
  begin
    FHeader.LeftPos := FHPos;
    {//LockedColumn, update
    FHeader.Left := -FHPos;
    FHeader.Width := FHPos + FView.Width;}
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.DoHeaderDraw;
begin
  if assigned(FOnColumnDraw) then FOnColumnDraw(Header, Canvas, Section, Rect, Pressed);
end;

{$ifndef CLX_USED}
procedure TCustomElTree.CMFontChanged(var Message: TMessage);
{$else}
procedure TCustomElTree.FontChanged;
{$endif}
begin
  inherited;
  if Canvas <> nil then Canvas.Font.Assign(Font);
  if (FHeader <> nil) and (FHeaderUseTreeFont) then
  begin
    FHeaderFont.Assign(Font);
    FHeader.Font.Assign(Font);
    TElHeaderHack(FHeader).OnFontChange(FHeader.Font);
  end;

  //FView.Font.Assign(Font);

  //if FHScrollBar <> nil then THackScrollBar(FHScrollBar).Font.Assign(Font);
  //if FVScrollBar <> nil then THackScrollBar(FVScrollBar).Font.Assign(Font);

  IsUpdating := true;
  FView.FRangeUpdate := true;
  FUpdated := true;
  IsUpdating := false;
end;

{$ifndef CLX_USED}
procedure TCustomElTree.CMColorChanged(var Message: TMessage);
{$else}
procedure TCustomElTree.ColorChanged;
{$endif}
begin
  inherited;
  if Canvas <> nil then Canvas.Brush.Color := Color;
end;

{$ifndef CLX_USED}
{$endif}

procedure TCustomElTree.SetHeaderHeight(value: integer);
begin
  IsUpdating := true;
  if csLoading in ComponentState then FHeaderHeight := value else
  begin
    FHeader.Height := value;
    AlignPieces;
    {$IFNDEF LITE}
    if not (BackgroundType in [bgtColorFill, bgtCenterBitmap]) then FView.RedoTmpBmp;
    {$ENDIF}
  end;
  with FView do
  begin
    FClearAll := true;
    FClearVis := true;
  end;
  FUpdated := true;
  IsUpdating := false;
end;

function TCustomElTree.GetHeaderHeight;
begin
  Result := FHeader.Height;
end;

procedure TCustomElTree.SetHeaderSections;
begin
  IsUpdating := true;
  FHeader.Sections := value;
  FUpdated := true;
  FView.FRangeUpdate := true;
  IsUpdating := false;
end;

function TCustomElTree.GetHeaderSections;
begin
  Result := FHeader.Sections;
end;

procedure TCustomElTree.SaveStringsToStream(Stream: TStream);

  procedure IntSaveString(Item: TElTreeItem);
  var
    NodeStr: string;
  begin
    NodeStr := StringOfChar(' ', Pred(Item.Level)) + Item.Text + #13#10;
    Stream.Write(Pointer(NodeStr)^, Length(NodeStr));
  end;

var i : integer;
    Item : TElTreeItem;

begin
  for i := 0 to Pred(FAllList.Count) do
  begin
    Item := TElTreeItem(FAllList[i]);
    IntSaveString(Item);
  end;
end;

function TCustomElTree.GetItemAtY(y: integer): TElTreeItem;
begin
  Result := FView.GetItemAtY(y - FView.Top);
end;

function TCustomElTree.Focused : boolean;
begin
  result := FTreeIsFocused;
  exit;
  (*
{$ifndef CLX_USED}
  result := (HandleAllocated and (GetFocus = Handle)) or
            (FView.HandleAllocated and (GetFocus = FView.Handle));
{$else}
  result := inherited Focused or (FView.HandleAllocated and FView.Focused);
{$endif}
*)
end;

procedure TCustomElTree.FullCollapse;
var
  i: integer;
begin
  if FItems.FRoot.FChildren = nil then exit;
  IsUpdating := true;
  try
    for i := 0 to FItems.FRoot.FChildren.Count - 1 do
      TElTreeItem(THackElList(FItems.FRoot.FChildren).FList[i]).Collapse(true);
  finally
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.FullExpand;
var
  i: integer;
begin
  if FItems.FRoot.FChildren = nil then exit;
  IsUpdating := true;
  try
    for i := 0 to FItems.FRoot.FChildren.Count - 1 do
      TElTreeItem(THackElList(FItems.FRoot.FChildren).FList[i]).Expand(true);
  finally
    IsUpdating := false;
  end;
end;

{$ifndef CLX_USED}
{$IFDEF ELPACK_COMPLETE}
procedure TCustomElTree.ImageFormChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomElTree.SetImageForm(newValue : TElImageForm);
begin
  if FImgForm <> newValue then
  begin
    if FImgForm <> nil then
    begin
      {$ifdef VCL_5_USED}
      FImgForm.RemoveFreeNotification(Self);
      {$endif}
      FImgForm.UnregisterChanges(FImgFormChLink);
    end;
    FImgForm := newValue;
    if FImgForm <> nil then
    begin
      FImgForm.RegisterChanges(FImgFormChLink);
      FImgForm.FreeNotification(Self);
    end;
    Invalidate;
  end;
end;
{$ENDIF}
{$endif}
procedure TCustomElTree.SetTextColor(value: TColor);
begin
  if FTextColor = value then exit;
  FTextColor := value;
  IsUpdating := true;
  FUpdated := true;
  with FView do
  begin
    FVisUpdated := true;
    FClearVis := true;
    FClearAll := true;
  end;
  IsUpdating := false;
end;

procedure TCustomElTree.SetBkColor(value: TColor);
begin
  if FBkColor = value then exit;
  FBkColor := value;
  IsUpdating := true;
  FUpdated := true;
  with FView do
  begin
    FVisUpdated := true;
    FClearVis := true;
    FClearAll := true;
  end;
  IsUpdating := false;
end;                           

procedure TCustomElTree.SetItems(value: TElTreeItems);
begin
  FItems.Assign(value);
end;

function TCustomElTree.CreateItemsExt(ItemClass : TElTreeItemClass): TElTreeItems;
begin
  result := TElTreeItems.CreateClass(Self, ItemClass);
end;

function TCustomElTree.CreateItems;
begin
  result := TElTreeItems.Create(Self);
end;

procedure TCustomElTree.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
{$ifdef SUPPORT_STORAGE}
    if AComponent = FStorage then FStorage := nil;
{$endif}
{$ifndef CLX_USED}
{$IFDEF ELPACK_COMPLETE}
    if AComponent = FImgForm then
    begin
      ImageForm := nil;
      Invalidate;
    end;
{$ENDIF}
{$endif}
    if AComponent = Images then
    begin
      Images.UnRegisterChanges(FImageChangeLink);
      Images := nil;
    end;
    if AComponent = Images2 then
    begin
      Images2.UnRegisterChanges(FImageChangeLink);
      Images2 := nil;
    end;
  end;
end;

procedure TCustomElTree.SelectRangeEx2(FromItem, ToItem: TElTreeItem; IncludeHidden, SelectDisabled: boolean);
var i,
    EndIdx,
    StartIdx : integer;
    Item,
    LItem : TElTreeItem;
begin
  if FMultiSelect = false then exit;
  if (FromItem = ToItem) and (FromItem <> nil) then
     exit;
  if (not FilteredVisibility) or IncludeHidden then
     SelectRange2(FromItem, ToItem, SelectDisabled)
  else
  begin
    IsUpdating := true;
    try
      if FromItem = nil then
         StartIdx := 0
      else
         StartIdx := FromItem.AbsoluteIndex;

      if ToItem = nil then
         EndIdx := 0
      else
         EndIdx := ToItem.AbsoluteIndex;

      if StartIdx > EndIdx then
      begin
        i := EndIdx;
        EndIdx := StartIdx;
        StartIdx := i;
      end;
      i := StartIdx;
      while i < EndIdx do
      begin
        Item := TElTreeItem(FAllList[i]);
        if Item.Hidden then
        begin
          LItem := Item.GetLastSubItem;
          if LItem <> nil then
             i := FAllList.IndexOfFrom(i, LItem);
        end
        else
        begin
          if SelectDisabled or (IgnoreEnabled or TElTreeItem(FAllList[i]).Enabled) then
             TElTreeItem(FAllList[i]).Selected := true;
          if not Item.Expanded then
          begin
            LItem := Item.GetLastSubItem;
            if LItem <> nil then
              i := FAllList.IndexOfFrom(i, LItem);
          end;
        end;
        inc(i);
      end;
    finally
      IsUpdating := false;
    end;
  end;
end;

procedure TCustomElTree.SelectRangeEx(FromItem, ToItem: TElTreeItem; IncludeHidden: boolean);
begin
  SelectRangeEx2(FromItem, ToItem, IncludeHidden, true);
end;

procedure TCustomElTree.SelectRange2(FromItem, ToItem: TElTreeItem; SelectDisabled : boolean);
var i, EndIdx,
    StartIdx : integer;
begin
  if FMultiSelect = false then exit;
  if (FromItem = ToItem) and (FromItem <> nil) then exit;
  IsUpdating := true;
  try
    if FromItem = nil then
       StartIdx := 0
    else
       StartIdx := FromItem.AbsoluteIndex;

    if ToItem = nil then
       EndIdx := 0
    else
       EndIdx := ToItem.AbsoluteIndex;

    (*
    if StartIdx > EndIdx then
    begin
      i := EndIdx;
      EndIdx := StartIdx;
      StartIdx := i;
    end;
    *)
    if StartIdx > EndIdx then
    begin
      for i := StartIdx downto EndIdx do
        if SelectDisabled or (IgnoreEnabled or TElTreeItem(FAllList[i]).Enabled) then
           TElTreeItem(FAllList[i]).Selected := true
    end
    else
    begin
      for i := StartIdx to EndIdx do
        if SelectDisabled or (IgnoreEnabled or TElTreeItem(FAllList[i]).Enabled) then
           TElTreeItem(FAllList[i]).Selected := true;
    end;
  finally
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.SelectRange(FromItem, ToItem: TElTreeItem);
begin
  SelectRange2(FromItem, ToItem, true);
end;

procedure TCustomElTree.SetUseStdBars(value : boolean);
begin
  if Value <> FUseCustomBars then
  begin
    FUseCustomBars := Value;
    {$ifndef CLX_USED}
    RecreateWnd;
    {$endif}
  end;
end;

procedure TCustomElTree.SetChStateImage(value: boolean);
begin
  if value = FChStateImage then exit;
  IsUpdating := true;
  FChStateImage := value;
  FView.FRangeUpdate := true;
  FUpdated := true;
  IsUpdating := false;
end;

procedure TCustomElTree.SetHideSelect(value: boolean);
begin
  if value = FHideSelect then exit;
  FHideSelect := value;
  if (not Focused) and (HandleAllocated) then
  begin
    IsUpdating := true;
    FView.FRangeUpdate := true;
    FUpdated := true;
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.Assign(Source: TPersistent);
var
  T: TCustomElTree;
{$ifndef CLX_USED}
{$IFNDEF VER90}
  Form: TCustomForm;
{$ELSE}
  Form: TForm;
{$ENDIF}
{$endif}

begin
  if Source is TCustomElTree then
  begin
    //inherited;
    IsUpdating := true;
    T := TCustomElTree(Source);
    FItems.Assign(T.FItems);
    FLinesStyle := T.FLinesStyle;
    FLinesColor := T.LinesColor;
    FDeselectChildrenOnCollapse := T.FDeselectChildrenOnCollapse;
    FDrawFocusRect := T.FDrawFocusRect;
    FBarStyle := T.FBarStyle;
    FAlwaysKeepSelection := T.FAlwaysKeepSelection;
    FFullRowSelect := T.FFullRowSelect;
    FDragType := T.FDragType;
    FAutoLookup := T.FAutoLookup;
    FSelectColumn := T.FSelectColumn;
    FAutoExpand := AutoExpand;
    PlusPicture.Assign(T.PlusPicture);
    FMinusPicture.Assign(T.FMinusPicture);
    FLeafPicture.Assign(T.FLeafPicture);
    FCustomPlusMinus := T.FCustomPlusMinus;
    FShowCheckboxes := T.FShowCheckboxes;
    FShowLeafButton := T.FShowLeafButton; 
    FHideHorzScrollBar := T.FHideHorzScrollBar;
    FHideVertScrollBar := T.FHideVertScrollBar;

    FBkColor := T.FBkColor;
    FTextColor := T.FTextColor;
    FShowButtons := T.FShowButtons;
    FShowLines := T.FShowLines;
    FShowImages := T.FShowImages;
    FShowRoot := T.FShowRoot;
    FShowHintMode := T.FShowHintMode;
    ShowColumns := T.ShowColumns;
    FBorderStyle := T.FBorderStyle;
    FCanEdit := T.FCanEdit;
    FHLines := T.FHLines;
    FVLines := T.FVLines;
    FScrollTracking := T.FScrollTracking;
    FTracking := T.FTracking;
    HeaderHotTrack := T.HeaderHotTrack;
    FODFollowCol := T.FODFollowCol;
    FODMask := T.FODMask;
    FDragTrgDrawMode := T.FDragTrgDrawMode;
    TopIndex := T.TopIndex;
    FAutoLineHeight := T.FAutoLineHeight;
    FLineHeight := T.FLineHeight;
{$IFDEF ELPACK_COMPLETE}
    FStorage := T.FStorage;
    FStoragePath := T.FStoragePath;
{$ENDIF}
    FUpdated := true;
    with FView do
    begin
      FHRange := -1;
      FVisUpdated := true;
      FClearVis := true;
      FClearAll := true;
    end;
    IsUpdating := false;
    {$ifndef CLX_USED}
    if csDesigning in ComponentState then
    begin
      Form := GetParentForm(self);
      if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
    end;
    {$endif}
  end
  else
    inherited;
end;

procedure TCustomElTree.Sort;
begin
  TriggerSortBegin;
  inc(FInSorting);
  FItems.FRoot.Sort(recursive);
  dec(FInSorting);
  TriggerSortEnd;
end;

{$ifndef CLX_USED}
procedure TCustomElTree.CMMouseEnter;
begin
  inherited;
  FMouseOver := true;
  if Flat and (not FTreeIsFocused) and
     (FInactiveBorderType <> FActiveBorderType) and (not IsThemeApplied) then
       DrawFlatBorder(false, false);
end;

procedure TCustomElTree.CMMouseLeave;
begin
  FMouseOver := false;
  if Flat and (not FTreeIsFocused) and
     (FInactiveBorderType <> FActiveBorderType)  and (not IsThemeApplied) then
       DrawFlatBorder(false, false);
  inherited;
end;
{$endif}

procedure TCustomElTree.OnHeaderSectionDelete;
begin
  if FHeader.Sections.Count = 0 then
    ShowColumns := false;
  IsUpdating := true;
  with FView do
    if not FShowHeader then
      FHRange := -1
    else
      FClearVis := true;
  AlignPieces;
  if FSortSection >= FHeader.Sections.Count then
    FSortSection := -1;
  if (FSortSections <> nil) then
  begin
    if (Section = nil) or (FSortSections.IndexOf(Section) >= 0) then
    begin
      if (Section <> nil) then
        FSortSections.Remove(Section);
      if SortMode in [smClick, smAddClick] then
        FSortRequired := true;
    end;
  end;
  FUpdated := true;
  IsUpdating := false;
end;

function TCustomElTree.DoGetPicture(Item: TElTreeItem): integer;
begin
  result := Item.ImageIndex;
  if Assigned(FOnItemPicDraw) then FOnItemPicDraw(self, Item, result);
end;

function TCustomElTree.DoGetPicture2(Item: TElTreeItem): integer;
begin
  result := Item.ImageIndex2;
  if Assigned(FOnItemPicDraw2) then FOnItemPicDraw2(self, Item, result);
end;

function TCustomElTree.GetDraggableSections: Boolean;
{ Returns the value of data member FDraggableSections. }
begin
  result := FHeader.AllowDrag;
end; { GetDraggableSections }

procedure TCustomElTree.SetDraggableSections(newValue: Boolean);
begin
  if FHeader.AllowDrag <> newValue then
  begin
    FHeader.AllowDrag := newValue;
  end; { if }
end; { SetDraggableSections }

procedure TCustomElTree.OnHeaderSectionMove(Sender: TCustomElHeader; Section: TElHeaderSection; OldPos, NewPos: integer); { protected }
begin
  IsUpdating := true;
  FView.FClearAll := true;
  FUpdated := true;
  IsUpdating := false;
  TriggerHeaderColumnMoveEvent(Section, OldPos, NewPos);
end; { OnHeaderSectionMove }

procedure TCustomElTree.TriggerHotTrackEvent(OldItem, NewItem: TElTreeItem);
begin
  if assigned(FOnHotTrack) then FOnHotTrack(Self, OldItem, NewItem);
end; { TriggerHotTrackEvent }

procedure TCustomElTree.SetSortMode(newValue: TSortModes);
begin
  if FSortMode <> newValue then FSortMode := newValue;
end; { SetSortMode }

procedure TCustomElTree.SetSortSection(newValue: Integer);
begin
  if FSortSection <> newValue then
  begin
    FSortSection := newValue;
    if FSortSections <> nil then
      FSortSections.Clear;
  end;
end; { SetSortSection }

function TCustomElTree.CompareItems(Item1, Item2: TElTreeItem; SM : TElSSortMode; ST : TSortTypes; FSortSection : integer): integer;
var
  T1, T2   : TDateTime;
  N1, N2   : integer;
  F1, F2   : extended;
  B1, B2   : boolean;
  C1, C2   : Currency;
  CurIdx   : integer;
begin
  (*
  if FSortSection = -1 then
  begin
    DoCompareItems(Item1, Item2, result);
    if FSortDir = sdDescend then
       result := -result;
  end
  else
  *)
  begin
    if ST = stCustom then
    begin
      DoCompareItems(Item1, Item2, result);
      if (SM = hsmDescend) or ((SM = hsmNone) and (FSortDir = sdDescend)) then
         result := -result;
    end
    else
    begin
      {$IFDEF USE_VARIANT}
      if (not VarIsEmpty(Item1.FSortData)) and (not VarIsEmpty(Item2.FSortData)) then
      {$ELSE}
      if (Item1.FSortType <> -1) and (Item2.FSortType <> -1) then
      {$ENDIF}
      begin
        {$IFDEF USE_VARIANT}
        if TVarData(Item1.FSortData).VType = TVarData(Item2.FSortData).VType then
        {$ELSE}
        if Item1.FSortType = Item2.FSortType then
        {$ENDIF}
        begin
          if ST = stText then
          begin
{$ifdef ELPACK_UNICODE}
            {$IFDEF USE_VARIANT}
            result := Sign(WideCompareText(Item1.FSortData, Item2.FSortData));
            {$ELSE}
            //if SortUseCase then
              result := Sign(WideStrComp(PWideChar(Item1.FSortData), PWideChar(Item2.FSortData)))
            ;
            //else
            //  result := Sign(WideCompareText(PWideChar(WideUppercase(WideStrPas(PWidechar(Item1.FSortData)))), PWideChar(WideUppercase(WideStrPas(PWidechar(Item2.FSortData))))));
            {$ENDIF}
{$else}
            {$IFDEF USE_VARIANT}
            result := Sign(AnsiCompareText(Item1.FSortData, Item2.FSortData));
            {$ELSE}
            //if SortUseCase then
              result := Sign(AnsiStrComp(PChar(Item1.FSortData), PChar(Item2.FSortData)))
            ;
            //else
            //  result := Sign(AnsiStrIComp(PChar(Item1.FSortData), PChar(Item2.FSortData)));
            {$ENDIF}
{$endif}
          end
          else
          if ST = stNumber then
          begin
            {$IFDEF USE_VARIANT}
            N1 := Item1.FSortData;
            N2 := Item2.FSortData;
            {$ELSE}
            N1 := Integer(Item1.FSortData);
            N2 := Integer(Item2.FSortData);
            {$ENDIF}
            if N1 > N2 then
               result := 1
            else
            if N1 < N2 then
               result := -1
            else
               result := 0;
          end
          else
          case ST of
            stFloating:
              begin
                {$IFDEF USE_VARIANT}
                F1 := Item1.FSortData;
                F2 := Item2.FSortData;
                {$ELSE}
                F1 := PDouble(Item1.FSortData)^;
                F2 := PDouble(Item2.FSortData)^;
                {$ENDIF}
                if F1 > F2 then
                   result := 1
                else
                if F1 < F2 then
                   result := -1
                else
                   result := 0;
              end;
            stCurrency:
              begin
                {$IFDEF USE_VARIANT}
                C1 := Item1.FSortData;
                C2 := Item2.FSortData;
                {$ELSE}
                C1 := PCurrency(Item1.FSortData)^;
                C2 := PCurrency(Item2.FSortData)^;
                {$ENDIF}
                if C1 > C2 then
                   result := 1
                else
                if C1 < C2 then
                   result := -1
                else
                   result := 0;
              end;
            stDateTime:
              begin
                {$IFDEF USE_VARIANT}
                T1 := Item1.FSortData;
                T2 := Item2.FSortData;
                {$ELSE}
                T1 := PDouble(Item1.FSortData)^;
                T2 := PDouble(Item2.FSortData)^;
                {$ENDIF}
                if T1 > T2 then
                   result := 1
                else
                if T1 < T2 then
                   result := -1
                else
                   result := 0;
              end;
            stDate:
              begin
                {$IFDEF USE_VARIANT}
                T1 := Item1.FSortData;
                T2 := Item2.FSortData;
                T1 := Trunc(T1);
                T2 := Trunc(T2);
                {$ELSE}
                T1 := PDouble(Item1.FSortData)^;
                T2 := PDouble(Item2.FSortData)^;
                T1 := Trunc(T1);
                T2 := Trunc(T2);
                {$ENDIF}

                if T1 > T2 then
                   result := 1
                else
                if T1 < T2 then
                   result := -1
                else
                   result := 0;
              end;
            stTime:
              begin
                {$IFDEF USE_VARIANT}
                T1 := Item1.FSortData;
                T2 := Item2.FSortData;
                T1 := Frac(T1);
                T2 := Frac(T2);
                {$ELSE}
                T1 := PDouble(Item1.FSortData)^;
                T2 := PDouble(Item2.FSortData)^;
                T1 := Frac(T1);
                T2 := Frac(T2);
                {$ENDIF}
                if T1 > T2 then
                   result := 1
                else
                if T1 < T2 then
                   result := -1
                else
                   result := 0;
              end;
            stBoolean:
              begin
               {$IFDEF USE_VARIANT}
                B1 := Item1.FSortData;
                B2 := Item2.FSortData;
                {$ELSE}
                B1 := Integer(Item1.FSortData) > 0;
                B2 := Integer(Item2.FSortData) > 0;
                {$ENDIF}
                if B1 = B2 then
                   result := 0
                else
                if B1 then
                   result := 1
                else
                   result := -1;
              end;
          end;
        end
        else
          // if items can't be compared, they are treated as equal
          result := 0;
      end
      else
      {$IFDEF USE_VARIANT}
      if (VarIsEmpty(Item1.FSortData)) then
      begin
        if (VarIsEmpty(Item2.FSortData)) then
      {$ELSE}
      if Item1.FSortType = -1 then
      begin
        if Item2.FSortType = -1 then
      {$ENDIF}
        begin
          result := 0;
        end
        else
        begin
          result := -1;
        end;
      end
      else
      {$IFDEF USE_VARIANT}
      if (VarIsEmpty(Item2.FSortData)) then
      {$ELSE}
      if Item2.FSortType = -1 then
      {$ENDIF}
        result := 1
      ;
      if (not (SM = hsmAscend)) then
          if (SM = hsmDescend) or ((SM = hsmNone) and (FSortDir = sdDescend)) then
              result := -result;
    end;
  end;
  // he-he, multisort.
  if (result = 0) and (FSortSections <> nil) then
  begin
    CurIdx := 0;
    while (CurIdx < FSortSections.Count) do
    begin
      SlowCompareItems(Item1, Item2, FSortSections[CurIdx], Result);
      if (Result <> 0) then
        break;
      Inc(CurIdx);
    end;
  end;
  if (Result = 0) and (Item1 <> Item2) then
  begin
      if (Integer(Pointer(Item1)) > Integer(Pointer(Item2))) then
        Result := 1
      else
        Result := -1;
    if (SM = hsmDescend) or ((SM = hsmNone) and (FSortDir = sdDescend)) then
       result := -result;
  end;
end;

procedure TCustomElTree.SlowCompareItems(Item1, Item2: TElTreeItem; Section :
    TElHeaderSection; var Result : integer);
var
  SectIdx  : Integer;
  Text1,
  Text2    : TElFString;

  T1, T2   : TDateTime;
  N1, N2   : integer;
  F1, F2   : extended;
  B1, B2   : boolean;
  C1, C2   : Currency;
  V        : Variant;
  IntConv  : boolean;

begin
  SectIdx := Section.Index;
  IntConv := true;
  if (VirtualityLevel <> vlNone) then
  begin
    if Assigned(FOnVirtualValueNeeded) then
    begin
      IntConv := false;
      try
        case SectionTypeToSortType(Section.FieldType) of
          stCustom:
            DoCompareItems(Item1, Item2, Result);
          stText :
            begin
              TriggerVirtualTextNeeded(Item1, SectIdx, Text1);
              TriggerVirtualTextNeeded(Item2, SectIdx, Text2);

              {$ifdef ELPACK_UNICODE}
              if SortUseCase then
                result := Sign(WideStrComp(PWideChar(WideString(Text1)), PWideChar(WideString(Text2))))
              else
                result := Sign(WideCompareText(PWideChar(WideUppercase(Text1)), PWideChar(WideUppercase(Text2))));
              {$else}
              if SortUseCase then
                result := Sign(AnsiStrComp(PChar(Text1), PChar(Text2)))
              else
                result := Sign(AnsiStrIComp(PChar(Text1), PChar(Text2)));
              {$endif}
            end;
          stNumber:
            begin
              TriggerVirtualValueNeeded(Item1, SectIdx, vtInteger, V);
              N1 := V;
              TriggerVirtualValueNeeded(Item2, SectIdx, vtInteger, V);
              N2 := V;

              if N1 < N2 then
                result := 1
              else
              if N1 > N2 then
                result := -1
              else
                result := 0;
            end;
          stFloating:
            begin
              TriggerVirtualValueNeeded(Item1, SectIdx, vtExtended, V);
              F1 := V;
              TriggerVirtualValueNeeded(Item2, SectIdx, vtExtended, V);
              F2 := V;
              if F1 < F2 then
                result := 1
              else
              if F1 > F2 then
                result := -1
              else
                result := 0;
            end;
          stDate,
          stTime,
          stDateTime:
            begin
              TriggerVirtualValueNeeded(Item1, SectIdx, vtExtended, V);
              T1 := V;
              TriggerVirtualValueNeeded(Item2, SectIdx, vtExtended, V);
              T2 := V;
              if T1 < T2 then
                result := 1
              else
              if T1 > T2 then
                result := -1
              else
                result := 0;
            end;
          stBoolean:
            begin
              TriggerVirtualValueNeeded(Item1, SectIdx, vtBoolean, V);
              B1 := V;
              TriggerVirtualValueNeeded(Item2, SectIdx, vtBoolean, V);
              B2 := V;

              if B1 = B2 then
                 result := 0
              else
              if B1 then
                 result := 1
              else
                 result := -1;
            end;
          stCurrency:
            begin
              TriggerVirtualValueNeeded(Item1, SectIdx, vtCurrency, V);
              C1 := V;
              TriggerVirtualValueNeeded(Item2, SectIdx, vtCurrency, V);
              C2 := V;

              if C1 > C2 then
                 result := 1
              else
              if C1 < C2 then
                 result := -1
              else
                 result := 0;
            end;
        end;
      except
        on E : EVariantError do
          IntConv := true;
      end;
    end;
  end;

  if IntConv then
  begin
    if SectIdx = MainTreeColumn then
    begin
      Text1 := Item1.Text;
      Text2 := Item2.Text;
    end
    else
    begin
      if SectIdx > MainTreeColumn then
        dec(SectIdx);

        if SectIdx < Item1.ColumnText.Count then
          Text1 := Item1.ColumnText[SectIdx]
        else
          Text1 := '';
        if SectIdx < Item2.ColumnText.Count then
          Text2 := Item2.ColumnText[SectIdx]
        else
          Text2 := '';
    end;
    case SectionTypeToSortType(Section.FieldType) of
      stCustom:
        DoCompareItems(Item1, Item2, Result);
      stText :
        {$ifdef ELPACK_UNICODE}
        if SortUseCase then
          result := Sign(WideStrComp(PWideChar(WideString(Text1)), PWideChar(WideString(Text2))))
        else
          result := Sign(WideCompareText(PWideChar(WideUppercase(Text1)), PWideChar(WideUppercase(Text2))));
        {$else}
        if SortUseCase then
          result := Sign(AnsiStrComp(PChar(Text1), PChar(Text2)))
        else
          result := Sign(AnsiStrIComp(PChar(Text1), PChar(Text2)));
        {$endif}
      stNumber:
        begin
          N1 := StrToIntDef(Text1, 0);
          N2 := StrToIntDef(Text2, 0);
          if N1 < N2 then
            result := 1
          else
          if N1 > N2 then
            result := -1
          else
            result := 0;
        end;
      stFloating:
        begin

          TextToFloat(PChar(String(Text1)), F1, fvExtended);
          TextToFloat(PChar(String(Text2)), F2, fvExtended);
          if F1 < F2 then
            result := 1
          else
          if F1 > F2 then
            result := -1
          else
            result := 0;
        end;
      stDateTime:
        begin
          T1 := StrToDateTime(Text1);
          T2 := StrToDateTime(Text2);
          if T1 < T2 then
            result := 1
          else
          if T1 > T2 then
            result := -1
          else
            result := 0;
        end;
      stDate:
        begin
          T1 := StrToDate(Text1);
          T2 := StrToDate(Text2);
          if T1 < T2 then
            result := 1
          else
          if T1 > T2 then
            result := -1
          else
            result := 0;
        end;
      stTime:
        begin
          T1 := StrToTime(Text1);
          T2 := StrToTime(Text2);
          if T1 < T2 then
            result := 1
          else
          if T1 > T2 then
            result := -1
          else
            result := 0;
        end;
      stBoolean:
        begin
          B1 := Length(Text1) > 0;
          B2 := Length(Text2) > 0;
          if B1 = B2 then
             result := 0
          else
          if B1 then
             result := 1
          else
             result := -1;
        end;
      stCurrency:
        begin
          C1 := PrettyStrToCurr(Text1);
          C2 := PrettyStrToCurr(Text2);
          if C1 > C2 then
             result := 1
          else
          if C1 < C2 then
             result := -1
          else
             result := 0;
        end;
    end;
  end;
  {
  if (Result = 0) and (Item1 <> Item2) then
    if (Integer(Pointer(Item1)) > Integer(Pointer(Item2))) then
      Result := 1
    else
      Result := -1;
  }
  if Section.SortMode = hsmDescend then
    Result := -Result;
end;

procedure TCustomElTree.Update;
begin
  inherited;
end;


{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifndef VER3_EDITORS}
function TCustomElTree.IsEditing: boolean;
begin
  result := FView.FEditing;
end;
{$else}
function TCustomElTree.IsEditing: boolean;
begin
  result := (FView.FInpEdit <> nil) and (FView.FInpEdit.Visible);
end;
{$endif}
{$endif}

{$ifdef SUPPORT_STORAGE}
procedure TCustomElTree.SetStorage(newValue: TElIniFile);
begin
  if newValue <> FStorage then
  begin
    {$ifdef VCL_5_USED}
    if FStorage <> nil then
      FStorage.RemoveFreeNotification(Self);
    {$endif}
    FStorage := newValue;
    if FStorage <> nil then
      FStorage.FreeNotification(Self);
  end;
end;
{$ENDIF}

function TCustomElTree.GetNodeAt(X, Y: integer): TElTreeItem; { public }
begin
  result := GetItemAtY(Y);
end; { GetNodeAt }

procedure TCustomElTree.SetShowCheckboxes(newValue: Boolean);
begin
  if (FShowCheckboxes <> newValue) then
  begin
    IsUpdating := true;
    FShowCheckboxes := newValue;
    with FView do
    begin
      if not FShowHeader then FView.FHRange := -1;
      FClearAll := true;
    end;
    FUpdated := true;
    IsUpdating := false;
  end; {if}
end; {SetShowCheckboxes}

procedure TCustomElTree.SetPlusPicture(newValue: TBitmap);
begin
  PlusPicture.Assign(newValue);
end; {SetPlusPicture}

procedure TCustomElTree.SetMinusPicture(newValue: TBitmap);
begin
  MinusPicture.Assign(newValue);
end; {SetMinusPicture}

procedure TCustomElTree.SetCustomPlusMinus(newValue: Boolean);
begin
  if (FCustomPlusMinus <> newValue) then
  begin
    IsUpdating := true;
    FCustomPlusMinus := newValue;
    if FCustomPlusMinus then
    begin
      GetPlusPicture;
      GetMinusPicture;
      GetLeafPicture;
    end;
    with FView do
    begin
      if not FShowHeader then FView.FHRange := -1;
      FClearAll := true;
    end;
    FUpdated := true;
    IsUpdating := false;
  end; {if}
end; {SetCustomPlusMinus}

procedure TCustomElTree.SetSelectColumn(newValue: Integer);
begin
  if (FSelectColumn <> newValue) then
  begin
    if (newValue > -2) and ((newValue < FHeader.Sections.Count) or
       (csLoading in ComponentState)) then
    begin
      FSelectColumn := newValue;
      if not FRowSelect then
      begin
        if not FMultiSelect then
        begin
          with FView do
            if (FFocused <> nil) then FFocused.UpdateItem;
        end else
        begin
          IsUpdating := true;
          FView.FClearAll := true;
          FUpdated := true;
          IsUpdating := false;
        end; // if/else
      end; //if
    end; // if
  end;
end; {SetSelectColumn}

procedure TCustomElTree.TriggerHeaderLookupEvent;
begin
  if (assigned(FOnHeaderLookup)) then FOnHeaderLookup(Self, Section, Text);
end; {TriggerHeaderLookupEvent}

procedure TCustomElTree.TriggerHeaderLookupDoneEvent;
begin
  if (assigned(FOnHeaderLookupDone)) then FOnHeaderLookupDone(Self, Section, Text, Accepted);
end; {TriggerHeaderLookupDoneEvent}

procedure TCustomElTree.TriggerHeaderSectionExpandEvent(Section:
  TElHeaderSection);
begin
  if (assigned(FOnHeaderSectionExpand)) then FOnHeaderSectionExpand(Self, Section);
end;

procedure TCustomElTree.TriggerHeaderSectionCollapseEvent(Section: TElHeaderSection);
begin
  if (assigned(FOnHeaderSectionCollapse)) then
    FOnHeaderSectionCollapse(Self, Section);
end;

procedure TCustomElTree.ScrollBarMouseDown;
begin
  if (not FView.Focused) and FView.CanFocus and CanFocus {and
     (not (csDesigning in ComponentState))} then
      FView.SetFocus;
end;

procedure TCustomElTree.SBChanged;
begin
  if (not FIgnoreSBChange) and (not (csLoading in ComponentState)) then
  begin
    AlignPieces;
    UpdateScrollBars;
  end;
end;

procedure TCustomElTree.Loaded;
var
  DT: TElDragType;
begin
  inherited;
  IsUpdating := true;
  FHeader.Loaded;
  if (FSortMode = smAdd) or (FSortMode = smAddClick) then
  begin
    TriggerSortBegin;
    FItems.FRoot.Sort(true);
    TriggerSortEnd;
  end;
  FUpdated := true;
  FView.FClearAll := true;
  DT := FDragType;
  FDragType := dtDelphi;
  DragType := DT;
  OnFontChange(Font);

  {$ifdef HAS_HTML_RENDER}
  ReRenderAllHTMLItems;
  {$endif}
  if SelectColumn >= FHeader.Sections.Count then
    SelectColumn := -1;
  if FHeader.Height <> FHeaderHeight then
  begin
    FHeader.Height := FHeaderHeight;
    AlignPieces;
    {$IFNDEF LITE}
    if not (BackgroundType in [bgtColorFill, bgtCenterBitmap]) then FView.RedoTmpBmp;
    {$ENDIF}
  end;
  FHScrollBar.Loaded;
  FVScrollBar.Loaded;
  Resize;
  if FHeaderUseTreeFont then
    FHeader.Font := Font
  else
    FHeader.Font := FHeaderFont;
  AutoSizeAllColumns;
  IsUpdating := false;
end;

procedure TCustomElTree.SetDragType(newValue: TElDragType);
begin
  if (FDragType <> newValue) then
  begin
{$IFNDEF VER90}
    if newValue = dtDelphi then
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF LITE}
    begin
      FDropTarget.Free;
      FDropTarget := nil;
    end
{$ENDIF}
{$endif}
    else
    begin
      if (FDragType = dtDelphi) and FView.HandleAllocated and (not (csLoading in ComponentState)) then // when switching from one to another OLE mode we don't create a drop target
      begin
{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF LITE}
        FDropTarget := TElDropTarget.Create(nil);
        FDropTarget.Target := FView;
        FDropTarget.OnTargetDrag := FView.OnDropTargetDrag;
        FDropTarget.OnTargetDrop := FView.OnDropTargetDrop;
{$ENDIF}
{$endif}
      end;
    end;
{$ENDIF}
    FDragType := newValue;
  end; {if}
end; {SetDragType}

{$ifdef ELTREE_USE_OLE_DRAGDROP}
{$IFNDEF VER90}
{$IFNDEF LITE}
procedure TCustomElTree.TriggerOleTargetDragEvent;
begin
  Y := Y + FView.Top;
  X := X + FView.Left;
  if (assigned(FOnOleTargetDrag)) then
    FOnOleTargetDrag(Self, State, Source, Shift, X, Y, DragType);
end; {TriggerOleTargetDragEvent}

procedure TCustomElTree.TriggerOleTargetDropEvent;
begin
  Y := Y + FView.Top;
  X := X + FView.Left;
  if (assigned(FOnOleTargetDrop)) then
    FOnOleTargetDrop(Self, Source, Shift, X, Y, DragType);
end; {TriggerOleTargetDropEvent}

procedure TCustomElTree.TriggerOleDragStartEvent;
begin
  if (assigned(FOnOleDragStart)) then
    FOnOleDragStart(Self, dataObj, dropSource, dwOKEffects);
end; {TriggerOleDragStartEvent}

procedure TCustomElTree.TriggerOleDragFinishEvent;
begin
  if (assigned(FOnOleDragFinish)) then FOnOleDragFinish(Self, dwEffect, Result);
end; {TriggerOleDragFinishEvent}
{$ENDIF}
{$ENDIF}
{$endif}

procedure TCustomElTree.HeaderResizeTransfer(Sender: TObject);
begin
  if (assigned(FOnHeaderResize)) then
    FOnHeaderResize(Self);
end;

procedure TCustomElTree.HeaderResizeHandler(Sender: TObject);
begin
  DoHeaderResize(Sender);
  HeaderResizeTransfer(Self);
end;

function TCustomElTree.IsInView(Item: TElTreeItem): Boolean;
begin
  result := FView.FVisible.IndexOf(Item) <> -1;
end;

function TCustomElTree.SectionTypeToSortType(SectionType: TElFieldType):
  TSortTypes;
begin
  case SectionType of
    sftText: result := stText;
    sftNumber: result := stNumber;
    sftFloating: result := stFloating;
    sftDateTime: result := stDateTime;
    sftDate: result := stDate;
    sftTime: result := stTime;
    sftBool: result := stBoolean;
    sftCurrency: result := stCurrency;
  else
    result := stCustom;
  end;
end;

function TCustomElTree.GetStickyHeaderSections: Boolean;
begin
  result := FHeader.StickySections;
end; {GetStickyHeaderSections}

procedure TCustomElTree.SetStickyHeaderSections(newValue: Boolean);
begin
  FHeader.StickySections := newValue;
end; {SetStickyHeaderSections}

procedure TCustomElTree.SetBarStyle(newValue: Boolean);
begin
  if (FBarStyle <> newValue) then
  begin
    IsUpdating := true;
    FBarStyle := newValue;
    if FBarStyle then
    begin
      BkColor := clBtnFace;
      TextColor := clBtnText;
      LinesColor := clBtnShadow;
    end;
    if FAutoLineHeight then FLineHeight := DefineLineHeight;
    with FView do
    begin
      if not FShowHeader then FHRange := -1;
      FClearAll := true;
    end;
    FUpdated := true;
    IsUpdating := false;
  end; {if}
end; {SetBarStyle}

procedure TCustomElTree.SetDrawFocusRect(newValue: Boolean);
begin
  if (FDrawFocusRect <> newValue) then
  begin
    FDrawFocusRect := newValue;
    if ItemFocused <> nil then ItemFocused.RedrawItem(true);
  end; {if}
end; {SetDrawFocusRect}

procedure TCustomElTree.TriggerMeasureItemPartEvent(Item: TElTreeItem; PartIndex: integer; var Size: TPoint);
begin
  if (assigned(FOnMeasureItemPart)) then
    FOnMeasureItemPart(Self, Item, PartIndex, Size);
end; {TriggerMeasureItemPartEvent}

procedure TCustomElTree.SetHorzDivLinesColor(newValue: TColor);
begin
  if (FHorzDivLinesColor <> newValue) then
  begin
    IsUpdating := true;
    FHorzDivLinesColor := newValue;
    if FHLines then
    begin
      FView.FClearAll := true;
      FUpdated := true;
    end;
    IsUpdating := false;
  end; {if}
end; {SetLinesColor}

procedure TCustomElTree.SetLinesColor(newValue: TColor);
begin
  if (FLinesColor <> newValue) then
  begin
    IsUpdating := true;
    FLinesColor := newValue;
    if FShowLines then
    begin
      FView.FClearAll := true;
      FUpdated := true;
    end;
    IsUpdating := false;
  end; {if}
end; {SetLinesColor}

procedure TCustomElTree.SetLinesStyle(newValue: TPenStyle);
begin
  if (FLinesStyle <> newValue) then
  begin
    IsUpdating := true;
    FLinesStyle := newValue;
    if FShowLines then
    begin
      FView.FClearAll := true;
      FUpdated := true;
    end;
    IsUpdating := false;
  end; {if}
end; {SetLinesStyle}

procedure TCustomElTree.SetImages2(newValue: TImageList);
var
  i: integer;
begin
  if FImages2 = newValue then exit;
  IsUpdating := true;
  if FImages2 <> nil then
  begin
    {$ifdef VCL_5_USED}
    FImages2.RemoveFreeNotification(Self);
    {$endif}
    FImages2.UnRegisterChanges(FImageChangeLink);
  end;
  FImages2 := newValue;
  if FImages2 <> nil then
  begin
    FImages2.RegisterChanges(FImageChangeLink);
    FImages2.FreeNotification(Self);
  end;
  if csDestroying in ComponentState then exit;
  i := DefineLineHeight;
  if FAutoLineHeight and (i <> FLineHeight) then
  begin
    FLineHeight := i;
    with FView do
    begin
      FClearVis := true;
      FClearAll := true;
    end;
  end;
  if not FShowHeader then
  begin
    with FView do
    begin
      FHRange := -1;
      DefineHRange;
    end;
    if RightAlignedTree then
    begin
      FRightAlignedTree := false;
      RightAlignedTree := true;
    end;
  end;
  FView.FRangeUpdate := true;
  FUpdated := true;
  IsUpdating := false;
end;

procedure TCustomElTree.SetRightAlignedTree(newValue: Boolean);
begin
  if (FRightAlignedTree <> newValue) then
  begin
    FRightAlignedTree := newValue;
    FHeader.RightAlignedOrder := newValue;
    if not FShowHeader then
    begin
      with FView do
      begin
        if FHRange = -1 then DefineHRange;
        if newValue then SetHPosition(FHRange - FHPos) else SetHPosition(0);
      end;
    end;
    FView.Invalidate;//Repaint;
  end; {if}
end;

procedure TCustomElTree.SetFlat(newValue: Boolean);
begin
  if (FFlat <> newValue) then
  begin
    FFlat := newValue;
    {$ifndef CLX_USED}
    if Flat then
      Ctl3D := true;
    UpdateFrame;
    {$endif}
  end; { if }
end; { SetFlat }

{$ifdef MSWINDOWS}
procedure TCustomElTree.DrawFlatBorder(HorzTracking, VertTracking : boolean);
var
  DC : Windows.HDC;
  SavedDC : Windows.HDC;
  {$ifndef CLX_USED}
  b  : boolean;
  BS : TElFlatBorderType;
  {$endif}
  Theme: HTheme;
  RW,
  RC : TRect;
  {$ifndef CLX_USED}
  AColor : TColor;
  {$endif}

const ScrollBars: array [boolean, boolean] of TScrollStyle = ((ssNone, ssVertical), (ssHorizontal, ssBoth));

begin
  {$ifndef CLX_USED}
  Windows.GetClientRect(Handle, RC);
  {$else}
  Windows.GetClientRect(QWidget_winID(Handle), RC);
  {$endif}
  {$ifndef CLX_USED}
  if not UseCustomScrollBars then
  begin
    if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
      inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
    if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
      inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
  end;
  {$endif}
  {$ifndef CLX_USED}
  GetWindowRect(Handle, RW);
  MapWindowPoints(0, Handle, RW, 2);
  {$else}
  GetWindowRect(QWidget_winID(Handle), RW);
  MapWindowPoints(0, QWidget_winID(Handle), RW, 2);
  {$endif}

  OffsetRect(RC, -RW.Left, -RW.Top);
  OffsetRect(RW, -RW.Left, -RW.Top);

  {$ifndef CLX_USED}
  DC := GetWindowDC(Handle);
  {$else}
  DC := GetWindowDC(QWidget_winID(Handle));
  {$endif}

  try
    if IsThemeApplied {$ifndef CLX_USED}and (BorderStyle = bsSingle){$endif} then
    begin
      Theme := OpenThemeData(0, 'EDIT');
      if Theme <> 0 then
      begin
        SavedDC := SaveDC(DC);
        ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
        DrawThemeBackground(Theme, DC, 0, 0, RW, nil);

        CloseThemeData(Theme);

        RestoreDC(DC, SavedDC);
      end;
    end;
    {$ifndef CLX_USED}
    begin
      if (BorderStyle = bsSingle) and not IsThemeApplied then
      begin
        b := FTreeIsFocused or FMouseOver;
        if b then BS := FActiveBorderType else BS := FInactiveBorderType;
        if ((not FFlat) {$ifndef CLX_USED}and FUseCustomBars{$endif}) then
           BS := fbtSunken;
        if bs = fbtRaised then bs := fbtRaisedOuter;
        if Focused or FMouseOver then
          AColor := LineBorderActiveColor
        else
          AColor := LineBorderInactiveColor;

        DrawFlatFrameEx2(DC, RW, AColor, BkColor, b, Enabled, BorderSides, BS);
      end;
      {$ifndef CLX_USED}
      if (not FUseCustomBars) and (not IsThemeApplied) then
         DrawFlatScrollBars(Handle, DC, RW, (Focused or FMouseOver) and (not FlatFocusedScrollBars),
                            ScrollBars[FHScrollVisible, FVScrollVisible], HorzTracking, VertTracking, false, GetWindowLong(Handle, GWL_STYLE), GetWindowLong(Handle, GWL_EXSTYLE));
      {$endif}
    end;
    {$endif}
  finally
    {$ifndef CLX_USED}
    ReleaseDC(Handle, DC);
    {$else}
    ReleaseDC(QWidget_winID(Handle), DC);
    {$endif}
  end;
end;

procedure TCustomElTree.DrawFlatBorderEx(DC : Windows.HDC; HorzTracking,
VertTracking : boolean);
var
  {$ifndef CLX_USED}
  b  : boolean;
  BS : TElFlatBorderType;
  {$endif}
  SavedDC : Windows.HDC;
  RW,
  RC : TRect;
  {$ifndef CLX_USED}
  AColor : TColor;
  {$endif}

const ScrollBars: array [boolean, boolean] of TScrollStyle = ((ssNone, ssVertical), (ssHorizontal, ssBoth));
var Theme : HTheme;
begin
  {$ifndef CLX_USED}
  Windows.GetClientRect(Handle, RC);
  {$else}
  Windows.GetClientRect(QWidget_winID(Handle), RC);
  {$endif}
  {$ifndef CLX_USED}
  if not UseCustomScrollBars then
  begin
    if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
      inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
    if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
      inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
  end;
  {$endif}

  {$ifndef CLX_USED}
  GetWindowRect(Handle, RW);
  MapWindowPoints(0, Handle, RW, 2);
  {$else}
  GetWindowRect(QWidget_winID(Handle), RW);
  MapWindowPoints(0, QWidget_winID(Handle), RW, 2);
  {$endif}

  OffsetRect(RC, -RW.Left, -RW.Top);
  OffsetRect(RW, -RW.Left, -RW.Top);

  if IsThemeApplied {$ifndef CLX_USED}and (BorderStyle = bsSingle){$endif} then
  begin
    Theme := OpenThemeData(0, 'EDIT');
    if Theme <> 0 then
    begin
      SavedDC := SaveDC(DC);
      ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
      DrawThemeBackground(Theme, DC, 0, 0, RW, nil);

      CloseThemeData(Theme);

      RestoreDC(DC, SavedDC);
    end;
  end;

  {$ifndef CLX_USED}
  begin
    if (BorderStyle = bsSingle) and not IsThemeApplied then
    begin
      b := FTreeIsFocused or FMouseOver;
      if b then BS := FActiveBorderType else BS := FInactiveBorderType;
      if ((not FFlat) {$ifndef CLX_USED}and FUseCustomBars{$endif}) then
         BS := fbtSunken;
      if bs = fbtRaised then bs := fbtRaisedOuter;
      if Focused or FMouseOver then
        AColor := LineBorderActiveColor
      else
        AColor := LineBorderInactiveColor;

      DrawFlatFrameEx2(DC, RW, AColor, BkColor, b, Enabled, FBorderSides, BS);
    end;

    if (not FUseCustomBars) and (not IsThemeApplied) then
       DrawFlatScrollBars(Handle, DC, RW,
                          (Focused or FMouseOver) and (not FlatFocusedScrollBars),
                          ScrollBars[FHScrollVisible, FVScrollVisible],
                          HorzTracking, VertTracking, false, GetWindowLong(Handle, GWL_STYLE), GetWindowLong(Handle, GWL_EXSTYLE));
  end;
  {$endif}
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElTree.WMNCCalcSize(var Message : TWMNCCalcSize);
begin
  if not FUseCustomBars then
    inherited
  else
  begin
    if BorderStyle = bsSingle then
    begin
      inc(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(smYEdge[Ctl3D]));
      inc(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(smXEdge[Ctl3D]));
      dec(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(smYEdge[Ctl3D]));
      dec(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(smXEdge[Ctl3D]));
    end;
  end;

  if BorderStyle = bsSingle then
  begin
    if not (ebsLeft in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Left, GetSystemMetrics(smYEdge[Ctl3D]));
    if not (ebsTop in BorderSides) then
      dec(Message.CalcSize_Params.rgrc[0].Top, GetSystemMetrics(smXEdge[Ctl3D]));
    if not (ebsRight in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Right, GetSystemMetrics(smYEdge[Ctl3D]));
    if not (ebsBottom in BorderSides) then
      Inc(Message.CalcSize_Params.rgrc[0].Bottom, GetSystemMetrics(smXEdge[Ctl3D]));
  end;
  // Message.Result := WVR_REDRAW;
end;
{$endif}

{$ifndef CLX_USED}
procedure TCustomElTree.CMEnabledChanged(var Message: TMessage);
{$else}
procedure TCustomElTree.EnabledChanged;
{$endif}
begin
  inherited;
  FVScrollBar.Enabled := Enabled;
  FHScrollBar.Enabled := Enabled;
end;

{$ifndef CLX_USED}
procedure TCustomElTree.WMNCHITTEST(var Msg : TMessage);  { private }
begin
  inherited;
  if FUseCustomBars then
  begin
    if (Msg.Result = HTHSCROLL) or (Msg.Result = HTVSCROLL) then
        Msg.Result := HTBORDER;
  end;
end;  { WMNCHITTEST }
{$endif}

{$ifndef CLX_USED}
procedure TCustomElTree.WMNCPaint(var Msg: TMessage); { private }
var
  DC : HDC;
  // RW, RC: TRect;
begin
  if FHook <> nil then
  begin
    if FHook.Control = nil then
    begin
      FHook.Control := GetParentForm(Self);
      FHook.Active := DoubleBuffered and (not (csDesigning in componentState));
    end;
  end;
  
  if not FUseCustomBars then
    inherited;
  if (Flat or FUseCustomBars or IsThemeApplied) and (BorderStyle = bsSingle) then
  begin
    //DC := GetDCEx(Handle, HRGN(Msg.wParam), DCX_WINDOW or DCX_INTERSECTRGN);
    //if DC = 0 then
    DC := GetWindowDC(Handle);
    DrawFlatBorderEx(DC, false, false);

    ReleaseDC(Handle, DC);
    Msg.Result := 0;
  end;
end; { WMNCPaint }
{$endif}

{$ifndef CLX_USED}
procedure TCustomElTree.WMMouseMove(var Message: TWMMouseMove);
begin
  inherited;
  FView.MouseMove(KeysToShiftState(Message.Keys),Message.XPos - FView.Left, Message.YPos - FView.Top);
end;
{$endif}

procedure TCustomElTree.SetRightAlignedText(newValue: Boolean);
begin
  if (FRightAlignedText <> newValue) then
  begin
    FRightAlignedText := newValue;
    FHeader.RightAlignedText := newValue;
    {$ifndef CLX_USED}
    RecreateWnd;
    {$else}
    RecreateWidget;
    {$endif}
  end; {if}
end;

procedure TCustomElTree.SetForcedScrollBars(newValue : TScrollStyle);
begin
  if (FForcedScrollBars <> newValue) then
  begin
    FForcedScrollBars := newValue;
    if HandleAllocated then
    begin
      {$ifndef CLX_USED}
      if (not FUseCustomBars) then
         RecreateWnd;
      {$endif}
      IsUpdating := true;
      FUpdated   := true;
      IsUpdating := false;
    end;
  end;  { if }
end;  { SetForcedScrollBars }

procedure TCustomElTree.SetFilteredVisibility(newValue: Boolean);
begin
  if (FFilteredVisibility <> newValue) then
  begin
    IsUpdating := true;
    FFilteredVisibility := newValue;
    if newValue then
      UpdateDiffItems;
    with FView do
    begin
      FVisUpdated := true;
      FClearVis := true;
      FClearAll := true;
      if not FShowHeader then
        FHRange := -1;
      FRangeUpdate := true;
    end;
    FUpdated := true;
    IsUpdating := false;
  end; {if}
end;

procedure TCustomElTree.TriggerApplyVisFilterEvent(Item: TElTreeItem; var
  Hidden: boolean);
begin
  if (assigned(FOnApplyVisFilter)) then
    FOnApplyVisFilter(Self, Item, Hidden);
end;

procedure TCustomElTree.SetHPosition(value: integer);
begin
  FView.SetHPosition(value);
end;

procedure TCustomElTree.SetVPosition(value: integer);
begin
  FView.SetVPosition(value);
end;

procedure TCustomElTree.ClickTransfer(Sender : TObject);
{ Transfers FView OnClick event to the outside world. }
begin
  if (assigned(FOnClick)) then FOnClick(Self);  { Substitute Self for subcomponent's Sender. }
end;  { ClickTransfer }

procedure TCustomElTree.DblClickTransfer(Sender : TObject);
{ Transfers FView OnDblClick event to the outside world. }
begin
  if (assigned(FOnDblClick)) then FOnDblClick(Self);
end;  { DblClickTransfer }
(*
procedure TCustomElTree.EndDragTransfer(Sender : TObject; Source : TObject; X : Integer; Y : Integer);
begin
  y := Y + FView.Top;
  X := X + FView.Left;
  if Source = FView then Source := Self
  else
  if Source is TElTreeDragObject then Source := TElTreeDragObject(Source).Control;
  if (assigned(FOnDrop)) then
    FOnDrop(Self, Source , X , Y );  { Substitute Self for subcomponent's Sender. }
end;
*)
procedure TCustomElTree.DropTransfer(Sender : TObject; Source : TObject; X : Integer; Y : Integer);
{ Transfers FView OnDragDrop event to the outside world. }
begin
  y := Y + FView.Top;
  X := X + FView.Left;
  {if Source is TElTreeDragObject then
     Sender := Source
  else
     }Sender := Self;
  if Source = FView then
     Source := Self
  else
  if Source is TElTreeDragObject then
     Source := TElTreeDragObject(Source).Control;
  if (assigned(FOnDrop)) then
     FOnDrop(Sender, Source , X , Y );  { Substitute Self for subcomponent's Sender. }
end;  { DropTransfer }

procedure TCustomElTree.OverTransfer(Sender : TObject; Source : TObject; X : Integer; Y : Integer; State : TDragState; var Accept : Boolean);
{ Transfers FView OnDragOver event to the outside world. }
begin
  Accept := false;
  y := Y + FView.Top;
  X := X + FView.Left;
  {if Source is TElTreeDragObject then
     Sender := Source
  else}
     Sender := Self;
  if Source = FView then
     Source := Self
  else
  if Source is TElTreeDragObject then
     Source := TElTreeDragObject(Source).Control;
  if (assigned(FOnOver)) then
     FOnOver(Sender, Source , X , Y , State , Accept );  { Substitute Self for subcomponent's Sender. }
end;  { OverTransfer }

procedure TCustomElTree.DragTransfer(Sender : TObject; Target : TObject; X : Integer; Y : Integer);
{ Transfers FView OnEndDrag event to the outside world. }
begin
  y := Y + FView.Top;
  X := X + FView.Left;
  {if Target is TElTreeDragObject then
     Sender := Target
  else
     }Sender := Self;
  if Target = FView then Target := Self;
  if (assigned(FOnDrag)) then
    FOnDrag(Sender, Target , X , Y );  { Substitute Self for subcomponent's Sender. }
end;  { DragTransfer }

procedure TCustomElTree.EnterTransfer(Sender : TObject);
{ Transfers FView OnEnter event to the outside world. }
begin
  if (assigned(FOnEnter)) then
    FOnEnter(Self);  { Substitute Self for subcomponent's Sender. }
end;  { EnterTransfer }

procedure TCustomElTree.ExitTransfer(Sender : TObject);
{ Transfers FView OnExit event to the outside world. }
begin
  if (assigned(FOnExit)) then
    FOnExit(Self);  { Substitute Self for subcomponent's Sender. }
end;  { ExitTransfer }

procedure TCustomElTree.KeyDownTransfer(Sender : TObject; var Key : Word; Shift : TShiftState);
{ Transfers FView OnKeyDown event to the outside world. }
begin
  if (assigned(FOnKeyDown)) then
    FOnKeyDown(Self, Key , Shift );  { Substitute Self for subcomponent's Sender. }
end;  { KeyDownTransfer }

procedure TCustomElTree.KeyPressTransfer(Sender : TObject; var Key : Char);
{ Transfers FView OnKeyPress event to the outside world. }
begin
  if (assigned(FOnKeyPress)) then
    FOnKeyPress(Self, Key );  { Substitute Self for subcomponent's Sender. }
end;  { KeyPressTransfer }

procedure TCustomElTree.KeyUpTransfer(Sender : TObject; var Key : Word; Shift : TShiftState);
{ Transfers FView OnKeyUp event to the outside world. }
begin
  if (assigned(FOnKeyUp)) then
    FOnKeyUp(Self, Key , Shift );  { Substitute Self for subcomponent's Sender. }
end;  { KeyUpTransfer }

procedure TCustomElTree.MouseDownTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer);
{ Transfers FView OnMouseDown event to the outside world. }
begin
  y := Y + FView.Top;
  X := X + FView.Left;
  if (assigned(FOnMouseDown)) then
    FOnMouseDown(Self, Button , Shift , X , Y );  { Substitute Self for subcomponent's Sender. }
end;  { MouseDownTransfer }

procedure TCustomElTree.MouseMoveTransfer(Sender : TObject; Shift : TShiftState; X : Integer; Y : Integer);
{ Transfers FView OnMouseMove event to the outside world. }
begin
  Y := Y + FView.Top;
  X := X + FView.Left;
  if (assigned(FOnMouseMove)) then
    FOnMouseMove(Self, Shift , X , Y );  { Substitute Self for subcomponent's Sender. }
end;  { MouseMoveTransfer }

procedure TCustomElTree.MouseUpTransfer(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X : Integer; Y : Integer);
{ Transfers FView OnMouseUp event to the outside world. }
begin
  Y := Y + FView.Top;
  X := X + FView.Left;
  if (assigned(FOnMouseUp)) then
    FOnMouseUp(Self, Button , Shift , X , Y );  { Substitute Self for subcomponent's Sender. }
end;  { MouseUpTransfer }

procedure TCustomElTree.StartDragTransfer(Sender : TObject; var DragObject : TDragObject);
{ Transfers FView OnStartDrag event to the outside world. }
begin
{$IFDEF VCL_4_USED}
  if DragObject <> nil then
  {$ifndef CLX_USED}
  with DragObject.DragPos do
  {$else}
  with DragObject.DragTargetPos do
  {$endif}
  begin
    y := Y + FView.Top;
    X := X + FView.Left;
  end;
{$ENDIF VCL_4_USED}
  if (assigned(FOnStartDrag)) then
    FOnStartDrag(Self, DragObject);  { Substitute Self for subcomponent's Sender. }
{$IFDEF VCL_4_USED}
  if DragObject <> nil then
{$ifndef CLX_USED}
  with DragObject.DragPos do
  {$else}
  with DragObject.DragTargetPos do
  {$endif}
  begin
    y := Y - FView.Top;
    X := X - FView.Left;
  end;
{$ENDIF VCL_4_USED}
  if DragObject = nil then
  begin
    DragObject := TElTreeDragObject.Create(Self);
  end;
end;  { StartDragTransfer }

procedure TCustomElTree.MeasureSectionTransfer(Sender : TObject; Section : TElHeaderSection; var Size: TPoint);
begin
  if Assigned(FOnHeaderSectionMeasure) then FOnHeaderSectionMeasure(Self, Section, Size);
end;

function TCustomElTree.CreateView : TElTreeView;  { protected }
begin
  result := TElTreeView.Create(Self);
end;  { CreateView }

{ Exposed properties' Read/Write methods: }
procedure TCustomElTree.SetCursor(newValue : TCursor);
begin
  FView.Cursor := newValue;
  if newValue <> FCursor then FCursor := newValue;
end;  { SetCursor }

function TCustomElTree.GetCursor : TCursor;
{ Returns the Cursor property from the FView subcomponent. }
begin
  GetCursor := FView.Cursor;
end;  { GetCursor }

{$ifndef CLX_USED}
function TCustomElTree.SetScrollInfo(hWnd: HWND; BarFlag: Integer; const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer;
begin
  if (hWnd = FHScrollBar.Handle) or (hWnd = FVScrollBar.Handle) then
  begin
    result := SendMessage(hWnd, SBM_SetScrollInfo, Integer(Redraw), Integer(@ScrollInfo));
  end else result := -1;
end;

function TCustomElTree.GetScrollInfo(hWnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): BOOL;
begin
  if (hWnd = FHScrollBar.Handle) or (hWnd = FVScrollBar.Handle) then
  begin
    SendMessage(hWnd, SBM_GetScrollInfo, 0, Integer(@ScrollInfo));
    result := BOOL(true);
  end else result := BOOL(false);
end;
{$else}
function TCustomElTree.SetScrollInfo(Wnd: TCustomElScrollBar; BarFlag: Integer; const ScrollInfo: ElCLXUtils.TScrollInfo; Redraw: boolean): Integer;
begin
  if (Wnd <> nil) then
  begin
    result := Wnd.SetScrollInfo(ScrollInfo, Redraw);
  end
  else
    result := -1;
end;

function TCustomElTree.GetScrollInfo(Wnd: TCustomElScrollBar; BarFlag: Integer; var ScrollInfo: ElCLXUtils.TScrollInfo): boolean;
begin
  if (Wnd <> nil) then
  begin
    result := Wnd.GetScrollInfo(ScrollInfo);
  end
  else
    result := BOOL(false);
end;
{$endif}

procedure TCustomElTree.HorzScrollDrawPartTransfer(Sender : TObject;
                                                   Canvas : TCanvas;
                                                   R : TRect;
                                                   Part : TElScrollBarPart;
                                                   Enabled : Boolean;
                                                   Focused : Boolean;
                                                   Pressed : Boolean;
                                                   var DefaultDraw : Boolean);
{ Transfers FHScrollBar OnDrawPart event to the outside world. }
begin
  if (assigned(FOnHorzScrollDrawPart)) then
    FOnHorzScrollDrawPart(Self, Canvas , R , Part , Enabled , Focused , Pressed , DefaultDraw );  { Substitute Self for subcomponent's Sender. }
end;  { HorzScrollDrawPartTransfer }

procedure TCustomElTree.HorzScrollHintNeededTransfer(Sender : TObject; TrackPosition : Integer; var Hint : TElFString);
{ Transfers FHScrollBar OnScrollHintNeeded event to the outside world. }
begin
  if (assigned(FOnHorzScrollHintNeeded)) then
    FOnHorzScrollHintNeeded(Self, TrackPosition , Hint );  { Substitute Self for subcomponent's Sender. }
end;  { HorzScrollHintNeededTransfer }

procedure TCustomElTree.VertScrollDrawPartTransfer(Sender : TObject; Canvas : TCanvas;
                                                   R : TRect;
                                                   Part : TElScrollBarPart;
                                                   Enabled : Boolean;
                                                   Focused : Boolean;
                                                   Pressed : Boolean;
                                                   var DefaultDraw : Boolean);
{ Transfers FVScrollBar OnDrawPart event to the outside world. }
begin
  if (assigned(FOnVertScrollDrawPart)) then
    FOnVertScrollDrawPart(Self, Canvas , R , Part , Enabled , Focused , Pressed , DefaultDraw );  { Substitute Self for subcomponent's Sender. }
end;  { VertScrollDrawPartTransfer }

procedure TCustomElTree.VertScrollHintNeededHandler(Sender : TObject; TrackPosition : Integer; var Hint : TElFString);
var Item : TElTreeItem;
    FCol : integer;
begin
  VertScrollHintNeededTransfer(Self, TrackPosition, Hint);
  if Hint = '' then
  begin
    if FSortSection = -1 then exit;
    Item := Items.ItemAsVis[TrackPosition];
    if Item = nil then
      exit;
    if Item.FStaticData <> nil then
    begin
      if FSortSection = FMainTreeCol then
        Hint := Item.Text
      else
      begin
        FCol := FSortSection;
        if FSortSection > FMainTreeCol then dec(FCol);
        try
          if FCol < Item.FStaticData.FColText.Count then
            Hint := Item.FStaticData.FColText[FCol];
        except
        end;
      end;
    end
    else
      TriggerVirtualTextNeeded(Item, FSortSection, Hint);
  end;
end;  { VertScrollHintNeededHandler }

procedure TCustomElTree.VertScrollHintNeededTransfer(Sender : TObject; TrackPosition : Integer; var Hint : TElFString);
{ Transfers FHScrollBar OnScrollHintNeeded event to the outside world. }
begin
  if (assigned(FOnVertScrollHintNeeded)) then
    FOnVertScrollHintNeeded(Self, TrackPosition , Hint );  { Substitute Self for subcomponent's Sender. }
end;  { HorzScrollHintNeededTransfer }

{$ifndef CLX_USED}
procedure TCustomElTree.CreateWnd;  { protected }
{$else}
procedure TCustomElTree.CreateWidget;  { protected }
{$endif}
begin
  FScrollbarsInitialized := false;
  inherited;
  TElScrollBarHack(FHScrollBar).SetDesigning(false);
  TElScrollBarHack(FVScrollBar).SetDesigning(false);
  //FHScrollBar.ControlStyle := FHScrollBar.ControlStyle + [csNoDesignVisible];
  //FVScrollBar.ControlStyle := FVScrollBar.ControlStyle + [csNoDesignVisible];
  TElHeaderHack(FHeader).IsDesigning := false;
  FView.SetDesigning(csDesigning in ComponentState);
  AlignPieces;
end;  { CreateWnd }

procedure TCustomElTree.AlignControls(AControl: TControl; var Rect: TRect);
begin
  if not (csDesigning in ComponentState) then
     inherited;
end;

function TCustomElTree.GetHeaderInvertSortArrows : Boolean;
{ Returns the value of data member FHeaderInvertSortArrows. }
begin
  result := FHeader.InvertSortArrows;
end;  { GetHeaderInvertSortArrows }

procedure TCustomElTree.SetHeaderInvertSortArrows(newValue : Boolean);
{ Sets data member FHeaderInvertSortArrows to newValue. }
begin
  FHeader.InvertSortArrows := newValue;
end;  { SetHeaderInvertSortArrows }

{$ifndef CLX_USED}
function TCustomElTree.GetDragImages: TDragImageList;
begin
  result := FView.GetDragImages;
end;
{$endif}

{$IFNDEF LITE}
function TCustomElTree.GetLockedHeaderSection : TElHeaderSection;
{ Returns the value of data member FLockedHeaderSection. }
begin
  result := FHeader.LockedSection;
end;  { GetLockedHeaderSection }

procedure TCustomElTree.SetLockedHeaderSection(newValue : TElHeaderSection);
{ Sets data member FLockedHeaderSection to newValue. }
begin
  FHeader.LockedSection := newValue;
end;  { SetLockedHeaderSection }
{$ENDIF LITE}

{$IFDEF VCL_4_USED}
procedure TCustomElTree.ActionChange(Sender : TObject; CheckDefaults : Boolean);
begin
  inherited;
end;
{$ENDIF}

procedure TCustomElTree.SetAutoExpand(value: boolean);
begin
  if AutoExpand <> value then
  begin
    FAutoExpand := value;
    //if value then FMoveFocusOnCollapse := false;
  end;
end;

procedure TCustomElTree.SetMoveFocusOnCollapse(value: boolean);
begin
  if FMoveFocusOnCollapse <> value then
  begin
    FMoveFocusOnCollapse := value;
    //if value then AutoExpand := false;
  end;
end;

{$ifndef CLX_USED}
function TCustomElTree.GetDragCursor : TCursor;
begin
  result := FView.DragCursor;
end;

procedure TCustomElTree.SetDragCursor(Value : TCursor);
begin
  FView.DragCursor := Value;
end;
{$endif}

procedure TCustomElTree.SetTrackColor(value : TColor);
begin
  if FTrackColor <> Value then
  begin
    FTrackColor := Value;
    if Assigned(FView.FTrackItem) then
       FView.FTrackItem.RedrawItem(true);
  end;
end;

procedure TCustomElTree.SetTransButtons(newValue : boolean);
begin
  if newValue <> FTransButtons then
  begin
    FTransButtons := newValue;
    if FShowButtons then
    begin
      IsUpdating := true;
      FUpdated := true;
      FView.FRangeUpdate := true;
      IsUpdating := false;
    end;
  end;
end;

procedure TCustomElTree.SetHeaderColor(newValue : TColor);
{ Sets the FHeader subcomponent's Color property to newValue. }
begin
  FHeader.Color := newValue;
end;  { SetColor }

function TCustomElTree.GetHeaderColor : TColor;
{ Returns the Color property from the FHeader subcomponent. }
begin
  result := FHeader.Color;
end;  { GetColor }

{$IFNDEF LITE}
procedure TCustomElTree.SetAdjustMultilineHeight(newValue : Boolean);
var i, j : integer;
begin
  if (FAdjustMultilineHeight <> newValue) then
  begin
    FAdjustMultilineHeight := newValue;
    j := FAllList.Count - 1;
    for i := 0 to j do
    begin
      if TElTreeItem(FAllList[i]).Multiline then
      begin
        if newValue then
           inc(TotalVarHeightCount)
        else
           dec(TotalVarHeightCount);
      end;
    end;
    IsUpdating := true;
    with FView do
    begin
      FVisUpdated := true;
      FClearVis := true;
      FClearAll := true;
      FRangeUpdate := true;
    end;
    FUpdated := true;
    IsUpdating := false;
  end;  { if }
end;  { SetAdjustMultilineHeight }
{$ENDIF}

{$IFDEF HAS_HTML_RENDER}
procedure TCustomElTree.TriggerImageNeededEvent(Sender : TObject; Src : TElFString; var Image : TBitmap);
begin
  if (assigned(FOnImageNeeded)) then
    FOnImageNeeded(Self, Src , Image );
end;
{$ENDIF}

{$ifndef CLX_USED}
{$WARNINGS OFF}
procedure TCustomElTree.WMVScroll(var Msg : TWMVScroll);  { private }
var b : boolean;
    sc: TElscrollCode;
    sp: integer;
    ScrollInfo : TScrollInfo;
begin
  b := false;
  case Msg.ScrollCode of
    SB_TOP:       sc := escTop;
    SB_BOTTOM:    sc := escBottom;
    SB_ENDSCROLL: sc := escEndScroll;
    SB_LINEDOWN:  sc := escLineDown;
    SB_LINEUP:    sc := escLineUp;
    SB_PAGEDOWN:  sc := escPageDown;
    SB_PAGEUP:    sc := escPageUp;
    SB_THUMBPOSITION: sc := escPosition;
    SB_THUMBTRACK: sc := escTrack;
  end;
  ScrollInfo.cbSize := Sizeof(ScrollInfo);
  ScrollInfo.fMask := SIF_POS or SIF_TRACKPOS or SIF_RANGE;
  {$ifndef CLX_USED}
  if FUseCustomBars then
    GetScrollInfo(FVScrollBar.Handle, SB_CTL, ScrollInfo)
  else
    Windows.GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  {$else}
  Self.GetScrollInfo(FVScrollBar, SB_CTL, ScrollInfo);
  {$endif}
  if (Msg.ScrollCode <> SB_THUMBTRACK) and (Msg.ScrollCode <> SB_THUMBPOSITION) then
    sp := ScrollInfo.nPos
  else
    sp := ScrollInfo.nTrackPos;
  FView.OnVScroll(FVScrollBar, SC, sp, b);
end;  { WMVScroll }
{$endif}

{$ifndef CLX_USED}
procedure TCustomElTree.WMHScroll(var Msg : TWMHScroll);  { private }
var b : boolean;
    sc: TElscrollCode;
    sp: integer;
begin
  b := false;
  case Msg.ScrollCode of
    SB_TOP:       sc := escTop;
    SB_BOTTOM:    sc := escBottom;
    SB_ENDSCROLL: sc := escEndScroll;
    SB_LINEDOWN:  sc := escLineDown;
    SB_LINEUP:    sc := escLineUp;
    SB_PAGEDOWN:  sc := escPageDown;
    SB_PAGEUP:    sc := escPageUp;
    SB_THUMBPOSITION: sc := escPosition;
    SB_THUMBTRACK: sc := escTrack;
  end;
  sp := Msg.Pos;
  FView.OnHScroll(FHScrollBar, SC, sp, b);
end;  { WMHScroll }
{$WARNINGS ON}
{$endif}

procedure TCustomElTree.TriggerSortBegin;
begin
  if Assigned(FOnSortBegin) then FOnSortBegin(Self);
end;

procedure TCustomElTree.TriggerSortEnd;
begin
  if Assigned(FOnSortEnd) then FOnSortEnd(Self);
end;

procedure TCustomElTree.TriggerItemPostDrawEvent(Canvas : TCanvas; Item : TElTreeItem; ItemRect : TRect; var DrawFocusRect : boolean);
begin
  if (assigned(FOnItemPostDraw)) then
    FOnItemPostDraw(Self, Canvas , Item, ItemRect , DrawFocusRect );
end;  { TriggerItemPostDrawEvent }

{$ifndef CLX_USED}
{$IFDEF ELPACK_COMPLETE}
procedure TCustomElTree.SetHeaderImageForm(newValue : TElImageForm);
begin
  FHeader.ImageForm := newValue;
end;  { SetImageForm }

function TCustomElTree.GetHeaderImageForm : TElImageForm;
begin
  result := FHeader.ImageForm;
end;  { GetImageForm }
{$ENDIF}
{$endif}

function TCustomElTree.GetHint: TElFString;
begin
  result := FHint;
end;

procedure TCustomElTree.SetHint(newValue: TElFString);
begin
  FHint := newValue;
  FRealHint := newValue;
end;

procedure TCustomElTree.StartDelayedFocus(FocusItemToReport : TElTreeItem);
begin
  FDelayTimer.Enabled := false;
  FDelayedItem := FocusItemToReport;
  FDelayTimer.Interval := FChangeDelay;
  FDelayTimer.Enabled := true;
end;

procedure TCustomElTree.StopDelayedFocus;
begin
  FDelayTimer.Enabled := false;
  FDelayedItem := nil;
end;

procedure TCustomElTree.OnDelayTimer(Sender : TObject);
begin
  FDelayTimer.Enabled := false;
  if FDelayedItem = FView.FFocused then
    DoItemFocused;
end;

procedure TCustomElTree.DoAfterSelectionChange;
begin
  if Assigned(FOnAfterSelectionChange) then
    FOnAfterSelectionChange(Self);
end;

procedure TCustomElTree.SetDragRectAcceptColor(const Value: TColor);
begin
  if FDragRectAcceptColor <> Value then
  begin
    FDragRectAcceptColor := Value;
    if (FDragTrgDrawMode in [ColorFrame, ColorRect, dtdUpColorLine, dtdDownColorLine]) and (FView.FDropTrg <> nil) then
    begin
      if FDragObject <> nil then
        FDragObject.HideDragImage;
      FView.FDropTrg.RedrawItem(false);
      Update;
      if FDragObject <> nil then
        FDragObject.ShowDragImage;
    end;
  end;
end;

procedure TCustomElTree.SetDragRectDenyColor(Value: TColor);
begin
  if FDragRectDenyColor <> Value then
  begin
    FDragRectDenyColor := Value;
    if (FDragTrgDrawMode in [ColorFrame, ColorRect, dtdUpColorLine, dtdDownColorLine]) and (FView.FDropTrg <> nil) then
    begin
      if FDragObject <> nil then
        FDragObject.HideDragImage;
      FView.FDropTrg.RedrawItem(false);
      Update;
      if FDragObject <> nil then
        FDragObject.ShowDragImage;
    end;
  end;
end;

procedure TCustomElTree.SetDragTrgDrawMode(Value: TDragTargetDraw);
begin
  if FDragTrgDrawMode <> Value then
  begin
    DoSetDragTrgDrawMode(Value, True);
  end;
end;

function TCustomElTree.GetVisibleRowCount: Integer;
begin
  Result := FView.GetVisCount2;
end;

function TCustomElTree.IndexInView(Item : TElTreeItem): Integer;
begin
  Result := FView.FVisible.IndexOf(Item);
end;

function TCustomElTree.GetDropTarget: TElTreeItem;
begin
  Result := FView.FDropTrg;
end;

procedure TCustomElTree.DoSetDragTrgDrawMode(Value: TDragTargetDraw; RedrawItem 
    : boolean);
begin
  if FDragTrgDrawMode <> Value then
  begin
    FDragTrgDrawMode := Value;
    if (FView.FDropTrg <> nil) and (RedrawItem) then
    begin
      if FDragObject <> nil then
        FDragObject.HideDragImage;
      FView.FDropTrg.RedrawItem(false);
      Update;
      if FDragObject <> nil then
        FDragObject.ShowDragImage;
    end;
  end;
end;

procedure TCustomElTree.AllSelectedEx(SelectedItems : TElList; Order : boolean);
var i : integer;
    Item : TElTreeItem;
begin
  if FSelectedList = nil then
    exit;

  if not Order then
  begin
    SelectedItems.Assign(FSelectedList);
  end
  else
  begin
    SelectedItems.Capacity := FSelectedList.Capacity;
    for i := 0 to FAllList.Count - 1 do
    begin
      Item := TElTreeItem(FAllList.FastGet(i));
      if Item.Selected then
        SelectedItems.Add(Item);
    end;
  end;
end;

{$ifndef VER3_EDITORS}
{$ifdef ELTREE_USE_INPLACE_EDITORS}
procedure TCustomElTree.NotifyOnEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnEditKeyDown) then FOnEditKeyDown(Sender,Key,Shift);
end;
{$endif}
{$endif}

function TCustomElTree.GetFireFocusEvents: boolean;
begin
  Result := (FFireFocusEvents = 0) and not (csDestroying in ComponentState);
end;

procedure TCustomElTree.SetFireFocusEvents(Value: boolean);
begin
  if Value then
    dec(FFireFocusEvents)
  else
    inc(FFireFocusEvents);
end;

procedure TCustomElTree.SetScrollbarOpposite(Value: Boolean);
begin
  if FScrollbarOpposite <> Value then
  begin
    FScrollbarOpposite := Value;
    {$ifndef CLX_USED}
    if FUseCustomBars then
    {$endif}
      AlignPieces; 
  end;
end;

procedure TCustomElTree.SetLineHintTimeout(Value: Integer);
begin
  if FLineHintTimeout <> Value then
  begin
    FLineHintTimeout := Value;
  end;
end;

procedure TCustomElTree.SetVerticalLinesLong(Value: Boolean);
begin
  if FVerticalLinesLong <> Value then
  begin
    FVerticalLinesLong := Value;
    if FVLines then
      Invalidate;
  end;
end;

procedure TCustomElTree.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
  FView.DoEndDrag(Target, X, Y);
end;

function TCustomElTree.GetDefaultSectionWidth: Integer;
begin
  Result := FHeader.DefaultWidth;
end;

procedure TCustomElTree.SetDefaultSectionWidth(Value: Integer);
begin
  FHeader.DefaultWidth := Value;
end;

procedure TCustomElTree.UpdateDiffItems;
var AnItem : TElTreeItem;
    i, j : integer;
begin
  if (FView.FTrackItem <> nil) and FView.FTrackItem.Hidden then
   FView.FTrackItem := nil;
  if (FView.FDropTrg <> nil) and FView.FDropTrg.Hidden then
    FView.FDropTrg := nil;

  (*
  while true do
  begin
    AnItem := FView.FSelected;
    if (AnItem <> nil) and AnItem.Hidden then
    begin
      if AnItem.FParent = FItems.FRoot then
      begin
        i := AnItem.AbsoluteIndex;
        j := i;
        while (AnItem <> nil) and AnItem.Hidden do
        begin
          if i > 0 then
            AnItem := Items[i]
          else
            AnItem := nil;
          Dec(i);
        end;
        if AnItem <> nil then
          SetSelected(AnItem)
        else
        begin
          i := j;
          while (AnItem <> nil) and AnItem.Hidden do
          begin
            if i > 0 then
              AnItem := Items[i]
            else
              AnItem := nil;
            Dec(i);
          end;
          if AnItem <> nil then
            SetSelected(AnItem)
        end;
      end
      else
        if (AnItem <> nil) and (AnItem.FParent <> FItems.FRoot) then
          SetSelected(AnItem.FParent);
      if AnItem <> nil then
        AnItem.Selected := false;
    end
    else
      break;
  end;
  *)

  while true do
  begin
    AnItem := FView.FSelected;
    if (AnItem <> nil) and AnItem.Hidden then
    begin
      if AnItem.FParent = FItems.FRoot then
      begin
        i := AnItem.AbsoluteIndex;
        // j := i;
        while (AnItem <> nil) and AnItem.Hidden do
        begin
          if i >= 0 then  // RAH changed from i > 0
            AnItem := Items[i]
          else
            AnItem := nil;
          Dec(i);
        end;
        if AnItem <> nil then
        begin
          SetSelected(AnItem);
          break;
        end
        else
        begin
          AnItem := FView.FSelected;  // RAH need to restore AnItem
          i := AnItem.AbsoluteIndex; // RAH should be the same as j
          while (AnItem <> nil) and AnItem.Hidden do
          begin
            if i < Integer(Items.Count) then // RAH make sure we stay in bounds
              AnItem := Items[i]
            else
              AnItem := nil;
            inc(i);  // RAH work forward
          end;
          if AnItem <> nil then
             SetSelected(AnItem);
          break;
       end;
     end
     else
       if (AnItem <> nil) and (AnItem.FParent <> FItems.FRoot) then
          SetSelected(AnItem.FParent);
     if AnItem <> nil then
       AnItem.Selected := false;
   end
   else
     break;
  end;

  // Move focus, if needed
  while true do
  begin
    AnItem := FView.FFocused;
    if (AnItem <> nil) and AnItem.Hidden then
    begin
      if AnItem.FParent = FItems.FRoot then
      begin
        i := AnItem.AbsoluteIndex;
        j := i;
        while (AnItem <> nil) and AnItem.Hidden do
        begin
          if i > 0 then
            AnItem := Items[i]
          else
            AnItem := nil;
          Dec(i);
        end;
        if AnItem = nil then
        begin
          i := j;
          while (AnItem <> nil) and AnItem.Hidden do
          begin
            if i > 0 then
              AnItem := Items[i]
            else
              AnItem := nil;
            Dec(i);
          end;
        end;
        DoSetFocused(AnItem, true);
        break;
      end
      else
        if (AnItem <> nil) and (AnItem.FParent <> FItems.FRoot) then
          DoSetFocused(AnItem.FParent, true);
    end
    else
      break;
  end;
end;

procedure TCustomElTree.SetBorderSides(Value: TElBorderSides);
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    {$ifndef CLX_USED}
    if HandleAllocated then
      RecreateWnd;
    {$endif}
  end;
end;

procedure TCustomElTree.ClearSortList(ReSort : boolean);
begin
  if FSortSections <> nil then
  begin
    FSortSections.Clear;
    if Resort then
    begin
      if FUpdateCount > 0 then
        FSortRequired := true
      else
        Sort(true);
    end;
  end;
end;

procedure TCustomElTree.AddSortSection(Index : Integer; ReSort : boolean);
var ASection : TElHeaderSection;
begin
  if FSortSections = nil then
    FSortSections := TElList.Create;
  if FHeader.Sections.Count > Index then
  begin
    ASection := FHeader.Sections[Index];
    FSortSections.Remove(ASection);
    FSortSections.Add(ASection);
    if Resort then
    begin
      if FUpdateCount > 0 then
        FSortRequired := true
      else
        Sort(true);
    end;
  end;
end;

procedure TCustomElTree.RemoveSortSection(Index : Integer; ReSort : boolean);
var i : integer;
begin
  if FSortSections <> nil then
  begin
    if FHeader.Sections.Count > Index then
    begin
      i := FSortSections.IndexOf(FHeader.Sections[Index]);
      if i >= 0 then
      begin
        FSortSections.Delete(i);
        if Resort then
        begin
          if FUpdateCount > 0 then
            FSortRequired := true
          else
            Sort(true);
        end;
      end;
    end;
  end;
end;

procedure TCustomElTree.TriggerVirtualTextNeeded(Item : TElTreeItem; 
    SectionIndex : Integer; var Text : TElFString);
begin
  Text := '';
  if Assigned(FOnVirtualTextNeeded) then
    OnVirtualTextNeeded(Self, Item, SectionIndex, Text);
end;

procedure TCustomElTree.SetVirtualityLevel(Value: TVirtualityLevel);
var i : integer;
begin
  if FVirtualityLevel <> Value then
  begin
    if (FAllList.Count > 0) and (Value <> vlNone) and (csDesigning in ComponentState) and (ComponentState * [csLoading, csReading] = []) then
    {$ifdef MSWINDOWS}
      if MessageBox(0, 'Changing Virtuality Level will clear all data of the items. Continue?', 'Warning', MB_ICONWARNING or MB_OKCANCEL) = IDCANCEL then exit;
    {$else}
      if MessageDlg('Changing Virtuality Level will clear all data of the items. Continue?', 'Warning', mtWarning, [mbYes, mbNo], 0) = mrNo then exit;
    {$endif}
    FVirtualityLevel := Value;
    IsUpdating := true;
    try
      {$ifdef ELTREE_USE_STYLES}
      if Value <> vlNone then
        FView.VirtStyle := TElCellStyle.Create(nil)
      else
      begin
        FView.VirtStyle.Free;
        FView.VirtStyle := nil;
      end;
      {$endif}
      for i := 0 to FAllList.Count - 1 do
      begin
        if Value = vlTextAndStyles then
          TElTreeItem(FAllList[i]).DisposeStaticData
        else
          TElTreeItem(FAllList[i]).NewStaticData;
      end;
      FView.FClearVis  := true;
      FView.FVisUpdated:= true;
    finally
      IsUpdating := false;
    end;
  end;
end;

procedure TCustomElTree.TriggerVirtualHintNeeded(Item : TElTreeItem; var Hint :
    TElFString);
begin
  Hint := '';
  if Assigned(FOnVirtualHintNeeded) then
    OnVirtualHintNeeded(Self, Item, Hint);
end;

procedure TCustomElTree.TriggerVirtualValueNeeded(Item : TElTreeItem;
    SectionIndex : Integer; VarType : integer; var Value : Variant);
begin
  Value := Variants.Unassigned;
  if Assigned(FOnVirtualValueNeeded) then
    FOnVirtualValueNeeded(Self, Item, SectionIndex, VarType, Value); 
end;

{$ifdef ELTREE_USE_STYLES}
procedure TCustomElTree.TriggerVirtualStyleNeeded(Item : TElTreeItem;
    SectionIndex : Integer; Style : TElCellStyle);
begin
  if Assigned(FOnVirtualStyleNeeded) then
    OnVirtualStyleNeeded(Self, Item, SectionIndex, Style);
end;
{$endif}

procedure TCustomElTree.SetStripedOddColor(Value: TColor);
begin
  if FStripedOddColor <> Value then
  begin
    FStripedOddColor := Value;
    if FStripedItems then
      Invalidate;
  end;
end;

procedure TCustomElTree.SetStripedEvenColor(Value: TColor);
begin
  if FStripedEvenColor <> Value then
  begin
    FStripedEvenColor := Value;
    if FStripedItems then
      Invalidate;
  end;
end;

procedure TCustomElTree.SetStripedItems(Value: Boolean);
begin
  if FStripedItems <> Value then
  begin
    FStripedItems := Value;
    Invalidate;
  end;
end;

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifdef VER3_EDITORS}
procedure TCustomElTree.TriggerInplaceEditorNeeded(Item : TElTreeItem;
    SectionIndex : Integer; SupposedFieldType : TElFieldType; var Editor :
    TElTreeInplaceEditor);
begin
   Editor := FEditorManager.GetSuitableEditor(SupposedFieldType);
   if Assigned(FOnInplaceEditorNeeded) then
     FOnInplaceEditorNeeded(Self, Item, SectionIndex, SupposedFieldType, Editor);
end;
{$endif}
{$endif}
procedure TCustomElTree.VertScrollHitTestTransfer(Sender : TObject; X, Y : 
    integer; var Part : TElScrollBarPart; var DefaultTest : boolean);
begin
  if assigned(FOnVertScrollHitTest) then
    FOnVertScrollHitTest(Self, X, Y, Part, DefaultTest);
end;

procedure TCustomElTree.HorzScrollHitTestTransfer(Sender : TObject; X, Y :
    integer; var Part : TElScrollBarPart; var DefaultTest : boolean);
begin
  if assigned(FOnHorzScrollHitTest) then
    FOnHorzScrollHitTest(Self, X, Y, Part, DefaultTest);
end;

procedure TCustomElTree.SetVertDivLinesColor(Value: TColor);
begin
  if (FVertDivLinesColor <> Value) then
  begin
    IsUpdating := true;
    FVertDivLinesColor := Value;
    if FVLines then
    begin
      FView.FClearAll := true;
      FUpdated := true;
    end;
    IsUpdating := false;
  end; {if}
end;

procedure TCustomElTree.SetCheckBoxSize(Value: Integer);
begin
  if FCheckBoxSize <> Value then
  begin
    IsUpdating := true;
    FCheckBoxSize := Value;
    if FShowCheckboxes then
    with FView do
    begin
      if not FShowHeader then
        FView.FHRange := -1;
      FClearAll := true;
    end;
    FUpdated := true;
    IsUpdating := false;
  end;
end;

function TCustomElTree.GetTrackItem: TElTreeItem;
begin
  Result := FView.FTrackItem;
end;

function TCustomElTree.GetDragging: Boolean;
begin
  Result := FView.FInDragging;
end;

{$ifndef CLX_USED}
{$ifdef ELPACK_COMPLETE}
procedure TCustomElTree.IFMRepaintChildren(var Message: TMessage);
begin
  inherited;
  Invalidate;
  Broadcast(Message);
end;
{$endif}

procedure TCustomElTree.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  {$ifdef ELPACK_COMPLETE}
  Perform(IFM_REPAINTCHILDREN, 0, 0);
  {$endif}
end;

{$ifdef ELPACK_COMPLETE}
procedure TCustomElTree.IFMCanPaintBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;
{$endif}
{$endif}

procedure TCustomElTree.SetShowLeafButton(Value: Boolean);
begin
  if FShowLeafButton <> Value then
  begin
    FShowLeafButton := Value;
    IsUpdating := true;
    with FView do
    begin
      if not FShowHeader then
        FHRange := -1;
      FUpdated := true;
      FRangeUpdate := true;
    end;
    IsUpdating := false;
  end;
end;

procedure TCustomElTree.SetLeafPicture(Value: TBitmap);
begin
  LeafPicture.Assign(Value);
end;

{$ifdef VCL_4_USED}
procedure TCustomElTree.MouseWheelTransfer(Sender : TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TCustomElTree.MouseWheelDownTransfer(Sender : TObject; Shift: 
    TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := DoMouseWheelDown(Shift, MousePos);
end;

procedure TCustomElTree.MouseWheelUpTransfer(Sender : TObject; Shift: 
    TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := DoMouseWheelUp(Shift, MousePos);
end;
{$endif}

procedure TCustomElTree.FitMostChildren(Item : TElTreeItem);
begin
  FView.FitMostChildren(Item);
end;

function TCustomElTree.GetThemedClassName: WideString;
begin
  Result := 'TREEVIEW';
end;

procedure TCustomElTree.SetUseXPThemes(const Value: Boolean);
begin
  inherited;
  Self.FHScrollBar.UseXPThemes := Value;
  Self.FVScrollBar.UseXPThemes := Value;
  TElHEaderHack(FHeader).UseXPThemes := Value;
end;

function TCustomElTree.GetCheckBoxSize: Integer;
{$ifdef MSWINDOWS}
var Theme : HTheme;
    PX    : TSize;
{$endif}    
begin
  {$ifdef MSWINDOWS}
  if IsThemeApplied and (not (csDesigning in ComponentState)) then
  begin
    {$ifndef CLX_USED}
    Theme := OpenThemeData(Handle, 'BUTTON');
    {$else}
    Theme := OpenThemeData(QWidget_winID(Handle), 'BUTTON');
    {$endif}
    if Theme <> 0 then
    begin
      {$ifndef CLX_USED}
      GetThemePartSize(Theme, Canvas.Handle, BP_CHECKBOX, 0, nil, 1, PX);
      {$else}
      Canvas.Start;
      GetThemePartSize(Theme, QPaintDevice_handle(QPainter_device(Canvas.Handle)), BP_CHECKBOX, 0, nil, 1, PX);
      Canvas.stop;
      {$endif}
      result := max(PX.cx, PX.cy);
      CloseThemeData(Theme);
    end
    else
      Result := FCheckBoxSize;
  end
  else
  {$endif}
    Result := FCheckBoxSize;
end;

function TCustomElTree.GetHeaderPopupMenu: TPopupMenu;
begin
  Result := FHeader.PopupMenu;
end;

procedure TCustomElTree.SetHeaderPopupMenu(Value: TPopupMenu);
begin
  FHeader.PopupMenu := Value;
end;

{$ifdef HAS_HTML_RENDER}
procedure TCustomElTree.ReRenderAllHTMLItems;
var i : integer;
    Item : TElTreeItem;
begin
  for i := 0 to FAllList.Count - 1 do
  begin
    Item := TElTreeItem(FAllList[i]);
    if Item.IsHTML then
    begin
      Item.ReRenderMainText;
      Item.ReRenderAllTexts;
    end;
  end;
end;
{$endif}

procedure TCustomElTree.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TCustomElTree.SetHeaderUseTreeFont(Value: Boolean);
begin
  if FHeaderUseTreeFont <> Value then
  begin
    FHeaderUseTreeFont := Value;
    if FHeaderUseTreeFont then
      FHeader.Font := Font
    else
      FHeader.Font := FHeaderFont;
  end;
end;

procedure TCustomElTree.HeaderFontChanged(Sender: TObject);
begin
  if not HeaderUseTreeFont then
    FHeader.Font := FHeaderFont;
end;

function TCustomElTree.IsStripedColorStored: Boolean;
begin
  Result := StripedItems;
end;

{$ifndef CLX_USED}
procedure TCustomElTree.WMThemeChanged(var Message: TMessage);
begin
  if ThemesAvailable and UseXPThemes then
  begin
    FreeThemeHandle;
    CreateThemeHandle;
    SetWindowPos(
      Handle,
      0,
      0, 0, 0, 0,
      SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER
      );
    RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  end;
  Message.Result := 1;
end;
{$endif}
procedure TCustomElTree.SetSortUseCase(Value: Boolean);
begin
  if FSortUseCase <> Value then
  begin
    FSortUseCase := Value;
  end;
end;

procedure TCustomElTree.DoEnter;
begin
  inherited;
  FTreeIsFocused := true;
end;

procedure TCustomElTree.DoExit;
begin
  FTreeIsFocused := false;
  inherited;
end;

procedure TCustomElTree.SetLineBorderActiveColor(Value: TColor);
begin
  if FLineBorderActiveColor <> Value then
  begin
    FLineBorderActiveColor := Value;
    if Flat and (Focused or FMouseOver) then
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TCustomElTree.SetLineBorderInactiveColor(Value: TColor);
begin
  if FLineBorderInactiveColor <> Value then
  begin
    FLineBorderInactiveColor := Value;
    if Flat and not (Focused or FMouseOver) then
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TCustomElTree.SetDblClickMode(Value: TElDblClickMode);
begin
  if FDblClickMode <> Value then
  begin
    FDblClickMode := Value;
    if FDblClickMode <> dcmExpand then
      ExpandOnDblClick := false;
  end;
end;

procedure TCustomElTree.SetExpandOnDblClick(Value: Boolean);
begin
  if FExpandOnDblClick <> Value then
  begin
    FExpandOnDblClick := Value;
    if Value then
      FDblClickMode := dcmExpand;
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElTree.OnBeforeHook(Sender : TObject; var Message : TMessage;
    var Handled : boolean);
begin
  Handled := false;
  if Message.Msg = WM_ENTERSIZEMOVE then
    InSizeMove := true
  else
  if Message.Msg = WM_EXITSIZEMOVE then
    InSizeMove := false;
end;

procedure TCustomElTree.SetParent(AParent: TWinControl);
begin
  inherited;
  if (AParent <> nil) and (FHook <> nil) then
  begin
    FHook.Control := GetParentForm(AParent);
    FHook.Active := DoubleBuffered and (not (csDesigning in componentState));
  end
  else
    if FHook <> nil then
      FHook.Control := nil;
  InSizeMove := false;
end;

{$endif}

function TCustomElTree.GetPlusPicture: TBitmap;
begin
  if FPlusPicture = nil then
  begin
    FPlusPicture := TBitmap.Create;
    FPlusPicture.OnChange := OnSignChange;
  end;
  Result := FPlusPicture;
end;

function TCustomElTree.GetLeafPicture: TBitmap;
begin
  if FLeafPicture = nil then
  begin
    FLeafPicture := TBitmap.Create;
    FLeafPicture.OnChange := OnSignChange;
  end;
  Result := FLeafPicture;
end;

function TCustomElTree.GetMinusPicture: TBitmap;
begin
  if FMinusPicture = nil then
  begin
    FMinusPicture := TBitmap.Create;
    FMinusPicture.OnChange := OnSignChange;
  end;
  Result := FMinusPicture;
end;

function TCustomElTree.GetCheckBoxGlyph: TBitmap;
begin
  if FCheckBoxGlyph = nil then
  begin
    FCheckBoxGlyph := TBitmap.Create;
    FCheckBoxGlyph.OnChange := OnCheckSignChange;
  end;
  Result := FCheckBoxGlyph;
end;

function TCustomElTree.GetRadioButtonGlyph: TBitmap;
begin
  if FRadioButtonGlyph = nil then
  begin
    FRadioButtonGlyph := TBitmap.Create;
    FRadioButtonGlyph.OnChange := OnCheckSignChange;
  end;
  Result := FRadioButtonGlyph;
end;

procedure TCustomElTree.OnCheckSignChange(Sender: TObject);
var
  i: integer;
begin
  if not FCustomCheckboxes then exit;
  if FView.FPainting then exit;
  IsUpdating := true;
  if AutoLineHeight then
  begin
    i := DefineLineHeight;
    if i <> FLineHeight then
    begin
      with FView do
      begin
        FClearVis := true;
        if i < FLineHeight then FClearAll := true;
      end;
      FLineHeight := i;
    end;
    FView.FVisUpdated := true;
  end;
  with FView do
  begin
    FRangeUpdate := true;
    FUpdated := true;
  end;
  IsUpdating := false;
end;

{$ifndef CLX_USED}
procedure TCustomElTree.WMUpdateSBFrame(var Message: TMessage);
begin
 SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
end;
{$endif}

procedure TCustomElTree.SetDoubleBuffered(Value: Boolean);
begin
  if FDoubleBuffered <> Value then
  begin
    FDoubleBuffered := Value;
    {$ifndef CLX_USED}
    if FHook <> nil then
      FHook.Active := DoubleBuffered and (not (csDesigning in componentState));
    {$endif}
  end;
end;

{$ifndef CLX_USED}
procedure TCustomElTree.WMSysColorChange(var Msg: TMessage);
begin
  inherited;
  PostMessage(FVScrollBar.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
  PostMessage(FHScrollBar.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
  PostMessage(FHeader.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end; { WMSysColorChange }

{$endif}

// ****************************************************************************
//                                  TElCellStyle
// ****************************************************************************
{$ifdef ELTREE_USE_STYLES}
constructor TElCellStyle.Create;
begin
  inherited Create;
  FOwner := Owner;
  {$ifndef CLX_USED}
  FTextFlags := DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER or DT_LEFT or DT_END_ELLIPSIS;
  {$else}
  FTextFlags := Integer(AlignmentFlags_AlignLeft) or
                Integer(AlignmentFlags_AlignVCenter) or
                Integer(AlignmentFlags_SingleLine);
  {$endif}
  FCellType := sftText;
end;

destructor TElCellStyle.Destroy;
begin
  Control := nil;
  inherited;
end;

procedure TElCellStyle.SetCellBkColor(newValue: TColor);
begin
  if (FCellBkColor <> newValue) then
  begin
    FCellBkColor := newValue;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
      FOwner.UpdateItem;
  end; {if}
end;

procedure TElCellStyle.SetTextBkColor(newValue: TColor);
begin
  if (FTextBkColor <> newValue) then
  begin
    FTextBkColor := newValue;
    if FStyle <> elhsText then exit;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
      FOwner.UpdateItem;
  end; {if}
end;

procedure TElCellStyle.SetTextColor(newValue: TColor);
begin
  if (FTextColor <> newValue) then
  begin
    FTextColor := newValue;
    if FStyle <> elhsText then exit;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
      FOwner.UpdateItem;
  end; {if}
end;

procedure TElCellStyle.SetTextFlags(newValue: DWORD);
begin
  if (FTextFlags <> newValue) then
  begin
    FTextFlags := newValue;
    if FStyle <> elhsText then exit;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
      FOwner.UpdateItem;
  end; {if}
end;

procedure TElCellStyle.SetPicture(newValue: TBitmap);
begin
  if (FPicture <> newValue) then
  begin
    FPicture := newValue;
    if FStyle <> elhsPictureOnly then exit;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
      FOwner.UpdateItem;
  end; {if}
end;

procedure TElCellStyle.SetCellType(newValue: TElFieldType);
begin
  if (FCellType <> newValue) then
  begin
    FCellType := newValue;
  end; {if}
end;

procedure TElCellStyle.SetStyle(newValue: TElSectionStyle);
begin
  if (FStyle <> newValue) then
  begin
    FStyle := newValue;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
      FOwner.UpdateItem;
  end; {if}
end;

procedure TElCellStyle.SetOwnerColors(newValue: Boolean);
begin
  if (FOwnerProps <> newValue) then
  begin
    FOwnerProps := newValue;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
      FOwner.UpdateItem;
  end; {if}
end;

procedure TElCellStyle.SetFontSize(newValue: integer);
begin
  if (FFontSize <> newValue) then
  begin
    FFontSize := newValue;
    if FStyle <> elhsText then exit;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
    begin
      with FOwner.FOwner do
        if FShowHeader and (FUpdateCount = 0) and (VirtualityLevel = vlNone) then
          AutoSizeColumn(Self.FOwner.FStaticData.FStyles.IndexOf(Self))
        else
          FColSizeUpdate := true;
      FOwner.UpdateItem;
    end;
  end; {if}
end;

procedure TElCellStyle.SetFontStyles(newValue: TFontStyles);
begin
  if (FFontStyles <> newValue) then
  begin
    FFontStyles := newValue;
    if FStyle <> elhsText then exit;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
    begin
      with FOwner.FOwner do
        if FShowHeader and (FUpdateCount = 0) and (VirtualityLevel = vlNone) then
          AutoSizeColumn(Self.FOwner.FStaticData.FStyles.IndexOf(Self))
        else
          FColSizeUpdate := true;
      FOwner.UpdateItem;
    end;
  end; {if}
end;

procedure TElCellStyle.SetFontName(newValue: TFontName);
begin
  if (FFontName <> newValue) then
  begin
    FFontName := newValue;
    if FStyle <> elhsText then exit;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
    begin
      with FOwner.FOwner do
        if FShowHeader and (FUpdateCount = 0) and (VirtualityLevel = vlNone) then
          AutoSizeColumn(Self.FOwner.FStaticData.FStyles.IndexOf(Self))
        else
          FColSizeUpdate := true;
      FOwner.UpdateItem;
    end;
  end; {if}
end;

procedure TElCellStyle.Assign(Source: TElCellStyle);
begin
  if Source = nil then exit;
  FUseBkColor  := Source.FUseBkColor; 
  FCellBkColor := Source.FCellBkColor;
  FTextBkColor := Source.FTextBkColor;
  FTextColor := Source.FTextColor;
  FTextFlags := Source.FTextFlags;
  FPicture := Source.FPicture;
  FCellType := Source.FCellType;
  FStyle := Source.FStyle;
  FOwnerProps := Source.FOwnerProps;
  FFontSize := Source.FFontSize;
  FFontStyles := Source.FFontStyles;
  FFontName := Source.FFontName;
end; {Assign}

procedure TElCellStyle.SetControl(newValue: TElCellControl);
begin
  if (FControl <> newValue) then
  begin
    if FControl <> nil then
      if (FOwner.FOwner <> nil) and (FOwner.FOwner.VirtualityLevel = vlNone) then
        FControl.Destroy;
    FControl := newValue;
    if FControl <> nil then
      FControl.FOwner := Self;
    Update;
  end; { if }
end; { SetControl }

procedure TElCellStyle.Update; { public }
begin
  if FOwner <> nil then FOwner.RedrawItem(true);
end; { Update }

procedure TElCellStyle.SetUseBkColor(Value: Boolean);
begin
  if FUseBkColor <> Value then
  begin
    FUseBkColor := Value;
    if Assigned(FOwner) and ((FOwner.FBoolData1 and ibfUseStyles) = ibfUseStyles) then
      FOwner.UpdateItem;
  end;
end;

{$endif}

{$ifdef ELTREE_USE_INPLACE_EDITORS}
{$ifdef VER3_EDITORS}

procedure TElTreeInplaceEditor.SetTree(Value: TCustomElTree);
begin
  if FTree <> Value then
  begin
    if Visible then CompleteOperation(false);
    if FTree <> nil then
      FTree.FEditorManager.UnregisterEditor(Self);
    FTree := Value;
    if FTree <> nil then
      FTree.FEditorManager.RegisterEditor(Self);
  end;
end;

procedure TElTreeInplaceEditor.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FTree then
      Tree := nil;

    if (FTree <> nil) and (AComponent = FTree.FEditorManager) then
    begin
      if Visible then
        CompleteOperation(false);
      FTree.FEditorManager.UnregisterEditor(Self);
      FTree := nil;
    end;
  end;
end;

procedure TElTreeInplaceEditor.CompleteOperation(Accepted : boolean);
var AcceptResults,
    DefaultConversion : boolean;
    AForm             : TForm;
begin
  FEditing := false;
  AcceptResults := Accepted;
  TriggerAfterOperation(AcceptResults, DefaultConversion);
  AForm := TForm(GetParentForm(FTree));
  CanReFocus := AForm.ActiveControl <> nil;
  DoStopOperation(true);
  if Accepted then
    FTree.FView.EditOperationAccepted
  else
    FTree.FView.EditOperationCancelled;
end;

procedure TElTreeInplaceEditor.SetEditorParent;
begin
  // default implementation does nothing
end;

procedure TElTreeInplaceEditor.StartOperation;
var DefaultConversion : boolean;
begin
  SetEditorParent;
  TriggerBeforeOperation(DefaultConversion);
  DoStartOperation();
  FEditing := true;
end;

procedure TElTreeInplaceEditor.TriggerBeforeOperation(var DefaultConversion : boolean);
begin
  DefaultConversion := true;
  if Assigned(FOnBeforeOperation) then
    FOnBeforeOperation(Self, DefaultConversion);
  if DefaultConversion and (ValueAsText = '') then
    FValueAsText := DefaultValueAsText;
end;

procedure TElTreeInplaceEditor.TriggerAfterOperation(var Accepted : boolean; var DefaultConversion : boolean);
begin
  DefaultConversion := true;
  if Assigned(FOnAfterOperation) then
    FOnAfterOperation(Self, Accepted, DefaultConversion);
end;

procedure TElTreeInplaceEditor.TriggerValidateResult(var InputValid : boolean);
begin
  if Assigned(FOnValidateResult) then FOnValidateResult(Self, InputValid);
end;

procedure TElTreeInplaceEditor.DoStopOperation(Accepted : boolean);
begin
  if FTree.View.CanFocus and FTree.CanFocus and FTree.Parent.CanFocus and (not (csDestroying in FTree.ComponentState)) then
  try
    if CanRefocus then
      FTree.View.SetFocus;
  except
  end;
end;

procedure TElTreeInplaceManager.RegisterEditor(Editor : TElTreeInplaceEditor);
begin
  if FEditorsList.IndexOf(Editor) = -1 then
  begin
    FEditorsList.Add(Editor);
    Editor.FreeNotification(Self);
  end;
end;

procedure TElTreeInplaceManager.UnregisterEditor(Editor : TElTreeInplaceEditor);
begin
  FEditorsList.Remove(Editor);
  {$ifdef VCL_5_USED}
  if not (csDestroying in Editor.ComponentState) then
    Editor.RemoveFreeNotification(Self);
  {$endif}
end;

constructor TElTreeInplaceManager.Create;
begin
  inherited Create(AOwner);
  FEditorsList := TElList.Create;
end;

destructor TElTreeInplaceManager.Destroy;
begin
  inherited;
  FEditorsList.Free;
  FEditorsList := nil;
end;

function TElTreeInplaceManager.GetSuitableEditor(SupposedFieldType : 
    TElFieldType): TElTreeInplaceEditor;
var i : integer;
    Editor : TElTreeInplaceEditor;
begin
  result := nil;
  for i := 0 to FEditorsList.Count - 1 do
  begin
    Editor := TElTreeInplaceEditor(FEditorsList[i]);
    if SupposedFieldType in Editor.Types then
    begin
      Result := Editor;
      exit;
    end;
  end;
end;

procedure TElTreeInplaceManager.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TElTreeInplaceEditor) then
  begin
    FEditorsList.Remove(AComponent);
  end;
end;

{$endif VER3_EDITORS}
{$endif ELTREE_USE_INPLACE_EDITORS}

{$ifndef CLX_USED}
var OleDLL : HModule;
{$endif}

initialization

{$ifndef CLX_USED}
  OleDLL := LoadLibrary('OLE32.DLL');
  Screen.Cursors[crDragSingleNo]   := LoadCursor(OleDLL, PChar(1));
  Screen.Cursors[crDragSingleMove] := LoadCursor(OleDLL, PChar(2));
  Screen.Cursors[crDragSingleCopy] := LoadCursor(OleDLL, PChar(3));
{$endif}

  PlusBmp := TBitmap.Create;
  MinusBmp := TBitmap.Create;
  LeafBmp := TBitmap.Create;
  PlusBmp.LoadFromResourceName(HInstance, 'PlusBmp');
  MinusBmp.LoadFromResourceName(HInstance, 'MinusBmp');
  LeafBmp.LoadFromResourceName(HInstance, 'LeafBmp');

{$ifndef CLX_USED}
  PlusBmp.PixelFormat := pf32bit;
  MinusBmp.PixelFormat := pf32bit;
  LeafBmp.PixelFormat := pf32bit;

  ChangeButtonColors(PlusBmp);
  ChangeButtonColors(MinusBmp);
  ChangeButtonColors(LeafBmp);

  PlusBmp.PixelFormat := pfDevice;
  MinusBmp.PixelFormat := pfDevice;
  LeafBmp.PixelFormat := pfDevice;
{$endif}

finalization

  MinusBmp.Free;
  PlusBmp.Free;
  LeafBmp.Free;
{$ifndef CLX_USED}
  FreeLibrary(OleDLL);
{$endif}

end.

