{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2001, EldoS                   }
{                                                    }
{====================================================}

(*

Version History

11/16/2001

  Added ElMouseHint and ElEdit

07/19/2001 (c) Akzhan Abdulin

  Image index property editors-descendants
  partially removed, partially rewritten
  (because of new TElImageIndexProperty functionality).

07/15/2001 (c) Akzhan Abdulin

  Abstract image index property editor initiated

  New image index property editors added for:
    TElSideBarSection.ImageIndex,
    TElSideBarItem.ImageIndex,
    TElToolButton.ImageIndex,
    TElGraphicButton.ImageIndex,
    TElPopupButton.ImageIndex,
    TElCurrencyEdit.ButtonImageIndex,
    TElImageIndexEdit.ImageIndex.

  TElImageIndexEdit visual component added to handle
  image index properties mainly inside design-time item editors.

*)

{$I 'ElPack.inc'}

{$ifndef LINUX}
{$ifdef CLX_USED}
{$R 'CLXDesign\ElHeader.dcr'}
{$R 'CLXDesign\ElTree.dcr'}
{$R 'CLXDesign\ElScrollBar.dcr'}
{$else}
{$R 'Design\ElHeader.dcr'}
{$R 'Design\ElTree.dcr'}
{$R 'Design\ElEdits.dcr'}
{$R 'Design\ElScrollBar.dcr'}
{$endif}
{$else}
{$R 'CLXDesign/ElHeader.dcr'}
{$R 'CLXDesign/ElTree.dcr'}
{$R 'CLXDesign/ElScrollBar.dcr'}
{$endif}

{$IFDEF ELPACK_COMPLETE}

{$ifndef CLX_USED}
{$R 'Design\ElCheckCtl.dcr'}
{$R 'Design\ElCheckGrp.dcr'}
{$R 'Design\ElCalendar.dcr'}
{$R 'Design\ElDTPick.dcr'}
{$else}
{$ifndef LINUX}
{$R 'CLXDesign\ElCheckCtl.dcr'}
{$R 'CLXDesign\ElCheckGrp.dcr'}
{$R 'CLXDesign\ElCalendar.dcr'}
{$R 'CLXDesign\ElDTPick.dcr'}
{$else}
{$R 'CLXDesign/ElCheckCtl.dcr'}
{$R 'CLXDesign/ElCheckGrp.dcr'}
{$R 'CLXDesign/ElCalendar.dcr'}
{$R 'CLXDesign/ElDTPick.dcr'}
{$endif}
{$endif}

{$ifndef CLX_USED}
  {$R 'Design\ElAdvPanel.dcr'}
{$endif}

{$ifndef CLX_USED}
  {$R 'Design\ElBtnEdit.dcr'}
  {$R 'Design\ElHotKey.dcr'}
{$endif}

{$ifndef CLX_USED}
  {$R 'Design\ElCalendDlg.dcr'}
  {$R 'Design\ElCaption.dcr'}
{$endif}


{$ifndef CLX_USED}
  {$R 'Design\ElScrollBox.dcr'}
  {$R 'Design\ElMenus.dcr'}
  {$R 'Design\ElClipMon.dcr'}
  {$R 'Design\ElClock.dcr'}
  {$R 'Design\ElClrCmb.dcr'}
  {$R 'Design\ElCurEdit.dcr'}
  {$R 'Design\ElColorMap.dcr'}
  {$R 'Design\ElDailyTip.dcr'}
  {$R 'Design\ElDragDrop.dcr'}
  {$R 'Design\ElDriveCombo.dcr'}
{$endif}

{$ifndef CLX_USED}
  {$R 'Design\ElFlatCtl.dcr'}
  {$R 'Design\ElFolderDlg.dcr'}
  {$R 'Design\ElFrmPers.dcr'}
{$endif}

{$ifndef LINUX}
{$ifndef CLX_USED}
{$R 'Design\ElGraphs.dcr'}
{$R 'Design\ElGroupBox.dcr'}
{$R 'Design\ElGauge.dcr'}
{$R 'Design\ElHstgrm.dcr'}
{$R 'Design\ElHook.dcr'}
{$else}
{$R 'CLXDesign\ElGraphs.dcr'}
{$R 'CLXDesign\ElGroupBox.dcr'}
{$R 'CLXDesign\ElGauge.dcr'}
{$R 'CLXDesign\ElHstgrm.dcr'}
{$R 'CLXDesign\ElHook.dcr'}
{$endif}
{$else}
{$R 'CLXDesign/ElGraphs.dcr'}
{$R 'CLXDesign/ElGroupBox.dcr'}
{$R 'CLXDesign/ElGauge.dcr'}
{$R 'CLXDesign/ElHstgrm.dcr'}
{$R 'CLXDesign/ElHook.dcr'}
{$endif}

{$ifndef CLX_USED}
  {$R 'Design\ElHTMLLbl.dcr'}
  {$R 'Design\ElHTMLPanel.dcr'}
  {$R 'Design\HTMLView.dcr'}
  {$R 'Design\HTMLLbx.dcr'}
  {$R 'Design\ElMouseHint.dcr'}
{$endif}


{$ifndef CLX_USED}
  {$R 'Design\ElImgLst.dcr'}
{$endif}

{$ifndef CLX_USED}
  {$R 'Design\ElExpBar.dcr'}
  {$R 'Design\ElInputDlg.dcr'}
  {$R 'Design\ElIpEdit.dcr'}
  {$R 'Design\ElLabel.dcr'}
  {$R 'Design\ElImgFrm.dcr'}
  {$R 'Design\ElCombos.dcr'}
  {$R 'Design\ElFontCombo.dcr'}
  {$R 'Design\ElMemoCombo.dcr'}
  {$R 'Design\ElMaskEdit.dcr'}
  {$R 'Design\ElNameEdits.dcr'}
  {$R 'Design\ElOneInst.dcr'}
  {$R 'Design\ElPrompt.dcr'}
  {$R 'Design\ElListBox.dcr'}
  {$R 'Design\ElPgCtl.dcr'}
  {$R 'Design\ElSpin.dcr'}
  {$R 'Design\ElSplit.dcr'}
  {$R 'Design\ElTimers.dcr'}
  {$R 'Design\ElTray.dcr'}
  {$R 'Design\ElTrackBar.dcr'}
  {$R 'Design\ElTrayInfo.dcr'}
  {$R 'Design\ElTreeCombo.dcr'}
  {$R 'Design\ElTreeGrids.dcr'}
  {$R 'Design\ElURLLabel.dcr'}
  {$R 'Design\ElVerInfo.dcr'}
{$endif}

{$ifndef LINUX}
{$ifndef CLX_USED}
{$R 'Design\ElMRU.dcr'}
{$R 'Design\HTMLHint.dcr'}
{$R 'Design\ElIni.dcr'}
{$R 'Design\ElPanel.dcr'}
{$R 'Design\ElPopBtn.dcr'}
{$R 'Design\ElRadioGrp.dcr'}
{$R 'Design\ElSideBar.dcr'}
{$R 'Design\ElSndMap.dcr'}
{$R 'Design\ElSpinBtn.dcr'}
{$R 'Design\ElStatBar.dcr'}
{$R 'Design\ElStrPool.dcr'}
{$R 'Design\ElToolBar.dcr'}
{$R 'Design\ElTreeInplaceEditors.dcr'}
{$R 'Design\ElBiProgr.dcr'}
{$else}
{$R 'CLXDesign\ElMRU.dcr'}
{$R 'CLXDesign\HTMLHint.dcr'}
{$R 'CLXDesign\ElIni.dcr'}
{$R 'CLXDesign\ElPanel.dcr'}
{$R 'CLXDesign\ElPopBtn.dcr'}
{$R 'CLXDesign\ElRadioGrp.dcr'}
{$R 'CLXDesign\ElSideBar.dcr'}
{$R 'CLXDesign\ElSndMap.dcr'}
{$R 'CLXDesign\ElSpinBtn.dcr'}
{$R 'CLXDesign\ElStatBar.dcr'}
{$R 'CLXDesign\ElStrPool.dcr'}
{$R 'CLXDesign\ElToolBar.dcr'}
{$R 'CLXDesign\ElTreeInplaceEditors.dcr'}
{$R 'CLXDesign\ElBiProgr.dcr'}
{$endif}
{$else}
{$R 'CLXDesign/ElMRU.dcr'}
{$R 'CLXDesign/HTMLHint.dcr'}
{$R 'CLXDesign/ElIni.dcr'}
{$R 'CLXDesign/ElPanel.dcr'}
{$R 'CLXDesign/ElPopBtn.dcr'}
{$R 'CLXDesign/ElRadioGrp.dcr'}
{$R 'CLXDesign/ElSideBar.dcr'}
{$R 'CLXDesign/ElSndMap.dcr'}
{$R 'CLXDesign/ElSpinBtn.dcr'}
{$R 'CLXDesign/ElStatBar.dcr'}
{$R 'CLXDesign/ElStrPool.dcr'}
{$R 'CLXDesign/ElToolBar.dcr'}
{$R 'CLXDesign/ElTreeInplaceEditors.dcr'}
{$R 'CLXDesign/ElBiProgr.dcr'}
{$endif}

{$ENDIF}

unit ElReg;

interface

procedure Register;

implementation

uses
  Classes,
  SysUtils,
  {$ifndef CLX_USED}
  Forms,
  Controls,
  {$else}
  QForms,
  QControls,
  Types,
  QTypes,
  {$endif}
{$ifndef CLX_USED}
{$ifdef VCL_6_USED}
  DesignEditors, DesignWindows, DsnConst, DesignIntf,
{$else}
  DsgnIntf,
{$endif}
{$else}
  DesignEditors, DesignWindows, DsnConst, DesignIntf,
{$endif}
  ElHeader,
  ElTree,
  ElScrollBar,
  {$ifndef CLX_USED}
  ElTreeGrids,
  {$endif}
  ElTreeStdEditors,
  {$ifndef CLX_USED}
  ElTreeModalEdit,
  {$endif}
  ElStrUtils
{$IFDEF ELPACK_COMPLETE}
{$ifndef CLX_USED}
  , ElTreeAdvEdit, ElTreeBtnEdit, ElTreeCheckBoxEdit, ElTreeComboBox,
  ElTreeCurrEdit, ElTreeDTPickEdit, ElTreeMaskEdit,
  ElTreeMemoEdit, ElTreeSpinEdit, ElTreeTreeComboEdit
{$endif}
{$ENDIF}

{$IFDEF ELPACK_COMPLETE}

{$ifndef CLX_USED}
{$IFDEF D_5_UP}
  , ImgList
{$ENDIF}
{$endif}

{$IFDEF D_5_UP}
  , ElImageIndexProp
{$ENDIF}
  ,
  {$ifndef CLX_USED}
  ElACtrls,
  ElAppBar,
  {$endif}
  ElBiProgr,
  {$ifndef CLX_USED}
  ElBtnEdit,
  ElHotKey,
  {$endif}
  ElCalendar,
  {$ifndef CLX_USED}
  ElCalendDlg,
  ElCaption,
  {$endif}
  ElCheckCtl,
  {$ifndef CLX_USED}
  ElClrCmb,
  ElClock,
  ElColorMap,
  ElDailyTip,
  ElEdits,
  ElInputDlg,
  ElDragDrop,
  ElFlatCtl,
  ElFrmPers,
  {$endif}
  ElGraphs,
  ElHook,
  ElHstgrm,
  ElGauge,
  {$ifndef CLX_USED}
  ElImgLst,
  {$endif}
  ElIni,
  {$ifndef CLX_USED}
  ElMemoCombo,
  {$endif}
  ElMRU,
  {$ifndef CLX_USED}
  ElOneInst,
  {$endif}
  ElPanel,
  ElPopBtn,
  {$ifndef CLX_USED}
  ElFileUtils,
  {$endif}
  ElCheckItemGrp,
  {$ifndef CLX_USED}
  ElImgCombo,
  ElTrackBar,
  ElDriveCombo,
  ElFontCombo,
  ElScrollBox,
  ElExpBar,
  ElEBDsgn,

  ElListBox,
  {$endif}
  ElSideBar,
  ElSndMap,
  ElSpinBtn,
  {$ifndef CLX_USED}
  ElSpin,
  {$endif}
  ElStatBar,
  ElToolBar,
  ElTBDsgn,

  {$ifndef CLX_USED}
  ElTray,
  ElTreeCombo,
  ElVerInfo,
  ElURLLabel,
  ElTimers,
  ElMouseHint,
  ElSplit,
  frmFormPers,
  ElPgCtl,
  PgCtlProp,
  frmSoundMap,
  ElImgFrm,
  ElMaskEdit,
  ElNameEdits,
  ElAdvPanel,
  ElHTMLPanel,
  {$endif}

{$ifndef CLX_USED}
{$ifndef BUILDER_USED}
  ColnProp,
{$endif}
{$endif}

{$ifndef CLX_USED}
{$ifndef VCL_6_USED}
  FormCtlProp,
{$endif}
  {$endif}
  {$ifndef CLX_USED}
  ColorMapProp,
  ElCombos,
  {$endif}
  ElStrPool,
  ElStrArray,
  {$ifndef CLX_USED}
  frmStrPoolEdit,
  ElPromptDlg,
  ElLabel,
  ElFolderDlg,
  ElTrayInfo,
  ElHTMLLbl,
  {$endif}
  ElHTMLHint,
  {$ifndef CLX_USED}
  HTMLLbx,
  ElHTMLView,
  {$endif}

  ElDTPick,
  ElGroupBox,

  Menus,
  ElMenus,
  ElMenuDsgn,

  {$ifndef CLX_USED}
  ElClipMon,
  {$endif}
  MlCapProp
  {$ifndef CLX_USED}
  , ElIPEdit,
  ElCurrEdit
  {$endif}
{$ENDIF}

{$ifdef ELPACK_UNICODE}
{$ifndef BROKEN_UNICODE}
  , ElUnicodeStrings
{$endif}
{$endif}
  , TreeDsgn, frmItemsProp, frmSectProp;

{$IFDEF ELPACK_COMPLETE}
type
  TMRUCollectionEditor = class(TDefaultEditor)
  public
{$ifdef VCL_6_USED}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
{$else}
    procedure EditProperty(PropertyEditor : TPropertyEditor; var Continue, FreeEditor : Boolean); override;
{$endif}
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

{$ifndef CLX_USED}
  TTimerPoolEditor = class(TDefaultEditor)
  public
{$ifdef VCL_6_USED}
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
{$else}
    procedure EditProperty(PropertyEditor : TPropertyEditor; var Continue, FreeEditor : Boolean); override;
{$endif}
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;
{$endif}

{$ifdef VCL_6_USED}
procedure TMRUCollectionEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
{$else}
procedure TMRUCollectionEditor.EditProperty(PropertyEditor : TPropertyEditor; var Continue, FreeEditor : Boolean);
{$endif}
var
  PropName : string;
begin
{$ifdef VCL_6_USED}
  PropName := Prop.GetName;
  if (CompareText(PropName, 'SECTIONS') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
{$else}
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'SECTIONS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
{$endif}  
end;

procedure TMRUCollectionEditor.ExecuteVerb(Index: integer);
begin
  Edit;
end;

function TMRUCollectionEditor.GetVerb(Index: integer): string;
begin
  Result := 'Se&ctions...';
end;

function TMRUCollectionEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

{ TTimerPoolEditor }

{$ifndef CLX_USED}
{$ifdef VCL_6_USED}
procedure TTimerPoolEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
{$else}
procedure TTimerPoolEditor.EditProperty(PropertyEditor : TPropertyEditor; var Continue, FreeEditor : Boolean);
{$endif}
var
  PropName : string;
begin
{$ifdef VCL_6_USED}
  PropName := Prop.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
{$else}
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
{$endif}
end;

procedure TTimerPoolEditor.ExecuteVerb(Index: integer);
begin
  Edit;
end;

function TTimerPoolEditor.GetVerb(Index: integer): string;
begin
  Result := 'Ti&mers...';
end;

function TTimerPoolEditor.GetVerbCount: integer;
begin
  Result := 1;
end;
{$ENDIF}
{$endif}

{$IFDEF ELPACK_COMPLETE}
{$IFDEF D_5_UP}

type
  TElSideBarCItemHack = class(TElSideBarCItem);

{$ifndef CLX_USED}
{$IFDEF D_5_UP}
type
  TElSideBarSectionImageIndexProperty = class(TElImageIndexProperty)
  protected
    function GetImages: TCustomImageList; override;
  end;

{ TElSideBarSectionImageIndexProperty }

function TElSideBarSectionImageIndexProperty.GetImages: TCustomImageList;
var
  SideBarItem: TElSideBarCItemHack;
  SideBar: TElSideBar;
  Component: TPersistent;
begin
  Result := nil;
  Component := GetComponent(0);
  if not (Assigned(Component) and (Component is TElSideBarSection)) then Exit;
  SideBarItem := TElSideBarCItemHack(Component);
  SideBar := SideBarItem.GetBar;
  if Assigned(SideBar) then
  begin
    Result := SideBar.SectionImages;
  end;
end;

type
  TElSideBarItemImageIndexProperty = class(TElImageIndexProperty)
  protected
    function GetImages: TCustomImageList; override;
  end;

{ TElSideBarItemImageIndexProperty }

function TElSideBarItemImageIndexProperty.GetImages: TCustomImageList;
var
  SideBarItem: TElSideBarCItemHack;
  SideBar: TElSideBar;
  Component: TPersistent;
begin
  Result := nil;
  Component := GetComponent(0);
  if not (Assigned(Component) and (Component is TElSideBarItem)) then Exit;
  SideBarItem := TElSideBarCItemHack(Component);
  SideBar := SideBarItem.GetBar;
  if Assigned(SideBar) then
  begin
    Result := SideBar.ItemImages;
  end;
end;

type
  TElToolButtonImageIndexProperty = class(TElImageIndexProperty)
  protected
    function GetImages: TCustomImageList; override;
  end;

{ TElToolButtonImageIndexProperty }

function TElToolButtonImageIndexProperty.GetImages: TCustomImageList;
var
  ToolButton: TElToolButton;
  Component: TPersistent;
begin
  Result := nil;
  Component := GetComponent(0);
  if not (Assigned(Component) and (Component is TElToolButton)) then Exit;
  ToolButton := TElToolButton(Component);
  if Assigned(ToolButton.Parent) and (ToolButton.Parent is TElToolBar) then
  begin
    Result := TElToolBar(ToolButton.Parent).Images;
  end;
end;

type
  TElCurrencyEditButtonImageIndexProperty = class(TElImageIndexProperty)
  protected
    function GetImagesPropertyName: String; override;
  end;

{ TElCurrencyEditButtonImageIndexProperty }

function TElCurrencyEditButtonImageIndexProperty.GetImagesPropertyName: String;
begin
  Result := 'ButtonImages';
end;

{$endif}

{$endif}

{$ENDIF}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('EldoS', [TElHeader, TElTree{$ifndef CLX_USED}, TElTreeStringGrid{$endif}]);

  RegisterComponents('ElTree Editors', [TElTreeInplaceEdit, TElTreeInplaceMemo,
  TElTreeInplaceCheckBox
  {$ifndef CLX_USED}
  , TElTreeInplaceComboBox,
  TElTreeInplaceDateTimePicker,
  TElTreeInplaceModalEdit
  {$endif}]);

{$ifndef CLX_USED}
{$IFDEF ELPACK_COMPLETE}
  RegisterComponents('ElTree Editors', [TElTreeInplaceAdvancedEdit,
  TElTreeInplaceButtonEdit, TElTreeInplaceCheckBoxEdit, TElTreeInplaceComboBox,
  TElTreeInplaceCurrencyEdit, TElTreeInplaceDateTimePicker, TElTreeInplaceMaskEdit,
  TElTreeInplaceMemoEdit, TElTreeInplaceSpinEdit,
  TElTreeInplaceFloatSpinEdit, TElTreeInplaceTreeComboEdit]);
{$ENDIF}
{$endif}

{$IFDEF ELPACK_COMPLETE}
  RegisterComponents('EldoS Edit', [TElDateTimePicker
    {$ifndef CLX_USED}
    , TElAdvancedEdit, TElAdvancedMemo, TElEdit, TElButtonEdit,
    TElSpinEdit, TElFloatSpinEdit, TElMaskEdit, TElCurrencyEdit, TElIPEdit, 
    TElFileNameEdit, TElFolderNameEdit, TElHotKey
     {$endif}]);
  
  {$ifndef CLX_USED}
  RegisterComponents('EldoS List && Combo', [TElColorCombo,
    TElListBox, TElComboBox, TElDriveComboBox, TElAdvancedComboBox,
    TElAdvancedListBox, TElImageComboBox, TElTreeCombo,
    TElMemoCombo, TElFontComboBox]);
  {$endif}
    
  RegisterComponents('EldoS', [TElScrollBar, TElPanel, TElCalendar, TElGroupBox, TElCheckBox,
    TElRadioButton, TElCheckGroup, TElRadioGroup, TElToolBar, TElStatusBar, TElSideBar, TElBiProgressBar,
    TElSpinButton, TElPopupButton, TElGraphicButton, TElGauge, TElHistogram, TElGraph

    {$ifndef CLX_USED}
    , TElPageControl, TElTrackBar, TElSplitter, TElURLLabel, TElDragDrop,
    TElSpeedButton,  TElClock, TElLabel, TElScrollBox, TElExplorerBar
    {$endif}
  ]);

  RegisterComponents('EldoS Tools', [TElIniFile, TElMRU, TElStringPool
    {$ifndef CLX_USED}
    , TElOneInstance, TElTrayIcon, TElFormCaption, TElFormPersist,
    TElImageList, TElVersionInfo, TElSoundMap,
    TElColorMap, TElMouseHint,
    TElDropTarget, TElHook, TElTimerPool, 
    TElImageForm, TElTrayInfo, 
    TElClipboardMonitor, TElFlatController, TElFlatMultiController
    {$endif}
    ]);

  {$ifndef CLX_USED}
  RegisterComponents('EldoS Dialogs', [TElPromptDialog, TElFolderDialog,
    TElDailyTipDialog, TElCalendarDialog, TElInputDialog]);
  {$endif}

  RegisterComponents('EldoS HTML', [TElHTMLHint
    {$ifndef CLX_USED}
    , TElAdvancedPanel, TElHTMLLabel, TElHTMLListBox,
    TElHTMLComboBox, TElHTMLView, TElHTMLPanel{$endif}
    ]);

  RegisterNoIcon([TElToolButton]);
  Classes.RegisterClass(TElToolButton);
  RegisterClass(TElTreeInplaceEditor);

  {$ifndef CLX_USED}
  RegisterPropertyEditor(TypeInfo(TElColorEntries), TElColorMap, 'ItemsList',
    TColorMapItemsProperty);
  RegisterComponentEditor(TElExplorerBar, TElExplorerBarEditor);
  RegisterNoIcon([TElExplorerBarGroup]);
  RegisterClass(TElExplorerBarGroup);
{$ifndef VCL_6_USED}
  RegisterPropertyEditor(TypeInfo(TWinControl), TElHook, 'Control',
    TFormCtlProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TElDropTarget, 'Target',
    TFormCtlProperty);
{$endif}
  {$endif}
  RegisterComponentEditor(TElToolBar, TElToolBarEditor);

{$ifdef ELPACK_UNICODE}
{$ifndef BROKEN_UNICODE}
  RegisterPropertyEditor(TypeInfo(TElWideStrings), nil, 'Items',
    TElWideStringsProperty);
{$endif}
{$endif}

{$ifndef B_5_UP}
  RegisterPropertyEditor(TypeInfo(TElFString), nil, 'Caption',
    TMlCaptionProperty);
  RegisterPropertyEditor(TypeInfo(TElFString), nil, 'Hint',
    TMlCaptionProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), nil, 'Caption',
    TMlCaptionProperty);
{$ifdef ELPACK_UNICODE}
{$ifndef BROKEN_UNICODE}
  RegisterPropertyEditor(TypeInfo(WideString), nil, 'Hint',
    TMlCaptionProperty);
{$endif}
{$endif}
  RegisterPropertyEditor(TypeInfo(string), nil, 'Hint',
    TMlCaptionProperty);
{$endif}

{$ifndef CLX_USED}
  RegisterPropertyEditor(TypeInfo(TElStoredProps), TElFormPersist,
    'StoredProps', TPropListProperty);
  RegisterPropertyEditor(TypeInfo(TElSoundName), nil, '', TSoundNameProperty);
  RegisterPropertyEditor(TypeInfo(TElTreeItems), TElTreeCombo, 'Items',
    TElTreeItemsProperty);
{$ifndef VCL_6_USED}
 RegisterPropertyEditor(TypeInfo(String), TElTrayIcon, 'ExtendedHint',
    TFormProperty);
{$endif}
{$endif}

{$ifndef CLX_USED}
  RegisterComponentEditor(TElStringPool, TStrPoolItemsEditor);
{$ifdef ELPACK_UNICODE}
  RegisterPropertyEditor(TypeInfo(TElWideStringArray), nil,
    'Items', TStrPoolItemsProperty);
{$endif}
  RegisterPropertyEditor(TypeInfo(TElStringArray), nil,
    'Items', TStrPoolItemsProperty);

  RegisterComponentEditor(TElFormPersist, TElPropListEditor);
  RegisterComponentEditor(TElMRU, TMRUCollectionEditor);
  RegisterComponentEditor(TElSoundMap, TSoundMapEditor);
  RegisterComponentEditor(TElTimerPool, TTimerPoolEditor);

  RegisterComponentEditor(TElPageControl, TElPageControlEditor);
  RegisterComponentEditor(TElTabSheet, TElPageControlEditor);
  RegisterPropertyEditor(TypeInfo(TElTabSheet), TElPageControl, 'ActivePage',
    TElActivePageProperty);
  RegisterClass(TElTabSheet);

  RegisterComponents('EldoS', [TElMainMenu, TElPopupMenu]);
  RegisterNoIcon([TElMenuItem]);
  RegisterClasses([TElMenuItem, TElMainMenu, TElPopupMenu]);
  RegisterComponentEditor(TElMainMenu, TElMenuEditor);
  RegisterComponentEditor(TElPopupMenu, TElMenuEditor);
  RegisterPropertyEditor(TypeInfo(TMenuItem), TElMainMenu, 'Items', TElMenuItemsProperty);
  RegisterPropertyEditor(TypeInfo(TMenuItem), TElPopupMenu, 'Items', TElMenuItemsProperty);
  RegisterCustomModule(TElAppBar, TCustomModule);
{$ENDIF}
{$endif}
{$ifndef CLX_USED}
{$ifndef BUILDER_USED}
  RegisterComponentEditor(TElStatusBar, TElStatusBarEditor);
  RegisterComponentEditor(TElSideBar, TElSideBarEditor);
{$endif}
{$endif}

  RegisterComponentEditor(TElTree, TElTreeEditor);
  RegisterComponentEditor(TElHeader, TElHeaderEditor);
  RegisterPropertyEditor(TypeInfo(TElTreeItems), nil, 'Items',
    TElTreeItemsProperty);
  RegisterPropertyEditor(TypeInfo(TElHeaderSections), nil, 'Sections',
    TElSectionsProperty);

{$ifdef KYLIX_USED}
  GroupDescendentsWith(TElHTMLHint, TWidgetControl);
  GroupDescendentsWith(TElTreeInplaceEditor, TWidgetControl);
  GroupDescendentsWith(TElMRU, TWidgetControl);
{$else}
{$ifdef VCL_6_USED}
{$IFDEF CLX_USED}
  GroupDescendentsWith(TElHTMLHint, TWidgetControl);
  GroupDescendentsWith(TElTreeInplaceEditor, TWidgetControl);
  GroupDescendentsWith(TElMRU, TWidgetControl);
{$else}
  GroupDescendentsWith(TElHTMLHint, TWinControl);
  GroupDescendentsWith(TElTreeInplaceEditor, TWinControl);
  GroupDescendentsWith(TElMRU, TWinControl);
{$ENDIF}
{$endif}
{$endif}

{$ifndef CLX_USED}
{$IFDEF ELPACK_COMPLETE}
{$IFDEF D_5_UP}
  RegisterPropertyEditor(TypeInfo(Integer), TElSideBarSection, 'ImageIndex', TElSideBarSectionImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TElSideBarItem, 'ImageIndex', TElSideBarItemImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TElToolButton, 'ImageIndex', TElToolButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TElPopupButton, 'ImageIndex', TElImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TElGraphicButton, 'ImageIndex', TElImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TElImageComboBox, 'ImageIndex', TElImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TElTabSheet, 'ImageIndex', TElTabSheetImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TElCurrencyEdit, 'ButtonImageIndex', TElCurrencyEditButtonImageIndexProperty);
{$ENDIF}
{$ENDIF}
{$endif}
end;

end.
