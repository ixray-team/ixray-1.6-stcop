unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElImgFrm, ElPgCtl, ElTree, ElHTMLView, ElPromptDlg, ElShellUtils,
  ExtCtrls, ElPanel, ElImgLst, StdCtrls, ElTreeGrids, ElBtnCtl,
  ElCheckCtl, ElTreeAdvEdit, ElTreeSpinEdit, ElTreeDTPickEdit, ElVCLUtils,
  ElTreeCheckBoxEdit, ElTreeCurrEdit, ElDTPick, ElTreeCombo, ElHeader,
  ElGroupBox, ElACtrls, ElSpin, ElPopBtn, ElBtnEdit, ElStrPool,
  ElTools, ElXPThemedControl, ImgList;

type
  TfrmMain = class(TForm)
    NavigationTree: TElTree;
    MainPages: TElPageControl;
    tabElTree: TElTabSheet;
    tabElPageControl: TElTabSheet;
    tabElPack: TElTabSheet;
    tabReplacement: TElTabSheet;
    tabEnhancement: TElTabSheet;
    tabOriginal: TElTabSheet;
    tabHTML: TElTabSheet;
    tabVisible: TElTabSheet;
    tabDialogs: TElTabSheet;
    tabCustomization: TElTabSheet;
    tabDB: TElTabSheet;
    tabElTreeStringGrid: TElTabSheet;
    tabElHeader: TElTabSheet;
    tabElStatusBar: TElTabSheet;
    tabElToolBar: TElTabSheet;
    tabElPopupButton: TElTabSheet;
    tabElCheckbox: TElTabSheet;
    tabElRadioButton: TElTabSheet;
    tabElGroupBox: TElTabSheet;
    tabElRadioGroup: TElTabSheet;
    tabElCheckGroup: TElTabSheet;
    tabElScrollBar: TElTabSheet;
    tabElTrackBar: TElTabSheet;
    tabElDateTimePicker: TElTabSheet;
    tabElImageList: TElTabSheet;
    tabElPanel: TElTabSheet;
    tabElLabel: TElTabSheet;
    tabElAdvancedEdit: TElTabSheet;
    tabElAdvancedMemo: TElTabSheet;
    tabElAdvancedListBox: TElTabSheet;
    tabElAdvancedComboBox: TElTabSheet;
    tabElAdvancedPanel: TElTabSheet;
    tabElMaskEdit: TElTabSheet;
    tabElCalendar: TElTabSheet;
    tabElTreeCombo: TElTabSheet;
    tabElButtonEdit: TElTabSheet;
    tabElSpinEdit: TElTabSheet;
    tabElFloatSpinEdit: TElTabSheet;
    tabElIPEdit: TElTabSheet;
    tabElCurrencyEdit: TElTabSheet;
    tabElSideBar: TElTabSheet;
    tabElClock: TElTabSheet;
    tabElColorCombo: TElTabSheet;
    tabElMemoCombo: TElTabSheet;
    tabElDriveComboBox: TElTabSheet;
    tabElFontComboBox: TElTabSheet;
    tabElImageComboBox: TElTabSheet;
    tabElSplitter: TElTabSheet;
    tabElHTMLHint: TElTabSheet;
    tabElHTMLLabel: TElTabSheet;
    tabElHTMLListBox: TElTabSheet;
    tabElHTMLComboBox: TElTabSheet;
    tabElHTMLView: TElTabSheet;
    tabElHTMLPanel: TElTabSheet;
    tabElMRU: TElTabSheet;
    tabElFormCaption: TElTabSheet;
    tabElTrayIcon: TElTabSheet;
    tabElTrayInfo: TElTabSheet;
    tabElImageForm: TElTabSheet;
    tabDailyTipDialog: TElTabSheet;
    tabElCalendarDialog: TElTabSheet;
    tabElPromptDialog: TElTabSheet;
    tabElInputDialog: TElTabSheet;
    tabElFolderDialog: TElTabSheet;
    tabElVersionInfo: TElTabSheet;
    tabElIniFile: TElTabSheet;
    tabElTimerPool: TElTabSheet;
    tabElFormPersist: TElTabSheet;
    tabElOneInstance: TElTabSheet;
    tabElHook: TElTabSheet;
    tabElSoundMap: TElTabSheet;
    tabElColorMap: TElTabSheet;
    tabShellControls: TElTabSheet;
    tabElShellList: TElTabSheet;
    tabElShellTree: TElTabSheet;
    tabElShellComboBox: TElTabSheet;
    ElHTMLView9: TElHTMLView;
    ComponentGlyphs: TElImageList;
    ElPageControlPages: TElPageControl;
    tabElPageControlDescription: TElTabSheet;
    tabElPageControlDemo: TElTabSheet;
    Label1: TLabel;
    ElTreeStringGridPages: TElPageControl;
    tabElTreeStringGridDescription: TElTabSheet;
    tabElTreeStringGridDemo: TElTabSheet;
    ElHeaderPages: TElPageControl;
    tabElHeaderDescription: TElTabSheet;
    tabElHeaderDemo: TElTabSheet;
    FloatHTMLView: TElHTMLView;
    DescriptionPool: TElStringPool;
    ElTreePages: TElPageControl;
    tabElTreeDescription: TElTabSheet;
    tabElTreeDemo: TElTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    Label2: TLabel;
    ElStatusBarPages: TElPageControl;
    tabElStatusBarDescription: TElTabSheet;
    tabElStatusBarDemo: TElTabSheet;
    Label3: TLabel;
    ElToolBarPages: TElPageControl;
    tabElToolBarDescription: TElTabSheet;
    tabElToolbarDemo: TElTabSheet;
    Label4: TLabel;
    ElPopupButtonPages: TElPageControl;
    tabElPopupButtonDescription: TElTabSheet;
    tabElPopupButtonDemo: TElTabSheet;
    Label7: TLabel;
    ElCheckBoxPages: TElPageControl;
    tabElCheckBoxDescription: TElTabSheet;
    tabElCheckBoxDemo: TElTabSheet;
    Label8: TLabel;
    ElRadioButtonPages: TElPageControl;
    tabElRadioButtonDescription: TElTabSheet;
    tabElRadioButtonDemo: TElTabSheet;
    Label9: TLabel;
    ElGroupBoxPages: TElPageControl;
    tabElGroupBoxDescription: TElTabSheet;
    tabElGroupBoxDemo: TElTabSheet;
    Label10: TLabel;
    ElRadioGroupPages: TElPageControl;
    tabElRadioGroupDescription: TElTabSheet;
    tabElRadioGroupDemo: TElTabSheet;
    Label11: TLabel;
    ElCheckGroupPages: TElPageControl;
    tabElCheckGroupDescription: TElTabSheet;
    tabElCheckGroupDemo: TElTabSheet;
    Label12: TLabel;
    ElScrollBarPages: TElPageControl;
    tabElScrollBarDescription: TElTabSheet;
    tabElScrollBarDemo: TElTabSheet;
    Label13: TLabel;
    tabHTMLFormatting: TElTabSheet;
    ElTrackBarPages: TElPageControl;
    tabElTrackBarDescription: TElTabSheet;
    tabElTrackBarDemo: TElTabSheet;
    Label14: TLabel;
    ElDateTimePickerPages: TElPageControl;
    tabElDateTimePickerDescription: TElTabSheet;
    tabElDateTimePickerDemo: TElTabSheet;
    Label15: TLabel;
    ElImageListPages: TElPageControl;
    tabElImageListDescription: TElTabSheet;
    tabElImageListDemo: TElTabSheet;
    Label16: TLabel;
    ElPanelPages: TElPageControl;
    tabElPanelDescription: TElTabSheet;
    tabElPanelDemo: TElTabSheet;
    Label17: TLabel;
    tabElLabelDescription: TElPageControl;
    ElTabSheet3: TElTabSheet;
    tabElLabelDemo: TElTabSheet;
    Label18: TLabel;
    ElAdvancedEditPages: TElPageControl;
    tabElAdvancedDemoDescription: TElTabSheet;
    tabElAdvancedEditDemo: TElTabSheet;
    Label19: TLabel;
    ElAdvancedMemoPages: TElPageControl;
    tabElAdvancedMemoDescription: TElTabSheet;
    tabElAdvancedMemoDemo: TElTabSheet;
    Label20: TLabel;
    ElAdvancedListBoxPages: TElPageControl;
    tabElAdvancedListBoxDescription: TElTabSheet;
    tabElAdvancedListBoxDemo: TElTabSheet;
    Label21: TLabel;
    ElAdvancedComboBoxPages: TElPageControl;
    tabElAdvancedComboBoxDescription: TElTabSheet;
    tabElAdvancedComboBoxDemo: TElTabSheet;
    Label22: TLabel;
    ElAdvancedPanelPages: TElPageControl;
    tabElAdvancedPanelDescription: TElTabSheet;
    tabElAdvancedPanelDemo: TElTabSheet;
    Label23: TLabel;
    ElMaskEditPages: TElPageControl;
    tabElMaskEditDescription: TElTabSheet;
    tabElMaskEditDemo: TElTabSheet;
    Label24: TLabel;
    ElCalendarPages: TElPageControl;
    tabElCalendarDescription: TElTabSheet;
    tabElCalendarDemo: TElTabSheet;
    Label25: TLabel;
    ElTreeComboPages: TElPageControl;
    tabElTreeComboDescription: TElTabSheet;
    tabElTreeComboDemo: TElTabSheet;
    Label26: TLabel;
    ElButtonEditPages: TElPageControl;
    tabElButtonEditDescription: TElTabSheet;
    tabElButtonEditDemo: TElTabSheet;
    Label27: TLabel;
    ElSpinEditPages: TElPageControl;
    tabElSpinEditDescription: TElTabSheet;
    tabElSpinEditDemo: TElTabSheet;
    Label28: TLabel;
    ElFloatSpinEditPages: TElPageControl;
    tabElFloatSpinEditDescription: TElTabSheet;
    tabElFloatSpinEditDemo: TElTabSheet;
    Label29: TLabel;
    ElIPEditPages: TElPageControl;
    tabElIPEditDescription: TElTabSheet;
    tabElIPEditDemo: TElTabSheet;
    Label30: TLabel;
    ElCurrencyEditPages: TElPageControl;
    tabElCurrencyEditDescription: TElTabSheet;
    tabElCurrencyEditDemo: TElTabSheet;
    Label31: TLabel;
    ElSideBarPages: TElPageControl;
    tabElSideBarDescription: TElTabSheet;
    tabElSideBarDemo: TElTabSheet;
    Label32: TLabel;
    ElClockPages: TElPageControl;
    tabElClockDescription: TElTabSheet;
    tabElClockDemo: TElTabSheet;
    Label33: TLabel;
    ElColorComboPages: TElPageControl;
    tabElColorComboDescription: TElTabSheet;
    tabElColorComboDemo: TElTabSheet;
    Label34: TLabel;
    ElMemoComboPages: TElPageControl;
    tabElMemoComboDescription: TElTabSheet;
    tabElMemoComboDemo: TElTabSheet;
    Label35: TLabel;
    ElDriveComboBoxPages: TElPageControl;
    tabElDriveComboBoxDescription: TElTabSheet;
    tabElDriveComboBoxDemo: TElTabSheet;
    Label36: TLabel;
    ElfontComboBoxPages: TElPageControl;
    tabElFontComboBoxDescription: TElTabSheet;
    tabElFontComboBoxDemo: TElTabSheet;
    Label37: TLabel;
    ElImageComboBoxPages: TElPageControl;
    tabElImageComboBoxDescription: TElTabSheet;
    tabElImageComboBoxDemo: TElTabSheet;
    Label38: TLabel;
    ElSplitterPages: TElPageControl;
    ElTabSheet1: TElTabSheet;
    ElTabSheet2: TElTabSheet;
    Label39: TLabel;
    ElPageControl1: TElPageControl;
    ElTabSheet4: TElTabSheet;
    ElTabSheet5: TElTabSheet;
    Label40: TLabel;
    ElPageControl2: TElPageControl;
    ElTabSheet6: TElTabSheet;
    ElTabSheet7: TElTabSheet;
    Label41: TLabel;
    ElPageControl3: TElPageControl;
    ElTabSheet8: TElTabSheet;
    ElTabSheet9: TElTabSheet;
    Label42: TLabel;
    ElPageControl4: TElPageControl;
    ElTabSheet10: TElTabSheet;
    ElTabSheet11: TElTabSheet;
    Label43: TLabel;
    ElPageControl5: TElPageControl;
    ElTabSheet12: TElTabSheet;
    ElTabSheet13: TElTabSheet;
    Label44: TLabel;
    ElPageControl6: TElPageControl;
    ElTabSheet14: TElTabSheet;
    ElTabSheet15: TElTabSheet;
    Label45: TLabel;
    ElPageControl7: TElPageControl;
    ElTabSheet16: TElTabSheet;
    ElTabSheet17: TElTabSheet;
    Label46: TLabel;
    ElPageControl8: TElPageControl;
    ElTabSheet18: TElTabSheet;
    ElTabSheet19: TElTabSheet;
    Label47: TLabel;
    ElPageControl9: TElPageControl;
    ElTabSheet20: TElTabSheet;
    ElTabSheet21: TElTabSheet;
    Label48: TLabel;
    ElPageControl10: TElPageControl;
    ElTabSheet22: TElTabSheet;
    ElTabSheet23: TElTabSheet;
    Label49: TLabel;
    ElPageControl11: TElPageControl;
    ElTabSheet24: TElTabSheet;
    ElTabSheet25: TElTabSheet;
    Label50: TLabel;
    ElPageControl12: TElPageControl;
    ElTabSheet26: TElTabSheet;
    ElTabSheet27: TElTabSheet;
    Label51: TLabel;
    ElPageControl13: TElPageControl;
    ElTabSheet28: TElTabSheet;
    ElTabSheet29: TElTabSheet;
    Label52: TLabel;
    ElPageControl14: TElPageControl;
    ElTabSheet30: TElTabSheet;
    ElTabSheet31: TElTabSheet;
    Label53: TLabel;
    ElPageControl15: TElPageControl;
    ElTabSheet32: TElTabSheet;
    ElTabSheet33: TElTabSheet;
    Label54: TLabel;
    ElPageControl16: TElPageControl;
    ElTabSheet34: TElTabSheet;
    ElTabSheet35: TElTabSheet;
    Label55: TLabel;
    ElPageControl17: TElPageControl;
    ElTabSheet36: TElTabSheet;
    ElTabSheet37: TElTabSheet;
    Label56: TLabel;
    ElPageControl18: TElPageControl;
    ElTabSheet38: TElTabSheet;
    ElTabSheet39: TElTabSheet;
    Label57: TLabel;
    ElPageControl19: TElPageControl;
    ElTabSheet40: TElTabSheet;
    ElTabSheet41: TElTabSheet;
    Label58: TLabel;
    ElPageControl20: TElPageControl;
    ElTabSheet42: TElTabSheet;
    ElTabSheet43: TElTabSheet;
    Label59: TLabel;
    ElPageControl21: TElPageControl;
    ElTabSheet44: TElTabSheet;
    ElTabSheet45: TElTabSheet;
    Label60: TLabel;
    ElPageControl22: TElPageControl;
    ElTabSheet46: TElTabSheet;
    ElTabSheet47: TElTabSheet;
    Label61: TLabel;
    ElPageControl23: TElPageControl;
    ElTabSheet48: TElTabSheet;
    ElTabSheet49: TElTabSheet;
    Label62: TLabel;
    ElPageControl24: TElPageControl;
    ElTabSheet50: TElTabSheet;
    ElTabSheet51: TElTabSheet;
    Label63: TLabel;
    ElPageControl25: TElPageControl;
    ElTabSheet52: TElTabSheet;
    ElTabSheet53: TElTabSheet;
    Label64: TLabel;
    ElPageControl26: TElPageControl;
    ElTabSheet54: TElTabSheet;
    ElTabSheet55: TElTabSheet;
    Label65: TLabel;
    ElPageControl27: TElPageControl;
    ElTabSheet56: TElTabSheet;
    ElTabSheet57: TElTabSheet;
    Label66: TLabel;
    tabElStringPool: TElTabSheet;
    ElPageControl28: TElPageControl;
    ElTabSheet58: TElTabSheet;
    ElTabSheet59: TElTabSheet;
    Label67: TLabel;
    procedure NavigationLinkClick(Sender: TObject; HRef: TElFString);
    procedure NavigationTreeItemFocused(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComponentGlyphImageNeeded(Sender: TObject; Src: TElFString;
      var Image: TBitmap);
    procedure FormDestroy(Sender: TObject);
    procedure mainTabsShow(Sender: TObject);
    procedure compTabsShow(Sender: TObject);
    procedure compTabsHide(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    NotFirstTime : boolean;
  public
    procedure MailLinkClick(Sender: TObject; HRef: TElFString);
  end;

var
  frmMain  : TfrmMain;
  ProxyBmp : TBitmap;

implementation

uses ElStatusBarDemoMain, ElTreeStringGridDemoMain, ElHeaderDemoMain,
     ElToolbarDemoMain, PgCtlDemoMain, ElPopupButtonDemoMain,
  ElCheckBoxDemoMain, ElRadioButtonDemoMain, ElGroupBoxDemoMain,
  ElRadioGroupDemoMain, ElCheckGroupDemoMain, ElScrollBarDemoMain,
  ElDateTimePickerDemoMain, ElTreeDemoMain;

{$R *.DFM}

const idxElTree           = 9;
      idxElPageControl    = 10;
      idxElTreeStringGrid = 11;
      idxElHeader         = 12;
      idxElStatusBar      = 13;
      idxElToolbar        = 14;
      idxElPopupButton    = 15;
      idxElCheckBox       = 16;
      idxElRadioButton    = 17;
      idxElGroupBox       = 18;
      idxElRadioGroup     = 19;
      idxElCheckGroup     = 20;
      idxElScrollBar      = 21;
      idxElDateTimePicker = 23;

procedure TfrmMain.MailLinkClick(Sender: TObject; HRef: TElFString);
begin
  FireURL(HRef);
end;

procedure TfrmMain.NavigationLinkClick(Sender: TObject; HRef: TElFString);
var Item : TElTreeItem;
begin
  Item := NavigationTree.Items.LookForItem(nil, HRef, nil, -1, false, false, false, false, true);
  if Item <> nil then
  begin
    NavigationTree.ItemFocused := Item;
    NavigationTree.EnsureVisible(Item);
  end
  else
    ElMessageDlgEx2(HRef + ' - No such topic found. Please <a href="mailto:elpack@eldos.org">report</a> to EldoS', mtError, [mbOk], 0, true, MailLinkClick);
end;

procedure TfrmMain.NavigationTreeItemFocused(Sender: TObject);
begin
  if NavigationTree.ItemFocused <> nil then
    MainPages.ActivePageIndex := NavigationTree.ItemFocused.Tag;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin                      
  MainPages.ShowTabs := false;
  MainPages.ShowBorder := false;
  FloatHTMLView.Parent := nil;
  MainPages.ActivePageIndex := -1;
  if IsWinXPUp then
    FloatHTMLView.BorderStyle := bsNone;
end;

procedure TfrmMain.ComponentGlyphImageNeeded(Sender: TObject;
  Src: TElFString; var Image: TBitmap);
var i : integer;
begin
  i := StrToIntDef(Src, -1);
  if ProxyBmp = nil then
    ProxyBmp := TBitmap.Create;
  ProxyBmp.Width := 0;
  ProxyBmp.Height := 0;
  Image := ProxyBmp;
  if i >= 0 then
    ComponentGlyphs.GetBitmap(i, Image);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if ProxyBmp <> nil then
  begin
    ProxyBmp.Free;
    ProxyBmp := nil;
  end;
end;

procedure TfrmMain.mainTabsShow(Sender: TObject);
begin
  FloatHTMLView.Parent := Sender as TElTabSheet;
  FloatHTMLView.Align := alClient;
  FloatHTMLView.Visible := true;
  FloatHTMLView.Caption := DescriptionPool.Items[TElTabSheet(Sender).PageIndex];
end;

procedure TfrmMain.compTabsShow(Sender: TObject);
begin
  FloatHTMLView.Parent := (TElTabSheet(Sender).Controls[0] as TElPageControl).Pages[0];
  FloatHTMLView.Align := alClient;
  FloatHTMLView.Visible := true;
  FloatHTMLView.Caption := DescriptionPool.Items[TElTabSheet(Sender).PageIndex];
  case TElTabSheet(Sender).PageIndex of
    idxElTree:
      begin
        ElTreeDemoMainForm := TElTreeDemoMainForm.Create(Self);
        ElTreeDemoMainForm.Parent := tabElTreeDemo;
        ElTreeDemoMainForm.Align := alClient;
        ElTreeDemoMainForm.BorderStyle := bsNone;
        ElTreeDemoMainForm.Visible := true;
        ElTreeDemoMainForm.FormActivate(Self);
      end;
    idxElPageControl:
      begin
        PgCtlDemoMainForm := TPgCtlMDemoMainForm.Create(Self);
        PgCtlDemoMainForm.Parent := tabElPageControlDemo;
        PgCtlDemoMainForm.Align := alClient;
        PgCtlDemoMainForm.BorderStyle := bsNone;
        PgCtlDemoMainForm.Visible := true;
        PgCtlDemoMainForm.FormActivate(Self);
      end;
    idxElTreeStringGrid:
      begin
        ElTreeStringGridDemoForm := TElTreeStringGridDemoForm.Create(Self);
        ElTreeStringGridDemoForm.Parent := tabElTreeStringGridDemo;
        ElTreeStringGridDemoForm.Align := alClient;
        ElTreeStringGridDemoForm.BorderStyle := bsNone;
        ElTreeStringGridDemoForm.Visible := true;
        // ElTreeStringGridDemoForm.FormActivate(Self);
      end;
    idxElHeader:
      begin
        ElHeaderDemoMainForm := TElHeaderDemoMainForm.Create(Self);
        ElHeaderDemoMainForm.Parent := tabElHeaderDemo;
        ElHeaderDemoMainForm.Align := alClient;
        ElHeaderDemoMainForm.BorderStyle := bsNone;
        ElHeaderDemoMainForm.Visible := true;
        ElHeaderDemoMainForm.FormActivate(Self);
      end;
    idxElStatusBar:
      begin
        ElStatusBarDemoMainForm := TElStatusBarDemoMainForm.Create(Self);
        ElStatusBarDemoMainForm.Parent := tabElStatusBarDemo;
        ElStatusBarDemoMainForm.Align := alClient;
        ElStatusBarDemoMainForm.BorderStyle := bsNone;
        ElStatusBarDemoMainForm.Visible := true;
        ElStatusBarDemoMainForm.FormActivate(Self);
      end;
    idxElToolbar:
      begin
        ElToolBarDemoMainForm := TElToolBarDemoMainForm.Create(Self);
        ElToolBarDemoMainForm.Parent := tabElToolBarDemo;
        ElToolBarDemoMainForm.Align := alClient;
        ElToolBarDemoMainForm.BorderStyle := bsNone;
        ElToolBarDemoMainForm.Visible := true;
        ElToolBarDemoMainForm.FormActivate(Self);
      end;
    idxElPopupButton:
      begin
        ElPopupButtonDemoMainForm := TElPopupButtonDemoMainForm.Create(Self);
        ElPopupButtonDemoMainForm.Parent := tabElPopupButtonDemo;
        ElPopupButtonDemoMainForm.Align := alClient;
        ElPopupButtonDemoMainForm.BorderStyle := bsNone;
        ElPopupButtonDemoMainForm.Visible := true;
        ElPopupButtonDemoMainForm.FormActivate(Self);
      end;
    idxElCheckBox:
      begin
        ElCheckBoxDemoMainForm := TElCheckBoxDemoMainForm.Create(Self);
        ElCheckBoxDemoMainForm.Parent := tabElCheckBoxDemo;
        ElCheckBoxDemoMainForm.Align := alClient;
        ElCheckBoxDemoMainForm.BorderStyle := bsNone;
        ElCheckBoxDemoMainForm.Visible := true;
        ElCheckBoxDemoMainForm.FormActivate(Self);
      end;
    idxElRadioButton:
      begin
        ElRadioButtonDemoMainForm := TElRadioButtonDemoMainForm.Create(Self);
        ElRadioButtonDemoMainForm.Parent := tabElRadioButtonDemo;
        ElRadioButtonDemoMainForm.Align := alClient;
        ElRadioButtonDemoMainForm.BorderStyle := bsNone;
        ElRadioButtonDemoMainForm.Visible := true;
        ElRadioButtonDemoMainForm.FormActivate(Self);
      end;
    idxElGroupBox:
      begin
        ElGroupBoxDemoMainForm := TElGroupBoxDemoMainForm.Create(Self);
        ElGroupBoxDemoMainForm.Parent := tabElGroupBoxDemo;
        ElGroupBoxDemoMainForm.Align := alClient;
        ElGroupBoxDemoMainForm.BorderStyle := bsNone;
        ElGroupBoxDemoMainForm.Visible := true;
        ElGroupBoxDemoMainForm.FormActivate(Self);
      end;
    idxElRadioGroup:
      begin
        ElRadioGroupDemoMainForm := TElRadioGroupDemoMainForm.Create(Self);
        ElRadioGroupDemoMainForm.Parent := tabElRadioGroupDemo;
        ElRadioGroupDemoMainForm.Align := alClient;
        ElRadioGroupDemoMainForm.BorderStyle := bsNone;
        ElRadioGroupDemoMainForm.Visible := true;
        ElRadioGroupDemoMainForm.FormActivate(Self);
      end;
    idxElCheckGroup:
      begin
        ElCheckGroupDemoMainForm := TElCheckGroupDemoMainForm.Create(Self);
        ElCheckGroupDemoMainForm.Parent := tabElCheckGroupDemo;
        ElCheckGroupDemoMainForm.Align := alClient;
        ElCheckGroupDemoMainForm.BorderStyle := bsNone;
        ElCheckGroupDemoMainForm.Visible := true;
        ElCheckGroupDemoMainForm.FormActivate(Self);
      end;
    idxElScrollBar:
      begin
        ElScrollBarDemoMainForm := TElScrollBarDemoMainForm.Create(Self);
        ElScrollBarDemoMainForm.Parent := tabElScrollBarDemo;
        ElScrollBarDemoMainForm.Align := alClient;
        ElScrollBarDemoMainForm.BorderStyle := bsNone;
        ElScrollBarDemoMainForm.Visible := true;
        ElScrollBarDemoMainForm.FormActivate(Self);
      end;
    idxElDateTimePicker:
      begin
        ElDateTimePickerDemoMainForm := TElDateTimePickerDemoMainForm.Create(Self);
        ElDateTimePickerDemoMainForm.Parent := tabElDateTimePickerDemo;
        ElDateTimePickerDemoMainForm.Align := alClient;
        ElDateTimePickerDemoMainForm.BorderStyle := bsNone;
        ElDateTimePickerDemoMainForm.Visible := true;
        ElDateTimePickerDemoMainForm.FormActivate(Self);
      end;
  end;
end;

procedure TfrmMain.compTabsHide(Sender: TObject);
begin
  case TElTabSheet(Sender).PageIndex of
    idxElTree:
      begin
        ElTreeDemoMainForm.Free;
        ElTreeDemoMainForm := nil;
      end;
    idxElPageControl:
      begin
        PgCtlDemoMainForm.Free;
        PgCtlDemoMainForm := nil;
      end;
    idxElTreeStringGrid:
      begin
        ElTreeStringGridDemoForm.Free;
        ElTreeStringGridDemoForm := nil;
      end;
    idxElHeader:
      begin
        ElHeaderDemoMainForm.Free;
        ElHeaderDemoMainForm := nil;
      end;
    idxElToolBar:
      begin
        ElToolBarDemoMainForm.Free;
        ElToolBarDemoMainForm := nil;
      end;
    idxElPopupButton:
      begin
        ElPopupButtonDemoMainForm.Free;
        ElPopupButtonDemoMainForm := nil;
      end;
    idxElCheckBox:
      begin
        ElCheckBoxDemoMainForm.Free;
        ElCheckBoxDemoMainForm := nil;
      end;
    idxElRadioButton:
      begin
        ElRadioButtonDemoMainForm.Free;
        ElRadioButtonDemoMainForm := nil;
      end;
    idxElGroupBox:
      begin
        ElGroupBoxDemoMainForm.Free;
        ElGroupBoxDemoMainForm := nil;
      end;
    idxElRadioGroup:
      begin
        ElRadioGroupDemoMainForm.Free;
        ElRadioGroupDemoMainForm := nil;
      end;
    idxElCheckGroup:
      begin
        ElCheckGroupDemoMainForm.Free;
        ElCheckGroupDemoMainForm := nil;
      end;
    idxElScrollBar:
      begin
        ElScrollBarDemoMainForm.Free;
        ElScrollBarDemoMainForm := nil;
      end;
    idxElDateTimePicker:
      begin
        ElDateTimePickerDemoMainForm.Free;
        ElDateTimePickerDemoMainForm := nil;
      end;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if not NotFirstTime then
  begin
    MainPages.ActivePageIndex := 0;    
    NotFirstTime := true;
  end;
end;

end.

