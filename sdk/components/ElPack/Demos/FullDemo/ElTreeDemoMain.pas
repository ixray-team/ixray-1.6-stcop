unit ElTreeDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElPgCtl, ElXPThemedControl, ElTree, ElHeader, ElBtnCtl, ElCheckCtl, ExtCtrls,
  ElPanel, StdCtrls, ElACtrls, ElPopBtn, ElTreeModalEdit, ElTreeDTPickEdit,
  ElDTPick, ElTreeCurrEdit, ElTreeSpinEdit, ElTreeCheckBoxEdit, ElDragDrop,
  ElTreeDemoExportTemplate, ElStrUtils, ElShellUtils, ElTools, ShellAPI,
  ElTreeMemoEdit, ElTreeAdvEdit, ElPromptDlg, ElTreeTreeComboEdit, ElMlGen,
  ElTreeMLGen, ElHTMLLbl, Printers, ElPrinter, ElTreePrinter, ElTimers,
  ElVCLUtils, ElFontCombo;

type
  TElTreeDemoMainForm = class(TForm)
    ElTreeDemoPages: TElPageControl;
    tabHiddenItems: TElTabSheet;
    tabInplaceEditors: TElTabSheet;
    tabHTMLExport: TElTabSheet;
    tabPrinting: TElTabSheet;
    tabCellControls: TElTabSheet;
    treeCellCtl: TElTree;
    tabAdvText: TElTabSheet;
    TreeHiddenItems: TElTree;
    TreeInplaceEditors: TElTree;
    ModalEdit: TElTreeInplaceModalEdit;
    DateTimeEdit: TElTreeInplaceDateTimePicker;
    CurrencyEdit: TElTreeInplaceCurrencyEdit;
    IntegerEdit: TElTreeInplaceSpinEdit;
    FloatingEdit: TElTreeInplaceFloatSpinEdit;
    CheckboxEdit: TElTreeInplaceCheckBoxEdit;
    MemoEdit: TElTreeInplaceMemoEdit;
    Edit: TElTreeInplaceAdvancedEdit;
    TreeEdit: TElTreeInplaceTreeComboEdit;
    TreeHTMLExport: TElTree;
    ElTreeMLGen: TElTreeMLGenerator;
    TreePrint: TElTree;
    TimerPool: TElTimerPool;
    tabOtherFeatures: TElTabSheet;
    tabDragDrop: TElTabSheet;
    TreeDragDrop: TElTree;
    ElHTMLLabel3: TElHTMLLabel;
    tabUnicode: TElTabSheet;
    TreeUnicode: TElTree;
    TreeTextFeatures: TElTree;
    APrinter: TElPrinter;
    ATreePrinter: TElTreePrinter;
    Panel5: TElPanel;
    HTMLExportTemplateButton: TElPopupButton;
    HTMLExportPreviewButton: TElPopupButton;
    GenerateColumnsCB: TElCheckBox;
    Panel7: TElPanel;
    PrintButton: TElPopupButton;
    PrintPreviewButton: TElPopupButton;
    PrintColumnsCB: TElCheckBox;
    Panel1: TElPanel;
    IgnoreEnabledCB: TElCheckBox;
    ShowHiddenItemsCB: TElCheckBox;
    Panel3: TElPanel;
    Label3: TLabel;
    ExplorerEditModeCB: TElCheckBox;
    QuickEditModeCB: TElCheckBox;
    InplaceEditorsTypeCombo: TElAdvancedComboBox;
    InplaceEditorsAddButton: TElPopupButton;
    Panel11: TElPanel;
    Label6: TLabel;
    Panel9: TElPanel;
    ElHTMLLabel2: TElHTMLLabel;
    Label5: TLabel;
    DragTypeCombo: TElAdvancedComboBox;
    Panel10: TElPanel;
    ElHTMLLabel4: TElHTMLLabel;
    TreeUnicodeFontCombo: TElFontComboBox;
    Panel6: TElPanel;
    Label4: TElHTMLLabel;
    Panel8: TElPanel;
    ElHTMLLabel1: TElHTMLLabel;
    Panel2: TElPanel;
    Label1: TLabel;
    Panel4: TElPanel;
    Label2: TLabel;
    procedure ShowHiddenItemsCBClick(Sender: TObject);
    procedure IgnoreEnabledCBClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure InplaceEditorsTypeComboChange(Sender: TObject);
    procedure ExplorerEditModeCBClick(Sender: TObject);
    procedure QuickEditModeCBClick(Sender: TObject);
    procedure InplaceEditorsAddButtonClick(Sender: TObject);
    procedure DateTimeEditBeforeOperation(Sender: TObject;
      var DefaultConversion: Boolean);
    procedure ModalEditExecute(Sender: TObject; var Accepted: Boolean);
    procedure TreeEditBeforeOperation(Sender: TObject;
      var DefaultConversion: Boolean);
    procedure HTMLExportTemplateButtonClick(Sender: TObject);
    procedure Label4LinkClick(Sender: TObject; HRef: TElFString);
    procedure ElTreeMLGenBeforeExecute(Sender: TObject);
    procedure ElTreeMLGenAfterExecute(Sender: TObject);
    procedure ElTreeMLGenWriteString(Sender: TObject; Value: String);
    procedure HTMLExportPreviewButtonClick(Sender: TObject);
    procedure GenerateColumnsCBClick(Sender: TObject);
    procedure PrintPreviewButtonClick(Sender: TObject);
    procedure ATreePrinterBeforePage(Sender: TObject; PageNumber: Integer);
    procedure PrintColumnsCBClick(Sender: TObject);
    procedure TimerPoolTimer(Sender: TObject);
    procedure TreeDragDropDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeDragDropStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure TreeDragDropEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TreeDragDropDragTargetChange(Sender: TObject;
      Item: TElTreeItem; ItemRect: TRect; X, Y: Integer);
    procedure TreeDragDropDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeDragDropKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeUnicodeFontComboChange(Sender: TObject);
  private
    NotFirstTime : boolean;
    HTMLStream   : TStream;
    DragItem     : TElTreeItem;
    procedure CreateCellControls;
    procedure CellControlButtonClick(Sender: TObject);
    procedure CellControlCheckBoxClick(Sender: TObject);
  protected
    procedure CreateTextCells;
  public
    { Public declarations }
  end;

var
  ElTreeDemoMainForm: TElTreeDemoMainForm;

implementation

{$R *.DFM}

procedure TElTreeDemoMainForm.ShowHiddenItemsCBClick(Sender: TObject);
begin
  TreeHiddenItems.FilteredVisibility := not ShowHiddenItemsCB.Checked;
end;

procedure TElTreeDemoMainForm.IgnoreEnabledCBClick(Sender: TObject);
begin
  TreeHiddenItems.IgnoreEnabled := IgnoreEnabledCB.Checked;
end;

const JP : array [0..7] of Word = ($65B0,$5E74,$304A,$3081,$3067,$3068,$3046,$0000);
const JP1: array [0..7] of Word = ($65B2,$5E76,$304C,$3083,$3069,$3070,$3048,$0000);

procedure TElTreeDemoMainForm.FormActivate(Sender: TObject);
begin
  if not NotFirstTime then
  begin
    DragTypeCombo.ItemIndex := 0;
    TreeUnicodeFontCombo.SampleText := WideStrPas(@jP);
    TreeUnicode.Items[0].Text := WideStrPas(@jP);
    TreeUnicode.HeaderSections[0].Text := WideStrPas(@jP1);
    CreateCellControls;
    CreateTextCells;
    NotFirstTime := true;
  end;
end;

procedure TElTreeDemoMainForm.InplaceEditorsTypeComboChange(
  Sender: TObject);
begin
  InplaceEditorsAddButton.Enabled := InplaceEditorsTypeCombo.ItemIndex <> -1;
end;

procedure TElTreeDemoMainForm.ExplorerEditModeCBClick(Sender: TObject);
begin
  treeInplaceEditors.ExplorerEditMode := ExplorerEditModeCB.Checked;
end;

procedure TElTreeDemoMainForm.QuickEditModeCBClick(Sender: TObject);
begin
  treeInplaceEditors.QuickEditMode := QuickEditModeCB.Checked;
end;

procedure TElTreeDemoMainForm.InplaceEditorsAddButtonClick(
  Sender: TObject);
var Item : TElTreeItem;
begin
  Item := treeInplaceEditors.Items.AddItem(nil);
  case InplaceEditorsTypeCombo.ItemIndex of
    0: begin
         Item.MainStyle.CellType := sftCustom;
         Item.Text := 'Custom item';
       end;
    1: begin
         Item.MainStyle.CellType := sftText;
         Item.Text := 'Text item';
       end;
    2: begin
         Item.MainStyle.CellType := sftNumber;
         Item.Text := '123';
       end;
    3: begin
         Item.MainStyle.CellType := sftFloating;
         Item.Text := '123';
       end;
    4: begin
         Item.MainStyle.CellType := sftDateTime;
         Item.Text := DateTimeToStr(Now);
       end;
    5: begin
         Item.MainStyle.CellType := sftDate;
         Item.Text := DateToStr(Now);
       end;
    6: begin
         Item.MainStyle.CellType := sftTime;
         Item.Text := TimeToStr(Now);
       end;
    7: begin
         Item.MainStyle.CellType := sftEnum;
         Item.Text := 'Choice 1';
       end;
    8: begin
         Item.MainStyle.CellType := sftBool;
         Item.Text := '*';
       end;
    9: begin
         Item.MainStyle.CellType := sftCurrency;
         Item.Text := '123';
       end;
    10:begin
         Item.MainStyle.CellType := sftMemo;
         Item.Text := 'Multiline Item';
         Item.Multiline := true;
       end;
  end;
  Item.UseStyles := true;
end;

procedure TElTreeDemoMainForm.DateTimeEditBeforeOperation(Sender: TObject;
  var DefaultConversion: Boolean);
begin
  if DateTimeEdit.DataType = sftDateTime then
    DateTimeEdit.Editor.Format := edfShortDateLongTime
  else
  if DateTimeEdit.DataType = sftDate then
    DateTimeEdit.Editor.Format := edfShortDate
  else
  if DateTimeEdit.DataType = sftTime then
    DateTimeEdit.Editor.Format := edfLongTime;
end;

procedure TElTreeDemoMainForm.ModalEditExecute(Sender: TObject;
  var Accepted: Boolean);
begin
  ElMessageDlg('This is a sample of custom editor that shows a modal dialog', mtInformation, [mbOk], 0);
end;

procedure TElTreeDemoMainForm.TreeEditBeforeOperation(Sender: TObject;
  var DefaultConversion: Boolean);
begin
  TreeEdit.Editor.ShowLines := false;
  TreeEdit.Editor.ShowButtons := false;
  TreeEdit.Editor.ShowRootButtons := false;
  TreeEdit.Editor.Ctl3D := false;
  TreeEdit.Editor.PathSeparator := #0;
  
  if TreeEdit.Editor.Items.Count = 0 then
  begin
    TreeEdit.Editor.Items.Add(nil, 'Choice 1');
    TreeEdit.Editor.Items.Add(nil, 'Choice 2');
    TreeEdit.Editor.Items.Add(nil, 'Choice 3');
  end;
  TreeEdit.Editor.Selection := TreeEdit.Editor.Items.LookForItem(nil, TreeEdit.ValueAsText, nil, -1, false, false, false, false, true);
end;

procedure TElTreeDemoMainForm.HTMLExportTemplateButtonClick(
  Sender: TObject);
begin
  with TElTreeDemoExportTemplateForm.Create(nil) do
  try
    TemplateMemo.Lines.Assign(ElTreeMLGen.Template);
    if ShowModal = mrOk then
      ElTreeMLGen.Template.Assign(TemplateMemo.Lines);
  finally
    Free;
  end;
end;

procedure TElTreeDemoMainForm.Label4LinkClick(Sender: TObject;
  HRef: TElFString);
begin
  FireURL(HRef);
end;

procedure TElTreeDemoMainForm.ElTreeMLGenBeforeExecute(Sender: TObject);
var S : string;
begin
  try
    S := GetTempFile  + '.html';
    HTMLStream := TNamedFileStream.Create(S, fmCreate or fmShareDenyWrite);
  except
    ElMessageDlg('Failed to create temporary file for resulting page', mtError, [mbOk], 0);
    raise;
  end;
end;

procedure TElTreeDemoMainForm.ElTreeMLGenAfterExecute(Sender: TObject);
var S : String;
  SHI : TShellExecuteInfo;
begin
  if HTMLStream <> nil then
  begin
    S := TNamedFileStream(HTMLStream).FileName;
    HTMLStream.Free;
    ZeroMemory(@SHI, sizeof(SHI));
    SHI.cbSize := sizeof(SHI);
    SHI.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_NOCLOSEPROCESS;
    SHI.Wnd := Application.Handle;
    SHI.lpVerb := PChar('open');
    SHI.lpFile := PChar(S);
    SHI.lpParameters := nil;
    SHI.lpDirectory := nil;
    if ShellExecuteEx(@SHI) then
    begin
      if SHI.hProcess <> INVALID_HANDLE_VALUE then
      begin
        WaitForInputIdle(SHI.hProcess, 10000);
        CloseHandle(SHI.hProcess);
      end;
      Sleep(5000);
      DeleteFile(S);
    end
    else
      ElMessageDlg('Failed to open default browser with resulting page', mtError, [mbOk], 0);
  end;
end;

procedure TElTreeDemoMainForm.ElTreeMLGenWriteString(Sender: TObject;
  Value: String);
begin
  if HTMLStream <> nil then
    HTMLStream.Write(PChar(Value)^, Length(Value));
end;

procedure TElTreeDemoMainForm.HTMLExportPreviewButtonClick(
  Sender: TObject);
begin
  ElTreeMLGen.Execute;
end;

procedure TElTreeDemoMainForm.GenerateColumnsCBClick(Sender: TObject);
begin
  ElTreeMLGen.GenerateColumns := GenerateColumnsCB.Checked;
end;

procedure TElTreeDemoMainForm.PrintPreviewButtonClick(Sender: TObject);
begin
  ATreePrinter.Print;
  APrinter.Preview;
end;

procedure TElTreeDemoMainForm.ATreePrinterBeforePage(Sender: TObject;
  PageNumber: Integer);
var S : String;
    R : TRect;
    WP,
    HP: integer;
    FDC : THandle;

begin
  S  := 'Page ' + IntToStr(PageNumber + 1);

  FDC:= GetDC(0);
  WP := MulDiv(APrinter.PageWidth, GetDeviceCaps(FDC, LOGPIXELSX), 2540);
  HP := MulDiv(APrinter.TopMargin, GetDeviceCaps(FDC, LOGPIXELSY), 2540);
  ReleaseDC(0, FDC);

  R.Left := (WP - APrinter.Canvas[PageNumber].TextWidth(S)) div 2;
  R.Top  := -(HP - APrinter.Canvas[PageNumber].TextHeight(S)) div 2;
  APrinter.Canvas[PageNumber].TextOut(R.Left, R.Top, S);
end;

procedure TElTreeDemoMainForm.PrintColumnsCBClick(Sender: TObject);
begin
  ATreePrinter.ShowColumns := PrintColumnsCB.Checked;
end;

procedure TElTreeDemoMainForm.CreateCellControls;
var Item : TElTreeItem;
    C    : TElCellControl;
    S    : TElCellStyle;
    i    : integer;
begin
  TreeCellCtl.Items.BeginUpdate;
  for i := 0 to TreeCellCtl.Items.Count - 1 do
  begin
    Item := TreeCellCtl.Items[i];
    Item.MainStyle.OwnerProps := true;

    // checkbox
    S := Item.AddStyle;
    S.OwnerProps := false;
    C := TElCellCheckBox.Create;
    TElCellCheckBox(C).Alignment := taCenter;
    TElCellCheckBox(C).Checked := true;
    C.OnClick := CellControlCheckBoxClick;
    C.BorderWidth := 1;
    S.Control := C;

    // button
    S := Item.AddStyle;
    S.OwnerProps := false;
    C := TElCellButton.Create;
    TElCellButton(C).Caption := 'Start';
    C.OnClick := CellControlButtonClick;
    C.BorderWidth := 1;
    S.Control := C;

    // progress
    S := Item.AddStyle;
    S.OwnerProps := false;
    C := TElCellProgressBar.Create;
    TElCellProgressBar(C).TextAlignment := taRightJustify;
    C.BorderWidth := 1;
    S.Control := C;

    Item.UseStyles := true;
  end;
  TreeCellCtl.Items.EndUpdate;
end;

procedure TElTreeDemoMainForm.CellControlButtonClick(Sender: TObject);
var Item : TElTreeItem;
begin
  Item := TElCellButton(Sender).Owner.Owner;
  if TimerPool.Items.Items[Item.Index].Enabled then
  begin
    TimerPool.Items.Items[Item.Index].Enabled := false;
    TElCellButton(Sender).Caption := 'Start';
  end
  else
  begin
    TElCellButton(Sender).Caption := 'Stop';
    with TimerPool.Items.Items[Item.Index] do
    begin
      Interval := 100 + Random(500);
      Enabled := true;
    end;
    TElCellProgressBar(Item.Styles[2].Control).Value := 0;
  end;
end;

procedure TElTreeDemoMainForm.CellControlCheckBoxClick(Sender: TObject);
var Item : TElTreeItem;
begin
  Item := TElCellCheckBox(Sender).Owner.Owner;
  TElCellProgressBar(Item.Styles[2].Control).Visible := TElCellCheckBox(Sender).Checked;
end;

procedure TElTreeDemoMainForm.TimerPoolTimer(Sender: TObject);
var P : TElCellProgressBar;
begin
  P := TElCellProgressBar(TreeCellCtl.Items[TElPoolTimer(Sender).Owner.Index].Styles[2].Control);
  P.Value := P.Value + 5;
  if P.Value >= 100 then
  begin
    TElPoolTimer(Sender).Owner.Enabled := false;
    TElCellButton(TreeCellCtl.Items[TElPoolTimer(Sender).Owner.Index].Styles[1].Control).Caption := 'Start';
  end;
end;

procedure TElTreeDemoMainForm.TreeDragDropDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var Item : TElTreeItem;
    IP   : TSTItemPart;
    HCol : integer;
begin
  if (DragItem = nil) then
    Accept := false
  else
  begin
    Item := TreeDragDrop.GetItemAt(X, Y, IP, HCol);
    Accept := (Item <> nil) and (Item <> DragItem);
  end;
end;

procedure TElTreeDemoMainForm.TreeDragDropStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  if (TreeDragDrop.ItemFocused <> nil) and (TreeDragDrop.ItemFocused.Level = 1) then
    DragItem := TreeDragDrop.ItemFocused
  else
    DragItem := nil;

  TreeDragDrop.DragTrgDrawMode := TDragTargetDraw(DragTypeCombo.ItemIndex);
end;

procedure TElTreeDemoMainForm.TreeDragDropEndDrag(Sender, Target: TObject;
  X, Y: Integer);
begin
  TreeDragDrop.DragTrgDrawMode := TDragTargetDraw(DragTypeCombo.ItemIndex);
  DragItem := nil;
end;

procedure TElTreeDemoMainForm.TreeDragDropDragTargetChange(Sender: TObject;
  Item: TElTreeItem; ItemRect: TRect; X, Y: Integer);
var R : TRect;
begin
  if Item <> nil then
  begin
    if Item.Level = 0 then
      TreeDragDrop.DragTrgDrawMode := TDragTargetDraw(DragTypeCombo.ItemIndex)
    else
    begin
      R := TreeDragDrop.GetItemRect(Item.VisIndex);
      if Y <= (R.Bottom + R.Top) div 2 then
      begin
        if DragTypeCombo.ItemIndex = 2 then
          TreeDragDrop.DragTrgDrawMode := dtdUpSelColorLine
        else
          TreeDragDrop.DragTrgDrawMode := dtdUpColorLine;
      end
      else
      begin
        if DragTypeCombo.ItemIndex = 2 then
          TreeDragDrop.DragTrgDrawMode := dtdDownSelColorLine
        else
          TreeDragDrop.DragTrgDrawMode := dtdDownColorLine;
      end;
    end;
  end;
end;

procedure TElTreeDemoMainForm.TreeDragDropDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var Item : TElTreeItem;
    IP   : TSTItemPart;
    HCol : integer;
    R    : TRect;
begin
  if DragItem <> nil then
  begin
    Item := TreeDragDrop.GetItemAt(X, Y, IP, HCol);
    if (Item <> nil) and (Item <> DragItem) then
    begin
      if ssCtrl in GetShiftState then
      begin
        if Item.Level = 0 then
          TreeDragDrop.Items.AddItem(Item).Assign(DragItem)
        else
        begin
          R := TreeDragDrop.GetItemRect(Item.VisIndex);

          if Y <= (R.Bottom + R.Top) div 2 then
            HCol := Item.Index
          else
            HCol := Item.Index + 1;

          TreeDragDrop.Items.InsertItem(HCol, Item.Parent).Assign(DragItem);
        end;
      end
      else
      begin
        if Item.Level = 0 then
          DragItem.MoveTo(Item)
        else
        begin
          if TreeDragDrop.DragTrgDrawMode in [dtdUpSelColorLine, dtdUpColorLine] then
            HCol := Item.Index
          else
          if TreeDragDrop.DragTrgDrawMode in [dtdDownSelColorLine, dtdDownColorLine] then
            HCol := Item.Index + 1
          else
            HCol := 0;
          DragItem.MoveToIns(Item.Parent, HCol);
        end;
      end;
    end;
  end;
end;

procedure TElTreeDemoMainForm.TreeDragDropKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (Shift = []) and (not TreeDragDrop.Dragging) and
     (TreeDragDrop.ItemFocused <> nil) and (TreeDragDrop.ItemFocused.Level = 1) then
  begin
    TreeDragDrop.Items.DeleteItem(TreeDragDrop.ItemFocused);
  end;
end;

procedure TElTreeDemoMainForm.TreeUnicodeFontComboChange(Sender: TObject);
begin
  TreeUnicode.Font.Name := TreeUnicodeFontCombo.FontName;
end;

procedure TElTreeDemoMainForm.CreateTextCells;
begin
  with TreeTextFeatures.Items[1] do
  begin
    MainStyle.OwnerProps := true;
    AddStyle.OwnerProps := true;
    with AddStyle do
    begin
      FontName := 'Arial';
      FontStyles := [fsItalic];
      OwnerProps := false;
      TextColor := clHighlight;
      UseBkColor := false;
      TextFlags := DT_RIGHT or DT_VCENTER;
    end;
    UseStyles := true;
  end;
end;

end.
