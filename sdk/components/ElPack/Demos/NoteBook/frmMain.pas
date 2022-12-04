unit frmMain;

interface

uses   ElCalendDlg, ElBaseComp, ElTray, ElCaption,
  ElSideBar, ElDTPick, ElCheckCtl, ElCombos, StdCtrls,
  ElACtrls, ElLabel, ElBtnCtl, ElPopBtn, ComCtrls, ElClock, ElStatBar,
  ElSplit, ExtCtrls, ElPanel, ElToolBar, ElTree,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  NoteRecord, ElHTMLView, ElTimers, ElImgLst, ElCLabel, ElPgCtl,
  ElXPThemedControl, ElGroupBox, ElEdits;
{
type ItemSettings = record
  PersonFont: TFont;
  EventFont:
  RemindFont:
}

type
  TfrmNoteBook = class(TForm)
    NoteTree: TElTree;
    ElToolBar1: TElToolBar;
    ElSplitter1: TElSplitter;
    ElStatusBar1: TElStatusBar;
    NoteSideBar: TElSideBar;
    pnlStatusBar: TElPanel;
    ElPanel5: TElPanel;
    ElClock1: TElClock;
    ElImageList1: TElImageList;
    btnNewItem: TElGraphicButton;
    btnDelItem: TElGraphicButton;
    tbButtonImages: TElImageList;
    ElTrayIcon1: TElTrayIcon;
    NoteCalendar: TElCalendarDialog;
    ElGraphicButton1: TElGraphicButton;
    ElGraphicButton2: TElGraphicButton;
    ElGraphicButton3: TElGraphicButton;
    ElGraphicButton4: TElGraphicButton;
    ElTimerPool1: TElTimerPool;
    NoteControl: TElPageControl;
    tabPerson: TElTabSheet;
    ElPanel2: TElPanel;
    ElLabel1: TElLabel;
    ElLabel2: TElLabel;
    ElLabel3: TElLabel;
    lPersonCreated: TElLabel;
    lPersonChanged: TElLabel;
    ElLabel13: TElLabel;
    ElGraphicButton5: TElGraphicButton;
    edTitlePerson: TElAdvancedEdit;
    ElPopupButton1: TElPopupButton;
    tabEvent: TElTabSheet;
    ElPanel1: TElPanel;
    ElLabel5: TElLabel;
    ElLabel6: TElLabel;
    ElLabel7: TElLabel;
    lEventCreated: TElLabel;
    lEventChanged: TElLabel;
    ElLabel10: TElLabel;
    ElGraphicButton6: TElGraphicButton;
    edTitleEvent: TElAdvancedEdit;
    tabRemind: TElTabSheet;
    ElPanel3: TElPanel;
    ElLabel14: TElLabel;
    ElLabel15: TElLabel;
    ElLabel16: TElLabel;
    lRemindCreated: TElLabel;
    lRemindChanged: TElLabel;
    ElLabel19: TElLabel;
    ElGraphicButton7: TElGraphicButton;
    edTitleRemind: TElAdvancedEdit;
    ElPanel6: TElPanel;
    ElLabel21: TElLabel;
    RemindTime: TElDateTimePicker;
    ckRemindEnabled: TElCheckBox;
    GroupBox4: TElGroupBox;
    mRemindNote: TElAdvancedMemo;
    GroupBox2: TElGroupBox;
    mEventNote: TElAdvancedMemo;
    GroupBox3: TElGroupBox;
    ElLabel4: TElLabel;
    ElLabel20: TElLabel;
    cbPeriod: TElComboBox;
    edEventPeriodNum: TElAdvancedEdit;
    ckEventEnabled: TElCheckBox;
    ckPeriodEnabled: TElCheckBox;
    edEventStart: TElDateTimePicker;
    GroupBox1: TElGroupBox;
    mPersonNote: TElAdvancedMemo;
    procedure FormCreate(Sender: TObject);
    procedure NoteControlChange(Sender: TObject);
    procedure ShowBranch(Item: String);
    procedure ElPopupButton1Click(Sender: TObject);
    procedure btnNewItemClick(Sender: TObject);
    procedure btnDelItemClick(Sender: TObject);
    procedure NoteSideBarClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ElGraphicButton1Click(Sender: TObject);
    procedure NoteTreeClick(Sender: TObject);
    procedure NoteTreeDblClick(Sender: TObject);
    procedure ElGraphicButton3Click(Sender: TObject);
    procedure NoteTreeItemSave(Sender: TObject; Stream: TStream;
      Item: TElTreeItem);
    procedure NoteTreeItemLoad(Sender: TObject; Stream: TStream;
      Item: TElTreeItem);
    procedure ElGraphicButton5Click(Sender: TObject);
    procedure ElGraphicButton6Click(Sender: TObject);
    procedure ElGraphicButton7Click(Sender: TObject);
    procedure NoteTreeItemFocused(Sender: TObject);
    procedure ElGraphicButton4Click(Sender: TObject);
    procedure ElTimerPool1Items0Timer(Sender: TObject);
    procedure ElTrayIcon1Click(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ElGraphicButton2Click(Sender: TObject);
    procedure ckPeriodEnabledClick(Sender: TObject);
  private
    { Private declarations }
    procedure InitRootItems;
    procedure InitStyle(Item: TElTreeItem);
  public
//    Root: TElTreeItem;
    Event: TElTreeItem;
    Person: TElTreeItem;
    Remind: TElTreeItem;

    { Public declarations }
  end;

const
  cEvent = 'Event';
  cPerson = 'Person';
  cRemind = 'Reminder';

var
  frmNoteBook: TfrmNoteBook;
{  Root: TElTreeItem;
  Event: TElTreeItem;
  Person: TElTreeItem;
  Remind: TElTreeItem;
}
implementation

uses PersonInfo, find, frmSettings;

{$R *.DFM}

procedure TfrmNoteBook.FormCreate(Sender: TObject);
begin
  InitRootItems;
end;

procedure TfrmNoteBook.NoteControlChange(Sender: TObject);
begin
  ShowBranch(NoteControl.ActivePage.Caption);
end;

procedure TfrmNoteBook.ShowBranch(Item: String);
begin
 if Item = cPerson then
 begin
   Event.Hidden := True;
   Person.Hidden := False;
   Remind.Hidden := True;
   NoteControl.ActivePage := tabPerson;
   NoteSideBar.Sections[0].Items[0].Active := True;
   Person.Selected := true;
   Person.Focused := true;
 end;
 if Item = cEvent then
 begin
   Person.Hidden := True;
   Event.Hidden := False;
   Remind.Hidden := True;
   NoteControl.ActivePage := tabEvent;
   NoteSideBar.Sections[0].Items[1].Active := True;
   Event.Selected := true;
   Event.Focused := true;
 end;
 if Item = cRemind then
 begin
   Remind.Hidden := False;
   Person.Hidden := True;
   Event.Hidden := True;
   NoteControl.ActivePage := tabRemind;
   NoteSideBar.Sections[0].Items[2].Active := True;
   Remind.Selected := true;
   Remind.Focused := true;
 end;
end;

procedure TfrmNoteBook.ElPopupButton1Click(Sender: TObject);
begin
  Application.CreateForm(TfrmPersonInfo, frmPersonInfo);
  frmPersonInfo.ShowModal;
end;

procedure TfrmNoteBook.btnNewItemClick(Sender: TObject);
var
  Item: TElTreeItem;
begin
  if NoteTree.ItemFocused <> nil then
  begin
    Item:=NoteTree.Items.AddItem(NoteTree.ItemFocused);
    NoteTree.ItemFocused.Expand(True);
    Item.UseStyles := True;
    InitStyle(Item);
    if NoteTree.ItemFocused.Owner.TopItem = Event then
    begin
      Item.Text:= 'Event item';
      Item.Data := TNoteEvent.Create(NoteTree);
      TNoteEvent(Item.Data).TimeStamp := Now;
      TNoteEvent(Item.Data).TimeChange := Now;
    end;
    if NoteTree.ItemFocused.Owner.TopItem = Person then
    begin
      Item.Text:= 'Person item';
      Item.Data := TNotePerson.Create(NoteTree);
      TNotePerson(Item.Data).TimeStamp := Now;
      TNotePerson(Item.Data).TimeChange := Now;
    end;
    if NoteTree.ItemFocused.Owner.TopItem = Remind then
    begin
      Item.Text:= 'Remind item';
      Item.Data := TNoteRemind.Create(NoteTree);
      TNoteRemind(Item.Data).TimeStamp := Now;
      TNoteRemind(Item.Data).TimeChange := Now;
    end;
  end;
end;

procedure TfrmNoteBook.btnDelItemClick(Sender: TObject);
begin
  if (NoteTree.ItemFocused.Text <> cPerson) and
     (NoteTree.ItemFocused.Text <> cEvent) and
     (NoteTree.ItemFocused.Text <> cRemind) then
  begin
    NoteTree.ItemFocused.Delete;
  end;
end;

procedure TfrmNoteBook.NoteSideBarClick(Sender: TObject);
begin
    case NoteSideBar.ItemIndex of
      0: ShowBranch(cPerson);
      1: ShowBranch(cEvent);
      2: ShowBranch(cRemind);
    end;
end;

procedure TfrmNoteBook.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  NoteTree.Items.SaveToFile('options.sav');
end;

procedure TfrmNoteBook.ElGraphicButton1Click(Sender: TObject);
begin
  NoteCalendar.Execute;
end;

procedure TfrmNoteBook.NoteTreeClick(Sender: TObject);
begin
  if NoteTree.ItemFocused <> nil then
  begin
    if NoteTree.ItemFocused.Owner.TopItem = Event then
    begin
      mEventNote.Lines.Text := TNoteEvent(NoteTree.ItemFocused.Data).Note;
      edTitleEvent.Text := TNoteEvent(NoteTree.ItemFocused.Data).Title;
      lEventCreated.Caption := DateTimeToStr(TNoteEvent(NoteTree.ItemFocused.Data).TimeStamp);
      lEventChanged.Caption := DateTimeToStr(TNoteEvent(NoteTree.ItemFocused.Data).TimeChange);
      if TNoteEvent(NoteTree.ItemFocused.Data).TimeEvent = 0 then
        edEventStart.DateTime := Now
      else
        edEventStart.DateTime := TNoteEvent(NoteTree.ItemFocused.Data).TimeEvent;
      ckEventEnabled.Checked := TNoteEvent(NoteTree.ItemFocused.Data).TimeEventEnabled;
    end;
    if NoteTree.ItemFocused.Owner.TopItem = Person then
    begin
      mPersonNote.Lines.Text := TNotePerson(NoteTree.ItemFocused.Data).Note;
      edTitlePerson.Text := TNotePerson(NoteTree.ItemFocused.Data).Title;
      lPersonCreated.Caption := DateTimeToStr(TNotePerson(NoteTree.ItemFocused.Data).TimeStamp);
      lPersonChanged.Caption := DateTimeToStr(TNotePerson(NoteTree.ItemFocused.Data).TimeChange);
    end;
    if NoteTree.ItemFocused.Owner.TopItem = Remind then
    begin
      mRemindNote.Lines.Text := TNoteRemind(NoteTree.ItemFocused.Data).Note;
      edTitleRemind.Text := TNoteRemind(NoteTree.ItemFocused.Data).Title;
      lRemindCreated.Caption := DateTimeToStr(TNoteRemind(NoteTree.ItemFocused.Data).TimeStamp);
      lRemindChanged.Caption := DateTimeToStr(TNoteRemind(NoteTree.ItemFocused.Data).TimeChange);
      if TNoteRemind(NoteTree.ItemFocused.Data).RemindTime = 0 then
        RemindTime.DateTime := Now
      else
        RemindTime.DateTime := TNoteRemind(NoteTree.ItemFocused.Data).RemindTime;
      ckRemindEnabled.Checked := TNoteRemind(NoteTree.ItemFocused.Data).Enabled;
    end;
  end;
end;

procedure TfrmNoteBook.InitStyle(Item: TElTreeItem);
begin
  with Item.MainStyle do
  begin
    TextBkColor := clWhite;
    TextColor := clBlack;
    FontSize := 8;
    FontName := 'MS Sans Serif';
    CellBkColor := clWhite;
  end;
end;

procedure TfrmNoteBook.InitRootItems;
begin
  if FileExists('options.sav') then
    NoteTree.Items.LoadFromFile('options.sav')
  else
  begin
    NoteTree.Items.AddItem(nil);
    NoteTree.Items.AddItem(nil);
    NoteTree.Items.AddItem(nil);
  end;
  Event := NoteTree.Items.RootItem[0];
  Person := NoteTree.Items.RootItem[1];
  Remind := NoteTree.Items.RootItem[2];
  Event.UseStyles := True;
  InitStyle(Event);
  Person.UseStyles := True;
  InitStyle(Person);
  Remind.UseStyles := True;
  InitStyle(Remind);
  Event.Text := cEvent;
  Person.Text := cPerson;
  Remind.Text := cRemind;
  Event.Hidden := True;
  Person.Hidden := False;
  Remind.Hidden := True;
  Event.Data := TNoteEvent.Create(NoteTree);
  Person.Data := TNotePerson.Create(NoteTree);
  Remind.Data := TNoteRemind.Create(NoteTree);
  TNoteEvent(Event.Data).Title := 'Event Notes';
  TNoteEvent(Person.Data).Title := 'Person Notes';
  TNoteEvent(Remind.Data).Title := 'Remind Notes';
  if TNoteEvent(Event.Data).TimeStamp = 0 then
  begin
    TNoteEvent(Event.Data).TimeStamp := Now;
    TNoteEvent(Person.Data).TimeStamp := Now;
    TNoteEvent(Remind.Data).TimeStamp := Now;
    TNoteEvent(Event.Data).TimeChange := Now;
    TNoteEvent(Person.Data).TimeChange := Now;
    TNoteEvent(Remind.Data).TimeChange := Now;
  end;
  NoteTree.FilteredVisibility := True;
  Person.Focused := true;
  Person.Selected := true;
end;

procedure TfrmNoteBook.NoteTreeDblClick(Sender: TObject);
begin
  if NoteTree.ItemFocused <> nil then
  begin
    NoteTree.ItemFocused.EditText;
  end;
end;

procedure TfrmNoteBook.ElGraphicButton3Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmNoteBook.NoteTreeItemSave(Sender: TObject; Stream: TStream;
  Item: TElTreeItem);
begin
  if (Item.Parent <> nil) then
  begin
    if Item.IsUnder(NoteTree.Items.RootItem[0]) then
    begin
      Stream.WriteComponent(TNoteEvent(Item.data));
    end
    else if Item.IsUnder(NoteTree.Items.RootItem[1]) then
    begin
      Stream.WriteComponent(TNotePerson(Item.data));
    end
    else if Item.IsUnder(NoteTree.Items.RootItem[2]) then
    begin
      Stream.WriteComponent(TNoteRemind(Item.data));
    end;
  end;
end;

procedure TfrmNoteBook.NoteTreeItemLoad(Sender: TObject; Stream: TStream;
  Item: TElTreeItem);
var
  TempEvent: TNoteEvent;
  TempPerson: TNotePerson;
  TempRemind: TNoteRemind;
begin
  if (Item.Parent <> nil) then
  begin
    if Item.IsUnder(NoteTree.Items.RootItem[0]) then
    begin
      TempEvent := TNoteEvent.Create(self);
      Stream.ReadComponent(TempEvent);
      TempEvent.Name := 'A' + IntToStr(Integer(TempEvent));
      Item.Data := TempEvent;
    end
    else if Item.IsUnder(NoteTree.Items.RootItem[1]) then
    begin
      TempPerson := TNotePerson.Create(NoteTree);
      Stream.ReadComponent(TempPerson);
      TempPerson.Name := 'A' + IntToStr(Integer(TempPerson));
      Item.Data := TempPerson;
    end
    else if Item.IsUnder(NoteTree.Items.RootItem[2]) then
    begin
      TempRemind := TNoteRemind.Create(self);
      Stream.ReadComponent(TempRemind);
      TempRemind.Name := 'A' + IntToStr(Integer(TempRemind));
      Item.Data := TempRemind;
    end;
  end;
end;

procedure TfrmNoteBook.ElGraphicButton5Click(Sender: TObject);
begin
  TNotePerson(NoteTree.ItemFocused.Data).Title := edTitlePerson.Text;
  TNotePerson(NoteTree.ItemFocused.Data).TimeChange := Now;
  TNotePerson(NoteTree.ItemFocused.Data).Note := mPersonNote.Lines.Text;
end;

procedure TfrmNoteBook.ElGraphicButton6Click(Sender: TObject);
  function EncodeDateTime(period: string): TDateTime;
  var
    Per : integer;
  begin
    Per := StrToInt(period);
      case cbPeriod.ItemIndex of
     0: EncodeDateTime := EncodeTime(0,0,Per,0); // Sec
     1: EncodeDateTime := EncodeTime(0,Per,0,0); // Min
     2: EncodeDateTime := EncodeTime(Per,0,0,0); // Hour
    else
     begin
       ckPeriodEnabled.Checked := False;
       EncodeDateTime := EncodeTime(0,0,0,0); 
       TNoteEvent(NoteTree.ItemFocused.Data).TimeEventEnabled := False;
       Application.MessageBox('Field "Event period is empty! "',
                 '', MB_ICONWARNING + MB_APPLMODAL + IDOK);
     end;
    end;
  end;

begin
  with TNoteEvent(NoteTree.ItemFocused.Data) do
  begin
    Note := mEventNote.Lines.Text;
    Title := edTitleEvent.Text;
    TimeChange := Now;
    TimeEvent := edEventStart.DateTime;
    TimeEventEnabled := ckEventEnabled.Checked;
    TimePeriodEnabled := ckPeriodEnabled.Checked;
    if ckPeriodEnabled.Checked = true then
//    TimePeriod := Now;
    TimePeriod := {TimePeriod +} EncodeDateTime(edEventPeriodNum.Text);
  end;

end;

procedure TfrmNoteBook.ElGraphicButton7Click(Sender: TObject);
begin
  TNoteRemind(NoteTree.ItemFocused.Data).Title := edTitleRemind.Text;
  TNoteRemind(NoteTree.ItemFocused.Data).TimeChange := Now;
  TNoteRemind(NoteTree.ItemFocused.Data).Note := mRemindNote.Lines.Text;
  TNoteRemind(NoteTree.ItemFocused.Data).RemindTime := RemindTime.DateTime;
  TNoteRemind(NoteTree.ItemFocused.Data).Enabled := ckRemindEnabled.Checked;
end;

procedure TfrmNoteBook.NoteTreeItemFocused(Sender: TObject);
begin
  NoteTreeClick(Sender);
end;

procedure TfrmNoteBook.ElGraphicButton4Click(Sender: TObject);
begin
  Application.CreateForm(TfrmFind, frmFind);
  frmFind.ShowModal;
end;

procedure EventCheck(Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree);
var
  pStr: PChar;
begin
  if Item.IsUnder(frmNoteBook.NoteTree.Items.RootItem[0]) then
  begin
    if (Now > TNoteEvent(Item.Data).TimeEvent) and
       (TNoteEvent(Item.Data).TimeEventEnabled)
    then
    begin
      GetMem(pStr, (Length(TNoteEvent(Item.Data).Title))+1);
      StrPCopy(pStr, TNoteEvent(Item.Data).Title);
      TNoteEvent(Item.Data).TimeEventEnabled := False;
      frmNoteBook.ckEventEnabled.Checked := False;
      Application.MessageBox(pStr , 'Event:', MB_ICONINFORMATION + MB_APPLMODAL + MB_OK);
      FreeMem(pStr);
      frmNoteBook.ckPeriodEnabled.Checked := TNoteEvent(Item.Data).TimePeriodEnabled;
      if (TNoteEvent(Item.Data).TimePeriodEnabled) then
      begin
        TNoteEvent(Item.Data).TimeEvent := Now + TNoteEvent(Item.Data).TimePeriod;
        frmNoteBook.edEventStart.DateTime := TNoteEvent(Item.Data).TimeEvent;
        TNoteEvent(Item.Data).TimeEventEnabled := True;
        frmNoteBook.ckEventEnabled.Checked := True;
      end;
    end;
//    TNoteEvent(Item.Data).TimeEvent;
//    TNoteEvent(Item.Data).TimePeriod;
//    TNoteEvent(Item.Data).TimeEventEnabled;
  end
  else if Item.IsUnder(frmNoteBook.NoteTree.Items.RootItem[1]) then
  begin
//    TNotePerson(NoteTree.ItemFocused.Data).;
  end
  else if Item.IsUnder(frmNoteBook.NoteTree.Items.RootItem[2]) then
  begin
    if (Now > TNoteRemind(Item.Data).RemindTime) and
       (TNoteRemind(Item.Data).Enabled)
    then
    begin
      GetMem(pStr, (Length(TNoteRemind(Item.Data).Title))+1);
      StrPCopy(pStr, TNoteRemind(Item.Data).Title);
      TNoteRemind(Item.Data).Enabled := False;
      frmNoteBook.ckRemindEnabled.Checked := False;
      Application.MessageBox(pStr , 'Remind:', MB_ICONINFORMATION + MB_APPLMODAL + MB_OK);
      FreeMem(pStr);
    end;
//    TNoteRemind(Item.Data).RemindTime;
//    TNoteRemind(Item.Data).Enabled;
  end;
end;

procedure TfrmNoteBook.ElTimerPool1Items0Timer(Sender: TObject);
begin
  NoteTree.Items.Iterate(False, true, EventCheck, nil);
end;

procedure TfrmNoteBook.ElTrayIcon1Click(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowWindow(Application.Handle, SW_SHOW);
  Application.Restore;
  Application.BringToFront;
  Application.MainForm.BringToFront;
end;

procedure TfrmNoteBook.ElGraphicButton2Click(Sender: TObject);
begin
  Application.CreateForm(TSettings, Settings);
  Settings.ShowModal;
end;

procedure TfrmNoteBook.ckPeriodEnabledClick(Sender: TObject);
begin
  TNoteEvent(NoteTree.ItemFocused.Data).TimePeriodEnabled := ckPeriodEnabled.Checked;
end;

end.
