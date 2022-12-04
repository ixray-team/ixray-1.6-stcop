unit find;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElBtnCtl, ElPopBtn, StdCtrls, ElLabel, ElACtrls, frmMain,
  ElXPThemedControl, ElCLabel;

type
  TfrmFind = class(TForm)
    edFindString: TElAdvancedEdit;
    ElLabel1: TElLabel;
    frmFind: TElPopupButton;
    ElPopupButton2: TElPopupButton;
    procedure ElPopupButton2Click(Sender: TObject);
    procedure frmFindClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFind: TfrmFind;

implementation

{$R *.DFM}

uses ElTree, NoteRecord;

procedure TfrmFind.ElPopupButton2Click(Sender: TObject);
begin
  Close;
end;

function CmpNote(Item: TElTreeItem; SearchDetails: Pointer): boolean;
begin
  if TNoteRecord(Item.Data).Title = String(SearchDetails^) then
    CmpNote := True
  else CmpNote := False;
end;

procedure TfrmFind.frmFindClick(Sender: TObject);

var
  SearchTitle: String;
  TempItem: TElTreeItem;
begin
  SearchTitle := edFindString.Text;
  if frmNoteBook.NoteControl.ActivePage = frmNoteBook.tabPerson then
  TempItem := frmNoteBook.NoteTree.Items.LookForItemEx(frmNoteBook.Person,
                            0, False, True, False, @SearchTitle, CmpNote)
  else if frmNoteBook.NoteControl.ActivePage = frmNoteBook.tabEvent then
  TempItem := frmNoteBook.NoteTree.Items.LookForItemEx(frmNoteBook.Event,
                            0, False, True, False, @SearchTitle, CmpNote)
  else if frmNoteBook.NoteControl.ActivePage = frmNoteBook.tabRemind then
  TempItem := frmNoteBook.NoteTree.Items.LookForItemEx(frmNoteBook.Remind,
                            0, False, True, False, @SearchTitle, CmpNote);

  if TempItem = nil then
  Application.MessageBox('Item not found"!"',
              '', MB_ICONWARNING + MB_APPLMODAL + IDOK)
  else
  begin
    TempItem.FullyExpanded := true;
    TempItem.Focused := true;
    TempItem.Selected := true;
  end;
  Close;
end;

end.
