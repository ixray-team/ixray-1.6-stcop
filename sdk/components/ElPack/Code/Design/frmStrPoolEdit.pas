{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit frmStrPoolEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElTree, StdCtrls, ElACtrls, ElSplit, ExtCtrls, ElPanel, ElBtnCtl,
  ElPopBtn, Menus, ElStrArray,
{$ifdef VCL_6_USED}
  DesignIntf, DesignEditors, DesignWindows, DsnConst,  
{$else}
  DsgnIntf, 
{$endif}
  ElStrPool, ElVCLUtils, ElStrUtils, TypInfo, ElFrmPers, ElIni,
  ElXPThemedControl, ElEdits;

type
  TStrPoolEditForm = class(TForm)
    ElPanel1: TElPanel;
    ElPanel2: TElPanel;
    ElPanel3: TElPanel;
    ElSplitter1: TElSplitter;
    ElPanel4: TElPanel;
    List: TElTree;
    OkBtn: TElPopupButton;
    CancelBtn: TElPopupButton;
    AddBtn: TElPopupButton;
    InsertBtn: TElPopupButton;
    DeleteBtn: TElPopupButton;
    PopupMenu: TPopupMenu;
    AddItem: TMenuItem;
    InsertItem: TMenuItem;
    DeleteItem: TMenuItem;
    MainMenu: TMainMenu;
    Pool1: TMenuItem;
    Clear1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Text1: TMenuItem;
    Open2: TMenuItem;
    Save2: TMenuItem;
    ElFormPersist1: TElFormPersist;
    ElIniFile1: TElIniFile;
    UpBtn: TElPopupButton;
    DownBtn: TElPopupButton;
    CopyBtn: TElPopupButton;
    Memo: TElEdit;
    procedure ListItemFocused(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure InsertBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure ListItemDeletion(Sender: TObject; Item: TElTreeItem);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListVirtualTextNeeded(Sender: TObject; Item: TElTreeItem;
      SectionIndex: Integer; var Text: TElFString);
    procedure ListVirtualHintNeeded(Sender: TObject; Item: TElTreeItem;
      var Hint: TElFString);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    CurIndex : integer;
    StrArray : TElFStringArray;
  public
    procedure GetData(anArray : TElFStringArray);
    procedure SetData(anArray : TElFStringArray);
  end;

var
  StrPoolEditForm: TStrPoolEditForm;

type
  TStrPoolItemsProperty = class(TPropertyEditor)
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
  end;

type
  TStrPoolItemsEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

implementation

{$R *.DFM}

procedure TStrPoolItemsEditor.ExecuteVerb(Index : Integer);
var Form : TCustomForm;
begin
  if Index = 0 then
  begin
    with TStrPoolEditForm.Create(Application) do
    begin
      try
        { Set dialog's caption. }
        if Component is TElStringPool then 
          SetData(TElStringPool(Component).Items);
        if (ShowModal = mrOk) then
        begin
          Form := GetOwnerForm(Component);
          if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
          GetData((Component as TElStringPool).Items);
        end;
      finally
        Free;  { Free dialog. }
      end;  { try/finally }
    end;  { with }
  end;
end;

function TStrPoolItemsEditor.GetVerb(Index : Integer) : string;
begin
  if Index = 0 then Result := 'Items Editor';
end;

function TStrPoolItemsEditor.GetVerbCount : Integer;
begin
  Result := 1;
end;

procedure TStrPoolItemsProperty.Edit;
var anArray : TElFStringArray;
    b       : boolean;
begin
  with TStrPoolEditForm.Create(Application) do
  begin
    try
      b := TObject(GetOrdProp(GetComponent(0), GetPropInfo)) is TElFStringArray;
      if not b then
      begin
        anArray := TElFStringArray.Create;
        anArray.Assign(TObject(GetOrdProp(GetComponent(0), GetPropInfo)) as TElStringArray);
      end
      else
        anArray := TObject(GetOrdProp(GetComponent(0), GetPropInfo)) as TElFStringArray;
      SetData(anArray);
      if (ShowModal = mrOk) then
      begin
        {Form := GetOwnerForm(GetComponent(0));
        if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;}
        GetData(anArray);
      end;
      if not b then AnArray.Free;
    finally
      Free;  { Free dialog. }
    end;  { try/finally }
  end;  { with }
end;  { Edit }

function TStrPoolItemsProperty.GetValue : string;
begin
  result := 'String Array Items';
end;

function TStrPoolItemsProperty.GetAttributes : TPropertyAttributes;
{ Returns information used by the Object Inspector to show the approprate adornments (e.g., "..." button, drop-down button). }
begin
  GetAttributes := [paDialog];
end;  { GetAttributes }

procedure TStrPoolEditForm.ListItemFocused(Sender: TObject);
var b : boolean;
begin
  if csDestroying in ComponentState then exit;
  b := List.ItemFocused <> nil;
  InsertBtn.Enabled := b;
  DeleteBtn.Enabled := b;
  InsertItem.Enabled := b;
  DeleteItem.Enabled := b;
  Memo.Enabled := b;
  CopyBtn.Enabled := b;

  if CurIndex <> -1 then
  begin
    StrArray[CurIndex] := Memo.Text;
    List.Items[CurIndex].RedrawItem(true);
    // List.Items[CurIndex].Text := StrArray[CurIndex];
  end;
  if b then
     CurIndex := List.ItemFocused.Index
  else
     CurIndex := -1;
  if CurIndex <> -1 then
    Memo.Text := StrArray[CurIndex];
  UpBtn.Enabled := CurIndex > 0;
  DownBtn.Enabled := (CurIndex >= 0) and (CurIndex < List.Items.Count - 1);
end;

procedure TStrPoolEditForm.AddBtnClick(Sender: TObject);
begin
  List.ItemFocused := List.Items.AddItem(nil);
  // List.ItemFocused.ColumnText.Add(IntToStr(List.ItemFocused.Index));
  StrArray.Add('');
  Memo.SetFocus;
end;

procedure TStrPoolEditForm.InsertBtnClick(Sender: TObject);
//var oi : TElTreeItem;
//    i : integer;
begin
  List.Items.BeginUpdate;
  // oi := List.ItemFocused;
  List.ItemFocused := List.Items.InsertItem(List.ItemFocused.Index, nil);
  StrArray.Insert(CurIndex, '');
  // oi.Text := List.ItemFocused.Text;
  // List.ItemFocused.Text := '';
  Memo.Text := '';
  List.Items.EndUpdate;
  Memo.SetFocus;
  (*
  List.Items.BeginUpdate;
  for i := 0 to List.Items.Count - 1 do
  begin
    if List.Items[i].ColumnText.Count > 0 then
       List.Items[i].ColumnText[0] := IntToStr(i)
    else
       List.Items[i].ColumnText.Add(IntToStr(i));
  end;
  List.Items.EndUpdate;
  *)
end;

procedure TStrPoolEditForm.DeleteBtnClick(Sender: TObject);
// var i : integer;
begin
  Memo.Lines.Clear;
  CurIndex := -1;
  List.Items.BeginUpdate;
  if List.ItemFocused <> nil then
    List.Items.DeleteItem(List.ItemFocused);
  // for i := 0 to List.Items.Count - 1 do List.Items[i].ColumnText[0] := IntToStr(i);
  List.Items.EndUpdate;
  UpBtn.Enabled := CurIndex > 0;
  DownBtn.Enabled := (CurIndex >= 0) and (CurIndex < List.Items.Count - 1);
end;

procedure TStrPoolEditForm.ListItemDeletion(Sender: TObject;
  Item: TElTreeItem);
begin
  if StrArray <> nil then
     StrArray.Delete(Item.Index);
end;

procedure TStrPoolEditForm.GetData(anArray : TElFStringArray);
begin
  if CurIndex <> -1 then
    StrArray[CurIndex] := Memo.Text;
  anArray.Assign(StrArray);
end;

procedure TStrPoolEditForm.SetData(anArray : TElFStringArray);
var i : integer;
    //ti: TElTreeItem;
begin
  StrArray.Assign(anArray);
  CurIndex := -1;

  for i := 0 to StrArray.Count - 1 do    { Iterate }
  begin
    List.Items.AddItem(nil);
    //ti.Text := StrArray[i];
    //ti.ColumnText.Add(IntToStr(i));
  end;    { for }
end;

procedure TStrPoolEditForm.FormCreate(Sender: TObject);
begin
  StrArray := TElFStringArray.Create;
end;

procedure TStrPoolEditForm.FormDestroy(Sender: TObject);
begin
  StrArray.Free;
  StrArray := nil; 
end;

procedure TStrPoolEditForm.ListVirtualTextNeeded(Sender: TObject;
  Item: TElTreeItem; SectionIndex: Integer; var Text: TElFString);
begin
  if SectionIndex = 0 then
    Text := IntToStr(Item.AbsoluteIndex)
  else
    Text := Copy(StrArray[Item.AbsoluteIndex], 1, 128);
end;

procedure TStrPoolEditForm.ListVirtualHintNeeded(Sender: TObject;
  Item: TElTreeItem; var Hint: TElFString);
begin
  Hint := StrArray[Item.AbsoluteIndex];
end;

procedure TStrPoolEditForm.UpBtnClick(Sender: TObject);
var i : integer;
begin
  Memo.Lines.Clear;
  CurIndex := -1;

  if List.ItemFocused <> nil then
  begin
    i := List.ItemFocused.Index;
    StrArray.Move(i, i - 1);
    List.ItemFocused.MoveToIns(nil, i - 1);
  end;
  ListItemFocused(Self);
  if List.ItemFocused <> nil then
    List.EnsureVisible(List.ItemFocused);
end;

procedure TStrPoolEditForm.DownBtnClick(Sender: TObject);
var i : integer;
begin
  Memo.Lines.Clear;
  CurIndex := -1;

  if List.ItemFocused <> nil then
  begin
    i := List.ItemFocused.Index;
    StrArray.Move(i, i + 1);
    List.ItemFocused.MoveToIns(nil, i + 1);
  end;
  ListItemFocused(Self);
  if List.ItemFocused <> nil then
    List.EnsureVisibleBottom(List.ItemFocused);
end;

procedure TStrPoolEditForm.CopyBtnClick(Sender: TObject);
var s : String;
begin
  if CurIndex <> -1 then
    S := Memo.Text// StrArray[CurIndex]
  else
    S := '';

  List.ItemFocused := List.Items.AddItem(nil);
  // List.ItemFocused.ColumnText.Add(IntToStr(List.ItemFocused.Index));
  List.ItemFocused.RedrawItem(true);
  StrArray.Add(S);
  Memo.Text := S;
  Memo.SetFocus;
end;

procedure TStrPoolEditForm.OkBtnClick(Sender: TObject);
begin
  List.ItemFocused := nil;
end;

end.

