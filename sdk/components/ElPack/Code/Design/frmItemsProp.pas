{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit frmItemsProp;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
{$ifdef VCL_6_USED}
  DesignIntf, DesignEditors, DesignWindows, DsnConst,  
{$else}
  DsgnIntf, 
{$endif}

  Buttons, ExtCtrls, ElTree, frmItemCol, Dialogs,
{$IFDEF ELPACK_COMPLETE}
  ElTreeCombo,
{$ENDIF}
  ElVCLUtils, ElXPThemedControl;

type
  TItemsPropDlg = class(TForm)
    ItemsGB : TGroupBox;
    OpenDlg : TOpenDialog;
    SaveDlg : TSaveDialog;
    Tree : TElTree;
    Panel1: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    Panel2: TPanel;
    NewItemBtn: TButton;
    SubitemBtn: TButton;
    DeleteBtn: TButton;
    SaveBtn: TButton;
    LoadBtn: TButton;
    EditBtn: TButton;
    MoveRightBtn: TButton;
    MoveLeftBtn: TButton;
    MoveDownBtn: TButton;
    MoveUpBtn: TButton;
    DuplicateBtn: TButton;
    procedure DeleteBtnClick(Sender : TObject);
    procedure SubitemBtnClick(Sender : TObject);
    procedure TreeItemFocused(Sender : TObject);
    procedure NewItemBtnClick(Sender : TObject);
    procedure EditBtnClick(Sender : TObject);
    procedure OKBtnClick(Sender : TObject);
    procedure SaveBtnClick(Sender : TObject);
    procedure LoadBtnClick(Sender : TObject);
    procedure TreeStartDrag(Sender : TObject; var DragObject : TDragObject);
    procedure TreeDragOver(Sender, Source : TObject; X, Y : Integer;
      State : TDragState; var Accept : Boolean);
    procedure TreeDragDrop(Sender, Source : TObject; X, Y : Integer);
    procedure FormCreate(Sender : TObject);
    procedure TreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MoveRightBtnClick(Sender: TObject);
    procedure MoveLeftBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure TreeDblClick(Sender: TObject);
    procedure DuplicateBtnClick(Sender: TObject);
  private
    { Private declarations }
    FDragItem : TElTreeItem;
//    procedure SetItems(value : TElTreeItems);
//    function  GetItems:TElTreeItems;
  public
    { Public declarations }
    AComp : TComponent;
    DTreeItems : TElTreeItems;
  end;

  TElTreeItemsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
  end;

var
  ItemsPropDlg : TItemsPropDlg;

implementation

{$R *.DFM}

type
  TElDragObject = class(TDragControlObject)
    function GetDragCursor(Accepted : Boolean; X, Y : Integer) : TCursor; override;
  end;

function TElDragObject.GetDragCursor(Accepted : Boolean; X, Y : Integer) : TCursor;
begin
  if Control is TElTree then
  begin
    if ((Control as TElTree).GetItemAtY(Y) <> nil) or (Accepted) then
      Result := (Control as TElTree).DragCursor
    else
      Result := crNoDrop;
  end
  else
    result := inherited GetDragCursor(Accepted, X, Y);
end;

procedure TElTreeItemsProperty.Edit;
var
  Editor : TItemsPropDlg;
begin
  Editor := TItemsPropDlg.Create(Application);
  try
    Editor.AComp := TComponent(GetComponent(0));
{$IFDEF ELPACK_COMPLETE}
    if Editor.AComp is TElTreeCombo then
    begin
      Editor.DTreeItems := TElTreeCombo(Editor.AComp).Items;
    end
    else
{$ENDIF}
      Editor.DTreeItems := TCustomElTree(Editor.AComp).Items;
    Editor.Tree.Items := Editor.DTreeItems;
    Editor.ShowModal;
  finally
    Editor.Free;
  end;
end;

function TElTreeItemsProperty.GetAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TElTreeItemsProperty.GetValue;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

// ===========================================================================

procedure TItemsPropDlg.DeleteBtnClick(Sender : TObject);
var
  Form : TCustomForm;
begin
  if Tree.ItemFocused <> nil then Tree.Items.DeleteItem(Tree.ItemFocused);
  Form := GetOwnerForm(AComp);
  if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
end;

procedure TItemsPropDlg.SubitemBtnClick(Sender : TObject);
var
  TSI : TElTreeItem;
var
  Form : TCustomForm;
begin
  if Tree.ItemFocused <> nil then
  begin
    Tree.ItemFocused.Expanded := true;
    TSI := Tree.Items.AddItem(Tree.ItemFocused);
    TSI.Text := 'Item ' +IntToStr(TSI.AbsoluteIndex);
    Tree.ItemFocused := TSI;
    Tree.EnsureVisible(TSI);
    Form := GetOwnerForm(AComp);
    if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
  end;
end;

procedure TItemsPropDlg.TreeItemFocused(Sender : TObject);
var b : boolean;
begin
  b := Tree.ItemFocused <> nil;
  SubItemBtn.Enabled := b;
  EditBtn.Enabled := b;
  DeleteBtn.Enabled := b;
  MoveUpBtn.Enabled := b;
  MoveDownBtn.Enabled := b;
  MoveLeftBtn.Enabled := b;
  MoveRightBtn.Enabled := b;
  DuplicateBtn.Enabled := b;
end;

procedure TItemsPropDlg.NewItemBtnClick(Sender : TObject);
var
  TSI : TElTreeItem;
var
  Form : TCustomForm;
begin
  TSI := Tree.Items.AddItem(nil);
  TSI.Text := 'Item ' +IntToStr(TSI.AbsoluteIndex);
  Tree.ItemFocused := TSI;
  Tree.EnsureVisible(TSI);
  Form := GetOwnerForm(AComp);
  if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
end;

procedure TItemsPropDlg.EditBtnClick(Sender : TObject);
var
  T : TItemColDlg;
var
  Form : TCustomForm;
begin
  if Tree.ItemFocused = nil then exit;
  T := nil;
  try
    T := TItemColDlg.Create(self);
    T.Item := Tree.ItemFocused;
    T.SetData;
    if T.ShowModal = mrOk then
    begin
      T.GetData;
      Form := GetOwnerForm(AComp);
      if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
    end;
    Tree.SetFocus;
  finally
    T.Free;
  end;
end;

procedure TItemsPropDlg.OKBtnClick(Sender : TObject);
begin
  if Tree.Items = nil then
    MessageBox(0, 'Serious error in ElTreeItems editor. Please report to EldoS', nil, 0)
  else
    DTreeItems.Assign(Tree.Items);
end;

procedure TItemsPropDlg.SaveBtnClick(Sender : TObject);
var
  T : TFileStream;
begin
  if not SaveDlg.Execute then exit;
  T := nil;
  try
    T := TFileStream.Create(SaveDlg.FileName, fmCreate or fmShareDenyWrite);
    Tree.Items.SaveToStream(T);
  finally
    T.Free;
  end;
end;

procedure TItemsPropDlg.LoadBtnClick(Sender : TObject);
var
  T : TFileStream;
var
  Form : TCustomForm;
begin
  if not OpenDlg.Execute then exit;
  T := nil;
  try
    T := TFileStream.Create(OpenDlg.FileName, fmOpenRead or fmShareDenyWrite);
    Tree.Items.LoadFromStream(T);
    Form := GetOwnerForm(AComp);
    if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
  finally
    T.Free;
  end;
end;

procedure TItemsPropDlg.TreeStartDrag(Sender : TObject;
  var DragObject : TDragObject);
begin
  FDragItem := Tree.ItemFocused;
  DragObject := TElDragObject.Create(Tree);
end;

procedure TItemsPropDlg.TreeDragOver(Sender, Source : TObject; X,
  Y : Integer; State : TDragState; var Accept : Boolean);
var
  TSI : TElTreeItem;
begin
  Accept := false;
  if (Source <> Tree) and ((not (Source is TDragControlObject)) or (TDragControlObject(Source).Control <> Tree)) then exit;
  TSI := Tree.GetItemAtY(Y);
  if (TSI <> nil) and (not TSI.IsUnder(FDragItem)) then Accept := true;
end;

procedure TItemsPropDlg.TreeDragDrop(Sender, Source : TObject; X,
  Y : Integer);
var
  TSI : TElTreeItem;
var
  Form : TCustomForm;
begin
  TSI := Tree.GetItemAtY(Y);
  if (TSI <> nil) and (not TSI.IsUnder(FDragItem)) then FDragItem.MoveToIns(TSI.Parent, TSI.Index);
  Form := GetOwnerForm(AComp);
  if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
end;

procedure TItemsPropDlg.FormCreate(Sender : TObject);
begin
  TreeItemFocused(self);
end;

procedure TItemsPropDlg.TreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_LEFT) and (Shift = [ssCtrl]) then MoveLeftBtnClick(Self) else
  if (Key = VK_RIGHT) and (Shift = [ssCtrl]) then MoveRightBtnClick(Self) else
  if (Key = VK_UP) and (Shift = [ssCtrl]) then MoveUpBtnClick(Self) else
  if (Key = VK_DOWN) and (Shift = [ssCtrl]) then MoveDownBtnClick(Self);
end;

procedure TItemsPropDlg.MoveRightBtnClick(Sender: TObject);
var Item : TElTreeItem;
begin
  if (Tree.ItemFocused <> nil) then
  begin
    Item := Tree.ItemFocused;
    if Item.GetPrevSibling <> nil then
       Item.MoveToIns(Item.GetPrevSibling, 0);
  end;
end;

procedure TItemsPropDlg.MoveLeftBtnClick(Sender: TObject);
var Item : TElTreeItem;
begin
  if (Tree.ItemFocused <> nil) then
  begin
    Item := Tree.ItemFocused;
    if Item.Parent <> nil then
    begin
      Item.MoveToIns(Item.Parent.Parent, Item.Parent.Index + 1);
    end;
  end;
end;

procedure TItemsPropDlg.MoveUpBtnClick(Sender: TObject);
var Item : TElTreeItem;
begin
  if (Tree.ItemFocused <> nil) then
  begin
    Item := Tree.ItemFocused;
    if Item.Index > 0 then
      Item.MoveToIns(Item.Parent, Item.Index - 1);
  end;
end;

procedure TItemsPropDlg.MoveDownBtnClick(Sender: TObject);
var Item : TElTreeItem;
begin
  if (Tree.ItemFocused <> nil) then
  begin
    Item := Tree.ItemFocused;
    if Item.GetNextSibling <> nil then
      Item.MoveToIns(Item.Parent, Item.GetNextSibling.Index);
  end;
end;

procedure TItemsPropDlg.TreeDblClick(Sender: TObject);
var HS : integer;
    Item : TElTreeItem;
    ItemPart: TSTItemPart;
    P       : TPoint;
begin
  GetCursorPos(P);
  P := Tree.ScreenToClient(P);
  Item := Tree.GetItemAt(P.x, P.Y, ItemPart, HS);
  if Item <> nil then
  begin
    Tree.ItemFocused := Item;
    EditBtnClick(Self);
  end;
end;

procedure TItemsPropDlg.DuplicateBtnClick(Sender: TObject);
var Item,
    Item1 : TElTreeItem;

begin
  if (Tree.ItemFocused <> nil) then
  begin
    Item := Tree.ItemFocused;
    Item1 := Tree.Items.InsertItem(Item.Index + 1, Item.Parent);
    Item1.Assign(Item);
    Item1.Text := 'Item ' +IntToStr(Item1.AbsoluteIndex);
    Tree.ItemFocused := Item1;
    Tree.EnsureVisible(Item1);
  end;
end;

end.
