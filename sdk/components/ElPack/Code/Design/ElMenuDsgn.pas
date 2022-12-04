{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{   Portion copyright (c) 2001, Alexander Hramov     }
{                                                    }
{====================================================}

(*

Version History
02/15/2001

  Add new features to menu editor: Load, Save, Move Item Up, Move Item Down, 
  << (Level Up) and << (Level Down), Drag&Drop.

20/12/2001
  
*)

{$I ..\ElPack.inc}

unit ElMenuDsgn;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  ElMenus,
  ElVCLUtils,
  ElXPThemedControl,
  ElStrUtils,
  ElTree,
  {$ifdef VCL_4_USED}
  ImgList,
  {$endif}
  {$ifdef ELPACK_UNICODE}
  ElUnicodeStrings,
  {$endif}
  ExtCtrls,
  {$ifdef VCL_6_USED}
  DesignEditors,
  DesignConst,
  DesignIntf
  {$else}
  DsgnIntf
  {$endif}
  ;

type
  TElDesignMenu = class(TForm)
    GroupBox1: TGroupBox;
    MenuEdit: TElTree;
    Panel2: TPanel;
    NewSubItemBtn: TButton;
    NewItemBtn: TButton;
    DeleteItemBtn: TButton;
    Load: TButton;
    Save: TButton;
    Bevel1: TBevel;
    OpenMenuDlg: TOpenDialog;
    SaveMenuDlg: TSaveDialog;
    Bevel2: TBevel;
    MoveUp: TButton;
    MoveDown: TButton;
    LevelUp: TButton;
    LevelDown: TButton;
    procedure NewItemBtnClick(Sender: TObject);
    procedure NewSubItemBtnClick(Sender: TObject);
    procedure DeleteItemBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MenuEditItemFocused(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
    procedure LevelUpClick(Sender: TObject);
    procedure LevelDownClick(Sender: TObject);
    procedure LoadClick(Sender: TObject);
    procedure MenuEditStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure MenuEditDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MenuEditDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    FMenu: TMenu;
    FDragItem: TElTreeItem;
    FElMenuItem: TElMenuItem;
    procedure SetElMenu(const Value: TMenu);
    procedure MenuChanged(Sender: TObject; Source: TElMenuItem; Rebuild: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    AComp: TComponent;
    ADesigner: {$ifdef VCL_4_USED}
                 {$ifdef VCL_6_USED}
                   IDesigner
                 {$else}
                   IFormDesigner
                 {$endif}
               {$else}
                 TFormDesigner
               {$endif};
    
    destructor Destroy; override;
    property Menu : TMenu read FMenu write SetElMenu;
  end;

type
  TElMenuEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TElMenuItemsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
  end;

{$ifdef D_6_UP}
var
{$else}
const
{$endif}
  ElDesignMenu : TElDesignMenu = nil;

implementation

{$R *.dfm}

{ TElDesignMenu }

function NewItem(Owner: TComponent; const ACaption: TElFString; AShortCut: TShortCut;
                 AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent; hCtx: Word;
                 const AName: TElFString): TElMenuItem;
begin
  Result := TElMenuItem.Create(Owner);
  with Result do
  begin
    Caption := ACaption;
    ShortCut := AShortCut;
    OnClick := AOnClick;
    HelpContext := hCtx;
    Checked := AChecked;
    Enabled := AEnabled;
    Name := AName;
  end;
end;

procedure TElMenuEditor.ExecuteVerb(Index: Integer);
begin
  try
    if ElDesignMenu = nil then
      ElDesignMenu := TElDesignMenu.Create(Application);
    with ElDesignMenu do
    begin
      AComp := Component;
      ADesigner := Self.Designer;
      if Component is TElPopupMenu then
        Menu := TElPopupMenu(Component)
      else
        Menu := TElMainMenu(Component);
      Show;
      BringToFront;
      ElDesignMenu.Caption := AComp.Owner.Name+'.'+AComp.Name;
    end;
  finally
    // ElDesignMenu.Free;
  end;
end;

function TElMenuEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'ElMenu Designer...';
  end;
end;

function TElMenuEditor.GetVerbCount: Integer;
begin
  Result := 1
end;

{ TElMenuItemsProperty }

procedure TElMenuItemsProperty.Edit;
begin
  try
    if ElDesignMenu = nil then
      ElDesignMenu := TElDesignMenu.Create(Application);
    with ElDesignMenu do
    begin
      AComp := TComponent(GetComponent(0));
      ADesigner := Self.Designer;
      if GetComponent(0) is TElPopupMenu then
        Menu := TElPopupMenu(GetComponent(0))
      else
        Menu := TElMainMenu(GetComponent(0));
      Show;
      BringToFront;
      ElDesignMenu.Caption := AComp.Owner.Name+'.'+AComp.Name;
    end;
  finally
    // ElDesignMenu.Free;
  end;
end;

function TElMenuItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TElMenuItemsProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

type
  TElDragObject = class (TDragControlObject)
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  end;

function TElDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Control is TElTree then
  begin
    if ((Control as TElTree).GetItemAtY(Y) <> nil) or (Accepted) then
       Result := (Control as TElTree).DragCursor
    else
       Result := crNoDrop;
  end
  else
    Result:=inherited GetDragCursor(Accepted,X,Y);
end;

procedure TElDesignMenu.MenuChanged(Sender: TObject; Source: TElMenuItem; Rebuild: Boolean);
var
  Form: TCustomForm;
  Temp: TElMenuItem;
begin
  if csDestroying in ComponentState then exit;
  if MenuEdit <> nil then
  if (MenuEdit.ItemFocused <> nil) and (MenuEdit.ItemFocused.Data <> nil) and
     (TComponent(MenuEdit.ItemFocused.Data) is TElMenuItem) then
  begin
    Temp := TElMenuItem(MenuEdit.ItemFocused.Data);

    if FMenu is TElPopupMenu then
    begin
      if (TElPopupMenu(FMenu).Images <> nil) and (MenuEdit.Images = nil) then
        MenuEdit.Images := TImageList(TElPopupMenu(FMenu).Images);
    end
    else
    begin
      if (TElMainMenu(FMenu).Images <> nil) then
        MenuEdit.Images := TImageList(TElMainMenu(FMenu).Images);
    end;

    MenuEdit.ItemFocused.Text := TElMenuItem(MenuEdit.ItemFocused.Data).Caption;
    MenuEdit.ItemFocused.ImageIndex := TElMenuItem(MenuEdit.ItemFocused.Data).ImageIndex;

    MenuEdit.ItemFocused.Checked := TElMenuItem(MenuEdit.ItemFocused.Data).Checked;
    MenuEdit.ItemFocused.Bold := TElMenuItem(MenuEdit.ItemFocused.Data).Default;
    MenuEdit.ItemFocused.ParentStyle := false;
    MenuEdit.ItemFocused.CheckBoxEnabled := false;
    MenuEdit.ItemFocused.ShowCheckBox := MenuEdit.ItemFocused.Checked;

    if (Temp.Caption <> '-') or (FMenu is TElPopupMenu) then
    begin
      while Temp.Parent <> nil do
        Temp := TElMenuItem(Temp.Parent);
      Temp.DesignRebuild;
    end
    else
    begin
      if (Temp.Parent <> nil) then
      begin
        Temp.OnChange:= nil;
        TElMenuItem(Temp.Parent).OnChange := nil;
        TElMenuItem(Temp.Parent).Remove(Temp);
        InsertItems(FMenu, TElMenuItem(MenuEdit.ItemFocused.Parent.Data), MenuEdit.ItemFocused.Index, Temp);
        Temp.OnChange := MenuChanged;
        TElMenuItem(Temp.Parent).OnChange := MenuChanged;
      end
      else
        Temp.DesignRebuild;
    end;

    Form := GetOwnerForm(FMenu);
    if (Form <> nil) and (not (csDestroying in Form.ComponentState)) then
      DrawMenuBar(Form.Handle);
  end;
end;

procedure TElDesignMenu.SetElMenu(const Value: TMenu);
var
  j: integer;
  Count: Integer;

  function AddMenuItem (MenuItem : TElMenuItem) : TElTreeItem;
  var
    i: integer;
    TreeNode: TElTreeItem;
    Node: TElTreeItem;
  begin
    MenuItem.OnChange := MenuChanged;
    TreeNode := MenuEdit.Items.AddChildObject(MenuEdit.ItemFocused, MenuItem.Caption, Pointer(MenuItem));
    TreeNode.ImageIndex := MenuItem.ImageIndex;
    result := TreeNode;
    TreeNode.ParentStyle := false;
    TreeNode.CheckBoxEnabled := false;
    TreeNode.Checked := MenuItem.Checked;
    TreeNode.ShowcheckBox := MenuItem.Checked;
    TreeNode.Bold := MenuItem.Default;

    for i := 0 to MenuItem.Count - 1 do
    begin
      MenuEdit.ItemFocused := TreeNode;
      MenuEdit.ItemFocused.Expanded := true;

      if TElMenuItem(MenuItem[i]).Count > 0 then
        Node := AddMenuItem(TElMenuItem(TElMenuItem(MenuItem).Items[i]))
      else
        Node := MenuEdit.Items.AddChildObject(MenuEdit.ItemFocused, TElMenuItem(MenuItem.Items[i]).Caption, Pointer(MenuItem.Items[i]));

      Node.ImageIndex := TElMenuItem(MenuItem[i]).ImageIndex;

      Node.Checked := TElMenuItem(MenuItem[i]).Checked;
      Node.Bold := TElMenuItem(MenuItem[i]).Default;
      Node.ParentStyle := false;
      Node.CheckBoxEnabled := false;
      Node.ShowCheckBox := Node.Checked;
    end;
  end;

begin
  if (Value <> nil) then
  begin
    {$ifdef VCL_5_USED}
    if (FMenu <> nil) then
      FMenu.RemoveFreeNotification(Self);
    {$endif}
    FMenu := Value;

    if csDestroying in ComponentState then
      exit;

    if (FMenu <> nil) then
      FMenu.FreeNotification(Self);

    MenuEdit.IsUpdating := true;
    try
      MenuEdit.Items.Clear;
      if FMenu is TElPopupMenu then
      begin
        if (TElPopupMenu(FMenu).Images <> nil) then
          MenuEdit.Images := TImageList(TElPopupMenu(FMenu).Images);
      end
      else
      begin
        if (TElMainMenu(FMenu).Images <> nil) then
          MenuEdit.Images := TImageList(TElMainMenu(FMenu).Images);
      end;

      if Menu is TElPopupMenu then
        Count := TElPopupMenu(Menu).Items.Count
      else
        Count := TElMainMenu(Menu).Items.Count;

      for j := 0 to Count - 1 do
      begin
        MenuEdit.ItemFocused := nil;

        if Menu is TElPopupMenu then
          AddMenuItem(TElMenuItem(TElPopupMenu(Menu).Items[j]))
        else
          AddMenuItem(TElMenuItem(TElMainMenu(Menu).Items[j]));
      end;
    finally
      MenuEdit.IsUpdating := false;
    end;
    // MenuEditItemFocused(Self);
  end;
end;

procedure TElDesignMenu.NewItemBtnClick(Sender: TObject);
var
  Index: Integer;
  TreeNode: TElTreeItem;
  Node: TElTreeItem;
  FElName: String;
  Form: TCustomForm;
  Temp: TElMenuItem;

begin
  Form := GetOwnerForm(FMenu);
  if (Form <> nil) and (Form.Designer <> nil) then
    FElName := {$ifdef VCL_5_USED}Form.Designer.UniqueName('ElMenuItem')
                           {$else}ADesigner.UniqueName('ElMenuItem'){$endif}
  else
    FElName := 'ElMenuItem';

  FElMenuItem := TElMenuItem.Create(ADesigner.GetRoot); //NewItem('ElMenuItem', 0, false, true, nil, 0, FElName);
  FElMenuItem.Caption := 'ElMenuItem';
  FElMenuItem.OnChange := MenuChanged;
  FElMenuItem.Name := FElName;

  Index := -1;
  TreeNode := nil;

  if (MenuEdit.ItemFocused <> nil) then
  begin
    Index:= MenuEdit.ItemFocused.Index;
    if (MenuEdit.ItemFocused.Level <> 0) then
      TreeNode:= MenuEdit.ItemFocused.Parent;
  end;

  if (TreeNode = nil) then
  begin
    InsertMenuItems(FMenu, Index, FElMenuItem);
    Temp := FElMenuItem;
  end
  else
  begin
    InsertItems(FMenu, TElMenuItem(TreeNode.Data), Index, FElMenuItem);
    Temp := TElMenuItem(TreeNode.Data);
  end;

  while Temp.Parent <> nil do
    Temp := TElMenuItem(Temp.Parent);
  Temp.DesignRebuild;

  if (Index <> -1) then
    Node := MenuEdit.Items.InsertObject(MenuEdit.ItemFocused, FElMenuItem.Caption, Pointer(FElMenuItem))
  else
    Node := MenuEdit.Items.AddChildObject(TreeNode, FElMenuItem.Caption, Pointer(FElMenuItem));

  Node.ImageIndex:= FElMenuItem.ImageIndex;
  Node.StateImageIndex := FElMenuItem.ImageIndex;
  MenuEdit.ItemFocused:= Node;
  Node.Checked := FElMenuItem.Checked;
  Node.Bold := FElMenuItem.Default;
  Node.ParentStyle := false;
  Node.CheckBoxEnabled := false;
  Node.ShowCheckBox := FElMenuItem.Checked;
  if (Form <> nil) and (Form.Designer <> nil) and not (csDestroying in Form.ComponentState) then
    Form.Designer.Modified;

  if (ADesigner <> nil) then
    ADesigner.Modified;

  if (Form <> nil) and not (csDestroying in Form.ComponentState) then
    DrawMenuBar(Form.Handle);
end;

procedure TElDesignMenu.NewSubItemBtnClick(Sender: TObject);
Var
  TreeNode: TElTreeItem;
  Node: TElTreeItem;
  Form: TCustomForm;
  Temp: TElMenuItem;

begin
  TreeNode := MenuEdit.ItemFocused;
  if (TreeNode <> nil) and (TreeNode.Data <> nil) then
  begin
    FElMenuItem:= NewItem(ADesigner.GetRoot, 'ElMenuItem', 0, false, true, nil, 0, ADesigner.UniqueName('ElMenuItem'));
    FElMenuItem.OnChange := MenuChanged;
    Temp := TElMenuItem(TreeNode.Data);
    Temp.Add(FElMenuItem);

    while Temp.Parent <> nil do
    begin
      Temp.DesignRebuild;
      Temp := TElMenuItem(Temp.Parent);
    end;
    Temp.DesignRebuild;

    Node := MenuEdit.Items.AddChildObject(TreeNode, FElMenuItem.Caption, Pointer(FElMenuItem));
    Node.ImageIndex := FElMenuItem.ImageIndex;
    Node.StateImageIndex := FElMenuItem.ImageIndex;
    Node.Checked := FElMenuItem.Checked;
    Node.Bold := FElMenuItem.Default;
    Node.ParentStyle := false;
    Node.CheckBoxEnabled := false;
    Node.ShowCheckBox := FElMenuItem.Checked;
    
    if TreeNode <> nil then
      TreeNode.Expanded := true;
    MenuEdit.ItemFocused := Node;
    MenuEdit.EnsureVisible(Node);
    MenuEdit.ItemFocused := Node;
    FElMenuItem.OnChange := MenuChanged;
    Form := GetOwnerForm(FMenu);
    if (Form <> nil) and (Form.Designer <> nil) and not (csDestroying in Form.ComponentState) then
      Form.Designer.Modified;

    if (ADesigner <> nil) then
      ADesigner.Modified;

    if (Form <> nil) and not (csDestroying in Form.ComponentState) then
      DrawMenuBar(Form.Handle);
  end;
end;

procedure TElDesignMenu.DeleteItemBtnClick(Sender: TObject);
var Form: TCustomForm;
begin
  if (MenuEdit.ItemFocused <> nil) and (MenuEdit.ItemFocused.Data <> nil) then
  begin
    if (TElMenuItem(MenuEdit.ItemFocused.Data).Parent <> nil) and (TElMenuItem(MenuEdit.ItemFocused.Data).Parent is TElMenuItem) then
    begin
      TElMenuItem(MenuEdit.ItemFocused.Data).OnChange:= Nil;
      TElMenuItem(TElMenuItem(MenuEdit.ItemFocused.Data).Parent).Remove(TElMenuItem(MenuEdit.ItemFocused.Data));
      TElMenuItem(MenuEdit.ItemFocused.Data).Free;
      MenuEdit.Items.DeleteItem(MenuEdit.ItemFocused);
    end
    else
    begin
      TElMenuItem(MenuEdit.ItemFocused.Data).OnChange:= Nil;
      TElMenuItem(FMenu.Items).Remove(TElMenuItem(MenuEdit.ItemFocused.Data));
    end;
  end;
  Form := GetOwnerForm(FMenu);
  if (Form <> nil) and (Form.Designer <> nil) and not (csDestroying in Form.ComponentState) then
    Form.Designer.Modified;
  if (ADesigner <> nil) then
    ADesigner.Modified;
  if (Form <> nil) and not (csDestroying in Form.ComponentState) then
    DrawMenuBar(Form.Handle);
end;

procedure TElDesignMenu.Notification(AComponent: TComponent; Operation:
    TOperation);
var
  Temp: TElMenuItem;
  Temp1: TElTreeItem;
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = Menu) and not (csDestroying in ComponentState) then
      Close
    else
    if AComponent is TElMenuItem then
    begin
      if not (csDestroying in FMenu.ComponentState) and not (csDestroying in ComponentState) then
      begin
        if FMenu is TElPopupMenu then
          Temp := TElPopupMenu(FMenu).FindItem((AComponent as TElMenuItem).Handle, fkHandle)
        else
          Temp := TElMainMenu(FMenu).FindItem((AComponent as TElMenuItem).Handle, fkHandle);
        if (Temp <> nil) then
        begin
          Temp1 := MenuEdit.Items.LookForItem(nil, '', Temp, -1, true, false, false, true, true);
          if Temp1 <> nil then
            MenuEdit.Items.DeleteItem(Temp1);
        end;
      end;
    end;
  end;
end;

procedure TElDesignMenu.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // Action := caFree;
  // Destroying;
end;

procedure TElDesignMenu.MenuEditItemFocused(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (MenuEdit.ItemFocused <> nil) and (MenuEdit.ItemFocused.Data <> nil) then
    begin
      if ADesigner <> nil then
        ADesigner.SelectComponent(TElMenuItem(MenuEdit.ItemFocused.Data));
      TElMenuItem(MenuEdit.ItemFocused.Data).OnChange := MenuChanged;
    end
    else
    if ADesigner <> nil then
      ADesigner.SelectComponent(nil);
  end
  else
    if ADesigner <> nil then
      ADesigner.SelectComponent(nil);
end;

destructor TElDesignMenu.Destroy;
begin
  ElDesignMenu := nil;
  Destroying;
  inherited;
  MenuEdit := nil;
end;

// Save menu's template to file
procedure TElDesignMenu.SaveClick(Sender: TObject);
var
  S: TStream;
  Temp: TElMenuItem;
begin
  Temp := TElMenuItem(MenuEdit.ItemFocused.Data);
  if Temp <> nil then
  begin
    while Temp.Parent.Parent <> nil do
      Temp := TElMenuItem(Temp.Parent);
    SaveMenuDlg.FileName := ElStripHotKey(Temp.Caption);

    if SaveMenuDlg.Execute then
    begin
      S := TFileStream.Create(SaveMenuDlg.FileName, fmCreate);
      S.WriteComponent(Temp);
      S.Free;
    end;
  end;
end;

// Load file with menu's template
procedure TElDesignMenu.LoadClick(Sender: TObject);
var
  S: TStream;
  Temp: TElMenuItem;
  i: Integer;
  Form: TCustomForm;

  procedure AddMenuItem(Parent: TElMenuItem; Item: TElMenuItem);
  var
    i: integer;
    Form: TCustomForm;
    Temp, FElMenuItem: TElMenuItem;
    FElName: String;
  begin
    Form := GetOwnerForm(FMenu);
    if (Form <> nil) and (Form.Designer <> nil) then
      FElName := {$ifdef VCL_5_USED}Form.Designer.UniqueName('ElMenuItem')
                             {$else}ADesigner.UniqueName('ElMenuItem'){$endif}
    else
      FElName := 'ElMenuItem';

    FElMenuItem := TElMenuItem.Create(ADesigner.GetRoot);
    with FElMenuItem do
    begin
      Caption := Item.Caption;
      Name := FElName;
      ShortCut := Item.ShortCut;
      Hint := Item.Hint;
      Enabled := Item.Enabled;
      Default := Item.Default;
      Checked := Item.Checked;
      RadioItem := Item.RadioItem;
      ImageIndex := Item.ImageIndex;
      Break := Item.Break;
      FElMenuItem.Visible := Item.Visible;
      OnChange := MenuChanged;
    end;

    if (Parent.Parent = nil) then
    begin
      InsertMenuItems(FMenu, Item.MenuIndex, FElMenuItem);
      Temp := FElMenuItem;
    end
    else
    begin
      InsertItems(FMenu, Parent, Item.MenuIndex, FElMenuItem);
      Temp := TElMenuItem(Parent);
    end;

    while Temp.Parent <> nil do
      Temp := TElMenuItem(Temp.Parent);
    Temp.DesignRebuild;

    if Item.Count > 0 then
      for i := 0 to Item.Count - 1 do
        AddMenuItem(FElMenuItem, TElMenuItem(TElMenuItem(Item).Items[i]));
  end;

begin
  if OpenMenuDlg.Execute then
  begin
    for i := 0 to OpenMenuDlg.Files.Count - 1 do
    begin
      S := TFileStream.Create(OpenMenuDlg.Files.Strings[i], fmOpenRead);
      Temp := TElMenuITem(S.ReadComponent(nil));
      if (FMenu is TElPopupMenu) then
        AddMenuItem(TElPopupMenu(FMenu).Items, Temp)
      else
        AddMenuItem(TElMainMenu(FMenu).Items, Temp);

      S.Free;
    end;

    Form := GetOwnerForm(FMenu);
    if (Form <> nil) and (Form.Designer <> nil) and not (csDestroying in Form.ComponentState) then
      Form.Designer.Modified;

    if (ADesigner <> nil) then
      ADesigner.Modified;

    if (Form <> nil) and not (csDestroying in Form.ComponentState) then
      DrawMenuBar(Form.Handle);

    SetElMenu(FMenu);
  end;
end;

procedure TElDesignMenu.MoveUpClick(Sender: TObject);
var
  TreeNode: TElTreeItem;
  Form: TCustomForm;
begin
  TreeNode := MenuEdit.ItemFocused;
  if (TreeNode <> nil) and (TreeNode.Data <> nil) then
  begin
    if TElMenuItem(TreeNode.Data).Caption = '-' then
      TElMenuItem(TElMenuItem(TreeNode.Data).Parent).OnChange := nil;

    TElMenuItem(TreeNode.Data).OnChange := nil;
    TElMenuItem(TreeNode.Data).MenuIndex := TElMenuItem(TreeNode.Data).MenuIndex - 1;
    TElMenuItem(TreeNode.Data).OnChange := MenuChanged;

    if TElMenuItem(TreeNode.Data).Caption = '-' then
      TElMenuItem(TElMenuItem(TreeNode.Data).Parent).OnChange := MenuChanged;

    if TreeNode.Index > 0 then
      TreeNode.MoveToIns(TreeNode.Parent, TreeNode.Index - 1);
    Form := GetOwnerForm(FMenu);
    if (Form <> nil) and (not (csDestroying in Form.ComponentState)) then
      DrawMenuBar(Form.Handle);
  end;
end;

procedure TElDesignMenu.MoveDownClick(Sender: TObject);
var
  TreeNode: TElTreeItem;
  Form: TCustomForm;
begin
  TreeNode := MenuEdit.ItemFocused;
  if (TreeNode <> nil) and (TreeNode.Data <> nil) then
  begin

    if TElMenuItem(TreeNode.Data).Caption = '-' then
      TElMenuItem(TElMenuItem(TreeNode.Data).Parent).OnChange := nil;

    TElMenuItem(TreeNode.Data).OnChange := nil;
    TElMenuItem(TreeNode.Data).MenuIndex := TElMenuItem(TreeNode.Data).MenuIndex + 1;
    TElMenuItem(TreeNode.Data).OnChange := MenuChanged;

    if TElMenuItem(TreeNode.Data).Caption = '-' then
      TElMenuItem(TElMenuItem(TreeNode.Data).Parent).OnChange := MenuChanged;

    if TreeNode.GetNextSibling <> nil then
      TreeNode.MoveToIns(TreeNode.Parent, TreeNode.GetNextSibling.Index);
    Form := GetOwnerForm(FMenu);
    if (Form <> nil) and (not (csDestroying in Form.ComponentState)) then
      DrawMenuBar(Form.Handle);
  end;
end;

procedure TElDesignMenu.LevelUpClick(Sender: TObject);
var
  TreeNode: TElTreeItem;
  Temp: TElMenuItem;
  Form: TCustomForm;
begin
  TreeNode := MenuEdit.ItemFocused;
  if (TreeNode <> nil) and (TreeNode.Data <> nil) then
    if TElMenuItem(TreeNode.Data).Parent.Parent <> nil then
    begin
      Temp := TElMenuItem(TElMenuItem(TreeNode.Data).Parent);
      Temp.Remove(TElMenuItem(TreeNode.Data));
      Temp.Parent.Insert(Temp.MenuIndex + 1, TElMenuItem(TreeNode.Data));
//      Temp.DesignRebuild;
      TElMenuItem(TreeNode.Data).DesignRebuild;
      Temp.OnChange := MenuChanged;

      TreeNode.MoveToIns(TreeNode.Parent.Parent, TreeNode.Parent.Index + 1);

      Form := GetOwnerForm(FMenu);
      if (Form <> nil) and (not (csDestroying in Form.ComponentState)) then
        DrawMenuBar(Form.Handle);
    end;
end;

procedure TElDesignMenu.LevelDownClick(Sender: TObject);
var
  TreeNode: TElTreeItem;
  Temp: TElMenuItem;
  Form: TCustomForm;
begin
  TreeNode := MenuEdit.ItemFocused;
  if (TreeNode <> nil) and (TreeNode.Data <> nil) then
    if TElMenuItem(TreeNode.Data).MenuIndex > 0 then
    begin

      Temp := TElMenuItem(TElMenuItem(TElMenuItem(TreeNode.Data).Parent).Items[TElMenuItem(TreeNode.Data).MenuIndex - 1]);
      TElMenuItem(TreeNode.Data).Parent.Remove(TElMenuItem(TreeNode.Data));
      Temp.Insert(0, TElMenuItem(TreeNode.Data));
      Temp.DesignRebuild;
      Temp.OnChange := MenuChanged;

      if TreeNode.GetPrevSibling <> nil then
         TreeNode.MoveToIns(TreeNode.GetPrevSibling, 0);

      Form := GetOwnerForm(FMenu);
      if (Form <> nil) and (not (csDestroying in Form.ComponentState)) then
        DrawMenuBar(Form.Handle);
    end;
end;

procedure TElDesignMenu.MenuEditStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragItem := MenuEdit.ItemFocused;
  DragObject := TElDragObject.Create(MenuEdit);
end;

procedure TElDesignMenu.MenuEditDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Temp: TElTreeItem;
begin
  Accept:=false;
  if Source.ClassType <> TElDragObject then
    exit;
  Temp := MenuEdit.GetItemAtY(Y);
  if Temp = nil then
  begin
    Accept:=true;
    exit;
  end;
  if ((not Temp.IsUnder(FDragItem)) and (Temp.Data <> nil)) then
    Accept:=true;
end;

procedure TElDesignMenu.MenuEditDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Temp: TElTreeItem;
  Src, Dst: TElMenuItem;
begin
  Temp := MenuEdit.GetItemAtY(Y);
  if ((Temp <> nil) and (not Temp.IsUnder(FDragItem)) and (Temp.Data <> nil)) then
  begin
    Src := TElMenuItem(FDragItem.Data);
    Dst := TElMenuItem(Temp.Data);

    FDragItem.MoveToIns(Temp.Parent, Temp.Index + 1);

    Src.Parent.Remove(Src);
    Dst.Parent.Insert(Dst.MenuIndex + 1, Src);

    Src.DesignRebuild;
  end;
  FDragItem := nil;
end;

end.
