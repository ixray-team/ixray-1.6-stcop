unit ElRegTrees;  { TElRegTree component. }

interface

{.$i elpack.inc}

//{$r elregtrees.dcr}

//{$R ELREGTREES.RES}

uses
  Classes,
  Menus,
  ElImgLst,
//  DragDrop,
{$IFDEF VCL_4_USED}
  ImgList,
{$ENDIF}
  StdCtrls,
  ExtCtrls,
  ElRegUtils,
  Dialogs,
  ElHeader,
  ElTree;

type
  TRegKeyActionEvent  = procedure(Sender : TObject; Root : TRegRootType;
  const ComputerName, Key: string) of object;
  TRegKeyConfirmEvent = procedure(Sender : TObject; Root : TRegRootType;
  const ComputerName, Key: string;
  var ActionAllowed : boolean) of object;

  TElRegSaveDialog = class (TSaveDialog)
   private
  FAllCB : TRadioButton;
  FKeyCB : TRadioButton;
  FKeyEdit : TEdit;
  FPanel : TPanel;
  function GetKey : string;
  procedure SetKey (newValue : string);
   protected
  procedure DoClose; override;
  procedure DoShow; override;
  procedure Clicked (Sender : TObject);
   public
  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
  function Execute: Boolean; override;
  property Key : string read GetKey write SetKey;
  end;

  TElRegOpenDialog = class (TOpenDialog)
   private
  FAllCB : TRadioButton;
  FKeyCB : TRadioButton;
  FKeyEdit : TEdit;
  FPanel : TPanel;
  function GetKey : string;
  procedure SetKey (newValue : string);

   protected
  procedure DoClose; override;
  procedure DoShow; override;
  procedure Clicked (Sender : TObject);
   public
  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
  function Execute: Boolean; override;
  property Key : string read GetKey write SetKey;
  end;

  TElRegTree = class(TCustomElTree)
  private
  FReadOnly : Boolean;
  FShowMenu : Boolean;
  { Private declarations }
  FRegMenu : TPopupMenu;
  ItemExpand : TMenuItem;
  ItemNewKey : TMenuItem;
  ItemNewValue : TMenuItem;
  ItemRegMenuSeparator1 : TMenuItem;
  ItemFind : TMenuItem;
  ItemReplace : TMenuItem;
  ItemRegMenuSeparator2 : TMenuItem;
  ItemDelete : TMenuItem;
  ItemRename : TMenuItem;
  ItemRegMenuSeparator3 : TMenuItem;

  ItemCopyKeyName : TMenuItem;
  FItemFindClick : TNotifyEvent;
  FItemReplaceClick : TNotifyEvent;
  FComputerName : String;
  FImages : TElImageList;
  FRegRoots : TRegRoots;
  fActive : Boolean;
  FOnKeyDelete : TRegKeyConfirmEvent;
  FOnKeyRename : TRegKeyConfirmEvent;
  FOnKeyActivate : TRegKeyActionEvent;
  FOnKeyMove   : TRegKeyActionEvent;
  FOnKeyCopy   : TRegKeyActionEvent;
  FOnKeyDeleted : TRegKeyActionEvent;
  FOnKeyRenamed : TRegKeyActionEvent;
  FOnKeyCreateSubKey : TRegKeyActionEvent;
  FOnKeyCreateValue : TRegKeyActionEvent;

  procedure   SetActive (V: Boolean);
  procedure SetComputerName(const newValue : String);
  procedure SetRegRoots(newValue : TRegRoots);
  procedure SetShowMenu(newValue : boolean);
  procedure BuildBranch(Root : TRegRootType);
  function GetKeyName (Item : TElTreeItem) : string;
  function GetStartItem (Item : TElTreeItem) : TElTreeItem;
  procedure RefreshBranch(Item : TElTreeItem);
  procedure SetReadOnly(newValue : Boolean);
  protected
  { Protected declarations }
  FPopupMenuTag: integer;
  { Event triggers: }
  procedure TriggerKeyDeleteEvent(Root : TRegRootType;
  const ComputerName, Key: string;
  var ActionAllowed : boolean); virtual;
  procedure TriggerKeyRenameEvent(Root : TRegRootType;
  const ComputerName, Key: string;
  var ActionAllowed : boolean); virtual;
  procedure TriggerKeyActivateEvent(Root : TRegRootType;
  const ComputerName, Key: string); virtual;
  procedure TriggerKeyMoveEvent(Root : TRegRootType;
  const ComputerName, Key: string); virtual;
  procedure TriggerKeyCopyEvent(Root : TRegRootType;
  const ComputerName, Key: string); virtual;
  procedure TriggerKeyDeletedEvent(Root : TRegRootType;
  const ComputerName, Key: string); virtual;
  procedure Loaded; override;
  procedure SetupMenuItem(AMenuItem : TMenuItem;
  const ACaption : string;
  AEnabled, AChecked, ARadioItem : boolean;
  AGroupIndex, AShortCut : integer;
  AHandler : TNotifyEvent); dynamic;
  procedure CreatePopupMenuItems; dynamic;
  procedure InitializePopupMenuItems; dynamic;
  procedure AddMenuItemsToPopupMenu; dynamic;
  { Event handlers and transfer methods for FRegMenu. }
  procedure PopupRegMenuHandler(Sender : TObject); dynamic;
  procedure ItemExpandClickHandler(Sender : TObject); dynamic;
  procedure ItemNewKeyClickHandler(Sender : TObject); dynamic;
  procedure ItemNewValueClickHandler(Sender : TObject); dynamic;
  procedure ItemFindClickTransfer(Sender : TObject); dynamic;
  procedure ItemReplaceClickTransfer(Sender : TObject); dynamic;
  procedure ItemDeleteClickHandler(Sender : TObject); dynamic;
  procedure ItemRenameClickHandler(Sender : TObject); dynamic;
  procedure ItemCopyKeyNameClickHandler(Sender : TObject); dynamic;
  procedure DoItemFocused; override;
  procedure DoItemExpanding(Item : TElTreeItem; var CanProcess: boolean); override;
  procedure DoItemCollapsing(Item : TElTreeItem; var CanProcess: boolean); override;
  function DoGetPicture (Item:TElTreeItem) : integer; override;
  // procedure DoValidate(Item:TElTreeItem; Section : TElHeaderSection; var Text:string; var Accept:boolean); override;
  // procedure TriggerKeyRenamedEvent(Root : TRegRootType; const ComputerName, Key: string); virtual;
  procedure TriggerKeyCreateSubKeyEvent(Root : TRegRootType; const ComputerName, Key: string); virtual;
  procedure TriggerKeyCreateValueEvent(Root : TRegRootType; const ComputerName, Key: string); virtual;
  public
  { Public declarations }
  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
  function CurrentRoot : TRegRootType;
  function CurrentKey  : string;
  function SetCurrentKey(RT : TRegRootType; const KeyName : string) : boolean;
  procedure Rebuild;
  procedure Refresh;
  procedure RefreshSubTree;
  procedure DeleteKey;
  procedure CopyKeyName;
  function  CreateSubKey(const KeyName, ClassName : string) : boolean;
  function  CreateValue(const ValueName : string;
  ValueType : integer; Value : Pointer;
  ValueSize : integer) : boolean;
  published
  property  Active: Boolean read fActive write SetActive default FALSE;
  property ComputerName : String read FComputerName write SetComputerName;  { Published }
  property RegRoots : TRegRoots read FRegRoots write SetRegRoots;  { Published }
  property Align;
  property AutoExpand;
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
  property Color;
  property Ctl3D;
  property DragAllowed;
  property Enabled;
  property ExpandOnDblClick;
  property HideSelection;
  property Font;
  property ParentColor;
  property ParentCtl3D;
  property ParentFont;
  property ParentShowHint;
  property ReadOnly : Boolean read FReadOnly write SetReadOnly;  { Published }
  property ScrollTracking;
  property ShowHint;
  property ShowMenu : Boolean read FShowMenu write SetShowMenu;
  property Tracking;
  property UnderlineTracked;
  property Visible;
  property TabOrder;
  property TabStop;
  property LinesColor;
  property LinesStyle;
  property LineHintMode;

  property BorderStyle;
  property ChangeStateImage;
  property CustomPlusMinus;
  property DeselectChildrenOnCollapse;
  property DoInplaceEdit;
  property DragCursor;

  property RowSelect;
  property SelectColumn;
  property ShowButtons;
  property ShowImages;
  property ShowLines;
  property ShowRoot;
  property SelectionMode;

  property OnOleTargetDrag;
  property OnOleTargetDrop;
  property OnOleDragStart;
  property OnOleDragFinish;


  property SortDir;
  property SortMode;
  property SortType;

  property TextColor;
  property BkColor;

  property OnClick;
  property OnDblClick;
  property OnEnter;
  property OnExit;
  property OnKeyDown;
  property OnKeyPress;
  property OnKeyUp;
  property OnMouseDown;
  property OnMouseMove;
  property OnMouseUp;
  property OnItemFindClick : TNotifyEvent read FItemFindClick write FItemFindClick;
  property OnItemReplaceClick : TNotifyEvent read FItemReplaceClick write FItemReplaceClick;
  property OnKeyDeleted : TRegKeyActionEvent read FOnKeyDeleted write FOnKeyDeleted;
  property OnKeyRenamed : TRegKeyActionEvent read FOnKeyRenamed write FOnKeyRenamed;
  property OnKeyCreateSubKey : TRegKeyActionEvent read FOnKeyCreateSubKey write FOnKeyCreateSubKey;
  property OnKeyCreateValue : TRegKeyActionEvent read FOnKeyCreateValue write FOnKeyCreateValue;
  property OnKeyDelete : TRegKeyConfirmEvent read FOnKeyDelete write FOnKeyDelete;
  property OnKeyRename : TRegKeyConfirmEvent read FOnKeyRename write FOnKeyRename;
  property OnKeyActivate : TRegKeyActionEvent read FOnKeyActivate write FOnKeyActivate;
  property OnKeyMove : TRegKeyActionEvent read FOnKeyMove write FOnKeyMove;
  property OnKeyCopy : TRegKeyActionEvent read FOnKeyCopy write FOnKeyCopy;
  end;  { TElRegTree }



procedure Register;

implementation

uses
  Windows,
  SysUtils,
  Controls,
  Graphics,
  Forms,
  Clipbrd,
  ElTools,
  ElStrUtils
  ;

procedure TElRegSaveDialog.SetKey (newValue : string);
begin
  if Key <> newValue then
  begin
  if newValue = '' then FAllCB.checked := true else FKeyCB.Checked := true;
  FKeyEdit.Text := newValue;
  end;
end;

function TElRegSaveDialog.GetKey : string;
begin
  if FKeyCB.Checked then result := FKeyEdit.Text else result := '';
end;

procedure TElRegSaveDialog.DoClose;
begin
  inherited DoClose;
  Application.HideHint;
end;

procedure TElRegSaveDialog.Clicked;
begin
  //TRadioButton(Sender).Checked := true;
  FKeyEdit.Enabled := FKeyCB.Checked;
end;

procedure TElRegSaveDialog.DoShow;
begin
  inherited;
  FPanel.ParentWindow := Handle;
  FAllCB.Parent := FPanel;
  FKeyCB.Parent := FPanel;
  FKeyEdit.Parent := FPanel;
end;

function TElRegSaveDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
  Template := 'ELREGDLGTEMPLATE' else
  Template := nil;
  Result := inherited Execute;
end;

destructor TElRegSaveDialog.Destroy;
begin
  FKeyEdit.Free;
  FKeyCB.Free;
  FAllCB.Free;
  inherited;
end;

constructor TElRegSaveDialog.Create(AOwner : TComponent);
begin
  inherited;
  FPanel := TPanel.Create(nil);
  FPanel.SetBounds(0, 228, 420, 100);
  FPanel.BevelInner := bvNone;
  FPanel.BevelOuter := bvNone;
  FAllCB := TRadioButton.Create(nil);
  FKeyCB := TRadioButton.Create(nil);
  FKeyEdit := TEdit.Create(nil);
  FAllCB.OnClick := Clicked;
  FKeyCB.OnClick := Clicked;
  FAllCB.Enabled := true;
  FAllCB.Checked := true;
  FKeyCB.Enabled := true;
  FAllCB.SetBounds(8, 8, 200, 17);
  FKeyCB.SetBounds(8, 28, 200, 17);
  FKeyEdit.SetBounds(24, 48, 380, 21);
  FAllCB.Caption := 'Save all keys';
  FKeyCB.Caption := 'Save selected key:';
  FKeyEdit.Ctl3D := true;
  FKeyEdit.Enabled := false;
end;

function TElRegOpenDialog.GetKey : string;
begin
  if FKeyCB.Checked then result := FKeyEdit.Text else result := '';
end;

procedure TElRegOpenDialog.SetKey (newValue : string);
begin
  if Key <> newValue then
  begin
  if newValue = '' then FAllCB.checked := true else FKeyCB.Checked := true;
  FKeyEdit.Text := newValue;
  end;
end;

procedure TElRegOpenDialog.DoClose;
begin
  inherited DoClose;
  Application.HideHint;
end;

procedure TElRegOpenDialog.Clicked;
begin
  //TRadioButton(Sender).Checked := true;
  FKeyEdit.Enabled := FKeyCB.Checked;
end;

procedure TElRegOpenDialog.DoShow;
begin
  inherited;
  FPanel.ParentWindow := Handle;
  FAllCB.Parent := FPanel;
  FKeyCB.Parent := FPanel;
  FKeyEdit.Parent := FPanel;
end;

function TElRegOpenDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
  Template := 'ELREGDLGTEMPLATE' else
  Template := nil;
  Result := inherited Execute;
end;

destructor TElRegOpenDialog.Destroy;
begin
  FKeyEdit.Free;
  FKeyCB.Free;
  FAllCB.Free;
  inherited;
end;

constructor TElRegOpenDialog.Create(AOwner : TComponent);
begin
  inherited;
  FPanel := TPanel.Create(nil);
  FPanel.SetBounds(0, 228, 420, 100);
  FPanel.BevelInner := bvNone;
  FPanel.BevelOuter := bvNone;
  FAllCB := TRadioButton.Create(nil);
  FKeyCB := TRadioButton.Create(nil);
  FKeyEdit := TEdit.Create(nil);
  FAllCB.OnClick := Clicked;
  FKeyCB.OnClick := Clicked;
  FAllCB.Enabled := true;
  FAllCB.Checked := true;
  FKeyCB.Enabled := true;
  FAllCB.SetBounds(8, 8, 200, 17);
  FKeyCB.SetBounds(8, 28, 200, 17);
  FKeyEdit.SetBounds(24, 48, 380, 21);
  FAllCB.Caption := 'Save all keys';
  FKeyCB.Caption := 'Save selected key:';
  FKeyEdit.Ctl3D := true;
  FKeyEdit.Enabled := false;
end;


function TElRegTree.CurrentKey : string;
begin
  if ItemFocused <> nil then
  result := GetKeyName(ItemFocused)
  else
  result := '';
end;

function TElRegTree.SetCurrentKey(RT : TRegRootType;
  const KeyName : string) : boolean;
var i  : integer;
  TI : TElTreeItem;
  S  : String;
  sKey: string;
begin
  result := false;
  if not (RT in FRegRoots) then exit;
  TI := GetRoot;
  sKey := KeyName;

  while true do
  begin
  if Pos('\', KeyName) > 0 then
  begin
  S := Uppercase(Copy(sKey, 1, Pos('\', sKey) - 1));
  Delete(sKey, 1, Length(S) + 1);
  end else
  begin
  S := Uppercase(sKey);
  sKey := '';
  end;
  for i := 0 to TI.Count - 1 do  // Iterate
  begin
  if Uppercase(Trim(TI.Children[i].Text)) = S then
  begin
  TI := TI.Children[i];
  break;
  end;
  end;  // for
  if (i >= TI.Count) or (sKey = '') then break;
  end;
  if sKey <> '' then
  result := false
  else
  begin
  EnsureVisible(TI);
  ItemFocused := TI;
  result := true;
  end;
end;

function TElRegTree.CurrentRoot : TRegRootType;
begin
  if ItemFocused <> nil then
  result := NameToRootType(GetStartItem(ItemFocused).Text)
  else
  result := rrtUnknown;
end;

function TElRegTree.CreateValue(const ValueName : string;
  ValueType : integer; Value : Pointer;
  ValueSize : integer) : boolean;
begin
  result := KeySetValue(ComputerName,
  NameToRootType(GetStartItem(ItemFocused).Text),
  GetKeyName(ItemFocused), ValueName, ValueType, Value, ValueSize);
end;

function TElRegTree.CreateSubKey;
var TI : TElTreeItem;
begin
  result := false;
  if ItemFocused <> nil then
  begin
  if KeyCreateSubKey(ComputerName, NameToRootType(GetStartItem(ItemFocused).Text), GetKeyName(ItemFocused), KeyName, ClassName) then
  begin
  TI := Items.AddChild(ItemFocused, KeyName);
  TI.ForceButtons := false;
  result := true;
  end;
  end;
end;

procedure TElRegTree.DeleteKey;
begin
  ItemDeleteClickHandler(Self);
end;

procedure TElRegTree.RefreshSubTree;

type TSRec = record
   StartItem  : TElTreeItem;
   CheckStartItem,
   SubItemsOnly : boolean;
   end;
   PSRec = ^TSRec;

var SRec : TSRec;

   procedure IntRefresh(Item:TElTreeItem; Index: integer; var ContinueIterate:boolean;
  IterateData:pointer; Tree:TCustomElTree);
   begin
   if (PSRec(IterateData).SubItemsOnly) and (not Item.IsUnder(PSRec(IterateData).StartItem)) then
   begin
   ContinueIterate := false;
   exit;
   end;
   TElRegTree(Tree).RefreshBranch(Item);
   end;

begin
  IsUpdating := true;
  try
  SRec.StartItem := ItemFocused;
  SRec.CheckStartItem := true;
  SRec.SubItemsOnly := true;
  Items.IterateFrom(false, true, @IntRefresh, @SRec, SRec.StartItem);
  finally
  IsUpdating := false;
  end;
end;

procedure TElRegTree.Refresh;

type TSRec = record
   StartItem  : TElTreeItem;
   CheckStartItem,
   SubItemsOnly : boolean;
   end;
   PSRec = ^TSRec;

var SRec : TSRec;

   procedure IntRefresh(Item:TElTreeItem; Index: integer; var ContinueIterate:boolean;
  IterateData:pointer; Tree:TCustomElTree);
   begin
   if (PSRec(IterateData).SubItemsOnly) and (not Item.IsUnder(PSRec(IterateData).StartItem)) then
   begin
   ContinueIterate := false;
   exit;
   end;
   TElRegTree(Tree).RefreshBranch(Item);
   end;

begin
  IsUpdating := true;
  try
  SRec.StartItem := nil;
  SRec.CheckStartItem := true;
  SRec.SubItemsOnly := false;
  Items.Iterate(false, true, @IntRefresh, @SRec);
  finally
  IsUpdating := false;
  end;
end;

procedure TElRegTree.RefreshBranch(Item : TElTreeItem);
var SL : TStringList;
  i,
  j  : integer;
  TI : TElTreeItem;
  b : boolean;
  S : String;
begin
  if not Item.FullyExpanded then exit;
  SL := TStringList.Create;
  KeyEnumSubKeys(ComputerName, NameToRootType(GetStartItem(ItemFocused).Text), GetKeyName(Item), SL);
  for i := 0 to SL.Count - 1 do  // Iterate
  begin
  b := false;
  S := Uppercase(Trim(SL[i]));
  for j := 0 to Item.Count -1 do  // Iterate
  begin
  if Uppercase(Trim(Item.Children[j].Text)) = S then
  begin
  b := true;
  break;
  end;
  end;  // for
  if not b then
  begin
  TI := Items.AddChildObject(Item, SL[i], nil);
  TI.ForceButtons := KeyHasSubKeys(ComputerName, NameToRootType(GetStartItem(Item).Text), GetKeyName(TI));
  end;
  end;  // for
  j := 0;
  while j < Item.Count do  // Iterate
  begin
  S := Uppercase(Trim(Item.Children[j].Text));
  b := false;
  for i := 0 to SL.Count - 1 do  // Iterate
  begin
  if Uppercase(Trim(SL[i])) = S then
  begin
  b := true;
  break;
  end;
  end;  // for
  if not b then Item.Children[j].Delete else inc(j);
  end;  // for
  Item.ForceButtons := SL.Count > 0;
  SL.Free;
end;

procedure TElRegTree.SetShowMenu(newValue : boolean);
begin
  if (FShowMenu <> newValue) then
  begin
  FShowMenu := newValue;
  FRegMenu.AutoPopup := FShowMenu;
  end;  { if }
end;

procedure TElRegTree.SetComputerName(const newValue : String);
var S : String;
  i : dword;  //  T & R integer;
begin
  if newValue <> '' then
  begin
  if  Pos ('\\', newValue) = 0  then  S := '\\' + newValue
  else  S := newValue;

    FRegRoots := [rrtHKEY_LOCAL_MACHINE, rrtHKEY_USERS];

    if IsWin95 then Include(FRegRoots, rrtHKEY_CURRENT_CONFIG);
  end
  else
    S := '';
  FComputerName := S;   //newValue;
  Rebuild;
end;  { SetComputerName }

procedure TElRegTree.SetRegRoots(newValue : TRegRoots);
{ Sets data member FRegRoots to newValue. }
begin
  if (FRegRoots <> newValue) then
  begin
  FRegRoots := newValue;
  Rebuild;
  end;  { if }
end;  { SetRegRoots }

function TElRegTree.GetStartItem (Item : TElTreeItem) : TElTreeItem;
begin
  while Item.Parent <> nil do Item := Item.Parent;
  result := Item;
end;

function TElRegTree.GetKeyName (Item : TElTreeItem) : string;
var S : String;
begin
  S := Item.GetFullNameEx('\', false);
  if Pos('\', S) = 0 then
  result := S
  else
  result := Copy(S, Pos('\', S) + 1, Length(S));
end;

procedure TElRegTree.DoItemExpanding(Item : TElTreeItem; var CanProcess: boolean);
var i, N : integer;
  S : TStringList;
  TI : TElTreeItem;
  RT : TRegRootType;
  OldSort: TSortModes;
  Key: HKey;
begin
  if Item.Data = nil then
  begin
  RT := NameToRootType(GetStartItem(Item).Text);

  if  OpenRegKey (ComputerName, RT, GetKeyName(Item), Key)  then
  begin
  S := TStringList.Create;

  try
  KeyEnumSubKeys0(Key, '', S);
  Item.Data := Pointer(true);
  IsUpdating := true;
//  OldSort := SortMode;
//  SortMode := smNone;

  try
  N := S.Count - 1;

  for i := 0 to N do  // Iterate
  begin
  TI := Items.AddChildObject(Item, S[i], nil);
//  TI.ForceButtons := FALSE;
  TI.ForceButtons := KeyHasSubKeys0(Key, S [i]);
//   GetKeyName(TI));
  end;  // for
  finally
  Item.Sort (FALSE);
//  Item.SortMode := OldSort;
  IsUpdating := false;
  end;
  finally
  S.Free;
  RegCloseKey(Key);
  end;
  end;
  end;
end;

(*
procedure TElRegTree.DoValidate(Item:TElTreeItem; Section : TElHeaderSection; var Text:string; var Accept:boolean);
var i  : integer;
  CN : string;
  OldName,
  NewName : string;
begin
  inherited;
  if Accept then
  begin
  if Length(Text) = 0 then
  begin
  MessageDlg('Key name must contain at least one character', mtError, [mbOk], 0);
  Accept := false;
  end else
  begin
  for i := 1 to Length(Text) do  // Iterate
  begin
  if not (Text[i] in [#33 .. #127]) or (Text[i] in ['\', '*', '?']) then
  begin
  MessageDlg(Format('Can''t rename %s: The specified key name contains illegal characters', [Item.Text]), mtError, [mbOk], 0);
  Accept := false;
  break;
  end;
  end;  // for
  end;
  if Accept then
  begin
  Accept := false;
  if KeyGetClassName(ComputerName, NameToRootType(GetStartItem(Item).Text), GetKeyName(Item.Parent), CN) then
  begin
  if KeyCreateSubKey(ComputerName, NameToRootType(GetStartItem(Item).Text), GetKeyName(Item.Parent), Text, CN) then
  begin
  OldName := GetKeyName(Item);
  if Pos('\', OldName) = 0 then
  begin
  NewName := Text;
  end else
  begin
  NewName := Copy(OldName, 1, LastPos('\', OldName)) + Text;
  end;
  if CopyKey(ComputerName, ComputerName, NameToRootType(GetStartItem(Item).Text), NameToRootType(GetStartItem(Item).Text), OldName, NewName) then
  begin
  KeyDelete(ComputerName, NameToRootType(GetStartItem(Item).Text), OldName);
  Accept := true;
  end else
  begin
  MessageDlg(Format('Can''t rename %s: %s', [Item.Text, GetLastRegError]), mtError, [mbOk], 0);
  KeyDelete(ComputerName, NameToRootType(GetStartItem(Item).Text), NewName);
  end;
  end;
  end;
  end;
  end;
end;
*)
function TElRegTree.DoGetPicture (Item:TElTreeItem) : integer;
begin
  if Item.Expanded then result := 1 else Result := 0;
end;

procedure TElRegTree.DoItemCollapsing(Item : TElTreeItem; var CanProcess: boolean);
begin
end;

procedure TElRegTree.DoItemFocused;
var TI : TElTreeItem;
  S  : String;
  b  : boolean;
begin
  TI := ItemFocused;
  if TI <> nil then
  begin
  b := true;
  S := GetKeyName(TI);
  TriggerKeyActivateEvent(NameToRootType(GetStartItem(TI).Text), ComputerName, S);
  end else
  begin
  TriggerKeyActivateEvent(rrtUnknown, ComputerName, '');
  b := false;
  end;
  ItemDelete.Enabled := (TI <> nil) and (TI.Parent <> nil) and (not ReadOnly);
  ItemRename.Enabled := ItemDelete.Enabled;
  ItemNewKey.Enabled := b and (not ReadOnly);
  ItemNewValue.Enabled := b and (not ReadOnly);
  ItemExpand.Enabled := b;
  ItemCopyKeyName.Enabled := b;
  inherited;
end;

procedure TElRegTree.BuildBranch(Root : TRegRootType);
begin
  with Items.AddChildObject(nil, RootTypeName(Root), nil) do
  ForceButtons := KeyHasSubKeys(ComputerName, Root, Text);
end;

procedure TElRegTree.Rebuild;  { public }
var i : TRegRootType;
begin
  if  fActive   then
  begin
  try
  IsUpdating := true;
  Items.Clear;
  for i := rrtHKEY_CLASSES_ROOT to rrtHKEY_CURRENT_CONFIG  do  // Iterate
  begin
  if i in FRegRoots then BuildBranch(i);
  end;  // for
  finally
  IsUpdating := false;
  end;
  end;
end;  { Rebuild }

{ Event triggers: }
procedure TElRegTree.TriggerKeyDeleteEvent;
begin
  if (assigned(FOnKeyDelete)) then
  FOnKeyDelete(Self, Root , ComputerName, Key, ActionAllowed);
end;  { TriggerKeyDeleteEvent }

procedure TElRegTree.TriggerKeyRenameEvent;
begin
  if (assigned(FOnKeyRename)) then
  FOnKeyRename(Self, Root , ComputerName, Key, ActionAllowed);
end;  { TriggerKeyRenameEvent }

procedure TElRegTree.TriggerKeyActivateEvent(Root : TRegRootType;
  const ComputerName, Key: string);
begin
  if (assigned(FOnKeyActivate)) then
  FOnKeyActivate(Self, Root , ComputerName, Key);
end;  { TriggerKeyActivateEvent }

procedure TElRegTree.TriggerKeyMoveEvent(Root : TRegRootType;
  const ComputerName, Key: string);
begin
  if (assigned(FOnKeyMove)) then
  FOnKeyMove(Self, Root , ComputerName, Key);
end;  { TriggerKeyMoveEvent }

procedure TElRegTree.TriggerKeyCopyEvent(Root : TRegRootType;
  const ComputerName, Key: string);
begin
  if (assigned(FOnKeyCopy)) then
  FOnKeyCopy(Self, Root , ComputerName, Key);
end;  { TriggerKeyCopyEvent }

procedure TElRegTree.Loaded;
begin
  inherited Loaded;
  Rebuild;
end;  { Loaded }

procedure TElRegTree.SetupMenuItem(AMenuItem : TMenuItem;
  const ACaption : string;
  AEnabled, AChecked, ARadioItem : boolean;
  AGroupIndex, AShortCut : integer;
  AHandler : TNotifyEvent);
begin
  with AMenuItem do
  begin
  Enabled := AEnabled;
  Caption := ACaption;
  Checked := AChecked;
  RadioItem := ARadioItem;
  GroupIndex := AGroupIndex;
  ShortCut := AShortCut;
  OnClick := AHandler;
  Tag := FPopupMenuTag;
  inc(FPopupMenuTag);
  end;  { with }
end;  { SetupMenuItem }

procedure TElRegTree.CreatePopupMenuItems;
begin
  FRegMenu := TPopupMenu.Create(Self);
  PopupMenu := FRegMenu;
  ItemExpand := TMenuItem.Create(FRegMenu);
  ItemNewKey := TMenuItem.Create(FRegMenu);
  ItemNewValue := TMenuItem.Create(FRegMenu);
  ItemRegMenuSeparator1 := TMenuItem.Create(FRegMenu);
  ItemFind := TMenuItem.Create(FRegMenu);
  ItemReplace := TMenuItem.Create(FRegMenu);
  ItemRegMenuSeparator2 := TMenuItem.Create(FRegMenu);
  ItemDelete := TMenuItem.Create(FRegMenu);
  ItemRename := TMenuItem.Create(FRegMenu);
  ItemRegMenuSeparator3 := TMenuItem.Create(FRegMenu);
  ItemCopyKeyName := TMenuItem.Create(FRegMenu);

end;  { CreatePopupMenuItems }

procedure TElRegTree.InitializePopupMenuItems;
begin
  FPopupMenuTag := 0;
  SetupMenuItem(ItemExpand, 'Expand', false, false, false, 0, 0, ItemExpandClickHandler);
  SetupMenuItem(ItemNewKey, 'New Key ...', false, false, false, 0, 0, ItemNewKeyClickHandler);
  SetupMenuItem(ItemNewValue, 'New Value ...', false, false, false, 0, 0, ItemNewValueClickHandler);
  SetupMenuItem(ItemRegMenuSeparator1, '-', false, false, false, 0, 0, nil);
  SetupMenuItem(ItemFind, 'Find ...', true, false, false, 0, 0, ItemFindClickTransfer);
  SetupMenuItem(ItemReplace, 'Replace ...', true, false, false, 0, 0, ItemReplaceClickTransfer);
  SetupMenuItem(ItemRegMenuSeparator2, '-', true, false, false, 0, 0, nil);
  SetupMenuItem(ItemDelete, 'Delete', false, false, false, 0, 0, ItemDeleteClickHandler);
  SetupMenuItem(ItemRename, 'Rename', false, false, false, 0, 0, ItemRenameClickHandler);
  SetupMenuItem(ItemRegMenuSeparator3, '-', true, false, false, 0, 0, nil);
  SetupMenuItem(ItemCopyKeyName, 'Copy Key Name', false, false, false, 0, 0, ItemCopyKeyNameClickHandler);
end;  { InitializePopupMenuItems }

procedure TElRegTree.AddMenuItemsToPopupMenu;
begin
  with FRegMenu do
  begin
  OnPopup := PopupRegMenuHandler;
  Items.Add(ItemExpand);
  Items.Add(ItemNewKey);
  Items.Add(ItemNewValue);
  Items.Add(ItemRegMenuSeparator1);
  Items.Add(ItemFind);
  Items.Add(ItemReplace);
  Items.Add(ItemRegMenuSeparator2);
  Items.Add(ItemDelete);
  Items.Add(ItemRename);
  Items.Add(ItemRegMenuSeparator3);
  Items.Add(ItemCopyKeyName);
  end;  { with }
end;  { AddMenuItemsToPopupMenu }

procedure TElRegTree.PopupRegMenuHandler(Sender : TObject);
{ Handles the FRegMenu OnPopup event. }
begin
  if ItemFocused <> nil then
  begin
  if ItemFocused.Expanded
   then ItemExpand.Caption := 'Collapse'
   else ItemExpand.Caption := 'Expand';
  end else ItemExpand.Caption := 'Expand';
end;  { PopupRegMenuHandler }

procedure TElRegTree.ItemExpandClickHandler(Sender : TObject);
{ Handles the ItemExpand OnClick event. }
begin
  if ItemFocused <> nil then
  ItemFocused.Expanded := not ItemFocused.Expanded;
end;  { ItemExpandClickHandler }

procedure TElRegTree.ItemNewValueClickHandler(Sender : TObject);
begin
  if ItemFocused <> nil   then
  TriggerKeyCreateValueEvent(NameToRootType(GetStartItem(ItemFocused).Text),
   ComputerName, GetKeyName(ItemFocused))
  else
  TriggerKeyCreateValueEvent(rrtUnknown, ComputerName, '');
end;  { ItemNewClickHandler }

procedure TElRegTree.ItemNewKeyClickHandler(Sender : TObject);
begin
  if ItemFocused <> nil
   then TriggerKeyCreateSubKeyEvent(NameToRootType(GetStartItem(ItemFocused).Text), ComputerName, GetKeyName(ItemFocused))
   else TriggerKeyCreateSubKeyEvent(rrtUnknown, ComputerName, '');
end;  { ItemNewClickHandler }

procedure TElRegTree.ItemFindClickTransfer(Sender : TObject);
{ Transfers ItemFind OnClick event to the outside world. }
begin
  if (Assigned(FItemFindClick)) then FItemFindClick(Self);
end;  { ItemFindClickTransfer }

procedure TElRegTree.ItemReplaceClickTransfer(Sender : TObject);
{ Transfers ItemReplace OnClick event to the outside world. }
begin
  if (Assigned(FItemReplaceClick)) then FItemReplaceClick(Self);
end;  { ItemReplaceClickTransfer }

procedure TElRegTree.ItemDeleteClickHandler(Sender : TObject);
var DoAct : boolean;
begin
  if ItemFocused <> nil then
  begin
  DoAct := true;
  TriggerKeyDeleteEvent(NameToRootType(GetStartItem(ItemFocused).Text), ComputerName, GetKeyName(ItemFocused), DoAct);
  if DoAct then
  begin
  if not KeyDelete(ComputerName, NameToRootType(GetStartItem(ItemFocused).Text), GetKeyName(ItemFocused)) then
  MessageDlg('Failed to delete a key: '+ #13#10 + GetLastRegError, mtError, [mbOk], 0) else
  begin
  TriggerKeyDeletedEvent(NameToRootType(GetStartItem(ItemFocused).Text), ComputerName, GetKeyName(ItemFocused));
  ItemFocused.Delete;
  end;
  end;
  end;
end;  { ItemDeleteClickTransfer }

procedure TElRegTree.ItemRenameClickHandler(Sender : TObject);
begin
  if (ItemFocused <> nil) and (ItemFocused.Parent <> nil)
  then ItemFocused.EditText;
end;  { ItemRenameClickHandler }

procedure TElRegTree.CopyKeyName;
begin
  ItemCopyKeyNameClickHandler(Self);
end;

procedure TElRegTree.ItemCopyKeyNameClickHandler(Sender : TObject);
begin
  if ItemFocused <> nil then
    Clipboard.AsText := ItemFocused.GetFullNameEx('\', false);
end;  { ItemCopyKeyNameClickHandler }

procedure TElRegTree.TriggerKeyDeletedEvent(Root : TRegRootType;
  const ComputerName, Key: string);
begin
  if (assigned(FOnKeyDeleted)) then
    FOnKeyDeleted(Self, Root , ComputerName, Key);
end;  { TriggerKeyDeletedEvent }

(*
procedure TElRegTree.TriggerKeyRenamedEvent(Root : TRegRootType;
begin
  if (assigned(FOnKeyRenamed)) then
    FOnKeyRenamed(Self, Root , ComputerName, Key);
end;  { TriggerKeyRenamedEvent }
*)

procedure TElRegTree.TriggerKeyCreateSubKeyEvent(Root : TRegRootType; const ComputerName, Key: string);
begin
  if (assigned(FOnKeyCreateSubKey)) then
    FOnKeyCreateSubKey(Self, Root , ComputerName, Key);
end;  { TriggerKeyCreateSubKeyEvent }

procedure TElRegTree.TriggerKeyCreateValueEvent(Root : TRegRootType; const ComputerName, Key: string);
begin
  if (assigned(FOnKeyCreateValue)) then
  FOnKeyCreateValue(Self, Root , ComputerName, Key);
end;  { TriggerKeyCreateValueEvent }

procedure TElRegTree.SetReadOnly(newValue : Boolean);
{ Sets data member FReadOnly to newValue. }
begin
  if (FReadOnly <> newValue) then
  begin
  FReadOnly := newValue;
  end;  { if }
end;  { SetReadOnly }

procedure   TElRegTree.SetActive (V: Boolean);
begin
  if  fActive <> V  then
  begin
  fActive := V;

  if  fActive   then  Rebuild;
  end;
end;

{-----------}


destructor TElRegTree.Destroy;
begin
  Active := FALSE;
  Destroying;
  Images := nil;
  FImages.Free;
  inherited;
end;  { Destroy }

constructor TElRegTree.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fActive := FALSE;
  DragType := dtOLE;
  IsUpdating := true;
  FImages := TElImageList.Create(nil);
  FImages.ResourceLoad(RTBITMAP, 'REGTREEFOLDER', clAqua);
  FImages.ResourceLoad(RTBITMAP, 'REGTREEFOLDEROPEN', clAqua);
  Images := FImages;
  ShowImages := true;
  CreatePopupMenuItems;
  InitializePopupMenuItems;
  AddMenuItemsToPopupMenu;
  MultiSelect := false;
  ShowButtons := true;
  FShowMenu := true;
  ScrollTracking := true;
  FRegRoots := [rrtHKEY_CLASSES_ROOT, rrtHKEY_CURRENT_USER, rrtHKEY_LOCAL_MACHINE, rrtHKEY_USERS, rrtHKEY_CURRENT_CONFIG];
  IsUpdating := false;
end;  { Create }

procedure Register;
begin
  RegisterComponents('EldoS', [TElRegTree, TElRegSaveDialog, TElRegOpenDialog]);
end;  { Register }

end.

