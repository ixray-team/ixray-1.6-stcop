unit ExplorerMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ElACtrls, ElShellCtl, ExtCtrls, ElPanel, ElToolbar, ElSplit,
  ElTools, ShellAPI, ElShellUtils, ShlObj, ElXPThemedControl, ElTree, ElPopBtn, Menus;

type
  TExplorerForm = class(TForm)
    Toolbar: TElToolBar;
    Combo: TElShellComboBox;
    Tree: TElShellTree;
    Splitter: TElSplitter;
    List: TElShellList;
    UpLevelButton: TElToolButton;
    MainMenu1: TMainMenu;
    FileItem: TMenuItem;
    CloseItem: TMenuItem;
    procedure CloseItemClick(Sender: TObject);
    procedure ComboChange(Sender: TObject);
    procedure TreeItemFocused(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure UpLevelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExplorerForm: TExplorerForm;

implementation

{$R *.DFM}

procedure TExplorerForm.CloseItemClick(Sender: TObject);
begin
  Close;
end;

procedure TExplorerForm.ComboChange(Sender: TObject);
begin
  Tree.SetSelectionPIDL(Combo.Selection);
end;

procedure TExplorerForm.TreeItemFocused(Sender: TObject);
var PIDL : PItemIDList;
begin
  PIDL := Tree.FocusedPIDL;
  Combo.SetSelection(PIDL);
  if CalcPIDLSize(PIDL) <> 2 then
  begin
    List.SetRootPIDL(PIDL);
  end
  else
    List.Folder := sfoDesktop;
  FreeIDList(PIDL);
  if Tree.ItemFocused <> nil then
  begin
    Tree.ItemFocused.Expanded := true;
    Caption := 'EldoS Explorer - ' + Tree.ItemFocused.FullName;
  end
  else
    Caption := 'EldoS Explorer';
end;

procedure TExplorerForm.ListDblClick(Sender: TObject);
var Item : TElTreeItem;
    HCol : integer;
    IP   : TSTItemPart;
    P    : TPoint;
    i    : Cardinal;
    PIDL : PItemIDList;
begin
  List.IsUpdating := true;
  Tree.IsUpdating := true;
  i := GetMessagePos();
  P.x := LOWORD(i);
  P.y := HIWORD(i);
  P := List.ScreenToClient(P);
  Item := List.GetItemAt(P.x, p.y, IP, HCol);
  if Item <> nil then
  begin
    if TElShellListItem(Item).IsFolder then
    begin
      PIDL := TElShellListItem(Item).BuildFullPIDL;
      Combo.SetSelection(PIDL);
      Tree.SetSelectionPIDL(PIDL);
      FreeIDList(PIDL);
    end
    else
    begin
      ShellExecute(Handle, 'open', PChar(TElShellListItem(Item).FullName), nil, PChar(GetCurrentDir), SW_SHOW);
    end;
  end;
  List.IsUpdating := false;
  Tree.IsUpdating := false;
end;

procedure TExplorerForm.UpLevelButtonClick(Sender: TObject);
var ParentPIDL,
    PIDL : PItemIDList;
begin
  PIDL := List.BuildRootPIDL;
  if CalcPIDLSize(PIDL) <> 2 then
  begin
    ParentPIDL := GetParentPIDL(PIDL);
    Combo.SetSelection(ParentPIDL);
    Tree.SetSelectionPIDL(ParentPIDL);
    List.SetRootPIDL(ParentPIDL);
    FreeIDList(ParentPIDL);
  end;
  FreeIDList(PIDL);
end;

procedure TExplorerForm.FormCreate(Sender: TObject);
var PIDL : PItemIDList;
begin
  PIDL := GetEmptyPIDL;
  Tree.SetSelectionPIDL(PIDL);
  FreeIDList(PIDL);
end;

end.

