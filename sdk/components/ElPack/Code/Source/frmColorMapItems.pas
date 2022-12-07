
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
{$include elpack2.inc}
{$ifdef ELPACK_SINGLECOMP}
{$I ElPack.inc}
{$else}
{$ifdef LINUX}
{$I ../ElPack.inc}
{$else}
{$I ..\ElPack.inc}
{$endif}
{$endif}

unit frmColorMapItems;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ElColorMap, ElBtnCtl, ElPopBtn, ElACtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElCheckCtl, ElClrCmb, ElXPThemedControl;

type
  TColorMapItemsForm = class(TForm)
    Label1 : TLabel;
    Label2 : TLabel;
    FgColor : TElColorCombo;
    BkColor : TElColorCombo;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    IDLbl : TLabel;
    OkBtn: TElPopupButton;
    CancelBtn: TElPopupButton;
    AddBtn: TElPopupButton;
    DelBtn: TElPopupButton;
    AddGroupBtn: TElPopupButton;
    DelGroupBtn: TElPopupButton;
    EntryLB: TElAdvancedListBox;
    GroupLB: TElAdvancedListBox;
    UseBkCB: TElCheckBox;
    UseFgCB: TElCheckBox;
    procedure UseFgCBClick(Sender : TObject);
    procedure CustomFgCBClick(Sender : TObject);
    procedure AddGroupBtnClick(Sender : TObject);
    procedure GroupLBClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure UseBkCBClick(Sender : TObject);
    procedure DelGroupBtnClick(Sender : TObject);
    procedure AddBtnClick(Sender : TObject);
    procedure DelBtnClick(Sender : TObject);
    procedure EntryLBClick(Sender : TObject);
    procedure FgColorChange(Sender : TObject);
    procedure BkColorChange(Sender : TObject);
  private
    { Private declarations }
  protected
    GrSel,
      MapSel,
      EntSel : integer;

    SaveVal : string;
    procedure RefreshEntriesList;
  public
    { Public declarations }
    Runtime : boolean;
    Map : TElColorMap;
    procedure RefreshColors;
  end;

var
  ColorMapItemsForm : TColorMapItemsForm;

implementation

{$R *.DFM}

procedure TColorMapItemsForm.UseFgCBClick(Sender : TObject);
var
  T : TColorEntry;
begin
  if MapSel = -1 then exit;
  FgColor.Enabled := UseFgCB.Checked;
  T := Map[MapSel];
  T.UseFG := FgColor.Enabled;
  Map[MapSel] := T;
end;

procedure TColorMapItemsForm.CustomFgCBClick(Sender : TObject);
begin
  FgColor.Enabled := UseFgCB.Checked;
end;

procedure TColorMapItemsForm.AddGroupBtnClick(Sender : TObject);
var
  S, S1 : string;
  i : integer;
  b : boolean;
begin
  S := InputBox('New colors group', 'Enter the group name:', '');
  if S = '' then exit;
  S1 := UpperCase(S);
  b := false;
  for i := 0 to GroupLB.Items.Count - 1 do
  begin
    if S1 = UpperCase(GroupLB.Items[i]) then
    begin
      b := true;
      break;
    end;
  end;
  if b then
  begin
    MessageBox(0, 'Group already exists', '', 0);
    exit;
  end;
  GroupLB.ItemIndex := GroupLB.Items.Add(S);
  GroupLBClick(Self);
end;

procedure TColorMapItemsForm.GroupLBClick(Sender : TObject);
begin
  DelGroupBtn.Enabled := GroupLB.ItemIndex <> -1;
  AddBtn.Enabled := DelGroupBtn.Enabled;
  if GroupLB.ItemIndex <> GrSel then
  begin
    GrSel := GroupLB.ItemIndex;
    RefreshEntriesList;
  end;
end;

procedure TColorMapItemsForm.FormCreate(Sender : TObject);
begin
  Map := TElColorMap.Create(self);
end;

procedure TColorMapItemsForm.FormDestroy(Sender : TObject);
begin
  Map.Free;
end;

procedure TColorMapItemsForm.FormShow(Sender : TObject);
var
  i : integer;
  S : string;
begin
  for i := 0 to Map.Count - 1 do
  begin
    S := Map[i].Group;
    if GroupLB.Items.IndexOf(S) = -1 then GroupLB.Items.Add(S);
  end;
  if Runtime then
    Height := 260
  else
    Height := 310;
  GrSel := -1;
  MapSel := -1;
  EntSel := -1;
  GroupLB.ItemIndex := 0;
  GroupLBClick(self);
  IDLbl.Visible := not RunTime;
  Label5.Visible := not RunTime;
end;

procedure TColorMapItemsForm.RefreshEntriesList; { protected }
var
  i : integer;
begin
  EntryLB.Items.Clear;
  if GroupLB.ItemIndex = -1 then exit;
  for i := 0 to Map.Count - 1 do
    if Map[i].Group = GroupLB.Items[GroupLB.ItemIndex] then EntryLB.Items.AddObject(Map[i].Name, TObject(Map[i].ID));
  EntryLB.ItemIndex := 0;
  EntryLBClick(self);
end; { RefreshEntriesList }

procedure TColorMapItemsForm.RefreshColors; { public }
var
  T : TColorEntry;
begin
  if EntryLB.ItemIndex = -1 then
  begin
    MapSel := -1;
    exit;
  end;
  MapSel := integer(EntryLB.Items.Objects[EntryLB.ItemIndex]);
  T := Map[MapSel];
  UseFgCB.Checked := T.UseFg;
  UseFgCB.Visible := not Runtime;
  UseFgCBClick(self);
  UseBkCB.Checked := T.UseBk;
  UseBkCB.Visible := not Runtime;
  UseBkCBClick(self);
  FgColor.SelectedColor := T.FgColor;
  BkColor.SelectedColor := T.BkColor;
end; { RefreshColors }

procedure TColorMapItemsForm.UseBkCBClick(Sender : TObject);
var
  T : TColorEntry;
begin
  if MapSel = -1 then exit;
  BkColor.Enabled := UseBkCB.Checked;
  T := Map[MapSel];
  T.UseBK := BkColor.Enabled;
  Map[MapSel] := T;
end;

procedure TColorMapItemsForm.DelGroupBtnClick(Sender : TObject);
var
  S : string;
  i : integer;

begin
  if GroupLB.ItemIndex = -1 then exit;
  s := GroupLB.Items[GroupLB.ItemIndex];
  GroupLB.Items.Delete(GroupLB.ItemIndex);
  i := 0;
  while i < Map.Count do
    if Map[i].Group = S then
      Map.DeleteItem(i)
    else
      inc(i);
  RefreshEntriesList;
end;

procedure TColorMapItemsForm.AddBtnClick(Sender : TObject);
var
  T : TColorEntry;
  S, S1 : string;
  i : integer;
  b : boolean;
begin
  if GroupLB.ItemIndex = -1 then exit;
  T.Group := GroupLB.Items[GroupLB.ItemIndex];
  S := InputBox('New colors entry', 'Enter the entry name:', '');
  if S = '' then exit;
  S1 := UpperCase(S);
  b := false;
  for i := 0 to EntryLB.Items.Count - 1 do
  begin
    if S1 = UpperCase(EntryLB.Items[i]) then
    begin
      b := true;
      break;
    end;
  end;
  if b then
  begin
    MessageBox(0, 'Entry already exists', '', 0);
    exit;
  end;
  T.Name := S;
  T.UseFg := true;
  T.UseBk := true;
  T.FgColor := clNone;
  T.BkColor := clNone;
  i := Map.AddItem(T);
  EntryLB.ItemIndex := EntryLB.Items.AddObject(T.Name, TObject(Map[i].ID));
  EntryLbClick(Self);
end;

procedure TColorMapItemsForm.DelBtnClick(Sender : TObject);
begin
  Map.DeleteItem(MapSel);
  EntryLB.Items.Delete(EntSel);
end;

procedure TColorMapItemsForm.EntryLBClick(Sender : TObject);
var
  E : TColorEntry;
begin
  EntSel := EntryLB.ItemIndex;
  if EntryLB.ItemIndex = -1 then
  begin
    MapSel := -1;
    FgColor.Enabled := false;
    BkColor.Enabled := false;
    UseFgCb.Enabled := false;
    UseBkCb.Enabled := false;
    DelBtn.Enabled := false;
    UseFgCb.Checked := false;
    UseBkCb.Checked := false;
    IDLbl.Caption := '';
  end
  else
  begin
    MapSel := Map.EntryByID(integer(EntryLb.Items.Objects[EntryLB.ItemIndex]));
    E := Map.Items[MapSel];
    DelBtn.Enabled := true;
    UseFgCb.Enabled := not RunTime;
    UseBkCb.Enabled := not RunTime;
    UseFgCb.Checked := E.UseFg;
    if E.UseFg then FgColor.SelectedColor := E.FgColor;
    UseBkCb.Checked := E.UseBk;
    if E.UseBk then BkColor.SelectedColor := E.BkColor;
    IDLbl.Caption := IntToStr(E.ID);
  end;
end;

procedure TColorMapItemsForm.FgColorChange(Sender : TObject);
var
  T : TColorEntry;
begin
  T := Map[MapSel];
  T.FgColor := FgColor.SelectedColor;
  Map[MapSel] := T;
end;

procedure TColorMapItemsForm.BkColorChange(Sender : TObject);
var
  T : TColorEntry;
begin
  T := Map[MapSel];
  T.BkColor := BkColor.SelectedColor;
  Map[MapSel] := T;
end;

end.
