{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit frmSoundMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElTree, ElPopBtn, Menus, ElHeader,
  DesignIntf, DesignEditors, DesignWindows, DsnConst,
  TypInfo, ElSndMap, ElBtnCtl, ElTreeAdvEdit, ElTreeModalEdit, ElXPThemedControl;

type
  TSoundMapForm = class(TForm)
    OkBtn : TElPopupButton;
    List : TElTree;
    AddBtn : TElPopupButton;
    RemoveBtn : TElPopupButton;
    PopupMenu : TPopupMenu;
    AddItem : TMenuItem;
    RemoveItem : TMenuItem;
    CancelBtn : TElPopupButton;
    SoundDialog : TOpenDialog;
    PlayItem : TMenuItem;
    procedure AddItemClick(Sender : TObject);
    procedure RemoveItemClick(Sender : TObject);
    procedure ListItemFocused(Sender : TObject);
    procedure ListEditRequest(Sender : TObject; Item : TElTreeItem;
      Section : TElHeaderSection);
    procedure PlayItemClick(Sender : TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMap : TElSoundMap;
    {$ifdef VER3_EDITORS}
    AdvEditor: TElTreeInplaceAdvancedEdit;
    AnEditor : TElTreeInplaceModalEdit;
    procedure ModalEditorExecute(Sender : TObject; var Accepted : boolean);
    {$endif}
  public
    procedure SetData(AMap : TElSoundMap);
    procedure GetData(AMap : TElSoundMap);
  end;

type
  TSoundNameProperty = class(TPropertyEditor)
  public
    procedure GetValues(Proc : TGetStrProc); override;
    function GetValue : string; override;
    procedure SetValue(const Value : string); override;
    function GetAttributes : TPropertyAttributes; override;
  end;

var
  SoundMapForm : TSoundMapForm;

type
  TSoundMapEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

implementation

function TSoundNameProperty.GetAttributes : TPropertyAttributes;
begin
  result := [paValueList, paSortList, paMultiSelect];
end;

function TSoundNameProperty.GetValue : string;
var
  F : TComponent;
begin
  try
    F := GetComponent(0) as TComponent;
    if F <> nil then
      result := GetStrValue
    else
      result := '(unknown)';
  except
    result := '(unknown)';
  end; // try/except
end;

procedure TSoundNameProperty.GetValues(Proc : TGetStrProc);
var
  F : TComponent;
  SM : TElSoundMap;
  SL : TStringList;
  i : integer;

  function GetSoundMap(AComponent : TComponent) : TElSoundMap;
  var
    i : integer;
    PropCount : Integer;
    PropList : PPropList;
  begin
    PropCount := GetPropList(AComponent.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], nil);
    GetMem(PropList, PropCount * sizeof(pointer));
    GetPropList(AComponent.ClassInfo, [tkClass], PropList);
    result := nil;
    for i := 0 to PropCount - 1 do // Iterate
    begin
      if PropList[i]^.PropType^.Name = 'TElSoundMap' then
      begin
        result := TElSoundMap(GetOrdProp(AComponent, PropList[i]));
        break;
      end;
    end; // for
    FreeMem(PropList);
  end;

begin
  try
    F := GetComponent(0) as TComponent;
    if F <> nil then
    begin
      SM := GetSoundMap(F);
      if SM <> nil then
      begin
        SL := TStringList.Create;
        SL.Assign(SM.EventKeys);
        for i := 0 to SL.Count - 1 do // Iterate
          Proc(SL[i]);
        SL.Free;
      end
      else
        inherited;
    end
    else
      inherited;
  except
  end;
end;

procedure TSoundNameProperty.SetValue(const Value : string);
begin
  SetStrValue(Value);
end;

procedure TSoundMapEditor.ExecuteVerb(Index : Integer);
begin
  if Index = 0 then
  begin
    SoundMapForm := TSoundMapForm.Create(nil);
    try
      SoundMapForm.SetData(TElSoundMap(Component));
      if SoundMapForm.ShowModal = mrOk then
      begin
        SoundMapForm.GetData(TElSoundMap(Component));
      end;
    finally // wrap up
      SoundMapForm.Free;
    end; // try/finally
  end;
end;

function TSoundMapEditor.GetVerb(Index : Integer) : string;
begin
  if Index = 0 then Result := 'Sound Map Editor';
end;

function TSoundMapEditor.GetVerbCount : Integer;
begin
  Result := 1;
end;

{$R *.DFM}

procedure TSoundMapForm.AddItemClick(Sender : TObject);
var
  TI : TElTreeItem;
begin
  TI := List.Items.AddItem(nil);
  TI.ColumnText.Add('');
  TI.ColumnText.Add('');
  TI.ColumnText.Add('');
  TI.Checked := true;
  TI.ShowCheckBox := true;
  TI.CheckBoxType := ectCheckBox;
  TI.CheckBoxEnabled := true;
end;

procedure TSoundMapForm.RemoveItemClick(Sender : TObject);
begin
  if List.ItemFocused <> nil then List.ItemFocused.Delete;
end;

procedure TSoundMapForm.ListItemFocused(Sender : TObject);
begin
  RemoveItem.Enabled := List.ItemFocused <> nil;
  PlayItem.Enabled := List.ItemFocused <> nil;
end;

procedure TSoundMapForm.SetData(AMap : TElSoundMap);
var
  i,
    j : integer;
  TI : TElTreeItem;
  S : string;
begin
  FMap := AMap;
  j := AMap.EventKeys.Count;
  for i := 0 to j - 1 do // Iterate
  begin
    TI := List.Items.AddItem(nil);
    S := AMap.EventKeys[i];
    TI.ColumnText.Add(AMap.EventLabel[s]);
    TI.ColumnText.Add(AMap.EventValue[s]);
    TI.ColumnText.Add(S);
    TI.Checked := AMap.EventEnabled[s];
    TI.ShowCheckBox := true;
    TI.CheckBoxType := ectCheckBox;
    TI.CheckBoxEnabled := true;
  end;
end;

procedure TSoundMapForm.GetData(AMap : TElSoundMap);
var
  i : integer;
  TI : TElTreeItem;
begin
  i := AMap.EventKeys.Count - 1;
  while i >= 0 do
  begin
    AMap.Delete(AMap.EventKeys[i]);
    dec(i);
  end;
  for i := 0 to List.Items.Count - 1 do // Iterate
  begin
    TI := List.Items[i];
    AMap.Add(TI.ColumnText[2], TI.ColumnText[0], TI.ColumnText[1], TI.Checked);
  end; // for
end;

procedure TSoundMapForm.ListEditRequest(Sender : TObject; Item : TElTreeItem; Section : TElHeaderSection);
begin
  if Section.Index = 1 then
  begin
    if SoundDialog.Execute then Item.ColumnText[1] := SoundDialog.FileName;
  end;
end;

procedure TSoundMapForm.PlayItemClick(Sender : TObject);
var
  ti : TElTreeItem;
begin
  TI := List.ItemFocused;
  if (TI <> nil) then FMap.Play(TI.ColumnText[2]);
end;

{$ifdef VER3_EDITORS}
procedure TSoundMapForm.ModalEditorExecute(Sender : TObject; var Accepted : boolean);
begin
  if SoundDialog.Execute then
    AnEditor.ValueAsText := SoundDialog.FileName;
end;
{$endif}

procedure TSoundMapForm.FormCreate(Sender: TObject);
begin
{$ifdef VER3_EDITORS}
  AnEditor := TElTreeInplaceModalEdit.Create(Self);
  AnEditor.OnExecute := ModalEditorExecute;
  AnEditor.Tree := List;
  AdvEditor := TElTreeInplaceAdvancedEdit.Create(Self);
  AdvEditor.Tree := List;
{$else}
  List.OnEditRequest := ListEditRequest;
{$endif}
end;

end.
