
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit frmFormPers;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  ElTree, ExtCtrls, ElPopBtn, ElImgLst,
  DesignEditors, DesignWindows, DsnConst, DesignIntf, 
  ElFrmPers, ElMTree,
  Menus, ElIni, TypInfo, ElVCLUtils, StdCtrls, ElHeader, ElBtnCtl, Graphics 
{$IFDEF VCL_4_USED}
  , ImgList, ElXPThemedControl
{$ENDIF};

type
  TPersPropsForm = class(TForm)
    BtnPanel : TPanel;
    OkBtn : TElPopupButton;
    CancelBtn : TElPopupButton;
    ImageList : TElImageList;
    Tree : TElTree;
    ElFormPersist1 : TElFormPersist;
    ElIniFile1 : TElIniFile;
    procedure TreeItemChange(Sender : TObject; Item : TElTreeItem;
      ItemChangeMode : TItemChangeMode);
    procedure TreeHeaderSectionCollapse(Sender : TObject;
      Section : TElHeaderSection);
    procedure TreeHeaderSectionExpand(Sender : TObject;
      Section : TElHeaderSection);
    procedure ElFormPersist1Restore(Sender : TObject);
    procedure ElFormPersist1Save(Sender : TObject);
  private
    { Private declarations }
  protected
    FOldWidth : integer;
  public
    Props : TElMTree;
    FPers : TElFormPersist;
    procedure GetData;
    procedure SetData;
    constructor Create(AOwner : TComponent); override;
    { Public declarations }
  end;

type
  TPropListProperty = class(TPropertyEditor)
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
  end;

type
  TElPropListEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

var
  PersPropsForm : TPersPropsForm;

implementation

procedure TElPropListEditor.ExecuteVerb(Index : Integer);
var
  F : TElFormPersist;
  Form : TCustomForm;
begin
  if Index = 0 then
  begin
    with TPersPropsForm.Create(Application) do
    begin
      try
        try
          F := TElFormPersist(Component);
          if F = nil then 
          begin
            MessageBox(0, PChar('No component!'), nil, 0);
            Free;
            exit;
          end;
          Caption := F.Owner.Name + '.' + F.Name;
          Props := F.PropsToStore;
          FPers := F;
          SetData;
          if (ShowModal = mrOK) then
          begin
            GetData;
            Form := GetOwnerForm(F);
            if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
          end;
        except
          on E : Exception do
            MessageBox(0, PChar(E.Message), nil, 0);
        end;
      finally
        Free;
      end; {try/finally}
    end; {with}
  end;
end;

function TElPropListEditor.GetVerb(Index : Integer) : string;
begin
  if Index = 0 then Result := 'Stored propeties ...';
end;

function TElPropListEditor.GetVerbCount : Integer;
begin
  Result := 1;
end;

constructor TPersPropsForm.Create(AOwner : TComponent); 
begin
  inherited;
end;

procedure TPersPropsForm.GetData;

  procedure IntGetData(Item : TElTreeItem; MItem : TElMTreeItem);
  var
    MItem1 : TElMTreeItem;
    PropData : PElPropData;
    i : integer;
  begin
    if (Item.CheckBoxState = cbGrayed) or (Item.CheckBoxState = cbChecked) then
    begin
      if Item.Data = nil then
      begin
        New(PropData);
        MItem1 := Props.AddItem(MItem, PropData);
        MItem1.Data := PropData;
        PropData.Name := Item.Text;
      end
      else
      begin
        MItem1 := TElMTreeItem(Item.Data);
        PropData := PElPropData(MItem1.Data);
      end;
      PropData.Store := Item.CheckBoxState = cbChecked;
      if Item.CheckBoxType = ect3SCheckBox then
      begin
        if (Item.AnObject is TCollectionItem) or (Pos('<', PropData.Name) > 0) then
          PropData.PropType := estCollection
        else
          PropData.PropType := estComp;
      end
      else
        PropData.PropType := estProp;
      if ((PropData.PropType = estComp) or (PropData.PropType = estCollection)) and
         (not PropData.Store) then
        for i := 0 to Item.Count - 1 do
          IntGetData(Item.Children[i], MItem1);
    end
    else if (Item.CheckBoxType <> ect3SCheckBox) and (Item.CheckBoxState = cbUnchecked) then
    begin
      MItem1 := TElMTreeItem(Item.Data);
      if MItem1 <> nil then
      begin
        if MItem1.Data <> nil then Dispose(PElPropData(MItem1.Data));
        MItem1.Data := nil;
        Props.DeleteItem(MItem1);
      end;
    end
    else
    begin
      if (Item.CheckBoxType = ect3SCheckBox) and (Item.CheckBoxState = cbUnchecked) then
      begin
        if Item.Data = nil then
        begin
          New(PropData);
          MItem1 := Props.AddItem(MItem, PropData);
          MItem1.Data := PropData;
          PropData.Name := Item.Text;
        end
        else
        begin
          MItem1 := TElMTreeItem(Item.Data);
          PropData := PElPropData(MItem1.Data);
        end;
        PropData.Store := false;
        PropData.PropType := estComp;
        if MItem1 <> nil then
        begin
          if MItem1.Data <> nil then Dispose(PElPropData(MItem1.Data));
          MItem1.Data := nil;
          Props.DeleteItem(MItem1);
        end;
      end;
    end;
  end;

var
  Item : TElTreeItem;

begin
  if Tree.Items.Count > 0 then
  begin
    Item := Tree.Items[0];
    while Item <> nil do
    begin
      IntGetData(Item, nil);
      Item := Item.GetNextSibling;
    end;
  end;
end; {GetData}

procedure TPersPropsForm.SetData;
type TSRec = record
       AnObj : TObject;
       Item  : TElTreeItem;
     end;
     PSRec = ^TSRec;

  procedure IntFillTree(Comp : TObject; Name, TypeName : string; MItem : TElMTreeItem; Parent : TElTreeItem);
  var
    Item : TElTreeItem;
    PropCount : Integer;
    PropList : PPropList;
    Obj : TObject;
    i, j : integer;
    MItem1 : TElMTreeItem;
    SRec   : TSRec;
    IterateProc: TIterateProcAnonymusMethod;
  begin
    IterateProc :=   procedure (Item: TElTreeItem; Index: integer; var ContinueIterate: boolean;
    IterateData: pointer; Tree: TCustomElTree)
  begin
    if PSRec(IterateData).AnObj = Item.AnObject then
    begin
      ContinueIterate := false;
      PSRec(IterateData).Item := Item;
    end;
  end;

    Item := Tree.Items.AddItem(Parent);
    Item.Text := Name;
    MItem1 := nil;
    if MItem <> nil then
    begin
      for j := 0 to MItem.List.Count - 1 do // Iterate
      begin
        if Uppercase(PElPropData(TElMTreeItem(MItem.List[j]).Data).Name) = Uppercase(Name) then
        begin
          MItem1 := TElMTreeItem(MItem.List[j]);
          break;
        end;
      end; // for
    end;
    Item.Data := MItem1;
    Item.AnObject := Comp;
    Item.ShowCheckBox := true;
    Item.ColumnText.Add(TypeName);
    Item.CheckBoxState := cbUnchecked;
    if Comp <> nil then
    begin
      Item.ImageIndex := 1;
      Item.StateImageIndex := 1;
      Item.CheckBoxType := ect3SCheckBox;
      if MItem1 <> nil then
      begin
        if PElPropData(MItem1.Data).Store then
          Item.CheckBoxState := cbChecked
        else
          Item.CheckBoxState := cbGrayed;
      end;
    end
    else
    begin
      Item.ImageIndex := 0;
      Item.StateImageIndex := 0;
      Item.CheckBoxType := ectCheckBox;
      if (MItem1 <> nil) and (PElPropData(MItem1.Data).Store) then
        Item.CheckBoxState := cbChecked;
    end;
    if Parent <> nil then
      Item.CheckBoxEnabled := Parent.CheckBoxState = cbGrayed
    else
      ;
    if Assigned(Comp) and (not ({(Comp is TCollection) or }(Comp is TStrings))) then
    begin
      PropCount := GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], nil);
      GetMem(PropList, PropCount * sizeof(pointer));
      GetPropList(Comp.ClassInfo, [tkInteger, tkEnumeration, tkString, tkLString, tkSet, tkClass], PropList);
      for i := 0 to PropCount - 1 do // Iterate
      begin
        if (PropList[i]^.PropType^.Kind = tkClass) then
        begin
          Obj := TObject(GetOrdProp(Comp, PropList[i]));

          SRec.Item := nil;
          SRec.AnObj := Obj;
          Item.Owner.Items.Iterate(false, true, IterateProc, @SRec);
          if SRec.Item = nil then
          begin
            if (Obj is TCollection) then
            begin
              for j := 0 to TCollection(Obj).Count - 1 do
                IntFillTree(TCollection(Obj).Items[j], PropList[i].Name + '<' + IntToStr(j) + '>', PropList[i]^.PropType^.Name, MItem1, Item)
            end
            else
            if (Obj <> nil) and (not (Obj is TStrings)) then
              IntFillTree(Obj, PropList[i].Name, PropList[i]^.PropType^.Name, MItem1, Item)
            else
            if (Obj <> nil) and (not (Obj is TComponent)) then
              IntFillTree(nil, PropList[i].Name, PropList[i]^.PropType^.Name, MItem1, Item);
          end;
        end // if
        else
          IntFillTree(nil, PropList[i].Name, PropList[i]^.PropType^.Name, MItem1, Item);
      end; // for
    end; // if
  end;

var
  Form : TForm;
  i : integer;
  SaveCursor : TCursor;
  S : string;
begin

  Tree.Items.BeginUpdate;
  SaveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    Form := GetOwnerForm(FPers);
    
    if Form <> nil then
    begin
      for i := 0 to Form.ComponentCount - 1 do // Iterate
      begin
        try
          S := Form.Components[i].ClassName; 
        except
          S := 'Unknown component class';
        end;
        IntFillTree(Form.Components[i], Form.Components[i].Name, S, Props.Root, nil);
      end;
    end;    
  finally
    Tree.Items.EndUpdate;
    Screen.Cursor := SaveCursor;
  end;
end; {SetData}

procedure TPropListProperty.Edit;
var
  F : TElFormPersist;
  Form : TCustomForm;
begin
  with TPersPropsForm.Create(Application) do
  begin
    try
      try
        F := (GetComponent(0) as TElFormPersist);
        if F = nil then 
        begin
          MessageBox(0, PChar('No component!'), nil, 0);
          Free;
          exit;
        end;
        Caption := F.Owner.Name + '.' + F.Name;
        Props := F.PropsToStore;
        FPers := F;
        SetData;
        if (ShowModal = mrOK) then
        begin
          GetData;
          Form := GetOwnerForm(F);
          if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
        end;
      except
        on E : Exception do
          MessageBox(0, PChar(E.Message), nil, 0);
      end;
    finally
      Free;
    end; {try/finally}
  end; {with}
end; {Edit}

function TPropListProperty.GetAttributes : TPropertyAttributes;
begin
  GetAttributes := [paDialog];
end; {GetAttributes}

function TPropListProperty.GetValue : string;
begin
  result := '(Stored properties)';
end; {GetValue}

{$R *.DFM}

procedure TPersPropsForm.TreeItemChange(Sender : TObject; Item : TElTreeItem;
  ItemChangeMode : TItemChangeMode);
var
  i : integer;
begin
  if ItemChangeMode = icmCheckState then
  begin
    if Item.ImageIndex = 1 then // this is an object
    begin
      for i := 0 to Item.Count - 1 do // Iterate
      begin
        Item.Children[i].CheckBoxEnabled := Item.CheckBoxState = cbGrayed;
      end; // for
    end;
    //Item.Parent.CheckBoxState := cbGrayed;
  end;
end;

procedure TPersPropsForm.TreeHeaderSectionCollapse(Sender : TObject;
  Section : TElHeaderSection);
begin
  FOldWidth := Section.Width;
  Section.Width := Section.Width + Tree.HeaderSections[1].Width;
end;

procedure TPersPropsForm.TreeHeaderSectionExpand(Sender : TObject;
  Section : TElHeaderSection);
begin
  Section.Width := FOldWidth;
end;

procedure TPersPropsForm.ElFormPersist1Restore(Sender : TObject);
begin
{$IFDEF SUPPORT_STORAGE}
  Tree.Restore;
{$ENDIF}
end;

procedure TPersPropsForm.ElFormPersist1Save(Sender : TObject);
begin
{$IFDEF SUPPORT_STORAGE}
  Tree.Save;
{$ENDIF}
end;

end.
