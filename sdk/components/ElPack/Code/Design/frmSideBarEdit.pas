{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit frmSideBarEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, DsgnIntf, Dialogs, ElSideBar,
  ExtCtrls, ElPanel, StdCtrls, ElSpin, ElBtnCtl, ElCheckCtl, ElACtrls,
  ElPopBtn;

type
  TSideBarEditForm = class(TForm)
    SideBar: TElSideBar;
    SectionsGB: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    SectionImageSpin: TElSpinEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    ItemImageSpin: TElSpinEdit;
    Label5: TLabel;
    SectionTagSpin: TElSpinEdit;
    Label6: TLabel;
    ItemTagSpin: TElSpinEdit;
    Label7: TLabel;
    Label8: TLabel;
    SectionCaptionEdit: TElAdvancedEdit;
    SectionHintEdit: TElAdvancedEdit;
    ItemCaptionEdit: TElAdvancedEdit;
    ItemHintEdit: TElAdvancedEdit;
    VisibleSectionCB: TElCheckBox;
    EnabledSectionCB: TElCheckBox;
    VisibleItemCB: TElCheckBox;
    EnabledItemCB: TElCheckBox;
    ContainsControlsCB: TElCheckBox;
    AddSectionBtn: TElPopupButton;
    MoveUpSectionBtn: TElPopupButton;
    DeleteSectionBtn: TElPopupButton;
    MoveDownSectionBtn: TElPopupButton;
    AddItemBtn: TElPopupButton;
    MoveUpItemBtn: TElPopupButton;
    DeleteItemBtn: TElPopupButton;
    MoveDownItemBtn: TElPopupButton;
    OkBtn: TElPopupButton;
    CancelBtn: TElPopupButton;
    InactiveSectionCB: TElCheckBox;
    procedure SideBarSectionChange(Sender: TObject);
    procedure SideBarItemChange(Sender: TObject);
    procedure SectionCaptionEditChange(Sender: TObject);
    procedure SectionImageSpinChange(Sender: TObject);
    procedure EnabledSectionCBClick(Sender: TObject);
    procedure VisibleSectionCBClick(Sender: TObject);
    procedure VisibleItemCBClick(Sender: TObject);
    procedure EnabledItemCBClick(Sender: TObject);
    procedure ItemCaptionEditChange(Sender: TObject);
    procedure ItemImageSpinChange(Sender: TObject);
    procedure MoveUpItemBtnClick(Sender: TObject);
    procedure MoveDownItemBtnClick(Sender: TObject);
    procedure MoveUpSectionBtnClick(Sender: TObject);
    procedure MoveDownSectionBtnClick(Sender: TObject);
    procedure AddSectionBtnClick(Sender: TObject);
    procedure AddItemBtnClick(Sender: TObject);
    procedure DeleteItemBtnClick(Sender: TObject);
    procedure DeleteSectionBtnClick(Sender: TObject);
    procedure SectionTagSpinChange(Sender: TObject);
    procedure ItemTagSpinChange(Sender: TObject);
    procedure SectionHintEditChange(Sender: TObject);
    procedure ItemHintEditChange(Sender: TObject);
    procedure ContainsControlsCBClick(Sender: TObject);
    procedure InactiveSectionCBClick(Sender: TObject);
  private
    FCurItem    : TElSideBarItem;
    FCurSection : TElSideBarSection;
  public
    OrigSideBar : TElSideBar;
    procedure GetData;
    procedure SetData;
  end;

var
  SideBarEditForm: TSideBarEditForm;

type
  TSideBarSectionsProperty = class(TPropertyEditor)
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
  end;

type
  TSideBarEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index : Integer); override;
    function GetVerb(Index : Integer) : string; override;
    function GetVerbCount : Integer; override;
  end;

implementation

{$R *.DFM}

procedure TSideBarEditor.ExecuteVerb(Index : Integer);
var Form : TCustomForm;
begin
  if Index = 0 then
  begin
    with TSideBarEditForm.Create(Application) do
    begin
      try
        { Set dialog's caption. }
        Caption := TElSideBar(Component).Owner.Name + '.' + TElSideBar(Component).Name + ' - ' + Caption;
        OrigSideBar := TElSideBar(Component);
        SetData;
        if (ShowModal = mrOk) then
        begin
          Form := GetParentForm(OrigSideBar);
          if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
          GetData;
        end;
      finally
        Free;  { Free dialog. }
      end;  { try/finally }
    end;  { with }
  end;
end;

function TSideBarEditor.GetVerb(Index : Integer) : string;
begin
  if Index = 0 then Result := 'Items Editor';
end;

function TSideBarEditor.GetVerbCount : Integer;
begin
  Result := 1;
end;

procedure TSideBarSectionsProperty.Edit;
var Form : TCustomForm;
begin
  with TSideBarEditForm.Create(Application) do
  begin
    try
      { Set dialog's caption. }
      Caption := TComponent(GetComponent(0)).Owner.Name + '.' + TComponent(GetComponent(0)).Name + ' - ' + Caption;
      OrigSideBar := GetComponent(0) as TElSideBar;
      SetData;
      if (ShowModal = mrOk) then
      begin
        Form := GetParentForm(OrigSideBar);
        if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
        GetData;
      end;
    finally
      Free;  { Free dialog. }
    end;  { try/finally }
  end;  { with }
end;  { Edit }

function TSideBarSectionsProperty.GetValue : string;
begin
  result := 'SideBar Sections';
end;

function TSideBarSectionsProperty.GetAttributes : TPropertyAttributes;
{ Returns information used by the Object Inspector to show the approprate adornments (e.g., "..." button, drop-down button). }
begin
  GetAttributes := [paDialog];
end;  { GetAttributes }

procedure TSideBarEditForm.SetData;
var i : integer;
    j : integer;
    Item : TElSideBarItem;
    Section : TElSideBarSection;
begin
  SideBar.SectionImages := OrigSideBar.SectionImages;
  SideBar.SectionHotImages := OrigSideBar.SectionHotImages;
  SideBar.SectionDisabledImages := OrigSideBar.SectionDisabledImages;

  SideBar.ItemImages := OrigSideBar.ItemImages;
  SideBar.ItemHotImages := OrigSideBar.ItemHotImages;
  SideBar.ItemDisabledImages := OrigSideBar.ItemDisabledImages;

  if SideBar.SectionImages <> nil then
     SectionImageSpin.MaxValue := SideBar.SectionImages.Count - 1;
  if SideBar.ItemImages <> nil then
     ItemImageSpin.MaxValue := SideBar.ItemImages.Count - 1;

  SideBar.ItemSize := OrigSideBar.ItemSize;

  SideBar.TopSpacing := OrigSideBar.TopSpacing;
  SideBar.Spacing := OrigSideBar.Spacing;
  SideBar.WordWrap := OrigSideBar.WordWrap;
  SideBar.BeginUpdate;
  SideBar.Sections.Assign(OrigSideBar.Sections);
  for i := 0 to SideBar.Sections.Count - 1 do    // Iterate
  begin
    Section := SideBar.Sections[i];
    Section.Disabled := not Section.Enabled;
    Section.Enabled := true;
    Section.Hidden := not Section.Visible;
    Section.Visible := true;
    Section.Active := not Section.Inactive;
    Section.Inactive := false;
    Section.Data := OrigSideBar.Sections[i];

    for j := 0 to Section.Items.Count - 1 do    // Iterate
    begin
      Item := Section.Items[j];
      Item.Disabled := not Item.Enabled;
      Item.Enabled := true;
      Item.Hidden := not Item.Visible;
      Item.Visible := true;
    end;    // for
  end;    // for
  SideBar.SectionIndex := OrigSideBar.SectionIndex;
  SideBar.ItemIndex := OrigSideBar.ItemIndex;
  SideBar.EndUpdate;
  SideBarSectionChange(SideBar);
end;

procedure TSideBarEditForm.GetData;
var i : integer;
    j : integer;
    b : boolean;
    Item : TElSideBarItem;
    Section  : TElSideBarSection;
    ASection : TElSideBarSection;
begin
  SideBar.BeginUpdate;
  for i := 0 to SideBar.Sections.Count - 1 do    // Iterate
  begin
    Section := SideBar.Sections[i];
    Section.Enabled := not Section.Disabled;
    Section.Visible := not Section.Hidden;
    Section.Inactive := not Section.Active;
    for j := 0 to Section.Items.Count - 1 do    // Iterate
    begin
      Item := Section.Items[j];
      Item.Enabled := not Item.Disabled;
      Item.Visible := not Item.Hidden;
    end;    // for
  end;    // for
  SideBar.EndUpdate;
  OrigSideBar.SectionIndex := -1;
  OrigSideBar.BeginUpdate;
  i := 0;
  while i <= Pred(OrigSideBar.Sections.Count) do
  begin
    b := false;
    for j := 0 to Pred(SideBar.Sections.Count) do
    begin
      if SideBar.Sections[j].Data = OrigSideBar.Sections[i] then
      begin
        b := true;
        break;
      end;
    end;
    if not b then
    begin
      OrigSideBar.Sections[i].Free;
    end else inc(i);
  end;

  for i := 0 to Pred(SideBar.Sections.Count) do
  begin
    if (SideBar.Sections[i].Data = nil) then SideBar.Sections[i].Data := OrigSideBar.Sections.Add;
    TElSideBarSection(SideBar.Sections[i].Data).Index := i;
    TElSideBarSection(SideBar.Sections[i].Data).Assign(SideBar.Sections[i]);
  end;

  //OrigSideBar.Sections.Assign(SideBar.Sections);
  OrigSideBar.SectionIndex := SideBar.SectionIndex;
  OrigSideBar.ItemIndex := SideBar.ItemIndex;
  OrigSideBar.EndUpdate;

  for i := 0 to OrigSideBar.Sections.Count -1 do
  begin
    ASection := OrigSideBar.Sections[i];
    if i <> OrigSideBar.SectionIndex then
       if ASection.ContainsControls then ASection.Panel.Visible := false;
  end;
  if OrigSideBar.SectionIndex <> -1 then
  begin
    ASection := OrigSideBar.Sections[OrigSideBar.SectionIndex];
    if ASection.ContainsControls then
    begin
      ASection.Panel.Visible := false;
      OrigSideBar.UpdateChildControl;
    end;
  end;
  (*ASection := FSection;
  FSection := nil;
  if ASection <> nil then
    SectionIndex := ASection.Index
  else
    SectionIndex := -1;
  *)
end;

procedure TSideBarEditForm.SideBarSectionChange(Sender: TObject);
begin
  with SideBar do
  begin
    if SectionIndex <> -1 then
    begin
      FCurSection := Sections[SectionIndex];
      DeleteSectionBtn.Enabled := true;
      MoveUpSectionBtn.Enabled := SectionIndex > 0;
      MoveDownSectionBtn.Enabled := SectionIndex < FCurSection.Collection.Count - 1;
      VisibleSectionCB.Enabled := true;
      VisibleSectionCB.Checked := not FCurSection.Hidden;

      InactiveSectionCB.Enabled := true;
      InactiveSectionCB.Checked := not FCurSection.Active;

      EnabledSectionCB.Enabled := true;
      EnabledSectionCB.Checked := not FCurSection.Disabled;
      SectionCaptionEdit.Enabled := true;
      SectionCaptionEdit.Text := FCurSection.Caption;
      SectionImageSpin.Enabled := true;
      SectionImageSpin.Value := FCurSection.ImageIndex;
      SectionTagSpin.Enabled := true;
      SectionTagSpin.Value := FCurSection.Tag;
      SectionHintEdit.Enabled := true;
      SectionHintEdit.Text := FCurSection.Hint;
      AddItemBtn.Enabled := true;
      ContainsControlsCB.Checked := FCurSection.ContainsControls;
      ContainsControlsCB.Enabled := true;
    end else
    begin
      FCurSection := nil;
      DeleteSectionBtn.Enabled := false;
      MoveUpSectionBtn.Enabled := false;
      MoveDownSectionBtn.Enabled := false;
      VisibleSectionCB.Enabled := false;
      VisibleSectionCB.Checked := true;
      EnabledSectionCB.Enabled := false;
      EnabledSectionCB.Checked := true;
      InactiveSectionCB.Enabled := false;
      InactiveSectionCB.Checked := false;
      SectionCaptionEdit.Enabled := false;
      SectionCaptionEdit.Text := '';
      SectionImageSpin.Enabled := false;
      SectionImageSpin.Value := -1;
      SectionTagSpin.Enabled := false;
      SectionTagSpin.Value := -1;
      SectionHintEdit.Enabled := false;
      SectionHintEdit.Text := '';
      AddItemBtn.Enabled := false;
      ContainsControlsCB.Checked := false;
      ContainsControlsCB.Enabled := false;
    end;
  end;
  SideBarItemChange(SideBar);
end;

procedure TSideBarEditForm.SideBarItemChange(Sender: TObject);
begin
  with SideBar do
  begin
    if ItemIndex <> -1 then
    begin
      FCurItem := Sections[SectionIndex].Items[ItemIndex];
      DeleteItemBtn.Enabled := true;
      MoveUpItemBtn.Enabled := ItemIndex > 0;
      MoveDownItemBtn.Enabled := ItemIndex < FCurItem.Collection.Count - 1;
      VisibleItemCB.Enabled := true;
      VisibleItemCB.Checked := not FCurItem.Hidden;
      EnabledItemCB.Enabled := true;
      EnabledItemCB.Checked := not FCurItem.Disabled;
      ItemCaptionEdit.Enabled := true;
      ItemCaptionEdit.Text := FCurItem.Caption;
      ItemImageSpin.Enabled := true;
      ItemImageSpin.Value := FCurItem.ImageIndex;
      ItemTagSpin.Enabled := true;
      ItemHintEdit.Enabled := true;
      ItemHintEdit.Text := FCurItem.Hint;
      ItemTagSpin.Value := FCurItem.Tag;
    end else
    begin
      FCurItem := nil;
      DeleteItemBtn.Enabled := false;
      MoveUpItemBtn.Enabled := false;
      MoveDownItemBtn.Enabled := false;
      VisibleItemCB.Enabled := false;
      VisibleItemCB.Checked := true;
      EnabledItemCB.Enabled := false;
      EnabledItemCB.Checked := true;
      ItemCaptionEdit.Enabled := false;
      ItemCaptionEdit.Text := '';
      ItemImageSpin.Enabled := false;
      ItemImageSpin.Value := -1;
      ItemTagSpin.Enabled := false;
      ItemTagSpin.Value := -1;
      ItemHintEdit.Enabled := false;
      ItemHintEdit.Text := '';
    end;
  end;
end;

procedure TSideBarEditForm.SectionCaptionEditChange(Sender: TObject);
begin
  if FCurSection <> nil then FCurSection.Caption := SectionCaptionEdit.Text;
end;

procedure TSideBarEditForm.SectionImageSpinChange(Sender: TObject);
begin
  if FCurSection <> nil then FCurSection.ImageIndex := SectionImageSpin.Value;
end;

procedure TSideBarEditForm.EnabledSectionCBClick(Sender: TObject);
begin
  if FCurSection <> nil then FCurSection.Disabled := not EnabledSectionCB.Checked;
end;

procedure TSideBarEditForm.VisibleSectionCBClick(Sender: TObject);
begin
  if FCurSection <> nil then FCurSection.Hidden := not VisibleSectionCB.Checked;
end;

procedure TSideBarEditForm.VisibleItemCBClick(Sender: TObject);
begin
  if FCurItem <> nil then FCurItem.Hidden := not VisibleItemCB.Checked;
end;

procedure TSideBarEditForm.EnabledItemCBClick(Sender: TObject);
begin
  if FCurItem <> nil then FCurItem.Disabled := not EnabledItemCB.Checked;
end;

procedure TSideBarEditForm.ItemCaptionEditChange(Sender: TObject);
begin
  if FCurItem <> nil then FCurItem.Caption := ItemCaptionEdit.Text;
end;

procedure TSideBarEditForm.ItemImageSpinChange(Sender: TObject);
begin
  if FCurItem <> nil then FCurItem.ImageIndex := ItemImageSpin.Value;
end;

procedure TSideBarEditForm.MoveUpItemBtnClick(Sender: TObject);
begin
  if FCurItem <> nil then
  if FCurItem.Index > 0 then
  begin
    FCurItem.Index := FCurItem.Index - 1;
    if SideBar.TopIndex = FCurItem.Index + 1 then SideBar.TopIndex := FCurItem.Index;
    MoveDownItemBtn.Enabled := FCurItem.Index < FCurItem.Collection.Count - 1;
    MoveUpItemBtn.Enabled := FCurItem.Index > 0;
  end;
end;

procedure TSideBarEditForm.MoveDownItemBtnClick(Sender: TObject);
begin
  if FCurItem <> nil then
  if FCurItem.Index < FCurItem.Collection.Count - 1 then
  begin
    FCurItem.Index := FCurItem.Index + 1;
    MoveDownItemBtn.Enabled := FCurItem.Index < FCurItem.Collection.Count - 1;
    MoveUpItemBtn.Enabled := FCurItem.Index > 0;
  end;
end;

procedure TSideBarEditForm.MoveUpSectionBtnClick(Sender: TObject);
begin
  if FCurSection <> nil then
  if FCurSection.Index > 0 then
  begin
    FCurSection.Index := FCurSection.Index - 1;
    MoveUpSectionBtn.Enabled := FCurSection.Index > 0;
    MoveDownSectionBtn.Enabled := FCurSection.Index < FCurSection.Collection.Count - 1;
  end;
end;

procedure TSideBarEditForm.MoveDownSectionBtnClick(Sender: TObject);
begin
  if FCurSection <> nil then
  if FCurSection.Index < FCurSection.Collection.Count - 1 then
  begin
    FCurSection.Index := FCurSection.Index + 1;
    MoveUpSectionBtn.Enabled := FCurSection.Index > 0;
    MoveDownSectionBtn.Enabled := FCurSection.Index < FCurSection.Collection.Count - 1;
  end;
end;

procedure TSideBarEditForm.AddSectionBtnClick(Sender: TObject);
begin
  SideBar.SectionIndex := SideBar.Sections.Add.Index;
  SideBar.Sections[SideBar.SectionIndex].Active := true;
  SideBarSectionChange(SideBar);
end;

procedure TSideBarEditForm.AddItemBtnClick(Sender: TObject);
begin
  SideBar.ItemIndex := FCurSection.Items.Add.Index;
  SideBar.TopIndex := SideBar.ItemIndex;
  SideBarItemChange(SideBar);
end;

procedure TSideBarEditForm.DeleteItemBtnClick(Sender: TObject);
begin
  if FCurItem <> nil then
  begin
    FCurItem.Free;
    SideBarItemChange(SideBar);
  end;
end;

procedure TSideBarEditForm.DeleteSectionBtnClick(Sender: TObject);
begin
  if FCurSection <> nil then
  begin
    FCurSection.Free;
    SideBarSectionChange(SideBar);
  end;
end;

procedure TSideBarEditForm.SectionTagSpinChange(Sender: TObject);
begin
  if FCurSection <> nil then FCurSection.Tag := SectionTagSpin.Value;
end;

procedure TSideBarEditForm.ItemTagSpinChange(Sender: TObject);
begin
  if FCurItem <> nil then FCurItem.Tag := ItemTagSpin.Value;
end;

procedure TSideBarEditForm.SectionHintEditChange(Sender: TObject);
begin
  if FCurSection <> nil then FCurSection.Hint := SectionHintEdit.Text;
end;

procedure TSideBarEditForm.ItemHintEditChange(Sender: TObject);
begin
  if FCurItem <> nil then FCurItem.Hint := ItemHintEdit.Text;
end;

procedure TSideBarEditForm.ContainsControlsCBClick(Sender: TObject);
begin
  if FCurSection <> nil then FCurSection.ContainsControls := ContainsControlsCB.Checked;
end;

procedure TSideBarEditForm.InactiveSectionCBClick(Sender: TObject);
begin
  if FCurSection <> nil then FCurSection.Active := not InactiveSectionCB.Checked;
end;

end.

