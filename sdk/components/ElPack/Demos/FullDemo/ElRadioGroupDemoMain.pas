unit ElRadioGroupDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElImgFrm, StdCtrls, ElACtrls, ElBtnEdit, ElXPThemedControl, ElBtnCtl,
  ElCheckCtl, ElVCLUtils, ElSpin, ElImgLst, ExtCtrls, ElPanel,
  ElGroupBox, ElCheckItemGrp, ElClrCmb, ImgList;

type
  TElRadioGroupDemoMainForm = class(TForm)
    Label8: TLabel;
    ElRadioGroupTextButtonEdit: TElButtonEdit;
    ElRadioGroupImageForm: TElImageForm;
    ElRadioGroupUseImageFormCB: TElCheckBox;
    ElRadioGroupTextHTMLCB: TElCheckBox;
    ElRadioGroupBordersCheckGroup: TElCheckGroup;
    sampleElRadioGroup: TElRadioGroup;
    ElRadioGroupItemsEdit: TElButtonEdit;
    Label1: TLabel;
    Label6: TLabel;
    ElRadioGroupImageTypeCombo: TElAdvancedComboBox;
    RadioGroupGlyphs: TElImageList;
    Label2: TLabel;
    ElRadioGroupColumnsSpinEdit: TElSpinEdit;
    ElRadioGroupShowFocusCB: TElCheckBox;
    ElRadioGroupShowCheckBoxCB: TElCheckBox;
    Label3: TLabel;
    ElRadioGroupCaptionColorCombo: TElColorCombo;
    procedure ElRadioGroupTextButtonEditButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ElRadioGroupTextHTMLCBClick(Sender: TObject);
    procedure ElRadioGroupUseImageFormCBClick(Sender: TObject);
    procedure ElRadioGroupBordersCheckGroupClick(Sender: TObject);
    procedure ElRadioGroupItemsEditButtonClick(Sender: TObject);
    procedure ElRadioGroupImageTypeComboChange(Sender: TObject);
    procedure ElRadioGroupColumnsSpinEditChange(Sender: TObject);
    procedure ElRadioGroupShowCheckBoxCBClick(Sender: TObject);
    procedure ElRadioGroupShowFocusCBClick(Sender: TObject);
    procedure ElRadioGroupCaptionColorComboChange(Sender: TObject);
  private
    NotFirstTime : boolean;
  public
    { Public declarations }
  end;

var
  ElRadioGroupDemoMainForm: TElRadioGroupDemoMainForm;

implementation

{$R *.DFM}

procedure TElRadioGroupDemoMainForm.ElRadioGroupTextButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElRadioGroup.Caption := ElRadioGroupTextButtonEdit.Text;
end;

procedure TElRadioGroupDemoMainForm.FormActivate(Sender: TObject);
var i : integer;
begin
  if not NotFirstTime then
  begin
    for i := 0 to 3 do
      ElRadioGroupBordersCheckGroup.Checked[i] := true;
    ElRadioGroupImageTypeCombo.ItemIndex := 0;
  end;
  NotFirstTime := true;
end;

procedure TElRadioGroupDemoMainForm.ElRadioGroupTextHTMLCBClick(
  Sender: TObject);
begin
  SampleElRadioGroup.IsHTML := ElRadioGroupTextHTMLCB.Checked;
end;

procedure TElRadioGroupDemoMainForm.ElRadioGroupUseImageFormCBClick(
  Sender: TObject);
begin
  if ElRadioGroupUseImageFormCB.Checked then
    ElRadioGroupImageForm.Backgroundtype := bgtTileBitmap
  else
    ElRadioGroupImageForm.Backgroundtype := bgtColorFill;
end;

procedure TElRadioGroupDemoMainForm.ElRadioGroupBordersCheckGroupClick(
  Sender: TObject);
begin
  if ElRadioGroupBordersCheckGroup.Checked[0] then
    sampleElRadioGroup.BorderSides := sampleElRadioGroup.BorderSides + [ebsLeft]
  else
    sampleElRadioGroup.BorderSides := sampleElRadioGroup.BorderSides - [ebsLeft];
  if ElRadioGroupBordersCheckGroup.Checked[1] then
    sampleElRadioGroup.BorderSides := sampleElRadioGroup.BorderSides + [ebsRight]
  else
    sampleElRadioGroup.BorderSides := sampleElRadioGroup.BorderSides - [ebsRight];
  if ElRadioGroupBordersCheckGroup.Checked[2] then
    sampleElRadioGroup.BorderSides := sampleElRadioGroup.BorderSides + [ebsTop]
  else
    sampleElRadioGroup.BorderSides := sampleElRadioGroup.BorderSides - [ebsTop];
  if ElRadioGroupBordersCheckGroup.Checked[3] then
    sampleElRadioGroup.BorderSides := sampleElRadioGroup.BorderSides + [ebsBottom]
  else
    sampleElRadioGroup.BorderSides := sampleElRadioGroup.BorderSides - [ebsBottom];
end;

procedure TElRadioGroupDemoMainForm.ElRadioGroupItemsEditButtonClick(
  Sender: TObject);
begin
  SampleElRadioGroup.Items.Text := ElRadioGroupItemsEdit.Text;
end;

procedure TElRadioGroupDemoMainForm.ElRadioGroupImageTypeComboChange(
  Sender: TObject);
begin
  case ElRadioGroupImageTypeCombo.ItemIndex of
    0: begin
         sampleElRadioGroup.UseCustomGlyphs := false;
         sampleElRadioGroup.UseImageList := false;
       end;
    1: begin
         sampleElRadioGroup.UseCustomGlyphs := true;
         sampleElRadioGroup.UseImageList := false;
       end;
    2: begin
         sampleElRadioGroup.UseCustomGlyphs := false;
         sampleElRadioGroup.UseImageList := true;
       end;
  end;
end;

procedure TElRadioGroupDemoMainForm.ElRadioGroupColumnsSpinEditChange(
  Sender: TObject);
begin
  sampleElRadioGroup.Columns := ElRadioGroupColumnsSpinEdit.Value;
end;

procedure TElRadioGroupDemoMainForm.ElRadioGroupShowCheckBoxCBClick(
  Sender: TObject);
begin
  sampleElRadioGroup.ShowCheckBox := ElRadioGroupShowCheckBoxCB.Checked;
end;

procedure TElRadioGroupDemoMainForm.ElRadioGroupShowFocusCBClick(
  Sender: TObject);
begin
  sampleElRadioGroup.ShowFocus := ElRadioGroupShowFocusCB.Checked;
end;

procedure TElRadioGroupDemoMainForm.ElRadioGroupCaptionColorComboChange(
  Sender: TObject);
begin
  sampleElRadioGroup.CaptionColor := ElRadioGroupCaptionColorCombo.SelectedColor;
end;

end.

