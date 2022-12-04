unit ElCheckGroupDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElImgFrm, StdCtrls, ElACtrls, ElBtnEdit, ElXPThemedControl, ElBtnCtl,
  ElCheckCtl, ElVCLUtils, ElSpin, ElImgLst, ExtCtrls, ElPanel,
  ElGroupBox, ElCheckItemGrp, ElClrCmb, ImgList;

type
  TElCheckGroupDemoMainForm = class(TForm)
    Label8: TLabel;
    ElCheckGroupTextButtonEdit: TElButtonEdit;
    ElCheckGroupImageForm: TElImageForm;
    ElCheckGroupUseImageFormCB: TElCheckBox;
    ElCheckGroupTextHTMLCB: TElCheckBox;
    ElCheckGroupBordersCheckGroup: TElCheckGroup;
    ElCheckGroupItemsEdit: TElButtonEdit;
    Label1: TLabel;
    Label6: TLabel;
    ElCheckGroupImageTypeCombo: TElAdvancedComboBox;
    CheckGroupGlyphs: TElImageList;
    Label2: TLabel;
    ElCheckGroupColumnsSpinEdit: TElSpinEdit;
    sampleElCheckGroup: TElCheckGroup;
    ElCheckGroupShowCheckBoxCB: TElCheckBox;
    ElCheckGroupShowFocusCB: TElCheckBox;
    Label3: TLabel;
    ElCheckGroupCaptionColorCombo: TElColorCombo;
    procedure ElCheckGroupTextButtonEditButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ElCheckGroupTextHTMLCBClick(Sender: TObject);
    procedure ElCheckGroupUseImageFormCBClick(Sender: TObject);
    procedure ElCheckGroupBordersCheckGroupClick(Sender: TObject);
    procedure ElCheckGroupItemsEditButtonClick(Sender: TObject);
    procedure ElCheckGroupImageTypeComboChange(Sender: TObject);
    procedure ElCheckGroupColumnsSpinEditChange(Sender: TObject);
    procedure ElCheckGroupCaptionColorComboChange(Sender: TObject);
  private
    NotFirstTime : boolean;
  public
    { Public declarations }
  end;

var
  ElCheckGroupDemoMainForm: TElCheckGroupDemoMainForm;

implementation

{$R *.DFM}

procedure TElCheckGroupDemoMainForm.ElCheckGroupTextButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElCheckGroup.Caption := ElCheckGroupTextButtonEdit.Text;
end;

procedure TElCheckGroupDemoMainForm.FormActivate(Sender: TObject);
var i : integer;
begin
  if not NotFirstTime then
  begin
    for i := 0 to 3 do
      ElCheckGroupBordersCheckGroup.Checked[i] := true;
    ElCheckGroupImageTypeCombo.ItemIndex := 0;
  end;
  NotFirstTime := true;
end;

procedure TElCheckGroupDemoMainForm.ElCheckGroupTextHTMLCBClick(
  Sender: TObject);
begin
  SampleElCheckGroup.IsHTML := ElCheckGroupTextHTMLCB.Checked;
end;

procedure TElCheckGroupDemoMainForm.ElCheckGroupUseImageFormCBClick(
  Sender: TObject);
begin
  if ElCheckGroupUseImageFormCB.Checked then
    ElCheckGroupImageForm.Backgroundtype := bgtTileBitmap
  else
    ElCheckGroupImageForm.Backgroundtype := bgtColorFill;
end;

procedure TElCheckGroupDemoMainForm.ElCheckGroupBordersCheckGroupClick(
  Sender: TObject);
begin
  if ElCheckGroupBordersCheckGroup.Checked[0] then
    sampleElCheckGroup.BorderSides := sampleElCheckGroup.BorderSides + [ebsLeft]
  else
    sampleElCheckGroup.BorderSides := sampleElCheckGroup.BorderSides - [ebsLeft];
  if ElCheckGroupBordersCheckGroup.Checked[1] then
    sampleElCheckGroup.BorderSides := sampleElCheckGroup.BorderSides + [ebsRight]
  else
    sampleElCheckGroup.BorderSides := sampleElCheckGroup.BorderSides - [ebsRight];
  if ElCheckGroupBordersCheckGroup.Checked[2] then
    sampleElCheckGroup.BorderSides := sampleElCheckGroup.BorderSides + [ebsTop]
  else
    sampleElCheckGroup.BorderSides := sampleElCheckGroup.BorderSides - [ebsTop];
  if ElCheckGroupBordersCheckGroup.Checked[3] then
    sampleElCheckGroup.BorderSides := sampleElCheckGroup.BorderSides + [ebsBottom]
  else
    sampleElCheckGroup.BorderSides := sampleElCheckGroup.BorderSides - [ebsBottom];
end;

procedure TElCheckGroupDemoMainForm.ElCheckGroupItemsEditButtonClick(
  Sender: TObject);
begin
  SampleElCheckGroup.Items.Text := ElCheckGroupItemsEdit.Text;
end;

procedure TElCheckGroupDemoMainForm.ElCheckGroupImageTypeComboChange(
  Sender: TObject);
begin
  case ElCheckGroupImageTypeCombo.ItemIndex of
    0: begin
         sampleElCheckGroup.UseCustomGlyphs := false;
         sampleElCheckGroup.UseImageList := false;
       end;
    1: begin
         sampleElCheckGroup.UseCustomGlyphs := true;
         sampleElCheckGroup.UseImageList := false;
       end;
    2: begin
         sampleElCheckGroup.UseCustomGlyphs := false;
         sampleElCheckGroup.UseImageList := true;
       end;
  end;
end;

procedure TElCheckGroupDemoMainForm.ElCheckGroupColumnsSpinEditChange(
  Sender: TObject);
begin
  sampleElCheckGroup.Columns := ElCheckGroupColumnsSpinEdit.Value;
end;

procedure TElCheckGroupDemoMainForm.ElCheckGroupCaptionColorComboChange(
  Sender: TObject);
begin
  sampleElCheckGroup.CaptionColor := ElCheckGroupCaptionColorCombo.SelectedColor;
end;

end.

