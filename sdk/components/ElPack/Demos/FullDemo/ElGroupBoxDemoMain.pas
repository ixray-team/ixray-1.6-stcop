unit ElGroupBoxDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElImgFrm, StdCtrls, ElACtrls, ElBtnEdit, ElXPThemedControl, ElBtnCtl,
  ElCheckCtl, ElVCLUtils, ElSpin, ElImgLst, ExtCtrls, ElPanel,
  ElGroupBox, ElCheckItemGrp, ElClrCmb, ImgList;

type
  TElGroupBoxDemoMainForm = class(TForm)
    Label8: TLabel;
    ElGroupBoxTextButtonEdit: TElButtonEdit;
    ElGRoupBoxImageForm: TElImageForm;
    ElGroupBoxUseImageFormCB: TElCheckBox;
    ElGroupBoxTextHTMLCB: TElCheckBox;
    sampleElGroupBox: TElGroupBox;
    ElGroupBoxBordersCheckGroup: TElCheckGroup;
    ElGroupBoxShowCheckBoxCB: TElCheckBox;
    ElGroupBoxShowFocusCB: TElCheckBox;
    Label6: TLabel;
    ElGroupBoxImageTypeCombo: TElAdvancedComboBox;
    GroupBoxGlyphs: TElImageList;
    Label3: TLabel;
    ElGroupBoxCaptionColorCombo: TElColorCombo;
    procedure ElGroupBoxTextButtonEditButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ElGroupBoxTextHTMLCBClick(Sender: TObject);
    procedure ElGroupBoxUseImageFormCBClick(Sender: TObject);
    procedure ElGroupBoxBordersCheckGroupClick(Sender: TObject);
    procedure ElGroupBoxShowCheckBoxCBClick(Sender: TObject);
    procedure ElGroupBoxShowFocusCBClick(Sender: TObject);
    procedure ElGroupBoxImageTypeComboChange(Sender: TObject);
    procedure ElGroupBoxCaptionColorComboChange(Sender: TObject);
  private
    NotFirstTime : boolean;
  public
    { Public declarations }
  end;

var
  ElGroupBoxDemoMainForm: TElGroupBoxDemoMainForm;

implementation

{$R *.DFM}

procedure TElGroupBoxDemoMainForm.ElGroupBoxTextButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElGroupBox.Caption := ElGroupBoxTextButtonEdit.Text;
end;

procedure TElGroupBoxDemoMainForm.FormActivate(Sender: TObject);
var i : integer;
begin
  if not NotFirstTime then
  begin
    for i := 0 to 3 do
      ElGroupBoxBordersCheckGroup.Checked[i] := true;
  end;
  NotFirstTime := true;
end;

procedure TElGroupBoxDemoMainForm.ElGroupBoxTextHTMLCBClick(
  Sender: TObject);
begin
  SampleElGroupBox.IsHTML := ElGroupBoxTextHTMLCB.Checked;
end;

procedure TElGroupBoxDemoMainForm.ElGroupBoxUseImageFormCBClick(
  Sender: TObject);
begin
  if ElGroupBoxUseImageFormCB.Checked then
    ElGroupBoxImageForm.Backgroundtype := bgtTileBitmap
  else
    ElGroupBoxImageForm.Backgroundtype := bgtColorFill;
end;

procedure TElGroupBoxDemoMainForm.ElGroupBoxBordersCheckGroupClick(
  Sender: TObject);
begin
  if ElGroupBoxBordersCheckGroup.Checked[0] then
    sampleElGroupBox.BorderSides := sampleElGroupBox.BorderSides + [ebsLeft]
  else
    sampleElGroupBox.BorderSides := sampleElGroupBox.BorderSides - [ebsLeft];
  if ElGroupBoxBordersCheckGroup.Checked[1] then
    sampleElGroupBox.BorderSides := sampleElGroupBox.BorderSides + [ebsRight]
  else
    sampleElGroupBox.BorderSides := sampleElGroupBox.BorderSides - [ebsRight];
  if ElGroupBoxBordersCheckGroup.Checked[2] then
    sampleElGroupBox.BorderSides := sampleElGroupBox.BorderSides + [ebsTop]
  else
    sampleElGroupBox.BorderSides := sampleElGroupBox.BorderSides - [ebsTop];
  if ElGroupBoxBordersCheckGroup.Checked[3] then
    sampleElGroupBox.BorderSides := sampleElGroupBox.BorderSides + [ebsBottom]
  else
    sampleElGroupBox.BorderSides := sampleElGroupBox.BorderSides - [ebsBottom];
end;

procedure TElGroupBoxDemoMainForm.ElGroupBoxShowCheckBoxCBClick(
  Sender: TObject);
begin
  sampleElGroupBox.ShowCheckBox := ElGroupBoxShowCheckBoxCB.Checked;
end;

procedure TElGroupBoxDemoMainForm.ElGroupBoxShowFocusCBClick(
  Sender: TObject);
begin
  sampleElgroupBox.ShowFocus := ElGroupBoxShowFocusCB.Checked;
end;

procedure TElGroupBoxDemoMainForm.ElGroupBoxImageTypeComboChange(
  Sender: TObject);
begin
    case ElGroupBoxImageTypeCombo.ItemIndex of
    0: begin
         sampleElGroupBox.UseCustomGlyphs := false;
         sampleElGroupBox.UseImageList := false;
       end;
    1: begin
         sampleElGroupBox.UseCustomGlyphs := true;
         sampleElGroupBox.UseImageList := false;
       end;
    2: begin
         sampleElGroupBox.UseCustomGlyphs := false;
         sampleElGroupBox.UseImageList := true;
       end;
  end;
end;

procedure TElGroupBoxDemoMainForm.ElGroupBoxCaptionColorComboChange(
  Sender: TObject);
begin
  sampleElGroupBox.CaptionColor := ElGroupBoxCaptionColorCombo.SelectedColor;
end;

end.

