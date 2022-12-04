unit ElCheckBoxDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElImgFrm, StdCtrls, ElACtrls, ElBtnEdit, ElXPThemedControl, ElBtnCtl,
  ElCheckCtl, ElVCLUtils, ElSpin, ElImgLst, ImgList;

type
  TElCheckBoxDemoMainForm = class(TForm)
    Label9: TLabel;
    ElCheckBoxTextTypeCombo: TElAdvancedComboBox;
    Label8: TLabel;
    ElCheckBoxTextButtonEdit: TElButtonEdit;
    ElCheckBoxImageForm: TElImageForm;
    sampleElCheckBox: TElCheckBox;
    Label6: TLabel;
    ElCheckBoxImageTypeCombo: TElAdvancedComboBox;
    ElCheckBoxUseImageFormCB: TElCheckBox;
    ElCheckboxFlatCB: TElCheckBox;
    ElCheckBoxTextHTMLCB: TElCheckBox;
    ElCheckBoxAutosizeCB: TElCheckBox;
    CheckBoxGlyphs: TElImageList;
    Label2: TLabel;
    ElCheckBoxAlignmentCombo: TElAdvancedComboBox;
    ElCheckBoxUseXPThemesCB: TElCheckBox;
    procedure ElCheckBoxTextButtonEditButtonClick(Sender: TObject);
    procedure ElCheckBoxTextTypeComboChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ElCheckBoxMultilineCBClick(Sender: TObject);
    procedure ElCheckBoxTextHTMLCBClick(Sender: TObject);
    procedure ElCheckBoxImageTypeComboChange(Sender: TObject);
    procedure ElCheckBoxAutosizeCBClick(Sender: TObject);
    procedure ElCheckBoxUseImageFormCBClick(Sender: TObject);
    procedure ElCheckBoxAlignmentComboChange(Sender: TObject);
    procedure ElCheckboxFlatCBClick(Sender: TObject);
    procedure ElCheckBoxUseXPThemesCBClick(Sender: TObject);
  private
    NotFirstTime : boolean;
  public
    { Public declarations }
  end;

var
  ElCheckBoxDemoMainForm: TElCheckBoxDemoMainForm;

implementation

{$R *.DFM}

procedure TElCheckBoxDemoMainForm.ElCheckBoxTextButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElCheckBox.Caption := ElCheckBoxTextButtonEdit.Text;
end;

procedure TElCheckBoxDemoMainForm.ElCheckBoxTextTypeComboChange(
  Sender: TObject);
begin
  sampleElCheckBox.TextDrawType := TElTextDrawType(ElCheckBoxTextTypeCombo.ItemIndex);
end;

procedure TElCheckBoxDemoMainForm.FormActivate(Sender: TObject);
begin
  if not NotFirstTime then
  begin
    ElCheckBoxAlignmentCombo.ItemIndex := 1;
    ElCheckBoxImageTypeCombo.ItemIndex := 0;
    ElCheckBoxTextTypeCombo.ItemIndex  := 0;
  end;
  NotFirstTime := true;
end;

procedure TElCheckBoxDemoMainForm.ElCheckBoxMultilineCBClick(
  Sender: TObject);
begin
  // SampleElCheckBox.Multiline := ElCheckBoxMultilineCB.Checked;
end;

procedure TElCheckBoxDemoMainForm.ElCheckBoxTextHTMLCBClick(
  Sender: TObject);
begin
  SampleElCheckbox.IsHTML := ElCheckBoxTextHTMLCB.Checked;
end;

procedure TElCheckBoxDemoMainForm.ElCheckBoxImageTypeComboChange(
  Sender: TObject);
begin
  case ElCheckBoxImageTypeCombo.ItemIndex of
    0: begin
         sampleElCheckBox.UseCustomGlyphs := false;
       end;
    1: begin
         sampleElCheckBox.UseCustomGlyphs := true;
         sampleElCheckBox.UseImageList := false;
       end;
    2: begin
         sampleElCheckBox.UseCustomGlyphs := false;
         sampleElCheckBox.UseImageList := true;
       end;
  end;
end;

procedure TElCheckBoxDemoMainForm.ElCheckBoxAutosizeCBClick(
  Sender: TObject);
begin
  sampleElCheckBox.AutoSize := ElCheckBoxAutosizeCB.Checked;
end;

procedure TElCheckBoxDemoMainForm.ElCheckBoxUseImageFormCBClick(
  Sender: TObject);
begin
  if ElCheckBoxUseImageFormCB.Checked then
    ElCheckBoxImageForm.Backgroundtype := bgtTileBitmap
  else
    ElCheckBoxImageForm.Backgroundtype := bgtColorFill;
end;

procedure TElCheckBoxDemoMainForm.ElCheckBoxAlignmentComboChange(
  Sender: TObject);
begin
  sampleElCheckBox.Alignment := TLeftRight(ElCheckBoxAlignmentCombo.ItemIndex);
end;

procedure TElCheckBoxDemoMainForm.ElCheckboxFlatCBClick(Sender: TObject);
begin
  SampleElCheckBox.Flat := ElCheckboxFlatCB.Checked;
end;

procedure TElCheckBoxDemoMainForm.ElCheckBoxUseXPThemesCBClick(Sender: TObject);
begin
  SampleElCheckBox.UseXPThemes := ElCheckBoxUseXPThemesCB.Checked; 
end;

end.

