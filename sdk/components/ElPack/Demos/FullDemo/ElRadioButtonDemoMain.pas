unit ElRadioButtonDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElImgFrm, StdCtrls, ElACtrls, ElBtnEdit, ElXPThemedControl, ElBtnCtl,
  ElCheckCtl, ElVCLUtils, ElSpin, ElImgLst, ImgList;

type
  TElRadioButtonDemoMainForm = class(TForm)
    Label9: TLabel;
    ElRadioButtonTextTypeCombo: TElAdvancedComboBox;
    Label8: TLabel;
    ElRadioButtonTextButtonEdit: TElButtonEdit;
    ElRadioButtonImageForm: TElImageForm;
    Label6: TLabel;
    ElRadioButtonImageTypeCombo: TElAdvancedComboBox;
    ElRadioButtonUseImageFormCB: TElCheckBox;
    ElRadioButtonFlatCB: TElCheckBox;
    ElRadioButtonTextHTMLCB: TElCheckBox;
    ElRadioButtonAutosizeCB: TElCheckBox;
    ElRadioButtonGlyphs: TElImageList;
    Label2: TLabel;
    ElRadioButtonAlignmentCombo: TElAdvancedComboBox;
    sampleElRadioButton: TElRadioButton;
    ElRadioButtonUseXPThemesCB: TElCheckBox;
    procedure ElRadioButtonTextButtonEditButtonClick(Sender: TObject);
    procedure ElRadioButtonTextTypeComboChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ElRadioButtonTextHTMLCBClick(Sender: TObject);
    procedure ElRadioButtonImageTypeComboChange(Sender: TObject);
    procedure ElRadioButtonAutosizeCBClick(Sender: TObject);
    procedure ElRadioButtonUseImageFormCBClick(Sender: TObject);
    procedure ElRadioButtonAlignmentComboChange(Sender: TObject);
    procedure ElRadioButtonFlatCBClick(Sender: TObject);
    procedure ElRadioButtonUseXPThemesCBClick(Sender: TObject);
  private
    NotFirstTime : boolean;
  public
    { Public declarations }
  end;

var
  ElRadioButtonDemoMainForm: TElRadioButtonDemoMainForm;

implementation

{$R *.DFM}

procedure TElRadioButtonDemoMainForm.ElRadioButtonTextButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElRadioButton.Caption := ElRadioButtonTextButtonEdit.Text;
end;

procedure TElRadioButtonDemoMainForm.ElRadioButtonTextTypeComboChange(
  Sender: TObject);
begin
  sampleElRadioButton.TextDrawType := TElTextDrawType(ElRadioButtonTextTypeCombo.ItemIndex);
end;

procedure TElRadioButtonDemoMainForm.FormActivate(Sender: TObject);
begin
  if not NotFirstTime then
  begin
    ElRadioButtonAlignmentCombo.ItemIndex := 1;
    ElRadioButtonImageTypeCombo.ItemIndex := 0;
    ElRadioButtonTextTypeCombo.ItemIndex  := 0;
  end;
  NotFirstTime := true;
end;

procedure TElRadioButtonDemoMainForm.ElRadioButtonTextHTMLCBClick(
  Sender: TObject);
begin
  SampleElRadioButton.IsHTML := ElRadioButtonTextHTMLCB.Checked;
end;

procedure TElRadioButtonDemoMainForm.ElRadioButtonImageTypeComboChange(
  Sender: TObject);
begin
  case ElRadioButtonImageTypeCombo.ItemIndex of
    0: begin
         sampleElRadioButton.UseCustomGlyphs := false;
       end;
    1: begin
         sampleElRadioButton.UseCustomGlyphs := true;
         sampleElRadioButton.UseImageList := false;
       end;
    2: begin
         sampleElRadioButton.UseCustomGlyphs := false;
         sampleElRadioButton.UseImageList := true;
       end;
  end;
end;

procedure TElRadioButtonDemoMainForm.ElRadioButtonAutosizeCBClick(
  Sender: TObject);
begin
  sampleElRadioButton.AutoSize := ElRadioButtonAutosizeCB.Checked;
end;

procedure TElRadioButtonDemoMainForm.ElRadioButtonUseImageFormCBClick(
  Sender: TObject);
begin
  if ElRadioButtonUseImageFormCB.Checked then
    ElRadioButtonImageForm.Backgroundtype := bgtTileBitmap
  else
    ElRadioButtonImageForm.Backgroundtype := bgtColorFill;
end;

procedure TElRadioButtonDemoMainForm.ElRadioButtonAlignmentComboChange(
  Sender: TObject);
begin
  sampleElRadioButton.Alignment := TLeftRight(ElRadioButtonAlignmentCombo.ItemIndex);
end;

procedure TElRadioButtonDemoMainForm.ElRadioButtonFlatCBClick(Sender: TObject);
begin
  SampleElRadioButton.Flat := ElRadioButtonFlatCB.Checked;
end;

procedure TElRadioButtonDemoMainForm.ElRadioButtonUseXPThemesCBClick(
  Sender: TObject);
begin
  SampleElRadioButton.UseXPThemes := ElRadioButtonUseXPThemesCB.Checked;
end;

end.

