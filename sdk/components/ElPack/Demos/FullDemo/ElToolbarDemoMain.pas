unit ElToolbarDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ElPanel, ElToolBar, ElPopBtn, ElBtnCtl, ElCheckCtl, StdCtrls,
  ElACtrls, ElClrCmb, Buttons, ElImgFrm, ElVCLUtils, ElSpin,
  ElImgLst, ElXPThemedControl, ImgList;

type
  TElToolbarDemoMainForm = class(TForm)
    SampleElToolbar: TElToolBar;
    ElToolbarImageForm: TElImageForm;
    SmallImages: TElImageList;
    LargeImages: TElImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ElToolbarAddButtonButton: TElPopupButton;
    ElToolbarAddSeparatorButton: TElPopupButton;
    ElToolbarAddDividerButton: TElPopupButton;
    ElToolbarClearButton: TElPopupButton;
    ElToolbarLargeButtonsCB: TElCheckBox;
    ElToolbarFlatCB: TElCheckBox;
    ElToolbarButtonColorCombo: TElColorCombo;
    ElToolbarGlyphLayoutCombo: TElAdvancedComboBox;
    ElToolbarAlignCombo: TElAdvancedComboBox;
    ElToolbarUseBackgroundCB: TElCheckBox;
    ElToolbarTransparentButtonsCB: TElCheckBox;
    EltoolbarThinButtonsCB: TElCheckBox;
    ElToolbarUseImageFormCB: TElCheckBox;
    ElToolbarAutosizeCB: TElCheckBox;
    ElToolbarResizableCB: TElCheckBox;
    ElToolbarAutoWrapCB: TElCheckBox;
    ElToolbarShowGlyphsCB: TElCheckBox;
    ElToolbarShowTextCB: TElCheckBox;
    ElToolbarSmallButtonWidthSpinEdit: TElSpinEdit;
    ElToolbarSmallButtonHeightSpinEdit: TElSpinEdit;
    ElToolbarLargeButtonWidthSpinEdit: TElSpinEdit;
    ElToolbarLargeButtonHeightSpinEdit: TElSpinEdit;
    procedure FormActivate(Sender: TObject);
    procedure ElToolbarAddButtonButtonClick(Sender: TObject);
    procedure ElToolbarAddSeparatorButtonClick(Sender: TObject);
    procedure ElToolbarAddDividerButtonClick(Sender: TObject);
    procedure ElToolbarClearButtonClick(Sender: TObject);
    procedure ElToolbarLargeButtonsCBClick(Sender: TObject);
    procedure ElToolbarFlatCBClick(Sender: TObject);
    procedure ElToolbarGlyphLayoutComboChange(Sender: TObject);
    procedure ElToolbarAlignComboChange(Sender: TObject);
    procedure ElToolbarUseBackgroundCBClick(Sender: TObject);
    procedure ElToolbarButtonColorComboChange(Sender: TObject);
    procedure ElToolbarTransparentButtonsCBClick(Sender: TObject);
    procedure EltoolbarThinButtonsCBClick(Sender: TObject);
    procedure ElToolbarUseImageFormCBClick(Sender: TObject);
    procedure ElToolbarAutosizeCBClick(Sender: TObject);
    procedure ElToolbarResizableCBClick(Sender: TObject);
    procedure ElToolbarAutoWrapCBClick(Sender: TObject);
    procedure ElToolbarShowGlyphsCBClick(Sender: TObject);
    procedure ElToolbarShowTextCBClick(Sender: TObject);
    procedure ElToolbarSmallButtonWidthSpinEditChange(Sender: TObject);
    procedure ElToolbarSmallButtonHeightSpinEditChange(Sender: TObject);
    procedure ElToolbarLargeButtonWidthSpinEditChange(Sender: TObject);
    procedure ElToolbarLargeButtonHeightSpinEditChange(Sender: TObject);
  private
    { Private declarations }
  public
    NotFirstTime : boolean;
  end;

var
  ElToolbarDemoMainForm: TElToolbarDemoMainForm;

implementation

{$R *.DFM}

procedure TElToolbarDemoMainForm.FormActivate(Sender: TObject);
begin
  if not NotFirstTime then
  begin
    ElToolbarGlyphLayoutCombo.ItemIndex := 0;
    ElToolbarAlignCombo.ItemIndex := 2;
  end;
  NotFirstTime := true;
end;

procedure TElToolbarDemoMainForm.ElToolbarAddButtonButtonClick(
  Sender: TObject);
begin
  with SampleElToolbar.AddButton(ebtButton) do
  begin
    Caption := 'Button ' + IntToStr(SampleElToolbar.ControlCount - 1);
    ImageIndex := SampleElToolbar.ControlCount - 1;
  end;
end;

procedure TElToolbarDemoMainForm.ElToolbarAddSeparatorButtonClick(
  Sender: TObject);
begin
  SampleElToolbar.AddButton(ebtSeparator);
end;

procedure TElToolbarDemoMainForm.ElToolbarAddDividerButtonClick(
  Sender: TObject);
begin
  SampleElToolbar.AddButton(ebtDivider);
end;

procedure TElToolbarDemoMainForm.ElToolbarClearButtonClick(
  Sender: TObject);
var AControl : TControl;
begin
  while sampleEltoolbar.ControlCount > 0 do
  begin
    AControl := sampleEltoolbar.Controls[0];
    AControl.Free;
  end;
end;

procedure TElToolbarDemoMainForm.ElToolbarLargeButtonsCBClick(
  Sender: TObject);
begin
  sampleElToolbar.LargeSize := ElToolbarLargeButtonsCB.Checked;
  if sampleElToolbar.LargeSize then
    sampleElToolbar.Images := LargeImages
  else
    sampleElToolbar.Images := SmallImages;
end;

procedure TElToolbarDemoMainForm.ElToolbarFlatCBClick(Sender: TObject);
begin
  sampleElToolbar.Flat := ElToolbarFlatCB.Checked;
end;

procedure TElToolbarDemoMainForm.ElToolbarGlyphLayoutComboChange(
  Sender: TObject);
begin
  sampleElToolbar.GlyphLayout := TButtonLayout(ElToolbarGlyphLayoutCombo.ItemIndex);
end;

procedure TElToolbarDemoMainForm.ElToolbarAlignComboChange(
  Sender: TObject);
begin
  case ElToolbarAlignCombo.ItemIndex of
    0: sampleElToolbar.Align := alLeft;
    1: sampleElToolbar.Align := alRight;
    2: sampleElToolbar.Align := alTop;
    3: sampleElToolbar.Align := alBottom;
  end;
end;

procedure TElToolbarDemoMainForm.ElToolbarUseBackgroundCBClick(
  Sender: TObject);
begin
  if ElToolbarUseBackgroundCB.Checked then
  begin
    sampleElToolbar.ImageForm := nil;
    sampleElToolbar.ButtonImageForm := nil;
    sampleElToolbar.BackgroundType := bgtTileBitmap;
  end
  else
  begin
    if ElToolbarUseImageFormCB.Checked then
    begin
      sampleElToolbar.ImageForm := ElToolbarImageForm;
      sampleElToolbar.ButtonImageForm := ElToolbarImageForm;
    end;
    sampleElToolbar.BackgroundType := bgtColorFill;
  end;
end;

procedure TElToolbarDemoMainForm.ElToolbarButtonColorComboChange(
  Sender: TObject);
begin
  sampleElToolbar.ButtonColor := ElToolbarButtonColorCombo.SelectedColor;
end;

procedure TElToolbarDemoMainForm.ElToolbarTransparentButtonsCBClick(
  Sender: TObject);
begin
  sampleElToolbar.TransparentButtons := ElToolbarTransparentButtonsCB.Checked;
end;

procedure TElToolbarDemoMainForm.EltoolbarThinButtonsCBClick(
  Sender: TObject);
begin
  sampleElToolbar.ThinButtons := EltoolbarThinButtonsCB.Checked;
end;

procedure TElToolbarDemoMainForm.ElToolbarUseImageFormCBClick(
  Sender: TObject);
begin
  if ElToolbarUseImageFormCB.Checked then
  begin
    sampleElToolbar.ImageForm := ElToolbarImageForm;
    sampleElToolbar.ButtonImageForm := ElToolbarImageForm;
    ElToolbarImageForm.BackgroundType := bgtTileBitmap
  end
  else
  begin
    sampleElToolbar.ImageForm := nil;
    sampleElToolbar.ButtonImageForm := nil;
    ElToolbarImageForm.BackgroundType := bgtColorFill;
  end;
end;

procedure TElToolbarDemoMainForm.ElToolbarAutosizeCBClick(Sender: TObject);
begin
  sampleElToolbar.Autosize := ElToolbarAutosizeCB.Checked;
end;

procedure TElToolbarDemoMainForm.ElToolbarResizableCBClick(
  Sender: TObject);
begin
  sampleElToolbar.Resizable := ElToolbarResizableCB.Checked;
end;

procedure TElToolbarDemoMainForm.ElToolbarAutoWrapCBClick(Sender: TObject);
begin
  sampleElToolbar.AutoWrap := ElToolbarAutoWrapCB.Checked;
end;

procedure TElToolbarDemoMainForm.ElToolbarShowGlyphsCBClick(
  Sender: TObject);
begin
  sampleElToolbar.ShowGlyph := ElToolbarShowGlyphsCB.Checked;
end;

procedure TElToolbarDemoMainForm.ElToolbarShowTextCBClick(Sender: TObject);
begin
  sampleElToolbar.ShowCaption := ElToolbarShowTextCB.Checked;
end;

procedure TElToolbarDemoMainForm.ElToolbarSmallButtonWidthSpinEditChange(
  Sender: TObject);
begin
  sampleElToolbar.BtnWidth := ElToolbarSmallButtonWidthSpinEdit.Value;
end;

procedure TElToolbarDemoMainForm.ElToolbarSmallButtonHeightSpinEditChange(
  Sender: TObject);
begin
  sampleElToolbar.BtnHeight := ElToolbarSmallButtonHeightSpinEdit.Value;
end;

procedure TElToolbarDemoMainForm.ElToolbarLargeButtonWidthSpinEditChange(
  Sender: TObject);
begin
  sampleElToolbar.LargeBtnWidth := ElToolbarLargeButtonWidthSpinEdit.Value;
end;

procedure TElToolbarDemoMainForm.ElToolbarLargeButtonHeightSpinEditChange(
  Sender: TObject);
begin
  sampleElToolbar.LargeBtnHeight := ElToolbarLargeButtonHeightSpinEdit.Value;
end;

end.
