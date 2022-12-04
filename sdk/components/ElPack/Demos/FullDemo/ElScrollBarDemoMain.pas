unit ElScrollBarDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElXPThemedControl, ElScrollBar, ElImgFrm, StdCtrls, ElACtrls, ElSpin,
  ElVCLUtils, ElBtnCtl, ElStrUtils, ElCheckCtl, ExtCtrls, ElPanel,
  ElGroupBox, ElClrCmb;

type
  TElScrollBarDemoMainForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    ElScrollBarMinValueSpin: TElSpinEdit;
    ElScrollBarMaxValueSpin: TElSpinEdit;
    ElScrollBarShowTrackHintCB: TElCheckBox;
    ElScrollBarOrientationCombo: TElAdvancedComboBox;
    Label3: TLabel;
    ElScrollBarFlatCB: TElCheckBox;
    ElScrollBarActiveFlatCB: TElCheckBox;
    ElScrollBarSecondaryButtonsCombo: TElAdvancedComboBox;
    ElScrollbarSecondaryButtonsCB: TElCheckBox;
    ElScrollBarDrawFramesCB: TElCheckBox;
    ElScrollbarDrawArrowFramesCB: TElCheckBox;
    ElScrollBarDrawBarsCB: TElCheckBox;
    ElScrollBarNoDisableButtonsCB: TElCheckBox;
    ElScrollBarNoThunkenThumbCB: TElCheckBox;
    ElScrollBarShowLeftArrowsCB: TElCheckBox;
    ElScrollBarShowRightArrowsCB: TElCheckBox;
    ElScrollBarThinFramesCB: TElCheckBox;
    ElScrollBarManualThumbModeCB: TElCheckBox;
    ElScrollBarThumbSizeSpin: TElSpinEdit;
    ElScrollBarUseImageFormCB: TElCheckBox;
    ElScrollBarImageForm: TElImageForm;
    SamplePanel: TElPanel;
    SampleElScrollBar: TElScrollBar;
    ElScrollBarUseSystemMetricsCB: TElCheckBox;
    ElScrollBarButtonSizeSpin: TElSpinEdit;
    Label4: TLabel;
    ElScrollBarColorsGroupBox: TElGroupBox;
    Label5: TLabel;
    ElScrollBarColorCombo: TElColorCombo;
    ElScrollBarArrowColorCombo: TElColorCombo;
    Label6: TLabel;
    Label7: TLabel;
    ElScrollBarHotArrowColorCombo: TElColorCombo;
    Label8: TLabel;
    ElScrollBarBarColorCombo: TElColorCombo;
    ElScrollBarUseXPThemesCB: TElCheckBox;
    procedure SampleElScrollBarScrollHintNeeded(Sender: TObject;
      TrackPosition: Integer; var Hint: TElFString);
    procedure ElScrollBarShowTrackHintCBClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ElScrollBarFlatCBClick(Sender: TObject);
    procedure ElScrollbarSecondaryButtonsCBClick(Sender: TObject);
    procedure ElScrollBarDrawFramesCBClick(Sender: TObject);
    procedure ElScrollbarDrawArrowFramesCBClick(Sender: TObject);
    procedure ElScrollBarNoDisableButtonsCBClick(Sender: TObject);
    procedure ElScrollBarNoThunkenThumbCBClick(Sender: TObject);
    procedure ElScrollBarShowLeftArrowsCBClick(Sender: TObject);
    procedure ElScrollBarDrawBarsCBClick(Sender: TObject);
    procedure ElScrollBarShowRightArrowsCBClick(Sender: TObject);
    procedure ElScrollBarActiveFlatCBClick(Sender: TObject);
    procedure ElScrollBarThinFramesCBClick(Sender: TObject);
    procedure ElScrollBarMinValueSpinChange(Sender: TObject);
    procedure ElScrollBarMaxValueSpinChange(Sender: TObject);
    procedure ElScrollBarManualThumbModeCBClick(Sender: TObject);
    procedure ElScrollBarThumbSizeSpinChange(Sender: TObject);
    procedure ElScrollBarUseImageFormCBClick(Sender: TObject);
    procedure ElScrollBarOrientationComboChange(Sender: TObject);
    procedure ElScrollBarSecondaryButtonsComboChange(Sender: TObject);
    procedure ElScrollBarUseSystemMetricsCBClick(Sender: TObject);
    procedure ElScrollBarColorComboChange(Sender: TObject);
    procedure ElScrollBarArrowColorComboChange(Sender: TObject);
    procedure ElScrollBarBarColorComboChange(Sender: TObject);
    procedure ElScrollBarHotArrowColorComboChange(Sender: TObject);
    procedure ElScrollBarUseXPThemesCBClick(Sender: TObject);
  private
    NotFirstTime : boolean;
  public
    { Public declarations }
  end;

var
  ElScrollBarDemoMainForm: TElScrollBarDemoMainForm;

implementation

{$R *.DFM}

procedure TElScrollBarDemoMainForm.SampleElScrollBarScrollHintNeeded(Sender: TObject;
  TrackPosition: Integer; var Hint: TElFString);
begin
  Hint := '(custom hint)'#13#10' Position: ' + IntToStr(TrackPosition); 
end;

procedure TElScrollBarDemoMainForm.ElScrollBarShowTrackHintCBClick(Sender: TObject);
begin
  sampleElScrollBar.ShowTrackHint := ElScrollBarShowTrackHintCB.Checked;
end;

procedure TElScrollBarDemoMainForm.FormActivate(Sender: TObject);
begin
  if not NotFirstTime then
  begin
    ElScrollBarOrientationCombo.ItemIndex := 0;
    ElScrollBarSecondaryButtonsCombo.ItemIndex := 0;
  end;
  NotFirstTime := true;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarFlatCBClick(Sender: TObject);
begin
  sampleElScrollbar.Flat := ElScrollBarFlatCB.Checked;
  ElScrollBarActiveFlatCB.Enabled := ElScrollBarFlatCB.Checked;
  ElScrollBarthinFramesCB.Enabled := (not ElScrollBarFlatCB.Checked) and (ElScrollBarDrawFramesCB.Checked); 
end;

procedure TElScrollBarDemoMainForm.ElScrollbarSecondaryButtonsCBClick(Sender: TObject);
begin
  sampleElScrollBar.SecondaryButtons := ElScrollbarSecondaryButtonsCB.Checked;
  ElScrollbarSecondaryButtonsCombo.Enabled := ElScrollbarSecondaryButtonsCB.Checked;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarDrawFramesCBClick(Sender: TObject);
begin
  SampleElScrollBar.DrawFrames := ElScrollBarDrawFramesCB.Checked;
  ElScrollBarDrawArrowFramesCB.Checked := SampleElScrollBar.DrawArrowFrames;
  ElScrollBarFlatCB.Enabled := SampleElScrollBar.DrawFrames; 
  ElScrollBarThinFramesCB.Enabled := (not ElScrollBarFlatCB.Checked) and
                                     (ElScrollBarDrawFramesCB.Checked or
                                      ElScrollBarDrawArrowFramesCB.Checked);
end;

procedure TElScrollBarDemoMainForm.ElScrollbarDrawArrowFramesCBClick(
  Sender: TObject);
begin
  SampleElScrollBar.DrawArrowFrames := ElScrollBarDrawArrowFramesCB.Checked;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarNoDisableButtonsCBClick(
  Sender: TObject);
begin
  sampleElScrollBar.NoDisableButtons := ElScrollBarNoDisableButtonsCB.Checked;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarNoThunkenThumbCBClick(
  Sender: TObject);
begin
  SampleElScrollBar.NoSunkenThumb := ElScrollBarNoThunkenThumbCB.Checked;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarShowLeftArrowsCBClick(
  Sender: TObject);
begin
  sampleElScrollBar.ShowLeftArrows := ElScrollBarShowLeftArrowsCB.Checked;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarDrawBarsCBClick(
  Sender: TObject);
begin
  sampleElScrollBar.DrawBars := ElScrollBarDrawBarsCB.Checked;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarShowRightArrowsCBClick(
  Sender: TObject);
begin
  sampleElScrollBar.ShowRightArrows := ElScrollBarShowRightArrowsCB.Checked;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarActiveFlatCBClick(
  Sender: TObject);
begin
  sampleElScrollBar.ActiveFlat := ElScrollBarActiveFlatCB.Checked;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarThinFramesCBClick(
  Sender: TObject);
begin
  sampleElScrollBar.ThinFrames := ElScrollBarthinFramesCB.Checked;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarMinValueSpinChange(
  Sender: TObject);
begin
  SampleElScrollBar.Min := ElScrollBarMinValueSpin.Value;
  ElScrollBarMinValueSpin.Value := SampleElScrollBar.Min;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarMaxValueSpinChange(
  Sender: TObject);
begin
  SampleElScrollBar.Max := ElScrollBarMaxValueSpin.Value;
  ElScrollBarMaxValueSpin.Value := SampleElScrollBar.Max;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarManualThumbModeCBClick(
  Sender: TObject);
begin
  if ElScrollBarManualThumbModeCB.Checked then
  begin
    SampleElScrollBar.ThumbMode := etmFixed;
    ElScrollBarThumbSizeSpin.Value := SampleElScrollBar.ThumbSize;
    ElScrollBarThumbSizeSpin.Enabled := true;
  end
  else
  begin
    SampleElScrollBar.ThumbMode := etmAuto;
    ElScrollBarThumbSizeSpin.Enabled := false;
  end;

end;

procedure TElScrollBarDemoMainForm.ElScrollBarThumbSizeSpinChange(
  Sender: TObject);
begin
  SampleElScrollBar.ThumbSize := ElScrollBarThumbSizeSpin.Value;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarUseImageFormCBClick(
  Sender: TObject);
begin
  if ElScrollBarUseImageFormCB.Checked then
  begin
    ElScrollBarImageForm.BackgroundType := bgtTileBitmap;
    sampleElScrollBar.ImageForm := ElScrollBarImageForm;
  end
  else
  begin
    sampleElScrollBar.ImageForm := nil;
    ElScrollBarImageForm.BackgroundType := bgtColorFill;
  end;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarOrientationComboChange(
  Sender: TObject);
begin
  sampleElScrollBar.Kind := TScrollBarKind(ElScrollBarOrientationCombo.ItemIndex);
  if sampleElScrollBar.Kind = sbHorizontal then
    samplePanel.SetBounds(4, 4, 121, 21)
  else
    samplePanel.SetBounds(4, 4, 21, 121);
end;

procedure TElScrollBarDemoMainForm.ElScrollBarSecondaryButtonsComboChange(
  Sender: TObject);
begin
  SampleElScrollBar.SecondBtnKind := TElSecButtonsKind(ElScrollBarSecondaryButtonsCombo.ItemIndex);
end;

procedure TElScrollBarDemoMainForm.ElScrollBarUseSystemMetricsCBClick(
  Sender: TObject);
begin
  SampleElScrollBar.UseSystemMetrics := ElScrollBarUseSystemMetricsCB.Checked;
  ElScrollBarManualThumbModeCB.Enabled := not SampleElScrollBar.UseSystemMetrics;
  ElScrollBarThumbSizeSpin.Enabled := ElScrollBarManualThumbModeCB.Enabled and
                                      ElScrollBarManualThumbModeCB.Checked;
  label4.Enabled := ElScrollBarThumbSizeSpin.Enabled;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarColorComboChange(Sender: TObject);
begin
  SampleElScrollBar.Color := ElScrollBarColorCombo.SelectedColor;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarArrowColorComboChange(
  Sender: TObject);
begin
  sampleElScrollBar.ArrowColor := ElScrollBarArrowColorCombo.SelectedColor;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarBarColorComboChange(
  Sender: TObject);
begin
  sampleElScrollBar.BarColor := ElScrollBarBarColorCombo.SelectedColor;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarHotArrowColorComboChange(Sender: TObject);
begin
  sampleElScrollBar.ArrowHotTrackColor := ElScrollBarHotArrowColorCombo.SelectedColor;
end;

procedure TElScrollBarDemoMainForm.ElScrollBarUseXPThemesCBClick(
  Sender: TObject);
begin
  SampleElScrollBar.UseXPThemes := ElScrollBarUseXPThemesCB.Checked;
end;

end.

