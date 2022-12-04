unit ElStatusBarDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElBtnCtl, ElCheckCtl, ExtCtrls, ElPanel, ElStatBar, StdCtrls, ElACtrls,
  ElBtnEdit, ElPopBtn, ElSpin, ElGroupBox, ElCombos, ElVCLUtils,
  ElCheckItemGrp, ElURLLabel, ElXPThemedControl;

type
  TElstatusBarDemoMainForm = class(TForm)
    sampleElStatusBar: TElStatusBar;
    ElStatusBarGeneralGB: TElGroupBox;
    Label1: TLabel;
    Image1: TImage;
    ElStatusBarSimplePanelCB: TElCheckBox;
    ElStatusBarSimpleTextHTMLCB: TElCheckBox;
    ElStatusBarSimpleTextEdit: TElButtonEdit;
    ElStatusBarCanResizePanelsCB: TElCheckBox;
    ElStatusBarShowGripCB: TElCheckBox;
    ElStatusBarBevelRG: TElRadioGroup;
    ElStatusBarPanelsGB: TElGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ElStatusPanelSpinEdit: TElSpinEdit;
    ElStatusSectionAddButton: TElPopupButton;
    ElStatusSectionDeleteButton: TElPopupButton;
    ElStatusPanelTextButtonEdit: TElButtonEdit;
    ElStatusPanelHintButtonEdit: TElButtonEdit;
    ElStatusPanelResizableCB: TElCheckBox;
    ElStatusPanelIsHTMLCB: TElCheckBox;
    ElStatusPanelTextAlignRG: TElRadioGroup;
    ElStatusPanelVisibleCB: TElCheckBox;
    ElStatusPanelBevelRG: TElRadioGroup;
    ElStatusPanelStyleCombo: TElComboBox;
    procedure ElStatusBarSimplePanelCBClick(Sender: TObject);
    procedure ElStatusBarSimpleTextEditButtonClick(Sender: TObject);
    procedure ElStatusBarSimpleTextHTMLCBClick(Sender: TObject);
    procedure ElStatusBarCanResizePanelsCBClick(Sender: TObject);
    procedure ElStatusBarShowGripCBClick(Sender: TObject);
    procedure ElStatusPanelSpinEditChange(Sender: TObject);
    procedure ElStatusSectionAddButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ElStatusSectionDeleteButtonClick(Sender: TObject);
    procedure ElStatusPanelTextAlignRGClick(Sender: TObject);
    procedure ElStatusPanelBevelRGClick(Sender: TObject);
    procedure ElStatusPanelHintButtonEditButtonClick(Sender: TObject);
    procedure ElStatusPanelTextButtonEditButtonClick(Sender: TObject);
    procedure ElStatusBarBevelRGClick(Sender: TObject);
    procedure ElStatusPanelVisibleCBClick(Sender: TObject);
    procedure ElStatusPanelResizableCBClick(Sender: TObject);
    procedure ElStatusPanelIsHTMLCBClick(Sender: TObject);
    procedure ElStatusPanelStyleComboChange(Sender: TObject);
    procedure sampleElStatusBarPanelDraw(Sender: TObject;
      Panel: TElStatusPanel);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ElstatusBarDemoMainForm: TElstatusBarDemoMainForm;

implementation

{$R *.DFM}

procedure TElstatusBarDemoMainForm.ElStatusBarSimplePanelCBClick(
  Sender: TObject);
begin
  sampleElStatusBar.SimplePanel := ElStatusBarSimplePanelCB.Checked;
  if ElStatusBarSimplePanelCB.Checked then
  begin
    ElStatusBarSimpleTextEdit.Color := clWindow;
    ElStatusBarSimpleTextEdit.Enabled := true;
    ElStatusBarSimpleTextHTMLCB.Enabled := true;
    Label1.Enabled := true;
  end
  else
  begin
    ElStatusBarSimpleTextEdit.Color := clBtnFace;
    ElStatusBarSimpleTextEdit.Enabled := false;
    ElStatusBarSimpleTextHTMLCB.Enabled := false;
    Label1.Enabled := false;
  end;
end;

procedure TElstatusBarDemoMainForm.ElStatusBarSimpleTextEditButtonClick(
  Sender: TObject);
begin
  sampleElStatusBar.SimpleText := ElStatusBarSimpleTextEdit.Text;
end;

procedure TElstatusBarDemoMainForm.ElStatusBarSimpleTextHTMLCBClick(
  Sender: TObject);
begin
  sampleElStatusBar.SimpleTextIsHTML := ElStatusBarSimpleTextHTMLCB.Checked;
end;

procedure TElstatusBarDemoMainForm.ElStatusBarCanResizePanelsCBClick(Sender: TObject);
begin
  sampleElStatusBar.ResizablePanels := ElStatusBarCanResizePanelsCB.Checked;
end;

procedure TElstatusBarDemoMainForm.ElStatusBarShowGripCBClick(
  Sender: TObject);
begin
  sampleElStatusBar.SizeGrip := ElStatusBarShowGripCB.Checked;
end;

procedure TElstatusBarDemoMainForm.ElStatusPanelSpinEditChange(
  Sender: TObject);
begin
  ElStatusPanelTextAlignRG.ItemIndex := Integer(sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Alignment);
  ElStatusPanelBevelRG.ItemIndex   := Integer(sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Bevel);
  ElStatusPanelVisibleCB.Checked   := sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Visible;
  ElStatusPanelResizableCB.Checked := sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Resizable;
  ElStatusPanelIsHTMLCB.Checked    := sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].IsHTML;
  ElStatusPanelTextButtonEdit.Text := sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Text;
  ElStatusPanelHintButtonEdit.Text := sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Hint;
  ElStatusPanelStyleCombo.ItemIndex:= Integer(sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Style);
end;

procedure TElstatusBarDemoMainForm.ElStatusSectionAddButtonClick(
  Sender: TObject);
begin
  sampleElStatusBar.Panels.Add;
  ElStatusPanelSpinEdit.MaxValue := sampleElStatusBar.Panels.Count - 1;
  ElStatusSectionDeleteButton.Enabled := true;
  ElStatusPanelSpinEditChange(Self);
end;

procedure TElstatusBarDemoMainForm.FormActivate(Sender: TObject);
begin
  ElStatusPanelSpinEditChange(Self);
end;

procedure TElstatusBarDemoMainForm.ElStatusSectionDeleteButtonClick(
  Sender: TObject);
begin
  if sampleElstatusBar.Panels.Count > 1 then
  begin
    sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Free;
    if sampleElstatusBar.Panels.Count = 1 then
      ElStatusSectionDeleteButton.Enabled := false;
    ElStatusPanelSpinEditChange(Self);
  end;
end;

procedure TElstatusBarDemoMainForm.ElStatusPanelTextAlignRGClick(
  Sender: TObject);
begin
  sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Alignment := TAlignment(ElStatusPanelTextAlignRG.ItemIndex);
end;

procedure TElstatusBarDemoMainForm.ElStatusPanelBevelRGClick(Sender: TObject);
begin
  sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Bevel := TElStatusPanelBevel(ElStatusPanelBevelRG.ItemIndex);
end;

procedure TElstatusBarDemoMainForm.ElStatusPanelHintButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Hint := ElStatusPanelHintButtonEdit.Text;
end;

procedure TElstatusBarDemoMainForm.ElStatusPanelTextButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Text := ElStatusPanelTextButtonEdit.Text;
end;

procedure TElstatusBarDemoMainForm.ElStatusBarBevelRGClick(
  Sender: TObject);
begin
  sampleElstatusBar.Bevel := TElStatusPanelBevel(ElStatusBarBevelRG.ItemIndex);
end;

procedure TElstatusBarDemoMainForm.ElStatusPanelVisibleCBClick(
  Sender: TObject);
begin
  sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Visible := ElStatusPanelVisibleCB.Checked;
end;

procedure TElstatusBarDemoMainForm.ElStatusPanelResizableCBClick(
  Sender: TObject);
begin
  sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Resizable := ElStatusPanelResizableCB.Checked;
end;

procedure TElstatusBarDemoMainForm.ElStatusPanelIsHTMLCBClick(
  Sender: TObject);
begin
  sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].IsHTML := ElStatusPanelIsHTMLCB.Checked;
end;

procedure TElstatusBarDemoMainForm.ElStatusPanelStyleComboChange(
  Sender: TObject);
var aLabel : TElURLLabel;
begin
  if ElStatusPanelStyleCombo.ItemIndex <> Integer(sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Style) then
  begin
    if sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Style = epsControl then
      sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Control.Free;
    sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Style := TElStatusPanelStyle(ElStatusPanelStyleCombo.ItemIndex);
    if sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Style = epsControl then
    begin
      aLabel := TElURLLabel.Create(Self);
      aLabel.Caption := 'ElPack Homepage';
      aLabel.URL := 'http://www.eldos.org/elpack/elpack.html';
      sampleElstatusBar.Panels[ElStatusPanelSpinEdit.Value].Control := aLabel;
    end;
  end;
end;

procedure TElstatusBarDemoMainForm.sampleElStatusBarPanelDraw(
  Sender: TObject; Panel: TElStatusPanel);
var R : TRect;
    ACanvas : TCanvas;
begin
  R := Panel.PanelRect;
  ACanvas := SampleElStatusBar.Canvas;
  with Image1.Picture.Bitmap do
    DrawTransparentBitmapEx(ACanvas.Handle, Image1.Picture.Bitmap,
                           (R.Left + R.Right - Width) div 2,
                           (R.Top + R.Bottom - Height) div 2,
                            rect(0, 0, Width, Height),
                           Canvas.Pixels[0, Height - 1]); 
end;

end.

