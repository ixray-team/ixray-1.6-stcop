unit ElPopupButtonDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElBtnCtl, ElPopBtn, ElImgLst, StdCtrls, ElACtrls, ElSpin,
  ElCheckCtl, ElClrCmb, Buttons, Menus, ElImgFrm, ElVCLUtils, ElBtnEdit,
  ElXPThemedControl, ImgList;

type
  TElPopupButtonDemoMainForm = class(TForm)
    sampleElPopupButton: TElPopupButton;
    Label4: TLabel;
    ElPopupButtonWidthSpinEdit: TElSpinEdit;
    Label5: TLabel;
    ElPopupButtonHeightSpinEdit: TElSpinEdit;
    SmallImages: TElImageList;
    ElPopupButtonShowGlyphCB: TElCheckBox;
    ElPopupButtonShowTextCB: TElCheckBox;
    ElPopupButtonShowFocusCB: TElCheckBox;
    ElPopupButtonShowBorderCB: TElCheckBox;
    ElPopupButtonDefaultCB: TElCheckBox;
    ElPopupButtonShowDefaultFrameCB: TElCheckBox;
    ElPopupButtonThinBorderCB: TElCheckBox;
    ElPopupButtonFlatCB: TElCheckBox;
    Label2: TLabel;
    ElPopupButtonGlyphLayoutCombo: TElAdvancedComboBox;
    Label1: TLabel;
    ElPopupButtonColorCombo: TElColorCombo;
    ElPopupButtonIsSwitchCB: TElCheckBox;
    ElPopupButtonOldStyledCB: TElCheckBox;
    ElPopupButtonImageIndexSpinEdit: TElSpinEdit;
    Label3: TLabel;
    ElPopupButtonImageTypeCombo: TElAdvancedComboBox;
    Label6: TLabel;
    ElPopupButtonAttachMenuCB: TElCheckBox;
    ElPopupButtonShowArrowCB: TElCheckBox;
    PopupMenu: TPopupMenu;
    Item11: TMenuItem;
    Item21: TMenuItem;
    ElPopupButtonImageForm: TElImageForm;
    Label7: TLabel;
    ElPopupButtonMenuSideCombo: TElAdvancedComboBox;
    ElPopupButtonUseImageFormCB: TElCheckBox;
    Label8: TLabel;
    ElPopupButtonTextButtonEdit: TElButtonEdit;
    Label9: TLabel;
    ElPopupButtonTextTypeCombo: TElAdvancedComboBox;
    ElPopupButtonUseXPThemesCB: TElCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure ElPopupButtonWidthSpinEditChange(Sender: TObject);
    procedure ElPopupButtonHeightSpinEditChange(Sender: TObject);
    procedure ElPopupButtonShowGlyphCBClick(Sender: TObject);
    procedure ElPopupButtonShowTextCBClick(Sender: TObject);
    procedure ElPopupButtonShowFocusCBClick(Sender: TObject);
    procedure ElPopupButtonShowBorderCBClick(Sender: TObject);
    procedure ElPopupButtonDefaultCBClick(Sender: TObject);
    procedure ElPopupButtonThinBorderCBClick(Sender: TObject);
    procedure ElPopupButtonShowDefaultFrameCBClick(Sender: TObject);
    procedure ElPopupButtonColorComboChange(Sender: TObject);
    procedure ElPopupButtonGlyphLayoutComboChange(Sender: TObject);
    procedure ElPopupButtonIsSwitchCBClick(Sender: TObject);
    procedure ElPopupButtonOldStyledCBClick(Sender: TObject);
    procedure ElPopupButtonImageTypeComboChange(Sender: TObject);
    procedure ElPopupButtonImageIndexSpinEditChange(Sender: TObject);
    procedure ElPopupButtonShowArrowCBClick(Sender: TObject);
    procedure ElPopupButtonAttachMenuCBClick(Sender: TObject);
    procedure ElPopupButtonFlatCBClick(Sender: TObject);
    procedure ElPopupButtonUseImageFormCBClick(Sender: TObject);
    procedure ElPopupButtonMenuSideComboChange(Sender: TObject);
    procedure ElPopupButtonTextTypeComboChange(Sender: TObject);
    procedure ElPopupButtonTextButtonEditButtonClick(Sender: TObject);
    procedure ElPopupButtonUseXPThemesCBClick(Sender: TObject);
  private
    NotFirstTime: boolean;
  public
    { Public declarations }
  end;

var
  ElPopupButtonDemoMainForm: TElPopupButtonDemoMainForm;

implementation

{$R *.DFM}

procedure TElPopupButtonDemoMainForm.FormActivate(Sender: TObject);
begin
  if not NotFirstTime then
  begin
    ElPopupButtonGlyphLayoutCombo.ItemIndex := 0;
    ElPopupButtonImageTypeCombo.ItemIndex   := 0;
    ElPopupButtonMenuSideCombo.ItemIndex    := 0;
    ElPopupButtonTextTypeCombo.ItemIndex    := 0;
  end;
  NotFirstTime := true;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonWidthSpinEditChange(
  Sender: TObject);
begin
  sampleElPopupButton.Width := ElPopupButtonWidthSpinEdit.Value;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonHeightSpinEditChange(
  Sender: TObject);
begin
  sampleElPopupButton.Height := ElPopupButtonHeightSpinEdit.Value;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonShowGlyphCBClick(
  Sender: TObject);
begin
  sampleElPopupButton.ShowGlyph := ElPopupButtonShowGlyphCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonShowTextCBClick(
  Sender: TObject);
begin
  sampleElPopupButton.ShowText := ElPopupButtonShowTextCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonShowFocusCBClick(
  Sender: TObject);
begin
  sampleElPopupButton.ShowFocus := ElPopupButtonShowFocusCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonShowBorderCBClick(
  Sender: TObject);
begin
  sampleElPopupButton.ShowBorder := ElPopupButtonShowBorderCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonDefaultCBClick(
  Sender: TObject);
begin
  sampleElPopupButton.Default := ElPopupButtonDefaultCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonThinBorderCBClick(
  Sender: TObject);
begin
  sampleElPopupButton.ThinFrame := ElPopupButtonThinBorderCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonShowDefaultFrameCBClick(
  Sender: TObject);
begin
  sampleElPopupButton.DrawDefaultFrame := ElPopupButtonShowDefaultFrameCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonColorComboChange(
  Sender: TObject);
begin
  sampleElPopupButton.Color := ElPopupButtonColorCombo.SelectedColor;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonGlyphLayoutComboChange(
  Sender: TObject);
begin
  sampleElPopupButton.Layout := TButtonLayout(ElPopupButtonGlyphLayoutCombo.ItemIndex);
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonIsSwitchCBClick(Sender: TObject);
begin
  sampleElPopupButton.IsSwitch := ElPopupButtonIsSwitchCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonOldStyledCBClick(
  Sender: TObject);
begin
  sampleElPopupButton.OldStyled := ElPopupButtonOldStyledCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonImageTypeComboChange(
  Sender: TObject);
begin
  case ElPopupButtonImageTypeCombo.ItemIndex of
    0: begin
         sampleElPopupButton.UseIcon := false;
         sampleElPopupButton.UseImageList := false;
         ElPopupButtonImageIndexSpinEdit.Enabled := false;
         Label3.Enabled := false;
       end;
    1: begin
         sampleElPopupButton.UseIcon := true;
         sampleElPopupButton.UseImageList := false;
         ElPopupButtonImageIndexSpinEdit.Enabled := false;
         Label3.Enabled := false;
       end;
    2: begin
         sampleElPopupButton.UseIcon := false;
         sampleElPopupButton.UseImageList := true;
         ElPopupButtonImageIndexSpinEdit.Enabled := true;
         Label3.Enabled := true;
       end;
  end;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonImageIndexSpinEditChange(
  Sender: TObject);
begin
  sampleElPopupButton.ImageIndex := ElPopupButtonImageIndexSpinEdit.Value;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonShowArrowCBClick(Sender: TObject);
begin
  sampleElPopupButton.UseArrow := ElPopupButtonShowArrowCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonAttachMenuCBClick(
  Sender: TObject);
begin
  if ElPopupButtonAttachMenuCB.Checked then
  begin
    sampleElPopupButton.PullDownMenu := PopupMenu;
    ElPopupButtonShowArrowCB.Enabled := true;
    ElPopupButtonMenuSideCombo.Enabled := true;
    Label7.Enabled := true;
  end
  else
  begin
    sampleElPopupButton.PullDownMenu := nil;
    ElPopupButtonShowArrowCB.Enabled := false;
    ElPopupButtonMenuSideCombo.Enabled := false;
    Label7.Enabled := false;
  end;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonFlatCBClick(
  Sender: TObject);
begin
  sampleElPopupButton.ThinFrame := ElPopupButtonFlatCB.Checked;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonUseImageFormCBClick(
  Sender: TObject);
begin
  if ElPopupButtonUseImageFormCB.Checked then
    ElPopupButtonImageForm.Backgroundtype := bgtTileBitmap
  else
    ElPopupButtonImageForm.Backgroundtype := bgtColorFill;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonMenuSideComboChange(
  Sender: TObject);
begin
  sampleElPopupButton.PopupPlace := TPopupPlace(ElPopupButtonMenuSideCombo.ItemIndex);
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonTextTypeComboChange(
  Sender: TObject);
begin
  sampleElPopupButton.TextDrawType := TElTextDrawType(ElPopupButtonTextTypeCombo.ItemIndex);
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonTextButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElPopupButton.Caption := ElPopupButtonTextButtonEdit.Text;
end;

procedure TElPopupButtonDemoMainForm.ElPopupButtonUseXPThemesCBClick(
  Sender: TObject);
begin
  SampleElPopupButton.UseXPThemes := ElPopupButtonUseXPThemesCB.Checked;
end;

end.

