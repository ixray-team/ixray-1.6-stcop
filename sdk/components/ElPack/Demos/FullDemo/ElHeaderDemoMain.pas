unit ElHeaderDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElImgFrm, ElBtnEdit, ElPopBtn, StdCtrls, ElACtrls, ElSpin, ElBtnCtl,
  ElVCLUtils, ElCheckCtl, ElGroupBox, ElHeader, ExtCtrls, ElPanel,
  ElImgLst, ElCheckItemGrp, ElXPThemedControl, ImgList;

type
  TElHeaderDemoMainForm = class(TForm)
    ElHeaderImageForm: TElImageForm;
    ElHeaderSectionsGroupBox: TElGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ElHeaderSectionSpinEdit: TElSpinEdit;
    ElHeaderAddButton: TElPopupButton;
    ElHeaderDeleteButton: TElPopupButton;
    ElHeaderTextButtonEdit: TElButtonEdit;
    ElHeaderHintButtonEdit: TElButtonEdit;
    ElHeaderCanClickCB: TElCheckBox;
    ElHeaderGeneralGroupbox: TElGroupBox;
    ElHeaderFlatCB: TElCheckBox;
    ElHeaderDraggableSectionsCB: TElCheckBox;
    ElHeaderMoveOnDragCB: TElCheckBox;
    ElHeaderUseImageFormCB: TElCheckBox;
    ElHeaderWrapCaptionsCB: TElCheckBox;
    ElHeaderResizeOnDragCB: TElCheckBox;
    sampleElHeader: TElHeader;
    ElHeaderCanResizeCB: TElCheckBox;
    ElHeaderSectionMinWidthSpinEdit: TElSpinEdit;
    Label1: TLabel;
    Label5: TLabel;
    ElHeaderSectionMaxWidthSpinEdit: TElSpinEdit;
    ElHeaderAutoSizeCB: TElCheckBox;
    ElHeaderTextAlignRG: TElRadioGroup;
    ElHeaderImageAlignRG: TElRadioGroup;
    Label6: TLabel;
    ElHeaderSectionImageIndexSpinEdit: TElSpinEdit;
    ElHeaderSectionVisibleCB: TElCheckBox;
    ElHeaderSectionSortSignRG: TElRadioGroup;
    HeaderImages: TElImageList;
    procedure ElHeaderAddButtonClick(Sender: TObject);
    procedure ElHeaderCanClickCBClick(Sender: TObject);
    procedure ElHeaderDraggableSectionsCBClick(Sender: TObject);
    procedure ElHeaderFlatCBClick(Sender: TObject);
    procedure ElHeaderMoveOnDragCBClick(Sender: TObject);
    procedure ElHeaderSectionSpinEditChange(Sender: TObject);
    procedure ElHeaderUseImageFormCBClick(Sender: TObject);
    procedure ElHeaderResizeOnDragCBClick(Sender: TObject);
    procedure ElHeaderWrapCaptionsCBClick(Sender: TObject);
    procedure ElHeaderTextButtonEditButtonClick(Sender: TObject);
    procedure ElHeaderHintButtonEditButtonClick(Sender: TObject);
    procedure ElHeaderDeleteButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ElHeaderSectionMinWidthSpinEditChange(Sender: TObject);
    procedure ElHeaderSectionMaxWidthSpinEditChange(Sender: TObject);
    procedure ElHeaderAutoSizeCBClick(Sender: TObject);
    procedure ElHeaderSectionVisibleCBClick(Sender: TObject);
    procedure ElHeaderSectionImageIndexSpinEditChange(Sender: TObject);
    procedure ElHeaderTextAlignRGClick(Sender: TObject);
    procedure ElHeaderImageAlignRGClick(Sender: TObject);
    procedure ElHeaderSectionSortSignRGClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ElHeaderDemoMainForm: TElHeaderDemoMainForm;

implementation

{$R *.DFM}

procedure TElHeaderDemoMainForm.ElHeaderAddButtonClick(Sender: TObject);
begin
  sampleElHeader.Sections.AddSection;
  ElHeaderSectionSpinEdit.MaxValue := sampleElHeader.Sections.Count - 1;
  ElHeaderSectionSpinEditChange(Self);
end;

procedure TElHeaderDemoMainForm.ElHeaderCanClickCBClick(Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].AllowClick := ElHeaderCanClickCB.Checked;
end;

procedure TElHeaderDemoMainForm.ElHeaderDraggableSectionsCBClick(Sender: 
    TObject);
begin
  sampleElHeader.AllowDrag := ElHeaderDraggableSectionsCB.Checked;
end;

procedure TElHeaderDemoMainForm.ElHeaderFlatCBClick(Sender: TObject);
begin
  SampleElHeader.Flat := ElHeaderFlatCB.Checked;
end;

procedure TElHeaderDemoMainForm.ElHeaderMoveOnDragCBClick(Sender: TObject);
begin
  sampleElHeader.MoveOnDrag := ElHEaderMoveOnDragCB.Checked;
end;

procedure TElHeaderDemoMainForm.ElHeaderSectionSpinEditChange(Sender: TObject);
begin
  ElHeaderTextButtonEdit.Text := sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].Text;
  ElHeaderHintButtonEdit.Text := sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].Hint;
  ElHeaderCanClickCB.Checked := sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].AllowClick;
  ElHeaderAutoSizeCB.Checked := sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].AutoSize;
  ElHeaderCanResizeCB.Checked := sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].Resizable;
  ElHeaderSectionMinWidthSpinEdit.Value := sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].MinWidth;
  ElHeaderSectionMaxWidthSpinEdit.Value := sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].MaxWidth;
  ElHeaderSectionImageIndexSpinEdit.Value := sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].ImageIndex;
  ElHeaderTextAlignRG.ItemIndex := Integer(sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].Alignment);
  ElHeaderImageAlignRG.ItemIndex := Integer(sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].PictureAlign);
  ElHeaderSectionSortSignRG.ItemIndex := Integer(sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].SortMode);
end;

procedure TElHeaderDemoMainForm.ElHeaderUseImageFormCBClick(Sender: TObject);
begin
  if ElHeaderUseImageFormCB.Checked then
  begin
    ElHeaderImageForm.BackgroundType := bgtTileBitmap;
  end
  else
  begin
    ElHeaderImageForm.BackgroundType := bgtColorFill;
  end;
end;

procedure TElHeaderDemoMainForm.ElHeaderResizeOnDragCBClick(
  Sender: TObject);
begin
 sampleElHeader.ResizeOnDrag := ElHeaderResizeOnDragCB.Checked;
end;

procedure TElHeaderDemoMainForm.ElHeaderWrapCaptionsCBClick(
  Sender: TObject);
begin
  sampleElHeader.WrapCaptions := ElHeaderWrapCaptionsCB.Checked;
end;

procedure TElHeaderDemoMainForm.ElHeaderTextButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].Text := ElHEaderTextButtonEdit.Text;
end;

procedure TElHeaderDemoMainForm.ElHeaderHintButtonEditButtonClick(
  Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].Hint := ElHeaderHintButtonEdit.Text;
end;

procedure TElHeaderDemoMainForm.ElHeaderDeleteButtonClick(Sender: TObject);
begin
  if sampleElHeader.Sections.Count > 1 then
  begin
    sampleElHeader.Sections.DeleteSection(sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value]);
    ElHeaderSectionSpinEdit.MaxValue := sampleElHeader.Sections.Count - 1;
    ElHeaderDeleteButton.Enabled := sampleElHeader.Sections.Count > 0;
    ElHeaderSectionSpinEditChange(Self);
  end;
end;

procedure TElHeaderDemoMainForm.FormActivate(Sender: TObject);
begin
  ElHeaderSectionSpinEditChange(Self);
end;

procedure TElHeaderDemoMainForm.ElHeaderSectionMinWidthSpinEditChange(
  Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].MinWidth := ElHeaderSectionMinWidthSpinEdit.Value;
end;

procedure TElHeaderDemoMainForm.ElHeaderSectionMaxWidthSpinEditChange(
  Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].MaxWidth := ElHeaderSectionMaxWidthSpinEdit.Value;
end;

procedure TElHeaderDemoMainForm.ElHeaderAutoSizeCBClick(Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].AutoSize := ElHeaderAutoSizeCB.Checked;
end;

procedure TElHeaderDemoMainForm.ElHeaderSectionVisibleCBClick(
  Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].Visible := ElHeaderSectionVisibleCB.Checked;
end;

procedure TElHeaderDemoMainForm.ElHeaderSectionImageIndexSpinEditChange(
  Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].ImageIndex := ElHeaderSectionImageIndexSpinEdit.Value;
end;

procedure TElHeaderDemoMainForm.ElHeaderTextAlignRGClick(Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].Alignment := TElSAlignment(ElHeaderTextAlignRG.ItemIndex);
end;

procedure TElHeaderDemoMainForm.ElHeaderImageAlignRGClick(Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].PictureAlign := TElSAlignment(ElHeaderImageAlignRG.ItemIndex);
end;

procedure TElHeaderDemoMainForm.ElHeaderSectionSortSignRGClick(
  Sender: TObject);
begin
  sampleElHeader.Sections[ElHeaderSectionSpinEdit.Value].SortMode := TElSSortMode(ElHeaderSectionSortSignRG.ItemIndex);
end;

end.

