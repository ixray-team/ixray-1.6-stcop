unit ElDateTimePickerDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ElACtrls, ElXPThemedControl, ElDTPick, ExtCtrls, ElPanel,
  ElGroupBox, ElCheckItemGrp, ElVCLUtils, ElImgFrm, ElBtnCtl, ElCheckCtl,
  ElBtnEdit;

type
  TElDateTimePickerDemoMainForm = class(TForm)
    SampleElDTPick: TElDateTimePicker;
    Label1: TLabel;
    ElDTPickAlignmentCombo: TElAdvancedComboBox;
    ElDTPickBordersCheckGroup: TElCheckGroup;
    ElDTPickShowCheckboxCB: TElCheckBox;
    Label2: TLabel;
    ElDTPickFormatCombo: TElAdvancedComboBox;
    Label3: TLabel;
    SampleElDTPickCustomFormatEdit: TElButtonEdit;
    Label4: TLabel;
    Label5: TLabel;
    MinDateDTPick: TElDateTimePicker;
    MaxDateDTPick: TElDateTimePicker;
    SampleElDTPickShowPopupCalendarCB: TElCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure ElDTPickBordersCheckGroupClick(Sender: TObject);
    procedure ElDTPickShowCheckboxCBClick(Sender: TObject);
    procedure ElDTPickAlignmentComboChange(Sender: TObject);
    procedure ElDTPickFormatComboChange(Sender: TObject);
    procedure SampleElDTPickCustomFormatEditButtonClick(Sender: TObject);
    procedure MinDateDTPickChange(Sender: TObject);
    procedure MaxDateDTPickChange(Sender: TObject);
    procedure SampleElDTPickShowPopupCalendarCBClick(Sender: TObject);
  private
    NotFirstTime : boolean;
  public
    { Public declarations }
  end;

var
  ElDateTimePickerDemoMainForm: TElDateTimePickerDemoMainForm;

implementation

{$R *.DFM}

procedure TElDateTimePickerDemoMainForm.FormActivate(Sender: TObject);
begin
  if not NotFirstTime then
  begin
    ElDTPickAlignmentCombo.ItemIndex := 0;
    ElDTPickFormatCombo.ItemIndex := 0;
  end;
  NotFirstTime := true;
end;

procedure TElDateTimePickerDemoMainForm.ElDTPickBordersCheckGroupClick(
  Sender: TObject);
begin
  if ElDTPickBordersCheckGroup.Checked[0] then
    sampleElDTPick.BorderSides := sampleElDTPick.BorderSides + [ebsLeft]
  else
    sampleElDTPick.BorderSides := sampleElDTPick.BorderSides - [ebsLeft];
  if ElDTPickBordersCheckGroup.Checked[1] then
    sampleElDTPick.BorderSides := sampleElDTPick.BorderSides + [ebsRight]
  else
    sampleElDTPick.BorderSides := sampleElDTPick.BorderSides - [ebsRight];
  if ElDTPickBordersCheckGroup.Checked[2] then
    sampleElDTPick.BorderSides := sampleElDTPick.BorderSides + [ebsTop]
  else
    sampleElDTPick.BorderSides := sampleElDTPick.BorderSides - [ebsTop];
  if ElDTPickBordersCheckGroup.Checked[3] then
    sampleElDTPick.BorderSides := sampleElDTPick.BorderSides + [ebsBottom]
  else
    sampleElDTPick.BorderSides := sampleElDTPick.BorderSides - [ebsBottom];
end;

procedure TElDateTimePickerDemoMainForm.ElDTPickShowCheckboxCBClick(
  Sender: TObject);
begin
  SampleElDTPick.ShowCheckBox := ElDTPickShowCheckboxCB.Checked;
end;

procedure TElDateTimePickerDemoMainForm.ElDTPickAlignmentComboChange(
  Sender: TObject);
begin
  SampleElDTPick.Alignment := TAlignment(ElDTPickAlignmentCombo.ItemIndex);
end;

procedure TElDateTimePickerDemoMainForm.ElDTPickFormatComboChange(
  Sender: TObject);
begin
  SampleElDTPick.Format := TElDatePickerFormat(ElDTPickFormatCombo.ItemIndex);
  SampleElDTPickCustomFormatEdit.Enabled := ElDTPickFormatCombo.ItemIndex = 5;
  Label3.Enabled := ElDTPickFormatCombo.ItemIndex = 5;
end;

procedure TElDateTimePickerDemoMainForm.SampleElDTPickCustomFormatEditButtonClick(
  Sender: TObject);
begin
  SampleElDTPick.FormatString := SampleElDTPickCustomFormatEdit.Text;
end;

procedure TElDateTimePickerDemoMainForm.MinDateDTPickChange(
  Sender: TObject);
begin
  SampleElDTPick.MinDate := MinDateDTPick.DateTime
end;

procedure TElDateTimePickerDemoMainForm.MaxDateDTPickChange(
  Sender: TObject);
begin
  SampleElDTPick.MaxDate := MaxDateDTPick.DateTime;
end;

procedure TElDateTimePickerDemoMainForm.SampleElDTPickShowPopupCalendarCBClick(Sender: TObject);
begin
  SampleElDTPick.ShowPopupCalendar := SampleElDTPickShowPopupCalendarCB.Checked;
end;

end.
