unit ElTreeStringGridDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElTree, ElTreeGrids, ElBtnCtl, ElCheckCtl, ExtCtrls, ElPanel,
  ElTreeAdvEdit, ElTreeSpinEdit, ElTreeDTPickEdit, ElTreeCurrEdit,
  ElTreeCheckBoxEdit, ElDTPick, ElXPThemedControl;

type
  TElTreeStringGridDemoForm = class(TForm)
    ElTreeStringGridDemoPanel: TElPanel;
    ElTreeStringGridShowHeaderCB: TElCheckBox;
    ElTreeStringGridAllowEditCB: TElCheckBox;
    ElTreeStringGridAlwaysShowEditorCB: TElCheckBox;
    ElTreeStringGridCanMoveColsCB: TElCheckBox;
    ElTreeStringGridRowSelectCB: TElCheckBox;
    ElTreeStringGridUseTabCB: TElCheckBox;
    ElTreeStringGridUseAdvEditorsCB: TElCheckBox;
    EltreeStringGridExplorerModeCB: TElCheckBox;
    sampleElTreeStringGrid: TElTreeStringGrid;
    ElTreeStringGridCheckBoxEdit: TElTreeInplaceCheckBoxEdit;
    ElTreeStringGridCurrencyEdit: TElTreeInplaceCurrencyEdit;
    ElTreeStringGridDateTimePicker: TElTreeInplaceDateTimePicker;
    ElTreeStringGridFloatSpinEdit: TElTreeInplaceFloatSpinEdit;
    ElTreeStringGridSpinEdit: TElTreeInplaceSpinEdit;
    ElTreeStringGridAdvancedEdit: TElTreeInplaceAdvancedEdit;
    procedure ElTreeStringGridAllowEditCBClick(Sender: TObject);
    procedure ElTreeStringGridAlwaysShowEditorCBClick(Sender: TObject);
    procedure ElTreeStringGridCanMoveColsCBClick(Sender: TObject);
    procedure ElTreeStringGridDateTimePickerBeforeOperation(Sender: TObject; var 
        DefaultConversion: Boolean);
    procedure EltreeStringGridExplorerModeCBClick(Sender: TObject);
    procedure ElTreeStringGridRowSelectCBClick(Sender: TObject);
    procedure ElTreeStringGridShowHeaderCBClick(Sender: TObject);
    procedure ElTreeStringGridUseAdvEditorsCBClick(Sender: TObject);
    procedure ElTreeStringGridUseTabCBClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ElTreeStringGridDemoForm: TElTreeStringGridDemoForm;

implementation

procedure TElTreeStringGridDemoForm.ElTreeStringGridAllowEditCBClick(Sender: 
    TObject);
begin
  sampleElTreeStringGrid.goEditing := ElTreeStringGridAllowEditCB.Checked;
end;

procedure TElTreeStringGridDemoForm.ElTreeStringGridAlwaysShowEditorCBClick(
    Sender: TObject);
begin
  sampleElTreeStringGrid.goAlwaysShowEditor := ElTreeStringGridAlwaysShowEditorCB.Checked;
end;

procedure TElTreeStringGridDemoForm.ElTreeStringGridCanMoveColsCBClick(Sender: 
    TObject);
begin
  sampleElTreeStringGrid.goColMoving := ElTreeStringGridCanMoveColsCB.Checked;
end;

procedure 
    TElTreeStringGridDemoForm.ElTreeStringGridDateTimePickerBeforeOperation(
    Sender: TObject; var DefaultConversion: Boolean);
begin
  ElTreeStringGridDateTimePicker.Editor.ShowPopupCalendar := false;
  ElTreeStringGridDateTimePicker.Editor.Format := edfShortDateLongTime;
end;

procedure TElTreeStringGridDemoForm.EltreeStringGridExplorerModeCBClick(Sender: 
    TObject);
begin
  sampleElTreeStringGrid.ExplorerEditMode := not ElTreeStringGridUseAdvEditorsCB.Checked;
end;

procedure TElTreeStringGridDemoForm.ElTreeStringGridRowSelectCBClick(Sender:
    TObject);
begin
  sampleElTreeStringGrid.goRowSelect := ElTreeStringGridRowSelectCB.Checked;
end;

procedure TElTreeStringGridDemoForm.ElTreeStringGridShowHeaderCBClick(Sender: 
    TObject);
begin
  if ElTreeStringGridShowHeaderCB.Checked then
    sampleElTreeStringGrid.LockHeaderHeight := false
  else
  begin
    sampleElTreeStringGrid.LockHeaderHeight := true;
    sampleElTreeStringGrid.HeaderHeight := 0;
  end;
end;

procedure TElTreeStringGridDemoForm.ElTreeStringGridUseAdvEditorsCBClick(
    Sender: TObject);
begin
  sampleElTreeStringGrid.UseDefaultEditor := not ElTreeStringGridUseAdvEditorsCB.Checked;
end;

procedure TElTreeStringGridDemoForm.ElTreeStringGridUseTabCBClick(Sender: 
    TObject);
begin
  sampleElTreeStringGrid.goTabs := ElTreeStringGridUseTabCB.Checked;
end;


{$R *.DFM}

end.

