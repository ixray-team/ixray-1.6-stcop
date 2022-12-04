unit frmSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ElLabel, ElClrCmb, frmMain, ElTree, ElBtnCtl, ElPopBtn,
  ElACtrls, ElCLabel, ElXPThemedControl, ExtCtrls, ElPanel, ElGroupBox;

type
  TSettings = class(TForm)
    FontDialog: TFontDialog;
    ElPopupButton1: TElPopupButton;
    ElPopupButton2: TElPopupButton;
    GroupBox2: TElGroupBox;
    ElLabel1: TElLabel;
    ElLabel2: TElLabel;
    ElLabel4: TElLabel;
    BkColor: TElColorCombo;
    FrColor: TElColorCombo;
    BkCellColor: TElColorCombo;
    GroupBox: TElGroupBox;
    CurrentFont: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CurrentFontClick(Sender: TObject);
    procedure ElPopupButton2Click(Sender: TObject);
    procedure ElPopupButton1Click(Sender: TObject);
    procedure BkColorChange(Sender: TObject);
    procedure FrColorChange(Sender: TObject);
    procedure BkCellColorChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Settings: TSettings;
  Style: TElCellStyle;
implementation

{$R *.DFM}

procedure TSettings.FormCreate(Sender: TObject);
begin
  frmNoteBook.NoteTree.ItemFocused.MainStyle.OwnerProps := False;
  Style := frmNoteBook.NoteTree.ItemFocused.MainStyle;
  with Style do
  begin
//    OwnerProps := not Options.CustomColors;
    CurrentFont.Caption := FontName;
    CurrentFont.Font.Size := FontSize;
    CurrentFont.Font.Style := FontStyles;
    CurrentFont.Font.Color := TextColor;
    CurrentFont.Color := TextBkColor;
    BkColor.SelectedColor := TextBkColor;
    FrColor.SelectedColor := TextColor;
    BkCellColor.SelectedColor := CellBkColor;
    GroupBox.Color := CellBkColor;
//    CellBkColor := C.BkColor;
  end;
end;

procedure TSettings.CurrentFontClick(Sender: TObject);
begin
  FontDialog.Font := CurrentFont.Font;
  FontDialog.Execute;
  CurrentFont.Caption := FontDialog.Font.Name;
  CurrentFont.Font := FontDialog.Font;
  FrColor.SelectedColor := FontDialog.Font.Color;
  CurrentFont.Font.Color := FontDialog.Font.Color;
end;

procedure TSettings.ElPopupButton2Click(Sender: TObject);
begin
  Close;
end;

procedure TSettings.ElPopupButton1Click(Sender: TObject);
begin
  with Style do
  begin
//    OwnerProps := not Options.CustomColors;
    FontName := CurrentFont.Caption;
    FontSize := CurrentFont.Font.Size;
    FontStyles := CurrentFont.Font.Style;
    TextBkColor := BkColor.SelectedColor;
    TextColor := FrColor.SelectedColor;
    CellBkColor := BkCellColor.SelectedColor;
  end;
//  frmNoteBook.NoteTree.ItemFocused.MainStyle := Style;
  Style := frmNoteBook.NoteTree.ItemFocused.AddStyle;
  Close;
end;

procedure TSettings.BkColorChange(Sender: TObject);
begin
  CurrentFont.Color := BkColor.SelectedColor;
end;

procedure TSettings.FrColorChange(Sender: TObject);
begin
  CurrentFont.Font.Color := FrColor.SelectedColor;
end;

procedure TSettings.BkCellColorChange(Sender: TObject);
begin
  GroupBox.Color := BkCellColor.SelectedColor;
end;

end.
