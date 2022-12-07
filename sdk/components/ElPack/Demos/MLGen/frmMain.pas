
{*******************************************************}
{                                                       }
{  EldoS Markup Language Generator (MlGen) Demo         }
{                                                       }
{  Copyright (c) 1999-2001 Mikhail Chernyshev           }
{                                                       }
{*******************************************************}

unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Grids, Spin, clMlDemo;

type
  TMainForm = class(TForm)
    StatusLine: TStatusBar;
    Panel2: TPanel;
    LinesOnPageEdit: TSpinEdit;
    Label3: TLabel;
    HeaderCheckBox: TCheckBox;
    FooterCheckBox: TCheckBox;
    GenerateButton: TButton;
    MultipageCheckBox: TCheckBox;
    OutputFilesEdit: TEdit;
    Label4: TLabel;
    BrowseOutputButton: TButton;
    OpenTemplateDialog: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    StringGrid: TStringGrid;
    AddRowBtn: TButton;
    DeleteRowBtn: TButton;
    AddColBtn: TButton;
    DeleteColBtn: TButton;
    Label2: TLabel;
    TemplateEdit: TEdit;
    BrowseTemplateBtn: TButton;
    Label5: TLabel;
    TemplateParamsListBox: TListBox;
    Label6: TLabel;
    TranslationTableNamesListBox: TListBox;
    TranslationTableListBox: TListBox;
    OpenResultBtn: TButton;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure MultipageCheckBoxClick(Sender: TObject);
    procedure BrowseTemplateBtnClick(Sender: TObject);
    procedure BrowseOutputButtonClick(Sender: TObject);
    procedure AddRowBtnClick(Sender: TObject);
    procedure DeleteRowBtnClick(Sender: TObject);
    procedure AddColBtnClick(Sender: TObject);
    procedure DeleteColBtnClick(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure TemplateEditChange(Sender: TObject);
    procedure TranslationTableNamesListBoxClick(Sender: TObject);
    procedure OutputFilesEditChange(Sender: TObject);
    procedure OpenResultBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure FillStringGrid;
    procedure FillStringGridRow(aRow : Integer);
    procedure FillStringGridCol(aCol : Integer);
    procedure UpdateFixedRow;
    procedure UpdateFixedCol;
    procedure ChangeTranslationTable;
  public
    { Public declarations }
    DemoMlGen : TDemoMlGen;
  end;

var
  MainForm: TMainForm;
  ExePath : string;

implementation
uses ElMlGen, ShellAPI;
{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DemoMlGen := TDemoMlGen.Create(nil);
//  DemoMlGen.TagPrefix := '#';
  ExePath := CheckPath(ExtractFilePath(Application.ExeName), True);
  TemplateEdit.Text := ExePath + 'Templates\FullListTxt.templ';
  MultipageCheckBoxClick(nil);
  FillStringGrid;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DemoMlGen.Free;
end;

procedure TMainForm.ShowHint(Sender: TObject);
begin
  with StatusLine do begin
    Panels[0].Text := Application.Hint;
    Update;
  end;
end;

procedure TMainForm.MultipageCheckBoxClick(Sender: TObject);
var
  b : boolean;
begin
  b := MultipageCheckBox.Checked;
  Label3.Enabled := b;
  LinesOnPageEdit.Enabled := b;
end;

procedure TMainForm.BrowseTemplateBtnClick(Sender: TObject);
begin
  OpenTemplateDialog.InitialDir := ExtractFilePath(TemplateEdit.Text);
  OpenTemplateDialog.FileName := TemplateEdit.Text;
  if not OpenTemplateDialog.Execute then exit;
  TemplateEdit.Text := OpenTemplateDialog.FileName;
end;

procedure TMainForm.BrowseOutputButtonClick(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(OutputFilesEdit.Text);
  SaveDialog1.FileName := OutputFilesEdit.Text;
  if not SaveDialog1.Execute then exit;
  OutputFilesEdit.Text := SaveDialog1.FileName;
end;

procedure TMainForm.FillStringGrid;
var
  i : Integer; // Counters
begin
  UpdateFixedCol;
  UpdateFixedRow;
  for i := 1 to StringGrid.RowCount - 1 do
    FillStringGridRow(i);
end;

procedure TMainForm.FillStringGridRow(aRow: Integer);
var
  i : Integer; // Counters
begin
  for i := 1 to StringGrid.ColCount - 1 do
    StringGrid.Cells[i, aRow] := Format('Sample [%d, %d]', [i, aRow]);
end;

procedure TMainForm.FillStringGridCol(aCol: Integer);
var
  i : Integer; // Counters
begin
  for i := 1 to StringGrid.RowCount - 1 do
    StringGrid.Cells[aCol, i] := Format('Sample [%d, %d]', [aCol, i]);
end;

procedure TMainForm.UpdateFixedCol;
var
  i : Integer;
begin
  for i := 1 to StringGrid.RowCount - 1 do
    StringGrid.Cells[0, i] := Format('%d:', [i]);
end;

procedure TMainForm.UpdateFixedRow;
var
  i : Integer;
begin
  for i := 1 to StringGrid.ColCount - 1 do
    StringGrid.Cells[i, 0] := Format('Col %d', [i]);
end;

procedure TMainForm.AddRowBtnClick(Sender: TObject);
begin
  StringGrid.RowCount := StringGrid.RowCount + 1;
  FillStringGridRow(StringGrid.RowCount - 1);
  UpdateFixedCol;
end;

procedure TMainForm.DeleteRowBtnClick(Sender: TObject);
var
  i, j, k : Integer; // Counters
begin
  if StringGrid.RowCount <= 1 then
    exit;
  k := StringGrid.Row;
  for i := k to StringGrid.RowCount - 2 do
  begin
    for j := 0 to StringGrid.ColCount - 1 do
      StringGrid.Cells[j, i] := StringGrid.Cells[j, i+1];
  end; // for
  StringGrid.RowCount := StringGrid.RowCount - 1;
  UpdateFixedCol;
end;

procedure TMainForm.AddColBtnClick(Sender: TObject);
begin
  StringGrid.ColCount := StringGrid.ColCount + 1;
  FillStringGridCol(StringGrid.ColCount - 1);
  UpdateFixedRow;
end;

procedure TMainForm.DeleteColBtnClick(Sender: TObject);
var
  i, j, k : Integer; // Counters
begin
  if StringGrid.ColCount <= 1 then
    exit;
  k := StringGrid.Col;
  for i := k to StringGrid.ColCount - 2 do
  begin
    for j := 0 to StringGrid.RowCount - 1 do
      StringGrid.Cells[i, j] := StringGrid.Cells[i+1, j];
  end; // for
  StringGrid.ColCount := StringGrid.ColCount - 1;
  UpdateFixedRow;
end;

procedure TMainForm.GenerateButtonClick(Sender: TObject);
begin
  DemoMlGen.Execute;
  OutputFilesEditChange(nil);
end;

procedure TMainForm.TemplateEditChange(Sender: TObject);
begin
  if FileExists(TemplateEdit.Text) then
  begin
    DemoMlGen.Template.LoadFromFile(TemplateEdit.Text);
  end
  else begin
    DemoMlGen.Template.Text := '';
  end;
  // Params
  DemoMlGen.Parameters.AssignTo(TemplateParamsListBox.Items);
  // Translation tables
  TranslationTableNamesListBox.Clear;
  DemoMlGen.TranslationTables.GetTableNames(TranslationTableNamesListBox.Items);
  TranslationTableNamesListBox.ItemIndex := DemoMlGen.TranslationTables.DefaultTable;
  ChangeTranslationTable;
  OutputFilesEdit.Text := ExePath + 'Generated data\' + DemoMlGen.Parameters.GetValueByNameEx('OutputFileName', 'SampleData.txt');
end;

procedure TMainForm.TranslationTableNamesListBoxClick(Sender: TObject);
begin
  ChangeTranslationTable;
end;

procedure TMainForm.ChangeTranslationTable;
var
  TranslationTable : TTranslationTable;
begin
  TranslationTableListBox.Clear;
  if TranslationTableNamesListBox.ItemIndex >= 0 then
  begin
    TranslationTable := DemoMlGen.TranslationTables[TranslationTableNamesListBox.ItemIndex];
    TranslationTable.Table.AssignTo(TranslationTableListBox.Items);
  end;  
end;

procedure TMainForm.OutputFilesEditChange(Sender: TObject);
begin
  OpenResultBtn.Enabled := FileExists(OutputFilesEdit.Text);
end;

procedure TMainForm.OpenResultBtnClick(Sender: TObject);
begin
  ShellExecute (Application.Handle, 'open', PChar(OutputFilesEdit.Text), nil, nil, SW_NORMAL);
end;

end.
