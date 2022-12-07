
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit frmSectProp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DesignIntf, DesignEditors, DesignWindows, DsnConst,  
  ElTools, ElHeader, StdCtrls, frmSectEdit, ExtCtrls, ElXPThemedControl;

type
  TElSectionsPropDlg = class(TForm)
    OpenDlg : TOpenDialog;
    SaveDlg : TSaveDialog;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    SecList: TListBox;
    Panel2: TPanel;
    AddBtn: TButton;
    DeleteBtn: TButton;
    EditBtn: TButton;
    UpBtn: TButton;
    DownBtn: TButton;
    LoadBtn: TButton;
    SaveBtn: TButton;
    TestHeader: TElHeader;
    DuplicateBtn: TButton;
    ReindexBtn: TButton;
    procedure LoadBtnClick(Sender : TObject);
    procedure SaveBtnClick(Sender : TObject);
    procedure EditBtnClick(Sender : TObject);
    procedure AddBtnClick(Sender : TObject);
    procedure DeleteBtnClick(Sender : TObject);
    procedure UpBtnClick(Sender : TObject);
    procedure DownBtnClick(Sender : TObject);
    procedure SecListClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure SecListKeyPress(Sender : TObject; var Key : Char);
    procedure SecListDblClick(Sender: TObject);
    procedure DuplicateBtnClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ReindexBtnClick(Sender: TObject);
  protected
    //Designer : TFormDesigner;
  public
    ASect : TElHeaderSections;
    procedure FillSecList;
    procedure SetData;
    procedure GetData;
    constructor Create(AOwner: TComponent); override;
  end;

  TElSectionsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
  end;

  TElHeaderEditor = class(TDefaultEditor)
  public
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; override;
    function GetVerbCount: integer; override;
  end;

var
  ElSectionsPropDlg : TElSectionsPropDlg;

implementation

{$R *.DFM}

procedure TElSectionsProperty.Edit;
var
  Editor : TElSectionsPropDlg;
  Form : TCustomForm;
begin
  Editor := nil;
  try
    Editor := TElSectionsPropDlg.Create(Application);
    Editor.ASect := TElHeaderSections(GetOrdValue);
    Editor.SetData;
    Editor.Caption := Format('Editing %s.Sections', [Editor.ASect.Owner.Name]);
    if Editor.ShowModal = mrOk then
    begin
      Editor.GetData;
      Form := GetParentForm(Editor.ASect.Owner);
      if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
    end;
  finally
    Editor.Free;
  end;
end;

function TElSectionsProperty.GetAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TElSectionsProperty.GetValue;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

type THackElHeaderSections = class(TElHeaderSections);

procedure TElSectionsPropDlg.FillSecList;
var j : integer;
    S : string;
    Section : TElHeaderSection;
begin
  for j := 0 to TestHeader.Sections.Count - 1 do
  begin
    Section := TestHeader.Sections.ItemByPos[j];
    S := IntToStr(Section.Index) + ' - ';
    if Section.Text = '' then
      S := S + '(Untitled)'
    else
      S := S + Section.Text;
    SecList.Items.AddObject(S, Section);
  end;
end;

procedure TElSectionsPropDlg.SetData;
begin
  TestHeader.Sections.Assign(ASect);
  if THackElHeaderSections(ASect).FOwner <> nil then
    TestHeader.MultiSort := TElHeader(THackElHeaderSections(ASect).FOwner).Multisort;
  FillSecList;
end;

procedure TElSectionsPropDlg.GetData;
begin
  ASect.Assign(TestHeader.Sections);
end;

constructor TElSectionsPropDlg.Create(AOwner: TComponent);
begin
  Inherited;
  TestHeader := TElHeader.Create(AOwner);
end;

procedure TElSectionsPropDlg.LoadBtnClick(Sender : TObject);
var
  j : integer;
  Form : TCustomForm;
  S: string;
  Section : TElHeaderSection;
begin
  if not OpenDlg.Execute then exit;
  TestHeader.Sections.LoadFromFile(OpenDlg.FileName);
  SecList.Items.Clear;
  for j := 0 to TestHeader.Sections.Count - 1 do // Iterate
  begin
    Section := TestHeader.Sections.ItemByPos[j];
    S := IntToStr(Section.Index) + ' - ';
    if Section.Text = '' then
      S := S + '(Untitled)'
    else
      S := S + Section.Text;
    SecList.Items.AddObject(S, Section);
  end;
  Form := GetParentForm(ASect.Owner);
  if (Form <> nil) and (Form.Designer <> nil) then
    Form.Designer.Modified;
end;

procedure TElSectionsPropDlg.SaveBtnClick(Sender : TObject);
begin
  if not SaveDlg.Execute then exit;
  TestHeader.Sections.SaveToFile(SaveDlg.FileName);
end;

procedure TElSectionsPropDlg.EditBtnClick(Sender : TObject);
var
  SectDlg : TSectEdit;
  THS : TElHeaderSection;
  S: string;
begin
  SectDlg := nil;
  if SecList.ItemIndex = -1 then exit;
  try
    SectDlg := TSectEdit.Create(self);
    THS := TElHeaderSection(SecList.Items.Objects[SecList.ItemIndex]);
    SectDlg.Item := THS;
    SectDlg.Items := TestHeader.Sections;
    SectDlg.Form := GetParentForm(ASect.Owner);
    SectDlg.SetData;
    if SectDlg.ShowModal = mrOk then
    begin
      SectDlg.GetData;
      S := IntToStr(THS.Index) + ' - ';
      if THS.Text = '' then
        S := S + '(Untitled)'
      else
        S := S + THS.Text;
      SecList.Items[SecList.ItemIndex] := S;
    end;
  finally
    SectDlg.Free;
  end;
  SecList.SetFocus;
end;

procedure TElSectionsPropDlg.AddBtnClick(Sender : TObject);
var
  THS : TElHeaderSection;
begin
  THS := TestHeader.Sections.AddSection;
  THS.Text := '';
  SecList.ItemIndex := SecList.Items.AddObject(IntToStr(THS.Index) + ' - (Untitled)', THS);
  SecList.SetFocus;
  SecListClick(Sender);
end;

procedure TElSectionsPropDlg.DeleteBtnClick(Sender : TObject);
var
  THS : TElHeaderSection;
  j : integer;
  i : integer;
  S: string;
  Section : TElHeaderSection;
begin
  if SecList.ItemIndex = -1 then exit;
  i := SecList.ItemIndex;
  THS := TElHeaderSection(SecList.Items.Objects[SecList.ItemIndex]);
  TestHeader.Sections.DeleteSection(THS);
  SecList.Items.Clear;
  for j := 0 to TestHeader.Sections.Count - 1 do // Iterate
  begin
    Section := TestHeader.Sections.ItemByPos[j];
    S := IntToStr(Section.Index) + ' - ';
    if Section.Text = '' then
      S := S + '(Untitled)'
    else
      S := S + Section.Text;
    SecList.Items.AddObject(S, Section);
  end;
  SecList.ItemIndex := Min(i, SecList.Items.Count - 1);
  SecList.SetFocus;
  SecListClick(Sender);
end;

procedure TElSectionsPropDlg.UpBtnClick(Sender : TObject);
var
  i : integer;
  THS : TElHeaderSection;
begin
  if SecList.ItemIndex = -1 then exit;
  i := SecList.ItemIndex;
  THS := TElHeaderSection(SecList.Items.Objects[i]);
  SecList.Items.Exchange(i, i - 1);
  TestHeader.Sections.MoveSection(THS, i - 1);
  SecList.ItemIndex := i - 1;
  SecListClick(Self);
end;

procedure TElSectionsPropDlg.DownBtnClick(Sender : TObject);
var
  i : integer;
  THS : TElHeaderSection;
begin
  if SecList.ItemIndex = -1 then exit;
  i := SecList.ItemIndex;
  THS := TElHeaderSection(SecList.Items.Objects[i]);
  SecList.Items.Exchange(i, i + 1);
  TestHeader.Sections.MoveSection(THS, i + 1);
  SecList.ItemIndex := i + 1;
  SecListClick(Self);
end;

procedure TElSectionsPropDlg.SecListClick(Sender : TObject);
var
  b : boolean;
begin
  if (SecList.ItemIndex = 0) or (SecList.ItemIndex = -1) then
    UpBtn.Enabled := false
  else
    UpBtn.Enabled := true;
  b := SecList.ItemIndex <> -1;
  if SecList.ItemIndex = SecList.Items.Count - 1 then
    DownBtn.Enabled := false
  else
    DownBtn.Enabled := true;
  DeleteBtn.Enabled := b;
  EditBtn.Enabled := b;
  DuplicateBtn.Enabled := b;
end;

procedure TElSectionsPropDlg.FormCreate(Sender : TObject);
begin
  SecListClick(sender);
end;

procedure TElSectionsPropDlg.SecListKeyPress(Sender : TObject;
  var Key : Char);
begin
  SecListClick(sender);
end;

{ TElHeaderEditor }

procedure TElHeaderEditor.EditProperty(const Prop: IProperty; var Continue: Boolean); 
var
  PropName : string;
begin
  PropName := Prop.GetName;  
  if (CompareText(PropName, 'SECTIONS') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;


procedure TElHeaderEditor.ExecuteVerb(Index: integer);
begin
  Edit;
end;

function TElHeaderEditor.GetVerb(Index: integer): string;
begin
  Result := 'Se&ctions Editor...';
end;

function TElHeaderEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

procedure TElSectionsPropDlg.SecListDblClick(Sender: TObject);
begin
  EditBtnClick(Self);
end;

procedure TElSectionsPropDlg.DuplicateBtnClick(Sender: TObject);
var
  THS : TElHeaderSection;
  S    : string;
begin
  THS := TestHeader.Sections.AddSection;
  THS.Assign(TPersistent(SecList.Items.Objects[SecList.ItemIndex]));
  S := THS.Text;
  if S = '' then S := '(Untitled)';
  SecList.ItemIndex := SecList.Items.AddObject(IntToStr(THS.Index) + ' - ' + S, THS);
  SecList.SetFocus;
  SecListClick(Sender);
end;


procedure TElSectionsPropDlg.Button3Click(Sender: TObject);
var Form : TCustomForm;
begin
  ASect.Assign(TestHeader.Sections);
  Form := GetParentForm(ASect.Owner);
  if (Form <> nil) and (Form.Designer <> nil) then
    Form.Designer.Modified;
end;

procedure TElSectionsPropDlg.ReindexBtnClick(Sender: TObject);
begin
  TestHeader.Sections.Reindex;
  SecList.Items.Clear;
  FillSecList;
end;

end.
