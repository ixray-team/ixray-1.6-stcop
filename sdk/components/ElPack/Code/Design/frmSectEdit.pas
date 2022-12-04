
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit frmSectEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ElHeader, StdCtrls, ExtCtrls, Menus;

type
  TSectEdit = class(TForm)
    Label6 : TLabel;
    ImIndexEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    TextEB: TEdit;
    Label5: TLabel;
    FieldEdit: TEdit;
    Label11: TLabel;
    HintEdit: TEdit;
    Label8: TLabel;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    WidthEB: TEdit;
    MinWidthEB: TEdit;
    MaxWidthEB: TEdit;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    GroupBox3: TGroupBox;
    Panel1: TPanel;
    Label4: TLabel;
    Bevel1: TBevel;
    StyleCombo: TComboBox;
    ColTypeCB: TComboBox;
    PopupCombo: TComboBox;
    ParentCombo: TComboBox;
    ExpandableCB: TCheckBox;
    ExpandedCB: TCheckBox;
    FilterCB: TCheckBox;
    LookupCB: TCheckBox;
    ClickCB: TCheckBox;
    ClickSelCB: TCheckBox;
    ResizeCB: TCheckBox;
    PswCB: TCheckBox;
    EditCB: TCheckBox;
    AlignRG: TRadioGroup;
    LayoutRG: TRadioGroup;
    ImAlignRG: TRadioGroup;
    SortRG: TRadioGroup;
    ElPopupButton1: TButton;
    ElPopupButton2: TButton;
    VisCB: TCheckBox;
    AutosizeCB: TCheckBox;
    ShowSortMarkCB: TCheckBox;
    procedure ExpandableCBClick(Sender : TObject);
    procedure FilterCBClick(Sender : TObject);
    procedure LookupCBClick(Sender : TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Item : TElHeaderSection;
    Items : TElHeaderSections;
    Form : TCustomForm;
    procedure SetData;
    procedure GetData;
  end;

var
  SectEdit : TSectEdit;

implementation

{$R *.DFM}

procedure TSectEdit.SetData;
var
  s : string;
  i : integer;
  Section1,
    Section : TElHeaderSection;

begin
  TextEB.Text := Item.Text;
  str(Item.Width, s);
  WidthEB.Text := s;
  str(Item.MinWidth, s);
  MinWidthEB.Text := s;
  str(Item.MaxWidth, s);
  MaxWidthEB.Text := s;
  case Item.Alignment of
    hsaLeft : i := 0;
    hsaCenter : i := 1;
    hsaRight : i := 2;
  else
    i := 0;
  end; // case
  AlignRG.ItemIndex := i;

  case Item.TextLayout of
    tlTop : i := 0;
    tlCenter : i := 1;
    tlBottom : i := 2;
  else
    i := 0;
  end; // case
  LayoutRG.ItemIndex := i;

  case Item.SortMode of
    hsmNone : i := 0;
    hsmAscend : i := 1;
    hsmDescend : i := 2;
  end; // case
  SortRG.ItemIndex := i;
  StyleCombo.ItemIndex := integer(Item.Style);
  VisCB.Checked := Item.Visible;
  ClickCB.Checked := Item.AllowClick;
  EditCB.Checked := Item.Editable;
  FieldEdit.Text := Item.FieldName;
  ImIndexEdit.Text := IntToStr(Item.ImageIndex);
  case Item.PictureAlign of
    hsaLeft : i := 0;
    hsaCenter : i := 1;
    hsaRight : i := 2;
  end;
  ImAlignRG.ItemIndex := i;
  case Item.FieldType of
    sftCustom : i := 0;
    sftText : i := 1;
    sftNumber : i := 2;
    sftFloating : i := 3;
    sftDateTime : i := 4;
    sftDate : i := 5;
    sftTime : i := 6;
    sftPicture : i := 7;
    sftEnum : i := 8;
    sftBLOB : i := 9;
    sftBool: i := 10;
    sftCurrency: i := 11;
  end;
  ColTypeCB.ItemIndex := i;
  PswCB.Checked := Item.Password;
  ResizeCB.Checked := Item.Resizable;
  ParentCombo.Items.AddObject('(no parent)', nil);
  ParentCombo.ItemIndex := 0;
  for i := 0 to Items.Count - 1 do
  begin
    Section := Items[i];
    if (Section <> Item) and (Section.Expandable) then
    begin
      Section1 := Section;
      while (Section1 <> Item) and (Section1.ParentSection <> nil) do
        Section1 := Section1.ParentSection;
      if Section1 <> Item then
      begin
        ParentCombo.Items.AddObject(Section.Text, Section);
        if Section = Item.ParentSection then ParentCombo.ItemIndex := ParentCombo.Items.Count - 1;
      end;
    end;
  end;
  PopupCombo.Items.AddObject('(no menu)', nil);
  PopupCombo.ItemIndex := 0;
  if Form <> nil then
   for i := 0 to Form.ComponentCount - 1 do
   begin
     if (Form.Components[i] is TPopupMenu) and (TPopupMenu(Form.Components[i]).Name <> '') then
     begin
       PopupCombo.Items.AddObject(TPopupMenu(Form.Components[i]).Name, Form.Components[i]);
       if Form.Components[i] = Item.PopupMenu then PopupCombo.ItemIndex := PopupCombo.Items.Count - 1;
     end;
   end;
  LookupCB.Checked := Item.LookupEnabled;
  ExpandableCB.Checked := Item.Expandable;
  ExpandedCB.Checked := Item.Expanded;
  ClickSelCB.Checked := Item.ClickSelect;
  FilterCB.Checked := Item.FilterEnabled;
  AutoSizeCB.Checked := Item.AutoSize;
  ShowSortMarkCB.Checked := Item.ShowSortMark;
  HintEdit.Text := Item.Hint;
end;

procedure TSectEdit.GetData;
var
  i, c : integer;
  s : string;
begin
  Item.Text := TextEB.Text;
  Item.Visible := VisCB.Checked;

  // set section width
  s := WidthEB.Text;
  val(s, i, c);
  if c = 0 then Item.Width := i;

  // set section minimal width
  s := MinWidthEB.Text;
  val(s, i, c);
  if c = 0 then Item.MinWidth := i;

  // set section maximal width
  s := MaxWidthEB.Text;
  val(s, i, c);
  if c = 0 then Item.MaxWidth := i;

  // set alignment
  case AlignRG.ItemIndex of //
    0 : Item.Alignment := hsaLeft;
    1 : Item.Alignment := hsaCenter;
    2 : Item.Alignment := hsaRight;
  end; // case

  case LayoutRG.ItemIndex of
    0 : Item.TextLayout := tlTop;
    1 : Item.TextLayout := tlCenter;
    2 : Item.TextLayout := tlBottom;
  end; // case

  case SortRG.ItemIndex of //
    0 : Item.SortMode := hsmNone;
    1 : Item.SortMode := hsmAscend;
    2 : Item.SortMode := hsmDescend;
  end; // case
  Item.Style := TElSectionStyle(StyleCombo.ItemIndex);
  Item.AllowClick := ClickCB.Checked;
  Item.Editable := EditCB.Checked;
  s := ImIndexEdit.Text;
  val(s, i, c);
  if c = 0 then Item.ImageIndex := i;
  Item.FieldName := FieldEdit.Text;
  case ImAlignRG.ItemIndex of //
    0 : Item.PictureAlign := hsaLeft;
    1 : Item.PictureAlign := hsaCenter;
    2 : Item.PictureAlign := hsaRight;
  end; // case
  with Item do
    case ColTypeCB.ItemIndex of
      0 : FieldType := sftCustom;
      1 : FieldType := sftText;
      2 : FieldType := sftNumber;
      3 : FieldType := sftFloating;
      4 : FieldType := sftDateTime;
      5 : FieldType := sftDate;
      6 : FieldType := sftTime;
      7 : FieldType := sftPicture;
      8 : FieldType := sftEnum;
      9 : FieldType := sftBLOB;
      10 : FieldType := sftBool;
      11 : FieldType := sftCurrency;
    end;
  Item.ClickSelect := ClickSelCB.Checked;
  Item.Password := PswCB.Checked;
  Item.Resizable := ResizeCB.Checked;
  Item.LookupEnabled := LookupCB.Checked;
  Item.Expandable := ExpandableCB.Checked;
  Item.Expanded := ExpandedCB.Checked;
  Item.FilterEnabled := FilterCB.Checked;
  Item.Hint := HintEdit.Text;
  Item.AutoSize := AutoSizeCB.Checked;
  Item.ShowSortMark := ShowSortMarkCB.Checked;

  if ParentCombo.ItemIndex <> -1 then
    Item.ParentSection := TElHeaderSection(ParentCombo.Items.Objects[ParentCombo.ItemIndex])
  else
    Item.ParentSection := nil;
  if PopupCombo.ItemIndex <> -1 then
    Item.PopupMenu := TPopupMenu(PopupCombo.Items.Objects[PopupCombo.ItemIndex])
  else
    Item.PopupMenu := nil;
end;

procedure TSectEdit.ExpandableCBClick(Sender : TObject);
begin
  ExpandedCB.Enabled := ExpandableCB.Checked;
end;

procedure TSectEdit.FilterCBClick(Sender : TObject);
begin
  if FilterCB.Checked then LookupCB.Checked := false;
end;

procedure TSectEdit.LookupCBClick(Sender : TObject);
begin
  if LookupCB.Checked then FilterCB.Checked := false;
end;

end.
