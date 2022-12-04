
{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit frmItemCol;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ElTree, TypInfo, 
{$ifdef VCL_6_USED}
  DesignIntf, DesignEditors, DesignWindows, DsnConst,  
{$else}
  DsgnIntf, 
{$endif}
  ElTools, ComCtrls;

type
  TItemColDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label2: TLabel;
    TextEdit: TMemo;
    Label13: TLabel;
    HintEdit: TMemo;
    ColTextMemo: TMemo;
    Label1: TLabel;
    StylesCB: TCheckBox;
    StylesGB: TGroupBox;
    BoldCB: TCheckBox;
    ItCB: TCheckBox;
    ULCB: TCheckBox;
    StrikeCB: TCheckBox;
    ColorsCB: TCheckBox;
    ColorsGB: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label12: TLabel;
    ColorCombo: TComboBox;
    BkColorCombo: TComboBox;
    RowBkColorCombo: TComboBox;
    UseBkColorCB: TCheckBox;
    ShowChecksCB: TCheckBox;
    CBGroup: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    CBTypeCombo: TComboBox;
    CBStateCombo: TComboBox;
    CBEnabledCB: TCheckBox;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    StIndexEdit: TEdit;
    IndexEdit: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Index2Edit: TEdit;
    StIndex2Edit: TEdit;
    TabSheet3: TTabSheet;
    ForcedBtnsCB: TCheckBox;
    EnabledCB: TCheckBox;
    HiddenCB: TCheckBox;
    HtmlCB: TCheckBox;
    Label14: TLabel;
    TagEdit: TEdit;
    StrikeOutCB: TCheckBox;
    StrikeLineColorCB: TComboBox;
    HorZlineCB: TCheckBox;
    AllowEditCB: TCheckBox;
    SuppressButtonsCB: TCheckBox;
    MultilineCB: TCheckBox;
    OwnHeightCB: TCheckBox;
    HeightEdit: TEdit;
    IndentEdit: TEdit;
    IndentAdjustCB: TCheckBox;
    BorderStyleCombo: TComboBox;
    Label15: TLabel;
    SuppressLinesCB: TCheckBox;
    Label16: TLabel;
    OvIndexEdit: TEdit;
    OvIndex2Edit: TEdit;
    procedure ColorsCBClick(Sender : TObject);
    procedure StylesCBClick(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : Boolean);
    procedure OKBtnClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure CancelBtnClick(Sender : TObject);
    procedure ShowChecksCBClick(Sender : TObject);
    procedure CBTypeComboChange(Sender : TObject);
    procedure StrikeOutCBClick(Sender: TObject);
    procedure OwnHeightCBClick(Sender: TObject);
    procedure IndentAdjustCBClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Item : TElTreeItem;
    ByCancel : boolean;
    procedure SetData;
    procedure GetData;
  end;

var
  ItemColDlg : TItemColDlg;

implementation

{$R *.DFM}

procedure TItemColDlg.SetData;
{$ifdef ELPACK_UNICODE}
var i : integer;
{$endif}
begin
  ColorCombo.Text := ColorToString(Item.Color);
  BkColorCombo.Text := ColorToString(Item.BkColor);
  RowBkColorCombo.Text := ColorToString(Item.RowBkColor);

  ColorsCB.Checked := Item.ParentColors;
  ColorsGB.Enabled := not ColorsCB.Checked;
  UseBkColorCB.Checked := Item.UseBkColor;
  StylesCB.Checked := Item.ParentStyle;
  StylesGB.Enabled := not StylesCB.Checked;
  TextEdit.Text := Item.Text;
  HintEdit.Text := Item.Hint;
  {$ifdef ELPACK_UNICODE}
  ColTextMemo.Lines.Clear;
  for i := 0 to Item.ColumnText.Count -1 do
  begin
    ColTextMemo.Lines.Add(Item.ColumnText[i]);
  end;
  {$else}
  ColTextMemo.Lines.Assign(Item.ColumnText);
  {$endif}
  ItCB.Checked := Item.Italic;
  ULCB.Checked := Item.Underlined;
  BoldCB.Checked := Item.Bold;
  StrikeCB.Checked := Item.Strikeout;
  ForcedBtnsCB.Checked := Item.ForceButtons;
  IndexEdit.Text := IntToStr(Item.ImageIndex);
  StIndexEdit.Text := IntToStr(Item.StateImageIndex);
  Index2Edit.Text := IntToStr(Item.ImageIndex2);
  StIndex2Edit.Text := IntToStr(Item.StateImageIndex2);
  OvIndexEdit.Text := IntToStr(Item.OverlayIndex);
  OvIndex2Edit.Text := IntToStr(Item.OverlayIndex2);
  CBStateCombo.ItemIndex := Integer(Item.CheckBoxState);
  CBTypeCombo.ItemIndex := Integer(Item.CheckBoxType);
  CBEnabledCB.Checked := Item.CheckBoxEnabled;
  ShowChecksCB.Checked := Item.ShowCheckBox;
  HiddenCB.Checked := Item.Hidden;

  EnabledCB.Checked := Item.Enabled;
  {$ifdef HAS_HTML_RENDER}
  HTMLCB.Checked := Item.IsHTML;
  {$endif}
  MultilineCB.Checked := Item.Multiline;
  HorzLineCB.Checked := Item.DrawHLine;
  AllowEditCB.Checked := Item.AllowEdit;
  SuppressButtonsCB.Checked :=Item.SuppressButtons;
  SuppressLinesCB.Checked :=Item.SuppressLines;
  StrikeOutCB.Checked := Item.StrikedOutLine;
  StrikeLineColorCB.Enabled := StrikeOutCB.Checked;
  StrikeLineColorCB.Text := ColorToString(Item.StrikedLineColor);

  OwnHeightCB.Checked := not Item.OwnerHeight;
  HeightEdit.Enabled := OwnHeightCB.Checked;
  HeightEdit.Text := IntToStr(Item.Height);

  IndentAdjustCB.Checked := Item.IndentAdjust <> 0;
  IndentEdit.Enabled := IndentAdjustCB.Checked;
  IndentEdit.Text := IntToStr(Item.IndentAdjust);

  BorderStyleCombo.ItemIndex := Ord(Item.BorderStyle);

  TagEdit.Text := IntToStr(Item.Tag);
end;

procedure TItemColDlg.GetData;
var
  I, J : integer;
  S : string;
begin
  Item.ParentColors := ColorsCB.Checked;
  if not ColorsCB.Checked then
  begin
    IdentToColor(ColorCombo.Text, i);
    Item.Color := TColor(i);
    IdentToColor(BkColorCombo.Text, i);
    Item.BkColor := TColor(i);
    IdentToColor(RowBkColorCombo.Text, i);
    Item.RowBkColor := TColor(i);
    Item.UseBkColor := UseBkColorCB.Checked;
  end;
  
  Item.ParentStyle := StylesCB.Checked;
  Item.Text := TextEdit.Text;
  Item.Hint := HintEdit.Text;
  Item.ColumnText.Assign(ColTextMemo.Lines);
  Item.Italic := ItCB.Checked;
  Item.Underlined := ULCB.Checked;
  Item.Bold := BoldCB.Checked;
  Item.Strikeout := StrikeCB.Checked;
  Item.ForceButtons := ForcedBtnsCB.Checked;
  S := IndexEdit.Text;
  val(S, I, J);
  if J = 0 then Item.ImageIndex := I;
  S := StIndexEdit.Text;
  val(S, I, J);
  if J = 0 then Item.StateImageIndex := I;
  S := OvIndexEdit.Text;
  val(S, I, J);
  if J = 0 then Item.OverlayIndex := I;

  S := Index2Edit.Text;
  val(S, I, J);
  if J = 0 then Item.ImageIndex2 := I;
  S := StIndex2Edit.Text;
  val(S, I, J);
  if J = 0 then Item.StateImageIndex2 := I;
  S := OvIndex2Edit.Text;
  val(S, I, J);
  if J = 0 then Item.OverlayIndex2 := I;

  Item.CheckBoxState := TCheckBoxState(CBStateCombo.ItemIndex);
  Item.CheckBoxType := TElCheckBoxType(CBTypeCombo.ItemIndex);
  Item.ShowCheckBox := ShowChecksCB.Checked;
  Item.CheckBoxEnabled := CBEnabledCB.Checked;
  Item.Hidden := HiddenCB.Checked;

  Item.Enabled := EnabledCB.Checked;
{$ifdef HAS_HTML_RENDER}
  Item.IsHTML := HTMLCB.Checked;
{$endif}
  Item.Multiline := MultilineCB.Checked;
  Item.DrawHLine := HorzLineCB.Checked;
  Item.AllowEdit := AllowEditCB.Checked;
  Item.SuppressButtons := SuppressButtonsCB.Checked;
  Item.SuppressLines := SuppressLinesCB.Checked;

  Item.StrikedOutLine := StrikeOutCB.Checked;
  IdentToColor(StrikeLineColorCB.Text, i);
  if Item.StrikedOutLine then
    Item.StrikedLineColor := i;

  Item.OwnerHeight := not OwnHeightCB.Checked;
  if not Item.OwnerHeight then
    Item.Height := StrToIntDef(HeightEdit.Text, Item.Height);

  if IndentAdjustCB.Checked then
    Item.IndentAdjust := StrToIntDef(IndentEdit.Text, Item.IndentAdjust)
  else
    Item.IndentAdjust := 0;

  Item.BorderStyle := TElItemBorderStyle(Max(BorderStyleCombo.ItemIndex, 0));

  Item.Tag := StrtoIntDef(TagEdit.Text, Item.Tag);
end;

procedure TItemColDlg.ColorsCBClick(Sender : TObject);
begin
  ColorsGB.Enabled := not ColorsCB.Checked;
  ColorCombo.Enabled := ColorsGB.Enabled;
  BkColorCombo.Enabled := ColorsGB.Enabled;
  RowBkColorCombo.Enabled := ColorsGB.Enabled;
end;

procedure TItemColDlg.StylesCBClick(Sender : TObject);
begin
  StylesGB.Enabled := not StylesCB.Checked;
  ItCB.enabled := StylesGB.Enabled;
  BoldCB.enabled := StylesGB.Enabled;
  StrikeCB.enabled := StylesGB.Enabled;
  ULCB.enabled := StylesGB.Enabled;
end;

{$HINTS off}

procedure TItemColDlg.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
var
  C : integer;
  s : string;
  i, j : integer;
begin
  if (ByCancel) or (ColorsCB.Checked) then
  begin
    CanClose := true;
    exit;
  end;
  CanClose := false;
  if IdentToColor(ColorCombo.Text, C) and IdentToColor(BkColorCombo.Text, C) then
    CanClose := true
  else
  begin
    MessageBox(Handle, 'Invalid color property', 'Error', mb_IconError or mb_Ok);
    ByCancel := true;
    exit;
  end;
  S := IndexEdit.Text;
  val(S, I, J);
  if J > 0 then CanClose := false;
  S := StIndexEdit.Text;
  val(S, I, J);
  if J > 0 then CanClose := false;
  if not CanClose then
  begin
    MessageBox(Handle, 'Invalid image index property', 'Error', mb_IconError or mb_Ok);
    ByCancel := true;
  end;
end;
{$HINTS on}

procedure TItemColDlg.OKBtnClick(Sender : TObject);
begin
  ByCancel := false;
end;

procedure TItemColDlg.FormCreate(Sender : TObject);
begin
  ByCancel := true;
end;

procedure TItemColDlg.CancelBtnClick(Sender : TObject);
begin
  ByCancel := true;
end;

procedure TItemColDlg.ShowChecksCBClick(Sender : TObject);
var
  i : integer;
  b : boolean;
begin
  CBGroup.Enabled := ShowChecksCB.Checked;
  b := ShowChecksCB.Checked;
  for i := 0 to CBGroup.ControlCount - 1 do
  begin
    CBGroup.Controls[i].Enabled := b;
  end;
end;

procedure TItemColDlg.CBTypeComboChange(Sender : TObject);
begin
  if (CBTypeCombo.ItemIndex <> 1) then
  begin
    if CBStateCombo.Items.Count = 3 then CBStateCombo.Items.Delete(2);
  end
  else
  begin
    if CBStateCombo.Items.Count = 2 then CBStateCombo.items.Add('Grayed');
  end;
end;

procedure TItemColDlg.StrikeOutCBClick(Sender: TObject);
begin
  StrikeLineColorCB.Enabled := StrikeOutCB.Checked;
end;

procedure TItemColDlg.OwnHeightCBClick(Sender: TObject);
begin
  HeightEdit.Enabled := OwnHeightCB.Checked;
end;

procedure TItemColDlg.IndentAdjustCBClick(Sender: TObject);
begin
  IndentEdit.Enabled := IndentAdjustCB.Checked;
end;

end.
