unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElTools, ElPgCtl, ExtCtrls, ElPanel, StdCtrls, ElACtrls, ElSpin, ElBtnCtl,
  ElPopBtn, ElVCLUtils, ElCheckCtl, ElClrCmb, ElImgFrm, ElXPThemedControl,
  ElEdits;
                         
type
  TPgCtlDemoMainForm = class(TForm)
    ElPanel1: TElPanel;
    ElPanel2: TElPanel;
    PageCtl: TElPageControl;
    PagesSpin: TElSpinEdit;
    Label1: TLabel;
    StyleCombo: TElAdvancedComboBox;
    Label2: TLabel;
    TabPosCombo: TElAdvancedComboBox;
    Label3: TLabel;
    ShowTabsCB: TElCheckBox;
    ShowBorderCB: TElCheckBox;
    MultilineCB: TElCheckBox;
    FlatCB: TElCheckBox;
    RaggedRightCB: TElCheckBox;
    DrawFocusCB: TElCheckBox;
    Label4: TLabel;
    InactiveTabColorCombo: TElColorCombo;
    Label5: TLabel;
    ActiveTabColorCombo: TElColorCombo;
    Label6: TLabel;
    PageCtlColorCombo: TElColorCombo;
    Label7: TLabel;
    TabBackColorCombo: TElColorCombo;
    UseImageFormCB: TElCheckBox;
    ImageFrm: TElImageForm;
    UseXPStylesCB: TElCheckBox;
    procedure ShowBorderCBClick(Sender: TObject);
    procedure ShowTabsCBClick(Sender: TObject);
    procedure FlatCBClick(Sender: TObject);
    procedure RaggedRightCBClick(Sender: TObject);
    procedure MultilineCBClick(Sender: TObject);
    procedure TabPosComboChange(Sender: TObject);
    procedure StyleComboChange(Sender: TObject);
    procedure PagesSpinChange(Sender: TObject);
    procedure InactiveTabColorComboChange(Sender: TObject);
    procedure PageCtlColorComboChange(Sender: TObject);
    procedure TabBackColorComboChange(Sender: TObject);
    procedure ActiveTabColorComboChange(Sender: TObject);
    procedure UseImageFormCBClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DrawFocusCBClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure UseXPStylesCBClick(Sender: TObject);
  private
    { Private declarations }
  public
    FirstCall : boolean;
  end;

var
  PgCtlDemoMainForm: TPgCtlDemoMainForm;

implementation

{$R *.DFM}

procedure TPgCtlDemoMainForm.ShowBorderCBClick(Sender: TObject);
begin
  PageCtl.ShowBorder := ShowBorderCB.Checked;
  FlatCB.Enabled := PageCtl.ShowBorder or PageCtl.ShowTabs;
end;

procedure TPgCtlDemoMainForm.ShowTabsCBClick(Sender: TObject);
begin
  PageCtl.ShowTabs := ShowTabsCB.Checked;
  FlatCB.Enabled := PageCtl.ShowBorder or PageCtl.ShowTabs;
end;

procedure TPgCtlDemoMainForm.FlatCBClick(Sender: TObject);
begin
  PageCtl.Flat := FlatCB.Checked;
end;

procedure TPgCtlDemoMainForm.RaggedRightCBClick(Sender: TObject);
begin
  PageCtl.RaggedRight := RaggedRightCB.Checked;
end;

procedure TPgCtlDemoMainForm.MultilineCBClick(Sender: TObject);
begin
  try
    PageCtl.Multiline := MultilineCB.Checked;
  except
    MultilineCB.Checked := false;
  end;
  RaggedRightCB.Enabled := PageCtl.Multiline;
end;

procedure TPgCtlDemoMainForm.TabPosComboChange(Sender: TObject);
begin
  PageCtl.TabPosition := TElTabPosition(TabPosCombo.ItemIndex);
end;

procedure TPgCtlDemoMainForm.StyleComboChange(Sender: TObject);
begin
  try
    PageCtl.Style := TElTabStyle(StyleCombo.ItemIndex);
  except
    StyleCombo.ItemIndex := 0;
    StyleComboChange(StyleCombo);
  end;
end;

procedure TPgCtlDemoMainForm.PagesSpinChange(Sender: TObject);

  procedure AddPage;
  var ASheet : TElTabSheet;
      B      : TElPopupButton;
      i      : integer;
  begin
    ASheet := PageCtl.NewPage;
    ASheet.Caption := 'Page ' + IntToStr(ASheet.PageIndex + 1);
    for i := 0 to 4 do
    begin
      B := TElPopupButton.Create(ASheet);
      B.Caption := ASheet.Caption + ' Button';
      B.Parent := ASheet;
      B.Left := Random(PageCtl.ClientWidth - B.Width);
      B.Top := Random(PageCtl.ClientHeight - B.Height);
      B.Visible := true;
    end;
  end;

begin
  while PageCtl.PageCount > PagesSpin.Value do
    PageCtl.Pages[PageCtl.PageCount - 1].Free;
  while PageCtl.PageCount < PagesSpin.Value do
    AddPage;
end;

procedure TPgCtlDemoMainForm.InactiveTabColorComboChange(Sender: TObject);
begin
  PageCtl.InactiveTabColor := InactiveTabColorCombo.SelectedColor;
end;

procedure TPgCtlDemoMainForm.PageCtlColorComboChange(Sender: TObject);
begin
  PageCtl.Color := PageCtlColorCombo.SelectedColor;
end;

procedure TPgCtlDemoMainForm.TabBackColorComboChange(Sender: TObject);
begin
  PageCtl.TabBkColor := TabBackColorCombo.SelectedColor;
end;

procedure TPgCtlDemoMainForm.ActiveTabColorComboChange(Sender: TObject);
begin
  PageCtl.ActiveTabColor := ActiveTabColorCombo.SelectedColor;
end;

procedure TPgCtlDemoMainForm.UseImageFormCBClick(Sender: TObject);
begin
  if UseImageFormCB.Checked then
  begin
    ImageFrm.BackgroundType := bgtTileBitmap;
    PageCtl.ImageForm := ImageFrm;
  end
  else
  begin
    ImageFrm.BackgroundType := bgtColorFill;
    PageCtl.ImageForm := nil;
  end;
  InactiveTabColorCombo.Enabled := not UseImageFormCB.Checked;
  ActiveTabColorCombo.Enabled := not UseImageFormCB.Checked;
  TabBackColorCombo.Enabled := not UseImageFormCB.Checked;
  PageCtlColorCombo.Enabled := not UseImageFormCB.Checked;
end;

procedure TPgCtlDemoMainForm.FormCreate(Sender: TObject);
begin
  StyleCombo.ItemIndex := 0;
  TabPosCombo.ItemIndex := 0;
  FirstCall := true;
  UseXPStylesCB.Visible := IsWinXPUp; 
end;

procedure TPgCtlDemoMainForm.DrawFocusCBClick(Sender: TObject);
begin
  PageCtl.DrawFocus := DrawFocusCB.Checked;
end;

procedure TPgCtlDemoMainForm.FormActivate(Sender: TObject);
begin
  if FirstCall then
  begin
    FirstCall := false;
    PagesSpin.Value := 4;
    PagesSpinChange(Self);
  end;
end;

procedure TPgCtlDemoMainForm.UseXPStylesCBClick(Sender: TObject);
begin
  PageCtl.UseXPThemes := UseXPStylesCB.Checked;
end;

end.

