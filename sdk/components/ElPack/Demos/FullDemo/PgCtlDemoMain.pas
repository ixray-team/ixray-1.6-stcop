unit PgCtlDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElPgCtl, ExtCtrls, ElPanel, StdCtrls, ElACtrls, ElSpin, ElBtnCtl,
  ElVCLUtils, ElCheckCtl, ElClrCmb, ElImgFrm, ElXPThemedControl;
                         
type
  TPgCtlMDemoMainForm = class(TForm)
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
  private
    { Private declarations }
  public
    FirstCall : boolean;
  end;

var
  PgCtlDemoMainForm: TPgCtlMDemoMainForm;

implementation

{$R *.DFM}

procedure TPgCtlMDemoMainForm.ShowBorderCBClick(Sender: TObject);
begin
  PageCtl.ShowBorder := ShowBorderCB.Checked;
  FlatCB.Enabled := PageCtl.ShowBorder or PageCtl.ShowTabs;
end;

procedure TPgCtlMDemoMainForm.ShowTabsCBClick(Sender: TObject);
begin
  PageCtl.ShowTabs := ShowTabsCB.Checked;
  FlatCB.Enabled := PageCtl.ShowBorder or PageCtl.ShowTabs;
end;

procedure TPgCtlMDemoMainForm.FlatCBClick(Sender: TObject);
begin
  PageCtl.Flat := FlatCB.Checked;
end;

procedure TPgCtlMDemoMainForm.RaggedRightCBClick(Sender: TObject);
begin
  PageCtl.RaggedRight := RaggedRightCB.Checked;
end;

procedure TPgCtlMDemoMainForm.MultilineCBClick(Sender: TObject);
begin
  PageCtl.Multiline := MultilineCB.Checked;
  RaggedRightCB.Enabled := PageCtl.Multiline;
end;

procedure TPgCtlMDemoMainForm.TabPosComboChange(Sender: TObject);
begin
  PageCtl.TabPosition := TElTabPosition(TabPosCombo.ItemIndex);
end;

procedure TPgCtlMDemoMainForm.StyleComboChange(Sender: TObject);
begin
  PageCtl.Style := TElTabStyle(StyleCombo.ItemIndex);
end;

procedure TPgCtlMDemoMainForm.PagesSpinChange(Sender: TObject);

  procedure AddPage;
  var ASheet : TElTabSheet;
      B      : TButton;
  begin
    ASheet := PageCtl.NewPage;
    ASheet.Caption := 'Page ' + IntToStr(ASheet.PageIndex + 1);
    B := TButton.Create(ASheet);
    B.Caption := ASheet.Caption + ' Button';
    B.Parent := ASheet;
    B.Left := Random(ASheet.Width - B.Width);
    B.Top := Random(ASheet.Height - B.Height);
  end;

begin
  while PageCtl.PageCount > PagesSpin.Value do
    PageCtl.Pages[PageCtl.PageCount - 1].Free;
  while PageCtl.PageCount < PagesSpin.Value do
    AddPage;
end;

procedure TPgCtlMDemoMainForm.InactiveTabColorComboChange(Sender: TObject);
begin
  PageCtl.InactiveTabColor := InactiveTabColorCombo.SelectedColor;
end;

procedure TPgCtlMDemoMainForm.PageCtlColorComboChange(Sender: TObject);
begin
  PageCtl.Color := PageCtlColorCombo.SelectedColor;
end;

procedure TPgCtlMDemoMainForm.TabBackColorComboChange(Sender: TObject);
begin
  PageCtl.TabBkColor := TabBackColorCombo.SelectedColor;
end;

procedure TPgCtlMDemoMainForm.ActiveTabColorComboChange(Sender: TObject);
begin
  PageCtl.ActiveTabColor := ActiveTabColorCombo.SelectedColor;
end;

procedure TPgCtlMDemoMainForm.UseImageFormCBClick(Sender: TObject);
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

procedure TPgCtlMDemoMainForm.FormCreate(Sender: TObject);
begin
  StyleCombo.ItemIndex := 0;
  TabPosCombo.ItemIndex := 0;
  FirstCall := true;
end;

procedure TPgCtlMDemoMainForm.DrawFocusCBClick(Sender: TObject);
begin
  PageCtl.DrawFocus := DrawFocusCB.Checked;
end;

procedure TPgCtlMDemoMainForm.FormActivate(Sender: TObject);
begin
  if FirstCall then
  begin
    FirstCall := false;
    PagesSpin.Value := 4;
    PagesSpinChange(Self);
  end;
end;

end.

