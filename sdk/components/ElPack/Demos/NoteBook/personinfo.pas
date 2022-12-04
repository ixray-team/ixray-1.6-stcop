unit PersonInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElPopBtn, StdCtrls, ElACtrls, ElLabel, frmMain, ElCLabel;

type
  TfrmPersonInfo = class(TForm)
    ElLabel1: TElLabel;
    ElLabel2: TElLabel;
    ElLabel3: TElLabel;
    ElLabel4: TElLabel;
    ElLabel5: TElLabel;
    ElLabel6: TElLabel;
    ElLabel7: TElLabel;
    ElLabel8: TElLabel;
    ElLabel9: TElLabel;
    edCountry: TElAdvancedEdit;
    edState: TElAdvancedEdit;
    edCity: TElAdvancedEdit;
    edStreet: TElAdvancedEdit;
    edPhone: TElAdvancedEdit;
    edFax: TElAdvancedEdit;
    edMobile: TElAdvancedEdit;
    edEmail: TElAdvancedEdit;
    edWebPage: TElAdvancedEdit;
    OK: TElGraphicButton;
    procedure OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPersonInfo: TfrmPersonInfo;

implementation

{$R *.DFM}

uses NoteRecord;
var
  Address: TAddress;

procedure TfrmPersonInfo.OKClick(Sender: TObject);
begin
  with Address do
  begin
    Country := edCountry.text;
    State := edState.text;
    City := edCity.text;
    Street := edStreet.text;
    Phone := edPhone.text;
    Fax := edFax.text;
    Mobile := edMobile.text;
    Email := edEmail.text;
    WebPage := edWebPage.text;
  end;
  TNotePerson(frmNoteBook.NoteTree.ItemFocused.Data).Address := Address;
  frmPersonInfo.Close;
end;

procedure TfrmPersonInfo.FormCreate(Sender: TObject);
begin
  Address := TNotePerson(frmNoteBook.NoteTree.ItemFocused.Data).Address;
  with Address do
  begin
    edCountry.text := Country;
    edState.text := State;
    edCity.text := City;
    edStreet.text := Street;
    edPhone.text := Phone;
    edFax.text := Fax;
    edMobile.text := Mobile;
    edEmail.text := Email;
    if WebPage = '' then
      edWebPage.text := 'http://'
    else
      edWebPage.text := WebPage;
  end;
end;

end.
