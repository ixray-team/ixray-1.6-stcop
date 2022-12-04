program NoteBook;

uses
  Forms,
  frmMain in 'frmMain.pas' {frmNoteBook},
  PersonInfo in 'PersonInfo.pas' {frmPersonInfo},
  find in 'find.pas' {frmFind},
  NoteRecord in 'NoteRecord.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNoteBook, frmNoteBook);
  Application.Run;
end.
