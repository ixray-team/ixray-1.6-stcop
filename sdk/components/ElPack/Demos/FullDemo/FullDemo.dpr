program FullDemo;

uses
  Forms,
  PgCtlDemoMain in 'PgCtlDemoMain.pas' {PgCtlMDemoMainForm},
  ElTreeStringGridDemoMain in 'ElTreeStringGridDemoMain.pas' {ElTreeStringGridDemoForm},
  ElHeaderDemoMain in 'ElHeaderDemoMain.pas' {ElHeaderDemoMainForm},
  ElStatusBarDemoMain in 'ElStatusBarDemoMain.pas' {ElstatusBarDemoMainForm},
  ElToolbarDemoMain in 'ElToolbarDemoMain.pas' {ElToolbarDemoMainForm},
  ElPopupButtonDemoMain in 'ElPopupButtonDemoMain.pas' {ElPopupButtonDemoMainForm},
  ElCheckGroupDemoMain in 'ElCheckGroupDemoMain.pas' {ElCheckGroupDemoMainForm},
  ElCheckBoxDemoMain in 'ElCheckBoxDemoMain.pas' {ElCheckBoxDemoMainForm},
  ElRadioButtonDemoMain in 'ElRadioButtonDemoMain.pas' {ElRadioButtonDemoMainForm},
  ElGroupBoxDemoMain in 'ElGroupBoxDemoMain.pas' {ElGroupBoxDemoMainForm},
  ElRadioGroupDemoMain in 'ElRadioGroupDemoMain.pas' {ElRadioGroupDemoMainForm},
  ElScrollBarDemoMain in 'ElScrollBarDemoMain.pas' {ElScrollBarDemoMainForm},
  MainForm in 'MainForm.pas' {frmMain},
  ElDateTimePickerDemoMain in 'ElDateTimePickerDemoMain.pas' {ElDateTimePickerDemoMainForm},
  ElTreeDemoMain in 'ElTreeDemoMain.pas' {ElTreeDemoMainForm},
  ElTreeDemoExportTemplate in 'ElTreeDemoExportTemplate.pas' {ElTreeDemoExportTemplateForm},
  ElTreeMLGen in '..\..\Code\Source\MLGen\Source\ElTreeMLGen.pas',
  ElUnicodeStrings in '..\..\code\source\ElUnicodeStrings.pas',
  HTMLRender in '..\..\code\source\HTMLRender.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ElPack Demo';
  // InitBM;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
