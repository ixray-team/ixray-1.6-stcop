unit ElTreeDemoExportTemplate;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ElSplit, ExtCtrls, ElPanel, ElXPThemedControl, ElTree, StdCtrls, ElACtrls,
  Clipbrd, ElBtnCtl, ElPopBtn;

type
  TElTreeDemoExportTemplateForm = class(TForm)
    BottomPanel: TElPanel;
    LeftPanel: TElPanel;
    Splitter: TElSplitter;
    RightPanel: TElPanel;
    Label1: TLabel;
    TagsTree: TElTree;
    TemplateMemo: TElAdvancedMemo;
    Label2: TLabel;
    OkBtn: TElPopupButton;
    CancelBtn: TElPopupButton;
    procedure TagsTreeDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ElTreeDemoExportTemplateForm: TElTreeDemoExportTemplateForm;

implementation

{$R *.DFM}

procedure TElTreeDemoExportTemplateForm.TagsTreeDblClick(Sender: TObject);
var S : String;
begin
  if (TagsTree.ItemFocused <> nil) and (TagsTree.ItemFocused.ColumnText.Count > 0) then
  begin
      S := TagsTree.ItemFocused.ColumnText[0];
      SendMessage(TemplateMemo.Handle, EM_REPLACESEL, 1, Integer(PChar(S)));
      TemplateMemo.SetFocus;
  end;
end;

end.

