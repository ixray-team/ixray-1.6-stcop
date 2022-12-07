{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}
{$include elpack2.inc}
{$ifdef ELPACK_SINGLECOMP}
{$I ElPack.inc}
{$else}
{$ifdef LINUX}
{$I ../ElPack.inc}
{$else}
{$I ..\ElPack.inc}
{$endif}
{$endif}

unit ElTreeComboBox;

interface

uses

  Windows,
  Messages,
  Controls,
  Forms,

  SysUtils,
  Classes,
{$ifdef VCL_6_USED}
Types,
{$endif}


  ElTree,
  ElHeader,
  HtmlLbx
  ;

type

    THackInplaceComboBox = class(TElHTMLComboBox)
      procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    end;

    TElTreeInplaceComboBox = class(TElTreeInplaceEditor)
    private
      SaveWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
    protected
      FEditor: TElHTMLComboBox;
      FInitiallyDropped : boolean;
      procedure SetEditorParent; override;
      procedure DoStartOperation; override;
      procedure DoStopOperation(Accepted : boolean); override;
      function GetVisible: Boolean; override;
      procedure TriggerAfterOperation(var Accepted : boolean; var DefaultConversion :
          boolean); override;
      procedure TriggerBeforeOperation(var DefaultConversion : boolean); override;
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      property Editor: TElHTMLComboBox read FEditor;
    published
      property InitiallyDropped : boolean read FInitiallyDropped write FInitiallyDropped; 
    end;

implementation

constructor TElTreeInplaceComboBox.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := THackInplaceComboBox.Create(Self);
  FEditor.Visible := false;
  FEditor.ParentCtl3D := false;
  FEditor.Ctl3D := false;
  FEditor.HandleDialogKeys := true;

  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := EditorWndProc;
  //FEditor.ListWindowProc := ListWndProc;
  //FEditor.EditWindowProc := EditWndProc;
  FTypes := [sftEnum];
end;

destructor TElTreeInplaceComboBox.Destroy;
begin
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

procedure TElTreeInplaceComboBox.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;

procedure TElTreeInplaceComboBox.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus;
  if InitiallyDropped then
    FEditor.Perform(CB_SHOWDROPDOWN, 1, 0);
end;

procedure TElTreeInplaceComboBox.DoStopOperation(Accepted : boolean);
begin
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

type THackElTree = class(TCustomElTree);

procedure TElTreeInplaceComboBox.EditorWndProc(var Message : TMessage);
var InputValid : boolean;
begin
  if Message.Msg = WM_KEYDOWN then
  begin
    if KeyDataToShiftState(TWMKey(Message).KeyData) = [] then
    begin
      if TWMKey(Message).CharCode = VK_RETURN then
      begin
        InputValid := true;
        FEditing := false;
        TriggerValidateResult(InputValid);
        FEditing := true;
        if InputValid then
        begin
          CompleteOperation(true);
          exit;
        end
        else
          Editor.SetFocus;
        TWMKey(Message).CharCode := 0;
      end
      else
      if TWMKey(Message).CharCode = VK_ESCAPE then
      begin
        CompleteOperation(false);
        TWMKey(Message).CharCode := 0;
      end;
    end;
  end
  else
  if (Message.Msg = WM_CANCELMODE) or
     ((Message.Msg = CM_CANCELMODE) and
      ((Message.lParam = 0) or
      ((TControl(TObject(Pointer(Message.lParam))).Parent <> Editor)) and
      (TObject(Pointer(Message.lParam)) <> Editor))) or
     (Message.Msg = WM_KILLFOCUS) then
  begin
    if FEditing then
      PostMessage(Editor.Handle, TM_CLOSEINPLACEEDITOR, 0, 0);
  end
  else
  if (Message.Msg = TM_CLOSEINPLACEEDITOR) then
    if THackElTree(Tree).ExplorerEditMode then
    begin
      InputValid := true;
      TriggerValidateResult(InputValid);
      CompleteOperation(InputValid);
    end
    else
      CompleteOperation(false);

  SaveWndProc(Message);
end;

function TElTreeInplaceComboBox.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TElTreeInplaceComboBox.TriggerAfterOperation(var Accepted : boolean;
    var DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
    ValueAsText := FEditor.Text;
end;

procedure TElTreeInplaceComboBox.TriggerBeforeOperation(var DefaultConversion : 
    boolean);
begin
  inherited;
  if DefaultConversion then
    FEditor.Text := ValueAsText;
  FEditor.BoundsRect := FCellRect;
end;

procedure THackInplaceComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
var InputValid : boolean;
begin
  with TElTreeInplaceComboBox(Owner) do
  begin
    if Message.Msg = WM_KEYDOWN then
    begin
      if KeyDataToShiftState(TWMKey(Message).KeyData) = [] then
      begin
        if TWMKey(Message).CharCode = VK_RETURN then
        begin
          InputValid := true;
          TriggerValidateResult(InputValid);
          if InputValid then
          begin
            CompleteOperation(true);
            exit;
          end
          else
            Editor.SetFocus;
          TWMKey(Message).CharCode := 0;
        end
        else
        if TWMKey(Message).CharCode = VK_ESCAPE then
        begin
          CompleteOperation(false);
          TWMKey(Message).CharCode := 0;
          exit;
        end;
      end;
    end
    else
    if (Message.Msg = WM_KILLFOCUS) then
    begin
      if FEditing then
        PostMessage(Editor.Handle, TM_CLOSEINPLACEEDITOR, 0, 0);
    end
    else
    if (Message.Msg = TM_CLOSEINPLACEEDITOR) then
      if THackElTree(Tree).ExplorerEditMode then
      begin
        InputValid := true;
        TriggerValidateResult(InputValid);
        CompleteOperation(InputValid);
      end
      else
        CompleteOperation(false);
  end;
  inherited;
end;

end.
