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

unit ElTreeMemoComboEdit;

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
  ElTools,
  ElStrUtils,
  ElMemoCombo
  ;

type

    TElTreeInplaceMemoComboEdit = class(TElTreeInplaceEditor)
    private
      SaveWndProc: TWndMethod;
      SaveMemoWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
      procedure MemoWndProc(var Message : TMessage);
    protected
      FEditor: TElMemoCombo;
      procedure DoStartOperation; override;
      procedure DoStopOperation(Accepted : boolean); override;
      function GetVisible: Boolean; override;
      procedure TriggerAfterOperation(var Accepted : boolean; var DefaultConversion :
          boolean); override;
      procedure TriggerBeforeOperation(var DefaultConversion : boolean); override;
      procedure SetEditorParent; override;
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      property Editor: TElMemoCombo read FEditor;
    end;

implementation

constructor TElTreeInplaceMemoComboEdit.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := TElMemoCombo.Create(Self);
  FEditor.Visible := false;
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := Self.EditorWndProc;
  SaveMemoWndProc := FEditor.GetMemo.WindowProc;
  FEditor.GetMemo.WindowProc := MemoWndProc;

  FTypes := [sftText, sftMemo];
end;

destructor TElTreeInplaceMemoComboEdit.Destroy;
begin
  FEditor.GetMemo.WindowProc := SaveMemoWndProc;
  FEditor.WindowProc := SaveWndProc;
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

procedure TElTreeInplaceMemoComboEdit.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus; 
end;

procedure TElTreeInplaceMemoComboEdit.DoStopOperation(Accepted : boolean);
begin
  FEditor.Drop(false);
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

type THackElTree = class(TCustomElTree);

procedure TElTreeInplaceMemoComboEdit.MemoWndProc(var Message : TMessage);
var InputValid : boolean;
begin
  if (Message.Msg = WM_CANCELMODE) or
     (Message.Msg = CM_CANCELMODE) or
     (Message.Msg = WM_KILLFOCUS) then
    if FEditor.HandleAllocated and
       (TWMKillFocus(Message).FocusedWnd <> FEditor.Handle) then
    if FEditing then
    begin
      if THackElTree(Tree).ExplorerEditMode then
      begin
        InputValid := true;
        TriggerValidateResult(InputValid);
        CompleteOperation(InputValid);
      end
      else
        CompleteOperation(false);
    end;
  if Message.Msg = WM_KEYDOWN then
  begin
    if TWMKey(Message).CharCode = VK_ESCAPE then
    begin
      CompleteOperation(false);
      TWMKey(Message).CharCode := 0;
      exit;
    end
    else
    if TWMKey(Message).CharCode = VK_RETURN then
    begin
      if KeyDataToShiftState(TWMKey(Message).KeyData) = [] then
      begin
        InputValid := true;
        FEditing := false;
        TriggerValidateResult(InputValid);
        FEditing := true;
        if InputValid then
        begin
          FEditor.Drop(false);
          CompleteOperation(true);
          exit;
        end
        else
          Editor.SetFocus;
        TWMKey(Message).CharCode := 0;
      end
      else
      if KeyDataToShiftState(TWMKey(Message).KeyData) = [ssCtrl] then
      begin
        //PostMessage(FEditor.GetMemo.Handle, WM_CHAR, TMessage(Message).wParam, TMessage(Message).lParam);
        TWMKey(Message).CharCode := 0;
      end;
    end
  end;
  SaveMemoWndProc(Message);
end;

procedure TElTreeInplaceMemoComboEdit.EditorWndProc(var Message : TMessage);
var InputValid : boolean;
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
          CompleteOperation(true)
        else 
          Editor.SetFocus;  
        TWMKey(Message).CharCode := 0;
        exit;
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
  if Message.Msg = WM_KILLFOCUS then
    if FEditor.GetMemo.HandleAllocated and
       (TWMKillFocus(Message).FocusedWnd <> 0) and
       (TWMKillFocus(Message).FocusedWnd <> FEditor.Handle) and
       (TWMKillFocus(Message).FocusedWnd <> FEditor.GetMemo.Handle) then
    if FEditing then
    begin
      if THackElTree(Tree).ExplorerEditMode then
      begin
        InputValid := true;
        TriggerValidateResult(InputValid);
        CompleteOperation(InputValid);
      end
      else
        CompleteOperation(false);
    end;
  SaveWndProc(Message);
end;

function TElTreeInplaceMemoComboEdit.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TElTreeInplaceMemoComboEdit.TriggerAfterOperation(var Accepted :
    boolean; var DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
    ValueAsText := FEditor.Text;
end;

procedure TElTreeInplaceMemoComboEdit.TriggerBeforeOperation(var
    DefaultConversion : boolean);
begin
  inherited;
  if DefaultConversion then
    FEditor.Text := ValueAsText;
  FEditor.BoundsRect := FCellRect;
end;

procedure TElTreeInplaceMemoComboEdit.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;


end.
