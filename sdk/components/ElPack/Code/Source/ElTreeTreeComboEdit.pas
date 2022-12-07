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

unit ElTreeTreeComboEdit;

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
  ElTreeCombo
  ;

type

    TElTreeInplaceTreeComboEdit = class(TElTreeInplaceEditor)
    private
      SaveWndProc: TWndMethod;
      SaveTreeWndProc : TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
      procedure TreeWndProc(var Message : TMessage);
    protected
      FEditor: TElTreeCombo;
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
      property Editor: TElTreeCombo read FEditor;
    end;

implementation

constructor TElTreeInplaceTreeComboEdit.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := TElTreeCombo.Create(nil);
  FEditor.Visible := false;
  FEditor.HandleDialogKeys := true;
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := EditorWndProc;
  SaveTreeWndProc := FEditor.GetTree.WindowProc;
  FEditor.GetTree.WindowProc := TreeWndProc;
  FEditor.Ctl3D := false;
  FTypes := [sftText];
end;

destructor TElTreeInplaceTreeComboEdit.Destroy;
begin
  FEditor.GetTree.WindowProc := SaveTreeWndProc;
  FEditor.WindowProc := SaveWndProc;
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

type THackElTree = class(TCustomElTree);

procedure TElTreeInplaceTreeComboEdit.TreeWndProc(var Message : TMessage);
var InputValid : boolean;
begin
  if (Message.Msg = WM_KILLFOCUS) then
  begin
    if FEditor.HandleAllocated and (TWMKillFocus(Message).FocusedWnd <> FEditor.Handle) and
       (FEditor.GetTree.HandleAllocated) and (TWMKillFocus(Message).FocusedWnd <> FEditor.GetTree.Handle) and
       (TWMKillFocus(Message).FocusedWnd <> FEditor.GetTree.Parent.Handle) then
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
  end;
  SaveTreeWndProc(Message);
end;

procedure TElTreeInplaceTreeComboEdit.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus; 
end;

procedure TElTreeInplaceTreeComboEdit.DoStopOperation(Accepted : boolean);
begin
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

procedure TElTreeInplaceTreeComboEdit.EditorWndProc(var Message : TMessage);
var InputValid : boolean;
    b          : boolean;
begin
  if Message.Msg = WM_KEYDOWN then
  begin
    if KeyDataToShiftState(TWMKey(Message).KeyData) = [] then
    begin
      if TWMKey(Message).CharCode = VK_RETURN then
      begin
        b := false;
        if Editor.Dropped then
        begin
          b := true;
          // Dropped := false;
          SaveWndProc(Message);
        end;
        // else
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
          if b then exit;
        end;
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
  if (Message.Msg = CM_CANCELMODE) then
  begin
    if (Message.lParam = 0) or
       ((TControl(TObject(Pointer(Message.lParam))).Parent <> Editor) and
       (TControl(TObject(Pointer(Message.lParam))).Parent <> Editor.GetTree) and
       (TObject(Pointer(Message.lParam)) <> Editor) and
       (TObject(Pointer(Message.lParam)) <> Editor.GetTree)) then
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
  end
  else
  if (Message.Msg = WM_KILLFOCUS) then
  begin
    if (FEditor.GetTree.HandleAllocated) and
       (TWMKillFocus(Message).FocusedWnd <> FEditor.GetTree.Handle) and
       (TWMKillFocus(Message).FocusedWnd <> FEditor.{GetTree.Parent.}Handle) then
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
  end;
  SaveWndProc(Message);
end;

function TElTreeInplaceTreeComboEdit.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TElTreeInplaceTreeComboEdit.TriggerAfterOperation(var Accepted : 
    boolean; var DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
    ValueAsText := FEditor.Text;
end;

procedure TElTreeInplaceTreeComboEdit.TriggerBeforeOperation(var 
    DefaultConversion : boolean);
begin
  inherited;
  if DefaultConversion then
    FEditor.Text := ValueAsText;
  FEditor.BoundsRect := FCellRect;
end;

procedure TElTreeInplaceTreeComboEdit.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;



end.
