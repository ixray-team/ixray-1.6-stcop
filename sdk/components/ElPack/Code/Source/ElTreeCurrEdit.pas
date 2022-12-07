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

unit ElTreeCurrEdit;

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
  ElCurrEdit
  ;

type

    THackInplaceCurrencyEdit = class(TElCurrencyEdit)
      procedure DoExit; override;
      procedure KeyDown(var Key : Word; ShiftState : TShiftState); override;
    end;

    TElTreeInplaceCurrencyEdit = class(TElTreeInplaceEditor)
    private
      SaveIntWndProc : array[1..2] of TWndMethod;
      SaveWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
      procedure IntEditorWndProc2(var Message : TMessage);
      procedure IntEditorWndProc1(var Message : TMessage);
    protected
      FEditor: TElCurrencyEdit;
      procedure DoStartOperation; override;
      procedure DoStopOperation(Accepted : boolean); override;
      function GetVisible: Boolean; override;
      procedure TriggerAfterOperation(var Accepted : boolean; var DefaultConversion : 
          boolean); override;
      procedure TriggerBeforeOperation(var DefaultConversion : boolean); override;
      procedure SetEditorParent; override;
      procedure RealWndProc(var Message : TMessage);
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      property Editor: TElCurrencyEdit read FEditor;
    end;

implementation

type THackControl = class(TControl);

constructor TElTreeInplaceCurrencyEdit.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := THackInplaceCurrencyEdit.Create(Self);
  FEditor.Visible := false;
  FEditor.ParentCtl3D := false;
  FEditor.Ctl3D := false;
  FEditor.HandleDialogKeys := true;
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := EditorWndProc;
  FTypes := [sftCurrency];
  SaveIntWndProc[1] := THackControl(THackInplaceCurrencyEdit(FEditor).FPartEditors[1]).WindowProc;
  SaveIntWndProc[2] := THackControl(THackInplaceCurrencyEdit(FEditor).FPartEditors[2]).WindowProc;
  THackControl(THackInplaceCurrencyEdit(FEditor).FPartEditors[1]).WindowProc := IntEditorWndProc1;
  THackControl(THackInplaceCurrencyEdit(FEditor).FPartEditors[2]).WindowProc := IntEditorWndProc2;
end;

destructor TElTreeInplaceCurrencyEdit.Destroy;
begin
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

procedure TElTreeInplaceCurrencyEdit.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus;  
end;

procedure TElTreeInplaceCurrencyEdit.DoStopOperation(Accepted : boolean);
begin
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

type THackElTree = class(TCustomElTree);

procedure TElTreeInplaceCurrencyEdit.EditorWndProc(var Message : TMessage);
begin
  RealWndProc(Message);
  SaveWndProc(Message);
end;

function TElTreeInplaceCurrencyEdit.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TElTreeInplaceCurrencyEdit.TriggerAfterOperation(var Accepted :
    boolean; var DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
    ValueAsText := CurrToPrettyStr(FEditor.Value);
end;

procedure TElTreeInplaceCurrencyEdit.TriggerBeforeOperation(var 
    DefaultConversion : boolean);
begin
  inherited;
  if DefaultConversion then
    FEditor.Value := PrettyStrToCurr(ValueAsText);
  FEditor.BoundsRect := FCellRect;
end;

procedure TElTreeInplaceCurrencyEdit.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;

procedure TElTreeInplaceCurrencyEdit.IntEditorWndProc2(var Message : TMessage);
begin
  RealWndProc(Message);
  SaveIntWndProc[2](Message);
end;

procedure TElTreeInplaceCurrencyEdit.RealWndProc(var Message : TMessage);
var InputValid : boolean;
    ACtl       : TWinControl;
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
        exit;
      end;
    end;
  end
  else
  if (Message.Msg = WM_CANCELMODE) or
     ((Message.Msg = CM_CANCELMODE) and
      ((Message.lParam = 0) or
      (((TControl(TObject(Pointer(Message.lParam))).Parent <> Editor) and
      (TObject(Pointer(Message.lParam)) <> Editor))))
     ) or
     (Message.Msg = WM_KILLFOCUS) then
  begin
    if Message.Msg = WM_KILLFOCUS then
    begin
      ACtl := FindControl(TWMKillFocus(Message).FocusedWnd);
      if (ACtl <> nil) and (ACtl.Parent = Editor) then
      begin
        SaveWndProc(Message);
        exit;
      end;
    end;
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
end;

procedure TElTreeInplaceCurrencyEdit.IntEditorWndProc1(var Message : TMessage);
begin
  RealWndProc(Message);
  SaveIntWndProc[1](Message);
end;


procedure THackInplaceCurrencyEdit.DoExit;
begin
  inherited;
  (*
  with TElTreeInplaceCurrencyEdit(Owner) do
    if FEditing then
      CompleteOperation(false);
  *)
end;

procedure THackInplaceCurrencyEdit.KeyDown;
begin
  inherited;
  (*
  with TElTreeInplaceCurrencyEdit(Owner) do
  if ShiftState = [] then
  begin
    if Key = VK_RETURN then
    begin
      InputValid := true;
      TriggerValidateResult(InputValid);
      if InputValid then
        CompleteOperation(true);
      Key := 0;
    end
    else
    if Key = VK_ESCAPE then
    begin
      CompleteOperation(false);
      Key := 0;
    end;
  end;
  *)
end;

end.
