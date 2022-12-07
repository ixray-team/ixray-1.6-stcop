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

unit ElTreeBtnEdit;

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
  ElBtnEdit
  ;

type

    TElTreeInplaceButtonEdit = class(TElTreeInplaceEditor)
    private
      SaveWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
    protected
      FEditor: TElButtonEdit;
      function GetVisible: Boolean; override;
      procedure TriggerAfterOperation(var Accepted : boolean; var DefaultConversion : 
          boolean); override;
      procedure TriggerBeforeOperation(var DefaultConversion : boolean); override;
      procedure DoStartOperation; override;
      procedure DoStopOperation(Accepted : boolean); override;
      procedure SetEditorParent; override;
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      property Editor: TElButtonEdit read FEditor;
    end;

implementation

function TElTreeInplaceButtonEdit.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TElTreeInplaceButtonEdit.TriggerAfterOperation(var Accepted : boolean;
    var DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
    ValueAsText := FEditor.Text;
end;

procedure TElTreeInplaceButtonEdit.TriggerBeforeOperation(var DefaultConversion
    : boolean);
begin
  inherited;
  if DefaultConversion then
    FEditor.Text := ValueAsText;
  FEditor.BoundsRect := FCellRect;
  FEditor.Multiline := DataType = sftMemo;   
end;

constructor TElTreeInplaceButtonEdit.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := TElButtonEdit.Create(Self);
  FEditor.Visible := false;
  FEditor.ParentCtl3D := false;
  FEditor.Ctl3D := false;
  FEditor.AutoSelect := true;
  FEditor.AutoSize := false;
  FEditor.HandleDialogKeys := true;
  FEditor.Multiline := false;
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := EditorWndProc;
  FTypes := [sftText, sftMemo];
end;

destructor TElTreeInplaceButtonEdit.Destroy;
begin
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

procedure TElTreeInplaceButtonEdit.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus;  
end;

procedure TElTreeInplaceButtonEdit.DoStopOperation(Accepted : boolean);
begin
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

type THackElTree = class(TCustomElTree);
     THackButtonEdit = class(TElButtonEdit);
     
procedure TElTreeInplaceButtonEdit.EditorWndProc(var Message : TMessage);
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
        exit;
      end;
    end;
  end
  else
    if (Message.Msg = WM_CANCELMODE) or
     ((Message.Msg = CM_CANCELMODE) and
      ((Message.lParam = 0) or
      ((TControl(TObject(Pointer(Message.lParam))).Parent <> Editor) and
      (TObject(Pointer(Message.lParam)) <> Editor)))
     ) or
     (Message.Msg = WM_KILLFOCUS) then
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

procedure TElTreeInplaceButtonEdit.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;

end.
 
