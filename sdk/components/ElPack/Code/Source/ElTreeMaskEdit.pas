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

unit ElTreeMaskEdit;

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
  ElMaskEdit,
  Mask
  ;

type

    TElTreeInplaceMaskEdit = class(TElTreeInplaceEditor)
    private
      SaveWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
    protected
      FEditor: TElMaskEdit;
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
      property Editor: TElMaskEdit read FEditor;
    end;

implementation

constructor TElTreeInplaceMaskEdit.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := TElMaskEdit.Create(nil);
  FEditor.ParentCtl3D := false;
  FEditor.Ctl3D := false;
  FEditor.Visible := false;
  FEditor.HandleDialogKeys := true;
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := Self.EditorWndProc;
  FTypes := [sftText];
end;

destructor TElTreeInplaceMaskEdit.Destroy;
begin
  FEditor.WindowProc := SaveWndProc;
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

procedure TElTreeInplaceMaskEdit.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus; 
end;

procedure TElTreeInplaceMaskEdit.DoStopOperation(Accepted : boolean);
begin
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

type THackElTree = class(TCustomElTree);

procedure TElTreeInplaceMaskEdit.EditorWndProc(var Message : TMessage);
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
      (TObject(Pointer(Message.lParam)) <> Editor)) or
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

function TElTreeInplaceMaskEdit.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TElTreeInplaceMaskEdit.TriggerAfterOperation(var Accepted : boolean;
    var DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
    ValueAsText := FEditor.EditText;
end;

procedure TElTreeInplaceMaskEdit.TriggerBeforeOperation(var DefaultConversion :
    boolean);
begin
  inherited;
  if DefaultConversion then
    FEditor.EditText := ValueAsText;
  FEditor.BoundsRect := FCellRect;
end;

procedure TElTreeInplaceMaskEdit.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;



end.
