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

unit ElTreeDTPickEdit;

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
  ElDTPick
  ;

type

    TElTreeInplaceDateTimePicker = class(TElTreeInplaceEditor)
    private
      SaveWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
    protected
      FEditor: TElDateTimePicker;
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
      property Editor: TElDateTimePicker read FEditor;
    end;

implementation

constructor TElTreeInplaceDateTimePicker.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := TElDateTimePicker.Create(Self);
  FEditor.ParentCtl3D := false;
  FEditor.Ctl3D := false;
  FEditor.Visible := false;
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := EditorWndProc;
  FTypes := [sftDate, sftTime, sftDateTime];
end;

destructor TElTreeInplaceDateTimePicker.Destroy;
begin
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

procedure TElTreeInplaceDateTimePicker.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus;  
end;

procedure TElTreeInplaceDateTimePicker.DoStopOperation(Accepted : boolean);
begin
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

function TElTreeInplaceDateTimePicker.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TElTreeInplaceDateTimePicker.TriggerAfterOperation(var Accepted :
    boolean; var DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
  case Self.DataType of
    sftDate: ValueAsText := DateToStr(FEditor.Date);
    sftTime: ValueAsText := TimeToStr(FEditor.Time);
    sftDateTime: ValueAsText := DateTimeToStr(FEditor.DateTime);
  end;
end;

procedure TElTreeInplaceDateTimePicker.TriggerBeforeOperation(var
    DefaultConversion : boolean);
begin
  inherited;
  if DefaultConversion then
  try
    case Self.DataType of
      sftDate: FEditor.Date := StrToDate(ValueAsText);
      sftTime: FEditor.Time := StrToTime(ValueAsText);
      sftDateTime: FEditor.DateTime := StrToDateTime(ValueAsText);
    end;
  except
    on E: EConvertError do
      FEditor.DateTime := Now;
  end;
  FEditor.BoundsRect := FCellRect;
end;

type THackElTree = class(TCustomElTree);
     THackDTPick = class(TElDateTimePicker);

procedure TElTreeInplaceDateTimePicker.EditorWndProc(var Message : TMessage);
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

procedure TElTreeInplaceDateTimePicker.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;




end.

