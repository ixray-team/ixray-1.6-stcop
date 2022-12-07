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

unit ElTreeStdEditors;

interface

uses

  {$ifndef CLX_USED}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
{$ifdef VCL_6_USED}
  Types,
{$endif}

{$IFDEF VCL_4_USED}
  ImgList,
{$ENDIF}
  {$else}
  {$ifdef LINUX}
  Xlib,
  {$else}
  Windows,
  {$endif}
  Qt,
  QTypes,
  Types,
  QComCtrls,
  QGraphics,
  QControls,
  QForms,
  QMenus,
  QExtCtrls,
  QStdCtrls,
  ElCLXUtils,
{$IFDEF VCL_4_USED}
  QImgList,
{$ENDIF}
  {$endif}

  SysUtils,
  Classes,
  {$ifndef CLX_USED}
  ElEdits,
  {$endif}
  ElTree,
  ElHeader
  ;

type

    TElTreeInplaceEdit = class(TElTreeInplaceEditor)
    private
      {$ifndef CLX_USED}
      SaveWndProc : TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
      {$endif}
    protected
      {$ifdef CLX_USED}
      FEditor: TEdit;
      {$else}
      FEditor: TElEdit;
      {$endif}
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
      {$ifdef CLX_USED}
      property Editor: TEdit read FEditor;
      {$else}
      property Editor: TElEdit read FEditor;
      {$endif}
    end;

    TElTreeInplaceMemo = class(TElTreeInplaceEditor)
    private
      {$ifndef CLX_USED}
      SaveWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
      {$endif}
    protected
      FEditor: TMemo;
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
      property Editor: TMemo read FEditor;
    end;

    {$ifndef CLX_USED}
    TElTreeInplaceDateTimePicker = class(TElTreeInplaceEditor)
    private
      SaveWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
    protected
      FEditor: TDateTimePicker;
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
      property Editor: TDateTimePicker read FEditor;
    end;
    {$endif}

    TElTreeInplaceCheckBox = class(TElTreeInplaceEditor)
    private
      {$ifndef CLX_USED}
      SaveWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
      {$endif}
    protected
      FEditor: TCheckBox;
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
      property Editor: TCheckBox read FEditor;
    end;

    {$ifndef CLX_USED}
    THackInplaceComboBox = class(TComboBox)
      procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    protected
      procedure KeyPress(var Key: Char); override;
      procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    end;

    TElTreeInplaceComboBox = class(TElTreeInplaceEditor)
    private
      SaveWndProc: TWndMethod;
      procedure EditorWndProc(var Message : TMessage);
    protected
      FInitiallyDropped : boolean;
      FEditor: TComboBox;
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
      property Editor: TComboBox read FEditor;
    published
      property InitiallyDropped : boolean read FInitiallyDropped write FInitiallyDropped; 
    end;
    {$endif}
implementation

type

  THackElTree = class(TCustomElTree);

  {$ifdef CLX_USED}
  TElInpEdit = class(TEdit)
  {$else}
  TElInpEdit = class(TElEdit)
  {$endif}
  private
    {$ifndef CLX_USED}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    {$else}
    constructor Create(AOwner : TComponent); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure DoExit; override;
    {$endif}
    procedure KeyPress(var Key: Char); override;
  end;

  TElInpMemo = class(TMemo)
  private
    {$ifndef CLX_USED}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    {$else}
    constructor Create(AOwner : TComponent); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure DoExit; override;
    {$endif}
    procedure KeyPress(var Key: Char); override;
  end;

  TElInpCheckBox = class(TCheckBox)
  private
    procedure KeyPress(var Key: Char); override;
    {$ifndef CLX_USED}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    {$else}
    constructor Create(AOwner : TComponent); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure DoExit; override;
    {$endif}
  end;
  {$ifndef CLX_USED}
  TElInpDateTimePicker = class(TDateTimePicker)
  private
    procedure KeyPress(var Key: Char); override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  end;
  {$endif}

procedure TElInpMemo.KeyPress(var Key: Char);
begin                                      // get rid of Windows beep
  if Key = Chr(VK_RETURN) then
    key := Chr(0);
  if Key = Chr(VK_ESCAPE) then
    key := Chr(0);
  if Key <> #0 then
    inherited KeyPress(Key);
end;


{$ifdef CLX_USED}
constructor TElInpMemo.Create(AOwner : TComponent);
begin
  inherited;
  InputKeys := InputKeys + [ikReturns, ikEsc];
end;

procedure TElInpMemo.DoExit;
var InputValid : boolean;
begin
  inherited;
  with TElTreeInplaceEdit(Owner) do
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

procedure TElInpMemo.KeyDown(var Key : Word; Shift : TShiftState);
var InputValid : boolean;
begin
  with TElTreeInplaceEdit(Owner) do
  if Shift = [] then
  begin
    if Key = VK_RETURN then
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
      Key := 0;
    end
    else
    if Key = VK_ESCAPE then
    begin
      CompleteOperation(false);
      Key := 0;
      exit;
    end;
  end;
  inherited;
end;
{$else}
procedure TElInpMemo.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS; // we want the RETURN key
end;
{$endif}

{$ifdef CLX_USED}
constructor TElInpEdit.Create(AOwner : TComponent);
begin
  inherited;
  InputKeys := InputKeys + [ikReturns, ikEsc];
end;

procedure TElInpEdit.DoExit;
var InputValid : boolean;
begin
  inherited;   
  with TElTreeInplaceEdit(Owner) do
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

procedure TElInpEdit.KeyDown(var Key : Word; Shift : TShiftState);
var InputValid : boolean;
begin
  with TElTreeInplaceEdit(Owner) do
  if Shift = [] then
  begin
    if Key = VK_RETURN then
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
      Key := 0;
    end
    else
    if Key = VK_ESCAPE then
    begin
      CompleteOperation(false);
      Key := 0;
      exit;
    end;
  end;
  inherited;
end;

{$else}
procedure TElInpEdit.WMGetDlgCode(var Message: TWMGetDlgCode); // CNS
begin
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS; // we want the RETURN key
end;
{$endif}

procedure TElInpEdit.KeyPress(var Key: Char); // CNS
begin                                   // get rid of Windows beep
  if Key = Chr(VK_RETURN) then
    key := Chr(0);
  if Key = Chr(VK_ESCAPE) then
    key := Chr(0);
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TElTreeInplaceEdit.TriggerAfterOperation(var Accepted : boolean; var
    DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
    ValueAsText := FEditor.Text;
end;

procedure TElTreeInplaceEdit.TriggerBeforeOperation(var DefaultConversion :
    boolean);
begin
  inherited;
  if DefaultConversion then
    FEditor.Text := ValueAsText;
  FEditor.BoundsRect := FCellRect;
end;

{$ifndef CLX_USED}
procedure TElTreeInplaceEdit.EditorWndProc(var Message : TMessage);
var InputValid : boolean;
begin
  if Message.Msg = WM_GETDLGCODE then
  begin
    inherited;
    Message.Result := Message.Result or DLGC_WANTCHARS or DLGC_WANTARROWS or DLGC_WANTALLKEYS;
  end
  else
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
{$endif}

procedure TElTreeInplaceEdit.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus; 
end;

procedure TElTreeInplaceEdit.DoStopOperation(Accepted : boolean);
begin
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

function TElTreeInplaceEdit.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

constructor TElTreeInplaceEdit.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := TElInpEdit.Create(Self);
  {$ifndef CLX_USED}
  FEditor.ParentCtl3D := false;
  FEditor.Ctl3D := false;
  FEditor.AutoSelect := true;
  {$endif}
  FEditor.Visible := false;
  FEditor.AutoSize := false;
  {$ifndef CLX_USED}
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := Self.EditorWndProc;
  {$endif}
  FTypes := [sftText];
end;

destructor TElTreeInplaceEdit.Destroy;
begin
  {$ifndef CLX_USED}
  FEditor.WindowProc := SaveWndProc;
  {$endif}
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

procedure TElTreeInplaceEdit.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;

function TElTreeInplaceMemo.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TElTreeInplaceMemo.TriggerAfterOperation(var Accepted : boolean; var 
    DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
    ValueAsText := FEditor.Text;
end;

procedure TElTreeInplaceMemo.TriggerBeforeOperation(var DefaultConversion : 
    boolean);
begin
  inherited;
  if DefaultConversion then
    FEditor.Text := ValueAsText;
  FEditor.BoundsRect := FCellRect;
end;

constructor TElTreeInplaceMemo.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := TElInpMemo.Create(Self);
  {$ifndef CLX_USED}
  FEditor.ParentCtl3D := false;
  FEditor.Ctl3D := false;
  {$endif}
  FEditor.Visible := false;
  {$ifndef CLX_USED}
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := EditorWndProc;
  {$endif}
  FTypes := [sftMemo];
end;

destructor TElTreeInplaceMemo.Destroy;
begin
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

procedure TElTreeInplaceMemo.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus;  
end;

procedure TElTreeInplaceMemo.DoStopOperation(Accepted : boolean);
begin
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

{$ifndef CLX_USED}
procedure TElTreeInplaceMemo.EditorWndProc(var Message : TMessage);
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
{$endif}

procedure TElTreeInplaceMemo.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;

{$ifndef CLX_USED}
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
  begin
    case Self.DataType of
      sftDate: ValueAsText := DateToStr(FEditor.Date);
      sftTime: ValueAsText := TimeToStr(FEditor.Time);
    end;
  end;
end;

procedure TElTreeInplaceDateTimePicker.TriggerBeforeOperation(var 
    DefaultConversion : boolean);
begin
  inherited;
  if DefaultConversion then
    case Self.DataType of
      sftDate: FEditor.Date := StrToDate(ValueAsText);
      sftTime: FEditor.Time := StrTotime(ValueAsText);
    end;
  FEditor.BoundsRect := FCellRect;
end;

constructor TElTreeInplaceDateTimePicker.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := TElInpDateTimePicker.Create(Self);
  FEditor.Visible := false;
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := EditorWndProc;
  FTypes := [sftDate, sftTime];
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

procedure TElTreeInplaceDateTimePicker.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;


constructor TElTreeInplaceComboBox.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := THackInplaceComboBox.Create(Self);
  FEditor.Visible := false;
  FEditor.ParentCtl3D := false;
  FEditor.Ctl3D := false;
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := EditorWndProc;
  FTypes := [sftEnum];
end;

destructor TElTreeInplaceComboBox.Destroy;
begin
  FEditor.Free;
  FEditor := nil;
  inherited;
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
        exit;
      end;
    end;
  end
  else
  if (Message.Msg = WM_CANCELMODE) or
     ((Message.Msg = CM_CANCELMODE) and
      (TObject(Pointer(Message.lParam)) <> Editor)) or
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

procedure TElTreeInplaceComboBox.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
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

procedure THackInplaceComboBox.KeyPress(var Key: Char);
begin                                      // get rid of Windows beep
  if Key = Chr(VK_RETURN) then
    key := Chr(0);
  if Key = Chr(VK_ESCAPE) then
    key := Chr(0);
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure THackInplaceComboBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTALLKEYS; // we want the RETURN key
end;
{$endif}

constructor TElTreeInplaceCheckBox.Create(AOwner : TComponent);
begin
  inherited;
  FEditor := TElInpCheckBox.Create(Self);
  FEditor.Visible := false;
  {$ifndef CLX_USED}
  SaveWndProc := FEditor.WindowProc;
  FEditor.WindowProc := EditorWndProc;
  {$endif}
  FDefaultValueAsText := '*';
  FTypes := [sftBool];
end;

destructor TElTreeInplaceCheckBox.Destroy;
begin
  FEditor.Free;
  FEditor := nil;
  inherited;
end;

procedure TElTreeInplaceCheckBox.DoStartOperation;
begin
  FEditor.Visible := true;
  FEditor.SetFocus;
end;

procedure TElTreeInplaceCheckBox.DoStopOperation(Accepted : boolean);
begin
  FEditor.Visible := false;
  FEditor.Parent := nil;
  inherited;
end;

{$ifndef CLX_USED}
procedure TElTreeInplaceCheckBox.EditorWndProc(var Message : TMessage);
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
{$endif}

function TElTreeInplaceCheckBox.GetVisible: Boolean;
begin
  Result := FEditor.Visible;
end;

procedure TElTreeInplaceCheckBox.TriggerAfterOperation(var Accepted : boolean;
    var DefaultConversion : boolean);
begin
  FEditor.OnExit := nil;
  inherited;
  if DefaultConversion then
  begin
    if Editor.Checked then
      ValueAsText := FDefaultValueAsText
    else
      ValueAsText := '';
  end;
end;

procedure TElTreeInplaceCheckBox.TriggerBeforeOperation(var DefaultConversion :
    boolean);
begin
  inherited;
  if DefaultConversion then
    FEditor.Checked := ValueAsText <> '';
  FEditor.BoundsRect := FCellRect;
end;

procedure TElTreeInplaceCheckBox.SetEditorParent;
begin
  FEditor.Parent := FTree.View;
end;

procedure TElInpCheckBox.KeyPress(var Key: Char);
begin                                      // get rid of Windows beep
  if Key = Chr(VK_RETURN) then
    key := Chr(0);
  if Key = Chr(VK_ESCAPE) then
    key := Chr(0);
  if Key <> #0 then
    inherited KeyPress(Key);
end;

{$ifdef CLX_USED}

constructor TElInpCheckBox.Create;
begin
  inherited;
  InputKeys := InputKeys + [ikReturns, ikEsc];
end;

procedure TElInpCheckBox.DoExit;
var InputValid : boolean;
begin
  inherited;
  with TElTreeInplaceEdit(Owner) do
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

procedure TElInpCheckBox.KeyDown(var Key : Word; Shift : TShiftState);
var InputValid : boolean;
begin
  with TElTreeInplaceEdit(Owner) do
  if Shift = [] then
  begin
    if Key = VK_RETURN then
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
      Key := 0;
    end
    else
    if Key = VK_ESCAPE then
    begin
      CompleteOperation(false);
      Key := 0;
      exit;
    end;
  end;
  inherited;
end;
{$else}
procedure TElInpCheckBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTALLKEYS; // we want the RETURN key
end;
{$endif}

{$ifndef CLX_USED}
procedure TElInpDateTimePicker.KeyPress(var Key: Char);
begin                                      // get rid of Windows beep
  if Key = Chr(VK_RETURN) then
    key := Chr(0);
  if Key = Chr(VK_ESCAPE) then
    key := Chr(0);
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TElInpDateTimePicker.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTALLKEYS; // we want the RETURN key
end;
{$endif}

end.
