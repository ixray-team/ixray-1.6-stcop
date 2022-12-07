{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 2001, EldoS, Akzhan Abdulin        }
{                                                    }
{====================================================}

(*

Version History

06/09/2002

  Fixed getter for the ImageIndex

05/31/2002

  Changed Name parameter in OnImageName event to TElFString to provide Unicode support
  Fixed programmatic setting of ImageIndex

04/23/2002

  ImageIndex was offset by 2 when ShowEmptyValue was true. Fixed.

03/26/2002

  Now it is possible to specify the name for empty value
  and to draw the name instead of the index

03/06/2002

  Added unicode hint

01/11/2002

  Fixed painting of the image in the combo box

12/24/2001

  Fixed ShowEmptyValue behaviour when ItemIndex is changed programmatically

12/16/2001

  Now the image is drawn to the left from the edit box
  Added ShowEmptyValue property

10/02/2001

  Added ManualEdit property

07/15/2001 (c) Akzhan Abdulin

  Initiated (tested under Delphi 5 only)

*)

unit ElImgCombo;

{$I '..\ElPack.inc'}

interface

uses
  Windows,
  Messages,
  Classes,
  ImgList,
  Controls,
  StdCtrls,

  CommCtrl,
{$ifdef VCL_6_USED}
  Types,
{$endif}
  ElTools,
  ElACtrls,
{$ifdef ELPACK_UNICODE}  
  ElUnicodeStrings,
{$endif}
  ElStrUtils;

type

{$ifdef MSWINDOWS}
{$ifdef ELPACK_UNICODE}
  TElFStrings = TElWideStrings;
  TElFStringList = TElWideStringList;
{$else}
  TElFStrings = TStrings;
  TElFStringList = TStringList;
{$endif}
{$else}
  TElFStrings = TStrings;
  TElFStringList = TStringList;
{$endif}


  TElImageNameEvent = procedure(Sender : TObject; Index : integer; var Text : TElFString) of object;

  TElImageComboBox = class(TElAdvancedComboBox)
  private
    FChLink   : TChangeLink;
    FImages   : TImageList;
    FModified : Boolean;
    FDummyInt : integer;
    FImageNames: TElFStrings;
    IOffs: Integer;
    OwnMoveFlag: Boolean;
    FUseImageNames: Boolean;

    procedure ImagesChanged(Sender : TObject);
    procedure SetImages(const Value: TImageList);
    procedure Remake;
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    procedure SetModified(const Value: Boolean);
    procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure SetImageNames(Value: TElFStrings);
  protected
    FOnImageName: TElImageNameEvent;
    FManualEdit: Boolean;
    FEmptyValueText: string;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DropDown; override;
    procedure DblClick; override;
    procedure Change; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
      ComboProc: Pointer); override;
    procedure TriggerImageNameEvent(Index : Integer; var Text : TElFString); 
        virtual;
    procedure SetManualEdit(Value: Boolean);
    function GetShowEmptyValue: Boolean;
    procedure SetShowEmptyValue(Value: Boolean);
    procedure WMNCCalcSize(var Message: TMessage); message WM_NCCALCSIZE;
    procedure EditWndProc(var Message : TMessage); override;
    procedure WndProc(var Message : TMessage); override;
    procedure UpdateEditSize;
    procedure Loaded; override;
    procedure SetEmptyValueText(const Value: string);
    procedure RebuildList;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ItemIndex : integer read GetImageIndex write SetImageIndex;
  published
    property Items : integer read FDummyInt;
    property Style: Integer read FDummyInt;

    property Images: TImageList read FImages write SetImages;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default -1;
    property Modified: Boolean read FModified write SetModified;
    property ManualEdit: Boolean read FManualEdit write SetManualEdit default true;
    property ShowEmptyValue: Boolean read GetShowEmptyValue write SetShowEmptyValue
        default true;
    property EmptyValueText: string read FEmptyValueText write SetEmptyValueText;

    property OnImageName: TElImageNameEvent read FOnImageName write FOnImageName;
    property ImageNames: TElFStrings read FImageNames write SetImageNames;
    property UseImageNames: Boolean read FUseImageNames write FUseImageNames default false;

    {$ifdef VCL_4_USED}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property ParentBiDiMode;
    {$endif}
    property Color;
    property Ctl3D;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$ifdef VCL_5_USED}
    property OnContextPopup;
    {$endif}
  end;

implementation

uses
  Graphics,
  SysUtils;

{ TElImageIndexEdit }

procedure TElImageComboBox.Change;
begin
  SetModified(True);
  Invalidate;
end;

procedure TElImageComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; 
    ComboProc: Pointer);
begin
  with TWMChar(Message) do
  begin
    if Msg = WM_CHAR then
    begin
      if Char(CharCode) = '-' then
      begin
        ImageIndex := - ImageIndex;
        SetModified(True);
        SelStart := 1;
        SelLength := Length(Text) - 1;
        Result := 0;
        Invalidate;
        Exit;
      end;
      if (Length(Text) > 0) and (SelStart = 0) and (SelLength = 0) and (Text[1] = '-') then
      begin
        SelectAll;
      end;
    end;
  end;
  inherited;
end;

constructor TElImageComboBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csDoubleClicks];
  FChLink  := TChangeLink.Create;
  FChLink.OnChange := ImagesChanged;
  IOffs := -1;
  FEmptyValueText := '-1';
  FImageNames := TElFStringList.Create;
  FUseImageNames := false;
end;

procedure TElImageComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.style := Params.style or CBS_OWNERDRAWFIXED; 
end;

procedure TElImageComboBox.CreateWnd;
begin
  inherited CreateWnd;
  if (EditHandle <> 0) then
  begin
    SetWindowLong(EditHandle, GWL_STYLE, GetWindowLong(EditHandle, GWL_STYLE) or ES_NUMBER);
  end;
  if (inherited Items[0] <> EmptyValueText) and (IOffs = -1) then
  begin
    inherited Items.Add(EmptyValueText);
    Text := EmptyValueText;
  end;
end;

procedure TElImageComboBox.DblClick;
var
  NewImageIndex: Integer;
begin
  if Assigned(Images) and (Images.Count > 0) then
  begin
    NewImageIndex := ImageIndex;
    if NewImageIndex < -1 then
    begin
      NewImageIndex := -1;
    end;
    if (NewImageIndex >= -1) and (Succ(NewImageIndex) < Images.Count) then
    begin
      Inc(NewImageIndex);
    end
    else
    begin
      NewImageIndex := -1;
    end;
    if ImageIndex <> NewImageIndex then
    begin
      ImageIndex := NewImageIndex;
      SetModified(True);
    end;
  end;
  SelectAll;
end;

destructor TElImageComboBox.Destroy;
begin
  FChLink.Free;
  FImageNames.Free;
  inherited;
end;

procedure TElImageComboBox.DrawItem(Index: Integer; Rect: TRect; State:
    {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
var
  ATextRect: TRect;
  PrevBrushColor: TColor;
  AText : TElFString;
begin
  {$ifdef VCL_4_USED}
  TControlCanvas(Canvas).UpdateTextFlags;
  {$endif}
  ATextRect := Rect;
  AText := inherited Items[Index];
  TriggerImageNameEvent(Index, AText);
  Canvas.FillRect(Rect);
  if Images <> nil then
  begin
    Inc(ATextRect.Left, 1 + Images.Width + 1);
    with Rect do
    begin
      PrevBrushColor := Canvas.Brush.Color;
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(Classes.Rect(Left, Top, ATextRect.Left, Bottom));
      Canvas.Brush.Color := PrevBrushColor;
    end;
    Images.Draw(Canvas, Rect.Left + 1, Rect.Top + 1, Index + IOffs);
  end;
  with ATextRect, Canvas do
    TextRect(ATextRect, Left + 1, (Top + Bottom - TextHeight(AText)) div 2, AText);
end;

procedure TElImageComboBox.DropDown;
begin
  RebuildList;
  inherited;
end;

function TElImageComboBox.GetImageIndex: Integer;
var i : integer;
    S : TElFString;
begin
  i := inherited ItemIndex;
  if (i + IOffs = -1) and ShowEmptyValue then
    s := Self.EmptyValueText
  else
    s := IntToStr(I + IOffs);
  TriggerImageNameEvent(i + IOffs, S);

  if (Text = S) then
  begin
    Result := i + ioffs;
  end
  else
  begin
    i := (inherited items).IndexOf(Text);
    if i <> -1 then
      Result := i + IOffs
    else
      Result := -1;
  end;
end;

procedure TElImageComboBox.ImagesChanged(Sender: TObject);
begin
  Remake;
end;

procedure TElImageComboBox.Notification(AComponent: TComponent; Operation: 
    TOperation);
begin
  inherited;
  if (AComponent = Images) and (Operation = opRemove) then Images := nil;
end;

procedure TElImageComboBox.Remake;
begin
  SendCancelMode(Self);
  if (Images <> nil) and (ItemHeight < Images.Height + GetSystemMetrics(SM_CYEDGE) * 2) then
    inherited ItemHeight := Images.Height;
  RebuildList;
  UpdateEditSize;
  Invalidate;
end;

procedure TElImageComboBox.SetImageIndex(const Value: Integer);
var s : TElFString;
    i : integer;
begin
  if HandleAllocated then
  begin
    i := Value - IOffs;
    SendMessage(Handle, CB_SETCURSEL, i, 0);
    if (Value = -1) and ShowEmptyValue then
      s := Self.EmptyValueText
    else
      s := IntToStr(Value);
    TriggerImageNameEvent(Value, S);
    SendMessage(Handle, WM_SETTEXT, 0, Integer(PChar(String(S))));
  end
  else
    Text := IntToStr(Value);
  Invalidate;
end;

procedure TElImageComboBox.SetImages(const Value: TImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
    begin
      {$ifdef VCL_5_USED}
      FImages.RemoveFreeNotification(Self);
      {$endif}
      FImages.UnregisterChanges(FChLink);
    end;
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FChLink);
      FImages.FreeNotification(Self);
    end;
    Remake;
  end;
end;

procedure TElImageComboBox.SetModified(const Value: Boolean);
begin
  if not (csLoading in ComponentState) then
  begin
    FModified := Value;
    if Modified then
    begin
      Inherited Change;
    end;
  end;
end;

procedure TElImageComboBox.TriggerImageNameEvent(Index : Integer; var Text : 
    TElFString);
begin
  if (FUseImageNames) and (Index<FImageNames.Count) then
    Text := FImageNames.Strings[Index];
  if Assigned(FOnImageName) then
    FOnImageName(Self, Index, Text);
end;

procedure TElImageComboBox.SetManualEdit(Value: Boolean);
begin
  if FManualEdit <> Value then
  begin
    FManualEdit := Value;
    if FManualEdit then
      inherited Style := csDropDown
    else
      inherited Style := csDropDownList;
    RecreateWnd;
  end;
end;

function TElImageComboBox.GetShowEmptyValue: Boolean;
begin
  Result := IOffs = -1;
end;

procedure TElImageComboBox.SetShowEmptyValue(Value: Boolean);
begin
  if Value then
    IOffs := -1
  else
    IOffs := 0;
  Invalidate;
end;

procedure TElImageComboBox.WMNCCalcSize(var Message: TMessage);
begin
  inherited;
  //if Images <> nil then
  //  Inc(TWMNCCalcSize(Message).CalcSize_Params.rgrc[0].Left, Images.Width);
end;

procedure TElImageComboBox.UpdateEditSize;
var R : TRect;
begin
  OwnMoveFlag := true;
  R := Rect(0, 0, Width, Height);
  InflateRect(R, -(GetSystemMetrics(SM_CXEDGE) +1 ), -(GetSystemMetrics(SM_CYEDGE) + 1));
  Dec(R.Right, GetSystemMetrics(SM_CXVSCROLL));
  if Images <> nil then
    inc(R.Left, Images.Width);
  SetWindowPos(EditHandle, 0, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, SWP_NOZORDER);
  OwnMoveFlag := false;
end;

procedure TElImageComboBox.EditWndProc(var Message : TMessage);
begin
  inherited;
  if Message.Msg = WM_CREATE then
  begin
    SetWindowLong(EditHandle, GWL_STYLE, GetWindowLong(EditHandle, GWL_STYLE) or ES_NUMBER);
  end;
  if (Message.Msg = WM_SIZE) and not OwnMoveFlag then
  begin
    UpdateEditSize;
  end;
end;

procedure TElImageComboBox.WndProc(var Message : TMessage);
begin
  if (Message.Msg = WM_PARENTNOTIFY) then
  begin
    inherited;
    if LoWord(Message.wParam) = WM_CREATE then
    begin
      if EditHandle <> 0 then
      begin
        UpdateEditSize;
      end;
    end;
  end
  else
    inherited;
end;

procedure TElImageComboBox.WMPaint(var Msg: TWMPaint);
var R,R1   : TRect;
    DC     : HDC;
    ABrush : HBrush;
    ARgn   : HRGN;
begin
  inherited;
  if Images = nil then exit;
  DC := GetDC(Handle);
  R := Rect(GetSystemMetrics(SM_CXEDGE), GetSystemMetrics(SM_CYEDGE), Images.Width + 1, Height - GetSystemMetrics(SM_CYEDGE)*2 + 1);
  with R do
    ARgn := CreateRectRgn(Left, Top, Right, Bottom);
  SelectClipRgn(DC, ARgn);
  ABrush := CreateSolidBrush(ColorToRGB(Color));
  FillRect(DC, R, ABrush);
  CenterRects(Images.Width, R.Right - R.Left, Images.Height, R.Bottom - R.Top, R1);
  OffsetRect(R1, R.Left, R.Top);
  if ImageIndex <> -1 then
    ImageList_Draw(Images.Handle, ImageIndex, DC, R1.Left, R1.Top, ILD_NORMAL);
  DeleteObject(ARgn);
  ReleaseDC(Handle, DC);
end;

procedure TElImageComboBox.CNCommand(var Message: TWMCommand);
begin
  inherited;
  if Message.NotifyCode = CBN_CLOSEUP then
  begin
    UpdateEditSize;
    Invalidate;
  end;
end;

procedure TElImageComboBox.Loaded;
begin
  inherited;
  Remake;
end;

procedure TElImageComboBox.SetEmptyValueText(const Value: string);
begin
  if FEmptyValueText <> Value then
  begin
    FEmptyValueText := Value;
    if ShowEmptyValue then
    begin
      inherited Items[0] := FEmptyValueText;
      if ItemIndex = 0 then
        Text := FEmptyValueText;
    end;
  end;
end;

procedure TElImageComboBox.SetImageNames(Value: TElFStrings);
begin
  FImageNames.Assign(Value);
end;

procedure TElImageComboBox.RebuildList;
var
  I: Integer;
  AText : TElFString;
begin
  with inherited Items do
  begin
    Clear;
    if IOffs <> 0 then
      inherited Items.Add(EmptyValueText);
    if Images = nil then Exit;
    for I := 0 to Images.Count - 1 do
    begin
      AText := IntToStr(i);
      TriggerImageNameEvent(i, AText);
      inherited Items.Add(AText);
    end;
  end;
end;

end.
