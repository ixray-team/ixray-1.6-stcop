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

{

Version History
  
03/06/2002

  Added unicode hint

12/28/2201

  It was possible to select clNone color only on second attempt. Fixed.

12/02/2001

  Added OnAddMoreColors event
  Fixed painting in Windows XP with styles disabled

11/05/2001

  Improved alignment of the text

10/24/2001

  ! Now OnChange event is fired ONLY when the user changes this color. 

10/12/2001

  Options were not taken into account when combo was loaded. Fixed 

07/24/2001

  Control has been completely rewritten.  

}

unit ElClrCmb;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Forms,
  Classes,
  Graphics,
  Controls,
  ExtCtrls,
  Dialogs,
  StdCtrls,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElStrUtils,
  ElTools,
  ElTmSchema,
  ElUxTheme,
  ElACtrls;

type

  TColorComboOption = (ccoNoColor, cco4BitColors, ccoSystemColors, ccoCustomChoice, ccoShowNames);
  TColorComboOptions = set of TColorComboOption;

  TTranslateColorNameEvent = procedure(Sender : TObject; Color : TColor; var ColorName : string) of object;
  TColorComboAddMoreColorsEvent = procedure(Sender : TObject; Items : TStrings) of object;

  TElColorCombo = class(TElAdvancedComboBox)
  private
    //internally used
    FDown : Boolean;
    FMouseInControl : boolean;
    //storage for properties
    FDialogOptions : TColorDialogOptions;
    FSelectedColor : TColor;
    FOptions: TColorComboOptions;
    FOnTranslateColorName: TTranslateColorNameEvent;
    FOnAddMoreColors: TColorComboAddMoreColorsEvent;
    FDummyInt   : integer;
    FDummyBool  : boolean;
    FDummyStyle : TComboBoxStyle;
    FInDialog: Boolean;

    //msg handlers
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetOptions(Value: TColorComboOptions);
  protected
    procedure SetSelectedColor(aColor : TColor);
    procedure CreateWnd; override;
    {$ifndef CLX_USED}
    procedure DrawItem(Index: Integer; Rect: TRect; State: {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState); override;
    {$else}
    function DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState): Boolean; override;
    {$endif}
    procedure FillItems;
    procedure Change; override;
    procedure TriggerTranslateColorName(Color : TColor; var ColorName : string); virtual;
    procedure Loaded; override;
    procedure DoAddMoreColors(Items : TStrings); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Items      : integer read FDummyInt;
    property ItemHeight : integer read FDummyInt write FDummyInt;
    property ItemIndex  : integer read FDummyInt write FDummyInt;
    property Style      : TComboBoxStyle read FDummyStyle write FDummyStyle;
    property Transparent : boolean read FDummyBool write FDummyBool;

    property Options: TColorComboOptions read FOptions write SetOptions;
    property DialogOptions : TColorDialogOptions read FDialogOptions write FDialogOptions default [cdFullOpen];
    property SelectedColor : TColor read FSelectedColor write SetSelectedColor default clLime;
    property OnTranslateColorName: TTranslateColorNameEvent read FOnTranslateColorName
                                                            write FOnTranslateColorName;

    property Enabled;
    property Width;
    property Height;
    property TabStop default true;
    property TabOrder;
    property ParentShowHint;
    property ShowHint;

    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnEnter;
    property OnExit;
{$IFDEF VCL_4_USED}
    property Anchors;
    property Action;
    property Constraints;
    property DockOrientation;
    property Floating;
    property DragKind;
    property OnAddMoreColors: TColorComboAddMoreColorsEvent read FOnAddMoreColors 
        write FOnAddMoreColors;
{$ENDIF}
  end;

implementation

{$R *.res}

uses ElVCLUtils;

const sCustom = 'Custom ...';

constructor TElColorCombo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FDown := false;
  FMouseInControl := false;
  Width := 50;
  Height := 21;
  inherited Style := csOwnerDrawFixed;
  inherited ItemHeight := Abs(Font.Height) + GetSystemMetrics(SM_CXEDGE) * 2;
  FSelectedColor := clLime;
  FDialogOptions := [cdFullOpen];
  FOptions := [ccoNoColor, cco4BitColors, ccoSystemColors, ccoShowNames, ccoCustomChoice];
  TabStop := true;
end;

{$ifndef CLX_USED}
procedure TElColorCombo.DrawItem(Index: Integer; Rect: TRect; State:
    {$ifndef VCL_5_USED}StdCtrls{$else}Windows{$endif}.TOwnerDrawState);
{$else}
function TElColorCombo.DrawItem(Index: Integer; Rect: TRect; State:
    TOwnerDrawState): Boolean;
{$endif}
var C : TColor;
    CName, S : string;
    SaveRect : TRect;
    sid      : integer;
begin
  Canvas.Brush.Style := bsSolid;
  if odSelected in State then
    Canvas.Brush.Color := clHighlight
  else
    Canvas.Brush.Color := Color;

  Canvas.FillRect(Rect);
  //InflateRect(Rect, -1, -1);
  if (ccoCustomChoice in Options) and (Index = inherited Items.Count - 1) then
    C := SelectedColor
  else
  try
    C := StringToColor(inherited Items[Index]);
  except
    C := SelectedColor;
  end;
  if ccoShowNames in Options then
  begin
    SaveRect := Rect;
    Rect.Right := Rect.Left + (Rect.Bottom - Rect.Top);
    SaveRect.Left := Rect.Right + 2;
  end;
  with Canvas do
  if C = clNone then
  begin
    Brush.Color := Color;
    FillRect(Rect);
    Brush.Color := clWindowText;
    Pen.Color := clWindowText;
    InflateRect(Rect, -1, -1);
    FrameRect(Rect);
    MoveTo(Rect.Left, Rect.Top);
    LineTo(Rect.Right - 1, Rect.Bottom - 1);
    MoveTo(Rect.Left, Rect.Bottom - 1);
    LineTo(Rect.Right - 1, Rect.Top);
    InflateRect(Rect, 1, 1);
  end
  else
  begin
    Brush.Color := C;
    InflateRect(Rect, -1, -1);
    FillRect(Rect);
    Brush.Color := clWindowText;
    Pen.Color := clWindowText;
    FrameRect(Rect);
    InflateRect(Rect, 1, 1);
  end;

  if ccoShowNames in Options then
  begin
    if (ccoCustomChoice in Options) and (Index = inherited Items.Count - 1) then
    begin
      CName := sCustom
    end
    else
      CName := inherited Items[Index];

    if Uppercase(Copy(CName, 1, 2)) = 'CL' then
      CName := Copy(CName, 3, Length(CName) - 2);

    TriggerTranslateColorName(C, CName);
    Canvas.Font.Assign(Font);
    Canvas.Brush.Style := bsClear;
    Rect := SaveRect;
    if odSelected in State then
      Canvas.Font.Color := clHighlightText;

    if (ThemesAvailable and IsThemeActive) then
    begin
      if not Enabled then
        sid := ETS_DISABLED
      else
      if odSelected in State then
        sid := ETS_SELECTED
      else
      if FMouseOver then
        sid := ETS_HOT
      else
        sid := ETS_NORMAL;
      s := CName;
      DrawThemeTextTo('EDIT', Canvas.Handle, EP_EDITTEXT, sid, PWideChar(WideString(s)), Length(WideString(s)), DT_VCENTER or DT_NOPREFIX or DT_LEFT or DT_SINGLELINE, 0, Rect);
    end
    else
    begin
      DrawText(Canvas.Handle, PChar(CName), Length(CName), Rect, DT_VCENTER or DT_LEFT);
      // Canvas.TextRect(Rect, Rect.Left, Rect.Top, CName);
    end;
  end;

  (*
  if odFocused in State then
  begin
    InflateRect(Rect, 2, 2);
    DrawFocusRect(Canvas.Handle, Rect);
  end;
  *)
  {$ifdef CLX_USED}
  result := true;
  {$endif}
end;

procedure TElColorCombo.CMMouseEnter(var Message : TMessage);
begin
  inherited;
  if FMouseInControl or (not Enabled) then exit;
  FMouseInControl := True;
end;

procedure TElColorCombo.CMMouseLeave(var Message : TMessage);
begin
  inherited;
  if (not FMouseInControl) or (not Enabled) then exit;
  FMouseInControl := False;
end;

procedure TElColorCombo.SetSelectedColor(aColor : TColor);
var S  : String;
    idx: integer;
    SC : TColor;
begin
  if (aColor = FSelectedColor) and (inherited ItemIndex >= 0) then exit;
  sc := FSelectedColor;
  FSelectedColor := aColor;

  if ColorToIdent(aColor, S) then
  begin
    idx := inherited Items.IndexOf(S);
    if idx <> -1 then
    begin
      inherited ItemIndex := idx;
      Invalidate;
      {
      if not (csLoading in ComponentState) then
        Change;
      }
    end
    else
    if ccoCustomChoice in FOptions then
    begin
      inherited ItemIndex := inherited Items.Count - 1;
      Invalidate;
    end
    else
      FSelectedColor := SC;
  end
  else
  begin
    if ccoCustomChoice in FOptions then
    begin
      inherited ItemIndex := inherited Items.Count - 1;
      Invalidate;
    end
    else
      FSelectedColor := SC;
  end;
end;

destructor TElColorCombo.Destroy;  { public }
begin
  inherited;
end;  { Destroy }

procedure TElColorCombo.FillItems;
const
  c4BitColors : array[0..15] of TColor =
    (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clDkGray,
    clLtGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
  cSystemColors : array[0..24] of TColor =
    (clScrollBar, clBackground, clActiveCaption, clInactiveCaption, clMenu,
     clWindow, clWindowFrame, clMenuText, clWindowText, clCaptionText,
     clActiveBorder, clInactiveBorder, clAppWorkSpace, clHighlight,
     clHighlightText, clBtnFace, clBtnShadow, clGrayText, clBtnText,
     clInactiveCaptionText, clBtnHighlight, cl3DDkShadow, cl3DLight,
     clInfoText, clInfoBk);

var i : integer;
    s : string;
begin
  inherited Items.Clear;
  if ccoNoColor in FOptions then
    inherited Items.AddObject('clNone', TObject(Pointer(clNone)));
  if cco4BitColors in FOptions then
  begin
    for i := 0 to 16 - 1 do
    begin
      if ColorToIdent(c4BitColors[i], s) then
        inherited Items.AddObject(s, TObject(Pointer(c4BitColors[i])))
      else
        inherited Items.AddObject(ColorToString(c4BitColors[i]), TObject(Pointer(c4BitColors[i])));
    end;
  end;
  if ccoSystemColors in FOptions then
  begin
    for i := 0 to 25 - 1 do
    begin
      if ColorToIdent(cSystemColors[i], s) then
        inherited Items.AddObject(s, TObject(Pointer(cSystemColors[i])))
      else
        inherited Items.AddObject(ColorToString(cSystemColors[i]), TObject(Pointer(cSystemColors[i])));
    end;
  end;
  if ccoCustomChoice in FOptions then
  begin
    inherited Items.AddObject(sCustom, TObject(Pointer(SelectedColor)));
  end;
  DoAddMoreColors(inherited Items);
end;

procedure TElColorCombo.CreateWnd;
var SC : TColor;
begin
  inherited;
  SC := FSelectedColor;
  FillItems;
  SetSelectedColor(SC);
end;

procedure TElColorCombo.SetOptions(Value: TColorComboOptions);
var SC : TColor;
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if csLoading in ComponentState then exit;
    inherited Items.Clear;
    SC := FSelectedColor;
    FillItems;
    SetSelectedColor(SC);
  end;
end;

procedure TElColorCombo.TriggerTranslateColorName(Color : TColor; var ColorName : string);
begin
  if assigned(FOnTranslateColorName) then
    FOnTranslateColorName(Self, Color, ColorName);
end;

procedure TElColorCombo.Change;
var SC : TColor;
begin
  if (ccoCustomChoice in Options) and (inherited ItemIndex = inherited Items.Count - 1) then
  begin
    if not FInDialog then
    begin
      FInDialog := true;
      with TColorDialog.Create(self) do
      begin
        try
          Options := FDialogOptions;
          Color := FSelectedColor;
          if Execute and (Color <> FSelectedColor) then
          begin
            SetSelectedColor(Color);
            Change;
          end
          else
          begin
            SC := FSelectedColor;
            FSelectedColor := TColor(-2);
            SetSelectedColor(SC);
          end;
        finally
          Free;
        end;
      end;
      FInDialog := false;
    end;
  end
  else
  begin
    try
      SetSelectedColor(Integer(Pointer(inherited Items.Objects[inherited ItemIndex])));
      (*
      if inherited Items[inherited ItemIndex] <> '' then
      begin
        SC := StringToColor(inherited Items[inherited ItemIndex]);
        SetSelectedColor(SC);
      end;
      *)
    except
      on E : EConvertError do ;
    end;
  end;
  inherited;
end;

procedure TElColorCombo.Loaded;
var SC : TColor;
begin
  inherited;
  SC := FSelectedColor;
  FSelectedColor := TColor(-2);
  FillItems;
  SetSelectedColor(SC);
end;

procedure TElColorCombo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  inherited ItemHeight := Abs(Font.Height) + GetSystemMetrics(SM_CXEDGE) * 2;
end;

procedure TElColorCombo.DoAddMoreColors(Items : TStrings);
begin
  if assigned(FOnAddMoreColors) then FOnAddMoreColors(Self, Items);
end;

end.
