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

(*

Version History


06/05/2002

  Added Position property.

04/16/2002

  The dialog made Unicode.

*)

unit ElInputDlg;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Consts,
  ElBtnCtl,
  ElCheckCtl,
  ElHTMLLbl,
  ElACtrls,
  ElPanel,
  ElEdits,
  HTMLRender,
  ElVCLUtils,
  ExtCtrls,
  ElStrUtils,
  ElCaption,
{$ifdef VCL_6_USED}
  Types,
{$endif}
  ElPopBtn;

type

  TElInputDialog = class(TComponent)
  private
    FOnImageNeeded: TElHTMLImageNeededEvent;
    FOnLinkClick: TElHTMLLinkClickEvent;
  protected
    FPrompt: TElFString;
    FCaption: TElFString;
    FIsHTML: Boolean;
    FValue: TElFString;
    FFont: TFont;
    FParentFont: Boolean;
	FPosition : TPosition;

    procedure SetFont(Value: TFont);
    procedure SetParentFont(Value: Boolean);
    procedure FontChange(Sender : TObject);
  public
    function Execute: Boolean;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Prompt: TElFString read FPrompt write FPrompt;
    property Caption: TElFString read FCaption write FCaption;
    property IsHTML: Boolean read FIsHTML write FIsHTML;
    property Position : TPosition read FPosition write FPosition;
    property Value: TElFString read FValue write FValue;
    property OnImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write
        FOnImageNeeded;
    property OnLinkClick: TElHTMLLinkClickEvent read FOnLinkClick write
        FOnLinkClick;
    property Font: TFont read FFont write SetFont;
    property ParentFont: Boolean read FParentFont write SetParentFont default true;
  end;

function InputQuery(const ACaption, APrompt: TElFString; var AValue: TElFString;
    AIsHTML : boolean): Boolean;

type TInputDlgClass = class of TForm;

{$ifdef VCL_6_USED}
var
{$else}
const
{$endif}
InputDlgClass : TInputDlgClass = TForm;

implementation

uses ElList, ElTools;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function InputQuery(const ACaption, APrompt: TElFString; var AValue: TElFString;
    AIsHTML : boolean): Boolean;
begin
  with TElInputDialog.Create(nil) do
  try
    Caption := ACaption;
    Prompt := APrompt;
    Value := AValue;
    IsHTML := AIsHTML;
    result := Execute;
    if Result then
      AValue := Value;
  finally
    Free;
  end;
end;

function TElInputDialog.Execute: Boolean;
var
  Form: TForm;
  APanel: TElPanel;
  Prompt: TElHTMLLabel;
  Edit: TElEdit;
  DialogUnits: TPoint;
  FFormCaption : TElFormCaption;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := InputDlgClass.Create(Application);
  with Form do
    try
      Font := Self.Font;

      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := FCaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position := poScreenCenter;

      APanel := TElPanel.Create(Form);
      APanel.Parent := Form;
      APanel.Align := alClient;
      APanel.BevelOuter := bvNone;

      FFormCaption := TElFormCaption.Create(Form);
      FFormCaption.Texts.Add.Caption := FCaption;
      FFormCaption.Active := TRUE;
      FFormCaption.BackgroundType := bgtColorFill;
      FFormCaption.ActiveLeftColor := clActiveCaption;
      FFormCaption.ActiveRightColor := clActiveCaption;
      FFormCaption.InActiveLeftColor := clInActiveCaption;
      FFormCaption.InActiveRightColor := clInActiveCaption;

      Prompt := TElHTMLLabel.Create(Form);
      Prompt.IsHTML := IsHTML;
      Prompt.OnLinkClick := FOnLinkClick;
      Prompt.OnImageNeeded := FOnImageNeeded;
      Prompt.Transparent := true;
      with Prompt do
      begin
        Parent := APanel;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := FPrompt;
      end;
      Edit := TElEdit.Create(Form);
      with Edit do
      begin
        Parent := APanel;
        Left := Prompt.Left;
        Top := MulDiv(19, DialogUnits.Y, 8);
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        SelectAll;
        InactiveBorderType := fbtSunkenOuter;
        ActiveBorderType := fbtSunken;
        Flat := true;
        TabStop := true;
        TabOrder := 0;
      end;
      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TElPopupButton.Create(Form) do
      begin
        Parent := APanel;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
        TabStop := true;
        TabOrder := 1;
      end;
      with TElPopupButton.Create(Form) do
      begin
        Parent := APanel;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
        TabStop := true;
        TabOrder := 2;
      end;
      ActiveControl := Edit;
      Position := Self.Position;

      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

procedure TElInputDialog.SetFont(Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    ParentFont := false;
  end;
end;

procedure TElInputDialog.SetParentFont(Value: Boolean);
var AForm : TForm;
begin
  if FParentFont <> Value then
  begin
    if Value then
    begin
      AForm := GetOwnerForm(Self);
      if AForm <> nil then
        Font := AForm.Font;
      FParentFont := true;
    end
    else
      FParentFont := false; 
  end;
end;

constructor TElInputDialog.Create(AOwner : TComponent);
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  Position := poScreenCenter; 
end;

procedure TElInputDialog.FontChange(Sender : TObject);
begin
  ParentFont := false;
  //if FForm <> nil then
  //  FForm.Font := FFont;
end;

destructor TElInputDialog.Destroy;
begin
  inherited;
  FFont.Free;
end;

end.

