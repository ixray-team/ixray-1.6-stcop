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

unit ElDailyTip;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  ExtCtrls, ElPopBtn, StdCtrls, ElTools, Graphics, ElBtnCtl, ElStrUtils, ElStrPool,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElXPThemedControl,
  ElHTMLLbl, HTMLRender;

type
  TElDailyTipForm = class(TForm)
    OkBtn : TElPopupButton;
    NextTimeCB : TCheckBox;
    NextBtn : TElPopupButton;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Image1 : TImage;
    Panel3 : TPanel;
    Panel4 : TPanel;
    Label1 : TLabel;
    Panel5 : TPanel;
    TipNumLabel : TLabel;
    TipText: TElHTMLLabel;
    procedure NextBtnClick(Sender : TObject);
  private
    MinNum,
      CurNum,
      MaxNum : integer;
    FStringPool : TElstringPool;
  public
    { Public declarations }
  end;

  TElDailyTipDialog = class(TComponent)
  private
    FShowNextTime : Boolean;
    FStartID : Integer;
    FEndID : Integer;
    FShowTipNumber : boolean;
    FStringPool    : TElStringPool;
    FIsHTML        : boolean;
    FOnImageNeeded: TElHTMLImageNeededEvent;
    FOnLinkClick: TElHTMLLinkClickEvent;
    FLinkColor: TColor;
    FLinkStyle: TFontStyles;
    procedure SetStringPool(newValue : TElstringPool);
    procedure SetStartID(newValue : Integer);
    procedure SetEndID(newValue : Integer);
    procedure SetIsHTML(newValue : boolean);
  protected
    procedure SetLinkColor(newValue : TColor); virtual;
    procedure SetLinkStyle(newValue : TFontStyles); virtual;
  public
    procedure Execute;
    constructor Create(AOwner : TComponent); override;
    procedure Notification(AComponent : TComponent; operation : TOperation); override;
  published
    property ShowNextTime : Boolean read FShowNextTime write FShowNextTime;
    property StartID : Integer read FStartID write SetStartID default 10001;
    property EndID : Integer read FEndID write SetEndID default 10001;
    property ShowTipNumber : boolean read FShowTipNumber write FShowTipNumber default true;
    property StringPool : TElStringPool read FStringPool write SetStringPool;

    property IsHTML : boolean read FIsHTML write SetIsHTML;
    property OnImageNeeded: TElHTMLImageNeededEvent read FOnImageNeeded write
        FOnImageNeeded;
    property OnLinkClick: TElHTMLLinkClickEvent read FOnLinkClick write 
        FOnLinkClick;
    property LinkColor: TColor read FLinkColor write SetLinkColor;
    property LinkStyle: TFontStyles read FLinkStyle write SetLinkStyle;
  end;

var
  ElDailyTipForm : TElDailyTipForm;

implementation

procedure TElDailyTipDialog.SetIsHTML(newValue : boolean);
begin
  FIsHTmL := newValue;
end;

procedure TElDailyTipDialog.Notification(AComponent : TComponent; operation : TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FStringPool then StringPool := nil;
  end;
end;

procedure TElDailyTipDialog.SetStringPool(newValue : TElstringPool);
begin
  if FStringPool <> newValue then
  begin
    {$ifdef VCL_5_USED}
    if FStringPool <> nil then FStringPool.RemoveFreeNotification(Self);
    {$endif}
    FStringPool := NewValue;
    if FStringPool <> nil then FStringPool.FreeNotification(Self);
  end;
end;

constructor TElDailyTipDialog.Create(AOwner : TComponent);
begin
  inherited;
  FStartID := 10001;
  FEndID := 10001;
  FShowTipNumber := true;
end; {Create}

procedure TElDailyTipDialog.SetStartID(newValue : Integer);
begin
  if (FStartID <> newValue) and (newValue >= 0) then
  begin
    FStartID := newValue;
  end; {if}
end; {SetStartID}

procedure TElDailyTipDialog.SetEndID(newValue : Integer);
begin
  if (FEndID <> newValue) and (newValue >= FStartID) then
  begin
    FEndID := newValue;
  end; {if}
end; {SetEndID}

procedure TElDailyTipDialog.Execute;
begin
  ElDailyTipForm := TElDailyTipForm.Create(nil);
  Randomize;
  try
    with ElDailyTipForm do
    begin
      if Assigned(Self.FStringPool) then
      begin
        MinNum := 0;
        MaxNum := Self.FStringPool.Items.Count - 1;
      end else
      begin
        MinNum := Min(FStartID, FEndID);
        MaxNum := Max(FStartID, FEndID);
      end;
      CurNum := MinNum + Random(MaxNum - MinNum);
      TipNumLabel.Caption := Format('Tip #%d', [CurNum - MinNum + 1]);
      TipNumLabel.Visible := FShowTipNumber;
      if Assigned(Self.FStringPool) then
         TipText.Caption := Self.FStringPool.Items[CurNum]
      else
         TipText.Caption := LoadStr(CurNum);

      TipText.IsHTML     := IsHTML;
      TipText.LinkColor  := LinkColor;
      TipText.LinkStyle  := LinkStyle;
      TipText.OnLinkClick := FOnLinkClick;
      TipText.OnImageNeeded := FOnImageNeeded;
      NextTimeCB.Checked := FShowNextTime;
      FStringPool := Self.FStringPool;
      ShowModal;
      FShowNextTime := NextTimeCB.Checked;
    end; // with
  finally
    ElDailyTipForm.Free;
  end;
end; {Execute}

procedure TElDailyTipDialog.SetLinkColor(newValue : TColor);
{ Sets data member FLinkColor to newValue. }
begin
  if (FLinkColor <> newValue) then
  begin
    FLinkColor := newValue;
    if IsHTML then
    begin
      FIsHTML := false;
      IsHTML := true;
    end;
  end;  { if }
end;  { SetLinkColor }

procedure TElDailyTipDialog.SetLinkStyle(newValue : TFontStyles);
{ Sets data member FLinkStyle to newValue. }
begin
  if (FLinkStyle <> newValue) then
  begin
    FLinkStyle := newValue;
    if IsHTML then
    begin
      FIsHTML := false;
      IsHTML := true;
    end;
  end;  { if }
end;  { SetLinkStyle }


{$R *.DFM}

procedure TElDailyTipForm.NextBtnClick(Sender : TObject);
begin
  Inc(CurNum);
  if CurNum > MaxNum then CurNum := MinNum;
  if Assigned(FStringPool) then
     TipText.Caption := FStringPool.Items[CurNum]
  else
     TipText.Caption := LoadStr(CurNum);
  TipNumLabel.Caption := Format('Tip #%d', [CurNum - MinNum + 1]);
end;

end.
