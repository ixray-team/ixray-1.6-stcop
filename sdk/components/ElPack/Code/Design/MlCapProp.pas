{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998-2002, EldoS                   }
{                                                    }
{====================================================}

{$I ..\ElPack.inc}

unit MlCapProp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$ifdef VCL_6_USED}
  DesignIntf, DesignEditors, DesignWindows, DsnConst,
{$else}
  DsgnIntf,
{$endif}

  Typinfo,
  {$ifdef ELPACK_UNICODE}
  ElUnicodeStrings,
  {$endif}
  ExtCtrls, StdCtrls, ComCtrls, ElIni, ElFrmPers, ElXPThemedControl,
  ElEdits, Buttons;

type
  TMlCapEditDialog = class(TForm)
    Panel1: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    Panel2: TPanel;
    LineCounter: TLabel;
    Memo: TElEdit;
    Load: TSpeedButton;
    Save: TSpeedButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure MemoChange(Sender : TObject);
    procedure LoadClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
  end;

type
  TMlCaptionProperty = class(TStringProperty)
  private
  public
    procedure Edit; override;
    function GetAttributes : TPropertyAttributes; override;
  end;

{$ifdef ELPACK_UNICODE}
  TElWideStringsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
{$endif}

implementation

{$R *.DFM}

procedure TMlCapEditDialog.MemoChange(Sender : TObject);
begin
  LineCounter.Caption := Format('%d Lines', [Memo.Lines.Count])
end;

procedure TMlCaptionProperty.Edit;
begin
  with TMlCapEditDialog.Create(Application) do
  try
    {$ifdef D_6_UP}
    if GetPropType^.Kind = tkWString then
    begin
      Memo.Text := TypInfo.GetWideStrProp(GetComponent(0), GetPropInfo);
    end
    else
    {$endif}
      Memo.Text := GetValue;

    MemoChange(Self);
    if ShowModal = mrOk then
    begin
      {$ifdef D_6_UP}
      if GetPropType^.Kind = tkWString then
      begin
        SetWideStrProp(GetComponent(0), GetPropInfo, Memo.Text);
      end
      else
      {$endif}
        SetValue(Memo.Text);
    end;
  finally
    Free
  end
end;

function TMlCaptionProperty.GetAttributes : TPropertyAttributes;
begin
  {$ifdef D_6_UP}
  Result := [paDialog];
  {$else}
  Result := [paMultiSelect, paDialog];
  {$endif}
end;

{$ifdef ELPACK_UNICODE}
procedure TElWideStringsProperty.Edit;
begin
  with TMlCapEditDialog.Create(Application) do
  try
    Memo.Lines.Assign(TElWideStrings(GetOrdValue));
    MemoChange(Memo);
    if ShowModal = mrOk then
    begin
      SetOrdValue(Longint(Memo.Lines));
      // TElWideStrings(GetOrdValue).Assign(Memo.Lines);
    end;
  finally
    Free;
  end
end;

function TElWideStringsProperty.GetAttributes : TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

{$endif}

procedure TMlCapEditDialog.LoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    Memo.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TMlCapEditDialog.SaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    Memo.Lines.SaveToFile(SaveDialog.FileName);
end;

end.
