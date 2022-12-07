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

unit ElClipMon;

interface

uses
  sysUtils,
  Classes,
  Messages,
  Controls,
  ElCBFmts,
  Windows,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElBaseComp;

type

    TElClipboardMonitor = class(TElBaseComponent)
    protected
      FPrevHandle : HWND;
      FDataFormats: TStrings;
      FOnChange: TNotifyEvent;

      procedure WndProc(var Message : TMessage); override;
      procedure DoSetEnabled(AEnabled : boolean);override;
      procedure TriggerChangeEvent; virtual;
      function GetDataFormats: TStrings;
    public
      destructor Destroy; override;
      function GetDataString(Format : String): string;
      property DataFormats: TStrings read GetDataFormats;
    published
      property Enabled;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

implementation

procedure TElClipboardMonitor.WndProc(var Message : TMessage);
begin
  inherited;
  if Message.Msg = WM_CHANGECBCHAIN then
  begin
    if HWND(Message.wParam) = FPrevHandle then
      FPrevHandle := Message.lParam
    else
      with Message do
        Result := SendMessage(FPrevHandle, Msg, wParam, lParam);
  end
  else
  if Message.Msg = WM_DRAWCLIPBOARD then
  begin
    TriggerChangeEvent;
    with Message do
      Result := SendMessage(FPrevHandle, Msg, wParam, lParam);
  end;
end;

procedure TElClipboardMonitor.DoSetEnabled(AEnabled : boolean);
begin
  if not AEnabled then
    ChangeClipboardChain(Handle, FPrevHandle);
  inherited;
  if AEnabled then
    FPrevHandle := SetClipboardViewer(Handle);
end;

procedure TElClipboardMonitor.TriggerChangeEvent;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TElClipboardMonitor.GetDataFormats: TStrings;
var AFmt : integer;
begin
  if FDataFormats = nil then
    FDataFormats := TStringList.Create;
  FDataFormats.Clear;

  OpenClipboard(0);
  try
    AFmt := 0;
    repeat
      AFmt := EnumClipboardFormats(AFmt);
      FDataFormats.Add(GetFormatName(AFmt));
    until AFmt = 0;
    Result := FDataFormats;
  finally
    CloseClipboard;
  end;
end;

destructor TElClipboardMonitor.Destroy;
begin
  inherited;
  FDataFormats.Free;
end;

function TElClipboardMonitor.GetDataString(Format : String): string;
var i  : integer;
    H  : THandle;
    Buf: PChar;
    Len: integer;
begin
  if DataFormats.IndexOf(Format) = -1 then
    raise Exception.CreateFmt('No format %s present', [Format]);
  OpenClipboard(Handle);
  try
    i := GetFormatIndex(Format);
    if i = 0 then
      Result := ''
    else
    begin
      H := GetClipboardData(i);
      Buf := GlobalLock(H);
      Len := GlobalSize(H);
      SetLength(Result, Len);
      Move(Buf^, Result[1], Len);
      GlobalUnlock(H);
    end;
  finally
    CloseClipboard;
  end;
end;

end.
 
