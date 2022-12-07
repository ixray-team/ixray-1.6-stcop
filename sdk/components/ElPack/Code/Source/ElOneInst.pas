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

{$ifdef CLX_USED}
error ElOneInstance doesn''t currently work with CLX
{$endif}

unit ElOneInst;

{ Application single instance monitor }

interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Forms,
  ElTools,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElBaseComp;

type
  TInstanceRunEvent = procedure(Sender : TObject; Parameters : TStrings) of object;

type
  PElMemMapArr = ^TElMemMapArr;
  TElMemMapArr = record
    hPrevInst, hPrevAppWin,
      hPrevMainWin, hMonWin : THandle;
  end;

  TExistsEvent = procedure(Sender : TObject;
    hPrevInst : THandle; hPrevAppWin, hPrevMainWin : HWND; var Switch : boolean) of object;

  TElOneInstance = class(TElBaseComponent)
  private
    FMapHandle : THandle;
    FPrevInst,
    FPrevAppWin,
    FPrevMainWin : THandle;

    FView      : PElMemMapArr;
    FMapName   : string;
    FNoAutoTerminate : boolean;

    FOnExists : TExistsEvent;
    FOnInstanceRun : TInstanceRunEvent;

    procedure WMCopyData(var Msg : TMessage); message WM_COPYDATA;
  protected
    { Protected declarations }
    procedure TriggerExistsEvent(hPrevInst, hPrevAppWin, hPrevMainWin : THandle; var Switch : boolean); virtual;
    procedure TriggerInstanceRunEvent(Parameters : TStrings); virtual;
    procedure SetEnabled(AEnabled : boolean); override;
    procedure DoSetEnabled(AEnabled : boolean); override;
    procedure CreateMapping;
  public
    { Public declarations }
    destructor Destroy; override;
    property PrevInstance : THandle read FPrevInst;
    property FPrevMainWindow : THandle read FPrevMainWin;
    property FPrevAppWindow : THandle read FPrevAppWin;
  published
    property MapName : string read FMapName write FMapName;
    property NoAutoTerminate : boolean read FNoAutoTerminate write FNoAutoTerminate;
    property OnExists : TExistsEvent read FOnExists write FOnExists;
    property OnInstanceRun : TInstanceRunEvent read FOnInstanceRun write FOnInstanceRun;
    property Enabled;
  end; { TElOneInstance }

var

  rs_OneInstAlreadyExists : string;

implementation


procedure TElOneInstance.TriggerExistsEvent(hPrevInst, hPrevAppWin, hPrevMainWin : THandle; var Switch : boolean);
begin
  if (assigned(FOnExists)) then
    FOnExists(Self, hPrevInst, hPrevAppWin, hPrevMainWin, Switch);
end; { TriggerExistsEvent }

procedure TElOneInstance.WMCopyData(var Msg : TMessage); { private }
var
  ds : PCOPYDATASTRUCT;
  ParamsList : TStringList;
  Params : string;
begin
  with Msg do
  begin
    ds := PCOPYDATASTRUCT(LParam);
    params := StrPas(PChar(ds.lpData));
    ParamsList := TStringList.Create;
    ParamsList.Text := params;
    TriggerInstanceRunEvent(ParamsList);
    ParamsList.Free;
    Result := 1458;
  end; { with }
end; { WMCopyData }

procedure TElOneInstance.TriggerInstanceRunEvent(Parameters : TStrings);
begin
  if (assigned(FOnInstanceRun)) then FOnInstanceRun(Self, Parameters);
end; { TriggerInstanceRunEvent }

procedure TElOneInstance.SetEnabled(AEnabled : boolean);
begin
  if (FMapName = '') and AEnabled then
    raise Exception.Create('Memory mapping name should be specified to enable Instance Counter.');
  inherited;
end;

procedure TElOneInstance.DoSetEnabled(AEnabled : boolean);
begin
  inherited;
  if AEnabled then
    CreateMapping
  else if FMapHandle <> 0 then
  begin
    CloseHandle(FMapHandle);
    FMapHandle := 0;
  end;
end;

procedure TElOneInstance.CreateMapping;
var
  b, s : boolean;
  ds : TCOPYDATASTRUCT;
  ParamsList : TStringList;
  i : integer;
  Params : string;
  ParentForm : TComponent;
  SA : TSECURITYATTRIBUTES;
 pSD : TSECURITYDESCRIPTOR;

{$IFNDEF VCL_4_USED}
const
  SECURITY_DESCRIPTOR_REVISION = 1;
{$ENDIF}

begin
  if not (csDesigning in ComponentState) then
  begin
    if IsWinNT then
    begin
      if not InitializeSecurityDescriptor(@pSD, SECURITY_DESCRIPTOR_REVISION) then
         raise Exception.Create('Failed to initialize ElOneInstance object');
      if not SetSecurityDescriptorDacl(@pSD, true, nil, false) then
         raise Exception.Create('Failed to initialize ElOneInstance object');
      SA.nLength := sizeof(SA);
      SA.lpSecurityDescriptor := @pSD;
      SA.bInheritHandle := true;
      FMapHandle := CreateFileMapping($FFFFFFFF, @SA, PAGE_READWRITE, 0, sizeof(TElMemMapArr), PChar(FMapName));
    end else
    begin
      FMapHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, sizeof(TElMemMapArr), PChar(FMapName));
    end;
    if FMapHandle <> 0 then
    begin
      b := GetLastError = ERROR_ALREADY_EXISTS;
      FView := nil;
      try
        FView := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(TElMemMapArr));
        if FView = nil then
           raise Exception.Create('Failed to initialize ElOneInstance object');
        if b then
        begin
          s := false;
          TriggerExistsEvent(FView.HPrevInst, FView.hPrevAppWin, FView.hPrevMainWin, s);
          FPrevInst := FView.HPrevInst;
          FPrevMainWin := FView.hPrevMainWin;
          FPrevAppWin := FView.hPrevAppWin;
          if s then
          begin
            ParamsList := TStringList.Create;
            for i := 0 to ParamCount do
              ParamsList.Add(ParamStr(i));
            Params := ParamsList.Text;
            ParamsList.Free;
            ds.dwData := 0;
            ds.cbData := length(params) + 1;
            ds.lpData := PChar(Params);
            SendMessage(FView.hMonWin, WM_COPYDATA, Handle, integer(@ds));
            if not Assigned(FOnInstanceRun) then
              if (FView.hPrevMainWin <> 0) and (IsWindowEnabled(FView.hPrevMainWin)) then
                SetForegroundWindow(FView.hPrevMainWin)
              else
                SetForegroundWindow(FView.hPrevAppWin);
          end;
          if NoAutoTerminate then
          begin
            UnmapViewOfFile(FView);
            FView := nil;
            CloseHandle(FMapHandle);
            raise EBaseEnabledFailed.Create(rs_OneInstAlreadyExists);
          end else
          begin
            Application.Terminate;
          end;
        end
        else
        begin
          FView.hPrevInst := HInstance;
          FView.hPrevAppWin := Application.Handle;
          if Application.MainForm <> nil then
            FView.hPrevMainWin := Application.MainForm.Handle
          else
          begin
            ParentForm := self;
{$IFNDEF VER90}
            while not (ParentForm is TCustomForm) and (ParentForm.Owner <> nil) do
{$ELSE}
            while not (ParentForm is TForm) and (ParentForm.Owner <> nil) do
{$ENDIF}
              ParentForm := ParentForm.Owner;
            if ParentForm = Self then ParentForm := nil;
            if ParentForm <> nil
{$IFDEF VER90} then
              FView.hPrevMainWin := TForm(ParentForm).Handle
{$ELSE} then
              FView.hPrevMainWin := TCustomForm(ParentForm).Handle
{$ENDIF}
            else
              FView.hPrevMainWin := 0;
          end;
          FView.hMonWin := Handle;
        end;
      finally
        if Assigned(FView) then
           UnmapViewOfFile(FView);
      end;
    end
    else
      raise Exception.Create('Failed to initialize ElOneInstance object');
  end;
end;

destructor TElOneInstance.Destroy;
begin
  if FMapHandle <> 0 then CloseHandle(FMapHandle);
  FMapHandle := 0;
  inherited;
end; { Destroy }

initialization
  rs_OneInstAlreadyExists := 'Another instance of the application is started';

end.
