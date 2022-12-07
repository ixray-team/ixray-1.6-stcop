{====================================================}
{                                                    }
{   EldoS Visual Components                          }
{                                                    }
{   Copyright (c) 1998 Alex Shovkoplyas              }
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

unit ElBaseComp;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, 
{$ifdef VCL_6_USED}
Types,
{$endif}
ElTools;

type

  EBaseEnabledFailed = class(Exception)
  end;

  TElBaseComponent = class(TComponent)
  protected
    FHandle : THandle;
    FEnabled : boolean;
    FDesignActive : boolean;
    procedure WMQueryEndSession(var Message : TMessage); message WM_QUERYENDSESSION;
    procedure WndProc(var Message : TMessage); virtual;
    procedure SetEnabled(AEnabled : boolean); virtual;
    procedure DoSetEnabled(AEnabled : boolean); virtual;
    procedure Loaded; override;
    property Handle : THandle read FHandle;
  public
    property Enabled : boolean read FEnabled write SetEnabled default false;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

 { TElBaseComponent }

constructor TElBaseComponent.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FHandle := 0;
  FEnabled := false;
end;

destructor TElBaseComponent.Destroy;
begin
  Enabled := false;
  inherited Destroy;
end;

procedure TElBaseComponent.Loaded;
begin
  inherited;
  if Enabled then
  begin
    FEnabled := false;
    SetEnabled(true);
  end;
end;

procedure TElBaseComponent.WndProc(var Message : TMessage);
begin
  if Message.Msg = WM_QUERYENDSESSION then
    Message.Result := 1
  else
  try
    Dispatch(Message);
  except
    Application.HandleException(Self);
  end;
end;

procedure TElBaseComponent.WMQueryEndSession(var Message : TMessage);
begin
  try
    Enabled := false;
  except;
  end;
  inherited;
  Message.Result := integer(true);
end;

procedure TElBaseComponent.SetEnabled(AEnabled : boolean);
begin
  try
    if (FDesignActive or (not (csDesigning in ComponentState))) and
      (not (csLoading in ComponentState)) and (AEnabled <> FEnabled) then DoSetEnabled(AEnabled);
    FEnabled := AEnabled;
  except
    raise;
  end;
end;

procedure TElBaseComponent.DoSetEnabled(AEnabled : boolean);
begin
  if AEnabled then
{$warnings off}
    FHandle := AllocateHwnd(WndProc)
  else
  begin
    if FHandle <> 0 then DeallocateHwnd(FHandle);
{$warnings on}
    FHandle := 0;
  end;
end;

end.
