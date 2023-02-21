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

unit ElHook;

interface

uses
{$ifndef CLX_USED}
    Forms,
    Messages,
    Controls,
{$else}
    QControls,
{$endif}
{$ifdef VCL_6_USED}
Types,
{$endif}
    Classes;

type
  TElHookEvent = procedure(Sender : TObject; var Msg : TMessage; var Handled : boolean) of object;

type
  TElHook = class(TComponent)
  private
    FDesignActive : Boolean;
    FOnBeforeProcess : TElHookEvent;
    FOnAfterProcess : TElHookEvent;
    FActive : Boolean;
{$IFDEF OLD_EL_HOOK}
    FControl : TWinControl;
    procedure SetControl(newValue : TWinControl);
{$ELSE}
    FControl : TControl;
    procedure SetControl(newValue : TControl);
{$ENDIF}
    procedure SetActive(newValue : Boolean);
    procedure SetDesignActive(newValue : Boolean);
  protected
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure TriggerBeforeProcessEvent(var Msg : TMessage; var Handled : boolean); virtual;
    procedure TriggerAfterProcessEvent(var Msg : TMessage; var Handled : boolean); virtual;
  public
    procedure HookControl(AControl : TWinControl);
    destructor Destroy; override;
  published
    property Active : Boolean read FActive write SetActive;
    property DesignActive : Boolean read FDesignActive write SetDesignActive;
{$IFDEF OLD_EL_HOOK}
    property Control : TWinControl read FControl write SetControl;
{$ELSE}
    property Control : TControl read FControl write SetControl;
{$ENDIF}
    property OnBeforeProcess : TElHookEvent read FOnBeforeProcess write FOnBeforeProcess;
    property OnAfterProcess : TElHookEvent read FOnAfterProcess write FOnAfterProcess;
  end;

implementation

uses ElList
{$ifndef CLX_USED}
  , Windows
{$endif}
  ;

{$ifndef CLX_USED}
const
  WM_REMOVEELHOOK = WM_USER + 115;
  WM_RECREATEELHOOK = WM_USER + 116;
{$endif}

type
  TWinControlHack = class(TWinControl);

type
  TCtlHook = class
  private
    FHooks : TElList;
    FControl : TControl;
{$ifndef CLX_USED}
{$IFDEF OLD_EL_HOOK}
    FOldWndProc : integer;
    FElWndProc : pointer;
{$ELSE}
    FOldWndProc : TWndMethod;
    FElWndProc : TWndMethod;
{$ENDIF}
{$endif}
    FDestroying : boolean;
{$ifndef CLX_USED}
    procedure HookWndProc(var Msg : TMessage);
{$endif}
  protected
    procedure RemoveHook(Hook : TElHook);
    procedure AddHook(Hook : TElHook);
    function HooksCount : integer;

    constructor Create;
    destructor Destroy; override;
  end;

type
  TElHookList = class
  private
    FCtlHooks : TElList;
{$ifndef CLX_USED}
    FHandle : THandle;
{$endif}    
    procedure OnCtlHookDelete(Sender : TObject; Item : pointer);
  protected
{$ifndef CLX_USED}
    procedure WndProc(var Message : TMessage);
{$endif}
    function GetCtlHook(AControl : TControl) : TCtlHook;
    constructor Create;
    destructor Destroy; override;
  end;

var
  HookList : TElHookList;

constructor TCtlHook.Create;
begin
  inherited;
  FHooks := TElList.Create;
{$ifndef CLX_USED}
{$IFDEF OLD_EL_HOOK}
  FElWndProc := MakeObjectInstance(HookWndProc);
{$ELSE}
  FElWndProc := HookWndProc;
{$ENDIF}
{$endif}
end;

destructor TCtlHook.Destroy;
begin
  FDestroying := true;
  FHooks.Free;
{$ifndef CLX_USED}
{$ifndef D_2}
  if (@FOldWndProc <> nil) then
  begin
    FControl.WindowProc := FOldWndProc;
    FOldWndProc := nil;
  end;
{$endif}
{$IFDEF OLD_EL_HOOK}
  FreeObjectInstance(FElWndProc);
{$ENDIF}
  FElWndProc := nil;
{$endif}
  inherited;
end;

procedure TCtlHook.RemoveHook(Hook : TElHook);
begin
  FHooks.Remove(Hook);
  if FHooks.Count = 0 then
  begin
{$ifndef CLX_USED}
{$IFDEF OLD_EL_HOOK}
    if (FControl <> nil) and (FOldWndProc <> 0) then
{$ELSE}
    if (FControl <> nil) and (@FOldWndProc <> nil) then
{$ENDIF}
    begin
{$IFNDEF OLD_EL_HOOK}
      try
        FControl.Name := FControl.Name;
        {$ifndef D_2}
        FControl.WindowProc := FOldWndProc;
        {$endif}
      except
        FOldWndProc := nil;
      end;
      FOldWndProc := nil;
{$ELSE}
      if FControl.HandleAllocated then
        SetWindowLong(FControl.Handle, GWL_WNDPROC, FOldWndProc);
      FOldWndProc := 0;
      if HookList <> nil then
         PostMessage(HookList.FHandle, WM_REMOVEELHOOK, 0, integer(Self));
{$ENDIF}
    end;
{$endif}
  end;
end; {RemoveHook}

procedure TCtlHook.AddHook(Hook : TElHook);
begin
  FHooks.Add(Hook);
{$ifndef CLX_USED}
{$IFDEF OLD_EL_HOOK}
  if FOldWndProc = 0 then
  begin
    FControl.HandleNeeded;
    FOldWndProc := GetWindowLong(FControl.Handle, GWL_WNDPROC);
    SetWindowLong(FControl.Handle, GWL_WNDPROC, LongInt(FElWndProc));
  end;
{$ELSE}
  if @FOldWndProc = nil then
  begin
    {$ifndef D_2}
    FOldWndProc := FControl.WindowProc;
    FControl.WindowProc := FElWndProc;
    {$endif}
  end;
{$ENDIF}
{$endif}
end; {AddHook}

function TCtlHook.HooksCount : integer;
begin
  result := FHooks.Count;
end;

{$ifndef CLX_USED}
procedure TCtlHook.HookWndProc(var Msg : TMessage);
var
  i : integer;
  Handled : boolean;
begin
  Handled := false;
  if (Msg.Msg <> WM_QUIT) and (not FDestroying) then
    for i := FHooks.Count - 1 downto 0 do // Iterate
    begin
      TElHook(FHooks[i]).TriggerBeforeProcessEvent(Msg, Handled);
      if Handled then break;
    end; // for
{$IFDEF OLD_EL_HOOK}
  if (Msg.Msg = WM_DESTROY) or (not Handled) then
{$ELSE}
  if (not Handled) then
{$ENDIF}
  begin
{$IFDEF OLD_EL_HOOK}
    with Msg do
      if @FOldWndProc = nil then
        result := CallWindowProc(TWinControlHack(FControl).DefWndProc, FControl.Handle, Msg, wParam, lParam)
      else
        result := CallWindowProc(pointer(FOldWndProc), FControl.Handle, Msg, wParam, lParam);
{$ELSE}
    FOldWndProc(Msg);
{$ENDIF}
  end;
  Handled := false;
  for i := FHooks.Count - 1 downto 0 do // Iterate
  begin
    TElHook(FHooks[i]).TriggerAfterProcessEvent(Msg, Handled);
    if Handled then break;
  end; // for
{$IFDEF OLD_EL_HOOK}
  if (Msg.Msg = WM_DESTROY) then
  begin
    if not (FDestroying or (csDestroying in FControl.ComponentState)) then
    begin
      if HookList <> nil then
         PostMessage(HookList.FHandle, WM_RECREATEELHOOK, 0, integer(Self))
    end
    else
      while FHooks.Count > 0 do
        RemoveHook(TElHook(FHooks[0]));
  end;
{$ENDIF}
end;
{$endif}

constructor TElHookList.Create;
begin
  inherited;
  FCtlHooks := TElList.Create;
  FCtlHooks.OnDelete := OnCtlHookDelete;
{$ifndef CLX_USED}
  FHandle := AllocateHwnd(WndProc);
{$endif}
end;

destructor TElHookList.Destroy;
begin
  FCtlHooks.Free;
{$ifndef CLX_USED}
  if FHandle <> 0 then DeallocateHwnd(FHandle);
  FHandle := 0;
{$endif}
  inherited;
end;

{$ifndef CLX_USED}
procedure TElHookList.WndProc(var Message : TMessage);
var
  CH : TCtlHook;
begin                    
  try
    if Message.Msg = WM_REMOVEELHOOK then
    begin
      if TCtlHook(Message.lParam).FHooks.Count = 0 then
      begin
         FCtlHooks.Remove(pointer(Message.lParam));
      end;
      if FCtlHooks.Count = 0 then
      begin
        Free;
        HookList := nil;
      end;
    end
    else if Message.Msg = WM_RECREATEELHOOK then
    begin
      CH := TCtlHook(Message.lParam);
      try
{$IFDEF OLD_EL_HOOK}
        CH.FControl.HandleNeeded;
        CH.FOldWndProc := GetWindowLong(CH.FControl.Handle, GWL_WNDPROC);
        SetWindowLong(CH.FControl.Handle, GWL_WNDPROC, LongInt(CH.FElWndProc));
{$ELSE}
        {$ifndef D_2}
        CH.FOldWndProc := CH.FControl.WindowProc;
        CH.FControl.WindowProc := CH.FElWndProc;
        {$endif}
{$ENDIF}
      except
      end;
    end
    else
    if Message.Msg = WM_QUERYENDSESSION then
      Message.Result := 1
    else
      Dispatch(Message);
  except
    Application.HandleException(Self);
  end;
end;
{$endif}

function TElHookList.GetCtlHook(AControl : TControl) : TCtlHook;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FCtlHooks.Count - 1 do // Iterate
  begin
    if TCtlHook(FCtlHooks[i]).FControl = AControl then
    begin
      result := TCtlHook(FCtlHooks[i]);
      exit;
    end;
  end; // for
end;

procedure TElHookList.OnCtlHookDelete(Sender : TObject; Item : pointer);
begin
  TCtlHook(Item).Free;
end;

procedure TElHook.SetControl(newValue : TControl);
var
  FOldActive : boolean;
begin
  if (FControl <> newValue) then
  begin
    FOldActive := FActive;
    SetActive(false);
    {$ifdef VCL_5_USED}
    if FControl <> nil then
      FControl.RemoveFreeNotification(Self);
    {$endif}
    FControl := newValue;
    if FControl <> nil then
    begin
      SetActive(FOldActive);
      FControl.FreeNotification(Self);
    end;
  end; {if}
end; {SetControl}

procedure TElHook.SetActive(newValue : Boolean);
var
  CH : TCtlHook;
begin
  if (FActive <> newValue) then
  begin
    if ((csDesigning in ComponentState) or
       ((Owner <> nil) and (csDesigning in Owner.ComponentState))) and
       (not FDesignActive) then
      FActive := newValue
    else
    begin
      if not NewValue then
      begin
        if HookList <> nil then
        begin
          CH := HookList.GetCtlHook(FControl);
          if CH <> nil then
             CH.RemoveHook(Self);
        end;
      end
      else if (FControl <> nil) then
      begin
        if HookList = nil then
           HookList := TElHookList.Create;
        CH := HookList.GetCtlHook(FControl);
        if CH <> nil then
          CH.AddHook(Self)
        else
        begin
          CH := TCtlHook.Create;
          HookList.FCtlHooks.Add(CH);
          CH.FControl := FControl;
          CH.AddHook(Self);
        end;
      end;
      FActive := newValue;
    end;
  end; {if}
end; {SetActive}

procedure TElHook.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FControl) then Control := nil;
end; {Notification}

procedure TElHook.HookControl(AControl : TWinControl);
begin
end; {HookControl}

procedure TElHook.TriggerBeforeProcessEvent(var Msg : TMessage; var Handled : boolean);
begin
  if (assigned(FOnBeforeProcess)) then
    FOnBeforeProcess(Self, Msg, Handled);
end; {TriggerBeforeProcessEvent}

procedure TElHook.TriggerAfterProcessEvent(var Msg : TMessage; var Handled : boolean);
begin
  if (assigned(FOnAfterProcess)) then
    FOnAfterProcess(Self, Msg, Handled);
end; {TriggerAfterProcessEvent}

procedure TElHook.SetDesignActive(newValue : Boolean);
begin
  if (FDesignActive <> newValue) then
  begin
    FDesignActive := newValue;
    if (csDesigning in ComponentState) then
    begin
      if Active then
      begin
        FActive := false;
        Active := true;
      end
      else
      begin
        FActive := true;
        Active := false;
      end;
    end;
  end; {if}
end;

destructor TElHook.Destroy;
begin
  Destroying;
  Control := nil;
  inherited;
end;

initialization
  HookList := nil;

finalization
  HookList.Free;
  HookList := nil;
  
end.
