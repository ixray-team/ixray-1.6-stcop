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

unit ElShutdownWatcher;

interface

var IsShuttingDown : boolean;
    WatcherActive  : boolean;

implementation

uses
  Forms, Classes, Windows, Messages, SysUtils;

type
  TShutdownThread = class(TThread)
  private
    Wnd : HWND;
    procedure WndProc(var Msg : TMessage);
    procedure CloseApp;
  protected
    procedure Execute; override;
  end;

var StopWatcherEvent : THandle;

procedure TShutdownThread.CloseApp;
begin
  if (Application.MainForm <> nil) and (not Application.Terminated) then
     Application.MainForm.Close
  else 
    PostMessage(Application.Handle, WM_QUIT, 0, 0);
end;

procedure TShutdownThread.WndProc(var Msg : TMessage);
begin
  if (Msg.Msg = WM_QUERYENDSESSION) and WatcherActive then
  begin
    IsShuttingDown := true;
    Synchronize(CloseApp);
    WaitForSingleObject(StopWatcherEvent, INFINITE);
    ResetEvent(StopWatcherEvent);
    Msg.Result := 1;
  end else
    DefWindowProc(Wnd, Msg.Msg, Msg.wParam, msg.lParam);
end;

procedure TShutdownThread.Execute;
var Msg : TMsg;
    i : LongBool;
    AHandle : THandle;
begin
  StopWatcherEvent := CreateEvent(nil, true, false, nil);
  Wnd := AllocateHWND(WndProc);
  repeat
    i := GetMessage(Msg, 0, 0, 0);
    if i = TRUE then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
      if WaitForSingleObject(StopWatcherEvent, 0) = WAIT_OBJECT_0 then
         break;
    end;
  until i <> TRUE;                
  DeallocateHWND(Wnd);
  AHandle := StopWatcherEvent;
  StopWatcherEvent := 0;
  CloseHandle(AHandle);
end;

var Watcher : TShutdownThread;

initialization
  
  IsShuttingDown := false;
  Watcher := TShutdownThread.Create(true);
  Watcher.FreeOnTerminate := true;
  Watcher.Resume;

finalization
  if StopWatcherEvent <> 0 then 
     SetEvent(StopWatcherEvent);

end.

