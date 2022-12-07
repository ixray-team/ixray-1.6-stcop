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

unit ElTreeModalEdit;

interface

uses
    Classes,
    Messages,
    Windows,
    Forms,
{$ifdef VCL_6_USED}
    Types,
{$endif}
    ElHeader,
    ElTree;

type

    TModalEditExecuteEvent = procedure(Sender : TObject; var Accepted : boolean) of object;

    TElTreeInplaceModalEdit = class(TElTreeInplaceEditor)
    private
      procedure WndProc(var Message : TMessage);
    protected
      RegMsg: DWORD;
      FWnd  : HWND;
      FInProgress : boolean;
      FOnExecute: TModalEditExecuteEvent;
      function GetVisible: Boolean; override;
      procedure StartOperation; override;
      procedure DoStartOperation; override;
      procedure Execute(var Accepted : boolean); virtual;
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
    published
      property OnExecute: TModalEditExecuteEvent read FOnExecute write FOnExecute;
    end;

implementation

constructor TElTreeInplaceModalEdit.Create(AOwner : TComponent);
begin
  inherited;
  RegMsg := RegisterWindowMessage('TElTreeInplaceModalEdit.ControlMessage');
  FWnd := AllocateHwnd(WndProc);
  FTypes := [sftCustom];
end;

destructor TElTreeInplaceModalEdit.Destroy;
begin
  DeallocateHwnd(FWnd);
  inherited;
end;

procedure TElTreeInplaceModalEdit.WndProc(var Message : TMessage);
begin
  if Message.Msg = RegMsg then
  begin
    CompleteOperation(Message.lParam <> 0);
  end
  else
    with Message do
      DefWindowProc(FWnd, Msg, wParam, lParam);
end;

function TElTreeInplaceModalEdit.GetVisible: Boolean;
begin
  Result := FInProgress;
end;

procedure TElTreeInplaceModalEdit.StartOperation;
var Accepted : boolean;
    DefaultConversion : boolean;
begin
  DefaultConversion := true;
  TriggerBeforeOperation(DefaultConversion);
  FInProgress := true;
  Execute(Accepted);
  FInProgress := false;
  PostMessage(FWnd, WM_NULL, 0, 0);
  PostMessage(FWnd, RegMsg, 0, Integer(Accepted));
end;

procedure TElTreeInplaceModalEdit.DoStartOperation;
begin
end;

procedure TElTreeInplaceModalEdit.Execute(var Accepted : boolean);
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self, Accepted)
  else
    Accepted := false;
end;



end.
