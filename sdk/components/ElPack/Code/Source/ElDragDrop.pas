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

{$IFDEF BUILDER_USED}
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropTarget)'}
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropSource)'}
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumFORMATETC)'}
{$ENDIF}

unit ElDragDrop;

{ OLE drag source and target }

interface

{$IFDEF VER90}

implementation

{$ELSE}

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  ElTools,
  StdCtrls,
  ExtCtrls,
  Clipbrd,
  ActiveX,
{$ifdef VCL_6_USED}
Types,
{$endif}
  ElCBFmts,
  ElUxTheme,
  ElTmSchema,
  ElXPThemedControl,
  ElHook;

type

  TDragType = (dtCopy, dtMove, dtLink, dtNone);
  TDragTypes = set of TDragType;
  TDragResult = (drDropCopy, drDropMove, drDropLink, drCancel, drOutMemory, drUnknown);

  TDragContent = (edcText, edcBitmap, edcMetafile, edcFileList, edcOther);

  TOleDragObject = class;

  TTargetDragEvent = procedure(Sender : TObject; State : TDragState;
    Source : TOleDragObject; Shift : TShiftState; X, Y : integer;
    var DragType : TDragType) of object;
  TTargetDropEvent = procedure(Sender : TObject; Source : TOleDragObject;
    Shift : TShiftState; X, Y : integer; var DragType : TDragType) of object;
  TSourceDragEvent = procedure(Sender : TObject; DragType : TDragType;
    shift : TShiftState; var ContinueDrop : Boolean) of object;
  TSourceDropEvent = procedure(Sender : TObject; DragResult : TDragResult) of object;
  TOleStartDragEvent = procedure(Sender : TObject; var DragData : pointer;
    var DragDataType : integer; var DragDataSize : integer) of object;

  TElDragDrop = class;

  pFormatList = ^TFormatList;
  TFormatList = array[0..255] of TFormatEtc;

  TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
  private
    fFormatList : pFormatList;
    fFormatCount : Integer;
    fIndex : Integer;
  public
    constructor Create(FormatList : pFormatList; FormatCount, Index : Integer);
    { IEnumFormatEtc }
    function Next(Celt : LongInt; out Elt; pCeltFetched : pLongInt) : HRESULT; stdcall;
    function Skip(Celt : LongInt) : HRESULT; stdcall;
    function Reset : HRESULT; stdcall;
    function Clone(out Enum : IEnumFormatEtc) : HRESULT; stdcall;
    destructor Destroy; override;
  end;

  TOleDragObject = class(TDragObject)
  private
    dataObj : IDataObject;
    Fkeys : longint;
    FDown : boolean;
    FString : string;
    FList : TStringlist;
  protected
    function GetFileList : TStringList;
    function GetString : string;
      //function GetMetafile : TMetafile;
    function GetBitmap : TBitmap;
    function GetDragContent : TDragContent;
    property Keys : longint read Fkeys;
  public
    constructor create;
    destructor destroy; override;

    function GetFormatData(Format : integer) : Pointer;
    function HasDataFormat(Format : integer) : boolean;
    function DataObject : IDataObject;

    property Content : TDragContent read GetDragContent;
    property FileList : TStringList read GetFileList;
    property StringData : string read GetString;
    property Bitmap : TBitmap read GetBitmap;
      //property Metafile : TMetafile read GetMetafile;  { Public }
  end;

  IElDropSource = class(TInterfacedObject, IDropSource, IDataObject)
  private
    FOwner : TElDragDrop;
  protected
    // IDropSource Implementation
    function QueryContinueDrag(FEscapePressed : Bool;
      GrfKeyState : LongInt) : HRESULT; stdcall;
    function GiveFeedback(dwEffect : LongInt) : HRESULT; stdcall;
    function GetData(const FormatEtcIn : TFormatEtc;
      out Medium : TStgMedium) : HRESULT; stdcall;
    function GetDataHere(const FormatEtcIn : TFormatEtc;
      out Medium : TStgMedium) : HRESULT; stdcall;
    function QueryGetData(const FormatEtc : TFormatEtc) : HRESULT; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc : TFormatEtc;
      out FormatEtcOut : TFormatEtc) : HRESULT; stdcall;
    function SetData(const FormatEtc : TFormatEtc; var Medium : TStgMedium;
      fRelease : Bool) : HRESULT; stdcall;
    function EnumFormatEtc(dwDirection : LongInt;
      out EnumFormatEtc : IEnumFormatEtc) : HRESULT; stdcall;
    function dAdvise(const FormatEtc : TFormatEtc; advf : LongInt;
      const advsink : IAdviseSink; out dwConnection : LongInt) : HRESULT; stdcall;
    function dUnadvise(dwConnection : LongInt) : HRESULT; stdcall;
    function EnumdAdvise(out EnumAdvise : IEnumStatData) : HRESULT; stdcall;

    constructor Create(aOwner : TElDragDrop);
  public
    destructor destroy; override;
  end;

  IElDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FOwner   : TElDragDrop;
    FdragObj : TOleDragObject;
  public
    { IDropTarget }
    function DragEnter(const dataObj : IDataObject; grfKeyState : Longint;
      pt : TPoint; var dwEffect : Longint) : HResult; stdcall;
    function DragOver(grfKeyState : Longint; pt : TPoint;
      var dwEffect : Longint) : HResult; stdcall;
    function DragLeave : HResult; stdcall;
    function Drop(const dataObj : IDataObject; grfKeyState : Longint; pt : TPoint;
      var dwEffect : Longint) : HResult; stdcall;
    constructor create(aOwner : TElDragDrop);
    destructor destroy; override;
  end;

  TElDropTarget = class(TComponent, IUnknown, IDropTarget)
  private
    FRefCount : Integer;
    FTarget : TWinControl;
    FDragObj : ToleDragObject;
    FOnTargetDrop : TTargetDropEvent;
    FOnTargetDrag : TTargetDragEvent;
    FDataFormats : TStrings;
    FHook: TElHook;

    procedure SetTarget(newValue : TWinControl);
    procedure AfterMessage(Sender: TObject; var Msg: TMessage; var Handled: 
        boolean);
    procedure BeforeMessage(Sender: TObject; var Msg: TMessage; var Handled: 
        boolean);
  protected
    function QueryInterface(const IID : TGUID; out Obj) : HResult;
{$IFDEF VCL_4_USED}
    override;
{$ENDIF}
    stdcall;
    function _AddRef : Integer; stdcall;
    function _Release : Integer; stdcall;

    function DragEnter(const dataObj : IDataObject; grfKeyState : Longint;
      pt : TPoint; var dwEffect : Longint) : HResult; stdcall;
    function DragOver(grfKeyState : Longint; pt : TPoint;
      var dwEffect : Longint) : HResult; stdcall;
    function DragLeave : HResult; stdcall;
    function Drop(const dataObj : IDataObject; grfKeyState : Longint; pt : TPoint;
      var dwEffect : Longint) : HResult; stdcall;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property DataFormats : TStrings read FDataFormats;
    function HasDataFormat(Format : integer) : boolean;

  published
    property OnTargetDrag : TTargetDragEvent read FOnTargetDrag write FOnTargetDrag;
    property OnTargetDrop : TTargetDropEvent read FOnTargetDrop write FOnTargetDrop;
    property Target : TWinControl read FTarget write SetTarget; { Published }
  end;

  TElDragDrop = class(TElXPThemedControl)
  private
    FOldCursor : TCursor;
    FPressed : boolean;
    FOnTargetDrop : TTargetDropEvent;
    FOnTargetDrag : TTargetDragEvent;
    FOnPaint : TNotifyEvent;
    FIsDropSource : Boolean;
    FIsDropTarget : Boolean;
    FPicture : TPicture;
    FAutoSize : Boolean;
    FContentLength : Integer;
    { Private declarations }
    FContent : Pointer;
    FContentType : Integer;
    FOnOleStartDrag : TOleStartDragEvent;
    FOnSourceDrag : TSourceDragEvent;
    FOnSourceDrop : TSourceDropEvent;
    FElDropTarget : IDropTarget;
    FElDropSource : IDropSource;
    FDragTypes : TDragTypes;
    FDataFormats : TStrings;

    procedure SetContentType(newValue : Integer);
    procedure SetContentLength(newValue : Integer);
    procedure SetPicture(newValue : TPicture);
    procedure SetIsDropTarget(newValue : Boolean);
    procedure WMMouseMove(var Msg : TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Msg : TMessage); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg : TWMLButtonUp); message WM_LBUTTONUP;
    procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
  protected
    { Protected declarations }
    { Event triggers: }
    procedure SetAutoSize(newValue : Boolean);
{$ifdef VCL_6_USED}
    override;
{$endif}
    procedure TriggerSourceDropEvent(DragResult : TDragResult); virtual;
    procedure TriggerOleStartDragEvent(var DragData : pointer; var DragDataType : integer; var DragDataSize : integer); virtual;
    procedure TriggerSourceDragEvent(DragType : TDragType; Shift : TShiftState; var ContinueDrop : Boolean); virtual;
    procedure Paint; override;
    procedure OnPictureChange(Sender : TObject); virtual;
    procedure TriggerPaintEvent; virtual;
    procedure TriggerTargetDragEvent(State : TDragState; Source : TOleDragObject; Shift : TShiftState;
      X, Y : integer; var DragType : TDragType); virtual;
    procedure TriggerTargetDropEvent(Shift : TShiftState; Source : TOleDragObject;
      X, Y : integer; var DragType : TDragType); virtual;
    procedure WMCreate(var Message: TMessage); message WM_CREATE;
    function GetThemedClassName: WideString; override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property Content : Pointer read FContent; { Public }
    property ContentType : Integer read FContentType write SetContentType; { Public }
    property ContentLength : Integer read FContentLength write SetContentLength; { Public }
    property DataFormats : TStrings read FDataFormats;

    property Canvas;
  published
    { Published properties and events }
    property DragCursor;
    property Visible;
    property Enabled;
    property DragTypes : TDragTypes read FDragTypes write FDragTypes;
    property OnOleStartDrag : TOleStartDragEvent read FOnOleStartDrag write FOnOleStartDrag;
    property OnOleSourceDrag : TSourceDragEvent read FOnSourceDrag write FOnSourceDrag;
    property OnSourceDrop : TSourceDropEvent read FOnSourceDrop write FOnSourceDrop;

    property Picture : TPicture read FPicture write SetPicture; { Published }
    property AutoSize : Boolean read FAutoSize write SetAutoSize default false; { Published }
    property IsDropSource : Boolean read FIsDropSource write FIsDropSource; { Published }
    property IsDropTarget : Boolean read FIsDropTarget write SetIsDropTarget; { Published }
    property UseXPThemes;
    
    property OnPaint : TNotifyEvent read FOnPaint write FOnPaint;
    property OnTargetDrag : TTargetDragEvent read FOnTargetDrag write FOnTargetDrag;
    property OnTargetDrop : TTargetDropEvent read FOnTargetDrop write FOnTargetDrop;
  end; { TDragDrop }

implementation

{$R *.RES}

procedure TElDropTarget.Notification(AComponent : TComponent; Operation : TOperation); { protected }
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FTarget) and (Operation = opRemove) then Target := nil;
end; { Notification }

procedure TElDropTarget.SetTarget(newValue : TWinControl);
var
  rslt : HResult;
  obj : IDropTarget;
begin
  if (FTarget <> newValue) then
  begin
    if (csDesigning in ComponentState) then
    begin
      FTarget := newValue;
      exit;
    end;
    {$ifdef VCL_5_USED}
    if FTarget <> nil then
      FTarget.RemoveFreeNotification(Self);
    {$endif}

    if Assigned(FTarget) and (FTarget.HandleAllocated) then
      RevokeDragDrop(FTarget.Handle);

    FHook.Control := newValue;
    FTarget := newValue;
    if FTarget <> nil then
    begin
      newValue.FreeNotification(Self);
      if not GetInterface(Iunknown, obj) then
        raise Exception.Create('GetInterface failed');
      if Target.HandleAllocated then
      begin
        Rslt := RegisterDragDrop(FTarget.Handle, obj as IDroptarget);
        case Rslt of
          S_OK : ;
          DRAGDROP_E_INVALIDHWND : raise Exception.Create('RegisterDragDrop failed, invalide hwnd ');
          DRAGDROP_E_ALREADYREGISTERED : raise Exception.Create('RegisterDragDrop failed, already registered');
          E_OUTOFMEMORY : raise Exception.Create('RegisterDragDrop failed, out of memory');
          E_INVALIDARG : raise Exception.Create('RegisterDragDrop failed, invalid arg');
          CO_E_NOTINITIALIZED : raise Exception.Create('RegisterDragDrop failed, coInitialize had not been called');
        else
          raise Exception.Create('RegisterDragDrop failed, unknown error code ' + IntToStr(rslt and $7FFFFFFF));
        end;
      end;
    end;
  end; { if }
end; { SetTarget }

function TElDropTarget.DragEnter(const dataObj : IDataObject; grfKeyState : Longint; pt : TPoint; var dwEffect : Longint) : HResult;
var
  Shift : TShiftState;
  DragEffect : TDragType;
  FormatEtc : IEnumFormatEtc;
  Fmt : TFormatEtc;

begin
  // TDragState = (dsDragEnter, dsDragLeave, dsDragMove);
  // TDragMessage = (dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop, dmDragCancel,dmFindTarget);

  // Get possible data formats
  FDataFormats.Clear;
  if dataObj.EnumFormatEtc(DATADIR_GET, FormatEtc) = S_OK then
    while FormatEtc.Next(1, Fmt, nil) = S_OK do
      FDataFormats.Add(GetFormatName(Fmt.cfFormat));

  FDragobj := ToleDragObject.Create;
  Fdragobj.dataObj := dataObj;
  Fdragobj.Fkeys := grfKeyState;
  Shift := [];
  if (MK_LBUTTON and grfKeyState <> 0) then Shift := [ssLeft];
  if (MK_RBUTTON and grfKeyState <> 0) then Shift := Shift + [ssRight];
  if (MK_MBUTTON and grfKeyState <> 0) then Shift := Shift + [ssMiddle];
  if (MK_CONTROL and grfKeyState <> 0) then Shift := Shift + [ssCtrl];
  if (MK_SHIFT and grfKeyState <> 0) then Shift := Shift + [ssShift];
  if ($20 and grfKeyState <> 0) then Shift := Shift + [ssAlt];
  DragEffect := dtNone;
  pt := FTarget.ScreenToClient(pt);
  if Assigned(OnTargetDrag) then OnTargetDrag(Self, dsDragEnter, FDragobj, Shift, pt.X, pt.Y, DragEffect);
  case DragEffect of
    dtCopy : dwEffect := DROPEFFECT_COPY;
    dtMove : dwEffect := DROPEFFECT_MOVE;
    dtLink : dwEffect := DROPEFFECT_LINK;
  else
    dwEffect := DROPEFFECT_NONE;
  end;
  Result := S_OK;
end;

function TElDropTarget.DragOver(grfKeyState : Longint; pt : TPoint; var dwEffect : Longint) : HResult;
var
  Shift : TShiftState;
  DragEffect : TDragType;
begin
  Shift := [];
  if (MK_LBUTTON and grfKeyState <> 0) then Shift := [ssLeft];
  if (MK_RBUTTON and grfKeyState <> 0) then Shift := Shift + [ssRight];
  if (MK_MBUTTON and grfKeyState <> 0) then Shift := Shift + [ssMiddle];
  if (MK_CONTROL and grfKeyState <> 0) then Shift := Shift + [ssCtrl];
  if (MK_SHIFT and grfKeyState <> 0) then Shift := Shift + [ssShift];
  if ($20 and grfKeyState <> 0) then Shift := Shift + [ssAlt];
  DragEffect := dtNone;
  pt := FTarget.ScreenToClient(pt);
  if Assigned(OnTargetDrag) then OnTargetDrag(Self, dsDragMove, FDragobj, Shift, pt.X, pt.Y, DragEffect);
  case DragEffect of
    dtCopy : dwEffect := DROPEFFECT_COPY;
    dtMove : dwEffect := DROPEFFECT_MOVE;
    dtLink : dwEffect := DROPEFFECT_LINK;
  else
    dwEffect := DROPEFFECT_NONE;
  end;
  Result := S_OK;
end;

function TElDropTarget.DragLeave : HResult;
var
  DragEffect : TDragType;
begin
  DragEffect := dtNone;
  if Assigned(OnTargetDrag) then OnTargetDrag(Self, dsDragLeave, FDragobj, [], -1, -1, DragEffect);
  if Assigned(FDragObj) then
  begin
    Fdragobj.Free;
    Fdragobj := nil;
  end;
  Result := S_OK;
  FDataFormats.Clear;
end;

function TElDropTarget.Drop(const dataObj : IDataObject; grfKeyState : Longint; pt : TPoint; var dwEffect : Longint) : HResult;
var
  Shift : TShiftState;
  DragEffect : TDragType;

begin
  Shift := [];
  if (MK_LBUTTON and grfKeyState <> 0) then Shift := [ssLeft];
  if (MK_RBUTTON and grfKeyState <> 0) then Shift := Shift + [ssRight];
  if (MK_MBUTTON and grfKeyState <> 0) then Shift := Shift + [ssMiddle];
  if (MK_CONTROL and grfKeyState <> 0) then Shift := Shift + [ssCtrl];
  if (MK_SHIFT and grfKeyState <> 0) then Shift := Shift + [ssShift];
  if ($20 and grfKeyState <> 0) then Shift := Shift + [ssAlt];
  DragEffect := dtNone;
  pt := FTarget.ScreenToClient(pt);
  if Assigned(OnTargetDrop) then OnTargetDrop(self, FDragObj, Shift, pt.X, pt.Y, DragEffect);
  case DragEffect of
    dtCopy : dwEffect := DROPEFFECT_COPY;
    dtMove : dwEffect := DROPEFFECT_MOVE;
    dtLink : dwEffect := DROPEFFECT_LINK;
    dtNone : dwEffect := DROPEFFECT_NONE;
  end;
  if Assigned(FDragObj) then
  begin
    Fdragobj.Free;
    Fdragobj := nil;
  end;
  Result := S_OK;
  FDataFormats.Clear;
end;

function TElDropTarget.HasDataFormat(Format : integer) : boolean;
var
  s : string;
  i : integer;
begin
  s := GetFormatName(Format);
  result := false;
  for i := 0 to FDataFormats.Count - 1 do
    if FDataFormats[i] = s then
    begin
      result := true;
      break;
    end;
end;

function TElDropTarget.QueryInterface;
const
  E_NOINTERFACE = $80004002;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := HRESULT(E_NOINTERFACE);
end;

function TElDropTarget._AddRef : Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TElDropTarget._Release : Integer;
begin
  Dec(FRefCount);
  if FRefCount = 0 then
  begin
    Destroy;
    Result := 0;
    Exit;
  end;
  Result := FRefCount;
end;

constructor TElDropTarget.Create(AOwner : TComponent); { public }
begin
  inherited;
  FHook := TElHook.Create(Self);
  FHook.OnBeforeProcess := BeforeMessage;
  FHook.OnAfterProcess  := AfterMessage;
  FHook.Active := true;
  _AddRef;
  OleInitialize(nil);
  FDataFormats := TStringList.Create;
end; { Create }

destructor TElDropTarget.Destroy; { public }
begin
  SetTarget(nil);
  OleUnInitialize;
  FDataFormats.Free;
  inherited;
end; { Destroy }

procedure TElDropTarget.AfterMessage(Sender: TObject; var Msg: TMessage; var 
    Handled: boolean);
var Rslt : HRESULT;
    obj  : IDropTarget;
begin
  if (Msg.Msg = WM_CREATE) then
  begin
    if not GetInterface(Iunknown, obj) then
      raise Exception.Create('GetInterface failed');
    Rslt := RegisterDragDrop(FTarget.Handle, obj as IDroptarget);
    case Rslt of
      S_OK : ;
      DRAGDROP_E_INVALIDHWND : raise Exception.Create('RegisterDragDrop failed, invalide hwnd ');
      DRAGDROP_E_ALREADYREGISTERED : raise Exception.Create('RegisterDragDrop failed, already registered');
      E_OUTOFMEMORY : raise Exception.Create('RegisterDragDrop failed, out of memory');
      E_INVALIDARG : raise Exception.Create('RegisterDragDrop failed, invalid arg');
      CO_E_NOTINITIALIZED : raise Exception.Create('RegisterDragDrop failed, coInitialize had not been called');
    else
      raise Exception.Create('RegisterDragDrop failed, unknown error code ' + IntToStr(rslt and $7FFFFFFF));
    end;
  end;
end;

procedure TElDropTarget.BeforeMessage(Sender: TObject; var Msg: TMessage; var
    Handled: boolean);
begin
  if Msg.Msg = WM_DESTROY then
  begin
    RevokeDragDrop(FTarget.Handle);
  end;
end;

function IElDropSource.GiveFeedback(dwEffect : LongInt) : HRESULT; { public }
begin
  result := DRAGDROP_S_USEDEFAULTCURSORS;
end; { GiveFeedback }

function IElDropSource.GetData(const FormatEtcIn : TFormatEtc; out Medium : TStgMedium) : HRESULT; { public }
var StreamAdapter : TStreamAdapter;
    Stream        : TStream;
begin
  Medium.tymed := 0;
  Medium.UnkForRelease := nil;
  Medium.hGlobal := 0;
  if (FormatEtcIn.cfFormat = FOwner.FContentType) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (((FormatEtcIn.tymed and TYMED_HGLOBAL) = TYMED_HGLOBAL) or
     ((FormatEtcIn.tymed and TYMED_ISTREAM) = TYMED_ISTREAM)) then
  begin
    if (FormatEtcIn.tymed and TYMED_HGLOBAL) = TYMED_HGLOBAL then
    begin
      Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, FOwner.FContentLength);
      if (Medium.hGlobal = 0) then
      begin
        result := E_OUTOFMEMORY;
        Exit;
      end;
      medium.tymed := TYMED_HGLOBAL;
    end
    else
    begin
      Stream := TMemoryStream.Create;
      StreamAdapter := TStreamAdapter.Create(Stream{$ifdef VCL_4_USED}, soOwned{$endif});
      Medium.stm := Pointer(StreamAdapter as IStream);
      Medium.unkForRelease := Pointer(StreamAdapter as IUnknown);
    end;
    result := GetDataHere(FormatEtcIn, Medium);
  end
  else
  if (FormatEtcIn.tymed and TYMED_HGLOBAL) <> TYMED_HGLOBAL then
    result := DV_E_TYMED
  else
  if FormatEtcIn.dwAspect <> DVASPECT_CONTENT then
    result := DV_E_DVASPECT
  else
    result := DV_E_FORMATETC;
end; { GetData }

function IElDropSource.GetDataHere(const FormatEtcIn : TFormatEtc; out Medium : TStgMedium) : HRESULT; { public }
var
  p : pointer;
  Stream : IStream;
begin
  if (FormatEtcIn.cfFormat = FOwner.FContentType) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (((FormatEtcIn.tymed and TYMED_HGLOBAL) = TYMED_HGLOBAL) or
     ((FormatEtcIn.tymed and TYMED_ISTREAM) = TYMED_ISTREAM)) and
     (Medium.tymed = FormatEtcIn.tymed) then
  begin
    if ((FormatEtcIn.tymed and TYMED_HGLOBAL) = TYMED_HGLOBAL) then
    begin
      if (Medium.hGlobal = 0) then
      begin
        result := E_OUTOFMEMORY;
        Exit;
      end;
      p := GlobalLock(Medium.hGlobal);
      if FOwner.FContent <> nil then
        MoveMemory(p, FOwner.FContent, FOwner.FContentLength);
      GlobalUnlock(Medium.hGlobal);
      Medium.UnkForRelease := nil;
    end
    else
    begin
      Stream := IStream(Medium.stm);
      Stream.Write(FOwner.FContent, FOwner.FContentLength, nil);
    end;
    result := S_OK;
  end
  else
  if (FormatEtcIn.tymed and TYMED_HGLOBAL) <> TYMED_HGLOBAL then
    result := DV_E_TYMED
  else
  if FormatEtcIn.dwAspect <> DVASPECT_CONTENT then
    result := DV_E_DVASPECT
  else
    result := DV_E_FORMATETC;
end; { GetDataHere }

function IElDropSource.QueryGetData; { public }
begin
  if (FormatEtc.cfFormat = FOwner.FContentType) and
    (FormatEtc.dwAspect = DVASPECT_CONTENT) and
    (((FormatEtc.tymed and TYMED_HGLOBAL) = TYMED_HGLOBAL) or
     ((FormatEtc.tymed and TYMED_ISTREAM) = TYMED_ISTREAM)) then
    result := S_OK
  else
  if ((FormatEtc.tymed and TYMED_HGLOBAL) <> TYMED_HGLOBAL) and
     ((FormatEtc.tymed and TYMED_ISTREAM) <> TYMED_ISTREAM) then
    result := DV_E_TYMED
  else
  if FormatEtc.dwAspect <> DVASPECT_CONTENT then
    result := DV_E_DVASPECT
  else
    result := DV_E_FORMATETC;
end; { QueryGetData }

function IElDropSource.GetCanonicalFormatEtc(const FormatEtc : TFormatEtc; out FormatEtcOut : TFormatEtc) : HRESULT; { public }
begin
  FormatEtcOut.ptd := nil;
  Result := E_NOTIMPL;
end; { GetCanonicalFormatEtc }

function IElDropSource.SetData(const FormatEtc : TFormatEtc; var Medium : TStgMedium; fRelease : Bool) : HRESULT; { public }
begin
  result := E_NOTIMPL;
end; { SetData }

function IElDropSource.EnumFormatEtc(dwDirection : LongInt; out EnumFormatEtc : IEnumFormatEtc) : HRESULT; { public }
var
  F : PFormatEtc;
begin
  if (dwDirection = DATADIR_GET) then
  begin
    New(F);
    F^.cfFormat := FOwner.FContentType;
    F^.ptd := nil;
    F^.dwAspect := DVASPECT_CONTENT;
    F^.lIndex := -1;
    F^.tymed := TYMED_HGLOBAL;
    EnumFormatEtc := (TEnumFormatEtc.Create(PFormatList(F), 1, 0) as IEnumFormatEtc);
    result := S_OK;
  end
  else
  if (dwDirection = DATADIR_SET) then
    result := E_NOTIMPL
  else
    result := E_INVALIDARG;
end; { EnumFormatEtc }

function IElDropSource.dAdvise(const FormatEtc : TFormatEtc; advf : LongInt; const advsink : IAdviseSink; out dwConnection : LongInt) : HRESULT; { public }
begin
  result := OLE_E_ADVISENOTSUPPORTED;
end; { dAdvise }

function IElDropSource.dUnadvise(dwConnection : LongInt) : HRESULT; { public }
begin
  result := OLE_E_ADVISENOTSUPPORTED;
end; { dUnadvise }

function IElDropSource.EnumdAdvise(out EnumAdvise : IEnumStatData) : HRESULT; { public }
begin
  result := OLE_E_ADVISENOTSUPPORTED;
end; { EnumdAdvise }

type
  PDropFiles = ^TDropFiles;
  TDropFiles = record
    pfiles : DWORD;
    pt : TPOINT;
    fNC : BOOL;
    fWide : BOOL;
  end;

{function DragMessage(Handle : HWND; Msg : TDragMessage; Source : TDragObject; Target : Pointer; const Pos : TPoint) : Longint;
var
  DragRec : TDragRec;
begin
  Result := 0;
  if Handle <> 0 then
  begin
    DragRec.Pos := Pos;
    DragRec.Target := Target;
    DragRec.Source := Source;
    Result := SendMessage(Handle, CM_DRAG, Longint(Msg), Longint(@DragRec));
  end;
end;}

function IElDropSource.QueryContinueDrag(fEscapePressed : bool; grfKeyState : LongInt) : HRESULT; stdcall;
var
  ContinueDrop : Boolean;
  dragtype : TDragType;
  shift : TShiftState;
begin
  Shift := [];
  if (fEscapePressed) then
    RESULT := dragdrop_s_cancel
  else
  begin
    ContinueDrop := True;
    if (dtCopy in FOwner.FDragTypes) then
      dragtype := dtCopy
    else if (dtMove in FOwner.FDragTypes) then
      dragtype := dtMove
    else if (dtLink in FOwner.FDragTypes) then
      dragtype := dtLink
    else
    begin
      dragtype := dtNone;
      ContinueDrop := False;
    end;
    Shift := [];
    if (MK_LBUTTON and grfKeyState <> 0) then Shift := [ssLeft];
    if (MK_RBUTTON and grfKeyState <> 0) then Shift := Shift + [ssRight];
    if (MK_MBUTTON and grfKeyState <> 0) then Shift := Shift + [ssMiddle];
    if (MK_CONTROL and grfKeyState <> 0) then Shift := Shift + [ssCtrl];
    if (MK_SHIFT and grfKeyState <> 0) then Shift := Shift + [ssShift];
    if ($20 and grfKeyState <> 0) then Shift := Shift + [ssAlt];

    if ContinueDrop then
      FOwner.TriggerSourceDragEvent(DragType, Shift, ContinueDrop);
    if ContinueDrop then
    begin
      if ssLeft in Shift then
        result := S_OK
      else
        RESULT := DRAGDROP_S_DROP;
    end
    else
      RESULT := DRAGDROP_S_CANCEL;
  end;
end;

constructor TOleDragObject.create;
begin
  inherited create;
  FList := TStringList.Create;
end;

destructor TOleDragObject.destroy;
begin
  FList.Free;
  inherited destroy;
end;

function TOleDragObject.DataObject : IDataObject;
begin
  result := dataObj;
end;

function TOleDragObject.HasDataFormat(Format : integer) : boolean;
var
  fmt : TFormatEtc;
  efe : iEnumFormatEtc;
  fmtCount : LongInt;

begin
  if (FDown) or (not assigned(dataobj)) then
  begin
    result := false;
    exit;
  end;
  fillchar(fmt, sizeof(fmt), 0);
  DataObj.EnumFormatEtc(datadir_get, efe);
  EFE.Reset;
  repeat
    fmtCount := 0;
    if efe.Next(1, fmt, @fmtCount) <> S_OK then break;
  until (fmt.cfFormat = Format) or (fmtCount = 0);
  result := (fmt.cfFormat = Format);
end;

function TOleDragObject.GetFormatData(Format : integer) : Pointer;
var
  mdm : TStgMedium;
  pz  : pchar;
  fmt : TFormatEtc;
  efe : iEnumFormatEtc;
  ps  : integer;
  fmtCount : LongInt;
begin
  Result := nil;
  if (not assigned(DataObj)) or (not HasDataFormat(Format)) then exit;
  fillchar(fmt, sizeof(fmt), 0);
  DataObj.EnumFormatEtc(datadir_get, efe);
  EFE.Reset;
  repeat
    fmtCount := 0;
    efe.Next(1, fmt, @fmtCount);
  until (fmt.cfFormat = Format) or (fmtCount = 0);
  if fmt.cfFormat <> Format then
    exit;

  fmt.tymed := TYMED_HGLOBAL;
  fmt.lindex := -1;
  if (dataobj.GetData(fmt, mdm) = S_OK) and
     (fmt.cfFormat = Format) and
     (mdm.tymed = TYMED_HGLOBAL) then
  try
    pz := GlobalLock(mdm.HGlobal);
    ps := GlobalSize(mdm.HGlobal);
    GetMem(result, ps);
    if Result <> nil then
    begin
      MoveMemory(Result, pz, ps);
    end;
    GlobalUnlock(mdm.HGlobal);
  finally
    if Assigned(mdm.unkForRelease) then
      Iunknown(mdm.unkForRelease)._Release;
  end;
end;

function TOleDragObject.GetString;
var
  mdm : TStgMedium;
  pz : pchar;
  fmt : TFormatEtc;
  efe : iEnumFormatEtc;
  fmtCount : LongInt;

begin
  Result := '';
  if (FDown) or (not assigned(DataObj)) or (not HasDataFormat(CF_TEXT)) then exit;
  fillchar(fmt, sizeof(fmt), 0);
  DataObj.EnumFormatEtc(datadir_get, efe);
  EFE.Reset;
  repeat
    fmtCount := 0;
    efe.Next(1, fmt, @fmtCount);
  until (fmt.cfFormat = CF_TEXT) or (fmtCount = 0);
  if fmt.cfFormat <> CF_TEXT then
  begin
    result := '';
    exit;
  end;
  fmt.tymed := TYMED_HGLOBAL;
  fmt.lindex := -1;
  if dataobj.GetData(fmt, mdm) <> S_OK then
    result := ''
  else
  try
    if (fmt.cfFormat = CF_TEXT) and (mdm.tymed = TYMED_HGLOBAL) then
    begin
      pz := GlobalLock(mdm.HGlobal);
      FString := StrPas(pz);
      Result := FString;
      GlobalUnlock(mdm.HGlobal);
    end;
  finally
    if Assigned(mdm.unkForRelease) then
      Iunknown(mdm.unkForRelease)._Release;
//    FDown  := TRUE;
  end;
end;

function TOleDragObject.GetFileList;
var
  mdm : TStgMedium;
  pz : pchar;
  pdf : PDropFiles;
  fmt : TFormatEtc;
  s : string;

  function PWideCharLen(aPWideChar:PWideChar):integer;
  begin
    result := 0;
    while aPWideChar[result] <> #0 do Inc(result);
  end;

begin
  Result := nil;
  if (FDown) or (not assigned(DataObj)) or (not HasDataFormat(CF_HDROP)) then exit;
  Result := FList;
  FList.Clear;
  FillChar(fmt, sizeof(fmt), 0);
  fmt.cfFormat := CF_HDROP;
  fmt.tymed := TYMED_HGLOBAL;
  fmt.lindex := -1;
  if dataobj.GetData(fmt, mdm) <> S_OK then
    raise Exception.Create('IDataObject.GetData failed');
  try
    if mdm.tymed = TYMED_HGLOBAL then
    begin
      pdf := GlobalLock(mdm.HGlobal);
      pz := pchar(pdf);
      Inc(pz, pdf^.pFiles);
      if not (pdf.fWide) then
        while (pz[0] <> #0) do
        begin
          FList.Add(string(pz));
          Inc(pz, 1 + strlen(pz));
        end
      else
        while (pz[0] <> #0) do
        begin
          s := WideCharToString(PWideChar(pz));
          FList.Add(s);
          Inc(pz,PWideCharLen(PWideChar(pz))*2+2);
        end;
      GlobalUnlock(mdm.HGlobal);
    end;
  finally
    if Assigned(mdm.unkForRelease) then
      IUnknown(mdm.unkForRelease)._Release;
//    FDown  := TRUE;
  end;
end;

function TOleDragObject.GetBitmap : TBitmap;
var
  mdm : TStgMedium;
  fmt : TFormatEtc;
  Pict : TBitmap;
  Data : THandle;
  Palette : HPALETTE;
  EnumFormatEtc : IEnumFormatEtc;

begin
  Result := nil;
  if (FDown) or (not assigned(DataObj)) or (not HasDataFormat(CF_BITMAP)) then exit;
  if dataObj.EnumFormatEtc(DATADIR_GET, EnumFormatEtc) <> S_OK then exit;
  Pict := TBitmap.Create;
  EnumFormatEtc.Reset;
  while EnumFormatEtc.Next(1, fmt, nil) = S_OK do
  begin
    if (fmt.cfFormat = CF_BITMAP) then
    begin
      try
        if (DataObj.GetData(fmt, mdm) <> S_OK) or (mdm.tymed <> TYMED_GDI) then
        begin
          Pict.Free;
          exit;
        end;
        Data := mdm.hBitmap;
      finally
        if Assigned(mdm.unkForRelease) then
          Iunknown(mdm.unkForRelease)._Release;
      end;
      if mdm.tymed <> TYMED_GDI then
      begin
        Pict.Free;
        exit;
      end;
      EnumFormatEtc.Reset;
      Palette := 0;
      fillchar(fmt, sizeof(fmt), 0);
      try
        Pict.LoadFromClipboardFormat(CF_BITMAP, Data, Palette);
        result := Pict;
      except
        on E : Exception do ;
      end;
      exit;
    end;
  end;
  Pict.Free;
end;

{function TOleDragObject.GetPicture : TPicture;
var mdm:TStgMedium;
    pz  : pchar;
    fmt : TFormatEtc;
    Pict : TPicture;
    s   : string;
    Data: THandle;
    Format: Word;
    Palette: HPALETTE;
    EnumFormatEtc : IEnumFormatEtc;
    i : integer;

begin
  Result := nil;
  if (FDown) or (Not assigned(DataObj)) then exit;
  if dataObj.EnumFormatEtc(DATADIR_GET, EnumFormatEtc)<>S_OK then exit;
  Pict := TPicture.Create;
  EnumFormatEtc.Reset;
  while EnumFormatEtc.Next(1, fmt, nil) = S_OK do
  begin
    if (fmt.cfFormat <> 3) and (Pict.SupportsClipboardFormat(fmt.cfFormat)) then
    begin
      try
        if DataObj.GetData(fmt, mdm) <> S_OK then exit;
        case mdm.tymed of
          TYMED_GDI: Data := mdm.hBitmap;
          TYMED_MFPICT: Data := mdm.hMetaFilePict;
          TYMED_ENHMF: Data := mdm.hEnhMetaFile;
        end;
        Format := fmt.cfFormat;
      finally
        if Assigned(mdm.unkForRelease) then
           Iunknown(mdm.unkForRelease)._Release;
      end;
      EnumFormatEtc.Reset;
      Palette := 0;
      fillchar(fmt,sizeof(fmt),0);
      while EnumFormatEtc.Next(1, fmt, nil) = S_OK do
      begin
        if fmt.cfFormat = CF_PALETTE then
        begin
          try
            if DataObj.GetData(fmt, mdm) <> S_OK then exit;
            case mdm.tymed of
              TYMED_GDI: Palette := mdm.hBitmap;
              TYMED_MFPICT: Palette := mdm.hMetaFilePict;
              TYMED_ENHMF: Palette := mdm.hEnhMetaFile;
            end;
          finally
            if Assigned(mdm.unkForRelease) then
               Iunknown(mdm.unkForRelease)._Release;
          end;
        end;
      end; // while
      try
        Pict.LoadFromClipboardFormat(Format, Data, Palette);
        result := Pict;
      except

      end;
      exit;
    end;
  end;
  Pict.Free;
end;
}

function TOleDragObject.GetDragContent : TDragContent;
begin
  if HasDataFormat(CF_ENHMETAFILE) then
    result := edcMetaFile
  else if HasDataFormat(CF_METAFILEPICT) then
    result := edcMetaFile
  else if HasDataFormat(CF_BITMAP) then
    result := edcBitmap
  else if HasDataFormat(CF_HDROP) then
    result := edcFileList
  else if HasDataFormat(CF_TEXT) then
    result := edcText
  else
    result := edcOther;
end;
{
function TOleDragObject.GetMetafile : TMetafile;
var mdm:TStgMedium;
    fmt : TFormatEtc;
    Pict : TMetafile;
    Data: THandle;
    EnumFormatEtc : IEnumFormatEtc;
    Meta : PMetafilePict;
    S : TMemoryStream;
    l : integer;
begin
  Result := nil;
  if (FDown) or (Not assigned(DataObj)) then exit;
  if dataObj.EnumFormatEtc(DATADIR_GET, EnumFormatEtc)<>S_OK then exit;
  Pict := TMetafile.Create;
  EnumFormatEtc.Reset;
  while EnumFormatEtc.Next(1, fmt, nil) = S_OK do
  begin
    if (fmt.cfFormat = CF_METAFILEPICT) or (fmt.cfFormat = CF_ENHMETAFILE) then
    begin
      try
        if DataObj.GetData(fmt, mdm) <> S_OK then exit;
        Data := 0;
        if mdm.tymed = TYMED_MFPICT then Data := mdm.hMetaFilePict else
        if mdm.tymed = TYMED_ENHMF then Data := mdm.hEnhMetaFile;
      finally
        if Assigned(mdm.unkForRelease) then
           Iunknown(mdm.unkForRelease)._Release;
      end;
      if Data = 0 then
      begin
        Pict.Free;
        exit;
      end;
      try
        if mdm.tymed = TYMED_MFPICT then
        begin
          Meta := PMetafilePict(GlobalLock(mdm.hMetaFilePict));
          l := GetMetafileBitsEx(Meta.hMF, 0, nil);
          S := TMemoryStream.Create;
          S.SetSize(l);
          if GetMetafileBitsEx(Meta.hMF, l, S.Memory) <l then
          begin
            GlobalUnlock(mdm.hMetaFilePict);
            S.Free;
            Pict.Free;
            Exit;
          end;
          Pict.LoadFromStream(S);
          S.Free;
          result := Pict;
          GlobalUnlock(mdm.hMetaFilePict);
          exit;
        end else
        begin
          Pict.Handle := mdm.hEnhMetaFile;
          result := Pict;
        end;
      except
        On E : Exception do ;
      end;
      exit;
    end;
  end;
  Pict.Free;
end;
}

procedure TElDragDrop.TriggerSourceDragEvent;
begin
  if (assigned(FOnSourceDrag)) then
    FOnSourceDrag(Self, DragType, Shift, ContinueDrop);
end; { TriggerSourceDropEvent }

constructor IElDropSource.Create;
begin
  inherited create;
  OleInitialize(nil);
  FOwner := aOwner;
  SetCapture(FOwner.Handle);
  _AddRef;
end;

destructor IElDropSource.destroy;
begin
  ReleaseCapture;
  oleUnInitialize;
  inherited destroy;
end;

constructor IElDropTarget.create;
begin
  inherited Create;
  OleInitialize(nil);
  FOwner := AOwner;
end;

destructor IElDropTarget.destroy;
begin
  oleUnInitialize;
  inherited destroy;
end;

function IElDropTarget.DragEnter(const dataObj : IDataObject; grfKeyState : Longint; pt : TPoint; var dwEffect : Longint) : HResult;
var
  Shift : TShiftState;
  DragEffect : TDragType;
  FormatEtc : IEnumFormatEtc;
  Fmt : TFormatEtc;

begin
  // TDragState = (dsDragEnter, dsDragLeave, dsDragMove);
  // TDragMessage = (dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop, dmDragCancel,dmFindTarget);
    //get formats
  FOwner.FDataFormats.Clear;
  if dataObj.EnumFormatEtc(DATADIR_GET, FormatEtc) = S_OK then
    while FormatEtc.Next(1, Fmt, nil) = S_OK do
      FOwner.FDataFormats.Add(GetFormatName(Fmt.cfFormat));

  FDragobj := ToleDragObject.Create;
  Fdragobj.dataObj := dataObj;
  Fdragobj.Fkeys := grfKeyState;
  Shift := [];
  if (MK_LBUTTON and grfKeyState <> 0) then Shift := [ssLeft];
  if (MK_RBUTTON and grfKeyState <> 0) then Shift := Shift + [ssRight];
  if (MK_MBUTTON and grfKeyState <> 0) then Shift := Shift + [ssMiddle];
  if (MK_CONTROL and grfKeyState <> 0) then Shift := Shift + [ssCtrl];
  if (MK_SHIFT and grfKeyState <> 0) then Shift := Shift + [ssShift];
  if ($20 and grfKeyState <> 0) then Shift := Shift + [ssAlt];
  DragEffect := dtNone;
  FOwner.TriggerTargetDragEvent(dsDragEnter, FDragobj, Shift, pt.X, pt.Y, DragEffect);
  case DragEffect of
    dtCopy : dwEffect := DROPEFFECT_COPY;
    dtMove : dwEffect := DROPEFFECT_MOVE;
    dtLink : dwEffect := DROPEFFECT_LINK;
    dtNone : dwEffect := DROPEFFECT_NONE;
  end;
  Result := S_OK;
end;

function IElDropTarget.DragOver(grfKeyState : Longint; pt : TPoint; var dwEffect : Longint) : HResult;
var
  Shift : TShiftState;
  DragEffect : TDragType;
begin
  Shift := [];
  if (MK_LBUTTON and grfKeyState <> 0) then Shift := [ssLeft];
  if (MK_RBUTTON and grfKeyState <> 0) then Shift := Shift + [ssRight];
  if (MK_MBUTTON and grfKeyState <> 0) then Shift := Shift + [ssMiddle];
  if (MK_CONTROL and grfKeyState <> 0) then Shift := Shift + [ssCtrl];
  if (MK_SHIFT and grfKeyState <> 0) then Shift := Shift + [ssShift];
  if ($20 and grfKeyState <> 0) then Shift := Shift + [ssAlt];
  DragEffect := dtNone;
  FOwner.TriggerTargetDragEvent(dsDragMove, FDragobj, Shift, pt.X, pt.Y, DragEffect);
  case DragEffect of
    dtCopy : dwEffect := DROPEFFECT_COPY;
    dtMove : dwEffect := DROPEFFECT_MOVE;
    dtLink : dwEffect := DROPEFFECT_LINK;
  else
    dwEffect := DROPEFFECT_NONE;
  end;
  Result := S_OK;
end;

function IElDropTarget.DragLeave : HResult;
var
  DragEffect : TDragType;
begin
  DragEffect := dtNone;
  FOwner.TriggerTargetDragEvent(dsDragLeave, FDragobj, [], -1, -1, DragEffect);
  if Assigned(FDragObj) then
  begin
    Fdragobj.Free;
    Fdragobj := nil;
  end;
  Result := S_OK;
  FOwner.FDataFormats.Clear;
end;

function IElDropTarget.Drop(const dataObj : IDataObject; grfKeyState : Longint; pt : TPoint; var dwEffect : Longint) : HResult;
var
  Shift : TShiftState;
  DragEffect : TDragType;

begin
  Shift := [];
  if (MK_LBUTTON and grfKeyState <> 0) then Shift := [ssLeft];
  if (MK_RBUTTON and grfKeyState <> 0) then Shift := Shift + [ssRight];
  if (MK_MBUTTON and grfKeyState <> 0) then Shift := Shift + [ssMiddle];
  if (MK_CONTROL and grfKeyState <> 0) then Shift := Shift + [ssCtrl];
  if (MK_SHIFT and grfKeyState <> 0) then Shift := Shift + [ssShift];
  if ($20 and grfKeyState <> 0) then Shift := Shift + [ssAlt];
  DragEffect := dtNone;
  FOwner.TriggerTargetDropEvent(Shift, FDragObj, pt.X, pt.Y, DragEffect);
  case DragEffect of
    dtCopy : dwEffect := DROPEFFECT_COPY;
    dtMove : dwEffect := DROPEFFECT_MOVE;
    dtLink : dwEffect := DROPEFFECT_LINK;
    dtNone : dwEffect := DROPEFFECT_NONE;
  end;
  if Assigned(FDragObj) then
  begin
    Fdragobj.Free;
    Fdragobj := nil;
  end;
  Result := S_OK;
  FOwner.FDataFormats.Clear;
end;

procedure TElDragDrop.SetContentType(newValue : Integer);
{ Sets data member FContentType to newValue. }
begin
  if (FContentType <> newValue) then
  begin
    FContentType := newValue;
  end; { if }
end; { SetContentType }

const
  crOleDragCursor = 1534;
  crOleDragUsualCursor = 1535;

constructor TElDragDrop.Create(AOwner : TComponent); { protected }
begin
  inherited;
  FDragTypes := [dtCopy, dtLink];
  FPicture := TPicture.Create;
  FPicture.OnChange := OnPictureChange;
  FContent := AllocMem(100);
  FAutoSize := false;
  Screen.Cursors[crOleDragCursor] := LoadCursor(HInstance, 'ELDRAGDROPCURSOR');
  Screen.Cursors[crOleDragUsualCursor] := LoadCursor(HInstance, 'ELDRAGDROPNORMALCURSOR');
  Cursor := crOleDragUsualCursor;
  Width := 100;
  Height := 50;
end; { Create }

destructor TElDragDrop.Destroy; { public }
begin
  { Free member variables: }
  FPicture.Free;
  if (FContent <> nil) then
    FreeMem(FContent);
  FElDropTarget := nil;
  if assigned(FDataFormats) then FDataFormats.Free;
  inherited;
end; { Destroy }

procedure TElDragDrop.TriggerOleStartDragEvent;
begin
  if (assigned(FOnOleStartDrag)) then FOnOleStartDrag(Self, DragData, DragDataType, DragDataSize);
end; { TriggerOleStartDragEvent }

procedure TElDragDrop.SetContentLength(newValue : Integer);
{ Sets data member FContentLength to newValue. }
begin
  if (FContentLength <> newValue) then
  begin
    FContentLength := newValue;
    ReallocMem(FContent, FContentLength);
  end; { if }
end; { SetContentLength }

procedure TElDragDrop.Paint; { protected }
begin
  if IsThemeApplied then
  begin
    DrawThemeBackground(Theme, Canvas.Handle, 0, 0, ClientRect, nil);
  end
  else
    inherited;
  if assigned (FOnPaint) then
    FOnPaint(Self)
  else
    Canvas.CopyRect(Rect(0, 0, FPicture.Width, FPicture.Height),
      FPicture.Bitmap.Canvas, Rect(0, 0, FPicture.Width, FPicture.Height));
end; { Paint }

procedure TElDragDrop.SetPicture(newValue : TPicture);
{ Sets data member FPicture to newValue. }
begin
  FPicture.Assign(newValue);
  if AutoSize then
  begin
    Width := FPicture.Width;
    Height := FPicture.Height;
  end;
end; { SetPicture }

procedure TElDragDrop.SetAutoSize(newValue : Boolean);
{ Sets data member FAutoSize to newValue. }
begin
  if (FAutoSize <> newValue) then
  begin
    FAutoSize := newValue;
    if FAutoSize then
    begin
      Width := FPicture.Width;
      Height := FPicture.Height;
    end;
  end; { if }
end; { SetAutoSize }

procedure TElDragDrop.SetIsDropTarget(newValue : Boolean);
var
  rslt : HResult;
begin
  if (FIsDropTarget <> newValue) then
  begin
    FIsDropTarget := newValue;
    if FIsDropTarget then
    begin
      if not Assigned(FElDropTarget) then
      begin
        FElDropTarget := IElDropTarget.Create(Self);
        FDataFormats := TStringList.Create;
      end;
      if HandleAllocated then
      begin
        Rslt := RegisterDragDrop(Handle, FElDropTarget);
        case Rslt of
          S_OK : ;
          DRAGDROP_E_INVALIDHWND : raise Exception.Create('RegisterDragDrop failed, invalide hwnd ');
          DRAGDROP_E_ALREADYREGISTERED : raise Exception.Create('RegisterDragDrop failed, already registered');
          E_OUTOFMEMORY : raise Exception.Create('RegisterDragDrop failed, out of memory');
          E_INVALIDARG : raise Exception.Create('RegisterDragDrop failed, invalid arg');
          CO_E_NOTINITIALIZED : raise Exception.Create('RegisterDragDrop failed, coInitialize had not been called');
        else
          raise Exception.Create('RegisterDragDrop failed, unknown error code ' + IntToStr(rslt and $7FFFFFFF));
        end;
      end;
    end
    else
    begin
      if HandleAllocated then
        RevokeDragDrop(Handle);
      FElDropTarget := nil;
      FDataFormats.Free;
      FDataFormats := nil;
    end;
  end; { if }
end; { SetIsDropTarget }

procedure TElDragDrop.OnPictureChange(Sender : TObject); { private }
begin
  if FAutoSize then
  begin
    Width := FPicture.Width;
    Height := FPicture.Height;
  end;
  Repaint;
end; { OnPictureChange }

procedure TElDragDrop.TriggerSourceDropEvent(DragResult : TDragResult);
begin
  if (assigned(FOnSourceDrop)) then
    FOnSourceDrop(Self, DragResult);
end; { TriggerSourceDropEvent }

procedure TElDragDrop.TriggerPaintEvent;
begin
  if (assigned(FOnPaint)) then
    FOnPaint(Self);
end; { TriggerPaintEvent }

procedure TElDragDrop.WMMouseMove(var Msg : TWMMouseMove); { private }
var
  res : HRESULT;
  res1 : TDragResult;
  okeffect : LongInt;
  effect : LongInt;

begin
  if (MK_LBUTTON and Msg.Keys <> 0) and IsDropSource then
  begin
    if FContent <> nil then
    begin
      FreeMem(FContent);
      FContent := nil;
      FContentLength := 0;
    end;
    TriggerOleStartDragEvent(FContent, FContentType, FContentLength);
    if FContent = nil then
      exit;
    FElDropSource := IElDropSource.Create(Self);
    okeffect := DROPEFFECT_NONE;
    if dtCopy in fDragTypes then okeffect := okeffect + DROPEFFECT_COPY;
    if dtMove in fDragTypes then okeffect := okeffect + DROPEFFECT_MOVE;
    if dtLink in fDragTypes then okeffect := okeffect + DROPEFFECT_LINK;
    effect := DROPEFFECT_MOVE;
    res := DoDragDrop(FElDropSource as IDataObject, FElDropSource, okeffect, effect);
    if Succeeded(res) then
    begin
      case res of
        DRAGDROP_S_DROP :
          begin
            if (okeffect and effect <> 0) then
            begin
              if (effect and dropeffect_copy <> 0) then
                res1 := drdropcopy
              else if (effect and dropeffect_move <> 0) then
                res1 := drdropmove
              else
                res1 := drdroplink;
            end
            else
              res1 := drcancel;
          end;
        DRAGDROP_S_CANCEL : res1 := drCancel;
        E_OUTOFMEMORY : res1 := drOutMemory;
      else
        res1 := drUnknown;
      end;
      FElDropSource := nil;
      TriggerSourceDropEvent(res1);
    end;
  end
  else
    inherited;
end; { WMMouseMove }

procedure TElDragDrop.TriggerTargetDragEvent;
begin
  if (assigned(FOnTargetDrag)) then
    FOnTargetDrag(Self, State, Source, Shift, X, Y, DragType);
end; { TriggerTargetDragEvent }

procedure TElDragDrop.TriggerTargetDropEvent;
begin
  if (assigned(FOnTargetDrop)) then
    FOnTargetDrop(Self, Source, Shift, X, Y, DragType);
end; { TriggerTargetDropEvent }

procedure TElDragDrop.WMLButtonDown(var Msg : TMessage);
begin
  if IsDropSource then
  begin
    FOldCursor := Screen.Cursor;
    FPressed := true;
    Screen.Cursor := crOleDragCursor;
  end;
  inherited;
end; { WMLButtonDown }

procedure TElDragDrop.WMLButtonUp(var Msg : TWMLButtonUp); { private }
begin
  if FPressed then Screen.Cursor := FOldCursor;
  FPressed := false;
  inherited;
end; { WMLButtonUp }

procedure TElDragDrop.CMMouseLeave(var Msg : TMessage); { private }
begin
  inherited;
  if FPressed then Screen.Cursor := FOldCursor;
  FPressed := false;
end; { CMMouseLeave }

procedure TElDragDrop.WMCreate(var Message: TMessage);
begin
  inherited;
  if FIsDropTarget then
  begin
    FIsDropTarget := false;
    IsDropTarget  := true;
  end;
end;

function TElDragDrop.GetThemedClassName: WideString;
begin
  Result := 'EDIT';
end;

constructor TEnumFormatEtc.Create(FormatList : pFormatList;
  FormatCount, Index : Integer);
begin
  inherited Create;
  fFormatList := FormatList;
  fFormatCount := FormatCount;
  fIndex := Index;
end;

function TEnumFormatEtc.Skip(Celt : LongInt) : HRESULT;
begin
  if Celt <= fFormatCount - fIndex then
  begin
    fIndex := fIndex + Celt;
    result := S_OK;
  end
  else
  begin
    fIndex := fFormatCount;
    result := S_FALSE;
  end;
end;

function TEnumFormatEtc.ReSet : HRESULT;
begin
  fIndex := 0;
  result := S_OK;
end;

function TEnumFormatEtc.Next(Celt : LongInt; out Elt; pCeltFetched : pLongInt) : HRESULT;
var
  i : Integer;
  List : TFormatList absolute Elt;

begin
  i := 0;
  while (i < Celt) and (fIndex < fFormatCount) do
  begin
    List[i] := fFormatList^[fIndex];
    Inc(fIndex);
    Inc(i);
  end;
  if pCeltFetched <> nil then pCeltFetched^ := i;
  if i = Celt then
    result := S_OK
  else
    result := S_FALSE;
end;

function TEnumFormatEtc.Clone(out Enum : IEnumFormatEtc) : HRESULT;
begin
  enum := TEnumFormatEtc.Create(fFormatList, fFormatCount, fIndex);
  result := S_OK;
end;

destructor TEnumFormatEtc.Destroy; { public }
begin
  Dispose(fFormatList);
  inherited;
end; { Destroy }

{$ENDIF}

end.
