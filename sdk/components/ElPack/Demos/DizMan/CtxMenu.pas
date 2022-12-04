unit CtxMenu;

interface

uses
  Windows, Messages, Registry, SysUtils, Classes, ComObj, ComServ, ShlObj, ActiveX, ShellApi,
  ElTools, DizIni, ElStrUtils, ElIni;

const CtxMenuGUID : TGuid = '{38CD8D36-B1CA-11D2-A86E-0060080F094D}';
      CtxMenuGUIDStr = '{38CD8D36-B1CA-11D2-A86E-0060080F094D}';

type
  TCtxMenu = class(TComObject, IContextMenu, IShellExtInit)
   private
     FFileList : string;
     FFileCount : integer;
   public
     Description : string;
     Ini : TDizIni;
     destructor Destroy; override;
     function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst,
              idCmdLast, uFlags: UINT): HResult; stdcall;
     function InvokeCommand(var lpici: TCMInvokeCommandInfo) : HResult; stdcall;
     function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
              pszName: LPSTR; cchMax: UINT): HResult; stdcall;
     function Initialize(pidlFolder: PItemIDList; lpdobj: IDataObject;
              hKeyProgID: HKEY): HResult; stdcall;
   end;

{$R dialogs.res}

function DllRegServer: HResult; stdcall;
function DllUnregServer: HResult; stdcall;

implementation

function EditDlgProc(hwndDlg : HWND; uMsg : integer; wParam, lParam : integer) : integer; stdcall;
var p : pointer;
    i : integer;
    t : TCtxMenu;
begin
  result :=0;
  case uMsg of
    WM_INITDIALOG:
      begin
        wParam := GetDlgItem(hwndDlg, 101);
        SetFocus(wParam);
        SetDlgItemText(hwndDlg, 101, pchar(TCtxMenu(lparam).Description));
        SetWindowLong(hwndDlg, DWL_USER, lparam);
        result := 1;
      end;
    WM_COMMAND:
      begin
        if loword(wParam) = 1 then
        begin
          i := GetWindowTextLength(GetDlgItem(hwndDlg, 101));
          GetMem(P, i + 1);
          GetDlgItemText(hwndDlg, 101, p, i + 1);
          T := TCtxMenu(GetWindowLong(hwndDlg, DWL_USER));
          if T is TCtxMenu
             then T.Description := StrPas(P);
          FreeMem(P);
          EndDialog(hwndDlg, 1);
          result := 1;
        end else
        if loword(wParam) = 2 then
        begin
          EndDialog(hwndDlg, 2);
          result := 1;
        end;
      end;
  end;
end;

function ViewDlgProc(hwndDlg : HWND; uMsg : integer; wParam, lParam : integer) : integer; stdcall;
begin
  result :=0;
  case uMsg of
    WM_INITDIALOG:
      begin
        SetDlgItemText(hwndDlg, 101, pchar(TCtxMenu(lparam).Description));
        result := 1;
      end;
    WM_COMMAND:
      begin
        if loword(wParam) = 1 then
        begin
          EndDialog(hwndDlg, 0);
          result := 1;
        end;
      end;
  end;
end;

function DllRegServer: HResult; stdcall;
var
  ClassID, FileName: string;
begin
  Result := SELFREG_E_CLASS;
  SetLength(FileName, MAX_PATH);
  GetModuleFileName(HInstance, PChar(FileName), Length(FileName));
  SetLength(FileName, StrLen(PChar(FileName)));

  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    ClassID := '\CLSID\' + CtxMenuGUIDStr;
    OpenKey(ClassID, True);
    WriteString('','DizManager Context Menu'); // Default value
    OpenKey(ClassID + '\InprocServer32', True);
    WriteString('', FileName); // Default value
    WriteString('ThreadingModel', 'Apartment');
    OpenKey('\*\shellex\ContextMenuHandlers\DizManagerMenu', True);
    WriteString('', CtxMenuGUIDStr);
    OpenKey('\Folder\shellex\ContextMenuHandlers\DizManagerMenu', True);
    WriteString('', CtxMenuGUIDStr);
  finally
    Free;
  end;
  Result := NOERROR;
end;

function DllUnregServer: HResult; stdcall;
begin
  try
    with TRegistry.Create do
    try
      RootKey := HKEY_CLASSES_ROOT;
      DeleteKey('\CLSID\' + CtxMenuGUIDStr);
      DeleteKey('\*\shellex\ContextMenuHandlers\DizManagerMenu');
      DeleteKey('\Folder\shellex\ContextMenuHandlers\DizManagerMenu');
    finally
      Free;
    end;
  except
    Result := S_FALSE;
  end;
  Result := NOERROR;
end;

const ID_VIEW = 1;
      ID_EDIT = 2;
      ID_RUN  = 3;

function TCtxMenu.QueryContextMenu(Menu: HMENU; indexMenu,
         idCmdFirst, idCmdLast, uFlags: UINT): HResult;
var HSubMenu : HMENU;
    j : integer;
begin
  HSubMenu := CreatePopupMenu;
  if FFileCount > 1 then j := MF_GRAYED else j := 0;
  InsertMenu (HSubMenu, 0, MF_STRING or MF_BYPOSITION or j, idCmdFirst + ID_VIEW, '&View ... ');
  InsertMenu (HSubMenu, 1, MF_STRING or MF_BYPOSITION, idCmdFirst + ID_EDIT, '&Enter/modify ... ');
  InsertMenu (Menu, indexMenu, MF_STRING or MF_BYPOSITION or MF_POPUP, HSubMenu, 'Description');
  Result := 3;
end;

function TCtxMenu.InvokeCommand(var lpici: TCMInvokeCommandInfo) : HResult;
var FileName : string;
    h : THandle;
    lpProc : pointer;
    FL,
    FL1 : TStringList;
    i : integer;
    FName : string;
    DoDel : boolean;

    function GetFileDescription (FileName : string): string;
    var d : string;
    begin
      d := FileName;
      while true do if not Replace(d, #13#10, #32) then break;
      while true do if not Replace(d, '\\', '\?\') then break;
      d := Trim(d);
      if Ini.OpenKey('\'+ExtractFilePath(d), false) then
      begin
        Ini.ReadString('', ExtractFileName(d), '', d);
        result :=d;
      end
      else
      begin
        result := '';
      end;
    end;

var VE : TElIniEntry;

begin
  // Make sure we are not being called by an application
  if HiWord(Integer(lpici.lpVerb)) <> 0 then
  begin
    Result := E_FAIL;
    Exit;
  end;
  Result := NOERROR;
  SetLength(FileName, MAX_PATH);
  GetModuleFileName(HInstance, PChar(FileName), Length(FileName));
  SetLength(FileName, StrLen(PChar(FileName)));
  FileName := ExtractFilePath(FileName);
  if Ini = nil then
  begin
    Ini := TDizIni.Create(nil);
    Ini.Path := FileName + 'DizData.eif';
    Ini.DivChar := '*';
    try
      Ini.Load;
    except
    end;
  end;
  case LoWord(lpici.lpVerb) of
    ID_VIEW:
      begin
        Description := GetFileDescription(FFileList);
        lpProc := MakeProcInstance(@ViewDlgProc, HInstance);
        DialogBoxParam(HInstance, 'VIEWDIALOG', 0, lpProc, integer(Self));
        FreeProcInstance(lpProc);
      end;
    ID_EDIT:
      begin
        if FFileCount = 1 then Description := GetFileDescription(FFileList);
        lpProc := MakeProcInstance(@EditDlgProc, HInstance);
        DialogBoxParam(HInstance, 'EDITDIALOG', 0, lpProc, integer(Self));
        FreeProcInstance(lpProc);
        FL := TStringList.Create;
        FL1 := TStringList.Create;
        FL.Text := Description;
        FL1.Text := FFileList;
        DoDel := Length(Trim(Description)) = 0;
        for i := 0 to FL1.Count -1 do
        begin
          FName := Trim(FL1[i]);
          while true do if not Replace(FName, '\\', '\?\') then break;
          if DoDel then
          begin
            VE := Ini.GetValueEntry('\'+ExtractFilePath(FName), ExtractFileName(FName));
            if VE <> nil then
            begin
              if VE.IsKey and (VE.SubCount >0) then VE.Invalidate
              else
                Ini.Delete('\'+ExtractFilePath(FName), ExtractFileName(FName));
            end;
          end else
          begin
            if (Length(FName) = 3) and (FName[2] = ':') and ((FName[3] = '\') or (FName[3] = '/')) or
               DirExists(FName) then
            begin
              if Ini.OpenKey('\'+FName, true) then Ini.WriteMultiString('', '', FL);
            end else
            if FileExists(FName) then
              Ini.WriteMultiString('\'+ExtractFilePath(FName), ExtractFileName(FName), FL);
          end;
        end;
        FL.Free;
        FL1.Free;
      end;
  end;
end;

function TCtxMenu.GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
         pszName: LPSTR; cchMax: UINT): HRESULT;
begin
  case uType of    //
    GCS_HELPTEXT:
      begin
        case idCmd of    //
          ID_VIEW: StrLCopy(pszName, 'View current description', cchMax);
          ID_EDIT: StrLCopy(pszName, 'Enter new or modify current description', cchMax);
        end;    // case
      end;
    GCS_VERB:
      case idCmd of    //
        ID_VIEW: StrLCopy(pszName, 'VIEWDIZ', cchMax);
        ID_EDIT: StrLCopy(pszName, 'EDITDIZ', cchMax);
      end;    // case
  end;    // case
  Result := NOERROR;
end;

function TCtxMenu.Initialize(pidlFolder: PItemIDList; lpdobj: IDataObject;
         hKeyProgID: HKEY): HResult;
var
  StgMedium: TStgMedium;
  FormatEtc: TFormatEtc;
  Fl : TStringList;
  i, k : integer;
  p : pchar;
begin
  if lpdobj = Nil then
  begin
    Result := E_FAIL;
    Exit;
  end;

  with FormatEtc do
  begin
    cfFormat := CF_HDROP;         // Get file names delimited by double nulls
    ptd      := Nil;              // Don't need to provide target device info
    dwAspect := DVASPECT_CONTENT; // Get the content
    lindex   := -1;               // Get all data
    tymed    := TYMED_HGLOBAL;    // Storage medium is a global memory handle
  end;
  Result := lpdobj.GetData(FormatEtc, StgMedium);
  if Succeeded(Result) then
     try
       FL := TStringList.Create;
       FFileCount := DragQueryFile(StgMedium.hGlobal, $FFFFFFFF, Nil, 0);
       for i := 0 to FFileCount - 1 do
       begin
         k := DragQueryFile(StgMedium.hGlobal, i, Nil, 0);
         GetMem(P, k + 1);
         DragQueryFile(StgMedium.hGlobal, i, p, k + 1);
         p[k] := #0;
         FL.Add(StrPas(p));
         FreeMem(p);
       end;
       FFileList := FL.Text;
       Fl.Free;
       result := NOERROR;
     finally
       ReleaseStgMedium(StgMedium);
     end;
end;

destructor TCtxMenu.Destroy;
begin
  if Assigned(Ini) {and (Ini.Modified) }then
  begin
    Ini.Save;
    Ini.Free;
  end;
  inherited;
end;

initialization
   TComObjectFactory.Create( ComServer, TCtxMenu, CtxMenuGUID, '', 'EldoS DizManager', ciMultiInstance);
end.


