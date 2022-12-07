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

unit ElShellUtils;

interface

uses Windows,
     ActiveX,
     ShellAPI,
     Registry,
     Graphics,
     ShlObj,
     
     CommCtrl,
     Controls,
     {$ifdef VCL_4_USED}
     ImgList,
     {$endif}
     Classes,
     SysUtils,
     Forms,
{$ifdef VCL_6_USED}
Types,
{$endif}

     ElList,
     ElTools,
     ElStrToken,
     ElStrUtils;

type

    TShellFolders =
       (sfoDesktopExpanded,sfoDesktop,sfoPrograms, sfoControlPanel,
        sfoPrinters,sfoPersonal,sfoFavorites,sfoStartup,sfoRecent,
        sfoSendto,sfoRecycleBin,sfoStartMenu,sfoDesktopDirectory,
        sfoMyComputer,
        sfoNetwork,sfoNetworkNeighborhood,sfoFonts,sfoTemplates,
        // sfoCommon is available starting from NT 4
        sfoCommonStartMenu, sfoCommonPrograms, sfoCommonStartup,
        sfoCommonDesktopDirectory, sfoAppData, sfoPrintHood, sfoCustom);

{$ifndef D_3}
{$ifndef D_2}
{$HPPEMIT 'typedef UNALIGNED _ITEMIDLIST * LPITEMIDLIST;'}
{$HPPEMIT 'typedef const UNALIGNED _ITEMIDLIST * LPCITEMIDLIST;'}
{$HPPEMIT 'typedef struct _STRRET'}
{$HPPEMIT '{'}
{$HPPEMIT '    UINT uType; // One of the STRRET_* values'}
{$HPPEMIT '    union'}
{$HPPEMIT '    {'}
{$HPPEMIT '        LPWSTR          pOleStr;        // must be freed by caller of GetDisplayNameOf'}
{$HPPEMIT '        LPSTR           pStr;           // NOT USED'}
{$HPPEMIT '        UINT            uOffset;        // Offset into SHITEMID'}
{$HPPEMIT '        char            cStr[MAX_PATH]; // Buffer to fill in (ANSI)'}
(*$HPPEMIT '    } DUMMYUNIONNAME;'*)
(*$HPPEMIT '} STRRET, *LPSTRRET;'*)

{$HPPEMIT 'typedef struct {}
{$HPPEMIT '    GUID fmtid;'}
{$HPPEMIT '    DWORD pid;'}
(*$HPPEMIT '} SHCOLUMNID, *LPSHCOLUMNID;'*)

{$HPPEMIT 'typedef const SHCOLUMNID* LPCSHCOLUMNID;'}

{$HPPEMIT 'typedef SHCOLUMNID *PShColumnID;'}

{$HPPEMIT 'typedef struct _SHELLDETAILS'}
(*$HPPEMIT '{'*)
{$HPPEMIT '    int     fmt;            // LVCFMT_* value (header only)'}
{$HPPEMIT '    int     cxChar;         // Number of "average" characters (header only)'}
{$HPPEMIT '    STRRET  str;            // String information'}
(*$HPPEMIT '} SHELLDETAILS, *LPSHELLDETAILS;'*)

{$HPPEMIT 'typedef _SHELLDETAILS *PShellDetails;'}

{$HPPEMIT 'typedef _SHELLDETAILS  TShellDetails;'}

{$HPPEMIT 'DECLARE_INTERFACE_(IShellFolder, IUnknown)'}
{$HPPEMIT '{'}
{$HPPEMIT '    // *** IUnknown methods ***'}
{$HPPEMIT '    STDMETHOD(QueryInterface) (THIS_ REFIID riid, void **ppv) PURE;'}
{$HPPEMIT '    STDMETHOD_(ULONG,AddRef) (THIS)  PURE;'}
{$HPPEMIT '    STDMETHOD_(ULONG,Release) (THIS) PURE;'}
{$HPPEMIT ''}
{$HPPEMIT '    // *** IShellFolder methods ***'}
{$HPPEMIT '    STDMETHOD(ParseDisplayName)(THIS_ HWND hwnd, LPBC pbc, LPOLESTR pszDisplayName,'}
{$HPPEMIT '                                ULONG *pchEaten, LPITEMIDLIST *ppidl, ULONG *pdwAttributes) PURE;'}
{$HPPEMIT ''}
{$HPPEMIT '    STDMETHOD(EnumObjects)(THIS_ HWND hwnd, DWORD grfFlags, IEnumIDList **ppenumIDList) PURE;'}
{$HPPEMIT ''}
{$HPPEMIT '    STDMETHOD(BindToObject)(THIS_ LPCITEMIDLIST pidl, LPBC pbc, REFIID riid, void **ppv) PURE;'}
{$HPPEMIT '    STDMETHOD(BindToStorage)(THIS_ LPCITEMIDLIST pidl, LPBC pbc, REFIID riid, void **ppv) PURE;'}
{$HPPEMIT '    STDMETHOD(CompareIDs)(THIS_ LPARAM lParam, LPCITEMIDLIST pidl1, LPCITEMIDLIST pidl2) PURE;'}
{$HPPEMIT '    STDMETHOD(CreateViewObject)(THIS_ HWND hwndOwner, REFIID riid, void **ppv) PURE;'}
{$HPPEMIT '    STDMETHOD(GetAttributesOf)(THIS_ UINT cidl, LPCITEMIDLIST * apidl, ULONG * rgfInOut) PURE;'}
{$HPPEMIT '    STDMETHOD(GetUIObjectOf)(THIS_ HWND hwndOwner, UINT cidl, LPCITEMIDLIST * apidl,'}
{$HPPEMIT '                             REFIID riid, UINT * prgfInOut, void **ppv) PURE;'}
{$HPPEMIT '    STDMETHOD(GetDisplayNameOf)(THIS_ LPCITEMIDLIST pidl, DWORD uFlags, LPSTRRET lpName) PURE;'}
{$HPPEMIT '    STDMETHOD(SetNameOf)(THIS_ HWND hwnd, LPCITEMIDLIST pidl, LPCOLESTR pszName,'}
{$HPPEMIT '                         DWORD uFlags, LPITEMIDLIST *ppidlOut) PURE;'}
(*$HPPEMIT '};'*)

{$HPPEMIT 'typedef IShellFolder * LPSHELLFOLDER;'}

{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumExtraSearch);'}
{$endif}
{$endif}

{$ifndef D_2}
function  GetParentPIDL(PIDL : PItemIDList): PItemIDList;
function  GetOwnPIDL(PIDL : PItemIDList): PItemIDList;
function  GetItemIDOnly(PIDL : PItemIDList): PItemIDList;
function GetNextItemID(PIDL : PItemIDList) : PItemIDList;
function  GetEmptyPIDL : PItemIDList;
function  PIDLContainsAt(PIDL, SubPIDL : PItemIDList; Pos : integer) : boolean;
function  PIDLStartsWith(PIDL, SubPIDL : PItemIDList) : boolean;

function  AppendPIDL(ParentPIDL, ChildPIDL : PItemIDList): PItemIDList;
function  ClonePIDL(PIDL : PItemIDList): PItemIDList;
function  GetFolderPIDL(FolderID : TShellFolders; CustomName : string): PItemIDList;
function GetFolderPIDL2(FolderID : TShellFolders; CustomName : string): PItemIDList;
function  CalcPIDLSize(PIDL : PItemIDList): Integer;
function  CompareIDLists(IDList1, IDList2 : PItemIDList) : boolean;
procedure FreeIDList(PIDL : PItemIDList);
function  GetPathFromPIDL(PIDL : PItemIDList; var Path : string): Boolean;
function  GETPIDLFromPath(Path : String): PItemIDList;
function  StrRetToPas(str : TStrRet; PIDL : PItemIDList) : string;
procedure StrRetFree(str  : TStrRet);
function  IsDesktopPIDL(PIDL : PItemIDList) : boolean;
{$endif}

function FireURL (const URL: string): Boolean;

{$ifndef D_2}
type TElShellIconCache = class
     private
       FNames: TElList;
       FSmallImages: TImageList;
       FLargeImages: TImageList;
       DefSmallIcon: HICON;
       DefLargeIcon: HICON;
       procedure OnItemDelete(Sender : TObject; Item : Pointer);
     protected
       function LookForIcon(Name : PChar; Index : integer): Integer;
     public
       constructor Create;
       destructor Destroy; override;
       function AddIcon(Icon : IExtractIcon; Flags : Cardinal): Integer;
       function AddFromPIDL(PIDL : PItemIDList; Flags : Cardinal; OpenIcon : boolean): Integer;
       property SmallImages: TImageList read FSmallImages;
       property LargeImages: TImageList read FLargeImages;
     end;

function ShellIconCache : TElShellIconCache;

function GetCompressedColor : Integer;

const
  SID_IShellDetails       = '{000214EC-0000-0000-C000-000000000046}';
  SID_IShellFolder2       = '{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}';
  SID_IEnumExtraSearch    = '{0e700be1-9db6-11d1-A1CE-00C04FD75D13}';

  IID_IShellDetails:TGUID = '{000214EC-0000-0000-C000-000000000046}';
  IID_IShellFolder2:TGUID = '{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}';

type

  {$ifdef VCL_5_USED}
  {$EXTERNALSYM PExtraSearch}
  {$endif}
  PExtraSearch = ^TExtraSearch;
  {$ifdef VCL_5_USED}
  {$EXTERNALSYM tagExtraSearch}
  {$endif}
  tagExtraSearch = record
    guidSearch: TGUID;
    wszFriendlyName,
    wszMenuText: array[0..79] of WideChar;
    wszHelpText: array[0..MAX_PATH] of WideChar;
    wszUrl: array[0..2047] of WideChar;
    wszIcon,
    wszGreyIcon,
    wszClrIcon: array[0..MAX_PATH+10] of WideChar;
  end;
  {$ifdef VCL_5_USED}
  {$EXTERNALSYM TExtraSearch}
  {$endif}
  TExtraSearch = tagExtraSearch;

  PShColumnID = ^TShColumnID;
  {$ifdef VCL_5_USED}
  {$EXTERNALSYM SHCOLUMNID}
  {$endif}
  SHCOLUMNID = record
    fmtid: TGUID;
    pid: DWORD;
  end;
  {$ifdef VCL_5_USED}
  {$EXTERNALSYM TShColumnID}
  {$endif}
  TShColumnID = SHCOLUMNID;

  PShellDetails = ^TShellDetails;
  {$ifdef VCL_5_USED}
  {$EXTERNALSYM _SHELLDETAILS}
  {$endif}
  _SHELLDETAILS = record
    fmt,
    cxChar: Integer;
    str: TSTRRET;
  end;
  TShellDetails = _SHELLDETAILS;
  {$ifdef VCL_5_USED}
  {$EXTERNALSYM SHELLDETAILS}
  {$endif}
  SHELLDETAILS = _SHELLDETAILS;

  {$ifdef VCL_5_USED}
  {$EXTERNALSYM IEnumExtraSearch}
  {$endif}
  IEnumExtraSearch = interface(IUnknown)
    [SID_IEnumExtraSearch]
    function Next(celt: ULONG; out rgelt: PExtraSearch;
      out pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppEnum: IEnumExtraSearch): HResult; stdcall;
  end;

  {$ifdef D_3}
  POleVariant = ^OleVariant;
  {$endif}

  IShellFolder2 = interface(IShellFolder)
    [SID_IShellFolder2]
    function GetDefaultSearchGUID(out pguid: TGUID): HResult; stdcall;
    function EnumSearches(out ppEnum: IEnumExtraSearch): HResult; stdcall;
    function GetDefaultColumn(dwRes: DWORD; var pSort: ULONG;
      var pDisplay: ULONG): HResult; stdcall;
    function GetDefaultColumnState(iColumn: UINT; var pcsFlags: DWORD): HResult; stdcall;
    function GetDetailsEx(pidl: PItemIDList; const pscid: SHCOLUMNID;
      pv: POleVariant): HResult; stdcall;
    function GetDetailsOf(pidl: PItemIDList; iColumn: UINT;
      var psd: TShellDetails): HResult; stdcall;
    function MapNameToSCID(pwszName: LPCWSTR; var pscid: TShColumnID): HResult; stdcall;
  end;

  IShellDetails = interface(IUnknown)
    [SID_IShellDetails]
    function GetDetailsOf(pidl: PItemIDList; iColumn: UINT;
      var pDetails: TShellDetails): HResult; stdcall;
    function ColumnClick(iColumn: UINT): HResult; stdcall;
  end;
{$endif}

implementation

{$ifndef D_2}
var vShellIconCache : TElShellIconCache;

const
  ShellFolderIDs: array[TShellFolders] of Integer=
   ($FEFE {cfoDesktopExpanded}, CSIDL_DESKTOP,
    CSIDL_PROGRAMS, CSIDL_CONTROLS, CSIDL_PRINTERS, CSIDL_PERSONAL, CSIDL_FAVORITES,
    CSIDL_STARTUP, CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET, CSIDL_STARTMENU,
    CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_NETHOOD, CSIDL_FONTS,
    CSIDL_TEMPLATES,
    { CSIDL_COMMON_* is for NT 4.0+ }
    CSIDL_COMMON_STARTMENU, CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTUP,
    CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_APPDATA, CSIDL_PRINTHOOD, -1
    );

function GetFolderPIDL(FolderID : TShellFolders; CustomName : string): PItemIDList;
begin
  if FolderID = sfoDesktopExpanded then
    FolderID := sfoDesktop;
  result := GetFolderPIDL2(FolderID, CustomName);
end;

function GetFolderPIDL2(FolderID : TShellFolders; CustomName : string): PItemIDList;
begin
  result := nil;
  if FolderID = sfoCustom then
  begin
    Result := GetPIDLFromPath(CustomName);
  end
  else
  if not Succeeded(SHGetSpecialFolderLocation(0, ShellFolderIDs[FolderID], result)) then
  begin
    if Assigned(result) then
    begin
      FreeIDList(result);
      result := nil;
    end;
  end;
end;

procedure FreeIDList(PIDL : PItemIDList);
var
  Malloc: IMalloc;
begin
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    if Malloc.DidAlloc(PIDL) = 1 then
       Malloc.Free(PIDL);
    Malloc := nil;
  end;
end;

function GetPathFromPIDL(PIDL : PItemIDList; var Path :
    string): Boolean;
var
  APath: array [0..MAX_PATH] of char;
begin
  Path := '';
  if SHGetPathFromIDList(PIDL, @APath) then
  begin
    Path := StrPas(PAnsiChar(AnsiString(APath)));
    result := true;
  end else
    result := false;
end;

function GETPIDLFromPath(Path : String): PItemIDList;
var
  WSP: PWideChar;
  DesktopFolder: IShellFolder;
  Eaten, Attribs: Cardinal;
begin
  SHGetDesktopFolder(DesktopFolder);
  GetMem(WSP, length(Path) * 2 + 2);
  StringToWideChar(Path, WSP, length(Path) * 2 + 2);
  result := nil;
  if not Succeeded(DesktopFolder.ParseDisplayName(0, nil, WSP, Eaten, result, Attribs)) then
  begin
    if Assigned(result) then
      FreeIDList(result);
    result := nil;
  end;
  FreeMem(WSP);
  DesktopFolder := nil;
end;

function IsDesktopPIDL(PIDL : PItemIDList) : boolean;
begin
  result := PIDL.mkid.cb = 0;
end;

function StrRetToPas(str : TStrRet; PIDL : PItemIDList) : string;
begin
  if (Str.uType = STRRET_WSTR) then
  begin
    if (Str.pOLEStr <> nil) then
    begin
      result := WideCharToString(Str.pOLEStr);
      // result := WideStrPas(Str.pOLEStr)
    end
    else
      result := '';
  end
  else
  if (Str.uType = STRRET_CSTR) then
  begin
    Result := StrPas(PAnsiChar(AnsiString(Str.cStr)));
  end
  else
  if (Str.uType = STRRET_OFFSET) then
  begin
    Result := StrPas(PChar(PIDL) + Str.uOffset);
  end
  else
    result := '';
end;

procedure StrRetFree(str : TStrRet);
begin
  if (Str.uType = STRRET_WSTR) and (Str.pOLEStr <> nil) then
    CoTaskMemFree(Str.pOleStr);
end;

type TElShellIconCacheEntry = record
       Name  : PChar;
       Idx   : integer;
     end;
     PElShellIconCacheEntry = ^TElShellIconCacheEntry;

constructor TElShellIconCache.Create;
var Fl : Cardinal;
    FI : TSHFileInfo;
    Flags : integer;
const
  Mask: array[Boolean] of Longint = (0, ILC_MASK);

begin
  inherited;
  FSmallImages := TImageList.Create(nil);
  FLargeImages := TImageList.Create(nil);
  FSmallImages.Width := GetSystemMetrics(SM_CXSMICON);
  FSmallImages.Height := GetSystemMetrics(SM_CYSMICON);
  FLargeImages.Width := GetSystemMetrics(SM_CXICON);
  FLargeImages.Height := GetSystemMetrics(SM_CYICON);
  Flags := Mask[FSmallImages.Masked];
  if IsWinXPUp then
    Flags := Flags or ILC_COLOR32
  else
    Flags := Flags or ILC_COLORDDB;

  FSmallImages.Handle := ImageList_Create(FSmallImages.Width, FSmallImages.Height, Flags, 16, 16);
  Flags := Mask[FLargeImages.Masked];
  if IsWinXPUp then
    Flags := Flags or ILC_COLOR32
  else
    Flags := Flags or ILC_COLORDDB;

  FLargeImages.Handle := ImageList_Create(FLargeImages.Width, FSmallImages.Height, Flags, 16, 16);

  FNames := TElList.Create;
  FNames.OnDelete := OnItemDelete;

  Fl := SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES or SHGFI_ICON;
  SHGetFileInfo('', 0, FI, SizeOf(FI), Fl);
  DefSmallIcon := FI.hIcon;

  Fl := SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES or SHGFI_ICON;
  SHGetFileInfo('', 0, FI, SizeOf(FI), Fl);
  DefLargeIcon := FI.hIcon;

end;

destructor TElShellIconCache.Destroy;
begin
  DestroyIcon(DefSmallIcon);
  DestroyIcon(DefLargeIcon);
  FSmallImages.Free;
  FLargeImages.Free;
  FNames.Free;
  inherited;
end;

procedure TElShellIconCache.OnItemDelete(Sender : TObject; Item : Pointer);
begin
  if PElShellIconCacheEntry(Item).Name <> nil then
    FreeMem(PElShellIconCacheEntry(Item).Name);
  FreeMem(Item);
end;

function TElShellIconCache.AddIcon(Icon : IExtractIcon; Flags : Cardinal):
    Integer;
var S : String;
    i : integer;
    pFlags : Cardinal;
    Entry : PElShellIconCacheEntry;
    largeIcon,
    smallIcon : HICON;
begin
  SetLength(S, MAX_PATH + 1);
  i := 0;
  if not Succeeded(Icon.GetIconLocation(Flags, PChar(S), MAX_PATH, i, pFlags)) then
  begin
    result := -1;
    exit;
  end
  else
  begin
    if (pFlags and GIL_NOTFILENAME) = GIL_NOTFILENAME then
      i := -1;
    SetLength(S, StrLen(PChar(S)));
    result := LookForIcon(PChar(S), i);
    if result <> -1 then exit;
  end;
  LargeIcon := 0;
  SmallIcon := 0;
  if (Icon.Extract(PChar(S), i, largeIcon, smallIcon, FSmallImages.Width shl 16 or FLargeImages.Width) = S_FALSE) then
  begin
    result := -1;
    exit;
  end;
  if (largeIcon = 0) then
  begin
    SmallIcon := CopyIcon(DefSmallIcon);
  end;
  if (largeIcon = 0) then
  begin
    LargeIcon := CopyIcon(DefLargeIcon);
  end;
  GetMem(Entry, sizeof(TElShellIconCacheEntry));

  if Length(S) > 0 then
  begin
    GetMem(Entry.Name, Length(S) + 1);
    StrPCopy(Entry.Name, S);
  end
  else
    Entry.Name := nil;

  Entry.Idx := i;
  Result := FNames.Add(Entry);

  if smallIcon <> 0 then
    i := ImageList_AddIcon(FSmallImages.Handle, smallIcon)
  else
    i := ImageList_AddIcon(FSmallImages.Handle, defSmallIcon);
  if largeIcon <> 0 then
    ImageList_AddIcon(FLargeImages.Handle, largeIcon)
  else
    i := ImageList_AddIcon(FLargeImages.Handle, DefLargeIcon);

  DestroyIcon(smallIcon);
  DestroyIcon(largeIcon);
end;

function TElShellIconCache.LookForIcon(Name : PChar; Index : integer): Integer;
var i     : integer;
    Entry : PElShellIconCacheEntry;

begin
  for i := 0 to FNames.Count - 1 do
  begin
    Entry := PElShellIconCacheEntry(FNames[i]);
    if (Entry.Idx = Index) and
       (Entry.Name <> nil) and (StrComp(Name, Entry.Name) = 0) then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

function TElShellIconCache.AddFromPIDL(PIDL : PItemIDList; Flags : Cardinal; OpenIcon : boolean): Integer;
var Fl : Cardinal;
    FI : TSHFileInfo;
    Entry : PElShellIconCacheEntry;
    largeIcon,
    smallIcon : HICON;
    S : String;
    i : integer;
begin
  Fl := SHGFI_SMALLICON;
  if OpenIcon then
    Fl := Fl or SHGFI_OPENICON;

  // get icon location
  SHGetFileInfo(pchar(PIDL),0, FI, SizeOf(FI), SHGFI_PIDL or SHGFI_ICONLOCATION or Fl);
  S := StrPas(FI.szDisplayName);
  i := FI.iIcon;
  SetLength(S, StrLen(PChar(S)));

  if OpenIcon then
    Fl := SHGFI_SMALLICON or SHGFI_OPENICON
  else
    Fl := SHGFI_SMALLICON;
    
  // get icons
  SHGetFileInfo(pchar(PIDL),0, FI, SizeOf(FI), SHGFI_PIDL or SHGFI_ICON or Fl);
  smallIcon := FI.hIcon;

  Fl := SHGFI_LARGEICON;
  if OpenIcon then
    Fl := Fl or SHGFI_OPENICON;
  SHGetFileInfo(pchar(PIDL),0, FI, SizeOf(FI), SHGFI_PIDL or SHGFI_ICON or Fl);
  largeIcon := FI.hIcon;

  // allocate entry
  GetMem(Entry, sizeof(TElShellIconCacheEntry));
  if Length(S) > 0 then
  begin
    GetMem(Entry.Name, Length(S) + 1);
    StrPCopy(Entry.Name, S);
  end
  else
    Entry.Name := nil;

  Entry.Idx := i;
  Result := FNames.Add(Entry);

  ImageList_AddIcon(FSmallImages.Handle, smallIcon);
  ImageList_AddIcon(FLargeImages.Handle, largeIcon);

  DestroyIcon(smallIcon);
  DestroyIcon(largeIcon);
end;

function GetCompressedColor : Integer;
var Reg : TRegistry;
begin
  result := RGB(0, 0, 255);
  if IsWinNTUp then
  begin
    try
      reg := TRegistry.Create;
      try
        reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced', false);
        if Reg.ReadInteger('ShowCompColor') = 1 then
        begin
          reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Explorer', false);
          if Reg.ReadBinaryData('AltColor', result, sizeof(result)) <> sizeof(result) then
            result := RGB(0, 0, 255);
        end;
      finally
        reg.free;
      end;
    except
      result := RGB(0, 0, 255);
    end;
  end
  else
    result := RGB(0, 0, 255);
end;


function ShellIconCache : TElShellIconCache;
begin
  result := vShellIconCache;
end;

function GetNextItemID(PIDL : PItemIDList) : PItemIDList;
var cb : Cardinal;
begin
   if(pidl = nil) then
   begin
     result := nil;
     exit;
   end;
   // Get the size of the specified item identifier.
   cb := pidl.mkid.cb;

   // If the size is zero, it is the end of the list.
   if (cb = 0) then
   begin
     result := nil;
     exit;
   end;

   // Add cb to pidl (casting to increment by bytes).
   pidl := PItemIDList(Cardinal(pidl) + cb);
   if pidl.mkid.cb = 0 then
      result := nil
   else
      result := PIDL;
end;

function PIDLStartsWith(PIDL, SubPIDL : PItemIDList) : boolean;
var Size,
    SubSize: integer;
begin
  result := false;
  if (PIDL <> nil) and (SubPIDL <> nil) then
  begin
    Size := CalcPIDLSize(PIDL);
    SubSize := CalcPIDLSize(SubPIDL);
    if SubSize < Size then
      result := CompareMem(pidl, subPIDL, subSize - sizeof(SubPIDL.mkid.cb));
  end;
end;

function PIDLContainsAt(PIDL, SubPIDL : PItemIDList; Pos : integer) : boolean;
var Total  : integer;
begin
  result := false;
  Total := 0;
  if (PIDL <> nil) and (SubPIDL <> nil) then
  begin
    while (PIDL <> nil) do
    begin
      Inc(Total);
      if Pos = Total then
      begin
        if pidl.mkid.cb = SubPIDL.mkid.cb then
          result := CompareMem(@pidl.mkid.abID, @subPIDL.mkid.abID, SubPIDL.mkid.cb - sizeof(SubPIDL.mkid.cb));
        break;
      end;
      PIDL := GetNextItemID(pidl);
    end;
  end;
end;

function CalcPIDLSize(PIDL : PItemIDList): Integer;
var CBTotal : integer;
begin
  CBTotal := 0;
  if PIDL <> nil then
  begin
    Inc(cbTotal, sizeof(pidl.mkid.cb));
    while (pidl <> nil) do
    begin
      Inc(cbTotal, pidl.mkid.cb);
      pidl := GetNextItemID(pidl);
    end;
  end;
  result := cbTotal;
end;

function CompareIDLists(IDList1, IDList2 : PItemIDList) : boolean;
var i : integer;
begin
  i := CalcPIDLSize(IDList1);
  if i <> CalcPIDLSize(IDList2) then
    result := false
  else
    result := CompareMem(IDList1, IDList2, i);
end;

function ClonePIDL(PIDL : PItemIDList): PItemIDList;
var
  PIDLTemp: PItemIDList;
  Malloc: IMalloc;
  cb: Cardinal;

begin
  if PIDL = nil then
  begin
    result := nil;
    exit;
  end;
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    cb := CalcPIDLSize(PIDL);
    PIDLTemp := Malloc.alloc(cb);
    MoveMemory(PIDLTemp, PIDL, cb);
    result := PIDLTemp;
  end
  else
    result := nil;
end;

function GetOwnPIDL(PIDL : PItemIDList): PItemIDList;
var OldPIDL : PItemIDList;
begin
  OldPIDL := nil;
  while PIDL.mkid.cb > 0 do
  begin
    OldPIDL := PIDL;
    PIDL := PItemIDList(Integer(PIDL) + PIDL.mkid.cb);
  end;
  result := OldPIDL;
end;

function GetEmptyPIDL : PItemIDList;
var Malloc: IMalloc;
begin
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    result := Malloc.alloc(2);
    result.mkid.cb := 0;
  end
  else
    result := nil;
end;

function  GetItemIDOnly(PIDL : PItemIDList): PItemIDList;
var Malloc : IMalloc;
    pc     : PChar;
begin
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    result := Malloc.alloc(sizeof(PIDL.mkid.cb) + PIDL.mkid.cb);
    MoveMemory(result, PIDL, PIDL.mkid.cb);
    pc := PChar(result);
    inc(pc, PIDL.mkid.cb);
    PWord(pc)^ := 0;
  end
  else
    result := nil;
end;

function AppendPIDL(ParentPIDL, ChildPIDL : PItemIDList): PItemIDList;
var Malloc: IMalloc;
    pcb,
    ccb   : Cardinal;
begin
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    pcb := CalcPIDLSize(ParentPIDL);
    ccb := CalcPIDLSize(ChildPIDL);
    result := Malloc.alloc(pcb + ccb - sizeof(ParentPIDL.mkid.cb));
    if pcb - sizeof(ParentPIDL.mkid.cb) > 0 then
      MoveMemory(Result, ParentPIDL, pcb - sizeof(ParentPIDL.mkid.cb));
    if ccb > 0 then
      MoveMemory(PItemIDList(Cardinal(Result) + pcb  - sizeof(ParentPIDL.mkid.cb)), ChildPIDL, ccb);
  end
  else
    result := nil;
end;

function GetParentPIDL(PIDL : PItemIDList): PItemIDList;
var
  PIDLLast, PIDLTemp: PItemIDList;
  Malloc: IMalloc;
  cb: Cardinal;
begin
  if Succeeded(SHGetMalloc(Malloc)) then
  begin
    PIDLLast := GetOwnPIDL(PIDL);
    if PIDLLast = nil then
    begin
      result := Malloc.alloc(2);
      result.mkid.cb := 0;
    end
    else
    begin
      cb := Integer(PIDLLast) - Integer(PIDL) + sizeof(PIDLTemp.mkid.cb);
      PIDLTemp := Malloc.alloc(cb);
      MoveMemory(PIDLTemp, PIDL, cb);
      PItemIDList(Cardinal(PIDLTemp) + cb - sizeof(PIDLTemp.mkid.cb)).mkid.cb := 0;
      result := PIDLTemp;
    end;
  end
  else
    result := nil;
end;

{$endif}

function FireURL (const URL: string): Boolean;
var
  SHI : TShellExecuteInfo;
  URL2: String;
begin
  URL2 := URL;
  if Pos(':', URL2) = 0 then
  begin
    if ContainsAt(URL2, 1, 'www') then
    begin
      URL2 := 'http://' + URL2;
    end
    else
    begin
      if ContainsAt(URL2, 1, 'ftp') then
      begin
        URL2 := 'ftp://' + URL2;
      end
      else
      begin
        if Pos('@', URL2) > 1 then
        begin
          URL2 := 'mailto:' + URL2;
        end;
      end;
    end;
  end;
  ZeroMemory(@SHI, sizeof(SHI));
  SHI.cbSize := sizeof(SHI);
  SHI.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_NOCLOSEPROCESS;
  SHI.Wnd := Application.Handle;
  SHI.lpVerb := PChar('Open');
  SHI.lpFile := PChar(URL2);
  SHI.lpParameters := nil;
  SHI.lpDirectory := nil;
  Result := ShellExecuteEx(@SHI);
  if Result then
  begin
    CloseHandle(SHI.hProcess);
  end;
end;

{$ifndef D_2}

initialization

  vShellIconCache := TElShellIconCache.Create;

finalization

  vShellIconCache.Free;
{$endif}

end.
