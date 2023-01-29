// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElShellUtils.pas' rev: 34.00 (Windows)

#ifndef ElshellutilsHPP
#define ElshellutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ActiveX.hpp>
#include <Winapi.ShellAPI.hpp>
#include <System.Win.Registry.hpp>
#include <Vcl.Graphics.hpp>
#include <Winapi.ShlObj.hpp>
#include <Winapi.CommCtrl.hpp>
#include <Vcl.Controls.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Forms.hpp>
#include <ElList.hpp>
#include <ElTools.hpp>
#include <ElStrToken.hpp>
#include <ElStrUtils.hpp>

//-- user supplied -----------------------------------------------------------
typedef UNALIGNED _ITEMIDLIST * LPITEMIDLIST;
typedef const UNALIGNED _ITEMIDLIST * LPCITEMIDLIST;

namespace Elshellutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElShellIconCache;
struct tagExtraSearch;
struct SHCOLUMNID;
struct _SHELLDETAILS;
__interface DELPHIINTERFACE IEnumExtraSearch;
typedef System::DelphiInterface<IEnumExtraSearch> _di_IEnumExtraSearch;
__interface DELPHIINTERFACE IShellFolder2;
typedef System::DelphiInterface<IShellFolder2> _di_IShellFolder2;
__interface DELPHIINTERFACE IShellDetails;
typedef System::DelphiInterface<IShellDetails> _di_IShellDetails;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TShellFolders : unsigned char { sfoDesktopExpanded, sfoDesktop, sfoPrograms, sfoControlPanel, sfoPrinters, sfoPersonal, sfoFavorites, sfoStartup, sfoRecent, sfoSendto, sfoRecycleBin, sfoStartMenu, sfoDesktopDirectory, sfoMyComputer, sfoNetwork, sfoNetworkNeighborhood, sfoFonts, sfoTemplates, sfoCommonStartMenu, sfoCommonPrograms, sfoCommonStartup, sfoCommonDesktopDirectory, sfoAppData, sfoPrintHood, sfoCustom };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElShellIconCache : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Ellist::TElList* FNames;
	Vcl::Controls::TImageList* FSmallImages;
	Vcl::Controls::TImageList* FLargeImages;
	HICON DefSmallIcon;
	HICON DefLargeIcon;
	void __fastcall OnItemDelete(System::TObject* Sender, void * Item);
	
protected:
	int __fastcall LookForIcon(System::WideChar * Name, int Index);
	
public:
	__fastcall TElShellIconCache();
	__fastcall virtual ~TElShellIconCache();
	int __fastcall AddIcon(_di_IExtractIconW Icon, unsigned Flags);
	int __fastcall AddFromPIDL(Winapi::Shlobj::PItemIDList PIDL, unsigned Flags, bool OpenIcon);
	__property Vcl::Controls::TImageList* SmallImages = {read=FSmallImages};
	__property Vcl::Controls::TImageList* LargeImages = {read=FLargeImages};
};

#pragma pack(pop)

typedef tagExtraSearch *PExtraSearch;

struct DECLSPEC_DRECORD tagExtraSearch
{
public:
	GUID guidSearch;
	System::StaticArray<System::WideChar, 80> wszFriendlyName;
	System::StaticArray<System::WideChar, 80> wszMenuText;
	System::StaticArray<System::WideChar, 261> wszHelpText;
	System::StaticArray<System::WideChar, 2048> wszUrl;
	System::StaticArray<System::WideChar, 271> wszIcon;
	System::StaticArray<System::WideChar, 271> wszGreyIcon;
	System::StaticArray<System::WideChar, 271> wszClrIcon;
};


typedef tagExtraSearch TExtraSearch;

typedef SHCOLUMNID *PShColumnID;

struct DECLSPEC_DRECORD SHCOLUMNID
{
public:
	GUID fmtid;
	unsigned pid;
};


typedef SHCOLUMNID TShColumnID;

typedef _SHELLDETAILS *PShellDetails;

struct DECLSPEC_DRECORD _SHELLDETAILS
{
public:
	int fmt;
	int cxChar;
	_STRRET str;
};


typedef _SHELLDETAILS TShellDetails;

typedef _SHELLDETAILS SHELLDETAILS;

__interface  INTERFACE_UUID("{0E700BE1-9DB6-11D1-A1CE-00C04FD75D13}") IEnumExtraSearch  : public System::IInterface 
{
	virtual HRESULT __stdcall Next(unsigned celt, /* out */ PExtraSearch &rgelt, /* out */ unsigned &pceltFetched) = 0 ;
	virtual HRESULT __stdcall Skip(unsigned celt) = 0 ;
	virtual HRESULT __stdcall Reset() = 0 ;
	virtual HRESULT __stdcall Clone(/* out */ _di_IEnumExtraSearch &ppEnum) = 0 ;
};

__interface  INTERFACE_UUID("{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}") IShellFolder2  : public IShellFolder 
{
	virtual HRESULT __stdcall GetDefaultSearchGUID(/* out */ GUID &pguid) = 0 ;
	virtual HRESULT __stdcall EnumSearches(/* out */ _di_IEnumExtraSearch &ppEnum) = 0 ;
	virtual HRESULT __stdcall GetDefaultColumn(unsigned dwRes, unsigned &pSort, unsigned &pDisplay) = 0 ;
	virtual HRESULT __stdcall GetDefaultColumnState(unsigned iColumn, unsigned &pcsFlags) = 0 ;
	virtual HRESULT __stdcall GetDetailsEx(Winapi::Shlobj::PItemIDList pidl, const SHCOLUMNID &pscid, System::POleVariant pv) = 0 ;
	virtual HRESULT __stdcall GetDetailsOf(Winapi::Shlobj::PItemIDList pidl, unsigned iColumn, _SHELLDETAILS &psd) = 0 ;
	virtual HRESULT __stdcall MapNameToSCID(System::WideChar * pwszName, SHCOLUMNID &pscid) = 0 ;
};

__interface  INTERFACE_UUID("{000214EC-0000-0000-C000-000000000046}") IShellDetails  : public System::IInterface 
{
	virtual HRESULT __stdcall GetDetailsOf(Winapi::Shlobj::PItemIDList pidl, unsigned iColumn, _SHELLDETAILS &pDetails) = 0 ;
	virtual HRESULT __stdcall ColumnClick(unsigned iColumn) = 0 ;
};

//-- var, const, procedure ---------------------------------------------------
#define SID_IShellDetails L"{000214EC-0000-0000-C000-000000000046}"
#define SID_IShellFolder2 L"{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}"
#define SID_IEnumExtraSearch L"{0e700be1-9db6-11d1-A1CE-00C04FD75D13}"
extern DELPHI_PACKAGE GUID IID_IShellDetails;
extern DELPHI_PACKAGE GUID IID_IShellFolder2;
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall GetFolderPIDL(TShellFolders FolderID, System::UnicodeString CustomName);
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall GetFolderPIDL2(TShellFolders FolderID, System::UnicodeString CustomName);
extern DELPHI_PACKAGE void __fastcall FreeIDList(Winapi::Shlobj::PItemIDList PIDL);
extern DELPHI_PACKAGE bool __fastcall GetPathFromPIDL(Winapi::Shlobj::PItemIDList PIDL, System::UnicodeString &Path);
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall GETPIDLFromPath(System::UnicodeString Path);
extern DELPHI_PACKAGE bool __fastcall IsDesktopPIDL(Winapi::Shlobj::PItemIDList PIDL);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrRetToPas(const _STRRET &str, Winapi::Shlobj::PItemIDList PIDL);
extern DELPHI_PACKAGE void __fastcall StrRetFree(const _STRRET &str);
extern DELPHI_PACKAGE int __fastcall GetCompressedColor(void);
extern DELPHI_PACKAGE TElShellIconCache* __fastcall ShellIconCache(void);
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall GetNextItemID(Winapi::Shlobj::PItemIDList PIDL);
extern DELPHI_PACKAGE bool __fastcall PIDLStartsWith(Winapi::Shlobj::PItemIDList PIDL, Winapi::Shlobj::PItemIDList SubPIDL);
extern DELPHI_PACKAGE bool __fastcall PIDLContainsAt(Winapi::Shlobj::PItemIDList PIDL, Winapi::Shlobj::PItemIDList SubPIDL, int Pos);
extern DELPHI_PACKAGE int __fastcall CalcPIDLSize(Winapi::Shlobj::PItemIDList PIDL);
extern DELPHI_PACKAGE bool __fastcall CompareIDLists(Winapi::Shlobj::PItemIDList IDList1, Winapi::Shlobj::PItemIDList IDList2);
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall ClonePIDL(Winapi::Shlobj::PItemIDList PIDL);
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall GetOwnPIDL(Winapi::Shlobj::PItemIDList PIDL);
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall GetEmptyPIDL(void);
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall GetItemIDOnly(Winapi::Shlobj::PItemIDList PIDL);
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall AppendPIDL(Winapi::Shlobj::PItemIDList ParentPIDL, Winapi::Shlobj::PItemIDList ChildPIDL);
extern DELPHI_PACKAGE Winapi::Shlobj::PItemIDList __fastcall GetParentPIDL(Winapi::Shlobj::PItemIDList PIDL);
extern DELPHI_PACKAGE bool __fastcall FireURL(const System::UnicodeString URL);
}	/* namespace Elshellutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELSHELLUTILS)
using namespace Elshellutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElshellutilsHPP
