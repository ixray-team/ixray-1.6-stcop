// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxFileUtil.pas' rev: 34.00 (Windows)

#ifndef MxfileutilHPP
#define MxfileutilHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Consts.hpp>
#include <Vcl.Controls.hpp>
#include <System.RTLConsts.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxfileutil
{
//-- forward type declarations -----------------------------------------------
struct TRxSearchRecEx;
class DELPHICLASS TRxFindFiles;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TRxSearchRecEx
{
public:
	int Time;
	int Size;
	int Attr;
	System::Sysutils::TFileName Name;
	int IncludeAttr;
	int ExcludeAttr;
	NativeUInt FindHandle;
	_WIN32_FIND_DATAW FindData;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TRxFindFiles : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod int __fastcall FindFirst(const System::UnicodeString Path, int Attr, System::Sysutils::TSearchRec &F);
	__classmethod int __fastcall FindNext(System::Sysutils::TSearchRec &F);
	__classmethod void __fastcall FindClose(System::Sysutils::TSearchRec &F);
	__classmethod int __fastcall FindMatchingFileProc(TRxSearchRecEx &F);
	__classmethod int __fastcall FindFirstEx(const System::UnicodeString Path, int IncludeAttr, int ExcludeAttr, TRxSearchRecEx &F);
	__classmethod int __fastcall FindNextEx(TRxSearchRecEx &F);
	__classmethod void __fastcall FindCloseEx(TRxSearchRecEx &F);
public:
	/* TObject.Create */ inline __fastcall TRxFindFiles() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TRxFindFiles() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 faNotFiles = System::Int8(0x10);
extern DELPHI_PACKAGE bool __fastcall BrowseDirectory(System::UnicodeString &AFolderName, const System::UnicodeString DlgText, System::Classes::THelpContext AHelpContext);
extern DELPHI_PACKAGE bool __fastcall BrowseComputer(System::UnicodeString &ComputerName, const System::UnicodeString DlgText, System::Classes::THelpContext AHelpContext);
extern DELPHI_PACKAGE System::UnicodeString __fastcall NormalDir(const System::UnicodeString DirName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RemoveBackSlash(const System::UnicodeString DirName);
extern DELPHI_PACKAGE bool __fastcall DirExists(System::UnicodeString Name);
extern DELPHI_PACKAGE void __fastcall ForceDirectories(System::UnicodeString Dir);
extern DELPHI_PACKAGE void __fastcall CopyFile(const System::UnicodeString FileName, const System::UnicodeString DestName, Vcl::Controls::TControl* ProgressControl);
extern DELPHI_PACKAGE void __fastcall CopyFileEx(const System::UnicodeString FileName, const System::UnicodeString DestName, bool OverwriteReadOnly, bool ShellDialog, Vcl::Controls::TControl* ProgressControl);
extern DELPHI_PACKAGE void __fastcall MoveFile(const System::Sysutils::TFileName FileName, const System::Sysutils::TFileName DestName);
extern DELPHI_PACKAGE void __fastcall MoveFileEx(const System::Sysutils::TFileName FileName, const System::Sysutils::TFileName DestName, bool ShellDialog);
extern DELPHI_PACKAGE __int64 __fastcall GetFileSize(const System::UnicodeString FileName);
extern DELPHI_PACKAGE System::TDateTime __fastcall FileDateTime(const System::UnicodeString FileName);
extern DELPHI_PACKAGE bool __fastcall HasAttr(const System::UnicodeString FileName, int Attr);
extern DELPHI_PACKAGE bool __fastcall DeleteFiles(const System::UnicodeString FileMask);
extern DELPHI_PACKAGE bool __fastcall DeleteFilesEx(const System::UnicodeString *FileMasks, const int FileMasks_High);
extern DELPHI_PACKAGE bool __fastcall ClearDir(const System::UnicodeString Path, bool Delete);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetTempDir(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetWindowsDir(void);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetSystemDir(void);
extern DELPHI_PACKAGE bool __fastcall ValidFileName(const System::UnicodeString FileName);
extern DELPHI_PACKAGE int __fastcall FileLock(int Handle, int Offset, int LockSize)/* overload */;
extern DELPHI_PACKAGE int __fastcall FileUnlock(int Handle, int Offset, int LockSize)/* overload */;
extern DELPHI_PACKAGE int __fastcall FileLock(int Handle, __int64 Offset, __int64 LockSize)/* overload */;
extern DELPHI_PACKAGE int __fastcall FileUnlock(int Handle, __int64 Offset, __int64 LockSize)/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall ShortToLongFileName(const System::UnicodeString ShortName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LongToShortFileName(const System::UnicodeString LongName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ShortToLongPath(const System::UnicodeString ShortName);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LongToShortPath(const System::UnicodeString LongName);
extern DELPHI_PACKAGE void __fastcall CreateFileLink(const System::UnicodeString FileName, const System::UnicodeString DisplayName, int Folder);
extern DELPHI_PACKAGE void __fastcall DeleteFileLink(const System::UnicodeString DisplayName, int Folder);
}	/* namespace Mxfileutil */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXFILEUTIL)
using namespace Mxfileutil;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxfileutilHPP
