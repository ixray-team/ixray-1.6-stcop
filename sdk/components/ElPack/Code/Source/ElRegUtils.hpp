// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElRegUtils.pas' rev: 34.00 (Windows)

#ifndef ElregutilsHPP
#define ElregutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elregutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TRegRootType : unsigned char { rrtUnknown, rrtHKEY_CLASSES_ROOT, rrtHKEY_CURRENT_USER, rrtHKEY_LOCAL_MACHINE, rrtHKEY_USERS, rrtHKEY_CURRENT_CONFIG };

typedef System::Set<TRegRootType, TRegRootType::rrtUnknown, TRegRootType::rrtHKEY_CURRENT_CONFIG> TRegRoots;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetLastRegError(void);
extern DELPHI_PACKAGE bool __fastcall IsValidKeyName(System::UnicodeString Name);
extern DELPHI_PACKAGE bool __fastcall KeyClear(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName);
extern DELPHI_PACKAGE bool __fastcall KeyHasValue(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, const System::UnicodeString ValueName, bool &Exists);
extern DELPHI_PACKAGE bool __fastcall KeyRenameValue(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, const System::UnicodeString ValueName, const System::UnicodeString NewName);
extern DELPHI_PACKAGE bool __fastcall KeyDeleteValue(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, const System::UnicodeString ValueName);
extern DELPHI_PACKAGE bool __fastcall KeySetValue(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, const System::UnicodeString ValueName, int ValueType, void * Value, int ValueSize);
extern DELPHI_PACKAGE bool __fastcall KeyCreateSubKey(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, const System::UnicodeString SubKeyName, const System::UnicodeString NewClassName);
extern DELPHI_PACKAGE bool __fastcall KeyDelete(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName);
extern DELPHI_PACKAGE bool __fastcall CopyKey(const System::UnicodeString OldComputerName, const System::UnicodeString NewComputerName, TRegRootType OldRT, TRegRootType NewRT, const System::UnicodeString OldKeyName, const System::UnicodeString NewKeyName);
extern DELPHI_PACKAGE bool __fastcall KeyGetClassName(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, System::UnicodeString &ClassName);
extern DELPHI_PACKAGE bool __fastcall KeyEnumValues(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, System::Classes::TStringList* SL);
extern DELPHI_PACKAGE bool __fastcall KeyGetValueInfo(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, const System::UnicodeString ValueName, int &ValueType, System::UnicodeString &ValueString, int &ValueSize);
extern DELPHI_PACKAGE bool __fastcall KeyEnumSubKeys(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, System::Classes::TStringList* SL);
extern DELPHI_PACKAGE bool __fastcall KeyHasSubKeys(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName);
extern DELPHI_PACKAGE TRegRootType __fastcall NameToRootType(const System::UnicodeString Name);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RootTypeName(TRegRootType RT);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RootTypeShortName(TRegRootType RT);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ValueTypeToString(int VT);
extern DELPHI_PACKAGE HKEY __fastcall RootTypeToHandle(TRegRootType RT);
extern DELPHI_PACKAGE bool __fastcall KeyHasSubKeys0(HKEY Key, const System::UnicodeString KeyName);
extern DELPHI_PACKAGE bool __fastcall KeyEnumSubKeys0(HKEY Key, const System::UnicodeString KeyName, System::Classes::TStringList* SL);
extern DELPHI_PACKAGE bool __fastcall OpenRegKey(const System::UnicodeString ComputerName, TRegRootType RT, const System::UnicodeString KeyName, HKEY &KeyRes);
}	/* namespace Elregutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELREGUTILS)
using namespace Elregutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElregutilsHPP
