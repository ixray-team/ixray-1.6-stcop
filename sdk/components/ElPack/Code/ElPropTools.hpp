// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElPropTools.pas' rev: 35.00 (Windows)

#ifndef ElproptoolsHPP
#define ElproptoolsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <ElIni.hpp>
#include <ElTools.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elproptools
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall LoadSetProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall LoadFloatProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE System::Typinfo::PPropInfo __fastcall GetPropertyRecord(System::TObject* Comp, System::UnicodeString PropertyName);
extern DELPHI_PACKAGE bool __fastcall HasProperty(System::TObject* Comp, System::UnicodeString PropertyName);
extern DELPHI_PACKAGE void __fastcall LoadObject(System::TObject* Comp, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall LoadCollection(System::Classes::TCollection* Collection, System::UnicodeString Name, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall LoadIntegerProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall LoadEnumProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall LoadStringProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall LoadStringList(System::Classes::TStrings* Strings, System::UnicodeString Name, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall StoreStringList(System::Classes::TStrings* Strings, System::UnicodeString Name, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall StoreObject(System::TObject* Comp, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall StoreCollection(System::Classes::TCollection* Collection, System::UnicodeString Name, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall StoreIntegerProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall StoreEnumProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall StoreStringProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall StoreSetProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
extern DELPHI_PACKAGE void __fastcall StoreFloatProperty(System::TObject* Comp, System::Typinfo::PPropInfo Prop, Elini::TElIniFile* Storage);
}	/* namespace Elproptools */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELPROPTOOLS)
using namespace Elproptools;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElproptoolsHPP
