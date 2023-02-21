// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElOpts.pas' rev: 35.00 (Windows)

#ifndef EloptsHPP
#define EloptsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Win.Registry.hpp>
#include <System.IniFiles.hpp>
#include <ElIni.hpp>
#include <ElTools.hpp>
#include <System.TypInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elopts
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElOptions;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElStorageType : unsigned char { eosRegistry, eosIni, eosElIni };

class PASCALIMPLEMENTATION TElOptions : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	bool FAutoSave;
	System::UnicodeString FIniName;
	System::UnicodeString FIniSection;
	bool FLoading;
	Elini::TElIniFile* FStorage;
	TElStorageType FStorageType;
	virtual void __fastcall SetAutoSave(bool Value);
	
public:
	__fastcall virtual TElOptions(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElOptions();
	virtual void __fastcall Load();
	virtual void __fastcall Save();
	__property bool Loading = {read=FLoading, nodefault};
	
__published:
	__property bool AutoSave = {read=FAutoSave, write=SetAutoSave, nodefault};
	__property System::UnicodeString IniName = {read=FIniName, write=FIniName};
	__property System::UnicodeString IniSection = {read=FIniSection, write=FIniSection};
	__property Elini::TElIniFile* Storage = {read=FStorage, write=FStorage};
	__property TElStorageType StorageType = {read=FStorageType, write=FStorageType, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elopts */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELOPTS)
using namespace Elopts;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EloptsHPP
