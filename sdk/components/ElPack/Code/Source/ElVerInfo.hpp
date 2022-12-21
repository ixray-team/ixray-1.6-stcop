// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElVerInfo.pas' rev: 34.00 (Windows)

#ifndef ElverinfoHPP
#define ElverinfoHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Winapi.ShellAPI.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elverinfo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElVersionInfo;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TVersionAttribute : unsigned char { vaDebug, vaPatched, vaPreRelease, vaPrivateBuild, vaSpecialBuild };

typedef System::Set<TVersionAttribute, TVersionAttribute::vaDebug, TVersionAttribute::vaSpecialBuild> TVersionAttributes;

class PASCALIMPLEMENTATION TElVersionInfo : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	System::UnicodeString operator[](System::UnicodeString Name) { return this->Values[Name]; }
	
private:
	System::UnicodeString FBuffer;
	System::UnicodeString FFileName;
	tagVS_FIXEDFILEINFO *FFixedFileInfo;
	System::UnicodeString FLanguage;
	TVersionAttributes __fastcall GetAttributes();
	int __fastcall GetBuild();
	System::UnicodeString __fastcall GetLanguage();
	int __fastcall GetMajorVersion();
	int __fastcall GetMinorVersion();
	System::UnicodeString __fastcall GetPredefined(int Index);
	int __fastcall GetRelease();
	System::UnicodeString __fastcall GetValue(System::UnicodeString AName);
	void __fastcall SetAttributes(const TVersionAttributes Value);
	void __fastcall SetDummy(const System::UnicodeString Value);
	void __fastcall SetDummyEx(int Index, System::UnicodeString Value);
	void __fastcall SetFileName(const System::UnicodeString Value);
	void __fastcall SetDummyInt(const int Value);
	bool __fastcall StoreFileName();
	
public:
	__fastcall virtual TElVersionInfo(System::Classes::TComponent* AOwner);
	void __fastcall Refresh();
	__property System::UnicodeString Values[System::UnicodeString Name] = {read=GetValue/*, default*/};
	
__published:
	__property TVersionAttributes Attributes = {read=GetAttributes, write=SetAttributes, stored=false, nodefault};
	__property int Build = {read=GetBuild, write=SetDummyInt, stored=false, nodefault};
	__property System::UnicodeString Comments = {read=GetPredefined, write=SetDummyEx, stored=false, index=9};
	__property System::UnicodeString CompanyName = {read=GetPredefined, write=SetDummyEx, stored=false, index=0};
	__property System::UnicodeString FileDescription = {read=GetPredefined, write=SetDummyEx, stored=false, index=1};
	__property System::UnicodeString FileName = {read=FFileName, write=SetFileName, stored=StoreFileName};
	__property System::UnicodeString FileVersion = {read=GetPredefined, write=SetDummyEx, stored=false, index=2};
	__property System::UnicodeString InternalName = {read=GetPredefined, write=SetDummyEx, stored=false, index=3};
	__property System::UnicodeString Language = {read=GetLanguage, write=SetDummy, stored=false};
	__property System::UnicodeString LegalCopyright = {read=GetPredefined, write=SetDummyEx, stored=false, index=4};
	__property System::UnicodeString LegalTrademarks = {read=GetPredefined, write=SetDummyEx, stored=false, index=5};
	__property int MajorVersion = {read=GetMajorVersion, write=SetDummyInt, stored=false, nodefault};
	__property int MinorVersion = {read=GetMinorVersion, write=SetDummyInt, stored=false, nodefault};
	__property System::UnicodeString OriginalFilename = {read=GetPredefined, write=SetDummyEx, stored=false, index=6};
	__property System::UnicodeString ProductName = {read=GetPredefined, write=SetDummyEx, stored=false, index=7};
	__property System::UnicodeString ProductVersion = {read=GetPredefined, write=SetDummyEx, stored=false, index=8};
	__property int Release = {read=GetRelease, write=SetDummyInt, stored=false, nodefault};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TElVersionInfo() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elverinfo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELVERINFO)
using namespace Elverinfo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElverinfoHPP
