// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MXProps.pas' rev: 35.00 (Windows)

#ifndef MxpropsHPP
#define MxpropsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <System.TypInfo.hpp>
#include <mxVCLUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxprops
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TPropInfoList;
class DELPHICLASS TPropsStorage;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TPropInfoList : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::Typinfo::PPropInfo operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Typinfo::TPropList *FList;
	int FCount;
	int FSize;
	System::Typinfo::PPropInfo __fastcall Get(int Index);
	
public:
	__fastcall TPropInfoList(System::TObject* AObject, System::Typinfo::TTypeKinds Filter);
	__fastcall virtual ~TPropInfoList();
	bool __fastcall Contains(System::Typinfo::PPropInfo P);
	System::Typinfo::PPropInfo __fastcall Find(const System::UnicodeString AName);
	void __fastcall Delete(int Index);
	void __fastcall Intersect(TPropInfoList* List);
	__property int Count = {read=FCount, nodefault};
	__property System::Typinfo::PPropInfo Items[int Index] = {read=Get/*, default*/};
};

#pragma pack(pop)

typedef System::UnicodeString __fastcall (__closure *TReadStrEvent)(const System::UnicodeString ASection, const System::UnicodeString Item, const System::UnicodeString Default);

typedef void __fastcall (__closure *TWriteStrEvent)(const System::UnicodeString ASection, const System::UnicodeString Item, const System::UnicodeString Value);

typedef void __fastcall (__closure *TEraseSectEvent)(const System::UnicodeString ASection);

class PASCALIMPLEMENTATION TPropsStorage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::TObject* FObject;
	System::Classes::TComponent* FOwner;
	System::UnicodeString FPrefix;
	System::UnicodeString FSection;
	TReadStrEvent FOnReadString;
	TWriteStrEvent FOnWriteString;
	TEraseSectEvent FOnEraseSection;
	System::UnicodeString __fastcall StoreIntegerProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreCharProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreEnumProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreFloatProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreStringProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreSetProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreClassProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreStringsProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreComponentProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreLStringProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreWCharProperty(System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreVariantProperty(System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadLStringProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadWCharProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadVariantProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	System::UnicodeString __fastcall StoreInt64Property(System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadInt64Property(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadIntegerProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadCharProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadEnumProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadFloatProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadStringProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadSetProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadClassProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadStringsProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadComponentProperty(const System::UnicodeString S, System::Typinfo::PPropInfo PropInfo);
	System::Classes::TStrings* __fastcall CreateInfoList(System::Classes::TComponent* AComponent, System::Classes::TStrings* StoredList);
	void __fastcall FreeInfoLists(System::Classes::TStrings* Info);
	
protected:
	virtual System::UnicodeString __fastcall ReadString(const System::UnicodeString ASection, const System::UnicodeString Item, const System::UnicodeString Default);
	virtual void __fastcall WriteString(const System::UnicodeString ASection, const System::UnicodeString Item, const System::UnicodeString Value);
	virtual void __fastcall EraseSection(const System::UnicodeString ASection);
	virtual System::UnicodeString __fastcall GetItemName(const System::UnicodeString APropName);
	virtual TPropsStorage* __fastcall CreateStorage();
	
public:
	void __fastcall StoreAnyProperty(System::Typinfo::PPropInfo PropInfo);
	void __fastcall LoadAnyProperty(System::Typinfo::PPropInfo PropInfo);
	void __fastcall StoreProperties(System::Classes::TStrings* PropList);
	void __fastcall LoadProperties(System::Classes::TStrings* PropList);
	void __fastcall LoadObjectsProps(System::Classes::TComponent* AComponent, System::Classes::TStrings* StoredList);
	void __fastcall StoreObjectsProps(System::Classes::TComponent* AComponent, System::Classes::TStrings* StoredList);
	__property System::TObject* AObject = {read=FObject, write=FObject};
	__property System::UnicodeString Prefix = {read=FPrefix, write=FPrefix};
	__property System::UnicodeString Section = {read=FSection, write=FSection};
	__property TReadStrEvent OnReadString = {read=FOnReadString, write=FOnReadString};
	__property TWriteStrEvent OnWriteString = {read=FOnWriteString, write=FOnWriteString};
	__property TEraseSectEvent OnEraseSection = {read=FOnEraseSection, write=FOnEraseSection};
public:
	/* TObject.Create */ inline __fastcall TPropsStorage() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TPropsStorage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString sPropNameDelimiter;
extern DELPHI_PACKAGE System::UnicodeString __fastcall CreateStoredItem(const System::UnicodeString CompName, const System::UnicodeString PropName);
extern DELPHI_PACKAGE bool __fastcall ParseStoredItem(const System::UnicodeString Item, System::UnicodeString &CompName, System::UnicodeString &PropName);
extern DELPHI_PACKAGE void __fastcall UpdateStoredList(System::Classes::TComponent* AComponent, System::Classes::TStrings* AStoredList, bool FromForm);
}	/* namespace Mxprops */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXPROPS)
using namespace Mxprops;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxpropsHPP
