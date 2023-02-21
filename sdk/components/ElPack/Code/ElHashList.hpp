// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHashList.pas' rev: 35.00 (Windows)

#ifndef ElhashlistHPP
#define ElhashlistHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <ElCRC32.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elhashlist
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EElHashListError;
struct THashRecord;
class DELPHICLASS TElHashList;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EElHashListError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EElHashListError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EElHashListError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EElHashListError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EElHashListError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EElHashListError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EElHashListError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EElHashListError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EElHashListError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElHashListError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElHashListError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElHashListError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElHashListError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EElHashListError() { }
	
};

#pragma pack(pop)

typedef System::StaticArray<int, 4> THash;

typedef THash *PHash;

typedef THashRecord *PHashRecord;

struct DECLSPEC_DRECORD THashRecord
{
public:
	THash *Hash;
	void *ItemData;
};


typedef System::StaticArray<PHashRecord, 134217727> THashList;

typedef THashList *PHashList;

enum DECLSPEC_DENUM TElHashType : unsigned char { ehtMD5, ehtQuick, ehtCRC32 };

typedef void __fastcall (__closure *OnHashDeleteEvent)(TElHashList* Sender, void * Data);

enum DECLSPEC_DENUM THashInsertDupesMode : unsigned char { himInsert, himRaise, himReplace, himIgnore, himMove };

class PASCALIMPLEMENTATION TElHashList : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FAutoClearObjects;
	bool FNoCase;
	TElHashType FHashType;
	THashList *FList;
	int FCount;
	int FCapacity;
	OnHashDeleteEvent FOnDelete;
	bool FQuickHash;
	THashInsertDupesMode FInsertDupesMode;
	bool FRaiseOnAbsence;
	void __fastcall Grow();
	void __fastcall SetCapacity(int NewCapacity);
	void * __fastcall GetItem(System::UnicodeString Hash);
	void __fastcall SetQuickHash(bool newValue);
	void __fastcall SetHashType(TElHashType newValue);
	void __fastcall SetAutoClearObjects(bool newValue);
	
protected:
	virtual int __fastcall CalcQuickHash(System::UnicodeString Hash);
	
public:
	__fastcall virtual ~TElHashList();
	void __fastcall AddItem(System::UnicodeString Hash, void * Value);
	void __fastcall DeleteItem(System::UnicodeString Hash);
	void __fastcall InsertItem(int Index, System::UnicodeString Hash, void * Value);
	int __fastcall GetIndex(System::UnicodeString Hash);
	void __fastcall Clear();
	__property int Count = {read=FCount, nodefault};
	__property int Capacity = {read=FCapacity, nodefault};
	void * __fastcall GetByIndex(int Index);
	__fastcall TElHashList();
	__property void * Item[System::UnicodeString Hash] = {read=GetItem};
	__property OnHashDeleteEvent OnDelete = {read=FOnDelete, write=FOnDelete};
	__property bool QuickHash = {read=FQuickHash, write=SetQuickHash, nodefault};
	__property bool RaiseOnAbsence = {read=FRaiseOnAbsence, write=FRaiseOnAbsence, default=0};
	__property THashInsertDupesMode InsertDupesMode = {read=FInsertDupesMode, write=FInsertDupesMode, nodefault};
	__property TElHashType HashType = {read=FHashType, write=SetHashType, nodefault};
	__property bool NoCase = {read=FNoCase, write=FNoCase, nodefault};
	__property bool AutoClearObjects = {read=FAutoClearObjects, write=SetAutoClearObjects, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const int MaxHashListSize = int(0x7ffffff);
}	/* namespace Elhashlist */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHASHLIST)
using namespace Elhashlist;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElhashlistHPP
