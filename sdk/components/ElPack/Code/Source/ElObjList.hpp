// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElObjList.pas' rev: 34.00 (Windows)

#ifndef ElobjlistHPP
#define ElobjlistHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElList.hpp>
#include <ElIni.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elobjlist
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElObjectList;
class DELPHICLASS TElObjectListItem;
class DELPHICLASS TElHeteroObjectList;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TElObjectListItemClass;

typedef int __fastcall (*TElObjListSortCompare)(TElObjectListItem* Item1, TElObjectListItem* Item2, void * Cargo);

class PASCALIMPLEMENTATION TElObjectList : public Ellist::TElList
{
	typedef Ellist::TElList inherited;
	
public:
	TElObjectListItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	unsigned FLastID;
	TElObjectListItemClass FListItemClass;
	System::Classes::TPersistent* FOwner;
	TElObjectListItem* __fastcall GetItems(int Index);
	void __fastcall SetItems(int Index, TElObjectListItem* Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall TElObjectList(System::Classes::TPersistent* Owner, TElObjectListItemClass ListItemClass);
	HIDESBASE TElObjectListItem* __fastcall Add();
	void __fastcall AddItem(TElObjectListItem* Item);
	virtual void __fastcall AfterLoad(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
	virtual void __fastcall AfterSave(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall BeforeLoad(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
	virtual void __fastcall BeforeSave(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
	HIDESBASE TElObjectListItem* __fastcall First();
	HIDESBASE int __fastcall IndexOf(TElObjectListItem* Item);
	HIDESBASE int __fastcall IndexOfBack(int StartIndex, TElObjectListItem* Item);
	HIDESBASE int __fastcall IndexOfFrom(int StartIndex, TElObjectListItem* Item);
	HIDESBASE void __fastcall Insert(int Index, TElObjectListItem* Item);
	HIDESBASE TElObjectListItem* __fastcall Last();
	HIDESBASE void __fastcall Sort(TElObjListSortCompare Compare, void * Cargo);
	__property TElObjectListItem* Items[int Index] = {read=GetItems, write=SetItems/*, default*/};
	__property TElObjectListItemClass ListItemClass = {read=FListItemClass, write=FListItemClass};
	
__published:
	__property unsigned LastID = {read=FLastID, nodefault};
public:
	/* TElList.Destroy */ inline __fastcall virtual ~TElObjectList() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElObjectListItem : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	unsigned FID;
	TElObjectList* FList;
	bool FLoading;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	
public:
	__fastcall virtual TElObjectListItem(TElObjectList* List);
	__fastcall virtual ~TElObjectListItem();
	virtual void __fastcall AfterLoad(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
	virtual void __fastcall AfterSave(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
	virtual void __fastcall BeforeLoad(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
	virtual void __fastcall BeforeSave(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
	__property unsigned ID = {read=FID, nodefault};
	__property TElObjectList* List = {read=FList};
	__property bool Loading = {read=FLoading, write=FLoading, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElHeteroObjectList : public TElObjectList
{
	typedef TElObjectList inherited;
	
public:
	virtual void __fastcall BeforeLoad(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
	virtual void __fastcall BeforeSave(Elini::TElIniFile* IniFile, System::UnicodeString KeyName);
public:
	/* TElObjectList.Create */ inline __fastcall TElHeteroObjectList(System::Classes::TPersistent* Owner, TElObjectListItemClass ListItemClass) : TElObjectList(Owner, ListItemClass) { }
	
public:
	/* TElList.Destroy */ inline __fastcall virtual ~TElHeteroObjectList() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elobjlist */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELOBJLIST)
using namespace Elobjlist;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElobjlistHPP
