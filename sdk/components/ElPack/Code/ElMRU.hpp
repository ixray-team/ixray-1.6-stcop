// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElMRU.pas' rev: 35.00 (Windows)

#ifndef ElmruHPP
#define ElmruHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Menus.hpp>
#include <ElList.hpp>
#include <ElTools.hpp>
#include <ElStrUtils.hpp>
#include <ElIni.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elmru
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElMRUEntry;
class DELPHICLASS TElMRUSection;
class DELPHICLASS TElMRUSections;
class DELPHICLASS TElMRU;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TMRUChangeEvent)(System::TObject* Sender, TElMRUSection* Section);

typedef void __fastcall (__closure *TMRUClickEvent)(System::TObject* Sender, TElMRUEntry* Entry);

typedef void __fastcall (__closure *TMRUStreamEvent)(System::TObject* Sender, System::Classes::TStream* Stream, TElMRUEntry* Entry);

enum DECLSPEC_DENUM TMRUAddMode : unsigned char { mamAdd, mamInsert };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElMRUEntry : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FName;
	int FData;
	TElMRUSection* FOwner;
	bool FDefault;
	bool FChecked;
	bool FPersistent;
	void __fastcall SetName(System::UnicodeString value);
	void __fastcall SetData(int Value);
	void __fastcall SetDefault(bool Value);
	void __fastcall SetChecked(bool Value);
	
public:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property int Data = {read=FData, write=SetData, nodefault};
	__property TElMRUSection* Section = {read=FOwner};
	__property bool Default = {read=FDefault, write=SetDefault, nodefault};
	__property bool Checked = {read=FChecked, write=SetChecked, nodefault};
	__property bool Persistent = {read=FPersistent, write=FPersistent, nodefault};
public:
	/* TObject.Create */ inline __fastcall TElMRUEntry() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TElMRUEntry() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElMRUSection : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
protected:
	int FTag;
	bool FAutoHide;
	System::UnicodeString FCaption;
	Ellist::TElList* FValues;
	int FCapacity;
	System::UnicodeString FName;
	bool FShowName;
	bool FVisible;
	TElMRU* FOwner;
	void __fastcall SetName(System::UnicodeString newValue);
	void __fastcall SetShowName(bool newValue);
	void __fastcall SetVisible(bool newValue);
	TElMRUEntry* __fastcall GetValue(int index);
	void __fastcall SetCapacity(int newValue);
	int __fastcall GetCount();
	void __fastcall OnEntryDelete(System::TObject* Sender, void * Item);
	void __fastcall SetCaption(System::UnicodeString newValue);
	void __fastcall SetAutoHide(bool newValue);
	
public:
	__fastcall virtual TElMRUSection(System::Classes::TCollection* Collection);
	__fastcall virtual ~TElMRUSection();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Clear();
	__property int Count = {read=GetCount, nodefault};
	virtual TElMRUEntry* __fastcall Add(System::UnicodeString Name, int Data);
	virtual void __fastcall Remove(TElMRUEntry* Entry);
	TElMRUEntry* __fastcall EntryByName(System::UnicodeString Name);
	__property TElMRUEntry* Entries[int index] = {read=GetValue};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property bool ShowCaption = {read=FShowName, write=SetShowName, default=0};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property int Capacity = {read=FCapacity, write=SetCapacity, default=10};
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property bool AutoHide = {read=FAutoHide, write=SetAutoHide, nodefault};
	__property int Tag = {read=FTag, write=FTag, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElMRUSections : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TElMRUSection* operator[](int index) { return this->Items[index]; }
	
private:
	TElMRU* FMRU;
	HIDESBASE TElMRUSection* __fastcall GetItem(int index);
	HIDESBASE void __fastcall SetItem(int index, TElMRUSection* newValue);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TElMRUSections(TElMRU* MRU);
	HIDESBASE TElMRUSection* __fastcall Add();
	__property TElMRUSection* Items[int index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TElMRUSections() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElMRU : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Elini::TElIniFile* FStorage;
	bool FShowAccel;
	bool FAutoUpdate;
	bool FAutoEnable;
	TElMRUSections* FSections;
	Vcl::Menus::TMenuItem* FRecentMenu;
	TMRUAddMode FAddMode;
	bool FRemoveOnClick;
	System::WideChar FAccelDelimiter;
	TMRUChangeEvent FOnChange;
	TMRUClickEvent FOnClick;
	TMRUStreamEvent FOnSaveEntry;
	TMRUStreamEvent FOnLoadEntry;
	Vcl::Menus::TPopupMenu* FPopupMenu;
	System::UnicodeString FStoragePath;
	bool FIgnoreDuplicates;
	void __fastcall SetStorage(Elini::TElIniFile* newValue);
	void __fastcall SetIgnoreDuplicates(bool newValue);
	void __fastcall SetPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	void __fastcall SetSections(TElMRUSections* newValue);
	void __fastcall SetShowAccel(bool newValue);
	void __fastcall SetAutoUpdate(bool newValue);
	void __fastcall SetAutoEnable(bool newValue);
	void __fastcall SetRecentMenu(Vcl::Menus::TMenuItem* newValue);
	void __fastcall OnItemClick(System::TObject* Sender);
	void __fastcall SetAccelDelimiter(System::WideChar newValue);
	
protected:
	virtual void __fastcall TriggerChangeEvent(TElMRUSection* Section);
	virtual void __fastcall TriggerClickEvent(TElMRUEntry* Entry);
	virtual void __fastcall TriggerSaveEntryEvent(System::Classes::TStream* Stream, TElMRUEntry* Entry);
	virtual void __fastcall TriggerLoadEntryEvent(System::Classes::TStream* Stream, TElMRUEntry* Entry);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TElMRU(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElMRU();
	virtual void __fastcall RebuildMenu();
	void __fastcall Restore();
	void __fastcall Save();
	TElMRUSection* __fastcall SectionByName(System::UnicodeString Name);
	
__published:
	__property System::WideChar AccelDelimiter = {read=FAccelDelimiter, write=SetAccelDelimiter, default=32};
	__property TMRUAddMode AddMode = {read=FAddMode, write=FAddMode, default=1};
	__property TElMRUSections* Sections = {read=FSections, write=SetSections};
	__property Elini::TElIniFile* Storage = {read=FStorage, write=SetStorage};
	__property System::UnicodeString StoragePath = {read=FStoragePath, write=FStoragePath};
	__property bool ShowAccel = {read=FShowAccel, write=SetShowAccel, default=1};
	__property bool AutoUpdate = {read=FAutoUpdate, write=SetAutoUpdate, default=1};
	__property bool AutoEnable = {read=FAutoEnable, write=SetAutoEnable, default=1};
	__property Vcl::Menus::TMenuItem* RecentMenu = {read=FRecentMenu, write=SetRecentMenu};
	__property Vcl::Menus::TPopupMenu* PopupMenu = {read=FPopupMenu, write=SetPopupMenu};
	__property bool RemoveOnClick = {read=FRemoveOnClick, write=FRemoveOnClick, default=1};
	__property bool IgnoreDuplicates = {read=FIgnoreDuplicates, write=SetIgnoreDuplicates, default=1};
	__property TMRUChangeEvent OnChange = {read=FOnChange, write=FOnChange};
	__property TMRUClickEvent OnClick = {read=FOnClick, write=FOnClick};
	__property TMRUStreamEvent OnSaveEntry = {read=FOnSaveEntry, write=FOnSaveEntry};
	__property TMRUStreamEvent OnLoadEntry = {read=FOnLoadEntry, write=FOnLoadEntry};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elmru */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELMRU)
using namespace Elmru;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElmruHPP
