// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElColorMap.pas' rev: 34.00 (Windows)

#ifndef ElcolormapHPP
#define ElcolormapHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElTools.hpp>
#include <ElList.hpp>
#include <ElIni.hpp>
#include <ElCRC32.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcolormap
{
//-- forward type declarations -----------------------------------------------
struct TColorEntry;
class DELPHICLASS TMapChangeLink;
class DELPHICLASS TElColorEntries;
class DELPHICLASS TElColorMap;
//-- type declarations -------------------------------------------------------
typedef TColorEntry *PColorEntry;

struct DECLSPEC_DRECORD TColorEntry
{
public:
	int Id;
	System::UnicodeString Name;
	System::UnicodeString Group;
	bool UseFg;
	bool UseBk;
	System::Uitypes::TColor FgColor;
	System::Uitypes::TColor BkColor;
};


class PASCALIMPLEMENTATION TMapChangeLink : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TNotifyEvent FOnChange;
	
protected:
	virtual void __fastcall TriggerChangeEvent();
	
__published:
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
public:
	/* TObject.Create */ inline __fastcall TMapChangeLink() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TMapChangeLink() { }
	
};


typedef System::StaticArray<int, 16> TCustomColArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElColorEntries : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	/* TObject.Create */ inline __fastcall TElColorEntries() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TElColorEntries() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElColorMap : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
public:
	TColorEntry operator[](int index) { return this->Items[index]; }
	
private:
	Elini::TElIniFile* FStorage;
	Ellist::TElList* FList;
	Ellist::TElList* FLinkList;
	System::Classes::TNotifyEvent FOnChange;
	TElColorEntries* FEntries;
	bool FChanging;
	int FUpdCount;
	TColorEntry __fastcall GetItems(int index);
	void __fastcall SetItems(int index, const TColorEntry &newValue);
	void __fastcall NotifyLinks();
	int __fastcall GetCount();
	void __fastcall SetStorage(Elini::TElIniFile* newValue);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall TriggerChangeEvent();
	virtual void __fastcall ReadData(System::Classes::TStream* Stream);
	virtual void __fastcall WriteData(System::Classes::TStream* Stream);
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	
public:
	System::Classes::TStringList* CustomCols;
	__fastcall virtual TElColorMap(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElColorMap();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual bool __fastcall RegisterNotifyChange(TMapChangeLink* Link);
	virtual bool __fastcall UnregisterNotifyChange(TMapChangeLink* Link);
	virtual bool __fastcall Edit(System::UnicodeString ACaption);
	int __fastcall AddItem(TColorEntry &Entry);
	int __fastcall InsertItem(int Index, TColorEntry &Entry);
	void __fastcall DeleteItem(int index);
	void __fastcall ClearItems();
	int __fastcall EntryByID(int ID);
	int __fastcall MakeID(const TColorEntry &Entry);
	virtual void __fastcall BeginUpdate();
	virtual void __fastcall EndUpdate();
	__property TColorEntry Items[int index] = {read=GetItems, write=SetItems/*, default*/};
	__property int Count = {read=GetCount, nodefault};
	void __fastcall Restore();
	void __fastcall Save();
	
__published:
	__property TElColorEntries* ItemsList = {read=FEntries};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Elini::TElIniFile* Storage = {read=FStorage, write=SetStorage};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elcolormap */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCOLORMAP)
using namespace Elcolormap;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcolormapHPP
