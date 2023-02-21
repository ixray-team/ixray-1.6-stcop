// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElUnicodeStrings.pas' rev: 35.00 (Windows)

#ifndef ElunicodestringsHPP
#define ElunicodestringsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Consts.hpp>
#include <ElArray.hpp>
#include <ElTools.hpp>
#include <ElStrUtils.hpp>
#include <System.RTLConsts.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elunicodestrings
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElWideStrings;
class DELPHICLASS TElWideStringList;
struct TWideStringItem;
class DELPHICLASS TElWideStringArray;
//-- type declarations -------------------------------------------------------
typedef TElWideStrings TElFStrings;

typedef TElWideStringList TElFStringList;

typedef TWideStringItem *PWideStringItem;

struct DECLSPEC_DRECORD TWideStringItem
{
public:
	System::WideString FString;
	System::TObject* FObject;
};


typedef System::StaticArray<TWideStringItem, 16777216> TWideStringItemList;

typedef TWideStringItemList *PWideStringItemList;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElWideStrings : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	System::WideString operator[](int Index) { return this->Strings[Index]; }
	
private:
	int FUpdateCount;
	bool FSaveUnicode;
	bool FSaved;
	System::WideString __fastcall GetCommaText();
	System::WideString __fastcall GetName(int Index);
	System::WideString __fastcall GetValue(const System::WideString Name);
	void __fastcall ReadData(System::Classes::TReader* Reader);
	void __fastcall SetCommaText(System::WideString Value);
	void __fastcall SetValue(const System::WideString Name, const System::WideString Value);
	void __fastcall WriteData(System::Classes::TWriter* Writer);
	void __fastcall StrSwapByteOrder(System::WideChar * Str);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall Error(const System::UnicodeString Msg, int Data);
	virtual System::WideString __fastcall Get(int Index) = 0 ;
	virtual int __fastcall GetCapacity();
	virtual int __fastcall GetCount() = 0 ;
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual System::WideString __fastcall GetTextStr();
	virtual void __fastcall Put(int Index, const System::WideString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetTextStr(const System::WideString Value);
	virtual void __fastcall SetUpdateState(bool Updating);
	
public:
	__fastcall virtual ~TElWideStrings();
	virtual int __fastcall Add(const System::WideString S);
	virtual int __fastcall AddObject(const System::WideString S, System::TObject* AObject);
	virtual void __fastcall AddStrings(TElWideStrings* Strings);
	void __fastcall Append(const System::WideString S);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall BeginUpdate();
	virtual void __fastcall Clear() = 0 ;
	virtual void __fastcall Delete(int Index) = 0 ;
	void __fastcall EndUpdate();
	HIDESBASE bool __fastcall Equals(TElWideStrings* Strings);
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual System::WideChar * __fastcall GetText();
	virtual int __fastcall IndexOf(const System::WideString S);
	int __fastcall IndexOfName(const System::WideString Name);
	int __fastcall IndexOfObject(System::TObject* AObject);
	virtual void __fastcall Insert(int Index, const System::WideString S) = 0 ;
	void __fastcall InsertObject(int Index, const System::WideString S, System::TObject* AObject);
	virtual void __fastcall LoadFromFile(const System::UnicodeString FileName);
	virtual void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	virtual void __fastcall Move(int CurIndex, int NewIndex);
	virtual void __fastcall SaveToFile(const System::UnicodeString FileName);
	virtual void __fastcall SaveToStream(System::Classes::TStream* Stream);
	virtual void __fastcall SetText(System::WideChar * Text);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	__property int Capacity = {read=GetCapacity, write=SetCapacity, nodefault};
	__property System::WideString CommaText = {read=GetCommaText, write=SetCommaText};
	__property int Count = {read=GetCount, nodefault};
	__property System::WideString Names[int Index] = {read=GetName};
	__property System::TObject* Objects[int Index] = {read=GetObject, write=PutObject};
	__property bool SaveUnicode = {read=FSaveUnicode, write=FSaveUnicode, nodefault};
	__property System::WideString Strings[int Index] = {read=Get, write=Put/*, default*/};
	__property System::WideString Text = {read=GetTextStr, write=SetTextStr};
	__property System::WideString Values[const System::WideString Name] = {read=GetValue, write=SetValue};
public:
	/* TObject.Create */ inline __fastcall TElWideStrings() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

typedef int __fastcall (*TElWideStringListSortCompare)(TElWideStringList* List, int Index1, int Index2);

class PASCALIMPLEMENTATION TElWideStringList : public TElWideStrings
{
	typedef TElWideStrings inherited;
	
private:
	System::Types::TDuplicates FDuplicates;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnChanging;
	bool FSorted;
	int FCapacity;
	int FCount;
	TWideStringItemList *FList;
	void __fastcall ExchangeItems(int Index1, int Index2);
	void __fastcall Grow();
	void __fastcall InsertItem(int Index, const System::WideString S);
	void __fastcall QuickSort(int L, int R, TElWideStringListSortCompare SCompare);
	void __fastcall SetSorted(bool Value);
	
protected:
	virtual void __fastcall Changed();
	virtual void __fastcall Changing();
	virtual System::WideString __fastcall Get(int Index);
	virtual int __fastcall GetCapacity();
	virtual int __fastcall GetCount();
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall Put(int Index, const System::WideString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall SetCapacity(int NewCapacity);
	virtual void __fastcall SetUpdateState(bool Updating);
	
public:
	__fastcall virtual ~TElWideStringList();
	virtual int __fastcall Add(const System::WideString S);
	virtual void __fastcall Clear();
	virtual void __fastcall CustomSort(TElWideStringListSortCompare Compare);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual bool __fastcall Find(const System::WideString S, int &Index);
	virtual int __fastcall IndexOf(const System::WideString S);
	virtual void __fastcall Insert(int Index, const System::WideString S);
	virtual void __fastcall Sort();
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=FDuplicates, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property bool Sorted = {read=FSorted, write=SetSorted, nodefault};
public:
	/* TObject.Create */ inline __fastcall TElWideStringList() : TElWideStrings() { }
	
};


class PASCALIMPLEMENTATION TElWideStringArray : public TElWideStrings
{
	typedef TElWideStrings inherited;
	
private:
	bool FStoreAssociatedData;
	System::Types::TDuplicates FDuplicates;
	bool FSorted;
	System::Classes::TNotifyEvent FOnChanging;
	System::Classes::TNotifyEvent FOnChange;
	Elarray::TElArray* FArray;
	int FUpdateCount;
	void __fastcall OnItemDelete(System::TObject* Sender, void * Item);
	void __fastcall SetSorted(bool newValue);
	void __fastcall ExchangeItems(int Index1, int Index2);
	
protected:
	virtual void __fastcall QuickSort(int L, int R);
	virtual void __fastcall Changed();
	virtual void __fastcall Changing();
	virtual void __fastcall TriggerChangingEvent();
	virtual void __fastcall TriggerChangeEvent();
	virtual void __fastcall InsertItem(int Index, const System::WideString S);
	virtual System::WideString __fastcall Get(int Index);
	virtual int __fastcall GetCount();
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual void __fastcall Put(int Index, const System::WideString S);
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual void __fastcall PutStringEntry(int Index, const System::WideString S, System::TObject* AObject);
	virtual void __fastcall SetUpdateState(bool Updating);
	
public:
	virtual void __fastcall Clear();
	virtual int __fastcall Add(const System::WideString S);
	virtual int __fastcall AddStringEntry(const System::WideString S, System::TObject* AObject);
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Insert(int Index, const System::WideString S);
	virtual int __fastcall IndexOf(const System::WideString S);
	virtual bool __fastcall Find(const System::WideString S, int &Index);
	virtual void __fastcall Sort();
	virtual void __fastcall Exchange(int Index1, int Index2);
	virtual void __fastcall SaveToBinaryStream(System::Classes::TStream* Stream);
	virtual void __fastcall LoadFromBinaryStream(System::Classes::TStream* Stream);
	__fastcall TElWideStringArray();
	__fastcall virtual ~TElWideStringArray();
	__property System::Types::TDuplicates Duplicates = {read=FDuplicates, write=FDuplicates, nodefault};
	__property bool Sorted = {read=FSorted, write=SetSorted, nodefault};
	
__published:
	__property System::Classes::TNotifyEvent OnChanging = {read=FOnChanging, write=FOnChanging};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property bool StoreAssociatedData = {read=FStoreAssociatedData, write=FStoreAssociatedData, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static const System::WideChar U_LSB_FIRST = (System::WideChar)(0xfeff);
static const System::WideChar U_MSB_FIRST = (System::WideChar)(0xfffe);
}	/* namespace Elunicodestrings */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELUNICODESTRINGS)
using namespace Elunicodestrings;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElunicodestringsHPP
