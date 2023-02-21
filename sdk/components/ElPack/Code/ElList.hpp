// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElList.pas' rev: 35.00 (Windows)

#ifndef EllistHPP
#define EllistHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ElTools.hpp>
#include <ElContBase.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ellist
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElList;
//-- type declarations -------------------------------------------------------
typedef int __fastcall (*TElListSortCompare)(void * Item1, void * Item2, void * Cargo);

typedef int __fastcall (__closure *TElListSortCompareEx)(void * Item1, void * Item2, void * Cargo);

typedef void __fastcall (__closure *TElListDeleteEvent)(System::TObject* Sender, void * Item);

class PASCALIMPLEMENTATION TElList : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
public:
	void * operator[](int Index) { return this->Items[Index]; }
	
protected:
	bool FAutoClearObjects;
	System::Classes::TList* FList;
	TElListDeleteEvent FOnDelete;
	__classmethod void __fastcall Error(const System::UnicodeString Msg, int Data);
	virtual void * __fastcall Get(int Index);
	virtual void __fastcall Put(int Index, void * Item);
	void __fastcall SetCapacity(int NewCapacity);
	void __fastcall SetCount(int NewCount);
	int __fastcall GetCapacity();
	int __fastcall GetCount();
	virtual void __fastcall TriggerDeleteEvent(void * Item);
	
public:
	__fastcall TElList();
	__fastcall virtual ~TElList();
	void * __fastcall FastGet(int Index);
	int __fastcall Add(void * Item);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall DeleteRange(int StartIndex, int EndIndex);
	void __fastcall Exchange(int Index1, int Index2);
	void * __fastcall First();
	int __fastcall IndexOf(void * Item);
	int __fastcall IndexOfBack(int StartIndex, void * Item);
	int __fastcall IndexOfFrom(int StartIndex, void * Item);
	void __fastcall Insert(int Index, void * Item);
	void * __fastcall Last();
	void __fastcall Move(int CurIndex, int NewIndex);
	void __fastcall MoveRange(int CurStart, int CurEnd, int NewStart);
	void __fastcall Pack();
	int __fastcall Remove(void * Item);
	void __fastcall Sort(TElListSortCompare Compare, void * Cargo);
	void __fastcall SortC(TElListSortCompareEx Compare, void * Cargo);
	__property bool AutoClearObjects = {read=FAutoClearObjects, write=FAutoClearObjects, nodefault};
	__property int Capacity = {read=GetCapacity, write=SetCapacity, nodefault};
	__property int Count = {read=GetCount, write=SetCount, nodefault};
	__property void * Items[int Index] = {read=Get, write=Put/*, default*/};
	__property System::Classes::TList* List = {read=FList};
	__property TElListDeleteEvent OnDelete = {read=FOnDelete, write=FOnDelete};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word AlignMem = System::Word(0x500);
}	/* namespace Ellist */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELLIST)
using namespace Ellist;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EllistHPP
