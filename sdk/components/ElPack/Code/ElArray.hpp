// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElArray.pas' rev: 35.00 (Windows)

#ifndef ElarrayHPP
#define ElarrayHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ElContBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elarray
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElArray;
//-- type declarations -------------------------------------------------------
typedef int __fastcall (*TElArraySortCompare)(void * Item1, void * Item2, void * Cargo);

typedef void __fastcall (__closure *TElArrayDeleteEvent)(System::TObject* Sender, void * Item);

class PASCALIMPLEMENTATION TElArray : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	void * operator[](int Index) { return this->Items[Index]; }
	
protected:
	Elcontbase::TPointerList *FList;
	int FCount;
	int FCapacity;
	bool FAutoClearObjects;
	TElArrayDeleteEvent FOnDelete;
	virtual void * __fastcall Get(int Index);
	virtual void __fastcall Grow();
	virtual void __fastcall Put(int Index, void * Item);
	void __fastcall SetCapacity(int NewCapacity);
	void __fastcall SetCount(int NewCount);
	virtual void __fastcall TriggerDeleteEvent(void * Item);
	__classmethod void __fastcall Error(const System::UnicodeString Msg, int Data);
	
public:
	__fastcall TElArray();
	__fastcall virtual ~TElArray();
	int __fastcall Add(void * Item);
	void __fastcall Clear();
	void __fastcall Assign(TElArray* AList);
	virtual void __fastcall Delete(int Index);
	void __fastcall Exchange(int Index1, int Index2);
	TElArray* __fastcall Expand();
	void * __fastcall First();
	int __fastcall IndexOf(void * Item);
	int __fastcall IndexOfFrom(int StartIndex, void * Item);
	int __fastcall IndexOfBack(int StartIndex, void * Item);
	void __fastcall Insert(int Index, void * Item);
	void * __fastcall Last();
	void __fastcall Move(int CurIndex, int NewIndex);
	void __fastcall MoveRange(int CurStart, int CurEnd, int NewStart);
	int __fastcall Remove(void * Item);
	void __fastcall Pack();
	void __fastcall Sort(TElArraySortCompare Compare, void * Cargo);
	__property int Capacity = {read=FCapacity, write=SetCapacity, default=0};
	__property int Count = {read=FCount, write=SetCount, default=0};
	__property void * Items[int Index] = {read=Get, write=Put/*, default*/};
	__property Elcontbase::PPointerList List = {read=FList};
	__property bool AutoClearObjects = {read=FAutoClearObjects, write=FAutoClearObjects, default=0};
	__property TElArrayDeleteEvent OnDelete = {read=FOnDelete, write=FOnDelete};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elarray */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELARRAY)
using namespace Elarray;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElarrayHPP
