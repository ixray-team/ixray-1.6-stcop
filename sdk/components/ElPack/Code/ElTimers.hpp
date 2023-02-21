// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTimers.pas' rev: 35.00 (Windows)

#ifndef EltimersHPP
#define EltimersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltimers
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomElTimer;
class DELPHICLASS TElTimer;
class DELPHICLASS TElPoolTimer;
class DELPHICLASS TElTimerPoolItem;
class DELPHICLASS TElTimerPoolItems;
class DELPHICLASS TElTimerPool;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCustomElTimer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FEnabled;
	unsigned FInterval;
	bool FOneTime;
	System::Classes::TNotifyEvent FOnTimer;
	int FTag;
	void __fastcall SetInterval(const unsigned Value);
	
protected:
	virtual void __fastcall DoTick();
	virtual void __fastcall DoTimer();
	virtual void __fastcall SetEnabled(const bool Value);
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property unsigned Interval = {read=FInterval, write=SetInterval, default=1000};
	__property bool OneTime = {read=FOneTime, write=FOneTime, nodefault};
	__property System::Classes::TNotifyEvent OnTimer = {read=FOnTimer, write=FOnTimer};
	__property int Tag = {read=FTag, write=FTag, nodefault};
	
public:
	__fastcall virtual TCustomElTimer();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TCustomElTimer() { }
	
};


class PASCALIMPLEMENTATION TElTimer : public TCustomElTimer
{
	typedef TCustomElTimer inherited;
	
private:
	int FTimerID;
	HWND FWnd;
	void __fastcall WndProc(Winapi::Messages::TMessage &Msg);
	
protected:
	virtual void __fastcall SetEnabled(const bool Value);
	
public:
	__fastcall virtual TElTimer();
	__fastcall virtual ~TElTimer();
	__property Enabled;
	__property Interval = {default=1000};
	__property OneTime;
	__property OnTimer;
	__property Tag;
};


class PASCALIMPLEMENTATION TElPoolTimer : public TCustomElTimer
{
	typedef TCustomElTimer inherited;
	
private:
	unsigned FElapsed;
	TElTimerPoolItem* FOwner;
	
protected:
	virtual void __fastcall SetEnabled(const bool Value);
	
public:
	void __fastcall Tick(int TickCount);
	__property unsigned Elapsed = {read=FElapsed, write=FElapsed, nodefault};
	__property Enabled;
	__property Interval = {default=1000};
	__property OneTime;
	__property OnTimer;
	__property TElTimerPoolItem* Owner = {read=FOwner};
	__property Tag;
public:
	/* TCustomElTimer.Create */ inline __fastcall virtual TElPoolTimer() : TCustomElTimer() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElPoolTimer() { }
	
};


class PASCALIMPLEMENTATION TElTimerPoolItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TElPoolTimer* FTimer;
	System::Classes::TNotifyEvent FOnTimer;
	bool __fastcall GetEnabled();
	unsigned __fastcall GetInterval();
	bool __fastcall GetOneTime();
	System::Classes::TNotifyEvent __fastcall GetOnTimer();
	int __fastcall GetTag();
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetInterval(const unsigned Value);
	void __fastcall SetOneTime(const bool Value);
	void __fastcall SetOnTimer(const System::Classes::TNotifyEvent Value);
	void __fastcall SetTag(const int Value);
	
public:
	__fastcall virtual TElTimerPoolItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TElTimerPoolItem();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TElPoolTimer* Timer = {read=FTimer};
	
__published:
	__property bool Enabled = {read=GetEnabled, write=SetEnabled, nodefault};
	__property unsigned Interval = {read=GetInterval, write=SetInterval, default=1000};
	__property bool OneTime = {read=GetOneTime, write=SetOneTime, nodefault};
	__property System::Classes::TNotifyEvent OnTimer = {read=GetOnTimer, write=SetOnTimer};
	__property int Tag = {read=GetTag, write=SetTag, nodefault};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElTimerPoolItems : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TElTimerPoolItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	TElTimerPool* FOwner;
	HIDESBASE TElTimerPoolItem* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TElTimerPoolItem* const Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TElTimerPoolItems(TElTimerPool* AOwner);
	HIDESBASE TElTimerPoolItem* __fastcall Add();
	__property TElTimerPoolItem* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TElTimerPoolItems() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElTimerPool : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	int FEnableCount;
	TElTimerPoolItems* FItems;
	int FTimerID;
	unsigned FLastTick;
	bool FPrecise;
	HWND FWnd;
	bool FBlockEvents;
	void __fastcall SetItems(TElTimerPoolItems* Value);
	void __fastcall WndProc(Winapi::Messages::TMessage &Msg);
	
protected:
	virtual void __fastcall SetPrecise(bool newValue);
	virtual void __fastcall EnableTimer(bool Enable);
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TElTimerPool(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTimerPool();
	void __fastcall Tick(int TickCount);
	
__published:
	__property TElTimerPoolItems* Items = {read=FItems, write=SetItems};
	__property bool Precise = {read=FPrecise, write=SetPrecise, default=0};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltimers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTIMERS)
using namespace Eltimers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltimersHPP
