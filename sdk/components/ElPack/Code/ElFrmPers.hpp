// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElFrmPers.pas' rev: 35.00 (Windows)

#ifndef ElfrmpersHPP
#define ElfrmpersHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElHook.hpp>
#include <ElVCLUtils.hpp>
#include <ElMTree.hpp>
#include <System.TypInfo.hpp>
#include <ElTools.hpp>
#include <ElIni.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elfrmpers
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElMinMaxInfo;
class DELPHICLASS TElStoredProps;
class DELPHICLASS TElFormPersist;
struct TElPropData;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElPersistOption : unsigned char { epoState, epoPosition, epoProperties, epoActiveControl, epoSize };

typedef System::Set<TElPersistOption, TElPersistOption::epoState, TElPersistOption::epoSize> TElPersistOptions;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElMinMaxInfo : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	tagMINMAXINFO FInfo;
	TElFormPersist* FOwner;
	void __fastcall SetInfo(int index, int Value);
	int __fastcall GetInfo(int index);
	
__published:
	__property int MaxPosX = {read=GetInfo, write=SetInfo, index=0, default=0};
	__property int MaxPosY = {read=GetInfo, write=SetInfo, index=1, default=0};
	__property int MaxSizeX = {read=GetInfo, write=SetInfo, index=2, default=0};
	__property int MaxSizeY = {read=GetInfo, write=SetInfo, index=3, default=0};
	__property int MaxTrackX = {read=GetInfo, write=SetInfo, index=4, default=0};
	__property int MaxTrackY = {read=GetInfo, write=SetInfo, index=5, default=0};
	__property int MinTrackX = {read=GetInfo, write=SetInfo, index=6, default=0};
	__property int MinTrackY = {read=GetInfo, write=SetInfo, index=7, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TElMinMaxInfo() { }
	
public:
	/* TObject.Create */ inline __fastcall TElMinMaxInfo() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElStoredProps : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	/* TObject.Create */ inline __fastcall TElStoredProps() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TElStoredProps() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElFormPersist : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FTopMost;
	TElPersistOptions FPersistOptions;
	bool FActive;
	TElMinMaxInfo* FMinMaxInfo;
	Elmtree::TElMTree* FProps;
	Elini::TElIniFile* FStorage;
	System::UnicodeString FStoragePath;
	System::Classes::TNotifyEvent FOnSave;
	System::Classes::TNotifyEvent FOnRestore;
	Elhook::TElHook* FHook;
	TElStoredProps* FStoredProps;
	System::Types::TRect FSaveSize;
	System::Types::TRect RealDims;
	void __fastcall SetStorage(Elini::TElIniFile* newValue);
	void __fastcall SetMinMaxInfo(TElMinMaxInfo* newValue);
	void __fastcall SetActive(bool newValue);
	void __fastcall OnHook(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall OnAfterHook(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall ReadPropsList(System::Classes::TStream* Stream);
	void __fastcall WritePropsList(System::Classes::TStream* Stream);
	void __fastcall OnSavePropData(System::TObject* Sender, Elmtree::TElMTreeItem Item, System::Classes::TStream* Stream);
	void __fastcall OnLoadPropData(System::TObject* Sender, Elmtree::TElMTreeItem Item, System::Classes::TStream* Stream);
	void __fastcall OnDelPropData(System::TObject* Sender, Elmtree::TElMTreeItem Item, void * Data);
	Elmtree::TElMTree* __fastcall GetPropsToStore();
	void __fastcall SetTopMost(bool newValue);
	void __fastcall UpdateTopmost();
	
protected:
	bool FFlipped;
	virtual void __fastcall TriggerSaveEvent();
	virtual void __fastcall TriggerRestoreEvent();
	void __fastcall InfoChanged();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall UpdatePosition();
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	virtual void __fastcall Loaded();
	void __fastcall SetFlipped(bool Value);
	void __fastcall DoFlip(bool Flip);
	
public:
	__fastcall virtual TElFormPersist(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElFormPersist();
	void __fastcall Save();
	void __fastcall Restore();
	void __fastcall SavePosition();
	void __fastcall RestorePosition();
	void __fastcall SaveProps();
	void __fastcall RestoreProps();
	__property Elmtree::TElMTree* PropsToStore = {read=GetPropsToStore};
	
__published:
	__property Elini::TElIniFile* Storage = {read=FStorage, write=SetStorage};
	__property System::UnicodeString StoragePath = {read=FStoragePath, write=FStoragePath};
	__property System::Classes::TNotifyEvent OnSave = {read=FOnSave, write=FOnSave};
	__property System::Classes::TNotifyEvent OnRestore = {read=FOnRestore, write=FOnRestore};
	__property TElMinMaxInfo* MinMaxInfo = {read=FMinMaxInfo, write=SetMinMaxInfo};
	__property bool Active = {read=FActive, write=SetActive, default=1};
	__property TElPersistOptions PersistOptions = {read=FPersistOptions, write=FPersistOptions, nodefault};
	__property TElStoredProps* StoredProps = {read=FStoredProps};
	__property bool TopMost = {read=FTopMost, write=SetTopMost, nodefault};
	__property bool Flipped = {read=FFlipped, write=SetFlipped, nodefault};
};


enum DECLSPEC_DENUM TElStoreType : unsigned char { estComp, estProp, estCollection };

struct DECLSPEC_DRECORD TElPropData
{
public:
	TElStoreType PropType;
	bool Store;
	System::UnicodeString Name;
};


typedef TElPropData *PElPropData;

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elfrmpers */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELFRMPERS)
using namespace Elfrmpers;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElfrmpersHPP
