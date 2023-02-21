// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxPlacemnt.pas' rev: 35.00 (Windows)

#ifndef MxplacemntHPP
#define MxplacemntHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <Vcl.Controls.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Forms.hpp>
#include <System.IniFiles.hpp>
#include <Vcl.Dialogs.hpp>
#include <mxVCLUtils.hpp>
#include <mxHook.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxplacemnt
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TWinMinMaxInfo;
class DELPHICLASS TFormPlacement;
class DELPHICLASS TFormStorage;
class DELPHICLASS TIniLink;
class DELPHICLASS TStoredValue;
class DELPHICLASS TStoredValues;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TPlacementOption : unsigned char { fpState, fpPosition, fpActiveControl };

typedef System::Set<TPlacementOption, TPlacementOption::fpState, TPlacementOption::fpActiveControl> TPlacementOptions;

enum DECLSPEC_DENUM TPlacementOperation : unsigned char { poSave, poRestore };

enum DECLSPEC_DENUM TPlacementRegRoot : unsigned char { prCurrentUser, prLocalMachine, prCurrentConfig, prClassesRoot, prUsers, prDynData };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TWinMinMaxInfo : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TFormPlacement* FOwner;
	tagMINMAXINFO FMinMaxInfo;
	int __fastcall GetMinMaxInfo(int Index);
	void __fastcall SetMinMaxInfo(int Index, int Value);
	
public:
	bool __fastcall DefaultMinMaxInfo();
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property int MaxPosLeft = {read=GetMinMaxInfo, write=SetMinMaxInfo, index=0, default=0};
	__property int MaxPosTop = {read=GetMinMaxInfo, write=SetMinMaxInfo, index=1, default=0};
	__property int MaxSizeHeight = {read=GetMinMaxInfo, write=SetMinMaxInfo, index=2, default=0};
	__property int MaxSizeWidth = {read=GetMinMaxInfo, write=SetMinMaxInfo, index=3, default=0};
	__property int MaxTrackHeight = {read=GetMinMaxInfo, write=SetMinMaxInfo, index=4, default=0};
	__property int MaxTrackWidth = {read=GetMinMaxInfo, write=SetMinMaxInfo, index=5, default=0};
	__property int MinTrackHeight = {read=GetMinMaxInfo, write=SetMinMaxInfo, index=6, default=0};
	__property int MinTrackWidth = {read=GetMinMaxInfo, write=SetMinMaxInfo, index=7, default=0};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TWinMinMaxInfo() { }
	
public:
	/* TObject.Create */ inline __fastcall TWinMinMaxInfo() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TFormPlacement : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FActive;
	System::UnicodeString FIniFileName;
	System::UnicodeString FIniSection;
	System::Inifiles::TIniFile* FIniFile;
	bool FUseRegistry;
	System::Win::Registry::TRegIniFile* FRegIniFile;
	TPlacementRegRoot FRegistryRoot;
	System::Classes::TList* FLinks;
	TPlacementOptions FOptions;
	int FVersion;
	bool FSaved;
	bool FRestored;
	bool FDestroying;
	bool FPreventResize;
	TWinMinMaxInfo* FWinMinMaxInfo;
	bool FDefMaximize;
	Mxhook::TRxWindowHook* FWinHook;
	System::Classes::TNotifyEvent FSaveFormShow;
	System::Classes::TNotifyEvent FSaveFormDestroy;
	Vcl::Forms::TCloseQueryEvent FSaveFormCloseQuery;
	System::Classes::TNotifyEvent FOnSavePlacement;
	System::Classes::TNotifyEvent FOnRestorePlacement;
	void __fastcall SetEvents();
	void __fastcall RestoreEvents();
	void __fastcall SetHook();
	void __fastcall ReleaseHook();
	void __fastcall CheckToggleHook();
	bool __fastcall CheckMinMaxInfo();
	void __fastcall MinMaxInfoModified();
	void __fastcall SetWinMinMaxInfo(TWinMinMaxInfo* Value);
	System::UnicodeString __fastcall GetIniSection();
	void __fastcall SetIniSection(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetIniFileName();
	void __fastcall SetIniFileName(const System::UnicodeString Value);
	System::TObject* __fastcall GetIniFile();
	void __fastcall SetPreventResize(bool Value);
	void __fastcall UpdatePreventResize();
	void __fastcall UpdatePlacement();
	void __fastcall IniNeeded(bool ReadOnly);
	void __fastcall IniFree();
	void __fastcall AddLink(TIniLink* ALink);
	void __fastcall NotifyLinks(TPlacementOperation Operation);
	void __fastcall RemoveLink(TIniLink* ALink);
	void __fastcall WndMessage(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall FormCloseQuery(System::TObject* Sender, bool &CanClose);
	void __fastcall FormDestroy(System::TObject* Sender);
	Vcl::Forms::TForm* __fastcall GetForm();
	
protected:
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall Save();
	DYNAMIC void __fastcall Restore();
	virtual void __fastcall SavePlacement();
	virtual void __fastcall RestorePlacement();
	virtual System::UnicodeString __fastcall DoReadString(const System::UnicodeString Section, const System::UnicodeString Ident, const System::UnicodeString Default);
	virtual void __fastcall DoWriteString(const System::UnicodeString Section, const System::UnicodeString Ident, const System::UnicodeString Value);
	__property Vcl::Forms::TForm* Form = {read=GetForm};
	
public:
	__fastcall virtual TFormPlacement(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TFormPlacement();
	void __fastcall SaveFormPlacement();
	void __fastcall RestoreFormPlacement();
	System::UnicodeString __fastcall ReadString(const System::UnicodeString Ident, const System::UnicodeString Default);
	void __fastcall WriteString(const System::UnicodeString Ident, const System::UnicodeString Value);
	int __fastcall ReadInteger(const System::UnicodeString Ident, int Default);
	void __fastcall WriteInteger(const System::UnicodeString Ident, int Value);
	void __fastcall EraseSections();
	__property System::TObject* IniFileObject = {read=GetIniFile};
	__property System::Inifiles::TIniFile* IniFile = {read=FIniFile};
	__property System::Win::Registry::TRegIniFile* RegIniFile = {read=FRegIniFile};
	
__published:
	__property bool Active = {read=FActive, write=FActive, default=1};
	__property System::UnicodeString IniFileName = {read=GetIniFileName, write=SetIniFileName};
	__property System::UnicodeString IniSection = {read=GetIniSection, write=SetIniSection};
	__property TWinMinMaxInfo* MinMaxInfo = {read=FWinMinMaxInfo, write=SetWinMinMaxInfo};
	__property TPlacementOptions Options = {read=FOptions, write=FOptions, default=3};
	__property bool PreventResize = {read=FPreventResize, write=SetPreventResize, default=0};
	__property TPlacementRegRoot RegistryRoot = {read=FRegistryRoot, write=FRegistryRoot, default=0};
	__property bool UseRegistry = {read=FUseRegistry, write=FUseRegistry, default=0};
	__property int Version = {read=FVersion, write=FVersion, default=0};
	__property System::Classes::TNotifyEvent OnSavePlacement = {read=FOnSavePlacement, write=FOnSavePlacement};
	__property System::Classes::TNotifyEvent OnRestorePlacement = {read=FOnRestorePlacement, write=FOnRestorePlacement};
};


class PASCALIMPLEMENTATION TFormStorage : public TFormPlacement
{
	typedef TFormPlacement inherited;
	
private:
	System::Classes::TStrings* FStoredProps;
	TStoredValues* FStoredValues;
	void __fastcall SetStoredProps(System::Classes::TStrings* Value);
	void __fastcall SetStoredValues(TStoredValues* Value);
	System::Variant __fastcall GetStoredValue(const System::UnicodeString Name);
	void __fastcall SetStoredValue(const System::UnicodeString Name, const System::Variant &Value);
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall SavePlacement();
	virtual void __fastcall RestorePlacement();
	virtual void __fastcall SaveProperties();
	virtual void __fastcall RestoreProperties();
	virtual void __fastcall WriteState(System::Classes::TWriter* Writer);
	
public:
	__fastcall virtual TFormStorage(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TFormStorage();
	void __fastcall SetNotification();
	__property System::Variant StoredValue[const System::UnicodeString Name] = {read=GetStoredValue, write=SetStoredValue};
	
__published:
	__property System::Classes::TStrings* StoredProps = {read=FStoredProps, write=SetStoredProps};
	__property TStoredValues* StoredValues = {read=FStoredValues, write=SetStoredValues};
};


class PASCALIMPLEMENTATION TIniLink : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TFormPlacement* FStorage;
	System::Classes::TNotifyEvent FOnSave;
	System::Classes::TNotifyEvent FOnLoad;
	System::TObject* __fastcall GetIniObject();
	System::UnicodeString __fastcall GetRootSection();
	void __fastcall SetStorage(TFormPlacement* Value);
	
protected:
	virtual void __fastcall SaveToIni();
	virtual void __fastcall LoadFromIni();
	
public:
	__fastcall virtual ~TIniLink();
	__property System::TObject* IniObject = {read=GetIniObject};
	__property TFormPlacement* Storage = {read=FStorage, write=SetStorage};
	__property System::UnicodeString RootSection = {read=GetRootSection};
	__property System::Classes::TNotifyEvent OnSave = {read=FOnSave, write=FOnSave};
	__property System::Classes::TNotifyEvent OnLoad = {read=FOnLoad, write=FOnLoad};
public:
	/* TObject.Create */ inline __fastcall TIniLink() : System::Classes::TPersistent() { }
	
};


typedef void __fastcall (__closure *TStoredValueEvent)(TStoredValue* Sender, System::Variant &Value);

class PASCALIMPLEMENTATION TStoredValue : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	System::Variant FValue;
	System::UnicodeString FKeyString;
	TStoredValueEvent FOnSave;
	TStoredValueEvent FOnRestore;
	bool __fastcall IsValueStored();
	TStoredValues* __fastcall GetStoredValues();
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	virtual void __fastcall SetDisplayName(const System::UnicodeString Value);
	
public:
	__fastcall virtual TStoredValue(System::Classes::TCollection* Collection);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	void __fastcall Clear();
	virtual void __fastcall Save();
	virtual void __fastcall Restore();
	__property TStoredValues* StoredValues = {read=GetStoredValues};
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetDisplayName};
	__property System::Variant Value = {read=FValue, write=FValue, stored=IsValueStored};
	__property System::UnicodeString KeyString = {read=FKeyString, write=FKeyString};
	__property TStoredValueEvent OnSave = {read=FOnSave, write=FOnSave};
	__property TStoredValueEvent OnRestore = {read=FOnRestore, write=FOnRestore};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TStoredValue() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TStoredValues : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TStoredValue* operator[](int Index) { return this->Items[Index]; }
	
private:
	TFormPlacement* FStorage;
	TStoredValue* __fastcall GetValue(const System::UnicodeString Name);
	void __fastcall SetValue(const System::UnicodeString Name, TStoredValue* StoredValue);
	System::Variant __fastcall GetStoredValue(const System::UnicodeString Name);
	void __fastcall SetStoredValue(const System::UnicodeString Name, const System::Variant &Value);
	HIDESBASE TStoredValue* __fastcall GetItem(int Index);
	HIDESBASE void __fastcall SetItem(int Index, TStoredValue* StoredValue);
	
public:
	__fastcall TStoredValues(System::Classes::TPersistent* AOwner);
	int __fastcall IndexOf(const System::UnicodeString Name);
	virtual void __fastcall SaveValues();
	virtual void __fastcall RestoreValues();
	__property TFormPlacement* Storage = {read=FStorage, write=FStorage};
	__property TStoredValue* Items[int Index] = {read=GetItem, write=SetItem/*, default*/};
	__property TStoredValue* Values[const System::UnicodeString Name] = {read=GetValue, write=SetValue};
	__property System::Variant StoredValue[const System::UnicodeString Name] = {read=GetStoredValue, write=SetStoredValue};
public:
	/* TCollection.Destroy */ inline __fastcall virtual ~TStoredValues() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Mxplacemnt */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXPLACEMNT)
using namespace Mxplacemnt;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxplacemntHPP
