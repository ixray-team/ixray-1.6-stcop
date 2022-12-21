// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElSndMap.pas' rev: 34.00 (Windows)

#ifndef ElsndmapHPP
#define ElsndmapHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <ElRegUtils.hpp>
#include <Winapi.MMSystem.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElIni.hpp>
#include <ElTools.hpp>
#include <System.TypInfo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elsndmap
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElSoundMap;
//-- type declarations -------------------------------------------------------
typedef System::SmallString<255> TElSoundName;

class PASCALIMPLEMENTATION TElSoundMap : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TStringList* FSchemes;
	Elini::TElIniFile* FStorage;
	System::UnicodeString FStoragePath;
	System::UnicodeString FScheme;
	System::UnicodeString FApplicationKey;
	System::UnicodeString FApplicationName;
	Elini::TElIniFile* FRegIni;
	Elini::TElIniFile* ARegIni;
	System::Classes::TStringList* FEventKeys;
	bool FMute;
	System::UnicodeString __fastcall GetEventLabels(System::UnicodeString EventKey);
	void __fastcall SetEventLabels(System::UnicodeString EventKey, System::UnicodeString newValue);
	bool __fastcall GetEnabled(System::UnicodeString EventKey);
	void __fastcall SetEnabled(System::UnicodeString EventKey, bool newValue);
	System::Classes::TStringList* __fastcall GetSchemes();
	System::Classes::TStringList* __fastcall GetEventKeys();
	System::UnicodeString __fastcall GetEventValues(System::UnicodeString EventKey);
	void __fastcall SetEventValues(System::UnicodeString EventKey, System::UnicodeString newValue);
	void __fastcall SetApplicationName(System::UnicodeString newValue);
	void __fastcall SetApplicationKey(System::UnicodeString newValue);
	void __fastcall SetScheme(System::UnicodeString newValue);
	void __fastcall SetStorage(Elini::TElIniFile* newValue);
	void __fastcall SetStoragePath(System::UnicodeString newValue);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	
public:
	__fastcall virtual TElSoundMap(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElSoundMap();
	void __fastcall Play(System::UnicodeString EventKey);
	void __fastcall Add(System::UnicodeString EventKey, System::UnicodeString EventLabel, System::UnicodeString EventValue, bool Enabled);
	void __fastcall Delete(System::UnicodeString EventKey);
	virtual void __fastcall Loaded();
	__property System::Classes::TStringList* EventKeys = {read=GetEventKeys};
	__property System::UnicodeString EventLabel[System::UnicodeString EventKey] = {read=GetEventLabels, write=SetEventLabels};
	__property System::UnicodeString EventValue[System::UnicodeString EventKey] = {read=GetEventValues, write=SetEventValues};
	__property bool EventEnabled[System::UnicodeString EventKey] = {read=GetEnabled, write=SetEnabled};
	__property System::Classes::TStringList* Schemes = {read=GetSchemes};
	
__published:
	__property bool Mute = {read=FMute, write=FMute, nodefault};
	__property System::UnicodeString ApplicationName = {read=FApplicationName, write=SetApplicationName};
	__property System::UnicodeString ApplicationKey = {read=FApplicationKey, write=SetApplicationKey};
	__property System::UnicodeString Scheme = {read=FScheme, write=SetScheme};
	__property System::UnicodeString StoragePath = {read=FStoragePath, write=SetStoragePath};
	__property Elini::TElIniFile* Storage = {read=FStorage, write=SetStorage};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elsndmap */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELSNDMAP)
using namespace Elsndmap;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElsndmapHPP
