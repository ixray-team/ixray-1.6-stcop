// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElIni.pas' rev: 34.00 (Windows)

#ifndef EliniHPP
#define EliniHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Win.Registry.hpp>
#include <ElRegUtils.hpp>
#include <Vcl.Graphics.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElList.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elini
{
//-- forward type declarations -----------------------------------------------
struct TElValueData;
class DELPHICLASS EElIniError;
class DELPHICLASS TElIniEntry;
class DELPHICLASS TElIniFile;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElValueType : unsigned char { evtUnknown, evtBoolean, evtInt, evtString, evtMultiString, evtBinary, evtDouble };

struct DECLSPEC_DRECORD TElValueData
{
	
public:
	TElValueType ValueType;
	union
	{
		struct 
		{
			double DoubleValue;
		};
		struct 
		{
			void *DataValue;
			int DataLength;
		};
		struct 
		{
			System::Classes::TStringList* MStrValue;
		};
		struct 
		{
			System::WideChar *StrValue;
		};
		struct 
		{
			int IntValue;
		};
		struct 
		{
			bool BoolValue;
		};
		
	};
};


typedef TElValueData *PELValueData;

#pragma pack(push,4)
class PASCALIMPLEMENTATION EElIniError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EElIniError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EElIniError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EElIniError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EElIniError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EElIniError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EElIniError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EElIniError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EElIniError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElIniError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElIniError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElIniError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElIniError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EElIniError() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElIniEntry : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Ellist::TElList* FChildren;
	bool FIsKey;
	TElIniEntry* FParent;
	TElValueData FValueData;
	System::UnicodeString FValueName;
	int __fastcall GetSubCount();
	void __fastcall OnValueDelete(System::TObject* Sender, void * Data);
	void __fastcall SetParent(TElIniEntry* Value);
	
protected:
	__fastcall TElIniEntry();
	TElIniEntry* __fastcall GetValue(System::UnicodeString Name);
	__property TElIniEntry* Parent = {read=FParent, write=SetParent};
	
public:
	__fastcall virtual ~TElIniEntry();
	void __fastcall Invalidate();
	__property bool IsKey = {read=FIsKey, nodefault};
	__property int SubCount = {read=GetSubCount, nodefault};
};


class PASCALIMPLEMENTATION TElIniFile : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TNotifyEvent FOnBeforeLoad;
	System::Classes::TNotifyEvent FOnAfterSave;
	System::Classes::TNotifyEvent FOnBeforeSave;
	System::Classes::TNotifyEvent FOnAfterLoad;
	bool FBinaryMode;
	System::UnicodeString FComment;
	TElIniEntry* FCurEntry;
	System::UnicodeString FCurrentKey;
	System::WideChar FDelimiter;
	System::WideChar FDivChar;
	bool FLazyWrite;
	bool FModified;
	System::UnicodeString FPath;
	System::Win::Registry::TRegistry* FRegistry;
	unsigned FRegRoot;
	bool FUseRegistry;
	TElIniEntry* FRoot;
	bool FSimple;
	System::UnicodeString FWarningMessage;
	Elregutils::TRegRootType __fastcall GetRegRoot();
	void __fastcall SetCurrentKey(const System::UnicodeString newValue);
	void __fastcall SetDelimiter(System::WideChar newValue);
	void __fastcall SetLazyWrite(bool newValue);
	void __fastcall SetPath(System::UnicodeString newValue);
	void __fastcall SetSimple(bool newValue);
	void __fastcall SetRegRoot(Elregutils::TRegRootType newValue);
	void __fastcall SetUseRegistry(bool newValue);
	void __fastcall IntLoadKey(System::Classes::TStringList* SL, int CurLine);
	void __fastcall IntLoadBinKey(System::Classes::TStream* F);
	void __fastcall IntSaveKey(System::Classes::TStream* F, System::UnicodeString KeyName, TElIniEntry* KeyEntry);
	void __fastcall IntSaveBinKey(System::Classes::TStream* F, System::UnicodeString KeyName, TElIniEntry* KeyEntry);
	
protected:
	virtual TElIniEntry* __fastcall GetValueEntry(System::UnicodeString Key, System::UnicodeString ValueName);
	virtual void __fastcall ParseLine(System::UnicodeString S, System::UnicodeString &Name, System::UnicodeString &Value, bool &HasName);
	virtual void __fastcall TriggerBeforeSaveEvent();
	virtual void __fastcall TriggerAfterLoadEvent();
	virtual void __fastcall TriggerBeforeLoadEvent();
	virtual void __fastcall TriggerAfterSaveEvent();
	
public:
	__fastcall virtual TElIniFile(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElIniFile();
	bool __fastcall Clear();
	virtual bool __fastcall ClearKey(System::UnicodeString Key);
	TElIniEntry* __fastcall CreateValue(System::UnicodeString Key, System::UnicodeString ValueName);
	bool __fastcall Delete(System::UnicodeString Key, System::UnicodeString ValueName);
	bool __fastcall EnumSubKeys(System::UnicodeString Key, System::Classes::TStrings* Strings);
	bool __fastcall EnumValues(System::UnicodeString Key, System::Classes::TStrings* Strings);
	System::UnicodeString __fastcall FullKey(System::UnicodeString Key);
	TElValueType __fastcall GetValueType(System::UnicodeString Key, System::UnicodeString ValueName);
	bool __fastcall KeyExists(System::UnicodeString Key);
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
	bool __fastcall Load();
	virtual void __fastcall Loaded();
	bool __fastcall MoveEntry(System::UnicodeString Key, System::UnicodeString ValueName, System::UnicodeString NewKey);
	bool __fastcall OpenKey(System::UnicodeString Key, bool CanCreate);
	System::UnicodeString __fastcall OwnKey(System::UnicodeString Key);
	System::UnicodeString __fastcall ParentKey(System::UnicodeString Key);
	bool __fastcall ReadBinary(System::UnicodeString Key, System::UnicodeString ValueName, void *Buffer, int &BufferLen);
	bool __fastcall ReadBool(System::UnicodeString Key, System::UnicodeString ValueName, bool DefValue, bool &Value);
	bool __fastcall ReadDouble(System::UnicodeString Key, System::UnicodeString ValueName, double DefValue, double &Value);
	bool __fastcall ReadInteger(System::UnicodeString Key, System::UnicodeString ValueName, int DefValue, int &Value);
	bool __fastcall ReadMultiString(System::UnicodeString Key, System::UnicodeString ValueName, System::Classes::TStrings* Strings);
	bool __fastcall ReadObject(System::UnicodeString Key, System::Classes::TPersistent* AnObject);
	bool __fastcall ReadString(System::UnicodeString Key, System::UnicodeString ValueName, System::UnicodeString DefValue, System::UnicodeString &Value);
	bool __fastcall ReadColor(System::UnicodeString Key, System::UnicodeString ValueName, System::Uitypes::TColor DefValue, System::Uitypes::TColor &Value);
	bool __fastcall ReadRect(System::UnicodeString Key, System::UnicodeString ValueName, const System::Types::TRect &DefValue, System::Types::TRect &Value);
	virtual bool __fastcall RenameKey(System::UnicodeString Key, System::UnicodeString NewName);
	virtual bool __fastcall RenameValue(System::UnicodeString Key, System::UnicodeString ValueName, System::UnicodeString NewName);
	bool __fastcall Save();
	virtual void __fastcall SetValueType(System::UnicodeString Key, System::UnicodeString ValueName, TElValueType NewType);
	int __fastcall SubKeysCount(System::UnicodeString Key);
	bool __fastcall ValueExists(System::UnicodeString Key, System::UnicodeString ValueName);
	int __fastcall ValuesCount(System::UnicodeString Key);
	bool __fastcall WriteBinary(System::UnicodeString Key, System::UnicodeString ValueName, void *Buffer, int BufferLen);
	bool __fastcall WriteBool(System::UnicodeString Key, System::UnicodeString ValueName, bool Value);
	bool __fastcall WriteDouble(System::UnicodeString Key, System::UnicodeString ValueName, double Value);
	bool __fastcall WriteInteger(System::UnicodeString Key, System::UnicodeString ValueName, int Value);
	bool __fastcall WriteMultiString(System::UnicodeString Key, System::UnicodeString ValueName, System::Classes::TStrings* Strings);
	bool __fastcall WriteObject(System::UnicodeString Key, System::Classes::TPersistent* AnObject);
	bool __fastcall WriteString(System::UnicodeString Key, System::UnicodeString ValueName, System::UnicodeString Value);
	bool __fastcall WriteColor(System::UnicodeString Key, System::UnicodeString ValueName, System::Uitypes::TColor Value);
	bool __fastcall WriteRect(System::UnicodeString Key, System::UnicodeString ValueName, const System::Types::TRect &Value);
	__property System::UnicodeString CurrentKey = {read=FCurrentKey, write=SetCurrentKey};
	__property bool Modified = {read=FModified, nodefault};
	
__published:
	__property bool BinaryMode = {read=FBinaryMode, write=FBinaryMode, nodefault};
	__property System::UnicodeString Comment = {read=FComment, write=FComment};
	__property System::WideChar Delimiter = {read=FDelimiter, write=SetDelimiter, nodefault};
	__property System::WideChar DivChar = {read=FDivChar, write=FDivChar, default=61};
	__property bool LazyWrite = {read=FLazyWrite, write=SetLazyWrite, default=1};
	__property System::UnicodeString Path = {read=FPath, write=SetPath};
	__property bool Simple = {read=FSimple, write=SetSimple, default=0};
	__property Elregutils::TRegRootType RegRoot = {read=GetRegRoot, write=SetRegRoot, nodefault};
	__property bool UseRegistry = {read=FUseRegistry, write=SetUseRegistry, default=0};
	__property System::UnicodeString WarningMessage = {read=FWarningMessage, write=FWarningMessage};
	__property System::Classes::TNotifyEvent OnBeforeSave = {read=FOnBeforeSave, write=FOnBeforeSave};
	__property System::Classes::TNotifyEvent OnAfterLoad = {read=FOnAfterLoad, write=FOnAfterLoad};
	__property System::Classes::TNotifyEvent OnBeforeLoad = {read=FOnBeforeLoad, write=FOnBeforeLoad};
	__property System::Classes::TNotifyEvent OnAfterSave = {read=FOnAfterSave, write=FOnAfterSave};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elini */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELINI)
using namespace Elini;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EliniHPP
