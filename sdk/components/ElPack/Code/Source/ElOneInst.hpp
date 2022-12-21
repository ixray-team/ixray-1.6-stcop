// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElOneInst.pas' rev: 34.00 (Windows)

#ifndef EloneinstHPP
#define EloneinstHPP

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
#include <Vcl.Forms.hpp>
#include <ElTools.hpp>
#include <ElBaseComp.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eloneinst
{
//-- forward type declarations -----------------------------------------------
struct TElMemMapArr;
class DELPHICLASS TElOneInstance;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TInstanceRunEvent)(System::TObject* Sender, System::Classes::TStrings* Parameters);

typedef TElMemMapArr *PElMemMapArr;

struct DECLSPEC_DRECORD TElMemMapArr
{
public:
	NativeUInt hPrevInst;
	NativeUInt hPrevAppWin;
	NativeUInt hPrevMainWin;
	NativeUInt hMonWin;
};


typedef void __fastcall (__closure *TExistsEvent)(System::TObject* Sender, NativeUInt hPrevInst, HWND hPrevAppWin, HWND hPrevMainWin, bool &Switch);

class PASCALIMPLEMENTATION TElOneInstance : public Elbasecomp::TElBaseComponent
{
	typedef Elbasecomp::TElBaseComponent inherited;
	
private:
	NativeUInt FMapHandle;
	NativeUInt FPrevInst;
	NativeUInt FPrevAppWin;
	NativeUInt FPrevMainWin;
	TElMemMapArr *FView;
	System::UnicodeString FMapName;
	bool FNoAutoTerminate;
	TExistsEvent FOnExists;
	TInstanceRunEvent FOnInstanceRun;
	MESSAGE void __fastcall WMCopyData(Winapi::Messages::TMessage &Msg);
	
protected:
	virtual void __fastcall TriggerExistsEvent(NativeUInt hPrevInst, NativeUInt hPrevAppWin, NativeUInt hPrevMainWin, bool &Switch);
	virtual void __fastcall TriggerInstanceRunEvent(System::Classes::TStrings* Parameters);
	virtual void __fastcall SetEnabled(bool AEnabled);
	virtual void __fastcall DoSetEnabled(bool AEnabled);
	void __fastcall CreateMapping();
	
public:
	__fastcall virtual ~TElOneInstance();
	__property NativeUInt PrevInstance = {read=FPrevInst, nodefault};
	__property NativeUInt FPrevMainWindow = {read=FPrevMainWin, nodefault};
	__property NativeUInt FPrevAppWindow = {read=FPrevAppWin, nodefault};
	
__published:
	__property System::UnicodeString MapName = {read=FMapName, write=FMapName};
	__property bool NoAutoTerminate = {read=FNoAutoTerminate, write=FNoAutoTerminate, nodefault};
	__property TExistsEvent OnExists = {read=FOnExists, write=FOnExists};
	__property TInstanceRunEvent OnInstanceRun = {read=FOnInstanceRun, write=FOnInstanceRun};
	__property Enabled = {default=0};
public:
	/* TElBaseComponent.Create */ inline __fastcall virtual TElOneInstance(System::Classes::TComponent* AOwner) : Elbasecomp::TElBaseComponent(AOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString rs_OneInstAlreadyExists;
}	/* namespace Eloneinst */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELONEINST)
using namespace Eloneinst;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EloneinstHPP
