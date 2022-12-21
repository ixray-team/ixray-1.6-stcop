// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ColnProp.pas' rev: 34.00 (Windows)

#ifndef ColnpropHPP
#define ColnpropHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Controls.hpp>
#include <System.SysUtils.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <DesignWindows.hpp>
#include <DsnConst.hpp>
#include <ColnEdit.hpp>
#include <ElStatBar.hpp>
#include <ElSideBar.hpp>

//-- user supplied -----------------------------------------------------------

namespace Colnprop
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElStatusBarEditor;
class DELPHICLASS TElSideBarEditor;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TElStatusBarEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TElStatusBarEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElStatusBarEditor() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElSideBarEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TElSideBarEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElSideBarEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Colnprop */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_COLNPROP)
using namespace Colnprop;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ColnpropHPP
