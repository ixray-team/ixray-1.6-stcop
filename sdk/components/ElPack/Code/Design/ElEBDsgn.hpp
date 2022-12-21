// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElEBDsgn.pas' rev: 34.00 (Windows)

#ifndef ElebdsgnHPP
#define ElebdsgnHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ElExpBar.hpp>
#include <DesignIntf.hpp>
#include <DesignEditors.hpp>
#include <DesignWindows.hpp>
#include <DsnConst.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elebdsgn
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElExplorerBarEditor;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TElExplorerBarEditor : public Designeditors::TComponentEditor
{
	typedef Designeditors::TComponentEditor inherited;
	
public:
	virtual void __fastcall ExecuteVerb(int Index);
	virtual System::UnicodeString __fastcall GetVerb(int Index);
	virtual int __fastcall GetVerbCount();
public:
	/* TComponentEditor.Create */ inline __fastcall virtual TElExplorerBarEditor(System::Classes::TComponent* AComponent, Designintf::_di_IDesigner ADesigner) : Designeditors::TComponentEditor(AComponent, ADesigner) { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElExplorerBarEditor() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elebdsgn */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELEBDSGN)
using namespace Elebdsgn;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElebdsgnHPP
