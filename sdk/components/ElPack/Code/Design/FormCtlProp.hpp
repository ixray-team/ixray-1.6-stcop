// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FormCtlProp.pas' rev: 5.00

#ifndef FormCtlPropHPP
#define FormCtlPropHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Forms.hpp>	// Pascal unit
#include <Consts.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <TypInfo.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Exptintf.hpp>	// Pascal unit
#include <EditIntf.hpp>	// Pascal unit
#include <ToolIntf.hpp>	// Pascal unit
#include <DsgnIntf.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Formctlprop
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TFormCtlProperty;
class PASCALIMPLEMENTATION TFormCtlProperty : public Dsgnintf::TComponentProperty 
{
	typedef Dsgnintf::TComponentProperty inherited;
	
public:
	virtual void __fastcall GetValues(Classes::TGetStrProc Proc);
	virtual void __fastcall SetValue(const AnsiString Value);
protected:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TFormCtlProperty(const Dsgnintf::_di_IFormDesigner 
		ADesigner, int APropCount) : Dsgnintf::TComponentProperty(ADesigner, APropCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TFormCtlProperty(void) { }
	#pragma option pop
	
};


class DELPHICLASS TFormProperty;
class PASCALIMPLEMENTATION TFormProperty : public Dsgnintf::TEnumProperty 
{
	typedef Dsgnintf::TEnumProperty inherited;
	
private:
	Classes::TStringList* List;
	AnsiString FormName;
	AnsiString FileName;
	void __fastcall EnumProc(const AnsiString FileName, const AnsiString UnitName, const AnsiString FormName
		, const AnsiString DesignClass, Classes::TStrings* CoClasses);
	void __fastcall FNProc(const AnsiString FileName, const AnsiString UnitName, const AnsiString FormName
		, const AnsiString DesignClass, Classes::TStrings* CoClasses);
	
public:
	virtual Dsgnintf::TPropertyAttributes __fastcall GetAttributes(void);
	virtual AnsiString __fastcall GetValue(void);
	virtual void __fastcall GetValues(Classes::TGetStrProc Proc);
	virtual void __fastcall SetValue(const AnsiString Value);
protected:
	#pragma option push -w-inl
	/* TPropertyEditor.Create */ inline __fastcall virtual TFormProperty(const Dsgnintf::_di_IFormDesigner 
		ADesigner, int APropCount) : Dsgnintf::TEnumProperty(ADesigner, APropCount) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TPropertyEditor.Destroy */ inline __fastcall virtual ~TFormProperty(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Formctlprop */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Formctlprop;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// FormCtlProp
