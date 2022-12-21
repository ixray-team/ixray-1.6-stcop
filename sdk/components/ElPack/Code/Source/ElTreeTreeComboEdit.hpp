// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreeTreeComboEdit.pas' rev: 34.00 (Windows)

#ifndef EltreetreecomboeditHPP
#define EltreetreecomboeditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElTree.hpp>
#include <ElHeader.hpp>
#include <ElTools.hpp>
#include <ElStrUtils.hpp>
#include <ElTreeCombo.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreetreecomboedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElTreeInplaceTreeComboEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElTreeInplaceTreeComboEdit : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	System::Classes::TWndMethod SaveTreeWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	void __fastcall TreeWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	Eltreecombo::TElTreeCombo* FEditor;
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall SetEditorParent();
	
public:
	__fastcall virtual TElTreeInplaceTreeComboEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceTreeComboEdit();
	__property Eltreecombo::TElTreeCombo* Editor = {read=FEditor};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreetreecomboedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREETREECOMBOEDIT)
using namespace Eltreetreecomboedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreetreecomboeditHPP
