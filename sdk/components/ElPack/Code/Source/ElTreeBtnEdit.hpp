// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreeBtnEdit.pas' rev: 34.00 (Windows)

#ifndef EltreebtneditHPP
#define EltreebtneditHPP

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
#include <ElBtnEdit.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreebtnedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElTreeInplaceButtonEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElTreeInplaceButtonEdit : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	Elbtnedit::TElButtonEdit* FEditor;
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual void __fastcall SetEditorParent();
	
public:
	__fastcall virtual TElTreeInplaceButtonEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceButtonEdit();
	__property Elbtnedit::TElButtonEdit* Editor = {read=FEditor};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreebtnedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREEBTNEDIT)
using namespace Eltreebtnedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreebtneditHPP
