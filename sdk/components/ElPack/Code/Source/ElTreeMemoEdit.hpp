// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreeMemoEdit.pas' rev: 34.00 (Windows)

#ifndef EltreememoeditHPP
#define EltreememoeditHPP

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
#include <ElACtrls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreememoedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElTreeInplaceMemoEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElTreeInplaceMemoEdit : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	Elactrls::TElAdvancedMemo* FEditor;
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall SetEditorParent();
	
public:
	__fastcall virtual TElTreeInplaceMemoEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceMemoEdit();
	__property Elactrls::TElAdvancedMemo* Editor = {read=FEditor};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreememoedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREEMEMOEDIT)
using namespace Eltreememoedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreememoeditHPP
