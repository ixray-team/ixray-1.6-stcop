// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreeCheckBoxEdit.pas' rev: 34.00 (Windows)

#ifndef EltreecheckboxeditHPP
#define EltreecheckboxeditHPP

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
#include <ElCheckCtl.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreecheckboxedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElTreeInplaceCheckBoxEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElTreeInplaceCheckBoxEdit : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	System::Classes::TWndMethod SaveWndProc;
	void __fastcall EditorWndProc(Winapi::Messages::TMessage &Message);
	
protected:
	Elcheckctl::TElCheckBox* FEditor;
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall DoStopOperation(bool Accepted);
	virtual bool __fastcall GetVisible();
	virtual void __fastcall TriggerAfterOperation(bool &Accepted, bool &DefaultConversion);
	virtual void __fastcall TriggerBeforeOperation(bool &DefaultConversion);
	virtual void __fastcall SetEditorParent();
	
public:
	__fastcall virtual TElTreeInplaceCheckBoxEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceCheckBoxEdit();
	__property Elcheckctl::TElCheckBox* Editor = {read=FEditor};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreecheckboxedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREECHECKBOXEDIT)
using namespace Eltreecheckboxedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreecheckboxeditHPP
