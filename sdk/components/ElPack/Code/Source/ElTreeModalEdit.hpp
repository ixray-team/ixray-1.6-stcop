// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreeModalEdit.pas' rev: 34.00 (Windows)

#ifndef EltreemodaleditHPP
#define EltreemodaleditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Forms.hpp>
#include <ElHeader.hpp>
#include <ElTree.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreemodaledit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElTreeInplaceModalEdit;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TModalEditExecuteEvent)(System::TObject* Sender, bool &Accepted);

class PASCALIMPLEMENTATION TElTreeInplaceModalEdit : public Eltree::TElTreeInplaceEditor
{
	typedef Eltree::TElTreeInplaceEditor inherited;
	
private:
	void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	
protected:
	unsigned RegMsg;
	HWND FWnd;
	bool FInProgress;
	TModalEditExecuteEvent FOnExecute;
	virtual bool __fastcall GetVisible();
	virtual void __fastcall StartOperation();
	virtual void __fastcall DoStartOperation();
	virtual void __fastcall Execute(bool &Accepted);
	
public:
	__fastcall virtual TElTreeInplaceModalEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreeInplaceModalEdit();
	
__published:
	__property TModalEditExecuteEvent OnExecute = {read=FOnExecute, write=FOnExecute};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreemodaledit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREEMODALEDIT)
using namespace Eltreemodaledit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreemodaleditHPP
