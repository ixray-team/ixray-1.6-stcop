// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxBoxProcs.pas' rev: 35.00 (Windows)

#ifndef MxboxprocsHPP
#define MxboxprocsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <MXCtrls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxboxprocs
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall BoxMoveSelected(Vcl::Controls::TWinControl* List, System::Classes::TStrings* Items);
extern DELPHI_PACKAGE int __fastcall BoxGetFirstSelection(Vcl::Controls::TWinControl* List);
extern DELPHI_PACKAGE void __fastcall BoxSetItem(Vcl::Controls::TWinControl* List, int Index);
extern DELPHI_PACKAGE void __fastcall BoxMoveSelectedItems(Vcl::Controls::TWinControl* SrcList, Vcl::Controls::TWinControl* DstList);
extern DELPHI_PACKAGE void __fastcall BoxMoveAllItems(Vcl::Controls::TWinControl* SrcList, Vcl::Controls::TWinControl* DstList);
extern DELPHI_PACKAGE bool __fastcall BoxCanDropItem(Vcl::Controls::TWinControl* List, int X, int Y, int &DragIndex);
extern DELPHI_PACKAGE void __fastcall BoxDragOver(Vcl::Controls::TWinControl* List, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept, bool Sorted);
extern DELPHI_PACKAGE void __fastcall BoxMoveFocusedItem(Vcl::Controls::TWinControl* List, int DstIndex);
}	/* namespace Mxboxprocs */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXBOXPROCS)
using namespace Mxboxprocs;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxboxprocsHPP
