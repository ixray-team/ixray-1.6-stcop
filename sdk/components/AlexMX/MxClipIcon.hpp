// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxClipIcon.pas' rev: 34.00 (Windows)

#ifndef MxclipiconHPP
#define MxclipiconHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxclipicon
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::Word CF_ICON;
extern DELPHI_PACKAGE void __fastcall CopyIconToClipboard(Vcl::Graphics::TIcon* Icon, System::Uitypes::TColor BackColor);
extern DELPHI_PACKAGE void __fastcall AssignClipboardIcon(Vcl::Graphics::TIcon* Icon);
extern DELPHI_PACKAGE Vcl::Graphics::TIcon* __fastcall CreateIconFromClipboard(void);
extern DELPHI_PACKAGE void __fastcall GetIconSize(HICON Icon, int &W, int &H);
extern DELPHI_PACKAGE HICON __fastcall CreateRealSizeIcon(Vcl::Graphics::TIcon* Icon);
extern DELPHI_PACKAGE void __fastcall DrawRealSizeIcon(Vcl::Graphics::TCanvas* Canvas, Vcl::Graphics::TIcon* Icon, int X, int Y);
}	/* namespace Mxclipicon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXCLIPICON)
using namespace Mxclipicon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxclipiconHPP
