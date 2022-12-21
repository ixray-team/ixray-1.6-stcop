// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCRC32.pas' rev: 34.00 (Windows)

#ifndef Elcrc32HPP
#define Elcrc32HPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Consts.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcrc32
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int __fastcall CRCBuffer(int InitialCRC, void * Buffer, int BufLen);
extern DELPHI_PACKAGE int __fastcall CRCStr(System::UnicodeString Str);
extern DELPHI_PACKAGE int __fastcall CRC32(int crc, const System::Byte c);
}	/* namespace Elcrc32 */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCRC32)
using namespace Elcrc32;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Elcrc32HPP
