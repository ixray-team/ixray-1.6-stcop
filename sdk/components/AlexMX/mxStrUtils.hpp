// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'mxStrUtils.pas' rev: 35.00 (Windows)

#ifndef MxstrutilsHPP
#define MxstrutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxstrutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::Sysutils::TSysCharSet TCharSet;

//-- var, const, procedure ---------------------------------------------------
#define CRLF L"\r\n"
#define DigitChars (System::Set<char, _DELPHI_SET_CHAR(0), _DELPHI_SET_CHAR(255)>() << '\x30' << '\x31' << '\x32' << '\x33' << '\x34' << '\x35' << '\x36' << '\x37' << '\x38' << '\x39' )
#define Brackets (System::Set<char, _DELPHI_SET_CHAR(0), _DELPHI_SET_CHAR(255)>() << '\x28' << '\x29' << '\x5b' << '\x5d' << '\x7b' << '\x7d' )
#define StdWordDelims (System::Set<char, _DELPHI_SET_CHAR(0), _DELPHI_SET_CHAR(255)>() << '\x0' << '\x1' << '\x2' << '\x3' << '\x4' << '\x5' << '\x6' << '\x7' << '\x8' << '\x9' << '\xa' << '\xb' << '\xc' << '\xd' << '\xe' << '\xf' << '\x10' << '\x11' << '\x12' << '\x13' << '\x14' << '\x15' << '\x16' << '\x17' << '\x18' << '\x19' << '\x1a' << '\x1b' << '\x1c' << '\x1d' << '\x1e' << '\x1f' << '\x20' << '\x22' << '\x27' << '\x28' << '\x29' << '\x2c' << '\x2e' << '\x2f' << '\x3a' << '\x3b' << '\x5b' << '\x5c' << '\x5d' << '\x60' << '\x7b' << '\x7d' )
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrToOem(const System::UnicodeString AnsiStr);
extern DELPHI_PACKAGE System::UnicodeString __fastcall OemToAnsiStr(const System::UnicodeString OemStr);
extern DELPHI_PACKAGE bool __fastcall IsEmptyStr(const System::UnicodeString S, const System::Sysutils::TSysCharSet &EmptyChars);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ReplaceStr(const System::UnicodeString S, const System::UnicodeString Srch, const System::UnicodeString Replace);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DelSpace(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DelChars(const System::UnicodeString S, System::WideChar Chr);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DelBSpace(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DelESpace(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DelRSpace(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall DelSpace1(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Tab2Space(const System::UnicodeString S, System::Byte Numb);
extern DELPHI_PACKAGE System::UnicodeString __fastcall MakeStr(System::WideChar C, int N);
extern DELPHI_PACKAGE System::UnicodeString __fastcall MS(System::WideChar C, int N);
extern DELPHI_PACKAGE int __fastcall NPos(const System::UnicodeString C, System::UnicodeString S, int N);
extern DELPHI_PACKAGE System::UnicodeString __fastcall AddChar(System::WideChar C, const System::UnicodeString S, int N);
extern DELPHI_PACKAGE System::UnicodeString __fastcall AddCharR(System::WideChar C, const System::UnicodeString S, int N);
extern DELPHI_PACKAGE System::UnicodeString __fastcall LeftStr(const System::UnicodeString S, int N);
extern DELPHI_PACKAGE System::UnicodeString __fastcall RightStr(const System::UnicodeString S, int N);
extern DELPHI_PACKAGE int __fastcall CompStr(const System::UnicodeString S1, const System::UnicodeString S2);
extern DELPHI_PACKAGE int __fastcall CompText(const System::UnicodeString S1, const System::UnicodeString S2);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Copy2Symb(const System::UnicodeString S, System::WideChar Symb);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Copy2SymbDel(System::UnicodeString &S, System::WideChar Symb);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Copy2Space(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Copy2SpaceDel(System::UnicodeString &S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall AnsiProperCase(const System::UnicodeString S, const System::Sysutils::TSysCharSet &WordDelims);
extern DELPHI_PACKAGE int __fastcall WordCount(const System::UnicodeString S, const System::Sysutils::TSysCharSet &WordDelims);
extern DELPHI_PACKAGE int __fastcall WordPosition(const int N, const System::UnicodeString S, const System::Sysutils::TSysCharSet &WordDelims);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractWord(int N, const System::UnicodeString S, const System::Sysutils::TSysCharSet &WordDelims);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractWordPos(int N, const System::UnicodeString S, const System::Sysutils::TSysCharSet &WordDelims, int &Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractDelimited(int N, const System::UnicodeString S, const System::Sysutils::TSysCharSet &Delims);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractSubstr(const System::UnicodeString S, int &Pos, const System::Sysutils::TSysCharSet &Delims);
extern DELPHI_PACKAGE bool __fastcall IsWordPresent(const System::UnicodeString W, const System::UnicodeString S, const System::Sysutils::TSysCharSet &WordDelims);
extern DELPHI_PACKAGE System::UnicodeString __fastcall QuotedString(const System::UnicodeString S, System::WideChar Quote);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractQuotedString(const System::UnicodeString S, System::WideChar Quote);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Numb2USA(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CenterStr(const System::UnicodeString S, int Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Dec2Hex(int N, System::Byte A);
extern DELPHI_PACKAGE System::UnicodeString __fastcall D2H(int N, System::Byte A);
extern DELPHI_PACKAGE int __fastcall Hex2Dec(const System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall H2D(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Dec2Numb(int N, System::Byte A, System::Byte B);
extern DELPHI_PACKAGE int __fastcall Numb2Dec(System::UnicodeString S, System::Byte B);
extern DELPHI_PACKAGE int __fastcall RomanToInt(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall IntToRoman(int Value);
extern DELPHI_PACKAGE System::UnicodeString __fastcall IntToBin(int Value, int Digits, int Spaces);
extern DELPHI_PACKAGE int __fastcall FindPart(const System::UnicodeString HelpWilds, const System::UnicodeString InputStr);
extern DELPHI_PACKAGE bool __fastcall IsWild(System::UnicodeString InputStr, System::UnicodeString Wilds, bool IgnoreCase);
extern DELPHI_PACKAGE System::ShortString __fastcall XorString(const System::ShortString &Key, const System::ShortString &Src);
extern DELPHI_PACKAGE System::UnicodeString __fastcall XorEncode(const System::UnicodeString Key, const System::UnicodeString Source);
extern DELPHI_PACKAGE System::UnicodeString __fastcall XorDecode(const System::UnicodeString Key, const System::UnicodeString Source);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetCmdLineArg(const System::UnicodeString Switch, const System::Sysutils::TSysCharSet &SwitchChars);
}	/* namespace Mxstrutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXSTRUTILS)
using namespace Mxstrutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxstrutilsHPP
