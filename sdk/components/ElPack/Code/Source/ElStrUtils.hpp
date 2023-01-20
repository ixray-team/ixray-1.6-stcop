// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElStrUtils.pas' rev: 34.00 (Windows)

#ifndef ElstrutilsHPP
#define ElstrutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.RTLConsts.hpp>

//-- user supplied -----------------------------------------------------------
typedef WideString TElFString;

namespace Elstrutils
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
typedef System::WideString TElFString;

typedef System::WideChar TElFChar;

typedef System::WideChar *PElFChar;

enum DECLSPEC_DENUM Elstrutils__1 : unsigned char { wrfReplaceAll, wrfIgnoreCase };

typedef System::Set<Elstrutils__1, Elstrutils__1::wrfReplaceAll, Elstrutils__1::wrfIgnoreCase> TWideReplaceFlags;

typedef unsigned UTF32;

typedef System::Word UTF16;

typedef System::Byte UTF8;

typedef unsigned *pUTF32;

typedef System::Word *pUTF16;

typedef System::Byte *pUTF8;

enum DECLSPEC_DENUM ConversionResult : unsigned char { conversionOK, sourceExhausted, targetExhausted, sourceIllegal };

enum DECLSPEC_DENUM ConversionFlags : unsigned char { strictConversion, lenientConversion };

//-- var, const, procedure ---------------------------------------------------
#define oleaut L"oleaut32.dll"
extern DELPHI_PACKAGE bool doti;
#define SPathDelimiters L"/\\"
#define SWidePathDelimiters L"/\\"
extern DELPHI_PACKAGE unsigned UNI_REPLACEMENT_CHAR;
extern DELPHI_PACKAGE unsigned UNI_MAX_BMP;
extern DELPHI_PACKAGE unsigned UNI_MAX_UTF16;
extern DELPHI_PACKAGE unsigned UNI_MAX_UTF32;
extern DELPHI_PACKAGE int halfShift;
extern DELPHI_PACKAGE unsigned halfBase;
extern DELPHI_PACKAGE unsigned halfMask;
extern DELPHI_PACKAGE unsigned UNI_SUR_HIGH_START;
extern DELPHI_PACKAGE unsigned UNI_SUR_HIGH_END;
extern DELPHI_PACKAGE unsigned UNI_SUR_LOW_START;
extern DELPHI_PACKAGE unsigned UNI_SUR_LOW_END;
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 256> trailingBytesForUTF8;
extern DELPHI_PACKAGE System::StaticArray<unsigned, 6> offsetsFromUTF8;
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 7> firstByteMark;
extern DELPHI_PACKAGE System::UnicodeString __fastcall IntToStrFmt(int value);
extern DELPHI_PACKAGE System::UnicodeString __fastcall FloatToStrFmt(System::Extended value, int decims);
extern DELPHI_PACKAGE System::UnicodeString __fastcall IntToStrPad(int value, int MinSize);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CenterStr(System::UnicodeString Str, int len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall OEMToStr(System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StrToOEM(System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall MessageRes(int Txt, System::WideChar * Title, System::Word TextType, bool Sounds);
extern DELPHI_PACKAGE bool __fastcall replace(System::UnicodeString &Str, System::UnicodeString SourceString, System::UnicodeString DestString);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractWord(System::UnicodeString str, int n);
extern DELPHI_PACKAGE int __fastcall FstNonSpace(System::UnicodeString str);
extern DELPHI_PACKAGE int __fastcall NextWordBegin(System::UnicodeString str, int CurrentPos);
extern DELPHI_PACKAGE int __fastcall LastPos(System::UnicodeString SubStr, System::UnicodeString Strn);
extern DELPHI_PACKAGE bool __fastcall LineIsEmpty(System::UnicodeString str);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CompleteLine(System::UnicodeString Str, int FLen, System::WideChar symb);
extern DELPHI_PACKAGE System::UnicodeString __fastcall PrefixLine(System::UnicodeString Str, int FLen, System::WideChar symb);
extern DELPHI_PACKAGE System::UnicodeString __fastcall MakeString(int FLen, System::UnicodeString Seq);
extern DELPHI_PACKAGE int __fastcall H2D(System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall H2DDef(const System::UnicodeString S, int Def);
extern DELPHI_PACKAGE int __fastcall Bin2Int(System::UnicodeString S);
extern DELPHI_PACKAGE int __fastcall Bin2IntDef(System::UnicodeString S, int Default);
extern DELPHI_PACKAGE System::UnicodeString __fastcall Data2Str(void * Buffer, int BufLen);
extern DELPHI_PACKAGE bool __fastcall Str2Data(System::UnicodeString S, void * &Buffer, int &BufLen);
extern DELPHI_PACKAGE bool __fastcall IsDigit(System::WideChar ch);
extern DELPHI_PACKAGE bool __fastcall IsDigitStr(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall IsAlpha(System::WideChar ch);
extern DELPHI_PACKAGE bool __fastcall IsAlphaOrDigit(System::WideChar ch);
extern DELPHI_PACKAGE bool __fastcall IsAlphaStr(const System::UnicodeString S);
extern DELPHI_PACKAGE bool __fastcall IsIdentStr(const System::UnicodeString S);
extern DELPHI_PACKAGE System::UnicodeString __fastcall ExtractStr(System::UnicodeString &S, int SPos, int SLen);
extern DELPHI_PACKAGE int __fastcall LeftBreak(System::UnicodeString S, int Pos);
extern DELPHI_PACKAGE System::UnicodeString __fastcall EscapeURLString(System::UnicodeString aString, System::WideChar EscapeChar);
extern DELPHI_PACKAGE System::UnicodeString __fastcall EscapeString(System::UnicodeString aString, System::UnicodeString UnsafeChars, System::WideChar EscapeChar);
extern DELPHI_PACKAGE System::UnicodeString __fastcall UnEscapeString(System::UnicodeString aString, System::WideChar EscapeChar);
extern DELPHI_PACKAGE bool __fastcall StrStartsWith(System::WideChar * Source, System::WideChar * Seq);
extern DELPHI_PACKAGE bool __fastcall ContainsAt(System::UnicodeString Source, int Index, System::UnicodeString Seq);
extern DELPHI_PACKAGE bool __fastcall FileNameLike(System::UnicodeString FileName, System::UnicodeString Mask);
extern DELPHI_PACKAGE bool __fastcall AnsiSameText(const System::UnicodeString S1, const System::UnicodeString S2);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CurrToPrettyStr(const System::Currency Value);
extern DELPHI_PACKAGE System::Currency __fastcall PrettyStrToCurr(const System::UnicodeString Value);
extern DELPHI_PACKAGE int __fastcall CurrSign(const System::Currency Value);
extern DELPHI_PACKAGE System::WideChar * __fastcall StringDup(System::UnicodeString S);
extern DELPHI_PACKAGE System::WideString __fastcall uni2uppers(System::WideString s);
extern DELPHI_PACKAGE System::WideString __fastcall uni2lowers(System::WideString s);
extern DELPHI_PACKAGE System::WideString __fastcall uni2upperf(System::WideString s);
extern DELPHI_PACKAGE System::WideString __fastcall uni2lowerf(System::WideString s);
extern DELPHI_PACKAGE System::UnicodeString __fastcall CreateUnicodeHintString(System::WideString Value);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStringDup(System::WideString S);
extern DELPHI_PACKAGE int __fastcall WidePos(const System::WideString Substr, const System::WideString S);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrScan(const System::WideChar * Str, System::WideChar Chr);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrRScan(const System::WideChar * Str, System::WideChar Chr);
extern DELPHI_PACKAGE System::WideString __fastcall WideQuotedStr(const System::WideString S, System::WideChar Quote);
extern DELPHI_PACKAGE System::WideString __fastcall WideExtractQuotedStr(System::WideChar * &Src, System::WideChar Quote);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrEnd(const System::WideChar * Str);
extern DELPHI_PACKAGE bool __fastcall WideSameText(const System::WideString S1, const System::WideString S2);
extern DELPHI_PACKAGE int __fastcall WideCompareText(const System::WideString S1, const System::WideString S2);
extern DELPHI_PACKAGE System::WideString __fastcall WideExtractStr(System::WideString &S, int SPos, int SLen);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrCopy(System::WideChar * Target, System::WideChar * Source);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrPCopy(System::WideChar * Target, const System::WideString Source);
extern DELPHI_PACKAGE int __fastcall WideStrComp(const System::WideChar * S1, const System::WideChar * S2);
extern DELPHI_PACKAGE int __fastcall WideStrLComp(const System::WideChar * Str1, const System::WideChar * Str2, unsigned MaxLen);
extern DELPHI_PACKAGE unsigned __fastcall WideStrLen(const System::WideChar * Str);
extern DELPHI_PACKAGE System::WideString __fastcall WideStrPas(const System::WideChar * Source);
extern DELPHI_PACKAGE void __fastcall WideMove(const void *Source, void *Dest, int Count);
extern DELPHI_PACKAGE void __fastcall FillWord(void *X, int Count, System::Word Value);
extern DELPHI_PACKAGE void __fastcall FillWideChar(void *X, int Count, System::WideChar Value);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrMove(System::WideChar * Dest, const System::WideChar * Source, unsigned Count);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrECopy(System::WideChar * Dest, const System::WideChar * Source);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrLCopy(System::WideChar * Dest, const System::WideChar * Source, unsigned MaxLen);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrLCat(System::WideChar * Dest, const System::WideChar * Source, unsigned MaxLen);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrCat(System::WideChar * Dest, const System::WideChar * Source);
extern DELPHI_PACKAGE void __fastcall SetWideString(System::WideString &S, System::WideChar * Buffer, int Len);
extern DELPHI_PACKAGE int __fastcall CompareWideStr(const System::WideString S1, const System::WideString S2);
extern DELPHI_PACKAGE bool __fastcall SameWideStr(const System::WideString S1, const System::WideString S2);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideLastChar(const System::WideString S);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrAlloc(unsigned Size);
extern DELPHI_PACKAGE unsigned __fastcall WideStrBufSize(const System::WideChar * Str);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrNew(const System::WideChar * Str);
extern DELPHI_PACKAGE void __fastcall WideStrDispose(System::WideChar * Str);
extern DELPHI_PACKAGE System::WideString __fastcall WideUpperCase(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall WideLowerCase(const System::WideString S);
extern DELPHI_PACKAGE bool __fastcall IsWideDelimiter(const System::WideString Delimiters, const System::WideString S, int Index);
extern DELPHI_PACKAGE bool __fastcall IsWidePathDelimiter(const System::WideString S, int Index);
extern DELPHI_PACKAGE System::WideString __fastcall IncludeWideTrailingDelimiter(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall ExcludeWideTrailingDelimiter(const System::WideString S);
extern DELPHI_PACKAGE System::WideString __fastcall GetWideCharRangeString(System::WideChar FirstChar, System::WideChar LastChar);
extern DELPHI_PACKAGE System::WideString __fastcall GetWideStringOf(System::WideChar Char, unsigned Len);
extern DELPHI_PACKAGE System::WideString __fastcall WideStringReplace(const System::WideString S, const System::WideString OldPattern, const System::WideString NewPattern, TWideReplaceFlags Flags);
extern DELPHI_PACKAGE bool __fastcall WideReplace(System::WideString &Str, System::WideString SourceString, System::WideString DestString);
extern DELPHI_PACKAGE System::WideChar * __fastcall WideStrPos(const System::WideChar * Str1, const System::WideChar * Str2);
extern DELPHI_PACKAGE System::WideString __fastcall WideCopy(System::WideString S, int SPos, int SLen);
extern DELPHI_PACKAGE void __fastcall WideInsert(System::WideString Text, System::WideString &S, int SPos);
extern DELPHI_PACKAGE void __fastcall WideDelete(System::WideString &S, int SPos, int SLen);
extern DELPHI_PACKAGE System::WideString __fastcall WideMakeString(int FLen, System::WideString Seq);
extern DELPHI_PACKAGE int __fastcall WideLastPos(System::WideString SubStr, System::WideString Strn);
extern DELPHI_PACKAGE void __fastcall TStrDelete(TElFString &S, int SPos, int SLen);
extern DELPHI_PACKAGE TElFString __fastcall TStrExtractStr(TElFString &S, int SPos, int SLen);
extern DELPHI_PACKAGE void __fastcall SetTStr(TElFString &S, PElFChar Buffer, int Len);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetCharRangeString(System::WideChar FirstChar, System::WideChar LastChar);
extern DELPHI_PACKAGE ConversionResult __fastcall ConvertUTF16toUTF8(System::WideString &source, unsigned sourcelen, System::UnicodeString &target, unsigned targetlen, ConversionFlags flags);
extern DELPHI_PACKAGE ConversionResult __fastcall ConvertUTF8toUTF16(System::UnicodeString &source, unsigned sourcelen, System::WideString &target, unsigned targetlen, ConversionFlags flags);
extern DELPHI_PACKAGE bool __fastcall isLegalUTF8Sequence(System::UnicodeString source, unsigned sourcelen);
}	/* namespace Elstrutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELSTRUTILS)
using namespace Elstrutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElstrutilsHPP
