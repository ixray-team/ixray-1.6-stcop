// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElStrToken.pas' rev: 35.00 (Windows)

#ifndef ElstrtokenHPP
#define ElstrtokenHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elstrtoken
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EElStrTokenizerError;
class DELPHICLASS TElStringTokenizer;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EElStrTokenizerError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EElStrTokenizerError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EElStrTokenizerError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EElStrTokenizerError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EElStrTokenizerError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EElStrTokenizerError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EElStrTokenizerError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EElStrTokenizerError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EElStrTokenizerError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElStrTokenizerError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElStrTokenizerError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElStrTokenizerError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElStrTokenizerError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EElStrTokenizerError() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElStringTokenizer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FPos;
	System::UnicodeString FSourceString;
	bool FReturnDelims;
	System::UnicodeString FDelimiters;
	bool FLastWasToken;
	bool FSkipEmptyTokens;
	void __fastcall SetSourceString(System::UnicodeString newValue);
	bool __fastcall IntHasMoreTokens();
	bool __fastcall IntNextToken(System::UnicodeString &AResult);
	
public:
	__fastcall TElStringTokenizer();
	__fastcall TElStringTokenizer(System::UnicodeString str);
	__fastcall TElStringTokenizer(System::UnicodeString str, System::UnicodeString Delim);
	__fastcall TElStringTokenizer(System::UnicodeString str, System::UnicodeString Delim, bool ReturnDelims);
	bool __fastcall HasMoreTokens();
	System::UnicodeString __fastcall NextToken();
	int __fastcall CountTokens();
	System::UnicodeString __fastcall NextTokenDelim(System::UnicodeString Delims);
	void __fastcall FindAll(System::Classes::TStrings* AStrings);
	
__published:
	__property System::UnicodeString SourceString = {read=FSourceString, write=SetSourceString};
	__property bool ReturnDelims = {read=FReturnDelims, write=FReturnDelims, nodefault};
	__property System::UnicodeString Delimiters = {read=FDelimiters, write=FDelimiters};
	__property bool SkipEmptyTokens = {read=FSkipEmptyTokens, write=FSkipEmptyTokens, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TElStringTokenizer() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elstrtoken */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELSTRTOKEN)
using namespace Elstrtoken;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElstrtokenHPP
