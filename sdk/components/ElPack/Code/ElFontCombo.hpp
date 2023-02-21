// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElFontCombo.pas' rev: 35.00 (Windows)

#ifndef ElfontcomboHPP
#define ElfontcomboHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Printers.hpp>
#include <Vcl.StdCtrls.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElTools.hpp>
#include <ElStrUtils.hpp>
#include <ElVCLUtils.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElACtrls.hpp>
#include <Vcl.Controls.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elfontcombo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElFontComboBox;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElFontSampleMode : unsigned char { fsmFontName, fsmFontSample, fsmBoth, fsmNoSample };

enum DECLSPEC_DENUM TElFontDevice : unsigned char { efdScreen, efdPrinter, efdBoth };

enum DECLSPEC_DENUM TElFontComboOption : unsigned char { foAnsiOnly, foTrueTypeOnly, foIncludeOEMFonts, foIncludeSymbolFonts, foOEMFontsOnly, foScalableOnly };

typedef System::Set<TElFontComboOption, TElFontComboOption::foAnsiOnly, TElFontComboOption::foScalableOnly> TElFontComboOptions;

class PASCALIMPLEMENTATION TElFontComboBox : public Elactrls::TElAdvancedComboBox
{
	typedef Elactrls::TElAdvancedComboBox inherited;
	
protected:
	TElFontComboOptions FOptions;
	Elstrutils::TElFString FSampleText;
	System::Uitypes::TFontPitch FFontPitch;
	TElFontSampleMode FSampleMode;
	TElFontDevice FFontDevice;
	System::UnicodeString FFontName;
	int FFakeInt;
	void __fastcall SetFontName(System::Uitypes::TFontName Value);
	void __fastcall SetOptions(TElFontComboOptions Value);
	void __fastcall SetSampleText(Elstrutils::TElFString Value);
	void __fastcall SetFontPitch(System::Uitypes::TFontPitch Value);
	void __fastcall SetSampleMode(TElFontSampleMode Value);
	virtual void __fastcall CreateWnd();
	Elstrutils::TElFString __fastcall GetItemText(int index);
	virtual int __fastcall GetItemWidth(int Index);
	virtual void __fastcall MeasureItem(int Index, int &Height);
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	void __fastcall SetFontDevice(TElFontDevice Value);
	void __fastcall AddFont(System::Uitypes::TFontName Font, int FontType);
	System::Uitypes::TFontName __fastcall GetFontName();
	virtual void __fastcall Loaded();
	
public:
	void __fastcall RebuildFontList();
	__fastcall virtual TElFontComboBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElFontComboBox();
	
__published:
	__property int Items = {read=FFakeInt, nodefault};
	__property int Style = {read=FFakeInt, nodefault};
	__property int Text = {read=FFakeInt, nodefault};
	__property System::Uitypes::TFontName FontName = {read=GetFontName, write=SetFontName};
	__property TElFontComboOptions Options = {read=FOptions, write=SetOptions, nodefault};
	__property Elstrutils::TElFString SampleText = {read=FSampleText, write=SetSampleText};
	__property System::Uitypes::TFontPitch FontPitch = {read=FFontPitch, write=SetFontPitch, nodefault};
	__property TElFontSampleMode SampleMode = {read=FSampleMode, write=SetSampleMode, nodefault};
	__property TElFontDevice FontDevice = {read=FFontDevice, write=SetFontDevice, nodefault};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElFontComboBox(HWND ParentWindow) : Elactrls::TElAdvancedComboBox(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elfontcombo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELFONTCOMBO)
using namespace Elfontcombo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElfontcomboHPP
