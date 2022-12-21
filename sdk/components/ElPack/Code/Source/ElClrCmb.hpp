// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElClrCmb.pas' rev: 34.00 (Windows)

#ifndef ElclrcmbHPP
#define ElclrcmbHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Forms.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElStrUtils.hpp>
#include <ElTools.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElACtrls.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elclrcmb
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElColorCombo;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TColorComboOption : unsigned char { ccoNoColor, cco4BitColors, ccoSystemColors, ccoCustomChoice, ccoShowNames };

typedef System::Set<TColorComboOption, TColorComboOption::ccoNoColor, TColorComboOption::ccoShowNames> TColorComboOptions;

typedef void __fastcall (__closure *TTranslateColorNameEvent)(System::TObject* Sender, System::Uitypes::TColor Color, System::UnicodeString &ColorName);

typedef void __fastcall (__closure *TColorComboAddMoreColorsEvent)(System::TObject* Sender, System::Classes::TStrings* Items);

class PASCALIMPLEMENTATION TElColorCombo : public Elactrls::TElAdvancedComboBox
{
	typedef Elactrls::TElAdvancedComboBox inherited;
	
private:
	bool FDown;
	bool FMouseInControl;
	Vcl::Dialogs::TColorDialogOptions FDialogOptions;
	System::Uitypes::TColor FSelectedColor;
	TColorComboOptions FOptions;
	TTranslateColorNameEvent FOnTranslateColorName;
	TColorComboAddMoreColorsEvent FOnAddMoreColors;
	int FDummyInt;
	bool FDummyBool;
	Vcl::Stdctrls::TComboBoxStyle FDummyStyle;
	bool FInDialog;
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	void __fastcall SetOptions(TColorComboOptions Value);
	
protected:
	void __fastcall SetSelectedColor(System::Uitypes::TColor aColor);
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	void __fastcall FillItems();
	DYNAMIC void __fastcall Change();
	virtual void __fastcall TriggerTranslateColorName(System::Uitypes::TColor Color, System::UnicodeString &ColorName);
	virtual void __fastcall Loaded();
	virtual void __fastcall DoAddMoreColors(System::Classes::TStrings* Items);
	
public:
	__fastcall virtual TElColorCombo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElColorCombo();
	
__published:
	__property int Items = {read=FDummyInt, nodefault};
	__property int ItemHeight = {read=FDummyInt, write=FDummyInt, nodefault};
	__property int ItemIndex = {read=FDummyInt, write=FDummyInt, nodefault};
	__property Vcl::Stdctrls::TComboBoxStyle Style = {read=FDummyStyle, write=FDummyStyle, nodefault};
	__property bool Transparent = {read=FDummyBool, write=FDummyBool, nodefault};
	__property TColorComboOptions Options = {read=FOptions, write=SetOptions, nodefault};
	__property Vcl::Dialogs::TColorDialogOptions DialogOptions = {read=FDialogOptions, write=FDialogOptions, default=1};
	__property System::Uitypes::TColor SelectedColor = {read=FSelectedColor, write=SetSelectedColor, default=65280};
	__property TTranslateColorNameEvent OnTranslateColorName = {read=FOnTranslateColorName, write=FOnTranslateColorName};
	__property Enabled = {default=1};
	__property Width;
	__property Height;
	__property TabStop = {default=1};
	__property TabOrder = {default=-1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property OnClick;
	__property OnKeyDown;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnEnter;
	__property OnExit;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElColorCombo(HWND ParentWindow) : Elactrls::TElAdvancedComboBox(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elclrcmb */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCLRCMB)
using namespace Elclrcmb;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElclrcmbHPP
