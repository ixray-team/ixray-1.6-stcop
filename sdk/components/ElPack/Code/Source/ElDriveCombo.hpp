// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElDriveCombo.pas' rev: 34.00 (Windows)

#ifndef EldrivecomboHPP
#define EldrivecomboHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Controls.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Winapi.ShellAPI.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElTools.hpp>
#include <ElVCLUtils.hpp>
#include <ElACtrls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eldrivecombo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElDriveComboBox;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElDriveComboOption : unsigned char { dcoDisplayFloppy, dcoDisplayNetwork, dcoDisplayHard, dcoDisplayCD, dcoDisplayRAM };

typedef System::Set<TElDriveComboOption, TElDriveComboOption::dcoDisplayFloppy, TElDriveComboOption::dcoDisplayRAM> TElDriveComboOptions;

class PASCALIMPLEMENTATION TElDriveComboBox : public Elactrls::TElAdvancedComboBox
{
	typedef Elactrls::TElAdvancedComboBox inherited;
	
private:
	int FDummyInt;
	System::WideChar FDummyChar;
	
protected:
	System::WideChar FDrive;
	Elvclutils::TElTextCase FTextCase;
	TElDriveComboOptions FOptions;
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	void __fastcall FillItems();
	void __fastcall SetDrive(System::WideChar Value);
	virtual void __fastcall CreateWnd();
	void __fastcall SetTextCase(Elvclutils::TElTextCase Value);
	void __fastcall SetOptions(TElDriveComboOptions Value);
	DYNAMIC void __fastcall Change();
	
public:
	__fastcall virtual TElDriveComboBox(System::Classes::TComponent* AOwner);
	__property System::WideChar Drive = {read=FDrive, write=SetDrive, nodefault};
	__property System::WideChar ItemIndex = {read=FDummyChar, nodefault};
	
__published:
	__property int Items = {read=FDummyInt, nodefault};
	__property int Style = {read=FDummyInt, nodefault};
	__property int ItemHeight = {read=FDummyInt, nodefault};
	__property Elvclutils::TElTextCase TextCase = {read=FTextCase, write=SetTextCase, default=2};
	__property TElDriveComboOptions Options = {read=FOptions, write=SetOptions, nodefault};
public:
	/* TElAdvancedComboBox.Destroy */ inline __fastcall virtual ~TElDriveComboBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDriveComboBox(HWND ParentWindow) : Elactrls::TElAdvancedComboBox(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eldrivecombo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELDRIVECOMBO)
using namespace Eldrivecombo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EldrivecomboHPP
