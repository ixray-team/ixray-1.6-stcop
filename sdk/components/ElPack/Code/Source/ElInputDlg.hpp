// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElInputDlg.pas' rev: 34.00 (Windows)

#ifndef ElinputdlgHPP
#define ElinputdlgHPP

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
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Consts.hpp>
#include <ElBtnCtl.hpp>
#include <ElCheckCtl.hpp>
#include <ElHTMLLbl.hpp>
#include <ElACtrls.hpp>
#include <ElPanel.hpp>
#include <ElEdits.hpp>
#include <HTMLRender.hpp>
#include <ElVCLUtils.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElStrUtils.hpp>
#include <ElCaption.hpp>
#include <ElPopBtn.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elinputdlg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElInputDialog;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElInputDialog : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	
protected:
	Elstrutils::TElFString FPrompt;
	Elstrutils::TElFString FCaption;
	bool FIsHTML;
	Elstrutils::TElFString FValue;
	Vcl::Graphics::TFont* FFont;
	bool FParentFont;
	Vcl::Forms::TPosition FPosition;
	void __fastcall SetFont(Vcl::Graphics::TFont* Value);
	void __fastcall SetParentFont(bool Value);
	void __fastcall FontChange(System::TObject* Sender);
	
public:
	bool __fastcall Execute();
	__fastcall virtual TElInputDialog(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElInputDialog();
	
__published:
	__property Elstrutils::TElFString Prompt = {read=FPrompt, write=FPrompt};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=FCaption};
	__property bool IsHTML = {read=FIsHTML, write=FIsHTML, nodefault};
	__property Vcl::Forms::TPosition Position = {read=FPosition, write=FPosition, nodefault};
	__property Elstrutils::TElFString Value = {read=FValue, write=FValue};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property bool ParentFont = {read=FParentFont, write=SetParentFont, default=1};
};


typedef System::TMetaClass* TInputDlgClass;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TInputDlgClass InputDlgClass;
extern DELPHI_PACKAGE bool __fastcall InputQuery(const Elstrutils::TElFString ACaption, const Elstrutils::TElFString APrompt, Elstrutils::TElFString &AValue, bool AIsHTML);
}	/* namespace Elinputdlg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELINPUTDLG)
using namespace Elinputdlg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElinputdlgHPP
