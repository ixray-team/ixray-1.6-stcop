// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElFlatCtl.pas' rev: 35.00 (Windows)

#ifndef ElflatctlHPP
#define ElflatctlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <ElVCLUtils.hpp>
#include <Vcl.StdCtrls.hpp>
#include <System.TypInfo.hpp>
#include <ElTools.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElHook.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elflatctl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElFlatController;
class DELPHICLASS TElFlatEntry;
class DELPHICLASS TElFlatEntries;
class DELPHICLASS TElFlatMultiController;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElFlatController : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	Elhook::TElHook* FHook;
	bool FMouseOver;
	bool FFlatFocusedScrollbars;
	Elvclutils::TElBorderSides FBorderSides;
	NativeUInt FTheme;
	void __fastcall SetFlatFocusedScrollbars(bool newValue);
	void __fastcall HookAfterProcessHandler(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall SetActive(bool newValue);
	bool __fastcall GetActive();
	void __fastcall SetControl(Vcl::Controls::TWinControl* newValue);
	Vcl::Controls::TWinControl* __fastcall GetControl();
	void __fastcall SetDesignActive(bool newValue);
	bool __fastcall GetDesignActive();
	void __fastcall SetActiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetInactiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	void __fastcall HookBeforeProcessHandler(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	
protected:
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	bool FUseXPThemes;
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	bool __fastcall IsControlThemed(Vcl::Controls::TControl* Control);
	void __fastcall SetUseXPThemes(bool Value);
	void __fastcall CreateThemeHandle();
	void __fastcall FreeThemeHandle();
	virtual void __fastcall Loaded();
	void __fastcall DoNCPaint();
	
public:
	void __fastcall DrawFlatBorder();
	void __fastcall UpdateFrame();
	__fastcall virtual TElFlatController(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElFlatController();
	bool __fastcall IsThemeApplied();
	
__published:
	__property bool FlatFocusedScrollbars = {read=FFlatFocusedScrollbars, write=SetFlatFocusedScrollbars, nodefault};
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	__property Vcl::Controls::TWinControl* Control = {read=GetControl, write=SetControl};
	__property bool DesignActive = {read=GetDesignActive, write=SetDesignActive, default=1};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElFlatEntry : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	TElFlatController* FController;
	bool FDesignActive;
	Vcl::Controls::TWinControl* FControl;
	bool FActive;
	bool FFlatFocusedScrollbars;
	bool __fastcall GetActive();
	void __fastcall SetActive(bool newValue);
	bool __fastcall GetFlatFocusedScrollbars();
	void __fastcall SetFlatFocusedScrollbars(bool newValue);
	bool __fastcall GetDesignActive();
	void __fastcall SetDesignActive(bool newValue);
	void __fastcall SetControl(Vcl::Controls::TWinControl* newValue);
	Elvclutils::TElFlatBorderType __fastcall GetActiveBorderType();
	void __fastcall SetActiveBorderType(Elvclutils::TElFlatBorderType newValue);
	Elvclutils::TElFlatBorderType __fastcall GetInactiveBorderType();
	void __fastcall SetInactiveBorderType(Elvclutils::TElFlatBorderType newValue);
	
protected:
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	Elvclutils::TElBorderSides FBorderSides;
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	bool __fastcall GetUseXPThemes();
	void __fastcall SetUseXPThemes(bool Value);
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TElFlatEntry(System::Classes::TCollection* Collection);
	__fastcall virtual ~TElFlatEntry();
	
__published:
	__property bool Active = {read=GetActive, write=SetActive, nodefault};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property bool FlatFocusedScrollbars = {read=GetFlatFocusedScrollbars, write=SetFlatFocusedScrollbars, nodefault};
	__property bool DesignActive = {read=GetDesignActive, write=SetDesignActive, nodefault};
	__property Vcl::Controls::TWinControl* Control = {read=FControl, write=SetControl};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=GetActiveBorderType, write=SetActiveBorderType, nodefault};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=GetInactiveBorderType, write=SetInactiveBorderType, nodefault};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property bool UseXPThemes = {read=GetUseXPThemes, write=SetUseXPThemes, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElFlatEntries : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TElFlatEntry* operator[](int index) { return this->Entries[index]; }
	
private:
	TElFlatMultiController* FController;
	TElFlatEntry* __fastcall GetEntries(int index);
	void __fastcall SetEntries(int index, TElFlatEntry* newValue);
	
public:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	HIDESBASE TElFlatEntry* __fastcall Add();
	__property TElFlatEntry* Entries[int index] = {read=GetEntries, write=SetEntries/*, default*/};
public:
	/* TCollection.Create */ inline __fastcall TElFlatEntries(System::Classes::TCollectionItemClass ItemClass) : System::Classes::TCollection(ItemClass) { }
	/* TCollection.Destroy */ inline __fastcall virtual ~TElFlatEntries() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElFlatMultiController : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FFlatFocusedScrollbars;
	bool FAutoAddControls;
	Elhook::TElHook* FHook;
	TElFlatEntries* FEntries;
	Elvclutils::TElBorderSides FBorderSides;
	MESSAGE void __fastcall CMControlListChange(Winapi::Messages::TMessage &Msg);
	void __fastcall SetEntries(TElFlatEntries* newValue);
	void __fastcall SetActive(bool newValue);
	bool __fastcall GetActive();
	void __fastcall SetDesignActive(bool newValue);
	bool __fastcall GetDesignActive();
	void __fastcall AfterProcessHandler(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall SetAutoAddControls(bool newValue);
	void __fastcall ScanForm();
	void __fastcall SetFlatFocusedScrollbars(bool newValue);
	void __fastcall SetActiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetInactiveBorderType(Elvclutils::TElFlatBorderType newValue);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	
protected:
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	bool FUseXPThemes;
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	virtual void __fastcall Loaded();
	void __fastcall SetUseXPThemes(bool Value);
	
public:
	__fastcall virtual TElFlatMultiController(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElFlatMultiController();
	
__published:
	__property TElFlatEntries* Entries = {read=FEntries, write=SetEntries};
	__property bool Active = {read=GetActive, write=SetActive, default=1};
	__property bool DesignActive = {read=GetDesignActive, write=SetDesignActive, default=1};
	__property bool AutoAddControls = {read=FAutoAddControls, write=SetAutoAddControls, default=1};
	__property bool FlatFocusedScrollbars = {read=FFlatFocusedScrollbars, write=SetFlatFocusedScrollbars, nodefault};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elflatctl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELFLATCTL)
using namespace Elflatctl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElflatctlHPP
