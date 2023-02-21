// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElHotKey.pas' rev: 35.00 (Windows)

#ifndef ElhotkeyHPP
#define ElhotkeyHPP

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
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>
#include <ElXPThemedControl.hpp>
#include <ElEdits.hpp>
#include <ElStrUtils.hpp>
#include <ElVCLUtils.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elhotkey
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElHotKey;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TElHKInvalidKey : unsigned char { hcShift, hcAlt, hcCtrl, hcAltShift, hcAltCtrl, hcCtrlShift, hcCtrlAlftShift };

typedef System::Set<TElHKInvalidKey, TElHKInvalidKey::hcShift, TElHKInvalidKey::hcCtrlAlftShift> TElHKInvalidKeys;

enum DECLSPEC_DENUM TElHKModifier : unsigned char { hkShift, hkAlt, hkCtrl };

typedef System::Set<TElHKModifier, TElHKModifier::hkShift, TElHKModifier::hkCtrl> TElHKModifiers;

class PASCALIMPLEMENTATION TElHotKey : public Eledits::TCustomElEdit
{
	typedef Eledits::TCustomElEdit inherited;
	
private:
	Elstrutils::TElFString FText;
	bool FKeyPressed;
	TElHKInvalidKeys FInvalidKeys;
	TElHKModifiers FModifiers;
	System::Classes::TShiftState FShiftState;
	void __fastcall SetShortCut(System::Classes::TShortCut newValue);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyUp(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMSysKeyDown(Winapi::Messages::TWMKey &Message);
	void __fastcall SetInvalidKeys(TElHKInvalidKeys Value);
	void __fastcall SetModifiers(TElHKModifiers Value);
	Elstrutils::TElFString __fastcall ShiftStateToText(System::Classes::TShiftState state);
	System::Classes::TShortCut __fastcall GetShortCut();
	System::Classes::TShiftState __fastcall GetShiftState(System::Classes::TShiftState state);
	
protected:
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	
public:
	__fastcall virtual TElHotKey(System::Classes::TComponent* AOwner);
	
__published:
	__property System::Classes::TShortCut HotKey = {read=GetShortCut, write=SetShortCut, nodefault};
	__property TElHKInvalidKeys InvalidKeys = {read=FInvalidKeys, write=SetInvalidKeys, nodefault};
	__property TElHKModifiers Modifiers = {read=FModifiers, write=SetModifiers, nodefault};
	__property AutoSize = {default=1};
	__property Alignment;
	__property Background;
	__property BorderSides;
	__property UseBackground = {default=0};
	__property RTLContent;
	__property Transparent;
	__property ReadOnly = {default=0};
	__property LeftMargin = {default=1};
	__property RightMargin = {default=2};
	__property TopMargin = {default=1};
	__property BorderStyle;
	__property HideSelection = {default=1};
	__property ActiveBorderType = {default=1};
	__property Flat = {default=0};
	__property InactiveBorderType = {default=3};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property ChangeDisabledText = {default=0};
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property OnChange;
	__property Align = {default=0};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=1};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
public:
	/* TCustomElEdit.Destroy */ inline __fastcall virtual ~TElHotKey() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHotKey(HWND ParentWindow) : Eledits::TCustomElEdit(ParentWindow) { }
	
};


typedef System::StaticArray<System::UnicodeString, 7> Elhotkey__2;

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::ResourceString _rsShiftP;
#define Elhotkey_rsShiftP System::LoadResourceString(&Elhotkey::_rsShiftP)
extern DELPHI_PACKAGE System::ResourceString _rsAltP;
#define Elhotkey_rsAltP System::LoadResourceString(&Elhotkey::_rsAltP)
extern DELPHI_PACKAGE System::ResourceString _rsCtrlP;
#define Elhotkey_rsCtrlP System::LoadResourceString(&Elhotkey::_rsCtrlP)
extern DELPHI_PACKAGE System::ResourceString _rsLeftP;
#define Elhotkey_rsLeftP System::LoadResourceString(&Elhotkey::_rsLeftP)
extern DELPHI_PACKAGE System::ResourceString _rsRightP;
#define Elhotkey_rsRightP System::LoadResourceString(&Elhotkey::_rsRightP)
extern DELPHI_PACKAGE System::ResourceString _rsMiddleP;
#define Elhotkey_rsMiddleP System::LoadResourceString(&Elhotkey::_rsMiddleP)
extern DELPHI_PACKAGE System::ResourceString _rsDoubleP;
#define Elhotkey_rsDoubleP System::LoadResourceString(&Elhotkey::_rsDoubleP)
extern DELPHI_PACKAGE Elhotkey__2 nshift;
}	/* namespace Elhotkey */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELHOTKEY)
using namespace Elhotkey;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElhotkeyHPP
