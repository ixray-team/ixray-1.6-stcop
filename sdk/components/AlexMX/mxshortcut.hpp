// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MxShortcut.pas' rev: 34.00 (Windows)

#ifndef MxshortcutHPP
#define MxshortcutHPP

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
#include <System.Win.ComObj.hpp>
#include <Winapi.ShlObj.hpp>
#include <Winapi.ShellAPI.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Mask.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Consts.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxshortcut
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMxHotKey;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TMxHotKey : public Vcl::Stdctrls::TCustomEdit
{
	typedef Vcl::Stdctrls::TCustomEdit inherited;
	
private:
	System::Classes::TShortCut FHotKey;
	
protected:
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyUp(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	void __fastcall SetHotKey(System::Classes::TShortCut Value);
	
public:
	__fastcall virtual TMxHotKey(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMxHotKey();
	
__published:
	__property System::Classes::TShortCut HotKey = {read=FHotKey, write=SetHotKey, nodefault};
	__property Anchors = {default=3};
	__property AutoSelect = {default=1};
	__property AutoSize = {default=1};
	__property BevelEdges = {default=15};
	__property BevelInner = {index=0, default=2};
	__property BevelOuter = {index=1, default=1};
	__property BevelKind = {default=0};
	__property BevelWidth = {default=1};
	__property BiDiMode;
	__property BorderStyle = {default=1};
	__property CharCase = {default=0};
	__property Color = {default=-16777211};
	__property Constraints;
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragKind = {default=0};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property ParentBiDiMode = {default=1};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property Visible = {default=1};
	__property OnChange;
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDock;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnStartDock;
	__property OnStartDrag;
public:
	/* TWinControl.CreateParented */ inline __fastcall TMxHotKey(HWND ParentWindow) : Vcl::Stdctrls::TCustomEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::Classes::TShortCut __fastcall MxMakeShortCut(System::Word Key, System::Classes::TShiftState Shift);
extern DELPHI_PACKAGE System::UnicodeString __fastcall MxShortCutToText(System::Classes::TShortCut ShortCut);
}	/* namespace Mxshortcut */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXSHORTCUT)
using namespace Mxshortcut;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxshortcutHPP
