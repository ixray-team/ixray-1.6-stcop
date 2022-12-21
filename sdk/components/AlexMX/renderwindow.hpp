// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'RenderWindow.pas' rev: 34.00 (Windows)

#ifndef RenderwindowHPP
#define RenderwindowHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Dialogs.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Renderwindow
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TD3DWindow;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TD3DWindow : public Vcl::Controls::TCustomControl
{
	typedef Vcl::Controls::TCustomControl inherited;
	
private:
	System::Classes::TNotifyEvent FOnPaint;
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Message);
	
protected:
	Vcl::Extctrls::TPanel* FTBar;
	Vcl::Extctrls::TPanel* FBBar;
	Vcl::Extctrls::TPanel* FLBar;
	Vcl::Extctrls::TPanel* FRBar;
	HIDESBASE virtual void __fastcall Paint();
	System::Classes::TNotifyEvent FOnChangeFocus;
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	System::Uitypes::TColor FFocusedColor;
	System::Uitypes::TColor FUnfocusedColor;
	bool FDrawFocusRect;
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWindowHandle(const Vcl::Controls::TCreateParams &Params);
	void __fastcall ChangeFocus(bool p);
	__property ParentColor = {default=0};
	void __fastcall SetDrawFocusRect(bool Value);
	void __fastcall SetFocusedColor(System::Uitypes::TColor Value);
	void __fastcall SetUnfocusedColor(System::Uitypes::TColor Value);
	
public:
	__fastcall virtual TD3DWindow(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TD3DWindow();
	virtual void __fastcall DefaultHandler(void *Message);
	
__published:
	__property System::Uitypes::TColor FocusedColor = {read=FFocusedColor, write=SetFocusedColor, default=65535};
	__property System::Uitypes::TColor UnfocusedColor = {read=FUnfocusedColor, write=SetUnfocusedColor, default=8421504};
	__property bool DrawFocusRect = {read=FDrawFocusRect, write=SetDrawFocusRect, default=1};
	__property TabStop = {default=1};
	__property System::Classes::TNotifyEvent OnChangeFocus = {read=FOnChangeFocus, write=FOnChangeFocus};
	__property Align = {default=0};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property Color = {default=-16777211};
	__property Enabled = {default=1};
	__property Font;
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property Visible = {default=1};
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnResize;
	__property System::Classes::TNotifyEvent OnPaint = {read=FOnPaint, write=FOnPaint};
public:
	/* TWinControl.CreateParented */ inline __fastcall TD3DWindow(HWND ParentWindow) : Vcl::Controls::TCustomControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define RWStyle (System::Set<Vcl::Controls::Vcl_Controls__31, Vcl::Controls::Vcl_Controls__31::csAcceptsControls, Vcl::Controls::Vcl_Controls__31::csOverrideStylePaint>() << Vcl::Controls::Vcl_Controls__31::csAcceptsControls << Vcl::Controls::Vcl_Controls__31::csCaptureMouse << Vcl::Controls::Vcl_Controls__31::csClickEvents << Vcl::Controls::Vcl_Controls__31::csOpaque << Vcl::Controls::Vcl_Controls__31::csReplicatable )
}	/* namespace Renderwindow */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_RENDERWINDOW)
using namespace Renderwindow;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// RenderwindowHPP
