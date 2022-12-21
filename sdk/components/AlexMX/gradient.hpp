// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Gradient.pas' rev: 34.00 (Windows)

#ifndef GradientHPP
#define GradientHPP

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
#include <Vcl.ExtCtrls.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gradient
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TGradient;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TFillDirection : unsigned char { fdLeftToRight, fdRightToLeft, fdUpToBottom, fdBottomToUp };

typedef System::Byte TNumberOfColors;

class PASCALIMPLEMENTATION TGradient : public Vcl::Extctrls::TCustomPanel
{
	typedef Vcl::Extctrls::TCustomPanel inherited;
	
private:
	TFillDirection FDirection;
	System::Uitypes::TColor FBeginColor;
	System::Uitypes::TColor FEndColor;
	bool FAutoSize;
	TNumberOfColors FNumberOfColors;
	Vcl::Graphics::TFont* FFont;
	System::UnicodeString FCaption;
	int FTextTop;
	int FTextLeft;
	bool FBorder;
	int FBorderWidth;
	System::Uitypes::TColor FBorderColor;
	void __fastcall SetFillDirection(TFillDirection Value);
	HIDESBASE void __fastcall SetAutoSize(bool Value);
	void __fastcall SetBeginColor(System::Uitypes::TColor Value);
	void __fastcall SetEndColor(System::Uitypes::TColor Value);
	void __fastcall SetNumberOfColors(TNumberOfColors Value);
	HIDESBASE void __fastcall SetFont(Vcl::Graphics::TFont* AFont);
	void __fastcall SetCaption(System::UnicodeString Value);
	void __fastcall SetTextTop(int Value);
	void __fastcall SetTextLeft(int Value);
	void __fastcall SetBorder(bool Value);
	HIDESBASE void __fastcall SetBorderWidth(int Value);
	void __fastcall SetBorderColor(System::Uitypes::TColor Value);
	void __fastcall GradientFill();
	
protected:
	System::Classes::TNotifyEvent FOnPaint;
	virtual void __fastcall Paint();
	
public:
	__property Canvas;
	__fastcall virtual TGradient(System::Classes::TComponent* AOwner);
	
__published:
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, default=0};
	__property System::Uitypes::TColor BeginColor = {read=FBeginColor, write=SetBeginColor, default=16711680};
	__property System::Uitypes::TColor EndColor = {read=FEndColor, write=SetEndColor, default=0};
	__property TFillDirection FillDirection = {read=FDirection, write=SetFillDirection, default=0};
	__property TNumberOfColors NumberOfColors = {read=FNumberOfColors, write=SetNumberOfColors, default=255};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property System::UnicodeString Caption = {read=FCaption, write=SetCaption};
	__property int TextTop = {read=FTextTop, write=SetTextTop, nodefault};
	__property int TextLeft = {read=FTextLeft, write=SetTextLeft, nodefault};
	__property bool Border = {read=FBorder, write=SetBorder, nodefault};
	__property int BorderWidth = {read=FBorderWidth, write=SetBorderWidth, nodefault};
	__property System::Uitypes::TColor BorderColor = {read=FBorderColor, write=SetBorderColor, nodefault};
	__property Color = {default=-16777201};
	__property Align = {default=0};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property BorderStyle = {default=0};
	__property System::Classes::TNotifyEvent OnPaint = {read=FOnPaint, write=FOnPaint};
public:
	/* TCustomControl.Destroy */ inline __fastcall virtual ~TGradient() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TGradient(HWND ParentWindow) : Vcl::Extctrls::TCustomPanel(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Gradient */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GRADIENT)
using namespace Gradient;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GradientHPP
