// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ExtBtn.pas' rev: 34.00 (Windows)

#ifndef ExtbtnHPP
#define ExtbtnHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Winapi.CommCtrl.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Extbtn
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TExtBtn;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TButtonLayout : unsigned char { blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom };

enum DECLSPEC_DENUM TButtonState : unsigned char { bsUp, bsDisabled, bsDown, bsExclusive };

enum DECLSPEC_DENUM TButtonStyle : unsigned char { bsAutoDetect, bsWin31, bsNew };

enum DECLSPEC_DENUM TKind : unsigned char { knClose, knMinimize, knNone };

typedef System::Int8 TNumGlyphs;

class PASCALIMPLEMENTATION TExtBtn : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
private:
	int FGroupIndex;
	void *FGlyph;
	void *FExtGlyph;
	bool FDown;
	bool FDragging;
	bool FAllowAllUp;
	TButtonLayout FLayout;
	int FSpacing;
	bool FTransparent;
	int FMargin;
	int FExtMargin;
	bool FFlat;
	bool FMouseInControl;
	bool FExtButton;
	int FExtWidth;
	bool FExtTransparent;
	System::Classes::TNotifyEvent FOnExtBtnClick;
	TKind FKind;
	bool FBevelShow;
	bool FThinBorder;
	bool FHotTrack;
	System::Uitypes::TColor FNormalColor;
	System::Uitypes::TColor FHotColor;
	System::Uitypes::TColor FDownColor;
	System::Uitypes::TColor FBtnColor;
	bool FNeedManualMouseUp;
	void __fastcall GlyphChanged(System::TObject* Sender);
	void __fastcall UpdateExclusive();
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	TNumGlyphs __fastcall GetNumGlyphs();
	void __fastcall SetNumGlyphs(TNumGlyphs Value);
	void __fastcall SetDown(bool Value);
	void __fastcall SetHotTrack(bool Value);
	void __fastcall SetFlat(bool Value);
	void __fastcall SetAllowAllUp(bool Value);
	void __fastcall SetGroupIndex(int Value);
	void __fastcall SetLayout(TButtonLayout Value);
	void __fastcall SetSpacing(int Value);
	void __fastcall SetTransparent(bool Value);
	void __fastcall SetThinBorder(bool Value);
	void __fastcall SetMargin(int Value);
	void __fastcall SetExtMargin(int Value);
	void __fastcall SetExtButton(bool Value);
	void __fastcall SetBevelShow(bool Value);
	void __fastcall SetExtWidth(int Value);
	void __fastcall SetExtTransparent(bool Value);
	void __fastcall SetKind(TKind Value);
	void __fastcall SetBtnColor(System::Uitypes::TColor Value);
	void __fastcall SetNormalColor(System::Uitypes::TColor Value);
	void __fastcall SetDownColor(System::Uitypes::TColor Value);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMButtonPressed(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMDialogChar(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Message);
	
protected:
	TButtonState FState;
	TButtonState FExtState;
	DYNAMIC void __fastcall ActionChange(System::TObject* Sender, bool CheckDefaults);
	DYNAMIC HPALETTE __fastcall GetPalette();
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Paint();
	__property bool MouseInControl = {read=FMouseInControl, nodefault};
	void __fastcall UpdateTracking();
	
public:
	void __fastcall MouseManualUp();
	__fastcall virtual TExtBtn(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TExtBtn();
	DYNAMIC void __fastcall Click();
	void __fastcall UpdateMouseInControl();
	
__published:
	__property Align = {default=3};
	__property Action;
	__property bool AllowAllUp = {read=FAllowAllUp, write=SetAllowAllUp, default=0};
	__property Anchors = {default=3};
	__property BiDiMode;
	__property Constraints;
	__property bool BevelShow = {read=FBevelShow, write=SetBevelShow, default=1};
	__property bool HotTrack = {read=FHotTrack, write=SetHotTrack, default=0};
	__property System::Uitypes::TColor HotColor = {read=FHotColor, write=FHotColor, default=16711680};
	__property System::Uitypes::TColor NormalColor = {read=FNormalColor, write=SetNormalColor, default=0};
	__property System::Uitypes::TColor BtnColor = {read=FBtnColor, write=SetBtnColor, default=-16777201};
	__property System::Uitypes::TColor DownColor = {read=FDownColor, write=SetDownColor, default=-16777196};
	__property bool ExtButton = {read=FExtButton, write=SetExtButton, default=0};
	__property int ExtWidth = {read=FExtWidth, write=SetExtWidth, default=14};
	__property bool ExtTransparent = {read=FExtTransparent, write=SetExtTransparent, default=0};
	__property int GroupIndex = {read=FGroupIndex, write=SetGroupIndex, default=0};
	__property bool Down = {read=FDown, write=SetDown, default=0};
	__property Caption = {default=0};
	__property Enabled = {default=1};
	__property bool Flat = {read=FFlat, write=SetFlat, default=1};
	__property Font;
	__property Vcl::Graphics::TBitmap* Glyph = {read=GetGlyph, write=SetGlyph};
	__property TKind Kind = {read=FKind, write=SetKind, default=0};
	__property TButtonLayout Layout = {read=FLayout, write=SetLayout, default=0};
	__property int Margin = {read=FMargin, write=SetMargin, default=-1};
	__property int ExtMargin = {read=FExtMargin, write=SetExtMargin, default=-1};
	__property TNumGlyphs NumGlyphs = {read=GetNumGlyphs, write=SetNumGlyphs, default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ParentBiDiMode = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property int Spacing = {read=FSpacing, write=SetSpacing, default=4};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=1};
	__property bool FlatAlwaysEdge = {read=FThinBorder, write=SetThinBorder, default=0};
	__property Visible = {default=1};
	__property System::Classes::TNotifyEvent OnExtBtnClick = {read=FOnExtBtnClick, write=FOnExtBtnClick};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnStartDrag;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::Classes::TList* ExtButtonList;
}	/* namespace Extbtn */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_EXTBTN)
using namespace Extbtn;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ExtbtnHPP
