// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MxMenus.pas' rev: 34.00 (Windows)

#ifndef MxmenusHPP
#define MxmenusHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.ImgList.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>
#include <mxHook.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Mxmenus
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMxPopupMenu;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TMxMenuStyle : unsigned char { msStandard, msOwnerDraw, msBtnLowered, msBtnRaised };

enum DECLSPEC_DENUM Mxmenus__1 : unsigned char { mdSelected, mdGrayed, mdDisabled, mdChecked, mdFocused, mdDefault };

typedef System::Set<Mxmenus__1, Mxmenus__1::mdSelected, Mxmenus__1::mdDefault> TMenuOwnerDrawState;

typedef void __fastcall (__closure *TDrawMenuItemEvent)(Vcl::Menus::TMenu* Sender, Vcl::Menus::TMenuItem* Item, const System::Types::TRect &Rect, TMenuOwnerDrawState State);

typedef void __fastcall (__closure *TMeasureMenuItemEvent)(Vcl::Menus::TMenu* Sender, Vcl::Menus::TMenuItem* Item, int &Width, int &Height);

typedef void __fastcall (__closure *TDrawMarginEvent)(Vcl::Menus::TMenu* Sender, const System::Types::TRect &Rect);

typedef void __fastcall (__closure *TItemParamsEvent)(Vcl::Menus::TMenu* Sender, Vcl::Menus::TMenuItem* Item, TMenuOwnerDrawState State, Vcl::Graphics::TFont* AFont, System::Uitypes::TColor &Color, Vcl::Graphics::TGraphic* &Graphic, int &NumGlyphs);

typedef void __fastcall (__closure *TItemImageEvent)(Vcl::Menus::TMenu* Sender, Vcl::Menus::TMenuItem* Item, TMenuOwnerDrawState State, int &ImageIndex);

class PASCALIMPLEMENTATION TMxPopupMenu : public Vcl::Menus::TPopupMenu
{
	typedef Vcl::Menus::TPopupMenu inherited;
	
private:
	System::Uitypes::TColor FMarginStartColor;
	System::Uitypes::TColor FMarginEndColor;
	System::Uitypes::TColor FSepHColor;
	System::Uitypes::TColor FSepLColor;
	System::Uitypes::TColor FBKColor;
	System::Uitypes::TColor FSelColor;
	System::Uitypes::TColor FSelFontColor;
	System::Uitypes::TColor FFontColor;
	TMxMenuStyle FStyle;
	Vcl::Graphics::TCanvas* FCanvas;
	bool FShowCheckMarks;
	unsigned FMinTextOffset;
	unsigned FLeftMargin;
	System::Uitypes::TCursor FCursor;
	TDrawMenuItemEvent FOnDrawItem;
	TMeasureMenuItemEvent FOnMeasureItem;
	TDrawMarginEvent FOnDrawMargin;
	TItemParamsEvent FOnGetItemParams;
	System::Types::TPoint FPopupPoint;
	bool FParentBiDiMode;
	Vcl::Controls::TImageList* FImages;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	TItemImageEvent FOnGetImageIndex;
	HIDESBASE void __fastcall SetImages(Vcl::Controls::TImageList* Value);
	HIDESBASE void __fastcall ImageListChange(System::TObject* Sender);
	void __fastcall SetStyle(TMxMenuStyle Value);
	void __fastcall WndMessage(System::TObject* Sender, Winapi::Messages::TMessage &AMsg, bool &Handled);
	MESSAGE void __fastcall WMDrawItem(Winapi::Messages::TWMDrawItem &Message);
	MESSAGE void __fastcall WMMeasureItem(Winapi::Messages::TWMMeasureItem &Message);
	HIDESBASE void __fastcall SetBiDiModeFromPopupControl();
	
protected:
	virtual void __fastcall Loaded();
	HIDESBASE bool __fastcall UseRightToLeftAlignment();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	DYNAMIC void __fastcall GetImageIndex(Vcl::Menus::TMenuItem* Item, TMenuOwnerDrawState State, int &ImageIndex);
	virtual void __fastcall DrawItem(Vcl::Menus::TMenuItem* Item, const System::Types::TRect &Rect, TMenuOwnerDrawState State);
	virtual void __fastcall DrawMargin(const System::Types::TRect &ARect);
	DYNAMIC void __fastcall GetItemParams(Vcl::Menus::TMenuItem* Item, TMenuOwnerDrawState State, Vcl::Graphics::TFont* AFont, System::Uitypes::TColor &Color, Vcl::Graphics::TGraphic* &Graphic, int &NumGlyphs);
	DYNAMIC void __fastcall MeasureItem(Vcl::Menus::TMenuItem* Item, int &Width, int &Height);
	virtual void __fastcall RefreshMenu(bool AOwnerDraw);
	bool __fastcall IsOwnerDrawMenu();
	
public:
	__fastcall virtual TMxPopupMenu(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMxPopupMenu();
	void __fastcall Refresh();
	virtual void __fastcall Popup(int X, int Y);
	void __fastcall DefaultDrawItem(Vcl::Menus::TMenuItem* Item, const System::Types::TRect &Rect, TMenuOwnerDrawState State);
	void __fastcall DefaultDrawMargin(const System::Types::TRect &ARect, System::Uitypes::TColor StartColor, System::Uitypes::TColor EndColor);
	__property Vcl::Graphics::TCanvas* Canvas = {read=FCanvas};
	
__published:
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=FCursor, default=0};
	__property System::Uitypes::TColor MarginStartColor = {read=FMarginStartColor, write=FMarginStartColor, default=16711680};
	__property System::Uitypes::TColor MarginEndColor = {read=FMarginEndColor, write=FMarginEndColor, default=16711680};
	__property System::Uitypes::TColor BKColor = {read=FBKColor, write=FBKColor, default=-16777201};
	__property System::Uitypes::TColor SelColor = {read=FSelColor, write=FSelColor, default=-16777200};
	__property System::Uitypes::TColor SelFontColor = {read=FSelFontColor, write=FSelFontColor, default=-16777202};
	__property System::Uitypes::TColor FontColor = {read=FFontColor, write=FFontColor, default=-16777209};
	__property System::Uitypes::TColor SepHColor = {read=FSepHColor, write=FSepHColor, default=-16777200};
	__property System::Uitypes::TColor SepLColor = {read=FSepLColor, write=FSepLColor, default=-16777196};
	__property unsigned LeftMargin = {read=FLeftMargin, write=FLeftMargin, default=0};
	__property unsigned MinTextOffset = {read=FMinTextOffset, write=FMinTextOffset, default=0};
	__property TMxMenuStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property bool ShowCheckMarks = {read=FShowCheckMarks, write=FShowCheckMarks, default=1};
	__property OwnerDraw = {stored=false, default=0};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property TItemImageEvent OnGetImageIndex = {read=FOnGetImageIndex, write=FOnGetImageIndex};
	__property TDrawMenuItemEvent OnDrawItem = {read=FOnDrawItem, write=FOnDrawItem};
	__property TDrawMarginEvent OnDrawMargin = {read=FOnDrawMargin, write=FOnDrawMargin};
	__property TItemParamsEvent OnGetItemParams = {read=FOnGetItemParams, write=FOnGetItemParams};
	__property TMeasureMenuItemEvent OnMeasureItem = {read=FOnMeasureItem, write=FOnMeasureItem};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall IsItemPopup(Vcl::Menus::TMenuItem* Item);
extern DELPHI_PACKAGE void __fastcall SetDefaultMenuFont(Vcl::Graphics::TFont* AFont);
}	/* namespace Mxmenus */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MXMENUS)
using namespace Mxmenus;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MxmenusHPP
