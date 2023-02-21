// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElCheckCtl.pas' rev: 35.00 (Windows)

#ifndef ElcheckctlHPP
#define ElcheckctlHPP

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
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.ImgList.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <HTMLRender.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElImgFrm.hpp>
#include <ElSndMap.hpp>
#include <ElVCLUtils.hpp>
#include <ElBtnCtl.hpp>
#include <ElHandPt.hpp>
#include <ElStrUtils.hpp>
#include <ElTools.hpp>
#include <ElXPThemedControl.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elcheckctl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElCheckItem;
class DELPHICLASS TElCheckBox;
class DELPHICLASS TElRadioButton;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElCheckItem : public Elbtnctl::TElButtonControl
{
	typedef Elbtnctl::TElButtonControl inherited;
	
private:
	bool FFlat;
	Elsndmap::TElSoundMap* FSoundMap;
	Elsndmap::TElSoundName FCheckSound;
	bool FUseCustomGlyphs;
	Vcl::Graphics::TBitmap* FGlyph;
	System::Classes::TAlignment FAlignment;
	bool FMouseInControl;
	bool FPressed;
	HWND FOldCapture;
	Vcl::Imglist::TChangeLink* FChLink;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	bool FAutoSize;
	bool FIsHTML;
	Htmlrender::TElHTMLRender* FRender;
	bool FModified;
	Vcl::Controls::TImageList* FImages;
	bool FUseImageList;
	System::Uitypes::TCursor FCursor;
	System::Types::TPoint FTextPos;
	System::Uitypes::TColor FLinkColor;
	Vcl::Menus::TPopupMenu* FLinkPopupMenu;
	System::Uitypes::TFontStyles FLinkStyle;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLLinkClickEvent FOnLinkClick;
	void __fastcall AdjustAutoSize();
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall ImagesChanged(System::TObject* Sender);
	void __fastcall setImageForm(Elimgfrm::TElImageForm* newValue);
	void __fastcall setUseCustomGlyphs(bool newValue);
	void __fastcall setGlyph(Vcl::Graphics::TBitmap* newValue);
	void __fastcall setAlignment(System::Classes::TLeftRight newValue);
	void __fastcall setSoundMap(Elsndmap::TElSoundMap* newValue);
	void __fastcall SetIsHTML(bool Value);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMTextChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMFocusChanged(Vcl::Controls::TCMFocusChanged &Msg);
	void __fastcall SetFlat(bool newValue);
	void __fastcall SetImages(Vcl::Controls::TImageList* Value);
	void __fastcall SetUseImageList(bool Value);
	
protected:
	bool FFlatAlways;
	bool FHandleDialogKeys;
	int __fastcall GetPartId();
	System::Types::TSize __fastcall GetCheckBoxSize();
	HIDESBASE void __fastcall setAutoSize(bool newValue);
	DYNAMIC void __fastcall DoEnter();
	DYNAMIC void __fastcall DoExit();
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	virtual void __fastcall GlyphChange(System::TObject* Sender);
	virtual void __fastcall DrawFlatFrame(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R) = 0 ;
	virtual void __fastcall DrawGlyph(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &DestRect, const System::Types::TRect &SrcRect);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall Paint();
	void __fastcall SetFlatAlways(bool Value);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall WMRButtonUp(Winapi::Messages::TWMMouse &Message);
	virtual void __fastcall CreateWnd();
	virtual int __fastcall CalcAutoHeight(bool Multiline);
	HIDESBASE virtual void __fastcall SetCursor(System::Uitypes::TCursor newValue);
	System::Types::TRect __fastcall CalcTextRect();
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SetLinkPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	virtual void __fastcall SetLinkColor(System::Uitypes::TColor newValue);
	virtual void __fastcall SetLinkStyle(System::Uitypes::TFontStyles newValue);
	void __fastcall DoLinkPopup(const System::Types::TPoint &MousePos);
	void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	virtual void __fastcall TriggerLinkClickEvent(Elstrutils::TElFString HRef);
	__property Elsndmap::TElSoundName CheckSound = {read=FCheckSound, write=FCheckSound};
	__property Elsndmap::TElSoundMap* SoundMap = {read=FSoundMap, write=setSoundMap};
	__property System::Classes::TLeftRight Alignment = {read=FAlignment, write=setAlignment, default=1};
	__property bool UseCustomGlyphs = {read=FUseCustomGlyphs, write=setUseCustomGlyphs, default=0};
	__property Vcl::Graphics::TBitmap* Glyph = {read=FGlyph, write=setGlyph};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=setImageForm};
	__property bool AutoSize = {read=FAutoSize, write=setAutoSize, default=1};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, default=0};
	__property bool FlatAlways = {read=FFlatAlways, write=SetFlatAlways, default=0};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property bool UseImageList = {read=FUseImageList, write=SetUseImageList, default=0};
	
public:
	__fastcall virtual TElCheckItem(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElCheckItem();
	virtual void __fastcall Loaded();
	DYNAMIC void __fastcall Click();
	__property bool Modified = {read=FModified, write=FModified, nodefault};
	__property System::Uitypes::TCursor Cursor = {read=FCursor, write=SetCursor, nodefault};
	__property System::Uitypes::TColor LinkColor = {read=FLinkColor, write=SetLinkColor, default=16711680};
	__property Vcl::Menus::TPopupMenu* LinkPopupMenu = {read=FLinkPopupMenu, write=SetLinkPopupMenu};
	__property System::Uitypes::TFontStyles LinkStyle = {read=FLinkStyle, write=SetLinkStyle, nodefault};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
	__property Htmlrender::TElHTMLLinkClickEvent OnLinkClick = {read=FOnLinkClick, write=FOnLinkClick};
	
__published:
	__property bool HandleDialogKeys = {read=FHandleDialogKeys, write=FHandleDialogKeys, default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElCheckItem(HWND ParentWindow) : Elbtnctl::TElButtonControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElCheckBox : public TElCheckItem
{
	typedef TElCheckItem inherited;
	
private:
	Vcl::Stdctrls::TCheckBoxState FState;
	bool FAllowGrayed;
	void __fastcall SetState(Vcl::Stdctrls::TCheckBoxState newValue);
	void __fastcall SetAllowGrayed(bool newValue);
	
protected:
	virtual bool __fastcall GetChecked();
	virtual void __fastcall SetChecked(bool newValue);
	virtual void __fastcall DrawFlatFrame(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	virtual void __fastcall Toggle();
	
__published:
	__property Alignment = {default=1};
	__property bool AllowGrayed = {read=FAllowGrayed, write=SetAllowGrayed, default=0};
	__property AutoSize = {default=1};
	__property UseCustomGlyphs = {default=0};
	__property Checked = {default=0};
	__property CheckSound = {default=0};
	__property SoundMap;
	__property Glyph;
	__property ImageForm;
	__property TextDrawType = {default=0};
	__property Transparent = {default=0};
	__property Flat = {default=0};
	__property FlatAlways = {default=0};
	__property IsHTML = {default=0};
	__property Images;
	__property Vcl::Stdctrls::TCheckBoxState State = {read=FState, write=SetState, default=0};
	__property Cursor;
	__property LinkColor = {default=16711680};
	__property LinkPopupMenu;
	__property LinkStyle;
	__property OnImageNeeded;
	__property OnLinkClick;
	__property MoneyFlat = {default=0};
	__property MoneyFlatActiveColor;
	__property MoneyFlatInactiveColor;
	__property MoneyFlatDownColor;
	__property UseImageList = {default=0};
	__property Caption;
	__property Enabled = {default=1};
	__property TabStop = {default=1};
	__property TabOrder = {default=-1};
	__property PopupMenu;
	__property Color;
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property UseXPThemes = {default=1};
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnStartDrag;
	__property Anchors = {default=3};
	__property Action;
	__property Constraints;
	__property DockOrientation;
	__property Floating;
	__property DoubleBuffered;
	__property DragKind = {default=0};
	__property OnStartDock;
	__property OnEndDock;
public:
	/* TElCheckItem.Create */ inline __fastcall virtual TElCheckBox(System::Classes::TComponent* AOwner) : TElCheckItem(AOwner) { }
	/* TElCheckItem.Destroy */ inline __fastcall virtual ~TElCheckBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElCheckBox(HWND ParentWindow) : TElCheckItem(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElRadioButton : public TElCheckItem
{
	typedef TElCheckItem inherited;
	
private:
	bool FChecked;
	
protected:
	virtual void __fastcall DrawFlatFrame(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	virtual bool __fastcall GetChecked();
	virtual void __fastcall SetChecked(bool newValue);
	
__published:
	__property AutoSize = {default=1};
	__property Checked = {default=0};
	__property Cursor;
	__property LinkColor = {default=16711680};
	__property LinkPopupMenu;
	__property LinkStyle;
	__property OnImageNeeded;
	__property OnLinkClick;
	__property UseCustomGlyphs = {default=0};
	__property CheckSound = {default=0};
	__property SoundMap;
	__property Alignment = {default=1};
	__property Glyph;
	__property ImageForm;
	__property MoneyFlat = {default=0};
	__property MoneyFlatActiveColor;
	__property MoneyFlatInactiveColor;
	__property MoneyFlatDownColor;
	__property Transparent = {default=0};
	__property TextDrawType = {default=0};
	__property Flat = {default=0};
	__property FlatAlways = {default=0};
	__property IsHTML = {default=0};
	__property Images;
	__property UseImageList = {default=0};
	__property UseXPThemes = {default=1};
	__property Caption;
	__property Enabled = {default=1};
	__property TabStop = {default=1};
	__property TabOrder = {default=-1};
	__property PopupMenu;
	__property Color;
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnStartDrag;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
public:
	/* TElCheckItem.Create */ inline __fastcall virtual TElRadioButton(System::Classes::TComponent* AOwner) : TElCheckItem(AOwner) { }
	/* TElCheckItem.Destroy */ inline __fastcall virtual ~TElRadioButton() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElRadioButton(HWND ParentWindow) : TElCheckItem(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elcheckctl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELCHECKCTL)
using namespace Elcheckctl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElcheckctlHPP
