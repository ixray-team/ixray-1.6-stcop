// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElGroupBox.pas' rev: 35.00 (Windows)

#ifndef ElgroupboxHPP
#define ElgroupboxHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <ElImgFrm.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ImgList.hpp>
#include <ElList.hpp>
#include <ElPanel.hpp>
#include <ElVCLUtils.hpp>
#include <HTMLRender.hpp>
#include <ElTools.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElSndMap.hpp>
#include <ElCheckCtl.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <ElStrUtils.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elgroupbox
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomElGroupBox;
class DELPHICLASS TElGroupBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCustomElGroupBox : public Elpanel::TCustomElPanel
{
	typedef Elpanel::TCustomElPanel inherited;
	
protected:
	bool FReading;
	Htmlrender::TElHTMLRender* FRender;
	bool FIsHTML;
	Elvclutils::TElBorderSides FBorderSides;
	System::Types::TRect FTextRect;
	System::Types::TRect FCheckRect;
	Elsndmap::TElSoundName FCheckSound;
	Vcl::Graphics::TBitmap* FGlyph;
	Vcl::Controls::TImageList* FImages;
	Elsndmap::TElSoundMap* FSoundMap;
	bool FUseCustomGlyphs;
	bool FUseImageList;
	Vcl::Imglist::TChangeLink* FChLink;
	bool FShowCheckBox;
	bool FCheckBoxChecked;
	bool FShowFocus;
	System::Uitypes::TColor FCaptionColor;
	bool FMouseInText;
	bool FMouseInCheckBox;
	bool FFlat;
	bool FFlatAlways;
	bool FAutoDisableChildren;
	bool FMoneyFlat;
	System::Uitypes::TColor FMoneyFlatInactiveColor;
	System::Uitypes::TColor FMoneyFlatActiveColor;
	System::Uitypes::TColor FMoneyFlatDownColor;
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	void __fastcall ImagesChanged(System::TObject* Sender);
	virtual void __fastcall SetIsHTML(bool Value);
	virtual void __fastcall ReadState(System::Classes::TReader* Reader);
	virtual bool __fastcall CanModify();
	virtual void __fastcall Paint();
	virtual System::WideString __fastcall GetThemedClassName();
	void __fastcall SetShowCheckBox(bool Value);
	virtual void __fastcall SetCheckBoxChecked(bool Value);
	HIDESBASE void __fastcall SetShowFocus(bool Value);
	void __fastcall SetCaptionColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetCheckSound(Elsndmap::TElSoundName &Value);
	virtual void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	virtual void __fastcall SetSoundMap(Elsndmap::TElSoundMap* Value);
	virtual void __fastcall SetUseCustomGlyphs(bool Value);
	virtual void __fastcall SetImages(Vcl::Controls::TImageList* Value);
	virtual void __fastcall SetUseImageList(bool Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	virtual void __fastcall GlyphChange(System::TObject* Sender);
	System::Types::TSize __fastcall GetCheckBoxSize();
	virtual void __fastcall DrawGlyph(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &DestRect, const System::Types::TRect &SrcRect);
	virtual void __fastcall SetFlat(bool newValue);
	void __fastcall DrawFlatFrame(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &R);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall AdjustClientRect(System::Types::TRect &Rect);
	int __fastcall GetTopOffset();
	HIDESBASE MESSAGE void __fastcall WMEraseBkGnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Msg);
	void __fastcall SetFlatAlways(bool Value);
	System::Types::TRect __fastcall GetCaptionRect();
	System::Types::TRect __fastcall GetCheckRect();
	int __fastcall GetLineTopOffset();
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	void __fastcall SetAutoDisableChildren(bool Value);
	virtual void __fastcall SetMoneyFlat(bool Value);
	virtual void __fastcall SetMoneyFlatInactiveColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetMoneyFlatActiveColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetMoneyFlatDownColor(System::Uitypes::TColor Value);
	bool __fastcall GetMoneyFlat();
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, default=0};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property bool ShowCheckBox = {read=FShowCheckBox, write=SetShowCheckBox, default=0};
	__property bool CheckBoxChecked = {read=FCheckBoxChecked, write=SetCheckBoxChecked, default=1};
	__property bool ShowFocus = {read=FShowFocus, write=SetShowFocus, nodefault};
	__property System::Uitypes::TColor CaptionColor = {read=FCaptionColor, write=SetCaptionColor, default=536870911};
	__property Elsndmap::TElSoundName CheckSound = {read=FCheckSound, write=SetCheckSound};
	__property Vcl::Graphics::TBitmap* Glyph = {read=FGlyph, write=SetGlyph};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property Elsndmap::TElSoundMap* SoundMap = {read=FSoundMap, write=SetSoundMap};
	__property bool UseCustomGlyphs = {read=FUseCustomGlyphs, write=SetUseCustomGlyphs, default=0};
	__property bool UseImageList = {read=FUseImageList, write=SetUseImageList, default=0};
	__property bool Flat = {read=FFlat, write=SetFlat, nodefault};
	__property bool FlatAlways = {read=FFlatAlways, write=SetFlatAlways, nodefault};
	__property bool AutoDisableChildren = {read=FAutoDisableChildren, write=SetAutoDisableChildren, nodefault};
	__property bool MoneyFlat = {read=GetMoneyFlat, write=SetMoneyFlat, default=0};
	__property System::Uitypes::TColor MoneyFlatInactiveColor = {read=FMoneyFlatInactiveColor, write=SetMoneyFlatInactiveColor, stored=GetMoneyFlat, nodefault};
	__property System::Uitypes::TColor MoneyFlatActiveColor = {read=FMoneyFlatActiveColor, write=SetMoneyFlatActiveColor, stored=GetMoneyFlat, nodefault};
	__property System::Uitypes::TColor MoneyFlatDownColor = {read=FMoneyFlatDownColor, write=SetMoneyFlatDownColor, stored=GetMoneyFlat, nodefault};
	
public:
	__fastcall virtual TCustomElGroupBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElGroupBox();
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElGroupBox(HWND ParentWindow) : Elpanel::TCustomElPanel(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElGroupBox : public TCustomElGroupBox
{
	typedef TCustomElGroupBox inherited;
	
__published:
	__property Align = {default=0};
	__property BorderSides;
	__property Caption;
	__property CaptionColor = {default=536870911};
	__property CheckBoxChecked = {default=1};
	__property CheckSound = {default=0};
	__property Color = {default=-16777201};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Flat;
	__property FlatAlways;
	__property Font;
	__property Glyph;
	__property ImageForm;
	__property Images;
	__property MoneyFlat = {default=0};
	__property MoneyFlatInactiveColor;
	__property MoneyFlatActiveColor;
	__property MoneyFlatDownColor;
	__property IsHTML = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentColor = {default=0};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowCheckBox = {default=0};
	__property ShowFocus;
	__property ShowHint;
	__property SoundMap;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Transparent = {default=0};
	__property UseCustomGlyphs = {default=0};
	__property UseImageList = {default=0};
	__property UseXPThemes = {default=1};
	__property Visible = {default=1};
	__property OnClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnMouseDown;
	__property OnMouseUp;
	__property OnMouseMove;
	__property OnStartDrag;
public:
	/* TCustomElGroupBox.Create */ inline __fastcall virtual TElGroupBox(System::Classes::TComponent* AOwner) : TCustomElGroupBox(AOwner) { }
	/* TCustomElGroupBox.Destroy */ inline __fastcall virtual ~TElGroupBox() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElGroupBox(HWND ParentWindow) : TCustomElGroupBox(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elgroupbox */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELGROUPBOX)
using namespace Elgroupbox;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElgroupboxHPP
