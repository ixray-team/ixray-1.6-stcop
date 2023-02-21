// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'HTMLRender.pas' rev: 35.00 (Windows)

#ifndef HtmlrenderHPP
#define HtmlrenderHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <ElList.hpp>
#include <ElStack.hpp>
#include <ElTools.hpp>
#include <ElVCLUtils.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElStrUtils.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Htmlrender
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElHTMLItem;
class DELPHICLASS TElHTMLBreakItem;
class DELPHICLASS TElHTMLData;
class DELPHICLASS TElHTMLRender;
//-- type declarations -------------------------------------------------------
using Elstrutils::TElFString;

typedef void __fastcall (__closure *TElHTMLImageNeededEvent)(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);

typedef void __fastcall (__closure *TElHTMLLinkClickEvent)(System::TObject* Sender, Elstrutils::TElFString HRef);

enum DECLSPEC_DENUM THTMLItemType : unsigned char { hitChar, hitSoftBreak, hitBreak, hitPara, hitBitmap, hitHR, hitLI, hitUL };

typedef System::Set<THTMLItemType, THTMLItemType::hitChar, THTMLItemType::hitUL> THTMLItemTypes;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElHTMLItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TElHTMLData* FOwner;
	THTMLItemType ItemType;
	Elstrutils::TElFString FText;
	System::Uitypes::TFontStyles FontStyle;
	int FontHeight;
	System::Uitypes::TColor FontBgColor;
	System::Uitypes::TColor FontColor;
	Elstrutils::TElFString FLinkRef;
	Elstrutils::TElFString FFontRef;
	int FFntCnt;
	System::Word TWidth;
	System::Word Width;
	System::Word Height;
	int Indent;
	int FBoolState;
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight(int &BaseLine);
	bool __fastcall GetIsLink();
	void __fastcall SetIsLink(bool Value);
	bool __fastcall GetIsSub();
	void __fastcall SetIsSub(bool Value);
	bool __fastcall GetIsSuper();
	void __fastcall SetIsSuper(bool Value);
	
public:
	void __fastcall Assign(TElHTMLItem* Source);
	__fastcall TElHTMLItem(TElHTMLData* Owner);
	__fastcall virtual ~TElHTMLItem();
	__property Elstrutils::TElFString Text = {read=FText, write=FText};
	__property Elstrutils::TElFString LinkRef = {read=FLinkRef, write=FLinkRef};
	__property bool IsLink = {read=GetIsLink, write=SetIsLink, nodefault};
	__property bool IsSub = {read=GetIsSub, write=SetIsSub, nodefault};
	__property bool IsSuper = {read=GetIsSuper, write=SetIsSuper, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElHTMLBreakItem : public TElHTMLItem
{
	typedef TElHTMLItem inherited;
	
private:
	int FParams;
	int ListLevel;
	int ListItemN;
	virtual int __fastcall GetWidth();
	virtual int __fastcall GetHeight(int &BaseLine);
	
public:
	HIDESBASE void __fastcall Assign(TElHTMLItem* Source);
	void __fastcall AssignBreakProps(TElHTMLBreakItem* Source);
public:
	/* TElHTMLItem.Create */ inline __fastcall TElHTMLBreakItem(TElHTMLData* Owner) : TElHTMLItem(Owner) { }
	/* TElHTMLItem.Destroy */ inline __fastcall virtual ~TElHTMLBreakItem() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElHTMLData : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TElHTMLRender* FRender;
	System::Uitypes::TColor FLinkColor;
	System::Uitypes::TColor FDefaultColor;
	System::Uitypes::TFontStyles FLinkStyle;
	System::Uitypes::TFontStyles FDefaultStyle;
	int FDefaultHeight;
	System::Uitypes::TFontCharset FCharset;
	System::UnicodeString FDefaultFont;
	Ellist::TElList* FArray;
	System::Uitypes::TColor FDefaultBgColor;
	System::Uitypes::TColor FHighlightBgColor;
	System::Uitypes::TColor FHighlightColor;
	TElHTMLItem* FSelectedItem;
	System::Types::TRect FRect;
	System::Types::TSize FTextSize;
	
public:
	__fastcall TElHTMLData();
	__fastcall virtual ~TElHTMLData();
	void __fastcall ClearArray();
	int __fastcall LineCount();
	__property System::Types::TSize TextSize = {read=FTextSize};
	__property System::Uitypes::TColor LinkColor = {read=FLinkColor, write=FLinkColor, nodefault};
	__property System::Uitypes::TColor DefaultBgColor = {read=FDefaultBgColor, write=FDefaultBgColor, nodefault};
	__property System::Uitypes::TColor DefaultColor = {read=FDefaultColor, write=FDefaultColor, nodefault};
	__property System::Uitypes::TFontStyles LinkStyle = {read=FLinkStyle, write=FLinkStyle, nodefault};
	__property System::Uitypes::TFontStyles DefaultStyle = {read=FDefaultStyle, write=FDefaultStyle, nodefault};
	__property int DefaultHeight = {read=FDefaultHeight, write=FDefaultHeight, nodefault};
	__property System::UnicodeString DefaultFont = {read=FDefaultFont, write=FDefaultFont};
	__property System::Uitypes::TFontCharset Charset = {read=FCharset, write=FCharset, nodefault};
	__property System::Uitypes::TColor HighlightBgColor = {read=FHighlightBgColor, write=FHighlightBgColor, nodefault};
	__property System::Uitypes::TColor HighlightColor = {read=FHighlightColor, write=FHighlightColor, nodefault};
	__property TElHTMLItem* SelectedItem = {read=FSelectedItem, write=FSelectedItem};
	__property System::Types::TRect Rect = {read=FRect, write=FRect};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElHTMLRender : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TElHTMLImageNeededEvent FOnImageNeeded;
	TElHTMLData* FIntData;
	TElHTMLData* FData;
	Vcl::Graphics::TCanvas* Canvas;
	Vcl::Graphics::TBitmap* Bitmap;
	HGDIOBJ FSaveObj;
	
protected:
	virtual void __fastcall TriggerImageNeededEvent(Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	void __fastcall CalcTokenSizes(TElHTMLData* FCurData);
	System::Types::TSize __fastcall GetTextSize();
	TElHTMLItem* __fastcall FindItemAt(const System::Types::TPoint &Point, const System::Types::TPoint &SrcPoint, const System::Types::TRect &R);
	
public:
	__fastcall TElHTMLRender();
	__fastcall virtual ~TElHTMLRender();
	void __fastcall DestroyData(TElHTMLData* Data);
	void __fastcall SetData(TElHTMLData* NewData);
	TElHTMLData* __fastcall CreateData();
	void __fastcall DrawText(Vcl::Graphics::TCanvas* Canvas, const System::Types::TPoint &SrcPoint, const System::Types::TRect &R, System::Uitypes::TColor AdjustFromColor);
	void __fastcall DrawTextEx(Vcl::Graphics::TCanvas* Canvas, const System::Types::TPoint &SrcPoint, const System::Types::TRect &R, bool UseOverColors, System::Uitypes::TColor Color, System::Uitypes::TColor BkColor, System::Uitypes::TColor SelColor, System::Uitypes::TColor SelBkColor, System::Uitypes::TColor AdjustFromColor);
	bool __fastcall IsCursorOverLink(const System::Types::TPoint &Point, const System::Types::TPoint &SrcPoint, const System::Types::TRect &R, Elstrutils::TElFString &href);
	void __fastcall SelectLinkAt(const System::Types::TPoint &Point, const System::Types::TPoint &SrcPoint, const System::Types::TRect &R);
	void __fastcall SelectPrevLink();
	void __fastcall SelectNextLink();
	void __fastcall PrepareToData(Elstrutils::TElFString Text, int MaxWidth, bool AutoWrap, TElHTMLData* CurData);
	void __fastcall PrepareText(Elstrutils::TElFString Text, int MaxWidth, bool AutoWrap);
	__property TElHTMLData* Data = {read=FData};
	__property TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 flIsLink = System::Int8(0x1);
static const System::Int8 flSub = System::Int8(0x2);
static const System::Int8 flSuper = System::Int8(0x4);
}	/* namespace Htmlrender */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_HTMLRENDER)
using namespace Htmlrender;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// HtmlrenderHPP
