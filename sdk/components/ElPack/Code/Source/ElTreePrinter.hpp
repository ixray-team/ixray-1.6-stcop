// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTreePrinter.pas' rev: 34.00 (Windows)

#ifndef EltreeprinterHPP
#define EltreeprinterHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ElTree.hpp>
#include <ElPrinter.hpp>
#include <ElHeader.hpp>
#include <ElStack.hpp>
#include <HTMLRender.hpp>
#include <ElStrUtils.hpp>
#include <ElVCLUtils.hpp>
#include <ElTools.hpp>
#include <Vcl.ImgList.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltreeprinter
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EElTreePrinterError;
class DELPHICLASS TElTreePrinter;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TPrintTreeItemEvent)(System::TObject* Sender, Eltree::TElTreeItem* Item, bool &Print);

typedef void __fastcall (__closure *TPrintHeaderSectionEvent)(System::TObject* Sender, Elheader::TElHeaderSection* Section, bool &Print);

#pragma pack(push,4)
class PASCALIMPLEMENTATION EElTreePrinterError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EElTreePrinterError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EElTreePrinterError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EElTreePrinterError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EElTreePrinterError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EElTreePrinterError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EElTreePrinterError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EElTreePrinterError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EElTreePrinterError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElTreePrinterError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElTreePrinterError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElTreePrinterError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElTreePrinterError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EElTreePrinterError() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TDrawPageNumberEvent)(System::TObject* Sender, Elstrutils::TElFString &Text, int PageNumber);

typedef void __fastcall (__closure *TDrawPageCaptionEvent)(System::TObject* Sender, Elstrutils::TElFString &Text, int PageNumber, System::Types::TRect &Rec);

enum DECLSPEC_DENUM TPNLayout : unsigned char { plTop, plBottom };

class PASCALIMPLEMENTATION TElTreePrinter : public Elprinter::TElControlPrinter
{
	typedef Elprinter::TElControlPrinter inherited;
	
private:
	Elprinter::TPageEvent FOnAfterPage;
	Elprinter::TPageEvent FOnBeforePage;
	
protected:
	bool FShowPageNumbers;
	TPNLayout FPageNambersLayout;
	Elstrutils::TElFString FPageNumbersText;
	System::Classes::TAlignment FPageNumbersAlignment;
	Elstrutils::TElFString FCaption;
	TDrawPageNumberEvent FOnDrawPageNumber;
	TDrawPageCaptionEvent FOnDrawCaption;
	bool FPrinting;
	Htmlrender::TElHTMLRender* FRender;
	System::Uitypes::TColor FBkColor;
	bool FShowButtons;
	bool FShowCheckboxes;
	bool FShowColumns;
	bool FShowEmptyImages;
	bool FShowEmptyImages2;
	bool FShowHeader;
	bool FshowHeaderExpandMarks;
	bool FShowHeaderImages;
	bool FShowHeaderSortMarks;
	bool FShowHiddenItems;
	bool FShowHiddenSections;
	bool FShowImages;
	bool FShowInvisibleItems;
	bool FShowInvisibleSections;
	bool FShowLines;
	bool FShowRoot;
	bool FShowRootButtons;
	bool FFillBackground;
	Vcl::Graphics::TFont* FFont;
	bool FHeaderOnEveryPage;
	TPrintTreeItemEvent FOnItemPrinting;
	TPrintHeaderSectionEvent FOnSectionPrinting;
	int FScale;
	System::Uitypes::TColor FStripedEvenColor;
	bool FStripedItems;
	System::Uitypes::TColor FStripedOddColor;
	System::Uitypes::TColor FTextColor;
	Eltree::TCustomElTree* FTree;
	Eltree::TElCellStyle* VirtStyle;
	bool FShowLeafButton;
	bool FVerticalLines;
	bool FHorizontalLines;
	System::Uitypes::TColor FHorzDivLinesColor;
	System::Uitypes::TColor FVertDivLinesColor;
	void __fastcall DoDrawHeader(Vcl::Graphics::TCanvas* Canvas, const System::Types::TRect &ARect);
	void __fastcall DoDrawHeaderSection(Vcl::Graphics::TCanvas* Canvas, Elheader::TElHeaderSection* Section, const System::Types::TRect &ARect);
	void __fastcall DoDrawItem(Vcl::Graphics::TCanvas* Canvas, int ItemIndex, Eltree::TElTreeItem* Item, const System::Types::TRect &ARect);
	void __fastcall DoDrawItemCellContents(Vcl::Graphics::TCanvas* Canvas, Eltree::TElTreeItem* Item, Elheader::TElHeaderSection* Section, const System::Types::TRect &ARect, System::Uitypes::TColor TextColor, System::Uitypes::TColor TextBkColor, System::Uitypes::TColor ItemBkColor, System::Uitypes::TFontStyles FontStyle);
	void __fastcall DoDrawItemTree(Vcl::Graphics::TCanvas* Canvas, Eltree::TElTreeItem* Item, Elheader::TElHeaderSection* Section, System::Types::TRect &ARect);
	void __fastcall SetBkColor(System::Uitypes::TColor Value);
	void __fastcall SetShowButtons(bool Value);
	void __fastcall SetShowCheckboxes(bool newValue);
	void __fastcall SetShowColumns(bool Value);
	void __fastcall SetShowEmptyImages(bool newValue);
	void __fastcall SetShowEmptyImages2(bool newValue);
	void __fastcall SetShowHeader(bool Value);
	void __fastcall SetshowHeaderExpandMarks(bool Value);
	void __fastcall SetShowHeaderImages(bool Value);
	void __fastcall SetShowHeaderSortMarks(bool Value);
	void __fastcall SetShowHiddenItems(bool Value);
	void __fastcall SetShowHiddenSections(bool Value);
	void __fastcall SetShowImages(bool Value);
	void __fastcall SetShowInvisibleItems(bool Value);
	void __fastcall SetShowInvisibleSections(bool Value);
	void __fastcall SetShowLines(bool Value);
	void __fastcall SetShowRoot(bool Value);
	void __fastcall SetShowRootButtons(bool newValue);
	void __fastcall SetFillBackground(bool Value);
	void __fastcall SetHeaderOnEveryPage(bool Value);
	void __fastcall SetScale(int Value);
	void __fastcall SetTree(Eltree::TCustomElTree* Value);
	virtual void __fastcall TriggerItemPrintingEvent(Eltree::TElTreeItem* Item, bool &Print);
	virtual void __fastcall TriggerSectionPrintingEvent(Elheader::TElHeaderSection* Section, bool &Print);
	void __fastcall DrawButtons(Vcl::Graphics::TCanvas* ACanvas, Eltree::TElTreeItem* Item, bool IsNode, System::Types::TRect &R);
	void __fastcall DrawCheckBoxes(Vcl::Graphics::TCanvas* ACanvas, Eltree::TElTreeItem* Item, System::Types::TRect &R);
	void __fastcall DrawImages(Vcl::Graphics::TCanvas* ACanvas, Eltree::TElTreeItem* Item, System::Types::TRect &R);
	void __fastcall DrawItemLines(Vcl::Graphics::TCanvas* ACanvas, Eltree::TElTreeItem* Item, System::Types::TRect &R);
	void __fastcall SetVerticalLines(bool Value);
	void __fastcall SetHorizontalLines(bool Value);
	void __fastcall SetShowLeafButton(bool Value);
	void __fastcall SetHorzDivLinesColor(System::Uitypes::TColor Value);
	void __fastcall SetVertDivLinesColor(System::Uitypes::TColor Value);
	virtual void __fastcall TriggerAfterPage(int PageNumber);
	virtual void __fastcall TriggerBeforePage(int PageNumber);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall DoDrawPageNumber(int PageNumber);
	virtual void __fastcall DoDrawCaption(int PageNumber, System::Types::TRect &Rec);
	
public:
	__fastcall virtual TElTreePrinter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTreePrinter();
	void __fastcall Print();
	
__published:
	__property System::Uitypes::TColor BkColor = {read=FBkColor, write=SetBkColor, nodefault};
	__property bool ShowButtons = {read=FShowButtons, write=SetShowButtons, default=0};
	__property bool ShowCheckboxes = {read=FShowCheckboxes, write=SetShowCheckboxes, default=0};
	__property bool ShowColumns = {read=FShowColumns, write=SetShowColumns, nodefault};
	__property bool ShowEmptyImages = {read=FShowEmptyImages, write=SetShowEmptyImages, default=0};
	__property bool ShowEmptyImages2 = {read=FShowEmptyImages2, write=SetShowEmptyImages2, default=0};
	__property bool ShowHeader = {read=FShowHeader, write=SetShowHeader, nodefault};
	__property bool showHeaderExpandMarks = {read=FshowHeaderExpandMarks, write=SetshowHeaderExpandMarks, nodefault};
	__property bool ShowHeaderImages = {read=FShowHeaderImages, write=SetShowHeaderImages, nodefault};
	__property bool ShowHeaderSortMarks = {read=FShowHeaderSortMarks, write=SetShowHeaderSortMarks, nodefault};
	__property bool ShowHiddenItems = {read=FShowHiddenItems, write=SetShowHiddenItems, nodefault};
	__property bool ShowHiddenSections = {read=FShowHiddenSections, write=SetShowHiddenSections, nodefault};
	__property bool ShowImages = {read=FShowImages, write=SetShowImages, default=1};
	__property bool ShowInvisibleItems = {read=FShowInvisibleItems, write=SetShowInvisibleItems, default=1};
	__property bool ShowInvisibleSections = {read=FShowInvisibleSections, write=SetShowInvisibleSections, nodefault};
	__property bool ShowLines = {read=FShowLines, write=SetShowLines, default=1};
	__property bool ShowRoot = {read=FShowRoot, write=SetShowRoot, default=0};
	__property bool ShowRootButtons = {read=FShowRootButtons, write=SetShowRootButtons, default=0};
	__property bool FillBackground = {read=FFillBackground, write=SetFillBackground, nodefault};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=FFont};
	__property bool HeaderOnEveryPage = {read=FHeaderOnEveryPage, write=SetHeaderOnEveryPage, nodefault};
	__property TPrintTreeItemEvent OnItemPrinting = {read=FOnItemPrinting, write=FOnItemPrinting};
	__property TPrintHeaderSectionEvent OnSectionPrinting = {read=FOnSectionPrinting, write=FOnSectionPrinting};
	__property int Scale = {read=FScale, write=SetScale, default=100};
	__property System::Uitypes::TColor StripedEvenColor = {read=FStripedEvenColor, write=FStripedEvenColor, nodefault};
	__property bool StripedItems = {read=FStripedItems, write=FStripedItems, default=0};
	__property System::Uitypes::TColor StripedOddColor = {read=FStripedOddColor, write=FStripedOddColor, nodefault};
	__property System::Uitypes::TColor TextColor = {read=FTextColor, write=FTextColor, nodefault};
	__property Eltree::TCustomElTree* Tree = {read=FTree, write=SetTree};
	__property bool ShowLeafButton = {read=FShowLeafButton, write=SetShowLeafButton, default=0};
	__property bool VerticalLines = {read=FVerticalLines, write=SetVerticalLines, default=0};
	__property bool HorizontalLines = {read=FHorizontalLines, write=SetHorizontalLines, nodefault};
	__property System::Uitypes::TColor HorzDivLinesColor = {read=FHorzDivLinesColor, write=SetHorzDivLinesColor, nodefault};
	__property System::Uitypes::TColor VertDivLinesColor = {read=FVertDivLinesColor, write=SetVertDivLinesColor, nodefault};
	__property Elprinter::TPageEvent OnAfterPage = {read=FOnAfterPage, write=FOnAfterPage};
	__property Elprinter::TPageEvent OnBeforePage = {read=FOnBeforePage, write=FOnBeforePage};
	__property bool ShowPageNumbers = {read=FShowPageNumbers, write=FShowPageNumbers, nodefault};
	__property TPNLayout PageNambersLayout = {read=FPageNambersLayout, write=FPageNambersLayout, default=0};
	__property Elstrutils::TElFString PageNumbersText = {read=FPageNumbersText, write=FPageNumbersText};
	__property System::Classes::TAlignment PageNumbersAlignment = {read=FPageNumbersAlignment, write=FPageNumbersAlignment, default=2};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=FCaption};
	__property TDrawPageNumberEvent OnDrawPageNumber = {read=FOnDrawPageNumber, write=FOnDrawPageNumber};
	__property TDrawPageCaptionEvent OnDrawCaption = {read=FOnDrawCaption, write=FOnDrawCaption};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eltreeprinter */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTREEPRINTER)
using namespace Eltreeprinter;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltreeprinterHPP
