// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElEdits.pas' rev: 34.00 (Windows)

#ifndef EleditsHPP
#define EleditsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Clipbrd.hpp>
#include <ElTools.hpp>
#include <ElImgFrm.hpp>
#include <ElList.hpp>
#include <ElStack.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElVCLUtils.hpp>
#include <ElXPThemedControl.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElScrollBar.hpp>
#include <ElUnicodeStrings.hpp>
#include <Winapi.Imm.hpp>
#include <ElStrUtils.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eledits
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EElEditorError;
class DELPHICLASS TElAction;
class DELPHICLASS TElActionList;
class DELPHICLASS TElParagraph;
class DELPHICLASS TElParagraphList;
class DELPHICLASS TElEditStrings;
class DELPHICLASS TCustomElEdit;
class DELPHICLASS TElEdit;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EElEditorError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EElEditorError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EElEditorError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EElEditorError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EElEditorError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EElEditorError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EElEditorError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EElEditorError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EElEditorError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElEditorError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EElEditorError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElEditorError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EElEditorError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EElEditorError() { }
	
};

#pragma pack(pop)

typedef Elunicodestrings::TElWideStrings TElFStrings;

typedef Elunicodestrings::TElWideStringList TElFStringList;

enum DECLSPEC_DENUM TElActionType : unsigned char { atInsert, atDelete, atLineBreak, atGroupBreak, atPaste, atBackSpace, atDeleteSel, atInsertSel };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElAction : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	TElActionType FAction;
	System::Types::TPoint FStartPos;
	System::Types::TPoint FEndPos;
	Elstrutils::TElFString FStr;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__property TElActionType Action = {read=FAction, nodefault};
	__property System::Types::TPoint StartPos = {read=FStartPos};
	__property System::Types::TPoint EndPos = {read=FEndPos};
	__property Elstrutils::TElFString CString = {read=FStr};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TElAction() { }
	
public:
	/* TObject.Create */ inline __fastcall TElAction() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TElActionList : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
protected:
	Elstack::TElStack* FAStack;
	int FMaxUndo;
	int FLockCount;
	virtual bool __fastcall GetCanUndo();
	virtual void __fastcall SetMaxUndo(const int Value);
	
public:
	__fastcall TElActionList();
	__fastcall virtual ~TElActionList();
	void __fastcall Lock();
	void __fastcall UnLock();
	void __fastcall AddAction(TElActionType AAction, const System::Types::TPoint &ASPos, const System::Types::TPoint &AEPos, Elstrutils::TElFString AStr);
	TElAction* __fastcall PeekItem();
	TElAction* __fastcall PopItem();
	void __fastcall PushItem(TElAction* Item);
	__property bool CanUndo = {read=GetCanUndo, nodefault};
	__property int MaxUndo = {read=FMaxUndo, write=SetMaxUndo, default=10};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElParagraph : public Elunicodestrings::TElWideStringList
{
	typedef Elunicodestrings::TElWideStringList inherited;
	
protected:
	int FPCount;
	virtual void __fastcall SetTextStr(const System::WideString Value);
	virtual System::WideString __fastcall GetTextStr();
	virtual System::WideString __fastcall Get(int Index);
	virtual void __fastcall Put(int Index, const System::WideString S);
	
public:
	__property System::WideString Text = {read=GetTextStr, write=SetTextStr};
public:
	/* TElWideStringList.Destroy */ inline __fastcall virtual ~TElParagraph() { }
	
public:
	/* TObject.Create */ inline __fastcall TElParagraph() : Elunicodestrings::TElWideStringList() { }
	
};


class PASCALIMPLEMENTATION TElParagraphList : public Ellist::TElList
{
	typedef Ellist::TElList inherited;
	
public:
	TElParagraph* operator[](int Index) { return this->Items[Index]; }
	
protected:
	HIDESBASE TElParagraph* __fastcall Get(int Index);
	HIDESBASE void __fastcall Put(int Index, TElParagraph* const Value);
	HIDESBASE void __fastcall Delete(int Index);
	
public:
	virtual void __fastcall Clear();
	__property TElParagraph* Items[int Index] = {read=Get, write=Put/*, default*/};
public:
	/* TElList.Create */ inline __fastcall TElParagraphList() : Ellist::TElList() { }
	/* TElList.Destroy */ inline __fastcall virtual ~TElParagraphList() { }
	
};


class PASCALIMPLEMENTATION TElEditStrings : public Elunicodestrings::TElWideStringList
{
	typedef Elunicodestrings::TElWideStringList inherited;
	
private:
	TCustomElEdit* FElEdit;
	System::Classes::TStringList* RealStrings;
	TElParagraphList* FParagraphs;
	
protected:
	int FMaxLen;
	int FIdxMaxLen;
	Elstrutils::TElFString FMaxStr;
	Elstrutils::TElFString FSaveStr;
	System::Classes::TNotifyEvent FOnChange;
	void __fastcall Reformat();
	virtual void __fastcall Changed();
	virtual System::WideString __fastcall Get(int Index);
	virtual void __fastcall Put(int Index, const System::WideString S);
	virtual void __fastcall SetTextStr(const System::WideString Value);
	virtual System::WideString __fastcall GetTextStr();
	virtual void __fastcall PutObject(int Index, System::TObject* AObject);
	virtual System::TObject* __fastcall GetObject(int Index);
	virtual int __fastcall GetCount();
	void __fastcall ReformatParagraph(int Index);
	void __fastcall ReCount(int Index);
	Elstrutils::TElFString __fastcall GetParaString(int Index);
	void __fastcall SetParaString(int Index, const Elstrutils::TElFString Value);
	int __fastcall GetParaCount();
	
public:
	__fastcall TElEditStrings();
	__fastcall virtual ~TElEditStrings();
	virtual void __fastcall AddStrings(Elunicodestrings::TElWideStrings* Strings);
	virtual int __fastcall Add(const System::WideString S);
	virtual void __fastcall Insert(int Index, const System::WideString S);
	void __fastcall InsertText(int &ACaretX, int &ACaretY, const System::WideString S);
	virtual bool __fastcall Find(const System::WideString S, int &Index);
	virtual void __fastcall Clear();
	virtual void __fastcall Delete(int Index);
	virtual void __fastcall Exchange(int Index1, int Index2);
	void __fastcall IndexToParagraph(int index, int &Paragraph, int &ParaIndex);
	void __fastcall CaretToParagraph(int ACaretX, int ACaretY, int &Paragraph, int &ParaOffs);
	void __fastcall CaretFromParagraph(int Paragraph, int ParaOffs, int &ACaretX, int &ACaretY);
	Elstrutils::TElFString __fastcall GetReText();
	Elstrutils::TElFString __fastcall CutString(Elstrutils::TElFString &S, int Len, bool &RealStr);
	__property Elstrutils::TElFString ParagraphStrings[int Index] = {read=GetParaString, write=SetParaString};
	__property int ParagraphCount = {read=GetParaCount, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
};


enum DECLSPEC_DENUM TElEditCharCase : unsigned char { eecNormal, eecUpperCase, eecLowerCase };

enum DECLSPEC_DENUM TElEditScrollDir : unsigned char { esdLineUp, esdLineDown, esdPageUp, esdPageDown };

class PASCALIMPLEMENTATION TCustomElEdit : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
protected:
	bool FModified;
	System::Types::TRect FEditRect;
	int FLeftMargin;
	int FTopMargin;
	int FRightMargin;
	bool FMouseClick;
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	bool FAutoSelect;
	bool FHideSelection;
	System::Classes::TAlignment FAlignment;
	bool FReadOnly;
	bool FWantTabs;
	Elstrutils::TElFChar FPasswordChar;
	int FMaxLength;
	bool FSelected;
	int FSelLength;
	int FSelStartX;
	int FSelStartY;
	int FSelFirstX;
	int FSelFirstY;
	int FSelLastX;
	int FSelLastY;
	bool FMultiline;
	bool FTransparent;
	Elstrutils::TElFString FTabString;
	int FTabSpaces;
	bool FHasCaret;
	int FCaretX;
	int FCaretY;
	TElActionList* FUndo;
	TElActionList* FRedo;
	int FModifyCount;
	int FLineHeight;
	int FLeftChar;
	int FCharsInView;
	bool FSelecting;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnSelectionChange;
	System::Classes::TNotifyEvent FOnResize;
	bool ForceLeftAlignment;
	Vcl::Graphics::TBitmap* FBackground;
	bool FUseBackground;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	Elvclutils::TElBorderSides FBorderSides;
	Elvclutils::TElFlatBorderType FActiveBorderType;
	bool FFlat;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FHandleDialogKeys;
	bool FMouseOver;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	TElEditStrings* FElEditList;
	int FTopLine;
	bool FWordWrap;
	System::Uitypes::TScrollStyle FScrollBars;
	int FLinesInRect;
	int FCharset;
	HKL FKeybLayout;
	Elscrollbar::TElScrollBar* scbVert;
	Elscrollbar::TElScrollBar* scbHorz;
	bool FFlatFocusedScrollBars;
	bool FUseCustomScrollBars;
	Elscrollbar::TElScrollBarStyles* FVertScrollBarStyles;
	Elscrollbar::TElScrollBarStyles* FHorzScrollBarStyles;
	bool FAlienFocus;
	bool FAlignBottom;
	System::WideString FKeys;
	bool FKeyDown;
	System::WideString FHint;
	bool FRTLContent;
	bool FAutoSize;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	bool FNotifyUserChangeOnly;
	TElEditCharCase FCharCase;
	bool FChangeDisabledText;
	bool FEnd;
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMCaptureChanged(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Msg);
	MESSAGE void __fastcall WMCut(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMCopy(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMPaste(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMClear(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMSetText(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	MESSAGE void __fastcall WMEnable(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Msg);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TWMNoParams &Msg);
	HIDESBASE MESSAGE void __fastcall WMVScroll(Winapi::Messages::TWMScroll &Msg);
	HIDESBASE MESSAGE void __fastcall WMHScroll(Winapi::Messages::TWMScroll &Msg);
	HIDESBASE MESSAGE void __fastcall WMInputLangChange(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMCommand(Winapi::Messages::TWMCommand &Msg);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Msg);
	MESSAGE void __fastcall WMImeStartComposition(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMImeComposition(Winapi::Messages::TMessage &Message);
	void __fastcall OnHScroll(System::TObject* Sender, Elscrollbar::TElScrollCode ScrollCode, int &ScrollPos, bool &DoChange);
	void __fastcall OnVScroll(System::TObject* Sender, Elscrollbar::TElScrollCode ScrollCode, int &ScrollPos, bool &DoChange);
	void __fastcall SBChanged(System::TObject* Sender);
	void __fastcall SetReadOnly(bool newValue);
	void __fastcall SetAlignment(System::Classes::TAlignment newValue);
	void __fastcall SetLeftMargin(int newValue);
	void __fastcall SetRightMargin(int newValue);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle newValue);
	void __fastcall SetHideSelection(bool newValue);
	Elstrutils::TElFString __fastcall GetPasswordChar();
	void __fastcall SetPasswordChar(Elstrutils::TElFString newValue);
	void __fastcall SetTransparent(bool newValue);
	void __fastcall DoSetEditRect(const System::Types::TRect &newValue);
	void __fastcall SetTabSpaces(int newValue);
	void __fastcall SetModified(bool newValue);
	HIDESBASE void __fastcall SetText(Elstrutils::TElFString newValue);
	Elstrutils::TElFString __fastcall GetSelectedText();
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* const Value);
	void __fastcall SetUseBackground(const bool Value);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall DrawBackground(HDC DC, const System::Types::TRect &R);
	void __fastcall DrawFlatBorder();
	void __fastcall DrawParentControl(HDC DC);
	void __fastcall SetScrollBars(const System::Uitypes::TScrollStyle Value);
	void __fastcall SetTopLine(const int Value);
	void __fastcall UpdateHeight();
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Message);
	void __fastcall BackgroundChanged(System::TObject* Sender);
	void __fastcall SetFlatFocusedScrollBars(const bool Value);
	void __fastcall AdjustHeight();
	void __fastcall SetVertScrollBarStyles(Elscrollbar::TElScrollBarStyles* newValue);
	void __fastcall SetHorzScrollBarStyles(Elscrollbar::TElScrollBarStyles* newValue);
	void __fastcall SetUseCustomScrollBars(bool newValue);
	virtual void __fastcall SetMaxLength(int newValue);
	virtual int __fastcall GetSelStart();
	virtual void __fastcall SetSelStart(int newValue);
	virtual void __fastcall SetSelLength(int newValue);
	virtual void __fastcall SetSelText(const Elstrutils::TElFString newValue);
	Elstrutils::TElFString __fastcall StringToPassword(Elstrutils::TElFString AString);
	Elstrutils::TElFString __fastcall ExpandTabbedString(Elstrutils::TElFString Text);
	System::Types::TSize __fastcall TextSize(Elstrutils::TElFString ALine);
	void __fastcall SetScrollBarsInfo();
	void __fastcall MoveCaret(int CharNum);
	void __fastcall MakeCaret();
	void __fastcall RepaintText(const System::Types::TRect &Rect);
	void __fastcall DrawTabbedText(HDC DC, int X, int Y, Elstrutils::TElFString AText, System::Types::TSize &Size);
	Elstrutils::TElFString __fastcall ConvertBreaksFormat(Elstrutils::TElFString Text);
	int __fastcall CharsFitRight(int AWidth, Elstrutils::TElFString FText, int StartPos);
	int __fastcall CharsFitLeft(int AWidth, Elstrutils::TElFString FText, int StartPos);
	virtual void __fastcall Change();
	virtual void __fastcall TriggerSelectionChangeEvent();
	virtual void __fastcall TriggerResizeEvent();
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	virtual void __fastcall Paint();
	void __fastcall PaintText(Vcl::Graphics::TCanvas* Canvas);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	virtual void __fastcall SetFlat(const bool Value);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual System::WideString __fastcall GetThemedClassName();
	int __fastcall GetLinesCount();
	void __fastcall SetWordWrap(bool Value);
	void __fastcall SetLeftChar(int Value);
	HIDESBASE void __fastcall SetAutoSize(bool Value);
	virtual void __fastcall CreateWnd();
	MESSAGE void __fastcall WMGetText(Winapi::Messages::TMessage &Message);
	void __fastcall SetLines(Elunicodestrings::TElWideStrings* Value);
	Elunicodestrings::TElWideStrings* __fastcall GetLines();
	void __fastcall SetTopMargin(int Value);
	void __fastcall SetAlignBottom(bool Value);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	virtual void __fastcall DoMouseEnter();
	virtual void __fastcall DoMouseLeave();
	virtual void __fastcall Loaded();
	virtual void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	virtual void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	MESSAGE void __fastcall EMSetRect(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMSetRectNP(Winapi::Messages::TMessage &Message);
	virtual void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	virtual void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	virtual void __fastcall SetUseXPThemes(const bool Value);
	void __fastcall SetCharCase(TElEditCharCase Value);
	void __fastcall SetSelection(int SelX, int SelY);
	void __fastcall UnSelect();
	void __fastcall DeleteSelection();
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Winapi::Messages::TWMSetCursor &Message);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	void __fastcall SetBottomAlign();
	HIDESBASE Elstrutils::TElFString __fastcall GetText();
	Elstrutils::TElFString __fastcall ConvertToCRLF(Elstrutils::TElFString Str);
	void __fastcall SetCaretX(const int value);
	void __fastcall SetCaretY(const int value);
	System::Types::TPoint __fastcall GetCaretXY();
	void __fastcall SetCaretXY(const System::Types::TPoint &value);
	void __fastcall CorrectLeftChar();
	void __fastcall SetCaretPosition(const int X, const int Y);
	void __fastcall SetMultiline(const bool Value);
	void __fastcall SetMaxLevel(const int Value);
	int __fastcall GetMaxLevel();
	MESSAGE void __fastcall EMGetSel(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMGetLine(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMGetLineCount(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMLineIndex(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMSetSel(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMReplaceSel(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMGetFirstVisibleLine(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMScroll(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMLineScroll(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMScrollCaret(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMLineFromChar(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMPosFromChar(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMCanUndo(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall EMUndo(Winapi::Messages::TMessage &Message);
	void __fastcall SetChangeDisabledText(bool Value);
	virtual void __fastcall SetEditRect(const System::Types::TRect &Value);
	__property bool RTLContent = {read=FRTLContent, write=FRTLContent, nodefault};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property bool UseBackground = {read=FUseBackground, write=SetUseBackground, default=0};
	__property Elstrutils::TElFString PasswordChar = {read=GetPasswordChar, write=SetPasswordChar};
	__property int MaxLength = {read=FMaxLength, write=SetMaxLength, default=0};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, nodefault};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property System::Classes::TNotifyEvent OnSelectionChange = {read=FOnSelectionChange, write=FOnSelectionChange};
	__property bool ReadOnly = {read=FReadOnly, write=SetReadOnly, default=0};
	__property bool WantTabs = {read=FWantTabs, write=FWantTabs, default=0};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, nodefault};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, nodefault};
	__property bool AutoSelect = {read=FAutoSelect, write=FAutoSelect, default=0};
	__property bool HideSelection = {read=FHideSelection, write=SetHideSelection, default=1};
	__property System::Classes::TNotifyEvent OnResize = {read=FOnResize, write=FOnResize};
	__property System::Types::TRect EditRect = {read=FEditRect, write=DoSetEditRect};
	__property int TabSpaces = {read=FTabSpaces, write=SetTabSpaces, default=4};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property bool WordWrap = {read=FWordWrap, write=SetWordWrap, default=0};
	__property System::Uitypes::TScrollStyle ScrollBars = {read=FScrollBars, write=SetScrollBars, default=0};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property bool AutoSize = {read=FAutoSize, write=SetAutoSize, default=1};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property Elunicodestrings::TElWideStrings* Lines = {read=GetLines, write=SetLines};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property bool FlatFocusedScrollBars = {read=FFlatFocusedScrollBars, write=SetFlatFocusedScrollBars, default=0};
	__property TElEditCharCase CharCase = {read=FCharCase, write=SetCharCase, default=0};
	__property int MaxUndoLevel = {read=GetMaxLevel, write=SetMaxLevel, default=10};
	__property bool Multiline = {read=FMultiline, write=SetMultiline, default=0};
	__property bool ChangeDisabledText = {read=FChangeDisabledText, write=SetChangeDisabledText, default=0};
	
public:
	__fastcall virtual TCustomElEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElEdit();
	System::Types::TPoint __fastcall GetNextWord(const System::Types::TPoint &ACaret);
	System::Types::TPoint __fastcall GetPrevWord(const System::Types::TPoint &ACaret);
	System::Types::TPoint __fastcall CaretFromChar(const int CharNum);
	int __fastcall CharFromCaret(const int X, const int Y);
	virtual System::Types::TPoint __fastcall PosFromCaret(const int X, const int Y);
	virtual void __fastcall CaretFromPos(const System::Types::TPoint &APos, int &ACaretX, int &ACaretY);
	void __fastcall SelectAll();
	void __fastcall CutToClipboard();
	void __fastcall CopyToClipboard();
	void __fastcall PasteFromClipboard();
	void __fastcall Undo();
	void __fastcall Scroll(TElEditScrollDir ScrollDir);
	void __fastcall ScrollCaret();
	bool __fastcall GetCanUndo();
	virtual void __fastcall SetBounds(int ALeft, int ATop, int AWidth, int AHeight);
	__property int SelStart = {read=GetSelStart, write=SetSelStart, nodefault};
	__property int SelLength = {read=FSelLength, write=SetSelLength, nodefault};
	__property Elstrutils::TElFString SelText = {read=GetSelectedText, write=SetSelText};
	__property bool Modified = {read=FModified, write=SetModified, default=0};
	__property Elstrutils::TElFString SelectedText = {read=GetSelectedText};
	__property bool MouseOver = {read=FMouseOver, nodefault};
	__property int LinesCount = {read=GetLinesCount, nodefault};
	__property int TopLine = {read=FTopLine, write=SetTopLine, nodefault};
	__property int LeftChar = {read=FLeftChar, write=SetLeftChar, nodefault};
	__property int LeftMargin = {read=FLeftMargin, write=SetLeftMargin, default=1};
	__property int RightMargin = {read=FRightMargin, write=SetRightMargin, default=2};
	__property bool HandleDialogKeys = {read=FHandleDialogKeys, write=FHandleDialogKeys, default=0};
	__property Elstrutils::TElFString Text = {read=GetText, write=SetText};
	__property int TopMargin = {read=FTopMargin, write=SetTopMargin, default=1};
	__property bool AlignBottom = {read=FAlignBottom, write=SetAlignBottom, default=1};
	__property int CaretX = {read=FCaretX, write=SetCaretX, nodefault};
	__property int CaretY = {read=FCaretY, write=SetCaretY, nodefault};
	__property System::Types::TPoint CaretXY = {read=GetCaretXY, write=SetCaretXY};
	__property bool CanUndo = {read=GetCanUndo, nodefault};
	
__published:
	__property Elscrollbar::TElScrollBarStyles* VertScrollBarStyles = {read=FVertScrollBarStyles, write=SetVertScrollBarStyles};
	__property Elscrollbar::TElScrollBarStyles* HorzScrollBarStyles = {read=FHorzScrollBarStyles, write=SetHorzScrollBarStyles};
	__property bool UseCustomScrollBars = {read=FUseCustomScrollBars, write=SetUseCustomScrollBars, nodefault};
	__property bool NotifyUserChangeOnly = {read=FNotifyUserChangeOnly, write=FNotifyUserChangeOnly, nodefault};
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property TabStop = {default=1};
	__property UseXPThemes = {default=1};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElEdit(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElEdit : public TCustomElEdit
{
	typedef TCustomElEdit inherited;
	
__published:
	__property AutoSize = {default=1};
	__property Alignment;
	__property AlignBottom = {default=1};
	__property Background;
	__property BorderSides;
	__property CharCase = {default=0};
	__property ChangeDisabledText = {default=0};
	__property UseBackground = {default=0};
	__property RTLContent;
	__property PasswordChar = {default=0};
	__property MaxLength = {default=0};
	__property Transparent;
	__property FlatFocusedScrollBars = {default=0};
	__property ReadOnly = {default=0};
	__property WantTabs = {default=0};
	__property LeftMargin = {default=1};
	__property RightMargin = {default=2};
	__property TopMargin = {default=1};
	__property BorderStyle;
	__property AutoSelect = {default=0};
	__property HandleDialogKeys = {default=0};
	__property HideSelection = {default=1};
	__property TabSpaces = {default=4};
	__property Lines = {stored=false};
	__property Text;
	__property ImageForm;
	__property ActiveBorderType = {default=1};
	__property Flat = {default=0};
	__property InactiveBorderType = {default=3};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property MaxUndoLevel = {default=10};
	__property WordWrap = {default=0};
	__property Multiline = {default=0};
	__property ScrollBars = {default=0};
	__property VertScrollBarStyles;
	__property HorzScrollBarStyles;
	__property UseCustomScrollBars;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property OnChange;
	__property OnSelectionChange;
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
	/* TCustomElEdit.Create */ inline __fastcall virtual TElEdit(System::Classes::TComponent* AOwner) : TCustomElEdit(AOwner) { }
	/* TCustomElEdit.Destroy */ inline __fastcall virtual ~TElEdit() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElEdit(HWND ParentWindow) : TCustomElEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool RepaintAll;
extern DELPHI_PACKAGE bool FlagEdit;
static const System::WideChar ElFSpace = (System::WideChar)(0x20);
static const System::WideChar ElFTab = (System::WideChar)(0x9);
static const System::WideChar ElFCR = (System::WideChar)(0xd);
static const System::WideChar ElFLF = (System::WideChar)(0xa);
#define ElFCRLF L"\r\n"
static const System::Word ID_UNDO = System::Word(0x304);
static const System::Word ID_CUT = System::Word(0x300);
static const System::Word ID_COPY = System::Word(0x301);
static const System::Word ID_PASTE = System::Word(0x302);
static const System::Word ID_DELETE = System::Word(0x303);
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Eledits */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELEDITS)
using namespace Eledits;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EleditsHPP
