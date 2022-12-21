// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElMenus.pas' rev: 34.00 (Windows)

#ifndef ElmenusHPP
#define ElmenusHPP

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
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Clipbrd.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.ImgList.hpp>
#include <Winapi.CommCtrl.hpp>
#include <ElTools.hpp>
#include <ElImgFrm.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElVCLUtils.hpp>
#include <ElXPThemedControl.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElStrUtils.hpp>
#include <ElColor.hpp>
#include <ElHook.hpp>
#include <ElUnicodeStrings.hpp>
#include <HTMLRender.hpp>
#include <ElList.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elmenus
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EMenuError;
class DELPHICLASS THackClass;
class DELPHICLASS TElMenu;
class DELPHICLASS TElMenuItem;
class DELPHICLASS TElMainMenu;
class DELPHICLASS TElPopupMenu;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EMenuError : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EMenuError(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EMenuError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EMenuError(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EMenuError(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EMenuError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EMenuError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EMenuError(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EMenuError(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EMenuError(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EMenuError(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EMenuError(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EMenuError(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EMenuError() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TMenuBreak : unsigned char { mbNone, mbBreak, mbBarBreak };

typedef void __fastcall (__closure *TMenuChangeEvent)(System::TObject* Sender, TElMenuItem* Source, bool Rebuild);

typedef void __fastcall (__closure *TMenuDrawItemEvent)(System::TObject* Sender, Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &ARect, bool Selected);

typedef void __fastcall (__closure *TMenuMeasureItemEvent)(System::TObject* Sender, Vcl::Graphics::TCanvas* ACanvas, int &Width, int &Height);

enum DECLSPEC_DENUM TMenuItemAutoFlag : unsigned char { maAutomatic, maManual, maParent };

typedef TMenuItemAutoFlag TMenuAutoFlag;

enum DECLSPEC_DENUM TTrackButton : unsigned char { tbRightButton, tbLeftButton };

enum DECLSPEC_DENUM TMenuAnimations : unsigned char { maLeftToRight, maRightToLeft, maTopToBottom, maBottomToTop, maNone };

typedef System::Set<TMenuAnimations, TMenuAnimations::maLeftToRight, TMenuAnimations::maNone> TMenuAnimation;

class PASCALIMPLEMENTATION THackClass : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Vcl::Menus::TMenuItem* FItems;
public:
	/* TComponent.Create */ inline __fastcall virtual THackClass(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~THackClass() { }
	
};


class PASCALIMPLEMENTATION TElMenu : public Vcl::Menus::TMenu
{
	typedef Vcl::Menus::TMenu inherited;
	
public:
	/* TMenu.Create */ inline __fastcall virtual TElMenu(System::Classes::TComponent* AOwner) : Vcl::Menus::TMenu(AOwner) { }
	/* TMenu.Destroy */ inline __fastcall virtual ~TElMenu() { }
	
};


enum DECLSPEC_DENUM TDrawStyle : unsigned char { tdsNormal, tdsOfficeXP, tdsWindowsXP };

class PASCALIMPLEMENTATION TElMenuItem : public Vcl::Menus::TMenuItem
{
	typedef Vcl::Menus::TMenuItem inherited;
	
public:
	TElMenuItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	Vcl::Imglist::TCustomImageList* FSubMenuImages;
	Elstrutils::TElFString FCaption;
	HMENU FHandle;
	bool FChecked;
	bool FEnabled;
	bool FDefault;
	bool FRadioItem;
	bool FVisible;
	System::Byte FGroupIndex;
	int FImageIndex;
	TMenuBreak FBreak;
	Vcl::Graphics::TBitmap* FBitmap;
	Vcl::Graphics::TBitmap* FDisBitmap;
	System::Word FCommand;
	System::Classes::THelpContext FHelpContext;
	Elstrutils::TElFString FHint;
	Ellist::TElList* FItems;
	System::Classes::TShortCut FShortCut;
	TElMenuItem* FParent;
	TElMenuItem* FMerged;
	TElMenuItem* FMergedWith;
	TElMenu* FMenu;
	bool FStreamedRebuild;
	TMenuChangeEvent FOnChange;
	System::Classes::TNotifyEvent FOnClick;
	TMenuDrawItemEvent FOnDrawItem;
	TMenuMeasureItemEvent FOnMeasureItem;
	HIDESBASE void __fastcall AppendTo(HMENU Menu, bool ARightToLeft);
	HIDESBASE void __fastcall ReadShortCutText(System::Classes::TReader* Reader);
	HIDESBASE void __fastcall MergeWith(TElMenuItem* Menu);
	HIDESBASE void __fastcall RebuildHandle();
	HIDESBASE void __fastcall PopulateMenu();
	HIDESBASE void __fastcall SubItemChanged(System::TObject* Sender, TElMenuItem* Source, bool Rebuild);
	HIDESBASE void __fastcall TurnSiblingsOff();
	void __fastcall WriteShortCutText(System::Classes::TWriter* Writer);
	HIDESBASE void __fastcall VerifyGroupIndex(int Position, System::Byte Value);
	HIDESBASE void __fastcall SetSubMenuImages(Vcl::Imglist::TCustomImageList* Value);
	HIDESBASE Vcl::Graphics::TBitmap* __fastcall GetBitmap();
	HIDESBASE void __fastcall SetBitmap(Vcl::Graphics::TBitmap* Value);
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	HIDESBASE void __fastcall ImageListChange(System::TObject* Sender);
	virtual void __fastcall AssignTo(System::Classes::TPersistent* Dest);
	HIDESBASE void __fastcall DoDrawText(Vcl::Graphics::TCanvas* ACanvas, const Elstrutils::TElFString ACaption, System::Types::TRect &Rect, bool Selected, int Flags);
	HIDESBASE void __fastcall DrawItem(Vcl::Graphics::TCanvas* ACanvas, const System::Types::TRect &ARect, bool Selected);
	HIDESBASE HMENU __fastcall GetHandle();
	HIDESBASE int __fastcall GetCount();
	DYNAMIC void __fastcall GetChildren(System::Classes::TGetChildProc Proc, System::Classes::TComponent* Root);
	HIDESBASE TElMenuItem* __fastcall GetItem(int Index);
	HIDESBASE int __fastcall GetMenuIndex();
	HIDESBASE void __fastcall MeasureItem(Vcl::Graphics::TCanvas* ACanvas, int &Width, int &Height);
	virtual void __fastcall MenuChanged(bool Rebuild);
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE void __fastcall SetBreak(TMenuBreak Value);
	HIDESBASE void __fastcall SetCaption(const Elstrutils::TElFString Value);
	HIDESBASE void __fastcall SetChecked(bool Value);
	DYNAMIC void __fastcall SetChildOrder(System::Classes::TComponent* Child, int Order);
	HIDESBASE void __fastcall SetDefault(bool Value);
	HIDESBASE void __fastcall SetEnabled(bool Value);
	HIDESBASE void __fastcall SetGroupIndex(System::Byte Value);
	HIDESBASE void __fastcall SetImageIndex(int Value);
	HIDESBASE void __fastcall SetMenuIndex(int Value);
	DYNAMIC void __fastcall SetParentComponent(System::Classes::TComponent* Value);
	HIDESBASE void __fastcall SetRadioItem(bool Value);
	HIDESBASE void __fastcall SetShortCut(System::Classes::TShortCut Value);
	HIDESBASE void __fastcall SetVisible(bool Value);
	HIDESBASE void __fastcall UpdateItems();
	int __fastcall GetImageWidth();
	void __fastcall SetHint(Elstrutils::TElFString Value);
	void __fastcall UpdateCommand();
	
public:
	__fastcall virtual TElMenuItem(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElMenuItem();
	void __fastcall DesignRebuild();
	HIDESBASE void __fastcall Insert(int Index, TElMenuItem* Item);
	HIDESBASE void __fastcall Delete(int Index);
	virtual void __fastcall Click();
	HIDESBASE TElMenuItem* __fastcall Find(Elstrutils::TElFString ACaption);
	HIDESBASE int __fastcall IndexOf(TElMenuItem* Item);
	DYNAMIC System::Classes::TComponent* __fastcall GetParentComponent();
	HIDESBASE TElMenu* __fastcall GetParentMenu();
	DYNAMIC bool __fastcall HasParent();
	HIDESBASE void __fastcall Add(TElMenuItem* Item);
	HIDESBASE void __fastcall Remove(TElMenuItem* Item);
	HIDESBASE void __fastcall Clear();
	__property HMENU Handle = {read=GetHandle, nodefault};
	__property int Count = {read=GetCount, nodefault};
	__property TElMenuItem* Items[int Index] = {read=GetItem/*, default*/};
	__property int MenuIndex = {read=GetMenuIndex, write=SetMenuIndex, nodefault};
	__property TElMenuItem* Parent = {read=FParent};
	
__published:
	__property Vcl::Imglist::TCustomImageList* SubMenuImages = {read=FSubMenuImages, write=SetSubMenuImages};
	__property Vcl::Graphics::TBitmap* Bitmap = {read=GetBitmap, write=SetBitmap};
	__property TMenuBreak Break = {read=FBreak, write=SetBreak, default=0};
	__property Elstrutils::TElFString Caption = {read=FCaption, write=SetCaption};
	__property bool Checked = {read=FChecked, write=SetChecked, default=0};
	__property bool Default = {read=FDefault, write=SetDefault, default=0};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property System::Byte GroupIndex = {read=FGroupIndex, write=SetGroupIndex, default=0};
	__property System::Classes::THelpContext HelpContext = {read=FHelpContext, write=FHelpContext, default=0};
	__property Elstrutils::TElFString Hint = {read=FHint, write=SetHint};
	__property int ImageIndex = {read=FImageIndex, write=SetImageIndex, default=-1};
	__property bool RadioItem = {read=FRadioItem, write=SetRadioItem, default=0};
	__property System::Classes::TShortCut ShortCut = {read=FShortCut, write=SetShortCut, default=0};
	__property bool Visible = {read=FVisible, write=SetVisible, default=1};
	__property System::Classes::TNotifyEvent OnClick = {read=FOnClick, write=FOnClick};
	__property TMenuDrawItemEvent OnDrawItem = {read=FOnDrawItem, write=FOnDrawItem};
	__property TMenuMeasureItemEvent OnMeasureItem = {read=FOnMeasureItem, write=FOnMeasureItem};
	__property TMenuChangeEvent OnChange = {read=FOnChange, write=FOnChange};
};


class PASCALIMPLEMENTATION TElMainMenu : public Vcl::Menus::TMainMenu
{
	typedef Vcl::Menus::TMainMenu inherited;
	
private:
	HMENU FOle2Menu;
	Elstrutils::TElFString FMenuImage;
	TElMenuItem* FUnicodeItems;
	Elhook::TElHook* FHook;
	TDrawStyle FDrawStyle;
	Vcl::Graphics::TFont* FFont;
	Vcl::Forms::TForm* FForm;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	Vcl::Controls::TImageList* FImages;
	bool FOwnerDraw;
	bool FRightToLeft;
	bool FIsHTML;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Htmlrender::TElHTMLRender* FRender;
	void __fastcall SetIsHTML(bool Value);
	HIDESBASE void __fastcall SetOwnerDraw(bool Value);
	HIDESBASE void __fastcall ImageListChange(System::TObject* Sender);
	HIDESBASE void __fastcall SetImages(Vcl::Controls::TImageList* Value);
	HIDESBASE void __fastcall ItemChanged();
	void __fastcall OnBeforeHook(System::TObject* Sender, Winapi::Messages::TMessage &Message, bool &Handled);
	void __fastcall SetDrawStyle(TDrawStyle Value);
	void __fastcall SetFont(Vcl::Graphics::TFont* const Value);
	HIDESBASE bool __fastcall UpdateImage();
	
protected:
	bool FSystemFont;
	HIDESBASE bool __fastcall IsOwnerDraw();
	HIDESBASE void __fastcall ProcessMenuChar(Winapi::Messages::TWMMenuChar &Message);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE int __fastcall DoGetMenuString(HMENU Menu, unsigned ItemID, System::WideChar * Str, int MaxCount, unsigned Flag);
	HIDESBASE void __fastcall MenuChanged(System::TObject* Sender, Vcl::Menus::TMenuItem* Source, bool Rebuild);
	virtual HMENU __fastcall GetHandle();
	HIDESBASE bool __fastcall DispatchCommand(System::Word ACommand);
	virtual void __fastcall Loaded();
	void __fastcall SetRightToLeft(bool Value);
	void __fastcall SetSystemFont(bool Value);
	void __fastcall GetFont();
	void __fastcall FontChange(System::TObject* Sender);
	void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	void __fastcall UpdateCommands();
	
public:
	__fastcall virtual TElMainMenu(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElMainMenu();
	HIDESBASE TElMenuItem* __fastcall FindItem(int Value, Vcl::Menus::TFindItemKind Kind);
	HIDESBASE bool __fastcall IsShortCut(Winapi::Messages::TWMKey &Message);
	HIDESBASE void __fastcall UpdateItems();
	HIDESBASE bool __fastcall DispatchPopup(HMENU AHandle);
	
__published:
	__property TElMenuItem* Items = {read=FUnicodeItems};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property TDrawStyle DrawStyle = {read=FDrawStyle, write=SetDrawStyle, default=0};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property bool OwnerDraw = {read=FOwnerDraw, write=SetOwnerDraw, default=0};
	__property bool RightToLeft = {read=FRightToLeft, write=SetRightToLeft, default=0};
	__property bool SystemFont = {read=FSystemFont, write=SetSystemFont, default=1};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, default=0};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
};


class PASCALIMPLEMENTATION TElPopupMenu : public Vcl::Menus::TPopupMenu
{
	typedef Vcl::Menus::TPopupMenu inherited;
	
private:
	bool FIsHTML;
	Htmlrender::TElHTMLRender* FRender;
	Htmlrender::TElHTMLImageNeededEvent FOnImageNeeded;
	Elhook::TElHook* FHook;
	TDrawStyle FDrawStyle;
	TElMenuItem* FUnicodeItems;
	System::Types::TPoint FPopupPoint;
	Vcl::Forms::TForm* FForm;
	Vcl::Graphics::TFont* FFont;
	TMenuAnimation FMenuAnimation;
	TTrackButton FTrackButton;
	Vcl::Imglist::TChangeLink* FImageChangeLink;
	Vcl::Controls::TImageList* FImages;
	bool FOwnerDraw;
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	HIDESBASE void __fastcall ImageListChange(System::TObject* Sender);
	HIDESBASE void __fastcall SetImages(Vcl::Controls::TImageList* Value);
	HIDESBASE void __fastcall SetOwnerDraw(bool Value);
	void __fastcall SetDrawStyle(TDrawStyle Value);
	void __fastcall OnBeforeHook(System::TObject* Sender, Winapi::Messages::TMessage &Message, bool &Handled);
	void __fastcall SetFont(Vcl::Graphics::TFont* const Value);
	void __fastcall SetIsHTML(bool Value);
	
protected:
	bool FSystemFont;
	HIDESBASE void __fastcall DoPopup(System::TObject* Sender);
	HIDESBASE bool __fastcall IsOwnerDraw();
	HIDESBASE bool __fastcall DispatchCommand(System::Word ACommand);
	HIDESBASE virtual HMENU __fastcall GetHandle();
	void __fastcall GetFont();
	void __fastcall SetSystemFont(bool Value);
	void __fastcall FontChange(System::TObject* Sender);
	virtual void __fastcall Loaded();
	void __fastcall TriggerImageNeededEvent(System::TObject* Sender, Elstrutils::TElFString Src, Vcl::Graphics::TBitmap* &Image);
	void __fastcall UpdateCommands();
	
public:
	__fastcall virtual TElPopupMenu(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElPopupMenu();
	virtual void __fastcall Popup(int X, int Y);
	HIDESBASE TElMenuItem* __fastcall FindItem(int Value, Vcl::Menus::TFindItemKind Kind);
	HIDESBASE bool __fastcall IsShortCut(Winapi::Messages::TWMKey &Message);
	HIDESBASE void __fastcall ProcessMenuChar(Winapi::Messages::TWMMenuChar &Message);
	HIDESBASE void __fastcall UpdateItems();
	HIDESBASE bool __fastcall DispatchPopup(HMENU AHandle);
	__property System::Types::TPoint PopupPoint = {read=FPopupPoint};
	__property Handle = {read=GetHandle};
	
__published:
	__property TElMenuItem* Items = {read=FUnicodeItems};
	__property Vcl::Graphics::TFont* Font = {read=FFont, write=SetFont};
	__property TDrawStyle DrawStyle = {read=FDrawStyle, write=SetDrawStyle, default=0};
	__property TMenuAnimation MenuAnimation = {read=FMenuAnimation, write=FMenuAnimation, default=0};
	__property bool OwnerDraw = {read=FOwnerDraw, write=SetOwnerDraw, default=0};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property TTrackButton TrackButton = {read=FTrackButton, write=FTrackButton, default=0};
	__property bool SystemFont = {read=FSystemFont, write=SetSystemFont, default=1};
	__property bool IsHTML = {read=FIsHTML, write=SetIsHTML, default=0};
	__property Htmlrender::TElHTMLImageNeededEvent OnImageNeeded = {read=FOnImageNeeded, write=FOnImageNeeded};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE NativeUInt FTheme;
extern DELPHI_PACKAGE HFONT __fastcall GetMenuFont(void);
extern DELPHI_PACKAGE void __fastcall CopyMenuItems(TElMenuItem* Dest, TElMenuItem* Source);
extern DELPHI_PACKAGE void __fastcall InsertItems(Vcl::Menus::TMenu* &AMenu, TElMenuItem* MainItem, int Index, TElMenuItem* *Items, const int Items_High);
extern DELPHI_PACKAGE void __fastcall InsertMenuItems(Vcl::Menus::TMenu* &AMenu, int Index, TElMenuItem* *Items, const int Items_High);
extern DELPHI_PACKAGE void __fastcall ElInitMenuItems(Vcl::Menus::TMenu* AMenu, TElMenuItem* *Items, const int Items_High);
extern DELPHI_PACKAGE TElMainMenu* __fastcall ElNewMenu(System::Classes::TComponent* Owner, const Elstrutils::TElFString AName, TElMenuItem* *Items, const int Items_High);
extern DELPHI_PACKAGE TElMenuItem* __fastcall ElNewSubMenu(const Elstrutils::TElFString ACaption, System::Word hCtx, const Elstrutils::TElFString AName, TElMenuItem* *Items, const int Items_High, bool AEnabled);
extern DELPHI_PACKAGE TElMenuItem* __fastcall ElNewItem(const Elstrutils::TElFString ACaption, System::Classes::TShortCut AShortCut, bool AChecked, bool AEnabled, System::Classes::TNotifyEvent AOnClick, System::Word hCtx, const Elstrutils::TElFString AName);
extern DELPHI_PACKAGE TElMenuItem* __fastcall ElNewLine(void);
extern DELPHI_PACKAGE Elstrutils::TElFString __fastcall ElGetHotkey(const Elstrutils::TElFString Text);
extern DELPHI_PACKAGE Elstrutils::TElFString __fastcall ElStripHotKey(const Elstrutils::TElFString Text);
}	/* namespace Elmenus */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELMENUS)
using namespace Elmenus;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElmenusHPP
