// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElAppBar.pas' rev: 34.00 (Windows)

#ifndef ElappbarHPP
#define ElappbarHPP

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
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Winapi.ShellAPI.hpp>
#include <Vcl.Menus.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elappbar
{
//-- forward type declarations -----------------------------------------------
struct TAppBarSettings;
class DELPHICLASS TElAppBar;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TAppBarMessage : unsigned char { abmNew, abmRemove, abmQueryPos, abmSetPos, abmGetState, abmGetTaskBarPos, abmActivate, abmGetAutoHideBar, abmSetAutoHideBar, abmWindowPosChanged };

enum DECLSPEC_DENUM TAppBarEdge : unsigned char { abeLeft, abeTop, abeRight, abeBottom, abeUnknown, abeFloat };

enum DECLSPEC_DENUM TAppBarFlag : unsigned char { abfAllowLeft, abfAllowTop, abfAllowRight, abfAllowBottom, abfAllowFloat };

typedef System::Set<TAppBarFlag, TAppBarFlag::abfAllowLeft, TAppBarFlag::abfAllowFloat> TAppBarFlags;

enum DECLSPEC_DENUM TAppBarTaskEntry : unsigned char { abtShow, abtHide, abtFloatDependent };

struct DECLSPEC_DRECORD TAppBarSettings
{
public:
	TAppBarEdge abEdge;
	bool bAutohide;
	bool bAlwaysOnTop;
	bool bSlideEffect;
	int nTimerInterval;
	System::Types::TRect rcDockDims;
	System::Types::TRect rcFloat;
	int nMinWidth;
	int nMinHeight;
	int nMaxWidth;
	int nMaxHeight;
	TAppBarTaskEntry abTaskEntry;
};


class PASCALIMPLEMENTATION TElAppBar : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
private:
	bool FInPosChanged;
	
protected:
	bool FForceReHide;
	bool FPreventOffScreen;
	bool FKeepSize;
	System::Types::TPoint FDraggingOffset;
	TAppBarSettings FABS;
	TAppBarFlags FabFlags;
	System::Types::TSize FszSizeInc;
	TAppBarEdge FabEdgeProposedPrev;
	bool FbFullScreenAppOpen;
	bool FbAutoHideIsVisible;
	Vcl::Extctrls::TTimer* FTimer;
	System::Classes::TNotifyEvent FOnEdgeChanged;
	unsigned __fastcall AppBarMessage(TAppBarMessage abMessage, TAppBarEdge abEdge, NativeInt lParam, bool bRect, System::Types::TRect &rc);
	unsigned __fastcall AppBarMessage1(TAppBarMessage abMessage);
	unsigned __fastcall AppBarMessage2(TAppBarMessage abMessage, TAppBarEdge abEdge);
	unsigned __fastcall AppBarMessage3(TAppBarMessage abMessage, TAppBarEdge abEdge, NativeInt lParam);
	unsigned __fastcall AppBarMessage4(TAppBarMessage abMessage, TAppBarEdge abEdge, NativeInt lParam, System::Types::TRect &rc);
	TAppBarEdge __fastcall CalcProposedState(System::Types::TSmallPoint &pt);
	bool __fastcall AdjustLocationForAutohide(bool bShow, System::Types::TRect &rc);
	void __fastcall SlideWindow(System::Types::TRect &rcEnd);
	TAppBarEdge __fastcall GetAutohideEdge();
	System::Types::TSmallPoint __fastcall GetMessagePosition();
	void __fastcall SetKeepSize(bool newValue);
	void __fastcall SetPreventOffScreen(bool newValue);
	void __fastcall SetHorzInc(int newValue);
	void __fastcall SetVertInc(int newValue);
	int __fastcall GetVertInc();
	int __fastcall GetHorzInc();
	void __fastcall DoEdgeChanged();
	virtual void __fastcall GetRect(TAppBarEdge abEdgeProposed, System::Types::TRect &rcProposed);
	TAppBarEdge __fastcall GetEdge();
	void __fastcall SetEdge(TAppBarEdge abEdge);
	void __fastcall SetSlideTime(int nInterval);
	bool __fastcall IsAutoHide();
	void __fastcall SetAutoHide(bool bAutoHide);
	bool __fastcall IsAlwaysOnTop();
	void __fastcall SetAlwaysOnTop(bool bAlwaysOnTop);
	void __fastcall SetFlags(TAppBarFlags newValue);
	virtual void __fastcall OnAppBarForcedToDocked();
	virtual void __fastcall OnABNFullScreenApp(bool bOpen);
	virtual void __fastcall OnABNPosChanged();
	virtual void __fastcall OnABNWindowArrange(bool bBeginning);
	MESSAGE void __fastcall OnAppBarCallbackMsg(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WmCreate(Winapi::Messages::TWMCreate &Msg);
	HIDESBASE MESSAGE void __fastcall WmDestroy(Winapi::Messages::TWMNoParams &Msg);
	MESSAGE void __fastcall OnWindowPosChanged(Winapi::Messages::TWMWindowPosMsg &Msg);
	HIDESBASE MESSAGE void __fastcall OnActivate(Winapi::Messages::TWMActivate &Msg);
	void __fastcall OnAppBarTimer(System::TObject* Sender);
	MESSAGE void __fastcall OnNcMouseMove(Winapi::Messages::TWMNCHitMessage &Msg);
	HIDESBASE MESSAGE void __fastcall OnMouseMove(Winapi::Messages::TWMMouse &Msg);
	MESSAGE void __fastcall OnNcHitTest(Winapi::Messages::TWMNCHitTest &Msg);
	MESSAGE void __fastcall OnEnterSizeMove(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall OnExitSizeMove(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall OnMoving(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall OnSizing(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall OnGetMinMaxInfo(Winapi::Messages::TWMGetMinMaxInfo &Msg);
	bool __fastcall IsEdgeLeftOrRight(TAppBarEdge abEdge);
	bool __fastcall IsEdgeTopOrBottom(TAppBarEdge abEdge);
	bool __fastcall IsFloating(TAppBarEdge abEdge);
	bool __fastcall IsDockable(TAppBarFlags abFlags);
	bool __fastcall IsDockableVertically(TAppBarFlags abFlags);
	bool __fastcall IsDockableHorizontally(TAppBarFlags abFlags);
	void __fastcall ResetSystemKnowledge();
	TAppBarEdge __fastcall GetEdgeFromPoint(TAppBarFlags abFlags, System::Types::TSmallPoint pt);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	
public:
	System::Types::TRect __fastcall GetDockDims();
	void __fastcall SetDockDims(const System::Types::TRect &rc);
	System::Types::TRect __fastcall GetFloatRect();
	void __fastcall SetFloatRect(const System::Types::TRect &rc);
	__fastcall virtual TElAppBar(System::Classes::TComponent* Owner);
	__fastcall virtual ~TElAppBar();
	void __fastcall UpdateBar();
	virtual void __fastcall ShowHiddenAppBar(bool bShow);
	__property bool AutoHideIsVisible = {read=FbAutoHideIsVisible, nodefault};
	__property System::Types::TRect DockDims = {read=GetDockDims, write=SetDockDims};
	__property System::Types::TRect FloatRect = {read=GetFloatRect, write=SetFloatRect};
	
__published:
	__property TAppBarFlags Flags = {read=FabFlags, write=SetFlags, nodefault};
	__property int HorzSizeInc = {read=GetHorzInc, write=SetHorzInc, nodefault};
	__property int VertSizeInc = {read=GetVertInc, write=SetVertInc, nodefault};
	__property TAppBarEdge Edge = {read=GetEdge, write=SetEdge, nodefault};
	__property bool AutoHide = {read=IsAutoHide, write=SetAutoHide, nodefault};
	__property bool AlwaysOnTop = {read=IsAlwaysOnTop, write=SetAlwaysOnTop, nodefault};
	__property int MinWidth = {read=FABS.nMinWidth, write=FABS.nMinWidth, nodefault};
	__property int MinHeight = {read=FABS.nMinHeight, write=FABS.nMinHeight, nodefault};
	__property int MaxWidth = {read=FABS.nMaxWidth, write=FABS.nMaxWidth, nodefault};
	__property int MaxHeight = {read=FABS.nMaxHeight, write=FABS.nMaxHeight, nodefault};
	__property TAppBarTaskEntry TaskEntry = {read=FABS.abTaskEntry, write=FABS.abTaskEntry, nodefault};
	__property bool KeepSize = {read=FKeepSize, write=SetKeepSize, nodefault};
	__property bool PreventOffScreen = {read=FPreventOffScreen, write=SetPreventOffScreen, default=0};
	__property System::Classes::TNotifyEvent OnEdgeChanged = {read=FOnEdgeChanged, write=FOnEdgeChanged};
public:
	/* TCustomForm.CreateNew */ inline __fastcall virtual TElAppBar(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElAppBar(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word WM_APPBARNOTIFY = System::Word(0x464);
static const System::Word SLIDE_DEF_TIMER_INTERVAL = System::Word(0x190);
static const System::Int8 AB_DEF_SIZE_INC = System::Int8(0x1);
static const System::Int8 AB_DEF_DOCK_DIM = System::Int8(0x20);
}	/* namespace Elappbar */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELAPPBAR)
using namespace Elappbar;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElappbarHPP
