// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElTray.pas' rev: 35.00 (Windows)

#ifndef EltrayHPP
#define EltrayHPP

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
#include <Vcl.Graphics.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Forms.hpp>
#include <ElBaseComp.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ImgList.hpp>
#include <ElVCLUtils.hpp>
#include <ElHook.hpp>
#include <ElTools.hpp>
#include <ElTimers.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.ShellAPI.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eltray
{
//-- forward type declarations -----------------------------------------------
struct TClickInfo;
class DELPHICLASS TElTrayIcon;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TElTrayExtHintShowEvent)(System::TObject* Sender, bool &DoShow);

typedef void __fastcall (__closure *TElTrayExtHintCreateEvent)(System::TObject* Sender, System::UnicodeString FormClass, Vcl::Forms::TForm* &Form);

typedef void __fastcall (__closure *TQueryEndSessionEvent)(System::TObject* Sender, int Action, bool &CanClose);

struct DECLSPEC_DRECORD TClickInfo
{
public:
	System::Uitypes::TMouseButton Button;
	System::Classes::TShiftState Shift;
	int X;
	int Y;
};


class PASCALIMPLEMENTATION TElTrayIcon : public Elbasecomp::TElBaseComponent
{
	typedef Elbasecomp::TElBaseComponent inherited;
	
private:
	unsigned FExtHintWndStyle;
	unsigned FExtHintWndExStyle;
	TElTrayExtHintShowEvent FOnBeforeExtendedHintShow;
	TElTrayExtHintCreateEvent FOnExtHintFormCreate;
	bool FHideTaskBarIcon;
	unsigned FExtendedHintDelay;
	TQueryEndSessionEvent FOnQueryEndSession;
	bool FPopupWhenModal;
	System::UnicodeString FHint;
	Vcl::Graphics::TIcon* FStaticIcon;
	bool FAnimated;
	Vcl::Graphics::TIcon* FIcon;
	Vcl::Controls::TImageList* FIcons;
	int FInterval;
	Vcl::Menus::TPopupMenu* FPopupMenu;
	TClickInfo FClickInfo;
	Eltimers::TElTimerPoolItem* FClickTimer;
	Eltimers::TElTimerPoolItem* FHintTimer;
	Eltimers::TElTimerPoolItem* FTimer;
	Eltimers::TElTimerPool* FTimerPool;
	int FImgIdx;
	Vcl::Controls::TMouseEvent FOnClick;
	System::Classes::TNotifyEvent FOnDblClick;
	Vcl::Controls::TMouseEvent FOnMouseDown;
	Vcl::Controls::TMouseEvent FOnMouseUp;
	Vcl::Controls::TMouseMoveEvent FOnMouseMove;
	Vcl::Imglist::TChangeLink* StaticChangeLink;
	Vcl::Imglist::TChangeLink* MyChangeLink;
	bool FSet;
	_NOTIFYICONDATAW FIconData;
	bool FLClick;
	bool FMClick;
	bool FRClick;
	Vcl::Forms::TForm* FExtForm;
	System::UnicodeString FExtFormName;
	System::Types::TPoint FExtFormPt;
	bool FHideForm;
	bool FExtFormInt;
	bool FNoShow;
	Vcl::Controls::TMouseEvent FOnDblClickEx;
	Vcl::Controls::TImageList* FStaticIcons;
	bool FAnimateOnce;
	bool FUseStaticIcons;
	int FStaticIconIndex;
	void __fastcall SetExtForm(System::UnicodeString newValue);
	void __fastcall SetAnimated(bool newValue);
	void __fastcall SetDesignActive(bool newValue);
	void __fastcall SetIcons(Vcl::Controls::TImageList* newValue);
	void __fastcall SetInterval(int newValue);
	void __fastcall SetPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	void __fastcall SetStaticIcon(Vcl::Graphics::TIcon* newValue);
	void __fastcall OnClickTimer(System::TObject* Sender);
	void __fastcall OnTimer(System::TObject* Sender);
	void __fastcall OnHintTimer(System::TObject* Sender);
	void __fastcall OnImagesChange(System::TObject* Sender);
	void __fastcall SetHint(System::UnicodeString newValue);
	bool __fastcall DoPopupMenu(int X, int Y);
	bool __fastcall DoDblClick(bool Perform);
	void __fastcall SetStaticIcons(Vcl::Controls::TImageList* Value);
	void __fastcall OnStaticIconsChange(System::TObject* Sender);
	
protected:
	virtual void __fastcall TriggerClickEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall TriggerDblClickEvent();
	virtual void __fastcall TriggerMouseDownEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall TriggerMouseUpEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall TriggerMouseMoveEvent(System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	bool __fastcall HookWndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall DoSetEnabled(bool AEnabled);
	virtual void __fastcall UpdateIconData(bool remove);
	virtual void __fastcall Loaded();
	virtual void __fastcall TriggerQueryEndSessionEvent(int Action, bool &CanClose);
	virtual void __fastcall SetExtendedHintDelay(unsigned newValue);
	virtual void __fastcall SetHideTaskBarIcon(bool newValue);
	void __fastcall DoHideTaskBarIcon();
	void __fastcall DoShowTaskBarIcon();
	virtual void __fastcall TriggerBeforeExtendedHintShowEvent(bool &DoShow);
	System::Classes::TComponentClass __fastcall FindExtForm(System::UnicodeString Name);
	virtual void __fastcall TriggerDblClickExEvent(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SetUseStaticIcons(bool Value);
	int __fastcall GetStaticIconIndex();
	void __fastcall SetStaticIconIndex(int Value);
	void __fastcall UpdateStaticIcon();
	
public:
	__fastcall virtual TElTrayIcon(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElTrayIcon();
	void __fastcall ShowExtForm(int X, int Y);
	__property Handle;
	__property Vcl::Forms::TForm* ExtendedHintForm = {read=FExtForm};
	__property unsigned ExtHintWndStyle = {read=FExtHintWndStyle, write=FExtHintWndStyle, nodefault};
	__property unsigned ExtHintWndExStyle = {read=FExtHintWndExStyle, write=FExtHintWndExStyle, nodefault};
	
__published:
	__property Vcl::Controls::TImageList* Icons = {read=FIcons, write=SetIcons};
	__property bool Animated = {read=FAnimated, write=SetAnimated, default=0};
	__property bool DesignActive = {read=FDesignActive, write=SetDesignActive, default=0};
	__property int Interval = {read=FInterval, write=SetInterval, default=200};
	__property Vcl::Menus::TPopupMenu* PopupMenu = {read=FPopupMenu, write=SetPopupMenu};
	__property Vcl::Controls::TMouseEvent OnClick = {read=FOnClick, write=FOnClick};
	__property System::Classes::TNotifyEvent OnDblClick = {read=FOnDblClick, write=FOnDblClick};
	__property Vcl::Controls::TMouseEvent OnMouseDown = {read=FOnMouseDown, write=FOnMouseDown};
	__property Vcl::Controls::TMouseEvent OnMouseUp = {read=FOnMouseUp, write=FOnMouseUp};
	__property Vcl::Controls::TMouseMoveEvent OnMouseMove = {read=FOnMouseMove, write=FOnMouseMove};
	__property Vcl::Graphics::TIcon* StaticIcon = {read=FStaticIcon, write=SetStaticIcon};
	__property System::UnicodeString Hint = {read=FHint, write=SetHint};
	__property Enabled = {default=0};
	__property System::UnicodeString ExtendedHint = {read=FExtFormName, write=SetExtForm};
	__property bool ExtendedHintInteractive = {read=FExtFormInt, write=FExtFormInt, default=0};
	__property bool PopupWhenModal = {read=FPopupWhenModal, write=FPopupWhenModal, default=0};
	__property TQueryEndSessionEvent OnQueryEndSession = {read=FOnQueryEndSession, write=FOnQueryEndSession};
	__property unsigned ExtendedHintDelay = {read=FExtendedHintDelay, write=SetExtendedHintDelay, nodefault};
	__property bool HideTaskBarIcon = {read=FHideTaskBarIcon, write=SetHideTaskBarIcon, default=0};
	__property TElTrayExtHintShowEvent OnBeforeExtendedHintShow = {read=FOnBeforeExtendedHintShow, write=FOnBeforeExtendedHintShow};
	__property TElTrayExtHintCreateEvent OnExtHintFormCreate = {read=FOnExtHintFormCreate, write=FOnExtHintFormCreate};
	__property Vcl::Controls::TMouseEvent OnDblClickEx = {read=FOnDblClickEx, write=FOnDblClickEx};
	__property bool AnimateOnce = {read=FAnimateOnce, write=FAnimateOnce, default=0};
	__property Vcl::Controls::TImageList* StaticIcons = {read=FStaticIcons, write=SetStaticIcons};
	__property bool UseStaticIcons = {read=FUseStaticIcons, write=SetUseStaticIcons, default=0};
	__property int StaticIconIndex = {read=GetStaticIconIndex, write=SetStaticIconIndex, default=-1};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE int TrackInterval;
extern DELPHI_PACKAGE int HideInterval;
extern DELPHI_PACKAGE bool FInMenu;
}	/* namespace Eltray */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELTRAY)
using namespace Eltray;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EltrayHPP
