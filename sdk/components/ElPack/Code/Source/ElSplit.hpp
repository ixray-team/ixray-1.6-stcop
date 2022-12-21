// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElSplit.pas' rev: 34.00 (Windows)

#ifndef ElsplitHPP
#define ElsplitHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <ElHook.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <ElUxTheme.hpp>
#include <ElPanel.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elsplit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElSplitter;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TElSplitterEvent)(System::TObject* Sender, int &NewPos, bool &Accept);

class PASCALIMPLEMENTATION TElSplitter : public Elpanel::TCustomElPanel
{
	typedef Elpanel::TCustomElPanel inherited;
	
private:
	bool FSnapTopLeft;
	bool FSnapBottomRight;
	bool FAutoHide;
	HWND FOldFocused;
	bool FDragging;
	int FOffset;
	Vcl::Controls::TControl* FControlTopLeft;
	Vcl::Controls::TControl* FControlBottomRight;
	int FMinSizeTopLeft;
	int FMinSizeBottomRight;
	bool FAutoSnap;
	bool FLineVisible;
	HDC FLineDC;
	int FCurPos;
	int FSizeBeforeSnap;
	bool FLeftSnapButtonPushed;
	bool FLeftSnapButtonPushing;
	bool FRightSnapButtonPushed;
	bool FRightSnapButtonPushing;
	bool FShowSnapButton;
	System::Uitypes::TCursor FSnapButtonCursor;
	System::Uitypes::TColor FSnapButtonColor;
	System::Uitypes::TColor FSnapButtonDotColor;
	System::Uitypes::TColor FSnapButtonArrowColor;
	Elhook::TElHook* FLeftHook;
	Elhook::TElHook* FRightHook;
	System::Classes::TNotifyEvent FOnPositionChanged;
	TElSplitterEvent FOnPositionChanging;
	NativeUInt FArrowTheme;
	void __fastcall AfterMessage(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall SetMinSizeTopLeft(int newValue);
	void __fastcall SetControlTopLeft(Vcl::Controls::TControl* newValue);
	void __fastcall SetControlBottomRight(Vcl::Controls::TControl* newValue);
	void __fastcall DrawLine();
	void __fastcall AllocateLineDC();
	void __fastcall ReleaseLineDC();
	void __fastcall SetAutoHide(bool newValue);
	void __fastcall UpdateAutoVis();
	System::Types::TRect __fastcall GetLeftSnapButtonRect();
	void __fastcall RecalcCurPos(int X, int Y);
	void __fastcall StopMode();
	void __fastcall UpdateShowSnapButton();
	void __fastcall SetSnapBottomRight(const bool Value);
	void __fastcall SetSnapTopLeft(const bool Value);
	void __fastcall SetShowSnapButton(const bool Value);
	void __fastcall DoSizing(int L);
	void __fastcall SetSnapButtonArrowColor(const System::Uitypes::TColor Value);
	void __fastcall SetSnapButtonColor(const System::Uitypes::TColor Value);
	void __fastcall SetSnapButtonDotColor(const System::Uitypes::TColor Value);
	System::Types::TRect __fastcall GetRightSnapButtonRect();
	
protected:
	bool FSnappedLeft;
	bool FSnappedRight;
	bool FInvertSnapButtons;
	virtual void __fastcall TriggerPositionChangedEvent();
	virtual void __fastcall TriggerPositionChangingEvent(int &NewPos, bool &Accept);
	HIDESBASE MESSAGE void __fastcall WMCancelMode(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetCursor(Winapi::Messages::TWMSetCursor &Msg);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation operation);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall StopSizing(int X, int Y, bool Accept);
	virtual void __fastcall Loaded();
	virtual System::WideString __fastcall GetThemedClassName();
	virtual void __fastcall SetUseXPThemes(const bool Value);
	virtual void __fastcall CreateArrowThemeHandle();
	virtual void __fastcall FreeArrowThemeHandle();
	void __fastcall SetInvertSnapButtons(bool Value);
	
public:
	__fastcall virtual TElSplitter(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElSplitter();
	virtual void __fastcall Paint();
	void __fastcall Snap(bool SnapLeft);
	__property NativeUInt ArrowTheme = {read=FArrowTheme, nodefault};
	__property bool SnappedLeft = {read=FSnappedLeft, nodefault};
	__property bool SnappedRight = {read=FSnappedRight, nodefault};
	
__published:
	__property int MinSizeTopLeft = {read=FMinSizeTopLeft, write=SetMinSizeTopLeft, default=20};
	__property int MinSizeBottomRight = {read=FMinSizeBottomRight, write=FMinSizeBottomRight, default=20};
	__property bool AutoSnap = {read=FAutoSnap, write=FAutoSnap, default=0};
	__property bool SnapTopLeft = {read=FSnapTopLeft, write=SetSnapTopLeft, default=1};
	__property bool SnapBottomRight = {read=FSnapBottomRight, write=SetSnapBottomRight, default=1};
	__property Vcl::Controls::TControl* ControlTopLeft = {read=FControlTopLeft, write=SetControlTopLeft};
	__property Vcl::Controls::TControl* ControlBottomRight = {read=FControlBottomRight, write=SetControlBottomRight};
	__property bool AutoHide = {read=FAutoHide, write=SetAutoHide, nodefault};
	__property bool ShowSnapButton = {read=FShowSnapButton, write=SetShowSnapButton, default=0};
	__property System::Uitypes::TCursor SnapButtonCursor = {read=FSnapButtonCursor, write=FSnapButtonCursor, default=0};
	__property System::Uitypes::TColor SnapButtonColor = {read=FSnapButtonColor, write=SetSnapButtonColor, default=-16777196};
	__property System::Uitypes::TColor SnapButtonDotColor = {read=FSnapButtonDotColor, write=SetSnapButtonDotColor, default=-16777200};
	__property System::Uitypes::TColor SnapButtonArrowColor = {read=FSnapButtonArrowColor, write=SetSnapButtonArrowColor, default=-16777200};
	__property Align = {default=0};
	__property BevelInner = {default=0};
	__property BevelOuter = {default=2};
	__property BevelWidth = {default=1};
	__property BorderStyle = {default=0};
	__property BorderWidth = {default=0};
	__property Color = {default=-16777201};
	__property Enabled = {default=1};
	__property ParentColor = {default=0};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property Visible = {default=1};
	__property UseXPThemes = {default=1};
	__property System::Classes::TNotifyEvent OnPositionChanged = {read=FOnPositionChanged, write=FOnPositionChanged};
	__property TElSplitterEvent OnPositionChanging = {read=FOnPositionChanging, write=FOnPositionChanging};
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
	__property bool InvertSnapButtons = {read=FInvertSnapButtons, write=SetInvertSnapButtons, default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElSplitter(HWND ParentWindow) : Elpanel::TCustomElPanel(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elsplit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELSPLIT)
using namespace Elsplit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElsplitHPP
