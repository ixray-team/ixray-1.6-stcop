// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElMemoCombo.pas' rev: 34.00 (Windows)

#ifndef ElmemocomboHPP
#define ElmemocomboHPP

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
#include <ElBtnEdit.hpp>
#include <ElACtrls.hpp>
#include <ElVCLUtils.hpp>
#include <ElPopBtn.hpp>
#include <ElTools.hpp>
#include <ElTmSchema.hpp>
#include <ElUxTheme.hpp>
#include <ElEdits.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElXPThemedControl.hpp>
#include <System.UITypes.hpp>
#include <ElStrUtils.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElImgFrm.hpp>
#include <ElSndMap.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elmemocombo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElMemoCombo;
class DELPHICLASS TElHackMemo;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TElMemoComboDropEvent)(System::TObject* Sender, bool &Dropped, System::UnicodeString &Text);

typedef Elactrls::TElAdvancedMemo TElIntMemo;

class PASCALIMPLEMENTATION TElMemoCombo : public Elbtnedit::TCustomElButtonEdit
{
	typedef Elbtnedit::TCustomElButtonEdit inherited;
	
private:
	System::Classes::TNotifyEvent FOnMemoChange;
	System::Classes::TNotifyEvent FOnMemoClick;
	System::Classes::TNotifyEvent FOnMemoDblClick;
	Vcl::Controls::TKeyEvent FOnMemoKeyDown;
	Vcl::Controls::TKeyPressEvent FOnMemoKeyPress;
	Vcl::Controls::TKeyEvent FOnMemoKeyUp;
	Vcl::Controls::TMouseEvent FOnMemoMouseDown;
	Vcl::Controls::TMouseMoveEvent FOnMemoMouseMove;
	Vcl::Controls::TMouseEvent FOnMemoMouseUp;
	Elactrls::TElAdvancedMemo* FMemo;
	System::Classes::TAlignment FMemoAlignment;
	bool FDropAutoWidth;
	int FDropHeight;
	int FDropWidth;
	bool FDropped;
	TElMemoComboDropEvent FOnDrop;
	bool FIgnoreBtn;
	void __fastcall SetDropHeight(int newValue);
	void __fastcall SetDropWidth(int newValue);
	void __fastcall SetDropped(bool newValue);
	void __fastcall SetMemoAlignment(System::Classes::TAlignment newValue);
	void __fastcall SetMemoColor(System::Uitypes::TColor newValue);
	System::Uitypes::TColor __fastcall GetMemoColor();
	void __fastcall SetMemoFont(Vcl::Graphics::TFont* newValue);
	Vcl::Graphics::TFont* __fastcall GetMemoFont();
	void __fastcall SetMemoMaxLength(int newValue);
	int __fastcall GetMemoMaxLength();
	void __fastcall SetMemoPopupMenu(Vcl::Menus::TPopupMenu* newValue);
	Vcl::Menus::TPopupMenu* __fastcall GetMemoPopupMenu();
	void __fastcall SetMemoScrollBars(System::Uitypes::TScrollStyle newValue);
	System::Uitypes::TScrollStyle __fastcall GetMemoScrollBars();
	void __fastcall SetMemoWordWrap(bool newValue);
	bool __fastcall GetMemoWordWrap();
	void __fastcall MemoDeactivate(System::TObject* Sender);
	void __fastcall ButtonClick(System::TObject* Sender);
	void __fastcall SetMemoFlatScrollBars(bool newValue);
	bool __fastcall GetMemoFlatScrollBars();
	void __fastcall SetMemoActiveBorderType(Elvclutils::TElFlatBorderType newValue);
	Elvclutils::TElFlatBorderType __fastcall GetMemoActiveBorderType();
	void __fastcall MemoChangeTransfer(System::TObject* Sender);
	void __fastcall MemoClickTransfer(System::TObject* Sender);
	void __fastcall MemoDblClickTransfer(System::TObject* Sender);
	void __fastcall MemoKeyDownTransfer(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall MemoKeyPressTransfer(System::TObject* Sender, System::WideChar &Key);
	void __fastcall MemoKeyUpTransfer(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall MemoMouseDownTransfer(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall MemoMouseMoveTransfer(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall MemoMouseUpTransfer(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TWMSetFocus &Msg);
	void __fastcall SetDropAutoWidth(const bool Value);
	
protected:
	virtual void __fastcall TriggerDropEvent(bool &Dropped, System::UnicodeString &Text);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	
public:
	__fastcall virtual TElMemoCombo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElMemoCombo();
	void __fastcall Drop(bool Dropped);
	Elactrls::TElAdvancedMemo* __fastcall GetMemo();
	
__published:
	__property bool DropAutoWidth = {read=FDropAutoWidth, write=SetDropAutoWidth, default=1};
	__property int DropHeight = {read=FDropHeight, write=SetDropHeight, default=80};
	__property int DropWidth = {read=FDropWidth, write=SetDropWidth, nodefault};
	__property bool Dropped = {read=FDropped, write=SetDropped, nodefault};
	__property TElMemoComboDropEvent OnDrop = {read=FOnDrop, write=FOnDrop};
	__property System::Classes::TAlignment MemoAlignment = {read=FMemoAlignment, write=SetMemoAlignment, nodefault};
	__property System::Uitypes::TColor MemoColor = {read=GetMemoColor, write=SetMemoColor, nodefault};
	__property Vcl::Graphics::TFont* MemoFont = {read=GetMemoFont, write=SetMemoFont};
	__property int MemoMaxLength = {read=GetMemoMaxLength, write=SetMemoMaxLength, nodefault};
	__property Vcl::Menus::TPopupMenu* MemoPopupMenu = {read=GetMemoPopupMenu, write=SetMemoPopupMenu};
	__property System::Uitypes::TScrollStyle MemoScrollBars = {read=GetMemoScrollBars, write=SetMemoScrollBars, nodefault};
	__property bool MemoWordWrap = {read=GetMemoWordWrap, write=SetMemoWordWrap, nodefault};
	__property Elvclutils::TElFlatBorderType MemoActiveBorderType = {read=GetMemoActiveBorderType, write=SetMemoActiveBorderType, nodefault};
	__property bool MemoFlatScrollBars = {read=GetMemoFlatScrollBars, write=SetMemoFlatScrollBars, nodefault};
	__property TopMargin = {default=1};
	__property LeftMargin = {default=1};
	__property RightMargin = {default=2};
	__property AutoSize = {default=1};
	__property RTLContent;
	__property BorderSides;
	__property PasswordChar = {default=0};
	__property MaxLength = {default=0};
	__property Transparent;
	__property FlatFocusedScrollBars = {default=0};
	__property WantTabs = {default=0};
	__property HandleDialogKeys = {default=0};
	__property HideSelection = {default=1};
	__property TabSpaces = {default=4};
	__property Lines = {stored=false};
	__property ImageForm;
	__property WordWrap = {default=0};
	__property ScrollBars = {default=0};
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property OnChange;
	__property OnSelectionChange;
	__property Text;
	__property Multiline = {default=1};
	__property Align = {default=0};
	__property ActiveBorderType = {default=1};
	__property AutoSelect = {default=0};
	__property BorderStyle;
	__property ButtonClickSound = {default=0};
	__property ButtonDownSound = {default=0};
	__property ButtonUpSound = {default=0};
	__property ButtonSoundMap;
	__property ButtonColor;
	__property ButtonHint = {default=0};
	__property ButtonWidth = {default=15};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Flat = {default=0};
	__property Font;
	__property InactiveBorderType = {default=3};
	__property ParentColor = {default=1};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ReadOnly = {default=0};
	__property ShowHint;
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
	__property System::Classes::TNotifyEvent OnMemoChange = {read=FOnMemoChange, write=FOnMemoChange};
	__property System::Classes::TNotifyEvent OnMemoClick = {read=FOnMemoClick, write=FOnMemoClick};
	__property System::Classes::TNotifyEvent OnMemoDblClick = {read=FOnMemoDblClick, write=FOnMemoDblClick};
	__property Vcl::Controls::TKeyEvent OnMemoKeyDown = {read=FOnMemoKeyDown, write=FOnMemoKeyDown};
	__property Vcl::Controls::TKeyPressEvent OnMemoKeyPress = {read=FOnMemoKeyPress, write=FOnMemoKeyPress};
	__property Vcl::Controls::TKeyEvent OnMemoKeyUp = {read=FOnMemoKeyUp, write=FOnMemoKeyUp};
	__property Vcl::Controls::TMouseEvent OnMemoMouseDown = {read=FOnMemoMouseDown, write=FOnMemoMouseDown};
	__property Vcl::Controls::TMouseMoveEvent OnMemoMouseMove = {read=FOnMemoMouseMove, write=FOnMemoMouseMove};
	__property Vcl::Controls::TMouseEvent OnMemoMouseUp = {read=FOnMemoMouseUp, write=FOnMemoMouseUp};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElMemoCombo(HWND ParentWindow) : Elbtnedit::TCustomElButtonEdit(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElHackMemo : public Elactrls::TElAdvancedMemo
{
	typedef Elactrls::TElAdvancedMemo inherited;
	
private:
	System::Classes::TComponent* Owner;
	System::Classes::TNotifyEvent FOnDeactivate;
	HIDESBASE MESSAGE void __fastcall WMNCHitTest(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TWMKillFocus &Msg);
	HIDESBASE MESSAGE void __fastcall WMSysKeyDown(Winapi::Messages::TWMKey &Msg);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Msg);
	MESSAGE void __fastcall WMActivate(Winapi::Messages::TWMActivate &Msg);
	
protected:
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	__property System::Classes::TNotifyEvent OnDeactivate = {read=FOnDeactivate, write=FOnDeactivate};
	
public:
	__fastcall virtual TElHackMemo(System::Classes::TComponent* Owner);
public:
	/* TElAdvancedMemo.Destroy */ inline __fastcall virtual ~TElHackMemo() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElHackMemo(HWND ParentWindow) : Elactrls::TElAdvancedMemo(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elmemocombo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELMEMOCOMBO)
using namespace Elmemocombo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElmemocomboHPP
