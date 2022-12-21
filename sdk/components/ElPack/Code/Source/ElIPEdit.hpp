// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElIPEdit.pas' rev: 34.00 (Windows)

#ifndef ElipeditHPP
#define ElipeditHPP

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
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.WinSock.hpp>
#include <Vcl.Menus.hpp>
#include <ElUxTheme.hpp>
#include <ElXPThemedControl.hpp>
#include <ElTools.hpp>
#include <ElEdits.hpp>
#include <ElVCLUtils.hpp>
#include <ElImgFrm.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elipedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElIPPartEdit;
class DELPHICLASS TElIPEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElIPPartEdit : public Eledits::TElEdit
{
	typedef Eledits::TElEdit inherited;
	
private:
	System::Classes::TNotifyEvent OnPoint;
	System::Classes::TNotifyEvent OnLeftPoint;
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	
protected:
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
public:
	/* TCustomElEdit.Create */ inline __fastcall virtual TElIPPartEdit(System::Classes::TComponent* AOwner) : Eledits::TElEdit(AOwner) { }
	/* TCustomElEdit.Destroy */ inline __fastcall virtual ~TElIPPartEdit() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElIPPartEdit(HWND ParentWindow) : Eledits::TElEdit(ParentWindow) { }
	
};


typedef TElIPPartEdit IntEditClass;

class PASCALIMPLEMENTATION TElIPEdit : public Elxpthemedcontrol::TElXPThemedControl
{
	typedef Elxpthemedcontrol::TElXPThemedControl inherited;
	
private:
	Vcl::Forms::TFormBorderStyle FBorderStyle;
	bool FFlat;
	Elvclutils::TElFlatBorderType FActiveBorderType;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FMouseOver;
	bool FModified;
	System::Classes::TNotifyEvent FOnChange;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	System::StaticArray<bool, 4> FPartCanEdit;
	System::StaticArray<TElIPPartEdit*, 4> FPartEditors;
	System::StaticArray<System::Byte, 4> FParts;
	Elvclutils::TElBorderSides FBorderSides;
	System::WideString FHint;
	int __fastcall FindPart(TElIPPartEdit* Editor);
	System::Byte __fastcall GetPart(int Index);
	bool __fastcall GetPartCanEdit(int Index);
	void __fastcall SetPart(int Index, System::Byte Value);
	void __fastcall SetPartCanEdit(int Index, bool Value);
	unsigned __fastcall GetIPAddress();
	void __fastcall SetIPAddress(unsigned Value);
	void __fastcall SetFlat(const bool Value);
	void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetBorderStyle(Vcl::Forms::TBorderStyle Value);
	void __fastcall UpdateFrame();
	void __fastcall SetModified(bool Value);
	void __fastcall SetIPString(System::UnicodeString value);
	System::UnicodeString __fastcall GetIPString();
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	void __fastcall ClickHandler(System::TObject* Sender);
	void __fastcall DblClickHandler(System::TObject* Sender);
	void __fastcall DragDropHandler(System::TObject* Sender, System::TObject* Source, int X, int Y);
	void __fastcall DragOverHandler(System::TObject* Sender, System::TObject* Source, int X, int Y, System::Uitypes::TDragState State, bool &Accept);
	void __fastcall EndDragHandler(System::TObject* Sender, System::TObject* Target, int X, int Y);
	void __fastcall KeyDownHandler(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall KeyPressHandler(System::TObject* Sender, System::WideChar &Key);
	void __fastcall KeyUpHandler(System::TObject* Sender, System::Word &Key, System::Classes::TShiftState Shift);
	void __fastcall MouseDownHandler(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall MouseEnterHandler(System::TObject* Sender);
	void __fastcall MouseLeaveHandler(System::TObject* Sender);
	void __fastcall MouseMoveHandler(System::TObject* Sender, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall MouseUpHandler(System::TObject* Sender, System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall StartDragHandler(System::TObject* Sender, Vcl::Controls::TDragObject* &DragObject);
	
protected:
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	bool FChangeDisabledText;
	void __fastcall DrawFlatBorder(HDC DC);
	virtual void __fastcall AdjustEditorPositions();
	HIDESBASE MESSAGE void __fastcall CMColorChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMSysColorChange(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	DYNAMIC void __fastcall DoMouseEnter();
	DYNAMIC void __fastcall DoMouseLeave();
	virtual void __fastcall CreateWindowHandle(const Vcl::Controls::TCreateParams &Params);
	void __fastcall OnEditorChange(System::TObject* Sender);
	void __fastcall OnEditorEnter(System::TObject* Sender);
	void __fastcall OnEditorExit(System::TObject* Sender);
	void __fastcall OnEditorPoint(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TMessage &Message);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	void __fastcall OnEditorLeftPoint(System::TObject* Sender);
	virtual void __fastcall TriggerChangeEvent();
	virtual System::WideString __fastcall GetThemedClassName();
	virtual void __fastcall Loaded();
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	void __fastcall SetChangeDisabledText(bool Value);
	
public:
	__fastcall virtual TElIPEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElIPEdit();
	virtual void __fastcall Paint();
	__property unsigned IPAddress = {read=GetIPAddress, write=SetIPAddress, nodefault};
	__property bool MouseOver = {read=FMouseOver, nodefault};
	__property bool Modified = {read=FModified, write=SetModified, nodefault};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
	__property Vcl::Forms::TBorderStyle BorderStyle = {read=FBorderStyle, write=SetBorderStyle, default=1};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property System::Byte Part1 = {read=GetPart, write=SetPart, index=1, default=0};
	__property System::Byte Part2 = {read=GetPart, write=SetPart, index=2, default=0};
	__property System::Byte Part3 = {read=GetPart, write=SetPart, index=3, default=0};
	__property System::Byte Part4 = {read=GetPart, write=SetPart, index=4, default=0};
	__property bool Part1CanEdit = {read=GetPartCanEdit, write=SetPartCanEdit, index=1, default=1};
	__property bool Part2CanEdit = {read=GetPartCanEdit, write=SetPartCanEdit, index=2, default=1};
	__property bool Part3CanEdit = {read=GetPartCanEdit, write=SetPartCanEdit, index=3, default=1};
	__property bool Part4CanEdit = {read=GetPartCanEdit, write=SetPartCanEdit, index=4, default=1};
	__property System::UnicodeString IPString = {read=GetIPString, write=SetIPString};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	__property bool ChangeDisabledText = {read=FChangeDisabledText, write=SetChangeDisabledText, default=0};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Color = {default=-16777211};
	__property Ctl3D = {default=1};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Visible = {default=1};
	__property UseXPThemes = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElIPEdit(HWND ParentWindow) : Elxpthemedcontrol::TElXPThemedControl(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elipedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELIPEDIT)
using namespace Elipedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElipeditHPP
