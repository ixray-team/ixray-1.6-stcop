// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElMaskEdit.pas' rev: 34.00 (Windows)

#ifndef ElmaskeditHPP
#define ElmaskeditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Graphics.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElXPThemedControl.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Clipbrd.hpp>
#include <ElVCLUtils.hpp>
#include <ElImgFrm.hpp>
#include <Vcl.Mask.hpp>
#include <System.UITypes.hpp>
#include <System.MaskUtils.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elmaskedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TCustomElMaskEdit;
class DELPHICLASS TElMaskEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TCustomElMaskEdit : public Vcl::Mask::TCustomMaskEdit
{
	typedef Vcl::Mask::TCustomMaskEdit inherited;
	
private:
	Elvclutils::TElFlatBorderType FActiveBorderType;
	System::Classes::TAlignment FAlignment;
	Vcl::Graphics::TBitmap* FBackground;
	Elvclutils::TElBorderSides FBorderSides;
	bool FFlat;
	bool FHandleDialogKeys;
	Elimgfrm::TElImageForm* FImgForm;
	Elimgfrm::TImgFormChangeLink* FImgFormChLink;
	Elvclutils::TElFlatBorderType FInactiveBorderType;
	bool FMouseOver;
	System::Classes::TNotifyEvent FOnMouseEnter;
	System::Classes::TNotifyEvent FOnMouseLeave;
	bool FPainting;
	bool FPaintingTo;
	bool FReturnPressed;
	bool FTransparent;
	bool FUseBackground;
	NativeUInt FTheme;
	System::WideString FHint;
	void __fastcall BackgroundChanged(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMMouseEnter(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMMouseLeave(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall CMParentColorChanged(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall CNCtlColorEdit(Winapi::Messages::TWMCtlColor &Msg);
	MESSAGE void __fastcall CNCtlColorStatic(Winapi::Messages::TWMCtlColor &Msg);
	void __fastcall DrawBackground(HDC DC, const System::Types::TRect &R);
	void __fastcall DrawFlatBorder(HDC DC);
	void __fastcall DrawParentControl(HDC DC);
	HIDESBASE MESSAGE void __fastcall WMEraseBkgnd(Winapi::Messages::TWMEraseBkgnd &Msg);
	MESSAGE void __fastcall WMGetDlgCode(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMKillFocus(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMMove(Winapi::Messages::TMessage &Msg);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TWMNCCalcSize &Message);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall WMSetFocus(Winapi::Messages::TMessage &Msg);
	void __fastcall ImageFormChange(System::TObject* Sender);
	void __fastcall SetActiveBorderType(const Elvclutils::TElFlatBorderType Value);
	HIDESBASE void __fastcall SetAlignment(const System::Classes::TAlignment Value);
	void __fastcall SetBackground(Vcl::Graphics::TBitmap* const Value);
	void __fastcall SetBorderSides(Elvclutils::TElBorderSides Value);
	void __fastcall SetInactiveBorderType(const Elvclutils::TElFlatBorderType Value);
	void __fastcall SetTransparent(const bool Value);
	void __fastcall SetUseBackground(const bool Value);
	
protected:
	bool FNoHandleEnter;
	bool FUseXPThemes;
	System::Uitypes::TColor FLineBorderActiveColor;
	System::Uitypes::TColor FLineBorderInactiveColor;
	DYNAMIC void __fastcall Change();
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	DYNAMIC void __fastcall DoMouseEnter();
	DYNAMIC void __fastcall DoMouseLeave();
	DYNAMIC void __fastcall DoPaint();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall SetFlat(const bool Value);
	virtual void __fastcall SetImageForm(Elimgfrm::TElImageForm* newValue);
	MESSAGE void __fastcall IFMRepaintChildren(Winapi::Messages::TMessage &Message);
	virtual void __fastcall CreateThemeHandle();
	virtual void __fastcall CreateWnd();
	virtual void __fastcall DestroyWnd();
	virtual void __fastcall FreeThemeHandle();
	bool __fastcall IsThemeApplied();
	virtual void __fastcall SetUseXPThemes(bool Value);
	HIDESBASE MESSAGE void __fastcall WMNCPaint(Winapi::Messages::TMessage &Msg);
	MESSAGE void __fastcall WMThemeChanged(Winapi::Messages::TMessage &Message);
	System::WideString __fastcall GetThemedClassName();
	void __fastcall SetLineBorderActiveColor(System::Uitypes::TColor Value);
	void __fastcall SetLineBorderInactiveColor(System::Uitypes::TColor Value);
	HIDESBASE MESSAGE void __fastcall WMPaste(Winapi::Messages::TMessage &Message);
	void __fastcall SetHint(System::WideString Value);
	HIDESBASE MESSAGE void __fastcall CMHintShow(Winapi::Messages::TMessage &Message);
	__property Elvclutils::TElFlatBorderType ActiveBorderType = {read=FActiveBorderType, write=SetActiveBorderType, default=1};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property Vcl::Graphics::TBitmap* Background = {read=FBackground, write=SetBackground};
	__property bool Flat = {read=FFlat, write=SetFlat, default=0};
	__property Elvclutils::TElFlatBorderType InactiveBorderType = {read=FInactiveBorderType, write=SetInactiveBorderType, default=3};
	__property System::Classes::TNotifyEvent OnMouseEnter = {read=FOnMouseEnter, write=FOnMouseEnter};
	__property System::Classes::TNotifyEvent OnMouseLeave = {read=FOnMouseLeave, write=FOnMouseLeave};
	__property bool Transparent = {read=FTransparent, write=SetTransparent, default=0};
	__property bool UseBackground = {read=FUseBackground, write=SetUseBackground, default=0};
	__property Elvclutils::TElBorderSides BorderSides = {read=FBorderSides, write=SetBorderSides, nodefault};
	__property bool HandleDialogKeys = {read=FHandleDialogKeys, write=FHandleDialogKeys, default=1};
	__property Elimgfrm::TElImageForm* ImageForm = {read=FImgForm, write=SetImageForm};
	__property bool UseXPThemes = {read=FUseXPThemes, write=SetUseXPThemes, default=1};
	__property System::Uitypes::TColor LineBorderActiveColor = {read=FLineBorderActiveColor, write=SetLineBorderActiveColor, nodefault};
	__property System::Uitypes::TColor LineBorderInactiveColor = {read=FLineBorderInactiveColor, write=SetLineBorderInactiveColor, nodefault};
	
public:
	__fastcall virtual TCustomElMaskEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElMaskEdit();
	__property bool MouseOver = {read=FMouseOver, default=0};
	__property NativeUInt Theme = {read=FTheme, nodefault};
	
__published:
	__property System::WideString Hint = {read=FHint, write=SetHint};
public:
	/* TWinControl.CreateParented */ inline __fastcall TCustomElMaskEdit(HWND ParentWindow) : Vcl::Mask::TCustomMaskEdit(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElMaskEdit : public TCustomElMaskEdit
{
	typedef TCustomElMaskEdit inherited;
	
__published:
	__property ActiveBorderType = {default=1};
	__property Alignment = {default=0};
	__property Background;
	__property Flat = {default=0};
	__property InactiveBorderType = {default=3};
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property Transparent = {default=0};
	__property UseBackground = {default=0};
	__property BorderSides;
	__property HandleDialogKeys = {default=1};
	__property ImageForm;
	__property UseXPThemes = {default=1};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property Align = {default=0};
	__property AutoSelect = {default=1};
	__property AutoSize = {default=1};
	__property BorderStyle = {default=1};
	__property CharCase = {default=0};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property Cursor = {default=0};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property EditMask = {default=0};
	__property Font;
	__property HideSelection = {default=1};
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property MaxLength = {default=0};
	__property OEMConvert = {default=0};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PasswordChar = {default=0};
	__property PopupMenu;
	__property ReadOnly = {default=0};
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property Text;
	__property Visible = {default=1};
	__property OnChange;
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
	__property OnStartDrag;
public:
	/* TCustomElMaskEdit.Create */ inline __fastcall virtual TElMaskEdit(System::Classes::TComponent* AOwner) : TCustomElMaskEdit(AOwner) { }
	/* TCustomElMaskEdit.Destroy */ inline __fastcall virtual ~TElMaskEdit() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElMaskEdit(HWND ParentWindow) : TCustomElMaskEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elmaskedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELMASKEDIT)
using namespace Elmaskedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElmaskeditHPP
