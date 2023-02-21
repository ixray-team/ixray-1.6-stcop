// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'multi_edit.pas' rev: 35.00 (Windows)

#ifndef Multi_editHPP
#define Multi_editHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Winapi.CommCtrl.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Multi_edit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMultiObjSpinButton;
class DELPHICLASS TMultiObjLWButton;
class DELPHICLASS TMultiObjSpinEdit;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TSpinButtonState : unsigned char { sbNotDown, sbTopDown, sbBottomDown };

enum DECLSPEC_DENUM TValueType : unsigned char { vtInt, vtFloat, vtHex };

enum DECLSPEC_DENUM TSpinButtonKind : unsigned char { bkStandard, bkDiagonal, bkLightWave };

enum DECLSPEC_DENUM TLWButtonState : unsigned char { sbLWNotDown, sbLWDown };

class PASCALIMPLEMENTATION TMultiObjSpinButton : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
private:
	TSpinButtonState FDown;
	Vcl::Graphics::TBitmap* FUpBitmap;
	Vcl::Graphics::TBitmap* FDownBitmap;
	bool FDragging;
	bool FInvalidate;
	Vcl::Graphics::TBitmap* FTopDownBtn;
	Vcl::Graphics::TBitmap* FBottomDownBtn;
	Vcl::Extctrls::TTimer* FRepeatTimer;
	Vcl::Graphics::TBitmap* FNotDownBtn;
	TSpinButtonState FLastDown;
	Vcl::Controls::TWinControl* FFocusControl;
	System::Classes::TNotifyEvent FOnTopClick;
	System::Classes::TNotifyEvent FOnBottomClick;
	void __fastcall TopClick();
	void __fastcall BottomClick();
	void __fastcall GlyphChanged(System::TObject* Sender);
	Vcl::Graphics::TBitmap* __fastcall GetUpGlyph();
	Vcl::Graphics::TBitmap* __fastcall GetDownGlyph();
	void __fastcall SetUpGlyph(Vcl::Graphics::TBitmap* Value);
	void __fastcall SetDownGlyph(Vcl::Graphics::TBitmap* Value);
	void __fastcall SetDown(TSpinButtonState Value);
	void __fastcall SetFocusControl(Vcl::Controls::TWinControl* Value);
	void __fastcall DrawAllBitmap();
	void __fastcall DrawBitmap(Vcl::Graphics::TBitmap* ABitmap, TSpinButtonState ADownState);
	void __fastcall TimerExpired(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	
protected:
	virtual void __fastcall Paint();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	__fastcall virtual TMultiObjSpinButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMultiObjSpinButton();
	__property TSpinButtonState Down = {read=FDown, write=SetDown, default=0};
	
__published:
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Visible = {default=1};
	__property Vcl::Graphics::TBitmap* DownGlyph = {read=GetDownGlyph, write=SetDownGlyph};
	__property Vcl::Graphics::TBitmap* UpGlyph = {read=GetUpGlyph, write=SetUpGlyph};
	__property Vcl::Controls::TWinControl* FocusControl = {read=FFocusControl, write=SetFocusControl};
	__property ShowHint;
	__property ParentShowHint = {default=1};
	__property System::Classes::TNotifyEvent OnBottomClick = {read=FOnBottomClick, write=FOnBottomClick};
	__property System::Classes::TNotifyEvent OnTopClick = {read=FOnTopClick, write=FOnTopClick};
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnStartDrag;
};


typedef void __fastcall (__closure *TLWNotifyEvent)(System::TObject* Sender, int Val);

class PASCALIMPLEMENTATION TMultiObjLWButton : public Vcl::Controls::TGraphicControl
{
	typedef Vcl::Controls::TGraphicControl inherited;
	
private:
	TLWButtonState FDown;
	Vcl::Graphics::TBitmap* FLWBitmap;
	bool FDragging;
	bool FInvalidate;
	Vcl::Graphics::TBitmap* FLWDownBtn;
	Vcl::Graphics::TBitmap* FLWNotDownBtn;
	Vcl::Controls::TWinControl* FFocusControl;
	TLWNotifyEvent FOnLWChange;
	double FSens;
	double FAccum;
	void __fastcall LWChange(System::TObject* Sender, int Val);
	void __fastcall GlyphChanged(System::TObject* Sender);
	Vcl::Graphics::TBitmap* __fastcall GetGlyph();
	void __fastcall SetGlyph(Vcl::Graphics::TBitmap* Value);
	void __fastcall SetDown(TLWButtonState Value);
	void __fastcall SetFocusControl(Vcl::Controls::TWinControl* Value);
	void __fastcall DrawAllBitmap();
	void __fastcall DrawBitmap(Vcl::Graphics::TBitmap* ABitmap, TLWButtonState ADownState);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	
protected:
	virtual void __fastcall Paint();
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseMove(System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall MouseUp(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetSens(double Value);
	
public:
	__fastcall virtual TMultiObjLWButton(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMultiObjLWButton();
	__property TLWButtonState Down = {read=FDown, write=SetDown, default=0};
	virtual void __fastcall Invalidate();
	
__published:
	__property double LWSensitivity = {read=FSens, write=SetSens};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Visible = {default=1};
	__property Vcl::Graphics::TBitmap* LWGlyph = {read=GetGlyph, write=SetGlyph};
	__property Vcl::Controls::TWinControl* FocusControl = {read=FFocusControl, write=SetFocusControl};
	__property TLWNotifyEvent OnLWChange = {read=FOnLWChange, write=FOnLWChange};
	__property ShowHint;
	__property ParentShowHint = {default=1};
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnStartDrag;
};


class PASCALIMPLEMENTATION TMultiObjSpinEdit : public Vcl::Stdctrls::TCustomEdit
{
	typedef Vcl::Stdctrls::TCustomEdit inherited;
	
private:
	bool bIsMulti;
	bool bChanged;
	System::Extended BeforeValue;
	System::Uitypes::TColor StartColor;
	System::Uitypes::TColor FBtnColor;
	System::Classes::TAlignment FAlignment;
	System::Extended FMinValue;
	System::Extended FMaxValue;
	System::Extended FIncrement;
	int FButtonWidth;
	System::Byte FDecimal;
	bool FChanging;
	bool FEditorEnabled;
	TValueType FValueType;
	TMultiObjSpinButton* FButton;
	Vcl::Controls::TWinControl* FBtnWindow;
	bool FArrowKeys;
	System::Classes::TNotifyEvent FOnTopClick;
	System::Classes::TNotifyEvent FOnBottomClick;
	TSpinButtonKind FButtonKind;
	Vcl::Comctrls::TCustomUpDown* FUpDown;
	TMultiObjLWButton* FLWButton;
	TLWNotifyEvent FOnLWChange;
	double FSens;
	TSpinButtonKind __fastcall GetButtonKind();
	void __fastcall SetButtonKind(TSpinButtonKind Value);
	void __fastcall UpDownClick(System::TObject* Sender, Vcl::Comctrls::TUDBtnType Button);
	void __fastcall LWChange(System::TObject* Sender, int Val);
	System::Extended __fastcall GetValue();
	System::Extended __fastcall CheckValue(System::Extended NewValue);
	int __fastcall GetAsInteger();
	bool __fastcall IsIncrementStored();
	bool __fastcall IsMaxStored();
	bool __fastcall IsMinStored();
	bool __fastcall IsValueStored();
	void __fastcall SetArrowKeys(bool Value);
	void __fastcall SetAsInteger(int NewValue);
	void __fastcall SetValue(System::Extended NewValue);
	void __fastcall SetValueType(TValueType NewType);
	void __fastcall SetButtonWidth(int NewValue);
	void __fastcall SetDecimal(System::Byte NewValue);
	int __fastcall GetButtonWidth();
	void __fastcall RecreateButton();
	void __fastcall ResizeButton();
	void __fastcall SetEditRect();
	HIDESBASE void __fastcall SetAlignment(System::Classes::TAlignment Value);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMPaste(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall WMCut(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMCtl3DChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Message);
	void __fastcall SetBtnColor(System::Uitypes::TColor Value);
	
protected:
	DYNAMIC void __fastcall Change();
	virtual bool __fastcall IsValidChar(System::WideChar Key);
	virtual void __fastcall UpClick(System::TObject* Sender);
	virtual void __fastcall DownClick(System::TObject* Sender);
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	void __fastcall SetSens(double Val);
	double __fastcall GetSens();
	
public:
	__fastcall virtual TMultiObjSpinEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMultiObjSpinEdit();
	__property int AsInteger = {read=GetAsInteger, write=SetAsInteger, default=0};
	__property Text = {default=0};
	void __fastcall ObjFirstInit(float v);
	void __fastcall ObjNextInit(float v);
	void __fastcall ObjApplyFloat(float &_to);
	void __fastcall ObjApplyInt(int &_to);
	
__published:
	__property double LWSensitivity = {read=GetSens, write=SetSens};
	__property System::Classes::TAlignment Alignment = {read=FAlignment, write=SetAlignment, default=0};
	__property bool ArrowKeys = {read=FArrowKeys, write=SetArrowKeys, default=1};
	__property System::Uitypes::TColor BtnColor = {read=FBtnColor, write=SetBtnColor, default=-16777201};
	__property TSpinButtonKind ButtonKind = {read=FButtonKind, write=SetButtonKind, default=0};
	__property System::Byte Decimal = {read=FDecimal, write=SetDecimal, default=2};
	__property int ButtonWidth = {read=FButtonWidth, write=SetButtonWidth, default=14};
	__property bool EditorEnabled = {read=FEditorEnabled, write=FEditorEnabled, default=1};
	__property System::Extended Increment = {read=FIncrement, write=FIncrement, stored=IsIncrementStored};
	__property System::Extended MaxValue = {read=FMaxValue, write=FMaxValue, stored=IsMaxStored};
	__property System::Extended MinValue = {read=FMinValue, write=FMinValue, stored=IsMinStored};
	__property TValueType ValueType = {read=FValueType, write=SetValueType, default=0};
	__property System::Extended Value = {read=GetValue, write=SetValue, stored=IsValueStored};
	__property AutoSelect = {default=1};
	__property AutoSize = {default=1};
	__property BorderStyle = {default=1};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property Anchors = {default=3};
	__property BiDiMode;
	__property Constraints;
	__property DragKind = {default=0};
	__property ParentBiDiMode = {default=1};
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property MaxLength = {default=0};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ReadOnly = {default=0};
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Visible = {default=1};
	__property TLWNotifyEvent OnLWChange = {read=FOnLWChange, write=FOnLWChange};
	__property System::Classes::TNotifyEvent OnBottomClick = {read=FOnBottomClick, write=FOnBottomClick};
	__property System::Classes::TNotifyEvent OnTopClick = {read=FOnTopClick, write=FOnTopClick};
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
	__property OnContextPopup;
	__property OnMouseWheelDown;
	__property OnMouseWheelUp;
	__property OnEndDock;
	__property OnStartDock;
public:
	/* TWinControl.CreateParented */ inline __fastcall TMultiObjSpinEdit(HWND ParentWindow) : Vcl::Stdctrls::TCustomEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Multi_edit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MULTI_EDIT)
using namespace Multi_edit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Multi_editHPP
