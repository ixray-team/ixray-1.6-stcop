// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElImgFrm.pas' rev: 35.00 (Windows)

#ifndef ElimgfrmHPP
#define ElimgfrmHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.TypInfo.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <System.Classes.hpp>
#include <ElHook.hpp>
#include <ElTools.hpp>
#include <ElVCLUtils.hpp>
#include <ElList.hpp>
#include <ElUxTheme.hpp>
#include <ElTmSchema.hpp>
#include <ElExtBkgnd.hpp>
#include <System.UITypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elimgfrm
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TImgFormChangeLink;
class DELPHICLASS TCustomElImageForm;
class DELPHICLASS TElImageForm;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TImgFormChangeLink : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCustomElImageForm* FSender;
	System::Classes::TNotifyEvent FOnChange;
	
public:
	__fastcall virtual ~TImgFormChangeLink();
	DYNAMIC void __fastcall Change();
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property TCustomElImageForm* Sender = {read=FSender, write=FSender};
public:
	/* TObject.Create */ inline __fastcall TImgFormChangeLink() : System::TObject() { }
	
};


class PASCALIMPLEMENTATION TCustomElImageForm : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Vcl::Controls::TGraphicControl* FCaptionControl;
	bool FChangeRegion;
	Vcl::Graphics::TBitmap* FBkImage;
	Vcl::Extctrls::TImage* FImage;
	Vcl::Graphics::TBitmap* FBmp;
	bool FMoveForm;
	Elhook::TElHook* FMoveHook;
	System::Classes::TNotifyEvent FOldImageEvent;
	HRGN FRegion;
	Elvclutils::TElBkGndType FBackgroundType;
	int FGradientSteps;
	System::Uitypes::TColor FGradientStartColor;
	System::Uitypes::TColor FGradientEndColor;
	bool FNoBk;
	Ellist::TElList* FCLients;
	Vcl::Graphics::TBitmap* CacheBackground;
	void __fastcall AfterMessage(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall BeforeMessage(System::TObject* Sender, Winapi::Messages::TMessage &Msg, bool &Handled);
	void __fastcall PictureChanged(System::TObject* Sender);
	void __fastcall BkImageChange(System::TObject* Sender);
	void __fastcall SetCaptionControl(Vcl::Controls::TGraphicControl* const Value);
	void __fastcall SetChangeRegion(const bool Value);
	void __fastcall SetImage(Vcl::Extctrls::TImage* const Value);
	void __fastcall SetBkImage(Vcl::Graphics::TBitmap* const Value);
	void __fastcall SetMoveForm(const bool Value);
	void __fastcall SetGradientStartColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientEndColor(System::Uitypes::TColor newValue);
	void __fastcall SetGradientSteps(int newValue);
	void __fastcall SetBackgroundType(Elvclutils::TElBkGndType newValue);
	void __fastcall Change();
	
protected:
	Vcl::Controls::TWinControl* FControl;
	System::Uitypes::TColor FTransparentColor;
	void __fastcall CreateHook();
	void __fastcall CreateRegion();
	void __fastcall DestroyHook();
	void __fastcall DestroyRegion();
	System::Uitypes::TColor __fastcall GetTransparentColor();
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetControl(Vcl::Controls::TWinControl* Value);
	void __fastcall SetTransparentColor(System::Uitypes::TColor Value);
	void __fastcall CreateCacheBackground();
	void __fastcall DisposeCacheBackground();
	__property Vcl::Controls::TGraphicControl* CaptionControl = {read=FCaptionControl, write=SetCaptionControl};
	__property bool ChangeFormRegion = {read=FChangeRegion, write=SetChangeRegion, nodefault};
	__property bool MoveForm = {read=FMoveForm, write=SetMoveForm, default=0};
	__property Vcl::Extctrls::TImage* FormImage = {read=FImage, write=SetImage};
	__property Vcl::Graphics::TBitmap* Background = {read=FBkImage, write=SetBkImage};
	__property Elvclutils::TElBkGndType BackgroundType = {read=FBackgroundType, write=SetBackgroundType, default=2};
	__property System::Uitypes::TColor GradientStartColor = {read=FGradientStartColor, write=SetGradientStartColor, nodefault};
	__property System::Uitypes::TColor GradientEndColor = {read=FGradientEndColor, write=SetGradientEndColor, nodefault};
	__property int GradientSteps = {read=FGradientSteps, write=SetGradientSteps, default=16};
	
public:
	__fastcall virtual TCustomElImageForm(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TCustomElImageForm();
	void __fastcall RegisterChanges(TImgFormChangeLink* Value);
	void __fastcall UnregisterChanges(TImgFormChangeLink* Value);
	void __fastcall PaintBkgnd(HDC DC, const System::Types::TRect &R, const System::Types::TPoint &Origin, bool Direct);
	Vcl::Controls::TWinControl* __fastcall GetRealControl();
	
__published:
	__property Vcl::Controls::TWinControl* Control = {read=FControl, write=SetControl};
	__property System::Uitypes::TColor TransparentColor = {read=FTransparentColor, write=SetTransparentColor, nodefault};
};


class PASCALIMPLEMENTATION TElImageForm : public TCustomElImageForm
{
	typedef TCustomElImageForm inherited;
	
__published:
	__property CaptionControl;
	__property ChangeFormRegion;
	__property FormImage;
	__property MoveForm = {default=0};
	__property Background;
	__property BackgroundType = {default=2};
	__property GradientStartColor;
	__property GradientEndColor;
	__property GradientSteps = {default=16};
public:
	/* TCustomElImageForm.Create */ inline __fastcall virtual TElImageForm(System::Classes::TComponent* AOwner) : TCustomElImageForm(AOwner) { }
	/* TCustomElImageForm.Destroy */ inline __fastcall virtual ~TElImageForm() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word IFM_EFFECTIVESIZE = System::Word(0x92e);
static const System::Word IFM_REPAINTCHILDREN = System::Word(0x1cf5);
static const System::Word IFM_CANPAINTBKGND = System::Word(0x1cf7);
}	/* namespace Elimgfrm */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELIMGFRM)
using namespace Elimgfrm;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElimgfrmHPP
