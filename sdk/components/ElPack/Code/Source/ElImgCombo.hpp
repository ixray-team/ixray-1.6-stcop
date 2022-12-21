// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElImgCombo.pas' rev: 34.00 (Windows)

#ifndef ElimgcomboHPP
#define ElimgcomboHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Vcl.ImgList.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.CommCtrl.hpp>
#include <ElTools.hpp>
#include <ElACtrls.hpp>
#include <ElUnicodeStrings.hpp>
#include <ElStrUtils.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Menus.hpp>

//-- user supplied -----------------------------------------------------------

namespace Elimgcombo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElImageComboBox;
//-- type declarations -------------------------------------------------------
typedef Elunicodestrings::TElWideStrings TElFStrings;

typedef Elunicodestrings::TElWideStringList TElFStringList;

typedef void __fastcall (__closure *TElImageNameEvent)(System::TObject* Sender, int Index, Elstrutils::TElFString &Text);

class PASCALIMPLEMENTATION TElImageComboBox : public Elactrls::TElAdvancedComboBox
{
	typedef Elactrls::TElAdvancedComboBox inherited;
	
private:
	Vcl::Imglist::TChangeLink* FChLink;
	Vcl::Controls::TImageList* FImages;
	bool FModified;
	int FDummyInt;
	Elunicodestrings::TElWideStrings* FImageNames;
	int IOffs;
	bool OwnMoveFlag;
	bool FUseImageNames;
	void __fastcall ImagesChanged(System::TObject* Sender);
	void __fastcall SetImages(Vcl::Controls::TImageList* const Value);
	void __fastcall Remake();
	int __fastcall GetImageIndex();
	void __fastcall SetImageIndex(const int Value);
	void __fastcall SetModified(const bool Value);
	HIDESBASE MESSAGE void __fastcall WMPaint(Winapi::Messages::TWMPaint &Msg);
	HIDESBASE MESSAGE void __fastcall CNCommand(Winapi::Messages::TWMCommand &Message);
	void __fastcall SetImageNames(Elunicodestrings::TElWideStrings* Value);
	
protected:
	TElImageNameEvent FOnImageName;
	bool FManualEdit;
	System::UnicodeString FEmptyValueText;
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall CreateParams(Vcl::Controls::TCreateParams &Params);
	virtual void __fastcall CreateWnd();
	DYNAMIC void __fastcall DropDown();
	DYNAMIC void __fastcall DblClick();
	DYNAMIC void __fastcall Change();
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	virtual void __fastcall ComboWndProc(Winapi::Messages::TMessage &Message, HWND ComboWnd, void * ComboProc);
	virtual void __fastcall TriggerImageNameEvent(int Index, Elstrutils::TElFString &Text);
	void __fastcall SetManualEdit(bool Value);
	bool __fastcall GetShowEmptyValue();
	void __fastcall SetShowEmptyValue(bool Value);
	HIDESBASE MESSAGE void __fastcall WMNCCalcSize(Winapi::Messages::TMessage &Message);
	virtual void __fastcall EditWndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall WndProc(Winapi::Messages::TMessage &Message);
	void __fastcall UpdateEditSize();
	virtual void __fastcall Loaded();
	void __fastcall SetEmptyValueText(const System::UnicodeString Value);
	void __fastcall RebuildList();
	
public:
	__fastcall virtual TElImageComboBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElImageComboBox();
	__property int ItemIndex = {read=GetImageIndex, write=SetImageIndex, nodefault};
	
__published:
	__property int Items = {read=FDummyInt, nodefault};
	__property int Style = {read=FDummyInt, nodefault};
	__property Vcl::Controls::TImageList* Images = {read=FImages, write=SetImages};
	__property int ImageIndex = {read=GetImageIndex, write=SetImageIndex, default=-1};
	__property bool Modified = {read=FModified, write=SetModified, nodefault};
	__property bool ManualEdit = {read=FManualEdit, write=SetManualEdit, default=1};
	__property bool ShowEmptyValue = {read=GetShowEmptyValue, write=SetShowEmptyValue, default=1};
	__property System::UnicodeString EmptyValueText = {read=FEmptyValueText, write=SetEmptyValueText};
	__property TElImageNameEvent OnImageName = {read=FOnImageName, write=FOnImageName};
	__property Elunicodestrings::TElWideStrings* ImageNames = {read=FImageNames, write=SetImageNames};
	__property bool UseImageNames = {read=FUseImageNames, write=FUseImageNames, default=0};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property DropDownCount = {default=8};
	__property Enabled = {default=1};
	__property Font;
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Visible = {default=1};
	__property OnChange;
	__property OnClick;
	__property OnDblClick;
	__property OnEnter;
	__property OnExit;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElImageComboBox(HWND ParentWindow) : Elactrls::TElAdvancedComboBox(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Elimgcombo */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELIMGCOMBO)
using namespace Elimgcombo;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// ElimgcomboHPP
