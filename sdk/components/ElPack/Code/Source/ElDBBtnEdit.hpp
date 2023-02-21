// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElDBBtnEdit.pas' rev: 35.00 (Windows)

#ifndef EldbbtneditHPP
#define EldbbtneditHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>
#include <ElBtnEdit.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <ElEdits.hpp>
#include <ElXPThemedControl.hpp>
#include <ElVCLUtils.hpp>
#include <ElStrUtils.hpp>
#include <ElImgFrm.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Graphics.hpp>
#include <ElSndMap.hpp>
#include <ElPopBtn.hpp>
#include <Vcl.Menus.hpp>
#include <ElUnicodeStrings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eldbbtnedit
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElDBButtonEdit;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElDBButtonEdit : public Elbtnedit::TCustomElButtonEdit
{
	typedef Elbtnedit::TCustomElButtonEdit inherited;
	
private:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	void __fastcall ActiveChange(System::TObject* Sender);
	void __fastcall DataChange(System::TObject* Sender);
	void __fastcall EditingChange(System::TObject* Sender);
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	void __fastcall ResetMaxLength();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall UpdateData(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall CMGetDataLink(Winapi::Messages::TMessage &Message);
	
protected:
	virtual void __fastcall Change();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TElDBButtonEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDBButtonEdit();
	__property Data::Db::TField* Field = {read=GetField};
	__property Text;
	__property Lines;
	
__published:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
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
	__property ImageForm;
	__property WordWrap = {default=0};
	__property ScrollBars = {default=0};
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property OnChange;
	__property OnSelectionChange;
	__property Multiline = {default=0};
	__property Flat = {default=0};
	__property ActiveBorderType = {default=1};
	__property InactiveBorderType = {default=3};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property UseBackground = {default=0};
	__property Alignment;
	__property AutoSelect = {default=0};
	__property Background;
	__property ButtonCaption = {default=0};
	__property ButtonClickSound = {default=0};
	__property ButtonDownSound = {default=0};
	__property ButtonUpSound = {default=0};
	__property ButtonSoundMap;
	__property ButtonColor;
	__property ButtonDown;
	__property ButtonEnabled;
	__property ButtonFlat;
	__property ButtonGlyph;
	__property ButtonHint = {default=0};
	__property ButtonIcon;
	__property ButtonNumGlyphs;
	__property ButtonPopupPlace;
	__property ButtonPullDownMenu;
	__property ButtonShortcut;
	__property ButtonUseIcon;
	__property ButtonVisible;
	__property ButtonWidth;
	__property OnButtonClick;
	__property AltButtonCaption = {default=0};
	__property AltButtonClickSound = {default=0};
	__property AltButtonDownSound = {default=0};
	__property AltButtonUpSound = {default=0};
	__property AltButtonSoundMap;
	__property AltButtonColor;
	__property AltButtonDown;
	__property AltButtonEnabled;
	__property AltButtonFlat;
	__property AltButtonGlyph;
	__property AltButtonHint = {default=0};
	__property AltButtonIcon;
	__property AltButtonNumGlyphs;
	__property AltButtonPopupPlace;
	__property AltButtonPosition = {default=1};
	__property AltButtonPullDownMenu;
	__property AltButtonShortcut;
	__property AltButtonUseIcon;
	__property AltButtonVisible;
	__property AltButtonWidth;
	__property OnAltButtonClick;
	__property BorderStyle;
	__property Ctl3D;
	__property ParentCtl3D = {default=1};
	__property Enabled = {default=1};
	__property TabStop = {default=1};
	__property TabOrder = {default=-1};
	__property PopupMenu;
	__property Color = {default=-16777211};
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property ReadOnly = {default=0};
	__property OnEnter;
	__property OnExit;
	__property OnClick;
	__property OnDblClick;
	__property OnKeyDown;
	__property OnKeyPress;
	__property OnKeyUp;
	__property OnStartDrag;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDBButtonEdit(HWND ParentWindow) : Elbtnedit::TCustomElButtonEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eldbbtnedit */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELDBBTNEDIT)
using namespace Eldbbtnedit;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EldbbtneditHPP
