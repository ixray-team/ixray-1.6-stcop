// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElDBCtrls.pas' rev: 34.00 (Windows)

#ifndef EldbctrlsHPP
#define EldbctrlsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>
#include <ElPanel.hpp>
#include <ElToolbar.hpp>
#include <ElTmSchema.hpp>
#include <ElPopBtn.hpp>
#include <ElACtrls.hpp>
#include <ElMaskEdit.hpp>
#include <ElCheckCtl.hpp>
#include <ElCheckItemGrp.hpp>
#include <ElStrUtils.hpp>
#include <ElUnicodeStrings.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.Graphics.hpp>
#include <ElPromptDlg.hpp>
#include <ElEdits.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Vcl.Mask.hpp>
#include <ElGroupBox.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <ElVCLUtils.hpp>
#include <ElImgFrm.hpp>
#include <System.UITypes.hpp>
#include <Vcl.Menus.hpp>
#include <ElBtnCtl.hpp>
#include <ElXPThemedControl.hpp>
#include <ElSndMap.hpp>
#include <Vcl.Buttons.hpp>
#include <ElScrollBar.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eldbctrls
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElDBEdit;
class DELPHICLASS TElDBMemo;
class DELPHICLASS TElDBCheckBox;
class DELPHICLASS TElDBRadioGroup;
class DELPHICLASS TElDBNavButton;
class DELPHICLASS TElDBNavigator;
class DELPHICLASS TElNavDataLink;
class DELPHICLASS TElWideDBEdit;
class DELPHICLASS TElWideDBMemo;
//-- type declarations -------------------------------------------------------
typedef Elunicodestrings::TElWideStrings TElFStrings;

typedef Elunicodestrings::TElWideStringList TElFStringList;

class PASCALIMPLEMENTATION TElDBEdit : public Elmaskedit::TCustomElMaskEdit
{
	typedef Elmaskedit::TCustomElMaskEdit inherited;
	
private:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	bool FFocused;
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall ResetMaxLength();
	void __fastcall DataChange(System::TObject* Sender);
	void __fastcall ActiveChange(System::TObject* Sender);
	void __fastcall EditingChange(System::TObject* Sender);
	void __fastcall UpdateData(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall CMGetDataLink(Winapi::Messages::TMessage &Message);
	void __fastcall SetFocused(bool Value);
	HIDESBASE bool __fastcall GetReadOnly();
	HIDESBASE void __fastcall SetReadOnly(bool Value);
	HIDESBASE MESSAGE void __fastcall WMCut(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPaste(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMUndo(Winapi::Messages::TMessage &Message);
	
protected:
	DYNAMIC void __fastcall Change();
	virtual bool __fastcall EditCanModify();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Reset();
	
public:
	__fastcall virtual TElDBEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDBEdit();
	__property Data::Db::TField* Field = {read=GetField};
	
__published:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
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
	__property Cursor = {default=0};
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property HideSelection = {default=1};
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
	__property MaxLength = {default=0};
	__property OEMConvert = {default=0};
	__property ParentColor = {default=0};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PasswordChar = {default=0};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
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
	/* TWinControl.CreateParented */ inline __fastcall TElDBEdit(HWND ParentWindow) : Elmaskedit::TCustomElMaskEdit(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElDBMemo : public Elactrls::TElAdvancedMemo
{
	typedef Elactrls::TElAdvancedMemo inherited;
	
private:
	bool FAutoDisplay;
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	bool FFocused;
	bool FMemoLoaded;
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall SetAutoDisplay(bool Value);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall CMGetDataLink(Winapi::Messages::TMessage &Message);
	void __fastcall DataChange(System::TObject* Sender);
	void __fastcall EditingChange(System::TObject* Sender);
	HIDESBASE bool __fastcall GetReadOnly();
	void __fastcall SetFocused(bool Value);
	HIDESBASE void __fastcall SetReadOnly(bool Value);
	void __fastcall UpdateData(System::TObject* Sender);
	MESSAGE void __fastcall WMCut(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Message);
	MESSAGE void __fastcall WMPaste(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMUndo(Winapi::Messages::TMessage &Message);
	
protected:
	DYNAMIC void __fastcall Change();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	virtual void __fastcall LoadMemo();
	__fastcall virtual TElDBMemo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDBMemo();
	__property Data::Db::TField* Field = {read=GetField};
	
__published:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property bool AutoDisplay = {read=FAutoDisplay, write=SetAutoDisplay, default=1};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDBMemo(HWND ParentWindow) : Elactrls::TElAdvancedMemo(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElDBCheckBox : public Elcheckctl::TElCheckBox
{
	typedef Elcheckctl::TElCheckBox inherited;
	
private:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	Elstrutils::TElFString FValueCheck;
	Elstrutils::TElFString FValueUncheck;
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	bool __fastcall GetReadOnly();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall SetReadOnly(bool Value);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall CMGetDataLink(Winapi::Messages::TMessage &Message);
	void __fastcall DataChange(System::TObject* Sender);
	Vcl::Stdctrls::TCheckBoxState __fastcall GetFieldState();
	void __fastcall SetValueCheck(const Elstrutils::TElFString Value);
	void __fastcall SetValueUncheck(const Elstrutils::TElFString Value);
	void __fastcall UpdateData(System::TObject* Sender);
	bool __fastcall ValueMatch(const System::UnicodeString ValueList, const System::UnicodeString Value);
	
protected:
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Toggle();
	
public:
	__fastcall virtual TElDBCheckBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDBCheckBox();
	DYNAMIC void __fastcall Click();
	__property Data::Db::TField* Field = {read=GetField};
	
__published:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property Elstrutils::TElFString ValueChecked = {read=FValueCheck, write=SetValueCheck};
	__property Elstrutils::TElFString ValueUnchecked = {read=FValueUncheck, write=SetValueUncheck};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDBCheckBox(HWND ParentWindow) : Elcheckctl::TElCheckBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElDBRadioGroup : public Elcheckitemgrp::TCustomElRadioGroup
{
	typedef Elcheckitemgrp::TCustomElRadioGroup inherited;
	
private:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	System::Classes::TNotifyEvent FOnChange;
	Elstrutils::TElFString FValue;
	Elunicodestrings::TElWideStrings* FValues;
	bool FInSetValue;
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Message);
	void __fastcall DataChange(System::TObject* Sender);
	Elstrutils::TElFString __fastcall GetButtonValue(int Index);
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	bool __fastcall GetReadOnly();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	HIDESBASE void __fastcall SetItems(Elunicodestrings::TElWideStrings* Value);
	void __fastcall SetReadOnly(bool Value);
	void __fastcall SetValue(Elstrutils::TElFString Value);
	void __fastcall SetValues(Elunicodestrings::TElWideStrings* Value);
	void __fastcall UpdateData(System::TObject* Sender);
	
protected:
	virtual bool __fastcall CanModify();
	DYNAMIC void __fastcall Change();
	DYNAMIC void __fastcall Click();
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	__property Vcl::Dbctrls::TFieldDataLink* DataLink = {read=FDataLink};
	
public:
	__fastcall virtual TElDBRadioGroup(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDBRadioGroup();
	__property Data::Db::TField* Field = {read=GetField};
	__property Elstrutils::TElFString Value = {read=FValue, write=SetValue};
	
__published:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property Elunicodestrings::TElWideStrings* Values = {read=FValues, write=SetValues};
	__property Items = {write=SetItems};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property Align = {default=0};
	__property Alignment = {default=1};
	__property BorderSides;
	__property Caption;
	__property CaptionColor = {default=536870911};
	__property CheckBoxChecked = {default=1};
	__property Color = {default=-16777201};
	__property Columns = {default=1};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Flat;
	__property FlatAlways;
	__property Font;
	__property Hints;
	__property ImageForm;
	__property IsHTML = {default=0};
	__property MoneyFlat = {default=0};
	__property MoneyFlatInactiveColor;
	__property MoneyFlatActiveColor;
	__property MoneyFlatDownColor;
	__property ParentColor = {default=0};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowCheckBox = {default=0};
	__property ShowFocus;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Transparent = {default=0};
	__property Visible = {default=1};
	__property UseXPThemes = {default=1};
	__property CheckSound = {default=0};
	__property SoundMap;
	__property Glyph;
	__property Images;
	__property UseCustomGlyphs = {default=0};
	__property UseImageList = {default=0};
	__property OnClick;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnEnter;
	__property OnExit;
	__property OnStartDrag;
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDBRadioGroup(HWND ParentWindow) : Elcheckitemgrp::TCustomElRadioGroup(ParentWindow) { }
	
};


enum DECLSPEC_DENUM TElNavButtonRole : unsigned char { nbrFirst, nbrPrior, nbrNext, nbrLast, nbrInsert, nbrDelete, nbrEdit, nbrPost, nbrCancel, nbrRefresh, nbrSearch, nbrSetFilter, nbrRemoveFilter, nbrClear, nbrOpen, nbrClose, nbrFindFirst, nbrFindPrior, nbrFindNext, nbrFindLast, nbrCustom };

class PASCALIMPLEMENTATION TElDBNavButton : public Eltoolbar::TCustomElToolButton
{
	typedef Eltoolbar::TCustomElToolButton inherited;
	
protected:
	TElNavButtonRole FRole;
	void __fastcall SetRole(TElNavButtonRole Value);
	virtual System::WideString __fastcall GetArrowThemedClassName();
	virtual int __fastcall GetArrowThemePartID();
	virtual int __fastcall GetArrowThemeStateID();
	virtual System::WideString __fastcall GetThemedClassName();
	virtual int __fastcall GetThemePartID();
	virtual int __fastcall GetThemeStateID();
	virtual void __fastcall SetUseImageList(bool newValue);
	virtual void __fastcall Loaded();
	virtual void __fastcall SetImageList(Vcl::Controls::TImageList* newValue);
	virtual void __fastcall SetImageIndex(int newValue);
	
public:
	virtual void __fastcall AClick(bool Arrow);
	__fastcall virtual TElDBNavButton(System::Classes::TComponent* AOwner);
	
__published:
	__property TElNavButtonRole Role = {read=FRole, write=SetRole, default=20};
	__property Wrap;
	__property LargeGlyph;
	__property NumLargeGlyphs;
	__property Glyph;
	__property NumGlyphs;
	__property OwnerSettings = {default=1};
	__property PullDownMenu;
	__property PopupPlace = {default=0};
	__property DisableAutoPopup = {default=0};
	__property Flat = {default=0};
	__property Layout = {default=0};
	__property Margin = {default=-1};
	__property Spacing = {default=4};
	__property UseArrow = {default=0};
	__property ShadowFollowsColor = {default=1};
	__property ShowGlyph = {default=1};
	__property ShowText = {default=1};
	__property OnArrowClick;
	__property Icon;
	__property TextDrawType = {default=0};
	__property ThinFrame = {default=0};
	__property DownSound = {default=0};
	__property UpSound = {default=0};
	__property ClickSound = {default=0};
	__property ArrowClickSound = {default=0};
	__property SoundMap;
	__property UseIcon = {default=0};
	__property ImageIndex = {default=-1};
	__property UseImageList = {default=0};
	__property OldStyled = {default=0};
	__property Background;
	__property DownBackground;
	__property BackgroundDrawBorder = {default=0};
	__property UseXPThemes = {default=1};
	__property Caption;
	__property Enabled = {default=1};
	__property PopupMenu;
	__property Color;
	__property ParentColor = {default=1};
	__property Align = {default=0};
	__property Font;
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property ShowHint;
	__property Visible = {default=1};
	__property OnClick;
	__property OnDblClick;
	__property OnMouseDown;
	__property OnMouseMove;
	__property OnMouseUp;
	__property OnDragDrop;
	__property OnDragOver;
	__property OnEndDrag;
	__property OnStartDrag;
public:
	/* TCustomElToolButton.Destroy */ inline __fastcall virtual ~TElDBNavButton() { }
	
};


class PASCALIMPLEMENTATION TElDBNavigator : public Eltoolbar::TElToolBar
{
	typedef Eltoolbar::TElToolBar inherited;
	
private:
	System::UnicodeString FDeleteRecordQuestion;
	TElNavDataLink* FDataLink;
	Data::Db::TDataSource* __fastcall GetDataSource();
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	HIDESBASE MESSAGE void __fastcall CMEnabledChanged(Winapi::Messages::TMessage &Message);
	
protected:
	bool FConfirmDelete;
	System::Classes::TNotifyEvent FOnSearch;
	bool FIsToolbar;
	Vcl::Controls::TImageList* FIntImageList;
	void __fastcall ActiveChanged();
	void __fastcall DataChanged();
	void __fastcall EditingChanged();
	virtual void __fastcall DoSearch();
	virtual Eltoolbar::TElToolButtonClass __fastcall GetButtonClass();
	void __fastcall SetIsToolbar(bool Value);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Loaded();
	HIDESBASE MESSAGE void __fastcall CMControlChange(Vcl::Controls::TCMControlChange &Msg);
	
public:
	__fastcall virtual TElDBNavigator(System::Classes::TComponent* AOwner);
	TElDBNavButton* __fastcall FindButtonByRole(TElNavButtonRole Role);
	HIDESBASE TElDBNavButton* __fastcall AddButton(TElNavButtonRole Role);
	__fastcall virtual ~TElDBNavigator();
	
__published:
	__property System::UnicodeString DeleteRecordQuestion = {read=FDeleteRecordQuestion, write=FDeleteRecordQuestion};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property bool ConfirmDelete = {read=FConfirmDelete, write=FConfirmDelete, default=1};
	__property System::Classes::TNotifyEvent OnSearch = {read=FOnSearch, write=FOnSearch};
	__property bool IsToolbar = {read=FIsToolbar, write=SetIsToolbar, default=1};
	__property BtnOffsHorz = {default=0};
	__property BtnOffsVert = {default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDBNavigator(HWND ParentWindow) : Eltoolbar::TElToolBar(ParentWindow) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TElNavDataLink : public Data::Db::TDataLink
{
	typedef Data::Db::TDataLink inherited;
	
private:
	TElDBNavigator* FNavigator;
	
protected:
	virtual void __fastcall EditingChanged();
	virtual void __fastcall DataSetChanged();
	virtual void __fastcall ActiveChanged();
	
public:
	__fastcall TElNavDataLink(TElDBNavigator* ANav);
	__fastcall virtual ~TElNavDataLink();
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TElWideDBEdit : public Eledits::TCustomElEdit
{
	typedef Eledits::TCustomElEdit inherited;
	
private:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	bool FFocused;
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall ResetMaxLength();
	void __fastcall DataChange(System::TObject* Sender);
	void __fastcall ActiveChange(System::TObject* Sender);
	void __fastcall EditingChange(System::TObject* Sender);
	void __fastcall UpdateData(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall CMGetDataLink(Winapi::Messages::TMessage &Message);
	void __fastcall SetFocused(bool Value);
	bool __fastcall GetReadOnly();
	HIDESBASE void __fastcall SetReadOnly(bool Value);
	HIDESBASE MESSAGE void __fastcall WMCut(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMPaste(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMUndo(Winapi::Messages::TMessage &Message);
	
protected:
	virtual void __fastcall Change();
	bool __fastcall EditCanModify();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall Reset();
	
public:
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
	DYNAMIC bool __fastcall UseRightToLeftAlignment();
	__fastcall virtual TElWideDBEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElWideDBEdit();
	__property Data::Db::TField* Field = {read=GetField};
	
__published:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property AutoSize = {default=1};
	__property Alignment;
	__property Background;
	__property BorderSides;
	__property CharCase = {default=0};
	__property UseBackground = {default=0};
	__property RTLContent;
	__property PasswordChar = {default=0};
	__property MaxLength = {default=0};
	__property Transparent;
	__property FlatFocusedScrollBars = {default=0};
	__property WantTabs = {default=0};
	__property LeftMargin = {default=1};
	__property RightMargin = {default=2};
	__property TopMargin = {default=1};
	__property BorderStyle;
	__property AutoSelect = {default=0};
	__property HandleDialogKeys = {default=0};
	__property HideSelection = {default=1};
	__property TabSpaces = {default=4};
	__property Lines = {stored=false};
	__property Text;
	__property ImageForm;
	__property ActiveBorderType = {default=1};
	__property Flat = {default=0};
	__property InactiveBorderType = {default=3};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property WordWrap = {default=0};
	__property ScrollBars = {default=0};
	__property VertScrollBarStyles;
	__property HorzScrollBarStyles;
	__property UseCustomScrollBars;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property OnChange;
	__property OnSelectionChange;
	__property Align = {default=0};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=1};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Visible = {default=1};
	__property UseXPThemes = {default=1};
	__property Cursor = {default=0};
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
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
	/* TWinControl.CreateParented */ inline __fastcall TElWideDBEdit(HWND ParentWindow) : Eledits::TCustomElEdit(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElWideDBMemo : public Eledits::TCustomElEdit
{
	typedef Eledits::TCustomElEdit inherited;
	
private:
	bool FAutoDisplay;
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	bool FFocused;
	bool FMemoLoaded;
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall SetAutoDisplay(bool Value);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TWMNoParams &Message);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall CMGetDataLink(Winapi::Messages::TMessage &Message);
	void __fastcall DataChange(System::TObject* Sender);
	void __fastcall EditingChange(System::TObject* Sender);
	bool __fastcall GetReadOnly();
	void __fastcall SetFocused(bool Value);
	HIDESBASE void __fastcall SetReadOnly(bool Value);
	void __fastcall UpdateData(System::TObject* Sender);
	HIDESBASE MESSAGE void __fastcall WMCut(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMPaste(Winapi::Messages::TMessage &Message);
	MESSAGE void __fastcall WMUndo(Winapi::Messages::TMessage &Message);
	
protected:
	virtual void __fastcall Change();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Loaded();
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	
public:
	virtual void __fastcall LoadMemo();
	__fastcall virtual TElWideDBMemo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElWideDBMemo();
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
	DYNAMIC bool __fastcall UseRightToLeftAlignment();
	__property Data::Db::TField* Field = {read=GetField};
	
__published:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property bool AutoDisplay = {read=FAutoDisplay, write=SetAutoDisplay, default=1};
	__property bool ReadOnly = {read=GetReadOnly, write=SetReadOnly, default=0};
	__property AutoSize = {default=1};
	__property Alignment;
	__property Background;
	__property BorderSides;
	__property CharCase = {default=0};
	__property UseBackground = {default=0};
	__property RTLContent;
	__property PasswordChar = {default=0};
	__property MaxLength = {default=0};
	__property Transparent;
	__property FlatFocusedScrollBars = {default=0};
	__property WantTabs = {default=0};
	__property LeftMargin = {default=1};
	__property RightMargin = {default=2};
	__property TopMargin = {default=1};
	__property BorderStyle;
	__property AutoSelect = {default=0};
	__property HandleDialogKeys = {default=0};
	__property HideSelection = {default=1};
	__property TabSpaces = {default=4};
	__property Lines = {stored=false};
	__property Text;
	__property ImageForm;
	__property ActiveBorderType = {default=1};
	__property Flat = {default=0};
	__property InactiveBorderType = {default=3};
	__property LineBorderActiveColor;
	__property LineBorderInactiveColor;
	__property WordWrap = {default=0};
	__property ScrollBars = {default=0};
	__property VertScrollBarStyles;
	__property HorzScrollBarStyles;
	__property UseCustomScrollBars;
	__property OnMouseEnter;
	__property OnMouseLeave;
	__property OnResize;
	__property OnChange;
	__property OnSelectionChange;
	__property Align = {default=0};
	__property Color = {default=-16777211};
	__property Ctl3D;
	__property DragCursor = {default=-12};
	__property DragMode = {default=0};
	__property Enabled = {default=1};
	__property Font;
	__property ParentColor = {default=1};
	__property ParentCtl3D = {default=1};
	__property ParentFont = {default=1};
	__property ParentShowHint = {default=1};
	__property PopupMenu;
	__property ShowHint;
	__property TabOrder = {default=-1};
	__property TabStop = {default=1};
	__property Visible = {default=1};
	__property UseXPThemes = {default=1};
	__property Cursor = {default=0};
	__property ImeMode = {default=3};
	__property ImeName = {default=0};
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
	/* TWinControl.CreateParented */ inline __fastcall TElWideDBMemo(HWND ParentWindow) : Eledits::TCustomElEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eldbctrls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELDBCTRLS)
using namespace Eldbctrls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EldbctrlsHPP
