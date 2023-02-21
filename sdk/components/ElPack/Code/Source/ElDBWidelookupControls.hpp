// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElDBWideLookupControls.pas' rev: 35.00 (Windows)

#ifndef EldbwidelookupcontrolsHPP
#define EldbwidelookupcontrolsHPP

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
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.StdCtrls.hpp>
#include <ElListBox.hpp>
#include <ElCombos.hpp>
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>
#include <ElStrUtils.hpp>
#include <ElEdits.hpp>
#include <ElXPThemedControl.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eldbwidelookupcontrols
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElWideDBLookupListControl;
class DELPHICLASS TElWideDBLookUpComboControl;
class DELPHICLASS TElWideDBLookupListBox;
class DELPHICLASS TElWideDBLookupComboBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElWideDBLookupListControl : public Vcl::Dbctrls::TDBLookupControl
{
	typedef Vcl::Dbctrls::TDBLookupControl inherited;
	
private:
	TElWideDBLookupListBox* FElDBWideLookupListBox;
	
protected:
	virtual void __fastcall KeyValueChanged();
	virtual void __fastcall UpdateListFields();
	void __fastcall SelectCurrent();
	void __fastcall Select(int Value);
	
public:
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
public:
	/* TDBLookupControl.Create */ inline __fastcall virtual TElWideDBLookupListControl(System::Classes::TComponent* AOwner) : Vcl::Dbctrls::TDBLookupControl(AOwner) { }
	/* TDBLookupControl.Destroy */ inline __fastcall virtual ~TElWideDBLookupListControl() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElWideDBLookupListControl(HWND ParentWindow) : Vcl::Dbctrls::TDBLookupControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElWideDBLookUpComboControl : public Vcl::Dbctrls::TDBLookupControl
{
	typedef Vcl::Dbctrls::TDBLookupControl inherited;
	
private:
	TElWideDBLookupComboBox* FElDBWideLookupComboBox;
	
protected:
	virtual void __fastcall KeyValueChanged();
	virtual void __fastcall UpdateListFields();
	void __fastcall SelectCurrent();
	void __fastcall Select(int Value);
	
public:
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
public:
	/* TDBLookupControl.Create */ inline __fastcall virtual TElWideDBLookUpComboControl(System::Classes::TComponent* AOwner) : Vcl::Dbctrls::TDBLookupControl(AOwner) { }
	/* TDBLookupControl.Destroy */ inline __fastcall virtual ~TElWideDBLookUpComboControl() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElWideDBLookUpComboControl(HWND ParentWindow) : Vcl::Dbctrls::TDBLookupControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElWideDBLookupListBox : public Ellistbox::TElListBox
{
	typedef Ellistbox::TElListBox inherited;
	
	
private:
	typedef System::DynamicArray<System::Classes::TStrings*> _TElWideDBLookupListBox__1;
	
	typedef System::DynamicArray<int> _TElWideDBLookupListBox__2;
	
	
private:
	System::Classes::TNotifyEvent FOnChange;
	TElWideDBLookupListControl* FElDBWideLookupControl;
	bool FReadOnly;
	int FFieldCount;
	_TElWideDBLookupListBox__1 FFields;
	_TElWideDBLookupListBox__2 FFieldWidth;
	int FIndex;
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall SetListSource(Data::Db::TDataSource* Value);
	void __fastcall SetDataFieldName(const System::UnicodeString Value);
	void __fastcall SetListFieldName(const System::UnicodeString Value);
	void __fastcall SetKeyFieldName(const System::UnicodeString Value);
	void __fastcall SetKeyValue(const System::Variant &Value);
	void __fastcall SetListFieldIndex(int Value);
	Data::Db::TField* __fastcall GetField();
	int __fastcall GetListFieldIndex();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TDataSource* __fastcall GetListSource();
	System::UnicodeString __fastcall GetListFieldName();
	System::UnicodeString __fastcall GetDataFieldName();
	System::UnicodeString __fastcall GetKeyFieldName();
	System::Variant __fastcall GetKeyValue();
	System::WideString __fastcall GetSelectedString();
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Message);
	System::Classes::TStrings* __fastcall GetFields(int Index);
	
protected:
	virtual void __fastcall SetHorizontalExtent();
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &R, Winapi::Windows::TOwnerDrawState State);
	virtual void __fastcall DoDrawText(Vcl::Graphics::TCanvas* ACanvas, const Elstrutils::TElFString ACaption, System::Types::TRect &Rect, int Flags);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SetFieldCount(int Value);
	void __fastcall ClearFields();
	void __fastcall AddItem(const System::UnicodeString Value, int Field);
	__property System::Classes::TStrings* Fields[int Index] = {read=GetFields};
	
public:
	__fastcall virtual TElWideDBLookupListBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElWideDBLookupListBox();
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
	__property System::Variant KeyValue = {read=GetKeyValue, write=SetKeyValue};
	__property System::WideString SelectedItem = {read=GetSelectedString};
	__property int ListFieldIndex = {read=GetListFieldIndex, write=SetListFieldIndex, nodefault};
	__property Data::Db::TField* Field = {read=GetField};
	
__published:
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property Data::Db::TDataSource* ListSource = {read=GetListSource, write=SetListSource};
	__property System::UnicodeString DataField = {read=GetDataFieldName, write=SetDataFieldName};
	__property System::UnicodeString ListField = {read=GetListFieldName, write=SetListFieldName};
	__property System::UnicodeString KeyField = {read=GetKeyFieldName, write=SetKeyFieldName};
	__property System::Classes::TNotifyEvent OnChange = {read=FOnChange, write=FOnChange};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElWideDBLookupListBox(HWND ParentWindow) : Ellistbox::TElListBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElWideDBLookupComboBox : public Elcombos::TElComboBox
{
	typedef Elcombos::TElComboBox inherited;
	
	
private:
	typedef System::DynamicArray<System::Classes::TStrings*> _TElWideDBLookupComboBox__1;
	
	typedef System::DynamicArray<int> _TElWideDBLookupComboBox__2;
	
	
private:
	System::Classes::TWndMethod FSaveListWindowProc;
	TElWideDBLookUpComboControl* FElDBWideLookupControl;
	int FFieldCount;
	_TElWideDBLookupComboBox__1 FFields;
	_TElWideDBLookupComboBox__2 FFieldWidth;
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall SetListSource(Data::Db::TDataSource* Value);
	void __fastcall SetDataFieldName(const System::UnicodeString Value);
	void __fastcall SetListFieldName(const System::UnicodeString Value);
	void __fastcall SetKeyFieldName(const System::UnicodeString Value);
	void __fastcall SetKeyValue(const System::Variant &Value);
	void __fastcall SetListFieldIndex(int Value);
	void __fastcall SetSelected(int index, bool Value);
	bool __fastcall GetSelected(int index);
	Data::Db::TField* __fastcall GetField();
	int __fastcall GetListFieldIndex();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TDataSource* __fastcall GetListSource();
	System::UnicodeString __fastcall GetListFieldName();
	System::UnicodeString __fastcall GetDataFieldName();
	System::UnicodeString __fastcall GetKeyFieldName();
	System::Variant __fastcall GetKeyValue();
	System::WideString __fastcall GetSelectedString();
	void __fastcall ListWindowProc(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TMessage &Message);
	HIDESBASE MESSAGE void __fastcall WMMouseMove(Winapi::Messages::TWMMouse &Message);
	System::Classes::TStrings* __fastcall GetFields(int Index);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Message);
	
protected:
	void __fastcall SetFieldCount(int Value);
	void __fastcall ClearFields();
	void __fastcall AddItem(const System::UnicodeString Value, int Field);
	__property System::Classes::TStrings* Fields[int Index] = {read=GetFields};
	
public:
	__fastcall virtual TElWideDBLookupComboBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElWideDBLookupComboBox();
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
	void __fastcall DoDrawText(Vcl::Graphics::TCanvas* ACanvas, int Index, System::Types::TRect &Rect, int Flags);
	__property System::Variant KeyValue = {read=GetKeyValue, write=SetKeyValue};
	__property System::WideString SelectedItem = {read=GetSelectedString};
	__property int ListFieldIndex = {read=GetListFieldIndex, write=SetListFieldIndex, nodefault};
	__property Data::Db::TField* Field = {read=GetField};
	__property bool Selected[int Index] = {read=GetSelected, write=SetSelected};
	
__published:
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property Data::Db::TDataSource* ListSource = {read=GetListSource, write=SetListSource};
	__property System::UnicodeString DataField = {read=GetDataFieldName, write=SetDataFieldName};
	__property System::UnicodeString ListField = {read=GetListFieldName, write=SetListFieldName};
	__property System::UnicodeString KeyField = {read=GetKeyFieldName, write=SetKeyFieldName};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElWideDBLookupComboBox(HWND ParentWindow) : Elcombos::TElComboBox(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eldbwidelookupcontrols */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELDBWIDELOOKUPCONTROLS)
using namespace Eldbwidelookupcontrols;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EldbwidelookupcontrolsHPP
