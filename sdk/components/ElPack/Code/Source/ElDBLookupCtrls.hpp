// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElDBLookupCtrls.pas' rev: 35.00 (Windows)

#ifndef EldblookupctrlsHPP
#define EldblookupctrlsHPP

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
#include <ElACtrls.hpp>
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eldblookupctrls
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElDBLookupListControl;
class DELPHICLASS TElDBLookUpComboControl;
class DELPHICLASS TElDBLookupListBox;
class DELPHICLASS TElDBLookupComboBox;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElDBLookupListControl : public Vcl::Dbctrls::TDBLookupControl
{
	typedef Vcl::Dbctrls::TDBLookupControl inherited;
	
private:
	TElDBLookupListBox* FElDBLookupListBox;
	
protected:
	virtual void __fastcall KeyValueChanged();
	virtual void __fastcall UpdateListFields();
	void __fastcall SelectCurrent();
	void __fastcall Select(int Value);
	
public:
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
public:
	/* TDBLookupControl.Create */ inline __fastcall virtual TElDBLookupListControl(System::Classes::TComponent* AOwner) : Vcl::Dbctrls::TDBLookupControl(AOwner) { }
	/* TDBLookupControl.Destroy */ inline __fastcall virtual ~TElDBLookupListControl() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDBLookupListControl(HWND ParentWindow) : Vcl::Dbctrls::TDBLookupControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElDBLookUpComboControl : public Vcl::Dbctrls::TDBLookupControl
{
	typedef Vcl::Dbctrls::TDBLookupControl inherited;
	
private:
	TElDBLookupComboBox* FElDBLookupComboBox;
	
protected:
	virtual void __fastcall KeyValueChanged();
	virtual void __fastcall UpdateListFields();
	void __fastcall SelectCurrent();
	void __fastcall Select(int Value);
	
public:
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
public:
	/* TDBLookupControl.Create */ inline __fastcall virtual TElDBLookUpComboControl(System::Classes::TComponent* AOwner) : Vcl::Dbctrls::TDBLookupControl(AOwner) { }
	/* TDBLookupControl.Destroy */ inline __fastcall virtual ~TElDBLookUpComboControl() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDBLookUpComboControl(HWND ParentWindow) : Vcl::Dbctrls::TDBLookupControl(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElDBLookupListBox : public Elactrls::TElAdvancedListBox
{
	typedef Elactrls::TElAdvancedListBox inherited;
	
	
private:
	typedef System::DynamicArray<System::Classes::TStrings*> _TElDBLookupListBox__1;
	
	typedef System::DynamicArray<int> _TElDBLookupListBox__2;
	
	
private:
	TElDBLookupListControl* FElDBLookupControl;
	System::Classes::TNotifyEvent FOnChange;
	bool FReadOnly;
	int FFieldCount;
	_TElDBLookupListBox__1 FFields;
	_TElDBLookupListBox__2 FFieldWidth;
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
	System::UnicodeString __fastcall GetSelectedString();
	HIDESBASE MESSAGE void __fastcall WMLButtonDown(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMLButtonDblClk(Winapi::Messages::TWMMouse &Message);
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Message);
	System::Classes::TStrings* __fastcall GetFields(int Index);
	HIDESBASE MESSAGE void __fastcall WMSize(Winapi::Messages::TWMSize &Message);
	HIDESBASE MESSAGE void __fastcall WMKeyDown(Winapi::Messages::TWMKey &Message);
	
protected:
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	void __fastcall SetFieldCount(int Value);
	void __fastcall ClearFields();
	HIDESBASE void __fastcall AddItem(const System::UnicodeString Value, int Field);
	__property System::Classes::TStrings* Fields[int Index] = {read=GetFields};
	
public:
	__fastcall virtual TElDBLookupListBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDBLookupListBox();
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
	__property System::Variant KeyValue = {read=GetKeyValue, write=SetKeyValue};
	__property System::UnicodeString SelectedItem = {read=GetSelectedString};
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
	/* TWinControl.CreateParented */ inline __fastcall TElDBLookupListBox(HWND ParentWindow) : Elactrls::TElAdvancedListBox(ParentWindow) { }
	
};


class PASCALIMPLEMENTATION TElDBLookupComboBox : public Elactrls::TElAdvancedComboBox
{
	typedef Elactrls::TElAdvancedComboBox inherited;
	
	
private:
	typedef System::DynamicArray<bool> _TElDBLookupComboBox__1;
	
	typedef System::DynamicArray<System::Classes::TStrings*> _TElDBLookupComboBox__2;
	
	typedef System::DynamicArray<int> _TElDBLookupComboBox__3;
	
	
private:
	TElDBLookUpComboControl* FElDBLookupControl;
	_TElDBLookupComboBox__1 FSelected;
	int FMaxItems;
	int FFieldCount;
	_TElDBLookupComboBox__2 FFields;
	_TElDBLookupComboBox__3 FFieldWidth;
	bool FReadOnly;
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
	System::UnicodeString __fastcall GetSelectedString();
	HIDESBASE MESSAGE void __fastcall WMChar(Winapi::Messages::TWMKey &Message);
	System::Classes::TStrings* __fastcall GetFields(int Index);
	HIDESBASE MESSAGE void __fastcall WMMouseWheel(Winapi::Messages::TWMMouseWheel &Message);
	void __fastcall SetHScrollBarWidth();
	HIDESBASE MESSAGE void __fastcall CMFontChanged(Winapi::Messages::TMessage &Msg);
	
protected:
	virtual void __fastcall EditWndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall ListWndProc(Winapi::Messages::TMessage &Message);
	virtual void __fastcall DrawItem(int Index, const System::Types::TRect &Rect, Winapi::Windows::TOwnerDrawState State);
	void __fastcall SetFieldCount(int Value);
	void __fastcall ClearFields();
	HIDESBASE void __fastcall AddItem(const System::UnicodeString Value, int Field);
	__property System::Classes::TStrings* Fields[int Index] = {read=GetFields};
	DYNAMIC void __fastcall MouseDown(System::Uitypes::TMouseButton Button, System::Classes::TShiftState Shift, int X, int Y);
	DYNAMIC void __fastcall Change();
	
public:
	__fastcall virtual TElDBLookupComboBox(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDBLookupComboBox();
	DYNAMIC bool __fastcall ExecuteAction(System::Classes::TBasicAction* Action);
	virtual bool __fastcall UpdateAction(System::Classes::TBasicAction* Action);
	__property System::Variant KeyValue = {read=GetKeyValue, write=SetKeyValue};
	__property System::UnicodeString SelectedItem = {read=GetSelectedString};
	__property int ListFieldIndex = {read=GetListFieldIndex, write=SetListFieldIndex, nodefault};
	__property Data::Db::TField* Field = {read=GetField};
	__property bool Selected[int Index] = {read=GetSelected, write=SetSelected};
	
__published:
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property Data::Db::TDataSource* ListSource = {read=GetListSource, write=SetListSource};
	__property System::UnicodeString DataField = {read=GetDataFieldName, write=SetDataFieldName};
	__property System::UnicodeString ListField = {read=GetListFieldName, write=SetListFieldName};
	__property System::UnicodeString KeyField = {read=GetKeyFieldName, write=SetKeyFieldName};
	__property bool ReadOnly = {read=FReadOnly, write=FReadOnly, default=0};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDBLookupComboBox(HWND ParentWindow) : Elactrls::TElAdvancedComboBox(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eldblookupctrls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELDBLOOKUPCTRLS)
using namespace Eldblookupctrls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EldblookupctrlsHPP
