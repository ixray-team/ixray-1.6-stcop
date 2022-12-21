// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElDBDTPick.pas' rev: 34.00 (Windows)

#ifndef EldbdtpickHPP
#define EldbdtpickHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>
#include <ElDTPick.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <ElXPThemedControl.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eldbdtpick
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElDBDateTimePicker;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElDBDateTimePicker : public Eldtpick::TElDateTimePicker
{
	typedef Eldtpick::TElDateTimePicker inherited;
	
private:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	bool FNowForNULLValues;
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TWMNoParams &Message);
	MESSAGE void __fastcall CMGetDataLink(Winapi::Messages::TMessage &Message);
	void __fastcall DataChange(System::TObject* Sender);
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	void __fastcall UpdateData(System::TObject* Sender);
	
protected:
	virtual bool __fastcall GetReadOnly();
	virtual void __fastcall SetReadOnly(bool Value);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall SetModified(bool newValue);
	virtual void __fastcall TriggerChangeEvent();
	DYNAMIC void __fastcall KeyDown(System::Word &Key, System::Classes::TShiftState Shift);
	HIDESBASE MESSAGE void __fastcall CMEnter(Winapi::Messages::TMessage &Message);
	virtual void __fastcall CloseUp(bool AcceptValue);
	virtual void __fastcall DropDown();
	
public:
	__fastcall virtual TElDBDateTimePicker(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDBDateTimePicker();
	__property Data::Db::TField* Field = {read=GetField};
	
__published:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
	__property bool NowForNULLValues = {read=FNowForNULLValues, write=FNowForNULLValues, nodefault};
public:
	/* TWinControl.CreateParented */ inline __fastcall TElDBDateTimePicker(HWND ParentWindow) : Eldtpick::TElDateTimePicker(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eldbdtpick */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELDBDTPICK)
using namespace Eldbdtpick;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EldbdtpickHPP
