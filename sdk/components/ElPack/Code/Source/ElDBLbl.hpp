// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'ElDBLbl.pas' rev: 35.00 (Windows)

#ifndef EldblblHPP
#define EldblblHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Data.DB.hpp>
#include <Vcl.DBCtrls.hpp>
#include <ElHTMLLbl.hpp>
#include <ElStrUtils.hpp>
#include <Vcl.Forms.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>

//-- user supplied -----------------------------------------------------------

namespace Eldblbl
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TElDBLabel;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TElDBLabel : public Elhtmllbl::TElHTMLLabel
{
	typedef Elhtmllbl::TElHTMLLabel inherited;
	
private:
	Vcl::Dbctrls::TFieldDataLink* FDataLink;
	MESSAGE void __fastcall CMGetDataLink(Winapi::Messages::TMessage &Message);
	void __fastcall DataChange(System::TObject* Sender);
	System::UnicodeString __fastcall GetDataField();
	Data::Db::TDataSource* __fastcall GetDataSource();
	Data::Db::TField* __fastcall GetField();
	void __fastcall SetDataField(const System::UnicodeString Value);
	void __fastcall SetDataSource(Data::Db::TDataSource* Value);
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	Elstrutils::TElFString __fastcall GetFieldText();
	
public:
	__fastcall virtual TElDBLabel(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TElDBLabel();
	__property Data::Db::TField* Field = {read=GetField};
	
__published:
	__property System::UnicodeString DataField = {read=GetDataField, write=SetDataField};
	__property Data::Db::TDataSource* DataSource = {read=GetDataSource, write=SetDataSource};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Eldblbl */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_ELDBLBL)
using namespace Eldblbl;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EldblblHPP
