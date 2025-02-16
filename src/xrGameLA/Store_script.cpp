#include "stdafx.h"
#include "Store.h"
#include "pch_script.h"

using namespace luabind;

#pragma optimize("s",on)


//void script_call_add_u64(CStoreHouse* self,LPCSTR name)//,unsigned	__int64 value)
//{
//	self->add_u64(0);
//}

void CStoreHouse::script_register(lua_State *L)
{
/*
	module(L)
	[
		class_<CStoreHouse>("StoreHouse")
			.def(constructor<>())
			
			.def("add_string",					&CStoreHouse::add_string)
			
			.def("add_vector",					&CStoreHouse::add_vector)
			.def("add_boolean",					&CStoreHouse::add_boolean)
			.def("add_number",					&CStoreHouse::add_number)
			.def("add_table",					&CStoreHouse::add_table)


			
			.def("get_string",					&CStoreHouse::get_string)
	
			.def("get_vector",					&CStoreHouse::get_vector)
			.def("get_number",					&CStoreHouse::get_number)
			.def("get_boolean",					&CStoreHouse::get_boolean)
			.def("get_table",					&CStoreHouse::get_table)

		//	.def("get_data_type",				&CStoreHouse::get_data_type)
			.def("data_exist",					&CStoreHouse::data_exist)
			.def("get_vector",					&CStoreHouse::get_vector)
			
			.def("delete_data",					&CStoreHouse::delete_data)

		
	];
*/
}


