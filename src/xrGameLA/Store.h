#pragma once
#ifndef __STORE_H__
#define __STORE_H__
#include "object_interfaces.h"
#include "script_export_space.h"
#include "alife_space.h"
//#include "..\xrNETServer\NET_utils.h"
//#include "xr_time.h"
/*
enum TypeOfData 
{
	lua_nil,
	lua_bool,
	lua_string,
	lua_number,
	lua_vector,
	lua_table,
	lua_u32,
	lua_u16,
	lua_u8,
	lua_s32,
	lua_s16,
	lua_s8,
	lua_float,
	lua_ctime
};

namespace luabind
{
	class object;
};

struct lua_State;

#define WRITE_TEMPLATE(T, type, method) \
	template <> static void Write<T>(NET_Packet *P, shared_str str, T val) { \
		P->w_stringZ	(*str); \
		P->w_u8			(type); \
		P->method		(val);  \
		Msg("~ %s %d", *str, type); \
	}
*/
class CStoreHouse : public IPureSerializeObject<IReader, IWriter>
{
	public:
								CStoreHouse		();
		virtual					~CStoreHouse	();
		virtual void			load			(IReader& stream);
		virtual void			save			(IWriter& stream);
	
//	private:
//				void			OnSave			();
//				u16				OnLoad			(IReader &file_stream);
//				luabind::object& UnserializeTable(IReader &file_stream, u16& total);
//				void			SerializeTable	(luabind::object& tbl);

	public:
		DECLARE_SCRIPT_REGISTER_FUNCTION;
/*
	private:
		struct SStorageHelper
		{
			template <typename T> static void Write(NET_Packet *P, shared_str str, T val)
			{
				FATAL("unsupported type");
			}

			WRITE_TEMPLATE(bool, lua_bool, w_bool);

			WRITE_TEMPLATE(double, lua_number, w_double);
			WRITE_TEMPLATE(Fvector3, lua_vector, w_vec3);

			WRITE_TEMPLATE(u32, lua_u32, w_u32);
			WRITE_TEMPLATE(s32, lua_s32, w_s32);
			WRITE_TEMPLATE(u16, lua_u16, w_u16);
			WRITE_TEMPLATE(s16, lua_s16, w_s16);
			WRITE_TEMPLATE(u8, lua_u8, w_u8);
			WRITE_TEMPLATE(s8, lua_s8, w_s8);
			WRITE_TEMPLATE(float, lua_float, w_float);

			WRITE_TEMPLATE(LPCSTR, lua_string, w_stringZ);
			WRITE_TEMPLATE(shared_str, lua_string, w_stringZ);

			template <> static void CStoreHouse::SStorageHelper::Write<xrTime&>(NET_Packet *P, shared_str str, xrTime& val) 
			{ 
				P->w_stringZ	(*str); 
				P->w_u8			(lua_ctime); 
				P->w_u64		(val.time());  
			}

		//	static void*	Read	(NET_Packet *P, shared_str* str);
		};

		NET_Packet*					m_buffer;
		lua_State*					m_lua;
*/
};
											


add_to_type_list(CStoreHouse)
#undef script_type_list
#define script_type_list save_type_list(CStoreHouse)

#endif