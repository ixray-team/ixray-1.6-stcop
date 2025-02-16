#include "stdafx.h"
#include "Store.h"
#include "pch_script.h"
#include "ai_space.h"
#include "alife_simulator.h"
#include "alife_space.h"
#include "../xrScripts/script_engine_space.h"
#include "../xrScripts/script_engine.h"

CStoreHouse::CStoreHouse()
{
//	m_buffer = new NET_Packet		();
//	m_buffer->write_start				();
}

CStoreHouse::~CStoreHouse() 
{
//	xr_delete							(m_buffer);
}
/*
void CStoreHouse::add(shared_str name, void* ptr_data, u32 size, TypeOfData _type)
{
	//add_data_exist(name);
	if (update(name, ptr_data, size, _type)) return;

	void* ptr = xr_malloc(size);
	xr_memcpy(ptr,ptr_data,size);

	StoreData d;
	d.data = ptr;
	d.type = _type;
	data[name] = d;
}

void CStoreHouse::add_boolean(LPCSTR name,bool b)
{
	add(name,&b,sizeof(b),lua_bool);
}

void CStoreHouse::add_number(LPCSTR name, double number)
{
	add(name,&number,sizeof(number),lua_number);
}

void CStoreHouse::add_string(LPCSTR name, LPCSTR string)
{
	add(name,(void*)string,xr_strlen(string)+1,lua_string);
}

void CStoreHouse::add_vector(LPCSTR name, Fvector v)
{
	add(name,&v,sizeof(float)*3,lua_vector);
}

void CStoreHouse::add_table(LPCSTR name, LPCSTR string)
{
	add(name,(void*)string,xr_strlen(string)+1,lua_table);
}

bool CStoreHouse::update(shared_str name, void *ptr_data, u32 size, TypeOfData _type)
{
	xr_map<shared_str,StoreData>::iterator it = data.find(name);
	if (it == data.end()) 
		return false;
//	it->second.type = _type;
	xr_delete(it->second.data);
	it->second.data = xr_malloc(size);
	xr_memcpy(it->second.data,ptr_data,size);
	return true;
}

void CStoreHouse::add_data_exist(shared_str name)
{
	xr_map<shared_str,StoreData>::iterator it = data.find(name);
	R_ASSERT3(data.find(name)==data.end(),"Can't save data with the same name ",name.c_str());
}

void CStoreHouse::get_data_exist(shared_str name)
{
	xr_map<shared_str,StoreData>::iterator it = data.find(name);
	xr_map<shared_str,StoreData>::iterator it_end = data.end();

	R_ASSERT3(data.find(name)!=data.end(),"Data doesn't exist! ",name.c_str());
}

void CStoreHouse::delete_data(LPCSTR c_name)
{
	shared_str name(c_name);
	xr_map<shared_str,StoreData>::iterator it = data.find(name);
	R_ASSERT3(it!=data.end(),"Data doesn't exist! ",name.c_str());
	xr_delete(it->second.data);
	data.erase(it);
}

void CStoreHouse::get(shared_str name,void* p,u32 size)
{
	get_data_exist(name);
	xr_memcpy(p,data[name].data,size);
}

bool CStoreHouse::get_boolean(LPCSTR name)
{
	bool tmp;
	get(name,&tmp,sizeof(bool));
	return tmp;
}

double CStoreHouse::get_number(LPCSTR name)
{
	double tmp;
	get(name,&tmp,sizeof(tmp));
	return tmp;
}

LPCSTR CStoreHouse::get_string(LPCSTR name)
{
	shared_str s;
	get_data_exist(name);
	return LPCSTR(data[name].data);
}

Fvector CStoreHouse::get_vector(LPCSTR name)
{
	Fvector v;
	get(name,&v,sizeof(float)*3);
	return v;
}

LPCSTR CStoreHouse::get_table(LPCSTR name)
{
	shared_str s;
	get_data_exist(name);
	return LPCSTR(data[name].data);
}

bool CStoreHouse::data_exist(LPCSTR name)
{
	shared_str str(name);
	xr_map<shared_str, StoreData>::iterator it= data.find(str);
	xr_map<shared_str, StoreData>::iterator it_end = data.end();
	bool result = it!=it_end;
	return result;
}

LPCSTR CStoreHouse::get_data_type(LPCSTR name)
{
	shared_str s_name(name);
	xr_map<shared_str, StoreData>::iterator it = data.find(s_name);
	if (it!=data.end()){
		return get_data_type(it->second.type);
	}
	return "ERROR";
}

LPCSTR CStoreHouse::get_data_type(TypeOfData d)
{
	switch(d) {
		case lua_bool : return "boolean";
		case lua_vector : return "vector";
		case lua_string : return "string";
		case lua_nil : return "nil";
		case lua_number : return "number";
		case lua_table : return "table";
		case lua_u32: return "u32";
		case lua_s32: return "s32";
		case lua_u16: return "u16";
		case lua_s16: return "s16";
		case lua_u8: return "u8";
		case lua_s8: return "s8";
		case lua_float: return "float";
		case lua_ctime: return "CTime";
	}
	return "ERROR";
}

u32 CStoreHouse::type_to_size(StoreData d)
{
	switch (d.type) {
		case lua_bool: return sizeof(bool);
		case lua_table:
		case lua_string: return xr_strlen((char*)d.data) * sizeof(char)+1;
		case lua_vector: return 3*sizeof(float);
		case lua_number: return sizeof(double);
		case lua_u32: return sizeof(u32);
		case lua_s32: return sizeof(s32);
		case lua_u16: return sizeof(u16);
		case lua_s16: return sizeof(s16);
		case lua_u8: return sizeof(u8);
		case lua_s8: return sizeof(s8);
		case lua_float: return sizeof(float);
		case lua_ctime: return sizeof(u64);
	};
	R_ASSERT2(0,make_string("StoreHouse unknown type [%d]", d.type));
	return u32(-1);
}


void CStoreHouse::save(IWriter &memory_stream)
{
	Msg("* Writing Store...");
	memory_stream.open_chunk	(STORE_CHUNK_DATA);
	xr_map<shared_str,StoreData>::iterator it, last;
	

	if (ai().script_engine().ready())
	{
		luabind::functor<void>		func;
		string256					func_name;
		xr_strcpy(func_name,pSettings->r_string("lost_alpha_cfg","on_save_store_callback"));
		R_ASSERT					(ai().script_engine().functor<void>(func_name,func));
		func						();	
	}

	memory_stream.w_u16(data.size());
	for (it=data.begin(),last=data.end();it!=last;++it)
	{
		memory_stream.w_stringZ(it->first);
		switch (it->second.type)
		{
			case lua_number:
			{	
				TypeOfData num_type;
				double number; 
				xr_memcpy(&number, it->second.data, sizeof(double));
				int i = (int) number;
				int d = ((int) (i * prec) % prec);
				if (!d && i <= type_max(u32) && i >= type_min(s32))
				{
					if (i >= 0)
					{
						if (i <= type_max(u8))
							num_type = lua_u8;
						else if (i <= type_max(u16))
							num_type = lua_u16;
						else
							num_type = lua_u32;
					}
					else
					{
						if (i >= type_min(s8) && i <= type_max(s8))
							num_type = lua_s8;
						else if (i >= type_min(s16) && i <= type_max(s16))
							num_type = lua_s16;
						else
							num_type = lua_s32;
					}
					it->second.type = num_type;
				}
				break;
			}
			default:
			{
				break;
			}
		}
		memory_stream.w_u8(it->second.type);
		memory_stream.w(it->second.data, type_to_size(it->second));
	}
	memory_stream.close_chunk	();

	Msg("* %d store values successfully saved", data.size());
}

#define CAST_HELPER_MACRO(type, T) case type: T val##T; xr_memcpy(&val##T, d.data, sizeof(T)); f(name, type_name, val##T); break;

namespace detail
{
	
	static void CallHelper(luabind::functor<void>& f, LPCSTR name, StoreData &d, LPCSTR type_name)
	{
		switch (d.type)
		{
			CAST_HELPER_MACRO(lua_bool, bool);
		
			CAST_HELPER_MACRO(lua_number, double);
			CAST_HELPER_MACRO(lua_vector, Fvector);
			
			CAST_HELPER_MACRO(lua_u32, u32);
			CAST_HELPER_MACRO(lua_s32, s32);
			CAST_HELPER_MACRO(lua_u16, u16);
			CAST_HELPER_MACRO(lua_s16, s16);
			CAST_HELPER_MACRO(lua_u8, u8);
			CAST_HELPER_MACRO(lua_s8, s8);
			CAST_HELPER_MACRO(lua_float, float);
			case lua_ctime: u64 val; xr_memcpy(&val, d.data, sizeof(u64)); f(name, type_name, xrTime(val)); break;
			case lua_table:
			case lua_string: f(name, type_name, LPCSTR(d.data)); break;

		}
	}
	
};

void CStoreHouse::load(IReader &file_stream)
{
	R_ASSERT2					(file_stream.find_chunk(STORE_CHUNK_DATA),"Can't find chunk STORE_CHUNK_DATA!");
	luabind::functor<void>		func;
	
	if (pSettings->section_exist("lost_alpha_cfg") && pSettings->line_exist("lost_alpha_cfg","on_load_store_callback"))
	{
		string256					func_name;
		xr_strcpy(func_name,pSettings->r_string("lost_alpha_cfg","on_load_store_callback"));
		R_ASSERT					(ai().script_engine().functor<void>(func_name,func));	
	}
	Msg("* Loading Store...");
	u16 count = file_stream.r_u16();
	for (u16 i=0;i<count;i++) 
	{
		StoreData d;
		shared_str name;
		file_stream.r_stringZ(name);
		d.type = (TypeOfData) file_stream.r_u8();
		LPCSTR stype = get_data_type(d.type);
		switch (d.type)
		{
			case lua_string:
			case lua_table:
			{
				shared_str s_data;
				file_stream.r_stringZ(s_data);
				
				u32 size = sizeof(char)*(s_data.size()+1);
				void* ptr = xr_malloc(size);
				xr_memcpy(ptr,s_data.c_str(),size);
				d.data = ptr;
				break;
			}
			default:
			{
				u32 size = type_to_size(d);
				void* ptr = xr_malloc(size);
				file_stream.r(ptr, size);
				d.data = ptr;
				break;
			}
		}
		data[*name]=d;

		detail::CallHelper(func, *name, d, get_data_type(d.type));

		//script callback
		
	//	func(*name, , detail::CastHelper::CastTo(d.type, d.data));
		
	}
	Msg("* %d store values successfully loaded", count);

}

xrTime CStoreHouse::get_time(LPCSTR name)
{
	u64 val;
	get(name,&val,sizeof(u64));
	return xrTime(val);
}

void CStoreHouse::add_time(LPCSTR name, xrTime *t)
{
	u64 val = t->time();
	add(name,&val,sizeof(u64),lua_ctime);
}

*/

void CStoreHouse::save(IWriter &memory_stream)
{
//	u16 size = 0;
	Msg("* Writing Store...");
//	m_buffer->write_start		();
//	OnSave						();
//	m_buffer->read_start		();
//	size = m_buffer->r_u16		();
	memory_stream.open_chunk	(STORE_CHUNK_DATA);
//	memory_stream.w				(m_buffer->B.data, m_buffer->B.count);

	string256					fn;
	luabind::functor<LPCSTR>	callback;
	xr_strcpy					(fn, pSettings->r_string("lost_alpha_cfg", "on_save_store_callback"));
	R_ASSERT					(ai().script_engine().functor<LPCSTR>(fn, callback));

	LPCSTR						str = callback();
	
	memory_stream.w_stringZ		(str);

	memory_stream.close_chunk	();
	
//	Msg("* %d store values successfully saved", size);
}
/*

#define LOAD_HELPER_NUMBER(type, T)					\
	case type:	{								\
		T val##T;								\
		file_stream.r(&val##T, sizeof(T));		\
		lua_pushstring(m_lua, *str);				\
		lua_pushnumber(m_lua, val##T);				\
		lua_settable(m_lua, -3 * (i + 1));		\
		break;									\
}
*/

void CStoreHouse::load(IReader &file_stream)
{
	R_ASSERT2					(file_stream.find_chunk(STORE_CHUNK_DATA),"Can't find chunk STORE_CHUNK_DATA!");
	Msg							("* Loading Store...");

	string256					fn;
	luabind::functor<void>		callback;

	xr_strcpy					(fn, pSettings->r_string("lost_alpha_cfg", "on_load_store_callback"));
	R_ASSERT					(ai().script_engine().functor<void>(fn, callback));

	xr_string					str;
	
	file_stream.r_stringZ		(str);

	callback					(str.c_str());
	
	

//	u16 count = OnLoad			(file_stream);
//	Msg							("* %d store values successfully loaded", count);

}
/*
u16 CStoreHouse::OnLoad(IReader &file_stream)
{
	u16						total = 0;
	string256				fn;
	luabind::functor<void>	callback;
	m_lua					      = ai().script_engine().lua();
	xr_strcpy											(fn, pSettings->r_string("lost_alpha_cfg", "on_load_store_callback"));
	R_ASSERT											(ai().script_engine().functor<void>(fn, callback));

	luabind::object&		table = UnserializeTable	(file_stream, total);
	
	callback											(table);

	return total;
	
}

luabind::object& CStoreHouse::UnserializeTable(IReader &file_stream, u16& total)
{
	u16 count = file_stream.r_u16			();
	lua_newtable							(m_lua);
	for (u16 i = 0, l = count; i < l; ++i)
	{
		shared_str				key		= "";
		u8						marker	= 0;
		file_stream.r_stringZ									(key);
		marker							= file_stream.r_u8		();
		switch (marker)
		{	
			case lua_bool: 
			{
				bool b = file_stream.r_bool();
				lua_pushstring(m_lua, *str);				
				lua_pushboolean(m_lua, b);
				lua_settable(m_lua, -3 * (i + 1));
				break;
			}
			LOAD_HELPER_NUMBER(lua_number, double);
	//		LOAD_HELPER(lua_vector, Fvector3);
			
			LOAD_HELPER_NUMBER(lua_u32, u32);
			LOAD_HELPER_NUMBER(lua_s32, s32);
			LOAD_HELPER_NUMBER(lua_u16, u16);
			LOAD_HELPER_NUMBER(lua_s16, s16);
			LOAD_HELPER_NUMBER(lua_u8, u8);
			LOAD_HELPER_NUMBER(lua_s8, s8);
			LOAD_HELPER_NUMBER(lua_float, float);
			//case lua_ctime: u64 val; file_stream.r(&val, sizeof(u64)); tbl[*key] = xrTime(val); break;
			//case lua_table: UnserializeTable(file_stream, total); break;
			//case lua_string: shared_str s; file_stream.r_stringZ(s); tbl[*key] = *s; break;
			case lua_vector: 
			{
				Fvector3 v = file_stream.r_vec3(v);
				lua_pushstring(m_lua, *str);	
				Fvector3* obj = (Fvector3*) lua_newuserdata(m_lua, sizeof(Fvector3));
				obj->add(v);
				lua_settable(m_lua, -3 * (i + 1));
				break;
			}
			case lua_ctime: 
			{
				u64 val = file_stream.r_u64();
				lua_pushstring(m_lua, *str);	
				xrTime* obj = (xrTime*) lua_newuserdata(m_lua, sizeof(xrTime));
				obj->add(xrTime(val));
				lua_settable(m_lua, -3 * (i + 1));
				break;
			}
			case lua_bool: 
			{
				bool b = file_stream.r_bool();
				lua_pushstring(m_lua, *str);				
				lua_pushboolean(m_lua, b);
				lua_settable(m_lua, -3 * (i + 1));
				break;
			}
			case lua_bool: 
			{
				bool b = file_stream.r_bool();
				lua_pushstring(m_lua, *str);				
				lua_pushboolean(m_lua, b);
				lua_settable(m_lua, -3 * (i + 1));
				break;
			}
		}
	}
	total		+= count;
	return tbl;
}

static long prec = 1e16;

void CStoreHouse::OnSave()
{
	R_ASSERT(ai().script_engine().ready());
//	luabind::object&			table = luabind::newtable			(ai().script_engine().lua());

	luabind::functor<luabind::object> func;
	string256					func_name;
	xr_strcpy														(func_name,pSettings->r_string("lost_alpha_cfg","on_save_store_callback"));
	R_ASSERT														(ai().script_engine().functor<luabind::object>(func_name,func));

	luabind::object table = func									();	
	
	SerializeTable													(table);

}

void CStoreHouse::SerializeTable(luabind::object& tbl)
{
	R_ASSERT															(tbl.type() == LUA_TTABLE);
	u16		pos		= m_buffer->w_tell									();
	m_buffer->w_u16														(u16(0));
	u16		cnt		= 0;
	for (luabind::object::iterator it = tbl.begin(), end = tbl.end(); it != end; ++it, ++cnt)
	{
		shared_str				k		= "";
		try
		{
			k = luabind::object_cast<LPCSTR>	(it.key());
		}
		catch (...)
		{
			try
			{
				k.sprintf("%d", luabind::object_cast<int>	(it.key()));
			}
			catch (...)
			{
				FATAL("wtf");
			}
		}
		LPCSTR					str		= *k;
		luabind::object			val		= *it;
		switch (val.type())
		{
			case LUA_TNUMBER:
			{
				double			number	= luabind::object_cast<double>(val);
				int				i		= (int) number;
				int				d		= ((int) (i * prec) % prec);
				if (!d && i <= type_max(u32) && i >= type_min(s32))
				{
					if (i >= 0)
					{
						if (i <= type_max(u8))
							SStorageHelper::Write<u8>(m_buffer, str, i);
						else if (i <= type_max(u16))
							SStorageHelper::Write<u16>(m_buffer, str, i);
						else
							SStorageHelper::Write<u32>(m_buffer, str, i);
					}
					else
					{
						if (i >= type_min(s8) && i <= type_max(s8))
							SStorageHelper::Write<s8>(m_buffer, str, i);
						else if (i >= type_min(s16) && i <= type_max(s16))
							SStorageHelper::Write<s16>(m_buffer, str, i);
						else
							SStorageHelper::Write<s32>(m_buffer, str, i);
					}
				}
				SStorageHelper::Write<float>(m_buffer, str, number); // engine is based on float values
				break;
			}
			case LUA_TSTRING:
			{
				LPCSTR			value	= luabind::object_cast<LPCSTR>	(val);
				SStorageHelper::Write<LPCSTR>							(m_buffer, str, value);
				break;
			}
			case LUA_TBOOLEAN:
			{
				bool			value   = luabind::object_cast<bool>	(val);
				SStorageHelper::Write<bool>								(m_buffer, str, value);
				break;
			}
			case LUA_TNIL:
			{
				break;
			}
			case LUA_TUSERDATA:
			{
				// only xrTime/Fvector3 supported so far
				try
				{
					xrTime			value   = luabind::object_cast<xrTime>	(val);
					SStorageHelper::Write<xrTime>							(m_buffer, str, value);
				}
				catch (...)
				{
					try
					{
						Fvector3	value	= luabind::object_cast<Fvector3>(val);
						SStorageHelper::Write<Fvector3>							(m_buffer, str, value);
					}
					catch (...)
					{
						R_ASSERT2(0, make_string("unsupported userdata '%s'", str));
					}
				}
				break;
			}
			case LUA_TTABLE:
			{
				m_buffer->w_stringZ										(str); 
				m_buffer->w_u8											(lua_table); 
				SerializeTable											(val);
				break;
			}
		}
    
	}
	m_buffer->w_seek													(pos, &cnt, sizeof(u16));
}


*/

