#include "stdafx.h"
#include "pch_script.h"
#include "SaveSystem_script.h"
#include "../xrCore/Save/SaveChunk.h"
#include "../xrCore/Save/SaveVariables.h"
#include "../xrCore/Save/SaveObject.h"
#include "../../xrGame/xr_time.h"
#include "lua_ext.h"
#include "lua.h"
#ifndef IXRAY_NO_LUA
#include <luabind/luabind.hpp>
#endif

#include "script_engine.h"

namespace CSaveChunk_script {
	bool r_bool(CSaveChunk* Chunk) {
		bool Value;
		Chunk->r_bool(Value);
		return Value;
	}

	float r_float(CSaveChunk* Chunk) {
		float Value;
		Chunk->r_float(Value);
		return Value;
	}

	u64 r_u64(CSaveChunk* Chunk) {
		u64 Value;
		Chunk->r_u64(Value);
		return Value;
	}

	s64 r_s64(CSaveChunk* Chunk) {
		s64 Value;
		Chunk->r_s64(Value);
		return Value;
	}

	u32 r_u32(CSaveChunk* Chunk) {
		u32 Value;
		Chunk->r_u32(Value);
		return Value;
	}

	s32 r_s32(CSaveChunk* Chunk) {
		s32 Value;
		Chunk->r_s32(Value);
		return Value;
	}

	u16 r_u16(CSaveChunk* Chunk) {
		u16 Value;
		Chunk->r_u16(Value);
		return Value;
	}

	s16 r_s16(CSaveChunk* Chunk) {
		s16 Value;
		Chunk->r_s16(Value);
		return Value;
	}

	u8 r_u8(CSaveChunk* Chunk) {
		u8 Value;
		Chunk->r_u8(Value);
		return Value;
	}

	s8 r_s8(CSaveChunk* Chunk) {
		s8 Value;
		Chunk->r_s8(Value);
		return Value;
	}

	u64 ReadArray(CSaveChunk* Chunk) {
		u64 Size;
		Chunk->ReadArray(Size);
		return Size;
	}

	CSaveChunk* BeginChunk(CSaveChunk* Chunk, str_c ChunkName) {
		return Chunk->BeginChunk(ChunkName);
	}

	CSaveChunk* FindChunk(CSaveChunk* Chunk, str_c ChunkName) {
		return Chunk->FindChunk(ChunkName);
	}

}

namespace CSaveObject_script
{
	
	bool HasChunk(ISaveObject* Obj, str_c Name){
		VERIFY(Obj);
		return Obj->HasChunk(Name);
	}

#ifndef MASTER_GOLD
#define SaveLog(ToCall) \
	if (EngineExternal()[EEngineExternalSystem::AdvancedSerializationDebugLog]){ \
		ToCall(); \
	}
#else
#define SaveLog(ToCall)
#endif

	void ForChunk(ISaveObject* Obj, str_c Name, const luabind::object& func)
	{
		SaveLog([&](){Msg("Enter ForChunk [%s]", Name);})
		I_ASSERT(Obj);
		I_ASSERT(func.type() == LUA_TFUNCTION);
		BEGIN_CHUNK(*Obj, Name)
		{
			luabind::call_function<void>(func);
		}
		SaveLog([&](){Msg("Exit ForChunk [%s]", Name);})
	}

	void ForArray(ISaveObject* Obj, luabind::object func)
	{
		SaveLog([&](){Msg("Enter ForArray");})
		VERIFY(Obj);
		VERIFY(func.type() == LUA_TFUNCTION);
		BEGIN_ARRAY(*Obj)
		{
			luabind::call_function<void>(func);
		}
		SaveLog([&](){Msg("Exit ForArray");})
	}
	
	Fvector s_vec3(ISaveObject* Obj, Fvector Value) {
		SaveLog([&](){Msg("Serialize vec3");})
		VERIFY(Obj);
		*Obj << Value;
		return Value;
	}

#define VALUE_VALIDATION(type, var) \
	{ \
		type Min = std::numeric_limits<type>::min();\
		type Max = std::numeric_limits<type>::max();\
		I_ASSERT_M(Min <= var, "Attempt to save " #type " [%s] with value lower than minimum [%s]!", std::to_string(var).c_str(), std::to_string(Min).c_str()); \
		I_ASSERT_M(var <= Max, "Attempt to save " #type " [%s] with value bigger than maximum [%s]!", std::to_string(var).c_str(), std::to_string(Max).c_str()); \
	}
	
	float s_float(ISaveObject* Obj, double Value) {
		SaveLog([&](){Msg("Serialize float");})
		VERIFY(Obj);
		float Casted;
		if (Obj->IsSave()) {
			{
				static float Min = -std::numeric_limits<float>::max();
				static float Max = std::numeric_limits<float>::max();
				I_ASSERT_M(Min <= Value, "Attempt to save " "float" " [%s] with value lower than minimum [%s]!", std::to_string(Value).c_str(), std::to_string(Min).c_str());
				I_ASSERT_M(Value <= Max, "Attempt to save " "float" " [%s] with value bigger than maximum [%s]!", std::to_string(Value).c_str(), std::to_string(Max).c_str());
			};
			Casted = (float)Value;
		}
		*Obj << Casted;
		return Casted;
	}
	
	u64 s_u64(ISaveObject* Obj, u64 Value) {
		SaveLog([&](){Msg("Serialize u64");})
		VERIFY(Obj);
		*Obj << Value;
		return Value;
	}
	
	s64 s_s64(ISaveObject* Obj, s64 Value) {
		SaveLog([&](){Msg("Serialize s64");})
		VERIFY(Obj);
		*Obj << Value;
		return Value;
	}
	
	u32 s_u32(ISaveObject* Obj, u64 Value) {
		SaveLog([&](){Msg("Serialize u32");})
		VERIFY(Obj);
		u32 Casted;
		if (Obj->IsSave())
		{
			if (Value == std::numeric_limits<u64>::max())
			{
				Value = std::numeric_limits<u32>::max();
			}
			VALUE_VALIDATION(u32, Value);
			//I_ASSERT_M(std::numeric_limits<u32>::min() >= Value, "Attempt to save u32 [%ul] with value lower than minimum [%ul]!", Value, std::numeric_limits<u32>::min());
			//I_ASSERT_M(Value <= std::numeric_limits<u32>::max(), "Attempt to save u32 [%ul] with value bigger than maximum [%ul]!", Value, std::numeric_limits<u32>::max());
			Casted = (u32)Value;
		}
		*Obj << Casted;
		return Casted;
	}
	
	s32 s_s32(ISaveObject* Obj, s64 Value) {
		SaveLog([&](){Msg("Serialize s32");})
		VERIFY(Obj);
		s32 Casted;
		if (Obj->IsSave())
		{
			VALUE_VALIDATION(s32, Value);
			//I_ASSERT_M(std::numeric_limits<s32>::min() >= Value, "Attempt to save s32 [%ul] with value lower than minimum [%ul]!", Value, std::numeric_limits<s32>::min());
			//I_ASSERT_M(Value <= std::numeric_limits<s32>::max(), "Attempt to save s32 [%ul] with value bigger than maximum [%ul]!", Value, std::numeric_limits<s32>::max());
			Casted = (s32)Value;
		}
		*Obj << Casted;
		return Casted;
	}
	
	u16 s_u16(ISaveObject* Obj, u64 Value) {
		SaveLog([&](){Msg("Serialize u16");})
		VERIFY(Obj);
		u16 Casted;
		if (Obj->IsSave())
		{
			if (Value == std::numeric_limits<u64>::max())
			{
				Value = std::numeric_limits<u16>::max();
			}
			VALUE_VALIDATION(u16, Value);
			Casted = (u16)Value;
		}
		*Obj << Casted;
		return Casted;
	}
	
	s16 s_s16(ISaveObject* Obj, s64 Value) {
		SaveLog([&](){Msg("Serialize s16");})
		VERIFY(Obj);
		s16 Casted;
		if (Obj->IsSave())
		{
			VALUE_VALIDATION(s16, Value);
			Casted = (s16)Value;
		}
		*Obj << Casted;
		return Casted;
	}
	
	u8 s_u8(ISaveObject* Obj, u64 Value) {
		SaveLog([&](){Msg("Serialize u8");})
		VERIFY(Obj);
		u8 Casted;
		if (Obj->IsSave())
		{
			if (Value == std::numeric_limits<u64>::max())
			{
				Value = std::numeric_limits<u8>::max();
			}
			VALUE_VALIDATION(u8, Value);
			Casted = (u8)Value;
		}
		*Obj << Casted;
		return Casted;
	}
	
	s8 s_s8(ISaveObject* Obj, s64 Value) {
		SaveLog([&](){Msg("Serialize s8");})
		VERIFY(Obj);
		s8 Casted;
		if (Obj->IsSave())
		{
			VALUE_VALIDATION(s8, Value);
			Casted = (s8)Value;
		}
		*Obj << Casted;
		return Casted;
	}
	
	bool s_bool(ISaveObject* Obj, bool Value) {
		SaveLog([&](){Msg("Serialize bool");})
		VERIFY(Obj);
		*Obj << Value;
		return Value;
	}

#undef VALUE_VALIDATION
	
	str_c s_stringZ(ISaveObject* Obj, str_c Value) {
		SaveLog([&](){Msg("Serialize string");})
		if (g_pScriptEngine)
		{
			lua_State* L = g_pScriptEngine->lua();
			VERIFY(Obj);
			if (Value) // not null
			{
				auto size = xr_strlen(Value) + 1; // with 0 at end
				if (size > shared_str_limit)
				{
					// Ты блять "Войну и мир" решил сохранить нахуй!?
					auto& StrData = *Obj->SerializeEnourmousString(Value);
					if (!Obj->IsSave())
					{
						lua_pushlstring(L, StrData.c_str(), size);
						size_t lua_len;
						str_c lua_str = lua_tolstring(L, -1, &lua_len);
						//lua_pushvalue(L, -1);
						//int ref = luaL_ref(L, LUA_REGISTRYINDEX);
						//lua_pop(L, 1);
						return lua_str;
					}
					return Value;
				}
			}
		}
		shared_str Casted;
		if (Obj->IsSave()) {
			if (Value)
			{
				R_ASSERT(xr_strlen(Value) + 1 <= shared_str_limit);
			}
			Casted = Value;
		}
		*Obj << Casted;
		return Casted.c_str();
	}
	
#undef SaveLog
	
	bool IsSave(ISaveObject* Obj){
		VERIFY(Obj);
		return Obj->IsSave();
	}

}

#ifndef IXRAY_NO_LUA
using namespace luabind;

void SaveSystemScript::script_register(lua_State* L)
{
	module(L)
		[
			class_<ISaveObjectStackHandler>("SaveObjectStackHandler"),
			class_<ISaveObject>("SaveObject")
				.def("HasChunk", &CSaveObject_script::HasChunk)
				.def("ForChunk", &CSaveObject_script::ForChunk)
				.def("ForArray", &CSaveObject_script::ForArray)
				.def("s_vec3", &CSaveObject_script::s_vec3)
				.def("s_float", &CSaveObject_script::s_float)
				.def("s_u64", &CSaveObject_script::s_u64)
				.def("s_s64", &CSaveObject_script::s_s64)
				.def("s_u32", &CSaveObject_script::s_u32)
				.def("s_s32", &CSaveObject_script::s_s32)
				.def("s_u16", &CSaveObject_script::s_u16)
				.def("s_s16", &CSaveObject_script::s_s16)
				.def("s_u8", &CSaveObject_script::s_u8)
				.def("s_s8", &CSaveObject_script::s_s8)
				.def("s_bool", &CSaveObject_script::s_bool)
				.def("s_stringZ", &CSaveObject_script::s_stringZ)
				.def("IsSave", &CSaveObject_script::IsSave)
		];
}
#endif