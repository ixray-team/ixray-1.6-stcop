////////////////////////////////////////////////////////////////////////////
//	Module 		: script_engine_script.cpp
//	Created 	: 25.12.2002
//  Modified 	: 13.05.2004
//	Author		: Dmitriy Iassenev
//	Description : ALife Simulator script engine export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "script_engine.h"
#include "lua_ext.h"

using namespace luabind;

XREFLECT(LuaExport, Name = "log", Condition = "DevicePtr != nullptr && Device.IsEditorMode()")
void LuaLog(const char* caMessage)
{
#ifndef MASTER_GOLD
	g_pScriptEngine->script_log(ScriptStorage::eLuaMessageTypeMessage, "%s", caMessage);
#endif // #ifndef MASTER_GOLD
}

XREFLECT(LuaExport, Name = "error_log")
void ErrorLog(const char* caMessage)
{
	g_pScriptEngine->error_log("%s", caMessage);
	g_pScriptEngine->print_stack();

	R_ASSERT2(0, caMessage);
}

XREFLECT(LuaExport, Name = "flush")
void FlushLogs()
{
#ifdef DEBUG
	xrLogger::FlushLog();
	g_pScriptEngine->flush_log();
#endif // DEBUG
}

XREFLECT(LuaExport)
void verify_if_thread_is_running()
{
	VERIFY2(g_pScriptEngine->current_thread(), "coroutine.yield() is called outside the LUA thread!");
}

XREFLECT(LuaExport, Name = "editor")
bool is_editor()
{
#ifdef XRGAME_EXPORTS
	return (false);
#else
	return (true);
#endif
}

#ifdef XRGAME_EXPORTS
XREFLECT(LuaExport, Name = "device")
CRenderDevice* get_device()
{
	return (&Device);
}

XREFLECT(LuaExport)
void trigger_assert(const char* pStringFromLua)
{
	R_ASSERT(false && "catch the thing!");
}

XREFLECT(LuaExport)
void trigger_vs_log(const char* pStringFromLua)
{
#ifdef IXR_WINDOWS
	OutputDebugStringA(pStringFromLua);
#endif
}

XREFLECT(LuaExport, Name = "load_script_to_global")
void LoadScriptToGlobal_script(const char* script_name)
{
	LoadScriptToGlobal(g_pScriptEngine->lua(), script_name, false);
}
#endif

XREFLECT(LuaExport)
const char* user_name()
{
	return (Core.UserName);
}

XREFLECT(LuaExport, Name = "prefetch")
void prefetch_module(const char* file_name)
{
	g_pScriptEngine->process_file(file_name);
}

XREFLECT(LuaExport, Name = "profile_timer", LuaToString)
struct profile_timer_script
{
	CTimer measure;
	u64 m_accumulator;
	u64 m_count;
	int m_recurse_mark;

	XPROPERTY(LuaConstructor)
	IC profile_timer_script()
	{
		m_accumulator = 0;
		m_count = 0;
		m_recurse_mark = 0;
	}

	XPROPERTY(LuaConstructor)
	IC profile_timer_script(const profile_timer_script& profile_timer)
	{
		*this = profile_timer;
	}

	IC profile_timer_script& operator=(const profile_timer_script& profile_timer)
	{
		measure = profile_timer.measure;
		m_accumulator = profile_timer.m_accumulator;
		m_count = profile_timer.m_count;
		m_recurse_mark = profile_timer.m_recurse_mark;
		return (*this);
	}

	XPROPERTY(LuaOperator)
	IC bool operator<(const profile_timer_script& profile_timer) const
	{
		return (m_accumulator < profile_timer.m_accumulator);
	}

	XPROPERTY(LuaCallable)
	IC void start()
	{
		if (m_recurse_mark)
		{
			++m_recurse_mark;
			return;
		}

		++m_recurse_mark;
		++m_count;
		measure.Start();
	}

	XPROPERTY(LuaCallable)
	IC void stop()
	{
		if (!m_recurse_mark)
		{
			return;
		}

		--m_recurse_mark;

		if (m_recurse_mark)
		{
			return;
		}

		m_accumulator += measure.GetElapsed_mcs();
	}

	XPROPERTY(LuaCallable)
	IC float time() const
	{
		float result = float(double(m_accumulator));
		return (result);
	}

	XPROPERTY(LuaOperator)
	IC profile_timer_script operator+(const profile_timer_script& portion) const
	{
		profile_timer_script result;
		result.m_accumulator = m_accumulator + portion.m_accumulator;
		result.m_count = m_count + portion.m_count;
		return (result);
	}
};

std::ostream& operator<<(std::ostream& os, const profile_timer_script& pt)
{
	return os << pt.time();
}

XREFLECT(LuaExport, Name = "TinyLog")
void MyLog(const char* A)
{
	Log(A);
}

// Log is overloaded, Lua gets the const char* one.
XREFLECT(LuaExport, Name = "log1")
void LuaLog1(const char* Message)
{
	Log(Message);
}

#ifdef XRGAME_EXPORTS
XREFLECT(LuaExport, Name = "time_global")
ICF u32 script_time_global()
{
	return Device.dwTimeGlobal;
}
XREFLECT(LuaExport, Name = "time_global_async")
ICF u32 script_time_global_async()
{
	return Device.TimerAsync_MMT();
}
#else
XREFLECT(LuaExport, Name = "time_global")
ICF u32 script_time_global()
{
	return 0;
}
XREFLECT(LuaExport, Name = "time_global_async")
ICF u32 script_time_global_async()
{
	return 0;
}
#endif

XREFLECT(LuaExport, Name = "IsSupportMP")
bool CheckMP()
{
#ifdef XR_MP_BUILD
	return true;
#else
	return false;
#endif
}

XREFLECT(LuaExport, Name = "IsEditor")
bool IsEditorMode()
{
	if (DevicePtr == nullptr)
	{
		return false;
	}

	return Device.IsEditorMode();
}

XREFLECT(LuaExport)
void SemiLog(const char* message)
{
	Msg(message);
}

namespace ixray::save
{
xr_string CurrentSaveStage = "";
XREFLECT(LuaExport, Name = "save.call_error")
void SaveError()
{
	R_ASSERT2(!"Save file is big. Chunk: ", CurrentSaveStage.c_str());
}

XREFLECT(LuaExport, Name = "save.set_stage")
void SaveStage(const char* Name)
{
	CurrentSaveStage = Name;
}
} // namespace ixray::save

XREFLECT(LuaExport, Name = "try_load_file")
void TryLoadFile(const char* FileName)
{
	FS.TryLoad(FileName);
}

XREFLECT(LuaExport, Name = "debug_print")
void lua_debug_print(const char* str)
{
	if (!xr_strlen(str))
	{
		return;
	}

	Msg("$ DBG:[%lu] %s", Device.dwTimeGlobal, str);
}

XREFLECT(LuaExport, Name = "help")
void DumpLuaExports()
{
	lua_State* L = g_pScriptEngine->lua();
	using namespace luabind::detail;

	class_registry* Registry = class_registry::get_registry(L);
	auto GetArgTypeName = [](const auto info) -> xr_string
	{
		if (!info)
		{
			return "unknown";
		}

		xr_string Result = (*info).name();

		const xr_vector<xr_string> Need2Remove = {"class ", "struct ", " __ptr64"};
		for (const auto& Str : Need2Remove)
		{
			size_t Pos = 0;
			while ((Pos = Result.find(Str, Pos)) != xr_string::npos)
			{
				Result.erase(Pos, Str.length());
			}
		}

		return Result;
	};

	Registry->iterate_classes(L, [&](lua_State* st, class_rep* crep)
							  {
		if (!crep->bases().empty())
		{
			xr_string OutStr = "C++ class ";
			OutStr += crep->name();
			OutStr += " : ";

			bool IsFirstStep = true;
			for (const class_rep::base_info& BaseClassInfo : crep->bases())
			{
				if (!IsFirstStep)
				{
					OutStr += ", ";
				}
				
				OutStr += BaseClassInfo.base->name();
				IsFirstStep = false;
			}

			Msg("C++ class %s", OutStr.c_str());
		}
		else
		{
			Msg("C++ class %s", crep->name());
		}

		Msg("{");

		for (const auto&[Name, _] : crep->m_getters)
		{
			Msg("\tproperty %s;", Name);
		}

		for (const auto& [Name, Value] : crep->static_constants())
		{
			Msg("\tconst %s = %d;", Name, Value);
		}

		for (const auto& Method : crep->m_methods)
		{
			for (const auto& Overload : Method.overloads())
			{
				int ArgsCount = Overload.m_arity;
				xr_string args;
				bool IsPrinted = false;
				for (int i = 0; i < ArgsCount; ++i)
				{
					if (Overload.m_params_.size() > i)
					{
						if (IsPrinted)
						{
							args += ", ";
						}

						args += GetArgTypeName(Overload.m_params_[i]);
						IsPrinted = true;
					}
				}
				Msg("\tfunction %s(%s);", Method.name, args.c_str());
			}
		}

		Msg("};\n"); });
}

XREFLECT(LuaExport)
bool IsTracyConnected()
{
#ifdef IXRAY_PROFILER_TRACY
	return tracy::GetProfiler().IsConnected();
#endif
	return false;
}

void XReflectRegister(lua_State* L);

void CScriptEngine::script_register(lua_State* L)
{
	XReflectRegister(L);
}

#include "script_engine_script.reflect.inl"
