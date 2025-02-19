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

using namespace luabind;

void LuaLog(LPCSTR caMessage)
{
#ifndef MASTER_GOLD
	g_pScriptEngine->script_log	(ScriptStorage::eLuaMessageTypeMessage,"%s",caMessage);
#endif // #ifndef MASTER_GOLD
}

void ErrorLog(LPCSTR caMessage)
{
	g_pScriptEngine->error_log("%s",caMessage);
#ifdef PRINT_CALL_STACK
	g_pScriptEngine->print_stack();
#endif // #ifdef PRINT_CALL_STACK
	
	R_ASSERT2(0, caMessage);
}

void FlushLogs()
{
#ifdef DEBUG
	xrLogger::FlushLog();
	g_pScriptEngine->flush_log();
#endif // DEBUG
}

void verify_if_thread_is_running()
{
	VERIFY2(g_pScriptEngine->current_thread(), "coroutine.yield() is called outside the LUA thread!");
}

bool is_editor()
{
#ifdef XRGAME_EXPORTS
	return		(false);
#else
	return		(true);
#endif
}

#ifdef XRGAME_EXPORTS
CRenderDevice *get_device()
{
	return		(&Device);
}
#endif

LPCSTR user_name()
{
	return (Core.UserName);
}

void prefetch_module(LPCSTR file_name)
{
	g_pScriptEngine->process_file(file_name);
}

struct profile_timer_script {
	CTimer						measure;
	u64							m_accumulator;
	u64							m_count;
	int							m_recurse_mark;
	
	IC								profile_timer_script	()
	{
		m_accumulator			= 0;
		m_count					= 0;
		m_recurse_mark			= 0;
	}

	IC								profile_timer_script	(const profile_timer_script &profile_timer)
	{
		*this					= profile_timer;
	}

	IC		profile_timer_script&	operator=				(const profile_timer_script &profile_timer)
	{
		measure					= profile_timer.measure;
		m_accumulator			= profile_timer.m_accumulator;
		m_count					= profile_timer.m_count;
		m_recurse_mark			= profile_timer.m_recurse_mark;
		return					(*this);
	}

	IC		bool					operator<				(const profile_timer_script &profile_timer) const
	{
		return					(m_accumulator < profile_timer.m_accumulator);
	}

	IC		void					start					()
	{
		if (m_recurse_mark) {
			++m_recurse_mark;
			return;
		}

		++m_recurse_mark;
		++m_count;
		measure.Start();
	}

	IC		void					stop					()
	{
		if (!m_recurse_mark)
			return;

		--m_recurse_mark;
		
		if (m_recurse_mark)
			return;
		
		m_accumulator += measure.GetElapsed_mcs();
	}

	IC		float					time					() const
	{
		float result = float(double(m_accumulator));
		return (result);
	}
};

IC	profile_timer_script	operator+	(const profile_timer_script &portion0, const profile_timer_script &portion1)
{
	profile_timer_script	result;
	result.m_accumulator	= portion0.m_accumulator + portion1.m_accumulator;
	result.m_count			= portion0.m_count + portion1.m_count;
	return					(result);
}

std::ostream& operator<<(std::ostream& os, const profile_timer_script& pt) { return os << pt.time(); }

void MyLog(const char* A)
{
	Log(A);
}

#ifdef XRGAME_EXPORTS
ICF	u32	script_time_global	()	{ return Device.dwTimeGlobal; }
ICF	u32	script_time_global_async	()	{ return Device.TimerAsync_MMT(); }
#else
ICF	u32	script_time_global	()	{ return 0; }
ICF	u32	script_time_global_async	()	{ return 0; }
#endif

bool CheckMP()
{
#ifdef XR_MP_BUILD
	return true;
#else
	return false;
#endif
}

bool IsEditorMode()
{
	if (DevicePtr == nullptr)
		return false;

	return Device.IsEditorMode();
}

void SemiLog(const char* message)
{
	Msg(message);
}

namespace ixray::save
{
	xr_string CurrentSaveStage = "";
	void SaveError()
	{
		R_ASSERT2(!"Save file is big. Chunk: ", CurrentSaveStage.c_str());
	}

	void SaveStage(const char* Name)
	{
		CurrentSaveStage = Name;
	}
}

#pragma optimize("s",on)
void CScriptEngine::script_register(lua_State *L)
{
	module(L)
	[
		class_<profile_timer_script>("profile_timer")
			.def(constructor<>())
			.def(constructor<profile_timer_script&>())
			.def(const_self + profile_timer_script())
			.def(const_self < profile_timer_script())
			.def(tostring(self))
			.def("start",&profile_timer_script::start)
			.def("stop",&profile_timer_script::stop)
			.def("time",&profile_timer_script::time),

		def("error_log",						&ErrorLog),
		def("flush",							&FlushLogs),
		def("prefetch",							&prefetch_module),
		def("verify_if_thread_is_running",		&verify_if_thread_is_running),
		def("editor",							&is_editor),
		def("user_name",						&user_name),
		def("time_global",						&script_time_global),
		def("SemiLog",							&SemiLog),
		def("time_global_async",				&script_time_global_async),
		def("IsSupportMP",						&CheckMP),
		def("IsEditor",							&IsEditorMode)

#ifdef XRGAME_EXPORTS
		,def("device",							&get_device),
		def("TinyLog",							&MyLog)
#endif // #ifdef XRGAME_EXPORTS
	];

	if (DevicePtr != nullptr && Device.IsEditorMode())
	{
		module(L)
		[
			def("log", &LuaLog)
		];
	}
	
	module(L, "save")
	[
		def("call_error", &ixray::save::SaveError),
		def("set_stage", &ixray::save::SaveStage)
	];
}
