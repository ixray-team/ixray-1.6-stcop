#include "StdAfx.h"
#include "pch_script.h"
#include "console_registrator.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrScripts/script_engine.h"
#include "../xrScripts/script_process.h"
#include "ai_space.h"

using namespace luabind;

CConsole*	console()
{
	return Console;
}

int get_console_integer( CConsole* c, const char* cmd )
{
	int min = 0, max = 0;
	int val = c->GetInteger ( cmd, min, max );
	return val;
}

float get_console_float( CConsole* c, const char* cmd )
{
	float min = 0.0f, max = 0.0f;
	float val = c->GetFloat ( cmd, min, max );
	return val;
}

bool get_console_bool( CConsole* c, const char* cmd )
{
	return c->GetBool( cmd );
}

void execute_console_command_deferred	(CConsole* c, const char* string_to_execute)
{
	g_pEventManager->Event.Defer	("KERNEL:console", size_t(xr_strdup(string_to_execute)) );
}

class CCC_ScriptLuaCommand : public IConsole_Command {
public:
	xr_vector<shared_str> m_fill_tips;
	luabind::functor<void> functor;

	shared_str CommandName;

	CCC_ScriptLuaCommand(const char* N, luabind::functor<void>& funct, const char* m_tips_string) : IConsole_Command(N), CommandName(N)
	{
		cName = CommandName.c_str();
		bEmptyArgsHandled = true;
		Update(funct, m_tips_string);
	}

	void Update(const luabind::functor<void>& Handler, const char* Tips)
	{
		functor = Handler;
		m_fill_tips.clear();

		int cnt = _GetItemCount(Tips);
		m_fill_tips.reserve(cnt);

		for (int i = 0; i < cnt; ++i)
		{
			string128 tmp;
			m_fill_tips.push_back(_GetItem(Tips, i, tmp));
		}
	}

	~CCC_ScriptLuaCommand() override
	{
		if (Console)
		{
			Console->RemoveCommand(this);
		}
	}

	virtual void Execute(const char* args)
	{
		int cnt = _GetItemCount(args);
		luabind::object params_to_lua = luabind::newtable(ai().script_engine().lua());

		for (int i = 0; i < cnt; ++i)
		{
			string128 tmp;
			params_to_lua[i + 1] = _GetItem(args, i, tmp);
		}

		const auto Handler = functor;
		Handler(params_to_lua);
	}

	virtual void fill_tips(vecTips& tips, u32 mode)
	{
		tips = m_fill_tips;
		IConsole_Command::fill_tips(tips, mode);
	}
};

namespace
{
	xr_string_map<xr_string, xr_unique_ptr<CCC_ScriptLuaCommand>> LuaCommands;
}

void console_registrator::ClearLuaCommands()
{
	LuaCommands.clear();
}

void registerLuaCommand(CConsole* c, const char* command_name, luabind::functor<void> functor, const char* m_tips_string)
{
	if (!command_name || !*command_name)
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "! Error missing lua command name");
		return;
	}
	const auto Existing = c->Commands.find(command_name);
	const auto Owned = LuaCommands.find(xr_string_view(command_name));
	if (Existing != c->Commands.end() && (Owned == LuaCommands.end() || Existing->second != Owned->second.get()))
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "Cannot replace built-in console command '%s'", command_name);
		return;
	}

	const char* Tips = m_tips_string ? m_tips_string : "";
	if (Owned != LuaCommands.end())
	{
		Owned->second->Update(functor, Tips);
		c->AddCommand(Owned->second.get());
		return;
	}

	auto Command = xr_make_unique<CCC_ScriptLuaCommand>(command_name, functor, Tips);
	const auto Stored = LuaCommands.emplace(command_name, std::move(Command)).first;
	c->AddCommand(Stored->second.get());
}

#pragma optimize("s",on)
void console_registrator::script_register(lua_State *L)
{
	module(L)
	[
		def("get_console",					&console),

		class_<CConsole>("CConsole")
			.def("execute", &CConsole::Execute)
			.def("execute_script",			&CConsole::ExecuteScript)
			.def("show",					&CConsole::Show)
			.def("hide",					&CConsole::Hide)

			.def("register_lua_command",	&registerLuaCommand)

			.def("get_string",				&CConsole::GetString)
			.def("get_integer",				&get_console_integer)
			.def("get_bool",				&get_console_bool)
			.def("get_float",				&get_console_float)
			.def("get_token",				&CConsole::GetToken)
	];
}