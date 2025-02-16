#include "pch_script.h"
#include "console_registrator.h"
#include "../xr_ioconsole.h"

using namespace luabind;

CConsole*	console()
{
	return Console;
}

int get_console_integer(CConsole* c, LPCSTR cmd)
{
	int min=0, max=0;
	int val = c->GetInteger ( cmd, min, max);
	return val;
}

bool is_console_active(CConsole *c)
{
	return (c->bVisible != 0) ? true : false;
}

float get_console_float(CConsole* c, LPCSTR cmd)
{
	float min = 0.0f, max = 0.0f;
	float val = c->GetFloat ( cmd, min, max );
	return val;
}

bool get_console_bool(CConsole* c, LPCSTR cmd)
{
	return c->GetBool( cmd );
}

#pragma optimize("s",on)
void console_registrator::script_register(lua_State *L)
{
	module(L)
	[
		def("get_console",					&console),
		class_<CConsole>("CConsole")
		.def("execute",						&CConsole::Execute)
		.def("execute_script",				&CConsole::ExecuteScript)
		.def("show",						&CConsole::Show)
		.def("hide",						&CConsole::Hide)
//		.def("save",						&CConsole::Save)
		.def("get_string",					&CConsole::GetString)
		.def("get_integer",					&get_console_integer)
		.def("get_bool",					&get_console_bool)
		.def("get_float",					&get_console_float)
		.def("get_token",					&CConsole::GetToken)
		.def("is_active", &is_console_active)
//		.def("",				&CConsole::)

	];
}