////////////////////////////////////////////////////////////////////////////
//	Module 		: script_storage.cpp
//	Created 	: 01.04.2004
//  Modified 	: 01.04.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script Storage
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "script_storage.h"
#include "script_thread.h"
#include <stdarg.h>
#include "../xrCore/doug_lea_allocator.h"
#include <sstream>
#include "lua_ext.h"
#include "script_process.h"

LPCSTR	file_header = "\
local function script_name() \
return \"%s\" \
end \
local this = {} \
%s this %s \
setmetatable(this, {__index = _G}) \
setfenv(1, this) \
		";

#include "script_engine.h"

#ifndef XRGAME_EXPORTS
#	define NO_XRGAME_SCRIPT_ENGINE
#endif

CScriptStorage::CScriptStorage() : 
	m_jit(false)
{
	m_current_thread = nullptr;
	m_virtual_machine = nullptr;
}

CScriptStorage::~CScriptStorage()
{
	if (m_virtual_machine)
		lua_close(m_virtual_machine);
}

void CScriptStorage::reinit	()
{
	if (m_virtual_machine)
	{
		lua_close(m_virtual_machine);
	}

	m_virtual_machine		= luaL_newstate();

	if (!m_virtual_machine) {
		Msg					("! ERROR : Cannot initialize script virtual machine!");
		return;
	}

	// Lua 5.1 namespaces 
	luajit::open_lib(lua(),	"",					luaopen_base);
	luajit::open_lib(lua(),	LUA_LOADLIBNAME,	luaopen_package);
	luajit::open_lib(lua(),	LUA_TABLIBNAME,		luaopen_table);
	luajit::open_lib(lua(),	LUA_IOLIBNAME,		luaopen_io);
	luajit::open_lib(lua(),	LUA_OSLIBNAME,		luaopen_os);
	luajit::open_lib(lua(),	LUA_MATHLIBNAME,	luaopen_math);
	luajit::open_lib(lua(),	LUA_STRLIBNAME,		luaopen_string);
	luajit::open_lib(lua(), LUA_DBLIBNAME,		luaopen_debug);

	// LuaJIT
	luajit::open_lib(lua(), LUA_JITLIBNAME, luaopen_jit);
	luajit::open_lib(lua(), LUA_BITLIBNAME, luaopen_bit);
	luajit::open_lib(lua(), LUA_FFILIBNAME, luaopen_ffi);

	// Lua Plugins
#ifdef XRGAME_EXPORTS
	lua_init_ext(lua());
#endif
}

int CScriptStorage::vscript_log		(ScriptStorage::ELuaMessageType tLuaMessageType, LPCSTR caFormat, va_list marker)
{

	LPCSTR		S = "", SS = "";
	LPSTR		S1;
	string4096	S2;
	switch (tLuaMessageType) {
		case ScriptStorage::eLuaMessageTypeInfo : {
			S	= "* [LUA] ";
			SS	= "[INFO]        ";
			break;
		}
		case ScriptStorage::eLuaMessageTypeError : {
			S	= "! [LUA] ";
			SS	= "[ERROR]       ";
			break;
		}
		case ScriptStorage::eLuaMessageTypeMessage : {
			S	= "[LUA] ";
			SS	= "[MESSAGE]     ";
			break;
		}
		case ScriptStorage::eLuaMessageTypeHookCall : {
			S	= "[LUA][HOOK_CALL] ";
			SS	= "[CALL]        ";
			break;
		}
		case ScriptStorage::eLuaMessageTypeHookReturn : {
			S	= "[LUA][HOOK_RETURN] ";
			SS	= "[RETURN]      ";
			break;
		}
		case ScriptStorage::eLuaMessageTypeHookLine : {
			S	= "[LUA][HOOK_LINE] ";
			SS	= "[LINE]        ";
			break;
		}
		case ScriptStorage::eLuaMessageTypeHookCount : {
			S	= "[LUA][HOOK_COUNT] ";
			SS	= "[COUNT]       ";
			break;
		}
		case ScriptStorage::eLuaMessageTypeHookTailReturn : {
			S	= "[LUA][HOOK_TAIL_RETURN] ";
			SS	= "[TAIL_RETURN] ";
			break;
		}
		default : NODEFAULT;
	}
	
	xr_strcpy	(S2,S);
	S1		= S2 + xr_strlen(S);
	int		l_iResult = vsprintf(S1,caFormat,marker);
	Msg		("%s",S2);
	
	xr_strcpy	(S2,SS);
	S1		= S2 + xr_strlen(SS);
	vsprintf(S1,caFormat,marker);
	xr_strcat	(S2,"\r\n");

#ifdef DEBUG
#	ifndef ENGINE_BUILD
	g_pScriptEngine->m_output.w(S2,xr_strlen(S2)*sizeof(char));
#	endif // #ifdef ENGINE_BUILD
#endif // #ifdef DEBUG

	return	(l_iResult);
}

void LogLuaVariable(lua_State* L, const char* Name, int Level, bool bOpenTable, int Index /*= -1*/)
{
	int LuaType = lua_type(L, Index);
	if (LuaType == -1 || LuaType == LUA_TFUNCTION)
		return;

	const char* TypeName = lua_typename(L, LuaType);

	char TabBuffer[32];
	ZeroMemory(TabBuffer, sizeof(TabBuffer));
	std::memset(TabBuffer, '\t', Level);

	auto LuaLogTable = [](lua_State* l, const char* S, int level, int index /*= -1*/) {
		if (lua_istable(l, index)) {
			lua_pushnil(l);  /* first key */
			while (lua_next(l, index - 1) != 0) {
				char sname[256];
				char sFullName[256];
				sprintf(sname, "%s", lua_tostring(l, index - 1));
				sprintf(sFullName, "%s.%s", S, sname);
				LogLuaVariable(l, sFullName, level + 1, false, index);

				lua_pop(l, 1);  /* removes `value'; keeps `key' for next iteration */
			}
		}
	};

	switch (LuaType) {
		case LUA_TNUMBER: {
			Msg("%s %s %s : %.02f", TabBuffer, TypeName, Name, lua_tonumber(L, Index));
			break;
		}
		case LUA_TBOOLEAN: {
			Msg("%s %s %s : %.02f", TabBuffer, TypeName, Name, lua_toboolean(L, Index) == 1 ? TEXT("true") : TEXT("false"));
			break;
		}
		case LUA_TSTRING: {
			Msg("%s %s %s : %s", TabBuffer, TypeName, Name, lua_tostring(L, Index));
			break;
		}
		case LUA_TTABLE: {
			if (bOpenTable) {
				Msg("%s %s %s : Table", TabBuffer, TypeName, Name);

				LuaLogTable(L, Name, Level + 1, Index);
			} else {
				Msg("%s %s %s : Table [...]", TabBuffer, TypeName, Name);
			}
			break;
		}
		case LUA_TUSERDATA: {
			void* UserDataPtr = lua_touserdata(L, Index);
			const luabind::detail::object_rep* obj = static_cast<luabind::detail::object_rep*>(UserDataPtr);
			const luabind::detail::class_rep* objectClass = obj->crep();

			if (objectClass != nullptr) {
				std::stringstream ss;
				ss << UserDataPtr;
				Msg("%s Userdata: %s(%s:0x%s)", TabBuffer, Name, objectClass->name(), ss.str().c_str());
			} else {
				Msg("%s Userdata: %s", TabBuffer, Name);
			}

			return;
			break;
		}
		default: {
			Log("[not available]");
			break;
		}
	}
}

void CScriptStorage::print_stack() {
	lua_State* L = lua();

	if (lua_isstring(L, -1)) {
		Msg("[LUA] Error: %s\n", lua_tostring(L, -1));
		//lua_pop(L, 1);
	}

	if (!Core.ParamsData.test(ECoreParams::use_callstack))
		return;

	lua_Debug LuaDebugInfo;
	const char* LocalVarName;

	for (int i = 0; lua_getstack(L, i, &LuaDebugInfo); ++i) {
		lua_getinfo(L, "nSlu", &LuaDebugInfo);
		if (!LuaDebugInfo.name) {
			script_log(ScriptStorage::eLuaMessageTypeError, "%2d : [%s] %s(%d) : %s", i, LuaDebugInfo.what, LuaDebugInfo.short_src, LuaDebugInfo.currentline, "");
		} else if (!xr_strcmp(LuaDebugInfo.what, "C")) {
			script_log(ScriptStorage::eLuaMessageTypeError, "%2d : [C  ] %s", i, LuaDebugInfo.name);
		} else {
			script_log(ScriptStorage::eLuaMessageTypeError, "%2d : [%s] %s(%d) : %s", i, LuaDebugInfo.what, LuaDebugInfo.short_src, LuaDebugInfo.currentline, LuaDebugInfo.name);
		}

		bool bPringName = true;
		int Iter = 1;
		while ((LocalVarName = lua_getlocal(L, &LuaDebugInfo, Iter++)) != nullptr) {
			if (bPringName) {
				Log("Local variables:");
				bPringName = false;
			}
			LogLuaVariable(L, LocalVarName, 1, true, -1);

			lua_pop(L, 1);  /* remove variable value */
		}
	}
}

int __cdecl CScriptStorage::script_log	(ScriptStorage::ELuaMessageType tLuaMessageType, LPCSTR caFormat, ...)
{
	va_list			marker;
	va_start		(marker,caFormat);
	int				result = vscript_log(tLuaMessageType,caFormat,marker);
	va_end			(marker);

#	ifndef ENGINE_BUILD
	static bool	reenterability = false;
	if (!reenterability) {
		reenterability = true;
		if (eLuaMessageTypeError == tLuaMessageType)
			g_pScriptEngine->print_stack	();
		reenterability = false;
	}
#	endif // #ifndef ENGINE_BUILD

	return			(result);
}

bool CScriptStorage::parse_namespace(LPCSTR caNamespaceName, LPSTR b, u32 const b_size, LPSTR c, u32 const c_size)
{
	*b				= 0;
	*c				= 0;
	LPSTR			S2 = xr_strdup(caNamespaceName);

	LPSTR			S	= S2;
	for (int i=0;;++i) {
		if (!xr_strlen(S)) {
			script_log	(ScriptStorage::eLuaMessageTypeError,"the namespace name %s is incorrect!",caNamespaceName);
			return		(false);
		}
		LPSTR			S1 = strchr(S,'.');
		if (S1)
			*S1				= 0;

		if (i)
			xr_strcat		(b,b_size,"{");
		xr_strcat			(b,b_size,S);
		xr_strcat			(b,b_size,"=");
		if (i)
			xr_strcat		(c,c_size,"}");
		if (S1)
			S			= ++S1;
		else
			break;
	}

	return			(true);
}

bool CScriptStorage::load_buffer(lua_State* L, LPCSTR caBuffer, size_t tSize, LPCSTR caScriptName, LPCSTR caNameSpaceName)
{
	int					l_iErrorCode;
	if (caNameSpaceName && xr_strcmp("_G", caNameSpaceName)) 
	{
		string512 insert, a, b;
		LPCSTR header = file_header;

		if (!parse_namespace(caNameSpaceName, a, sizeof(a), b, sizeof(b)))
			return		(false);

		xr_sprintf(insert, header, caNameSpaceName, a, b);
		u32				str_len = xr_strlen(insert);
		size_t const total_size = str_len + tSize;
		LPSTR			script = 0;
		bool dynamic_allocation = false;

		__try 
		{
			if (total_size < 768 * 1024)
				script = (LPSTR)_alloca(total_size);
			else {
				script = (LPSTR)Memory.mem_alloc(total_size);
				dynamic_allocation = true;
			}
		}
		__except (GetExceptionCode() == STATUS_STACK_OVERFLOW)
		{
			int							errcode = _resetstkoflw();
			R_ASSERT2(errcode, "Could not reset the stack after \"Stack overflow\" exception!");
			script = (LPSTR)Memory.mem_alloc(total_size);
			dynamic_allocation = true;
		};

		xr_strcpy(script, total_size, insert);
		CopyMemory(script + str_len, caBuffer, u32(tSize));

		l_iErrorCode = luaL_loadbuffer(L, script, tSize + str_len, caScriptName);

		if (dynamic_allocation)
			xr_free(script);
	}
	else
	{
		l_iErrorCode = luaL_loadbuffer(L, caBuffer, tSize, caScriptName);
	}

	if (l_iErrorCode) {
		print_output(L, caScriptName, l_iErrorCode);

		on_error(L);
		return (false);
	}
	return (true);
}

bool CScriptStorage::do_file(LPCSTR caScriptName, LPCSTR caNameSpaceName)
{
	int				start = lua_gettop(lua());
	IReader* l_tpFileReader = FS.r_open(caScriptName);

	if (!l_tpFileReader) {
		script_log(eLuaMessageTypeError, "Cannot open file \"%s\"", caScriptName);
		return		(false);
	}

	if (!load_buffer(lua(), static_cast<LPCSTR>(l_tpFileReader->pointer()), (size_t)l_tpFileReader->length(), caNameSpaceName, caNameSpaceName))
	{
		lua_settop(lua(), start);
		FS.r_close(l_tpFileReader);
		return		(false);
	}
	FS.r_close(l_tpFileReader);

	int errFuncId = -1;

	// because that's the first and the only call of the main chunk - there is no point to compile it
	int	l_iErrorCode = lua_pcall(lua(), 0, 0, (-1 == errFuncId) ? 0 : errFuncId);

	if (l_iErrorCode) 
	{
		print_output(lua(), caScriptName, l_iErrorCode);
		on_error(lua());
		lua_settop(lua(), start);
		return (false);
	}

	return (true);
}

bool CScriptStorage::load_file_into_namespace(LPCSTR caScriptName, LPCSTR caNamespaceName)
{
	int				start = lua_gettop(lua());
	if (!do_file(caScriptName,caNamespaceName)) {
		lua_settop	(lua(),start);
		return		(false);
	}
	VERIFY			(lua_gettop(lua()) == start);
	return			(true);
}

bool CScriptStorage::namespace_loaded(LPCSTR N, bool remove_from_stack)
{
	int						start = lua_gettop(lua());
	lua_pushstring 			(lua(),"_G"); 
	lua_rawget 				(lua(),LUA_GLOBALSINDEX); 
	string256				S2;
	xr_strcpy					(S2,N);
	LPSTR					S = S2;
	for (;;) { 
		if (!xr_strlen(S)) {
			VERIFY			(lua_gettop(lua()) >= 1);
			lua_pop			(lua(), 1); 
			VERIFY			(start == lua_gettop(lua()));
			return			(false); 
		}
		LPSTR				S1 = strchr(S,'.'); 
		if (S1)
			*S1				= 0; 
		lua_pushstring 		(lua(),S); 
		lua_rawget 			(lua(),-2); 
		if (lua_isnil(lua(),-1)) { 
//			lua_settop		(lua(),0);
			VERIFY			(lua_gettop(lua()) >= 2);
			lua_pop			(lua(),2); 
			VERIFY			(start == lua_gettop(lua()));
			return			(false);	//	there is no namespace!
		}
		else 
			if (!lua_istable(lua(),-1)) { 
//				lua_settop	(lua(),0);
				VERIFY		(lua_gettop(lua()) >= 1);
				lua_pop		(lua(),1); 
				VERIFY		(start == lua_gettop(lua()));
				FATAL		(" Error : the namespace name is already being used by the non-table object!\n");
				return		(false); 
			} 
			lua_remove		(lua(),-2); 
			if (S1)
				S			= ++S1; 
			else
				break; 
	} 
	if (!remove_from_stack) {
		VERIFY				(lua_gettop(lua()) == start + 1);
	}
	else {
		VERIFY				(lua_gettop(lua()) >= 1);
		lua_pop				(lua(),1);
		VERIFY				(lua_gettop(lua()) == start);
	}
	return					(true); 
}

bool CScriptStorage::object	(LPCSTR identifier, int type)
{
	int						start = lua_gettop(lua());
	lua_pushnil				(lua()); 
	while (lua_next(lua(), -2)) {
		auto luaType = lua_type(lua(), -1);
		if ((luaType == type) && !xr_strcmp(identifier,lua_tostring(lua(), -2))) {
			VERIFY			(lua_gettop(lua()) >= 3);
			lua_pop			(lua(), 3); 
			VERIFY			(lua_gettop(lua()) == start - 1);
			return			(true); 
		} 
		lua_pop				(lua(), 1); 
	} 
	VERIFY					(lua_gettop(lua()) >= 1);
	lua_pop					(lua(), 1); 
	VERIFY					(lua_gettop(lua()) == start - 1);
	return					(false); 
}

bool CScriptStorage::object	(LPCSTR namespace_name, LPCSTR identifier, int type)
{
	int						start = lua_gettop(lua());
	if (xr_strlen(namespace_name) && !namespace_loaded(namespace_name,false)) {
		VERIFY				(lua_gettop(lua()) == start);
		return				(false); 
	}
	bool					result = object(identifier,type);
	VERIFY					(lua_gettop(lua()) == start);
	return					(result); 
}

luabind::object CScriptStorage::name_space(LPCSTR namespace_name)
{
	string256			S1;
	xr_strcpy				(S1,namespace_name);
	LPSTR				S = S1;
	luabind::object		lua_namespace = luabind::get_globals(lua());
	for (;;) {
		if (!xr_strlen(S))
			return		(lua_namespace);
		LPSTR			I = strchr(S,'.');
		if (!I)
			return		(lua_namespace[S]);
		*I				= 0;
		lua_namespace	= lua_namespace[S];
		S				= I + 1;
	}
}

struct raii_guard 
{
	int m_error_code;
	LPCSTR const& m_error_description;
	raii_guard(const raii_guard& other) = delete;
	raii_guard& operator=(const raii_guard& other) = delete;
	raii_guard	(int error_code, LPCSTR const& m_description) : m_error_code(error_code), m_error_description(m_description) {}
	~raii_guard()
	{
		if (!m_error_code)
			return;

		R_ASSERT2(!m_error_code, m_error_description);
	}
}; // struct raii_guard

bool CScriptStorage::print_output(lua_State *L, LPCSTR caScriptFileName, int iErorCode)
{	
	if (iErorCode)
		print_error		(L,iErorCode);

#if 1
	lua_getglobal(L, "debug"); // stack: err debug
	lua_getfield(L, -1, "traceback"); // stack: err debug debug.traceback

	if (lua_pcall(L, 0, 1, 0)) {
		const char* err = lua_tostring(L, -1);
		Msg("[LUA] Error in debug.traceback() call: %s\n", err);
	}
#endif

	LPCSTR err = "see call_stack for details!";
	raii_guard guard(iErorCode, err);

	return true;
}

void CScriptStorage::print_error(lua_State *L, int iErrorCode)
{
	switch (iErrorCode) {
		case LUA_ERRRUN : {
			script_log (ScriptStorage::eLuaMessageTypeError,"SCRIPT RUNTIME ERROR");
			break;
		}
		case LUA_ERRMEM : {
			script_log (ScriptStorage::eLuaMessageTypeError,"SCRIPT ERROR (memory allocation)");
			break;
		}
		case LUA_ERRERR : {
			script_log (ScriptStorage::eLuaMessageTypeError,"SCRIPT ERROR (while running the error handler function)");
			break;
		}
		case LUA_ERRFILE : {
			script_log (ScriptStorage::eLuaMessageTypeError,"SCRIPT ERROR (while running file)");
			break;
		}
		case LUA_ERRSYNTAX : {
			script_log (ScriptStorage::eLuaMessageTypeError,"SCRIPT SYNTAX ERROR");
			break;
		}
		case LUA_YIELD : {
			script_log (ScriptStorage::eLuaMessageTypeInfo,"Thread is yielded");
			break;
		}
		default : NODEFAULT;
	}
}

#ifdef DEBUG
void CScriptStorage::flush_log()
{
	string_path log_file_name;
	xr_strconcat(log_file_name, Core.ApplicationName, "_", Core.UserName, "_lua.log");
	FS.update_path(log_file_name, "$logs$", log_file_name);
	m_output.save_to(log_file_name);
}
#endif // DEBUG

int CScriptStorage::error_log	(LPCSTR	format, ...)
{
	va_list			marker;
	va_start		(marker,format);

	LPCSTR			S = "! [LUA][ERROR] ";
	LPSTR			S1;
	string4096		S2;
	xr_strcpy		(S2,S);
	S1				= S2 + xr_strlen(S);

	int				result = vsprintf(S1,format,marker);
	va_end			(marker);

	Msg				("%s",S2);

	return			(result);
}