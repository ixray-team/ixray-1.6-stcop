////////////////////////////////////////////////////////////////////////////
//	Module 		: script_storage.h
//	Created 	: 01.04.2004
//  Modified 	: 01.04.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script Storage
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "script_storage_space.h"
#include "script_space_forward.h"

struct lua_State;
class CScriptThread;

using namespace ScriptStorage;

class SCRIPTS_API CScriptStorage 
{
private:
	lua_State					*m_virtual_machine	;
	CScriptThread				*m_current_thread	;
	bool						m_jit				;

protected:
	CMemoryWriter				m_output;

protected:
	static	int					vscript_log					(ScriptStorage::ELuaMessageType tLuaMessageType, const char* caFormat, va_list marker);
			bool				parse_namespace				(const char* caNamespaceName, LPSTR b, u32 const b_size, LPSTR c, u32 const c_size);
			bool				do_file						(const char*	caScriptName, const char* caNameSpaceName);
			void				reinit						();

public:
			void				print_stack					();

public:
								CScriptStorage				();
	virtual						~CScriptStorage				();
	IC		lua_State			*lua						();
	IC		void				current_thread				(CScriptThread *thread);
	IC		CScriptThread		*current_thread				() const;
			bool				load_buffer					(lua_State *L, const char* caBuffer, size_t tSize, const char* caScriptName, const char* caNameSpaceName = 0);
			bool				load_file_into_namespace	(const char*	caScriptName, const char* caNamespaceName);
			bool				namespace_loaded			(const char*	caName, bool remove_from_stack = true);
			bool				object						(const char*	caIdentifier, int type);
			bool				object						(const char*	caNamespaceName, const char*	caIdentifier, int type);
			luabind::object		name_space					(const char*	namespace_name);
			int					error_log					(const char*	caFormat, ...);
	static	int		__cdecl		script_log					(ELuaMessageType message,	const char*	caFormat, ...);
	static	bool				print_output				(lua_State *L,		const char*	caScriptName,		int		iErorCode = 0);
	static	void				print_error					(lua_State *L,		int		iErrorCode);
	virtual	void				on_error					(lua_State *L) = 0;

#ifdef DEBUG
public:
			void				flush_log					();
#endif // DEBUG
};

#include "script_storage_inline.h"