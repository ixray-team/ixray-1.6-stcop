////////////////////////////////////////////////////////////////////////////
//	Module 		: script_binder_object_script.cpp
//	Created 	: 29.03.2004
//  Modified 	: 29.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Script object binder script export
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_binder_object.h"
#include "../xrScripts/script_export_space.h"
#include "script_binder_object_wrapper.h"
#include "xrServer_Objects_ALife.h"

using namespace luabind;

#pragma optimize("s",on)
void CScriptBinderObject::script_register(lua_State *L)
{
	module(L)
	[
		class_<CScriptBinderObject, CScriptBinderObjectWrapper>("object_binder")
			.def_readwrite("object",			&CScriptBinderObject::m_object)
			.def(								constructor<luabind::object>())
	];
}