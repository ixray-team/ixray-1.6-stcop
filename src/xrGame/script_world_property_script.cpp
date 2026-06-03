////////////////////////////////////////////////////////////////////////////
//	Module 		: script_world_property_script.h
//	Created 	: 19.03.2004
//  Modified 	: 19.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Script world property script export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "script_world_property.h"
#include "operator_abstract.h"

using namespace luabind;

#pragma optimize("s",on)
void CScriptWorldPropertyWrapper::script_register(lua_State *L)
{
	module(L)
	[
		class_<CWorldProperty>("world_property")
			.def(constructor<u32, bool>())
			.def("condition", &CWorldProperty::condition)
			.def("value", &CWorldProperty::value)
			.def(const_self < other<CWorldProperty>())
			.def(const_self == other<CWorldProperty>())
	];
}