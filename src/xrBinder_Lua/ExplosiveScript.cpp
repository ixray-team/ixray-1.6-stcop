#include "StdAfx.h"
#include "pch_script.h"
#include "../xrGame/Explosive.h"

using namespace luabind;

#pragma optimize("s",on)
void CExplosive::script_register(lua_State *L)
{
	module(L)
	[
		class_<CExplosive>("explosive")
			.def("explode",					(&CExplosive::Explode))
	];
}

