#include "stdafx.h"
#include "pch_script.h"
#include "chimera.h"

using namespace luabind;

#pragma optimize("s",on)
void CChimeraBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CChimeraBase,CGameObject>("CChimeraBase")
			.def(constructor<>())
	];
}
