#include "pch_script.h"
#include "stdafx.h"
#include "swampbeast.h"

using namespace luabind;

#pragma optimize("s",on)
void CAI_SwampBeast::script_register(lua_State *L)
{
	module(L)
	[
		class_<CAI_SwampBeast,CGameObject>("CAI_SwampBeast")
			.def(constructor<>())
	];
}
