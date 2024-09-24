#include "stdafx.h"
#include "pch_script.h"
#include "bloodsucker.h"

using namespace luabind;

#pragma optimize("s",on)
void CustomBloodsucker::script_register(lua_State *L)
{
	module(L)
	[
		class_<CustomBloodsucker,CGameObject>("CAI_Bloodsucker")
			.def(constructor<>())
			.def("force_visibility_state", &CustomBloodsucker::force_visibility_state)
	];
}
