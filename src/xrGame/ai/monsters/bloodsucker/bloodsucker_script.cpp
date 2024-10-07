#include "stdafx.h"
#include "pch_script.h"
#include "bloodsucker.h"

using namespace luabind;

#pragma optimize("s",on)
void CBloodsuckerBase::script_register(lua_State *L)
{
	module(L)
	[
		class_<CBloodsuckerBase,CGameObject>("CBloodsuckerBase")
			.def(constructor<>())
			.def("force_visibility_state", &CBloodsuckerBase::force_visibility_state)
	];
}
