#include "pch_script.h"
#include "ghostboss.h"

using namespace luabind;

#pragma optimize("s",on)
void CGhostBoss::script_register(lua_State *L)
{
	module(L)
	[
		class_<CGhostBoss, CGameObject>("CGhostBoss")
			.def(constructor<>())
	];
}
