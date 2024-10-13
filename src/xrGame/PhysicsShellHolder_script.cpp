#include "pch_script.h"
#include "PhysicsShellHolder.h"

using namespace luabind;

#pragma optimize("s",on)
void CPhysicsShellHolder::script_register(lua_State *L)
{
	module(L)
		[
			class_<CPhysicsShellHolder, CGameObject>("CPhysicsShellHolder")
			.def(constructor<>())
		];
}