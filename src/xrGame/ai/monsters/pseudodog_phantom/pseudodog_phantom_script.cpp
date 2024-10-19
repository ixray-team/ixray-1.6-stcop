#include "stdafx.h"
#include "pch_script.h"
#include "../pseudodog/pseudodog.h"
#include "pseudodog_phantom.h"

using namespace luabind;

void CPseudoPsyDogPhantomBase::script_register(lua_State* L)
{
	module(L)
	[
		class_<CPseudoPsyDogPhantomBase, CGameObject>("CPseudoPsyDogPhantomBase")
			.def(constructor<>())
	];
}