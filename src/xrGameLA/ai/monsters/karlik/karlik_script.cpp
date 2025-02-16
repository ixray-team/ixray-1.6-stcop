#include "stdafx.h"
#include "../../../pch_script.h"
#include "karlik.h"

using namespace luabind;

#pragma optimize("s",on)
void CKarlik::script_register(lua_State *L)
{
	module(L)
	[
		class_<CKarlik,CGameObject>("CKarlik")
			.def(constructor<>())
	];
}
