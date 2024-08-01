///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Летающий)
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "tushkano_fly.h"

using namespace luabind;

#pragma optimize("s",on)
void CTushkanoFly::script_register(lua_State* L)
{
	module(L)
	[
		class_<CTushkanoFly, CGameObject>("CTushkanoFly")
		.def(constructor<>())
	];
}
