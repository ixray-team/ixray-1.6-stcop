///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Упырь (Подземный)
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"

#include "../../tushkano/tushkano.h"

#include "ghoul.h"

using namespace luabind;

#pragma optimize("s",on)
void CGhoul::script_register(lua_State* L)
{
	module(L)
		[
			class_<CGhoul, CGameObject>("CGhoul")
				.def(constructor<>())
		];
}