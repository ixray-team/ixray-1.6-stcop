///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Матёрый)
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"

#include "../../tushkano/tushkano.h"

#include "tushkano_black.h"

using namespace luabind;

#pragma optimize("s",on)
void CTushkanoBlack::script_register(lua_State* L)
{
	module(L)
	[
		class_<CTushkanoBlack, CGameObject>("CTushkanoBlack")
			.def(constructor<>())
	];
}