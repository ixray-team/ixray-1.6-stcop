///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан
//	Мутант: Тушкан (Большой)
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"

#include "../../tushkano/tushkano.h"

#include "tushkano_big.h"

using namespace luabind;

#pragma optimize("s",on)
void CTushkanoBig::script_register(lua_State* L)
{
	module(L)
	[
		class_<CTushkanoBig, CGameObject>("CTushkanoBig")
			.def(constructor<>())
	];
}