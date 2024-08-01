///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Тушкан (Большой)
//	Мутант: Тушкан (Матёрый большой)
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"

#include "../../tushkano/tushkano.h"
#include "../../extended/tushkano_big/tushkano_big.h"

#include "tushkano_big_black.h"

using namespace luabind;

#pragma optimize("s",on)
void CTushkanoBigBlack::script_register(lua_State* L)
{
	module(L)
	[
		class_<CTushkanoBigBlack, CGameObject>("CTushkanoBigBlack")
		.def(constructor<>())
	];
}