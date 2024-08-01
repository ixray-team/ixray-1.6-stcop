///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Таракан (Чернобыльский)
//	Мутант: Таракан (Чернобыльский матёрый)
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"

#include "../../../tushkano/tushkano.h"
#include "../../../extended/beetle/beetle_normal/beetle.h"

#include "beetle_black.h"

using namespace luabind;

#pragma optimize("s",on)
void CBeetleBlack::script_register(lua_State *L) 
{
	module(L) 
	[
		class_<CBeetleBlack,CGameObject>("CBeetleBlack")
		.def(constructor<>())
	];
}
