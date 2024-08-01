///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "bloodsucker_ix.h"

using namespace luabind;

#pragma optimize("s",on)
void CAI_BloodsuckerIX::script_register(lua_State* L)
{
	module(L)
	[
		class_<CAI_BloodsuckerIX, CGameObject>("CAI_BloodsuckerIX")
		.def(constructor<>())
	];
}