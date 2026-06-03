#include "stdafx.h"
#include "anomal_pseudo_gigant.h"
//#include "anomal_pseudo_gigant_wrapper.h"

using namespace luabind;

#pragma optimize("s",on)
void CAnomalPseudoGigant::script_register(lua_State *L)
{
	module(L)
	[
		class_<CAnomalPseudoGigant, CGameObject>("CAnomalPseudoGigant")
			.def(constructor<>())
			/*.def("on_shield_on", &CAnomalPseudoGigant::on_shield_on, &CAnomalPseudoGigantWrapper::on_shield_on)
			.def("on_shield_off", &CAnomalPseudoGigant::on_shield_off, &CAnomalPseudoGigantWrapper::on_shield_off)
			.def("on_hit", &CAnomalPseudoGigant::on_hit, &CAnomalPseudoGigantWrapper::on_hit)
			.def("on_jump", &CAnomalPseudoGigant::on_jump, &CAnomalPseudoGigantWrapper::on_jump)*/
	];
}
