////////////////////////////////////////////////////////////////////////////
//	Module 		: actor_script.cpp
//	Created 	: 17.01.2008
//  Modified 	: 17.01.2008
//	Author		: Dmitriy Iassenev
//	Description : actor script export
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "actor.h"
#include "level_changer.h"

using namespace luabind;

bool isFirstPerson() { return Actor()->active_cam() == eacFirstEye; }
void setFirstPerson() { Actor()->cam_Set(eacFirstEye); }
void setThirdPerson() { Actor()->cam_Set(eacLookAt); }

#pragma optimize("s",on)
void CActor::script_register(lua_State* L)
{
	module(L)
		[
			class_<CActor, CGameObject>("CActor")
				.def(constructor<>()),
				class_<CLevelChanger, CGameObject>("CLevelChanger")
				.def(constructor<>()),
				def("is_first_person", isFirstPerson),
				def("set_first_person", setFirstPerson),
				def("set_third_person", setThirdPerson)
		];
}
