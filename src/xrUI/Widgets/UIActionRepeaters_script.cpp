#include "stdafx.h"
#include "UIActionRepeaters.h"

#include <luabind/luabind.hpp>
using namespace luabind;

#pragma optimize("s",on)

void CUIActionRepeatersOwner::script_register(lua_State *L)
{
	module(L)
		[
			class_<CUIActionRepeatersOwner>("CUIActionRepeatersOwner")
		];
}


void CUIActionRepeatersManager::script_register(lua_State *L)
{
	module(L)
		[
			class_<CUIActionRepeatersManager>("CUIActionRepeatersManager")

			.def("Register",			(void (CUIActionRepeatersManager::*)(CUIActionRepeatersOwner*,int,int,int))(&CUIActionRepeatersManager::Register))
			.def("UnregisterOwner",		(void (CUIActionRepeatersManager::*)(CUIActionRepeatersOwner*)) &CUIActionRepeatersManager::UnregisterOwner)
			.def("ResetAll",			(void (CUIActionRepeatersManager::*)(CUIActionRepeatersOwner*)) &CUIActionRepeatersManager::ResetAll)
			.def("Reset",				(void (CUIActionRepeatersManager::*)(int)) &CUIActionRepeatersManager::Reset)
			.def("Reset",				(void (CUIActionRepeatersManager::*)(CUIActionRepeatersOwner*, int)) &CUIActionRepeatersManager::Reset)
			.def("CanRepeatActionNow",	(bool (CUIActionRepeatersManager::*)(CUIActionRepeatersOwner*, int)) &CUIActionRepeatersManager::CanRepeatActionNow)
			.def("SetActionStarted",	(void (CUIActionRepeatersManager::*)(CUIActionRepeatersOwner*, int)) &CUIActionRepeatersManager::SetActionStarted)
			.def("IsActionStarted",		(bool (CUIActionRepeatersManager::*)(CUIActionRepeatersOwner*, int)) &CUIActionRepeatersManager::IsActionStarted)
		];
}