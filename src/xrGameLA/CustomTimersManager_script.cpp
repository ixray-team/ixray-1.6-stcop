#include "stdafx.h"
#include "CustomTimersManager.h"
#include "pch_script.h"

using namespace luabind;

#pragma optimize("s",on)
void CTimersManager::script_register(lua_State *L)
{
	module(L)
		[

			class_<CTimersManager>("TimerManager")
				.def(constructor<>())
				.def("AddTimer",						&CTimersManager::AddTimer)
				.def("RemoveTimer",						&CTimersManager::RemoveTimer)
				.def("GetTimerByName",					&CTimersManager::GetTimerByName)
				.def("IsAnyHUDTimerActive",				&CTimersManager::IsAnyHUDTimerActive)
				.def("TimerExist",						&CTimersManager::TimerExist)
		];
}