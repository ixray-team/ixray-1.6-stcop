#include "StdAfx.h"
#include "pch_script.h"
#include "../xrGame/holder_custom.h"
#include "../xrGame/HolderEntityObject.h"

using namespace luabind;

#pragma optimize("s",on)
void CHolderCustom::script_register(lua_State *L)
{
	module(L)
		[
			class_<CHolderCustom>("holder")
			.def("engaged",			&CHolderCustom::Engaged)
			.def("Action",			&CHolderCustom::Action)
//			.def("SetParam",		(void (CHolderCustom::*)(int,Fvector2)) &CHolderCustom::SetParam)
			.def("SetParam",		(void (CHolderCustom::*)(int,Fvector)) &CHolderCustom::SetParam)
			.def("SetEnterLocked",	&CHolderCustom::SetEnterLocked)
			.def("SetExitLocked",	&CHolderCustom::SetExitLocked)
		];
}