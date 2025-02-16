#include "stdafx.h"
#include "CustomTimer.h"
#include "pch_script.h"
#include "actor.h"

using namespace luabind;

CTimerCustomWrapper::CTimerCustomWrapper(CTimersManager *parent) :
	CTimerCustom(parent)
{
}

CTimerCustomWrapper::~CTimerCustomWrapper()
{
}
 
void CTimerCustomWrapper::load				(IReader& stream)
{
	luabind::call_member<void>		(this, "load", stream);
}
void CTimerCustomWrapper::save				(NET_Packet& stream)
{
	luabind::call_member<void>		(this, "save", stream);
}

void CTimerCustomWrapper::load_static			(CTimerCustom* self, IReader& stream)
{
	self->CTimerCustom::load		(stream);
}

void CTimerCustomWrapper::save_static			(CTimerCustom* self, NET_Packet& stream)
{
	self->CTimerCustom::save		(stream);
}

void CTimerCustomWrapper::execute				(LPCSTR action)
{
	luabind::call_member<void>		(this, "execute", action);
}

void CTimerCustomWrapper::execute_static		(CTimerCustom* self, LPCSTR action)
{
	self->CTimerCustom::execute					(action);
}



#pragma optimize("s",on)
void CTimerCustom::script_register(lua_State *L)
{
	module(L)
	[
		class_<CTimerCustom, CTimerCustomWrapper>("Timer")
			.def(constructor<>())
			.def("SetName",						&CTimerCustom::SetName)
			.def("Name",						&CTimerCustom::Name)
			.def("SetTimerType",				&CTimerCustom::SetTimerType)
			.def("Time",						&CTimerCustom::Time)
			.def("isGameTimer",					&CTimerCustom::isGameTimer)
			.def("TimeElapsed",					&CTimerCustom::TimeElapsed)
			.def("SetAction",					&CTimerCustom::SetAction)

			.def_readwrite("day",				&CTimerCustom::m_day)
			.def_readwrite("hour",				&CTimerCustom::m_hour)
			.def_readwrite("min",				&CTimerCustom::m_min)
			.def_readwrite("sec",				&CTimerCustom::m_sec)
			.def_readwrite("ms",				&CTimerCustom::m_ms)
			.def_readwrite("game_time",			&CTimerCustom::m_game_time)

			.def("SetHUD",						&CTimerCustom::SetHUD)
			.def("isHUD",						&CTimerCustom::isHUD)
			.def("Valide",						&CTimerCustom::valide)

			.def("save",						&CTimerCustom::save, &CTimerCustomWrapper::save_static)
			.def("load",						&CTimerCustom::load, &CTimerCustomWrapper::load_static)

			.def("execute",						&CTimerCustom::execute, &CTimerCustomWrapper::execute_static)

		
	];
}


