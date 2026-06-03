#include "stdafx.h"
#include "pch_script.h"
#include "CustomOutfit.h"
#include "ActorHelmet.h"

using namespace luabind;

#pragma optimize("s",on)
void CCustomOutfit::script_register(lua_State *L)
{
	module(L)
		[
			class_<CCustomOutfit, CGameObject>("CCustomOutfit")
			.def(constructor<>())
			.def_readwrite("m_fPowerLoss", static_cast<float CCustomOutfit::*>(&CCustomOutfit::m_fPowerLoss))
			.def_readwrite("m_additional_weight", &CCustomOutfit::m_additional_weight)
			.def_readwrite("m_additional_weight2", &CCustomOutfit::m_additional_weight2)
			.def_readwrite("m_fHealthRestoreSpeed", static_cast<float CCustomOutfit::*>(&CCustomOutfit::m_fHealthRestoreSpeed))
			.def_readwrite("m_fRadiationRestoreSpeed", static_cast<float CCustomOutfit::*>(&CCustomOutfit::m_fRadiationRestoreSpeed))
			.def_readwrite("m_fSatietyRestoreSpeed", static_cast<float CCustomOutfit::*>(&CCustomOutfit::m_fSatietyRestoreSpeed))
			.def_readwrite("m_fThirstRestoreSpeed", static_cast<float CCustomOutfit::*>(&CCustomOutfit::m_fThirstRestoreSpeed))
			.def_readwrite("m_fPowerRestoreSpeed", static_cast<float CCustomOutfit::*>(&CCustomOutfit::m_fPowerRestoreSpeed))
			.def_readwrite("m_fBleedingRestoreSpeed", static_cast<float CCustomOutfit::*>(&CCustomOutfit::m_fBleedingRestoreSpeed))
			.def_readonly("bIsHelmetAvaliable", &CCustomOutfit::bIsHelmetAvaliable)
			.def("BonePassBullet", &CCustomOutfit::BonePassBullet)
			.def("get_artefact_count", &CCustomOutfit::get_artefact_count),

			class_<CHelmet, CGameObject>("CHelmet")
			.def(constructor<>())
		];
}