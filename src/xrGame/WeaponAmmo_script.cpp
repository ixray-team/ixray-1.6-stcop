#include "stdafx.h"
#include "pch_script.h"
#include "WeaponAmmo.h"
#include "script_game_object.h"

using namespace luabind;

#pragma optimize("s",on)
void CWeaponAmmo::script_register(lua_State *L)
{
	module(L)
		[
			class_<SCartridgeParam>("SCartridgeParam")
			.def(constructor<>())
			.def("Init", &SCartridgeParam::Init)
			.def_readwrite("kDist ", &SCartridgeParam::kDist)
			.def_readwrite("kDisp ", &SCartridgeParam::kDisp)
			.def_readwrite("kHit ", &SCartridgeParam::kHit)
			.def_readwrite("kImpulse ", &SCartridgeParam::kImpulse)
			.def_readwrite("kAP ", &SCartridgeParam::kAP)
			.def_readwrite("kAirRes ", &SCartridgeParam::kAirRes)
			.def_readwrite("buckShot", &SCartridgeParam::buckShot)
			.def_readwrite("impair", &SCartridgeParam::impair)
			.def_readwrite("u8ColorID", &SCartridgeParam::u8ColorID)
			,
			class_<CCartridge>("CCartridge")
			.def(constructor<>())
			.def("Weight", &CCartridge::Weight)
			.def_readwrite("m_LocalAmmoType", &CCartridge::m_LocalAmmoType)
			.def_readwrite("m_4to1_tracer", &CCartridge::m_4to1_tracer)
			.def_readwrite("bullet_material_idx", &CCartridge::bullet_material_idx)
			.def_readwrite("m_flags", &CCartridge::m_flags)
			.def_readwrite("param_s", &CCartridge::param_s)
			.enum_("cartridge_flags")
			[
				value("cfTracer", int(CCartridge::cfTracer)),
				value("cfRicochet", int(CCartridge::cfRicochet)),
				value("cfCanBeUnlimited", int(CCartridge::cfCanBeUnlimited)),
				value("cfExplosive", int(CCartridge::cfExplosive)),
				value("cfMagneticBeam", int(CCartridge::cfMagneticBeam))
			]
			.def("GetInventoryName", &CCartridge::GetInventoryName)
			,
			class_<CWeaponAmmo, CGameObject>("CWeaponAmmo")
			.def(constructor<>())
			.def_readwrite("m_boxSize", &CWeaponAmmo::m_boxSize)
			.def_readwrite("m_boxCurr", &CWeaponAmmo::m_boxCurr)
			.def_readwrite("m_tracer", &CWeaponAmmo::m_tracer)
			.def_readwrite("m_4to1_tracer", &CWeaponAmmo::m_4to1_tracer)
			.def("Weight", &CWeaponAmmo::Weight)
			.def("Cost", &CWeaponAmmo::Cost)
			.def("Get", &CWeaponAmmo::Get)
		];
}