#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject")
class CWeaponSVD final : 
	public CWeaponMagazined
{
	using inherited = CWeaponMagazined;

protected:
	virtual void switch2_Fire() override;
	virtual void OnAnimationEnd(u8 state) override;


public:
	XPROPERTY(LuaConstructor)
	CWeaponSVD() = default;
	virtual ~CWeaponSVD() override = default;
};