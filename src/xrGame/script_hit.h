////////////////////////////////////////////////////////////////////////////
//	Module 		: script_hit.h
//	Created 	: 06.02.2004
//  Modified 	: 24.06.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script hit class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "alife_space.h"
#include "../xrScripts/script_export_space.h"

class CScriptGameObject;

XREFLECT(LuaEnum, Self="CScriptHit", Name="hit_type", Names="burn=eScriptHitBurn,shock=eScriptHitShock,strike=eScriptHitStrike,wound=eScriptHitWound,radiation=eScriptHitRadiation,telepatic=eScriptHitTelepatic,chemical_burn=eScriptHitChemicalBurn,explosion=eScriptHitExplosion,fire_wound=eScriptHitFireWound,light_burn=eScriptHitLightBurn,physic_strike=eScriptHitPhysicStrike,dummy=eScriptHitMax")
enum EScriptHitType
{
	eScriptHitBurn = ALife::eHitTypeBurn,
	eScriptHitShock = ALife::eHitTypeShock,
	eScriptHitStrike = ALife::eHitTypeStrike,
	eScriptHitWound = ALife::eHitTypeWound,
	eScriptHitRadiation = ALife::eHitTypeRadiation,
	eScriptHitTelepatic = ALife::eHitTypeTelepatic,
	eScriptHitChemicalBurn = ALife::eHitTypeChemicalBurn,
	eScriptHitExplosion = ALife::eHitTypeExplosion,
	eScriptHitFireWound = ALife::eHitTypeFireWound,
	eScriptHitLightBurn = ALife::eHitTypeLightBurn,
	eScriptHitPhysicStrike = ALife::eHitTypePhysicStrike,
	eScriptHitMax = ALife::eHitTypeMax,
};

XREFLECT(LuaExport, Name="hit")
class CScriptHit
{
public:
	XPROPERTY(LuaReadWrite, Name="power")
	float m_fPower = 100;
	XPROPERTY(LuaReadWrite, Name="direction")
	Fvector m_tDirection = { 1, 1, 1};
	shared_str m_caBoneName = "";
	XPROPERTY(LuaReadWrite, Name="draftsman")
	CScriptGameObject* m_tpDraftsman = nullptr;
	XPROPERTY(LuaReadWrite, Name="impulse")
	float m_fImpulse = 0;
	XPROPERTY(LuaReadWrite, Name="type")
	ALife::EHitType m_tHitType = ALife::eHitTypeWound;

public:
	XPROPERTY(LuaConstructor)
	IC CScriptHit() = default;
	virtual ~CScriptHit() = default;

	XPROPERTY(LuaConstructor)
	IC CScriptHit(const CScriptHit* tpLuaHit)
	{
		*this = *tpLuaHit;
	}

	XPROPERTY(LuaCallable, Name="bone")
	IC void set_bone_name(const char* bone_name)
	{
		m_caBoneName = bone_name;
	}

};