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

class CScriptHit
{
public:
	float m_fPower = 100;
	Fvector m_tDirection = { 1, 1, 1};
	shared_str m_caBoneName = "";
	CScriptGameObject* m_tpDraftsman = nullptr;
	float m_fImpulse = 0;
	ALife::EHitType m_tHitType = ALife::eHitTypeWound;

public:
	IC CScriptHit() = default;
	virtual ~CScriptHit() = default;

	IC CScriptHit(const CScriptHit* tpLuaHit)
	{
		*this = *tpLuaHit;
	}

	IC void set_bone_name(const char* bone_name)
	{
		m_caBoneName = bone_name;
	}

	DECLARE_SCRIPT_REGISTER_FUNCTION
};