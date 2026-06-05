#pragma once

#include "AnomalyZone.h"
//#include "../Include/xrRender/KinematicsAnimated.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "ZoneVisual.h"

#include "../xrScripts/script_export_space.h"

class CHairsZone final : public CVisualZone
{
	using inherited = CVisualZone;

public:
	virtual void Affect(SZoneObjectInfo* O);
	virtual void Load(const char* section);

	CHairsZone* cast_hairs_zone() override { return this; }

protected:
	float m_min_speed_to_react;
	virtual bool BlowoutState();
	virtual void CheckForAwaking();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
