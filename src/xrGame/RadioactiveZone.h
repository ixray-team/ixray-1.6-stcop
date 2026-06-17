#pragma once
#include "AnomalyZone.h"
#include "../xrScripts/script_export_space.h"

class CRadioactiveZone final : public CAnomalyZone
{
	using inherited = CAnomalyZone;

public:
	CRadioactiveZone();
	virtual ~CRadioactiveZone();

	virtual void Load(const char* section);
	virtual void Affect(SZoneObjectInfo* O);
	virtual void feel_touch_new(CObject* O);
	virtual void UpdateWorkload(u32 dt); // related to fast-mode optimizations
	virtual bool feel_touch_contact(CObject* O);
	float nearest_shape_radius(SZoneObjectInfo* O);

	CRadioactiveZone* cast_radioactive_zone() override { return this; }

	float fHitPower = 0;

protected:
	bool legacyHit = false;
	virtual bool BlowoutState();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
