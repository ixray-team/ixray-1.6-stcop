#pragma once
#include "AnomalyZone.h"
#include "../xrScripts/script_export_space.h"

class CRadioactiveZone final : public CAnomalyZone
{
	using inherited = CAnomalyZone;

public:
	CRadioactiveZone();
	virtual ~CRadioactiveZone() override;

	virtual void Load(const char* section) override;
	virtual void Affect(SZoneObjectInfo* O) override;
	virtual void feel_touch_new(CObject* O) override;
	virtual void UpdateWorkload(u32 dt) override; // related to fast-mode optimizations
	virtual bool feel_touch_contact(CObject* O) override;
	float nearest_shape_radius(SZoneObjectInfo* O);

	CRadioactiveZone* cast_radioactive_zone() override { return this; }

	float fHitPower = 0;

protected:
	bool legacyHit = false;
	virtual bool BlowoutState() override;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
