#pragma once

#include "AnomalyZone.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "ZoneVisual.h"
#include "../../../xrPhysics/PHUpdateObject.h"

class CAmebaZone final : public CVisualZone, public CPHUpdateObject
{
	using inherited = CVisualZone;
	float m_fVelocityLimit;

public:
	CAmebaZone();
	virtual ~CAmebaZone() override;
	virtual void Affect(SZoneObjectInfo* O) override;

	CAmebaZone* cast_ameba_zone() override { return this; }

protected:
	virtual void PhTune(float step) override;
	virtual void PhDataUpdate(float step) override { ; }
	virtual bool BlowoutState() override;
	virtual void SwitchZoneState(EZoneState new_state) override;
	virtual void Load(const char* section) override;
	virtual float distance_to_center(CObject* O);
};
