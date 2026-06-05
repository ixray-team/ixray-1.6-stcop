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
	~CAmebaZone();
	virtual void Affect(SZoneObjectInfo* O);

	CAmebaZone* cast_ameba_zone() override { return this; }

protected:
	virtual void PhTune(float step);
	virtual void PhDataUpdate(float step) { ; }
	virtual bool BlowoutState();
	virtual void SwitchZoneState(EZoneState new_state);
	virtual void Load(const char* section);
	virtual float distance_to_center(CObject* O);
};
