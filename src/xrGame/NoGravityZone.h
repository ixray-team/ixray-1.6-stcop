#pragma once
#include "AnomalyZone.h"

class CNoGravityZone final : public CAnomalyZone
{
	using inherited = CAnomalyZone;

public:
	CNoGravityZone* cast_no_gravity_zone() override { return this; }

protected:
	virtual void enter_Zone(SZoneObjectInfo& io) override;
	virtual void exit_Zone(SZoneObjectInfo& io) override;

private:
	void switchGravity(SZoneObjectInfo& io, bool val);
	virtual void UpdateWorkload(u32 dt) override;
};
