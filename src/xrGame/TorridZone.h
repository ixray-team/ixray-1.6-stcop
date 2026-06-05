#pragma once
#include "MosquitoBald.h"

class CObjectAnimator;

class CTorridZone final : public CMosquitoBald
{
	using inherited = CAnomalyZone;
	CObjectAnimator* m_animator;

public:
	CTorridZone();
	virtual ~CTorridZone();
	virtual void UpdateWorkload(u32 dt);
	virtual void shedule_Update(u32 dt);
	bool net_Spawn(CSE_Abstract* DC);
	virtual bool IsVisibleForZones() { return true; }
	virtual bool Enable();
	virtual bool Disable();
	// Lain: added
	virtual bool light_in_slow_mode();
	virtual bool AlwaysTheCrow();
	CTorridZone* cast_torrid_zone() override { return this; }
};
