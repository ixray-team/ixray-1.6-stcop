#pragma once
#include "MosquitoBald.h"

class CObjectAnimator;

XREFLECT(LuaExport)
class CTorridZone final : public CMosquitoBald
{
	using inherited = CAnomalyZone;
	CObjectAnimator* m_animator;

public:
	XPROPERTY(LuaConstructor)
	CTorridZone();
	virtual ~CTorridZone() override;
	virtual void UpdateWorkload(u32 dt) override;
	virtual void shedule_Update(u32 dt) override;
	virtual bool net_Spawn(CSE_Abstract* DC) override;
	virtual bool IsVisibleForZones() override { return true; }
	virtual bool Enable() override;
	virtual bool Disable() override;
	// Lain: added
	virtual bool light_in_slow_mode() override;
	virtual bool AlwaysTheCrow() override;
	CTorridZone* cast_torrid_zone() override { return this; }
};
