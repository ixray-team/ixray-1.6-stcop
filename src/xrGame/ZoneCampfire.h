#pragma once
#include "MosquitoBald.h"

XREFLECT(LuaExport)
class CZoneCampfire final : public CMosquitoBald
{
	using inherited = CMosquitoBald;

protected:
	xr_shared_ptr<CParticlesObject> m_pEnablingParticles;
	xr_shared_ptr<CParticlesObject> m_pDisabledParticles;
	ref_sound m_disabled_sound;
	bool m_turned_on;
	u32 m_turn_time;

	virtual void PlayIdleParticles(bool bIdleLight = true);
	virtual void StopIdleParticles(bool bIdleLight = true);
	virtual bool AlwaysTheCrow();
	virtual void UpdateWorkload(u32 dt);

public:
	XPROPERTY(LuaConstructor)
	CZoneCampfire();
	virtual ~CZoneCampfire();
	virtual void Load(const char* section);
	virtual void GoEnabledState();
	virtual void GoDisabledState();

	XPROPERTY(LuaCallable, Name="turn_on")
	void turn_on_script();
	XPROPERTY(LuaCallable, Name="turn_off")
	void turn_off_script();
	XPROPERTY(LuaCallable)
	bool is_on();
	virtual void shedule_Update(u32 dt);
	CZoneCampfire* cast_zone_campfire() override { return this; }
};
