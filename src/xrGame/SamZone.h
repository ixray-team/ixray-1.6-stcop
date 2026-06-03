#pragma once
#include "HudSound.h"
#include "RocketLauncher.h"
#include "smart_zone.h"
#include "space_restrictor.h"

class CSamZone : public CSmartZone, public CRocketLauncher {
private:
	typedef CSmartZone			inherited;

protected:

	shared_str m_missile_section;

public:

	virtual void					shedule_Update(u32		time_delta) override;
	virtual bool					net_Spawn(CSE_Abstract* DC) override;

	HUD_SOUND_COLLECTION_LAYERED	m_layered_sounds;

	void Load(str_c		section) override;
	void LaunchMissile(CGameObject* target);

	virtual void OnEvent(NET_Packet& P, u16 type) override;

};
