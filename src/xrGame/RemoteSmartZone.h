#pragma once

#include "../xrServerEntities/xrServer_Objects_ALife.h"

class CLevel;

class CRemoteSmartZone final : public CSE_ALifeSmartZone
{
	CLevel* ServerLevel = nullptr;
	CSE_ALifeSmartZone* Remote = nullptr;

public:
	CRemoteSmartZone();
	bool Initialize(CSE_ALifeSmartZone* remote, CLevel* serverLevel);

	void update() override;
	void smart_touch(CSE_ALifeMonsterAbstract* monster) override;
	bool enabled(CSE_ALifeMonsterAbstract* object) const override;
	float suitable(CSE_ALifeMonsterAbstract* object) const override;
	void register_npc(CSE_ALifeMonsterAbstract* object) override;
	void unregister_npc(CSE_ALifeMonsterAbstract* object) override;
	CALifeSmartTerrainTask* task(CSE_ALifeMonsterAbstract* object) override;
	float detect_probability() override;
};
