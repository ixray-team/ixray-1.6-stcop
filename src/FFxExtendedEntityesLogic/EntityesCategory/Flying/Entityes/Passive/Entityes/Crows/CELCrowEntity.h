#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingEntityBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowAnimation.h"

class CELCrowEntity final : public CELFlyingEntityBase
{
public:
	CELCrowEntity();
	~CELCrowEntity() override;
	bool net_Spawn(CSE_Abstract* ServerEntity) override;
	void net_Destroy() override;
	void Die(CObject* Killer) override;
	void UpdateCL() override;
	void Serialize(ISaveObject& Object) override;
	CELCrowAnimation& Animation() { return CrowAnimation; }
	char* get_monster_class_name() override { return const_cast<char*>("scavenger_crow"); }

private:
	CELCrowAnimation CrowAnimation;
};
