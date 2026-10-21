#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowSettings.h"
class CELCrowEntity;
class CELCrowServer;
class ISaveObject;

class CELCrowPersistence
{
public:
	CELCrowPersistence(CELCrowEntity& OwnerEntity, const SELCrowSettings& CrowSettings, SELCrowState& CrowState);
	bool Restore();
	void Sync(bool Forced = false);
	void Serialize(ISaveObject& Object);

private:
	CELCrowServer* ResolveServer() const;
	bool Place();
	CELCrowEntity& Owner;
	const SELCrowSettings& Settings;
	SELCrowState& State;
	bool HasRestored = false;
	bool HasReadServer = false;
	bool HasClientState = false;
	bool HasPlacementSupport = false;
	Fvector PlacementContact = {0.f, 0.f, 0.f};
	u32 NextSync = 0;
	u32 PlacementAttempt = 0;
};
