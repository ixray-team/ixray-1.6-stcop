#pragma once
#include "FFxExtendedEntityesLogic/CoreComponent/CELSharedBehaviorManager.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELLogic.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundReactionBehavior.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBodyBehavior.h"
class CELGround;

class CELGroundBehaviorManager final : public CELSharedBehaviorManager
{
public:
	CELGroundBehaviorManager(CELGround& OwnerLogic, IELAdapter& OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	~CELGroundBehaviorManager() override;
	EELControl UpdateGround(float DeltaTime);

private:
	bool UpdateEscape(float DeltaTime);
	bool UpdateInvestigation(float DeltaTime);
	CELGround& Owner;
	IELAdapter& Adapter;
	const SELPerceptionProfile& Profile;
	CELGroundReactionBehavior ReactionBehavior;
	CELGroundBodyBehavior BodyBehavior;
	u32 BodyRetryAfter = 0;
};
