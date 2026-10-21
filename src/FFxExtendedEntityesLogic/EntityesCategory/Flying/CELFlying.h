#pragma once
#include "FFxExtendedEntityesLogic/CoreComponent/CELComponentBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingSettings.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingMovement.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingBehaviorManager.h"
class CELFlyingBehaviorBase;

class CELFlying : public CELComponentBase
{
public:
	CELFlying(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	~CELFlying() override;
	EELControl Update(float DeltaTime) override;
	void FrameUpdate(float DeltaTime) override;
	void Detach() override;
	void ForgetTarget(u16 ObjectIdentifier) override;
	void RemoveObject(u16 ObjectIdentifier) override;

protected:
	void ObserveVision(CBaseMonster& OwnerMonster, u32 CurrentTime) override;
	void SelectFlight(CELFlyingBehaviorBase& Behavior);
	void ClearBehavior();
	virtual bool IsAttacking() const { return false; }
	virtual Fvector DebugGoal() const;
	const SELFlyingSettings& FlightSettings;
	CELFlyingMovement Movement;
	friend class CELFlyingBehaviorManager;
	CELFlyingBehaviorManager BehaviorManager;
	u32 VisionCursor = 0;
	u32 NextSphereVisionAt = 0;
};
