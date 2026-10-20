#pragma once
#include "../../CoreComponent/CELComponentBase.h"
#include "CELFlyingSettings.h"
#include "CELFlyingMovement.h"
#include "Behaviors/CELFlyingBehaviorManager.h"
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
	void DiagnoseFlight(float ActualSpeed, bool CanMove);
	u32 StalledAt = 0;
	u32 LastStallLogAt = 0;
	bool HasStallLog = false;
};
