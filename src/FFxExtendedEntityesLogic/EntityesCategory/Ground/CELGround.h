#pragma once
#include "FFxExtendedEntityesLogic/CoreComponent/CELComponentBase.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundBehaviorManager.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/Behaviors/CELGroundCloseScent.h"

class CELGround : public CELComponentBase
{
public:
	CELGround(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile);
	~CELGround() override;
	EELControl Update(float DeltaTime) override;
	void Detach() override;
	void ForgetTarget(u16 ObjectIdentifier) override;
	void RemoveObject(u16 ObjectIdentifier) override;
	void FrameUpdate(float DeltaTime) override;

protected:
	void OnVisualContact(CEntityAlive& Target, u32 ObservedAt) override;
	void ObserveCloseScent() override;
	CELGroundCloseScent CloseScent;
	virtual EELControl UpdateTarget(const SELObservation& Observation, float DeltaTime);
	friend class CELGroundBehaviorManager;
	CELGroundBehaviorManager BehaviorManager;
};
