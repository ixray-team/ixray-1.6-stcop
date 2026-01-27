#pragma once
class CActor;

class CPickUpManager
{
	CActor* Owner;

	//расстояние подсветки предметов
	float PickupInfoRadius;

	//режим подбирания предметов
	bool PickupMode;
	collide::rq_results RQR;

	// HoldToAction settings
	float HoldTime = 10.0f;
	float CurrentHoldTime = 0.0f;
	bool bIsPressed = false;
	bool bIsProcessed = false;
	
public:
	CPickUpManager(CActor* Owner);

	// HoldToAction events
	xr_delegate<void(float)> OnHoldActivating; // calling on tick during pick up activating with [0-1] fraction of activation status
	xr_delegate<void()> OnHoldActivate; // process pick up
	xr_delegate<void()> OnHoldAbort; // if we failed to start pick up
	xr_delegate<void()> OnSimpleActivate; // if we don't need HoldToAction
	xr_delegate<bool()> VerifyHoldToActionAvailable; // return true if this can trigger HoldToAction, false otherwise

	void UpdateClPickup(bool IsPressed);
	
	void RenderInfo();

	bool CanPickItem(const CFrustum& frustum, const Fvector& from, CObject* item);

	IC void SetPickupRadius(float Radius) { PickupInfoRadius = Radius; }
	IC void SetPickupMode(bool State) { PickupMode = State; }
	IC bool GetPickupMode() const { return PickupMode; }

private:
	void PickupInfoDraw(CObject* object);
};