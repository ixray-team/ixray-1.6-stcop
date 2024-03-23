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
public:
	CPickUpManager(CActor* Owner);
	
	void RenderInfo();

	bool CanPickItem(const CFrustum& frustum, const Fvector& from, CObject* item);

	IC void SetPickupRadius(float Radius) { PickupInfoRadius = Radius; }
	IC void SetPickupMode(bool State) { PickupMode = State; }
	IC bool GetPickupMode() const { return PickupMode; }

private:
	void PickupInfoDraw(CObject* object);
};