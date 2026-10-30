#pragma once
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Behaviors/CELFlyingBehaviorBase.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"

class CELFlyingSearchBehavior final : public CELFlyingBehaviorBase
{
public:
	CELFlyingSearchBehavior(IELAdapter& Adapter, const SELFlyingSettings& Settings, CELFlyingMovement& Movement, const SELPerceptionProfile& Profile);
	void SetEvidence(const SELObservation& Evidence);
	void Enter() override;
	void Update(float DeltaTime) override;
	void FrameUpdate(float DeltaTime) override;
	void AfterMovement() override;
	SELBehaviorValidation Validate() const override;
	const char* ModeDescription() const override { return "ВОЗДУШНЫЙ ПОИСК"; }
protected:
	void ChooseGoal() override;
	const char* FlightDescription() const override { return Phase < 2 ? "Проверка последней известной позиции" : "Расширение поиска по следу"; }
private:
	bool Expired() const;
	const SELPerceptionProfile& Profile;
	u32 Until = 0;
	u32 Phase = 0;
};
