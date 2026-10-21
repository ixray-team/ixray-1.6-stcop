#pragma once
#include "xrCore/ECS/EntityOwner.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELLogic.h"
class IECSOwner;

struct TExtendedEntityesLogic final
{
	TExtendedEntityesLogic() = default;
	~TExtendedEntityesLogic();
	TExtendedEntityesLogic(TExtendedEntityesLogic&& Other) noexcept;
	TExtendedEntityesLogic& operator=(TExtendedEntityesLogic&& Other) noexcept;
	TExtendedEntityesLogic(const TExtendedEntityesLogic&) = delete;
	TExtendedEntityesLogic& operator=(const TExtendedEntityesLogic&) = delete;
	void BeginComponent(IECSOwner* ComponentOwner);
	void EndComponent();
	void Initialize();
	void Detach();
	bool Observe();
	void Serialize(ISaveObject& Object);
	EELControl Update(float DeltaTime);
	void FrameUpdate(float DeltaTime);
	void NoticeActorLight(const Fvector& Point);
	void Hear(const SELSoundEvent& SoundEvent);
	void ForgetTarget(u16 ObjectIdentifier);
	void RemoveObject(u16 ObjectIdentifier);
	bool Accept(const SELObservation& Observation, CBaseMonster& SourceMonster);
	bool IsActive() const;
	u16 GetTargetIdentifier() const;

private:
	CBaseMonster* OwnerMonster = nullptr;
	xr_unique_ptr<IELLogic> Logic;
	bool HasAttached = false;
	u32 ControlRevision = 0;
	u32 LastBehaviorUpdate = 0;
	u32 NextDecisionAt = 0;
	ECS_COMPONENT(TExtendedEntityesLogic)
	ECS_VALUE(HasAttached, "Attached")
	ECS_END
};
