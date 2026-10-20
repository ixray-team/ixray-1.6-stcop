#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingAdapter.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai/monsters/control_path_builder_base.h"
#include "Level.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"

// Проверяет сферический обзор с коротким интервалом обновления, чтобы стая не забирала всю квоту первыми владельцами.
bool CELFlyingAdapter::CanSee(const CEntityAlive& Candidate) const
{
	if (!Candidate.g_Alive() || Candidate.getDestroy() || ExtendedEntityes::IsIgnored(Candidate))
	{
		return false;
	}
	const u32 Now = Device.dwTimeGlobal;
	const u32 Generation = ExtendedEntityes::OwnerGeneration(Candidate.ID());
	constexpr u32 SightInterval = 50;
	auto Found = SightResults.find(Candidate.ID());
	if (Found != SightResults.end() && Found->second.TargetGeneration == Generation && Now - Found->second.CheckedAt < SightInterval)
	{
		return Found->second.Visible;
	}

	// Штатный RayQuery требует изменяемый CObject; координаты скрытой цели не передаются схеме.
	Fvector ViewOrigin;
	OwnerMonster.Center(ViewOrigin);
	// Сферический обзор не привязан к анимированной голове, которая может оказаться у самого перекрытия.
	const EELSightResult Result = CELSharedSight::CheckFrom(OwnerMonster, const_cast<CEntityAlive&>(Candidate), 360.f, OwnerMonster.eye_range, ViewOrigin);
	if (Result == EELSightResult::WaitingForBudget)
	{
		return false;
	}
	if (SightResults.size() >= 128)
	{
		for (auto Iterator = SightResults.begin(); Iterator != SightResults.end();)
		{
			if (Now - Iterator->second.CheckedAt >= SightInterval)
			{
				Iterator = SightResults.erase(Iterator);
			}
			else
			{
				++Iterator;
			}
		}
	}
	const bool IsVisible = Result == EELSightResult::Visible;
	if (SightResults.size() < 128 || SightResults.find(Candidate.ID()) != SightResults.end())
	{
		SightResults[Candidate.ID()] = {Now, Generation, IsVisible};
	}
	return IsVisible;
}

// Проверяет допустимость назначения с учётом ограничений категории.
bool CELFlyingAdapter::CanReach(const Fvector& TargetPosition, u32 TargetVertex) const
{
	return _valid(TargetPosition) && Level().ObjectSpace.GetBoundingVolume().contains(TargetPosition);
}

// Отключает штатное наземное построение пути у воздушной сущности.
void CELFlyingAdapter::ReleaseControl()
{
	// The native flying entity has no ground FSM to restart or character to move.
	OwnerMonster.path().disable_path();
}
