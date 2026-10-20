#include "stdafx.h"
#include <algorithm>
#include "ai/ExtendedEntityesLogic/EntityesCategory/Ground/Entityes/Aggressive/Behaviors/CELGroundApproach.h"
#include "ai/ExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "ai_space.h"
#include "level_graph.h"

// Сбрасывает временное состояние перед новым действием.
void CELGroundApproach::Reset()
{
	ReconsiderAt = 0;
	GoalVertex = u32(-1);
}

// Выбирает разрешённый способ подхода к текущему противнику.
void CELGroundApproach::ChooseStrategy(IELAdapter& Adapter, const SELPerceptionProfile& Profile, float Distance, bool HasVision, u32 NearbyCount, const Fvector& KnownPosition)
{
	EELApproachStrategy Allowed[u32(EELApproachStrategy::Count)];
	u32 Count = 0;
	u32 StrategyIndex;
	const bool CanPrepareJump = HasVision && Adapter.Monster().com_man().get_jump_control() && Adapter.Monster().com_man().check_if_jump_possible(KnownPosition, true);
	for (StrategyIndex = 0; StrategyIndex < u32(EELApproachStrategy::Count); ++StrategyIndex)
	{
		if ((Profile.Strategies & (1u << StrategyIndex)) && (EELApproachStrategy(StrategyIndex) != EELApproachStrategy::Lunge || CanPrepareJump))
		{
			Allowed[Count++] = EELApproachStrategy(StrategyIndex);
		}
	}
	Strategy = Count ? Allowed[(Adapter.Monster().ID() + Phase++) % Count] : EELApproachStrategy::Direct;
	if (HasVision && Distance > 15.f && (Profile.Strategies & ELApproachStrategyMask(EELApproachStrategy::Charge)))
	{
		Strategy = EELApproachStrategy::Charge;
	}
	if (CanPrepareJump && Distance < 7.f && (Profile.Strategies & ELApproachStrategyMask(EELApproachStrategy::Lunge)))
	{
		Strategy = EELApproachStrategy::Lunge;
	}
	if (Adapter.Monster().GetfHealth() < .35f && !NearbyCount && (Profile.Strategies & ELApproachStrategyMask(EELApproachStrategy::Retreat)))
	{
		Strategy = EELApproachStrategy::Retreat;
	}
	if (NearbyCount >= 3 && (Profile.Strategies & ELApproachStrategyMask(EELApproachStrategy::Encircle)))
	{
		Strategy = Adapter.Monster().ID() % 5 < 3 ? EELApproachStrategy::Direct : EELApproachStrategy::Encircle;
	}
	if (!HasVision && !Profile.CanTrackLive)
	{
		Strategy = (Profile.Strategies & ELApproachStrategyMask(EELApproachStrategy::Cautious)) ? EELApproachStrategy::Cautious : EELApproachStrategy::Direct;
	}
}

// Выбирает достижимую точку подхода по текущей стратегии и расположению союзников.
void CELGroundApproach::Select(IELAdapter& Adapter, const SELPerceptionProfile& Profile, const Fvector& KnownPosition, bool HasVision)
{
	CBaseMonster& OwnerMonster = Adapter.Monster();
	if (s32(Device.dwTimeGlobal - ReconsiderAt) < 0 && LastKnownPosition.distance_to_sqr(KnownPosition) < 16.f &&
		OwnerMonster.Position().distance_to_sqr(GoalPosition) > 2.25f)
	{
		return;
	}
	Fvector Separation;
	const u32 NearbyCount = GatherPeers(Adapter, KnownPosition, Separation, Profile.TargetCrowdDistance);
	const float Distance = OwnerMonster.Position().distance_to(KnownPosition);
	ChooseStrategy(Adapter, Profile, Distance, HasVision, NearbyCount, KnownPosition);
	Fvector Forward;
	Forward.sub(OwnerMonster.Position(), KnownPosition);
	Forward.y = 0.f;
	Forward.normalize_safe();
	Fvector Lateral = {Forward.z, 0.f, -Forward.x};
	Lateral.mul(OwnerMonster.ID() % 2 ? 1.f : -1.f);
	const float Width = std::min(Profile.ApproachWidth, Distance * .45f);
	GoalPosition = KnownPosition;
	if (HasVision || Profile.CanTrackLive)
	{
		if (Strategy == EELApproachStrategy::Flank || Strategy == EELApproachStrategy::Wide || Strategy == EELApproachStrategy::Encircle || Strategy == EELApproachStrategy::Pressure)
		{
			GoalPosition.mad(Lateral, Width * (Strategy == EELApproachStrategy::Wide ? 1.6f : 1.f));
			if (NearbyCount >= 3)
			{
				GoalPosition.mad(Forward, 3.f + float(OwnerMonster.ID() % 4));
			}
		}
		else if (Strategy == EELApproachStrategy::Lunge && Distance < 7.f && Phase % 3 == 0)
		{
			GoalPosition.mad(Forward, 3.f);
			GoalPosition.mad(Lateral, 2.f);
		}
		else if (Strategy == EELApproachStrategy::Retreat && !NearbyCount && OwnerMonster.GetfHealth() < .35f)
		{
			GoalPosition.mad(Forward, 8.f);
		}
		else if (Strategy == EELApproachStrategy::Direct && NearbyCount)
		{
			GoalPosition.mad(Lateral, std::min(Width, 1.5f));
		}
		GoalPosition.add(Separation);
	}
	GoalVertex = ai().level_graph().vertex_id(GoalPosition);
	if (ai().level_graph().valid_vertex_id(GoalVertex))
	{
		GoalPosition.y = ai().level_graph().vertex_plane_y(GoalVertex, GoalPosition.x, GoalPosition.z);
	}
	if (!Adapter.CanReach(GoalPosition, GoalVertex))
	{
		GoalPosition = KnownPosition;
		GoalVertex = ai().level_graph().vertex_id(GoalPosition);
	}
	if (ai().level_graph().valid_vertex_id(GoalVertex))
	{
		GoalPosition.y = ai().level_graph().vertex_plane_y(GoalVertex, GoalPosition.x, GoalPosition.z);
	}
	IsRunning = (HasVision || Profile.CanTrackLive) && Strategy != EELApproachStrategy::Cautious;
	LastKnownPosition = KnownPosition;
	ReconsiderAt = Device.dwTimeGlobal + (Strategy == EELApproachStrategy::Charge ? 2200 : 1400 + OwnerMonster.ID() % 1200);
}

// Возвращает короткое имя текущей стратегии для отладки.
const char* CELGroundApproach::Description() const
{
	static const char* Names[] = {"Прямой подход", "Обход с фланга", "Широкая дуга", "Осторожный подход", "Сближение перед прыжком", "Рывок", "Окружение", "Дуга сближения", "Отступление"};
	static_assert(std::size(Names) == u32(EELApproachStrategy::Count));
	return Names[u32(Strategy)];
}

// Собирает близких союзников для выбора свободного направления подхода.
u32 CELGroundApproach::GatherPeers(IELAdapter& Adapter, const Fvector& KnownPosition, Fvector& Separation, float CrowdDistance)
{
	CBaseMonster& OwnerMonster = Adapter.Monster();
	u32 NearbyCount = 0;
	u32 Checked = 0;
	CEntityAlive* PeerEntity;
	CBaseMonster* PeerMonster;
	xr_vector<u16>::const_iterator OwnerIterator;
	Separation.set(0.f, 0.f, 0.f);
	Fvector Away;
	float Gap;
	for (OwnerIterator = ExtendedEntityes::Owners().begin(); OwnerIterator != ExtendedEntityes::Owners().end() && Checked < 32; ++OwnerIterator)
	{
		++Checked;
		PeerEntity = ExtendedEntityes::FindEntity(*OwnerIterator);
		PeerMonster = PeerEntity ? smart_cast<CBaseMonster*>(PeerEntity) : nullptr;
		if (!PeerMonster || PeerMonster == &OwnerMonster || !CELSharedEnemySharing::CanReceive(OwnerMonster, *PeerMonster))
		{
			continue;
		}
		if (PeerMonster->Position().distance_to_sqr(KnownPosition) < CrowdDistance * CrowdDistance)
		{
			++NearbyCount;
		}
		Away.sub(OwnerMonster.Position(), PeerMonster->Position());
		Gap = Away.magnitude();
		if (Gap > EPS_L && Gap < 3.f)
		{
			Separation.mad(Away, (3.f - Gap) / Gap);
		}
	}
	return NearbyCount;
}
