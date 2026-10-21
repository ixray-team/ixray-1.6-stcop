#include "FFxExtendedEntityesLogic/stdafx.h"
#include "xrGame/Level.h"
#include <algorithm>
#include <cmath>
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedDeaths.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedEnemySharing.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedSight.h"
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedAnomalies.h"
#include "FFxExtendedEntityesLogic/CELSharedSettings.h"
#include "FFxExtendedEntityesLogic/CELSharedPerceptionBudget.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai_object_location.h"
#include "xrGame/enemy_manager.h"

namespace ExtendedEntityesDeaths
{
struct SDeath
{
	u16 Identifier = u16(-1);
	u32 Time = 0;
	bool IsEnvironmental = true;
	u16 Killer = u16(-1);
	u32 KillerGeneration = 0;
};
struct SLocalKnowledge
{
	CELSharedMemory::Records Evidence;
	xr_vector<SELNavigationObstacle> Hazards;
	u32 Generation = 0;
	u32 NextScan = 0;
	u32 FightUntil = 0;
	u32 WitnessedAt = 0;
	u16 WitnessedVictim = u16(-1);
};
struct SClaim
{
	u16 OwnerIdentifier = u16(-1);
	u16 BodyIdentifier = u16(-1);
	u32 DeathTime = 0;
	u32 PackIdentifier = 0;
	u32 Until = 0;
	bool IsCompleted = false;
};
xr_map<u16, SLocalKnowledge> Knowledge;
xr_vector<SDeath> Deaths;
xr_vector<SClaim> Claims;
u32 DeathCursor = 0;

// Упаковывает команду, отряд и группу монстра в идентификатор его стаи.
u32 Pack(const CBaseMonster& Monster)
{
	return (u32(Monster.g_Team()) << 16) | (u32(Monster.g_Squad()) << 8) | u32(Monster.g_Group());
}
// Возвращает личное хранилище сведений указанного владельца.
SLocalKnowledge& Local(u16 OwnerIdentifier)
{
	SLocalKnowledge& Result = Knowledge[OwnerIdentifier];
	const u32 Generation = ExtendedEntityes::OwnerGeneration(OwnerIdentifier);
	if (Result.Generation != Generation)
	{
		Result = SLocalKnowledge();
		Result.Generation = Generation;
	}
	return Result;
}
// Ищет сохранённую запись по владельцу или идентификатору цели.
const SLocalKnowledge* Find(u16 OwnerIdentifier)
{
	xr_map<u16, SLocalKnowledge>::const_iterator Found = Knowledge.find(OwnerIdentifier);
	return Found != Knowledge.end() && Found->second.Generation == ExtendedEntityes::OwnerGeneration(OwnerIdentifier) ? &Found->second : nullptr;
}

// Сохраняет сведения об опасной области в памяти получателя.
bool StoreHazard(SLocalKnowledge& LocalKnowledge, const SELObservation& Incoming)
{
	CELSharedMemory::Records::iterator Existing;
	for (Existing = LocalKnowledge.Evidence.begin(); Existing != LocalKnowledge.Evidence.end(); ++Existing)
	{
		if (Existing->Kind != EELObservationKind::Hazard || Existing->Position.distance_to_sqr(Incoming.Position) > Incoming.Radius * Incoming.Radius)
		{
			continue;
		}
		if (s32(Incoming.ObservedAt - Existing->ObservedAt) > 0)
		{
			*Existing = Incoming;
			return true;
		}
		return false;
	}
	if (LocalKnowledge.Evidence.size() < 48)
	{
		LocalKnowledge.Evidence.push_back(Incoming);
		return true;
	}
	return false;
}
// Пересчитывает известные опасные области по накопленным свидетельствам смертей.
void RebuildHazards(SLocalKnowledge& LocalKnowledge)
{
	LocalKnowledge.Hazards.clear();
	CELSharedMemory::Records::const_iterator Evidence;
	SELNavigationObstacle Hazard;
	for (Evidence = LocalKnowledge.Evidence.begin(); Evidence != LocalKnowledge.Evidence.end(); ++Evidence)
	{
		if (Evidence->Kind != EELObservationKind::Hazard || s32(Device.dwTimeGlobal - Evidence->ExpiresAt) >= 0)
		{
			continue;
		}
		Hazard.Position = Evidence->Position;
		Hazard.Radius = Evidence->Radius;
		LocalKnowledge.Hazards.push_back(Hazard);
	}
}
// Удаляет устаревшие свидетельства и обновляет связанные области опасности.
void Refresh(SLocalKnowledge& LocalKnowledge, const SELPerceptionProfile& Profile)
{
	const u32 CurrentTime = Device.dwTimeGlobal;
	LocalKnowledge.Evidence.erase(std::remove_if(LocalKnowledge.Evidence.begin(), LocalKnowledge.Evidence.end(), [CurrentTime](const SELObservation& Evidence)
												 { return s32(CurrentTime - Evidence.ExpiresAt) >= 0; }),
								  LocalKnowledge.Evidence.end());
	if (!Profile.CanAvoidDeaths)
	{
		LocalKnowledge.Hazards.clear();
		return;
	}
	CELSharedMemory::Records::const_iterator Anchor;
	CELSharedMemory::Records::const_iterator Evidence;
	xr_vector<SELObservation> NewHazards;
	u32 Count;
	u32 Latest;
	SELObservation Hazard;
	for (Anchor = LocalKnowledge.Evidence.begin(); Anchor != LocalKnowledge.Evidence.end() && NewHazards.size() < 16; ++Anchor)
	{
		if (Anchor->Kind != EELObservationKind::Body || !Anchor->IsEnvironmentalDeath || CurrentTime - Anchor->ObservedAt > Profile.BodyInterestTime)
		{
			continue;
		}
		Count = 0;
		Latest = 0;
		for (Evidence = LocalKnowledge.Evidence.begin(); Evidence != LocalKnowledge.Evidence.end(); ++Evidence)
		{
			if (Evidence->Kind == EELObservationKind::Body && Evidence->IsEnvironmentalDeath && CurrentTime - Evidence->ObservedAt <= Profile.BodyInterestTime && Evidence->Position.distance_to_sqr(Anchor->Position) <= Profile.DeathHazardRadius * Profile.DeathHazardRadius)
			{
				++Count;
				Latest = std::max(Latest, Evidence->ObservedAt);
			}
		}
		if (Count < Profile.DeathHazardThreshold)
		{
			continue;
		}
		Hazard = SELObservation();
		Hazard.Kind = EELObservationKind::Hazard;
		Hazard.Position = Anchor->Position;
		Hazard.Radius = Profile.DeathHazardRadius;
		Hazard.ObservedAt = Latest;
		Hazard.ExpiresAt = CurrentTime + Profile.DeathHazardTime;
		NewHazards.push_back(Hazard);
	}
	xr_vector<SELObservation>::const_iterator NewHazard;
	for (NewHazard = NewHazards.begin(); NewHazard != NewHazards.end(); ++NewHazard)
	{
		StoreHazard(LocalKnowledge, *NewHazard);
	}
	RebuildHazards(LocalKnowledge);
}
} // namespace ExtendedEntityesDeaths

// Регистрирует событие смерти для последующего обнаружения наблюдателями.
void CELSharedDeaths::Record(CBaseMonster& Victim, CObject* Killer)
{
	if (!CELSharedSettings::IsEnabled() || !OnServer())
	{
		return;
	}
	ExtendedEntityesDeaths::SDeath Death;
	Death.Identifier = Victim.ID();
	Death.Time = Device.dwTimeGlobal;
	Death.Killer = Killer ? Killer->ID() : u16(-1);
	Death.KillerGeneration = Killer ? ExtendedEntityes::OwnerGeneration(Killer->ID()) : 0;
	Death.IsEnvironmental = !Killer || !Killer->cast_entity_alive();
	xr_vector<ExtendedEntityesDeaths::SDeath>::iterator Existing;
	for (Existing = ExtendedEntityesDeaths::Deaths.begin(); Existing != ExtendedEntityesDeaths::Deaths.end(); ++Existing)
	{
		if (Existing->Identifier == Death.Identifier)
		{
			*Existing = Death;
			return;
		}
	}
	if (ExtendedEntityesDeaths::Deaths.size() < 256)
	{
		ExtendedEntityesDeaths::Deaths.push_back(Death);
	}
	else
	{
		ExtendedEntityesDeaths::Deaths[ExtendedEntityesDeaths::DeathCursor++ % 256] = Death;
	}
}

// Witnessing requires two independent visual confirmations. A hidden killer is never disclosed.
bool CELSharedDeaths::Witness(CBaseMonster& Observer, const SELPerceptionProfile& Profile, SELObservation& Enemy)
{
	if (Profile.IsPassive || !Profile.CanWitnessAggression)
	{
		return false;
	}
	ExtendedEntityesDeaths::SLocalKnowledge& Local = ExtendedEntityesDeaths::Local(Observer.ID());
	for (const ExtendedEntityesDeaths::SDeath& Death : ExtendedEntityesDeaths::Deaths)
	{
		if (Death.IsEnvironmental || Device.dwTimeGlobal - Death.Time > Profile.WitnessWindow ||
			(Local.WitnessedAt && s32(Death.Time - Local.WitnessedAt) <= 0))
		{
			continue;
		}
		CEntityAlive* VictimEntity = ExtendedEntityes::FindEntity(Death.Identifier);
		CBaseMonster* Victim = VictimEntity ? smart_cast<CBaseMonster*>(VictimEntity) : nullptr;
		CEntityAlive* Killer = ExtendedEntityes::FindEntity(Death.Killer);
		if (!Victim || !Killer || !Killer->g_Alive() || ExtendedEntityes::IsIgnored(*Killer) ||
			Victim->g_Alive() || std::abs(s32(Victim->GetLevelDeathTime() - Death.Time)) > 1000 ||
			Death.KillerGeneration != ExtendedEntityes::OwnerGeneration(Death.Killer) ||
			!CELSharedEnemySharing::IgnoresOrdinarySound(Observer, *Victim))
		{
			continue;
		}
		const float Angle = Observer.UsedAI_Locations() ? Observer.eye_fov : 360.f;
		const EELSightResult VictimSight = CELSharedSight::Check(Observer, *Victim, Angle, Observer.eye_range);
		if (VictimSight == EELSightResult::WaitingForBudget)
		{
			return false;
		}
		if (VictimSight != EELSightResult::Visible)
		{
			continue;
		}
		const EELSightResult KillerSight = CELSharedSight::Check(Observer, *Killer, Angle, Observer.eye_range);
		if (KillerSight == EELSightResult::WaitingForBudget)
		{
			return false;
		}
		Local.WitnessedAt = Death.Time;
		Local.WitnessedVictim = Death.Identifier;
		Enemy = SELObservation();
		Enemy.Position = Victim->Position();
		Enemy.ObservedAt = Device.dwTimeGlobal;
		Enemy.Source = EELObservationSource::Vision;
		if (KillerSight == EELSightResult::Visible && Observer.EnemyMan.is_enemy(Killer))
		{
			Enemy.TargetIdentifier = Killer->ID();
			Enemy.Position = Killer->Position();
			Enemy.VertexIdentifier = Killer->ai_location().level_vertex_id();
		}
		return true;
	}
	return false;
}

// Запоминает допустимое наблюдение, сохраняя срок памяти и ограниченный след.
bool CELSharedDeaths::Remember(CBaseMonster& Observer, const SELObservation& Evidence, const SELPerceptionProfile& Profile)
{
	if (Evidence.Kind == EELObservationKind::Enemy || s32(Device.dwTimeGlobal - Evidence.ExpiresAt) >= 0 || (!Profile.CanInspectBodies && !Profile.CanAvoidDeaths))
	{
		return false;
	}
	if (Evidence.Kind == EELObservationKind::Hazard)
	{
		if (!Profile.CanAvoidDeaths || !_valid(Evidence.Position) || !_valid(Evidence.Radius) || Evidence.Radius <= 0.f)
		{
			return false;
		}
		ExtendedEntityesDeaths::SLocalKnowledge& Local = ExtendedEntityesDeaths::Local(Observer.ID());
		if (!ExtendedEntityesDeaths::StoreHazard(Local, Evidence))
		{
			return false;
		}
		ExtendedEntityesDeaths::RebuildHazards(Local);
		return true;
	}
	CEntityAlive* BodyEntity = ExtendedEntityes::FindEntity(Evidence.TargetIdentifier);
	CBaseMonster* Body = BodyEntity ? smart_cast<CBaseMonster*>(BodyEntity) : nullptr;
	if (!Body || Body->g_Alive() || !CELSharedEnemySharing::AreFriends(Observer, *Body))
	{
		return false;
	}
	ExtendedEntityesDeaths::SLocalKnowledge& Local = ExtendedEntityesDeaths::Local(Observer.ID());
	CELSharedMemory::Records::iterator Existing;
	for (Existing = Local.Evidence.begin(); Existing != Local.Evidence.end(); ++Existing)
	{
		if (Existing->Kind == EELObservationKind::Body && Existing->TargetIdentifier == Evidence.TargetIdentifier)
		{
			if (s32(Evidence.ObservedAt - Existing->ObservedAt) <= 0)
			{
				return false;
			}
			*Existing = Evidence;
			ExtendedEntityesDeaths::Refresh(Local, Profile);
			return true;
		}
	}
	if (Local.Evidence.size() >= 48)
	{
		Existing = std::min_element(Local.Evidence.begin(), Local.Evidence.end(), [](const SELObservation& First, const SELObservation& Second)
									{ return First.ObservedAt < Second.ObservedAt; });
		*Existing = Evidence;
	}
	else
	{
		Local.Evidence.push_back(Evidence);
	}
	ExtendedEntityesDeaths::Refresh(Local, Profile);
	return true;
}

// Обновляет личные наблюдения владельца с учётом доступного бюджета.
void CELSharedDeaths::Observe(CBaseMonster& Observer, const SELPerceptionProfile& Profile)
{
	if (Profile.IsPassive || (!Profile.CanInspectBodies && !Profile.CanAvoidDeaths) || Profile.BodyDetectionRadius <= 0.f)
	{
		return;
	}
	ExtendedEntityesDeaths::SLocalKnowledge& Local = ExtendedEntityesDeaths::Local(Observer.ID());
	ExtendedEntityesDeaths::Refresh(Local, Profile);
	if (s32(Device.dwTimeGlobal - Local.NextScan) < 0 || !ExtendedEntityes::PerceptionBudget().TrySpatial())
	{
		return;
	}
	Local.NextScan = Device.dwTimeGlobal + Profile.PeripheralInterval + Observer.ID() % 201;
	xr_vector<ISpatialShared> Candidates;
	g_SpatialSpace->q_sphere(Candidates, 0, ESPATIAL_TYPE::MONSTER_DEAD, Observer.Position(), Profile.BodyDetectionRadius);
	xr_vector<ISpatialShared>::const_iterator CandidateIterator;
	xr_vector<ExtendedEntityesDeaths::SDeath>::const_iterator DeathIterator;
	CObject* CandidateObject;
	CBaseMonster* Body;
	SELObservation Evidence;
	for (CandidateIterator = Candidates.begin(); CandidateIterator != Candidates.end(); ++CandidateIterator)
	{
		CandidateObject = *CandidateIterator ? (*CandidateIterator)->dcast_CObject() : nullptr;
		Body = CandidateObject ? smart_cast<CBaseMonster*>(CandidateObject) : nullptr;
		if (!Body || Body->g_Alive() || Body->getDestroy() || !CELSharedEnemySharing::AreFriends(Observer, *Body) ||
			!CELSharedSight::Visible(Observer, *Body, Observer.eye_fov, Profile.BodyDetectionRadius))
		{
			continue;
		}
		Evidence = SELObservation();
		Evidence.Kind = EELObservationKind::Body;
		Evidence.TargetIdentifier = Body->ID();
		Evidence.ObservedAt = Body->GetLevelDeathTime();
		// Unknown older deaths are inspectable, but their cause must not be guessed.
		Evidence.IsEnvironmentalDeath = false;
		for (DeathIterator = ExtendedEntityesDeaths::Deaths.begin(); DeathIterator != ExtendedEntityesDeaths::Deaths.end(); ++DeathIterator)
		{
			if (DeathIterator->Identifier == Body->ID())
			{
				Evidence.ObservedAt = DeathIterator->Time;
				Evidence.IsEnvironmentalDeath = DeathIterator->IsEnvironmental;
				break;
			}
		}
		Evidence.Position = Body->Position();
		Evidence.VertexIdentifier = Body->ai_location().level_vertex_id();
		Evidence.ExpiresAt = Evidence.ObservedAt + std::max(Profile.BodyInterestTime, Profile.DeathHazardTime);
		Remember(Observer, Evidence, Profile);
	}
}

// Возвращает доступные владельцу сведения о смерти или теле.
const CELSharedMemory::Records& CELSharedDeaths::KnownEvidence(u16 OwnerIdentifier)
{
	const ExtendedEntityesDeaths::SLocalKnowledge* Local = ExtendedEntityesDeaths::Find(OwnerIdentifier);
	static const CELSharedMemory::Records Empty;
	return Local ? Local->Evidence : Empty;
}

// Резервирует исследование тела с учётом числа уже назначенных сущностей.
bool CELSharedDeaths::Claim(CBaseMonster& Observer, const SELPerceptionProfile& Profile, SELObservation& Evidence)
{
	if (!Profile.CanInspectBodies)
	{
		return false;
	}
	const u32 CurrentTime = Device.dwTimeGlobal;
	const u32 PackIdentifier = ExtendedEntityesDeaths::Pack(Observer);
	ExtendedEntityesDeaths::Claims.erase(std::remove_if(ExtendedEntityesDeaths::Claims.begin(), ExtendedEntityesDeaths::Claims.end(), [CurrentTime](const ExtendedEntityesDeaths::SClaim& Claim)
														{ return s32(CurrentTime - Claim.Until) >= 0; }),
										 ExtendedEntityesDeaths::Claims.end());
	const CELSharedMemory::Records& Bodies = KnownEvidence(Observer.ID());
	CELSharedMemory::Records::const_iterator BodyIterator;
	xr_vector<ExtendedEntityesDeaths::SClaim>::iterator ClaimIterator;
	u32 Investigators;
	bool Completed;
	for (BodyIterator = Bodies.begin(); BodyIterator != Bodies.end(); ++BodyIterator)
	{
		if (BodyIterator->Kind != EELObservationKind::Body || CurrentTime - BodyIterator->ObservedAt > Profile.BodyInterestTime || Contains(Observer.ID(), BodyIterator->Position))
		{
			continue;
		}
		Investigators = 0;
		Completed = false;
		for (ClaimIterator = ExtendedEntityesDeaths::Claims.begin(); ClaimIterator != ExtendedEntityesDeaths::Claims.end(); ++ClaimIterator)
		{
			if (ClaimIterator->PackIdentifier != PackIdentifier || ClaimIterator->BodyIdentifier != BodyIterator->TargetIdentifier || ClaimIterator->DeathTime != BodyIterator->ObservedAt)
			{
				continue;
			}
			Completed |= ClaimIterator->IsCompleted;
			++Investigators;
			if (ClaimIterator->OwnerIdentifier == Observer.ID() && !ClaimIterator->IsCompleted)
			{
				Evidence = *BodyIterator;
				return true;
			}
		}
		if (Completed || Investigators >= Profile.BodyInvestigatorLimit || ExtendedEntityesDeaths::Claims.size() >= 256)
		{
			continue;
		}
		ExtendedEntityesDeaths::SClaim Claim;
		Claim.OwnerIdentifier = Observer.ID();
		Claim.BodyIdentifier = BodyIterator->TargetIdentifier;
		Claim.DeathTime = BodyIterator->ObservedAt;
		Claim.PackIdentifier = PackIdentifier;
		Claim.Until = CurrentTime + Profile.SearchMax + 10000;
		ExtendedEntityesDeaths::Claims.push_back(Claim);
		Evidence = *BodyIterator;
		return true;
	}
	return false;
}

// Освобождает назначенное исследование тела.
void CELSharedDeaths::Release(u16 OwnerIdentifier, bool Completed)
{
	xr_vector<ExtendedEntityesDeaths::SClaim>::iterator ClaimIterator;
	for (ClaimIterator = ExtendedEntityesDeaths::Claims.begin(); ClaimIterator != ExtendedEntityesDeaths::Claims.end(); ++ClaimIterator)
	{
		if (ClaimIterator->OwnerIdentifier == OwnerIdentifier && !ClaimIterator->IsCompleted)
		{
			ClaimIterator->IsCompleted = Completed;
			ClaimIterator->Until = Completed ? ClaimIterator->DeathTime + 600000 : 0;
		}
	}
}

// Обновляет подтверждённый боевой контекст для правил опасных областей.
void CELSharedDeaths::UpdateCombat(CBaseMonster& Observer, const SELPerceptionProfile& Profile, bool SeesEnemy)
{
	ExtendedEntityesDeaths::SLocalKnowledge& Local = ExtendedEntityesDeaths::Local(Observer.ID());
	if (SeesEnemy)
	{
		Local.FightUntil = Device.dwTimeGlobal + Profile.DeathFightQuietTime;
	}
	ExtendedEntityesDeaths::Refresh(Local, Profile);
}

// Добавляет ещё актуальные препятствия в снимок следующего обхода.
void CELSharedDeaths::AppendObstacles(u16 OwnerIdentifier, xr_vector<SELNavigationObstacle>& Obstacles)
{
	const ExtendedEntityesDeaths::SLocalKnowledge* Local = ExtendedEntityesDeaths::Find(OwnerIdentifier);
	if (Local && s32(Device.dwTimeGlobal - Local->FightUntil) >= 0)
	{
		Obstacles.insert(Obstacles.end(), Local->Hazards.begin(), Local->Hazards.end());
	}
}

// Проверяет, находится ли точка внутри известной опасной области.
bool CELSharedDeaths::Contains(u16 OwnerIdentifier, const Fvector& Position)
{
	const ExtendedEntityesDeaths::SLocalKnowledge* Local = ExtendedEntityesDeaths::Find(OwnerIdentifier);
	if (!Local || s32(Device.dwTimeGlobal - Local->FightUntil) < 0)
	{
		return false;
	}
	xr_vector<SELNavigationObstacle>::const_iterator Hazard;
	for (Hazard = Local->Hazards.begin(); Hazard != Local->Hazards.end(); ++Hazard)
	{
		if (Hazard->Position.distance_to_sqr(Position) < Hazard->Radius * Hazard->Radius)
		{
			return true;
		}
	}
	return false;
}

// Проверяет пересечение отрезка с известной опасной областью.
bool CELSharedDeaths::Crosses(u16 OwnerIdentifier, const Fvector& Start, const Fvector& End)
{
	const ExtendedEntityesDeaths::SLocalKnowledge* Local = ExtendedEntityesDeaths::Find(OwnerIdentifier);
	return Local && s32(Device.dwTimeGlobal - Local->FightUntil) >= 0 && CELSharedAnomalies::Crosses(Local->Hazards, Start, End);
}

// Освобождает данные и назначения зарегистрированного владельца.
void CELSharedDeaths::RemoveOwner(u16 OwnerIdentifier)
{
	Release(OwnerIdentifier, false);
	ExtendedEntityesDeaths::Knowledge.erase(OwnerIdentifier);
}

// Удаляет знания и ссылки, связанные с исчезнувшим объектом.
void CELSharedDeaths::RemoveObject(u16 ObjectIdentifier)
{
	ExtendedEntityesDeaths::Deaths.erase(std::remove_if(ExtendedEntityesDeaths::Deaths.begin(), ExtendedEntityesDeaths::Deaths.end(), [ObjectIdentifier](const ExtendedEntityesDeaths::SDeath& Death)
														{ return Death.Identifier == ObjectIdentifier; }),
										 ExtendedEntityesDeaths::Deaths.end());
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedDeaths::Clear()
{
	ExtendedEntityesDeaths::Knowledge.clear();
	ExtendedEntityesDeaths::Deaths.clear();
	ExtendedEntityesDeaths::Claims.clear();
	ExtendedEntityesDeaths::DeathCursor = 0;
}
