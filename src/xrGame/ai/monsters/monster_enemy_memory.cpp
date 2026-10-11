#include "StdAfx.h"
#include "../../../xrEngine/MonsterLogicTelemetry.h"
#include "pch_script.h"
#include "monster_enemy_memory.h"
#include "MonsterBehaviorPolicy.h"
#include "basemonster/base_monster.h"
#include "../../memory_manager.h"
#include "../../visual_memory_manager.h"
#include "../../enemy_manager.h"
#include "../../ai_object_location.h"
#include "../../ai_space.h"
#include "../../level_graph.h"
#include "monster_home.h"
#include "dog/dog.h"
#include "ai_monster_squad.h"
#include "ai_monster_squad_manager.h"
#include "../../Actor.h"
#include "../../actor_memory.h"
#include "../../../xrCore/Kernel/EngineExternal.h"
#include "../../MonsterEnemySharingDebug.h"

CMonsterEnemyMemory::CMonsterEnemyMemory()
{
	monster			= 0;
	time_memory		= 15000;
}

CMonsterEnemyMemory::~CMonsterEnemyMemory() = default;

void CMonsterEnemyMemory::init_external(CBaseMonster *M, TTime mem_time)
{
	monster = M;
	time_memory = mem_time;
}

extern CActor*	g_actor;

u32 CMonsterEnemyMemory::GetMemoryTime() const
{
	return EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ?
		monster->EnemyMemoryRetentionMax : time_memory;
}

void CMonsterEnemyMemory::UpdateReachedPoint(const CEntityAlive* Target)
{
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		monster->MonsterPeaceful || monster->EnemyTrackingLive || !Target || Target->getDestroy() || !Target->g_Alive() ||
		monster->GetScriptControl() || monster->EnemyMan.get_script_enemy() || monster->HasDamagePanic() ||
		monster->EnemyMan.see_enemy_now(Target) || monster->ShouldUseCloseEnemyCombat() || monster->HasCloseSightCombat(Target))
	{
		return;
	}
	const auto Entry = m_objects.find(Target);
	if (Entry == m_objects.end() || !IsActual(Entry->second) || Entry->second.ReachedPointTime != 0 || Entry->second.UnreachableSearchUntil != 0 ||
		!ai().level_graph().valid_vertex_id(Entry->second.vertex) || !monster->Home->at_home(Entry->second.position) ||
		!monster->HasReachedEnemyPoint(Entry->second.position, Entry->second.vertex))
	{
		return;
	}
	// Arrival starts one bounded search phase; standing here cannot restart it.
	Entry->second.InspectionCommitted = false;
	Entry->second.ReachedPointTime = Device.dwTimeGlobal ? Device.dwTimeGlobal : 1;
	Entry->second.ForgetTime = Device.dwTimeGlobal + (monster->Behavior.Enabled ? u32(Random.randI(int(monster->Behavior.SearchMin), int(monster->Behavior.SearchMax) + 1)) : monster->EnemySearchAtPointTime);
}

bool CMonsterEnemyMemory::IsUnreachable(const CEntityAlive* Target) const
{
	const auto Entry = m_objects.find(Target);
	return Entry != m_objects.end() && Entry->second.UnreachableSearchUntil != 0;
}

bool CMonsterEnemyMemory::TryNavigationFallback(const CEntityAlive* Target)
{
	const auto Entry = m_objects.find(Target);
	if (Entry == m_objects.end() || Entry->second.NavigationFallback || monster->EnemyMan.see_enemy_now(Target)) return false;
	u32 Vertex = Entry->second.vertex;
	if (!ai().level_graph().valid_vertex_id(Vertex) || !monster->control().path_builder().accessible(Vertex) ||
		!ai().level_graph().inside(Vertex, Entry->second.position))
	{
		Vertex = CControlPathBuilder::find_nearest_vertex(monster->ai_location().level_vertex_id(), Entry->second.position, 10.f);
	}
	if (!ai().level_graph().valid_vertex_id(Vertex) || !monster->control().path_builder().accessible(Vertex)) return false;
	const Fvector Point = ai().level_graph().vertex_position(Vertex);
	if (!monster->Home->at_home(Point) || (Vertex == Entry->second.vertex && Point.distance_to_sqr(Entry->second.position) <= .25f)) return false;
	Entry->second.position = Point;
	Entry->second.vertex = Vertex;
	Entry->second.NavigationFallback = true;
	return true;
}

void CMonsterEnemyMemory::MarkUnreachable(const CEntityAlive* Target)
{
	const auto Entry = m_objects.find(Target);
	if (Entry == m_objects.end() || Entry->second.UnreachableSearchUntil)
	{
		return;
	}
	Entry->second.UnreachableSearchUntil = Device.dwTimeGlobal + (monster->Behavior.Enabled ?
		u32(Random.randI(int(monster->Behavior.SearchMin), int(monster->Behavior.SearchMax) + 1)) : monster->EnemySearchAtPointTime);
	Entry->second.InspectionCommitted = false;
	Entry->second.ForgetTime = Entry->second.UnreachableSearchUntil;
	Entry->second.ReachedPointTime = 0;
}

void CMonsterEnemyMemory::ClearUnreachable(const CEntityAlive* Target)
{
	const auto Entry = m_objects.find(Target);
	if (Entry != m_objects.end())
	{
		Entry->second.UnreachableSearchUntil = 0;
	}
}

void CMonsterEnemyMemory::ResetInspectionForDamage(const CEntityAlive* Target)
{
	UnreachableIgnoredTargets.erase(Target);
	const auto Entry = m_objects.find(Target);
	if (Entry != m_objects.end())
	{
		Entry->second.UnreachableSearchUntil = 0;
		Entry->second.ReachedPointTime = 0;
		Entry->second.NavigationFallback = false;
	}
}

bool CMonsterEnemyMemory::CanRefreshHiddenTarget(const CEntityAlive* Target)
{
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		return true;
	}
	CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal);
	if (monster->EnemyMan.see_enemy_now(Target))
	{
		UnreachableIgnoredTargets.erase(Target);
		return true;
	}
	const auto Ignored = UnreachableIgnoredTargets.find(Target);
	if (Ignored != UnreachableIgnoredTargets.end())
	{
		// Reconsider a genuinely moved nearby target only if there is an open AI corridor to it.
		const u32 CurrentVertex = monster->ai_location().level_vertex_id();
		const u32 TargetVertex = Target->ai_location().level_vertex_id();
		if (Target->Position().distance_to_sqr(Ignored->second) > 4.f && monster->EnemyCloseDetectionRadius > 0.f &&
			monster->Position().distance_to_sqr(Target->Position()) <= _sqr(monster->EnemyCloseDetectionRadius) &&
			ai().level_graph().valid_vertex_id(CurrentVertex) && ai().level_graph().valid_vertex_id(TargetVertex) &&
			monster->HasOpenEnemyCorridor(Target->Position(), TargetVertex))
		{
			UnreachableIgnoredTargets.erase(Ignored);
			return true;
		}
		return false;
	}
	const auto Entry = m_objects.find(Target);
	if (Entry != m_objects.end() && Entry->second.ReachedPointTime)
	{
		return monster->EnemyCloseDetectionRadius > 0.f &&
			monster->Position().distance_to_sqr(Target->Position()) <= _sqr(monster->EnemyCloseDetectionRadius) &&
			Target->Position().distance_to_sqr(Entry->second.position) > 4.f &&
			monster->HasOpenEnemyCorridor(Target->Position(), Target->ai_location().level_vertex_id());
	}
	return !IsUnreachable(Target);
}

bool CMonsterEnemyMemory::IsActual(const SMonsterEnemy& Entry) const
{
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		return CMonsterBehaviorPolicy::InspectionActive(Entry.InspectionCommitted, Entry.ReachedPointTime != 0, Entry.UnreachableSearchUntil != 0) ||
			(Entry.ForgetTime != 0 && s32(Entry.ForgetTime - Device.dwTimeGlobal) > 0) ||
			CMonsterBehaviorPolicy::ApproachActive(Device.dwTimeGlobal, Entry.ApproachUntil, Entry.ApproachProgressTime,
			 monster->EnemyStuckTime, Entry.ReachedPointTime != 0, Entry.UnreachableSearchUntil != 0);
	}
	return Device.dwTimeGlobal - Entry.time < time_memory;
}

u32 CMonsterEnemyMemory::GetRemainingMemoryTime(const SMonsterEnemy& Entry) const
{
	if (!IsActual(Entry))
	{
		return 0;
	}
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		const u32 Now = Device.dwTimeGlobal;
		const u32 MemoryRemaining = CMonsterBehaviorPolicy::Active(Now, Entry.ForgetTime) ? Entry.ForgetTime - Now : 0;
		const u32 ApproachRemaining = CMonsterBehaviorPolicy::ApproachActive(Now, Entry.ApproachUntil, Entry.ApproachProgressTime,
			monster->EnemyStuckTime, Entry.ReachedPointTime != 0, Entry.UnreachableSearchUntil != 0) ? Entry.ApproachUntil - Now : 0;
		return std::max(MemoryRemaining, ApproachRemaining);
	}
	return time_memory - (Device.dwTimeGlobal - Entry.time);
}

void CMonsterEnemyMemory::SetRetention(SMonsterEnemy& Entry, const SMonsterEnemy* Previous, bool FreshVisual)
{
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		return;
	}
	// Origin age is immutable across relays, including live psi position refreshes.
	if (!Entry.OriginTime) Entry.OriginTime = Entry.time;
	// Keep observation age separate from the receiver's own retention clock.
	Entry.RetentionTime = Previous && Previous->RetentionTime ? Previous->RetentionTime :
		u32(Random.randI(int(monster->EnemyMemoryRetentionMin), int(monster->EnemyMemoryRetentionMax) + 1));
	Entry.UnreachableSearchUntil = Previous ? Previous->UnreachableSearchUntil : 0;
	Entry.ForgetTime = Entry.UnreachableSearchUntil && !FreshVisual ? Previous->ForgetTime :
		Device.dwTimeGlobal + (Entry.UnreachableSearchUntil ? monster->EnemySearchAtPointTime : Entry.RetentionTime);
	if (Entry.UnreachableSearchUntil && FreshVisual)
	{
		Entry.UnreachableSearchUntil = Entry.ForgetTime;
	}
	Entry.ReachedPointTime = 0;
	Entry.InspectionCommitted = monster->Behavior.Enabled && !monster->EnemyTrackingLive && !Entry.UnreachableSearchUntil;
	if (Previous)
	{
		Entry.ApproachUntil = Previous->ApproachUntil;
		Entry.ApproachProgressTime = Previous->ApproachProgressTime;
		Entry.OwnVisiblePreviously = Previous->OwnVisiblePreviously;
		Entry.InspectionCommitted = Entry.InspectionCommitted || Previous->InspectionCommitted;
		Entry.Evidence = Previous->Evidence;
		Entry.NavigationFallback = Previous->NavigationFallback && Entry.position.distance_to_sqr(Previous->position) < 4.f;
		std::copy(std::begin(Previous->TrailTimes), std::end(Previous->TrailTimes), Entry.TrailTimes);
		Entry.OwnObservationTime = Previous->OwnObservationTime;
		Entry.TrailCount = Previous->TrailCount;
		Entry.TrailHeading = Previous->TrailHeading;
		Entry.HasTrailHeading = Previous->HasTrailHeading;
		std::copy(std::begin(Previous->Trail), std::end(Previous->Trail), Entry.Trail);
	}
}

void CMonsterEnemyMemory::RememberHeardEnemy(const CEntityAlive* Target, const Fvector& SoundPosition, u32 SoundTime, float HearingRadius)
{
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		!monster->g_Alive() || monster->MonsterPeaceful || !Target || Target == monster ||
		Target->getDestroy() || !monster->EnemyMan.is_enemy(Target) ||
		Target->m_bEntityIgnoredByMonsters || !monster->memory().enemy().is_useful(Target))
	{
		return;
	}

	const float Radius = HearingRadius >= 0.f ? HearingRadius : monster->EnemyCloseDetectionRadius;
	if (Radius <= 0.f || Device.dwTimeGlobal - SoundTime > 2000 ||
		monster->Position().distance_to_sqr(SoundPosition) > Radius * Radius)
	{
		return;
	}

	CMonsterLogicTimerScope NewLogicTimer(EMonsterLogicTimer::NewLogicTotal);
	const auto Entry = m_objects.find(Target);
	if (Entry != m_objects.end() && s32(SoundTime - Entry->second.time) <= 0)
	{
		return;
	}

	u32 SoundVertex = u32(-1);
	if (ai().level_graph().valid_vertex_position(SoundPosition))
	{
		const u32 Candidate = ai().level_graph().vertex_id(SoundPosition);
		if (ai().level_graph().valid_vertex_id(Candidate))
		{
			SoundVertex = Candidate;
		}
	}
	// Both coordinates belong to the sound, never to the moving source.
	add_enemy(Target, SoundPosition, SoundVertex, SoundTime);
	const auto HeardEntry = m_objects.find(Target);
	if (HeardEntry != m_objects.end() && HeardEntry->second.time == SoundTime) HeardEntry->second.Evidence = "SOUND";
	if (smart_cast<const CAI_Dog*>(monster))
	{
		if (CMonsterSquad* Squad = monster_squad().get_squad(monster))
		{
			Squad->set_home_in_danger();
		}
	}
}

void CMonsterEnemyMemory::update()
{
	CMonsterLogicTimerScope MemoryTimer(EMonsterLogicTimer::EnemyMemory);
	if (monster->MonsterPeaceful)
	{
		return;
	}
	VERIFY		(monster->g_Alive());
	if (MonsterEnemySharingDebug().IsEnabled() && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		const u32 Generation = MonsterEnemySharingDebug().GetEpoch();
		if (DebugVisionGeneration != Generation)
		{
			DebugVisionGeneration = Generation;
			DebugSawActor = false;
			DebugDetectedCloseActor = false;
		}
		const bool SeesActor = g_actor && monster->EnemyMan.see_enemy_now(g_actor);
		if (SeesActor != DebugSawActor)
		{
			MonsterEnemySharingDebug().Mark(*monster, true, SeesActor);
		}
		DebugSawActor = SeesActor;
		const bool DetectedCloseActor = g_actor && !monster->EnemyTrackingLive &&
			monster->EnemyCloseDetectionRadius > 0.f && monster->EnemyMan.is_enemy(g_actor) &&
			monster->Position().distance_to_sqr(g_actor->Position()) <=
				monster->EnemyCloseDetectionRadius * monster->EnemyCloseDetectionRadius;
		if (DetectedCloseActor != DebugDetectedCloseActor)
		{
			MonsterEnemySharingDebug().Mark(*monster, false, DetectedCloseActor, true);
		}
		DebugDetectedCloseActor = DetectedCloseActor;
	}
	else
	{
		DebugSawActor = false;
		DebugDetectedCloseActor = false;
	}


	const bool SnapshotTracking = EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] &&
		!monster->EnemyTrackingLive;

	const bool SharingEnabled = EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation];
	const bool SeesActorNow = SharingEnabled && g_actor && monster->EnemyMan.see_enemy_now(g_actor);
	const bool ReacquiredActor = SeesActorNow && !OwnSawActor;
	OwnSawActor = SeesActorNow;
	ActorSeenBefore = SharingEnabled && (ActorSeenBefore || SeesActorNow);

	CMonsterHitMemory& monster_hit_memory = monster->HitMemory;

	typedef CObjectManager<const CEntityAlive>::OBJECTS	objects_list;

	objects_list const& objects	=	monster->memory().enemy().objects();

	if ( monster_hit_memory.is_hit() && time() < monster_hit_memory.get_last_hit_time() + 1000 )
	{
		if (CEntityAlive* enemy = monster->HitMemory.get_last_hit_object() != nullptr ? monster->HitMemory.get_last_hit_object()->cast_entity_alive() : nullptr)
		{
			if ( monster->CCreature::useful(&monster->memory().enemy(), enemy) &&
				 monster->Position().distance_to(enemy->Position())
				                        <
				 monster->get_feel_enemy_who_just_hit_max_distance() )
			{
				if (SnapshotTracking)
				{
					const u32 HitTime = monster_hit_memory.get_last_hit_time();
					const auto Entry = m_objects.find(enemy);
					if (Entry == m_objects.end() || s32(HitTime - Entry->second.time) > 0)
					{
						add_enemy(enemy, enemy->Position(), enemy->ai_location().level_vertex_id(), HitTime);
					}
				}
				else
				{
					add_enemy(enemy);
				}

				bool const self_is_dog	=	!!smart_cast<const CAI_Dog*>(monster);
				if ( self_is_dog )
				{
					CMonsterSquad* const squad	=	monster_squad().get_squad(monster);
					squad->set_home_in_danger	();
				}
			}
		}
	}

	if (!SharingEnabled && monster->SoundMemory.IsRememberSound())
	{
		SoundElem sound;
		bool dangerous;
		monster->SoundMemory.GetSound(sound, dangerous);
		if ( g_actor && dangerous && Device.dwTimeGlobal < sound.time + 2000 )
		{
			CObject* cast_who = const_cast<CObject*>(sound.who);
			if (CEntityAlive const* enemy = cast_who != nullptr ? cast_who->cast_entity_alive() : nullptr)
			{
				const Fvector& SoundPosition = g_actor->Position();
				float const xz_dist = monster->Position().distance_to_xz(SoundPosition);
				float const y_dist = std::abs(monster->Position().y - SoundPosition.y);

				if ( monster->CCreature::useful(&monster->memory().enemy(), enemy) &&
					 y_dist < 10 &&
					 xz_dist < monster->get_feel_enemy_who_made_sound_max_distance() &&
					 g_actor->memory().visual().visible_now(monster))
				{
					add_enemy(enemy);

					bool const self_is_dog	=	!!smart_cast<const CAI_Dog*>(monster);
					if ( self_is_dog )
					{
						CMonsterSquad* const squad	=	monster_squad().get_squad(monster);
						squad->set_home_in_danger	();
					}
				}
			}
		}
	}

	for ( objects_list::const_iterator	I	=	objects.begin();
										I	!=	objects.end();
									  ++I	)
	{
		const CEntityAlive* enemy = *I;
		const bool feel_enemy	  = !SnapshotTracking && monster->Position().distance_to(enemy->Position())
													<
									monster->get_feel_enemy_max_distance();

		const float CloseRadius = monster->EnemyCloseDetectionRadius;
		if (SnapshotTracking ? monster->EnemyMan.see_enemy_now(enemy) :
			(feel_enemy || monster->memory().visual().visible_now(*I)))
		{
			add_enemy(enemy);
		}
		else if (SnapshotTracking && CloseRadius > 0.f && monster->EnemyMan.is_enemy(enemy) &&
			monster->Position().distance_to_sqr(enemy->Position()) <= CloseRadius * CloseRadius)
		{
			add_enemy(enemy, enemy->Position(), enemy->ai_location().level_vertex_id(), Device.dwTimeGlobal);
		}
	}

	if (SnapshotTracking && monster->EnemyCloseDetectionRadius > 0.f)
	{
		const float RadiusSquared = monster->EnemyCloseDetectionRadius * monster->EnemyCloseDetectionRadius;
		for (auto& Entry : m_objects)
		{
			const CEntityAlive* Target = Entry.first;
			if (Target && !Target->getDestroy() && !monster->EnemyMan.see_enemy_now(Target) && monster->EnemyMan.is_enemy(Target) &&
				monster->Position().distance_to_sqr(Target->Position()) <= RadiusSquared && CanRefreshHiddenTarget(Target) && !Entry.second.NavigationFallback)
			{
				Entry.second.position = Target->Position();
				Entry.second.vertex = Target->ai_location().level_vertex_id();
				Entry.second.time = Device.dwTimeGlobal;
				SetRetention(Entry.second, &Entry.second);
				Entry.second.Evidence = "CLOSE";
				Entry.second.OriginTime = Entry.second.time;
			}
		}
	}

	// Discover previously unknown nearby enemies without a visual acquisition requirement.
	// Known targets above still refresh every memory update; discovery shares the spatial budget.
	if (SnapshotTracking && monster->EnemyCloseDetectionRadius > 0.f &&
		s32(Device.dwTimeGlobal - NextCloseScan) >= 0 && SMonsterBehavior::TakeSpatialQuery())
	{
		NextCloseScan = Device.dwTimeGlobal + 250 + u32(monster->ID()) % 101;
		const float Radius = monster->EnemyCloseDetectionRadius;
		xr_vector<ISpatialShared> Candidates;
		g_SpatialSpace->q_sphere(Candidates, 0, ESPATIAL_TYPE::COLLIDEABLE, monster->Position(), Radius);
		for (const auto& Spatial : Candidates)
		{
			CObject* Object = Spatial ? Spatial->dcast_CObject() : nullptr;
			const CEntityAlive* Candidate = Object ? Object->cast_entity_alive() : nullptr;
			if (!Candidate || Candidate == monster || Candidate->getDestroy() || !Candidate->g_Alive() ||
				Candidate->m_bEntityIgnoredByMonsters || !monster->EnemyMan.is_enemy(Candidate) ||
				monster->Position().distance_to_sqr(Candidate->Position()) > Radius * Radius)
			{
				continue;
			}
			add_enemy(Candidate, Candidate->Position(), Candidate->ai_location().level_vertex_id(), Device.dwTimeGlobal);
		}
	}

	float const feel_enemy_max_distance	=	monster->get_feel_enemy_max_distance();
	if (g_actor && !SnapshotTracking)
	{
		float const xz_dist	=	monster->Position().distance_to_xz(g_actor->Position());
		float const y_dist	= std::abs(monster->Position().y - g_actor->Position().y);

		if ( xz_dist < feel_enemy_max_distance &&
			 y_dist < 10 &&
			 monster->memory().enemy().is_useful(g_actor) &&
			 g_actor->memory().visual().visible_now(monster) )
		{
			add_enemy(g_actor);
		}
	}

	// удалить устаревших врагов
	remove_non_actual();

	// обновить опасность
	for (ENEMIES_MAP_IT it = m_objects.begin(); it != m_objects.end(); it++) {
		u8		relation_value = u8(monster->tfGetRelationType(it->first));
		float	dist = monster->Position().distance_to(it->second.position);
		it->second.danger = (1 + relation_value*relation_value*relation_value) / (1 + dist);
	}
	const CEntityAlive* Urgent = ReacquiredActor ? g_actor : nullptr;
	if (SharingEnabled)
	{
		for (auto& Entry : m_objects)
		{
			const bool OwnVisible = monster->EnemyMan.see_enemy_now(Entry.first);
			if (OwnVisible && !Entry.second.OwnVisiblePreviously && !Urgent) Urgent = Entry.first;
			Entry.second.OwnVisiblePreviously = OwnVisible;
		}
	}
	if (Urgent && monster->EnemySharingEnabled) monster->EnemyMan.UpdateEnemySharing(Urgent, true);

}

bool CMonsterEnemyMemory::KnowsPlayer(const CEntityAlive* Player) const
{
	if (!Player || !Player->g_Alive() || Player->getDestroy())
	{
		return false;
	}
	const auto Entry = m_objects.find(Player);
	return Entry != m_objects.end() && IsActual(Entry->second);
}

void CMonsterEnemyMemory::RecordSharedPlayer(const CEntityAlive* Player, bool WasKnown)
{
	if (SharedPlayer != Player || !WasKnown)
	{
		PlayerFirstKnownThroughSharing = !WasKnown;
	}
	SharedPlayer = Player;
}

void CMonsterEnemyMemory::add_enemy(const CEntityAlive *enemy)
{
	if (!CanRefreshHiddenTarget(enemy))
	{
		return;
	}
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] && !monster->EnemyTrackingLive)
	{
		Fvector ObservedPosition;
		u32 ObservationTime = 0;
		if (!monster->EnemyMan.see_enemy_now(enemy) ||
			!monster->GetOwnVisionObservation(enemy, ObservedPosition, ObservationTime))
		{
			return;
		}
		u32 ObservedVertex = u32(-1);
		if (ai().level_graph().valid_vertex_position(ObservedPosition))
		{
			const u32 Candidate = ai().level_graph().vertex_id(ObservedPosition);
			if (ai().level_graph().valid_vertex_id(Candidate))
			{
				ObservedVertex = Candidate;
			}
		}
		add_enemy(enemy, ObservedPosition, ObservedVertex, ObservationTime);
		RecordOwnTrail(enemy);
		return;
	}
	if (enemy->m_bEntityIgnoredByMonsters)
	{
		return;
	}

	if (SharedPlayer == enemy && !KnowsPlayer(enemy))
	{
		ClearSharedPlayer();
	}
	SMonsterEnemy enemy_info;
	enemy_info.position = enemy->Position();
	enemy_info.vertex   = enemy->ai_location().level_vertex_id();
	enemy_info.time		= Device.dwTimeGlobal;
	enemy_info.danger	= 0.f;

	ENEMIES_MAP_IT it = m_objects.find(enemy);
	if (it != m_objects.end()) {
		// обновить данные о враге
		SetRetention(enemy_info, &it->second, monster->EnemyMan.see_enemy_now(enemy));
		it->second = enemy_info;
	} else {
		// добавить врага в список объектов
		SetRetention(enemy_info);
		m_objects.insert(std::make_pair(enemy, enemy_info));
	}
}

void CMonsterEnemyMemory::add_enemy(const CEntityAlive *enemy, const Fvector &pos, u32 vertex, u32 time)
{
	if (!CanRefreshHiddenTarget(enemy))
	{
		return;
	}
	if (enemy->m_bEntityIgnoredByMonsters)
	{
		return;
	}

	if (SharedPlayer == enemy && !KnowsPlayer(enemy))
	{
		ClearSharedPlayer();
	}
	SMonsterEnemy enemy_info;
	enemy_info.position = pos;
	enemy_info.vertex   = vertex;
	enemy_info.time		= time;
	enemy_info.danger	= 0.f;

	ENEMIES_MAP_IT it = m_objects.find(enemy);
	if (it != m_objects.end()) {
		// обновить данные о враге
		if (s32(enemy_info.time - it->second.time) > 0)
		{
			if (it->second.NavigationFallback && !monster->EnemyMan.see_enemy_now(enemy) && pos.distance_to_sqr(it->second.position) < 4.f)
			{
				enemy_info.position = it->second.position;
			}
			SetRetention(enemy_info, &it->second, monster->EnemyMan.see_enemy_now(enemy));
			it->second = enemy_info;
		}
	} else {
		// добавить врага в список объектов
		SetRetention(enemy_info);
		m_objects.insert(std::make_pair(enemy, enemy_info));
	}
}

void CMonsterEnemyMemory::remove_non_actual()
{

	// удалить 'старых' врагов и тех, расстояние до которых > 30м и др.
	for ( ENEMIES_MAP_IT	it	=	m_objects.begin(), nit;
							it	!=	m_objects.end();
							it	=	nit	)
	{
		nit = it; ++nit;
		// проверить условия удаления
		if ( !it->first									||
			 !it->first->g_Alive()						||
			 it->first->getDestroy()					||
			 !IsActual(it->second) ||
			 (it->first->g_Team() == monster->g_Team()) ||
			 (it->first->m_bEntityIgnoredByMonsters)	||
			 (!monster->memory().enemy().is_useful(it->first) &&
				(!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
				 ai().level_graph().valid_vertex_id(it->first->ai_location().level_vertex_id()) ||
				 !ai().level_graph().valid_vertex_id(it->second.vertex) ||
				 !monster->memory().enemy().UsefulForRememberedTarget(it->first))) )
		{
			if (SharedPlayer == it->first)
			{
				ClearSharedPlayer();
			}
			monster->EnemyMan.ResetPlayerVisualAcquisition(it->first);
			if (it->second.UnreachableSearchUntil && it->first && it->first->g_Alive() && !it->first->getDestroy())
			{
				if (UnreachableIgnoredTargets.size() >= 64)
				{
					UnreachableIgnoredTargets.erase(UnreachableIgnoredTargets.begin());
				}
				UnreachableIgnoredTargets[it->first] = it->second.position;
			}
			m_objects.erase (it);
		}
	}
}

const CEntityAlive *CMonsterEnemyMemory::get_enemy()
{
	ENEMIES_MAP_IT	it = find_best_enemy();
	if (it != m_objects.end()) return it->first;
	return (0);
}

SMonsterEnemy CMonsterEnemyMemory::get_enemy_info()
{
	SMonsterEnemy ret_val;
	ret_val.time = 0;

	ENEMIES_MAP_IT	it = find_best_enemy();
	if (it != m_objects.end()) ret_val = it->second;

	return ret_val;
}

ENEMIES_MAP_IT CMonsterEnemyMemory::find_best_enemy()
{
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] &&
		EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterDistributedTargeting])
	{
		CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal);
		const CEntityAlive* Assigned = monster->EnemyMan.GetAssignedEnemy();
		const auto Assignment = Assigned ? m_objects.find(Assigned) : m_objects.end();
		if (Assignment != m_objects.end() && monster->EnemyMan.see_enemy_now(Assigned))
		{
			return Assignment;
		}
		auto Closest = m_objects.end();
		float Distance = 0.f;
		for (auto Entry = m_objects.begin(); Entry != m_objects.end(); ++Entry)
		{
			if (monster->EnemyMan.see_enemy_now(Entry->first))
			{
				const float Range = monster->Position().distance_to_sqr(Entry->second.position);
				if (Closest == m_objects.end() || Range < Distance)
				{
					Closest = Entry;
					Distance = Range;
				}
			}
		}
		if (Closest != m_objects.end())
		{
			return Closest;
		}
		const auto Current = m_objects.find(monster->EnemyMan.get_enemy());
		if (Current != m_objects.end() && Current->second.InspectionCommitted) return Current;
		if (Assignment != m_objects.end())
		{
			return Assignment;
		}
	}
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] && monster->Behavior.Enabled)
	{
		for (auto Entry = m_objects.begin(); Entry != m_objects.end(); ++Entry)
		{
			if (monster->EnemyMan.see_enemy_now(Entry->first)) return Entry;
		}
		const auto Current = m_objects.find(monster->EnemyMan.get_enemy());
		if (Current != m_objects.end() && Current->second.InspectionCommitted) return Current;
	}
	if (monster->Behavior.Enabled && !monster->EnemyTrackingLive && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		auto Pending = m_objects.end();
		for (auto Entry = m_objects.begin(); Entry != m_objects.end(); ++Entry)
		{
			if (Entry->second.InspectionCommitted && monster->Home->at_home(Entry->second.position) &&
				(Pending == m_objects.end() || s32(Entry->second.time - Pending->second.time) < 0)) Pending = Entry;
		}
		if (Pending != m_objects.end()) return Pending;
	}
	ENEMIES_MAP_IT	it = m_objects.end();
	float			max_value = 0.f;

	// find best at home first
	for (ENEMIES_MAP_IT I = m_objects.begin(); I != m_objects.end(); I++) {
		if (!monster->Home->at_home(I->second.position)) continue;
		if (I->second.danger > max_value) {
			max_value = I->second.danger;
			it = I;
		}
	}

	// there is no best enemies at home
	if (it == m_objects.end()) {
		// find any
		max_value = 0.f;
		for (ENEMIES_MAP_IT I = m_objects.begin(); I != m_objects.end(); I++) {
			if (I->second.danger > max_value) {
				max_value = I->second.danger;
				it = I;
			}
		}
	}

	return it;
}

void CMonsterEnemyMemory::remove_links(CObject *O)
{
	for (auto Entry = UnreachableIgnoredTargets.begin(); Entry != UnreachableIgnoredTargets.end();)
	{
		if (Entry->first == O)
		{
			Entry = UnreachableIgnoredTargets.erase(Entry);
		}
		else
		{
			++Entry;
		}
	}
	if (SharedPlayer == O)
	{
		ClearSharedPlayer();
	}
	if ( monster )
	{
		monster->EnemyMan.remove_links(O);
	}

	for (ENEMIES_MAP_IT	I = m_objects.begin();I!=m_objects.end();++I) {
		if ((*I).first == O) {
			m_objects.erase(I);
			break;
		}
	}
}


void CMonsterEnemyMemory::KeepApproach(const CEntityAlive* Target, u32 Until)
{
	const auto Entry = m_objects.find(Target);
	if (Entry != m_objects.end() && !Entry->second.ReachedPointTime && !Entry->second.UnreachableSearchUntil)
	{
		if (!Entry->second.ApproachUntil) Entry->second.ApproachUntil = Until;
		Entry->second.ApproachProgressTime = monster->Behavior.ProgressTime;
	}
}

void CMonsterEnemyMemory::RecordOwnTrail(const CEntityAlive* Target)
{
	const auto Entry = m_objects.find(Target);
	if (Entry == m_objects.end()) return;
	auto& Fact = Entry->second;
	Fact.OwnObservationTime = Fact.time;
	Fact.OriginTime = Fact.time;
	Fact.Evidence = "VISUAL";
	Fact.NavigationFallback = false;
	Fact.ApproachUntil = 0;
	Fact.ApproachProgressTime = 0;
	if (monster->EnemyTrackingLive || !monster->Behavior.TrailEnabled) { Fact.TrailCount = 0; Fact.HasTrailHeading = false; return; }
	if (Fact.TrailCount && Fact.Trail[Fact.TrailCount - 1].distance_to_sqr(Fact.position) < .25f) return;
	if (Fact.TrailCount)
	{
		Fact.TrailHeading.sub(Fact.position, Fact.Trail[Fact.TrailCount - 1]);
		Fact.TrailHeading.y = 0.f;
		Fact.HasTrailHeading = Fact.TrailHeading.square_magnitude() > EPS_L * EPS_L;
		if (Fact.HasTrailHeading) Fact.TrailHeading.normalize();
	}
	if (Fact.TrailCount == 5)
	{
		for (u32 Index = 1; Index < 5; ++Index) { Fact.Trail[Index - 1] = Fact.Trail[Index]; Fact.TrailTimes[Index - 1] = Fact.TrailTimes[Index]; }
		--Fact.TrailCount;
	}
	Fact.TrailTimes[Fact.TrailCount] = Fact.time;
	Fact.Trail[Fact.TrailCount++] = Fact.position;
	while (Fact.TrailCount > 1 && Fact.Trail[0].distance_to_sqr(Fact.position) > _sqr(monster->Behavior.TrailLength))
	{
		for (u32 Index = 1; Index < Fact.TrailCount; ++Index) { Fact.Trail[Index - 1] = Fact.Trail[Index]; Fact.TrailTimes[Index - 1] = Fact.TrailTimes[Index]; }
		--Fact.TrailCount;
	}
}

void CMonsterEnemyMemory::ReceiveTrail(const CEntityAlive* Target, const SMonsterEnemy& Source, bool SourceLive)
{
	const auto Entry = m_objects.find(Target);
	if (Entry == m_objects.end()) return;
	const u32 SourceOrigin = Source.OriginTime ? Source.OriginTime : Source.time;
	if (Entry->second.OwnObservationTime && s32(Entry->second.OwnObservationTime - SourceOrigin) >= 0) return;
	Entry->second.HasTrailHeading = !SourceLive && !monster->EnemyTrackingLive && monster->Behavior.TrailEnabled && Source.HasTrailHeading;
	Entry->second.TrailHeading = Source.TrailHeading;
	Entry->second.TrailCount = SourceLive || monster->EnemyTrackingLive || !monster->Behavior.TrailEnabled ? 0 : std::min(Source.TrailCount, 5u);
	std::copy(std::begin(Source.Trail), std::end(Source.Trail), Entry->second.Trail);
	std::copy(std::begin(Source.TrailTimes), std::end(Source.TrailTimes), Entry->second.TrailTimes);
	Entry->second.OriginTime = Source.OriginTime ? Source.OriginTime : Source.time;
	const u32 OriginUntil = Entry->second.OriginTime + monster->EnemyMemory.GetMemoryTime();
	if (s32(Entry->second.ForgetTime - OriginUntil) > 0) Entry->second.ForgetTime = OriginUntil;
	Entry->second.Evidence = SourceLive ? "PSI RADIO" : "RADIO";
}

void CMonsterEnemyMemory::CommitInspection(const CEntityAlive* Target)
{
	if (!monster->Behavior.Enabled || monster->EnemyTrackingLive || !EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation]) return;
	const bool Combat = Target && monster->EnemyMan.see_enemy_now(Target);
	const auto Entry = m_objects.find(Target);
	if (Entry != m_objects.end() && !Combat && !Entry->second.ReachedPointTime && !Entry->second.UnreachableSearchUntil)
	{
		Entry->second.InspectionCommitted = true;
	}
}
