#include "StdAfx.h"
#include "../../../xrEngine/MonsterLogicTelemetry.h"
#include "pch_script.h"
#include "monster_enemy_memory.h"
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
		monster->EnemyMan.see_enemy_now(Target) || monster->ShouldApproachHiddenEnemy() || monster->ShouldUseCloseEnemyCombat())
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
	Entry->second.ReachedPointTime = Device.dwTimeGlobal ? Device.dwTimeGlobal : 1;
	Entry->second.ForgetTime = Device.dwTimeGlobal + monster->EnemySearchAtPointTime;
}

bool CMonsterEnemyMemory::IsUnreachable(const CEntityAlive* Target) const
{
	const auto Entry = m_objects.find(Target);
	return Entry != m_objects.end() && Entry->second.UnreachableSearchUntil != 0;
}

void CMonsterEnemyMemory::MarkUnreachable(const CEntityAlive* Target)
{
	const auto Entry = m_objects.find(Target);
	if (Entry == m_objects.end() || Entry->second.UnreachableSearchUntil)
	{
		return;
	}
	Entry->second.UnreachableSearchUntil = Device.dwTimeGlobal + monster->EnemySearchAtPointTime;
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
	return !IsUnreachable(Target);
}

bool CMonsterEnemyMemory::IsActual(const SMonsterEnemy& Entry) const
{
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		return Entry.ForgetTime != 0 && s32(Entry.ForgetTime - Device.dwTimeGlobal) > 0;
	}
	return Device.dwTimeGlobal - Entry.time < time_memory;
}

u32 CMonsterEnemyMemory::GetRemainingMemoryTime(const SMonsterEnemy& Entry) const
{
	if (!IsActual(Entry))
	{
		return 0;
	}
	return EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ?
		Entry.ForgetTime - Device.dwTimeGlobal : time_memory - (Device.dwTimeGlobal - Entry.time);
}

void CMonsterEnemyMemory::SetRetention(SMonsterEnemy& Entry, const SMonsterEnemy* Previous)
{
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		return;
	}
	// Keep observation age separate from the receiver's own retention clock.
	Entry.RetentionTime = Previous && Previous->RetentionTime ? Previous->RetentionTime :
		u32(Random.randI(int(monster->EnemyMemoryRetentionMin), int(monster->EnemyMemoryRetentionMax) + 1));
	Entry.UnreachableSearchUntil = Previous ? Previous->UnreachableSearchUntil : 0;
	Entry.ForgetTime = Device.dwTimeGlobal + (Entry.UnreachableSearchUntil ? monster->EnemySearchAtPointTime : Entry.RetentionTime);
	if (Entry.UnreachableSearchUntil)
	{
		Entry.UnreachableSearchUntil = Entry.ForgetTime;
	}
	Entry.ReachedPointTime = 0;
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
	const bool ReacquiredActor = SeesActorNow && !OwnSawActor && (ActorSeenBefore || KnowsPlayer(g_actor));
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
		if (SnapshotTracking && CloseRadius > 0.f && monster->EnemyMan.is_enemy(enemy) &&
			monster->Position().distance_to_sqr(enemy->Position()) <= CloseRadius * CloseRadius)
		{
			add_enemy(enemy, enemy->Position(), enemy->ai_location().level_vertex_id(), Device.dwTimeGlobal);
		}
		else if (SnapshotTracking ? monster->EnemyMan.see_enemy_now(enemy) :
			(feel_enemy || monster->memory().visual().visible_now(*I)))
		{
			add_enemy(*I);
		}
	}

	if (SnapshotTracking && monster->EnemyCloseDetectionRadius > 0.f)
	{
		const float RadiusSquared = monster->EnemyCloseDetectionRadius * monster->EnemyCloseDetectionRadius;
		for (auto& Entry : m_objects)
		{
			const CEntityAlive* Target = Entry.first;
			if (Target && !Target->getDestroy() && monster->EnemyMan.is_enemy(Target) &&
				monster->Position().distance_to_sqr(Target->Position()) <= RadiusSquared && CanRefreshHiddenTarget(Target))
			{
				Entry.second.position = Target->Position();
				Entry.second.vertex = Target->ai_location().level_vertex_id();
				Entry.second.time = Device.dwTimeGlobal;
				SetRetention(Entry.second, &Entry.second);
			}
		}
	}

	if (SnapshotTracking && g_actor && monster->EnemyCloseDetectionRadius > 0.f &&
		monster->EnemyMan.is_enemy(g_actor) &&
		monster->Position().distance_to_sqr(g_actor->Position()) <=
			monster->EnemyCloseDetectionRadius * monster->EnemyCloseDetectionRadius)
	{
		add_enemy(g_actor, g_actor->Position(), g_actor->ai_location().level_vertex_id(), Device.dwTimeGlobal);
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
	if (ReacquiredActor && monster->EnemySharingEnabled)
	{
		monster->EnemyMan.UpdateEnemySharing(g_actor, true);
	}

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
		SetRetention(enemy_info, &it->second);
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
			SetRetention(enemy_info, &it->second);
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
			 !monster->memory().enemy().is_useful(it->first) ) 
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
		if (Assignment != m_objects.end())
		{
			return Assignment;
		}
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

