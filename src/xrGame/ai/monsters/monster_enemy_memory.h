#pragma once

#include "ai_monster_defs.h"

class CBaseMonster;

class CMonsterEnemyMemory {
	CBaseMonster	*monster;
	TTime			time_memory;

	ENEMIES_MAP		m_objects;
	xr_map<const CEntityAlive*, Fvector> UnreachableIgnoredTargets;
	bool OwnSawActor = false;
	bool ActorSeenBefore = false;
	bool DebugSawActor = false;
	bool DebugDetectedCloseActor = false;
	u32 DebugVisionGeneration = 0;
	const CEntityAlive* SharedPlayer = nullptr;
	bool PlayerFirstKnownThroughSharing = false;

public:
						CMonsterEnemyMemory		();
						~CMonsterEnemyMemory	();

	void				init_external			(CBaseMonster *M, TTime mem_time);
	u32 GetMemoryTime() const;
	void UpdateReachedPoint(const CEntityAlive* Target);
	bool IsActual(const SMonsterEnemy& Entry) const;
	bool IsUnreachable(const CEntityAlive* Target) const;
	void MarkUnreachable(const CEntityAlive* Target);
	void ClearUnreachable(const CEntityAlive* Target);
	bool CanRefreshHiddenTarget(const CEntityAlive* Target);
	u32 GetRemainingMemoryTime(const SMonsterEnemy& Entry) const;
	void RememberHeardEnemy(const CEntityAlive* Target, const Fvector& SoundPosition, u32 SoundTime, float HearingRadius = -1.f);
	bool KnowsPlayer(const CEntityAlive* Player) const;
	bool PlayerWasShared(const CEntityAlive* Player) const { return Player && SharedPlayer == Player; }
	bool PlayerWasFirstShared(const CEntityAlive* Player) const { return PlayerWasShared(Player) && PlayerFirstKnownThroughSharing; }
	void RecordSharedPlayer(const CEntityAlive* Player, bool WasKnown);
	void ClearSharedPlayer() { SharedPlayer = nullptr; PlayerFirstKnownThroughSharing = false; }
	void				update					();

	// -----------------------------------------------------
	const CEntityAlive	*get_enemy				();
	SMonsterEnemy		get_enemy_info			();
	u32					get_enemies_count		() {return (u32)m_objects.size();}

	const ENEMIES_MAP	&get_memory				() {return m_objects;}

	void				clear					() {m_objects.clear(); UnreachableIgnoredTargets.clear(); OwnSawActor = false; ActorSeenBefore = false; DebugSawActor = false; DebugDetectedCloseActor = false; ClearSharedPlayer();}
	void				remove_links			(CObject *O);
	
	void				add_enemy				(const CEntityAlive *enemy);
	void				add_enemy				(const CEntityAlive *enemy, const Fvector &pos, u32 vertex, u32 time);

private:
	void SetRetention(SMonsterEnemy& Entry, const SMonsterEnemy* Previous = nullptr);

	void				remove_non_actual		();

	ENEMIES_MAP_IT		find_best_enemy			();

};

