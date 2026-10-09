#include "StdAfx.h"
#include "../../../xrEngine/MonsterLogicTelemetry.h"
#include "monster_enemy_manager.h"
#include "basemonster/base_monster.h"
#include "../ai_monsters_misc.h"
#include "../../ai_object_location.h"
#include "../../memory_manager.h"
#include "../../visual_memory_manager.h"
#include "../../Actor.h"
#include "../../actor_memory.h"
#include "../../../xrCore/Kernel/EngineExternal.h"
#include "../../../xrCore/Collision/xr_area.h"
#include "MonsterTargetAllocation.h"
#include "MonsterEnemySharingBudget.h"
#include "../../MonsterEnemySharingDebug.h"

namespace
{
	u32 EnemySharingDelay()
	{
		return u32(Random.randI(int(EngineExternal().GetMonsterEnemySharingIntervalMin()),
			int(EngineExternal().GetMonsterEnemySharingIntervalMax()) + 1));
	}

	CMonsterEnemySharingBudget& EnemySharingBudget()
	{
		static CMonsterEnemySharingBudget Budget;
		Budget.BeginFrame(Device.dwFrame);
		return Budget;
	}
}

#include "controlled_entity.h"
#include "../../sound_player.h"

CMonsterEnemyManager::CMonsterEnemyManager()
{
	monster							= 0;
	enemy							= 0;
	flags.zero						();
	forced							= false;
	prev_enemy						= 0;
	danger_type						= eNone;
	my_vertex_enemy_last_seen		= u32(-1);
	enemy_vertex_enemy_last_seen	= u32(-1);
	m_time_updated					= 0;
	m_time_start_see_enemy			= 0;
}

CMonsterEnemyManager::~CMonsterEnemyManager() = default;
void CMonsterEnemyManager::init_external(CBaseMonster *M)
{
	monster = M;
}


void CMonsterEnemyManager::update()
{
	CMonsterLogicTimerScope ManagerTimer(EMonsterLogicTimer::EnemyManager);
	if (monster->MonsterPeaceful)
	{
		enemy = nullptr;
		return;
	}
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		UpdateEnemySharing();
	}

	if (m_script_enemy && (m_script_enemy->getDestroy() || !m_script_enemy->g_Alive()))
	{
		script_enemy();
	}
	if (forced) {
		// проверить валидность force-объекта
		if (!enemy || enemy->getDestroy() || !enemy->g_Alive()) {
			enemy = 0;
			return;
		}
	} else {
		if (m_script_enemy ){
			enemy = m_script_enemy;
		}else if (HasDamageFocus(DamageFocus)) {
			enemy = DamageFocus;
		}else{
			enemy = monster->EnemyMemory.get_enemy();
		}
		
		if (enemy) {
			SMonsterEnemy enemy_info = monster->EnemyMemory.get_enemy_info();
			if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
			{
				const auto& Memory = monster->EnemyMemory.get_memory();
				const auto Entry = Memory.find(enemy);
				if (Entry != Memory.end())
				{
					enemy_info = Entry->second;
				}
			}
			position					= enemy_info.position;
			vertex						= enemy_info.vertex;
			time_last_seen				= enemy_info.time;
		}
	}
	
	if (!enemy) {
		return;
	}
	
	// обновить информацию о враге в соответствии со звуковой информацией
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] &&
		monster->SoundMemory.IsRememberSound()) {
		SoundElem	sound_elem;		
		if (monster->SoundMemory.get_sound_from_object	(enemy, sound_elem)) {
			if (sound_elem.time > time_last_seen) {
				position		= sound_elem.position;
				vertex			= u32(-1);
				time_last_seen	= sound_elem.time;
			}
		}
	}

	if (monster->EnemyTrackingLive && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		position = enemy->Position();
		vertex = enemy->ai_location().level_vertex_id();
	}

	// проверить видимость
	enemy_see_me = is_faced(enemy, monster);
	
	// обновить опасность врага
	danger_type = eNone;

	switch (dwfChooseAction(0, monster->panic_threshold(), 0.f, 0.f, 0.f, monster->g_Team(),monster->g_Squad(),monster->g_Group(),0,1,2,3,4, monster, 30.f)) {
		case 4 : 	
		case 3 : 	
		case 2 : 	
		case 1 :	danger_type = eStrong;		break;
		case 0 : 	danger_type = eWeak;		break;
	}

	// обновить флаги
	flags.zero();

	if ((prev_enemy == enemy) && (time_last_seen != Device.dwTimeGlobal))	flags.bor(FLAG_ENEMY_LOST_SIGHT);		
	if (prev_enemy && !prev_enemy->g_Alive())									flags.bor(FLAG_ENEMY_DIE);
	if (!enemy_see_me)															flags.bor(FLAG_ENEMY_DOESNT_SEE_ME);
	
	float dist_now, dist_prev;
	if (prev_enemy == enemy) {
		dist_now	= position.distance_to(monster->Position());
		dist_prev	= prev_enemy_position.distance_to(monster->Position());

		if (std::abs(dist_now - dist_prev) < 0.2f)								flags.bor(FLAG_ENEMY_STANDING);
		else {
			if (dist_now < dist_prev)										flags.bor(FLAG_ENEMY_GO_CLOSER);
			else															flags.bor(FLAG_ENEMY_GO_FARTHER);

			if (std::abs(dist_now - dist_prev) < 1.2f) {
				if (dist_now < dist_prev)									flags.bor(FLAG_ENEMY_GO_CLOSER_FAST);
				else														flags.bor(FLAG_ENEMY_GO_FARTHER_FAST);
			}
		}

		if (flags.is(FLAG_ENEMY_STANDING) && flags.is(FLAG_ENEMY_DOESNT_SEE_ME)) flags.bor(FLAG_ENEMY_DOESNT_KNOW_ABOUT_ME);
	} else flags.bor(FLAG_ENEMY_STATS_NOT_READY);

	// сохранить текущего врага
	prev_enemy			= enemy;
	prev_enemy_position = position;

	expediency			= true;

	if (enemy && see_enemy_now()) {
		my_vertex_enemy_last_seen		= monster->ai_location().level_vertex_id();
		enemy_vertex_enemy_last_seen	= enemy->ai_location().level_vertex_id();

		if (m_time_start_see_enemy == 0) m_time_start_see_enemy = time();
	} else m_time_start_see_enemy = 0;
	
	m_time_updated			= time();
}



void CMonsterEnemyManager::force_enemy (const CEntityAlive *enemy_)
{
	this->enemy		= enemy_;
	position		= enemy_->Position();
	vertex			= enemy_->ai_location().level_vertex_id();
	time_last_seen	= time();

	forced			= true;

	update			();
}

void CMonsterEnemyManager::unforce_enemy()
{
	enemy	= monster->EnemyMemory.get_enemy();

	if (enemy) {
		SMonsterEnemy enemy_info	= monster->EnemyMemory.get_enemy_info();
		position					= enemy_info.position;
		vertex						= enemy_info.vertex;
		time_last_seen				= enemy_info.time;
	}

	forced	= false;
	
	update	();
}


u32	CMonsterEnemyManager::get_enemies_count()
{
	return monster->EnemyMemory.get_enemies_count();
}

void CMonsterEnemyManager::reinit()
{
	ResetEnemySharingState();
	enemy						= 0;
	time_last_seen				= 0;
	flags.zero					();
	forced						= false;
	prev_enemy					= 0;
	danger_type					= eNone;

	my_vertex_enemy_last_seen		= monster->ai_location().level_vertex_id();
	enemy_vertex_enemy_last_seen	= u32(-1);

	m_time_updated				= 0;
	m_time_start_see_enemy		= 0;

	script_enemy				();
}


void CMonsterEnemyManager::add_enemy(const CEntityAlive *enemy_)
{
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] &&
		!see_enemy_now(enemy_))
	{
		return;
	}
	monster->EnemyMemory.add_enemy(enemy_);
}


bool CMonsterEnemyManager::see_enemy_now()
{
	return see_enemy_now(enemy);
}

bool CMonsterEnemyManager::see_enemy_now(const CEntityAlive* enemy_)
{
	if (enemy_ && smart_cast<const CActor*>(enemy_) && !PlayerVisualAcquired && PlayerExposureLastVisibleTime &&
		Device.dwTimeGlobal - PlayerExposureLastVisibleTime > monster->EnemyMemoryRetentionMax)
	{
		ResetPlayerVisualAcquisition(enemy_);
	}
	if (!monster->memory().visual().visible_right_now(enemy_))
	{
		return false;
	}
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		return true;
	}
	CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal);
	if (OwnVisionFrame != Device.dwFrame)
	{
		monster->GetOwnVisibleObjects(OwnVisibleObjects);
		std::sort(OwnVisibleObjects.begin(), OwnVisibleObjects.end(), std::less<const CObject*>{});
		OwnVisionFrame = Device.dwFrame;
	}
	const bool OwnVisible = std::binary_search(OwnVisibleObjects.begin(), OwnVisibleObjects.end(),
		static_cast<const CObject*>(enemy_), std::less<const CObject*>{});
	if (!OwnVisible || !smart_cast<const CActor*>(enemy_) || monster->PlayerVisualAcquireTime == 0)
	{
		return OwnVisible;
	}
	PlayerExposureLastVisibleTime = Device.dwTimeGlobal;
	if (PlayerVisualAcquired && PlayerExposureTargetID == enemy_->ID())
	{
		return true;
	}
	float Exposure = 0.f;
	if (!monster->GetOwnVisionExposure(enemy_, Exposure))
	{
		return false;
	}
	if (PlayerExposureTargetID != enemy_->ID())
	{
		ResetPlayerVisualAcquisition();
		PlayerExposureTargetID = enemy_->ID();
		PlayerExposureConsumed = Exposure;
	}
	if (Exposure < PlayerExposureConsumed)
	{
		PlayerExposureConsumed = 0.f;
	}
	PlayerExposureLastVisibleTime = Device.dwTimeGlobal;
	PlayerExposureAccumulated += Exposure - PlayerExposureConsumed;
	PlayerExposureConsumed = Exposure;
	PlayerVisualAcquired = PlayerVisualAcquired || PlayerExposureAccumulated >= float(monster->PlayerVisualAcquireTime);
	return PlayerVisualAcquired;
}

void CMonsterEnemyManager::ResetPlayerVisualAcquisition(const CEntityAlive* Target)
{
	if (Target && !smart_cast<const CActor*>(Target))
	{
		return;
	}
	PlayerExposureTargetID = Target ? Target->ID() : u32(-1);
	PlayerExposureConsumed = 0.f;
	PlayerExposureAccumulated = 0.f;
	PlayerVisualAcquired = false;
	PlayerExposureLastVisibleTime = 0;
	if (Target)
	{
		monster->GetOwnVisionExposure(Target, PlayerExposureConsumed);
	}
}

bool CMonsterEnemyManager::see_enemy_recently()
{
	return see_enemy_recently(enemy); 
}

bool CMonsterEnemyManager::see_enemy_recently(const CEntityAlive* enemy_)
{
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] && smart_cast<const CActor*>(enemy_) && !PlayerVisualAcquired)
	{
		return false;
	}
	return monster->memory().visual().visible_now(enemy_); 
}

bool CMonsterEnemyManager::enemy_see_me_now()
{
	CEntityAlive* cast_enemy = const_cast<CEntityAlive*>(enemy);
	const CActor* actor = cast_enemy != nullptr ? cast_enemy->cast_actor() : nullptr;
	if (actor) 
	{
		return (Level().CurrentViewEntity());
	}
	else 
	{
		CCreature *cm = const_cast<CEntityAlive*>(enemy)->cast_creature();
		if ( cm ) 
		{
			return cm->memory().visual().visible_right_now(monster);
		}
	}

	return false; 
}

bool CMonsterEnemyManager::is_faced(const CEntityAlive *object0, const CEntityAlive *object1)
{
	if (object0->Position().distance_to(object1->Position()) > object0->ffGetRange())
	{
		return false;
	}

	float			yaw1, pitch1, yaw2, pitch2, fYawFov, fPitchFov;
	Fvector			tPosition = object0->Position();

	yaw1			= object0->Orientation().yaw;
	pitch1			= object0->Orientation().pitch;
	fYawFov			= angle_normalize_signed(object0->ffGetFov()*PI/180.f);

	fYawFov			= angle_normalize_signed((std::abs(fYawFov) + std::abs(atanf(1.f/tPosition.distance_to(object1->Position()))))/2.f);
	fPitchFov		= angle_normalize_signed(fYawFov*1.f);
	tPosition.sub	(object1->Position());
	tPosition.mul	(-1);
	tPosition.getHP	(yaw2,pitch2);
	yaw1			= angle_normalize_signed(yaw1);
	pitch1			= angle_normalize_signed(pitch1);
	yaw2			= angle_normalize_signed(yaw2);
	pitch2			= angle_normalize_signed(pitch2);
	if ((angle_difference(yaw1,yaw2) <= fYawFov) && (angle_difference(pitch1,pitch2) <= fPitchFov))
		return		(true);
	return			(false);
}

bool CMonsterEnemyManager::is_enemy(const CEntityAlive *obj) 
{
	if (monster->MonsterPeaceful)
	{
		return false;
	}
	return ((monster->g_Team() != obj->g_Team()) && monster->is_relation_enemy(obj) && obj->g_Alive());
}

const Fvector&   CMonsterEnemyManager::get_enemy_position () 
{
	if (enemy && monster->EnemyTrackingLive &&
		EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		return enemy->Position();
	}
	return position;
}

const Fvector& CMonsterEnemyManager::GetTrackingPosition()
{
	if (enemy && (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		monster->EnemyTrackingLive || monster->HasCloseSightCombat(enemy)))
	{
		return enemy->Position();
	}
	return position;
}

const Fvector& CMonsterEnemyManager::GetKnownEnemyPosition(const CEntityAlive* Target)
{
	if (Target == enemy)
	{
		return GetTrackingPosition();
	}
	if (Target && (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		monster->EnemyTrackingLive))
	{
		return Target->Position();
	}
	const auto& Memory = monster->EnemyMemory.get_memory();
	const auto Entry = Memory.find(Target);
	if (Entry != Memory.end() && monster->EnemyMemory.IsActual(Entry->second))
	{
		return Entry->second.position;
	}
	return monster->Position();
}

u32 CMonsterEnemyManager::GetTrackingVertex()
{
	if (enemy && (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		monster->EnemyTrackingLive || monster->HasCloseSightCombat(enemy)))
	{
		return enemy->ai_location().level_vertex_id();
	}
	return vertex != u32(-1) ? vertex : monster->ai_location().level_vertex_id();
}

bool CMonsterEnemyManager::CanHearEnemySharing(CBaseMonster* Source)
{
	if (!Source || Source->EnemySharingRadius <= 0.f)
	{
		return false;
	}
	const float DistanceSquared = Source->Position().distance_to_sqr(monster->Position());
	const float CloseRadius = Source->EnemySharingRadius * Source->EnemySharingCloseRatio;
	return DistanceSquared <= Source->EnemySharingRadius * Source->EnemySharingRadius &&
		(Source->EnemySharingWireless || DistanceSquared <= CloseRadius * CloseRadius || see_enemy_now(Source));
}

void CMonsterEnemyManager::FocusDamageAttacker(const CEntityAlive* Attacker)
{
	if (!Attacker || Attacker->getDestroy() || !Attacker->g_Alive() || monster->MonsterPeaceful)
	{
		return;
	}
	DamageFocus = Attacker;
	DamageFocusUntil = Device.dwTimeGlobal + monster->EnemyMemory.GetMemoryTime();
	monster->EnemyMemory.add_enemy(Attacker, Attacker->Position(),
		Attacker->ai_location().level_vertex_id(), Device.dwTimeGlobal);
}

bool CMonsterEnemyManager::HasDamageFocus(const CEntityAlive* Target) const
{
	if (!Target || Target != DamageFocus || s32(DamageFocusUntil - Device.dwTimeGlobal) <= 0 ||
		!Target->g_Alive() || Target->getDestroy())
	{
		return false;
	}
	const auto& Memory = monster->EnemyMemory.get_memory();
	const auto Entry = Memory.find(Target);
	return Entry != Memory.end() && monster->EnemyMemory.IsActual(Entry->second);
}

bool CMonsterEnemyManager::CanAcceptEnemySharing(bool Immediate) const
{
	return !monster->MonsterPeaceful && !monster->m_skip_transfer_enemy && monster->g_Alive() &&
		!monster->getDestroy() && (Immediate || s32(Device.dwTimeGlobal - NextSharingReceive) >= 0);
}

bool CMonsterEnemyManager::CanReceiveFrom(CBaseMonster* Source) const
{
	CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal,
		EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation]);
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		!monster->g_Alive() || monster->getDestroy() || monster->MonsterPeaceful || !Source || Source->MonsterPeaceful || !Source->EnemySharingEnabled || Source == monster || !Source->g_Alive() || Source->getDestroy() ||
		monster->is_relation_enemy(Source) || Source->is_relation_enemy(monster))
	{
		return false;
	}
	const bool SamePack = monster->g_Team() == Source->g_Team() &&
		monster->g_Squad() == Source->g_Squad() && monster->g_Group() == Source->g_Group();
	if (SamePack)
	{
		return true;
	}
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFriendlyEnemySharing])
	{
		return false;
	}
	const bool MutualFriends = monster->tfGetRelationType(Source) == ALife::eRelationTypeFriend &&
		Source->tfGetRelationType(monster) == ALife::eRelationTypeFriend;
	return MutualFriends;
}

bool CMonsterEnemyManager::HasDirectEnemyEvidence(const CEntityAlive* Target)
{
	if (!Target || Target->getDestroy() || !Target->g_Alive())
	{
		return false;
	}
	if (monster->EnemyMemory.IsUnreachable(Target) && !see_enemy_now(Target))
	{
		return false;
	}
	if (monster->EnemyTrackingLive && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		const auto& Memory = monster->EnemyMemory.get_memory();
		const auto Entry = Memory.find(Target);
		if (Entry != Memory.end() && monster->EnemyMemory.IsActual(Entry->second) &&
			Device.dwTimeGlobal - Entry->second.time < monster->EnemyMemory.GetMemoryTime())
		{
			return true;
		}
	}
	if (monster->EnemyCloseDetectionRadius > 0.f &&
		EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] &&
		monster->Position().distance_to_sqr(Target->Position()) <=
			monster->EnemyCloseDetectionRadius * monster->EnemyCloseDetectionRadius)
	{
		return true;
	}
	if (see_enemy_now(Target))
	{
		return true;
	}
	if (monster->HitMemory.is_hit() && monster->HitMemory.get_last_hit_object() == Target &&
		Device.dwTimeGlobal - monster->HitMemory.get_last_hit_time() < 1000)
	{
		return true;
	}
	SoundElem Sound;
	return monster->SoundMemory.get_sound_from_object(Target, Sound) && Device.dwTimeGlobal - Sound.time < 2000;
}

bool CMonsterEnemyManager::ReceiveSharedEnemy(CBaseMonster* Source, const CEntityAlive* Target, bool ValidatedSource, bool Immediate)
{
	CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal,
		EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation]);
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		!CanAcceptEnemySharing(Immediate) || !Source || !Target || monster->m_skip_transfer_enemy || monster->MonsterPeaceful || !monster->g_Alive() || monster->getDestroy() ||
		Source->MonsterPeaceful || !Source->EnemySharingEnabled || !Source->g_Alive() || Source->getDestroy() ||
		(!ValidatedSource && !CanReceiveFrom(Source)))
	{
		return false;
	}
	CMonsterEnemySharingBudget& Budget = EnemySharingBudget();
	if (!Budget.TakeRecord())
	{
		g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::SharingDeferred);
		return false;
	}
	g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::SharingChecked);
	const u32 Now = Device.dwTimeGlobal;
	const auto& SourceMemory = Source->EnemyMemory.get_memory();
	const auto Entry = SourceMemory.find(Target);
	if (Entry == SourceMemory.end() || !Source->EnemyMemory.IsActual(Entry->second) ||
		Now - Entry->second.time >= Source->EnemyMemory.GetMemoryTime() ||
		Now - Entry->second.time >= monster->EnemyMemory.GetMemoryTime())
	{
		return false;
	}
	const Fvector SharedPosition = Source->EnemyTrackingLive ? Target->Position() : Entry->second.position;
	const u32 SharedVertex = Source->EnemyTrackingLive ? Target->ai_location().level_vertex_id() : Entry->second.vertex;
	const auto& OwnMemory = monster->EnemyMemory.get_memory();
	const auto Own = OwnMemory.find(Target);
	const bool WasKnown = Own != OwnMemory.end() && monster->EnemyMemory.IsActual(Own->second);
	if ((Immediate && WasKnown && s32(Entry->second.time - Own->second.time) <= 0) ||
		(!Immediate && !CMonsterEnemySharingBudget::NeedsRefresh(Own != OwnMemory.end(), Now, Entry->second.time,
		Own != OwnMemory.end() ? Own->second.time : 0, monster->EnemyMemory.GetMemoryTime(),
		Own != OwnMemory.end() ? Own->second.position.distance_to_sqr(SharedPosition) : 0.f)))
	{
		g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::SharingUnchanged);
		return false;
	}
	if (Target->getDestroy() || Target->m_bEntityIgnoredByMonsters || !is_enemy(Target))
	{
		return false;
	}
	if (!CanHearEnemySharing(Source) ||
		!Source->EnemyMan.HasDirectEnemyEvidence(Target))
	{
		return false;
	}
	if (!monster->EnemyMemory.CanRefreshHiddenTarget(Target))
	{
		return false;
	}
	if (!Budget.TakeTransfer())
	{
		g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::SharingDeferred);
		return false;
	}
	monster->EnemyMemory.add_enemy(Target, SharedPosition, SharedVertex, Entry->second.time);
	g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::Transfers);
	g_MonsterLogicTelemetry.Count(Source->EnemySharingWireless ? EMonsterLogicCounter::WirelessTransfers : EMonsterLogicCounter::VisibleTransfers);
	if (smart_cast<const CActor*>(Target))
	{
		monster->EnemyMemory.RecordSharedPlayer(Target, WasKnown);
		g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::ActorTransfers);
		MonsterEnemySharingDebug().Record(*Source, *monster, *Target, SharedPosition, Entry->second.time);
	}
	if (!ValidatedSource)
	{
		FinishEnemySharing(Source, xr_vector<const CEntityAlive*>{Target});
	}
	return true;
}

bool CMonsterEnemyManager::EmitEnemyCall(const xr_vector<const CEntityAlive*>& Targets)
{
	CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal,
		EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation]);
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		!monster->g_Alive() || monster->getDestroy() || !monster->EnemyCallEnabled || !monster->EnemySharingEnabled || monster->MonsterPeaceful || Targets.empty() ||
		!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterEnemySharingCalls])
	{
		return false;
	}
	const bool NewTarget = std::any_of(Targets.begin(), Targets.end(), [&](const CEntityAlive* Target)
	{
		return std::find(CalledEnemyIds.begin(), CalledEnemyIds.end(), Target->ID()) == CalledEnemyIds.end();
	});
	if (!NewTarget || s32(Device.dwTimeGlobal - NextCallSoundTime) < 0)
	{
		return false;
	}
	monster->set_state_sound(monster->EnemyCallSoundType, true);
	g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::Calls);
	if (std::any_of(Targets.begin(), Targets.end(), [](const CEntityAlive* Target)
	{
		return smart_cast<const CActor*>(Target) != nullptr;
	}))
	{
		MonsterEnemySharingDebug().Mark(*monster, false);
	}
	NextCallSoundTime = Device.dwTimeGlobal + u32(monster->EnemyCallCooldown * 1000.f);
	CalledEnemyIds.clear();
	for (const CEntityAlive* Target : Targets)
	{
		CalledEnemyIds.push_back(Target->ID());
	}
	return true;
}

bool CMonsterEnemyManager::ReceiveKnownEnemies(CBaseMonster* Source)
{
	if (!CanAcceptEnemySharing() || !Source || Source->EnemyMemory.get_memory().empty() ||
		!CanHearEnemySharing(Source) || !CanReceiveFrom(Source))
	{
		return false;
	}
	CMonsterEnemySharingBudget& Budget = EnemySharingBudget();
	const auto& Memory = Source->EnemyMemory.get_memory();
	if (Memory.empty() || !Budget.RecordsRemaining)
	{
		return false;
	}
	xr_vector<const CEntityAlive*> Accepted;
	Accepted.reserve(CMonsterEnemySharingBudget::MaxRecordsPerSource);
	auto Entry = SharingTargetCursor ? Memory.upper_bound(SharingTargetCursor) : Memory.begin();
	if (Entry == Memory.end())
	{
		Entry = Memory.begin();
	}
	const u32 Limit = std::min(u32(Memory.size()), CMonsterEnemySharingBudget::MaxRecordsPerSource);
	for (u32 Examined = 0; Examined < Limit && Budget.RecordsRemaining; ++Examined)
	{
		const CEntityAlive* Target = Entry->first;
		SharingTargetCursor = Target;
		++Entry;
		if (Entry == Memory.end())
		{
			Entry = Memory.begin();
		}
		if (ReceiveSharedEnemy(Source, Target, true))
		{
			Accepted.push_back(Target);
		}
	}
	if (Accepted.empty())
	{
		return true;
	}
	FinishEnemySharing(Source, Accepted);
	return true;
}

void CMonsterEnemyManager::FinishEnemySharing(CBaseMonster* Source, const xr_vector<const CEntityAlive*>& Accepted)
{
	NextSharingReceive = Device.dwTimeGlobal + EnemySharingDelay();
	const bool Called = Source->EnemyMan.EmitEnemyCall(Accepted);
	if (Called && !Source->EnemySharingWireless)
	{
		for (const CEntityAlive* Target : Accepted)
		{
			if (std::find(CalledEnemyIds.begin(), CalledEnemyIds.end(), Target->ID()) == CalledEnemyIds.end())
			{
				if (CalledEnemyIds.size() == CMonsterTargetAllocation::MaxTargets)
				{
					CalledEnemyIds.erase(CalledEnemyIds.begin());
				}
				CalledEnemyIds.push_back(Target->ID());
			}
		}
		NextCallSoundTime = std::max(NextCallSoundTime, Device.dwTimeGlobal + u32(monster->EnemyCallCooldown * 1000.f));
	}
}

const CEntityAlive* CMonsterEnemyManager::GetAssignedEnemy()
{
	CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal,
		EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation]);
	if (!AssignedEnemy || s32(AssignmentUntil - Device.dwTimeGlobal) <= 0 ||
		!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterDistributedTargeting] ||
		AssignedEnemy->getDestroy() || !is_enemy(AssignedEnemy))
	{
		return nullptr;
	}
	const auto& Memory = monster->EnemyMemory.get_memory();
	const auto Entry = Memory.find(AssignedEnemy);
	return Entry != Memory.end() && monster->EnemyMemory.IsActual(Entry->second) ? AssignedEnemy : nullptr;
}

void CMonsterEnemyManager::DistributeEnemies(xr_vector<CBaseMonster*>& Members)
{
	const u32 Now = Device.dwTimeGlobal;
	if (!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] ||
		!EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterDistributedTargeting] ||
		!monster->g_Alive() || monster->getDestroy() || monster->MonsterPeaceful ||
		s32(Now - NextDistributionUpdate) < 0 || Members.empty())
	{
		return;
	}
	static u32 PlanFrame = u32(-1), PlansRemaining = 0;
	if (PlanFrame != Device.dwFrame)
	{
		PlanFrame = Device.dwFrame;
		PlansRemaining = 2;
	}
	if (!PlansRemaining)
	{
		g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::PlanDeferred);
		NextDistributionUpdate = Now + Random.randI(200, 700);
		return;
	}
	--PlansRemaining;
	CMonsterLogicTimerScope PlanTimer(EMonsterLogicTimer::Distribution);
	g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::Plans);
	struct SCandidate
	{
		const CEntityAlive* Target;
		float Distance;
	};
	xr_vector<SCandidate> Candidates;
	Candidates.reserve(CMonsterTargetAllocation::MaxTargets);
	for (CBaseMonster* Member : Members)
	{
		u32 Examined = 0;
		for (const auto& Entry : Member->EnemyMemory.get_memory())
		{
			if (++Examined > 64)
			{
				break;
			}
			if (!Entry.first || Entry.first->getDestroy() || !Entry.first->g_Alive() ||
				!Member->EnemyMemory.IsActual(Entry.second))
			{
				continue;
			}
			const float Distance = monster->Position().distance_to_sqr(Entry.second.position);
			const auto Existing = std::find_if(Candidates.begin(), Candidates.end(), [&](const SCandidate& Candidate)
			{
				return Candidate.Target == Entry.first;
			});
			if (Existing != Candidates.end())
			{
				Existing->Distance = std::min(Existing->Distance, Distance);
			}
			else if (Candidates.size() < CMonsterTargetAllocation::MaxTargets)
			{
				Candidates.push_back({Entry.first, Distance});
			}
			else
			{
				auto Farthest = std::max_element(Candidates.begin(), Candidates.end(), [](const SCandidate& A, const SCandidate& B)
				{
					return A.Distance < B.Distance;
				});
				if (Distance < Farthest->Distance)
				{
					*Farthest = {Entry.first, Distance};
				}
			}
		}
	}
	if (Candidates.empty())
	{
		NextDistributionUpdate = Now + Random.randI(3000, 5001);
		return;
	}
	g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::Members, u32(Members.size()));
	g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::Targets, u32(Candidates.size()));
	CMonsterTargetAllocation Plan;
	const u32 Count = u32(Members.size()), TargetCount = u32(Candidates.size());
	u32 TransmissionChecks = 0;
	for (u32 Index = 0; Index < Count; ++Index)
	{
		CBaseMonster* Receiver = Members[Index];
		CMonsterEnemyManager& Manager = Receiver->EnemyMan;
		Plan.Fixed[Index] = Manager.forced || Manager.m_script_enemy || Manager.HasDamageFocus(Manager.DamageFocus) ||
			Receiver->HasDamagePanic() || Receiver->GetScriptControl() ||
			(Receiver->m_controlled && Receiver->m_controlled->is_under_control());
		for (u32 TargetIndex = 0; TargetIndex < TargetCount; ++TargetIndex)
		{
			const CEntityAlive* Target = Candidates[TargetIndex].Target;
			if (Plan.Fixed[Index])
			{
				const CEntityAlive* FixedEnemy = Manager.HasDamageFocus(Manager.DamageFocus) ? Manager.DamageFocus : Manager.enemy;
				if (FixedEnemy == Target && !Receiver->HasDamagePanic())
				{
					++Plan.Load[TargetIndex];
				}
				continue;
			}
			if (!Manager.is_enemy(Target))
			{
				continue;
			}
			const auto& Memory = Receiver->EnemyMemory.get_memory();
			if (Memory.find(Target) == Memory.end() && Manager.CanAcceptEnemySharing())
			{
				for (CBaseMonster* Source : Members)
				{
					if (TransmissionChecks >= 256 || !EnemySharingBudget().RecordsRemaining)
					{
						break;
					}
					if (Source != Receiver && Source->EnemyMemory.get_memory().find(Target) != Source->EnemyMemory.get_memory().end())
					{
						++TransmissionChecks;
						if (Manager.ReceiveSharedEnemy(Source, Target))
						{
							break;
						}
					}
				}
			}
			const auto Entry = Memory.find(Target);
			if (Entry == Memory.end() || !Receiver->EnemyMemory.IsActual(Entry->second))
			{
				continue;
			}
			const float Distance = Receiver->Position().distance_to_sqr(Entry->second.position);
			Plan.Distance[Index][TargetIndex] = Manager.AssignedEnemy == Target ? Distance * .85f : Distance;
		}
	}
	Plan.Distribute(Count, TargetCount);
	for (u32 Index = 0; Index < Count; ++Index)
	{
		CMonsterEnemyManager& Manager = Members[Index]->EnemyMan;
		Manager.NextDistributionUpdate = Now + Random.randI(3000, 5001);
		if (!Plan.Fixed[Index])
		{
			Manager.AssignedEnemy = Plan.Target[Index] == u32(-1) ? nullptr : Candidates[Plan.Target[Index]].Target;
			Manager.AssignmentUntil = Now + 6500;
		}
	}
}

void CMonsterEnemyManager::UpdateEnemySharing(const CEntityAlive* BroadcastTarget, bool Immediate)
{
	if (Immediate && BroadcastTarget == g_actor)
	{
		PendingActorReacquisition = true;
	}
	if (PendingActorReacquisition)
	{
		Immediate = true;
		BroadcastTarget = g_actor;
	}
	CMonsterLogicTimerScope TotalTimer(EMonsterLogicTimer::NewLogicTotal,
		EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation]);
	const u32 Now = Device.dwTimeGlobal;
	if (monster->MonsterPeaceful || !EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] || (!Immediate && s32(Now - NextSharingUpdate) < 0) || !monster->g_Alive() || monster->getDestroy())
	{
		return;
	}
	if (!EnemySharingBudget().TakeUpdate())
	{
		g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::SharingDeferred);
		NextSharingUpdate = Now + Random.randI(200, 700);
		return;
	}
	CMonsterLogicTimerScope SharingTimer(EMonsterLogicTimer::Sharing);
	g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::SharingUpdates);
	NextSharingUpdate = Now + EnemySharingDelay();
	if (!enemy)
	{
		CalledEnemyIds.clear();
	}
	const bool BuildMembers = !Immediate && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterDistributedTargeting] &&
		s32(Now - NextDistributionUpdate) >= 0;
	if (!Immediate && !BuildMembers && !CanAcceptEnemySharing() && !(monster->EnemySharingEnabled && monster->EnemySharingWireless))
	{
		return;
	}
	xr_vector<CBaseMonster*> Members;
	if (BuildMembers)
	{
		Members.reserve(CMonsterTargetAllocation::MaxMembers);
		Members.push_back(monster);
	}
	auto AddMember = [&](CBaseMonster* Peer)
	{
		if (!BuildMembers || Members.size() >= CMonsterTargetAllocation::MaxMembers || !Peer ||
			Peer == monster || Peer->MonsterPeaceful || !Peer->g_Alive() || Peer->getDestroy() ||
			monster->Position().distance_to_sqr(Peer->Position()) > monster->EnemySharingRadius * monster->EnemySharingRadius ||
			std::find(Members.begin(), Members.end(), Peer) != Members.end())
		{
			return;
		}
		if (CanReceiveFrom(Peer) || Peer->EnemyMan.CanReceiveFrom(monster))
		{
			Members.push_back(Peer);
		}
	};
	xr_vector<CObject*> Visible;
	monster->feel_vision_get(Visible);
	u32 SourceBatches = 0;
	const u32 VisibleLimit = std::min(u32(Visible.size()), 64u);
	const u32 StartPeer = Visible.empty() ? 0 : SharingPeerCursor % u32(Visible.size());
	for (u32 VisibleExamined = 0; VisibleExamined < VisibleLimit; ++VisibleExamined)
	{
		if ((!BuildMembers || Members.size() >= CMonsterTargetAllocation::MaxMembers) &&
			(!CanAcceptEnemySharing() || SourceBatches >= CMonsterEnemySharingBudget::MaxSourcesPerUpdate))
		{
			break;
		}
		CObject* Object = Visible[(StartPeer + VisibleExamined) % Visible.size()];
		CBaseMonster* Source = Object ? Object->cast_base_monster() : nullptr;
		if (Source)
		{
			if (!Immediate && SourceBatches < CMonsterEnemySharingBudget::MaxSourcesPerUpdate && CanAcceptEnemySharing() &&
				Source->EnemySharingEnabled && !Source->EnemyMemory.get_memory().empty())
			{
				if (ReceiveKnownEnemies(Source))
				{
					++SourceBatches;
				}
				SharingPeerCursor = StartPeer + VisibleExamined + 1;
			}
			AddMember(Source);
		}
	}
	bool DirectEvidence = monster->EnemySharingEnabled && HasDirectEnemyEvidence(BroadcastTarget ? BroadcastTarget : enemy);
	if (monster->EnemySharingEnabled && !DirectEvidence)
	{
		u32 Examined = 0;
		for (const auto& Entry : monster->EnemyMemory.get_memory())
		{
			if (++Examined > 64)
			{
				break;
			}
			if (HasDirectEnemyEvidence(Entry.first))
			{
				DirectEvidence = true;
				break;
			}
		}
	}
	if (monster->EnemySharingEnabled && monster->EnemySharingRadius > 0.f &&
		(Immediate || monster->EnemySharingWireless || monster->EnemySharingCloseRatio > 0.f) && DirectEvidence)
	{
		if (!EnemySharingBudget().TakeQuery())
		{
			g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::QueryDeferred);
			NextSharingUpdate = Now + Random.randI(200, 700);
		}
		else
		{
			PendingActorReacquisition = false;
			xr_vector<ISpatialShared> Neighbours;
			{
				CMonsterLogicTimerScope QueryTimer(EMonsterLogicTimer::Wireless);
				g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::Queries);
				g_SpatialSpace->q_sphere(Neighbours, 0, ESPATIAL_TYPE::COLLIDEABLE, monster->Position(), (Immediate || monster->EnemySharingWireless) ? monster->EnemySharingRadius : monster->EnemySharingRadius * monster->EnemySharingCloseRatio);
			}
			u32 Delivered = 0, SpatialExamined = 0;
			for (const auto& Spatial : Neighbours)
			{
				if (++SpatialExamined > 512)
				{
					break;
				}
				CObject* Object = Spatial ? Spatial->dcast_CObject() : nullptr;
				CBaseMonster* Receiver = Object ? Object->cast_base_monster() : nullptr;
				if (Receiver && Receiver != monster && Receiver->EnemyMan.CanAcceptEnemySharing(Immediate) && Receiver->EnemyMan.CanReceiveFrom(monster))
				{
					bool DeliveredEnemy = false;
					if (Immediate)
					{
						DeliveredEnemy = Receiver->EnemyMan.ReceiveSharedEnemy(monster, BroadcastTarget, false, true);
					}
					else
					{
						DeliveredEnemy = Receiver->EnemyMan.ReceiveKnownEnemies(monster);
					}
					AddMember(Receiver);
					if (DeliveredEnemy && ++Delivered == CMonsterTargetAllocation::MaxMembers)
					{
						break;
					}
				}
			}
		}
	}
	if (Immediate && !DirectEvidence)
	{
		PendingActorReacquisition = false;
	}
	DistributeEnemies(Members);
}

void CMonsterEnemyManager::transfer_enemy(CBaseMonster *friend_monster, bool ParentLink)
{
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] &&
		(!monster->g_Alive() || monster->getDestroy() || !friend_monster || !friend_monster->g_Alive() || friend_monster->getDestroy()))
	{
		return;
	}
	if (monster->MonsterPeaceful || (friend_monster && friend_monster->MonsterPeaceful))
	{
		return;
	}
	if (EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation] && !ParentLink)
	{
		ReceiveSharedEnemy(friend_monster, friend_monster ? friend_monster->EnemyMan.get_enemy() : nullptr);
		return;
	}

	CMonsterLogicTimerScope LegacySharingTimer(EMonsterLogicTimer::Sharing);
	g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::SharingUpdates);
	// если у friend_monster нет врага
	if (!friend_monster->EnemyMan.get_enemy()) return;

	monster->EnemyMemory.add_enemy(
		friend_monster->EnemyMan.get_enemy(), 
		friend_monster->EnemyMan.get_enemy_position(),
		friend_monster->EnemyMan.get_enemy_vertex(),
		friend_monster->EnemyMan.get_enemy_time_last_seen()
	);
	g_MonsterLogicTelemetry.Count(EMonsterLogicCounter::NativeTransfers);
}

u32 CMonsterEnemyManager::see_enemy_duration()
{
	return ((m_time_start_see_enemy == 0) ? 0 : (time() - m_time_start_see_enemy));
}

void CMonsterEnemyManager::script_enemy	()
{
	m_script_enemy		= 0;
}

void CMonsterEnemyManager::script_enemy	(const CEntityAlive &enemy_)
{
	m_script_enemy		= &enemy_;
}

void CMonsterEnemyManager::remove_links (CObject* O)
{
	if (O && PlayerExposureTargetID == O->ID())
	{
		ResetPlayerVisualAcquisition();
	}
	ResetPlayerVisualAcquisition();
	OwnVisionFrame = u32(-1);
	OwnVisibleObjects.clear();
	if (SharingTargetCursor == O)
	{
		SharingTargetCursor = nullptr;
	}
	if (O)
	{
		CalledEnemyIds.erase(std::remove(CalledEnemyIds.begin(), CalledEnemyIds.end(), O->ID()), CalledEnemyIds.end());
	}
	if (AssignedEnemy == O)
	{
		AssignedEnemy = nullptr;
		AssignmentUntil = 0;
	}
	if (DamageFocus == O)
	{
		DamageFocus = nullptr;
		DamageFocusUntil = 0;
	}
	if ( enemy == O )
	{
		enemy			= nullptr;
	}
	if ( prev_enemy == O )
	{
		prev_enemy		= nullptr;
	}
	if ( m_script_enemy	==	O )
	{
		m_script_enemy	= nullptr;
	}
}
void CMonsterEnemyManager::ResetEnemySharingState(bool ResetSelection)
{
	OwnVisionFrame = u32(-1);
	OwnVisibleObjects.clear();
	if (ResetSelection && !forced && !m_script_enemy)
	{
		enemy = nullptr;
		prev_enemy = nullptr;
		flags.zero();
	}
	DamageFocus = nullptr;
	DamageFocusUntil = 0;
	AssignedEnemy = nullptr;
	AssignmentUntil = 0;
	NextDistributionUpdate = 0;
	NextCallSoundTime = 0;
	CalledEnemyIds.clear();
	SharingPeerCursor = Random.randI(0, 64);
	SharingTargetCursor = nullptr;
	PendingActorReacquisition = false;
	NextSharingUpdate = 0;
	NextSharingReceive = 0;
	if (!monster->MonsterPeaceful && EngineExternal()[EEngineExternalMonstersLogic::EnableMonsterFactionEnemySharingIsolation])
	{
		NextSharingUpdate = Device.dwTimeGlobal + EnemySharingDelay();
	}
}
