#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Ground/CELGroundMovement.h"
#include "FFxExtendedEntityesLogic/Interfaces/IELAdapter.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
#include "xrGame/ai/monsters/control_path_builder_base.h"
#include "xrGame/ai/monsters/monster_velocity_space.h"
#include "xrGame/ai_monster_space.h"
#include "xrGame/ai/monsters/state_manager.h"

// Связывает движение штатным наземным контроллером с необходимыми службами и состоянием владельца.
CELGroundMovement::CELGroundMovement(IELAdapter& OwnerAdapter) : Adapter(OwnerAdapter) {}

// Передаёт запрос перемещения штатному контроллеру категории.
bool CELGroundMovement::MoveTo(const Fvector& TargetPosition, u32 TargetVertex, bool IsRunning)
{
	if (!Adapter.CanControl() || !Adapter.CanReach(TargetPosition, TargetVertex))
	{
		return false;
	}
	Adapter.BeginMovementControl();
	CBaseMonster& OwnerMonster = Adapter.Monster();
	if (OwnerMonster.m_bSleep)
	{
		if (OwnerMonster.StateMan)
		{
			OwnerMonster.StateMan->critical_finalize();
		}
		OwnerMonster.wake_up();
	}
	OwnerMonster.path().enable_path();
	OwnerMonster.path().set_level_path_type();
	OwnerMonster.path().set_use_covers(false);
	// Explicit movement commands must not inherit an old native run's continuation or five-second rebuild delay.
	OwnerMonster.path().extrapolate_path(false);
	OwnerMonster.path().set_use_dest_orient(false);
	OwnerMonster.path().set_try_min_time(false);
	OwnerMonster.path().set_rebuild_time(100);
	OwnerMonster.path().set_distance_to_end(.25f);
	OwnerMonster.path().set_target_point(TargetPosition, TargetVertex);
	OwnerMonster.path().set_velocity_mask(IsRunning ? MonsterMovement::eVelocityParamsRun : MonsterMovement::eVelocityParamsWalk);
	OwnerMonster.path().set_desirable_mask(IsRunning ? MonsterMovement::eVelocityParameterRunNormal : MonsterMovement::eVelocityParameterWalkNormal);
	OwnerMonster.set_action(IsRunning ? ACT_RUN : ACT_WALK_FWD);
	return true;
}

// Останавливает движение и отменяет связанные проверки маршрута.
void CELGroundMovement::Stop()
{
	if (!Adapter.CanControl())
	{
		return;
	}
	Adapter.BeginMovementControl();
	Adapter.Monster().path().disable_path();
	Adapter.Monster().set_action(ACT_STAND_IDLE);
}
