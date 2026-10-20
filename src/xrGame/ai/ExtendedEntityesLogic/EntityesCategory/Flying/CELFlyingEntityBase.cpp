#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingEntityBase.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingSettings.h"
#include "ai/ExtendedEntityesLogic/EntityesCategory/Flying/CELFlyingAnimationCycles.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/monsters/state_manager.h"
#include "ai/monsters/control_animation_base.h"
#include "ai/monsters/control_movement_base.h"
#include "ai/monsters/monster_velocity_space.h"
#include "movement_manager.h"
#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"
#include "sound_player.h"
#include "Level.h"

namespace ExtendedEntityesNativeFlight
{
// Native lifecycle endpoint. Behavior is supplied by the same ECS pipeline as Ground.
class CStateEndpoint final : public IStateManagerBase
{
public:
	// Сбрасывает состояние штатного менеджера-заглушки нового летуна.
	void reinit() override { State = eStateRest_Idle; }
	// Оставляет штатное обновление пустым: поведением летуна управляет ECS-компонент.
	void update() override {}
	// Сохраняет запрошенное скриптом состояние нативного менеджера.
	void force_script_state(EMonsterState NewState) override { State = NewState; }
	// Не запускает отдельный скриптовый FSM в нативном менеджере-заглушке.
	void execute_script_state() override {}
	// Возвращает нативный менеджер-заглушку в спокойное состояние.
	void critical_finalize() override { State = eStateRest_Idle; }
	// Не хранит ссылок на цели и потому не выполняет отдельную очистку.
	void remove_links(CObject*) override {}
	// Возвращает текущее состояние нативного менеджера.
	EMonsterState get_state_type() override { return State; }
	// Запрещает запуск штатного наземного управления для нативного летуна.
	bool check_control_start_conditions(ControlCom::EControlType) override { return false; }

private:
	EMonsterState State = eStateRest_Idle;
};
} // namespace ExtendedEntityesNativeFlight

// Связывает нативную базу нового игрового летуна с необходимыми службами и состоянием владельца.
CELFlyingEntityBase::CELFlyingEntityBase()
{
	StateMan = new ExtendedEntityesNativeFlight::CStateEndpoint();
}

// Завершает нативную базу нового игрового летуна и освобождает связанные ресурсы.
CELFlyingEntityBase::~CELFlyingEntityBase()
{
	ExtendedEntityes::Detach(*this);
	xr_delete(StateMan);
}

// Читает параметры секции и подготавливает связанные ресурсы.
void CELFlyingEntityBase::Load(const char* Section)
{
	CBaseMonster::Load(Section);
	const SELFlyingSettings& Settings = CELFlyingSettings::Read(Section);
	SVelocityParam& IdleVelocity = move().get_velocity(MonsterMovement::eVelocityParameterIdle);
	anim().AddAnim(eAnimStandIdle, Settings.IdleAnimation.c_str(), -1, &IdleVelocity, PS_STAND);
	anim().AddAnim(eAnimStandTurnLeft, Settings.IdleAnimation.c_str(), -1, &IdleVelocity, PS_STAND);
	anim().AddAnim(eAnimStandTurnRight, Settings.IdleAnimation.c_str(), -1, &IdleVelocity, PS_STAND);
	u32 ActionIndex;
	for (ActionIndex = ACT_STAND_IDLE; ActionIndex <= ACT_HOME_WALK_SMELLING; ++ActionIndex)
	{
		anim().LinkAction(EAction(ActionIndex), eAnimStandIdle);
	}
	PostLoad(Section);
}

// Восстанавливает нативную сущность и подключает её состояние после спавна.
bool CELFlyingEntityBase::net_Spawn(CSE_Abstract* ServerEntity)
{
	if (!CBaseMonster::net_Spawn(ServerEntity))
	{
		return false;
	}
	IsGrounded = false;
	movement().enable_movement(false);
	if (g_Alive())
	{
		character_physics_support()->movement()->DestroyCharacter();
		character_physics_support()->movement()->SetApplyGravity(false);
		IKinematicsAnimated* Skeleton = Visual() ? Visual()->dcast_PKinematicsAnimated() : nullptr;
		const SELFlyingSettings& Settings = CELFlyingSettings::Read(*cNameSect());
		if (Skeleton)
		{
			FlightAnimation = Skeleton->ID_Cycle_Safe(Settings.FlightAnimation.c_str());
			if (FlightAnimation.valid())
			{
				ExtendedEntityesFlightAnimation::PlayLoop(*Skeleton, FlightAnimation);
			}
		}
	}
	return true;
}

// Исполняет действие летуна в соответствии с текущим режимом управления.
void CELFlyingEntityBase::Exec_Action(float DeltaTime)
{
	if (!g_Alive())
	{
		CBaseMonster::Exec_Action(DeltaTime);
	}
}

// Обновляет положение глаз штатным методом, а обзор ориентирует по реальному полётному преобразованию.
void CELFlyingEntityBase::eye_pp_s0()
{
	CCreature::eye_pp_s0();
	eye_matrix.i = XFORM().i;
	eye_matrix.j = XFORM().j;
	eye_matrix.k = XFORM().k;
}

// Обновляет кадр нативной сущности и её расширенной логики.
void CELFlyingEntityBase::UpdateCL()
{
	if (!g_Alive())
	{
		CBaseMonster::UpdateCL();
		return;
	}
	CEntityAlive::UpdateCL();
	if (LastFrame == Device.dwFrame)
	{
		return;
	}
	LastFrame = Device.dwFrame;
	if (Local())
	{
		ExtendedEntityes::FrameUpdate(*this, Device.fTimeDelta);
	}
	else if (!NET.empty())
	{
		XFORM().setHPB(NET.back().o_model, NET.back().o_torso.pitch, NET.back().o_torso.roll);
		Position() = NET.back().p_pos;
	}
	eye_pp_s0();
	if (!sound().playing_sounds().empty())
	{
		sound().update(Device.fTimeDelta);
	}
	spatial_move();
}

// Выполняет плановое обновление сущности через штатный планировщик.
void CELFlyingEntityBase::shedule_Update(u32 DeltaMilliseconds)
{
	if (!g_Alive())
	{
		CBaseMonster::shedule_Update(DeltaMilliseconds);
		return;
	}
	// Exec_Visibility внутри CCreature должен получить свежие положение и направление глаз.
	eye_pp_s0();
	CCreature::shedule_Update(DeltaMilliseconds);
}

// Записывает текущее сетевое состояние нативной сущности.
void CELFlyingEntityBase::net_Export(NET_Packet& Packet)
{
	if (Local() && !NET.empty())
	{
		NET.back().p_pos = Position();
		XFORM().getHPB(NET.back().o_model, NET.back().o_torso.pitch, NET.back().o_torso.roll);
	}
	CBaseMonster::net_Export(Packet);
}

// Завершает управление живой сущностью и запускает штатную обработку смерти.
void CELFlyingEntityBase::Die(CObject* Attacker)
{
	character_physics_support()->movement()->SetApplyGravity(true);
	CBaseMonster::Die(Attacker);
}

// Возвращает диагностическое имя игрового типа.
char* CELFlyingEntityBase::get_monster_class_name()
{
	return const_cast<char*>("extended_flying");
}
