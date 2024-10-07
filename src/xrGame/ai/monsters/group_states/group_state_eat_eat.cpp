#include "StdAfx.h"
#include "../xrPhysics/PhysicsShell.h"
#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"

#include "../dog/dog.h"
#include "../ai_monster_squad.h"
#include "../ai_monster_squad_manager.h"
#include "entity_alive.h"
#include "group_state_eat_eat.h"

#define TIME_TO_EAT 20000

CStateGroupEating::CStateGroupEating(CBaseMonster* object) : inherited(object)
{
	m_pDog = smart_cast<CDogBase*>(object);

	corpse = nullptr;
	time_last_eat = 0;
}


CStateGroupEating::~CStateGroupEating()
{
}


void CStateGroupEating::initialize()
{
	inherited::initialize();
	time_last_eat = 0;
}


void CStateGroupEating::execute()
{
	if (object->EatedCorpse != corpse)  return;

	object->set_action(ACT_EAT);
	object->set_state_sound(MonsterSound::eMonsterSoundEat);

	// съесть часть
	if (time_last_eat + u32(1000 / object->db().m_fEatFreq) < Device.dwTimeGlobal) {
		object->ChangeSatiety(object->db().m_fEatSlice);
		corpse->m_fFood -= object->db().m_fEatSliceWeight;
		time_last_eat = Device.dwTimeGlobal;
	}
}

bool CStateGroupEating::check_start_conditions()
{
	corpse = const_cast<CEntityAlive*>(object->EatedCorpse);
	VERIFY(corpse);

	Fvector nearest_bone_pos;
	if ((corpse->m_pPhysicsShell == NULL) || (!corpse->m_pPhysicsShell->isActive())) {
		nearest_bone_pos = corpse->Position();
	}
	else nearest_bone_pos = object->character_physics_support()->movement()->PHCaptureGetNearestElemPos(corpse);

	float dist = nearest_bone_pos.distance_to(object->Position());
	float dist_to_corpse = object->db().m_fDistToCorpse;

	if (dist + 0.5f < dist_to_corpse) return true;
	return false;
}


bool CStateGroupEating::check_completion()
{
	CMonsterSquad* squad = monster_squad().get_squad(object);
	if (squad && squad->SquadActive())
	{
		const CEntity* squad_leader = squad->GetLeader();
		if (squad_leader && object != squad_leader)
		{
			if (object->Position().distance_to(squad_leader->Position()) < 5.f)
			{
				m_pDog->set_current_animation(6);
				return true;
			}
		}
	}
	if (time_state_started + TIME_TO_EAT < time())	return true;
	if (object->EatedCorpse != corpse)	return true;

	Fvector nearest_bone_pos;
	if ((corpse->m_pPhysicsShell == NULL) || (!corpse->m_pPhysicsShell->isActive())) {
		nearest_bone_pos = corpse->Position();
	}
	else nearest_bone_pos = object->character_physics_support()->movement()->PHCaptureGetNearestElemPos(corpse);

	float dist = nearest_bone_pos.distance_to(object->Position());
	float dist_to_corpse = object->db().m_fDistToCorpse;
	if (dist > dist_to_corpse + 0.5f) return true;

	return false;
}


void CStateGroupEating::remove_links(CObject* object_)
{
	if (corpse == object_)
		corpse = 0;
}
