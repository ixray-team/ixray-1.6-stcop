#include "StdAfx.h"

#include "../control_animation_base.h"
#include "../control_direction_base.h"

#include "monster_state_rest_fun.h"


#include "../../../../xrPhysics/PhysicsShell.h"

#include "../monster_cover_manager.h"
#include "../monster_sound_memory.h"
#include "../monster_home.h"

#define IMPULSE_TO_CORPSE	15.f
#define MIN_DELAY			100
#define TIME_IN_STATE		8000


CStateMonsterRestFun::CStateMonsterRestFun(CBaseMonster* obj) : inherited(obj)
{
}


void CStateMonsterRestFun::initialize()
{
	inherited::initialize();

	time_last_hit = 0;
}



void CStateMonsterRestFun::execute()
{
	Fvector point;
	float	dist;

	Fvector dir;
	dir.sub(object->CorpseMan.get_corpse_position(), object->Position());
	dist = dir.magnitude();
	dir.normalize();
	point.mad(object->CorpseMan.get_corpse_position(), dir, 2.0f);

	object->set_action(ACT_RUN);
	object->path().set_target_point(point);
	object->path().set_rebuild_time(100 + u32(50.f * dist));
	object->path().set_use_covers(false);
	object->path().set_distance_to_end(0.5f);
	object->anim().accel_activate(eAT_Calm);
	object->anim().accel_set_braking(false);

	object->set_state_sound(MonsterSound::eMonsterSoundIdle);

	if ((dist < object->db().m_fDistToCorpse + 0.5f) && (time_last_hit + MIN_DELAY < Device.dwTimeGlobal)) {
		CEntityAlive* corpse = const_cast<CEntityAlive*>		(object->CorpseMan.get_corpse());
		CPhysicsShellHolder* target = smart_cast<CPhysicsShellHolder*>	(corpse);

		if (target && target->m_pPhysicsShell) {
			Fvector			dir_;
			dir_.add(Fvector().sub(target->Position(), object->Position()), object->Direction());

			float			h, p;
			dir_.getHP(h, p);
			dir_.setHP(h, p + 5 * PI / 180);
			dir_.normalize();

			// выполнить бросок
			for (u32 i = 0; i < target->m_pPhysicsShell->get_ElementsNumber(); i++) {
				target->m_pPhysicsShell->get_ElementByStoreOrder((u16)i)->applyImpulse(dir_, IMPULSE_TO_CORPSE * target->m_pPhysicsShell->getMass() / target->m_pPhysicsShell->Elements().size());
			}

			time_last_hit = Device.dwTimeGlobal;
		}
	}
}


bool CStateMonsterRestFun::check_start_conditions()
{
	return ((object->CorpseMan.get_corpse() != 0) && object->Home->at_home(object->CorpseMan.get_corpse()->Position()));
}


bool CStateMonsterRestFun::check_completion()
{
	if (!object->CorpseMan.get_corpse()) return true;
	if (time_state_started + TIME_IN_STATE < Device.dwTimeGlobal) return true;
	return false;
}

#undef TIME_IN_STATE
#undef MIN_DELAY
#undef IMPULSE_TO_CORPSE
