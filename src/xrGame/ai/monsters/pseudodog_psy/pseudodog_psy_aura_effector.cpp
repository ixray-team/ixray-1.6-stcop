#include "stdafx.h"
#include "../pseudodog/pseudodog.h"
#include "../pseudodog_phantom/pseudodog_phantom.h"
#include "pseudodog_psy_aura.h"
#include "pseudodog_psy.h"
#include "pseudodog_psy_aura_effector.h"
#include "../../../actor.h"
#include "../../../ActorEffector.h"
#include "../../../actor_memory.h"
#include "../../../visual_memory_manager.h"
#include "../../../level.h"

CPsyDogAura::CPsyDogAura(CBaseMonster* object)
{
	m_object = smart_cast<CPseudoPsyDogBase*>(object);
}

CPsyDogAura::~CPsyDogAura()
{

}

void CPsyDogAura::reinit()
{
	m_time_actor_saw_phantom	= 0;
	m_time_phantom_saw_actor	= 0;

	m_actor						= smart_cast<CActor *>(Level().CurrentEntity());
	VERIFY						(m_actor);
}

void CPsyDogAura::update_schedule()
{
	if (!m_object->g_Alive() || !m_actor)
		return;

	m_time_phantom_saw_actor	= 0;

	CVisualMemoryManager::VISIBLES::const_iterator	I = m_actor->memory().visual().objects().begin();
	CVisualMemoryManager::VISIBLES::const_iterator	E = m_actor->memory().visual().objects().end();
	for ( ; I != E; ++I) {
		const CGameObject *obj = (*I).m_object;
		if (smart_cast<const CPseudoPsyDogPhantomBase*>(obj)) {
			if (m_actor->memory().visual().visible_now(obj))
				m_time_actor_saw_phantom = time();
		}
	}

	xr_vector<CPseudoPsyDogPhantomBase*>::iterator it = m_object->m_storage.begin();
	for (; it !=  m_object->m_storage.end();++it) {
		if ((*it)->EnemyMan.get_enemy() == m_actor)
			m_time_phantom_saw_actor = time();
		else {
			ENEMIES_MAP::const_iterator I_ = (*it)->EnemyMemory.get_memory().begin();
			ENEMIES_MAP::const_iterator E_ = (*it)->EnemyMemory.get_memory().end();
			for (; I_ != E_; ++I_) {
				if (I_->first == m_actor) {
					m_time_phantom_saw_actor = _max(m_time_phantom_saw_actor, I_->second.time);
				}
			}
		}

		if (m_time_phantom_saw_actor == time()) 
			break;
	}

	bool const close_to_actor	=	m_actor ? m_object->Position().distance_to(m_actor->Position()) < 30 : false;
	bool const need_be_active	=	((m_time_actor_saw_phantom + 2000 > time()) || 
									(m_time_phantom_saw_actor + 10000 > time())) && close_to_actor;
	if (active()) {
		if (!need_be_active) {
			m_effector->switch_off	();
			m_effector				= 0;
		}
	} else {
		if (need_be_active) {
			m_effector = new CPPEffectorPsyDogAura(m_state, 5000);
			Actor()->Cameras().AddPPEffector		(m_effector);
		}
	}
}

void CPsyDogAura::on_death()
{
	if (active()) {
		m_effector->switch_off	();
		m_effector				= 0;
	}
}
