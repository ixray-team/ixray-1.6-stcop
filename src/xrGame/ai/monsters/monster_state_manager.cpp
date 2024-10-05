#include "StdAfx.h"
#include "monster_state_manager.h"

void CMonsterStateManager::reinit()
{
	inherited::reinit();
}

namespace detail
{ // helper function implemented in file alife_simulator.cpp
	bool object_exists_in_alife_registry (u32 id);
} // namespace detail

void CMonsterStateManager::update()
{
	// Lain: added
	if ( !::detail::object_exists_in_alife_registry(this->object->ID()))
	{
		return;
	}

	if ( !this->object->g_Alive() )
	{
		return;		
	}	

	this->execute();
}

void CMonsterStateManager::force_script_state(EMonsterState state)
{
	// установить текущее состояние
	this->select_state(state);
}

void CMonsterStateManager::execute_script_state()
{
	// выполнить текущее состояние
	this->get_state_current()->execute();
}

bool CMonsterStateManager::can_eat()
{
	if (!this->object->CorpseMan.get_corpse()) return false;

	return check_state(eStateEat);
}

bool CMonsterStateManager::check_state(u32 state_id)
{
	if (this->prev_substate == state_id) {
		if (!this->get_state_current()->check_completion())		return true;
	} else {
		if (this->get_state(state_id)->check_start_conditions())	return true;
	}

	return false;
}

void CMonsterStateManager::critical_finalize()
{
	inherited::critical_finalize();
}

EMonsterState CMonsterStateManager::get_state_type()
{
	return inherited::get_state_type();
}

#ifdef DEBUG


void   CMonsterStateManager::add_debug_info (debug::text_tree& root_s)
{ 
	CState::add_debug_info(root_s); 
}

#endif
