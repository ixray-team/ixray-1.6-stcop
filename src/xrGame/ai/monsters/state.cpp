#include "StdAfx.h"
#include "state.h"

#include "basemonster/base_monster.h"

CState::CState(CBaseMonster* obj, void* data)
{
	reset();

	object = obj;
	_data = data;
}

CState::~CState()
{
	free_mem();
}

void CState::reinit()
{
	if (current_substate != u32(-1)) get_state_current()->critical_finalize();

	for (STATE_MAP_IT it = substates.begin(); it != substates.end(); it++)
		it->second->reinit();

	reset();
}



void CState::initialize()
{
	time_state_started = Device.dwTimeGlobal;

	current_substate = u32(-1); // means need reselect state
	prev_substate = u32(-1);
}


void CState::execute()
{
	VERIFY(object->g_Alive());
	// проверить внешние условия изменения состояния
	check_force_state();

	// если состояние не выбрано, перевыбрать
	if (current_substate == u32(-1)) {
		reselect_state();

#ifdef DEBUG
		// Lain: added
		if (current_substate == u32(-1))
		{
			debug::text_tree tree;
			if (CBaseMonster* p_monster = smart_cast<CBaseMonster*>(object))
			{
				p_monster->add_debug_info(tree);
			}

			debug::log_text_tree(tree);
			VERIFY(current_substate != u32(-1));
		}
#endif
	}

	// выполнить текущее состояние
	CSState* state = get_state(current_substate);
	state->execute();

	// сохранить текущее состояние
	prev_substate = current_substate;

	// проверить на завершение текущего состояния
	if (state->check_completion()) {
		state->finalize();
		current_substate = u32(-1);
	}
}


void CState::finalize()
{
	reset();
}


void CState::critical_finalize()
{
	if (current_substate != u32(-1)) get_state_current()->critical_finalize();
	reset();
}


void CState::reset()
{
	current_substate = u32(-1);
	prev_substate = u32(-1);
	time_state_started = 0;
}


void CState::select_state(u32 new_state_id)
{
	if (current_substate == new_state_id) return;
	CSState* state;

	// если предыдущее состояние активно, завершить его
	if (current_substate != u32(-1)) {
		state = get_state(current_substate);
		state->critical_finalize();
	}

	// установить новое состояние
	state = get_state(current_substate = new_state_id);

	// инициализировать новое состояние
	setup_substates();

	state->initialize();
}


CState* CState::get_state(u32 state_id)
{
	STATE_MAP_IT it = substates.find(state_id);
	VERIFY(it != substates.end());

	return it->second;
}


void CState::add_state(u32 state_id, CSState* s)
{
	substates.insert(std::make_pair(state_id, s));
}


void CState::free_mem()
{
	for (STATE_MAP_IT it = substates.begin(); it != substates.end(); it++) xr_delete(it->second);
}


void CState::fill_data_with(void* ptr_src, u32 size)
{
	VERIFY(ptr_src);
	VERIFY(_data);

	CopyMemory(_data, ptr_src, size);
}

#ifdef DEBUG


void   CState::add_debug_info(debug::text_tree& root_s)
{
	typedef debug::text_tree TextTree;
	if (!substates.empty())
	{
		for (typename SubStates::const_iterator i = substates.begin(), e = substates.end();
			i != e; ++i)
		{
			TextTree& current_state_s = root_s.add_line(EMonsterState((*i).first));
			if (current_substate == (*i).first)
			{
				if ((*i).second)
				{
					(*i).second->add_debug_info(current_state_s);
				}
			}
		}
	}
}

#endif


CState* CState::get_state_current()
{
	if (substates.empty() || (current_substate == u32(-1))) return 0;

	STATE_MAP_IT it = substates.find(current_substate);
	VERIFY(it != substates.end());

	return it->second;
}

EMonsterState CState::get_state_type()
{
	if (substates.empty() || (current_substate == u32(-1))) return eStateUnknown;

	EMonsterState state = get_state_current()->get_state_type();
	return ((state == eStateUnknown) ? EMonsterState(current_substate) : state);
}


void CState::remove_links(CObject* object_)
{
	typename SubStates::iterator	i = substates.begin();
	typename SubStates::iterator	e = substates.end();
	for (; i != e; ++i)
		(*i).second->remove_links(object_);
}


bool CState::check_control_start_conditions(ControlCom::EControlType type)
{
	CState* child = get_state_current();
	if (child && !child->check_control_start_conditions(type))
	{
		return false;
	}

	return true;
}
