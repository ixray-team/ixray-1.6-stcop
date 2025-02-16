////////////////////////////////////////////////////////////////////////////
//	Module 		: smart_cover_transition.cpp
//	Created 	: 20.12.2007
//	Author		: Alexander Dudin
//	Description : Transition class for smart_cover
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "smart_cover_transition.hpp"
#include "smart_cover_transition_animation.hpp"
#include "smart_cover_detail.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "../xrCore/object_broker.h"

using smart_cover::detail::parse_table;
using smart_cover::detail::parse_string;
using smart_cover::detail::parse_fvector;
using smart_cover::detail::parse_int;
using smart_cover::transitions::animation_action;

smart_cover::transitions::action::action					(luabind::object const &table)
{
	VERIFY						(luabind::get_type(table) == LUA_TTABLE);

	m_precondition_functor		= parse_string(table, "precondition_functor");
	m_precondition_params		= parse_string(table, "precondition_params");

	luabind::object				anim_table;
	parse_table					(table, "actions", anim_table);
	load_animations				(anim_table);
}

smart_cover::transitions::action::~action()
{
	delete_data(m_animations);
}

bool smart_cover::transitions::action::applicable			() const
{
	luabind::functor<bool>		functor;

	R_ASSERT2
	(
		ai().script_engine().functor(m_precondition_functor.c_str(),functor), 
		make_string<const char*>("failed to get [%s]", m_precondition_functor.c_str())
	);

	return						(functor(m_precondition_params.c_str()));
}

void smart_cover::transitions::action::load_animations	(luabind::object const &table)
{
	luabind::object::iterator I = table.begin();
	luabind::object::iterator E = table.end();
	for (; I != E; ++I)
	{
		luabind::object tmp = *I;
		Fvector	const& pos = smart_cover::detail::parse_fvector(tmp, "position");
		shared_str anim_id = smart_cover::detail::parse_string(tmp, "animation");
		MonsterSpace::EBodyState body_state = (MonsterSpace::EBodyState)smart_cover::detail::parse_int(tmp, "body_state");
		MonsterSpace::EMovementType movement_type = (MonsterSpace::EMovementType)smart_cover::detail::parse_int(tmp, "movement_type");
		animation_action* animation = new animation_action(pos, anim_id, body_state, movement_type);
		m_animations.push_back(animation);
	}
}

class body_state_predicate
{
	MonsterSpace::EBodyState	m_body_state;

public:
	IC	body_state_predicate	(MonsterSpace::EBodyState const &body_state) :
		m_body_state			(body_state)
	{
	}

	IC	bool	operator()		(animation_action *animation_action) const
	{
		VERIFY					(animation_action);

		return					(m_body_state == animation_action->body_state());
	}
};

animation_action const & smart_cover::transitions::action::animation	(MonsterSpace::EBodyState const &target_body_state) const
{
	Animations::const_iterator found = 
		std::find_if			(m_animations.begin(), m_animations.end(), body_state_predicate(target_body_state));
	
	if (found == m_animations.end()) {
#ifndef MASTER_GOLD
		Msg						("! There is no animation which can transfer bot to body_state [%i], selecting random transition", target_body_state);
#endif // #ifndef MASTER_GOLD
		return					(animation());
	}

#if 0 //for testing
	VERIFY2						(
		found != m_animations.end(),
		make_string("There is no animation which can transfer bot to body_state [%i]", target_body_state)
	);
#endif

	return						(**found);
}

animation_action const	& smart_cover::transitions::action::animation	() const
{
	return (*m_animations[Random.randI(m_animations.size())]);
}