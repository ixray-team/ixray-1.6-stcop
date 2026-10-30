////////////////////////////////////////////////////////////////////////////
//	Module 		: FactionState.cpp
//	Created 	: 22.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Faction War state class implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"

#include "FactionState.h"

#include "../Actor.h"
#include "../character_community.h"
#include "../character_reputation.h"
#include "../relation_registry.h"
#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"

using namespace luabind;

FactionState::FactionState():
	member_count(0),
	resource(0.0f),
	power(0.0f),
	m_actor_goodwill(0)
{
	m_id._set         ( nullptr );
	m_name._set       ( nullptr );
	m_icon._set       ( nullptr );
	m_icon_big._set   ( nullptr );
	m_target._set     ( nullptr );
	m_target_desc._set( nullptr );
	m_location._set   ( nullptr );
	
	ResetStates();
}

FactionState::FactionState( shared_str const& id ) :
	FactionState()
{
	set_faction_id2( id );
}

FactionState::~FactionState() = default;

void FactionState::ResetStates()
{
	for ( int i = 0; i < war_state_count ; ++i )
	{
		m_war_state_str[i]._set( nullptr );
		m_war_state_hint_str[i]._set( nullptr );
	}
	//for ( int i = 0; i < bonuses_count ; ++i )
	//{
	//bonuses_vs[i]._set( nullptr );
	//}
}

#pragma optimize("s",on)
void FactionState::update_info()
{
	if (m_id.size() == 0)
	{
		return;
	}

	m_actor_goodwill = 0;
	CObject* current_entity = Level().CurrentEntity();
	if (CActor* pActor = current_entity != nullptr ? current_entity->cast_actor() : nullptr)
	{
		CHARACTER_COMMUNITY	char_comm;
		char_comm.set(m_id);
		m_actor_goodwill = RELATION_REGISTRY().GetCommunityGoodwill(char_comm.index(), pActor->object_id());
	}

	ResetStates();

	luabind::functor<void> m_functor;
	R_ASSERT(ai().script_engine().functor("pda.fill_faction_state", m_functor));
	m_functor(this);
}
