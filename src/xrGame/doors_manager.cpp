////////////////////////////////////////////////////////////////////////////
//	Created		: 23.06.2009
//	Author		: Dmitriy Iassenev
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "doors_manager.h"
#include "gameobject.h"
#include "doors_door.h"
#include "doors_actor.h"

using doors::actor;
using doors::manager;
using doors::door;

manager::manager					( Fbox const& bounding_box ) :
	m_doors					( bounding_box, 2.f, 512, 2048 )
{
}

manager::~manager					( )
{
	VERIFY2					( m_doors.empty(), make_string<const char*>("there are %d still registered doors", m_doors.size()) );
}

door* manager::register_door		( CPhysicObject& object )
{
	door* const result		= xr_new<door>( &object );
	m_doors.insert			( result );
	return					result;
}

void manager::unregister_door		( door*& door )
{
	m_doors.remove			( door );
	xr_delete				( door );
}

bool manager::actualize_doors_state	( actor& actor, float const average_speed )
{
	float const radius		= average_speed*g_door_open_time + g_door_length;
	Fvector const& position	= actor.get_position();
	//check_bug_door			( );
	m_doors.nearest			( position, radius, m_nearest_doors );
	//check_bug_door			( );
	if ( m_nearest_doors.empty() && !actor.need_update() )
		return				true;

	return					actor.update_doors( m_nearest_doors, average_speed );
}

void manager::on_door_is_open		( door* door )
{
	door->on_change_state	( door_state_open );
}

void manager::on_door_is_closed		( door* door )
{
	door->on_change_state	( door_state_closed );
}

bool manager::is_door_locked		( door const* door ) const
{
	return					door->is_locked( doors::door_state_open ) || door->is_locked( doors::door_state_closed );
}

void manager::lock_door				( door* const door )
{
	door->lock				( );
}

void manager::unlock_door			( door* const door )
{
	door->unlock			( );
}

bool manager::is_door_blocked		( door* const door ) const
{
	return					door->is_blocked(door_state_open) || door->is_blocked(door_state_closed);
}