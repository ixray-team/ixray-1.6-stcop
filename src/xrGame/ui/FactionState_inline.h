////////////////////////////////////////////////////////////////////////////
//	Module 		: FactionState_inline.h
//	Created 	: 23.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Faction War state class
////////////////////////////////////////////////////////////////////////////

#pragma once

int    FactionState::get_actor_goodwill() const
{
	return m_actor_goodwill;
}
void   FactionState::set_actor_goodwill( int gw )
{
	m_actor_goodwill = gw;
}
const char* FactionState::get_faction_id() const
{
	return m_id.c_str();
}
shared_str const& FactionState::get_faction_id2() const
{
	return m_id;
}
void   FactionState::set_faction_id( const char* id )
{
	m_id._set( id );
}
void   FactionState::set_faction_id2( shared_str const& id )
{
	m_id._set( id );
}
const char* FactionState::get_name() const
{
	return m_name.c_str();
}
void   FactionState::set_name( const char* name )
{
	m_name._set( name );
}
const char* FactionState::get_icon() const
{
	return m_icon.c_str();
}
void   FactionState::set_icon( const char* icon )
{
	m_icon._set( icon );
}

const char* FactionState::get_icon_big() const
{
	return m_icon_big.c_str();
}
void   FactionState::set_icon_big( const char* icon_b )
{	m_icon_big._set( icon_b );
}

const char* FactionState::get_target() const
{
	return m_target.c_str();
}
void   FactionState::set_target( const char* target )
{
	m_target._set( target );
}

const char* FactionState::get_target_desc() const
{
	return m_target_desc.c_str();
}
void   FactionState::set_target_desc( const char* target_desc )
{
	m_target_desc._set( target_desc );
}

const char* FactionState::get_location() const
{
	return m_location.c_str();
}
void   FactionState::set_location( const char* location )
{
	m_location._set( location );
}

const char* FactionState::get_war_state( u8 index ) const
{
	VERIFY( 0 <= index && index < war_state_count );
	return	m_war_state_str[index].c_str();	
}

const char* FactionState::get_war_state_hint( u8 index ) const
{
	VERIFY( 0 <= index && index < war_state_count );
	return	m_war_state_hint_str[index].c_str();	
}

const char* FactionState::get_war_state1() const			{	return	m_war_state_str[0].c_str();		}
void   FactionState::set_war_state1( const char* icon )	{			m_war_state_str[0]._set( icon );}
const char* FactionState::get_war_state2() const			{	return	m_war_state_str[1].c_str();		}
void   FactionState::set_war_state2( const char* icon )	{			m_war_state_str[1]._set( icon );}
const char* FactionState::get_war_state3() const			{	return	m_war_state_str[2].c_str();		}
void   FactionState::set_war_state3( const char* icon )	{			m_war_state_str[2]._set( icon );}
const char* FactionState::get_war_state4() const			{	return	m_war_state_str[3].c_str();		}
void   FactionState::set_war_state4( const char* icon )	{			m_war_state_str[3]._set( icon );}
const char* FactionState::get_war_state5() const			{	return	m_war_state_str[4].c_str();		}
void   FactionState::set_war_state5( const char* icon )	{			m_war_state_str[4]._set( icon );}

const char* FactionState::get_war_state_hint1() const		{	return	m_war_state_hint_str[0].c_str();		}
void   FactionState::set_war_state_hint1( const char* text )	{			m_war_state_hint_str[0]._set( text );}
const char* FactionState::get_war_state_hint2() const		{	return	m_war_state_hint_str[1].c_str();		}
void   FactionState::set_war_state_hint2( const char* text )	{			m_war_state_hint_str[1]._set( text );}
const char* FactionState::get_war_state_hint3() const		{	return	m_war_state_hint_str[2].c_str();		}
void   FactionState::set_war_state_hint3( const char* text )	{			m_war_state_hint_str[2]._set( text );}
const char* FactionState::get_war_state_hint4() const		{	return	m_war_state_hint_str[3].c_str();		}
void   FactionState::set_war_state_hint4( const char* text )	{			m_war_state_hint_str[3]._set( text );}
const char* FactionState::get_war_state_hint5() const		{	return	m_war_state_hint_str[4].c_str();		}
void   FactionState::set_war_state_hint5( const char* text )	{			m_war_state_hint_str[4]._set( text );}
