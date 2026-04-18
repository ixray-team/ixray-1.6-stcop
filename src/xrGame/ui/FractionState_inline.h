////////////////////////////////////////////////////////////////////////////
//	Module 		: FractionState_inline.h
//	Created 	: 23.01.2008
//  Modified 	: 23.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Fraction War state class
////////////////////////////////////////////////////////////////////////////

#pragma once

int    FractionState::get_actor_goodwill() const
{
	return m_actor_goodwill;
}
void   FractionState::set_actor_goodwill( int gw )
{
	m_actor_goodwill = gw;
}
const char* FractionState::get_fraction_id() const
{
	return m_id.c_str();
}
void   FractionState::set_fraction_id( const char* id )
{
	m_id._set( id );
}
void   FractionState::set_fraction_id2( shared_str const& id )
{
	m_id._set( id );
}
const char* FractionState::get_name() const
{
	return m_name.c_str();
}
void   FractionState::set_name( const char* name )
{
	m_name._set( name );
}
const char* FractionState::get_icon() const
{
	return m_icon.c_str();
}
void   FractionState::set_icon( const char* icon )
{
	m_icon._set( icon );
}

const char* FractionState::get_icon_big() const
{
	return m_icon_big.c_str();
}
void   FractionState::set_icon_big( const char* icon_b )
{	m_icon_big._set( icon_b );
}

const char* FractionState::get_target() const
{
	return m_target.c_str();
}
void   FractionState::set_target( const char* target )
{
	m_target._set( target );
}

const char* FractionState::get_target_desc() const
{
	return m_target_desc.c_str();
}
void   FractionState::set_target_desc( const char* target_desc )
{
	m_target_desc._set( target_desc );
}

const char* FractionState::get_location() const
{
	return m_location.c_str();
}
void   FractionState::set_location( const char* location )
{
	m_location._set( location );
}
