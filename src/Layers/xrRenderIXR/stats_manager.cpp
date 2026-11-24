////////////////////////////////////////////////////////////////////////////
//	Created		: 29.07.2009
//	Author		: Armen Abroyan
//	Copyright (C) GSC Game World - 2009
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "stats_manager.h"

void stats_manager::increment_stats( u32 size, enum_stats_buffer_type type, _D3DPOOL location )
{
	if( g_dedicated_server )
		return;

	R_ASSERT( type >= 0 && type < enum_stats_buffer_type_COUNT );
	R_ASSERT( location >= 0 && location <= D3DPOOL_SCRATCH );
	memory_usage_summary [type][location]		+= size;
}
 
void stats_manager::increment_stats( u32 size, enum_stats_buffer_type type, _D3DPOOL location, void* buff_ptr )
{
	if( g_dedicated_server )
		return;

	R_ASSERT( buff_ptr != nullptr );
	R_ASSERT( type >= 0 && type < enum_stats_buffer_type_COUNT );
	R_ASSERT( location >= 0 && location <= D3DPOOL_SCRATCH );
	memory_usage_summary [type][location]		+= size;

#ifdef DEBUG
	stats_item new_item;
	new_item.buff_ptr	= buff_ptr;
	new_item.location	= location;
	new_item.type		= type;
	new_item.size		= size;
	
	m_buffers_list.push_back(new_item);
#endif
}

void stats_manager::increment_stats_rtarget( IRHISurface*		buff )
{
	if( g_dedicated_server )
		return;

	_D3DPOOL pool = D3DPOOL_MANAGED;
	u32 size = buff->GetHeight() * buff->GetWidth() * get_format_pixel_size(buff->GetFormat());
	increment_stats( size, enum_stats_buffer_type_rtarget, pool, buff );
}

void stats_manager::increment_stats_vb( IRHIBuffer* buff )
{
	if( g_dedicated_server )
		return;

	increment_stats( buff->GetSize(), enum_stats_buffer_type_vertex, D3DPOOL_MANAGED, buff );
}

void stats_manager::increment_stats_ib( IRHIBuffer*	buff )
{
	if( g_dedicated_server )
		return;

	increment_stats( buff->GetSize(), enum_stats_buffer_type_index, D3DPOOL_MANAGED, buff );
}

void stats_manager::decrement_stats_rtarget( IRHISurface*		buff )
{
	if( buff == nullptr || g_dedicated_server )
		return;

	buff->AddRef();
	int refcnt = 0;
	if( (refcnt = buff->Release()) > 1 )
		return;

	_D3DPOOL pool = D3DPOOL_MANAGED;
	u32 size = buff->GetHeight() * buff->GetWidth() * get_format_pixel_size(buff->GetFormat());
	decrement_stats( size, enum_stats_buffer_type_rtarget, pool, buff );

}

void stats_manager::decrement_stats_vb( IRHIBuffer* buff )
{
	if( buff == nullptr || g_dedicated_server )
		return;

	buff->AddRef();
	int refcnt = 0;
	if( (refcnt = buff->Release()) > 1 )
		return;

	decrement_stats( buff->GetSize(), enum_stats_buffer_type_vertex, D3DPOOL_MANAGED, buff );
}

void stats_manager::decrement_stats_ib( IRHIBuffer*	buff )
{	
	if( buff == nullptr || g_dedicated_server)
		return;

	buff->AddRef();
	int refcnt = 0;
	if( (refcnt = buff->Release()) > 1 )
		return;

	decrement_stats( buff->GetSize(), enum_stats_buffer_type_index, D3DPOOL_MANAGED, buff );
}

void stats_manager::decrement_stats( u32 size, enum_stats_buffer_type type, _D3DPOOL location )
{
	if( g_dedicated_server )
		return;

	R_ASSERT( type >= 0 && type < enum_stats_buffer_type_COUNT );
	R_ASSERT( location >= 0 && location <= D3DPOOL_SCRATCH );
	memory_usage_summary [type][location]		-= size;
}

void stats_manager::decrement_stats( u32 size, enum_stats_buffer_type type, _D3DPOOL location, void* buff_ptr )
{
	if( buff_ptr == nullptr || g_dedicated_server )
		return;

#ifdef DEBUG
	xr_vector<stats_item>::iterator			it = m_buffers_list.begin();
	xr_vector<stats_item>::const_iterator	en = m_buffers_list.end();
	bool find = false;
	for( ; it != en; ++it )
	{
		if( it->buff_ptr == buff_ptr )
		{
			// The pointers may coincide so this assertion may some times fail normally.
			//R_ASSERT ( it->type == type && it->location == location && it->size == size );
			m_buffers_list.erase(it);
			find = true;
			break;
		}
	}
	R_ASSERT( find );	//  "Specified buffer not fount in the buffers list. 
						//	The buffer may not incremented to stats or it already was removed"
#endif //DEBUG

	memory_usage_summary [type][location]		-= size;
}

stats_manager::~stats_manager ()
{
#ifdef DEBUG
	Msg		( "m_buffers_list.size() = %d", m_buffers_list.size() );
//	R_ASSERT( m_buffers_list.size() == 0);	//  Some buffers stats are not removed from the list.
#endif 
}
u32 get_format_pixel_size ( ERHI_FORMAT format )
{
	if( format >= ERHI_FORMAT::R32G32B32A32_TYPELESS && format <= ERHI_FORMAT::R32G32B32A32_SINT)
		return 16;
	else if( format >= ERHI_FORMAT::R32G32B32_TYPELESS && format <= ERHI_FORMAT::R32G32B32_SINT)
		return 12;
	else if( format >= ERHI_FORMAT::R16G16B16A16_TYPELESS && format <= ERHI_FORMAT::X32_TYPELESS_G8X24_UINT)
		return 8;
	else if( format >= ERHI_FORMAT::R10G10B10A2_TYPELESS && format <= ERHI_FORMAT::X24_TYPELESS_G8_UINT)
		return 4;
	else if( format >= ERHI_FORMAT::R8G8_TYPELESS && format <= ERHI_FORMAT::R16_SINT)
		return 2;
	else if( format >= ERHI_FORMAT::R8_TYPELESS && format <= ERHI_FORMAT::A8_UNORM)
		return 1;
	else
		// Do not consider extraordinary formats.
		return 0;
}
