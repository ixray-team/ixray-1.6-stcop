////////////////////////////////////////////////////////////////////////////
//	Module 		: visual_memory_params.cpp
//	Created 	: 09.12.2004
//  Modified 	: 09.12.2004
//	Author		: Dmitriy Iassenev
//	Description : Visual memory parameters
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "visual_memory_params.h"
#include "memory_space.h"

void CVisionParameters::Load	(const char* section, bool not_a_stalker)
{
	m_transparency_threshold	= READ_IF_EXISTS(pSettings, r_float, section, "transparency_threshold", 0.f);
	m_still_visible_time		= READ_IF_EXISTS(pSettings,r_u32,section,"still_visible_time",0.f);

#ifndef USE_STALKER_VISION_FOR_MONSTERS
	if (!not_a_stalker)
		return;
#endif
	m_min_view_distance			= READ_IF_EXISTS(pSettings, r_float, section, "min_view_distance", 0.f);
	m_max_view_distance			= READ_IF_EXISTS(pSettings, r_float, section, "max_view_distance", 0.f);
	m_visibility_threshold		= READ_IF_EXISTS(pSettings, r_float, section, "visibility_threshold", 0.f);
	m_always_visible_distance	= READ_IF_EXISTS(pSettings, r_float, section, "always_visible_distance", 0.f);
	m_time_quant				= READ_IF_EXISTS(pSettings, r_float, section, "time_quant", 0.f);
	m_decrease_value			= READ_IF_EXISTS(pSettings, r_float, section, "decrease_value", 0.f);
	m_velocity_factor			= READ_IF_EXISTS(pSettings, r_float, section, "velocity_factor", 0.f);
	m_luminocity_factor			= READ_IF_EXISTS(pSettings, r_float, section, "luminocity_factor", 0.f);
}
