////////////////////////////////////////////////////////////////////////////
//	Module 		: visual_memory_params.cpp
//	Created 	: 09.12.2004
//  Modified 	: 09.12.2004
//	Author		: Dmitriy Iassenev
//	Description : Visual memory parameters
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "visual_memory_params.h"
#include "memory_space.h"

void CVisionParameters::Load	(const char* section, bool not_a_stalker)
{
	m_transparency_threshold	= pSettings->read_if_exists<float>(section, "transparency_threshold", 0.f);
	m_still_visible_time		= pSettings->read_if_exists<u32>(section,"still_visible_time",0.f);
	m_vegetation_min_height		= pSettings->read_if_exists<float>(section, "vegetation_min_height", 0.8f);
	m_vegetation_opaque_distance= pSettings->read_if_exists<float>(section, "vegetation_opaque_distance", 2.5f);
	m_vegetation_sample_step	= pSettings->read_if_exists<float>(section, "vegetation_sample_step", 1.0f);

#ifndef USE_STALKER_VISION_FOR_MONSTERS
	if (!not_a_stalker)
		return;
#endif
	m_min_view_distance			= pSettings->read_if_exists<float>(section, "min_view_distance", 0.f);
	m_max_view_distance			= pSettings->read_if_exists<float>(section, "max_view_distance", 0.f);
	m_visibility_threshold		= pSettings->read_if_exists<float>(section, "visibility_threshold", 0.f);
	m_always_visible_distance	= pSettings->read_if_exists<float>(section, "always_visible_distance", 0.f);
	m_time_quant				= pSettings->read_if_exists<float>(section, "time_quant", 0.f);
	m_decrease_value			= pSettings->read_if_exists<float>(section, "decrease_value", 0.f);
	m_velocity_factor			= pSettings->read_if_exists<float>(section, "velocity_factor", 0.f);
	m_luminocity_factor			= pSettings->read_if_exists<float>(section, "luminocity_factor", 0.f);
}
