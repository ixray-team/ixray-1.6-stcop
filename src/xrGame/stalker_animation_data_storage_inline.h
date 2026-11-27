////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_animation_data_storage_inline.h
//	Created 	: 13.10.2005
//  Modified 	: 13.10.2005
//	Author		: Dmitriy Iassenev
//	Description : Stalker animation data storage inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "ai_space.h"

IC	CStalkerAnimationDataStorage &stalker_animation_data_storage	()
{
	return ai().animation_data();
}
