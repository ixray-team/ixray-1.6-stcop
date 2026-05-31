////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_animation_data.h
//	Created 	: 13.10.2005
//  Modified 	: 13.10.2005
//	Author		: Dmitriy Iassenev
//	Description : Stalker animation data
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "stalker_animation_state.h"
#include "stalker_animation_names.h"

class CStalkerAnimationData 
{
public:
	using GLOBAL_ANIMATIONS = CAniCollection<CStalkerAnimationState::WEAPON_ACTIONS, CStalkerAnimationNames::ECollectionType::Weapon>;
	using PART_ANIMATIONS = CAniCollection<CStalkerAnimationState, CStalkerAnimationNames::ECollectionType::State>;
	using HEAD_ANIMATIONS = CAniFVector<CStalkerAnimationNames::ECollectionType::Head>;

public:
	PART_ANIMATIONS		m_part_animations;
	HEAD_ANIMATIONS		m_head_animations;
	GLOBAL_ANIMATIONS	m_global_animations;

public:
	CStalkerAnimationData	(IKinematicsAnimated *skeleton_animated);
};
