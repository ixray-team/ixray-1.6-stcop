#include "stdafx.h"
#include "stalker_animation_state.h"
#include "object_broker.h"
#include "../Include/xrRender/Kinematics.h"
#include "AnimationNames.h"

CStalkerAnimationState::CStalkerAnimationState(const CStalkerAnimationState& stalker_animation_state)
{
	clone(stalker_animation_state.m_in_place, m_in_place);
}

void CStalkerAnimationState::Load(IKinematicsAnimated* kinematics, const char* base_name)
{
	string256 S;
	m_global.Load(kinematics, base_name);
	
	const auto& TorsoCollect = GAnimationNames.GetCollection(CStalkerAnimationNames::ECollectionType::Torso);
	m_torso.Load(kinematics, xr_strconcat(S, base_name, *TorsoCollect[0]));

	m_movement.Load(kinematics, base_name);
	m_in_place.Load(kinematics, base_name);
}