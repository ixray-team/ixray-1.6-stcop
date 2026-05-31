#pragma once

#include "ai/ai_monsters_anims.h"
#include "stalker_animation_names.h"

class CStalkerAnimationState
{
public:
	using MOVEMENT_ACTIONS = CAniCollection<CAniVector, CStalkerAnimationNames::ECollectionType::MovementAction>;
	using WEAPON_ACTIONS = CAniCollection<CAniVector, CStalkerAnimationNames::ECollectionType::WeaponAction>;
	using GLOBAL_ANIMATIONS = CAniCollection<CAniVector, CStalkerAnimationNames::ECollectionType::Global>;
	using WEAPON_ANIMATIONS = CAniCollection<WEAPON_ACTIONS, CStalkerAnimationNames::ECollectionType::Weapon>;
	using MOVEMENT_ANIMATIONS = CAniCollection<MOVEMENT_ACTIONS, CStalkerAnimationNames::ECollectionType::Movement>;

public:
	GLOBAL_ANIMATIONS m_global;
	WEAPON_ANIMATIONS m_torso;
	MOVEMENT_ANIMATIONS m_movement;
	CAniFVector<CStalkerAnimationNames::ECollectionType::InPlace> m_in_place;

public:
	CStalkerAnimationState() = default;
	CStalkerAnimationState(const CStalkerAnimationState& animations);
	virtual ~CStalkerAnimationState() = default;

	void Load(IKinematicsAnimated* kinematics, const char* base_name);
};