#pragma once

#include "HudSound.h"
#include "Actor.h"

class CNightVisionEffector
{
	CActor*					m_pActor = nullptr;
	HUD_SOUND_COLLECTION	m_sounds;
	RStringVec				m_disabled_maps = {};
	bool					m_bNightVisionOn = false;
public:

	enum EPlaySounds
	{
		eStartSound = 0,
		eStopSound,
		eIdleSound,
		eBrokeSound
	};

			CNightVisionEffector(CActor* actor);
			~CNightVisionEffector();
	void	Start(const shared_str& sect, bool play_sound = true);
	void	Stop(const float factor, bool play_sound = true);
	bool	IsActive();
	void	PlaySounds(EPlaySounds which);
	void	SwitchNightVision();
	void	SwitchNightVision(bool vision_on, bool use_sounds = true);
	bool	GetStatus() const { return m_bNightVisionOn; }
};

class CWeaponNightVision
{
	bool m_bNightVisionOn = false;
	shared_str m_section;
	CActor* m_pActor = nullptr;

public:
			CWeaponNightVision(const shared_str& sect, CActor* actor);
			~CWeaponNightVision();

	void	SwitchNightVision(bool vision_on);
	bool	IsActive();
	bool	GetStatus() const { return m_bNightVisionOn; }
};