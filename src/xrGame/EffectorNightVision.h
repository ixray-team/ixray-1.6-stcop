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
	void	Start(const shared_str& sect, bool play_sound, const Fcolor& color);
	void	Stop(const float factor, bool play_sound = true);
	bool	IsActive();
	void	PlaySounds(EPlaySounds which);
	void	SwitchNightVision();
	void	SwitchNightVision(bool vision_on, bool use_sounds = true);
	bool	GetStatus() const { return m_bNightVisionOn; }
};

struct TWeaponNightVision
{
private:
	bool m_bNightVisionOn = false;
	shared_str m_section;

public:
	void EndComponent();

	void Load(const shared_str& sect);
	void SwitchNightVision(bool vision_on);
	bool IsActive();
	bool GetStatus() const { return m_bNightVisionOn; }

private:
	ECS_COMPONENT(TWeaponNightVision)
		ECS_STRING(m_section.c_str(), "NightVision Section")
	ECS_END
};