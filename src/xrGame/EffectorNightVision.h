#pragma once

#include "HudSound.h"
#include "Actor.h"
#include "PostprocessAnimator.h"

class CUIGameCustom;

class CNightVisionEffector
{
	CActor*					m_pActor = nullptr;
	HUD_SOUND_COLLECTION	m_sounds;
	RStringVec				m_disabled_maps = {};
	bool					m_bNightVisionOn = false;
	bool ShaderNightVision = false;
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
	void UpdateMask(CUIGameCustom& GameUI);
};

struct TWeaponNightVision
{
private:
	bool m_bNightVisionOn = false;
	shared_str m_section;
	xr_unique_ptr<CPostprocessAnimator> ScopeAnimator;

public:
	void EndComponent();

	void Load(const shared_str& sect);
	void UpdateScopePostprocess(float Factor);
	void SwitchNightVision(bool vision_on);
	bool IsActive();
	bool GetStatus() const { return m_bNightVisionOn; }

private:
	ECS_COMPONENT(TWeaponNightVision)
		ECS_STRING(m_section.c_str(), "NightVision Section")
	ECS_END
};