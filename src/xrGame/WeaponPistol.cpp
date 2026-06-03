#include "stdafx.h"
#include "WeaponPistol.h"
#include "Actor.h"

void CWeaponPistol::Load	(const char* section)
{
	inherited::Load		(section);

	m_sounds.LoadSound(section, "snd_close", "sndClose", false, m_eSoundClose);
}

void CWeaponPistol::PlayAnimHide()
{
	VERIFY(GetState()==eHiding);
	if (!iAmmoElapsed)
	{
		PlaySound			("sndClose", get_LastFP());
		PlayHUDMotion		(SetCurrentStateAnimation("anm_hide"), SetCurrentStateAnimation("anm_holster"), EHudMixType::eMixAll, GetState());
	} 
	else 
		inherited::PlayAnimHide();
}

void CWeaponPistol::UpdateSounds()
{
	if (Device.dwFrame == dwUpdateSounds_Frame)
		return;

	inherited::UpdateSounds();

	if (Device.dwFrame % 3 == 0)
		m_sounds.SetPosition("sndClose", get_LastFP());
}