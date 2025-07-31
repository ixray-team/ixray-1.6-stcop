#include "StdAfx.h"
#include "WeaponPistol.h"
#include "Actor.h"

CWeaponPistol::CWeaponPistol()
{
	m_eSoundClose		= ESoundTypes(SOUND_TYPE_WEAPON_RECHARGING);
	SetPending			(FALSE);
}

CWeaponPistol::~CWeaponPistol(void)
{
}

void CWeaponPistol::Load	(LPCSTR section)
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
		PlayHUDMotion		(SetCurrentStateAnimation("anm_hide"), TRUE, GetState());
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