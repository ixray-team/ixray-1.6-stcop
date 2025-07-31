#include "StdAfx.h"
#include "weaponBM16.h"

CWeaponBM16::~CWeaponBM16()
{
}

void CWeaponBM16::Load(LPCSTR section)
{
	inherited::Load(section);
}

void CWeaponBM16::LoadSounds(LPCSTR section)
{
	inherited::LoadSounds(section);

	m_sounds.LoadSound(section, "snd_reload_1", "sndReload1", true, m_eSoundShot);
}

void CWeaponBM16::PlayReloadSound()
{
	if (m_magazine.size() == 1)	
		PlaySound	("sndReload1",get_LastFP());
	else						
		PlaySound	("sndReload",get_LastFP());
}

void CWeaponBM16::PlayAnimReload()
{
	UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, m_ammoType);
	bool b_both = HaveCartridgeInInventory(2);

	VERIFY(GetState()==eReload);
	
	if ((m_magazine.size() == 1 || !b_both) && (m_set_next_ammoType_on_reload == undefined_ammo_type || m_ammoType == m_set_next_ammoType_on_reload))
		PlayHUDMotion("anm_reload_1", TRUE, GetState());
	else
		PlayHUDMotion("anm_reload_2", TRUE, GetState());
}

shared_str CWeaponBM16::SetCurrentShootAnimation()
{
	shared_str anim = (HudAnimationExist("anm_shot_2") || HudAnimationExist("anm_shot_1")) ? "anm_shot" : "anm_shoot";
	string16 new_suffix = {};

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		xr_sprintf(new_suffix, "_%d", iAmmoElapsed);

		if (IsZoomed())
		{
			AddSuffixName(anim, "_aim", new_suffix);
		}

		if (IsMisfire())
		{
			AddSuffixName(anim, "_misfire", new_suffix);
			AddSuffixName(anim, "_jammed", new_suffix);
		}

		AddSuffixName(anim, new_suffix);
	}
	else
	{
		xr_sprintf(new_suffix, "%s%s", anim.c_str(), "_2");
		anim = new_suffix;
	}

	return anim;
}

shared_str CWeaponBM16::SetCurrentStateAnimation(const shared_str& first_name)
{
	shared_str anim = first_name;

	string16 new_suffix = {};

	if (H_Parent() && H_Parent() == Level().CurrentControlEntity())
	{
		xr_sprintf(new_suffix, "_%d", iAmmoElapsed);

		if (IsZoomed())
		{
			AddSuffixName(anim, "_aim", new_suffix);
		}

		if (IsMisfire())
		{
			AddSuffixName(anim, "_misfire", new_suffix);
			AddSuffixName(anim, "_jammed", new_suffix);
		}

		AddSuffixName(anim, new_suffix);
	}
	else
	{
		xr_sprintf(new_suffix, "%s%s", anim.c_str(), "_2");
		anim = new_suffix;
	}

	return anim;
}

bool CWeaponBM16::HudAnimationExist(const shared_str& anim_name)
{
	string128 new_name;
	xr_sprintf(new_name, "%s_%d", *anim_name, iAmmoElapsed);	

	bool has_anim = inherited::HudAnimationExist(new_name);

	if (has_anim)
	{
		return has_anim;
	}

	return inherited::HudAnimationExist(anim_name);
}