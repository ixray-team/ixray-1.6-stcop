#include "StdAfx.h"
#include "weaponBM16.h"

CWeaponBM16::~CWeaponBM16()
{
}

void CWeaponBM16::Load(LPCSTR section)
{
	m_bUseAltReloadSystem = READ_IF_EXISTS(pSettings, r_bool, section, "use_alt_reload_system", false);

	inherited::Load(section);
}

void CWeaponBM16::LoadSounds(LPCSTR section)
{
	inherited::LoadSounds(section);

	if (m_bUseAltReloadSystem)
	{
		m_sounds.LoadSound(section, "snd_reload_only", "sndReloadOnly", true, m_eSoundReload);
		m_sounds.LoadSound(section, "snd_reload_only_ammochange", "sndReloadOnlyAmmochange", true, m_eSoundReload);
		m_sounds.LoadSound(section, "snd_changecartridgetype_one", "sndChangeCartridgeTypeOne", true, m_eSoundReload);
		m_sounds.LoadSound(section, "snd_changecartridgetype_only", "sndChangeCartridgeTypeOnly", true, m_eSoundReload);
		m_sounds.LoadSound(section, "snd_changecartridgetype_one_only", "sndChangeCartridgeTypeOneOnly", true, m_eSoundReload);
	}
	else
	{
		m_sounds.LoadSound(section, "snd_reload_1", "sndReload1", true, m_eSoundShot);
	}
}

void CWeaponBM16::PlayReloadSound()
{
	if (m_bUseAltReloadSystem)
	{
		LPCSTR sound = "sndReload";

		if (GetAmmoElapsed() == 0)
		{
			sound = "sndReloadEmpty";
		}

		u8 get_target_ammotype = GetTargetAmmoType();

		if (GetAmmoCount(get_target_ammotype) < 2 && !unlimited_ammo())
		{
			if (!IsChangeAmmoType())
			{
				sound = "sndReloadOnly";
			}
			else
			{
				sound = "sndReloadOnlyAmmochange";
			}
		}

		if (IsChangeAmmoType() && GetAmmoElapsed() > 0)
		{
			if (GetAmmoCount(get_target_ammotype) < 2 && !unlimited_ammo())
			{
				if (GetAmmoElapsed() == 1)
				{
					sound = "sndChangeCartridgeTypeOneOnly";
				}
				else
				{
					sound = "sndChangeCartridgeTypeOnly";
				}
			}
			else
			{
				if (GetAmmoElapsed() == 1)
				{
					sound = "sndChangeCartridgeTypeOne";
				}
				else
				{
					sound = "sndChangeCartridgeType";
				}
			}
		}

		if (IsMisfire())
		{
			sound = "sndReloadMis";
		}

		PlaySound(sound, get_LastFP());
	}
	else
	{
		if ((GetAmmoElapsed() == 1 || !HaveCartridgeInInventory(2)) && (m_set_next_ammoType_on_reload == undefined_ammo_type || m_ammoType == m_set_next_ammoType_on_reload))
		{
			PlaySound("sndReload1", get_LastFP());
		}
		else
		{
			PlaySound("sndReload", get_LastFP());
		}
	}
}

shared_str CWeaponBM16::SetCurrentReloadAnimation()
{
	shared_str anim = "anm_reload";

	string16 new_suffix = {};

	if (ParentIsActor())
	{
		xr_sprintf(new_suffix, "_%d", iAmmoElapsed);

		m_iAmmoCountToReload = iMagazineSize;

		if (m_bUseAltReloadSystem)
		{
			if (IsMisfire() && !AddSuffixName(anim, "_jammed") || !IsMisfire())
			{
				u8 get_target_ammotype = GetTargetAmmoType();

				if (GetAmmoElapsed() == 0)
				{
					if (GetAmmoCount(get_target_ammotype) < 2 && !unlimited_ammo())
					{
						if (!IsChangeAmmoType())
						{
							AddSuffixName(anim, "_only");
						}
						else
						{
							AddSuffixName(anim, "_only_ammochange");
						}

						m_iAmmoCountToReload = 1;
					}
				}
				else if (IsChangeAmmoType())
				{
					AddSuffixName(anim, "_ammochange");

					if (GetAmmoCount(get_target_ammotype) < 2 && !unlimited_ammo())
					{
						AddSuffixName(anim, "_only");
						m_iAmmoCountToReload = 1;
					}
				}
			}
		}
		else
		{
			if (IsMisfire() && !AddSuffixName(anim, "_jammed") || !IsMisfire())
			{
				if ((GetAmmoElapsed() == 1 || !HaveCartridgeInInventory(2)) && (m_set_next_ammoType_on_reload == undefined_ammo_type || m_ammoType == m_set_next_ammoType_on_reload))
				{
					anim = "anm_reload_1";
				}
				else
				{
					anim = "anm_reload_2";
				}
			}
		}

		AddSuffixName(anim, new_suffix);
	}
	else
	{
		xr_sprintf(new_suffix, "%s%s", *anim, "_2");
		anim = new_suffix;
	}

	return anim;
}

void CWeaponBM16::PlayAnimReload()
{
	UpdateAmmoBones(m_ammo_bones_mag, iAmmoElapsed, m_ammoType);

	VERIFY(GetState() == eReload);
	
	PlayHUDMotion(SetCurrentReloadAnimation(), TRUE, GetState());
}

shared_str CWeaponBM16::SetCurrentShootAnimation()
{
	shared_str anim = (HudAnimationExist("anm_shot_2") || HudAnimationExist("anm_shot_1")) ? "anm_shot" : "anm_shoot";
	string16 new_suffix = {};

	if (ParentIsActor())
	{
		xr_sprintf(new_suffix, "_%d", iAmmoElapsed);

		if (IsMisfire() && IsZoomed())
		{
			AddSuffixName(anim, "_aim_misfire", new_suffix);
			AddSuffixName(anim, "_aim_jammed", new_suffix);
		}

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

	if (ParentIsActor())
	{
		xr_sprintf(new_suffix, "_%d", iAmmoElapsed);

		if (IsMisfire() && IsZoomed())
		{
			AddSuffixName(anim, "_aim_misfire", new_suffix);
			AddSuffixName(anim, "_aim_jammed", new_suffix);
		}

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