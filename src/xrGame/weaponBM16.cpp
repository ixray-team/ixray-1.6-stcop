#include "StdAfx.h"
#include "weaponBM16.h"

void CWeaponBM16::Load(const char* section)
{
	m_bUseAltReloadSystem = pSettings->read_if_exists<bool>(section, "use_alt_reload_system", false);

	inherited::Load(section);
}

void CWeaponBM16::LoadSounds(const char* section)
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

		if (SoundExist(section, "snd_changeammo_1"))
		{
			m_sounds.LoadSound(section, "snd_changeammo_1", "sndChangeAmmo1", true, m_eSoundReload);
		}

		if (SoundExist(section, "snd_changeammo_2"))
		{
			m_sounds.LoadSound(section, "snd_changeammo_2", "sndChangeAmmo2", true, m_eSoundReload);
		}
	}
}

void CWeaponBM16::PlayReloadSound()
{
	if (m_bUseAltReloadSystem)
	{
		const char* sound = "sndReload";

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

		if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_jam) && IsMisfire())
		{
			sound = "sndReloadMis";
		}

		PlaySound(sound, get_LastFP());
	}
	else
	{
		if (m_eSoundsFlags.test(ESoundsFlags::sf_reload_jam) && IsMisfire())
		{
			PlaySound("sndReloadMis", get_LastFP());
		}
		else
		{
			if (GetAmmoElapsed() == 1)
			{
				if (IsChangeAmmoType() && m_sounds.FindSoundItem("sndChangeAmmo1", false))
				{
					PlaySound("sndChangeAmmo1", get_LastFP());
				}
				else
				{
					PlaySound("sndReload1", get_LastFP());
				}
			}
			else
			{
				if (GetAmmoElapsed() == 2 && IsChangeAmmoType() && m_sounds.FindSoundItem("sndChangeAmmo2", false))
				{
					PlaySound("sndChangeAmmo2", get_LastFP());
				}
				else
				{
					PlaySound("sndReload", get_LastFP());
				}
			}
		}
	}
}

shared_str CWeaponBM16::SetCurrentReloadAnimation()
{
	shared_str anim = "anm_reload";

	string64 new_suffix = {};

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
			if (IsMisfire() && !AddSuffixName(anim, "_jammed") && !AddSuffixName(anim, "_misfire") || !IsMisfire())
			{
				if (GetAmmoElapsed() == 1)
				{
					if (!IsChangeAmmoType() || !AddSuffixName(anim, "_ammochange"))
					{
						anim = "anm_reload_1";
					}
				}
				else if (!IsChangeAmmoType() || !AddSuffixName(anim, "_ammochange"))
				{
					anim = HudAnimationExist("anm_reload_2") ? "anm_reload_2" : "anm_reload";
				}
			}
		}

		AddSuffixName(anim, new_suffix);
	}
	else
	{
		if ((GetAmmoElapsed() == 1 || !HaveCartridgeInInventory(2)) && (m_set_next_ammoType_on_reload == undefined_ammo_type || m_ammoType == m_set_next_ammoType_on_reload))
		{
			anim = "anm_reload_1";
		}
		else
		{
			anim = HudAnimationExist("anm_reload_2", false) ? "anm_reload_2" : "anm_reload";
		}
	}

	return anim;
}

void CWeaponBM16::PlayAnimReload()
{
	VERIFY(GetState() == eReload);
	
	PlayHUDMotion(SetCurrentReloadAnimation(), EHudMixType::eMixAll, GetState());
}

shared_str CWeaponBM16::SetCurrentShootAnimation()
{
	shared_str anim = (HudAnimationExist("anm_shot_2") || HudAnimationExist("anm_shot_1")) ? "anm_shot" : "anm_shoot";
	string64 new_suffix = {};

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
		xr_sprintf(new_suffix, "%s_%d", *anim, iAmmoElapsed);
		anim = new_suffix;
	}

	return anim;
}

shared_str CWeaponBM16::SetCurrentStateAnimation(const shared_str& first_name)
{
	shared_str anim = first_name;

	string64 new_suffix = {};

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

		if (const CActor* pActor = Level().CurrentControlEntity()->cast_actor())
		{
			if (pActor->IsSafemode())
			{
				AddSuffixName(anim, "_safemode", new_suffix);
			}
		}

		AddSuffixName(anim, new_suffix);
	}
	else
	{
		xr_sprintf(new_suffix, "%s_%d", *anim, iAmmoElapsed);
		if (HudAnimationExist(new_suffix, false))
			anim = new_suffix;
	}

	return anim;
}

bool CWeaponBM16::HudAnimationExist(const shared_str& anim_name, bool only_for_actor)
{
	string128 new_name;
	xr_sprintf(new_name, "%s_%d", *anim_name, iAmmoElapsed);	

	bool has_anim = inherited::HudAnimationExist(new_name, only_for_actor);

	if (has_anim)
	{
		return has_anim;
	}

	return inherited::HudAnimationExist(anim_name, only_for_actor);
}

shared_str CWeaponBM16::SetCurrentAimAnimation()
{
	switch (m_magazine.size())
	{
	break;
	case 1: 
	{
		return HudAnimationExist("anm_zoomed_idle_1") ? "anm_zoomed_idle_1" : inherited::SetCurrentAimAnimation();
	}
	break;
	case 2: 
	{
		return HudAnimationExist("anm_zoomedidle_2") ? "anm_zoomedidle_2" : inherited::SetCurrentAimAnimation();
	}
	break;
	};
	return inherited::SetCurrentAimAnimation();
}
