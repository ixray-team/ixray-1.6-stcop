#include "stdafx.h"
#include "weaponBM16.h"

CWeaponBM16::~CWeaponBM16()
{
}

void CWeaponBM16::Load	(LPCSTR section)
{
	inherited::Load		(section);

	const static bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if (!isGuns)
		m_sounds.LoadSound	(section, "snd_reload_1", "sndReload1", true, m_eSoundShot);
	else
	{
		m_sounds.LoadSound(section, "snd_reload_only", "sndReloadOnly", true, m_eSoundReload);
		m_sounds.LoadSound(section, "snd_reload_only_ammochange", "sndReloadOnlyAmmochange", true, m_eSoundReload);
		m_sounds.LoadSound(section, "snd_changecartridgetype_one", "sndChangeCartridgeTypeOne", true, m_eSoundReload);
		m_sounds.LoadSound(section, "snd_changecartridgetype_only", "sndChangeCartridgeTypeOnly", true, m_eSoundReload);
		m_sounds.LoadSound(section, "snd_changecartridgetype_one_only", "sndChangeCartridgeTypeOneOnly", true, m_eSoundReload);
	}
}

void CWeaponBM16::PlayReloadSound()
{
	const static bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if (isGuns)
	{
		LPCSTR sound = "sndReload";

		if (m_magazine.size() == 0)
			sound = "sndReloadEmpty";

		if (GetAmmoCount(GetAmmoTypeToReload()) < 2)
		{
			if (m_set_next_ammoType_on_reload == undefined_ammo_type)
				sound = "sndReloadOnly";
			else
				sound = "sndReloadOnlyAmmochange";
		}

		if (m_set_next_ammoType_on_reload != undefined_ammo_type)
		{
			if (GetAmmoCount(GetAmmoTypeToReload()) < 2)
			{
				if (iAmmoElapsed == 1)
					sound = "sndChangeCartridgeTypeOneOnly";
				else
					sound = "sndChangeCartridgeTypeOnly";
			}
			else
			{
				if (iAmmoElapsed == 1)
					sound = "sndChangeCartridgeTypeOne";
				else
					sound = "sndChangeCartridgeType";
			}
		}

		if (IsMisfire())
			sound = "sndReloadJammed";

		PlaySound(sound, get_LastFP());
	}
	else
	{
		if (m_magazine.size() == 1)
			PlaySound("sndReload1", get_LastFP());
		else
			PlaySound("sndReload", get_LastFP());
	}
}

void CWeaponBM16::PlayAnimShoot()
{
	const static bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	xr_string anm_name = isGuns ? "anm_shoot" : "anm_shot";

	PlayHUDMotion(anm_name, NeedShootMix(), GetState());
}

void CWeaponBM16::PlayAnimReload()
{
	VERIFY(GetState()==eReload);

	const static bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	xr_string anm_name = "anm_reload";
	if (!isGuns)
	{
		if ((m_magazine.size() == 1 || !HaveCartridgeInInventory(2)) && (m_set_next_ammoType_on_reload != undefined_ammo_type || m_ammoType == m_set_next_ammoType_on_reload))
			anm_name += "_1";
		else
			anm_name += "_2";
	}
	else
	{
		if (!IsMisfire())
		{
			ammo_cnt_to_reload = 2;

			if (m_magazine.size() <= 0)
			{
				if (GetAmmoCount(GetAmmoTypeToReload()) < 2)
				{
					if (m_set_next_ammoType_on_reload == undefined_ammo_type)
						anm_name += "_only";
					else
						anm_name += "_only_ammochange";

					ammo_cnt_to_reload = 1;
				}
			}
			else if (m_set_next_ammoType_on_reload != undefined_ammo_type)
			{
				anm_name += "_ammochange";
				if (GetAmmoCount(GetAmmoTypeToReload()) < 2)
				{
					anm_name += "_only";
					ammo_cnt_to_reload = 1;
				}
			}
		}
	}

	PlayHUDMotion(anm_name, TRUE, GetState(), false, isGuns);
	MakeLockByConfigParam("lock_time_start_" + GetActualCurrentAnim(), false, {CHudItem::TAnimationEffector(this, &CWeaponBM16::OnAmmoTimer)});
}

xr_string CWeaponBM16::NeedAddSuffix(const xr_string& M)
{
	xr_string new_name = M;

	if ((Actor()->IsActorSuicideNow() || Actor()->IsSuicideInreversible()) && READ_IF_EXISTS(pSettings, r_bool, HudSection(), "custom_suicide_shot", false))
		new_name = AddSuffixName(new_name, "_suicide", "_" + xr_string::ToString(iAmmoElapsed));

	if (IsZoomed())
		new_name = AddSuffixName(new_name, "_aim", "_" + xr_string::ToString(iAmmoElapsed));

	if (IsMisfire())
	{
		const static bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
		new_name = AddSuffixName(new_name, isGuns ? "_jammed" : "_misfire", "_" + xr_string::ToString(iAmmoElapsed));
	}

	new_name = AddSuffixName(new_name, "_" + xr_string::ToString(iAmmoElapsed));

	return new_name;
}