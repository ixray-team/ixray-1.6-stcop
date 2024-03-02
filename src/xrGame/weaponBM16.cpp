#include "stdafx.h"
#include "weaponBM16.h"

CWeaponBM16::~CWeaponBM16()
{
}

void CWeaponBM16::Load	(LPCSTR section)
{
	inherited::Load		(section);
	m_sounds.LoadSound	(section, "snd_reload_1", "sndReload1", true, m_eSoundShot);
}

void CWeaponBM16::PlayReloadSound()
{
	if(m_magazine.size()==1)	
		PlaySound	("sndReload1",get_LastFP());
	else						
		PlaySound	("sndReload",get_LastFP());
}

void CWeaponBM16::PlayAnimShoot()
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	std::string anm_name = isGuns ? "anm_shoot" : "anm_shot";

	PlayHUDMotion(anm_name, FALSE, this, GetState());
}

void CWeaponBM16::PlayAnimReload()
{
	bool b_both = HaveCartridgeInInventory(2);

	VERIFY(GetState()==eReload);

	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	std::string anm_name = "anm_reload";
	if (!isGuns)
	{
		if ((m_magazine.size() == 1 || !b_both) && (m_set_next_ammoType_on_reload == undefined_ammo_type || m_ammoType == m_set_next_ammoType_on_reload))
			anm_name += "_1";
		else
			anm_name += "_2";
	}

	PlayHUDMotion(anm_name, TRUE, this, GetState(), isGuns);
}

std::string CWeaponBM16::NeedAddSuffix(std::string M)
{
	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];

	std::string new_name = M;

	if (IsChangeAmmoType() && m_magazine.size() != 0)
	{
		switch (m_magazine.size())
		{
			case 1:
				new_name = AddSuffixName(new_name, "_ammochange", "_1");
			break;
			case 2:
				new_name = AddSuffixName(new_name, "_ammochange", "_2");
			break;
		}
	}

	if (IsZoomed())
	{
		switch (m_magazine.size())
		{
		case 0:
			new_name = AddSuffixName(new_name, "_aim", "_0");
			break;
		case 1:
			new_name = AddSuffixName(new_name, "_aim", "_1");
			break;
		case 2:
			new_name = AddSuffixName(new_name, "_aim", "_2");
			break;
		}
	}

	if (IsMisfire())
	{
		if (isGuns)
		{
			switch (m_magazine.size())
			{
				case 0:
					new_name = AddSuffixName(new_name, "_jammed", "_0");
				break;
				case 1:
					new_name = AddSuffixName(new_name, "_jammed", "_1");
				break;
				case 2:
					new_name = AddSuffixName(new_name, "_jammed", "_2");
				break;
			}
		}
		else
		{
			switch (m_magazine.size())
			{
				case 0:
					new_name = AddSuffixName(new_name, "_misfire", "_0");
				break;
				case 1:
					new_name = AddSuffixName(new_name, "_misfire", "_1");
				break;
				case 2:
					new_name = AddSuffixName(new_name, "_misfire", "_2");
				break;
			}
		}

		bMisfireReload = true;
	}

	switch (m_magazine.size())
	{
		case 0:
			new_name = AddSuffixName(new_name, "_0");
		break;
		case 1:
			new_name = AddSuffixName(new_name, "_1");
		break;
		case 2:
			new_name = AddSuffixName(new_name, "_2");
		break;
	}

	return new_name;
}