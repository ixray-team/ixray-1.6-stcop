#include "StdAfx.h"
#include "Magazine.h"
#include "WeaponAmmo.h"

void CMagazine::Load(const char* section)
{
	inherited::Load(section);

	CurrentMagazineDesc = &SMagazineDesc::Registry::Get(section);
}

bool CMagazine::net_Spawn(CSE_Abstract* DC)
{
	bool Result = inherited::net_Spawn(DC);

	CSE_ALifeItemMagazine* E = smart_cast<CSE_ALifeItemMagazine*>(DC);

	U8Vec ammo_ids = E->m_AmmoIDs;

	for (u32 i = 0; i < (u32)ammo_ids.size(); i++)
	{
		u8 LocalAmmoType = ammo_ids[i];
		if (i >= Cartridges.size())
		{
			continue;
		}

		CCartridge& l_cartridge = *(Cartridges.begin() + i);
		if (LocalAmmoType == l_cartridge.m_LocalAmmoType)
		{
			continue;
		}

		l_cartridge.Load(MagazineDesc().AmmoTypes[LocalAmmoType].c_str(), LocalAmmoType);
	}

	return Result;
}

void CMagazine::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);

	P.w_u8(u8(Cartridges.size()));

	for (u32 i = 0; i < Cartridges.size(); i++)
	{
		CCartridge& l_cartridge = *(Cartridges.begin() + i);
		P.w_u8(l_cartridge.m_LocalAmmoType);
	}
}

void CMagazine::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
	u8 AmmoCount = P.r_u8();
	for (u32 i = 0; i < AmmoCount; i++)
	{
		u8 LocalAmmoType = P.r_u8();
		if (i >= Cartridges.size())
		{
			continue;
		}

		CCartridge& l_cartridge = *(Cartridges.begin() + i);
		if (LocalAmmoType == l_cartridge.m_LocalAmmoType)
		{
			continue;
		}

		l_cartridge.Load(MagazineDesc().AmmoTypes[LocalAmmoType].c_str(), LocalAmmoType);
	}
}

u32 CMagazine::Cost() const
{
	u32 AllCost = inherited::Cost();

	for (const auto& Cart : Cartridges)
	{
		AllCost += Cart.Cost();
	}

	return AllCost;
}

float CMagazine::Weight() const
{
	u32 AllWeight = inherited::Cost();

	for (const auto& Cart : Cartridges)
	{
		AllWeight += Cart.Weight();
	}

	return AllWeight;
}