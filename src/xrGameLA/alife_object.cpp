////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_object.cpp
//	Created 	: 27.10.2005
//  Modified 	: 27.10.2005
//	Author		: Dmitriy Iassenev
//	Description : ALife object class
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "xrServer_Objects_ALife.h"
#include "alife_simulator.h"
#include "xrServer_Objects_ALife_Items.h"

void CSE_ALifeObject::spawn_supplies		()
{
	spawn_supplies(*m_ini_string);
}

void CSE_ALifeObject::spawn_supplies		(LPCSTR ini_string)
{
	if (!ini_string)
		return;

	if (!xr_strlen(ini_string))
		return;

#pragma warning(push)
#pragma warning(disable:4238)
	CInifile					ini(
		&IReader				(
			(void*)(ini_string),
			xr_strlen(ini_string)
		),
		FS.get_path("$game_config$")->m_Path
	);
#pragma warning(pop)

	if (ini.section_exist("spawn")) {
		LPCSTR					N,V;
		float					p;
		for (u32 k = 0, j; ini.r_line("spawn",k,&N,&V); k++) {
			VERIFY				(xr_strlen(N));
	
			float f_cond						= 1.0f;
			bool bScope							= false;
			bool bSilencer						= false;
			bool bLauncher						= false;

			j					= 1;
			p					= 1.f;
			
			if (V && xr_strlen(V)) {
				string64			buf;
				j					= atoi(_GetItem(V, 0, buf));
				if (!j)		j		= 1;

				bScope				= (NULL!=strstr(V,"scope"));
				bSilencer			= (NULL!=strstr(V,"silencer"));
				bLauncher			= (NULL!=strstr(V,"launcher"));
				//probability
				if (NULL != strstr(V,"prob="))
					p				= (float)atof(strstr(V,"prob=")+5);
				if (fis_zero(p)) p	= 1.0f;
				if (NULL != strstr(V,"cond="))
					f_cond			= (float)atof(strstr(V,"cond=")+5);
			}
			spawn_item(N, j, p, f_cond, bScope, bLauncher, bSilencer);
		}
	}
}

CSE_Abstract* CSE_ALifeObject::spawn_item(LPCSTR section)
{
	return alife().spawn_item(section,o_Position,m_tNodeID,m_tGraphID,ID);
}

void CSE_ALifeObject::spawn_item(LPCSTR sect, int count, float prob, float cond, bool scope, bool launcher, bool silencer)
{
	for (u32 i = 0; i < count; ++i)
	{
		if (::Random.randF(1.f) < prob)
		{
			spawn_item(sect, cond, scope, launcher, silencer);
		}
	}
}

CSE_Abstract* CSE_ALifeObject::spawn_item(LPCSTR sect, float cond, bool scope, bool launcher, bool silencer)
{
	CSE_Abstract* E = spawn_item(sect);
	//подсоединить аддоны к оружию, если включены соответствующие флажки
	CSE_ALifeItemWeapon* W = smart_cast<CSE_ALifeItemWeapon*>(E);
	if (W) {
		if (W->m_scope_status == CSE_ALifeItemWeapon::eAddonAttachable)
			W->m_addon_flags.set(CSE_ALifeItemWeapon::eWeaponAddonScope, scope);
		if (W->m_silencer_status == CSE_ALifeItemWeapon::eAddonAttachable)
			W->m_addon_flags.set(CSE_ALifeItemWeapon::eWeaponAddonSilencer, silencer);
		if (W->m_grenade_launcher_status == CSE_ALifeItemWeapon::eAddonAttachable)
			W->m_addon_flags.set(CSE_ALifeItemWeapon::eWeaponAddonGrenadeLauncher, launcher);
	}
	CSE_ALifeInventoryItem* IItem = smart_cast<CSE_ALifeInventoryItem*>(E);
	if (IItem)
		IItem->m_fCondition = cond;
	return E;
}

bool CSE_ALifeObject::keep_saved_data_anyway() const
{
	return			(false);
}
