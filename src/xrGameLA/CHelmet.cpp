#include "stdafx.h"

#include "customoutfit.h"
#include "PhysicsShell.h"
#include "inventory_space.h"
#include "Inventory.h"
#include "Actor.h"
#include "game_cl_base.h"
#include "Level.h"
#include "BoneProtections.h"
#include "../Include/xrRender/Kinematics.h"
#include "player_hud.h"
#include "CHelmet.h"

CHelmet::CHelmet()
{
	m_baseSlot = HELMET_SLOT;
}

void CHelmet::Load(LPCSTR section)
{
	inherited::Load(section);

	m_fShowNearestEnemiesDistance	= READ_IF_EXISTS(pSettings, r_float, section, "nearest_enemies_show_dist", 0.0f);
}

//------------>------Upgrades------<-------------

bool CHelmet::install_upgrade_impl(LPCSTR section, bool test)
{
	bool result = inherited::install_upgrade_impl(section, test);

	result |= process_if_exists(section, "nearest_enemies_show_dist", &CInifile::r_float, m_fShowNearestEnemiesDistance, test);

	return result;
}