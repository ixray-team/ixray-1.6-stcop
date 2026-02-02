#include "StdAfx.h"
#include "ActorHelmet.h"
#include "Actor.h"
#include "Inventory.h"
#include "BoneProtections.h"
#include "../Include/xrRender/Kinematics.h"

void CHelmet::Load(LPCSTR section)
{
	inherited::Load(section);

	m_fShowNearestEnemiesDistance = READ_IF_EXISTS(pSettings, r_float, section, "nearest_enemies_show_dist", 0.0f);
}

void CHelmet::OnMoveToSlot(const SInvItemPlace& previous_place)
{
	inherited::OnMoveToSlot(previous_place);
}

void CHelmet::OnMoveToRuck(const SInvItemPlace& previous_place)
{
	inherited::OnMoveToRuck(previous_place);
	if (m_pInventory != nullptr && (previous_place.type == eItemPlaceSlot))
	{
		CActor* pActor = H_Parent() ? H_Parent()->cast_actor() : nullptr;
		if (pActor != nullptr && pActor->GetNightVisionEffector())
		{
			pActor->GetNightVisionEffector()->SwitchNightVision(false);
		}
	}
}

bool CHelmet::install_upgrade_impl(LPCSTR section, bool test)
{
	bool result = inherited::install_upgrade_impl(section, test);

	result |= process_if_exists(section, "nearest_enemies_show_dist", &CInifile::r_float, m_fShowNearestEnemiesDistance, test);

	return result;
}
