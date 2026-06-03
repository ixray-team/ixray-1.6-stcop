#include "stdafx.h"
#include "ActorHelmet.h"
#include "Actor.h"
#include "Inventory.h"
#include "BoneProtections.h"
#include "../Include/xrRender/Kinematics.h"
#include "Torch.h"

void CHelmet::Load(const char* section)
{
	inherited::Load(section);

	m_fShowNearestEnemiesDistance = pSettings->read_if_exists<float>(section,"nearest_enemies_show_dist",0.0f);
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

		static const bool TorchOnlyOutfit = EngineExternal()[EEngineExternalGame::EnableTorchOnlyInOutfit];

		if (TorchOnlyOutfit)
		{
			CTorch* pTorch = static_cast<CTorch*>(pActor->inventory().ItemFromSlot(TORCH_SLOT));
			if (pTorch != nullptr)
			{
				pTorch->Switch(false);
			}
		}
	}
}

bool CHelmet::install_upgrade_impl(const char* section, bool test)
{
	bool result = inherited::install_upgrade_impl(section, test);

	result |= process_if_exists(section, "nearest_enemies_show_dist", m_fShowNearestEnemiesDistance, test);

	return result;
}

bool CHelmet::can_be_attached() const
{
	CObject* h_parent = const_cast<CObject*>(H_Parent());
	if (const CActor* pA = h_parent != nullptr ? h_parent->cast_actor() : nullptr)
	{
		return pA->inventory().InSlot(this);
	}

	return true;
}