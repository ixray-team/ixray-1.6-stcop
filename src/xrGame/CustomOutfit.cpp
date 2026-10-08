#include "StdAfx.h"

#include "CustomOutfit.h"
#include "../xrPhysics/PhysicsShell.h"
#include "inventory_space.h"
#include "Inventory.h"
#include "Actor.h"
#include "game_cl_base.h"
#include "Level.h"
#include "BoneProtections.h"
#include "../Include/xrRender/Kinematics.h"
#include "player_hud.h"
#include "ActorHelmet.h"
#include "UIGameCustom.h"
#include "UIActorMenu.h"

void CCustomOutfit::Load(const char* section)
{
	inherited::Load(section);
	CurrentOutfitDesc = &SCustomOutfitDesc::Registry::Get(section);

	m_HitTypeProtection[ALife::eHitTypeFireWound]	= READ_IF_EXISTS(pSettings, r_float, section,"fire_wound_protection", 0.f);

	m_additional_weight = pSettings->r_float(section, "additional_inventory_weight");
	m_additional_weight2 = pSettings->r_float(section, "additional_inventory_weight2");

	m_artefact_count = READ_IF_EXISTS(pSettings, r_u32, section, "artefact_count", 0);
}

void CCustomOutfit::OnMoveToSlot(const SInvItemPlace& prev)
{
	if (m_pInventory)
	{
		CActor* pActor = H_Parent() ? H_Parent()->cast_actor() : nullptr;
		if (pActor)
		{
			ApplySkinModel(pActor, true, false);
			PIItem pHelmet = pActor->inventory().ItemFromSlot(HELMET_SLOT);
			if (pHelmet != nullptr && !IsHelmetAvailable())
			{
				pActor->inventory().Ruck(pHelmet, false);
			}
		}
	}
}

void CCustomOutfit::OnMoveToRuck(const SInvItemPlace& prev)
{
	if (m_pInventory != nullptr && prev.type == eItemPlaceSlot)
	{
		CActor* pActor = H_Parent() ? H_Parent()->cast_actor() : nullptr;
		if (pActor)
		{
			ApplySkinModel(pActor, false, false);
			if (pActor->GetNightVisionEffector() && !IsHelmetAvailable())
			{
				pActor->GetNightVisionEffector()->SwitchNightVision(false);
			}

			static const bool TorchOnlyOutfit = EngineExternal()[EEngineExternalGame::EnableTorchOnlyInOutfit];

			if (TorchOnlyOutfit && !IsHelmetAvailable())
			{
				CTorch* pTorch = static_cast<CTorch*>(pActor->inventory().ItemFromSlot(TORCH_SLOT));
				if (pTorch != nullptr)
				{
					pTorch->Switch(false);
				}
			}
		}
	}
}

bool CCustomOutfit::install_upgrade_impl(const char* section, bool test)
{
	bool result = inherited::install_upgrade_impl(section, test);

	result |= process_if_exists(section, "artefact_count", m_artefact_count, test);

	if (m_boneProtection->m_hitFracType == SBoneProtections::HitFractionActorCS ||
		m_boneProtection->m_hitFracType == SBoneProtections::HitFractionActorCOP)
	{
		result |= process_if_exists(section, "hit_fraction_actor", m_boneProtection->m_fHitFrac, test);
	}

	result |= process_if_exists(section, "additional_inventory_weight", m_additional_weight, test);
	result |= process_if_exists(section, "additional_inventory_weight2", m_additional_weight2, test);

	return result;
}

bool CCustomOutfit::BonePassBullet(u16 boneID)
{
	return m_boneProtection->getBonePassBullet(boneID);
}

void CCustomOutfit::ApplySkinModel(CActor* pActor, bool bDress, bool bHUDOnly)
{
	const SCustomOutfitDesc& Desc = OutfitDesc();

	if (Desc.isDisableChangeSkin)
	{
		return;
	}

	if (bDress)
	{
		if (!bHUDOnly && Desc.m_ActorVisual.size())
		{
			shared_str NewVisual = nullptr;
			char* TeamSection = Game().getTeamSection(pActor->g_Team());
			if (TeamSection)
			{
				if (pSettings->line_exist(TeamSection, *cNameSect()))
				{
					NewVisual = pSettings->r_string(TeamSection, *cNameSect());
					string256 SkinName;

					xr_strcpy(SkinName, pSettings->r_string("mp_skins_path", "skin_path"));
					xr_strcat(SkinName, *NewVisual);
					xr_strcat(SkinName, ".ogf");
					NewVisual._set(SkinName);
				}
			}
			if (!NewVisual.size())
			{
				NewVisual = Desc.m_ActorVisual;
			}

			pActor->ChangeVisual(NewVisual);
		}


		if (pActor == Level().CurrentViewEntity())
		{
			if (Desc.m_character_portrait.size() > 0)
			{
				pActor->SetIcon(Desc.m_character_portrait, true);
				if (auto current_ui = CurrentGameUI())
				{
					if (current_ui->ActorMenu() && current_ui->ActorMenu()->IsShown())
					{
						current_ui->ActorMenu()->ReloadActorInfo();
					}
				}
			}

			g_player_hud->NextHUDSect = Desc.PlayerHudSection;
			g_player_hud->m_need_reload = false;
		}
	}
	else
	{
		if (!bHUDOnly && Desc.m_ActorVisual.size())
		{
			pActor->SetIcon("", true);
			if (auto current_ui = CurrentGameUI())
			{
				if (current_ui->ActorMenu() && current_ui->ActorMenu()->IsShown())
				{
					current_ui->ActorMenu()->ReloadActorInfo();
				}
			}
			shared_str DefVisual = pActor->GetDefaultVisualOutfit();
			if (DefVisual.size())
			{
				pActor->ChangeVisual(DefVisual);
			};
		}

		if (pActor == Level().CurrentViewEntity())
		{
			g_player_hud->NextHUDSect = 0;
			g_player_hud->m_need_reload = false;
		}
	}

}

u32	CCustomOutfit::ef_equipment_type() const
{
	return OutfitDesc().m_ef_equipment_type;
}

float CCustomOutfit::GetPowerLoss()
{
	// Hit fraction and power loss are unrelated,
	// but it's the only way we can distinguish between SOC/CS and COP.
	// Sorry.
	if (m_boneProtection->m_hitFracType != SBoneProtections::HitFractionActorCOP)
	{
		if (m_fPowerLoss < 1 && GetCondition() <= 0)
		{
			return 1.0f;
		}
	}
	return m_fPowerLoss;
}
