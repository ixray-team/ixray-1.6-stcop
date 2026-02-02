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

void CCustomOutfit::Load(LPCSTR section)
{
	inherited::Load(section);
	isDisableChangeSkin = READ_IF_EXISTS(pSettings, r_bool, section, "forbid_change_skin", false);
	
	m_HitTypeProtection[ALife::eHitTypeFireWound]	= READ_IF_EXISTS(pSettings, r_float, section,"fire_wound_protection", 0.f);
 
	if (pSettings->line_exist(section, "actor_visual"))
	{
		m_ActorVisual = pSettings->r_string(section, "actor_visual");
	}

	m_ef_equipment_type = pSettings->r_u32(section, "ef_equipment_type");

	m_additional_weight = pSettings->r_float(section, "additional_inventory_weight");
	m_additional_weight2 = pSettings->r_float(section, "additional_inventory_weight2");

	m_full_icon_name = pSettings->r_string(section, "full_icon_name");
	m_artefact_count = READ_IF_EXISTS(pSettings, r_u32, section, "artefact_count", 0);
	bIsHelmetAvaliable = !!READ_IF_EXISTS(pSettings, r_bool, section, "helmet_avaliable", true);

	IsExo = READ_IF_EXISTS(pSettings, r_bool, section, "is_exo", false);
	IsExoProto = READ_IF_EXISTS(pSettings, r_bool, section, "is_exo_proto", false);

	if (pSettings->line_exist(section, "character_portrait"))
	{
		m_character_portrait = pSettings->r_string(section, "character_portrait");
	}
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
			if (pHelmet != nullptr && !bIsHelmetAvaliable)
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
			if (pActor->GetNightVisionEffector() && !bIsHelmetAvaliable)
			{
				pActor->GetNightVisionEffector()->SwitchNightVision(false);
			}
		}
	}
}

bool CCustomOutfit::install_upgrade_impl(LPCSTR section, bool test)
{
	bool result = inherited::install_upgrade_impl(section, test);

	result |= process_if_exists(section, "artefact_count", &CInifile::r_u32, m_artefact_count, test);

	if (m_boneProtection->m_hitFracType == SBoneProtections::HitFractionActorCS ||
		m_boneProtection->m_hitFracType == SBoneProtections::HitFractionActorCOP)
	{
		result |= process_if_exists(section, "hit_fraction_actor", &CInifile::r_float, m_boneProtection->m_fHitFrac, test);
	}

	result |= process_if_exists(section, "additional_inventory_weight", &CInifile::r_float, m_additional_weight, test);
	result |= process_if_exists(section, "additional_inventory_weight2", &CInifile::r_float, m_additional_weight2, test);

	return result;
}

BOOL CCustomOutfit::BonePassBullet(int boneID)
{
	return m_boneProtection->getBonePassBullet(s16(boneID));
}

void CCustomOutfit::ApplySkinModel(CActor* pActor, bool bDress, bool bHUDOnly)
{
	if (isDisableChangeSkin)
	{
		return;
	}

	if (bDress)
	{
		if (!bHUDOnly && m_ActorVisual.size())
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
				NewVisual = m_ActorVisual;
			}

			pActor->ChangeVisual(NewVisual);
		}


		if (pActor == Level().CurrentViewEntity())
		{
			if (m_character_portrait.size() > 0)
			{
				pActor->SetIcon(m_character_portrait, true);
				if (auto current_ui = CurrentGameUI())
				{
					if (current_ui->ActorMenu()->IsShown())
					{
						current_ui->ActorMenu()->ReloadActorInfo();
					}
				}
			}

			g_player_hud->NextHUDSect = pSettings->r_string(cNameSect(), "player_hud_section");
			g_player_hud->m_need_reload = false;
		}
	}
	else
	{
		if (!bHUDOnly && m_ActorVisual.size())
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
	return m_ef_equipment_type;
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
