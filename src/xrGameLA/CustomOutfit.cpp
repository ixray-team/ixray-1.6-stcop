#include "stdafx.h"

#include "customoutfit.h"
#include "PhysicsShell.h"
#include "inventory_space.h"
#include "Inventory.h"
#include "Actor.h"
#include "game_cl_base.h"
#include "Level.h"
#include "BoneProtections.h"
#include "../../Include/xrRender/Kinematics.h"
#include "../../Include/xrRender/RenderVisual.h"
#include "ai_sounds.h"
#include "actorEffector.h"
#include "player_hud.h"

CCustomOutfit::CCustomOutfit()
{
	m_baseSlot = OUTFIT_SLOT;
}

CCustomOutfit::~CCustomOutfit() 
{
	/*HUD_SOUND_ITEM::DestroySound	(m_NightVisionOnSnd);
	HUD_SOUND_ITEM::DestroySound	(m_NightVisionOffSnd);
	HUD_SOUND_ITEM::DestroySound	(m_NightVisionIdleSnd);
	HUD_SOUND_ITEM::DestroySound	(m_NightVisionBrokenSnd);*/
}

void CCustomOutfit::net_Export(NET_Packet& P)
{
	inherited::net_Export(P);
	//P.w_u8(m_bNightVisionOn ? 1 : 0);
}

void CCustomOutfit::net_Import(NET_Packet& P)
{
	inherited::net_Import(P);
	/*bool new_bNightVisionOn = !!P.r_u8();

	if (new_bNightVisionOn != m_bNightVisionOn)	
		SwitchNightVision(new_bNightVisionOn);*/
}

void CCustomOutfit::Load(LPCSTR section) 
{
	inherited::Load(section);

	if (pSettings->line_exist(section, "actor_visual"))
		m_ActorVisual = pSettings->r_string(section, "actor_visual");
	else
		m_ActorVisual = NULL;

	if (pSettings->line_exist(section, "actor_visual_legs"))
		m_ActorVisual_legs = pSettings->r_string(section, "actor_visual_legs");
	else
		m_ActorVisual_legs = NULL;

	m_ef_equipment_type		= pSettings->r_u32(section,"ef_equipment_type");

	m_additional_weight				= pSettings->r_float(section,"additional_inventory_weight");
	m_additional_weight2			= pSettings->r_float(section,"additional_inventory_weight2");

		//tatarinrafa: added additional jump speed sprint speed walk speed
	m_additional_jump_speed		= READ_IF_EXISTS(pSettings, r_float, section, "additional_jump_speed", 0.0f);
	m_additional_run_coef		= READ_IF_EXISTS(pSettings, r_float, section, "additional_run_coef", 0.0f);
	m_additional_sprint_koef	= READ_IF_EXISTS(pSettings, r_float, section, "additional_sprint_koef", 0.0f);

	if (pSettings->line_exist(section, "nightvision_sect"))
		m_NightVisionSect = pSettings->r_string(section, "nightvision_sect");
	else
		m_NightVisionSect = NULL;

	block_pnv_slot						= READ_IF_EXISTS(pSettings, r_u32, section, "block_pnv_slot", 0);
	block_helmet_slot					= READ_IF_EXISTS(pSettings, r_u32, section, "block_helmet_slot", 0);
/*
	m_bNightVisionEnabled	= !!pSettings->r_bool(section,"night_vision");
	if(m_bNightVisionEnabled)
	{
		HUD_SOUND_ITEM::LoadSound(section,"snd_night_vision_on"	, m_NightVisionOnSnd	, SOUND_TYPE_ITEM_USING);
		HUD_SOUND_ITEM::LoadSound(section,"snd_night_vision_off"	, m_NightVisionOffSnd	, SOUND_TYPE_ITEM_USING);
		HUD_SOUND_ITEM::LoadSound(section,"snd_night_vision_idle", m_NightVisionIdleSnd	, SOUND_TYPE_ITEM_USING);
		HUD_SOUND_ITEM::LoadSound(section,"snd_night_vision_broken", m_NightVisionBrokenSnd, SOUND_TYPE_ITEM_USING);
	}
*/
	m_full_icon_name								= pSettings->r_string(section,"full_icon_name");
}

//void CCustomOutfit::SwitchNightVision()
//{
//	if (OnClient()) return;
//	SwitchNightVision(!m_bNightVisionOn);	
//}

//void CCustomOutfit::SwitchNightVision(bool vision_on)
//{
//	if(!m_bNightVisionEnabled) return;
//	
//	m_bNightVisionOn = vision_on;
//
//	CActor *pA = smart_cast<CActor*>(H_Parent());
//
//	if(!pA)					return;
//	bool bPlaySoundFirstPerson = (pA == Level().CurrentViewEntity());
//
//	LPCSTR disabled_names	= pSettings->r_string(cNameSect(),"disabled_maps");
//	LPCSTR curr_map			= *Level().name();
//	u32 cnt					= _GetItemCount(disabled_names);
//	bool b_allow			= true;
//	string512				tmp;
//	for(u32 i=0; i<cnt;++i)
//	{
//		_GetItem(disabled_names, i, tmp);
//		if(0==stricmp(tmp, curr_map))
//		{
//			b_allow = false;
//			break;
//		}
//	}
//
//	if(m_NightVisionSect.size()&&!b_allow)
//	{
//		HUD_SOUND_ITEM::PlaySound(m_NightVisionBrokenSnd, pA->Position(), pA, bPlaySoundFirstPerson);
//		return;
//	}
//
//	if(m_bNightVisionOn)
//	{
//		CEffectorPP* pp = pA->Cameras().GetPPEffector((EEffectorPPType)effNightvision);
//		if(!pp)
//		{
//			if (m_NightVisionSect.size())
//			{
//				AddEffector(pA,effNightvision, m_NightVisionSect);
//				HUD_SOUND_ITEM::PlaySound(m_NightVisionOnSnd, pA->Position(), pA, bPlaySoundFirstPerson);
//				HUD_SOUND_ITEM::PlaySound(m_NightVisionIdleSnd, pA->Position(), pA, bPlaySoundFirstPerson, true);
//			}
//		}
//	} else {
// 		CEffectorPP* pp = pA->Cameras().GetPPEffector((EEffectorPPType)effNightvision);
//		if(pp)
//		{
//			pp->Stop			(1.0f);
//			HUD_SOUND_ITEM::PlaySound(m_NightVisionOffSnd, pA->Position(), pA, bPlaySoundFirstPerson);
//			HUD_SOUND_ITEM::StopSound(m_NightVisionIdleSnd);
//		}
//	}
//}

void CCustomOutfit::net_Destroy() 
{
	//SwitchNightVision		(false);

	inherited::net_Destroy	();
}

BOOL CCustomOutfit::net_Spawn(CSE_Abstract* DC)
{
	//SwitchNightVision		(false);
	return inherited::net_Spawn(DC);
}

void CCustomOutfit::OnH_B_Independent	(bool just_before_destroy) 
{
	inherited::OnH_B_Independent	(just_before_destroy);

	/*SwitchNightVision			(false);

	HUD_SOUND_ITEM::StopSound		(m_NightVisionOnSnd);
	HUD_SOUND_ITEM::StopSound		(m_NightVisionOffSnd);
	HUD_SOUND_ITEM::StopSound		(m_NightVisionIdleSnd);*/
}

void	CCustomOutfit::OnMoveToSlot		()
{
	inherited::OnMoveToSlot();

	if (m_pCurrentInventory)
	{
		CActor* pActor = smart_cast<CActor*> (m_pCurrentInventory->GetOwner());
		if (pActor)
		{
			//SwitchNightVision(false);

			if (pActor->IsFirstEye() && IsGameTypeSingle() && !pActor->IsActorShadowsOn())
			{
				if (m_ActorVisual_legs.size())
				{
						shared_str NewVisual = m_ActorVisual_legs;
						pActor->ChangeVisual(NewVisual);

						if (pActor == Level().CurrentViewEntity())	
							g_player_hud->load(pSettings->r_string(cNameSect(),"player_hud_section"));

				} else {
						shared_str NewVisual = pActor->GetDefaultVisualOutfit_legs();
						pActor->ChangeVisual(NewVisual);

						if (pActor == Level().CurrentViewEntity())
							g_player_hud->load_default();
				}
			} else {
				if (m_ActorVisual.size())
				{
					shared_str NewVisual = NULL;
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
						NewVisual = m_ActorVisual;
	
					pActor->ChangeVisual(NewVisual);

					if (pActor == Level().CurrentViewEntity())	
						g_player_hud->load(pSettings->r_string(cNameSect(),"player_hud_section"));

				} else {
					shared_str NewVisual = pActor->GetDefaultVisualOutfit();
					pActor->ChangeVisual(NewVisual);

					if (pActor == Level().CurrentViewEntity())	
						g_player_hud->load_default();
				}
			}
		}
	}
};

void	CCustomOutfit::OnMoveToRuck		()
{
	inherited::OnMoveToRuck();

	if (m_pCurrentInventory)
	{
		CActor* pActor = smart_cast<CActor*> (m_pCurrentInventory->GetOwner());

		if (pActor)
		{
			CCustomOutfit* outfit	= pActor->GetOutfit();
			if (!outfit)
			{
				//pActor->SwitchNightVision();

				if (pActor->IsFirstEye() && IsGameTypeSingle())
				{
					shared_str DefVisual = pActor->GetDefaultVisualOutfit_legs();
					if (DefVisual.size())
					{
						pActor->ChangeVisual(DefVisual);
					}
				} else {
					shared_str DefVisual = pActor->GetDefaultVisualOutfit();
					if (DefVisual.size())
					{
						pActor->ChangeVisual(DefVisual);
					}
				}

				if (pActor == Level().CurrentViewEntity())
					g_player_hud->load_default();

			}
		}
	}
};

u32	CCustomOutfit::ef_equipment_type	() const
{
	return		(m_ef_equipment_type);
}

bool CCustomOutfit::install_upgrade_impl( LPCSTR section, bool test )
{
	bool result = inherited::install_upgrade_impl( section, test );

	LPCSTR str;
	bool result2 = process_if_exists_set( section, "nightvision_sect", &CInifile::r_string, str, test );
	if ( result2 && !test )
	{
		m_NightVisionSect._set( str );
	}
	result |= result2;

	result |= process_if_exists( section, "additional_inventory_weight",  &CInifile::r_float,  m_additional_weight,  test );
	result |= process_if_exists( section, "additional_inventory_weight2", &CInifile::r_float,  m_additional_weight2, test );

	result |= process_if_exists(section, "block_pnv_slot", &CInifile::r_u32, block_pnv_slot, test);
	result |= process_if_exists(section, "block_helmet_slot", &CInifile::r_u32, block_helmet_slot, test);

	result |= process_if_exists(section, "additional_jump_speed", &CInifile::r_float, m_additional_jump_speed, test);
	result |= process_if_exists(section, "additional_run_coef", &CInifile::r_float, m_additional_run_coef, test);
	result |= process_if_exists(section, "additional_sprint_koef", &CInifile::r_float, m_additional_sprint_koef, test);
	//result |= process_if_exists( section, "artefact_count", &CInifile::r_u32, m_artefact_count, test );
	//clamp( m_artefact_count, (u32)0, (u32)5 );

	return result;
}
