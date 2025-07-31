#include "StdAfx.h"
#include "Actor.h"
#include "Weapon.h"
#include "MercuryBall.h"
#include "Inventory.h"
#include "character_info.h"
#include "../xrEngine/xr_level_controller.h"
#include "UsableScriptObject.h"
#include "CustomZone.h"
#include "../xrEngine/GameMtlLib.h"
#include "ui/UIMainIngameWnd.h"
#include "UIGameCustom.h"
#include "Grenade.h"
#include "WeaponRPG7.h"
#include "ExplosiveRocket.h"
#include "game_cl_base.h"
#include "Level.h"
#include "clsid_game.h"
#include "HUDManager.h"


void CActor::feel_touch_new				(CObject* O)
{
	if (!O || O->getDestroy()) return;
	CGameObject* GO = O->cast_game_object(); if (!GO) return;
	CPhysicsShellHolder* sh = GO->cast_physics_shell_holder();

	if(sh&&sh->character_physics_support()) m_feel_touch_characters++;

}

void CActor::feel_touch_delete	(CObject* O)
{
	if (!O || O->getDestroy()) return;
	CGameObject* GO = O->cast_game_object(); if (!GO) return;
	CPhysicsShellHolder* sh = GO->cast_physics_shell_holder();

	if(sh&&sh->character_physics_support()) m_feel_touch_characters--;
}

BOOL CActor::feel_touch_contact		(CObject *O)
{
	if (!O || O->getDestroy()) return (FALSE);
	CGameObject* GO = O->cast_game_object(); if (!GO) return (FALSE);

	CInventoryItem	*item = GO->cast_inventory_item();
	CInventoryOwner	*inventory_owner = GO->cast_inventory_owner();

	if (item && item->Useful() && !item->object().H_Parent()) 
		return TRUE;

	if(inventory_owner && inventory_owner != cast_inventory_owner())
	{
		//CPhysicsShellHolder* sh=smart_cast<CPhysicsShellHolder*>(O);
		//if(sh&&sh->character_physics_support()) m_feel_touch_characters++;
		return TRUE;
	}

	return		(FALSE);
}

BOOL CActor::feel_touch_on_contact	(CObject *O)
{
	if (!O || O->getDestroy()) return (FALSE);
	CGameObject* GO = O->cast_game_object(); if (!GO) return (FALSE);

	CCustomZone	*custom_zone = GO->cast_custom_zone();
	if (!custom_zone)
		return	(TRUE);

	Fsphere		sphere;
	Center		(sphere.P);
	sphere.R	= 0.1f;
	if (custom_zone->inside(sphere))
		return	(TRUE);

	return		(FALSE);
}

#include "ai/monsters/ai_monster_utils.h"
#include "PickupManager.h"

void CActor::PickupModeUpdate()
{
	if(!pPickup->GetPickupMode())
		return; // kUSE key pressed

	if(!IsGameTypeSingle())
		return;

	//подбирание объекта
	if(	m_pObjectWeLookingAt									&& 
		m_pObjectWeLookingAt->cast_inventory_item()				&& 
		m_pObjectWeLookingAt->cast_inventory_item()->Useful()	&&
		m_pUsableObject											&& 
		!m_pUsableObject->nonscript_usable()					&&
		!Level().m_feel_deny.is_object_denied(m_pObjectWeLookingAt) )
	{
		m_pUsableObject->use(this);
		Game().SendPickUpEvent(ID(), m_pObjectWeLookingAt->ID());
	}

	pPickup->RenderInfo();
}

#include "../xrEngine/CameraBase.h"
BOOL	g_b_COD_PickUpMode = TRUE;
void	CActor::PickupModeUpdate_COD	()
{
	if (Level().CurrentViewEntity() != this || !g_b_COD_PickUpMode) return;
		
	if (!g_Alive() || eacFreeLook == cam_active)
	{
		if (!g_dedicated_server)
		{
			CurrentGameUI()->UIMainIngameWnd->SetPickUpItem(nullptr);
		}

		return;
	};
	
	CFrustum frustum;
	frustum.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB|FRUSTUM_P_FAR);

	ISpatialResult.resize(0);
	g_SpatialSpace->q_frustum		(ISpatialResult, 0, STYPE_COLLIDEABLE, frustum);

	float maxlen					= 1000.0f;
	CInventoryItem* pNearestItem	= nullptr;

	for (ISpatialShared& spatial : ISpatialResult)
	{
		ISpatial* spatial_ = spatial.get();
		CObject* O = spatial_->dcast_CObject(); if (!O || O->getDestroy() || O->H_Parent()) continue;
		CGameObject* GO = O->cast_game_object(); if (!GO || GO->cast_explosive_rocket()) continue;
		CInventoryItem*	pIItem = GO->cast_inventory_item(); if (!pIItem || !pIItem->CanTake()) continue;

		CMissile* pMissile = GO->cast_missile();
		if (pMissile && !pMissile->Useful())
			continue;

		Fvector A, B, tmp;
		pIItem->object().Center(A);
		if (A.distance_to_sqr(Position()) > 4)
			continue;

		tmp.sub(A, cam_Active()->vPosition);
		B.mad(cam_Active()->vPosition, cam_Active()->vDirection, tmp.dotproduct(cam_Active()->vDirection));

		float len = B.distance_to_sqr(A);
		if (len > 1)
			continue;

		if (maxlen>len && !pIItem->object().getDestroy())
		{
			maxlen = len;
			pNearestItem = pIItem;
		};
	}

	if(pNearestItem)
	{
		CFrustum frustum_;
		frustum_.CreateFromMatrix(Device.mFullTransform,FRUSTUM_P_LRTB|FRUSTUM_P_FAR);
		if (!pPickup->CanPickItem(frustum_, Device.vCameraPosition, &pNearestItem->object()))
			pNearestItem = nullptr;
	}

	if (pNearestItem && pNearestItem->cast_game_object())
	{
		if (Level().m_feel_deny.is_object_denied(pNearestItem->cast_game_object()))
				pNearestItem = nullptr;
	}
	
	if (pNearestItem && pNearestItem->cast_game_object())
	{
		if(!pNearestItem->cast_game_object()->getVisible())
				pNearestItem = nullptr;
	}

	if (!g_dedicated_server)
	{
		CurrentGameUI()->UIMainIngameWnd->SetPickUpItem(pNearestItem);
	}

	if (pNearestItem && pPickup->GetPickupMode())
	{
		CUsableScriptObject* pUsableObject = pNearestItem->object().cast_usable_script_object();
		if(pUsableObject && (!m_pUsableObject))
			pUsableObject->use(this);

		//подбирание объекта
		Game().SendPickUpEvent(ID(), pNearestItem->object().ID());
	}
};

void	CActor::Check_for_AutoPickUp()
{
	// mp only
	if (!psActorFlags.test(AF_AUTOPICKUP))		return;
	if (IsGameTypeSingle())						return;
	if (Level().CurrentControlEntity() != this) return;
	if (!g_Alive())								return;

	Fvector bc; 
	bc.add				(Position(), m_AutoPickUp_AABB_Offset);
	Fbox APU_Box;
	APU_Box.set			(Fvector().sub(bc, m_AutoPickUp_AABB), Fvector().add(bc, m_AutoPickUp_AABB));

	xr_vector<ISpatialShared>	ISpatialResult_;
	g_SpatialSpace->q_box   (ISpatialResult_, 0, STYPE_COLLIDEABLE, bc, m_AutoPickUp_AABB);

	// Determine visibility for dynamic part of scene
	for (ISpatialShared& spatial : ISpatialResult)
	{
		ISpatial* spatial_ = spatial.get();
		CObject* O = spatial_->dcast_CObject(); if (!O || O->getDestroy() || O->H_Parent() || Level().m_feel_deny.is_object_denied(O)) continue;
		CGameObject* GO = O->cast_game_object(); if (!GO || GO->cast_explosive_rocket()) continue;
		CInventoryItem* pIItem = GO->cast_inventory_item(); if (!pIItem || !pIItem->CanTake()) continue;

		CMissile* pMissile = GO->cast_missile();
		if (pMissile && !pMissile->Useful())
			continue;

		if (APU_Box.Pick(pIItem->object().Position(), pIItem->object().Position()))
		{
			if (GameID() == eGameIDDeathmatch || GameID() == eGameIDTeamDeathmatch)
			{
				if (pIItem->BaseSlot() == INV_SLOT_2 || pIItem->BaseSlot() == INV_SLOT_3 )
				{
					if (inventory().ItemFromSlot(pIItem->BaseSlot()))
						continue;
				}
			}			
			
			Game().SendPickUpEvent(ID(), pIItem->object().ID());
		}		
	}
}

void CActor::feel_sound_new(CObject* who, int type, CSound_UserDataPtr user_data, const Fvector& Position, float power)
{
	if(who == this)
		m_snd_noise = _max(m_snd_noise, power);
}

void CActor::Feel_Grenade_Update( float rad )
{
	if ( !IsGameTypeSingle() )
	{
		return;
	}
	// Find all nearest objects
	Fvector pos_actor;
	Center( pos_actor );

	q_nearest.resize(0);
	g_pGameLevel->ObjectSpace.GetNearest( q_nearest, pos_actor, rad, nullptr );

	// select only grenade
	for (CObject* O : q_nearest)
	{
		if (!O || O->getDestroy()) continue;					// Don't touch candidates for destroy
		CGameObject* GO = O->cast_game_object(); if (!GO) continue;

		CGrenade* grn = GO->cast_grenade();
		if( !grn || grn->Initiator() == ID() || grn->Useful() )
		{
			continue;
		}
		if ( grn->time_from_begin_throw() < m_fFeelGrenadeTime )
		{
			continue;
		}
		if ( HUD().AddGrenade_ForMark( grn ) )
		{
			//.	Msg("__ __ Add new grenade! id = %d ", grn->ID() );
		}
	}// for it

	HUD().Update_GrenadeView( pos_actor );
}

