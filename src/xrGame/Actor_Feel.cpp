#include "stdafx.h"
#include "Actor.h"
#include "Weapon.h"
#include "MercuryBall.h"
#include "Inventory.h"
#include "character_info.h"
#include "../xrEngine/xr_level_controller.h"
#include "UsableScriptObject.h"
#include "CustomZone.h"
#include "../xrEngine/gamemtllib.h"
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
	CPhysicsShellHolder* sh=smart_cast<CPhysicsShellHolder*>(O);
	if(sh&&sh->character_physics_support()) m_feel_touch_characters++;

}

void CActor::feel_touch_delete	(CObject* O)
{
	CPhysicsShellHolder* sh=smart_cast<CPhysicsShellHolder*>(O);
	if(sh&&sh->character_physics_support()) m_feel_touch_characters--;
}

BOOL CActor::feel_touch_contact		(CObject *O)
{
	CInventoryItem	*item = smart_cast<CInventoryItem*>(O);
	CInventoryOwner	*inventory_owner = smart_cast<CInventoryOwner*>(O);

	if (item && item->Useful() && !item->object().H_Parent()) 
		return TRUE;

	if(inventory_owner && inventory_owner != smart_cast<CInventoryOwner*>(this))
	{
		//CPhysicsShellHolder* sh=smart_cast<CPhysicsShellHolder*>(O);
		//if(sh&&sh->character_physics_support()) m_feel_touch_characters++;
		return TRUE;
	}

	return		(FALSE);
}

BOOL CActor::feel_touch_on_contact	(CObject *O)
{
	CCustomZone	*custom_zone = smart_cast<CCustomZone*>(O);
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

	for (u32 o_it=0; o_it<ISpatialResult.size(); o_it++)
	{
		ISpatial*		spatial_	= ISpatialResult[o_it];
		CInventoryItem*	pIItem	= smart_cast<CInventoryItem*> (spatial_->dcast_CObject        ());

		if (0 == pIItem)											continue;
		if (pIItem->object().H_Parent() != nullptr)					continue;
		if (!pIItem->CanTake())										continue;
		if ( smart_cast<CExplosiveRocket*>( &pIItem->object() ) )	continue;

		CMissile* pMissile = smart_cast<CMissile*> (spatial_->dcast_CObject());
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
		CUsableScriptObject* pUsableObject = smart_cast<CUsableScriptObject*>(pNearestItem);
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

	xr_vector<ISpatial*>	ISpatialResult_;
	g_SpatialSpace->q_box   (ISpatialResult_, 0, STYPE_COLLIDEABLE, bc, m_AutoPickUp_AABB);

	// Determine visibility for dynamic part of scene
	for (u32 o_it=0; o_it<ISpatialResult_.size(); o_it++)
	{
		ISpatial*		spatial_	= ISpatialResult_[o_it];
		CInventoryItem*	pIItem	= smart_cast<CInventoryItem*> (spatial_->dcast_CObject());

		if (0 == pIItem)														continue;
		if (!pIItem->CanTake())													continue;
		if (Level().m_feel_deny.is_object_denied(spatial_->dcast_CObject()) )	continue;


		CGrenade*	pGrenade	= smart_cast<CGrenade*> (pIItem);
		if (pGrenade) continue;

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

	xr_vector<CObject*>::iterator	it_b = q_nearest.begin();
	xr_vector<CObject*>::iterator	it_e = q_nearest.end();

	// select only grenade
	for ( ; it_b != it_e; ++it_b )
	{
		if ( (*it_b)->getDestroy() ) continue;					// Don't touch candidates for destroy

		CGrenade* grn = smart_cast<CGrenade*>( *it_b );
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

