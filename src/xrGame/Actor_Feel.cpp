#include "StdAfx.h"
#include "Actor.h"
#include "Inventory.h"
#include "InventoryWeaponSlotLayout.h"
#include "AnomalyZone.h"
#include "ui/UIMainIngameWnd.h"
#include "UIGameCustom.h"
#include "Grenade.h"
#include "HUDManager.h"
#include "../xrEngine/CameraBase.h"
#include "PickupManager.h"
#include "ai/monsters/ai_monster_utils.h"

bool g_b_COD_PickUpMode = true;

void CActor::feel_touch_new(CObject* O)
{

}

void CActor::feel_touch_delete(CObject* O)
{

}

bool CActor::feel_touch_contact(CObject* O)
{
	if (O == nullptr || O->getDestroy())
	{
		return false;
	}

	CGameObject* GO = O->cast_game_object();
	if (GO == nullptr)
	{
		return false;
	}

	CInventoryItem* item = GO->cast_inventory_item();
	CInventoryOwner* inventory_owner = GO->cast_inventory_owner();

	if (item != nullptr && item->Useful() && item->object().H_Parent() == nullptr)
	{
		return true;
	}

	if (inventory_owner != nullptr && inventory_owner != cast_inventory_owner())
	{
		return true;
	}

	return false;
}

bool CActor::feel_touch_on_contact(CObject* O)
{
	if (O == nullptr || O->getDestroy())
	{
		return false;
	}

	CGameObject* GO = O->cast_game_object();
	if (GO == nullptr)
	{
		return false;
	}

	CAnomalyZone* custom_zone = GO->cast_anomaly_zone();
	if (custom_zone == nullptr)
	{
		return true;
	}

	Fsphere sphere;
	Center(sphere.P);
	sphere.R = 0.1f;

	if (custom_zone->inside(sphere))
	{
		return true;
	}

	return false;
}

void CActor::PickupModeUpdate()
{
	if (!pPickup->GetPickupMode())
	{
		return; // kUSE key pressed
	}

	if (!IsGameTypeSingle())
	{
		return;
	}

	//подбирание объекта
	if (g_b_COD_PickUpMode) {
		if (m_pObjectWeLookingAt &&
			m_pObjectWeLookingAt->cast_inventory_item() &&
			m_pObjectWeLookingAt->cast_inventory_item()->Useful() &&
			m_pUsableObject &&
			!Level().m_feel_deny.is_object_denied(m_pObjectWeLookingAt))
		{
			m_pUsableObject->use(this);
			Game().SendPickUpEvent(ID(), m_pObjectWeLookingAt->ID());
		}
	}
	else {
		if (m_pObjectWeLookingAt && m_pObjectWeLookingAt->cast_inventory_item() &&
			m_pObjectWeLookingAt->cast_inventory_item()->Useful() &&
			m_pObjectWeLookingAt->cast_inventory_item()->CanTake() &&
			!Level().m_feel_deny.is_object_denied(m_pObjectWeLookingAt)) {
			if (m_pUsableObject && !m_pUsableObject->nonscript_usable())
				m_pUsableObject->use(this);
			Game().SendPickUpEvent(ID(), m_pObjectWeLookingAt->ID());
		}
	}

	pPickup->RenderInfo();
}

void CActor::PickupModeUpdate_COD()
{
	if (Level().CurrentViewEntity() != this || !g_b_COD_PickUpMode)
	{
		return;
	}

	if (!g_Alive() || eacFreeLook == cam_active)
	{
		if (!g_dedicated_server)
		{
			CurrentGameUI()->UIMainIngameWnd->SetPickUpItem(nullptr);
		}

		return;
	};

	Fmatrix project, view, transform;
	view.build_camera_dir(cam_FirstEye()->vPosition, cam_Active()->vDirection, cam_Active()->vNormal);
	project.build_projection(deg2rad(cam_Active()->f_fov), cam_Active()->f_aspect, Device.fViewportNear, 2.45f);
	transform.mul(project, view);

	CFrustum frustum;
	frustum.CreateFromMatrix(transform, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);
	g_SpatialSpace->q_frustum(ISpatialResult, 0, ESPATIAL_TYPE::ITEM, frustum);

	float maxlen = 1000.0f;
	CInventoryItem* pNearestItem = nullptr;

	for (ISpatialShared& spatial : ISpatialResult)
	{
		ISpatial* spatial_ = spatial.get();
		CObject* O = spatial_->dcast_CObject();
		if (O == nullptr || O->getDestroy() || O->H_Parent())
		{
			continue;
		}

		CGameObject* GO = O->cast_game_object();
		if (GO == nullptr || GO->cast_explosive_rocket())
		{
			continue;
		}

		CInventoryItem* pIItem = GO->cast_inventory_item();
		if (!pIItem || !pIItem->CanTake())
		{
			continue;
		}

		CMissile* pMissile = GO->cast_missile();
		if (pMissile != nullptr && !pMissile->Useful())
		{
			continue;
		}

		Fvector A, B, tmp;
		pIItem->object().Center(A);

		tmp.sub(A, cam_Active()->vPosition);
		B.mad(cam_Active()->vPosition, cam_Active()->vDirection, tmp.dotproduct(cam_Active()->vDirection));

		float len = B.distance_to_sqr(A);
		if (len > 1.0f)
		{
			continue;
		}

		if (maxlen > len)
		{
			maxlen = len;
			pNearestItem = pIItem;
		};
	}

	if (pNearestItem != nullptr)
	{
		if (!pPickup->CanPickItem(frustum, cam_FirstEye()->vPosition, &pNearestItem->object()))
		{
			pNearestItem = nullptr;
		}
	}

	if (pNearestItem != nullptr)
	{
		if (CGameObject* pNearestItemGameObject = pNearestItem->cast_game_object())
		{
			if (Level().m_feel_deny.is_object_denied(pNearestItemGameObject) || !pNearestItemGameObject->getVisible())
			{
				pNearestItem = nullptr;
			}
		}
	}

	if (!g_dedicated_server)
	{
		CurrentGameUI()->UIMainIngameWnd->SetPickUpItem(pNearestItem);
	}

	if (pNearestItem && pPickup->GetPickupMode())
	{
		CUsableScriptObject* pUsableObject = pNearestItem->object().cast_usable_script_object();
		if (pUsableObject != nullptr && m_pUsableObject == nullptr)
		{
			pUsableObject->use(this);
		}

		//подбирание объекта
		Game().SendPickUpEvent(ID(), pNearestItem->object().ID());
	}
};

void CActor::Check_for_AutoPickUp()
{
	if (!psActorFlags.test(AF_AUTO_PICKUP))
	{
		return;
	}

	if (IsGameTypeSingleCompatible())
	{
		return;
	}

	if (Level().CurrentControlEntity() != this)
	{
		return;
	}

	if (!g_Alive())
	{
		return;
	}

	Fvector bc;
	bc.add(Position(), m_AutoPickUp_AABB_Offset);
	Fbox APU_Box;
	APU_Box.set(Fvector().sub(bc, m_AutoPickUp_AABB), Fvector().add(bc, m_AutoPickUp_AABB));

	g_SpatialSpace->q_box(ISpatialResult, 0, ESPATIAL_TYPE::ITEM, bc, m_AutoPickUp_AABB);

	// Determine visibility for dynamic part of scene
	for (ISpatialShared& spatial : ISpatialResult)
	{
		ISpatial* spatial_ = spatial.get();
		CObject* O = spatial_->dcast_CObject();
		if (O == nullptr || O->getDestroy() || O->H_Parent() || Level().m_feel_deny.is_object_denied(O))
		{
			continue;
		}

		CGameObject* GO = O->cast_game_object();
		if (GO == nullptr || GO->cast_explosive_rocket())
		{
			continue;
		}

		CInventoryItem* pIItem = GO->cast_inventory_item();
		if (pIItem == nullptr || !pIItem->CanTake())
			continue;

		CMissile* pMissile = GO->cast_missile();
		if (pMissile != nullptr && !pMissile->Useful())
		{
			continue;
		}

		if (APU_Box.Pick(pIItem->object().Position(), pIItem->object().Position()))
		{
			if (GameID() & eGameIDDeathmatch || GameID() & eGameIDTeamDeathmatch)
			{
				if (IsSidearmOrPrimaryPhysicalSlot(pIItem->BaseSlot()))
				{
					if (inventory().ItemFromSlot(pIItem->BaseSlot()))
					{
						continue;
					}
				}
			}

			Game().SendPickUpEvent(ID(), pIItem->object().ID());
		}
	}
}

void CActor::feel_sound_new(CObject* who, int type, CSound_UserDataPtr user_data, const Fvector& Position, float power)
{
	if (who == this)
	{
		m_snd_noise = std::max(m_snd_noise, power);
	}
}

void CActor::Feel_Grenade_Update(float rad)
{
	if (!IsGameTypeSingle())
	{
		return;
	}

	// Find all nearest objects
	Fvector pos_actor;
	Center(pos_actor);

	// select only grenade
	g_SpatialSpace->q_sphere(q_nearest, 0, ESPATIAL_TYPE::COLLIDEABLE|ESPATIAL_TYPE::MISSILE, pos_actor, rad);
	for (ISpatialShared& SS : q_nearest)
	{
		ISpatial* S = SS.get();
		if (!S) continue;
		CObject* O = S->dcast_CObject();
		if (!O || O->getDestroy()) continue;

		CGrenade* grn = O->cast_grenade();
		if (grn == nullptr || grn->Initiator() == ID() || grn->Useful() || grn->IsExploding())
		{
			continue;
		}

		if (grn->time_from_begin_throw() < m_fFeelGrenadeTime)
		{
			continue;
		}

		HUD().AddGrenade_ForMark(grn);
	}

	HUD().Update_GrenadeView(pos_actor);
}

