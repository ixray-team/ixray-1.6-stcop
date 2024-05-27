#include "StdAfx.h"
#include "PickupManager.h"
#include "Actor.h"
#include "inventory_item.h"
#include "../xrEngine/GameMtlLib.h"

#define PICKUP_INFO_COLOR 0xFFDDDDDD

CPickUpManager::CPickUpManager(CActor* NewOwner) :
	Owner(NewOwner)
{
	PickupMode = false;
	PickupInfoRadius = 100;
}

void CPickUpManager::RenderInfo()
{
	Owner->feel_touch_update(Owner->Position(), PickupInfoRadius);

	CFrustum frustum;
	frustum.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);

	for (CObject* Item: Owner->feel_touch)
	{
		if (CanPickItem(frustum, Device.vCameraPosition, Item))
			PickupInfoDraw(Item);
	}
}

void CPickUpManager::PickupInfoDraw(CObject* object)
{
	CInventoryItem* item = smart_cast<CInventoryItem*>(object);
	if (!item)
		return;

	Fmatrix res;
	res.mul(Device.mFullTransform, object->XFORM());
	Fvector4 v_res;
	Fvector shift;

	LPCSTR draw_str = item->NameItem();
	shift.set(0, 0, 0);

	res.transform(v_res, shift);

	if (v_res.z < 0 || v_res.w < 0)
		return;

	if (v_res.x < -1.f || v_res.x > 1.f || v_res.y < -1.f || v_res.y > 1.f)
		return;

	float x = (1.f + v_res.x) / 2.f * (Device.TargetWidth);
	float y = (1.f - v_res.y) / 2.f * (Device.TargetHeight);

	CGameFont* font = g_FontManager->pFontSystem;
	font->SetAligment(CGameFont::alCenter);
	font->SetColor(PICKUP_INFO_COLOR);

	font->Out(x, y, draw_str);
}

ICF static BOOL info_trace_callback(collide::rq_result& result, LPVOID params)
{
	bool& bOverlaped = *(bool*)params;
	if (result.O)
	{
		if (Level().CurrentEntity() == result.O)
		{ //ignore self-actor
			return true;
		}
		else
		{ //check obstacle flag
			if (result.O->spatial.type & STYPE_OBSTACLE)
				bOverlaped = true;

			return TRUE;
		}
	}
	else
	{
		//получить треугольник и узнать его материал
		CDB::TRI* T = Level().ObjectSpace.GetStaticTris() + result.element;
		if (GMLib.GetMaterialByIdx(T->material)->Flags.is(SGameMtl::flPassable))
			return TRUE;
	}

	bOverlaped = true;
	return FALSE;
}
#include "DestroyablePhysicsObject.h"
bool CPickUpManager::CanPickItem(const CFrustum& frustum, const Fvector& from, CObject* item)
{
	if (!item->getVisible())
		return false;

	bool bOverlaped = false;
	Fvector dir, to;
	item->Center(to);
	float range = dir.sub(to, from).magnitude();
	if (range > 0.25f)
	{
		if (frustum.testSphere_dirty(to, item->Radius()))
		{
			dir.div(range);

			collide::ray_defs RD(from, dir, range, CDB::OPT_CULL, collide::rqtBoth);
			VERIFY(!fis_zero(RD.dir.square_magnitude()));

			RQR.r_clear();
			Level().ObjectSpace.RayQuery(RQR, RD, info_trace_callback, &bOverlaped, nullptr, item);

			for (collide::rq_result result : RQR.r_results())
			{
				CGameObject* GO = smart_cast<CGameObject*>(result.O);
				if(!GO) continue;
				if(GO == Owner->cast_game_object()) continue;
				CEntity* entity = smart_cast<CEntity*>(GO);
				if(entity && !entity->g_Alive()) continue;
				CDestroyablePhysicsObject* dstobj = smart_cast<CDestroyablePhysicsObject*>(GO);
				if(dstobj && dstobj->HasChildPart()) continue;
				if(GO->spawn_ini() && GO->spawn_ini()->section_exist("story_object")) continue;

				return false;
			}
		}
	}
	return !bOverlaped;
}