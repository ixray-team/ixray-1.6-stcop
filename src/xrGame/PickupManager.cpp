#include "StdAfx.h"
#include "PickupManager.h"
#include "Actor.h"
#include "inventory_item.h"
#include "../xrEngine/GameMtlLib.h"
#include "../xrEngine/CameraBase.h"
#include "DestroyablePhysicsObject.h"
#include "HUDTarget.h"
#include "HUDManager.h"

CPickUpManager::CPickUpManager(CActor* NewOwner) :
	Owner(NewOwner)
{
	PickupMode = false;
	PickupInfoRadius = 100;
}

void CPickUpManager::RenderInfo()
{
	Owner->feel_touch_update(Owner->CamPivot, PickupInfoRadius);

	for (CObject* Item: Owner->feel_touch)
	{
		if (Item->cast_inventory_item() && Item->cast_inventory_item()->CanTake() && Item->cast_inventory_item()->Useful() && CanPickItem(Owner->CamPivot, Item))
			PickupInfoDraw(Item);
	}
}

void CPickUpManager::PickupInfoDraw(CObject* object)
{
	Fmatrix res;
	res.mul(Device.mFullTransform, object->XFORM());

	Fvector4 v_res;
	Fvector shift;
	res.transform(v_res, shift);

	if (v_res.z < 0 || v_res.w < 0)
		return;

	if (v_res.x < -1.f || v_res.x > 1.f || v_res.y < -1.f || v_res.y > 1.f)
		return;

	float x = (1.f + v_res.x) / 2.f * (Device.TargetWidth);
	float y = (1.f - v_res.y) / 2.f * (Device.TargetHeight);

	CGameFont* font = g_FontManager->pFontSystem;
	font->SetAligment(CGameFont::alCenter);
	font->SetColor(color_rgba(221u, 221u, 221u, 255u));

	font->Out(x, y, object->cast_inventory_item()->NameItem());
}

ICF void closest_point_on_bone_shape(const SBoneShape& shape, const Fvector& point, Fvector& result, Fvector& d)
{
	constexpr float bone_shape_shrink = 0.89f;
	switch (shape.type)
	{
		case SBoneShape::stSphere:
		{
			const Fsphere& S = shape.sphere;
			d.sub(point, S.P);
			float l = d.magnitude();
			float r = S.R * bone_shape_shrink;
			if (l > r)
			{
				result.mad(S.P, d.div(l), r);
			}
			else
			{
				result.set(point);
			}
		}
		break;

		case SBoneShape::stBox:
		{
			const Fobb& B = shape.box;
			d.sub(point, B.m_translate);
			float x = d.dotproduct(B.m_rotate.i);
			float y = d.dotproduct(B.m_rotate.j);
			float z = d.dotproduct(B.m_rotate.k);
			float hx = B.m_halfsize.x * bone_shape_shrink;
			float hy = B.m_halfsize.y * bone_shape_shrink;
			float hz = B.m_halfsize.z * bone_shape_shrink;
			clamp(x, -hx, hx);
			clamp(y, -hy, hy);
			clamp(z, -hz, hz);
			result.set(B.m_translate);
			result.mad(B.m_rotate.i, x);
			result.mad(B.m_rotate.j, y);
			result.mad(B.m_rotate.k, z);
		}
		break;

		case SBoneShape::stCylinder:
		{
			const Fcylinder& C = shape.cylinder;
			d.sub(point, C.m_center);
			float h = d.dotproduct(C.m_direction);
			Fvector radial = d;
			radial.mad(C.m_direction, -h);

			float hh = C.m_height * 0.5f * bone_shape_shrink;
			float r = C.m_radius * bone_shape_shrink;
			clamp(h, -hh, hh);

			float l = radial.magnitude();
			if (l > r)
			{
				radial.mul(r / l);
			}

			result.set(C.m_center);
			result.mad(C.m_direction, h);
			result.add(radial);
		}
		break;

		default:
			result.set(point);
			break;
	}
}

void get_nearest_bone_position(IKinematics* pK, const Fvector& dest_pos, const Fvector& dest_dir, Fvector& target_pos, const Fmatrix& xform)
{
	Fmatrix inv_xform;
	inv_xform.invert(xform);
	Fvector ray_pos, ray_dir;
	inv_xform.transform_tiny(ray_pos, dest_pos);
	inv_xform.transform_dir(ray_dir, dest_dir);
	ray_dir.normalize_safe();

	float min_dist = 1000.f;
	Fvector t_pos, proj, d;
	for (auto& pair : *pK->LL_Bones())
	{
		CBoneData& bData = pK->LL_GetData(pair.second);
		CBoneInstance& BI = pK->LL_GetBoneInstance(pair.second);

		if (bData.shape.flags.test(SBoneShape::sfNoPickable) ||
			bData.shape.flags.test(SBoneShape::sfNoPhysics) || bData.shape.type == SBoneShape::stNone || !pK->LL_GetBoneVisible(pair.second))
		{
			continue;
		}

		Fmatrix inv_bone;
		inv_bone.invert(BI.mTransform);
		Fvector b_pos, b_dir;
		inv_bone.transform_tiny(b_pos, ray_pos);
		inv_bone.transform_dir(b_dir, ray_dir);
		b_dir.normalize_safe();

		closest_point_on_bone_shape(bData.shape, b_pos, t_pos, d);

		for (int it = 0; it < 4; it++)
		{
			d.sub(t_pos, b_pos);
			float t = d.dotproduct(b_dir);
			if (t < 0.f)
			{
				t = 0.f;
			}
			proj.mad(b_pos, b_dir, t);
			closest_point_on_bone_shape(bData.shape, proj, t_pos, d);
		}

		BI.mTransform.transform_tiny(t_pos);

		d.sub(t_pos, ray_pos);
		float t = d.dotproduct(ray_dir);
		if (t < 0.f)
		{
			t = 0.f;
		}
		proj.mad(ray_pos, ray_dir, t);
		float curr_dist = t_pos.distance_to_sqr(proj);
		if (curr_dist < min_dist)
		{
			min_dist = curr_dist;
			target_pos = t_pos;
		}
	}

	xform.transform_tiny(target_pos);
}
bool CPickUpManager::CanPickItem(const Fvector& from, CObject* item)
{
	if (!item->getVisible())
	{
		return false;
	}

	Fvector dir, to;
	get_nearest_bone_position(PKinematics(item->Visual()), from, Actor()->cam_Active()->vDirection, to, item->XFORM());
	float range = dir.sub(to, from).magnitude();
	dir.normalize();

	collide::ray_defs RD(from, dir, range, CDB::OPT_CULL, collide::rqtBoth);
	VERIFY(!fis_zero(RD.dir.square_magnitude()));
	struct SOverlaped
	{
		CObject* item = nullptr;
		bool bOverlaped = false;
	} tmp_s{item, false};
	Level().ObjectSpace.RayQuery(RQR, RD, [](const collide::rq_result& result, void* params) -> bool
	{
		SOverlaped& tmp_s = *(SOverlaped*)params;

		if (result.valid() && !result.IsStatic())
		{
			if ((result.GetDynamic()->SpatialComponent->type & ESPATIAL_TYPE::OBSTACLE) != ESPATIAL_TYPE::NONE)
			{
				tmp_s.bOverlaped = true;
				return true;
			}

			if (result.GetDynamic() == tmp_s.item)
			{
				return true;
			}

			CGameObject* GO = const_cast<CObject*>(result.GetDynamic())->cast_game_object();
			if (GO == nullptr)
			{
				return true;
			}

			if (!GO->getVisible())
			{
				return true;
			}

			if (GO->cast_inventory_item())
			{
				return true;
			}

			CEntity* entity = GO->cast_entity();
			if (entity != nullptr && !entity->g_Alive())
			{
				return true;
			}

			CDestroyablePhysicsObject* dstobj = smart_cast<CDestroyablePhysicsObject*>(GO);
			if (dstobj != nullptr && dstobj->HasChildPart())
			{
				return true;
			}

			if (GO->spawn_ini() && GO->spawn_ini()->section_exist("story_object"))
			{
				return true;
			}
		}
		
		auto& T = result.GetStatic()->get_tris()[result.element];
		if (GMLib.GetMaterialByIdx(T.material)->Flags.is(SGameMtl::flPassable))
		{
			return true;
		}

		tmp_s.bOverlaped = true;
		return false;
	}, &tmp_s, nullptr, Owner);

	return !tmp_s.bOverlaped;
}