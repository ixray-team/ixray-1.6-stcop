#include "stdafx.h"
#include "DetailManager.h"

#include "../../xrEngine/GameMtlLib.h"
#include "../../xrCore/Collision/cl_intersect.h"

#ifdef _EDITOR
#	include "../../Editors/LevelEditor/Editor/scene/scene.h"
#endif

//--------------------------------------------------- Decompression
ICF float Interpolate(float* base, u32 x, u32 y, u32 size)
{
	float f	= float(size);
	float fx = float(x)/f; float ifx = 1.f-fx;
	float fy = float(y)/f; float ify = 1.f-fy;

	float c01 = base[0]*ifx + base[1]*fx;
	float c23 = base[2]*ifx + base[3]*fx;

	float c02 = base[0]*ify + base[2]*fy;
	float c13 = base[1]*ify + base[3]*fy;

	float cx = ify*c01 + fy*c23;
	float cy = ifx*c02 + fx*c13;
	return (cx+cy)/2;
}

ICF bool InterpolateAndDither(float* alpha255, u32 x, u32 y, u32 sx, u32 sy, u32 size, int dither[16][16] )
{
	clamp(x,(u32)0,size-1);
	clamp(y,(u32)0,size-1);
	int c = iFloor(Interpolate(alpha255,x,y,size)+.5f);
	clamp(c,0,255);

	u32	row	= (y+sy) % 16;
	u32	col	= (x+sx) % 16;
	return c > dither[col][row];
}

ICF void ground_correction(Fmatrix& xform, const Fvector &ground_normal)
{
	xform.j = ground_normal;

	xform.i.crossproduct(xform.j, xform.k); xform.i.normalize();
	xform.k.crossproduct(xform.i, xform.j); xform.k.normalize();
}

void CDetailManager::UnpackSlotItems(Slot* S)
{
	VERIFY(S);
	Slot& D = *S;
	D.type = stReady;
	if (D.empty)
		return;

	DetailSlot& DS = *D.DS;

#ifdef _EDITOR
	extern ECORE_API CDB::COLLIDER XRC;
	XRC.box_options(CDB::OPT_FULL_TEST);
	// Select polygons
	SBoxPickInfoVec pinf;
    Scene->BoxPickObjects(D.vis.box,pinf,GetSnapList());
	u32	triCount = pinf.size();
#else
	xrc.box_options(CDB::OPT_FULL_TEST);
	xrc.box_query(g_pGameLevel->ObjectSpace.GetStaticModel(), D.vis.box);
	u32	triCount = xrc.r_count();
	xr_vector<CDB::TRI>& tris = g_pGameLevel->ObjectSpace.GetStaticTris();
	xr_vector<CDB::RESULT>& results = xrc.r_vec();
#endif

	if (0==triCount) return;

	// Build shading table
	float alpha255[dm_obj_in_slot][4];
	for (int i=0; i<dm_obj_in_slot; i++)
	{
		alpha255[i][0] = 255.f*float(DS.palette[i].a0)/15.f;
		alpha255[i][1] = 255.f*float(DS.palette[i].a1)/15.f;
		alpha255[i][2] = 255.f*float(DS.palette[i].a2)/15.f;
		alpha255[i][3] = 255.f*float(DS.palette[i].a3)/15.f;
	}
#ifndef _EDITOR 
	extern float ps_r__detail_rnd_scale_min;
	extern float ps_r__detail_rnd_scale_max;
	float rnd_scale_min = ps_r__detail_rnd_scale_min;
	float rnd_scale_max = ps_r__detail_rnd_scale_max;
#else
	float rnd_scale_min = 0.5f;
	float rnd_scale_max = 0.9f;
#endif
	// Prepare to selection
	float density = ps_r__Detail_density;
	float jitter = density/1.7f;
	u32 d_size = iCeil(dm_slot_size/density);

    s32 p_rnd = D.vis.box.max.x*D.vis.box.max.y*D.vis.box.max.z; // нужно для того чтобы убрать полосы(ряды)
	CRandom r_selection(p_rnd);
	CRandom r_jitter(p_rnd);
	CRandom r_yaw(p_rnd);
	CRandom r_scale(p_rnd);

	// Prepare to actual-bounds-calculations
	Fbox Bounds, ItemBB; Bounds.invalidate();
	Fvector Item_P;
	Fvector	dir; dir.set(0, -1, 0);
	Fvector normal; normal.set(0, 1, 0);
	Fquaternion q;
	Fmatrix mResult;
	// Decompressing itself
	u32 max_items_in_slot = (d_size + 1) * (d_size + 1);
	bool empty_slot = true;
	for (u32 z=0; z<=d_size; z++)
	{
		for (u32 x=0; x<=d_size; x++)
		{
			// Iterpolate and dither palette
			FixedVector<int, dm_obj_in_slot>selected;

			if ((DS.id0!=DetailSlot::ID_Empty)) selected.push_back(0);
			if ((DS.id1!=DetailSlot::ID_Empty)) selected.push_back(1);
			if ((DS.id2!=DetailSlot::ID_Empty)) selected.push_back(2);
			if ((DS.id3!=DetailSlot::ID_Empty)) selected.push_back(3);
			
			// Select
			if (selected.empty()) continue;

			u32 index = (selected.size() == 1) ? selected[0] : selected[r_selection.randI(selected.size())];

			// shift
			u32 shift_x = r_jitter.randI(16);
			u32 shift_z = r_jitter.randI(16);

			if(!InterpolateAndDither(alpha255[index], x, z, shift_x, shift_z, d_size, dither)) continue;

			// Position (XZ)
			float rx = (float(x)/float(d_size))*dm_slot_size + D.vis.box.min.x;
			float rz = (float(z)/float(d_size))*dm_slot_size + D.vis.box.min.z;

			Item_P.set(rx + r_jitter.randFs(jitter), D.vis.box.max.y, rz + r_jitter.randFs(jitter));

			// Position (Y)
			float y = D.vis.box.min.y-5;
			float r_u,r_v,r_range;
			bool no_push = false;
			for (u32 tid=0; tid<triCount; tid++)
			{
#ifdef _EDITOR
				Fvector verts[3];
				SBoxPickInfo& I=pinf[tid];
				for (int k=0; k<(int)I.inf.size(); k++)
				{
					VERIFY(I.s_obj);
					I.e_obj->GetFaceWorld(I.s_obj->_Transform(),I.e_mesh,I.inf[k].id,verts);
					if (CDB::TestRayTri(Item_P,dir,verts,r_u,r_v,r_range,true))
					{
						if (r_range>=0)
						{
							float y_test = Item_P.y - r_range;
							if (y_test>y)
								y = y_test;
						}
						normal.mknormal(verts[0], verts[1], verts[2]);
					}
				}
#else
				CDB::RESULT& R = results[tid];
				CDB::TRI& T = tris[R.id];
				SGameMtl* mtl = GMLib.GetMaterialByIdx(T.material);

				if(mtl->Flags.test(SGameMtl::flPassable))	
					continue;

				//Detect sector
				if(RImplementation.pOutdoorSector && T.sector < RImplementation.Sectors.size())
				{
					CSector* sector = (CSector*)RImplementation.Sectors[T.sector];
					if (sector != RImplementation.pOutdoorSector)
					{
						no_push = true;
						break;
					}
				}
				if (CDB::TestRayTri(Item_P,dir, R.verts,r_u,r_v,r_range,true))
				{
					if (r_range>=0)
					{
						float y_test = Item_P.y - r_range;
						if (y_test>y)
							y = y_test;
					}
					normal.mknormal(R.verts[0], R.verts[1], R.verts[2]);
					break;
				}
#endif
			}
			if(no_push) continue;
			if (y<D.vis.box.min.y)
				continue;

			Item_P.y = y;

			empty_slot = false;
			u8 obj_id = DS.r_id(index);

			if (obj_id == DetailSlot::ID_Empty) continue;
#ifndef _EDITOR 
			const CDetail& Dobj = objects[obj_id];
#else
			const CDetail& Dobj = *objects[obj_id];
#endif

			float scale = r_scale.randF(Dobj.m_fMinScale * rnd_scale_min, Dobj.m_fMaxScale * rnd_scale_max);
			mResult.k.x = r_yaw.randF(-0.99, 0.99);
			mResult.k.z = r_yaw.randF(-0.99, 0.99);

			u8 vis_ID = Dobj.m_Flags.is(DO_NO_WAVING) ? 0 : Random.randI(1, 3);
			//чтобы (только) листики травы ложились на поверхность террейна
			//	if (vis_ID == 0)
			ground_correction(mResult, normal);
			float radius = scale * Dobj.bv_bb.getradius();
			ItemBB.setb(Item_P, {radius ,radius ,radius});
			Bounds.merge(ItemBB);
			// Save it
			q.set(mResult);
			q.normalize();

			float hemi = DS.r_qclr(DS.c_hemi, 15) + EPS;
			hemi = DS.r_qclr(DS.c_dir, 15) > 0.07f ? hemi : -hemi;

			CDetail::SlotItem* ItemP = items_pool.create();
			ItemP->quat = { q.x, q.y, q.z };
			ItemP->scale = scale;
			ItemP->pos = Item_P;
			ItemP->c_hemi = hemi;

			SlotPart& SP = D.G[index];
			SP.id = obj_id;
			SP.items[vis_ID].reserve(max_items_in_slot);
			SP.items[vis_ID].push_back(ItemP);
		}
	}
	if(empty_slot)
	{
		D.empty = TRUE;
		return;
	}

	// Update bounds to more tight and real ones
	D.vis.clear();
	D.vis.box.set(Bounds);
	D.vis.box.getsphere(D.vis.sphere.P, D.vis.sphere.R);
}
