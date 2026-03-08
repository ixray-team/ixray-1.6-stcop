#include "stdafx.h"
#include "DetailManager.h"

#include "../../xrEngine/GameMtlLib.h"
#include "../../xrCore/Collision/cl_intersect.h"

#ifdef _EDITOR
#	include "../../Editors/LevelEditor/Editor/scene/scene.h"
#endif

//--------------------------------------------------- Decompression
IC float	Interpolate			(float* base,		u32 x, u32 y, u32 size)
{
	float	f	= float(size);
	float	fx	= float(x)/f; float ifx = 1.f-fx;
	float	fy	= float(y)/f; float ify = 1.f-fy;

	float	c01	= base[0]*ifx + base[1]*fx;
	float	c23	= base[2]*ifx + base[3]*fx;

	float	c02	= base[0]*ify + base[2]*fy;
	float	c13	= base[1]*ify + base[3]*fy;

	float	cx	= ify*c01 + fy*c23;
	float	cy	= ifx*c02 + fx*c13;
	return	(cx+cy)/2;
}

IC bool		InterpolateAndDither(float* alpha255,	u32 x, u32 y, u32 sx, u32 sy, u32 size, int dither[16][16] )
{
	clamp 	(x,(u32)0,size-1);
	clamp 	(y,(u32)0,size-1);
	int		c	= iFloor(Interpolate(alpha255,x,y,size)+.5f);
	clamp   (c,0,255);

	u32	row	= (y+sy) % 16;
	u32	col	= (x+sx) % 16;
	return	c	> dither[col][row];
}

static void ground_correction(Fmatrix& xform, const Fvector &ground_normal)
{
	xform.j = ground_normal;

	xform.i.crossproduct(xform.j, xform.k); xform.i.normalize();
	xform.k.crossproduct(xform.i, xform.j); xform.k.normalize();
}

void CDetailManager::cache_Decompress(Slot* S)
{
	VERIFY(S);
	Slot& D = *S;
	D.type = stReady;
	if (D.empty)
		return;

	DetailSlot& DS = QueryDB(D.sx, D.sz);

	// Select polygons
	Fvector bC,bD;
	D.vis.box.get_CD(bC, bD);

#ifdef _EDITOR
	extern ECORE_API CDB::COLLIDER XRC;
	XRC.box_options(CDB::OPT_FULL_TEST);
	// Select polygons
	SBoxPickInfoVec		pinf;
    Scene->BoxPickObjects(D.vis.box,pinf,GetSnapList());
	u32	triCount		= pinf.size();
#else
	xrc.box_options(CDB::OPT_FULL_TEST);
	xrc.box_query(g_pGameLevel->ObjectSpace.GetStaticModel(),bC,bD);
	u32	triCount = xrc.r_count();
	xr_vector<CDB::TRI>& tris = g_pGameLevel->ObjectSpace.GetStaticTris();
	xr_vector<Fvector>& verts = g_pGameLevel->ObjectSpace.GetStaticVerts();
	xr_vector<CDB::RESULT>& results = xrc.r_vec();
#endif

	if (0==triCount)	return;

	// Build shading table
	float		alpha255	[dm_obj_in_slot][4];
	for (int i=0; i<dm_obj_in_slot; i++)
	{
		alpha255[i][0]	= 255.f*float(DS.palette[i].a0)/15.f;
		alpha255[i][1]	= 255.f*float(DS.palette[i].a1)/15.f;
		alpha255[i][2]	= 255.f*float(DS.palette[i].a2)/15.f;
		alpha255[i][3]	= 255.f*float(DS.palette[i].a3)/15.f;
	}

	// Prepare to selection
	float		density		= ps_r__Detail_density;
	float		jitter		= density/1.7f;
	u32			d_size		= iCeil	(dm_slot_size/density);
	svector<int,dm_obj_in_slot>		selected;

    u32 p_rnd	= D.sx*D.sz; // нужно для того чтобы убрать полосы(ряды)
	CRandom				r_selection	(0x12071980^p_rnd);
	CRandom				r_jitter	(0x12071980^p_rnd);
	CRandom				r_yaw		(0x12071980^p_rnd);
	CRandom				r_scale		(0x12071980^p_rnd);

	// Prepare to actual-bounds-calculations
	Fbox				Bounds;
	Bounds.invalidate	();

	// Decompressing itself
	for (u32 z=0; z<=d_size; z++)
	{
		for (u32 x=0; x<=d_size; x++)
		{
			// shift
			u32 shift_x =  r_jitter.randI(16);
			u32 shift_z =  r_jitter.randI(16);

			// Iterpolate and dither palette
			selected.clear();

			if ((DS.id0!=DetailSlot::ID_Empty)&& InterpolateAndDither(alpha255[0],x,z,shift_x,shift_z,d_size,dither))	selected.push_back(0);
			if ((DS.id1!=DetailSlot::ID_Empty)&& InterpolateAndDither(alpha255[1],x,z,shift_x,shift_z,d_size,dither))	selected.push_back(1);
			if ((DS.id2!=DetailSlot::ID_Empty)&& InterpolateAndDither(alpha255[2],x,z,shift_x,shift_z,d_size,dither))	selected.push_back(2);
			if ((DS.id3!=DetailSlot::ID_Empty)&& InterpolateAndDither(alpha255[3],x,z,shift_x,shift_z,d_size,dither))	selected.push_back(3);
			
			// Select
			if (selected.empty())	continue;

			u32 index = (selected.size() == 1) ? selected[0] : selected[r_selection.randI(selected.size())];

#ifndef _EDITOR 
			const CDetail& Dobj	=	objects[DS.r_id(index)];
#else
			const CDetail& Dobj	=	*objects[DS.r_id(index)];
#endif

			CDetail::SlotItem	Item	= CDetail::SlotItem();
			// Position (XZ)
			float		rx = (float(x)/float(d_size))*dm_slot_size + D.vis.box.min.x;
			float		rz = (float(z)/float(d_size))*dm_slot_size + D.vis.box.min.z;
			Fvector		Item_P;

			Item_P.set	(rx + r_jitter.randFs(jitter), D.vis.box.max.y, rz + r_jitter.randFs(jitter));

			// Position (Y)
			float y		= D.vis.box.min.y-5;
			Fvector	dir; dir.set(0,-1,0);
			Fvector normal;normal.set(0,1,0);
			float		r_u,r_v,r_range;
			bool no_push = false;
			for (u32 tid=0; tid<triCount; tid++)
			{
#ifdef _EDITOR
				Fvector verts[3];
				SBoxPickInfo& I=pinf[tid];
				for (int k=0; k<(int)I.inf.size(); k++)
				{
					VERIFY(I.s_obj);
RDEVICE.Statistic->TEST0.Begin	();
					I.e_obj->GetFaceWorld(I.s_obj->_Transform(),I.e_mesh,I.inf[k].id,verts);
RDEVICE.Statistic->TEST0.End		();
					if (CDB::TestRayTri(Item_P,dir,verts,r_u,r_v,r_range,TRUE))
					{
						if (r_range>=0)	{
							float y_test	= Item_P.y - r_range;
							if (y_test>y)	y = y_test;
						}
					}
				}
#else
				CDB::TRI& T = tris[results[tid].id];
				SGameMtl* mtl = GMLib.GetMaterialByIdx(T.material);

				if(mtl->Flags.test(SGameMtl::flPassable))	
					continue;

				//Detect sector
				if(RImplementation.pOutdoorSector)
				{
					CSector* sector = (CSector*)RImplementation.getSector(T.sector);
					if (sector != RImplementation.pOutdoorSector)
					{
						no_push = true;
						break;
					}
				}
				auto& vids = T.verts;
				Fvector Tv[3] = { verts[vids[0]],verts[vids[1]],verts[vids[2]] };
				if (CDB::TestRayTri(Item_P,dir,Tv,r_u,r_v,r_range,TRUE))
				{
					if (r_range>=0)
					{
						float y_test = Item_P.y - r_range;
						if (y_test>y)
							y = y_test;
					}
					normal.mknormal(verts[vids[0]], verts[vids[1]], verts[vids[2]]);
					break;
				}
#endif
			}
			if(no_push) continue;
			if (y<D.vis.box.min.y)
				continue;

			Item_P.y = y;

			// Angles and scale
			Item.scale = r_scale.randF(Dobj.m_fMinScale * 0.5f, Dobj.m_fMaxScale * 0.9f);

			// X-Form BBox
			Fmatrix mRotY;

			mRotY.rotateY(r_yaw.randF(0, PI_MUL_2));

			Item.pos = Item_P;
			mRotY.translate_over(Item_P);

			Fmatrix mScale;
			mScale.scale(Item.scale, Item.scale, Item.scale);

			Fmatrix mXform;
			mXform.mul_43(mRotY, mScale);

			Fbox ItemBB;
			ItemBB.xform(Dobj.bv_bb, mXform);
			Bounds.merge(ItemBB);

			Item.c_hemi = DS.r_qclr(DS.c_hemi, 15)+EPS;
			Item.c_hemi = DS.r_qclr(DS.c_dir, 15)>0.07f ? Item.c_hemi : -Item.c_hemi;

			// Vis-sorting
			Item.vis_ID = Dobj.m_Flags.is(DO_NO_WAVING) ? 0 : Random.randI(1, 3);

			//чтобы (только) листики травы ложились на поверхность террейна
			//	if (Item.vis_ID == 0)

			ground_correction(mRotY, normal);
			// Save it

			mRotY.getHPB(Item.hpb);
			D.G[index].items.emplace_back(xr_make_shared<CDetail::SlotItem>(Item));
		}
	}

	// Update bounds to more tight and real ones
	D.vis.clear();
	D.vis.box.set(Bounds);
	D.vis.box.getsphere(D.vis.sphere.P, D.vis.sphere.R);
}
