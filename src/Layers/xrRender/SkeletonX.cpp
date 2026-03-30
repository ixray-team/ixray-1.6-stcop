// SkeletonX.cpp: implementation of the CSkeletonX class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#ifndef _EDITOR
	#include	"../../xrEngine/Render.h"
#else
	#include "../../xrCore/API/xrAPI.h"
#endif

#include "SkeletonX.h"

struct vertRender// T&B are not skinned, because in R2 skinning occurs always in hardware
{
	Fvector	P;
	Fvector	N;
	float	u, v;
};

ICF void  xrSkin1W_x86(	vertRender*		D,
								vertBoned1W*	S,
								u32				vCount,
								CBoneInstance*	Bones) 
{
	// Prepare
	int U_Count			= vCount/8;
	vertBoned1W*	V	= S;
	vertBoned1W*	E	= V+U_Count*8;

	// Unrolled loop
	for (; S!=E; )
	{
		Fmatrix& M0		= Bones[S->m].mRenderTransform;
		M0.transform_tiny(D->P,S->P);
		M0.transform_dir (D->N,S->N);
		D->u			= S->u;
		D->v			= S->v;
		S++; D++;
		
		Fmatrix& M1		= Bones[S->m].mRenderTransform;
		M1.transform_tiny(D->P,S->P);
		M1.transform_dir (D->N,S->N);
		D->u			= S->u;
		D->v			= S->v;
		S++; D++;
		
		Fmatrix& M2		= Bones[S->m].mRenderTransform;
		M2.transform_tiny(D->P,S->P);
		M2.transform_dir (D->N,S->N);
		D->u			= S->u;
		D->v			= S->v;
		S++; D++;
		
		Fmatrix& M3		= Bones[S->m].mRenderTransform;
		M3.transform_tiny(D->P,S->P);
		M3.transform_dir (D->N,S->N);
		D->u			= S->u;
		D->v			= S->v;
		S++; D++; 
		
		Fmatrix& M4		= Bones[S->m].mRenderTransform;
		M4.transform_tiny(D->P,S->P);
		M4.transform_dir (D->N,S->N);
		D->u			= S->u;
		D->v			= S->v;
		S++; D++;
		
		Fmatrix& M5		= Bones[S->m].mRenderTransform;
		M5.transform_tiny(D->P,S->P);
		M5.transform_dir (D->N,S->N);
		D->u			= S->u;
		D->v			= S->v;
		S++; D++;
		
		Fmatrix& M6		= Bones[S->m].mRenderTransform;
		M6.transform_tiny(D->P,S->P);
		M6.transform_dir (D->N,S->N);
		D->u			= S->u;
		D->v			= S->v;
		S++; D++;
		
		Fmatrix& M7		= Bones[S->m].mRenderTransform;
		M7.transform_tiny(D->P,S->P);
		M7.transform_dir (D->N,S->N);
		D->u			= S->u;
		D->v			= S->v;
		S++; D++; 
	}
	
	// The end part
	vertBoned1W* E2 = V+vCount;
	for (; S!=E2; )
	{
		Fmatrix& M		= Bones[S->m].mRenderTransform;
		M.transform_tiny(D->P,S->P);
		M.transform_dir (D->N,S->N);
		D->u			= S->u;
		D->v			= S->v;
		S++; D++;
	}
}
 
ICF void  xrSkin2W_x86(vertRender*		D,
							vertBoned2W*	S,
							u32				vCount,
							CBoneInstance*	Bones) 
{
	// Prepare
	int U_Count			= vCount;
	vertBoned2W*	V	= S;
	vertBoned2W*	E	= V+U_Count;
	Fvector			P0,N0,P1,N1;

	// NON-Unrolled loop
	for (; S!=E; ){
    	if (S->m[1] !=S->m[0]){
            Fmatrix& M0		= Bones[S->m[0]].mRenderTransform;
            Fmatrix& M1		= Bones[S->m[1]].mRenderTransform;
            M0.transform_tiny(P0,S->P);
            M0.transform_dir (N0,S->N);
            M1.transform_tiny(P1,S->P);
            M1.transform_dir (N1,S->N);
            D->P.lerp		(P0,P1,S->w);
            D->N.lerp		(N0,N1,S->w);
            D->u			= S->u;
            D->v			= S->v;
        }else{
            Fmatrix& M0		= Bones[S->m[0]].mRenderTransform;
            M0.transform_tiny(D->P,S->P);
            M0.transform_dir (D->N,S->N);
            D->u			= S->u;
            D->v			= S->v;
        }
		S++; D++;
	}
}



ICF void  xrSkin3W_x86(vertRender*		D,
							vertBoned3W*	S,
							u32				vCount,
							CBoneInstance*	Bones) 
{
	// Prepare
	int U_Count			= vCount;
	vertBoned3W*	V	= S;
	vertBoned3W*	E	= V+U_Count;
	Fvector			P0,N0,P1,N1,P2,N2;

	// NON-Unrolled loop
	for (; S!=E; )
	{
		Fmatrix& M0		= Bones[ S->m[0] ].mRenderTransform;
        Fmatrix& M1		= Bones[ S->m[1] ].mRenderTransform;
        Fmatrix& M2		= Bones[ S->m[2] ].mRenderTransform;

		M0.transform_tiny(P0,S->P); P0.mul(S->w[0]);
        M0.transform_dir (N0,S->N); N0.mul(S->w[0]);

        M1.transform_tiny(P1,S->P); P1.mul(S->w[1]);
        M1.transform_dir (N1,S->N); N1.mul(S->w[1]);

        M2.transform_tiny(P2,S->P); P2.mul(1.0f-S->w[0]-S->w[1]);
        M2.transform_dir (N2,S->N); N2.mul(1.0f-S->w[0]-S->w[1]);

		P0.add(P1);
		P0.add(P2);

		D->P			= P0;

		N0.add(N1);
		N0.add(N2);

		D->N			= N0;
		
		D->u			= S->u;
        D->v			= S->v;

		S++; 
		D++;
	}
}



ICF void  xrSkin4W_x86(vertRender*		D,
							vertBoned4W*	S,
							u32				vCount,
							CBoneInstance*	Bones) 
{
	// Prepare
	int U_Count			= vCount;
	vertBoned4W*	V	= S;
	vertBoned4W*	E	= V+U_Count;
	Fvector			P0,N0,P1,N1,P2,N2,P3,N3;

	// NON-Unrolled loop
	for (; S!=E; )
	{
	Fmatrix& M0		= Bones[ S->m[0] ].mRenderTransform;
        Fmatrix& M1		= Bones[ S->m[1] ].mRenderTransform;
        Fmatrix& M2		= Bones[ S->m[2] ].mRenderTransform;
        Fmatrix& M3		= Bones[ S->m[3] ].mRenderTransform;

	M0.transform_tiny(P0,S->P); P0.mul(S->w[0]);
        M0.transform_dir (N0,S->N); N0.mul(S->w[0]);

        M1.transform_tiny(P1,S->P); P1.mul(S->w[1]);
        M1.transform_dir (N1,S->N); N1.mul(S->w[1]);

        M2.transform_tiny(P2,S->P); P2.mul(S->w[2]);
        M2.transform_dir (N2,S->N); N2.mul(S->w[2]);

	M3.transform_tiny(P3,S->P); P3.mul(1.0f-S->w[0]-S->w[1]-S->w[2]);
        M3.transform_dir (N3,S->N); N3.mul(1.0f-S->w[0]-S->w[1]-S->w[2]);

		P0.add(P1);
		P0.add(P2);
		P0.add(P3);

		D->P			= P0;
		
		N0.add(N1);
		N0.add(N2);
		N0.add(N3);

		D->N			= N0;
		
		D->u			= S->u;
        D->v			= S->v;

		S++; 
		D++;
	}
}


shared_str	s_bones_array_const;

#ifdef USE_DX11
shared_str	s_bones_array_const_old;
#endif // USE_DX11

//////////////////////////////////////////////////////////////////////
// Body Part
//////////////////////////////////////////////////////////////////////
void CSkeletonX::AfterLoad	(CKinematics* parent, u16 child_idx)
{
	SetParent				(parent);
    ChildIDX				= child_idx;
	if (progressive_mesh)
	{
		FSlideWindow& SW = nSWI.sw[0]; // max LOD
		CSkeletonX::_CollectBoneFaces(this, iBase + SW.offset, SW.num_tris * 3);
	}
	else
		CSkeletonX::_CollectBoneFaces(this, iBase, iCount);
}
void CSkeletonX::_Copy(CSkeletonX *B)
{
	Parent					= nullptr;
	ChildIDX				= B->ChildIDX;
	Vertices1W				= B->Vertices1W;
	Vertices2W				= B->Vertices2W;
	Vertices3W				= B->Vertices3W;
	Vertices4W				= B->Vertices4W;
	BonesUsed				= B->BonesUsed;

	// caution - overlapped (union)
	cache_DiscardID			= B->cache_DiscardID;
	cache_vCount			= B->cache_vCount;
	cache_vOffset			= B->cache_vOffset;
	RenderMode				= B->RenderMode;
	RMS_boneid				= B->RMS_boneid;
	RMS_bonecount			= B->RMS_bonecount;

	m_Indices				= B->m_Indices;
}

void CSkeletonX::Render(float LOD)
{
	//PROF_EVENT("CSkeletonX_PM::Render");
	if (progressive_mesh)
	{
		int lod_id = FProgressive::last_lod;
		if (LOD >= 0.f)
		{
			clamp(LOD, 0.f, 1.f);
			lod_id = iFloor((1.f - LOD) * float(nSWI.count - 1) + 0.5f);
			FProgressive::last_lod = lod_id;
		}
		VERIFY(lod_id >= 0 && lod_id<int(nSWI.count));
		FSlideWindow& SW = nSWI.sw[lod_id];
		_Render(rm_geom, SW.num_verts, SW.offset, SW.num_tris);
	}
	else
		_Render(rm_geom, vCount, 0, dwPrimitives);
}

void CSkeletonX::_Render(ref_geom& hGeom, u32 vCount, u32 iOffset, u32 pCount)
{
	//PROF_EVENT("CSkeletonX::_Render");

#ifdef USE_DX11
	if(RImplementation.phase == RImplementation.PHASE_NORMAL) {
		Parent->StoreVisualMatrix(RCache.xforms.m_w);

		if(RenderMode != RM_SINGLE) {
			RCache.set_xform_world_old(Parent->mOldWorldMartrix);
		}
	}
#endif

	RCache.stat.r.s_dynamic.add		(vCount);
	switch (RenderMode)
	{
	case RM_SKINNING_SOFT:
		_Render_soft		(hGeom,vCount,iOffset,pCount);
		RCache.stat.r.s_dynamic_sw.add	(vCount);
		break;
	case RM_SINGLE:	
		{
			//PROF_EVENT("RM_SINGLE")
			Fmatrix	W;	W.mul_43(RCache.xforms.m_w, Parent->bone_instances[u16(RMS_boneid)].mRenderTransform);

			RCache.set_xform_world	(W);

#ifdef USE_DX11
			if(RImplementation.phase == RImplementation.PHASE_NORMAL) {
				Fmatrix	O; O.mul_43(Parent->mOldWorldMartrix, Parent->bone_instances[u16(RMS_boneid)].mRenderTransform_old);
				RCache.set_xform_world_old(O);
			}
#endif

			RCache.set_Geometry(hGeom);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, vCount, iOffset, pCount);
			RCache.stat.r.s_dynamic_inst.add(vCount);
		}
		break;
	case RM_SKINNING_1B:
	case RM_SKINNING_2B:
	case RM_SKINNING_3B:
	case RM_SKINNING_4B:
		{
			//PROF_EVENT("RM_SKINNING")
			// transfer matrices
			ref_constant array = RCache.get_c(s_bones_array_const);

#ifdef USE_DX11
			ref_constant array_old = RImplementation.phase == RImplementation.PHASE_NORMAL ? RCache.get_c(s_bones_array_const_old) : array;
#endif // USE_DX11
			{
				//PROF_EVENT("SEND_MATRICES")
				u16 count = u16(RMS_bonecount);
				for (u16 mid = 0; mid < count; mid++)
				{
					u32 id = u32(mid * 3);

					Fmatrix& M = Parent->bone_instances[mid].mRenderTransform;
					RCache.set_ca(&*array, id + 0, M._11, M._21, M._31, M._41);
					RCache.set_ca(&*array, id + 1, M._12, M._22, M._32, M._42);
					RCache.set_ca(&*array, id + 2, M._13, M._23, M._33, M._43);

#ifdef USE_DX11
					if(RImplementation.phase == RImplementation.PHASE_NORMAL)
					{
						Fmatrix& O = Parent->bone_instances[mid].mRenderTransform_old;
						RCache.set_ca(&*array_old, id + 0, O._11, O._21, O._31, O._41);
						RCache.set_ca(&*array_old, id + 1, O._12, O._22, O._32, O._42);
						RCache.set_ca(&*array_old, id + 2, O._13, O._23, O._33, O._43);
					}
#endif // USE_DX11
				}
			}
			// render
			RCache.set_Geometry(hGeom);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,0,0,vCount,iOffset,pCount);
			if (RM_SKINNING_1B==RenderMode)	
				RCache.stat.r.s_dynamic_1B.add	(vCount);
			else
			if (RM_SKINNING_2B==RenderMode)	
				RCache.stat.r.s_dynamic_2B.add	(vCount);
			else
			if (RM_SKINNING_3B==RenderMode)	
				RCache.stat.r.s_dynamic_3B.add	(vCount);
			else
			if (RM_SKINNING_4B==RenderMode)	
				RCache.stat.r.s_dynamic_4B.add	(vCount);
		}
		break;
	}
}
void CSkeletonX::_Render_soft	(ref_geom& hGeom, u32 vCount, u32 iOffset, u32 pCount)
{
	//PROF_EVENT("CSkeletonX::_Render_soft")
	u32 vOffset				= cache_vOffset;

	_VertexStream&	_VS		= RCache.Vertex;
	if (cache_DiscardID!=_VS.DiscardID() || vCount!=cache_vCount )
	{
		vertRender*	Dest	= (vertRender*)_VS.Lock(vCount,hGeom->vb_stride,vOffset);
		cache_DiscardID		= _VS.DiscardID();
		cache_vCount		= vCount;
		cache_vOffset		= vOffset;
		
		RDEVICE.Statistic->RenderDUMP_SKIN.Begin();
		if (*Vertices1W)
		{
			xrSkin1W_x86(
				Dest,										// dest
				*Vertices1W,								// source
				vCount,										// count
				Parent->bone_instances						// bones
				);
		}else 
		if(*Vertices2W)
		{
			xrSkin2W_x86(
				Dest,										// dest
				*Vertices2W,								// source
				vCount,										// count
				Parent->bone_instances						// bones
				);
		}else
		if(*Vertices3W)
		{
			xrSkin3W_x86(
				Dest,										// dest
				*Vertices3W,								// source
				vCount,										// count
				Parent->bone_instances						// bones
				);
		}else
		if(*Vertices4W)
		{
			xrSkin4W_x86(
				Dest,										// dest
				*Vertices4W,								// source
				vCount,										// count
				Parent->bone_instances						// bones
				);
		}else
			R_ASSERT2(0,"unsupported soft rendering");

		RDEVICE.Statistic->RenderDUMP_SKIN.End();
		_VS.Unlock(vCount,hGeom->vb_stride);
	}

	RCache.set_Geometry(hGeom);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,vOffset,0,vCount,iOffset,pCount);
}

void CSkeletonX::Load(const char* N, IReader* data, u32 dwFlags)
{
	_Load(N, data, vCount);
	void* _verts_ = data->pointer();
	if (progressive_mesh)
		FProgressive::Load(N, data, dwFlags | VLOAD_NOVERTICES);
	else
		Fvisual::Load(N, data, dwFlags | VLOAD_NOVERTICES);
	Engine.External.SetSkinningMode();
	_DuplicateIndices(N, data);

	vBase = 0;
	_Load_hw(*this, _verts_);
}

void CSkeletonX::_Load	(const char* N, IReader *data, u32& dwVertCount) 
{	
	s_bones_array_const		= "sbones_array";
#ifdef USE_DX11
	s_bones_array_const_old = "sbones_array_old";
#endif // USE_DX11

	xr_vector<u16>			bids;

	// Load vertices
	bool FoundedChunk = !!data->find_chunk(OGF_VERTICES);
	R_ASSERT2(FoundedChunk, "Not found chunk OGF_VERTICES");

	//u16 hw_bones_cnt = u16((Caps.geometry.dwRegisters-22)/3);
	//	Igor: some shaders in r1 need more free constant registers
	u16 hw_bones_cnt = 65; // 75 // u16((256 - 22 - 3) / 3);
	u16 sw_bones_cnt = 0;

#if 0 //def _EDITOR
	hw_bones_cnt = 0;
#endif

#ifdef USE_DX11
	hw_bones_cnt = 254;
#endif

	u32								dwVertType,size,it,crc;
	dwVertType						= data->r_u32(); 
	dwVertCount						= data->r_u32();

	Engine.External.SetSkinningMode();
	
	switch(dwVertType)
	{
	case OGF_VERTEXFORMAT_FVF_1L: // 1-Link
	case 1:
		{
			size					= dwVertCount*sizeof(vertBoned1W);
			vertBoned1W* pVO		= (vertBoned1W*)data->pointer();

			for (it=0; it<dwVertCount; ++it)
			{
				const vertBoned1W& VB = pVO[it];
				u16 mid				= (u16)VB.m;
				
				if(bids.end() == std::find(bids.begin(),bids.end(),mid))	
					bids.push_back	(mid);

				sw_bones_cnt		= std::max(sw_bones_cnt, mid);
			}
#ifdef _EDITOR
			// software
			crc						= crc32	(data->pointer(),size);
			Vertices1W.create		(crc,dwVertCount,(vertBoned1W*)data->pointer());
#else
			if (1 == bids.size())
			{
				// HW- single bone
				RenderMode = RM_SINGLE;
				RMS_boneid = *bids.begin();
				Engine.External.SetSkinningMode(0);
			}
			else if (sw_bones_cnt <= hw_bones_cnt)
			{
				// HW- one weight
				RenderMode = RM_SKINNING_1B;
				RMS_bonecount = sw_bones_cnt + 1;
				Engine.External.SetSkinningMode(1);
			}
			else
			{
				// software
				crc = crc32(data->pointer(), size);
				Vertices1W.create(crc, dwVertCount, (vertBoned1W*)data->pointer());
				Engine.External.SetSkinningMode();
			}
#endif        
		}
		break;
	case OGF_VERTEXFORMAT_FVF_2L: // 2-Link
	case 2:
		{
			size								= dwVertCount*sizeof(vertBoned2W);
			vertBoned2W* pVO					= (vertBoned2W*)data->pointer();

			for(it=0; it<dwVertCount; ++it)
			{
				const vertBoned2W& VB			= pVO[it];
				sw_bones_cnt					= std::max(sw_bones_cnt, VB.m[0]);
				sw_bones_cnt					= std::max(sw_bones_cnt, VB.m[1]);

				if(bids.end() == std::find(bids.begin(),bids.end(),VB.m[0]))
					bids.push_back(VB.m[0]);

				if(bids.end() == std::find(bids.begin(),bids.end(),VB.m[1]))
					bids.push_back(VB.m[1]);
			}
//.			R_ASSERT(sw_bones_cnt<=hw_bones_cnt);
			if (sw_bones_cnt <= hw_bones_cnt)
			{
				// HW- two weights
				RenderMode = RM_SKINNING_2B;
				RMS_bonecount = sw_bones_cnt + 1;
				Engine.External.SetSkinningMode(2);
			}
			else
			{
				// software
				crc = crc32(data->pointer(), size);
				Vertices2W.create(crc, dwVertCount, (vertBoned2W*)data->pointer());
				Engine.External.SetSkinningMode();
			}
		}break;
	case OGF_VERTEXFORMAT_FVF_3L: // 3-Link
	case 3:
		{
			size								= dwVertCount*sizeof(vertBoned3W);
			vertBoned3W* pVO					= (vertBoned3W*)data->pointer();

			for(it=0; it<dwVertCount; ++it)
			{
				const vertBoned3W& VB			= pVO[it];
				for(int i=0; i<3; ++i)
				{
					sw_bones_cnt				= std::max(sw_bones_cnt, VB.m[i]);

					if(bids.end() == std::find(bids.begin(),bids.end(),VB.m[i]))	
						bids.push_back(VB.m[i]);
				}
			}
//.			R_ASSERT(sw_bones_cnt<=hw_bones_cnt);
			if ((sw_bones_cnt <= hw_bones_cnt))
			{
				RenderMode = RM_SKINNING_3B;
				RMS_bonecount = sw_bones_cnt + 1;
				Engine.External.SetSkinningMode(3);
			}
			else
			{
				crc = crc32(data->pointer(), size);
				Vertices3W.create(crc, dwVertCount, (vertBoned3W*)data->pointer());
				Engine.External.SetSkinningMode();
			}
		}break;
	case OGF_VERTEXFORMAT_FVF_4L: // 4-Link
	case 4:
		{
			size								= dwVertCount*sizeof(vertBoned4W);
			vertBoned4W* pVO					= (vertBoned4W*)data->pointer();

			for(it=0; it<dwVertCount; ++it)
			{
				const vertBoned4W& VB			= pVO[it];

				for(int i=0; i<4; ++i)
				{
					sw_bones_cnt				= std::max(sw_bones_cnt, VB.m[i]);

					if(bids.end() == std::find(bids.begin(),bids.end(),VB.m[i]))	
						bids.push_back(VB.m[i]);
				}
			}
//.			R_ASSERT(sw_bones_cnt<=hw_bones_cnt);
			if(sw_bones_cnt<=hw_bones_cnt)
			{
				RenderMode						= RM_SKINNING_4B;
				RMS_bonecount					= sw_bones_cnt+1;
				Engine.External.SetSkinningMode(4);
			}
			else
			{
				crc								= crc32	(data->pointer(),size);
				Vertices4W.create				(crc,dwVertCount,(vertBoned4W*)data->pointer());
				Engine.External.SetSkinningMode();
			}
		}break;
	default:
		Debug.fatal	(DEBUG_INFO,"Invalid vertex type in skinned model '%s'",N);
		break;
	}
#ifdef _EDITOR
	if (bids.size()>0)	
#else
	if (bids.size()>1)	
#endif
    {
		crc					= crc32(&*bids.begin(),bids.size()*sizeof(u16)); 
		BonesUsed.create	(crc,(u32)bids.size(),&*bids.begin());
	}
}

BOOL CSkeletonX::has_visible_bones()
{
	if	(RM_SINGLE==RenderMode)	
	{
		return Parent->LL_GetBoneVisible((u16)RMS_boneid);
	}

	for (u32 it=0; it<BonesUsed.size(); it++)
		if (Parent->LL_GetBoneVisible(BonesUsed[it]))	
		{
			return	TRUE;
		}
	return	FALSE;
}

void CSkeletonX::fill_verts1W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size,
	u16* indices, CBoneData::FacesVec& faces)
{
	VERIFY(*Vertices1W);
	
	Fvector p[3];
	Fvector test_normal, UV;
	for (u16 face_id : faces)
	{
		u32 idx = face_id * 3;
		for (u32 k = 0; k < 3; k++)
			get_pos_bones(Vertices1W[indices[idx + k]], p[k], Parent->bone_instances);

		test_normal.mknormal(p[0], p[1], p[2]);
		float cosa = test_normal.dotproduct(normal);
		if (cosa < EPS) continue;
		if (CDB::TestSphereTri(wm.ContactPoint(), size, p))
		{
			CSkeletonWallmark::WMFace& F = wm.m_Faces.emplace_back();
			for (u32 k = 0; k < 3; k++)
			{
				vertBoned1W& vert = Vertices1W[indices[idx + k]];
				F.bone_id[k][0] = (u16)vert.m;
				F.vert[k] = vert.P;

				Fvector2& uv = F.uv[k];
				view.transform_tiny(UV, p[k]);
				uv.x = (1.f + UV.x) * .5f;
				uv.y = (1.f - UV.y) * .5f;
			}
		}
	}
}

void CSkeletonX::fill_verts2W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size,
	u16* indices, CBoneData::FacesVec& faces)
{
	VERIFY(*Vertices2W);
	Fvector p[3];
	Fvector test_normal, UV;
	for (u16 face_id : faces)
	{
		u32 idx = face_id * 3;
		for (u32 k = 0; k < 3; k++)
			get_pos_bones(Vertices2W[indices[idx + k]], p[k], Parent->bone_instances);

		test_normal.mknormal(p[0], p[1], p[2]);
		float cosa = test_normal.dotproduct(normal);
		if (cosa < EPS) continue;
		if (CDB::TestSphereTri(wm.ContactPoint(), size, p))
		{
			CSkeletonWallmark::WMFace& F = wm.m_Faces.emplace_back();
			for (u32 k = 0; k < 3; k++)
			{
				vertBoned2W& vert = Vertices2W[indices[idx + k]];
				F.bone_id[k][0] = vert.m[0];
				F.bone_id[k][1] = vert.m[1];
				F.weight[k][0] = vert.w;
				F.vert[k] = vert.P;

				Fvector2& uv = F.uv[k];
				view.transform_tiny(UV, p[k]);
				uv.x = (1.f + UV.x) * .5f;
				uv.y = (1.f - UV.y) * .5f;
			}
		}
	}
}

void CSkeletonX::fill_verts3W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size,
	u16* indices, CBoneData::FacesVec& faces)
{
	VERIFY(*Vertices3W);
	Fvector p[3];
	Fvector test_normal, UV;
	for (u16 face_id : faces)
	{
		u32 idx = face_id * 3;
		for (u32 k = 0; k < 3; k++)
			get_pos_bones(Vertices3W[indices[idx + k]], p[k], Parent->bone_instances);

		test_normal.mknormal(p[0], p[1], p[2]);
		float cosa = test_normal.dotproduct(normal);
		if (cosa < EPS) continue;

		if (CDB::TestSphereTri(wm.ContactPoint(), size, p))
		{
			CSkeletonWallmark::WMFace& F = wm.m_Faces.emplace_back();
			for (u32 k = 0; k < 3; k++)
			{
				const vertBoned3W& vert = Vertices3W[indices[idx + k]];
				F.bone_id[k][0] = vert.m[0];
				F.bone_id[k][1] = vert.m[1];
				F.bone_id[k][2] = vert.m[2];
				F.weight[k][0] = vert.w[0];
				F.weight[k][1] = vert.w[1];
				F.vert[k] = vert.P;

				Fvector2& uv = F.uv[k];
				view.transform_tiny(UV, p[k]);
				uv.x = (1.f + UV.x) * .5f;
				uv.y = (1.f - UV.y) * .5f;
			}
		}
	}
}

void CSkeletonX::fill_verts4W(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size,
	u16* indices, CBoneData::FacesVec& faces)
{
	VERIFY(*Vertices4W);
	Fvector p[3];
	Fvector test_normal, UV;
	for (u16 face_id : faces)
	{
		u32 idx = face_id * 3;
		for (u32 k = 0; k < 3; k++)
			get_pos_bones(Vertices4W[indices[idx + k]], p[k], Parent->bone_instances);

		test_normal.mknormal(p[0], p[1], p[2]);
		float cosa = test_normal.dotproduct(normal);
		if (cosa < EPS) continue;

		if (CDB::TestSphereTri(wm.ContactPoint(), size, p))
		{
			CSkeletonWallmark::WMFace& F = wm.m_Faces.emplace_back();
			for (u32 k = 0; k < 3; k++)
			{
				const vertBoned4W& vert = Vertices4W[indices[idx + k]];
				F.bone_id[k][0] = vert.m[0];
				F.bone_id[k][1] = vert.m[1];
				F.bone_id[k][2] = vert.m[2];
				F.bone_id[k][3] = vert.m[3];
				F.weight[k][0] = vert.w[0];
				F.weight[k][1] = vert.w[1];
				F.weight[k][2] = vert.w[2];
				F.vert[k] = vert.P;

				Fvector2& uv = F.uv[k];
				view.transform_tiny(UV, p[k]);
				uv.x = (1.f + UV.x) * .5f;
				uv.y = (1.f - UV.y) * .5f;
			}
		}
	}
}

void CSkeletonX::_DuplicateIndices(const char* N, IReader *data)
{
	//	We will have trouble with container since don't know were to take readable indices
	VERIFY(!data->find_chunk(OGF_ICONTAINER));

	//	Index buffer replica since we can't read from index buffer in DX10
	bool FoundedChunk = !!data->find_chunk(OGF_INDICES);
	R_ASSERT2(FoundedChunk, "Not found chunk OGF_INDICES");

	u32 iCount			= data->r_u32();

	u32 size				= iCount*2;
	u32 crc					= crc32( data->pointer(), size);
	m_Indices.create		( crc, iCount, (u16*)data->pointer());
}

static RHIInputElementDesc dwDecl_1W[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 20, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
};

static RHIInputElementDesc dwDecl_2W[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 20, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
};

static RHIInputElementDesc dwDecl_3W[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 20, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
};

static RHIInputElementDesc dwDecl_4W[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32A32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 16, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 20, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 0, 28, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 1, ERHI_FORMAT::R8G8B8A8_UNORM, 0, 36, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 }
};

ICF u8 q_N(float v)
{
	int _v = clampr(iFloor((v + 1.f) * 127.5f), 0, 255);
	return	u8(_v);
}

struct vertHW_1W
{
	Fvector _P;
	float dim;
	u32 _N_I;
	u32 _T;
	u32 _B;
	float _tc_i[2];

	ICF void quantize(vertBoned1W* src, u16 Multiplier)
	{
		Fvector N = src->N, T = src->T, B = src->B;
		N.normalize_safe();
		T.normalize_safe();
		B.normalize_safe();
		_P = src->P;
		_N_I = color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(src->m * Multiplier));
		_T = color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), 0);
		_B = color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), 0);
		_tc_i[0] = src->u;
		_tc_i[1] = src->v;
	}
};

struct vertHW_2W
{
	Fvector _P;
	float dim;
	u32 _N_w;
	u32 _T;
	u32 _B;
	float _tc_i[4];

	ICF void quantize(vertBoned2W* src, u16 Multiplier)
	{
		Fvector N = src->N, T = src->T, B = src->B;
		N.normalize_safe();
		T.normalize_safe();
		B.normalize_safe();
		_P = src->P;
		_N_w = color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(clampr(iFloor(src->w * 255.f + .5f), 0, 255)));
		_T = color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), 0);
		_B = color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), 0);
		_tc_i[0] = src->u;
		_tc_i[1] = src->v;
		_tc_i[2] = (float)src->m[0] * Multiplier;
		_tc_i[3] = (float)src->m[1] * Multiplier;
	}
};

struct vertHW_3W
{
	Fvector _P;
	float dim;
	u32 _N_w;
	u32 _T_w;
	u32 _B_i;
	float _tc_i[4];

	ICF void quantize(vertBoned3W* src, u16 Multiplier)
	{
		Fvector N = src->N, T = src->T, B = src->B;
		N.normalize_safe();
		T.normalize_safe();
		B.normalize_safe();
		_P = src->P;
		_N_w = color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(clampr(iFloor(src->w[0] * 255.f + .5f), 0, 255)));
		_T_w = color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), u8(clampr(iFloor(src->w[1] * 255.f + .5f), 0, 255)));
		_B_i = color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), u8(src->m[2] * Multiplier));
		_tc_i[0] = src->u;
		_tc_i[1] = src->v;
		_tc_i[2] = (float)src->m[0] * Multiplier;
		_tc_i[3] = (float)src->m[1] * Multiplier;
	}
};

struct vertHW_4W
{
	Fvector _P;
	float dim;
	u32 _N_w;
	u32 _T_w;
	u32 _B_w;
	float _tc[2];
	u32 _i;
	ICF void quantize(vertBoned4W* src, u16 Multiplier)
	{
		Fvector N = src->N, T = src->T, B = src->B;
		N.normalize_safe();
		T.normalize_safe();
		B.normalize_safe();

		_P = src->P;
		_N_w = color_rgba(q_N(N.x), q_N(N.y), q_N(N.z), u8(clampr(iFloor(src->w[0] * 255.f + .5f), 0, 255)));
		_T_w = color_rgba(q_N(T.x), q_N(T.y), q_N(T.z), u8(clampr(iFloor(src->w[1] * 255.f + .5f), 0, 255)));
		_B_w = color_rgba(q_N(B.x), q_N(B.y), q_N(B.z), u8(clampr(iFloor(src->w[2] * 255.f + .5f), 0, 255)));
		_tc[0] = src->u;
		_tc[1] = src->v;
		_i = color_rgba(u8(src->m[0] * Multiplier), u8(src->m[1] * Multiplier), u8(src->m[2] * Multiplier), u8(src->m[3] * Multiplier));
	}
};

template<typename DeclT, size_t Size>
ICF u32 ComputeStride(const DeclT(&decl)[Size])
{
	u32 stride = 0;
	for (size_t i = 0; i < Size; ++i)
	{
		switch (decl[i].Format)
		{
		case ERHI_FORMAT::R32G32B32A32_FLOAT: stride += 16; break;
		case ERHI_FORMAT::R32G32B32_FLOAT:    stride += 12; break;
		case ERHI_FORMAT::R32G32_FLOAT:       stride += 8;  break;
		case ERHI_FORMAT::R32_FLOAT:          stride += 4;  break;
		case ERHI_FORMAT::R8G8B8A8_UNORM:     stride += 4;  break;
		case ERHI_FORMAT::R8G8B8A8_UINT:      stride += 4;  break;
		default:
			VERIFY2(false, "Unknown DX11 format!");
		}
	}
	return stride;
}

template<typename VertSrc, typename VertHW, typename DeclT, size_t Size, typename VerticesContainer>
ICF void _Load_hw_generic(Fvisual& V, void* _verts_, DeclT(&decl)[Size], VerticesContainer& container)
{
#ifdef USE_DX11
	u16 Multiplier = 1;
#else
	u16 Multiplier = 3;
#endif
	// Back up vertex data
	u32 size = V.vCount * sizeof(VertSrc);
	u32 crc = crc32(_verts_, size);
	container.create(crc, V.vCount, (VertSrc*)_verts_);

	u32 vStride = (u32)ComputeStride(decl);

	VERIFY(vStride == sizeof(VertHW));
	VERIFY(nullptr == V.p_rm_Vertices);

	VertHW* dstOriginal = xr_alloc<VertHW>(V.vCount);
	VertHW* dst = dstOriginal;
	VertSrc* src = (VertSrc*)_verts_;
	for (u32 it = 0; it < V.vCount; it++, dst++, src++)
		dst->quantize(src, Multiplier);

	R_ASSERT(RHIUtils::CreateVertexBuffer(&V.p_rm_Vertices, dstOriginal, V.vCount * vStride));
	xr_free(dstOriginal);
	V.rm_geom.create(decl, Size, V.p_rm_Vertices, V.p_rm_Indices);
}

void CSkeletonX::_Load_hw(Fvisual& V, void* _verts_)
{
	switch (RenderMode)
	{
	case RM_SKINNING_SOFT:
	{
		V.rm_geom.create(vertRenderFVF, RCache.Vertex.Buffer(), V.p_rm_Indices);
		break;
	}
	case RM_SINGLE:
	case RM_SKINNING_1B:
	{
		_Load_hw_generic<vertBoned1W, vertHW_1W>(V, _verts_, dwDecl_1W, Vertices1W);
		break;
	}
	case RM_SKINNING_2B:
	{
		_Load_hw_generic<vertBoned2W, vertHW_2W>(V, _verts_, dwDecl_2W, Vertices2W);
		break;
	}
	case RM_SKINNING_3B:
	{
		_Load_hw_generic<vertBoned3W, vertHW_3W>(V, _verts_, dwDecl_3W, Vertices3W);
		break;
	}
	case RM_SKINNING_4B:
	{
		_Load_hw_generic<vertBoned4W, vertHW_4W>(V, _verts_, dwDecl_4W, Vertices4W);
		break;
	}
	}
}

void CSkeletonX::_CollectBoneFaces(Fvisual* V, u32 iBase, u32 iCount)
{
	u16* indices = *m_Indices + iBase;

	if (*Vertices1W)
	{
		vertBoned1W* vertices = *Vertices1W;
		for (u32 idx = 0; idx < iCount; idx++)
		{
			vertBoned1W& v = vertices[V->vBase + indices[idx]];
			Parent->LL_GetData((u16)v.m).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
	else if (*Vertices2W)
	{
		vertBoned2W* vertices = *Vertices2W;
		for (u32 idx = 0; idx < iCount; idx++)
		{
			vertBoned2W& v = vertices[V->vBase + indices[idx]];
			Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
	else if (*Vertices3W)
	{
		vertBoned3W* vertices = *Vertices3W;
		for (u32 idx = 0; idx < iCount; idx++)
		{
			vertBoned3W& v = vertices[V->vBase + indices[idx]];
			Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[2]).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
	else if (*Vertices4W)
	{
		vertBoned4W* vertices = *Vertices4W;
		for (u32 idx = 0; idx < iCount; idx++)
		{
			vertBoned4W& v = vertices[V->vBase + indices[idx]];
			Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[2]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[3]).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
}

BOOL CSkeletonX::_PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, Fvisual* V, u16 bone_id, u32 iBase, u32 iCount)
{
	VERIFY(Parent && (ChildIDX != u16(-1)));
	CBoneData& BD = Parent->LL_GetData(bone_id);
	CBoneData::FacesVec& faces = BD.child_faces[ChildIDX];
	u16* indices = *m_Indices + iBase;

	if (*Vertices1W)
		return pick_bone<vertBoned1W>(Vertices1W, Parent->bone_instances, r, dist, start, dir, indices, faces);
	else if (*Vertices2W)
		return pick_bone<vertBoned2W>(Vertices2W, Parent->bone_instances, r, dist, start, dir, indices, faces);
	else if (*Vertices3W)
		return pick_bone<vertBoned3W>(Vertices3W, Parent->bone_instances, r, dist, start, dir, indices, faces);
	else if (*Vertices4W)
		return pick_bone<vertBoned4W>(Vertices4W, Parent->bone_instances, r, dist, start, dir, indices, faces);

	return FALSE;
}

void CSkeletonX::_FillVertices(const Fmatrix& view, CSkeletonWallmark& wm, const Fvector& normal, float size, Fvisual* V, u16 bone_id, u32 iBase, u32 iCount)
{
	VERIFY(Parent && (ChildIDX != u16(-1)));
	CBoneData& BD = Parent->LL_GetData(bone_id);
	CBoneData::FacesVec& faces = BD.child_faces[ChildIDX];
	u16* indices = *m_Indices + iBase;

	if (*Vertices1W) fill_verts1W(view, wm, normal, size, indices, faces);
	else if (*Vertices2W) fill_verts2W(view, wm, normal, size, indices, faces);
	else if (*Vertices3W) fill_verts3W(view, wm, normal, size, indices, faces);
	else if (*Vertices4W) fill_verts4W(view, wm, normal, size, indices, faces);
}

void CSkeletonX::_EnumBoneVertices(SEnumVerticesCallback& C, Fvisual* V, u16 bone_id, u32 iBase, u32 iCount)
{
	VERIFY(Parent && (ChildIDX != u16(-1)));
	CBoneData& BD = Parent->LL_GetData(bone_id);
	CBoneData::FacesVec& faces = BD.child_faces[ChildIDX];
	u16* indices = *m_Indices + iBase;

	if (*Vertices1W) enum_verts(Vertices1W, indices, faces, C, Parent->bone_instances);
	else if (*Vertices2W) enum_verts(Vertices2W, indices, faces, C, Parent->bone_instances);
	else if (*Vertices3W) enum_verts(Vertices3W, indices, faces, C, Parent->bone_instances);
	else if (*Vertices4W) enum_verts(Vertices4W, indices, faces, C, Parent->bone_instances);
}
