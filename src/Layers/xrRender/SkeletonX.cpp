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
	float u, v;
};

ICF void xrSkin1W(vertRender* D, vertBoned1W* S, u32 vCount, CBoneInstance* BI)
{
	// Prepare
	int U_Count = vCount/8;
	vertBoned1W* V = S;
	vertBoned1W* E = V+U_Count*8;

	// Unrolled loop
	while(S!=E)
	{
		Fmatrix& M0 = BI[S->m].mRenderTransform;
		M0.transform_tiny(D->P,S->P);
		M0.transform_dir(D->N,S->N);
		D->u = S->u;
		D->v = S->v;
		S++; D++;
		
		Fmatrix& M1 = BI[S->m].mRenderTransform;
		M1.transform_tiny(D->P,S->P);
		M1.transform_dir(D->N,S->N);
		D->u = S->u;
		D->v = S->v;
		S++; D++;
		
		Fmatrix& M2 = BI[S->m].mRenderTransform;
		M2.transform_tiny(D->P,S->P);
		M2.transform_dir(D->N,S->N);
		D->u = S->u;
		D->v = S->v;
		S++; D++;
		
		Fmatrix& M3 = BI[S->m].mRenderTransform;
		M3.transform_tiny(D->P,S->P);
		M3.transform_dir(D->N,S->N);
		D->u = S->u;
		D->v = S->v;
		S++; D++; 
		
		Fmatrix& M4 = BI[S->m].mRenderTransform;
		M4.transform_tiny(D->P,S->P);
		M4.transform_dir(D->N,S->N);
		D->u = S->u;
		D->v = S->v;
		S++; D++;
		
		Fmatrix& M5 = BI[S->m].mRenderTransform;
		M5.transform_tiny(D->P,S->P);
		M5.transform_dir(D->N,S->N);
		D->u = S->u;
		D->v = S->v;
		S++; D++;
		
		Fmatrix& M6 = BI[S->m].mRenderTransform;
		M6.transform_tiny(D->P,S->P);
		M6.transform_dir(D->N,S->N);
		D->u = S->u;
		D->v = S->v;
		S++; D++;
		
		Fmatrix& M7 = BI[S->m].mRenderTransform;
		M7.transform_tiny(D->P,S->P);
		M7.transform_dir(D->N,S->N);
		D->u = S->u;
		D->v = S->v;
		S++; D++; 
	}
	
	// The end part
	vertBoned1W* E2 = V+vCount;
	while(S!=E2)
	{
		Fmatrix& M = BI[S->m].mRenderTransform;
		M.transform_tiny(D->P,S->P);
		M.transform_dir(D->N,S->N);
		D->u = S->u;
		D->v = S->v;
		S++; D++;
	}
}
 
ICF void xrSkin2W(vertRender* D, vertBoned2W* S, u32 vCount, CBoneInstance* BI) 
{
	// Prepare
	int U_Count = vCount;
	vertBoned2W* V = S;
	vertBoned2W* E = V+U_Count;
	Fvector P0,N0,P1,N1;

	// NON-Unrolled loop
	while(S!=E)
	{
    	if (S->m[1]!=S->m[0])
		{
            Fmatrix& M0 = BI[S->m[0]].mRenderTransform;
            Fmatrix& M1 = BI[S->m[1]].mRenderTransform;
            M0.transform_tiny(P0,S->P);
            M0.transform_dir(N0,S->N);

            M1.transform_tiny(P1,S->P);
            M1.transform_dir(N1,S->N);
            D->P.lerp(P0,P1,S->w);
            D->N.lerp(N0,N1,S->w);
            D->u = S->u;
            D->v = S->v;
        }
		else
		{
            Fmatrix& M0 = BI[S->m[0]].mRenderTransform;
            M0.transform_tiny(D->P,S->P);
            M0.transform_dir (D->N,S->N);
            D->u = S->u;
            D->v = S->v;
        }
		S++; D++;
	}
}

ICF void xrSkin3W(vertRender* D, vertBoned3W* S, u32 vCount, CBoneInstance* BI)
{
	// Prepare
	int U_Count = vCount;
	vertBoned3W* V = S;
	vertBoned3W* E = V+U_Count;
	Fvector P0,N0,P1,N1,P2,N2;

	// NON-Unrolled loop
	while(S!=E)
	{
		Fmatrix& M0 = BI[S->m[0]].mRenderTransform;
        Fmatrix& M1 = BI[S->m[1]].mRenderTransform;
        Fmatrix& M2 = BI[S->m[2]].mRenderTransform;

		M0.transform_tiny(P0,S->P);
        M0.transform_dir(N0,S->N);

        M1.transform_tiny(P1,S->P);
        M1.transform_dir(N1,S->N);

        M2.transform_tiny(P2,S->P);
        M2.transform_dir(N2,S->N);

		float PN2M = 1.0f-S->w[0]-S->w[1];
		D->P = (P0*S->w[0])+(P1*S->w[1])+(P2*PN2M);
		D->N = (N0*S->w[0])+(N1*S->w[1])+(N2*PN2M);
		
		D->u = S->u;
        D->v = S->v;

		S++; 
		D++;
	}
}

ICF void xrSkin4W(vertRender* D,vertBoned4W* S, u32 vCount, CBoneInstance* BI)
{
	// Prepare
	int U_Count = vCount;
	vertBoned4W* V = S;
	vertBoned4W* E = V+U_Count;
	Fvector P0,N0,P1,N1,P2,N2,P3,N3;

	// NON-Unrolled loop
	while(S!=E)
	{
		Fmatrix& M0 = BI[S->m[0]].mRenderTransform;
        Fmatrix& M1 = BI[S->m[1]].mRenderTransform;
        Fmatrix& M2 = BI[S->m[2]].mRenderTransform;
        Fmatrix& M3 = BI[S->m[3]].mRenderTransform;

		M0.transform_tiny(P0,S->P);
        M0.transform_dir(N0,S->N);

        M1.transform_tiny(P1,S->P);
        M1.transform_dir(N1,S->N);

        M2.transform_tiny(P2,S->P);
        M2.transform_dir(N2,S->N);

		M3.transform_tiny(P3,S->P);
        M3.transform_dir(N3,S->N);

		float PN3M = 1.0f - S->w[0] - S->w[1] - S->w[2];
		D->P = (P0*S->w[0])+(P1*S->w[1])+(P2*S->w[2])+(P3*PN3M);
		D->N = (N0*S->w[0])+(N1*S->w[1])+(N2*S->w[2])+(N3*PN3M);
		
		D->u = S->u;
        D->v = S->v;

		S++; 
		D++;
	}
}

shared_str s_bones_array_const;

#ifdef USE_DX11
shared_str s_bones_array_const_old;
#endif // USE_DX11

void CSkeletonX::Copy(dxRender_Visual* V)
{
	if (progressive_mesh)
		FProgressive::Copy(V);
	else
		Fvisual::Copy(V);

	CSkeletonX* B = (CSkeletonX*)V;

	Parent = nullptr;
	ChildIDX = B->ChildIDX;
	Vertices1W = B->Vertices1W;
	Vertices2W = B->Vertices2W;
	Vertices3W = B->Vertices3W;
	Vertices4W = B->Vertices4W;
	BonesUsed = B->BonesUsed;

	// caution - overlapped (union)
	cache_DiscardID = B->cache_DiscardID;
	cache_vCount = B->cache_vCount;
	cache_vOffset = B->cache_vOffset;
	RenderMode = B->RenderMode;
	RMS_boneid = B->RMS_boneid;
	RMS_bonecount = B->RMS_bonecount;

	m_Indices = B->m_Indices;
}

void CSkeletonX::Release()
{
	if (progressive_mesh)
		FProgressive::Release();
	else
		Fvisual::Release();
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

ICF void transfer_matrices(CBoneInstance* BI, u16 bonecount, bool phase_normal)
{
#ifdef USE_DX11
	struct arraybuff{Fvector4 buff[3];};

	arraybuff* array = 0;
	RCache.get_ConstantDirect(s_bones_array_const, bonecount * sizeof(arraybuff), (void**)&array, 0, 0);

	if (!array)
	{
		return;
	}

	arraybuff* array_old = 0;
	RCache.get_ConstantDirect(s_bones_array_const_old, bonecount * sizeof(arraybuff), (void**)&array_old, 0, 0);

	if (phase_normal && array_old)
	{
		for (u16 bid = 0; bid < bonecount; bid++)
		{
			Fmatrix& M = BI[bid].mRenderTransform;
			array[bid] =
			{
				M.i.x, M.j.x, M.k.x, M.c.x,
				M.i.y, M.j.y, M.k.y, M.c.y,
				M.i.z, M.j.z, M.k.z, M.c.z
			};

			Fmatrix& O = BI[bid].mRenderTransform_old;
			array_old[bid] =
			{
				O.i.x, O.j.x, O.k.x, O.c.x,
				O.i.y, O.j.y, O.k.y, O.c.y,
				O.i.z, O.j.z, O.k.z, O.c.z
			};
		}
	}
	else
	{
		for (u16 bid = 0; bid < bonecount; bid++)
		{
			Fmatrix& M = BI[bid].mRenderTransform;

			array[bid] =
			{
				M.i.x, M.j.x, M.k.x, M.c.x,
				M.i.y, M.j.y, M.k.y, M.c.y,
				M.i.z, M.j.z, M.k.z, M.c.z
			};
		}
	}
#else
	ref_constant array = RCache.get_c(s_bones_array_const);
	for (u16 bid = 0; bid < bonecount; bid++)
	{
		Fmatrix& M = BI[bid].mRenderTransform;
		u32 id = u32(bid * 3);

		RCache.set_ca(&*array, id, M.i.x, M.j.x, M.k.x, M.c.x);
		RCache.set_ca(&*array, id + 1, M.i.y, M.j.y, M.k.y, M.c.y);
		RCache.set_ca(&*array, id + 2, M.i.z, M.j.z, M.k.z, M.c.z);
	}
#endif // USE_DX11
}

void CSkeletonX::_Render(ref_geom& hGeom, u32 vCount, u32 iOffset, u32 pCount)
{
	//PROF_EVENT("CSkeletonX::_Render");
	bool phase_normal = RImplementation.phase == RImplementation.PHASE_NORMAL;

#ifdef USE_DX11
	if(phase_normal)
	{
		Parent->StoreVisualMatrix(RCache.xforms.m_w);

		if(RenderMode != RM_SINGLE)
			RCache.set_xform_world_old(Parent->mOldWorldMartrix);
	}
#endif

	//RCache.stat.r.s_dynamic.add		(vCount);
	switch (RenderMode)
	{
	case RM_SKINNING_SOFT:
		_Render_soft(hGeom,vCount,iOffset,pCount);
		//RCache.stat.r.s_dynamic_sw.add	(vCount);
		break;
	case RM_SINGLE:	
		{
			//PROF_EVENT("RM_SINGLE")
			Fmatrix	W;	W.mul_43(RCache.xforms.m_w, Parent->bone_instances[u16(RMS_boneid)].mRenderTransform);

			RCache.set_xform_world	(W);

#ifdef USE_DX11
			if(phase_normal)
			{
				Fmatrix	O; O.mul_43(Parent->mOldWorldMartrix, Parent->bone_instances[u16(RMS_boneid)].mRenderTransform_old);
				RCache.set_xform_world_old(O);
			}
#endif

			RCache.set_Geometry(hGeom);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, vCount, iOffset, pCount);
			//RCache.stat.r.s_dynamic_inst.add(vCount);
		}
		break;
	case RM_SKINNING_1B:
	case RM_SKINNING_2B:
	case RM_SKINNING_3B:
	case RM_SKINNING_4B:
		{
			//PROF_EVENT("RM_SKINNING")
			transfer_matrices(Parent->bone_instances, u16(RMS_bonecount), phase_normal);
			RCache.set_Geometry(hGeom);
			RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST,0,0,vCount,iOffset,pCount);
			//if (RM_SKINNING_1B==RenderMode)	
			//	RCache.stat.r.s_dynamic_1B.add	(vCount);
			//else
			//if (RM_SKINNING_2B==RenderMode)	
			//	RCache.stat.r.s_dynamic_2B.add	(vCount);
			//else
			//if (RM_SKINNING_3B==RenderMode)	
			//	RCache.stat.r.s_dynamic_3B.add	(vCount);
			//else
			//if (RM_SKINNING_4B==RenderMode)	
			//	RCache.stat.r.s_dynamic_4B.add	(vCount);
		}
		break;
	}
}
void CSkeletonX::_Render_soft(ref_geom& hGeom, u32 vCount_, u32 iOffset, u32 pCount)
{
	//PROF_EVENT("CSkeletonX::_Render_soft")
	u32 vOffset = cache_vOffset;

	_VertexStream& _VS = RCache.Vertex;
	if (cache_DiscardID!=_VS.DiscardID() || vCount_ !=cache_vCount )
	{
		vertRender*	Dest = (vertRender*)_VS.Lock(vCount_,hGeom->vb_stride,vOffset);
		cache_DiscardID = _VS.DiscardID();
		cache_vCount = vCount_;
		cache_vOffset = vOffset;
		
		//RDEVICE.Statistic->RenderDUMP_SKIN.Begin();
		if (*Vertices1W)
			xrSkin1W(Dest,*Vertices1W,vCount_,Parent->bone_instances);
		else if(*Vertices2W)
			xrSkin2W(Dest,*Vertices2W,vCount_,Parent->bone_instances);
		else if(*Vertices3W)
			xrSkin3W(Dest,*Vertices3W,vCount_,Parent->bone_instances);
		else if(*Vertices4W)
			xrSkin4W(Dest,*Vertices4W,vCount_,Parent->bone_instances);
		else
			R_ASSERT2(0,"unsupported soft rendering");

		//RDEVICE.Statistic->RenderDUMP_SKIN.End();
		_VS.Unlock(vCount_,hGeom->vb_stride);
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
	_DuplicateIndices(data);

	vBase = 0;
	_Load_hw(_verts_);
}

void CSkeletonX::_Load(const char* N, IReader *data, u32& dwVertCount) 
{	
	s_bones_array_const = "sbones_array";
#ifdef USE_DX11
	s_bones_array_const_old = "sbones_array_old";
#endif // USE_DX11

	// Load vertices
	bool FoundedChunk = !!data->find_chunk(OGF_VERTICES);
	R_ASSERT2(FoundedChunk, "Not found chunk OGF_VERTICES");

	//u16 hw_bones_cnt = u16((Caps.geometry.dwRegisters-22)/3);
	//	Igor: some shaders in r1 need more free constant registers
	u16 hw_bones_cnt = 65; // 75 // u16((256 - 22 - 3) / 3);
	u16 sw_bones_cnt = 0;

#ifdef USE_DX11
	hw_bones_cnt = 254;
#endif

	buffer_vector<u16> bids(_alloca(hw_bones_cnt * sizeof(u16)), hw_bones_cnt);
	//если поймаете исключение замените на xr_vector

	u32 dwVertType,size,it,crc;
	dwVertType = data->r_u32(); 
	dwVertCount = data->r_u32();

	Engine.External.SetSkinningMode();
	
	switch(dwVertType)
	{
	case OGF_VERTEXFORMAT_FVF_1L: // 1-Link
	case 1:
		{
			size = dwVertCount*sizeof(vertBoned1W);
			vertBoned1W* pVO = (vertBoned1W*)data->pointer();

			for (it=0; it<dwVertCount; ++it)
			{
				const vertBoned1W& VB = pVO[it];
				u16 mid = (u16)VB.m;
				
				if(bids.end() == std::find(bids.begin(),bids.end(),mid))	
					bids.push_back	(mid);

				sw_bones_cnt = std::max(sw_bones_cnt, mid);
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
			size = dwVertCount*sizeof(vertBoned2W);
			vertBoned2W* pVO = (vertBoned2W*)data->pointer();

			for(it=0; it<dwVertCount; ++it)
			{
				const vertBoned2W& VB = pVO[it];
				sw_bones_cnt = std::max(sw_bones_cnt, VB.m[0]);
				sw_bones_cnt = std::max(sw_bones_cnt, VB.m[1]);

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
			size = dwVertCount*sizeof(vertBoned3W);
			vertBoned3W* pVO = (vertBoned3W*)data->pointer();

			for(it=0; it<dwVertCount; ++it)
			{
				const vertBoned3W& VB = pVO[it];
				for(int i=0; i<3; ++i)
				{
					sw_bones_cnt = std::max(sw_bones_cnt, VB.m[i]);

					if(bids.end() == std::find(bids.begin(),bids.end(),VB.m[i]))	
						bids.push_back(VB.m[i]);
				}
			}

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
			size = dwVertCount*sizeof(vertBoned4W);
			vertBoned4W* pVO = (vertBoned4W*)data->pointer();

			for(it=0; it<dwVertCount; ++it)
			{
				const vertBoned4W& VB = pVO[it];

				for(int i=0; i<4; ++i)
				{
					sw_bones_cnt = std::max(sw_bones_cnt, VB.m[i]);

					if(bids.end() == std::find(bids.begin(),bids.end(),VB.m[i]))	
						bids.push_back(VB.m[i]);
				}
			}

			if(sw_bones_cnt<=hw_bones_cnt)
			{
				RenderMode = RM_SKINNING_4B;
				RMS_bonecount = sw_bones_cnt+1;
				Engine.External.SetSkinningMode(4);
			}
			else
			{
				crc = crc32	(data->pointer(),size);
				Vertices4W.create(crc,dwVertCount,(vertBoned4W*)data->pointer());
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
		crc = crc32(&*bids.begin(),bids.size()*sizeof(u16)); 
		BonesUsed.create(crc,(u32)bids.size(),&*bids.begin());
	}
}

bool CSkeletonX::has_visible_bones()
{
	if (RM_SINGLE==RenderMode)	
		return Parent->visimask.is((u16)RMS_boneid);

	for (u32 it=0; it<BonesUsed.size(); it++)
	{
		if (Parent->visimask.is(BonesUsed[it]))
			return TRUE;
	}
	return FALSE;
}

void CSkeletonX::_DuplicateIndices(IReader *data)
{
	//	We will have trouble with container since don't know were to take readable indices
	VERIFY(!data->find_chunk(OGF_ICONTAINER));

	//	Index buffer replica since we can't read from index buffer in DX10
	bool FoundedChunk = !!data->find_chunk(OGF_INDICES);
	R_ASSERT2(FoundedChunk, "Not found chunk OGF_INDICES");

	u32 iCount_ = data->r_u32();

	u32 size = iCount_*2;
	u32 crc = crc32( data->pointer(), size);
	m_Indices.create(crc, iCount_, (u16*)data->pointer());
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

template<typename VertSrc, typename VertHW, typename DeclT, size_t Size, typename VerticesContainer>
ICF void _Load_hw_generic(CSkeletonX& V, void* _verts_, DeclT(&decl)[Size], VerticesContainer& container)
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

	u32 vStride = 0;

	for (size_t i = 0; i < Size; ++i)
	{
		switch (decl[i].Format)
		{
		case ERHI_FORMAT::R32G32B32A32_FLOAT: vStride += 16; break;
		case ERHI_FORMAT::R32G32B32_FLOAT:    vStride += 12; break;
		case ERHI_FORMAT::R32G32_FLOAT:       vStride += 8;  break;
		case ERHI_FORMAT::R32_FLOAT:          vStride += 4;  break;
		case ERHI_FORMAT::R8G8B8A8_UNORM:     vStride += 4;  break;
		case ERHI_FORMAT::R8G8B8A8_UINT:      vStride += 4;  break;
		default:
			VERIFY2(false, "Unknown RHI format!");
		}
	}

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

void CSkeletonX::_Load_hw(void* _verts_)
{
	switch (RenderMode)
	{
	case RM_SKINNING_SOFT:
	{
		rm_geom.create(vertRenderFVF, RCache.Vertex.Buffer(), p_rm_Indices);
		break;
	}
	case RM_SINGLE:
	case RM_SKINNING_1B:
	{
		_Load_hw_generic<vertBoned1W, vertHW_1W>(*this, _verts_, dwDecl_1W, Vertices1W);
		break;
	}
	case RM_SKINNING_2B:
	{
		_Load_hw_generic<vertBoned2W, vertHW_2W>(*this, _verts_, dwDecl_2W, Vertices2W);
		break;
	}
	case RM_SKINNING_3B:
	{
		_Load_hw_generic<vertBoned3W, vertHW_3W>(*this, _verts_, dwDecl_3W, Vertices3W);
		break;
	}
	case RM_SKINNING_4B:
	{
		_Load_hw_generic<vertBoned4W, vertHW_4W>(*this, _verts_, dwDecl_4W, Vertices4W);
		break;
	}
	}
}

void CSkeletonX::AfterLoad(CKinematics* parent, u16 child_idx)
{
	SetParent(parent);
	ChildIDX = child_idx;
	_CollectBoneFaces();
}

void CSkeletonX::_CollectBoneFaces()
{
	u16* indices{nullptr};
	u32 iCount_{0};

	if (progressive_mesh)
	{
		FSlideWindow& SW = nSWI.sw[0]; // max LOD
		indices = *m_Indices + iBase + SW.offset;
		iCount_ = SW.num_tris * 3;
	}
	else
	{
		indices = *m_Indices + iBase;
		iCount_ = iCount;
	}

	if (*Vertices1W)
	{
		vertBoned1W* vertices = *Vertices1W;
		for (u32 idx = 0; idx < iCount_; idx++)
		{
			vertBoned1W& v = vertices[vBase + indices[idx]];
			Parent->LL_GetData((u16)v.m).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
	else if (*Vertices2W)
	{
		vertBoned2W* vertices = *Vertices2W;
		for (u32 idx = 0; idx < iCount_; idx++)
		{
			vertBoned2W& v = vertices[vBase + indices[idx]];
			Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
	else if (*Vertices3W)
	{
		vertBoned3W* vertices = *Vertices3W;
		for (u32 idx = 0; idx < iCount_; idx++)
		{
			vertBoned3W& v = vertices[vBase + indices[idx]];
			Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[2]).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
	else if (*Vertices4W)
	{
		vertBoned4W* vertices = *Vertices4W;
		for (u32 idx = 0; idx < iCount_; idx++)
		{
			vertBoned4W& v = vertices[vBase + indices[idx]];
			Parent->LL_GetData((u16)v.m[0]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[1]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[2]).AppendFace(ChildIDX, (u16)(idx / 3));
			Parent->LL_GetData((u16)v.m[3]).AppendFace(ChildIDX, (u16)(idx / 3));
		}
	}
}