#include "stdafx.h"
#pragma hdrstop

#include "DetailManager.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

#ifdef USE_DX11
#include "../xrRenderDX10/dx10BufferUtils.h"
#endif // USE_DX11

const int quant = 16384;

static D3DVERTEXELEMENT9 dwDecl[] =
{
	{ 0, 0,  D3DDECLTYPE_FLOAT3,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_POSITION,	0 },	// pos
	{ 0, 12, D3DDECLTYPE_SHORT4,	D3DDECLMETHOD_DEFAULT, 	D3DDECLUSAGE_TEXCOORD,	0 },	// uv
	D3DDECL_END()
};

#pragma pack(push,1)
struct	vertHW
{
	float		x,y,z;
	short		u,v,t,mid;
};
#pragma pack(pop)

short QC (float v)
{
	int t=iFloor(v*float(quant)); clamp(t,-32768,32767);
	return short(t&0xffff);
}

void CDetailManager::hw_Load	()
{
	hw_Load_Geom();
	hw_Load_Shaders();
}

void CDetailManager::hw_Load_Geom()
{
	// Analyze batch-size
	hw_BatchSize = 50;

#ifdef USE_DX11
	hw_BatchSize = 61;
#endif

	// Pre-process objects
	u32			dwVerts		= 0;
	u32			dwIndices	= 0;
	for (u32 o=0; o<objects.size(); o++)
	{
#ifndef _EDITOR 
		const CDetail& D	=	objects[o];
#else
		const CDetail& D	=	*objects[o];
#endif
		dwVerts		+=	D.number_vertices*hw_BatchSize;
		dwIndices	+=	D.number_indices*hw_BatchSize;
	}
	u32			vSize		= sizeof(vertHW);
	Msg("* [DETAILS] %d v(%d), %d p",dwVerts,vSize,dwIndices/3);

#ifndef USE_DX11
	// Determine POOL & USAGE
	u32 dwUsage		=	D3DUSAGE_WRITEONLY;

	// Create VB/IB
	R_CHK			(RDevice->CreateVertexBuffer	(dwVerts*vSize,dwUsage,0,D3DPOOL_MANAGED,&hw_VB,0));
	R_CHK			(RDevice->CreateIndexBuffer	(dwIndices*2,dwUsage,D3DFMT_INDEX16,D3DPOOL_MANAGED,&hw_IB,0));

#endif	//	USE_DX11
	Msg("* [DETAILS] Batch(%d), VB(%dK), IB(%dK)", hw_BatchSize, (dwVerts * vSize) / 1024, (dwIndices * 2) / 1024);

	// Fill VB
	{
		vertHW* pV{};
#ifdef USE_DX11
		vertHW*			pVOriginal;
		pVOriginal	=	xr_alloc<vertHW>(dwVerts);
		pV = pVOriginal;		
#else //USE_DX11
		R_CHK			(hw_VB->Lock(0,0,(void**)&pV,0));
#endif
		for (u32 o=0; o<objects.size(); o++)
		{
#ifndef _EDITOR 
			const CDetail& D	=	objects[o];
#else
			const CDetail& D	=	*objects[o];
#endif
			for (u32 batch=0; batch<hw_BatchSize; batch++)
			{
				u32 mid	=	batch;
				for (u32 v=0; v<D.number_vertices; v++)
				{
					const Fvector&	vP = D.vertices[v].P;
					pV->x	=	vP.x;
					pV->y	=	vP.y;
					pV->z	=	vP.z;
					pV->u	=	QC(D.vertices[v].u);
					pV->v	=	QC(D.vertices[v].v);
					pV->t	=	QC(vP.y/(D.bv_bb.max.y-D.bv_bb.min.y));
					pV->mid	=	short(mid);
					pV++;
				}
			}
		}
#ifdef USE_DX11
		R_CHK(dx10BufferUtils::CreateVertexBuffer(&hw_VB, pVOriginal, dwVerts*vSize));
		xr_free(pVOriginal);
#else //USE_DX11
		R_CHK			(hw_VB->Unlock());
#endif
	}

	// Fill IB
	{
		u16* pI{};
#ifdef USE_DX11
		u16*			pIOriginal;
		pIOriginal = xr_alloc<u16>(dwIndices);
		pI	= pIOriginal;
#else //USE_DX11
		R_CHK			(hw_IB->Lock(0,0,(void**)(&pI),0));
#endif
		for (u32 o=0; o<objects.size(); o++)
		{
#ifndef _EDITOR 
			const CDetail& D	=	objects[o];
#else
			const CDetail& D	=	*objects[o];
#endif
			u16		offset	=	0;
			for (u32 batch=0; batch<hw_BatchSize; batch++)
			{
				for (u32 i=0; i<u32(D.number_indices); i++)
					*pI++	=	u16(u16(D.indices[i]) + u16(offset));
				offset		=	u16(offset+u16(D.number_vertices));
			}
		}
#ifdef USE_DX11
		R_CHK(dx10BufferUtils::CreateIndexBuffer(&hw_IB, pIOriginal, dwIndices*2));
		xr_free(pIOriginal);
#else //USE_DX11
		R_CHK			(hw_IB->Unlock());
#endif
	}

	// Declare geometry
	hw_Geom.create		(dwDecl, hw_VB, hw_IB);
}

void CDetailManager::hw_Unload()
{
	// Destroy VS/VB/IB
	hw_Geom.destroy				();
	_RELEASE					(hw_IB);
	_RELEASE					(hw_VB);
}

#ifndef USE_DX11
void CDetailManager::hw_Load_Shaders()
{
	// Create shader to access constant storage
	ref_shader		S;	S.create("details\\set");
	R_constant_table&	T0	= *(S->E[0]->passes[0]->constants);
	R_constant_table&	T1	= *(S->E[1]->passes[0]->constants);
	hwc_consts			= T0.get("consts");
	hwc_wave			= T0.get("wave");
	hwc_wind			= T0.get("dir2D");
	hwc_array			= T0.get("array");
	hwc_s_consts		= T1.get("consts");
	hwc_s_xform			= T1.get("xform");
	hwc_s_array			= T1.get("array");
}

void CDetailManager::hw_Render(light*L)
{
	PROF_EVENT("CDetailManager::hw_Render")
	RCache.set_CullMode		(CULL_NONE);
	RCache.set_xform_world	(Fidentity);
	// Setup geometry and DMA
	RCache.set_Geometry		(hw_Geom);

	float		scale			=	1.f/float(quant);
	Fvector4	wave;

	// Wave0
	{
		PROF_EVENT("Wave0")
		wave.set(1.f / 5.f, 1.f / 7.f, 1.f / 3.f, m_time_pos);
		RCache.set_c(&*hwc_consts, scale, scale, ps_r__Detail_l_aniso, ps_r__Detail_l_ambient);				// consts
		RCache.set_c(&*hwc_wave, wave.div(PI_MUL_2));	// wave
		RCache.set_c(&*hwc_wind, wave_dir1);																					// wind-dir
		hw_Render_dump(&*hwc_array, 1, 0, L);
	}

	// Wave1
	{
		PROF_EVENT("Wave1")
		wave.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos);
		RCache.set_c(&*hwc_wave, wave.div(PI_MUL_2));	// wave
		RCache.set_c(&*hwc_wind, wave_dir2);																					// wind-dir
		hw_Render_dump(&*hwc_array, 2, 0, L);
	}

	// Still
	{
		PROF_EVENT("Still")
		RCache.set_c(&*hwc_s_consts, scale, scale, scale, 1.f);
		RCache.set_c(&*hwc_s_xform, RDEVICE.mFullTransform);
		hw_Render_dump(&*hwc_s_array, 0, 1, L);
	}

	RCache.set_CullMode		(CULL_CCW);
}

struct InstanceData
{
	Fvector hpb;
	float scale;
	Fvector pos;
	float hemi;
};

void	CDetailManager::hw_Render_dump		(ref_constant x_array, u32 var_id, u32 lod_id, light*L)
{
#if RENDER==R_R2
	if (RImplementation.phase == CRender::PHASE_SMAP && var_id == 0)
		return;
#endif

	// Matrices and offsets
	u32		vOffset	=	0;
	u32		iOffset	=	0;

	// Iterate
#ifndef _EDITOR 
	for (CDetail& Object : objects)
	{
#else
	for (CDetail* D : objects)
	{
		CDetail& Object = *D;
#endif
		// Setup matrices + colors (and flush it as nesessary)
		RCache.set_Element(Object.shader->E[lod_id]);
		RImplementation.apply_lmaterial();
		u32 c_base = x_array->vs.index;
		InstanceData* c_storage = (InstanceData*)RCache.get_ConstantCache_Vertex().get_array_f().access(c_base);

		u32 dwBatch	= 0;

		for (auto& S : Object.m_items[var_id][render_key])
		{
			CDetail::SlotItem& Instance = *S.get();

#ifndef _EDITOR
			if (RImplementation.pOutdoorSector && PortalTraverser.i_marker != RImplementation.pOutdoorSector->r_marker)
				continue;

#if RENDER==R_R2
			if (RImplementation.phase == CRender::PHASE_SMAP && L)
			{
				if(L->position.distance_to_sqr(Instance.pos) >= _sqr(L->range))
					continue;
			}
#endif

#endif  
			c_storage[dwBatch] = {Instance.hpb, Instance.scale_calculated, Instance.pos, Instance.c_hemi};
			dwBatch++;

			if (dwBatch >= hw_BatchSize)
			{
				// flush
				u32 dwCNT_verts			= dwBatch * Object.number_vertices;
				u32 dwCNT_prims			= (dwBatch * Object.number_indices)/3;
				RCache.get_ConstantCache_Vertex().b_dirty				=	TRUE;
				RCache.get_ConstantCache_Vertex().get_array_f().dirty	(c_base,c_base+dwBatch*4);
				RCache.Render			(D3DPT_TRIANGLELIST,vOffset, 0, dwCNT_verts,iOffset,dwCNT_prims);

				// restart
				dwBatch					= 0;
			}
		}
		// flush if nessecary
		if (dwBatch>0&&dwBatch<hw_BatchSize)
		{
			u32 dwCNT_verts			= dwBatch * Object.number_vertices;
			u32 dwCNT_prims			= (dwBatch * Object.number_indices)/3;
			RCache.get_ConstantCache_Vertex().b_dirty				=	TRUE;
			RCache.get_ConstantCache_Vertex().get_array_f().dirty	(c_base,c_base+dwBatch*4);
			RCache.Render				(D3DPT_TRIANGLELIST,vOffset,0,dwCNT_verts,iOffset,dwCNT_prims);
			dwBatch					= 0;
		}
		vOffset		+=	hw_BatchSize * Object.number_vertices;
		iOffset		+=	hw_BatchSize * Object.number_indices;
	}
}

#endif