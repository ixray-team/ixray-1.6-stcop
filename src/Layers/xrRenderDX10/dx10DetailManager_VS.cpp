#include "stdafx.h"
#include "../xrRender/DetailManager.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

#include "dx10BufferUtils.h"

const int			quant	= 16384;
const int			c_hdr	= 10;
const int			c_size	= 4;

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

short QC (float v);
//{
//	int t=iFloor(v*float(quant)); clamp(t,-32768,32767);
//	return short(t&0xffff);
//}

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
	RCache.set_Geometry(hw_Geom);

	float scale = 1.f / float(quant);
	Fvector4 wave, wave_old, consts;

	auto LodHQ = RImplementation.phase == RImplementation.PHASE_NORMAL ? SE_R2_NORMAL_HQ : SE_R2_DETAIL_SHADOW_HQ;
	auto LodLQ = RImplementation.phase == RImplementation.PHASE_NORMAL ? SE_R2_NORMAL_LQ : SE_R2_DETAIL_SHADOW_LQ;

	// Wave0
	{
		PROF_EVENT("Wave0")
		wave.set(1.f / 5.f, 1.f / 7.f, 1.f / 3.f, m_time_pos);
		wave_old.set(1.f / 5.f, 1.f / 7.f, 1.f / 3.f, m_time_pos_old);

		consts.set(scale, scale, ps_r__Detail_l_aniso, ps_r__Detail_l_ambient);
		hw_Render_dump(consts, wave.div(PI_MUL_2), wave_dir1, wave_old.div(PI_MUL_2), wave_dir1_old, 1, LodHQ, L);
	}

	// Wave1
	{
		PROF_EVENT("Wave1")
		wave.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos);
		wave_old.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos_old);

		hw_Render_dump(consts, wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 2, LodHQ, L);
	}

	// Still
	{
		PROF_EVENT("Still")
		consts.set(scale, scale, scale, 1.f);
		hw_Render_dump(consts, wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 0, LodLQ, L);
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

void CDetailManager::hw_Render_dump(const Fvector4& consts, const Fvector4& wave, const Fvector4& wind, const Fvector4& wave_old, const Fvector4& wind_old, u32 var_id, u32 lod_id, light*L)
{
	if (RImplementation.phase == CRender::PHASE_SMAP && var_id == 0)
		return;

	static shared_str strConsts("consts");
	static shared_str strWave("wave");
	static shared_str strDir2D("dir2D");

	static shared_str strWaveOld("wave_old");
	static shared_str strDir2DOld("dir2D_old");

	static shared_str strArray("array");

	// Matrices and offsets
	u32 vOffset	= 0;
	u32 iOffset	= 0;

	// Iterate
	for (CDetail& Object : objects)
	{
		for ( u32 iPass=0; iPass<Object.shader->E[lod_id]->passes.size(); ++iPass)
		{
			// Setup matrices + colors (and flush it as necessary)
			//RCache.set_Element				(Object.shader->E[lod_id]);
			RCache.set_Element				(Object.shader->E[lod_id], iPass);
			RImplementation.apply_lmaterial	();

			//	This could be cached in the corresponding consatant buffer
			//	as it is done for DX9
			RCache.set_c(strConsts, consts);

			RCache.set_c(strWave, wave);
			RCache.set_c(strDir2D, wind);

			RCache.set_c(strWaveOld, wave_old);
			RCache.set_c(strDir2DOld, wind_old);

			u32 dwBatch	= 0;
			for (auto& S : Object.m_items[var_id][render_key])
			{
				CDetail::SlotItem& Instance = *S.get();

				if (RImplementation.pOutdoorSector && PortalTraverser.i_marker != RImplementation.pOutdoorSector->r_marker)
					continue;

				if (RImplementation.phase == CRender::PHASE_SMAP && L)
				{
					if(L->position.distance_to_sqr(Instance.pos) >= _sqr(L->range))
						continue;
				}
				static InstanceData* c_storage = NULL;
				if (dwBatch == 0)
					RCache.get_ConstantDirect(strArray, hw_BatchSize*sizeof(InstanceData), (void**)&c_storage, 0, 0);


				if(!c_storage) continue;

				c_storage[dwBatch] = {Instance.hpb, Instance.scale_calculated, Instance.pos, Instance.c_hemi};
				dwBatch++;

				if (dwBatch >= hw_BatchSize)
				{
					// flush
					u32 dwCNT_verts			= dwBatch * Object.number_vertices;
					u32 dwCNT_prims			= (dwBatch * Object.number_indices)/3;
					RCache.Render			(D3DPT_TRIANGLELIST,vOffset, 0, dwCNT_verts,iOffset,dwCNT_prims);
					dwBatch = 0;
				}
			}
			// flush if nessecary
			if (dwBatch>0&&dwBatch<hw_BatchSize)
			{
				u32 dwCNT_verts			= dwBatch * Object.number_vertices;
				u32 dwCNT_prims			= (dwBatch * Object.number_indices)/3;
				RCache.Render				(D3DPT_TRIANGLELIST,vOffset,0,dwCNT_verts,iOffset,dwCNT_prims);
				dwBatch = 0;
			}
		}
		vOffset += hw_BatchSize * Object.number_vertices;
		iOffset += hw_BatchSize * Object.number_indices;
	}
}