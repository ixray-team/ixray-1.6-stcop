#include "stdafx.h"
#include "../xrRender/DetailManager.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

#include "dx10BufferUtils.h"

const int quant	= 16384;

struct InstanceData
{
	Fvector hpb;
	float scale;
	Fvector pos;
	float hemi;
};

const u32 bufferSizes[8] = {64, 128, 256, 512, 1024, 2048, 4096, 8192};

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

	//Prepare descs
	D3D11_BUFFER_DESC bufferDesc = {};
	bufferDesc.Usage = D3D11_USAGE_DYNAMIC;
	bufferDesc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
	bufferDesc.CPUAccessFlags = D3D11_CPU_ACCESS_WRITE;
	bufferDesc.MiscFlags = D3D11_RESOURCE_MISC_BUFFER_STRUCTURED;
	bufferDesc.StructureByteStride = sizeof(InstanceData);

	D3D11_SHADER_RESOURCE_VIEW_DESC srvDesc = {};
	srvDesc.Format = DXGI_FORMAT_UNKNOWN;
	srvDesc.ViewDimension = D3D11_SRV_DIMENSION_BUFFER;

	//Create the buffers & SRV
    for (int i = 0; i < 8; ++i)
    {
		//Buffer
		bufferDesc.ByteWidth = bufferSizes[i] * sizeof(InstanceData);

		ID3D11Buffer* buffer = NULL;
        R_CHK(RDevice->CreateBuffer(&bufferDesc, NULL, &buffer));

		if(buffer)
			detailBuffer_map.insert({bufferSizes[i], buffer});

		//SRV
        srvDesc.Buffer.ElementWidth = bufferSizes[i];

		ID3D11ShaderResourceView* srv = NULL;
        R_CHK(RDevice->CreateShaderResourceView(buffer, &srvDesc, &srv));

		if(srv)
			detailSRV_map.insert({bufferSizes[i], srv});
	}
}

void CDetailManager::hw_Render(light*L)
{
	PROF_EVENT("CDetailManager::hw_Render")

	RCache.set_CullMode		(CULL_NONE);
	RCache.set_xform_world	(Fidentity);

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

void CDetailManager::hw_Render_dump(const Fvector4& consts, const Fvector4& wave, const Fvector4& wind, const Fvector4& wave_old, const Fvector4& wind_old, u32 var_id, u32 lod_id, light* L)
{
    if (RImplementation.phase == CRender::PHASE_SMAP && var_id == 0)
        return;

	//Render state, shaders & so on [only 1st pass]
	RCache.set_Element(objects[0].shader->E[lod_id], 0);

	//Bind CBuffers
	RImplementation.apply_lmaterial(); //Material ID
	RCache.set_c("consts", consts);

	RCache.set_c("wave", wave);
	RCache.set_c("dir2D", wind);

	RCache.set_c("wave_old", wave_old);
	RCache.set_c("dir2D_old", wind_old);

	for (CDetail& Object : objects)
	{
		auto it = detailBuffer_map.lower_bound(Object.m_items[var_id][render_key].size());

		//Use largest buffer possible [should keep HUGE buffer around in those cases]
		if(it == detailBuffer_map.end())
			it = detailBuffer_map.find(8192);

		//Current buffer size and resources
		u32 currentSize = it->first;
		ID3D11Buffer* currentBuffer = it->second;
		ID3D11ShaderResourceView* currentSRV = detailSRV_map.find(currentSize)->second;

		//Bind (current) buffer SRV
		SRVSManager.SetVSResource(0, currentSRV);

		//Set IB, VB and decls
		RCache.set_Geometry(Object.hw_Geom);

		u32 instanceCount = 0;
		static InstanceData* c_storage = NULL;

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

			//LVutner: Update the instance buffer
			if(instanceCount == 0)
			{
				D3D11_MAPPED_SUBRESOURCE pSubRes;
				RContext->Map(currentBuffer, 0, D3D_MAP_WRITE_DISCARD, 0, &pSubRes);
				c_storage = reinterpret_cast<InstanceData*>(pSubRes.pData);
			}
			c_storage[instanceCount] = {Instance.hpb, Instance.scale_calculated, Instance.pos, Instance.c_hemi};

			//Increment
			instanceCount++;

			if (instanceCount >= currentSize)
			{ 
				RContext->Unmap(currentBuffer, 0);
				RCache.RenderInstancedIndexed(D3DPT_TRIANGLELIST, 0, 0, Object.number_vertices, 0, Object.number_indices / 3, instanceCount, 0);
				instanceCount = 0; //Reset
			}
		}

		//Render remaining instances
		if (instanceCount > 0 && instanceCount < currentSize)
		{
			RContext->Unmap(currentBuffer, 0);
			RCache.RenderInstancedIndexed(D3DPT_TRIANGLELIST, 0, 0, Object.number_vertices, 0, Object.number_indices / 3, instanceCount, 0);
		}
	}
}