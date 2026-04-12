#include "stdafx.h"
#include "../xrRender/DetailManager.h"

#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"

#include "dx10BufferUtils.h"

const int quant	= 16384;
const u32 bufferSizes[] = {64, 128, 256, 512, 1024, 2048, 4096, 8192/*, 16384, 32768, 65536*/ };

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
	RHIBufferDesc bufferDesc = {};
	bufferDesc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	bufferDesc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	bufferDesc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
	bufferDesc.StructureByteStride = sizeof(CDetail::SlotItem);

	RHIShaderResourceViewDesc srvDesc = {};
	srvDesc.Format = ERHI_FORMAT::UNKNOWN;

	//Create the buffers & SRV
	for (int i = 0; i < std::size(bufferSizes); ++i)
	{
		u32 buff_size = bufferSizes[i];
		//Buffer
		bufferDesc.Size = buff_size * sizeof(CDetail::SlotItem);
		IRHIBuffer* buffer = GRHI->CreateBuffer(bufferDesc, nullptr);

		//SRV
		srvDesc.ElementWidth = buff_size;
		IRHIShaderResourceView* srv = GRHI->CreateShaderResourceView(buffer, &srvDesc);
		DetailInstanceBuffers[buff_size] = std::make_pair(buffer, srv);
	}
}

void CDetailManager::hw_Render(light*L)
{
	PROF_EVENT("CDetailManager::hw_Render");
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
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
		if (CPU::ID().hasFeature(CPUFeature::AVX))
			hw_Render_dump<__m256>(consts, wave.div(PI_MUL_2), wave_dir1, wave_old.div(PI_MUL_2), wave_dir1_old, 1, LodHQ, L);
		else
			hw_Render_dump<CDetail::SlotItem>(consts, wave.div(PI_MUL_2), wave_dir1, wave_old.div(PI_MUL_2), wave_dir1_old, 1, LodHQ, L);
	}

	// Wave1
	{
		PROF_EVENT("Wave1")
		wave.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos);
		wave_old.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos_old);
		if (CPU::ID().hasFeature(CPUFeature::AVX))
			hw_Render_dump<__m256>(consts, wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 2, LodHQ, L);
		else
			hw_Render_dump<CDetail::SlotItem>(consts, wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 2, LodHQ, L);
	}

	// Still
	if(RImplementation.phase != CRender::PHASE_SMAP)
	{
		PROF_EVENT("Still")
		consts.set(scale, scale, scale, 1.f);
		if (CPU::ID().hasFeature(CPUFeature::AVX))
			hw_Render_dump<__m256>(consts, wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 0, LodLQ, L);
		else
			hw_Render_dump<CDetail::SlotItem>(consts, wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 0, LodLQ, L);
	}

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
}

template<typename T>
void CDetailManager::hw_Render_dump(const Fvector4& consts, const Fvector4& wave, const Fvector4& wind, const Fvector4& wave_old, const Fvector4& wind_old, u32 var_id, u32 lod_id, light* L)
{
	//Render state, shaders & so on [only 1st pass]
	RCache.set_Element(objects[0].shader->E[lod_id], 0);

	bool phase_shmap = RImplementation.phase == CRender::PHASE_SMAP;
	if(!phase_shmap)
		RImplementation.apply_lmaterial(); //Material ID

	if(var_id != 0)
	{
		RCache.set_c("wave", wave);
		RCache.set_c("dir2D", wind);

		RCache.set_c("wave_old", wave_old);
		RCache.set_c("dir2D_old", wind_old);
	}
	RCache.FlushConstants();

	bool in_outdoor = RImplementation.SectorsCount() <= 1 || (RImplementation.pOutdoorSector && PortalTraverser.i_marker == RImplementation.pOutdoorSector->r_marker);
	if (!in_outdoor)
		return;

	for (CDetail& Object : objects)
	{
		auto& items = Object.m_items[var_id][render_key];
		u32 totalInstances = items.size();
		if (u32(0)==totalInstances) continue;

		auto it = DetailInstanceBuffers.lower_bound(totalInstances);

		//Use largest buffer possible [should keep HUGE buffer around in those cases]
		if(it == DetailInstanceBuffers.end())
		{
			it = std::prev(DetailInstanceBuffers.end());
		}

		//Current buffer size and resources
		u32 currentSize = it->first;
		IRHIBuffer* currentBuffer = it->second.first;
		IRHIShaderResourceView* currentSRV = it->second.second;

		//Bind (current) buffer SRV
		GRHI->ShaderResourceCache->SetVSResource(0, currentSRV);

		//Set IB, VB and decls
		RCache.set_Geometry(Object.hw_Geom);
		if(phase_shmap && L)
		{
			if (!items.empty())
			{
				u32 instanceCount = 0;
				RHIMappedSubresource pSubRes;
				for (CDetail::SlotItem& Instance : items)
				{
					if (L->position.distance_to_sqr(Instance.pos) >= _sqr(L->range))
						continue;

					if(instanceCount==0)
						R_ASSERT(currentBuffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &pSubRes));

					static_cast<T*>(pSubRes.pData)[instanceCount] = reinterpret_cast<T&>(Instance);
		
					instanceCount++;
		
					if (instanceCount >= currentSize)
					{
						currentBuffer->Unmap();
						RCache.RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, Object.number_vertices, 0, Object.number_indices / 3, instanceCount, 0, false);
						instanceCount = 0; //Reset
						
					}
				}
		
				//Render remaining instances
				if (instanceCount > 0 && instanceCount < currentSize)
				{
					currentBuffer->Unmap();
					RCache.RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, Object.number_vertices, 0, Object.number_indices / 3, instanceCount, 0, false);
				}
			}
		}
		else
		{
			u32 offset = 0u, chunkSize = 0u;
			RHIMappedSubresource pSubRes;
			while (offset < totalInstances)
			{
				chunkSize = std::min(currentSize, totalInstances - offset);

				R_ASSERT(currentBuffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &pSubRes));

				// memcpy целого блока!
				memcpy(pSubRes.pData, items.data() + offset, chunkSize * sizeof(T));

				currentBuffer->Unmap();
				RCache.RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, Object.number_vertices, 0, Object.number_indices / 3, chunkSize, 0, false);

				offset += chunkSize;
			}
		}
	}
}