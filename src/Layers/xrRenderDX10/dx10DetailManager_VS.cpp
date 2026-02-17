#include "stdafx.h"
#include "../xrRender/DetailManager.h"
#include "../../xrEngine/xr_ioc_cmd.h"
void CDetailManager::hw_Load()
{
	//Prepare descs
	RHIBufferDesc bufferDesc{};
	bufferDesc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	bufferDesc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	bufferDesc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
	bufferDesc.StructureByteStride = sizeof(CDetail::SlotItem);

	RHIShaderResourceViewDesc srvDesc{};
	srvDesc.Format = ERHI_FORMAT::UNKNOWN;

	const u32 bufferSizes[] = { 64, 128, 256, 512, 1024, 2048, 4096, 8192/*, 16384, 32768, 65536*/ };
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

void CDetailManager::hw_Unload()
{
	for (auto& [_, it] : DetailInstanceBuffers)
	{
		_RELEASE(it.first);
		_RELEASE(it.second);
	}
}

void CDetailManager::hw_Render(light* L)
{
	PROF_EVENT("CDetailManager::hw_Render");
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	RCache.set_xform_world(Fidentity);

	Fvector4 wave, wave_old;

	auto LodHQ = RImplementation.phase == RImplementation.PHASE_NORMAL ? SE_R2_NORMAL_HQ : SE_R2_DETAIL_SHADOW_HQ;
	auto LodLQ = RImplementation.phase == RImplementation.PHASE_NORMAL ? SE_R2_NORMAL_LQ : SE_R2_DETAIL_SHADOW_LQ;

	// Wave0
	{
		PROF_EVENT("Wave0")
		wave.set(1.f / 5.f, 1.f / 7.f, 1.f / 3.f, m_time_pos);
		wave_old.set(1.f / 5.f, 1.f / 7.f, 1.f / 3.f, m_time_pos_old);

		if (CPU::ID().hasFeature(CPUFeature::AVX))
			hw_Render_dump<__m256>(wave.div(PI_MUL_2), wave_dir1, wave_old.div(PI_MUL_2), wave_dir1_old, 1, LodHQ, L);
		else
			hw_Render_dump<CDetail::SlotItem>(wave.div(PI_MUL_2), wave_dir1, wave_old.div(PI_MUL_2), wave_dir1_old, 1, LodHQ, L);
	}

	// Wave1
	{
		PROF_EVENT("Wave1")
		wave.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos);
		wave_old.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos_old);
		if (CPU::ID().hasFeature(CPUFeature::AVX))
			hw_Render_dump<__m256>(wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 2, LodHQ, L);
		else
			hw_Render_dump<CDetail::SlotItem>(wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 2, LodHQ, L);
	}

	// Still
	if(RImplementation.phase != CRender::PHASE_SMAP)
	{
		PROF_EVENT("Still")
		if (CPU::ID().hasFeature(CPUFeature::AVX))
			hw_Render_dump<__m256>(wave, wave_dir2, wave_old, wave_dir2_old, 0, LodLQ, L);
		else
			hw_Render_dump<CDetail::SlotItem>(wave, wave_dir2, wave_old, wave_dir2_old, 0, LodLQ, L);
	}

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
}

template<typename T>
void CDetailManager::hw_Render_dump(const Fvector4& wave, const Fvector4& wind, const Fvector4& wave_old, const Fvector4& wind_old, u32 var_id, u32 lod_id, light* L)
{
#ifndef _EDITOR
	//Render state, shaders & so on [only 1st pass]
	RCache.set_Element(objects[0].shader->E[lod_id], 0);
#endif

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

#ifndef _EDITOR
	bool in_outdoor = RImplementation.SectorsCount() <= 1 || (RImplementation.pOutdoorSector && PortalTraverser.i_marker == RImplementation.pOutdoorSector->r_marker);
	if (!in_outdoor)
		return;
#endif

	if (phase_shmap && L)
	{
		Fvector l_spatial_pos = L->SpatialComponent->sphere.P;
		float l_range_sqr = _sqr(L->SpatialComponent->sphere.R);

#ifdef _EDITOR
		for (CDetail* ObjectPtr : objects)
		{
			CDetail& Object = *ObjectPtr;
			RCache.set_Element(Object.shader->E[lod_id], 0);
#else
		for (CDetail& Object : objects)
		{
#endif
			auto& items = Object.m_items[render_key][var_id];
			u32 totalInstances = items.size();
			if (totalInstances == 0) continue;

			auto it = DetailInstanceBuffers.lower_bound(totalInstances);

			//Use largest buffer possible [should keep HUGE buffer around in those cases]
			if (it == DetailInstanceBuffers.end())
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

			u32 instanceCount = 0;
			RHIMappedSubresource pSubRes;
			for (CDetail::SlotItem& Instance : items)
			{
				if (l_spatial_pos.distance_to_sqr(Instance.pos) >= l_range_sqr)
					continue;

				if (instanceCount == 0)
					R_ASSERT(currentBuffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &pSubRes));

				if constexpr (std::is_same_v<T, __m256>)
				{
#ifdef IXR_CLANG_BUILD
					float* dest = static_cast<float*>(pSubRes.pData) + instanceCount * 8;
					_mm256_stream_ps(dest, reinterpret_cast<const __m256&>(Instance));
					instanceCount++;
#else
					_mm256_stream_ps(&static_cast<T*>(pSubRes.pData)[instanceCount++].m256_f32[0], reinterpret_cast<T&>(Instance)); // experimental
#endif
				}
				else
				{
					static_cast<T*>(pSubRes.pData)[instanceCount++] = reinterpret_cast<T&>(Instance);
				}

				if (instanceCount == currentSize)
				{
					currentBuffer->Unmap();
					RCache.RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, Object.number_vertices, 0, Object.number_indices / 3, instanceCount, 0, false);
					instanceCount = 0; //Reset

				}
			}

			//Render remaining instances
			if (instanceCount > 0)
			{
				currentBuffer->Unmap();
				RCache.RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, Object.number_vertices, 0, Object.number_indices / 3, instanceCount, 0, false);
			}
		}
	}
	else
	{
		if (ps_r2_ls_flags.test(R2FLAG_FAST_DETAILS_UPDATE))//experimental
		{
#ifdef _EDITOR
			for (CDetail* DPtr : objects)
			{
				CDetail& D = *DPtr;
				RCache.set_Element(D.shader->E[lod_id], 0);
#else
			for (CDetail& D : objects)
			{
#endif
				u32 buff_size = D.m_items[render_key][var_id].size();
				if (buff_size)
				{
					GRHI->ShaderResourceCache->SetVSResource(0, D.DetailGPUBoundBuffers[render_key][var_id].second);
					RCache.set_Geometry(D.hw_Geom);
					RCache.RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, D.number_vertices, 0, D.number_indices / 3, buff_size, 0, false);
				}
			}
		}
		else
		{
#ifdef _EDITOR
			for (CDetail* ObjectPtr : objects)
			{
				CDetail& Object = *ObjectPtr;
				RCache.set_Element(Object.shader->E[lod_id], 0);
#else
			for (CDetail& Object : objects)
			{
#endif
				auto& items = Object.m_items[render_key][var_id];
				u32 totalInstances = items.size();
				if (u32(0) == totalInstances) continue;

				auto it = DetailInstanceBuffers.lower_bound(totalInstances);

				//Use largest buffer possible [should keep HUGE buffer around in those cases]
				if (it == DetailInstanceBuffers.end())
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
				u32 offset = 0u, chunkSize = 0u;
				RHIMappedSubresource pSubRes;
				CDetail::SlotItem* items_data = items.data();
				while (offset < totalInstances)
				{
					chunkSize = std::min(currentSize, totalInstances - offset);

					R_ASSERT(currentBuffer->Map(ERHI_BUFFER_MAP::WRITE_DISCARD, 0, &pSubRes));

					memcpy(pSubRes.pData, items_data + offset, chunkSize * sizeof(T));

					currentBuffer->Unmap();
					RCache.RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, Object.number_vertices, 0, Object.number_indices / 3, chunkSize, 0, false);

					offset += chunkSize;
				}
			}
		}
	}
}