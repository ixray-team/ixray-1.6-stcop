#include "stdafx.h"
#include "../xrRender/DetailManager.h"
#include "../xrRender/DetailsWind.h"
#include "../xrRender/SH_Atomic.h"
#include "../../xrEngine/xr_ioc_cmd.h"
#include "../../xrEngine/EngineAPI.h"

#ifndef _EDITOR
extern float ps_r__detail_rnd_scale_max;

extern float ps_trample_bend;
extern float ps_trample_squash;
extern float ps_trample_trail_min;
extern float ps_trample_trail_max;
extern float ps_trample_obj_radius_scale;
extern float ps_trample_actor_radius_scale;
extern int   ps_trample_enabled;
extern float ps_trample_cooltime;
extern float ps_trample_draw_radius;
extern float ps_trample_brush_fill;
extern float ps_trample_press_speed;

#endif // !_EDITOR

void CDetailManager::hw_Load()
{
	RHIBufferDesc bufferDesc{};
	bufferDesc.Usage = ERHI_USAGE::USAGE_DYNAMIC;
	bufferDesc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	bufferDesc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_WRITE;
	bufferDesc.StructureByteStride = sizeof(CDetail::SlotItem);

	RHIShaderResourceViewDesc srvDesc{};
	srvDesc.Format = ERHI_FORMAT::UNKNOWN;

	const u32 bufferSizes[] = { 64, 128, 256, 512, 1024, 2048, 4096, 8192 };
	for (int i = 0; i < std::size(bufferSizes); ++i)
	{
		u32 buff_size = bufferSizes[i];
		bufferDesc.Size = buff_size * sizeof(CDetail::SlotItem);
		IRHIBuffer* buffer = GRHI->CreateBuffer(bufferDesc, nullptr);

		srvDesc.ElementWidth = buff_size;
		IRHIShaderResourceView* srv = GRHI->CreateShaderResourceView(buffer, &srvDesc);
		DetailInstanceBuffers[buff_size] = std::make_pair(buffer, srv);
	}
}

#ifndef _EDITOR

void CDetailManager::TrampleMark(float x, float y, float z, float radius, float weight, bool isActor)
{
	if (!ps_trample_enabled) return;
	radius = std::clamp(radius, ps_trample_trail_min, ps_trample_trail_max);
	radius *= isActor ? ps_trample_actor_radius_scale : ps_trample_obj_radius_scale;
	STramplePendingMark m;
	m.x = x; m.y = y; m.z = z; m.radius = radius; m.weight = weight;
	std::lock_guard<std::mutex> lock(TramplePendingLock);
	if (TramplePendingMarks.empty())
		TramplePlayerRadius = radius;
	TramplePendingMarks.push_back(m);
}

void CDetailManager::TrampleProcessMarks()
{
	if (!ps_trample_enabled) return;

	xr_vector<STramplePendingMark> marks;
	{
		std::lock_guard<std::mutex> lock(TramplePendingLock);
		marks.swap(TramplePendingMarks);
	}
	if (marks.empty() || cache.empty()) return;

	const float texel = 0.5f;

	for (const auto& mk : marks)
	{
		const int sx = iFloor(mk.x / dm_slot_size);
		const int sz = iFloor(mk.z / dm_slot_size);
		const int range = std::max(1, (int)std::ceil(mk.radius / dm_slot_size));

		for (int dz = -range; dz <= range; ++dz)
		{
			for (int dx = -range; dx <= range; ++dx)
			{
				const int wsx = sx + dx;
				const int wsz = sz + dz;
				const int gx = w2cg_X(wsx);
				const int gz = w2cg_Z(wsz);
				if (gx < 0 || gz < 0 || gx >= (int)dm_cache_line || gz >= (int)dm_cache_line) continue;
				Slot* S = cache[gz][gx];
				if (!S || S->empty) continue;

				bool painted = false;
				for (u32 sp = 0; sp < dm_obj_in_slot; ++sp)
				{
					SlotPart& part = S->G[sp];
					for (u32 vi = 0; vi < 3; ++vi)
					{
						for (CDetail::SlotItem* item : part.items[vi])
						{
							const float dx2 = item->pos.x - mk.x;
							const float dz2 = item->pos.z - mk.z;
							const float d2d = std::sqrt(dx2 * dx2 + dz2 * dz2);
							if (d2d >= mk.radius) continue;

							const float dy = mk.y - item->pos.y;
							const float d3d = std::sqrt(d2d * d2d + dy * dy * 0.25f);
							if (d3d >= mk.radius) continue;

							float innerR = mk.radius * ps_trample_brush_fill;
							if (innerR > mk.radius - 2.0f * texel) innerR = std::max(0.0f, mk.radius - 2.0f * texel);
							float t;
							if (d3d <= innerR)
								t = 1.0f;
							else
							{
								const float s = (mk.radius - d3d) / (mk.radius - innerR);
								t = s * s * (3.0f - 2.0f * s);
							}

							const float add = t * mk.weight;
							if (item->trample_strength < 0.001f && item->trample_visual < 0.001f)
							{
								item->trample_visual = 0.0f;
								const float nl = d2d / texel;
								const float radialX = (nl > 0.001f) ? (item->pos.x - mk.x) / (nl * texel) : 0.0f;
								const float radialZ = (nl > 0.001f) ? (item->pos.z - mk.z) / (nl * texel) : 0.0f;
								const float edgeF = 1.0f - t;
								const float radialW = 0.6f + 0.4f * edgeF;
								float bx = radialX * radialW;
								float bz = radialZ * radialW;
								const float bl = std::sqrt(bx * bx + bz * bz);
								if (bl > 0.001f) { bx /= bl; bz /= bl; }
								item->trample_dirX = bx;
								item->trample_dirZ = bz;
							}
							item->trample_strength = std::min(item->trample_strength + add, 1.0f);
							painted = true;
						}
					}
				}
				if (painted)
				{
					S->has_trample = true;
					if (S->trample_tick == 0)
						S->trample_tick = RDEVICE.dwTimeGlobal;
					const u32 packed = ((u32)(u16)wsz << 16) | (u32)(u16)wsx;
					bool found = false;
					for (u32 k = 0; k < TrampleActiveSlots.size(); ++k)
						if (TrampleActiveSlots[k] == packed) { found = true; break; }
					if (!found) TrampleActiveSlots.push_back(packed);
				}
			}
		}
	}
}

void CDetailManager::TrampleAnimateItems()
{
	if (!ps_trample_enabled)
	{
		if (!TrampleActiveSlots.empty())
		{
			for (u32 packed : TrampleActiveSlots)
			{
				const int wsx = (int)(s16)(packed & 0xFFFF);
				const int wsz = (int)(s16)((packed >> 16) & 0xFFFF);
				const int gx = w2cg_X(wsx);
				const int gz = w2cg_Z(wsz);
				if (gx >= 0 && gz >= 0 && gx < (int)dm_cache_line && gz < (int)dm_cache_line)
				{
					Slot* S = cache[gz][gx];
					if (S)
					{
						S->has_trample = false;
						for (u32 sp = 0; sp < dm_obj_in_slot; ++sp)
						{
							SlotPart& part = S->G[sp];
							for (u32 vi = 0; vi < 3; ++vi)
							{
								for (CDetail::SlotItem* item : part.items[vi])
								{
									item->trample_strength = 0.0f;
									item->trample_visual = 0.0f;
									item->trample_dirX = 0.0f;
									item->trample_dirZ = 0.0f;
								}
							}
						}
					}
				}
			}
			TrampleActiveSlots.clear();
		}
		return;
	}

	if (TrampleActiveSlots.empty()) return;

	const u32 now_ms = RDEVICE.dwTimeGlobal;

	u32 write = 0;
	for (u32 read = 0; read < TrampleActiveSlots.size(); ++read)
	{
		const u32 packed = TrampleActiveSlots[read];
		const int wsx = (int)(s16)(packed & 0xFFFF);
		const int wsz = (int)(s16)((packed >> 16) & 0xFFFF);

		const int gx = w2cg_X(wsx);
		const int gz = w2cg_Z(wsz);
		Slot* S = (gx >= 0 && gz >= 0 && gx < (int)dm_cache_line && gz < (int)dm_cache_line) ? cache[gz][gx] : nullptr;
		if (!S || S->empty || !S->has_trample)
			continue;

		float dt;
		if (S->trample_tick == 0)
			dt = 1.0f / 60.0f;
		else
			dt = std::min((now_ms - S->trample_tick) / 1000.0f, 0.5f);
		S->trample_tick = now_ms;

		const float decay_rate = dt / std::max(ps_trample_cooltime, 0.5f);
		const float anim_rate = std::min(1.0f, ps_trample_press_speed * dt);

		bool anyActive = false;
		for (u32 sp = 0; sp < dm_obj_in_slot; ++sp)
		{
			SlotPart& part = S->G[sp];
			for (u32 vi = 0; vi < 3; ++vi)
			{
				for (CDetail::SlotItem* item : part.items[vi])
				{
					if (item->trample_visual < 0.001f && item->trample_strength < 0.001f)
						continue;

					item->trample_strength = std::max(0.0f, item->trample_strength - decay_rate);
					item->trample_visual += (item->trample_strength - item->trample_visual) * anim_rate;

					if (item->trample_strength < 0.001f && item->trample_visual < 0.001f)
					{
						item->trample_visual = 0.0f;
						item->trample_strength = 0.0f;
					}
					else
					{
						anyActive = true;
					}
				}
			}
		}

		if (anyActive)
			TrampleActiveSlots[write++] = packed;
		else
			S->has_trample = false;
	}
	TrampleActiveSlots.resize(write);
}

#endif // !_EDITOR

void CDetailManager::hw_Unload()
{
	for (auto& [_, it] : DetailInstanceBuffers)
	{
		_RELEASE(it.first);
		_RELEASE(it.second);
	}

#ifndef _EDITOR
	{
		std::lock_guard<std::mutex> lock(TramplePendingLock);
		TramplePendingMarks.clear();
	}
	TrampleActiveSlots.clear();
#endif
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

		hw_Render_dump<CDetail::SlotItem>(wave.div(PI_MUL_2), wave_dir1, wave_old.div(PI_MUL_2), wave_dir1_old, 1, LodHQ, L);
	}

	// Wave1
	{
		PROF_EVENT("Wave1")
		wave.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos);
		wave_old.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos_old);
		hw_Render_dump<CDetail::SlotItem>(wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 2, LodHQ, L);
	}

	// Still
	if(RImplementation.phase != CRender::PHASE_SMAP)
	{
		PROF_EVENT("Still")
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

	if (LightingModeIsStatic())
		RCache.set_c("consts", 1.f, 1.f, ps_r__Detail_l_aniso, ps_r__Detail_l_ambient);

	if(var_id != 0)
	{
		RCache.set_c("wave", wave);
		RCache.set_c("dir2D", wind);

		RCache.set_c("wave_old", wave_old);
		RCache.set_c("dir2D_old", wind_old);
	}

#ifndef _EDITOR
	{
		CDetailWind::Constants wc;
		CDetailWind::ComputeConstants(wc);
		RCache.set_c("wind_global", wc.global);
		RCache.set_c("wind_xz1", wc.xz1);
		RCache.set_c("wind_xz1_dir", wc.xz1_dir);
		RCache.set_c("wind_xz2", wc.xz2);
		RCache.set_c("wind_xz2_dir", wc.xz2_dir);
		RCache.set_c("wind_xz3", wc.xz3);
		RCache.set_c("wind_xz3_dir", wc.xz3_dir);
		RCache.set_c("wind_swirl", wc.swirl);
		RCache.set_c("wind_swirl_dir", wc.swirl_dir);
	}
#endif

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
			RCache.set_Element(Object.shader->E[lod_id], 0);
#endif
#ifndef _EDITOR
			if (ps_trample_enabled)
			{
				Fvector4 tp_val; tp_val.set(ps_trample_bend, ps_trample_squash, 0.0f, 0.0f);
				RCache.set_c("trample_params", tp_val);
				RCache.FlushConstants();
			}
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

				static_cast<T*>(pSubRes.pData)[instanceCount++] = Instance;

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
			RCache.set_Element(D.shader->E[lod_id], 0);
#endif
#ifndef _EDITOR
				if (ps_trample_enabled)
				{
					Fvector4 tp_val; tp_val.set(ps_trample_bend, ps_trample_squash, 0.0f, 0.0f);
					RCache.set_c("trample_params", tp_val);
					RCache.FlushConstants();
				}
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
				RCache.set_Element(Object.shader->E[lod_id], 0);
#endif
#ifndef _EDITOR
				if (ps_trample_enabled)
				{
					Fvector4 tp_val; tp_val.set(ps_trample_bend, ps_trample_squash, 0.0f, 0.0f);
					RCache.set_c("trample_params", tp_val);
					RCache.FlushConstants();
				}
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
