#include "stdafx.h"
#include "DetailManager.h"
#include "DetailsWind.h"
#include "SH_Atomic.h"
#include "../../xrEngine/xr_ioc_cmd.h"
#include "../../xrEngine/EngineAPI.h"

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

namespace
{
// Device resource creation is thread-safe; the producer never touches the context.
bool CreateDetailSnapshot(std::pair<IRHIBuffer*, IRHIShaderResourceView*>& Buffer, const void* Data, u32 Count, u32 Stride)
{
	_RELEASE(Buffer.second);
	_RELEASE(Buffer.first);
	if (!Count)
	{
		return true;
	}
	RHIBufferDesc Description{};
	Description.Usage = ERHI_USAGE::USAGE_IMMUTABLE;
	Description.Type = ERHI_BUFFER_TYPE::STRUCTURED;
	Description.StructureByteStride = Stride;
	Description.Size = Count * Stride;
	RHIBufferSubresource Initial{};
	Initial.pSysMem = Data;
	Initial.SysMemSize = Description.Size;
	Buffer.first = GRHI->CreateBuffer(Description, &Initial);
	if (!Buffer.first)
	{
		return false;
	}
	RHIShaderResourceViewDesc View{};
	View.Format = ERHI_FORMAT::UNKNOWN;
	View.ElementWidth = Count;
	Buffer.second = GRHI->CreateShaderResourceView(Buffer.first, &View);
	return Buffer.second != nullptr;
}
} // namespace

void CDetailManager::hw_Load()
{
	// Instance snapshots are created by the detail producer, not mapped by Render.
}
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

bool CDetailManager::PrepareDetailBuffers(bool ReuseStatic)
{
	PROF_EVENT("Create detail packet buffers");
	bool Ready = true;
	for (CDetail* Model : objects)
	{
		for (u32 Group = 0; Group < 3; ++Group)
		{
			const auto& Items = Model->ProducerItems[Group];
			Model->PreparedInstances.Counts[Group] = u32(Items.size());
			Ready = CreateDetailSnapshot(Model->PreparedInstances.Buffers[Group], Items.data(), u32(Items.size()), sizeof(CDetail::SlotItem)) && Ready;
		}
		auto& Destination = Model->PreparedInstances.Buffers[3];
		if (ReuseStatic)
		{
			_RELEASE(Destination.second);
			_RELEASE(Destination.first);
			Destination = Model->RenderInstances.Buffers[3];
			if (Destination.first)
			{
				Destination.first->AddRef();
			}
			if (Destination.second)
			{
				Destination.second->AddRef();
			}
			Model->PreparedInstances.Counts[3] = Model->RenderInstances.Counts[3];
			Model->PreparedInstances.FarHasTrails = Model->RenderInstances.FarHasTrails;
		}
		else if (!Model->PreparedInstances.FarHasTrails)
		{
			const auto& Items = Model->ProducerDistantItems;
			Model->PreparedInstances.Counts[3] = u32(Items.size());
			Ready = CreateDetailSnapshot(Destination, Items.data(), u32(Items.size()), sizeof(CDetail::InstanceTransform)) && Ready;
		}
		else
		{
			const auto& Items = Model->ProducerStaticItems;
			Model->PreparedInstances.Counts[3] = u32(Items.size());
			Ready = CreateDetailSnapshot(Destination, Items.data(), u32(Items.size()), sizeof(CDetail::StaticItem)) && Ready;
		}
	}
	return Ready;
}
void CDetailManager::ResetTramplePresentation()
{
	TramplePresentationStates.clear();
	TramplePresentationSources.clear();
	TramplePresentationTable.clear();
	TramplePresentationFrame = u32(-1);
	TramplePresentationCapacity = 0;
	_RELEASE(TramplePresentationView);
	_RELEASE(TramplePresentationBuffer);
	_RELEASE(PreparedTrampleView);
	_RELEASE(PreparedTrampleBuffer);
	PreparedTrampleCapacity = 0;
}

void CDetailManager::UpdateTramplePresentation()
{
	// Only the producer owns presentation state. The shader interpolates the
	// previous/current samples between publications, including the entry blend.
	const u32 Frame = ++TramplePresentationFrame;
	PreparedTrampleCapacity = 0;
	_RELEASE(PreparedTrampleView);
	_RELEASE(PreparedTrampleBuffer);
	if (!ps_trample_enabled || TramplePresentationSources.empty())
	{
		TramplePresentationStates.clear();
		return;
	}
	u32 Capacity = 64;
	while (Capacity < TramplePresentationSources.size() * 4)
	{
		Capacity *= 2;
	}
	TramplePresentationTable.assign(Capacity, TramplePresentationEntry{});
	for (const auto* Source : TramplePresentationSources)
	{
		const auto Identity = DetailTramplePresentation::MakeKey(Source->pos.x, Source->pos.y, Source->pos.z, Source->scale);
		auto [Iterator, Inserted] = TramplePresentationStates.try_emplace(Identity);
		auto& State = Iterator->second;
		if (Inserted || Frame - State.Frame > 2)
		{
			State.Visual = Source->trample_visual;
			State.Progress = std::clamp(State.Visual / std::max(Source->trample_strength, .001f), 0.f, 1.f);
		}
		const float PreviousVisual = State.Visual, PreviousProgress = State.Progress;
		if (State.Frame != Frame)
		{
			// Native trail animation has already advanced the plant. Do not smooth it twice.
			State.Visual = Source->trample_visual;
			State.Frame = Frame;
		}
		State.Progress = std::max(State.Progress, std::clamp(State.Visual / std::max(Source->trample_strength, .001f), 0.0f, 1.0f));
		u32 Index = DetailTramplePresentation::Hash(Identity) & (Capacity - 1);
		while (TramplePresentationTable[Index].State.w > 0)
		{
			const auto& Existing = TramplePresentationTable[Index].Identity;
			if (DetailTramplePresentation::MakeKey(Existing.x, Existing.y, Existing.z, Existing.w) == Identity)
			{
				break;
			}
			Index = (Index + 1) & (Capacity - 1);
		}
		auto& Entry = TramplePresentationTable[Index];
		Entry.Identity.set(Source->pos.x, Source->pos.y, Source->pos.z, Source->scale);
		Entry.State.set(State.Visual, State.Progress, PreviousVisual, 1 + PreviousProgress);
	}
	if ((Frame % 120) == 0)
	{
		for (auto Iterator = TramplePresentationStates.begin(); Iterator != TramplePresentationStates.end();)
		{
			if (Frame - Iterator->second.Frame > 120)
			{
				Iterator = TramplePresentationStates.erase(Iterator);
			}
			else
			{
				++Iterator;
			}
		}
	}
	std::pair<IRHIBuffer*, IRHIShaderResourceView*> Buffer{};
	if (CreateDetailSnapshot(Buffer, TramplePresentationTable.data(), Capacity, sizeof(TramplePresentationEntry)))
	{
		PreparedTrampleBuffer = Buffer.first;
		PreparedTrampleView = Buffer.second;
		PreparedTrampleCapacity = Capacity;
	}
	else
	{
		_RELEASE(Buffer.second);
		_RELEASE(Buffer.first);
	}
}
void CDetailManager::hw_Unload()
{
	ResetTramplePresentation();

	{
		std::lock_guard<std::mutex> lock(TramplePendingLock);
		TramplePendingMarks.clear();
	}
	TrampleActiveSlots.clear();
}

void CDetailManager::hw_Render(light* L)
{
	if (!psDeviceFlags.is(rsDetails))
	{
		return;
	}
	PROF_EVENT("CDetailManager::hw_Render");
	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
	if (!L)
	{
		RCache.set_xform_world(Fidentity);
	}

	Fvector4 wave, wave_old;

	auto LodHQ = RImplementation.phase == RImplementation.PHASE_NORMAL ? SE_R2_NORMAL_HQ : SE_R2_DETAIL_SHADOW_HQ;
	auto LodLQ = RImplementation.phase == RImplementation.PHASE_NORMAL ? SE_R2_NORMAL_LQ : SE_R2_DETAIL_SHADOW_LQ;

	// Wave0
	{
		PROF_EVENT("Wave0")
		wave.set(1.f / 5.f, 1.f / 7.f, 1.f / 3.f, m_time_pos);
		wave_old.set(1.f / 5.f, 1.f / 7.f, 1.f / 3.f, m_time_pos_old);

		hw_Render_dump(wave.div(PI_MUL_2), wave_dir1, wave_old.div(PI_MUL_2), wave_dir1_old, 1, LodHQ, L);
	}

	// Wave1
	{
		PROF_EVENT("Wave1")
		wave.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos);
		wave_old.set(1.f / 3.f, 1.f / 7.f, 1.f / 5.f, m_time_pos_old);
		hw_Render_dump(wave.div(PI_MUL_2), wave_dir2, wave_old.div(PI_MUL_2), wave_dir2_old, 2, LodHQ, L);
	}

	// Still
	if (RImplementation.phase != CRender::PHASE_SMAP)
	{
		PROF_EVENT("Still")
		hw_Render_dump(wave, wave_dir2, wave_old, wave_dir2_old, 0, LodLQ, L);
		hw_Render_dump(wave, wave_dir2, wave_old, wave_dir2_old, 3, 4, L);
	}
	else
	{
		hw_Render_dump(wave, wave_dir2, wave_old, wave_dir2_old, 0, LodLQ, L);
	}

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
}

void CDetailManager::hw_Render_dump(const Fvector4& wave, const Fvector4& wind, const Fvector4& wave_old, const Fvector4& wind_old, u32 var_id, u32 lod_id, light* L)
{
	const bool EditorMode = Device.IsEditorMode();
	if (!EditorMode)
	{
		GRHI->ShaderResourceCache->SetVSResource(1, TramplePresentationCapacity ? TramplePresentationView : nullptr);
		Fvector4 TrampleParameters;
		TrampleParameters.set(ps_trample_bend, ps_trample_squash, float(TramplePresentationCapacity), 0);
		RCache.set_c("trample_params", TrampleParameters);
	}
	if (!EditorMode && !objects.empty())
	{
		// Render state, shaders & so on [only 1st pass]
		RCache.set_Element(objects[0]->shader->E[lod_id], 0);
	}

	bool phase_shmap = RImplementation.phase == CRender::PHASE_SMAP;
	if (!phase_shmap)
	{
		RImplementation.apply_lmaterial(); // Material ID
	}

	if (LightingModeIsStaticRender())
	{
		RCache.set_c("consts", 1.f, 1.f, ps_r__Detail_l_aniso, ps_r__Detail_l_ambient);
	}

	if (var_id != 0)
	{
		RCache.set_c("wave", wave);
		RCache.set_c("dir2D", wind);

		RCache.set_c("wave_old", wave_old);
		RCache.set_c("dir2D_old", wind_old);
	}

	if (!EditorMode)
	{
		CDetailWind::Constants wc;
		wc.global = PublishedWindConstants[0];
		wc.xz1 = PublishedWindConstants[1];
		wc.xz2 = PublishedWindConstants[2];
		wc.xz3 = PublishedWindConstants[3];
		// Interpolate completed worker packets on every rendered frame, including shadows.
		const float SincePublish = float(RDEVICE.dwTimeGlobal - WindPublishTime);
		const float Duration = std::max(1.0f, WindInterpolationMs);
		wc.xz2.set(std::clamp(SincePublish / Duration, 0.0f, 1.0f), std::clamp((SincePublish - Device.fTimeDelta * 1000) / Duration, 0.0f, 1.0f), wc.xz2.z, wc.xz2.w);
		wc.xz3.set(Duration * .001f, wc.xz3.y, wc.xz3.z, 0);
		RCache.set_c("wind_global", wc.global);
		RCache.set_c("wind_xz1", wc.xz1);
		RCache.set_c("wind_xz2", wc.xz2);
		RCache.set_c("wind_xz3", wc.xz3);
	}

	RCache.FlushConstants();

	if (!EditorMode)
	{
		bool in_outdoor = RImplementation.SectorsCount() <= 1 || (RImplementation.pOutdoorSector && PortalTraverser.i_marker == RImplementation.pOutdoorSector->r_marker);
		if (!in_outdoor)
		{
			return;
		}
	}

	const Fvector4 LightParameters = L ? Fvector4{L->SpatialComponent->sphere.P.x, L->SpatialComponent->sphere.P.y, L->SpatialComponent->sphere.P.z, _sqr(L->SpatialComponent->sphere.R)} : Fvector4{0, 0, 0, -1};
	for (CDetail* Model : objects)
	{
		const u32 Count = Model->RenderInstances.Counts[var_id];
		if (!Count || (phase_shmap && var_id == 0 && !Model->WindLowVertexBuffer))
		{
			continue;
		}
		const bool OriginalMesh = (var_id == 0 || var_id == 3 || PublishedWindConstants[0].x < .5f) && Model->WindLowVertexBuffer;
		auto& Geometry = OriginalMesh ? Model->WindLowGeometry : Model->hw_Geom;
		const u32 Vertices = OriginalMesh ? Model->WindLowVertexCount : Model->number_vertices;
		const u32 Indices = OriginalMesh ? Model->WindLowIndexCount : Model->number_indices;
		RCache.set_Element(Model->shader->E[var_id == 3 && !Model->RenderInstances.FarHasTrails ? 5 : lod_id], 0);
		Fvector4 Parameters;
		Parameters.set(ps_trample_bend, ps_trample_squash, float(TramplePresentationCapacity), 0);
		RCache.set_c("trample_params", Parameters);
		RCache.set_c("detail_light", LightParameters);
		RCache.FlushConstants();
		RCache.set_Geometry(Geometry);
		GRHI->ShaderResourceCache->SetVSResource(0, Model->RenderInstances.Buffers[var_id].second);
		RCache.RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, Vertices, 0, Indices / 3, Count, 0, false);
	}
}
