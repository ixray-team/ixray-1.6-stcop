#include "stdafx.h"
#include "DetailManager.h"
#include "DetailsWind.h"
#include "xrRender_console.h"
#include <bit>

extern int ps_trample_enabled;
extern bool ps_wind_enabled;
extern float ps_wind_blend;
extern float ps_wind_blend_current;
extern float ps_wind_direction;

float ps_r__detail_geometry_distance = 50.f;
float ps_r__detail_wind_distance = 200.f;

u32 dm_size = 24;
u32 dm_slide_window_line = 12;
u32 dm_cache_line = 49;
u32 dm_cache_size = 2401;
float dm_fade = 47.5;
u32 dm_current_size = 24;
u32 dm_current_slide_window_line = 12;
u32 dm_current_cache_line = 49;
u32 dm_current_cache_size = 2401;
float dm_current_fade = 47.5;
float ps_current_detail_density = 0.6;
float ps_current_detail_scale = 1.f;

// Set by r__detail_* console changes while the level is loaded.
// Deferred so that a whole cfg_load (many commands in one frame) triggers a
// single cache_ReInitialize instead of one full slot rebuild per command.
// Set from the console thread, consumed on the render thread.
std::atomic<bool> g_details_need_rebuild = false;

void CDetailManager::RequestCacheRebuild()
{
	if (RImplementation.b_loaded)
		g_details_need_rebuild.store(true);
}

bool CDetailManager::ConsumeCacheRebuildRequest()
{
	if (!g_details_need_rebuild.load())
		return false;
	g_details_need_rebuild.store(false);
	return true;
}

void CDetailManager::cache_Alloc(u32 Size, float Fade, float Density)
{
	dm_size = Size;
	// Four extra 2 m rows are RAM-only prefetch; visible distance stays unchanged.
	CacheSlotRadius = int(dm_size) + (Device.IsEditorMode() ? 0 : 4);
	dm_cache_line = 2 * CacheSlotRadius + 1;
	dm_slide_window_line = (dm_cache_line - 1) / dm_slide_window_count;
	dm_cache_size = dm_cache_line * dm_cache_line;
	StreamingInitialized = false;
	dm_fade = Fade;
	ps_r__Detail_density = Density;

	slide_window.resize(dm_slide_window_line, xr_vector<SlideSlot>(dm_slide_window_line));
	cache.resize(dm_cache_line, xr_vector<Slot*>(dm_cache_line));
	cache_pool.resize(dm_cache_size);
	unpacked_slots.reserve(1024);
}

void CDetailManager::cache_Free()
{
	PreparedPacketValid = false;
	// Only the producer owns the slot cache. Keep the published GPU packet alive
	// while a new radius, density or brush rebuild is prepared in the background.
	cache_pool.clear();
	cache_pool.shrink_to_fit();
	cache.clear();
	cache.shrink_to_fit();
	slide_window.clear();
	slide_window.shrink_to_fit();
	unpacked_slots.clear();
	unpacked_slots.shrink_to_fit();
	items_pool.clear();
	WindUpdateSlots.clear();
	PreviousPreparedSlots.clear();
	PreviousFarHadTrails = false;
	WindSamplingBounds.clear();
	TramplePresentationSources.clear();
	TramplePresentationStates.clear();
	TramplePresentationTable.clear();
}
void CDetailManager::Load()
{
	CDetailWindPresets::Get().Reload();
	ClusterFieldValid = false;
	FMBFieldValid = false;
	// Open file stream
	if (!FS.exist("$level$","level.details"))
	{
		dtFS = nullptr;
		return;
	}

	string_path fn;
	FS.update_path(fn,"$level$","level.details");
	dtFS = FS.r_open(fn);

	// Header
	dtFS->r_chunk_safe(0,&dtH,sizeof(dtH));
	R_ASSERT(dtH.version == DETAIL_VERSION);
	u32 m_count = dtH.object_count;

	// Models
	IReader* m_fs = dtFS->open_chunk(1);
	for (u32 m_id = 0; m_id < m_count; m_id++)
	{
		IReader* S = m_fs->open_chunk(m_id);
		CDetail& dt	= *objects.emplace_back(new CDetail());
		dt.Load(S);
		S->close();
	}
	m_fs->close();

	vanilla_grass_count = 0;
	for (u32 m_id = 0; m_id < m_count; m_id++)
	{
		if (vanilla_grass_count >= 64)
			break;
		if (objects[m_id]->number_vertices >= 8 && !objects[m_id]->m_Flags.is(DO_NO_WAVING))
		{
			vanilla_grass_indices[vanilla_grass_count++] = m_id;
		}
	}

	alt_models_start = (u32)objects.size();
	alt_models_count = 0;
	AlternativeModels.clear();
	AlternativeSpeciesGroups.clear();
	AlternativeCatalogCRC = 0;
	if (ps_r__detail_use_alternative_tree_assets)
	{
		FS_FileSet dm_files;
		string_path Directory;
		FS.update_path(Directory, "$level$", "alternative_tree_dm\\");
		FS.file_list(dm_files, Directory, FS_ListFiles, "*.dm");
		xr_string Catalog;
		constexpr u32 kMaxObjects = u32(u8(-1));
		u32 obj_count = (u32)objects.size();
		for (const auto& dmf : dm_files)
		{
			xr_string Relative = dmf.name;
			std::replace(Relative.begin(), Relative.end(), '/', '\\');
			const size_t Separator = Relative.find('\\');
			if (Separator != xr_string::npos && Relative.find('\\', Separator + 1) != xr_string::npos)
			{
				continue;
			}
			if (obj_count >= kMaxObjects)
				break;
			string_path full_path;
			const xr_string Path = "alternative_tree_dm\\" + Relative;
			FS.update_path(full_path, "$level$", Path.c_str());
			CDetail& dt = *objects.emplace_back(new CDetail());
			obj_count++;
			if (!dt.LoadFromDM(full_path))
			{
				xr_delete(objects.back());
				objects.pop_back();
				obj_count--;
				Msg("! CDetailManager::Load: alt model failed to load '%s'", full_path);
			}
			else
			{
				const xr_string Name = Separator == xr_string::npos ? Relative.substr(0, Relative.rfind('.')) : Relative.substr(0, Separator);
				u32 Species = u32(AlternativeSpeciesGroups.size());
				// Root files are independent species, even if a folder has the same name.
				if (Separator != xr_string::npos)
				{
					for (u32 I = 0; I < AlternativeSpeciesGroups.size(); ++I)
					{
						const auto& Group = AlternativeSpeciesGroups[I];
						if (Group.Name == Name && AlternativeModels[Group.Models.front()].Path.find('\\', sizeof("alternative_tree_dm\\") - 1) != xr_string::npos)
						{
							Species = I;
							break;
						}
					}
				}
				if (Species == AlternativeSpeciesGroups.size())
				{
					AlternativeSpeciesGroups.push_back({Name, {}});
				}
				AlternativeSpeciesGroups[Species].Models.push_back(u8(AlternativeModels.size()));
				AlternativeModels.push_back({Path, Species});
				Catalog += Path;
				Catalog += '\n';
			}
		}
		alt_models_count = (u32)objects.size() - alt_models_start;
		AlternativeCatalogCRC = crc32(Catalog.data(), u32(Catalog.size()));
		if (alt_models_count == 0)
			Msg("* CDetailManager::Load: no alt DM files in alternative_tree_dm, using vanilla assets");
		else
			Msg("* CDetailManager::Load: loaded %d alt DM models (start index: %d)", alt_models_count, alt_models_start);
	}

	// Get pointer to database (slots)
	IReader* m_slots = dtFS->open_chunk(2);
	dtSlots = (DetailSlot*)m_slots->pointer();
	m_slots->close();

	// Initialize 'vis' and 'cache'
	m_detail_layers_baking = true; // allow persisting the freshly generated bake
	DetailLayers_LoadUserMasks(); // sparse user strokes from $level$\detail_layers\user_mask_*.bin
	cache_ReInitialize();
	m_detail_layers_baking = false;

	// Hardware specific optimizations
	hw_Load();
	if (!Device.IsEditorMode())
	{
		ProducerThread.reset(new XRayWorkerThread([this]()
		{
			ProducerWork();
			task_finished.store(true, std::memory_order_release);
		}, "Vegetation producer"));
	}

	// swing desc
	// normal
	swing_desc[0].amp1	= pSettings->r_float("details","swing_normal_amp1");
	swing_desc[0].amp2	= pSettings->r_float("details","swing_normal_amp2");
	swing_desc[0].rot1	= pSettings->r_float("details","swing_normal_rot1");
	swing_desc[0].rot2	= pSettings->r_float("details","swing_normal_rot2");
	swing_desc[0].speed	= pSettings->r_float("details","swing_normal_speed");
	// fast
	swing_desc[1].amp1	= pSettings->r_float("details","swing_fast_amp1");
	swing_desc[1].amp2	= pSettings->r_float("details","swing_fast_amp2");
	swing_desc[1].rot1	= pSettings->r_float("details","swing_fast_rot1");
	swing_desc[1].rot2	= pSettings->r_float("details","swing_fast_rot2");
	swing_desc[1].speed	= pSettings->r_float("details","swing_fast_speed");
}

void CDetailManager::Unload		()
{
	StopProducer();
	CDetailWindPresets::Get().Clear();
	CDetailWind::Clear();
	hw_Unload();

	for (CDetail* dt : objects)
	{
		dt->Unload();
		xr_delete(dt);
	}
	objects.clear();
	AlternativeModels.clear();
	AlternativeSpeciesGroups.clear();
	alt_models_count = 0;
	AlternativeCatalogCRC = 0;

	FS.r_close(dtFS);
}

void CDetailManager::WaitForDetails()
{
	if (ProducerThread)
	{
		ProducerThread->Wait();
	}
	ApplyFieldEdits();
}

void CDetailManager::QueueFieldEdit(FieldEditKind Kind, float X, float Z, float Value, float Strength)
{
	xrCriticalSectionGuard Guard(FieldEditsAccess);
	PendingFieldEdits.push_back({Kind, X, Z, Value, Strength});
	FieldEditsQueued.store(true, std::memory_order_release);
}

void CDetailManager::ApplyFieldEdits()
{
	if (!FieldEditsQueued.load(std::memory_order_acquire))
	{
		return;
	}
	{
		xrCriticalSectionGuard Guard(FieldEditsAccess);
		PendingFieldEdits.swap(ProcessingFieldEdits);
		FieldEditsQueued.store(false, std::memory_order_release);
	}
	xrSRWLockGuard FieldsGuard(GeneratedFieldsAccess);
	for (const auto& Edit : ProcessingFieldEdits)
	{
		switch (Edit.Kind)
		{
			case FieldEditKind::Height: DetailLayers_PaintFMB(Edit.X, Edit.Z, Edit.Value, Edit.Strength); break;
			case FieldEditKind::Species: DetailLayers_PaintCluster(Edit.X, Edit.Z, u8(Edit.Value), Edit.Strength); break;
			case FieldEditKind::EraseHeight: DetailLayers_EraseFMB(Edit.X, Edit.Z, Edit.Strength); break;
			case FieldEditKind::EraseSpecies: DetailLayers_EraseCluster(Edit.X, Edit.Z, Edit.Strength); break;
		}
	}
	ProcessingFieldEdits.clear();
}

void CDetailManager::StopProducer()
{
	if (ProducerThread)
	{
		ProducerThread->Wait();
		ProducerThread->Stop();
		ProducerThread.reset();
	}
}

CDetailManager::~CDetailManager()
{
	StopProducer();
}

namespace
{
template <typename Visitor>
void VisitDetailSlots(CDetailManager& Manager, const Fvector& Camera, const CFrustum& View, bool Occlusion, Visitor&& Visit)
{
	const u32 Count = dm_slide_window_line * dm_slide_window_line;
	for (u32 Index = 0; Index < Count; ++Index)
	{
		auto& Group = Manager.slide_window[Index / dm_slide_window_line][Index % dm_slide_window_line];
		if (Group.empty)
		{
			continue;
		}
		u32 Mask = 0xff;
		const u32 Visible = View.testSAABB(Group.vis.sphere.P, Group.vis.sphere.R, Group.vis.box.data(), Mask);
		if (Visible == fcvNone || (Occlusion && !RImplementation.HOM.visible(Group.vis)))
		{
			continue;
		}
		for (u32 Cell = 0; Cell < dm_cache_count; ++Cell)
		{
			auto& Slot = **Group.slots[Cell];
			if (Slot.empty || Slot.type != CDetailManager::stReady)
			{
				continue;
			}
			if (Visible == fcvPartial)
			{
				u32 SlotMask = Mask;
				if (View.testSAABB(Slot.vis.sphere.P, Slot.vis.sphere.R, Slot.vis.box.data(), SlotMask) == fcvNone)
				{
					continue;
				}
			}
			const float DistanceSq = Camera.distance_to_sqr(Slot.vis.sphere.P);
			const float Radius = dm_fade + Slot.vis.sphere.R;
			if (DistanceSq > Radius * Radius || (Occlusion && !RImplementation.HOM.visible(Slot.vis)))
			{
				continue;
			}
			Visit(Slot, DistanceSq);
		}
	}
}
}

void CDetailManager::PrepareWindInstances(bool SimulateWind, const Fvector& Camera, const CFrustum& View, bool EditorMode, float GeometryDistance, float WindDistance)
{
	PROF_EVENT("Prepare detail render packet");
	const float GeometryDistanceSq = _sqr(GeometryDistance);
	const float WindDistanceSq = _sqr(WindDistance);
	const float FarDistance = std::max(GeometryDistance, WindDistance);
	const float FarDistanceSq = _sqr(FarDistance);
	WindUpdateSlots.clear();
	VisitDetailSlots(*this, Camera, View, !EditorMode, [this](Slot& Cell, float)
					 { WindUpdateSlots.push_back({&Cell, Cell.Generation}); });
	bool FarTrail = false;
	for (u32 Packed : TrampleActiveSlots)
	{
		const float X = float(s16(Packed & 0xffff)) * dm_slot_size + dm_slot_size * .5f - Camera.x;
		const float Z = float(s16(Packed >> 16)) * dm_slot_size + dm_slot_size * .5f - Camera.z;
		FarTrail |= X * X + Z * Z >= _sqr(std::max(0.f, FarDistance - dm_slot_size));
	}
	const bool ReuseStatic = PreparedPacketValid && FarDistance == PreviousPreparedFarDistance && !FarTrail && !PreviousFarHadTrails && Camera.similar(PreviousPreparedCamera, .000001f) && WindUpdateSlots == PreviousPreparedSlots;
	for (CDetail* Model : objects)
	{
		for (auto& Items : Model->ProducerItems)
		{
			Items.clear();
		}
		for (auto& Indices : Model->ProducerTrailIndices)
		{
			Indices.clear();
		}
		if (!ReuseStatic)
		{
			Model->ProducerStaticItems.clear();
			Model->ProducerDistantItems.clear();
		}
		Model->PreparedInstances.FarHasTrails = FarTrail || !Model->shader->E[5];
	}
	auto BoundsDistanceSq = [&](const Fbox& Bounds)
	{
		if (Bounds.min.x > Bounds.max.x)
		{
			return flt_max;
		}
		const float X = std::max({Bounds.min.x - Camera.x, 0.f, Camera.x - Bounds.max.x});
		const float Z = std::max({Bounds.min.z - Camera.z, 0.f, Camera.z - Bounds.max.z});
		return X * X + Z * Z;
	};
	auto BoundsInWind = [&](const Fbox& Bounds) { return BoundsDistanceSq(Bounds) < WindDistanceSq; };
	if (SimulateWind)
	{
		WindSamplingBounds.clear();
		for (const auto& Request : WindUpdateSlots)
		{
			if (BoundsInWind(Request.Cell->WindSampleBounds))
			{
				WindSamplingBounds.push_back(Request.Cell->WindSampleBounds);
			}
		}
		CDetailWind::PrefetchField(WindSamplingBounds);
	}
	for (const auto& Request : WindUpdateSlots)
	{
		Slot& Cell = *Request.Cell;
		const bool CellWind = SimulateWind && BoundsInWind(Cell.WindSampleBounds);
		// Fully distant cells can reuse their immutable 48-byte instance buffer.
		const bool CellDistant = BoundsDistanceSq(Cell.vis.box) >= FarDistanceSq;
		if (ReuseStatic && CellDistant)
		{
			bool AllStaticShaders = !EditorMode;
			for (const auto& Part : Cell.G)
			{
				if (Part.id < objects.size() && !objects[Part.id]->shader->E[4])
				{
					AllStaticShaders = false;
				}
			}
			if (AllStaticShaders)
			{
				continue;
			}
		}
		for (auto& Part : Cell.G)
		{
			if (Part.id >= objects.size())
			{
				continue;
			}
			CDetail& Model = *objects[Part.id];
			const bool HasFarShader = !EditorMode && Model.shader && Model.shader->E[4];
			const bool Animate = CellWind && !Model.m_Flags.is(DO_NO_WAVING);
			const float Height = Model.bv_bb.max.y - Model.bv_bb.min.y;
			for (u32 Variant = 0; Variant < 3; ++Variant)
			{
				for (CDetail::SlotItem* Item : Part.items[Variant])
				{
					const float X = Item->pos.x - Camera.x, Z = Item->pos.z - Camera.z;
					const float DistanceSq = X * X + Z * Z;
					if (DistanceSq >= FarDistanceSq && HasFarShader)
					{
						if (!ReuseStatic)
						{
							if (!Model.PreparedInstances.FarHasTrails)
							{
								Model.ProducerDistantItems.push_back(*Item);
								continue;
							}
							Model.ProducerStaticItems.push_back(*Item);
							if (Item->trample_strength >= .001f || Item->trample_visual >= .001f)
							{
								Model.ProducerTrailIndices[3].push_back(u32(Model.ProducerStaticItems.size() - 1));
							}
						}
						continue;
					}
					if (Animate && DistanceSq < WindDistanceSq)
					{
						CDetailWind::Update(Item->pos, std::max(Item->trample_visual, Item->trample_strength), Height * Item->scale, Item->WindBend, Item->WindPrevious, Item->WindVelocity, Item->WindPreviousVelocity);
					}
					const u32 Group = DistanceSq >= GeometryDistanceSq ? 0 : Variant;
					auto& Output = Model.ProducerItems[Group];
					Output.push_back(*Item);
					if (Item->trample_strength >= .001f || Item->trample_visual >= .001f)
					{
						Model.ProducerTrailIndices[Group].push_back(u32(Output.size() - 1));
					}
				}
			}
		}
	}
	TramplePresentationSources.clear();
	for (const CDetail* Model : objects)
	{
		for (u32 Group = 0; Group < 4; ++Group)
		{
			for (u32 Index : Model->ProducerTrailIndices[Group])
			{
				TramplePresentationSources.push_back(Group == 3 ? &Model->ProducerStaticItems[Index] : static_cast<const CDetail::StaticItem*>(&Model->ProducerItems[Group][Index]));
			}
		}
	}
	UpdateTramplePresentation();
	PreparedPacketValid = PrepareDetailBuffers(ReuseStatic);
	CDetailWind::Constants Constants;
	CDetailWind::ComputeConstants(Constants);
	PreparedWindConstants[0] = Constants.global;
	PreparedWindConstants[1] = Constants.xz1;
	PreparedWindConstants[2] = Constants.xz2;
	PreparedWindConstants[3] = Constants.xz3;
	PreparedWindConstants[3].y = WindDistance * .9f;
	PreparedWindConstants[3].z = std::max(.001f, WindDistance);
	PreviousPreparedSlots = WindUpdateSlots;
	PreviousPreparedCamera = Camera;
	PreviousPreparedFarDistance = FarDistance;
	PreviousFarHadTrails = FarTrail;
}

void CDetailManager::PublishDetailInstances()
{
	PROF_EVENT("Publish ready detail packet");
	for (CDetail* Model : objects)
	{
		std::swap(Model->RenderInstances, Model->PreparedInstances);
	}
	std::swap(TramplePresentationBuffer, PreparedTrampleBuffer);
	std::swap(TramplePresentationView, PreparedTrampleView);
	std::swap(TramplePresentationCapacity, PreparedTrampleCapacity);
	std::copy(std::begin(PreparedWindConstants), std::end(PreparedWindConstants), std::begin(PublishedWindConstants));
}
#include "../../xrEngine/xr_ioc_cmd.h"
void CDetailManager::Render()
{
	PROF_EVENT("Render details");
	const bool EditorMode = Device.IsEditorMode();
	if (!EditorMode && (!dtFS || !psDeviceFlags.is(rsDetails)))
	{
		return;
	}
	const bool Outdoor = EditorMode || RImplementation.SectorsCount() <= 1 ||
						 (RImplementation.pOutdoorSector && PortalTraverser.i_marker == RImplementation.pOutdoorSector->r_marker);
	if (Outdoor && VisibilityFrame != Device.dwFrame)
	{
		const bool Idle = task_finished.load(std::memory_order_acquire);
		if (Idle && CompletedWindTick != RunningWindTick)
		{
			CompletedWindTick = RunningWindTick;
			if (PreparedPacketValid)
			{
				PublishDetailInstances();
				WindInterpolationMs = CDetailWind::StepMilliseconds();
				WindPublishTime = RDEVICE.dwTimeGlobal;
			}
		}
		const bool Rebuild = Idle && ConsumeCacheRebuildRequest();
		const Fvector Camera = RDEVICE.vCameraPosition;
		const bool Schedule = Idle && (Rebuild || FieldEditsQueued.load(std::memory_order_acquire) || EditorMode || !StreamingInitialized || !RunningWindTick ||
									   RDEVICE.dwTimeGlobal - RunningWindTick >= 33 || Camera.distance_to_sqr(StreamingPreviousView) >= .01f);
		if (Schedule)
		{
			const Fvector Direction = RDEVICE.vCameraDirection;
			const Fmatrix ViewMatrix = Device.mView;
			const float Fov = Device.fFOV, Aspect = Device.fASPECT, NearPlane = Device.fViewportNear;
			const float FarPlane = g_pGamePersistent->Environment().CurrentEnv->fog_distance;
			const float Delta = Device.fTimeDelta;
			const bool SimulateWind = !EditorMode && ps_wind_enabled;
			const auto WindSettings = CDetailWind::Editable();
			const float WindBlend = ps_wind_blend, WindDirection = ps_wind_direction;
			const float GeometryDistance = ps_r__detail_geometry_distance, WindDistance = ps_r__detail_wind_distance;
			const u32 GrassSize = dm_current_size;
			const float GrassFade = dm_current_fade, GrassDensity = ps_current_detail_density;
			ps_wind_blend_current = std::clamp(WindBlend, 0.f, 1.f);
			const u32 FrameTime = RDEVICE.dwTimeGlobal;
			RunningWindTick = FrameTime;
			task_finished.store(false, std::memory_order_release);
			auto UpdateTask = [this, Rebuild, EditorMode, Camera, Direction, ViewMatrix, Fov, Aspect, NearPlane, FarPlane, Delta, SimulateWind, FrameTime, WindSettings, WindBlend, WindDirection, GeometryDistance, WindDistance, GrassSize, GrassFade, GrassDensity]()
			{
				ApplyFieldEdits();
				if (Rebuild)
				{
					cache_ReInitialize(GrassSize, GrassFade, GrassDensity);
				}
				cache_Update(Camera, Direction, FrameTime);
				if (!EditorMode && (ps_trample_enabled || !TrampleActiveSlots.empty()))
				{
					if (ps_trample_enabled)
					{
						TrampleProcessMarks();
					}
					TrampleAnimateItems();
				}
				CDetailWind::BeginFrame(Delta, Camera, WindSettings, WindBlend, WindDirection, SimulateWind);
				Fmatrix Projection, Transform;
				// A small guard band covers camera movement until the next ready packet.
				Projection.build_projection(deg2rad(std::min(170.f, Fov + 6.f)), Aspect, NearPlane, std::min(FarPlane, dm_fade));
				Transform.mul(Projection, ViewMatrix);
				CFrustum View;
				View.CreateFromMatrix(Transform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
				PrepareWindInstances(SimulateWind, Camera, View, EditorMode, GeometryDistance, WindDistance);
				CDetailWind::FinishFrame();
			};
			if (EditorMode)
			{
				UpdateTask();
				task_finished.store(true, std::memory_order_release);
				CompletedWindTick = RunningWindTick;
				if (PreparedPacketValid)
				{
					PublishDetailInstances();
				}
			}
			else
			{
				ProducerWork = std::move(UpdateTask);
				ProducerThread->Run();
			}
		}
		VisibilityFrame = Device.dwFrame;
	}
	float factor = 0.3f;
	if (!EditorMode)
	{
		float _factor = g_pGamePersistent->Environment().wind_strength_factor;
		static float smooth_factor = _factor;
		static float lastTime = 0.0f;
		float fTimeDelta = Device.fTimeDelta - lastTime;
		fTimeDelta *= 0.5f;
		smooth_factor += clampr(_factor - smooth_factor, -fTimeDelta, fTimeDelta);
		lastTime = Device.fTimeDelta;
		factor = smooth_factor;
	}
	swing_current.lerp(swing_desc[0], swing_desc[1], factor);

	float fDelta = Device.fTimeGlobal - m_global_time_old;

	if (fDelta < 0.0f || fDelta > 1.0f)
	{
		fDelta = Device.fTimeDelta;
	}

	wave_dir1.set(std::sin(m_time_rot_1), 0, std::cos(m_time_rot_1), 0).normalize().mul(swing_current.amp1);
	wave_dir2.set(std::sin(m_time_rot_2), 0, std::cos(m_time_rot_2), 0).normalize().mul(swing_current.amp2);

	m_global_time_old = Device.fTimeGlobal;

	m_time_rot_1 += (PI_MUL_2 * fDelta / swing_current.rot1);
	m_time_rot_2 += (PI_MUL_2 * fDelta / swing_current.rot2);
	m_time_pos += fDelta * swing_current.speed;

	hw_Render();

	if (m_frame_render != Device.dwFrame)
	{
		m_time_pos_old = m_time_pos;

		wave_dir1_old.set(wave_dir1);
		wave_dir2_old.set(wave_dir2);

		m_frame_render = Device.dwFrame;
	}
}
