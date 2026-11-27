#include "stdafx.h"
#include "DetailManager.h"
#include "GrassThin.h"
#include "xrRender_console.h"
#include <numeric>

extern int ps_trample_enabled;
extern int ps_r__detail_radius;

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

void SetDetailRadius(int Radius)
{
	dm_current_size = u32(Radius / 4) * 2;
	dm_current_slide_window_line = dm_current_size / 2;
	dm_current_cache_line = dm_current_size * 2 + 1;
	dm_current_cache_size = dm_current_cache_line * dm_current_cache_line;
	dm_current_fade = float(2 * dm_current_size) - 0.5f;
}

// Set by r__detail_* console changes while the level is loaded.
// Deferred so that a whole cfg_load (many commands in one frame) triggers a
// single cache_ReInitialize instead of one full slot rebuild per command.
// Set from the console thread, consumed on the render thread.
std::atomic<bool> g_details_need_rebuild = false;

void CDetailManager::RequestCacheRebuild()
{
	if (RImplementation.GraphMain.b_loaded)
	{
		g_details_need_rebuild.store(true);
	}
}

bool CDetailManager::ConsumeCacheRebuildRequest()
{
	if (!g_details_need_rebuild.load())
		return false;
	g_details_need_rebuild.store(false);
	return true;
}

void CDetailManager::cache_Alloc()
{
	dm_size = dm_current_size;
	dm_cache_line = dm_current_cache_line;
	dm_slide_window_line = dm_current_slide_window_line;
	dm_cache_size = dm_current_cache_size;
	dm_fade = dm_current_fade;
	ps_r__Detail_density = ps_current_detail_density;

	slide_window.resize(dm_slide_window_line, xr_vector<SlideSlot>(dm_slide_window_line));
	cache.resize(dm_cache_line, xr_vector<Slot*>(dm_cache_line));
	cache_pool.resize(dm_cache_size);
	unpacked_slots.reserve(1024);

	const auto Away = [Middle = float(dm_slide_window_line) * 0.5f - 0.5f](u32 Index)
	{
		return _sqr(float(Index / dm_slide_window_line) - Middle) + _sqr(float(Index % dm_slide_window_line) - Middle);
	};

	SlideOrder.resize(dm_slide_window_line * dm_slide_window_line);
	std::iota(SlideOrder.begin(), SlideOrder.end(), 0u);
	std::sort(SlideOrder.begin(), SlideOrder.end(), [&](u32 A, u32 B) { return Away(A) < Away(B); });
}

void CDetailManager::cache_Free()
{
	cache_pool.clear();
	cache_pool.shrink_to_fit();
	cache.clear();
	cache.shrink_to_fit();
	slide_window.clear();
	slide_window.shrink_to_fit();
	unpacked_slots.clear();
	unpacked_slots.shrink_to_fit();
	items_pool.clear();
	Store.Reset();

	for (u32 Key = 0; Key < 2; ++Key)
	{
		StoredChunks[Key].clear();
		StoredGroups[Key].clear();
		StoredTotals[Key].clear();
		IsStoredResult[Key] = false;
	}

	for (CDetail* D : objects)
	{
		CDetail& Objectl = *D;
		for (u32 i = 0; i < 2; ++i)
		{
			for (u32 j = 0; j < 3; ++j)
			{
				Objectl.m_items[i][j].clear();
				Objectl.m_items[i][j].shrink_to_fit();
			}
		}
	}
}

void CDetailManager::Load()
{
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
		dt.LoadGeom();
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
	if (ps_r__detail_use_alternative_tree_assets)
	{
		FS_FileSet dm_files;
		FS.file_list(dm_files, "$level$", FS_ListFiles, "alternative_tree_dm\\*.dm");
		constexpr u32 kMaxObjects = u32(u8(-1));
		u32 obj_count = (u32)objects.size();
		for (const auto& dmf : dm_files)
		{
			if (obj_count >= kMaxObjects)
				break;
			string_path full_path;
			FS.update_path(full_path, "$level$", dmf.name.c_str());
			CDetail& dt = *objects.emplace_back(new CDetail());
			obj_count++;
			if (!dt.LoadFromDM(full_path))
			{
				xr_delete(objects.back());
				objects.pop_back();
				obj_count--;
				Msg("! CDetailManager::Load: alt model failed to load '%s'", full_path);
			}
		}
		alt_models_count = (u32)objects.size() - alt_models_start;
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
	SetDetailRadius(ps_r__detail_radius);
	m_detail_layers_baking = true; // allow persisting the freshly generated bake
	DetailLayers_LoadUserMasks(); // sparse user strokes from $level$\detail_layers\user_mask_*.bin
	cache_ReInitialize();
	m_detail_layers_baking = false;

	// Hardware specific optimizations
	hw_Load();

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
	Device.DetailsTask.wait();
	hw_Unload();

	for (CDetail* dt : objects)
	{
		dt->Unload();
		xr_delete(dt);
	}
	objects.clear();

	FS.r_close(dtFS);
}

#include "../../xrEngine/xr_ioc_cmd.h"
// What a frame's views of the grass share, made by the first of them: the lists the worker has finished, the wind
bool CDetailManager::Prepare()
{
	if (PreparedFrame == RFrame.dwFrame)
	{
		return IsPrepared;
	}

	PreparedFrame = RFrame.dwFrame;
	IsPrepared = false;
	const bool EditorMode = Device.IsEditorMode();
	bool in_outdoor = true;
	if (!EditorMode)
	{
	if (!dtFS) return false;
	if (!psDeviceFlags.is(rsDetails)) return false;

	// Deferred rebuild: a whole cfg_load sets the flag for every r__detail_* command;
	// apply it here once per frame on the render thread (batched) instead of inside the
	// DetailsTask worker, where cache_Free/cache_Alloc would race hw_Render_dump's read
	// of m_items[render_key]. wait() keeps the worker idle while we rebuild.
	if (ConsumeCacheRebuildRequest())
	{
		Device.DetailsTask.wait();
		cache_ReInitialize();
	}

	in_outdoor = RImplementation.SectorsCount() <= 1 || (RImplementation.pOutdoorSector && RImplementation.GraphMain.PortalTraverser.IsVisited(RImplementation.pOutdoorSector->index));
	}

	if(in_outdoor && task_finished.load())
	{
		task_finished.store(false);
		std::swap(render_key, calc_key);
		Fvector cam_pos = RFrame.vCameraPosition;

		// The grass ends where the fog or the far plane hides it when that comes before r_grass_distance: a bound of
		// what is unpacked and drawn, while the cache keeps the size of the setting
		const CEnvDescriptorMixer& Env = RFrame.Env();
		Reach = std::min({ dm_fade, Env.fog_distance, EditorMode ? dm_fade : Env.far_plane });

		// The lists are drawn a frame or more after the eye they were made for: wider than the view, so that a turn
		// does not show the slots they left out
		Fmatrix mProject, mFullTransform;
		mProject.build_projection(deg2rad(std::min(RFrame.fFOV * 1.3f, 150.0f)), RFrame.fASPECT, RFrame.fViewportNear, Reach);
		mFullTransform.mul(mProject, RFrame.mView);
		CFrustum View; View.CreateFromMatrix(mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
		// The occlusion buffer's own view: a box past its edge is tested on its border texels, and what stands there hid
		// the slots a turn brings in
		CFrustum Seen; Seen.CreateFromMatrix(RFrame.mFullTransform, FRUSTUM_P_LRTB);

		auto UpdateTask = [=, this]()
			{
				if (!EditorMode)
				{
					if (!dtFS) return;
					if (!psDeviceFlags.is(rsDetails)) return;
				}
				Store.BeginTask();
				++StoreTask;
				cache_Update(cam_pos);

				if (EditorMode)
				{
				}
				else if (ps_trample_enabled)
				{
					TrampleProcessMarks();
					TrampleAnimateItems();
				}
				else if (!TrampleActiveSlots.empty())
				{
					TrampleAnimateItems();
				}

				const bool IsThinned = !EditorMode;
				// The frame takes runs of the slots' stored blades: nothing is copied for it
				const bool IsStored = IsThinned && RImplementation.Scene.HasForwardGrass();
				xr_vector<u32>& Chunks = StoredChunks[calc_key];
				xr_vector<u32>& Totals = StoredTotals[calc_key];
				xr_vector<u32>& Groups = StoredGroups[calc_key];
				Chunks.clear();
				Groups.clear();
				Totals.assign(objects.size() * 3, 0);
				IsStoredResult[calc_key] = IsStored;

				{
					PROF_EVENT("UpdateVisible");
					for (CDetail* D : objects)
					{
						CDetail& Objectl = *D;
						for (u32 i = 0; i < 3; ++i)
							Objectl.m_items[calc_key][i].clear();
					}
					CHOM& HOM = RImplementation.HOM;
					float fade_limit = dm_fade;	fade_limit = fade_limit * fade_limit;
					float fade_start = 1.f; fade_start = fade_start * fade_start;
					float fade_range = fade_limit - fade_start;
					extern RENDER_API float r_ssaDISCARD;
					float ssa_D = r_ssaDISCARD;
					float r_ssaCHEAP = 16 * ssa_D;

					bool IsStoreFull = false;
					u32 Wanted = 0; // the blades of the nearest slot the store had no place for
					size_t Missed = 0; // its place among the shown
					xr_vector<ShownSlot>& Shown = StoreShownSlots;
					Shown.clear();

					// Nearest first: when the store is full, the farthest slots wait
					for (const u32 index : SlideOrder)
					{
						u32 _mz = index / dm_slide_window_line;
						u32 _mx = index % dm_slide_window_line;
						SlideSlot& MS = slide_window[_mz][_mx];
						if (MS.empty)
							continue;

						// The grass around the eye is in the lists whatever the view and the occlusion of the last frame
						// say: the GPU culls its blades by this frame's
						constexpr float AllAround = 16.0f;
						const bool IsAround = cam_pos.distance_to_sqr(MS.vis.sphere.P) < _sqr(MS.vis.sphere.R + AllAround);
						const auto IsHidden = [&](vis_data& Vis)
						{
							u32 SeenMask = 0xff;
							return !EditorMode && !IsAround && Seen.testSAABB(Vis.sphere.P, Vis.sphere.R, Vis.box.data(), SeenMask) == fcvFully && !HOM.visible(Vis);
						};
						u32 mask = 0xff;
						u32 res = IsAround ? u32(fcvFully) : View.testSAABB(MS.vis.sphere.P, MS.vis.sphere.R, MS.vis.box.data(), mask);
						if (fcvNone == res)
							continue;
						if (IsHidden(MS.vis))
							continue;
						for (u32 _i = 0; _i < dm_cache_count; _i++)
						{
							Slot** slots = MS.slots[_i];
							Slot* S = *slots;

							if (S->empty || S->type == stPending)
								continue;

							if (fcvPartial == res)
							{
								u32 _mask = mask;
								u32 _res = View.testSAABB(S->vis.sphere.P, S->vis.sphere.R, S->vis.box.data(), _mask);
								if (fcvNone == _res)
									continue;
							}
							if (IsHidden(S->vis))
								continue;
							float dist_sq = cam_pos.distance_to_sqr(S->vis.sphere.P);
							float alpha = (dist_sq < fade_start) ? 0.f : (dist_sq - fade_start) / fade_range;
							float dist_sq_rcp = 1.f / dist_sq;
							const float Near = std::max(_sqrt(dist_sq) - S->vis.sphere.R, 0.0f);
							const float KeptMost = IsThinned ? GrassKept(Near * Near) : 2.0f;

							float R = S->vis.sphere.R;
							float Rq_drcp = R * R * dist_sq_rcp;

							float scale = (R*0.8f)*(1.f - alpha);
							float ssa = scale * scale * Rq_drcp;

							// A game slot keeps its blades' wind at any distance: one that stopped swaying at once jumped
							u32 ssa_vis_id = 1;
							if (!IsThinned && (ssa <= r_ssaCHEAP || ssa < ssa_D))
							{
								ssa_vis_id = 0;
							}

							if (IsStored)
							{
								// Stored with room to come nearer; again when it has come nearer than that, or gone far enough to
								// give most of its place back. Past a slot the store had no place for, nothing is stored
								S->StoreShown = StoreTask;

								if (!IsStoreFull && (KeptMost > S->StoreKept || KeptMost < S->StoreKept * 0.25f))
								{
									StoreSlot(*S, std::min(KeptMost * 1.5f, 1.0f));

									if (S->StoreKept == 0.0f)
									{
										IsStoreFull = true;
										Wanted = S->StoreWanted;
										Missed = Shown.size();
									}
								}

								Shown.push_back({ S, KeptMost, ssa_vis_id, (_mz / 4) * ((dm_slide_window_line + 3) / 4) + _mx / 4 });
								continue;
							}

							for (int sp_id = 0; sp_id < dm_obj_in_slot; sp_id++)
							{
								SlotPart& sp = S->G[sp_id];

								if (sp.id >= objects.size())
								{
									continue;
								}
								CDetail& D = *objects[sp.id];
								auto& ditems = D.m_items[calc_key];
								auto& items = sp.items;
								for (u32 i = 0; i < 3; ++i)
								{
									auto& List = ditems[ssa_vis_id ? i : 0];

									for (CDetail::SlotItem* Item : items[i])
									{
										const float Lot = GrassLot(Item->pos);
										if (Lot >= KeptMost)
										{
											break;
										}

										if (!IsThinned || Lot < GrassKept(Item->pos.distance_to_sqr(cam_pos)))
										{
											List.push_back(*Item);
										}
									}
								}
							}
						}
					}

					// The slots this task did not show give their place back, and the farthest it shows as many blades as
					// the nearest one that found none wants: the store keeps the grass nearest to the eye. Settled before the
					// slots are listed, so that a frame never names a place given back
					if (IsStoreFull)
					{
						for (Slot& Each : cache_pool)
						{
							if (Each.StoreCount && Each.StoreShown != StoreTask)
							{
								StoreRelease(Each);
							}
						}

						for (size_t Each = Shown.size() - 1; Each > Missed && Wanted; --Each)
						{
							Wanted -= std::min(Wanted, Shown[Each].S->StoreCount);
							StoreRelease(*Shown[Each].S);
						}
					}

					Fbox Bounds;
					Bounds.invalidate();
					size_t GroupFirst = 0;
					const auto CloseGroup = [&]()
					{
						if (Chunks.size() > GroupFirst)
						{
							u32 Record[5];
							Fsphere Sphere;
							Bounds.getsphere(Sphere.P, Sphere.R);
							std::memcpy(Record, &Sphere, sizeof(Sphere));
							Record[4] = u32(GroupFirst) | (u32(Chunks.size() - GroupFirst - 1) << 24);
							Groups.insert(Groups.end(), Record, Record + 5);
						}

						GroupFirst = Chunks.size();
						Bounds.invalidate();
					};

					// Listed a block of 4 x 4 slots at a time, where its nearest slot comes: a group of the cull fills with the
					// blades of neighbouring slots, as far slots have a few each
					xr_vector<u32>& BlockSeen = StoreBlockSeen;
					xr_vector<u64>& Order = StoreListOrder;
					BlockSeen.assign(_sqr((dm_slide_window_line + 3) / 4), u32(-1));
					Order.clear();

					for (u32 Index = 0; Index < Shown.size(); ++Index)
					{
						u32& Seen = BlockSeen[Shown[Index].Block];
						Seen = std::min(Seen, Index);
						Order.push_back((u64(Seen) << 32) | Index);
					}

					std::sort(Order.begin(), Order.end());

					for (size_t Place = 0; Place < Order.size(); ++Place)
					{
						const ShownSlot& Each = Shown[u32(Order[Place])];

						if (Place && Order[Place] >> 32 != Order[Place - 1] >> 32)
						{
							CloseGroup();
						}

						Slot* S = Each.S;
						const float KeptMost = Each.KeptMost;
						const u32 ssa_vis_id = Each.Wind;
						u32 First = S->StoreFirst;

						for (int sp_id = 0; sp_id < dm_obj_in_slot && S->StoreCount; sp_id++)
						{
							const SlotPart& sp = S->G[sp_id];

							for (u32 i = 0; i < 3; ++i)
							{
								const auto& List = sp.items[i];
								u32 Kept = 0;
								u32 Stored = 0;

								// In the order of their lot: the blades a slot this far keeps are the first of those it has stored
								for (; Stored < List.size(); ++Stored)
								{
									const float Lot = GrassLot(List[Stored]->pos);
									if (Lot >= S->StoreKept)
									{
										break;
									}

									Kept += sp.id < objects.size() && Lot < KeptMost;
								}

								const u32 Key = sp.id * 3 + (ssa_vis_id ? i : 0);

								// The sphere around the root a blade is culled by on the GPU, as GrassBin::Radius
								const Fsphere& Model = Kept ? objects[sp.id]->bv_sphere : Fsphere{};
								const float Radius = Model.R + _sqrt(Model.P.x * Model.P.x + Model.P.z * Model.P.z) + std::abs(Model.P.y);

								for (u32 Blade = 0; Blade < Kept; ++Blade)
								{
									const CDetail::SlotItem& Item = *List[Blade];
									Bounds.modify(Fvector(Item.pos).sub(Radius * Item.scale));
									Bounds.modify(Fvector(Item.pos).add(Radius * Item.scale));
									Chunks.push_back((First + Blade) | (Key << GrassSlotStore::Bits));

									if (Chunks.size() - GroupFirst == SCENE_GRASS_GROUP)
									{
										CloseGroup();
									}
								}

								if (Kept)
								{
									Totals[Key] += Kept;
								}

								First += Stored;
							}
						}
					}

					CloseGroup();
				}
				if (IsStored)
				{
					// A run's bin, as hw_Render_batch counts them: the swaying variants first, a model and variant with blades each
					xr_vector<u32>& Bins = StoreBins;
					Bins.assign(Totals.size(), GrassSlotStore::Absent);
					u32 BinCount = 0;

					for (u32 Order = 0; Order < 3; ++Order)
					{
						for (size_t Index = 0; Index < objects.size(); ++Index)
						{
							const size_t Key = Index * 3 + (Order + 1) % 3;

							if (Totals[Key] && BinCount < SCENE_GRASS_BINS)
							{
								Bins[Key] = BinCount++;
							}
						}
					}

					size_t Written = 0;
					size_t Read = 0;
					size_t GroupsKept = 0;

					for (size_t Group = 0; Group < Groups.size(); Group += 5)
					{
						const size_t Start = Written;

						for (const size_t End = Read + (Groups[Group + 4] >> 24) + 1; Read < End; ++Read)
						{
							const u32 Word = Chunks[Read];

							if (const u32 Bin = Bins[Word >> GrassSlotStore::Bits]; Bin != GrassSlotStore::Absent)
							{
								Chunks[Written++] = (Word & (GrassSlotStore::Capacity - 1)) | (Bin << GrassSlotStore::Bits);
							}
						}

						if (Written > Start)
						{
							std::copy_n(Groups.begin() + Group, 4, Groups.begin() + GroupsKept);
							Groups[GroupsKept + 4] = u32(Start) | (u32(Written - Start - 1) << 24);
							GroupsKept += 5;
						}
					}

					Chunks.resize(Written);
					Groups.resize(GroupsKept);
				}

				for (CDetail* D : objects)
				{
					for (u32 i = 0; i < 3; ++i)
					{
						auto& List = D->m_items[calc_key][i];
						const auto Far = IsThinned && D->FarCount ? std::partition(List.begin(), List.end(), [&](const CDetail::SlotItem& Item) { return !GrassIsFar(Item.pos, cam_pos); }) : List.end();
						D->FarStart[calc_key][i] = u32(Far - List.begin());
					}
				}

				// After the lists, so that the depth map they are tested on is still this frame's
				if (!EditorMode)
				{
					UnpackPending(cam_pos, Reach, View);
				}

				task_finished.store(true);
			};

		// Editor slot unpacking picks scene geometry through the shared XRC collider, which the
		// primary thread also uses for picking. Keep the editor update on the primary thread.
		if (EditorMode)
		{
			UpdateTask();
		}
		else
		{
			Device.DetailsTask.run(UpdateTask);
		}
	}

	float factor = 0.3f;
	if (!EditorMode)
	{
		float _factor = RFrame.WindStrength;
		static float smooth_factor = _factor;
		static float lastTime = 0.0f;
		float fTimeDelta = RFrame.fTimeDelta - lastTime; fTimeDelta *= 0.5f;
		smooth_factor += clampr(_factor - smooth_factor, -fTimeDelta, fTimeDelta);
		lastTime = RFrame.fTimeDelta;
		factor = smooth_factor;
	}
	swing_current.lerp(swing_desc[0], swing_desc[1], factor);

	float fDelta = RFrame.fTimeGlobal - m_global_time_old;

	if (fDelta < 0.0f || fDelta > 1.0f)
		fDelta = RFrame.fTimeDelta;

	wave_dir1.set(std::sin(m_time_rot_1), 0, std::cos(m_time_rot_1), 0).normalize().mul(swing_current.amp1);
	wave_dir2.set(std::sin(m_time_rot_2), 0, std::cos(m_time_rot_2), 0).normalize().mul(swing_current.amp2);

	m_global_time_old = RFrame.fTimeGlobal;

	m_time_rot_1 += (PI_MUL_2 * fDelta / swing_current.rot1);
	m_time_rot_2 += (PI_MUL_2 * fDelta / swing_current.rot2);
	m_time_pos += fDelta * swing_current.speed;

	IsPrepared = true;
	return true;
}

void CDetailManager::Render(GfxCmdList& Cmd)
{
	PROF_EVENT("Render details");

	if (!Prepare())
	{
		return;
	}

	hw_Render(Cmd);

	if (m_frame_render != RFrame.dwFrame) {
		m_time_pos_old = m_time_pos;

		wave_dir1_old.set(wave_dir1);
		wave_dir2_old.set(wave_dir2);

		m_frame_render = RFrame.dwFrame;
	}
}