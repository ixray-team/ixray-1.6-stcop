#include "stdafx.h"
#include "DetailManager.h"

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
#ifndef _EDITOR 
	for (CDetail& Objectl : objects)
	{
#else
	for (CDetail* D : objects)
	{
		CDetail& Objectl = *D;
#endif
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

#ifndef _EDITOR
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
		CDetail& dt	= objects.emplace_back();
		dt.Load(S);
		S->close();
	}
	m_fs->close();

	// Get pointer to database (slots)
	IReader* m_slots = dtFS->open_chunk(2);
	dtSlots = (DetailSlot*)m_slots->pointer();
	m_slots->close();

	// Initialize 'vis' and 'cache'
	cache_ReInitialize();

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
#endif
void CDetailManager::Unload		()
{
	Device.DetailsTask.wait();
	hw_Unload();

#ifndef _EDITOR
	for (CDetail& dt : objects)
		dt.Unload();
#else
	for (CDetail* dt : objects)
	{
		dt->Unload();
		xr_delete(dt);
	}
#endif
	objects.clear();

	FS.r_close(dtFS);
}

#include "../../xrEngine/xr_ioc_cmd.h"
void CDetailManager::Render()
{
	PROF_EVENT("Render details");
#ifndef _EDITOR
	if (!dtFS) return;
	if (!psDeviceFlags.is(rsDetails)) return;
	bool in_outdoor = RImplementation.SectorsCount() <= 1 || (RImplementation.pOutdoorSector && PortalTraverser.i_marker == RImplementation.pOutdoorSector->r_marker);
	if(in_outdoor && task_finished.load())
#else
	if (task_finished.load())
#endif
	{
		task_finished.store(false);
		std::swap(render_key, calc_key);
		Fvector cam_pos = RDEVICE.vCameraPosition;

		Fmatrix mProject, mFullTransform;
		mProject.build_projection(deg2rad(Device.fFOV), Device.fASPECT, Device.fViewportNear, std::min(g_pGamePersistent->Environment().CurrentEnv->fog_distance, dm_fade));
		mFullTransform.mul(mProject, Device.mView);
		CFrustum View; View.CreateFromMatrix(mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);

		Device.DetailsTask.run
		(
			[=, this]()
			{
#ifndef _EDITOR
				if (!dtFS) return;
				if (!psDeviceFlags.is(rsDetails)) return;
#endif
				cache_Update(cam_pos);

				{
					PROF_EVENT("UpdateVisible");
#ifndef _EDITOR 
					for (CDetail& Objectl : objects)
					{
#else
					for (CDetail* D : objects)
					{
						CDetail& Objectl = *D;
#endif
						for (u32 i = 0; i < 3; ++i)
							Objectl.m_items[calc_key][i].clear();
					}
#ifndef _EDITOR
					CHOM& HOM = RImplementation.HOM;
#endif
					float fade_limit = dm_fade;	fade_limit = fade_limit * fade_limit;
					float fade_start = 1.f; fade_start = fade_start * fade_start;
					float fade_range = fade_limit - fade_start;
					extern ECORE_API float r_ssaDISCARD;
					float ssa_D = r_ssaDISCARD;
					float r_ssaCHEAP = 16 * ssa_D;

					u32 max_index = dm_slide_window_line * dm_slide_window_line;
					for (u32 index = 0; index < max_index; index++)
					{
						u32 _mz = index / dm_slide_window_line;
						u32 _mx = index % dm_slide_window_line;
						SlideSlot& MS = slide_window[_mz][_mx];
						if (MS.empty)
							continue;

						u32 mask = 0xff;
						u32 res = View.testSAABB(MS.vis.sphere.P, MS.vis.sphere.R, MS.vis.box.data(), mask);
						if (fcvNone == res)
							continue;
#ifndef _EDITOR
						if (!HOM.visible(MS.vis))
							continue;
#endif
						for (u32 _i = 0; _i < dm_cache_count; _i++)
						{
							Slot** slots = MS.slots[_i];
							Slot* S = *slots;

							if (S->empty)
								continue;

							if (fcvPartial == res)
							{
								u32 _mask = mask;
								u32 _res = View.testSAABB(S->vis.sphere.P, S->vis.sphere.R, S->vis.box.data(), _mask);
								if (fcvNone == _res)
									continue;
							}
#ifndef _EDITOR
							if (!HOM.visible(S->vis))
								continue;
#endif
							float dist_sq = cam_pos.distance_to_sqr(S->vis.sphere.P);
							float alpha = (dist_sq < fade_start) ? 0.f : (dist_sq - fade_start) / fade_range;
							float dist_sq_rcp = 1.f / dist_sq;

							float R = S->vis.sphere.R;
							float Rq_drcp = R * R * dist_sq_rcp;

							float scale = (R*0.8f)*(1.f - alpha);
							float ssa = scale * scale * Rq_drcp;

							u32 ssa_vis_id = 1;
							if (ssa <= r_ssaCHEAP || ssa < ssa_D)
								ssa_vis_id = 0;

							for (int sp_id = 0; sp_id < dm_obj_in_slot; sp_id++)
							{
								SlotPart& sp = S->G[sp_id];

								if (sp.id >= objects.size())
								{
									continue;
								}
#ifndef _EDITOR 
								CDetail& D = objects[sp.id];
#else
								CDetail& D = *objects[sp.id];
#endif
								auto& ditems = D.m_items[calc_key];
								auto& items = sp.items;
								if (ssa_vis_id == 0)
								{
									for (u32 i = 0; i < 3; ++i)
									{
										for (CDetail::SlotItem* Item : items[i])
											ditems[0].push_back(*Item);
									}
								}
								else
								{
									for (u32 i = 0; i < 3; ++i)
									{
										for (CDetail::SlotItem* Item : items[i])
											ditems[i].push_back(*Item);
									}
								}
							}
						}
					}
				}
#ifdef USE_DX11
				if (ps_r2_ls_flags.test(R2FLAG_FAST_DETAILS_UPDATE))//experimental
				{
					PROF_EVENT("UpdateBuffers");
					RHIBufferDesc bufferDesc{};
					bufferDesc.Usage = ERHI_USAGE::USAGE_IMMUTABLE;
					bufferDesc.Type = ERHI_BUFFER_TYPE::STRUCTURED;
					bufferDesc.CPUAccessFlags = ERHI_CPU_ACCESS_FLAG::ERHI_CPU_ACCESS_FLAG_NONE;
					bufferDesc.StructureByteStride = sizeof(CDetail::SlotItem);

					RHIShaderResourceViewDesc srvDesc{};
					srvDesc.Format = ERHI_FORMAT::UNKNOWN;
					RHIBufferSubresource vbInit{};

#ifdef _EDITOR
					for (CDetail* DPtr : objects)
					{
						CDetail& D = *DPtr;
#else
					for (CDetail& D : objects)
					{
#endif
						auto& ditems = D.m_items[calc_key];
						auto& buffers = D.DetailGPUBoundBuffers[calc_key];
						for (u32 i = 0; i < 3; ++i)
						{
							auto& buff = buffers[i];
							u32 buff_size = ditems[i].size();
							if (buff_size)
							{
								_RELEASE(buff.first);
								_RELEASE(buff.second);

								bufferDesc.Size = buff_size * sizeof(CDetail::SlotItem);

								vbInit.pSysMem = ditems[i].data();
								buff.first = GRHI->CreateBuffer(bufferDesc, &vbInit);

								srvDesc.ElementWidth = buff_size;
								buff.second = GRHI->CreateShaderResourceView(buff.first, &srvDesc);
							}
							else
							{
								_RELEASE(buff.first);
								_RELEASE(buff.second);
							}
						}
					}
				}
#endif
				task_finished.store(true);
			}
		);
	}

#ifndef _EDITOR
	float _factor = g_pGamePersistent->Environment().wind_strength_factor;
	static float factor = _factor;
	static float lastTime = 0.0f;
	float fTimeDelta = Device.fTimeDelta - lastTime; fTimeDelta *= 0.5f;
	factor += clampr(_factor - factor, -fTimeDelta, fTimeDelta);
	lastTime = Device.fTimeDelta;
#else
	float factor = 0.3f;
#endif
	swing_current.lerp(swing_desc[0], swing_desc[1], factor);

	float fDelta = Device.fTimeGlobal - m_global_time_old;

	if (fDelta < 0.0f || fDelta > 1.0f)
		fDelta = Device.fTimeDelta;

	wave_dir1.set(std::sin(m_time_rot_1), 0, std::cos(m_time_rot_1), 0).normalize().mul(swing_current.amp1);
	wave_dir2.set(std::sin(m_time_rot_2), 0, std::cos(m_time_rot_2), 0).normalize().mul(swing_current.amp2);

	m_global_time_old = Device.fTimeGlobal;

	m_time_rot_1 += (PI_MUL_2 * fDelta / swing_current.rot1);
	m_time_rot_2 += (PI_MUL_2 * fDelta / swing_current.rot2);
	m_time_pos += fDelta * swing_current.speed;

	hw_Render();

	if (m_frame_render != Device.dwFrame) {
		m_time_pos_old = m_time_pos;

		wave_dir1_old.set(wave_dir1);
		wave_dir2_old.set(wave_dir2);

		m_frame_render = Device.dwFrame;
	}
}