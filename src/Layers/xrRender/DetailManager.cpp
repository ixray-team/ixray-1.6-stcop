// DetailManager.cpp: implementation of the CDetailManager class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#pragma hdrstop

#include "DetailManager.h"
#include "cl_intersect.h"

#include "../../xrEngine/igame_persistent.h"
#include "../../xrEngine/environment.h"

const float dbgOffset			= 0.f;
const int	dbgItems			= 128;

u32			dm_size = 24;
u32 		dm_cache1_line = 12;
u32			dm_cache_line = 49;
u32			dm_cache_size = 2401;
float		dm_fade = 47.5;
u32			dm_current_size = 24;
u32 		dm_current_cache1_line = 12;
u32			dm_current_cache_line = 49;
u32			dm_current_cache_size = 2401;
float		dm_current_fade = 47.5;
float		ps_current_detail_density = 0.6;
float		ps_current_detail_scale = 1.f;

void CDetailManager::SSwingValue::lerp(const SSwingValue& A, const SSwingValue& B, float f)
{
	float fi	= 1.f-f;
	amp1		= fi*A.amp1  + f*B.amp1;
	amp2		= fi*A.amp2  + f*B.amp2;
	rot1		= fi*A.rot1  + f*B.rot1;
	rot2		= fi*A.rot2  + f*B.rot2;
	speed		= fi*A.speed + f*B.speed;
}
//---------------------------------------------------

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CDetailManager::CDetailManager()
{
	dtFS 		= 0;
	dtSlots		= 0;
	hw_Geom		= 0;
	hw_BatchSize= 0;
	hw_VB		= 0;
	hw_IB		= 0;
	m_time_rot_1 = 0;
	m_time_rot_2 = 0;
	m_time_pos	= 0;
	m_global_time_old = 0;

	cache_Alloc();
}

void CDetailManager::cache_Alloc()
{
	dm_size = dm_current_size;
	dm_cache_line = dm_current_cache_line;
	dm_cache1_line = dm_current_cache1_line;
	dm_cache_size = dm_current_cache_size;
	dm_fade = dm_current_fade;
	ps_r__Detail_density = ps_current_detail_density;

	cache_level1.resize(dm_cache1_line, xr_vector<CacheSlot1>(dm_cache1_line));
	cache.resize(dm_cache_line, xr_vector<Slot*>(dm_cache_line));
	cache_pool.resize(dm_cache_size);
}

void CDetailManager::cache_Free()
{
	cache_pool.clear();
	cache.clear();
	cache_level1.clear();
	cache_task.clear();

#ifndef _EDITOR 
	for (CDetail& Objectl : objects)
	{
#else
	for (CDetail* D : objects)
	{
		CDetail& Objectl = *D;
#endif
		for (u32 i = 0; i < 3; ++i)
			for (u32 j = 0; j < 2; ++j)
				Objectl.m_items[i][j].clear();
	}
}

CDetailManager::~CDetailManager()
{
	cache_Free();
}

#ifndef _EDITOR
void CDetailManager::Load()
{
	// Open file stream
	if (!FS.exist("$level$","level.details"))
	{
		dtFS	= nullptr;
		return;
	}

	string_path			fn;
	FS.update_path		(fn,"$level$","level.details");
	dtFS				= FS.r_open(fn);

	// Header
	dtFS->r_chunk_safe	(0,&dtH,sizeof(dtH));
	R_ASSERT			(dtH.version == DETAIL_VERSION);
	u32 m_count			= dtH.object_count;

	// Models
	IReader* m_fs		= dtFS->open_chunk(1);
	for (u32 m_id = 0; m_id < m_count; m_id++)
	{
		IReader* S = m_fs->open_chunk(m_id);
		CDetail& dt	= objects.emplace_back();
		dt.Load(S);
		S->close();
	}
	m_fs->close		();

	// Get pointer to database (slots)
	IReader* m_slots	= dtFS->open_chunk(2);
	dtSlots				= (DetailSlot*)m_slots->pointer();
	m_slots->close		();

	// Initialize 'vis' and 'cache'
	cache_Initialize	();

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

	render_key = 0;
	calc_key = 1;
}
#endif
void CDetailManager::Unload		()
{
	hw_Unload();

	objects.clear();

	FS.r_close(dtFS);
	dtFS = 0;
}

extern ECORE_API float r_ssaDISCARD;

void CDetailManager::UpdateVisibleM()
{
	PROF_EVENT("CDetailManager");
	Fvector		EYE				= RDEVICE.vCameraPosition_saved;
	cache_Update(EYE);

	PROF_EVENT("UpdateVisibleM");
	CFrustum	View;
	View.CreateFromMatrix		(RDEVICE.mFullTransform_saved, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
	
	float fade_limit			= dm_fade;	fade_limit=fade_limit*fade_limit;
	float fade_start			= 1.f;		fade_start=fade_start*fade_start;
	float fade_range			= fade_limit-fade_start;
 	float r_ssaCHEAP			= 16*r_ssaDISCARD;

#ifndef _EDITOR 
	for (CDetail& Objectl : objects)
	{
#else
	for (CDetail* D : objects)
	{
		CDetail& Objectl = *D;
#endif
		for (u32 i = 0; i < 3; ++i)
			Objectl.m_items[i][calc_key].clear();
	}

	// Initialize 'vis' and 'cache'
	// Collect objects for rendering
	u32 max_index = dm_cache1_line*dm_cache1_line;
	for (u32 index = 0; index < max_index; index++)
	{
	    u32 _mz = index / dm_cache1_line;
	    u32 _mx = index % dm_cache1_line;
		CacheSlot1& MS = cache_level1[_mz][_mx];
		if (MS.empty)
			continue;

		u32 mask = 0xff;
		u32 res = View.testSAABB(MS.vis.sphere.P, MS.vis.sphere.R, MS.vis.box.data(), mask);
		if (fcvNone==res)
			continue;	// invisible-view frustum

		// test slots
		for (u32 _i=0; _i < dm_cache_count ; _i++)
		{
			Slot& S = **MS.slots[_i];

			// if slot empty - continue
			if (S.empty)
				continue;

			// if upper test = fcvPartial - test inner slots
			if (fcvPartial==res)
			{
				u32 _mask	= mask;
				u32 _res = View.testSAABB(S.vis.sphere.P, S.vis.sphere.R, S.vis.box.data(), _mask);
				if (fcvNone==_res)
					continue;	// invisible-view frustum
			}
#ifndef _EDITOR
			if (!RImplementation.HOM.visible(S.vis))
				continue;	// invisible-occlusion
#endif
			// Add to visibility structures
			// Calc fade factor	(per slot)
			float dist_sq = EYE.distance_to_sqr(S.vis.sphere.P);
			if (dist_sq>fade_limit) continue;
			float alpha = (dist_sq<fade_start)?0.f:(dist_sq-fade_start)/fade_range;
			float alpha_i = 1.f - alpha;
			float dist_sq_rcp = 1.f / dist_sq;

			for (int sp_id=0; sp_id<dm_obj_in_slot; sp_id++)
			{
				SlotPart& sp = S.G[sp_id];
				if (sp.id==DetailSlot::ID_Empty) continue;
#ifndef _EDITOR 
				CDetail& D = objects[sp.id];
#else
				CDetail& D = *objects[sp.id];
#endif
				float R = D.bv_sphere.R;
				float Rq_drcp = R*R*dist_sq_rcp;	// reordered expression for 'ssa' calc

				for (auto& SItem : sp.items)
				{
					CDetail::SlotItem& Item  = *SItem;
					float scale = Item.scale_calculated = Item.scale*alpha_i;
					float ssa = scale*scale*Rq_drcp;

					if (ssa < r_ssaDISCARD)
						continue;

					u32 vis_id = 0;
					if (ssa > r_ssaCHEAP)
						vis_id = Item.vis_ID;
					D.m_items[vis_id][calc_key].push_back(SItem);
				}
			}
		}
	}
}

void CDetailManager::Render()
{
	PROF_EVENT("Render details");
#ifndef _EDITOR
	if (0 == dtFS)						return;
	if (!psDeviceFlags.is(rsDetails))	return;
#endif

	MT_CALC.wait();

	int idx = calc_key;
	render_key = idx;
	calc_key = (idx + 1) % 2;

	static DWORD this_thread_id = 0;
	this_thread_id = GetCurrentThreadId();

	MT_CALC.run
	(
		[&]()
		{
#ifndef _EDITOR
			if (0 == dtFS)						return;
			if (!psDeviceFlags.is(rsDetails))	return;
#endif
			if (this_thread_id != GetCurrentThreadId()) 
			{
				PROF_THREAD("Details async")
			}
			UpdateVisibleM();
		}
	);

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

	if (fDelta < 0.0f || fDelta > 1.0f) {
		fDelta = Device.fTimeDelta;
	}

	wave_dir1.set(_sin(m_time_rot_1), 0, _cos(m_time_rot_1), 0).normalize().mul(swing_current.amp1);
	wave_dir2.set(_sin(m_time_rot_2), 0, _cos(m_time_rot_2), 0).normalize().mul(swing_current.amp2);

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