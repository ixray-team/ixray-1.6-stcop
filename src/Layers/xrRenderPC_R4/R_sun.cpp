#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IRenderable.h"

#include "FBasicVisual.h"
#include "R_sun_support.h"

#include <DirectXMath.h>

using namespace DirectX;

constexpr float tweak_COP_initial_offs = 1200.0f;

extern float r_ssaDISCARD;
extern float r_ssaLOD_A;
extern float r_ssaLOD_B;
 
//////////////////////////////////////////////////////////////////////////
// tables to calculate view-frustum bounds in world space
// note: D3D uses [0..1] range for Z
static Fvector3		corners [8]			= {
	{ -1, -1,  0 },		{ -1, -1, +1},
	{ -1, +1, +1 },		{ -1, +1,  0},
	{ +1, +1, +1 },		{ +1, +1,  0},
	{ +1, -1, +1},		{ +1, -1,  0}
};

static int			facetable[6][4]		= {
	{ 6, 7, 5, 4 },		{ 1, 0, 7, 6 },
	{ 1, 2, 3, 0 },		{ 3, 2, 4, 5 },		
	// near and far planes
	{ 0, 3, 5, 7 },		{  1, 6, 4, 2 },
};

Fvector3		wform	(Fmatrix& m, Fvector3 const& v)
{
	Fvector4	r;
	r.x			= v.x*m._11 + v.y*m._21 + v.z*m._31 + m._41;
	r.y			= v.x*m._12 + v.y*m._22 + v.z*m._32 + m._42;
	r.z			= v.x*m._13 + v.y*m._23 + v.z*m._33 + m._43;
	r.w			= v.x*m._14 + v.y*m._24 + v.z*m._34 + m._44;
	// VERIFY		(r.w>0.f);
	float invW = 1.0f/r.w;
	Fvector3	r3 = { r.x*invW, r.y*invW, r.z*invW };
	return		r3;
}

void CRender::init_cacades()
{
	u32 cascade_count = 3;
	m_sun_cascades.resize(cascade_count);

	float fBias = -0.0000025f;

	m_sun_cascades[0].reset_chain = true;
	m_sun_cascades[0].size = 15;
	m_sun_cascades[0].bias = m_sun_cascades[0].size*fBias;

	m_sun_cascades[1].size = 40;
	m_sun_cascades[1].bias = m_sun_cascades[1].size*fBias;

	m_sun_cascades[2].size = 160;
	m_sun_cascades[2].bias = m_sun_cascades[2].size*fBias;
}

void CRender::reset_sun_collect()
{
	sun_kicked = false;
}

void CRender::publish_sun_collect(bool active)
{
	sun_collect_active.store(active, std::memory_order_release);
	sun_kicked = true;
	sun_ticket.fetch_add(1, std::memory_order_release);
	sun_ticket.notify_all();
}

void CRender::ensure_sun_collect()
{
	if (!sun_kicked)
		publish_sun_collect(false);
}

void CRender::begin_sun_collect()
{
	publish_sun_collect(prepare_sun_cascade_xforms());
}

void CRender::wait_sun_collect()
{
	const u32 ticket = sun_ticket.load(std::memory_order_acquire);
	u32 done = sun_done.load(std::memory_order_acquire);
	while (done != ticket)
	{
		sun_done.wait(done, std::memory_order_acquire);
		done = sun_done.load(std::memory_order_acquire);
	}
}

void CRender::collect_sun_cascades()
{
	u32 ticket = sun_ticket.load(std::memory_order_acquire);
	while (ticket == sun_seen)
	{
		sun_ticket.wait(sun_seen, std::memory_order_acquire);
		ticket = sun_ticket.load(std::memory_order_acquire);
	}
	sun_seen = ticket;

	if (sun_collect_active.load(std::memory_order_acquire) && pOutdoorSector && sun_cascade_count > 0)
	{
		R_CullTLS cull;
		cull.active = true;
		cull.phase = PHASE_SMAP;
		cull.ssa_discard = r_ssaDISCARD;
		cull.ssa_lod_a = r_ssaLOD_A;
		cull.ssa_lod_b = r_ssaLOD_B;
		g_r_cull_tls = cull;

		const u32 sector_count = (u32)Sectors.size();
		const u32 portal_count = (u32)Portals.size();
		const u32 count = std::min(sun_cascade_count, (u32)GraphSun.size());

		for (u32 i = 0; i < count; ++i)
		{
			R_dsgraph_structure& Graph = GraphSun[i];
			Graph.private_marker = true;
			Graph.val_pTransform = &Fidentity;
			Graph.val_bHUD = false;
			Graph.val_bUI = false;
			Graph.val_bInvisible = false;
			Graph.val_pObject = nullptr;
			Graph.private_visuals.clear();
			Graph.r_dsgraph_clear_aux();
			Graph.r_pmask(true, false);
			Graph.PortalTraverser.prepare_local_clips(sector_count, portal_count);
			Graph.r_dsgraph_render_subspace(pOutdoorSector, sun_cascade_xforms[i], sun_cull_cop, true);
		}

		g_r_cull_tls.active = false;
	}

	sun_done.store(ticket, std::memory_order_release);
	sun_done.notify_all();
}

bool CRender::prepare_sun_cascade_xforms()
{
	light* fuckingsun = (light*)Lights.sun_adapted._get();
	if (!fuckingsun || o.sunstatic || u_diffuse2s(fuckingsun->color) <= EPS)
		return false;

	bool b_need_to_render_sunshafts = RImplementation.Target->need_to_render_sunshafts();
	bool last_cascade_chain_mode = m_sun_cascades.back().reset_chain;

	if (psDeviceFlags.test(rsClearBB))
	{
		m_sun_cascades[2].size = 1000.0f;
		b_need_to_render_sunshafts = true;
	}

//	b_need_to_render_sunshafts |= !!RImplementation.o.offscreen_reflecitons;

	if (b_need_to_render_sunshafts)
	{
		m_sun_cascades[m_sun_cascades.size() - 1].reset_chain = true;
	}

	{
		PROF_EVENT("Render Cascades: Batch Prepass");

		// Calculate view-frustum bounds in world space once for all cascades
		Fmatrix ex_project = Device.mProject;
		Fmatrix ex_full;
		ex_full.mul(ex_project, Device.mView);
		Fmatrix ex_full_inverse;
		ex_full_inverse.invert44(ex_full);
		Fmatrix fullxform_inv = ex_full_inverse;

		// COP - 100 km away (constant across cascades)
		Fvector3 cull_COP;
		cull_COP.mad(Device.vCameraPosition, fuckingsun->direction, -tweak_COP_initial_offs);

		// Create approximate ortho-xform (constant across cascades)
		Fmatrix mdir_View;
		Fvector L_pos = fuckingsun->position;
		Fvector L_dir = fuckingsun->direction;
		L_dir.normalize();
		Fvector L_right(1.f, 0.f, 0.f);
		if (std::abs(L_right.dotproduct(L_dir)) > .99f)
			L_right.set(0.f, 0.f, 1.f);
		Fvector L_up;
		L_up.crossproduct(L_dir, L_right).normalize();
		L_right.crossproduct(L_up, L_dir).normalize();
		mdir_View.build_camera_dir(L_pos, L_dir, L_up);

		// Light top plane (constant across cascades)
		Fplane light_top_plane;
		light_top_plane.build_unit_normal(L_pos, L_dir);
		float dist = light_top_plane.classify(Device.vCameraPosition);

		// Build viewport xform once
		float view_dim = float(RImplementation.o.smapsize);
		Fmatrix m_viewport = {
			view_dim / 2.f,	0.0f,				0.0f,		0.0f,
			0.0f,			-view_dim / 2.f,	0.0f,		0.0f,
			0.0f,			0.0f,				1.0f,		0.0f,
			view_dim / 2.f,	view_dim / 2.f,		0.0f,		1.0f
		};
		Fmatrix m_viewport_inv{};
		m_viewport_inv.invert44(m_viewport);

		const u32 cascade_count = std::min((u32)m_sun_cascades.size(), (u32)sun_cascade_xforms.size());
		
		sun_cascade_count = cascade_count;
		sun_cull_cop = cull_COP;
		sun_restore_shafts = b_need_to_render_sunshafts;
		sun_saved_reset_chain = last_cascade_chain_mode;

		for (u32 cascade_ind = 0; cascade_ind < cascade_count; ++cascade_ind)
		{
			Fvector cam_dir = Device.vCameraDirection;
			if (cascade_ind == (cascade_count - 1) && !!RImplementation.o.offscreen_reflecitons && !psDeviceFlags.test(rsClearBB))
			{
				cam_dir.mad(Fidentity.c, fuckingsun->direction, -1.0f);
			}

#ifdef _DEBUG
			typedef FixedConvexVolume<true> t_cuboid;
#else
			typedef FixedConvexVolume<false> t_cuboid;
#endif
			t_cuboid light_cuboid;
			if (cascade_ind == 0 || m_sun_cascades[cascade_ind].reset_chain)
			{
				Fvector3 near_p, edge_vec;
				for (int p = 0; p < 4; p++)
				{
					near_p = wform(fullxform_inv, corners[facetable[4][p]]);
					edge_vec = wform(fullxform_inv, corners[facetable[5][p]]);
					edge_vec.sub(near_p);
					edge_vec.normalize();
					light_cuboid.view_frustum_rays.push_back(sun::ray(near_p, edge_vec));
				}
			}
			else
			{
				light_cuboid.view_frustum_rays = m_sun_cascades[cascade_ind].rays;
			}

			light_cuboid.view_ray.P = Device.vCameraPosition;
			light_cuboid.view_ray.D = cam_dir;
			light_cuboid.light_ray.P = L_pos;
			light_cuboid.light_ray.D = L_dir;

			float map_size = m_sun_cascades[cascade_ind].size;
			Fmatrix mdir_Project;
			mdir_Project.OrthographicOffCenterLH(-map_size * 0.5f, map_size * 0.5f, -map_size * 0.5f, map_size * 0.5f, 0.1f, dist + 1.41421f * map_size);

			Fmatrix cull_xform;
			cull_xform.mul(mdir_Project, mdir_View);
			Fmatrix cull_xform_inv;
			cull_xform_inv.invert(cull_xform);

			for (int p = 0; p < 8; p++)
			{
				Fvector3 xf = wform(cull_xform_inv, corners[p]);
				light_cuboid.light_cuboid_points[p] = xf;
			}

			for (int plane = 0; plane < 4; plane++)
			{
				for (int pt = 0; pt < 4; pt++)
				{
					int asd = facetable[plane][pt];
					light_cuboid.light_cuboid_polys[plane].points[pt] = asd;
				}
			}

			xr_vector<Fplane> cull_planes;
			Fvector lightXZshift;
			light_cuboid.compute_caster_model_fixed(cull_planes, lightXZshift, m_sun_cascades[cascade_ind].size, m_sun_cascades[cascade_ind].reset_chain);

			if (cascade_ind < cascade_count - 1)
				m_sun_cascades[cascade_ind + 1].rays = light_cuboid.view_frustum_rays;

			Fvector cam_shifted = L_pos;
			cam_shifted.add(lightXZshift);

			Fmatrix cascade_mdir_View;
			cascade_mdir_View.build_camera_dir(cam_shifted, L_dir, L_up);
			cull_xform.mul(mdir_Project, cascade_mdir_View);
			cull_xform_inv.invert(cull_xform);

			Fvector cam_proj = Device.vCameraPosition;
			constexpr float align_aim_step_coef = 4.f;
			cam_proj.set(floorf(cam_proj.x / align_aim_step_coef) + align_aim_step_coef / 2, floorf(cam_proj.y / align_aim_step_coef) + align_aim_step_coef / 2, floorf(cam_proj.z / align_aim_step_coef) + align_aim_step_coef / 2);
			cam_proj.mul(align_aim_step_coef);
			Fvector cam_pixel = wform(cull_xform, cam_proj);
			cam_pixel = wform(m_viewport, cam_pixel);
			Fvector shift_proj = lightXZshift;
			cull_xform.transform_dir(shift_proj);
			m_viewport.transform_dir(shift_proj);

			constexpr float align_granularity = 4.f;
			shift_proj.x = shift_proj.x > 0 ? align_granularity : -align_granularity;
			shift_proj.y = shift_proj.y > 0 ? align_granularity : -align_granularity;
			shift_proj.z = 0;

			cam_pixel.x = cam_pixel.x / align_granularity - floorf(cam_pixel.x / align_granularity);
			cam_pixel.y = cam_pixel.y / align_granularity - floorf(cam_pixel.y / align_granularity);
			cam_pixel.x *= align_granularity;
			cam_pixel.y *= align_granularity;
			cam_pixel.z = 0;

			cam_pixel.sub(shift_proj);

			m_viewport_inv.transform_dir(cam_pixel);
			cull_xform_inv.transform_dir(cam_pixel);
			Fvector diff = cam_pixel;
			static float sign_test = -1.f;
			diff.mul(sign_test);
			Fmatrix adjust;
			adjust.translate(diff);
			cull_xform.mulB_44(adjust);

			m_sun_cascades[cascade_ind].xform = cull_xform;
			sun_cascade_xforms[cascade_ind] = cull_xform;
		}

		return true;
	}

	return false;
}

void CRender::render_sun_cascades()
{
	wait_sun_collect();
	if (!sun_collect_active.load(std::memory_order_acquire))
		return;

	light* fuckingsun = (light*)Lights.sun_adapted._get();
	const u32 cascade_count = sun_cascade_count;

	RHIViewport viewport = {
			0.f, 0.f, (float)RImplementation.o.smapsize, (float)RImplementation.o.smapsize, 0.f, 1.f
		};

		GRHI->SetViewport(viewport);

	phase = PHASE_SMAP;

	for (u32 i = 0; i < cascade_count; ++i)
	{
		PROF_EVENT("Render Cascade: SMAP");

		R_dsgraph_structure& Graph = GraphSun[i];
		fuckingsun->X.D.combine = sun_cascade_xforms[i];

		bool bNormal = Graph.mapNormalPasses[0][0].size() || Graph.mapMatrixPasses[0][0].size();
		bool bSpecial = Graph.mapNormalPasses[1][0].size() || Graph.mapMatrixPasses[1][0].size() || Graph.mapSorted.size();

		if (bNormal || bSpecial)
		{
			GRHI->ClearDepthStencil(Target->rt_smap_depth_sun_dsv[i], ERHI_CLEAR_TARGET::DEPTH, 1.f, 0);
			Target->u_setrt(Target->rt_smap_surf, nullptr, nullptr, Target->rt_smap_depth_sun_dsv[i]);

			RCache.set_xform_world(Fidentity);
			RCache.set_xform_view(Fidentity);
			RCache.set_xform_project(fuckingsun->X.D.combine);

			Graph.r_dsgraph_render_graph(0);

			if (Details && Details->dtFS && ps_r2_ls_flags.test(R2FLAG_SUN_DETAILS))
			{
				Details->hw_Render();
			}

			fuckingsun->X.D.transluent = false;

			if (bSpecial)
			{
				fuckingsun->X.D.transluent = true;
				Target->phase_smap_direct_tsh(fuckingsun, SE_SUN_FAR);
				Graph.r_dsgraph_render_graph(1);
				Graph.r_dsgraph_render_sorted();
			}
		}

		Graph.r_pmask(true, false);
	}

	GraphMain.r_pmask(true, false);

	RCache.set_xform_world(Fidentity);
	RCache.set_xform_view(Device.mView);
	RCache.set_xform_project(Device.mProject);

	viewport.Width = (float)RCache.get_width();
	viewport.Height = (float)RCache.get_height();

	GRHI->SetViewport(viewport);
	Target->accum_direct_cascade();

	if (sun_restore_shafts)
	{
		m_sun_cascades[m_sun_cascades.size() - 1].reset_chain = sun_saved_reset_chain;
	}

	if (psDeviceFlags.test(rsClearBB))
	{
		m_sun_cascades[2].size = 160.0f;
	}
}

void CRender::render_sun_cascade(u32 cascade_ind)
{
	PROF_EVENT("Render Cascade");
	light* fuckingsun = (light*)Lights.sun_adapted._get();

	Fvector cam_dir = Device.vCameraDirection;

	if (cascade_ind == (m_sun_cascades.size() - 1) && !!RImplementation.o.offscreen_reflecitons && !psDeviceFlags.test(rsClearBB))
	{
		cam_dir.mad(Fidentity.c, fuckingsun->direction, -1.0f);
	}

	CFrustum cull_frustum;
	xr_vector<Fplane> cull_planes;
	Fvector3 cull_COP;
	Fmatrix cull_xform;
	{
		PROF_EVENT("Render Cascade: Prepass");

		// calculate view-frustum bounds in world space
		Fmatrix	ex_project, ex_full, ex_full_inverse;
		{
			ex_project = Device.mProject;
			ex_full.mul(ex_project, Device.mView);
			ex_full_inverse.invert44(ex_full);
		}

		// Compute volume(s) - something like a frustum for infinite directional light
		// Also compute virtual light position and sector it is inside
		{
			// Lets begin from base frustum
			Fmatrix		fullxform_inv = ex_full_inverse;
#ifdef	_DEBUG
			typedef		DumbConvexVolume<true>	t_volume;
#else
			typedef		DumbConvexVolume<false>	t_volume;
#endif

			//******************************* Need to be placed after cuboid built **************************

			// COP - 100 km away
			cull_COP.mad(Device.vCameraPosition, fuckingsun->direction, -tweak_COP_initial_offs);

			// Create approximate ortho-xform
			// view: auto find 'up' and 'right' vectors
			Fmatrix						mdir_View, mdir_Project;
			Fvector						L_dir, L_up, L_right, L_pos;
			L_pos.set(fuckingsun->position);
			L_dir.set(fuckingsun->direction).normalize();
			L_right.set(1, 0, 0);					if (std::abs(L_right.dotproduct(L_dir)) > .99f)	L_right.set(0, 0, 1);
			L_up.crossproduct(L_dir, L_right).normalize();
			L_right.crossproduct(L_up, L_dir).normalize();
			mdir_View.build_camera_dir(L_pos, L_dir, L_up);



			//////////////////////////////////////////////////////////////////////////
#ifdef	_DEBUG
			typedef		FixedConvexVolume<true>		t_cuboid;
#else
			typedef		FixedConvexVolume<false>	t_cuboid;
#endif

			t_cuboid light_cuboid;
			{
				// Initialize the first cascade rays, then each cascade will initialize rays for next one.
				if (cascade_ind == 0 || m_sun_cascades[cascade_ind].reset_chain)
				{
					Fvector3				near_p, edge_vec;
					for (int p = 0; p < 4; p++)
					{
						near_p = wform(fullxform_inv, corners[facetable[4][p]]);

						edge_vec = wform(fullxform_inv, corners[facetable[5][p]]);
						edge_vec.sub(near_p);
						edge_vec.normalize();

						light_cuboid.view_frustum_rays.push_back(sun::ray(near_p, edge_vec));
					}
				}
				else
					light_cuboid.view_frustum_rays = m_sun_cascades[cascade_ind].rays;

				light_cuboid.view_ray.P = Device.vCameraPosition;
				light_cuboid.view_ray.D = cam_dir;
				light_cuboid.light_ray.P = L_pos;
				light_cuboid.light_ray.D = L_dir;
			}

			// THIS NEED TO BE A CONSTATNT
			Fplane light_top_plane;
			light_top_plane.build_unit_normal(L_pos, L_dir);
			float dist = light_top_plane.classify(Device.vCameraPosition);

			float map_size = m_sun_cascades[cascade_ind].size;

			mdir_Project.OrthographicOffCenterLH(-map_size * 0.5f, map_size * 0.5f, -map_size * 0.5f, map_size * 0.5f, 0.1, dist + 1.41421f * map_size);

			// build viewport xform
			float	view_dim = float(RImplementation.o.smapsize);
			Fmatrix	m_viewport = {
				view_dim / 2.f,	0.0f,				0.0f,		0.0f,
				0.0f,			-view_dim / 2.f,		0.0f,		0.0f,
				0.0f,			0.0f,				1.0f,		0.0f,
				view_dim / 2.f,	view_dim / 2.f,		0.0f,		1.0f
			};
			Fmatrix m_viewport_inv{};
			m_viewport_inv.invert44(m_viewport);

			// snap view-position to pixel
			cull_xform.mul(mdir_Project, mdir_View);
			Fmatrix	cull_xform_inv; cull_xform_inv.invert(cull_xform);


			//		light_cuboid.light_cuboid_points.reserve		(9);
			for (int p = 0; p < 8; p++) {
				Fvector3				xf = wform(cull_xform_inv, corners[p]);
				light_cuboid.light_cuboid_points[p] = xf;
			}

			// only side planes
			for (int plane = 0; plane < 4; plane++)
				for (int pt = 0; pt < 4; pt++)
				{
					int asd = facetable[plane][pt];
					light_cuboid.light_cuboid_polys[plane].points[pt] = asd;
				}


			Fvector lightXZshift;
			light_cuboid.compute_caster_model_fixed(cull_planes, lightXZshift, m_sun_cascades[cascade_ind].size, m_sun_cascades[cascade_ind].reset_chain);
			Fvector proj_view = cam_dir;
			proj_view.y = 0;
			proj_view.normalize();
			//			lightXZshift.mad(proj_view, 20);

						// Initialize rays for the next cascade
			if (cascade_ind < m_sun_cascades.size() - 1)
				m_sun_cascades[cascade_ind + 1].rays = light_cuboid.view_frustum_rays;

#ifdef	_DEBUG
			static bool draw_debug = false;
			if (draw_debug && cascade_ind == 0)
				for (u32 it = 0; it < cull_planes.size(); it++)
					RImplementation.Target->dbg_addplane(cull_planes[it], it * 0xFFF);
#endif

			Fvector cam_shifted = L_pos;
			cam_shifted.add(lightXZshift);

			// rebuild the view transform with the shift.
			mdir_View.identity();
			mdir_View.build_camera_dir(cam_shifted, L_dir, L_up);
			cull_xform.identity();
			cull_xform.mul(mdir_Project, mdir_View);
			cull_xform_inv.invert(cull_xform);

			// Create frustum for query
			cull_frustum._clear();

			for (u32 p = 0; p < cull_planes.size(); p++)
			{
				cull_frustum._add(cull_planes[p]);
			}

			{
				Fvector cam_proj = Device.vCameraPosition;
				constexpr float		align_aim_step_coef = 4.f;
				cam_proj.set(floorf(cam_proj.x / align_aim_step_coef) + align_aim_step_coef / 2, floorf(cam_proj.y / align_aim_step_coef) + align_aim_step_coef / 2, floorf(cam_proj.z / align_aim_step_coef) + align_aim_step_coef / 2);
				cam_proj.mul(align_aim_step_coef);
				Fvector	cam_pixel = wform(cull_xform, cam_proj);
				cam_pixel = wform(m_viewport, cam_pixel);
				Fvector shift_proj = lightXZshift;
				cull_xform.transform_dir(shift_proj);
				m_viewport.transform_dir(shift_proj);

				constexpr float	align_granularity = 4.f;
				shift_proj.x = shift_proj.x > 0 ? align_granularity : -align_granularity;
				shift_proj.y = shift_proj.y > 0 ? align_granularity : -align_granularity;
				shift_proj.z = 0;

				cam_pixel.x = cam_pixel.x / align_granularity - floorf(cam_pixel.x / align_granularity);
				cam_pixel.y = cam_pixel.y / align_granularity - floorf(cam_pixel.y / align_granularity);
				cam_pixel.x *= align_granularity;
				cam_pixel.y *= align_granularity;
				cam_pixel.z = 0;

				cam_pixel.sub(shift_proj);

				m_viewport_inv.transform_dir(cam_pixel);
				cull_xform_inv.transform_dir(cam_pixel);
				Fvector diff = cam_pixel;
				static float sign_test = -1.f;
				diff.mul(sign_test);
				Fmatrix adjust;		adjust.translate(diff);
				cull_xform.mulB_44(adjust);
			}

			m_sun_cascades[cascade_ind].xform = cull_xform;

			s32		limit = RImplementation.o.smapsize - 1;
			fuckingsun->X.D.minX = 0;
			fuckingsun->X.D.maxX = limit;
			fuckingsun->X.D.minY = 0;
			fuckingsun->X.D.maxY = limit;
		}
	}

	// Begin SMAP-render
	{
		PROF_EVENT("Render Cascade: SMAP");
		{
			bool bSpecialFull = GraphMain.mapNormalPasses[1][0].size() || GraphMain.mapMatrixPasses[1][0].size() || GraphMain.mapSorted.size();
			VERIFY(!bSpecialFull);
			phase = PHASE_SMAP;
			GraphMain.r_pmask(true, /*!!RImplementation.o.Tshadows &&*/ false);
		}

		// Fill the database
		GraphMain.r_dsgraph_render_subspace(pOutdoorSector, cull_xform, cull_COP, true);

		// Finalize & Cleanup
		fuckingsun->X.D.combine = cull_xform;

		// Render shadow-map
		//. !!! We should clip based on shrinked frustum (again)
		{
			bool bNormal = GraphMain.mapNormalPasses[0][0].size() || GraphMain.mapMatrixPasses[0][0].size();
			bool bSpecial = GraphMain.mapNormalPasses[1][0].size() || GraphMain.mapMatrixPasses[1][0].size() || GraphMain.mapSorted.size();

			if (bNormal || bSpecial)
			{
				GRHI->ClearDepthStencil(Target->rt_smap_depth_sun_dsv[cascade_ind], ERHI_CLEAR_TARGET::DEPTH, 1.f, 0);
				Target->u_setrt(Target->rt_smap_surf, nullptr, nullptr, Target->rt_smap_depth_sun_dsv[cascade_ind]);
				RCache.set_xform_world(Fidentity);
				RCache.set_xform_view(Fidentity);

				RCache.set_xform_project(fuckingsun->X.D.combine);

				GraphMain.r_dsgraph_render_graph(0);

				if (Details && Details->dtFS && ps_r2_ls_flags.test(R2FLAG_SUN_DETAILS))
				{
					Details->hw_Render();
				}

				fuckingsun->X.D.transluent = false;

				if (bSpecial)
				{
					fuckingsun->X.D.transluent = true;
					Target->phase_smap_direct_tsh(fuckingsun, SE_SUN_FAR);
					GraphMain.r_dsgraph_render_graph(1); // normal level, secondary priority
					GraphMain.r_dsgraph_render_sorted(); // strict-sorted geoms
				}
			}
		}

		// End SMAP-render
		GraphMain.r_pmask(true, false);
	}

}