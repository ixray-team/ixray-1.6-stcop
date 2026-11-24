#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IRenderable.h"
#include "../../xrEngine/EngineAPI.h"

#include "FBasicVisual.h"
#include "R_sun_support.h"

#include <DirectXMath.h>

using namespace DirectX;

constexpr float tweak_COP_initial_offs = 1200.0f;
 
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
	m_sun_cascades.resize(3);
	m_sun_cascades[0].reset_chain = true;
	ResetSunShadowCache();
}

void CRender::ResetSunShadowCache()
{
	for (sun::cascade& Cascade : m_sun_cascades)
	{
		Cascade.IsValid = false;
		Cascade.xform.identity();
		Cascade.CaptureDepthRange = 0;
		Cascade.CaptureBias = 0;
		Cascade.CaptureFrame = 0;
	}
	for (sun::CascadePreparation& Prepared : PreparedSunCascades)
	{
		Prepared.PrepareMilliseconds = 0;
		Prepared.RenderMilliseconds = 0;
	}
	SunUpdatedMask = 0;
}

// The forward frame keeps no cache of the sun's cascades: it draws the nearest every frame and the others in turn
int CRender::GetSunShadowCacheMode() const
{
	return 0;
}

void CRender::GetSunShadowConstants(u32 Index, Fmatrix& Transform, Fvector4& Capture, Fvector4& Warp) const
{
	Transform.identity();
	Capture.set(0, 0, 0, 0);
	Warp.set(0, 0, 0, 0);
	if (Index >= m_sun_cascades.size())
	{
		return;
	}
	const sun::cascade& Cascade = m_sun_cascades[Index];
	Fmatrix Adjust{0.5f, 0, 0, 0, 0, -0.5f, 0, 0, 0, 0, 1, 0, 0.5f, 0.5f, Cascade.CaptureBias, 1};
	Transform.mul(Adjust, Cascade.xform);
	Capture.set(Cascade.CaptureDirection.x, Cascade.CaptureDirection.y, Cascade.CaptureDirection.z, Cascade.IsValid ? Cascade.CaptureDepthRange : 0.f);
	Warp.w = Cascade.CaptureBias;
}

void CRender::DumpSunShadowCache() const
{
	Msg("* sun shadow cascades: rendered_mask=%u", SunUpdatedMask);
	for (u32 Index = 0; Index < m_sun_cascades.size(); ++Index)
	{
		const sun::cascade& Cascade = m_sun_cascades[Index];
		const sun::CascadePreparation& Prepared = PreparedSunCascades[Index];
		Msg("* sun cascade %u: valid=%d capture_frame=%u age=%u prepare_cpu=%.3fms render_cpu=%.3fms", Index + 1, Cascade.IsValid, Cascade.CaptureFrame, Device.dwFrame - Cascade.CaptureFrame, Prepared.PrepareMilliseconds, Prepared.RenderMilliseconds);
	}
}

// The pre-render stage starts before FrameMove, so the cascades follow the camera of the previous
// frame. The captures are sampled with the same matrices: only their coverage lags by a frame.
void CRender::prepare_sun_collect(u32 Frame)
{
	SunCollectFrame = Frame;
	RestoreSunCascadeChain();
	SunView = MakeGraphView(PHASE_SMAP);
	sun_collect_active = prepare_sun_cascade_xforms(Frame);
}

GraphView CRender::MakeSunView(u32 Cascade) const
{
	GraphView View = SunView;
	View.SetSubspace(pOutdoorSector, sun_cascade_xforms[Cascade], sun_cull_cop);
	return View;
}

bool CRender::IsSunCascadeCollected(u32 Cascade) const
{
	// The editors have no sectors: they collect their cascades themselves (RenderFarSunCascade)
	return sun_collect_active && (pOutdoorSector || Device.IsEditorMode()) && (SunUpdatedMask & (1u << Cascade));
}

void CRender::RestoreSunCascadeChain()
{
	if (sun_restore_shafts)
	{
		m_sun_cascades[m_sun_cascades.size() - 1].reset_chain = sun_saved_reset_chain;
		sun_restore_shafts = false;
	}

	m_sun_cascades[0].size = ps_r2_sun_near;
	m_sun_cascades[1].size = _sqrt(ps_r2_sun_near * ps_r2_sun_far);
	m_sun_cascades[2].size = ps_r2_sun_far;
	for (auto& Cascade : m_sun_cascades) {
		Cascade.bias = Cascade.size * -0.0000025f;
	}
}

bool CRender::prepare_sun_cascade_xforms(u32 Frame)
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
		Fmatrix ex_project = RFrame.mProject;
		Fmatrix ex_full;
		ex_full.mul(ex_project, RFrame.mView);
		Fmatrix ex_full_inverse;
		ex_full_inverse.invert44(ex_full);
		Fmatrix fullxform_inv = ex_full_inverse;

		// COP - 100 km away (constant across cascades)
		Fvector3 cull_COP;
		cull_COP.mad(RFrame.vCameraPosition, fuckingsun->direction, -tweak_COP_initial_offs);

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
		float dist = light_top_plane.classify(RFrame.vCameraPosition);

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

		SunUpdatedMask = 0;

		for (u32 cascade_ind = 0; cascade_ind < cascade_count; ++cascade_ind)
		{
			CTimer PrepareTimer;
			PrepareTimer.Start();
			Fvector cam_dir = RFrame.vCameraDirection;
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

			light_cuboid.view_ray.P = RFrame.vCameraPosition;
			light_cuboid.view_ray.D = cam_dir;
			light_cuboid.light_ray.P = L_pos;
			light_cuboid.light_ray.D = L_dir;

			float map_size = m_sun_cascades[cascade_ind].size;
			Fmatrix mdir_Project;
			mdir_Project.OrthographicOffCenterLH(-map_size * 0.5f, map_size * 0.5f, -map_size * 0.5f, map_size * 0.5f, 0.1f, dist + 1.41421f * map_size);
			PreparedSunCascades[cascade_ind].DepthRange = dist + 1.41421f * map_size - 0.1f;

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

			Fvector cam_proj = RFrame.vCameraPosition;
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

			sun_cascade_xforms[cascade_ind] = cull_xform;
			PreparedSunCascades[cascade_ind].PrepareMilliseconds = PrepareTimer.GetElapsed_sec() * 1000.f;
			SunUpdatedMask |= 1u << cascade_ind;
		}

		return true;
	}

	return false;
}
