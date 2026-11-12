#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IRenderable.h"
#include "FBasicVisual.h"

#include "R_sun_support.h"

const	float	tweak_rain_COP_initial_offs = 1200.f;
const	float	tweak_rain_ortho_xform_initial_offs = 1000.f;	//. ?

//	Defined in r2_R_sun.cpp
Fvector3		wform(Fmatrix& m, Fvector3 const& v);

//////////////////////////////////////////////////////////////////////////
// tables to calculate view-frustum bounds in world space
// note: D3D uses [0..1] range for Z
static Fvector3		corners[8] = {
	{ -1, -1,  0 },		{ -1, -1, +1},
	{ -1, +1, +1 },		{ -1, +1,  0},
	{ +1, +1, +1 },		{ +1, +1,  0},
	{ +1, -1, +1},		{ +1, -1,  0}
};
static int			facetable[6][4] = {
	{ 0, 3, 5, 7 },		{ 1, 2, 3, 0 },
	{ 6, 7, 5, 4 },		{ 4, 2, 1, 6 },
	{ 3, 2, 4, 5 },		{ 1, 0, 7, 6 },
};

bool CRender::IsRainPassEnabled() const
{
	const bool PbrRainPuddles = !o.dx11_use_legacy_light && ps_r2_ls_flags_ext.test(R4FLAG_RAIN_PUDDLES) && (o.HasRainPuddleMask || o.ProceduralRainPuddles);
	return ps_r2_ls_flags.test(R3FLAG_DYN_WET_SURF) || PbrRainPuddles;
}

GraphView CRender::MakeRainView() const
{
	GraphView View = MakeGraphView(PHASE_SMAP);
	View.SetSubspace(pOutdoorSector, RainTransform, RainOrigin);
	return View;
}

void CRender::begin_rain_collect()
{
	if (!CanCollectSceneOnWorker() || !IsRainPassEnabled())
	{
		return;
	}

	// FrameMove increments the counter after this point
	RainCollectFrame = Device.dwFrame + 1;
	RainActive = prepare_rain_collect();
	if (RainActive && pOutdoorSector)
	{
		GraphJobs.push_back({ &GraphRain, MakeRainView() });
	}
}

// The matrices follow the camera of the previous frame, like the sun cascades
bool CRender::prepare_rain_collect()
{
	float fRainFactor = g_pGamePersistent->Environment().CurrentEnv->rain_density;
	float& fRainFov = RainFov;

	float fWetnessFactor = g_pGamePersistent->Environment().wetness_factor;

	if (std::max(fWetnessFactor, fRainFactor) < EPS_L)
	{
		fRainFov = Device.fFOV;
		return false;
	}

	//	Use light as placeholder for rain data.
	// нет необходимости создавать каждый кадр структуру размером почти в киллобайт на стеке.
	light& RainLight = *RImplementation.Lights.rain_light;

	static const float source_offset = 10000.f;
	RainLight.direction.set(0.0f, -1.0f, 0.0f);
	RainLight.position.set(Device.vCameraPosition.x, Device.vCameraPosition.y + source_offset, Device.vCameraPosition.z);

	float	fBoundingSphereRadius = 0;

	// calculate view-frustum bounds in world space
	Fmatrix	ex_project{}, ex_full{};
	Fmatrix ex_full_inverse{};

	const float fRainFar = ps_r3_dyn_wet_surf_far;
	fRainFov = std::max(fRainFov, Device.fFOV);

	ex_project.build_projection(deg2rad(fRainFov), Device.fASPECT, Device.fViewportNear, fRainFar);
	ex_full.mul(ex_project, Device.mView);
	ex_full_inverse.invert44(ex_full);

	//	Calculate view frustum were we can see dynamic rain radius
	{
		const float H = fRainFar;
		const float a = tanf(deg2rad(fRainFov) / 2);
		const float c = tanf(deg2rad(fRainFov * Device.fASPECT) / 2);
		const float b_2 = H * H * (1.0f + a * a + c * c);

		fBoundingSphereRadius = b_2 / (2.0f * H);
	}

	// Compute volume(s) - something like a frustum for infinite directional light
	// Also compute virtual light position and sector it is inside
	CFrustum					cull_frustum;
	xr_vector<Fplane>			cull_planes;
	Fvector3					cull_COP;
	CSector* cull_sector;
	Fmatrix						cull_xform;

	{
		// Lets begin from base frustum
		Fmatrix		fullxform_inv = ex_full_inverse;
#ifdef	_DEBUG
		typedef		DumbConvexVolume<true>	t_volume;
#else
		typedef		DumbConvexVolume<false>	t_volume;
#endif
		t_volume					hull;
		{
			hull.points.reserve(9);
			for(int p = 0; p < 8; p++) {
				Fvector3				xf = wform(fullxform_inv, corners[p]);
				hull.points.push_back(xf);
			}
			for(int plane = 0; plane < 6; plane++) {
				hull.polys.push_back(t_volume::_poly());
				for(int pt = 0; pt < 4; pt++)
					hull.polys.back().points.push_back(facetable[plane][pt]);
			}
		}
		//hull.compute_caster_model	(cull_planes,fuckingsun->direction);
		hull.compute_caster_model(cull_planes, RainLight.direction);

#ifdef	_DEBUG
		for(u32 it = 0; it < cull_planes.size(); it++)
			RImplementation.Target->dbg_addplane(cull_planes[it], 0xffffffff);
#endif

		// COP - 100 km away
		cull_COP.mad(Device.vCameraPosition, RainLight.direction, -tweak_rain_COP_initial_offs);
		cull_COP.x += fBoundingSphereRadius * Device.vCameraDirection.x;
		cull_COP.z += fBoundingSphereRadius * Device.vCameraDirection.z;

		// Create frustum for query
		cull_frustum._clear();

		for(u32 p = 0; p < cull_planes.size(); p++)
		{
			cull_frustum._add(cull_planes[p]);
		}

		// Create approximate ortho-xform
		// view: auto find 'up' and 'right' vectors
		Fmatrix mdir_View, mdir_Project;
		Fvector L_dir, L_up, L_right, L_pos;

		L_pos.set(RainLight.position);
		L_dir.set(RainLight.direction).normalize();

		L_right = abs(L_dir.x) > 0.99f ? Fidentity.k : Fidentity.i;

		L_up.crossproduct(L_dir, L_right).normalize();
		L_right.crossproduct(L_up, L_dir).normalize();

		mdir_View.build_camera_dir(L_pos, L_dir, L_up);

		// projection: box
		//	Simple
		Fbox frustum_bb; frustum_bb.invalidate();
		for(int it = 0; it < 8; it++) 
		{
			Fvector	xf = wform(mdir_View, hull.points[it]);
			frustum_bb.modify(xf);
		}

		Fbox& bb = frustum_bb;
		bb.grow(EPS);

		//	Offset RainLight position to center rain shadowmap
		Fvector3 vRectOffset;
		vRectOffset.set(fBoundingSphereRadius * Device.vCameraDirection.x, 0, fBoundingSphereRadius * Device.vCameraDirection.z);
		bb.min.x = -fBoundingSphereRadius + vRectOffset.x;
		bb.max.x = fBoundingSphereRadius + vRectOffset.x;
		bb.min.y = -fBoundingSphereRadius + vRectOffset.z;
		bb.max.y = fBoundingSphereRadius + vRectOffset.z;

		mdir_Project.OrthographicOffCenterLH(bb.min.x, bb.max.x, bb.min.y, bb.max.y,
			bb.min.z - tweak_rain_ortho_xform_initial_offs,
			bb.min.z + 2 * tweak_rain_ortho_xform_initial_offs);
		cull_xform.mul(mdir_Project, mdir_View);

		s32		limit = std::min((int)RImplementation.o.smapsize, ps_r3_dyn_wet_surf_sm_res);

		// build viewport xform
		float	view_dim = float(limit);
		float	fTexelOffs = (.5f / RImplementation.o.smapsize);
		Fmatrix	m_viewport = {
			view_dim / 2.f,	0.0f,				0.0f,		0.0f,
			0.0f,			-view_dim / 2.f,		0.0f,		0.0f,
			0.0f,			0.0f,				1.0f,		0.0f,
			view_dim / 2.f + fTexelOffs,	view_dim / 2.f + fTexelOffs,		0.0f,		1.0f
		};
		Fmatrix m_viewport_inv{};
		m_viewport_inv.invert44(m_viewport);

		// snap view-position to pixel
		//	snap zero point to pixel
		Fvector cam_proj = wform(cull_xform, Fvector().set(0, 0, 0));
		Fvector	cam_pixel = wform(m_viewport, cam_proj);
		cam_pixel.x = floorf(cam_pixel.x);
		cam_pixel.y = floorf(cam_pixel.y);
		Fvector cam_snapped = wform(m_viewport_inv, cam_pixel);
		Fvector diff;		diff.sub(cam_snapped, cam_proj);
		Fmatrix adjust;		adjust.translate(diff);
		cull_xform.mulA_44(adjust);

		RainLight.X.D.minX = 0;
		RainLight.X.D.maxX = limit;
		RainLight.X.D.minY = 0;
		RainLight.X.D.maxY = limit;
	}

	RainTransform = cull_xform;
	RainOrigin = cull_COP;
	RainLight.X.D.combine = cull_xform;
	return true;
}

void CRender::render_rain(GfxCmdList& Cmd)
{
	// Editors and frames the pre-render stage could not foresee collect on this thread
	const bool EditorMode = Device.IsEditorMode();
	if (EditorMode || RainCollectFrame != Device.dwFrame)
	{
		RainCollectFrame = Device.dwFrame;
		RainActive = prepare_rain_collect();
		if (RainActive && !EditorMode && pOutdoorSector)
		{
			GraphRain.Build(MakeRainView());
		}
	}
	else
	{
		WaitGraphJobs(RainJobsEnd);
	}

	if (!RainActive)
	{
		return;
	}

	GPU_EVENT(render_rain);
	light& RainLight = *Lights.rain_light;

	HOM.Disable();
	phase = PHASE_SMAP;

	R_dsgraph_structure& Graph = EditorMode ? BuildPassGraph(Cmd, MakeRainView()) : GraphRain;

	// Render shadow-map
	//. !!! We should clip based on shrinked frustum (again)

	bool bNormal = Graph.HasPasses(0);
	bool bSpecial = Graph.HasPasses(1) || Graph.mapSorted.size();

	if (bNormal || bSpecial || Graph.SkipDepthBatched)
	{
		Target->phase_smap_direct(Cmd, &RainLight, SE_SUN_RAIN_SMAP);
		if (Graph.SkipDepthBatched)
		{
			const RHIViewport Area = { float(RainLight.X.D.minX), float(RainLight.X.D.minY), float(RainLight.X.D.maxX - RainLight.X.D.minX), float(RainLight.X.D.maxY - RainLight.X.D.minY), 0.0f, 1.0f };
			GpuScene::DepthDraw Draw = { Target->rt_smap_depth->pZRT, 0, &Area };
			Draw.Look = RainLight.direction;
			Scene.DrawDepth(Cmd, Graph, Draw);
		}
		Cmd.SetObjectTransform(Fidentity);
		Cmd.SetView(Fidentity, RainLight.X.D.combine);
		Graph.r_dsgraph_render_graph(0);
	}

	// Restore XForms
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetView(Device.mView, Device.mProject);

	// Accumulate
	Target->phase_rain(Cmd);
	Target->draw_rain(Cmd, RainLight);
}