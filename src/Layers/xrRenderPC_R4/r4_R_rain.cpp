#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IRenderable.h"
#include "../xrRender/FBasicVisual.h"

#include "../xrRender/R_sun_support.h"

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

//////////////////////////////////////////////////////////////////////////
void CRender::render_rain() {
	//return;
	float	fRainFactor = g_pGamePersistent->Environment().CurrentEnv->rain_density;
	if(fRainFactor < EPS_L)			return;

	GPU_EVENT(render_rain);

	//	Use light as placeholder for rain data.
	// нет необходимости создавать каждый кадр структуру размером почти в киллобайт на стеке.
	light& RainLight = *Lights.rain_light;

	//static const float	source_offset		= 40.f;

	static const float	source_offset = 10000.f;
	RainLight.direction.set(0.0f, -1.0f, 0.0f);
	RainLight.position.set(Device.vCameraPosition.x, Device.vCameraPosition.y + source_offset, Device.vCameraPosition.z);

	float	fBoundingSphereRadius = 0;

	// calculate view-frustum bounds in world space
	Fmatrix	ex_project{}, ex_full{};
	Fmatrix ex_full_inverse{};
	{
		//	
		const float fRainFar = ps_r3_dyn_wet_surf_far;
		ex_project.build_projection(deg2rad(Device.fFOV/* *Device.fASPECT*/), Device.fASPECT, Device.fViewportNear, fRainFar);
		ex_full.mul(ex_project, Device.mView);
		ex_full_inverse.invert44(ex_full);

		//	Calculate view frustum were we can see dynamic rain radius
		{
			//	b^2 = 2RH, B - side enge of the pyramid, h = height
			//	R = b^2/(2*H)
			const float H = fRainFar;
			const float a = tanf(deg2rad(Device.fFOV) / 2);
			const float c = tanf(deg2rad(Device.fFOV * Device.fASPECT) / 2);
			const float b_2 = H * H * (1.0f + a * a + c * c);
			fBoundingSphereRadius = b_2 / (2.0f * H);
		}
	}

	//Device.vCameraDirection

	// Compute volume(s) - something like a frustum for infinite directional light
	// Also compute virtual light position and sector it is inside
	xr_vector<Fplane>			cull_planes;
	{
		// Lets begin from base frustum
		Fmatrix		fullxform_inv = ex_full_inverse;
#ifdef	_DEBUG
		typedef		DumbConvexVolume<true>	t_volume;
#else
		typedef		DumbConvexVolume<false>	t_volume;
#endif
		static t_volume hull;
		hull.polys.clear();
		hull.edges.clear();
		hull.points.clear();
		{
			for(int p = 0; p < 8; p++)
				hull.points.push_back(wform(fullxform_inv, corners[p]));
			for(int plane = 0; plane < 6; plane++)
			{
				hull.polys.push_back(t_volume::_poly());
				for(int pt = 0; pt < 4; pt++)
					hull.polys.back().points.push_back(facetable[plane][pt]);
			}
		}
		//hull.compute_caster_model	(cull_planes,fuckingsun->direction);
		hull.compute_caster_model(cull_planes, RainLight.direction);
#ifdef	_DEBUG
		for(u32 it = 0; it < cull_planes.size(); it++)
			Target->dbg_addplane(cull_planes[it], 0xffffffff);
#endif

		// COP - 100 km away
		rainwet_cull_COP.mad(Device.vCameraPosition, RainLight.direction, -tweak_rain_COP_initial_offs);
		rainwet_cull_COP.x += fBoundingSphereRadius * Device.vCameraDirection.x;
		rainwet_cull_COP.z += fBoundingSphereRadius * Device.vCameraDirection.z;

		// Create frustum for query
		rainwet_cull_frustum._clear();
		for(u32 p = 0; p < cull_planes.size(); p++)
			rainwet_cull_frustum._add(cull_planes[p]);


		// Create approximate ortho-xform
		// view: auto find 'up' and 'right' vectors
		Fmatrix						mdir_View, mdir_Project;
		Fvector						L_dir, L_up, L_right, L_pos;
		L_pos.set(RainLight.position);
		L_dir.set(RainLight.direction).normalize();
		L_right.set(1, 0, 0);					if(_abs(L_right.dotproduct(L_dir)) > .99f)	L_right.set(0, 0, 1);
		L_up.crossproduct(L_dir, L_right).normalize();
		L_right.crossproduct(L_up, L_dir).normalize();
		mdir_View.build_camera_dir(L_pos, L_dir, L_up);

		// projection: box
		//	Simple
		Fbox	frustum_bb;			frustum_bb.invalidate();
		for(int it = 0; it < 8; it++) {
			//for (int it=0; it<9; it++)	{
			Fvector	xf = wform(mdir_View, hull.points[it]);
			frustum_bb.modify(xf);
		}
		Fbox& bb = frustum_bb;
		bb.grow(EPS);

		//	HACK
		//	TODO: DX10: Calculate bounding sphere for view frustum
		//	TODO: DX10: Reduce resolution.
		//bb.min.x = -50;
		//bb.max.x = 50;
		//bb.min.y = -50;
		//bb.max.y = 50;

		//	Offset RainLight position to center rain shadowmap
		Fvector3	vRectOffset;
		vRectOffset.set(fBoundingSphereRadius * Device.vCameraDirection.x, 0, fBoundingSphereRadius * Device.vCameraDirection.z);
		bb.min.x = -fBoundingSphereRadius + vRectOffset.x;
		bb.max.x = fBoundingSphereRadius + vRectOffset.x;
		bb.min.y = -fBoundingSphereRadius + vRectOffset.z;
		bb.max.y = fBoundingSphereRadius + vRectOffset.z;

		mdir_Project.OrthographicOffCenterLH(bb.min.x, bb.max.x, bb.min.y, bb.max.y,
			bb.min.z - tweak_rain_ortho_xform_initial_offs,
			bb.min.z + 2 * tweak_rain_ortho_xform_initial_offs);
		rainwet_cull_xform.mul(mdir_Project, mdir_View);

		s32		limit = _min(o.smapsize, ps_r3_dyn_wet_surf_sm_res);

		// build viewport xform
		float	view_dim = float(limit);
		float	fTexelOffs = (.5f / o.smapsize);
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
		Fvector cam_proj = wform(rainwet_cull_xform, Fvector().set(0, 0, 0));
		Fvector	cam_pixel = wform(m_viewport, cam_proj);
		cam_pixel.x = floorf(cam_pixel.x);
		cam_pixel.y = floorf(cam_pixel.y);
		Fvector cam_snapped = wform(m_viewport_inv, cam_pixel);
		Fvector diff;		diff.sub(cam_snapped, cam_proj);
		Fmatrix adjust;		adjust.translate(diff);
		rainwet_cull_xform.mulA_44(adjust);

		RainLight.X.D.minX = 0;
		RainLight.X.D.maxX = limit;
		RainLight.X.D.minY = 0;
		RainLight.X.D.maxY = limit;
	}

	// Begin SMAP-render
	phase = PHASE_SMAP;
	GMRainWet.traverse(pOutdoorSector, rainwet_cull_frustum, rainwet_cull_COP, rainwet_cull_xform);
	GMRainWet.r_dsgraph_capture_static();

	// Finalize & Cleanup
	RainLight.X.D.combine = rainwet_cull_xform;

	// Render shadow-map
	//. !!! We should clip based on shrinked frustum (again)
	{
		bool bDeffered_Shadows = GMRainWet.RGraph.mapStaticPasses[0][0].size() || GMRainWet.RGraph.mapDynamicPasses[0][0].size();
		bool bForward_Shadows = GMRainWet.RGraph.mapStaticPasses[1][0].size() || GMRainWet.RGraph.mapDynamicPasses[1][0].size() || GMRainWet.RGraph.mapStaticSorted.Sorted.size() || GMRainWet.RGraph.mapDynamicSorted.Sorted.size();
		if(bDeffered_Shadows || bForward_Shadows)
		{
			Target->phase_smap_direct(&RainLight, SE_SUN_RAIN_SMAP);
			RCache.set_xform_world(Fidentity);
			RCache.set_xform_view(Fidentity);
			RCache.set_xform_project(RainLight.X.D.combine);
			GMRainWet.r_dsgraph_render_graph(0);
		}
	}

	// End SMAP-render
	{
		//		fuckingsun->svis.end					();
	}

	// Restore XForms
	RCache.set_xform_world(Fidentity);
	RCache.set_xform_view(Device.mView);
	RCache.set_xform_project(Device.mProject);

	// Accumulate
	Target->phase_rain();
	Target->draw_rain(RainLight);
}