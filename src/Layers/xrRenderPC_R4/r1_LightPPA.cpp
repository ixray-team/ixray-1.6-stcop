

#include "stdafx.h"
#include "r1_LightPPA.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "FBasicVisual.h"
#include "../../xrEngine/CustomHUD.h"

const u32	MAX_POLYGONS			=	1024*8;
const float MAX_DISTANCE			=	50.f;
const float	SSM_near_plane			=	.1f;
const float	SSM_tex_size 			=	32.f;

void CLightR_Manager::render_point	(u32 _priority)
{
	
	Fvector		lc_COP		= Device.vCameraPosition	;
	float		lc_limit	= ps_r1_dlights_clip		;
	for (xr_vector<light*>::iterator it = selected_point.begin(); it != selected_point.end(); it++)
	{
		light* L = *it;
		if (L->SpatialComponent->sector == nullptr && _valid(L->range))
		{
			continue;
		}

		float lc_dist = lc_COP.distance_to(L->SpatialComponent->sphere.P) - L->SpatialComponent->sphere.R;
		float lc_scale = 1 - lc_dist / lc_limit;
		if (lc_scale < EPS)
		{
			continue;
		}
		if (L->range < 0.01f)
		{
			continue;
		}

		Fvector L_dir, L_up, L_right, L_pos;
		Fmatrix L_view, L_project, L_combine;
		L_dir.set(0, -1, 0);
		L_up.set(0, 0, 1);
		L_right.crossproduct(L_up, L_dir);
		L_right.normalize();
		L_up.crossproduct(L_dir, L_right);
		L_up.normalize();
		float _camrange = 300.f;
		L_pos.set(L->position);

		L_view.build_camera_dir(L_pos, L_dir, L_up);
		L_project.build_projection(deg2rad(2.f), 1.f, _camrange - L->range, _camrange + L->range);
		L_combine.mul(L_project, L_view);

		float fTexelOffs = (.5f / SSM_tex_size);
		float fRange = 1.f / L->range;
		float fBias = 0.f;
		Fmatrix m_TexelAdjust =
			{
				0.5f, 0.0f, 0.0f, 0.0f, 0.0f, -0.5f, 0.0f, 0.0f, 0.0f, 0.0f, fRange, 0.0f, 0.5f + fTexelOffs, 0.5f + fTexelOffs, fBias, 1.0f
			};
		Fmatrix L_texgen;
		L_texgen.mul(m_TexelAdjust, L_combine);

		RCache.set_c("L_dynamic_pos", L->position.x, L->position.y, L->position.z, 0.5f / L->range);
		RCache.set_c("L_dynamic_color", L->color.r * clampr(lc_scale, 0.f, 1.f), L->color.g * clampr(lc_scale, 0.f, 1.f), L->color.b * clampr(lc_scale, 0.f, 1.f), 1.f);
		RCache.set_c("L_dynamic_xform", L_texgen);

		VERIFY(L->SpatialComponent->sector);
		if (_priority == 1)
		{
			RImplementation.GraphMain.r_pmask(false, true);
		}

		static const Fvector axes[6] = {{1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0, 0, -1}};
		Fplane box[6];
		for (u32 i = 0; i < 6; ++i)
		{
			box[i].build(Fvector().mad(L->position, axes[i], L->range), axes[i]);
		}
		CFrustum F;
		F.CreateFromPlanes(box, 6);

		RImplementation.GraphMain.r_dsgraph_render_subspace(L->SpatialComponent->sector, &F, L_combine, L_pos, true, true);

		if (_priority == 1)
		{
			RImplementation.GraphMain.r_pmask(true, true);
		}

		bool bHUD = F.testSphere_dirty(Device.vCameraPosition, 2.f);

		RCache.set_Constants((R_constant_table*)0);
		if (bHUD && _priority == 0)
		{
			g_hud->Render_Last();
		}
		RImplementation.GraphMain.r_dsgraph_render_graph(_priority);
		if (bHUD && _priority == 0)
		{
			RImplementation.GraphMain.r_dsgraph_render_hud();
		}
	}
}

void CLightR_Manager::render_spot	(u32 _priority)
{

	Fvector		lc_COP		= Device.vCameraPosition	;
	float		lc_limit	= ps_r1_dlights_clip		;

	for (xr_vector<light*>::iterator it = selected_spot.begin(); it != selected_spot.end(); it++)
	{
		light* L = *it;
		if (L->SpatialComponent->sector == nullptr)
		{
			continue;
		}
		
		float	lc_dist = lc_COP.distance_to(L->SpatialComponent->sphere.P) - L->SpatialComponent->sphere.R;
		float	lc_scale = 1 - lc_dist / lc_limit;
		if (lc_scale < EPS)		continue;

		Fvector						L_dir, L_up, L_right, L_pos;
		Fmatrix						L_view, L_project, L_combine;
		L_dir.set(L->direction);			L_dir.normalize();
		L_up.set(0, 1, 0);				if (std::abs(L_up.dotproduct(L_dir)) > .99f)	L_up.set(0, 0, 1);
		L_right.crossproduct(L_up, L_dir);			L_right.normalize();
		L_up.crossproduct(L_dir, L_right);		L_up.normalize();
		L_pos.set(L->position);
		L_view.build_camera_dir(L_pos, L_dir, L_up);
		L_project.build_projection(L->cone, 1.f, SSM_near_plane, L->range + EPS_S);
		L_combine.mul(L_project, L_view);

		float			fTexelOffs = (.5f / SSM_tex_size);
		float			fRange = 1.f / L->range;
		float			fBias = 0.f;
		Fmatrix			m_TexelAdjust =
		{
			0.5f,				0.0f,				0.0f,			0.0f,
			0.0f,				-0.5f,				0.0f,			0.0f,
			0.0f,				0.0f,				fRange,			0.0f,
			0.5f + fTexelOffs,	0.5f + fTexelOffs,	fBias,			1.0f
		};
		Fmatrix		L_texgen;		L_texgen.mul(m_TexelAdjust, L_combine);

		RCache.set_c("L_dynamic_pos", L->position.x, L->position.y, L->position.z, 1.f / L->range);
		RCache.set_c("L_dynamic_color", L->color.r * clampr(lc_scale, 0.f, 1.f), L->color.g * clampr(lc_scale, 0.f, 1.f), L->color.b * clampr(lc_scale, 0.f, 1.f), 1.f);
		RCache.set_c("L_dynamic_xform", L_texgen);

		VERIFY(L->SpatialComponent->sector);
		
		if (_priority == 1)
		{
			RImplementation.GraphMain.r_pmask(false, true);
		}

		RImplementation.GraphMain.r_dsgraph_render_subspace(
			L->SpatialComponent->sector,
			L_combine,
			L_pos,
			true,
			true			
		);

		if (_priority == 1)
		{
			RImplementation.GraphMain.r_pmask(true, true);
		}

		bool bHUD = false;
		CFrustum F;
		F.CreateFromMatrix(L_combine, FRUSTUM_P_ALL);
		bHUD = F.testSphere_dirty(Device.vCameraPosition, 2.f);

		RCache.set_Constants(nullptr);
		if (bHUD && _priority == 0)
			g_hud->Render_Last();

		RImplementation.GraphMain.r_dsgraph_render_graph(_priority);

		if (bHUD && _priority == 0)
		{
			RImplementation.GraphMain.r_dsgraph_render_hud();
		}
	}
}

void CLightR_Manager::render		(u32 _priority)
{
	if (selected_spot.size())		{ 
		RImplementation.phase		= CRender::PHASE_SPOT;
		render_spot			(_priority);	

		if(_priority == 1)
			selected_spot.clear	();
	}
	if (selected_point.size())		{ 
		RImplementation.phase		= CRender::PHASE_POINT;
		render_point		(_priority);	
		
		if(_priority == 1)
			selected_point.clear();
	}
}

void CLightR_Manager::add(light* L)
{
	if (L->range < 0.1f)
		return;

	if (L->SpatialComponent->sector == nullptr)
		return;
	if (IRender_Light::POINT == L->flags.type)
		selected_point.push_back(L);
	else if (IRender_Light::SPOT == L->flags.type)
		selected_spot.push_back(L);
}

CLightR_Manager::CLightR_Manager	()
{
}

CLightR_Manager::~CLightR_Manager	()
{
}
