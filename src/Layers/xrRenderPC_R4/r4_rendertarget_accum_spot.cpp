#include "stdafx.h"
#include "du_cone.h"
#include "CHudInitializer.h"

Fvector3 ps_ssfx_volumetric = { .0f, 1.0f, 3.0f }; // Force Volumetric, Vol Intensity, Vol Quality

void CRenderTarget::accum_spot(GfxCmdList& Cmd, light* L)
{
	if (L == nullptr)
	{
		return;
	}

	PROF_EVENT("CRenderTarget::accum_spot")
	phase_accumulator(Cmd);
	RImplementation.stats.l_visible++;

	// *** assume accumulator already setup ***
	// *****************************	Mask by stencil		*************************************
	ref_shader		shader;
	if(IRender_Light::OMNIPART == L->flags.type)
	{
		shader = L->s_point;
		if(!shader) {
			shader = s_accum_point;
		}
	}
	else 
	{
		shader = L->s_spot;
		if(!shader) {
			shader = s_accum_spot;
		}
	}

	CHudInitializer initalizer(Cmd, false, true);

	if(L->flags.bHudMode) 
	{
		initalizer.SetHudMode();
	}

	bool	bIntersect = false; //enable_scissor(L);
	{
		// setup xform
		Cmd.SetObjectTransform(L->m_xform);
		Cmd.SetView(Device.mView, Device.mProject);
		bIntersect = enable_scissor(L);
		enable_dbt_bounds(L);

		// *** similar to "Carmack's reverse", but assumes convex, non intersecting objects,
		// *** thus can cope without stencil clear with 127 lights
		// *** in practice, 'cause we "clear" it back to 0x1 it usually allows us to > 200 lights :)
		Cmd.SetPass(s_accum_mask->E[SE_MASK_SPOT]);		// masker

		// backfaces: if (stencil>=1 && zfail)			stencil = light_id
		Cmd.SetCullMode(ERHI_CULLMODE::FRONT);
		Cmd.SetStencil(true, D3DCMP_LESSEQUAL, dwLightMarkerID, 0x01, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE);
		draw_volume(Cmd, L);

		// frontfaces: if (stencil>=light_id && zfail)	stencil = 0x1
		Cmd.SetCullMode(ERHI_CULLMODE::BACK);
		Cmd.SetStencil(true, D3DCMP_LESSEQUAL, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE);
		draw_volume(Cmd, L);
	}

	// nv-stencil recompression
	if(RImplementation.o.nvstencil)	u_stencil_optimize(Cmd);

	// *****************************	Minimize overdraw	*************************************
	// Select shader (front or back-faces), *** back, if intersect near plane
	Cmd.SetColorWriteEnable();
	Cmd.SetCullMode(ERHI_CULLMODE::FRONT);

	// 2D texgens 
	Fmatrix			m_Texgen;			u_compute_texgen_screen(Cmd, m_Texgen);

	// Shadow xform (+texture adjustment matrix)
	Fmatrix			m_Shadow, m_Lmap;
	{
		float			smapsize = float(RImplementation.o.smapsize);
		float			fTexelOffs = (.5f / smapsize);
		float			view_dim = float(L->X.S.size - 2) / smapsize;
		float			view_sx = float(L->X.S.posX + 1) / smapsize;
		float			view_sy = float(L->X.S.posY + 1) / smapsize;
		float			fRange = float(1.f) * ps_r2_ls_depth_scale;
		float			fBias = ps_r2_ls_depth_bias;
		Fmatrix			m_TexelAdjust = {
			view_dim / 2.f,							0.0f,									0.0f,		0.0f,
			0.0f,									-view_dim / 2.f,							0.0f,		0.0f,
			0.0f,									0.0f,									fRange,		0.0f,
			view_dim / 2.f + view_sx + fTexelOffs,	view_dim / 2.f + view_sy + fTexelOffs,	fBias,		1.0f
		};

		// compute xforms
		Fmatrix			xf_world;		xf_world.invert(Device.mView);
		Fmatrix			xf_view = L->X.S.view;
		Fmatrix			xf_project;		xf_project.mul(m_TexelAdjust, L->X.S.project);
		m_Shadow.mul(xf_view, xf_world);
		m_Shadow.mulA_44(xf_project);

		// lmap
		view_dim = 1.f;
		view_sx = 0.f;
		view_sy = 0.f;
		Fmatrix			m_TexelAdjust2 = {
			view_dim / 2.f,							0.0f,									0.0f,		0.0f,
			0.0f,									-view_dim / 2.f,							0.0f,		0.0f,
			0.0f,									0.0f,									fRange,		0.0f,
			view_dim / 2.f + view_sx + fTexelOffs,	view_dim / 2.f + view_sy + fTexelOffs,	fBias,		1.0f
		};

		// compute xforms
		xf_project.mul(m_TexelAdjust2, L->X.S.project);
		m_Lmap.mul(xf_view, xf_world);
		m_Lmap.mulA_44(xf_project);
	}

	// Common constants
	Fvector		L_dir, L_clr, L_pos;	float L_spec;
	L_clr.set(L->color.r, L->color.g, L->color.b);
	L_clr.mul(L->get_LOD());
	L_spec = u_diffuse2s(L_clr);
	Device.mView.transform_tiny(L_pos, L->position);
	Device.mView.transform_dir(L_dir, L->direction);
	L_dir.normalize();

	// Draw volume with projective texgen
	{
		// Select shader
		u32 _id = 0;

		if(L->flags.bShadow) 
		{
			bool bFullSize = (L->X.S.size == RImplementation.o.smapsize);
			if (L->X.S.transluent)	_id = SE_L_TRANSLUENT;
			else if (bFullSize)	_id = SE_L_FULLSIZE;
			else _id = SE_L_NORMAL;
		}
		else
		{
			_id = SE_L_UNSHADOWED;
			m_Shadow = m_Lmap;
		}

		Cmd.SetPass(shader->E[_id]);
		Cmd.SetCullMode(ERHI_CULLMODE::FRONT);

		// Constants
		//float	att_R = L->range * .95f;
		float	att_R = L->range;
		float	att_factor = 1.f / (att_R * att_R);
		Cmd.Constants.Write<CBLight>().Ldynamic_pos.set(L_pos.x, L_pos.y, L_pos.z, att_factor);
		Cmd.Constants.Write<CBLight>().Ldynamic_color.set(L_clr.x, L_clr.y, L_clr.z, L_spec);
		Cmd.Constants.Write<CBPass>().m_texgen = m_Texgen;
		Cmd.Constants.Write<CBLight>().m_shadow = m_Shadow;
		Cmd.Constants.Write<CBLight>().Ldynamic_hud = (int)L->flags.bHudMode;
		Cmd.Constants.Write<CBMaterial>().m_lmap[0].set(m_Lmap._11, m_Lmap._21, m_Lmap._31, m_Lmap._41);
		Cmd.Constants.Write<CBMaterial>().m_lmap[1].set(m_Lmap._12, m_Lmap._22, m_Lmap._32, m_Lmap._42);

		Cmd.SetStencil(true, D3DCMP_LESSEQUAL, dwLightMarkerID, 0xff, 0x00);
		draw_volume(Cmd, L);
	}

	Cmd.SetScissor(0);
	increment_light_marker(Cmd);

	u_DBT_disable();

	if(L->flags.bHudMode)
	{
		initalizer.SetDefaultMode();
	}
}

void CRenderTarget::accum_volumetric(GfxCmdList& Cmd, light* L)
{
	if (L == nullptr)
	{
		return;
	}

	if(!L->flags.bVolumetric) return;
	PROF_EVENT("CRenderTarget::accum_volumetric")
	phase_vol_accumulator(Cmd);

	ref_shader			shader;

	shader = L->s_volumetric;
	if(!shader) {
		shader = s_accum_volume;
	}

	// *** assume accumulator setted up ***
	// *****************************	Mask by stencil		*************************************
	bool	bIntersect = false; //enable_scissor(L);
	{
		// setup xform
		Cmd.SetObjectTransform(L->m_xform);
		Cmd.SetView(Device.mView, Device.mProject);
		bIntersect = enable_scissor(L);
	}

	Cmd.SetColorWriteEnable();
	Cmd.SetCullMode(ERHI_CULLMODE::NONE);		// back

	// 2D texgens 
	Fmatrix			m_Texgen;			u_compute_texgen_screen(Cmd, m_Texgen);

	// Shadow xform (+texture adjustment matrix)
	Fmatrix			m_Shadow, m_Lmap;
	Fmatrix			mFrustumSrc;
	CFrustum		ClipFrustum;
	{
		float			smapsize = float(RImplementation.o.smapsize);
		float			fTexelOffs = (.5f / smapsize);
		float			view_dim = float(L->X.S.size - 2) / smapsize;
		float			view_sx = float(L->X.S.posX + 1) / smapsize;
		float			view_sy = float(L->X.S.posY + 1) / smapsize;
		float			fRange = float(1.f) * ps_r2_ls_depth_scale;
		float			fBias = ps_r2_ls_depth_bias;
		Fmatrix			m_TexelAdjust = {
			view_dim / 2.f,							0.0f,									0.0f,		0.0f,
			0.0f,									-view_dim / 2.f,							0.0f,		0.0f,
			0.0f,									0.0f,									fRange,		0.0f,
			view_dim / 2.f + view_sx + fTexelOffs,	view_dim / 2.f + view_sy + fTexelOffs,	fBias,		1.0f
		};

		// compute xforms
		Fmatrix			xf_world;		xf_world.invert(Device.mView);
		Fmatrix			xf_view = L->X.S.view;
		Fmatrix			xf_project;		xf_project.mul(m_TexelAdjust, L->X.S.project);
		m_Shadow.mul(xf_view, xf_world);
		m_Shadow.mulA_44(xf_project);

		// lmap
		view_dim = 1.f;
		view_sx = 0.f;
		view_sy = 0.f;
		Fmatrix			m_TexelAdjust2 = {
			view_dim / 2.f,							0.0f,									0.0f,		0.0f,
			0.0f,									-view_dim / 2.f,							0.0f,		0.0f,
			0.0f,									0.0f,									fRange,		0.0f,
			view_dim / 2.f + view_sx + fTexelOffs,	view_dim / 2.f + view_sy + fTexelOffs,	fBias,		1.0f
		};

		// compute xforms
		xf_project.mul(m_TexelAdjust2, L->X.S.project);
		m_Lmap.mul(xf_view, xf_world);
		m_Lmap.mulA_44(xf_project);

		// Compute light frustum in world space
		mFrustumSrc.mul(L->X.S.project, xf_view);
		ClipFrustum.CreateFromMatrix(mFrustumSrc, FRUSTUM_P_ALL);
		//	Adjust frustum far plane
		//	4 - far, 5 - near
		ClipFrustum.planes[4].d -= (ClipFrustum.planes[4].d + ClipFrustum.planes[5].d) * (1 - L->m_volumetric_distance);
	}

	//	Calculate camera space AABB
	//	Adjust AABB according to the adjusted distance for the light volume
	float	ScaledRadius = L->SpatialComponent->sphere.R * L->m_volumetric_distance;
	Fvector	rr = Fvector().set(ScaledRadius, ScaledRadius, ScaledRadius);
	Fvector pt = L->SpatialComponent->sphere.P;
	pt.sub(L->position);
	pt.mul(L->m_volumetric_distance);
	pt.add(L->position);

	//	Don't adjust AABB

	Device.mView.transform(pt);
	Fbox aabb;
	aabb.setb(pt, rr);

	// Common constants
	float		fQuality = L->m_volumetric_quality;
	int			iNumSlises = (int)(VOLUMETRIC_SLICES * fQuality);
	//			min 10 surfaces
	iNumSlises = std::max(10, iNumSlises);
	//	Adjust slice intensity
	fQuality = ((float)iNumSlises) / VOLUMETRIC_SLICES;
	Fvector		L_dir, L_clr, L_pos;	float L_spec;
	L_clr.set(L->color.r, L->color.g, L->color.b);
	L_clr.mul(L->m_volumetric_intensity);
	L_clr.mul(L->m_volumetric_distance);
	L_clr.mul(1 / fQuality);
	L_clr.mul(L->get_LOD());
	L_spec = u_diffuse2s(L_clr);
	Device.mView.transform_tiny(L_pos, L->position);
	Device.mView.transform_dir(L_dir, L->direction);
	L_dir.normalize();

	// Draw volume with projective texgen
	{
		Cmd.SetPass(shader->E[0]);

		// Constants
		float	att_R = L->m_volumetric_distance * L->range;// *.95f;
		float	att_factor = 1.f / (att_R * att_R);
		Cmd.Constants.Write<CBLight>().Ldynamic_pos.set(L_pos.x, L_pos.y, L_pos.z, att_factor);
		Cmd.Constants.Write<CBLight>().Ldynamic_color.set(L_clr.x, L_clr.y, L_clr.z, L_spec);
		Cmd.Constants.Write<CBPass>().m_texgen = m_Texgen;
		Cmd.Constants.Write<CBLight>().m_shadow = m_Shadow;
		Cmd.Constants.Write<CBMaterial>().m_lmap[0].set(m_Lmap._11, m_Lmap._21, m_Lmap._31, m_Lmap._41);
		Cmd.Constants.Write<CBMaterial>().m_lmap[1].set(m_Lmap._12, m_Lmap._22, m_Lmap._32, m_Lmap._42);
		Cmd.Constants.Write<CBLight>().vMinBounds.set(aabb.x1, aabb.y1, aabb.z1);
		//	Increase camera-space aabb z size to compensate decrease of slices number
		Cmd.Constants.Write<CBLight>().vMaxBounds.set(aabb.x2, aabb.y2, aabb.z1 + (aabb.z2 - aabb.z1) / fQuality);

		//	Set up user clip planes
		{
			//	TODO: DX10: Check if it's equivalent to the previouse code.

			//	Transform frustum to clip space
			Fmatrix PlaneTransform;
			PlaneTransform.transpose(Device.mInvFullTransform);

			for(int i = 0; i < 6; ++i) {
				Fvector4& ClipPlane = *(Fvector4*)&ClipFrustum.planes[i].n.x;
				Fvector4	TransformedPlane;
				PlaneTransform.transform(TransformedPlane, ClipPlane);
				TransformedPlane.mul(-1.0f);
				Cmd.Constants.Write<CBLight>().FrustumClipPlane[i] = TransformedPlane;
			}
		}

		Cmd.SetColorWriteEnable(D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE);

		Cmd.SetGeometry(g_accum_volumetric);
		//	Igor: no need to do it per sub-sample. Plain AA will go just fine.
		Cmd.DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, (VOLUMETRIC_SLICES * 2) * 3, 0, 0);
		Cmd.SetColorWriteEnable();
	}

	Cmd.SetScissor(0);
}

void CRenderTarget::accum_volumetric_lv(GfxCmdList& Cmd, light* L)
{
	//LV: Point lights will be fixed later... I hope
	if (L == nullptr)
		return;
	GPU_EVENT(accum_volumetric_lv);

	if (!L->flags.bVolumetric || (ps_ssfx_volumetric.x > 0 && ps_ssfx_volumetric.y > 0))
	{
		return;
	}

	phase_vol_accumulator(Cmd);

	//Set XFORMs, we gonna transform our geometry in VS
	L->xform_calc();
	Cmd.SetObjectTransform(L->m_xform);
	Cmd.SetView(Device.mView, Device.mProject);

	//Shadow xform (+texture adjustment matrix)
	Fmatrix m_Shadow, m_Lmap;
	{
		float smapsize = float(RImplementation.o.smapsize);
		float fTexelOffs = (.5f / smapsize);
		float view_dim = float(L->X.S.size - 2) / smapsize;
		float view_sx = float(L->X.S.posX + 1) / smapsize;
		float view_sy = float(L->X.S.posY + 1) / smapsize;
		float fRange = float(1.f) * ps_r2_ls_depth_scale;
		float fBias = ps_r2_ls_depth_bias;

		Fmatrix m_TexelAdjust = {
			view_dim / 2.f, 0.0f, 0.0f, 0.0f,
			0.0f, -view_dim / 2.f, 0.0f, 0.0f,
			0.0f, 0.0f, fRange, 0.0f,
			view_dim / 2.f + view_sx + fTexelOffs, view_dim / 2.f + view_sy + fTexelOffs, fBias, 1.0f
		};

		//Compute xforms
		Fmatrix xf_world;
		xf_world.invert(Device.mView);
		Fmatrix xf_view = L->X.S.view;
		Fmatrix xf_project;
		xf_project.mul(m_TexelAdjust, L->X.S.project);

		m_Shadow.set(xf_view);
		m_Shadow.mulA_44(xf_project);
	}

	//Light direction (spot), color, and position. All in world space
	Fvector L_dir, L_clr, L_pos;
	L_clr.set(L->color.r, L->color.g, L->color.b);
	L_clr.mul(std::max(ps_ssfx_volumetric.y, L->m_volumetric_intensity));
	L_clr.mul(L->m_volumetric_distance);
	L_clr.mul(L->get_LOD());

	L_pos.set(L->position);
	L_dir.set(L->direction);
	L_dir.normalize();

	//Attenuation
	float att_R = L->m_volumetric_distance * L->range * .95f;
	float att_factor = 1.f / (att_R * att_R);

	//Set the shader
	Cmd.SetPass(s_combine->E[3]);
	Cmd.SetCullMode(ERHI_CULLMODE::FRONT);
	Cmd.SetStencil(false);

	Cmd.SetColorWriteEnable();
	//Set constants
	Cmd.Constants.Write<CBLight>().Ldynamic_pos.set(L_pos.x, L_pos.y, L_pos.z, att_factor);
	Cmd.Constants.Write<CBLight>().Ldynamic_dir.set(L_dir.x, L_dir.y, L_dir.z, -cos(L->cone * 0.5));
	Cmd.Constants.Write<CBLight>().Ldynamic_color.set(L_clr.x, L_clr.y, L_clr.z, L->get_LOD());

	Cmd.Constants.Write<CBLight>().m_shadow = m_Shadow;

	//Render, no IB/VB - geometry in VS
	draw_volume(Cmd, L);
}