#include "stdafx.h"
#include "r4_rendertarget.h"

bool CRenderTarget::phase_puddles()
{
	auto& wetness_factor = g_pGamePersistent->Environment().wetness_factor;

	if(RImplementation.m_levels_puddles.empty() || wetness_factor == 0.0f)
	{
		return false;
	}

	GRHI->StateManager->SetCullMode(ERHI_CULLMODE::BACK);
	RCache.set_Shader(s_puddles);

	auto puddles = 0;

	for(CRender::PuddleBase& puddle : RImplementation.m_levels_puddles) 
	{
		if(!RImplementation.ViewBase.testSphere_dirty(puddle.m_world.c, puddle.m_radius))
		{
			continue;
		}

		RCache.set_xform_world(puddle.m_world);

		RCache.set_c("puddle_constants", g_pGamePersistent->Environment().wetness_factor * puddle.m_height);
		RCache.Render_noIA(6);

		++puddles;
	}

	return puddles > 0;
}


// Chat GPT moment
// В DX11 нет нативного clip-plane через API.
// Так как нам нужен только один клип плейн, его
// мы можем запечь как Near-plane в матрицу проекции. 

void ApplyObliqueClipPlane
(
	Fmatrix& mProject,
	const Fplane& mPlane
)
{
	Fvector Q =
	{
		(mPlane.n.x >= 0.0f ? 1.0f : -1.0f) / mProject._11,
		(mPlane.n.y >= 0.0f ? 1.0f : -1.0f) / mProject._22,
		1.0f
	};

	float Denom = 1.0f / (mPlane.n.dotproduct(Q) + mPlane.d * (1.0f - mProject._33) / mProject._43);

	mProject._13 = mPlane.n.x * Denom;
	mProject._23 = mPlane.n.y * Denom;
	mProject._33 = mPlane.n.z * Denom;

	mProject._43 = mPlane.d * Denom;
}

void CRenderTarget::phase_planar()
{
	if (RImplementation.m_levels_puddles.empty())
	{
		return;
	}

	Fmatrix ReflectProject { }, ReflectView { }, ReflectFullTransform { };
	Fvector C { }, D { }, N { }, P { };
	Fplane PlanarPlane { };

	CEnvDescriptorMixer* CurrentEnv = g_pGamePersistent->Environment().CurrentEnv;

	for (auto& planar : RImplementation.m_levels_puddles)
	{
		if (!RImplementation.ViewBase.testSphere_dirty(planar.m_world.c, planar.m_radius))
		{
			continue;
		}

		C = planar.m_world.c + planar.m_world.j * 0.01f;
		PlanarPlane.build(C, planar.m_world.j);

		P.mad(Device.vCameraPosition, PlanarPlane.n, -2.0f * PlanarPlane.classify(Device.vCameraPosition));

		D.reflect(Device.vCameraDirection, PlanarPlane.n);
		N.reflect(Device.vCameraTop, PlanarPlane.n);

		ReflectView.build_camera_dir(P, D, N);
		PlanarPlane.transform(ReflectView);

		ReflectProject.build_projection
		(
			deg2rad(Device.fFOV), Device.fASPECT,
			Device.fViewportNear,
			CurrentEnv->far_plane * 0.5f
		);

		RImplementation.phase = CRender::PHASE_REFLECT;
		RImplementation.r_pmask(true, false);

		ApplyObliqueClipPlane(ReflectProject, PlanarPlane);
		ReflectFullTransform.mul(ReflectProject, ReflectView);

		RImplementation.r_dsgraph_render_subspace(RImplementation.pLastSector, ReflectFullTransform, P, true, false);

		bool IsRender = RImplementation.mapNormalPasses[0][0].size() || RImplementation.mapMatrixPasses[0][0].size();
		IsRender |= RImplementation.mapNormalPasses[1][0].size() || RImplementation.mapMatrixPasses[1][0].size() || RImplementation.mapSorted.size();

		if (IsRender)
		{
			GRHI->ClearTarget(rt_planar_color->pRT, ERTColor::Black);
			GRHI->ClearDepthStencil(rt_planar_depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);

			u_setrt(rt_planar_color, NULL, rt_planar_depth->pZRT);
			RImplementation.rmNormal();

			RCache.set_Stencil(FALSE);
			RCache.set_ColorWriteEnable();

			ps_r_taa_jitter.x = -ps_r_taa_jitter.x;
			ps_r_taa_jitter_full.x = -ps_r_taa_jitter_full.x;

			RCache.set_xform_project(ReflectProject);
			RCache.set_xform_view(ReflectView);

			RImplementation.r_dsgraph_render_graph(0);

			RCache.set_xform_project(Device.mProject);
			RCache.set_xform_view(Device.mView);

			RImplementation.phase = CRender::PHASE_NORMAL;

			ps_r_taa_jitter.x = -ps_r_taa_jitter.x;
			ps_r_taa_jitter_full.x = -ps_r_taa_jitter_full.x;
		}

		u_setrt(rt_sslr, 0, 0, 0);
		RImplementation.rmNormal();

		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);
		RCache.set_Shader(s_puddles);

		RCache.set_xform_world(planar.m_world);
		RCache.Render_noIA(6);
	}
}
