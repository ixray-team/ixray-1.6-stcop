#include "stdafx.h"

void CRenderTarget::draw_rain(light& RainSetup)
{
	GPU_EVENT(draw_rain);

	float fRainFactor = g_pGamePersistent->Environment().CurrentEnv->rain_density;

	const UINT tgroupsX = (RCache.get_width() + 7u) / 8u;
	const UINT tgroupsY = (RCache.get_height() + 7u) / 8u;

	Fvector L_dir;
	Fvector W_dirX;
	Fvector W_dirZ;

	Device.mView.transform_dir(L_dir, RainSetup.direction);
	L_dir.normalize();

	Device.mView.transform_dir(W_dirX, Fvector().set(1.0f, 0.0f, 0.0f));
	W_dirX.normalize();

	Device.mView.transform_dir(W_dirZ, Fvector().set(0.0f, 0.0f, 1.0f));
	W_dirZ.normalize();

	const float fRainFar = ps_r3_dyn_wet_surf_far;
	{
		float fRange = 1;
		float fBias = -0.0001;

		float smapsize = float(RImplementation.o.smapsize);
		float fTexelOffs = (.5f / smapsize);

		float view_dimX = float(RainSetup.X.D.maxX - RainSetup.X.D.minX) / smapsize;
		float view_dimY = float(RainSetup.X.D.maxX - RainSetup.X.D.minX) / smapsize;

		float view_sx = float(RainSetup.X.D.minX) / smapsize;
		float view_sy = float(RainSetup.X.D.minY) / smapsize;

		Fmatrix m_TexelAdjust =
		{
			view_dimX / 2.f, 0.0f, 0.0f, 0.0f,
			0.0f, -view_dimY / 2.f, 0.0f, 0.0f,
			0.0f, 0.0f, fRange, 0.0f,
			view_dimX / 2.f + view_sx + fTexelOffs,	view_dimY / 2.f + view_sy + fTexelOffs,	fBias, 1.0f
		};

		Fmatrix xf_invview; xf_invview.invert(Device.mView);

		Fmatrix m_shadow;
		{
			Fmatrix xf_project; xf_project.mul(m_TexelAdjust, RainSetup.X.D.combine);
			m_shadow.mul(xf_project, xf_invview);
		}

		Fmatrix m_clouds_shadow;
		{
			static float w_shift = 0;
			Fmatrix m_xform;
			Fvector normal;	normal.setHP(1, 0);
			m_xform.identity();
			Fvector localnormal; m_xform.transform_dir(localnormal, normal); localnormal.normalize();
			m_clouds_shadow.mul(m_xform, xf_invview);
			m_xform.scale(1.f, 1.f, 1.f);
			m_clouds_shadow.mulA_44(m_xform);
			m_xform.translate(localnormal.mul(w_shift));
			m_clouds_shadow.mulA_44(m_xform);
		}

		ID3D11UnorderedAccessView* uav_dummy[3] = 
		{
			nullptr, 
			nullptr, 
			nullptr 
		};

		ID3D11ShaderResourceView* srv_dummy[16] = {};

		ShaderElement* S;
		S = (&*(s_rain->E[0]));
		SPass& P = *(S->passes[0]);
		RCache.set_States(P.state);
		RCache.set_Constants(P.constants);
		RCache.set_Textures(P.T);
		RCache.set_CS(P.cs);

		RCache.set_c("Ldynamic_dir", L_dir.x, L_dir.y, L_dir.z, 0);

		RCache.set_c("WorldX", W_dirX.x, W_dirX.y, W_dirX.z, 0);
		RCache.set_c("WorldZ", W_dirZ.x, W_dirZ.y, W_dirZ.z, 0);

		RCache.set_c("m_shadow", m_shadow);
		RCache.set_c("m_sunmask", m_clouds_shadow);

		static float AngleFactor = 0.0f;
		AngleFactor += Device.fTimeDelta * (0.15f + fRainFactor * 0.15f);

		AngleFactor = AngleFactor - std::floor(AngleFactor);

		RCache.set_c("RainDensity", fRainFactor, AngleFactor, 0, 0);
		RCache.set_c("RainFallof", ps_r3_dyn_wet_surf_near, ps_r3_dyn_wet_surf_far, 0, 0);

		RCache.set_c
		(
			"m_level_scale", 

			RImplementation.m_puddles_level_bound.lt.x,
			RImplementation.m_puddles_level_bound.lt.y,
			RImplementation.m_puddles_level_bound.rb.x,
			RImplementation.m_puddles_level_bound.rb.y
		);

		UINT UAVInitialCounts = 1;

		ID3D11UnorderedAccessView* our_uav[3] = 
		{
			reinterpret_cast<ID3D11UnorderedAccessView*>(rt_Color->pUAView->GetRaw()),
			reinterpret_cast<ID3D11UnorderedAccessView*>(rt_Normal->pUAView->GetRaw()),
			reinterpret_cast<ID3D11UnorderedAccessView*>(rt_Surface->pUAView->GetRaw())
		};

		RContext->CSSetUnorderedAccessViews(0, std::size(our_uav), our_uav, &UAVInitialCounts);

		//Dispatch
		RCache.Compute(tgroupsX, tgroupsY, 1);

		//Unbind
		RContext->CSSetUnorderedAccessViews(0, std::size(uav_dummy), uav_dummy, &UAVInitialCounts);
		RContext->CSSetShaderResources(0, std::size(srv_dummy), srv_dummy);
	}
}
