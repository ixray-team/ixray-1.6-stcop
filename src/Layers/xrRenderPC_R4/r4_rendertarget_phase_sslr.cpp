#include "stdafx.h"

#include "r4_rendertarget.h"

void CRenderTarget::phase_sslr()
{
	GPU_EVENT(phase_sslr);

	//groups
	const UINT tgroupsX = (RCache.get_width() + 7u) / 8u;
	const UINT tgroupsY = (RCache.get_height() + 7u) / 8u;

	{
		GPU_EVENT(sslr_render);

		//Dummy
		ID3D11UnorderedAccessView* uav_dummy[2] = { nullptr, nullptr };
		ID3D11ShaderResourceView* srv_dummy[16] = {};

		//Shader setup... can't use set_element because of set_PS bullshit
	    ShaderElement* S;
        S = (&*(s_sslr->E[0]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);

		//Bind UAVs
		UINT UAVInitialCounts = 1;

		ID3D11UnorderedAccessView* our_uav[2] = {
            reinterpret_cast<ID3D11UnorderedAccessView*>(rt_sslr->pUAView->GetRaw()),
            reinterpret_cast<ID3D11UnorderedAccessView*>(rt_sslr_data->pUAView->GetRaw())
		};

		RContext->CSSetUnorderedAccessViews(0, 2, our_uav, &UAVInitialCounts);

		//Dispatch
		RCache.Compute(tgroupsX, tgroupsY, 1);

		//Unbind
		RContext->CSSetUnorderedAccessViews(0, 2, uav_dummy, &UAVInitialCounts);
		RContext->CSSetShaderResources(0, 16, srv_dummy);
	}


	{
		GPU_EVENT(sslr_filter);

		ID3D11UnorderedAccessView* uav_dummy = nullptr;
		ID3D11ShaderResourceView* srv_dummy[16] = {};

	    ShaderElement* S;
        S = (&*(s_sslr->E[1]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);

		UINT UAVInitialCounts = 1;

		ID3D11UnorderedAccessView* our_uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_sslr_temp->pUAView->GetRaw());

		RContext->CSSetUnorderedAccessViews(0, 1, &our_uav, &UAVInitialCounts);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav_dummy, &UAVInitialCounts);
		RContext->CSSetShaderResources(0, 16, srv_dummy);
	}

	{
		GPU_EVENT(sslr_temporal);

		ID3D11UnorderedAccessView* uav_dummy = nullptr;
		ID3D11ShaderResourceView* srv_dummy[16] = {};

	    ShaderElement* S;
        S = (&*(s_sslr->E[2]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);

		UINT UAVInitialCounts = 1;

		ID3D11UnorderedAccessView* our_uav = reinterpret_cast<ID3D11UnorderedAccessView*>(rt_sslr->pUAView->GetRaw());

		RContext->CSSetUnorderedAccessViews(0, 1, &our_uav, &UAVInitialCounts);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		RContext->CSSetUnorderedAccessViews(0, 1, &uav_dummy, &UAVInitialCounts);
		RContext->CSSetShaderResources(0, 16, srv_dummy);

		//LVutner: Meh.
		GRHI->CopySurface(rt_sslr_old->pSurface, rt_sslr->pSurface);
	}

}

bool is_render_cubemap = false;

void CRender::render_reflections()
{
	if (RImplementation.pLastSector)
	{
		Device.Statistic->TEST2.Begin();

		GPU_EVENT(FORWARD_REFLECTIONS);

		extern float g_fSCREEN;

		extern float r_ssaDISCARD;
		extern float r_ssaDONTSORT;
		extern float r_ssaLOD_A;
		extern float r_ssaLOD_B;
		extern float r_ssaHZBvsTEX;
		extern float r_ssaGLOD_start, r_ssaGLOD_end;

		auto saved_g_fSCREEN = g_fSCREEN;
		auto saved_r_ssaDISCARD = r_ssaDISCARD;
		auto saved_r_ssaDONTSORT = r_ssaDONTSORT;
		auto saved_r_ssaLOD_A = r_ssaLOD_A;
		auto saved_r_ssaLOD_B = r_ssaLOD_B;
		auto saved_r_ssaGLOD_start = r_ssaGLOD_start;
		auto saved_r_ssaGLOD_end = r_ssaGLOD_end;
		auto saved_r_ssaHZBvsTEX = r_ssaHZBvsTEX;

		u32 dwSize = Target->rt_Reflection->dwSize;

		float fov_factor = _sqr(90.f / Device.fFOV);
		g_fSCREEN = _sqr((float)dwSize) * fov_factor * (EPS_S + ps_r__LOD);

		r_ssaDISCARD = _sqr(ps_r__ssaDISCARD) / g_fSCREEN;
		r_ssaDONTSORT = _sqr(ps_r__ssaDONTSORT / 3) / g_fSCREEN;

		r_ssaLOD_A = _sqr(ps_r2_ssaLOD_A / 3) / g_fSCREEN;
		r_ssaLOD_B = _sqr(ps_r2_ssaLOD_B / 3) / g_fSCREEN;

		r_ssaGLOD_start = _sqr(ps_r__GLOD_ssa_start / 3) / g_fSCREEN;
		r_ssaGLOD_end = _sqr(ps_r__GLOD_ssa_end / 3) / g_fSCREEN;
		r_ssaHZBvsTEX = _sqr(ps_r__ssaHZBvsTEX / 3) / g_fSCREEN;

		static Fmatrix EnvProject;

		static Fmatrix EnvView;
		static Fmatrix EnvFullTransform;

		static Fvector cmNorm[6];
		static Fvector cmDir[6];

		cmDir[2].mul(Device.vCameraTop, +1.0f);
		cmDir[3].mul(Device.vCameraTop, -1.0f);

		cmNorm[2].mul(Device.vCameraDirection, -1.0f);
		cmNorm[3].mul(Device.vCameraDirection, +1.0f);

		cmDir[0].mul(Device.vCameraRight, +1.0f);
		cmDir[1].mul(Device.vCameraRight, -1.0f);

		cmNorm[0].mul(Device.vCameraTop, +1.0f);
		cmNorm[1].mul(Device.vCameraTop, +1.0f);

		cmDir[4].mul(Device.vCameraDirection, +1.0f);
		cmDir[5].mul(Device.vCameraDirection, -1.0f);

		cmNorm[4].mul(Device.vCameraTop, +1.0f);
		cmNorm[5].mul(Device.vCameraTop, +1.0f);

		CEnvDescriptorMixer* CurrentEnv = g_pGamePersistent->Environment().CurrentEnv;

		EnvProject.build_projection
		(
			PI_DIV_2, 1.0f,
			Device.fViewportNear,
			CurrentEnv->far_plane * 0.4f
		);

		RCache.set_xform_project(EnvProject);

		Fvector4 fog_color4 = 
		{
			CurrentEnv->fog_far,
			0.0f, 0.0f, 0.0f,
		};

		is_render_cubemap = true;

		for (u32 i = 0; i < 6; ++i)
		{
			GPU_EVENT(FORWARD_REFLECTION_SIDE);

			phase = PHASE_REFLECT;

			r_pmask(true, false, true);
			mapWmark.clear();

			EnvView.build_camera_dir(Device.vCameraPosition, cmDir[i], cmNorm[i]);
			EnvFullTransform.mul(EnvProject, EnvView);

			r_dsgraph_render_subspace(pLastSector, EnvFullTransform, Device.vCameraPosition, false, false);

			GRHI->ClearTarget(Target->rt_Reflection_temp->pRT[i], (const float*)&fog_color4);
			GRHI->ClearDepthStencil(Target->rt_Depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);

			Target->u_setrt(dwSize, dwSize, Target->rt_Reflection->pRT[i], Target->rt_Reflection_temp->pRT[i], NULL, Target->rt_Depth->pZRT);
			RImplementation.rmNormal();

			RCache.set_Stencil(FALSE);
			RCache.set_ColorWriteEnable();

			RCache.set_xform_view(EnvView);

			r_dsgraph_render_graph(0);
		}

		RCache.set_xform_project(Device.mProject);
		RCache.set_xform_view(Device.mView);

		is_render_cubemap = false;
		phase = PHASE_NORMAL;

		g_fSCREEN = saved_g_fSCREEN;
		r_ssaDISCARD = saved_r_ssaDISCARD;
		r_ssaDONTSORT = saved_r_ssaDONTSORT;
		r_ssaLOD_A = saved_r_ssaLOD_A;
		r_ssaLOD_B = saved_r_ssaLOD_B;
		r_ssaGLOD_start = saved_r_ssaGLOD_start;
		r_ssaGLOD_end = saved_r_ssaGLOD_end;
		r_ssaHZBvsTEX = saved_r_ssaHZBvsTEX;

		GPU_EVENT(FORWARD_REFLECTION_FORWARD);

		Target->u_setrt(Target->rt_Reflection_forward, nullptr, nullptr);
		RImplementation.rmNormal();

		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE);

		RCache.set_Element(Target->s_sslr->E[3]);
		RCache.set_Geometry(Target->FSTriangleGeom);

		RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);

		Target->u_setrt(dwSize, dwSize, nullptr, nullptr, nullptr, nullptr);
		GRHI->GenerateMips(Target->rt_Reflection_forward->pTexture->get_SRView());

		Device.Statistic->TEST2.End();
	}
}
