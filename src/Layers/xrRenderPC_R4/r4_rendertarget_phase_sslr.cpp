#include "stdafx.h"
#include "r4_rendertarget.h"
#include "ShaderBindings.h"

// Level 0 of the SSLR min-Z pyramid and the GTAO view-z pyramid, from one depth read before both
void CRenderTarget::PhaseDepthPyramid(GfxCmdList& Cmd)
{
	const bool Sslr = !RImplementation.o.dx11_use_legacy_light && RImplementation.o.deffered_reflecitons;
	const bool Gtao = ps_r_ssao_mode == 2;

	if (!Sslr && !Gtao)
	{
		return;
	}

	GPU_EVENT(depth_pyramid);

	// Writes to unbound UAVs are dropped
	IRHIUnorderedAccessView* UAVs[6] = {};

	if (Sslr)
	{
		UAVs[0] = rt_sslr_hiz->pMippedUAV[0];
	}

	for (u32 MipLevel = 0; Gtao && MipLevel < 5; ++MipLevel)
	{
		UAVs[MipLevel + 1] = rt_ao_depth->pMippedUAV[MipLevel];
	}

	SPass& P = *(s_sslr->E[3]->passes[0]);
	Cmd.SetStates(P.state);
	Cmd.SetConstantTable(P.constants);
	Cmd.SetTextures(P.T);
	Cmd.SetCS(P.cs);

	Cmd.SetComputeUAVs(0, 6, UAVs);
	Cmd.Dispatch(((u32)Cmd.GetWidth() + 15u) / 16u, ((u32)Cmd.GetHeight() + 15u) / 16u, 1);

	IRHIUnorderedAccessView* Unbind[6] = {};
	Cmd.SetComputeUAVs(0, 6, Unbind);
	Cmd.UnbindComputeTextures();
}

void CRenderTarget::phase_sslr(GfxCmdList& Cmd)
{
	if (RImplementation.o.dx11_use_legacy_light || !RImplementation.o.deffered_reflecitons)
		return;

	GPU_EVENT(phase_sslr);

	// sslr_render reads the AO target, SSAO leaves it bound as a render target
	u_setrt(Cmd, (u32)Cmd.GetWidth(), (u32)Cmd.GetHeight(), nullptr, nullptr, nullptr, nullptr);

	//groups
	const UINT tgroupsX = (Cmd.GetWidth() + 7u) / 8u;
	const UINT tgroupsY = (Cmd.GetHeight() + 7u) / 8u;

	// Min-Z pyramid for the hierarchical trace (Stachowiak, Stochastic SSR), level 0 comes from PhaseDepthPyramid
	{
		GPU_EVENT(sslr_hiz);

		IRHIUnorderedAccessView* uav_dummy[2] = { nullptr, nullptr };

		const xr_vector<IRHIUnorderedAccessView*>& MipUAVs = rt_sslr_hiz->pMippedUAV;

		ShaderElement* S = &*(s_sslr->E[4]);
		SPass& P = *(S->passes[0]);
		Cmd.SetStates(P.state);
		Cmd.SetConstantTable(P.constants);
		Cmd.SetTextures(P.T);
		Cmd.SetCS(P.cs);

		for (u32 MipLevel = 1; MipLevel < MipUAVs.size(); ++MipLevel)
		{
			const u32 MipWidth = std::max(rt_sslr_hiz->dwWidth >> MipLevel, 1u);
			const u32 MipHeight = std::max(rt_sslr_hiz->dwHeight >> MipLevel, 1u);

			IRHIUnorderedAccessView* MipUAV[2] =
			{
				MipUAVs[MipLevel - 1],
				MipUAVs[MipLevel]
			};

			Cmd.SetComputeUAVs(0, 2, MipUAV);
			Cmd.Dispatch((MipWidth + 7u) / 8u, (MipHeight + 7u) / 8u, 1);
		}

		Cmd.SetComputeUAVs(0, 2, uav_dummy);
	}

	{
		GPU_EVENT(sslr_render);

		//Dummy
		IRHIUnorderedAccessView* uav_dummy[2] = { nullptr, nullptr };

		//Shader setup... can't use set_element because of set_PS bullshit
	    ShaderElement* S;
        S = (&*(s_sslr->E[0]));
        SPass& P = *(S->passes[0]);
        Cmd.SetStates(P.state);
        Cmd.SetConstantTable(P.constants);
        Cmd.SetTextures(P.T);
        Cmd.SetCS(P.cs);

		//Bind UAVs
		IRHIUnorderedAccessView* our_uav[2] = {
            rt_sslr_temp->pUAView,
            rt_sslr_data->pUAView
		};

		Cmd.SetComputeUAVs(0, 2, our_uav);

		//Dispatch, half resolution traces one pixel of every 2x2 quad
		const u32 TraceShift = RImplementation.o.SslrHq ? 0 : 1;
		Cmd.Dispatch((((u32)Cmd.GetWidth() >> TraceShift) + 7u) / 8u, (((u32)Cmd.GetHeight() >> TraceShift) + 7u) / 8u, 1);

		//Unbind
		Cmd.SetComputeUAVs(0, 2, uav_dummy);
		Cmd.UnbindComputeTextures();
	}


	// Ray reuse and history in one pass: last frame's output is the history, the two targets swap instead of a copy
	{
		GPU_EVENT(sslr_filter);

		SslrFlip = !SslrFlip;
		const ref_rt& History = SslrFlip ? rt_sslr : rt_sslr_old;

		IRHIUnorderedAccessView* uav_dummy = nullptr;

	    ShaderElement* S;
        S = (&*(s_sslr->E[1]));
        SPass& P = *(S->passes[0]);
        Cmd.SetStates(P.state);
        Cmd.SetConstantTable(P.constants);
        Cmd.SetTextures(P.T);
        Cmd.SetCS(P.cs);
		Cmd.SetResource(ERHI_SHADER_TYPE::CS, ShaderBindings::Texture("s_sslr_history"), History->pTexture->get_SRView());

		// The output may still be combine's s_refl from two frames ago: D3D11 unbinds it for the UAV behind the resource cache
		Cmd.SetResource(ERHI_SHADER_TYPE::PS, ShaderBindings::Texture("s_refl"), nullptr);

		IRHIUnorderedAccessView* our_uav = SslrOutput()->pUAView;

		Cmd.SetComputeUAVs(0, 1, &our_uav);

		Cmd.Dispatch(tgroupsX, tgroupsY, 1);

		Cmd.SetComputeUAVs(0, 1, &uav_dummy);
		Cmd.UnbindComputeTextures();
	}
}

void CRender::begin_reflection_collect()
{
	reflection_cam_pos = Device.vCameraPosition;
	reflection_cam_dir = Device.vCameraDirection;
	reflection_cam_top = Device.vCameraTop;
	reflection_cam_right = Device.vCameraRight;
	reflection_near = Device.fViewportNear;
	reflection_sector = pLastSector;
	reflection_far = 1000.f;
	if (g_pGamePersistent && g_pGamePersistent->pEnvironment && g_pGamePersistent->pEnvironment->CurrentEnv)
		reflection_far = g_pGamePersistent->pEnvironment->CurrentEnv->far_plane;

	const u32 ticket = reflection_ticket.fetch_add(1, std::memory_order_acq_rel) + 1;
	reflection_done.store(ticket - 1, std::memory_order_release);
	reflection_done.notify_all();
}

void CRender::wait_reflection_collect()
{
	const u32 ticket = reflection_ticket.load(std::memory_order_acquire);
	u32 done = reflection_done.load(std::memory_order_acquire);
	while (done != ticket)
	{
		reflection_done.wait(done, std::memory_order_acquire);
		done = reflection_done.load(std::memory_order_acquire);
	}
}

void CRender::collect_reflections()
{
	const u32 ticket = reflection_ticket.load(std::memory_order_acquire);
	for (R_dsgraph_structure& Graph : GraphReflection)
	{
		Graph.r_dsgraph_clear_passes();
	}

	if (o.dx11_use_legacy_light || !o.offscreen_reflecitons || !Target || !Target->rt_Reflection)
	{
		reflection_done.store(ticket, std::memory_order_release);
		reflection_done.notify_all();
		return;
	}

	if (!reflection_sector)
	{
		reflection_done.store(ticket, std::memory_order_release);
		reflection_done.notify_all();
		return;
	}

	Device.Statistic->TEST2.Begin();

	const u32 dwSize = Target->rt_Reflection->dwSize;
	const float screen = _sqr((float)dwSize) * (EPS_S + ps_r__LOD);

	R_CullTLS cull;
	cull.active = true;
	cull.phase = PHASE_REFLECT;
	cull.ssa_discard = _sqr(ps_r__ssaDISCARD) / screen;
	cull.ssa_lod_a = _sqr(ps_r2_ssaLOD_A / 3) / screen;
	cull.ssa_lod_b = _sqr(ps_r2_ssaLOD_B / 3) / screen;

	Fmatrix env_project;
	env_project.build_projection(PI_DIV_2, 1.0f, reflection_near, reflection_far * 0.4f);

	Fvector cm_norm[6];
	Fvector cm_dir[6];
	cm_dir[2].mul(reflection_cam_top, +1.0f);
	cm_dir[3].mul(reflection_cam_top, -1.0f);
	cm_norm[2].mul(reflection_cam_dir, -1.0f);
	cm_norm[3].mul(reflection_cam_dir, +1.0f);
	cm_dir[0].mul(reflection_cam_right, +1.0f);
	cm_dir[1].mul(reflection_cam_right, -1.0f);
	cm_norm[0].mul(reflection_cam_top, +1.0f);
	cm_norm[1].mul(reflection_cam_top, +1.0f);
	cm_dir[4].mul(reflection_cam_dir, +1.0f);
	cm_dir[5].mul(reflection_cam_dir, -1.0f);
	cm_norm[4].mul(reflection_cam_top, +1.0f);
	cm_norm[5].mul(reflection_cam_top, +1.0f);

	const u32 sector_count = (u32)Sectors.size();
	const u32 portal_count = (u32)Portals.size();
	g_r_cull_tls = cull;

	for (u32 i = 0; i < (u32)GraphReflection.size(); ++i)
	{
		Fmatrix env_view;
		Fmatrix env_full;
		env_view.build_camera_dir(reflection_cam_pos, cm_dir[i], cm_norm[i]);
		env_full.mul(env_project, env_view);

		R_dsgraph_structure& Graph = GraphReflection[i];
		Graph.private_marker = true;
		Graph.val_pTransform = &Fidentity;
		Graph.private_visuals.clear();
		Graph.r_pmask(true, false);
		Graph.PortalTraverser.prepare_local_clips(sector_count, portal_count);
		Graph.r_dsgraph_render_subspace(reflection_sector, env_full, reflection_cam_pos, false, false);
	}

	g_r_cull_tls.active = false;

	Device.Statistic->TEST2.End();
	reflection_done.store(ticket, std::memory_order_release);
	reflection_done.notify_all();
}

void CRender::render_reflections(GfxCmdList& Cmd)
{
	if (o.dx11_use_legacy_light || !o.offscreen_reflecitons)
	{
		return;
	}

	wait_reflection_collect();

	GPU_EVENT(RENDER_REFLECTIONS);

	if (!RImplementation.pLastSector)
	{
		return;
	}

	Device.Statistic->TEST2.Begin();
	GPU_EVENT(FORWARD_REFLECTIONS);

	u32 DwSize = Target->rt_Reflection->dwSize;

	static Fmatrix EnvProject;
	static Fmatrix EnvView;
	static Fvector CmNorm[6];
	static Fvector CmDir[6];

	CmDir[2].mul(reflection_cam_top, +1.0f);
	CmDir[3].mul(reflection_cam_top, -1.0f);
	CmNorm[2].mul(reflection_cam_dir, -1.0f);
	CmNorm[3].mul(reflection_cam_dir, +1.0f);

	CmDir[0].mul(reflection_cam_right, +1.0f);
	CmDir[1].mul(reflection_cam_right, -1.0f);
	CmNorm[0].mul(reflection_cam_top, +1.0f);
	CmNorm[1].mul(reflection_cam_top, +1.0f);

	CmDir[4].mul(reflection_cam_dir, +1.0f);
	CmDir[5].mul(reflection_cam_dir, -1.0f);
	CmNorm[4].mul(reflection_cam_top, +1.0f);
	CmNorm[5].mul(reflection_cam_top, +1.0f);

	CEnvDescriptorMixer* CurrentEnv = g_pGamePersistent->Environment().CurrentEnv;

	EnvProject.build_projection(PI_DIV_2, 1.0f, reflection_near, reflection_far * 0.4f);

	Fvector4 fog_color4 = 
	{
		CurrentEnv->fog_far, 0.0f, 0.0f, 0.0f
	};

	is_render_cubemap = true;
	phase = PHASE_REFLECT;

	for (u32 i = 0; i < GraphReflection.size(); ++i)
	{
		EnvView.build_camera_dir(reflection_cam_pos, CmDir[i], CmNorm[i]);

		Cmd.ClearTarget(Target->rt_Reflection_temp->pRT[i], (const float*)&fog_color4);

		bool NeedRender = GraphReflection[i].mapNormalPasses[0][0].size() || GraphReflection[i].mapMatrixPasses[0][0].size() ||
						GraphReflection[i].mapNormalPasses[1][0].size() || GraphReflection[i].mapMatrixPasses[1][0].size() || GraphReflection[i].mapSorted.size();

		if (!NeedRender)
		{
			continue;
		}

		Cmd.ClearDepthStencil(Target->rt_Depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
		Target->u_setrt(Cmd, DwSize, DwSize, Target->rt_Reflection->pRT[i], Target->rt_Reflection_temp->pRT[i], NULL, Target->rt_Depth->pZRT);

		RImplementation.rmNormal();
		Cmd.SetStencil(FALSE);
		Cmd.SetColorWriteEnable();
		Cmd.SetView(EnvView, EnvProject);

		GraphReflection[i].r_dsgraph_render_graph(0);
	}

	Cmd.SetView(Device.mView, Device.mProject);
	is_render_cubemap = false;
	phase = PHASE_NORMAL;

	Target->DrawSQ(Cmd, Target->s_sslr, Target->rt_Reflection_forward, 2, [&]
	{
		RImplementation.rmNormal();
		Cmd.SetCullMode(ERHI_CULLMODE::NONE); 
	});

	Target->u_setrt(Cmd, DwSize, DwSize, nullptr, nullptr, nullptr, nullptr);
	Cmd.GenerateMips(Target->rt_Reflection_forward->pTexture->get_SRView());

	Device.Statistic->TEST2.End();
}
