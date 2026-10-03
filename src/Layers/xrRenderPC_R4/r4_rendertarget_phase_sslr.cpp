#include "stdafx.h"
#include "r4_rendertarget.h"
#include "dx10FixedConstants.h"

void CRenderTarget::phase_sslr()
{
	if (RImplementation.o.dx11_use_legacy_light || !RImplementation.o.deffered_reflecitons)
		return;

	GPU_EVENT(phase_sslr);
	FixedConstants::SetReflectionHistory(_sslrJitter, _sslrFrame + 1u == Device.dwFrame);

	//groups
	const UINT tgroupsX = (RCache.get_width() + 7u) / 8u;
	const UINT tgroupsY = (RCache.get_height() + 7u) / 8u;

	{
		GPU_EVENT(sslr_depth_min);

		IRHIUnorderedAccessView* uav_dummy = nullptr;
		IRHIShaderResourceView* srv_dummy[16] = {};

		ShaderElement* S = (&*(s_sslr->E[4]));
		SPass& P = *(S->passes[0]);
		RCache.set_States(P.state);
		RCache.set_Constants(P.constants);
		RCache.set_Textures(P.T);
		RCache.set_CS(P.cs);

		IRHIUnorderedAccessView* our_uav = rt_sslr_depth_min->pUAView;

		GRHI->SetComputeUAVs(0, 1, &our_uav, nullptr);

		RCache.Compute(rt_sslr_depth_min->dwWidth, rt_sslr_depth_min->dwHeight, 1);

		GRHI->SetComputeUAVs(0, 1, &uav_dummy, nullptr);
		GRHI->SetComputeResources(0, 16, srv_dummy);
	}

	{
		GPU_EVENT(sslr_render);

		//Dummy
		IRHIUnorderedAccessView* uav_dummy[2] = { nullptr, nullptr };
		IRHIShaderResourceView* srv_dummy[16] = {};

		//Shader setup... can't use set_element because of set_PS bullshit
	    ShaderElement* S;
        S = (&*(s_sslr->E[0]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);

		//Bind UAVs

		IRHIUnorderedAccessView* our_uav[2] = {
            rt_sslr_trace->pUAView,
            rt_sslr_data->pUAView
		};

		GRHI->SetComputeUAVs(0, 2, our_uav, nullptr);

		//Dispatch
		RCache.Compute(tgroupsX, tgroupsY, 1);

		//Unbind
		GRHI->SetComputeUAVs(0, 2, uav_dummy, nullptr);
		GRHI->SetComputeResources(0, 16, srv_dummy);
	}


	{
		GPU_EVENT(sslr_filter);

		IRHIUnorderedAccessView* uav_dummy = nullptr;
		IRHIShaderResourceView* srv_dummy[16] = {};

	    ShaderElement* S;
        S = (&*(s_sslr->E[1]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);


		IRHIUnorderedAccessView* our_uav = rt_sslr_temp->pUAView;

		GRHI->SetComputeUAVs(0, 1, &our_uav, nullptr);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		GRHI->SetComputeUAVs(0, 1, &uav_dummy, nullptr);
		GRHI->SetComputeResources(0, 16, srv_dummy);
	}

	{
		GPU_EVENT(sslr_temporal);

		IRHIShaderResourceView* srv_dummy[16] = {};

		//The history alternates between two targets, so the final image is written once more instead of being copied
	    ShaderElement* S;
        S = (&*(s_sslr->E[sslr_history_flip ? 5 : 2]));
        SPass& P = *(S->passes[0]);
        RCache.set_States(P.state);
        RCache.set_Constants(P.constants);
        RCache.set_Textures(P.T);
        RCache.set_CS(P.cs);


		IRHIUnorderedAccessView* our_uav[3] = {
			rt_sslr->pUAView,
			(sslr_history_flip ? rt_sslr_old : rt_sslr_hist)->pUAView,
			(sslr_history_flip ? rt_sslr_old_surface : rt_sslr_hist_surface)->pUAView
		};
		IRHIUnorderedAccessView* uav_dummy[3] = { nullptr, nullptr, nullptr };

		GRHI->SetComputeUAVs(0, 3, our_uav, nullptr);

		RCache.Compute(tgroupsX, tgroupsY, 1);

		GRHI->SetComputeUAVs(0, 3, uav_dummy, nullptr);
		GRHI->SetComputeResources(0, 16, srv_dummy);

		sslr_history_flip = !sslr_history_flip;
		_sslrJitter = ps_r_taa_jitter;
		_sslrFrame = Device.dwFrame;
	}
}

void CRender::begin_reflection_collect()
{
	reflection_cam_pos = Device.vCameraPosition;
	reflection_cam_dir = Device.vCameraDirection;
	reflection_cam_top = Device.vCameraTop;
	reflection_cam_right = Device.vCameraRight;
	reflection_fov = Device.fFOV;
	reflection_near = Device.fViewportNear;
	_reflectionDistance = ps_r4_vslr_distance;
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
	const float fov_factor = _sqr(90.f / reflection_fov);
	const float screen = _sqr((float)dwSize) * fov_factor * (EPS_S + ps_r__LOD);

	R_CullTLS cull;
	cull.active = true;
	cull.phase = PHASE_REFLECT;
	cull.ssa_discard = _sqr(ps_r__ssaDISCARD) / screen;
	cull.ssa_lod_a = _sqr(ps_r2_ssaLOD_A / 3) / screen;
	cull.ssa_lod_b = _sqr(ps_r2_ssaLOD_B / 3) / screen;

	Fmatrix env_project;
	env_project.build_projection(PI_DIV_2, 1.0f, reflection_near, reflection_far * _reflectionDistance);

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
		Graph.r_pmask(true, true);
		Graph.PortalTraverser.prepare_local_clips(sector_count, portal_count);
		Graph.r_dsgraph_render_subspace(reflection_sector, env_full, reflection_cam_pos, false, false);
	}

	g_r_cull_tls.active = false;

	Device.Statistic->TEST2.End();
	reflection_done.store(ticket, std::memory_order_release);
	reflection_done.notify_all();
}

void CRender::render_reflections()
{
	if (o.dx11_use_legacy_light || !o.offscreen_reflecitons)
	{
		return;
	}

	wait_reflection_collect();

	GPU_EVENT(RENDER_REFLECTIONS);

	if (!reflection_sector)
	{
		FixedConstants::SetReflectionCapture(Fidentity, 0.f, false);
		const Fvector4 fallbackColor = { 0.f, 0.f, 0.f, 1.f };
		GRHI->ClearTarget(Target->rt_Reflection_forward->pRT, &fallbackColor.x);
		GRHI->GenerateMips(Target->rt_Reflection_forward->pTexture->get_SRView());
		return;
	}

	Fmatrix captureView;
	captureView.build_camera_dir(reflection_cam_pos, reflection_cam_dir, reflection_cam_top);
	FixedConstants::SetReflectionCapture(captureView, reflection_far * _reflectionDistance * std::sqrt(3.f), true);

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

	EnvProject.build_projection(PI_DIV_2, 1.0f, reflection_near, reflection_far * _reflectionDistance);

	const Fvector4 distanceClear =
	{
		-1.0f, 0.0f, 0.0f, 0.0f
	};

	is_render_cubemap = true;
	phase = PHASE_REFLECT;

	for (u32 i = 0; i < GraphReflection.size(); ++i)
	{
		EnvView.build_camera_dir(reflection_cam_pos, CmDir[i], CmNorm[i]);

		GRHI->ClearTarget(Target->rt_Reflection->pRT[i]);
		GRHI->ClearTarget(Target->rt_Reflection_temp->pRT[i], &distanceClear.x);

		bool NeedRender = GraphReflection[i].mapNormalPasses[0][0].size() || GraphReflection[i].mapMatrixPasses[0][0].size() ||
						GraphReflection[i].mapNormalPasses[1][0].size() || GraphReflection[i].mapMatrixPasses[1][0].size() || GraphReflection[i].mapSorted.size();

		if (!NeedRender)
		{
			continue;
		}

		GRHI->ClearDepthStencil(Target->rt_Depth->pZRT, ERHI_CLEAR_TARGET::DEPTH, 1.0f, 0L);
		Target->u_setrt(DwSize, DwSize, Target->rt_Reflection->pRT[i], Target->rt_Reflection_temp->pRT[i], NULL, Target->rt_Depth->pZRT);

		RImplementation.rmNormal();
		RCache.set_Stencil(FALSE);
		RCache.set_ColorWriteEnable();
		RCache.set_xform_view(EnvView);
		RCache.set_xform_project(EnvProject);

		GraphReflection[i].r_dsgraph_render_graph(0);
		GraphReflection[i].r_dsgraph_render_graph(1);
		GraphReflection[i].r_dsgraph_render_sorted(false);
	}

	RCache.set_xform_project(Device.mProject);
	RCache.set_xform_view(Device.mView);
	is_render_cubemap = false;
	phase = PHASE_NORMAL;

	Target->DrawSQ(Target->s_sslr, Target->rt_Reflection_forward, 3, []
	{
		RImplementation.rmNormal();
		GRHI->StateManager->SetCullMode(ERHI_CULLMODE::NONE); 
	});

	Target->u_setrt(DwSize, DwSize, nullptr, nullptr, nullptr, nullptr);
	GRHI->GenerateMips(Target->rt_Reflection_forward->pTexture->get_SRView());

	Device.Statistic->TEST2.End();
}
