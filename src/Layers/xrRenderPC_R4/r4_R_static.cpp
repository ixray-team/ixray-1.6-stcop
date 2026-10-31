#include "stdafx.h"
#include "r1_GlowManager.h"
#include "r1_LightPPA.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IGame_Actor.h"

void CRender::render_static(GfxCmdList& Cmd)
{
	GPU_EVENT(R1_STATIC_FRAME);
	ps_r_taa_jitter.set(0, 0, -1);
	ps_r_taa_jitter_full.set(ps_r_taa_jitter);
	Cmd.Constants.UpdateScreen();
	o.distortion = o.distortion_enabled;
	vis_intersect = false;
	phase = PHASE_NORMAL;
	if (GActorInterface)
	{
		Target->u_setrt(Cmd, Target->rt_ui_pda, nullptr, nullptr);
		rmNormal();
		GActorInterface->RenderItemUI();
	}
	Cmd.SetObjectTransform(Fidentity);
	ViewBase.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
	GraphMain.View = nullptr;
	HOM.Enable();
	if (!ps_r2_ls_flags.test(R2FLAG_EXP_MT_CALC)) HOM.Render(ViewBase);
	GraphMain.r_pmask(true, true);
	render_main(Cmd, true);
	L_Shadows->calculate(Cmd);
	L_Projector->calculate(Cmd);
	Cmd.SetObjectTransform(Fidentity);
	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
	Cmd.ClearTarget(Target->rt_Generic_0->pRT);
	Cmd.ClearDepthStencil(RDepth, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.f, 0);
	rmNormal();
	Cmd.SetStencil(false);
	Cmd.SetColorWriteEnable();
	GraphMain.r_dsgraph_render_hud();
	GraphMain.r_dsgraph_render_graph(0);
	if (Details) Details->Render(Cmd);
	GraphMain.r_dsgraph_render_lods(true, false);
	g_pGamePersistent->Environment().RenderSky();
	g_pGamePersistent->Environment().RenderClouds();
	GraphMain.r_pmask(true, false);
	vis_intersect = true;
	L_Dynamic->render(Cmd, 0);
	vis_intersect = false;
	phase = PHASE_NORMAL;
	GraphMain.r_pmask(true, true);
	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, RDepth);
	if (Wallmarks) Wallmarks->Render(Cmd);
	L_Shadows->render(Cmd);
	GraphMain.r_dsgraph_render_lods(false, true);
	GraphMain.r_dsgraph_render_graph(1);
	GraphMain.r_pmask(false, true);
	vis_intersect = true;
	L_Dynamic->render(Cmd, 1);
	vis_intersect = false;
	phase = PHASE_NORMAL;
	GraphMain.r_pmask(true, true);
	HOM.Enable();
	GraphMain.PortalTraverser.fade_render(Cmd);
	GraphMain.r_dsgraph_render_sorted(false);
	GraphMain.r_dsgraph_render_sorted_hud();
	L_Glows->Render(Cmd);
	g_pGamePersistent->Environment().RenderFlares();
	g_pGamePersistent->Environment().RenderLast();
	Target->u_setrt(Cmd, Target->rt_Generic_0, nullptr, nullptr);
	g_pGamePersistent->OnRenderPPUI_main();
	Target->u_setrt(Cmd, Target->rt_Generic_1, nullptr, RDepth);
	Cmd.ClearTarget(Target->rt_Generic_1->pRT, ERTColor::Gray);
	GraphMain.r_dsgraph_render_distort();
	g_pGamePersistent->OnRenderPPUI_PP();
	Target->u_setrt(Cmd, Target->rt_Back_Buffer, nullptr, nullptr);
	Cmd.SetPass(Target->s_r1_distort->E[0]);
	Cmd.DrawNoInput(3);
	Target->phase_pp(Cmd);
	L_Projector->finalize();
}
