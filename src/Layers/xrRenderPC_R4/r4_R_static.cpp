#include "stdafx.h"
#include "r1_LightPPA.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/IGame_Actor.h"

void CRender::render_static()
{
    GPU_EVENT(R1_STATIC_FRAME);
    ps_r_taa_jitter.set(0, 0, -1);
    ps_r_taa_jitter_full.set(ps_r_taa_jitter);
    o.distortion = o.distortion_enabled;
    vis_intersect = false;
    phase = PHASE_NORMAL;
    if (GActorInterface)
    {
        Target->u_setrt(Target->rt_ui_pda, nullptr, nullptr);
        rmNormal();
        GActorInterface->RenderItemUI();
    }
    RCache.set_xform_world(Fidentity);
    ViewBase.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
    View = nullptr;
    HOM.Enable();
    if (!ps_r2_ls_flags.test(R2FLAG_EXP_MT_CALC)) HOM.Render(ViewBase);
    r_pmask(true, true);
    render_main(true);
    L_Shadows->calculate();
    L_Projector->calculate();
    RCache.set_xform_world(Fidentity);
    Target->u_setrt(Target->rt_Generic_0, nullptr, RDepth);
    GRHI->ClearTarget(Target->rt_Generic_0->pRT);
    GRHI->ClearDepthStencil(RDepth, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.f, 0);
    rmNormal();
    RCache.set_Stencil(false);
    RCache.set_ColorWriteEnable();
    r_dsgraph_render_hud();
    r_dsgraph_render_graph(0);
    if (Details) Details->Render();
    r_dsgraph_render_lods(true, false);
    g_pGamePersistent->Environment().RenderSky();
    g_pGamePersistent->Environment().RenderClouds();
    r_pmask(true, false);
    vis_intersect = true;
    L_Dynamic->render(0);
    vis_intersect = false;
    phase = PHASE_NORMAL;
    r_pmask(true, true);
    Target->u_setrt(Target->rt_Generic_0, nullptr, RDepth);
    if (Wallmarks) Wallmarks->Render();
    L_Shadows->render();
    r_dsgraph_render_lods(false, true);
    r_dsgraph_render_graph(1);
    r_pmask(false, true);
    vis_intersect = true;
    L_Dynamic->render(1);
    vis_intersect = false;
    phase = PHASE_NORMAL;
    r_pmask(true, true);
    HOM.Enable();
    PortalTraverser.fade_render();
    r_dsgraph_render_sorted(false);
    r_dsgraph_render_sorted_hud();
    L_Glows->Render();
    g_pGamePersistent->Environment().RenderFlares();
    g_pGamePersistent->Environment().RenderLast();
    Target->u_setrt(Target->rt_Generic_0, nullptr, nullptr);
    g_pGamePersistent->OnRenderPPUI_main();
    Target->u_setrt(Target->rt_Generic_1, nullptr, RDepth);
    GRHI->ClearTarget(Target->rt_Generic_1->pRT, ERTColor::Gray);
    r_dsgraph_render_distort();
    g_pGamePersistent->OnRenderPPUI_PP();
    Target->u_setrt(Target->rt_Back_Buffer, nullptr, nullptr);
    RCache.set_Element(Target->s_r1_distort->E[0]);
    RCache.set_Geometry(Target->FSTriangleGeom);
    RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, 3, 0, 1);
    Target->phase_pp();
    L_Projector->finalize();
}
