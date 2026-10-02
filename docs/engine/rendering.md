# Rendering

Client DLL: `xrRender_R4`. API: `CEngineAPI::GetAPI()` → `ERHI_API_LAYER::D3D11`. Dedicated: `xrRender_DS0`, and `on_idle` skips `seqRender`.

`GRHI` is created at stage 4 before `Device.InitRenderDevice`. `message_loop` calls `GRHI->BeginFrame()` before SDL poll. `Device.ConnectToRender` sets `Device.m_pRender` to `dxRenderDeviceRender`.

Resources: `CResourceManager`. Shader compile/cache: `CSCompiler`. Materials: `IBlender` → permutations (`uber_deffer`, static forward shaders). Game code requests visuals from `Render` (`ModelPool`, `SkeletonAnimated`, `WallmarksEngine`, `PSLibrary`).

## Lighting mode

`ELightingMode` in `src/xrEngine/EngineAPI.h`. `LightingModeLockActive` runs before `LoadLibrary("xrRender_R4")`.

| Mode | Token | Alias | `g_current_renderer` | Frame |
| --- | --- | --- | --- | --- |
| `Static` | `renderer_r4_static` | `renderer_r1` | 1 | `CRender::render_static` |
| `Dynamic` | `renderer_r4` | `renderer_r2` | 2 | deferred path in `CRender::Render` |

Branch with `LightingModeIsStatic()` / `LightingModeIsDynamic()`. Mode change needs a restart.

`CreateRendererList` emits both tokens when `xrRender_R4.dll` (`libxrRender_R4.so` off Windows) is beside the exe. Dedicated list is a dummy `renderer_r1` token.

## `CRender::Render`

`src/Layers/xrRenderPC_R4/r4_R_render.cpp`, from `seqRender`.

| Early out | Result |
| --- | --- |
| `OnRenderPPUI_query` | `render_menu` |
| menu skips scene, or no level and no HUD | LUT only |
| `m_bFirstFrameAfterReset` | `xrRender_apply_tf` |
| `LightingModeIsStatic()` | `render_static` |

Dynamic order:

1. Offscreen reflections if `o.offscreen_reflecitons` and `!o.dx11_use_legacy_light` (jitter cleared).
2. TAA/FSR3 jitter if `ps_r_scale_mode > 1` or `ps_r2_aa_type == 3`. Phases: `r4_rendertarget_phase_dlss.cpp`, `phase_fsr.cpp`, `phase_xess.cpp`.
3. Actor PDA → `rt_ui_pda`.
4. Clear color; velocity too if `NeedMotionVectors()`.
5. Sky. `ViewBase` from `Device.mFullTransform`.
6. `HOM.Render` on this thread unless `R2FLAG_EXP_MT_CALC`.
7. `R2FLAG_ZFILL`: `render_main(false, true)`, depth-only flush.
8. `render_main(true)` fills `GraphMain`. Draw is `r_dsgraph_render_graph`, not `render_main`.
9. G-buffer: `phase_scene_begin`, HUD, scope, graph 0, LODs, `Details`, `phase_scene_end`. `R2FLAG_EXP_SPLIT_SCENE` flushes graph 0 early unless a scope mask is active.
10. `phase_occq` for point, spot, shadowed lights.
11. Shadows, accumulation, combine, SSAO/GTAO, SSLR, bloom, TAA, CAS, gamma: `Target->phase_*` in `r4_rendertarget_phase_*.cpp`.

Sun: primary calls `ResetSunCollect`, `PreRenderThread` runs `CollectSunCascades`, primary calls `EnsureSunCollect` after `seqRender`.

## Static frame

`r4_R_static.cpp`. Same device and meshes. Jitter forced off.

`HOM` + `render_main` → HUD, graph 0, details, LODs into `rt_Generic_0` → sky, clouds → `L_Dynamic` pass 0 → wallmarks, `L_Shadows`, LOD pass 2, graph 1 → `L_Dynamic` pass 1 → portals, sorted, sorted HUD, `L_Glows`, flares → distortion to `rt_Back_Buffer` → `phase_pp` → `L_Projector->finalize`.

Static lights are `L_Dynamic`, `L_Shadows`, `L_Projector`, `L_Glows`. Deferred combine changes do not apply to this path.

## Visibility and cvars

Sectors and portals: `render_main` from the camera sector. HOM rejects before the graph. Surviving light volumes get an occlusion query.

Grass: `CDetailManager`, cvars `ps_r__detail_*`. Trample: `Render->detail_trample_mark` from `CLevel::OnFrame`.

Cvars `ps_r*` / `ps_r2*` register in `xrRender_console.h`. Presets: `rspec_*.ltx` console scripts. New switchable features get a `ps_r` flag. Shader permutations go on the material `IBlender`.

`NeedMotionVectors`: dynamic lighting and (`ps_r_scale_mode >= 2` or `ps_r2_aa_type == 3` (`r_aa 3`) or `ps_r4_mblur_quality > 0`). `CRender::level_Load` copies that into `o.dx11_disable_motion_vectors` and clears shader options. Set those cvars before `start`. Headless check: [lifecycle.md](lifecycle.md). Velocity target: windowed RenderDoc.
