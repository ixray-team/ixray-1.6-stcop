# Rendering

This branch uses separate client renderer DLLs. Dedicated servers load `xrRender_DS0`; `on_idle` skips `seqRender` for them.

## Renderer selection

| Token | DLL | API | Lighting | Main frame source |
| --- | --- | --- | --- | --- |
| `renderer_r1` | `xrRender_R1` | D3D9 | static | `src/Layers/xrRenderPC_R1/FStaticRender.cpp` |
| `renderer_r2` | `xrRender_R2` | D3D9 | dynamic, deferred | `src/Layers/xrRenderPC_R2/r2_R_render.cpp` |
| `renderer_r4` | `xrRender_R4` | D3D11 | dynamic, deferred | `src/Layers/xrRenderPC_R4/r4_R_render.cpp` |

`CApplication::ConfigureRenderer` applies `-r4`, `-r2`, or the config renderer before DLL loading. The console command in `src/xrEngine/xr_ioc_cmd.cpp` sets `rsR2` / `rsR4`. `CEngineAPI::InitializeNotDedicated` tries R4 when `rsR4` is set, then R2 if R4 loading fails or `rsR2` is set. `Initialize` falls back to R1 when no renderer loaded. Changing the renderer requires a restart.

`CreateRendererList` checks each renderer library beside the executable and emits its token independently (`.dll` on Windows, `lib*.so` elsewhere). `-perfhud_hack` bypasses the presence checks. Dedicated list is a dummy `renderer_r1` token.

`CEngineAPI::GetAPI()` returns `ERHI_API_LAYER::D3D11` for `rsR4`, otherwise `D3D9`. This branch has no `ELightingMode`, `LightingModeLockActive`, `LightingModeIsStatic()` / `LightingModeIsDynamic()`, or `renderer_r4_static` token. R1 is its own renderer.

## Device and resources

`Device.InitRenderDevice(Engine.External.GetAPI())` creates `GRHI` and its device during stage 4. `src/xrRHI` contains D3D9 and D3D11 backends; Linux builds use DXVK Native. `message_loop` calls `GRHI->BeginFrame()` before SDL polling. `Device.ConnectToRender` binds the renderer device interface (`dxRenderDeviceRender` for clients).

Shared code is in `src/Layers/xrRender`, `xrRenderDX9`, and `xrRenderDX10`; renderer CMake files select the sources compiled into each DLL. Resources use `CResourceManager`; materials use `IBlender` and shader permutations. R4 compute shaders use `CSCompiler`. Game code requests visuals through `Render` (`ModelPool`, `SkeletonAnimated`, `WallmarksEngine`, `PSLibrary`).

## R4 dynamic frame

`CRender::Render` in `src/Layers/xrRenderPC_R4/r4_R_render.cpp` is reached from `seqRender`.

| Early out | Result |
| --- | --- |
| `OnRenderPPUI_query` | `render_menu` |
| menu skips scene, or no level and no HUD | select LUT render target and return |
| `m_bFirstFrameAfterReset` | `xrRender_apply_tf`, clear the flag, return |

Main order:

1. Offscreen reflections if `o.offscreen_reflecitons` and `!o.dx11_use_legacy_light`, with jitter cleared.
2. TAA/upscaler jitter if `ps_r_scale_mode > 1` or `ps_r2_aa_type == 3`. Upscaler phases are `r4_rendertarget_phase_dlss.cpp`, `r4_rendertarget_phase_fsr.cpp`, and `r4_rendertarget_phase_xess.cpp`.
3. Actor item UI into `rt_ui_pda`.
4. Clear `rt_Generic_0` and `rt_Velocity`, then render sky.
5. Build `ViewBase`; run `HOM.Render` here unless `R2FLAG_EXP_MT_CALC` is set.
6. Optional `R2FLAG_ZFILL` depth pass through `render_main(false, true)`.
7. `render_main(true)` collects visible geometry; `r_dsgraph_render_graph` submits it.
8. G-buffer: `phase_scene_begin`, HUD, scope, graph 0, LODs, details, `phase_scene_end`. `R2FLAG_EXP_SPLIT_SCENE` draws graph 0 early; a scope mask disables splitting.
9. `phase_occq` for lights, wallmarks, sun cascades (`render_sun_cascades`), light accumulation, and `phase_combine`.
10. Within combine, the renderer calls `render_forward` for forward geometry, puddles, and volumetric combine. Postprocessing is in `r4_rendertarget_phase_*.cpp`.

Reflections and sun cascades are called from this render path. `PreRenderThread` runs `seqParallelRender`, rain item updates, and particle updates; it has no `CollectSunCascades` or `CollectReflections` delegates in this branch. GPU submission and swapchain operations stay on the primary thread.

## R2 dynamic frame

R2 has its own `CRender::Render` in `src/Layers/xrRenderPC_R2/r2_R_render.cpp` and D3D9 render targets and phases. It collects visibility, fills the deferred scene, accumulates lighting, combines the result, and draws forward geometry. R4-specific upscalers and motion-vector targets belong to R4 sources.

## R1 static frame

`src/Layers/xrRenderPC_R1/FStaticRender.cpp` implements visibility calculation and `CRender::Render` for R1. It uses D3D9 and its own render target; there is no `r4_R_static.cpp` path in this branch.

Frame order: actor item UI → `Target->Begin` → HUD, graph 0, details, LODs → sky and clouds → `L_Dynamic` pass 0 → wallmarks, `L_Shadows`, LODs, graph 1 → `L_Dynamic` pass 1 → portal fade, sorted geometry, `L_Glows`, flares, rain/thunderbolts → `Target->End` → `L_Projector->finalize`.

Static lighting uses `L_Dynamic`, `L_Shadows`, `L_Projector`, and `L_Glows`. Deferred combine changes in R2/R4 do not apply to this path.

## Visibility and shader options

Sectors and portals are traversed from the camera sector. HOM rejects hidden objects before drawing. Deferred renderers use occlusion queries for lights.

Grass uses `CDetailManager` and `ps_r__detail_*` cvars. Renderer cvars register in `src/Layers/xrRender/xrRender_console.h`; presets are `rspec_*.ltx` console scripts. Shader permutations belong to material blenders and renderer shader options.

R4 initializes `o.dx11_disable_motion_vectors` in `r4.cpp` from the `DISABLE_MOTION_VECTORS` entry in `EngineExternal().ShadersOptions`. It is not derived by a `NeedMotionVectors()` helper from AA, upscaler, or motion-blur cvars in this branch. Check the option, scene shader permutations, and `rt_Velocity` when debugging motion vectors; inspect the target with a windowed RenderDoc capture. Autotest launch details are in [lifecycle.md](lifecycle.md).

Sources: [renderer loading](../../src/xrEngine/EngineAPI.cpp), [R4 frame](../../src/Layers/xrRenderPC_R4/r4_R_render.cpp), [R1 frame](../../src/Layers/xrRenderPC_R1/FStaticRender.cpp), [shader options](../../src/xrCore/EngineExternal.cpp).
