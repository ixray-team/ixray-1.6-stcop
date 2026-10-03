# Rendering

Client DLL: `xrRender_R4`. RHI backends: D3D11 and D3D12 (Windows). `CEngineAPI::GetAPI()` returns the active `GRHI->APILevel` after device creation, or configured `g_graphicsAPI` before it. `CRHI::CreateDevice` selects `InternalDevice11` or `InternalDevice12`. Graphics API and `ELightingMode` are independent choices. Dedicated: `xrRender_DS0`, and `on_idle` skips `seqRender`.

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

## Reflections

Deferred SSR and offscreen VSLR share tracing helpers in `shaders/d3d11/reflections.hlsli` on both D3D11 and D3D12. [reflections.md](reflections.md) records the pass/resource contracts, water path, temporal rejection, known limits, and user validation procedure.

Reflection collection remains asynchronous: snapshot the camera before `FrameMove`, collect six private face graphs on `PreRenderThread`, wait before primary-thread GPU capture, and transform current-view points through world space into the captured view. A current-frame opaque lighting source (combine element 4, excluding SSR) is prepared before SSR and shared with water. Screen hits read its current UVs; previous-frame reprojection is confined to history. SSR dispatches depth minimum, trace, filter, then temporal; temporal binds three UAVs and alternates matched color/depth and surface histories. The extra source pass is one full-screen combine plus a sky/cloud background copy; geometry, shadows and per-light accumulation still run once. The source excludes later forward transparency and postprocessing. On 2026-10-03 the user confirmed the current-frame source/sky fixes work. The later foreground-occlusion fallback searches up to 24 neighboring cube depths on covered misses with no forward candidate and awaits user validation. Cross-API/overlay/configuration coverage and GPU cost remain unmeasured.

## Static frame

`r4_R_static.cpp`. Same device and meshes. Jitter forced off.

`HOM` + `render_main` → HUD, graph 0, details, LODs into `rt_Generic_0` → sky, clouds → `L_Dynamic` pass 0 → wallmarks, `L_Shadows`, LOD pass 2, graph 1 → `L_Dynamic` pass 1 → portals, sorted, sorted HUD, `L_Glows`, flares → distortion to `rt_Back_Buffer` → `phase_pp` → `L_Projector->finalize`.

Static lights are `L_Dynamic`, `L_Shadows`, `L_Projector`, `L_Glows`. Deferred combine changes do not apply to this path.

## Console and font submission

The regular console uses `CConsole::OnRender` in [XR_IOConsole.cpp](../../src/xrEngine/XR_IOConsole.cpp): draw backgrounds, queue prompt/tips/history through `CGameFont::OutI`, then flush the two console fonts. The ImGui debug console is a separate path in `XR_IOConsole_UI.cpp`.

[dxFontRender.cpp](../../src/Layers/xrRenderPC_R4/dxFontRender.cpp) is shared by both RHI backends. Previously, `RenderBase` mapped the vertex stream and issued an indexed draw for every queued string. It now batches consecutive strings from one font, with one map/unmap and draw per nonempty batch. Colors, gradients and positions remain per vertex; string order is preserved. Gamepad icons still render after the base text through separate draws.

Batch capacity is `min(RCache.Vertex.GetSize() / pGeom.stride() / 4, 16384)`. Each glyph uses four vertices, so 16-bit indices allow at most 16,384 glyph quads per draw. The larger allocation in `CBackend::CreateQuadIB` does not remove that index limit. Reserved glyph counts use string byte lengths, which conservatively bound emitted glyphs. Keep both the vertex-stream bound and the index bound when changing batching.

`CGameFont::MasterOut` adds eight extra strings when outlining is enabled. The supplied console font configuration does not enable outlining by default. `CConsole::OutFont` also recursively wraps long log entries and measures growing prefixes; that CPU work remains separate from draw batching. Neither was measured as the cause of the reported FPS drop.

## D3D12 dynamic streams

[_VertexStream::Lock](../../src/Layers/xrRenderPC_R4/R_DStreams.cpp) appends with `WRITE_NO_OVERWRITE` and wraps with `WRITE_DISCARD`. In [D3D12/Resources.cpp](../../src/xrRHI/D3D12/Resources.cpp), dynamic vertex/index buffers map their upload resource directly. `WRITE_NO_OVERWRITE` reuses owned upload storage; discard acquires another stream through the fence-aware pool in `D3D12/Device.cpp`. Their write unmap returns without uploading the whole shadow buffer. Do not infer a full-buffer copy or GPU flush for every console line from the generic map API.

`InternalDevice12::PrepareDraw` in `D3D12/Binding.cpp` still prepares descriptor/root bindings, pipeline state and vertex/index views for each draw. Batching reduces how often this path is called; it does not change its synchronization or resource lifetime.

The 2026-10-03 font batching change passed a Debug translation-unit compile with no warnings or errors. Runtime FPS and rendered output remain unverified. Check console-open versus console-closed frame times in the same loaded scene, and inspect text order, colors, selection, scrolling and long lines. A RenderDoc capture should show font draws split at batch capacity rather than at every string. Compilation alone does not establish the performance fix.

## Visibility and cvars

Sectors and portals: `render_main` from the camera sector. HOM rejects before the graph. Surviving light volumes get an occlusion query.

Grass: `CDetailManager`, cvars `ps_r__detail_*`. Trample: `Render->detail_trample_mark` from `CLevel::OnFrame`.

Cvars `ps_r*` / `ps_r2*` register in `xrRender_console.h`. Presets: `rspec_*.ltx` console scripts. New switchable features get a `ps_r` flag. Shader permutations go on the material `IBlender`.

`NeedMotionVectors`: dynamic lighting and (`ps_r_scale_mode >= 2` or `ps_r2_aa_type == 3` (`r_aa 3`) or `ps_r4_mblur_quality > 0`). `CRender::level_Load` copies that into `o.dx11_disable_motion_vectors` and clears shader options. Set those cvars before `start`. Headless check: [lifecycle.md](lifecycle.md). Velocity target: windowed RenderDoc.
