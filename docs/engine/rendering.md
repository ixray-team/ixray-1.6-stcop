# Rendering

This branch uses separate client renderer DLLs. Dedicated servers load `xrRender_DS0`; `on_idle` skips `seqRender` for them.

## Renderer selection

| Token | DLL | API | Lighting | Main frame source |
| --- | --- | --- | --- | --- |
| `renderer_r1` | `xrRender_R1` | D3D9 | static | `src/Layers/xrRenderPC_R1/FStaticRender.cpp` |
| `renderer_r2` | `xrRender_R2` | D3D9 | dynamic, deferred | `src/Layers/xrRenderPC_R2/r2_R_render.cpp` |
| `renderer_r4` | `xrRender_R4` | D3D11 | dynamic, deferred | `src/Layers/xrRenderPC_R4/r4_R_render.cpp` |

`CApplication::ConfigureRenderer` applies `-r4`, `-r2`, or the config renderer before DLL loading. The console command in `src/xrEngine/xr_ioc_cmd.cpp` sets `rsR2` / `rsR4` only until the renderer library is loaded (`Device.m_pRender`); later calls from `user.ltx` or the options menu only change the saved token, so `-r2` / `-r4` survive the config and the running renderer keeps its device API. `CEngineAPI::InitializeNotDedicated` tries R4 when `rsR4` is set, then R2 if R4 loading fails or `rsR2` is set. `Initialize` falls back to R1 when no renderer loaded. Changing the renderer requires a restart.

`CreateRendererList` checks each renderer library beside the executable and emits its token independently (`.dll` on Windows, `lib*.so` elsewhere). `-perfhud_hack` bypasses the presence checks. Dedicated list is a dummy `renderer_r1` token.

`CEngineAPI::GetAPI()` returns `ERHI_API_LAYER::D3D11` for `rsR4`, otherwise `D3D9`. This branch has no `ELightingMode`, `LightingModeLockActive`, `LightingModeIsStatic()` / `LightingModeIsDynamic()`, or `renderer_r4_static` token. R1 is its own renderer.

## Device and resources

`Device.InitRenderDevice(Engine.External.GetAPI())` creates `GRHI` and its device during stage 4. `src/xrRHI` contains D3D9 and D3D11 backends; Linux builds use DXVK Native. `message_loop` calls `GRHI->BeginFrame()` before SDL polling. `Device.ConnectToRender` binds the renderer device interface (`dxRenderDeviceRender` for clients).

Shared code is in `src/Layers/xrRender`, `xrRenderDX9`, and `xrRenderDX10`; renderer CMake files select the sources compiled into each DLL. Resources use `CResourceManager`; materials use `IBlender` and shader permutations. R4 compute shaders use `CSCompiler`. Game code requests visuals through `Render` (`ModelPool`, `SkeletonAnimated`, `WallmarksEngine`, `PSLibrary`).

## Texture loading

With `r__mt_texture_load on`, creating a texture starts its DDS load as soon as the render device is ready, including during deferred resource creation. Each `CTexture` owns a task group allocated only while a load is pending. Repeated requests reuse that task. `Load`, surface/view access, unloading, and destruction wait for it; worker tasks do not change the texture's bind delegate. D3D11 staging uploads and binding remain on the rendering thread. Video textures, sequences, user surfaces, and editor loads use the synchronous path.

`CResourceManager::DeferredUpload` uses the same tasks and waits for completion before returning. It also drains pending loads when deferred upload is disabled. Device reset/destruction and texture-description reload wait before changing the resources used by the loader. Turning multithreaded loading off retains synchronous or lazy loading according to `bDeferredLoad`; `r__deferred_texture_load` controls bulk upload/unload on R4. R1 and R2 ignore it and always unload/upload: Direct3D 9 textures live in `D3DPOOL_DEFAULT`, and `IDirect3DDevice9::Reset` fails while any of them exists. `InternalDevice9::ResizeBuffers` retries a reset only while the device is lost and exits with an error on any other result.

The R4 `render.experemental.target_res` command is registered during renderer console initialization, before any background texture load. Texture loading never registers console commands from a worker.

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
3. Clear `rt_ui_pda` to transparent, then render actor item UI and the active 3D scope detector once into it.
4. Clear `rt_Generic_0` and `rt_Velocity`, then render sky.
5. Build `ViewBase`; run `HOM.Render` here unless `R2FLAG_EXP_MT_CALC` is set.
6. Optional `R2FLAG_ZFILL` depth pass through `render_main(false, true)`.
7. `render_main(true)` collects visible geometry; `r_dsgraph_render_graph` submits it.
8. G-buffer: `phase_scene_begin`, HUD, scope, graph 0, LODs, details, `phase_scene_end`. `R2FLAG_EXP_SPLIT_SCENE` draws graph 0 early; a scope mask disables splitting.
9. `phase_occq` for lights, wallmarks, sun cascades (`render_sun_cascades`), light accumulation, and `phase_combine`.
10. Within combine, the renderer calls `render_forward` for forward geometry, puddles, and volumetric combine. Postprocessing is in `r4_rendertarget_phase_*.cpp`.

Reflections and sun cascades are called from this render path. `PreRenderThread` runs `seqParallelRender`, rain item updates, and particle updates; it has no `CollectSunCascades` or `CollectReflections` delegates in this branch. GPU submission and swapchain operations stay on the primary thread.

## 3D scope target detector

With `g_3d_scopes on` on R4, a lensed scope enables target frames through `scope_alive_detector = scope_detector` in its scope section (or a weapon upgrade). The detector also works when there is no 2D `scope_texture`. The section `scope_detector` supplies `vis_frame_speed`, `vis_frame_color`, `found_snd`, and `catch_snd`, as in the 2D detector.

`CActor::RenderScopeUI` routes the active first-person weapon to `TBinocularsVision::DrawScopeUI`. Visual-memory selection stays in the main frame update; rendering updates only target projection with the current world camera once per frame. The regular 2D UI query stays separate; switching to alternate aim, grenade mode, zooming out, or removing the scope leaves the shared texture clear of detector frames.

Detector upgrades are applied after HUD/addon initialization. `LoadScopeEffects` resolves weapon and scope defaults, then installed upgrades, so `up_sect_wpn_gauss_detector` remains active after addon reloads and save loading. Upgrade previews do not apply the detector.

`rt_ui_pda` (`$user$ui_pda`) uses RGBA16F to retain coverage alpha. `hud_scope_detector` writes linear premultiplied color with `ONE / INVSRCALPHA` blending, preserving overlapping frames. The scope shaders sample it in normalized screen coordinates, recover straight color and apply `lerp(World, UI.xyz, UI.w)` before scope fade and the reticle. Normalized coordinates keep the overlay aligned when render resolution differs from display resolution. Ordinary, variable-reticle, night-vision and Gauss scope materials bind `s_scope_ui` to the shared texture.

## R2 dynamic frame

R2 has its own `CRender::Render` in `src/Layers/xrRenderPC_R2/r2_R_render.cpp` and D3D9 render targets and phases. It collects visibility, fills the deferred scene, accumulates lighting, combines the result, and draws forward geometry. R4-specific upscalers and motion-vector targets belong to R4 sources.

## R1 static frame

`src/Layers/xrRenderPC_R1/FStaticRender.cpp` implements visibility calculation and `CRender::Render` for R1. It uses D3D9 and its own render target; there is no `r4_R_static.cpp` path in this branch.

Frame order: actor item UI → `Target->Begin` → HUD, graph 0, details, LODs → sky and clouds → `L_Dynamic` pass 0 → wallmarks, `L_Shadows`, LODs, graph 1 → `L_Dynamic` pass 1 → portal fade, sorted geometry, `L_Glows`, flares, rain/thunderbolts → `Target->End` → `L_Projector->finalize`.

Static lighting uses `L_Dynamic`, `L_Shadows`, `L_Projector`, and `L_Glows`. Deferred combine changes in R2/R4 do not apply to this path.

## Console and font submission

The regular console uses `CConsole::OnRender` in [XR_IOConsole.cpp](../../src/xrEngine/XR_IOConsole.cpp): draw backgrounds, queue prompt/tips/history through `CGameFont::OutI`, then flush the two console fonts. The ImGui debug console is a separate path in `XR_IOConsole_UI.cpp`.

[dxFontRender.cpp](../../src/Layers/xrRenderPC_R4/dxFontRender.cpp) is shared by both RHI backends. Previously, `RenderBase` mapped the vertex stream and issued an indexed draw for every queued string. It now batches consecutive strings from one font, with one map/unmap and draw per nonempty batch. Colors, gradients and positions remain per vertex; string order is preserved. Gamepad icons still render after the base text through separate draws.

Batch capacity is `Cmd.Vertex.GetCapacity() / 4`, where `Cmd` is the immediate backend (`RImplementation.Imm()`). Each glyph uses four ring records drawn by `DrawQuads` (non-indexed, `SV_VertexID`), so there is no 16-bit index limit. Reserved glyph counts use string byte lengths, which conservatively bound emitted glyphs.

`CGameFont::MasterOut` adds eight extra strings when outlining is enabled. The supplied console font configuration does not enable outlining by default. `CConsole::OutFont` also recursively wraps long log entries and measures growing prefixes; that CPU work remains separate from draw batching. Neither was measured as the cause of the reported FPS drop.

## D3D12 dynamic streams

[VertexRing::Lock](../../src/Layers/xrRenderPC_R4/R_DStreams.cpp) appends with `WRITE_NO_OVERWRITE` and wraps with `WRITE_DISCARD` (every lock discards on D3D11 drivers without `MapNoOverwriteOnDynamicBufferSRV`). Legacy formats are written to CPU scratch and encoded into 32 B records on `Unlock`. In [D3D12/Resources.cpp](../../src/xrRHI/D3D12/Resources.cpp), dynamic vertex/index buffers map their upload resource directly. `WRITE_NO_OVERWRITE` reuses owned upload storage; discard acquires another stream through the fence-aware pool in `D3D12/Device.cpp`. Their write unmap returns without uploading the whole shadow buffer. Do not infer a full-buffer copy or GPU flush for every console line from the generic map API.

`InternalDevice12::PrepareDraw` in `D3D12/Binding.cpp` still prepares descriptor/root bindings, pipeline state and vertex/index views for each draw. Batching reduces how often this path is called; it does not change its synchronization or resource lifetime.

## D3D12 per-draw path

`PrepareDraw` drains queued uploads once, then resolves bindings with `_preparing` set so no upload copy lands between binding validation and the draw. Draw calls record on `_commands` directly after it.

The graphics PSO key is rebuilt only when `_pipelineDirty` is set (shaders, layout, topology, attachments, depth-bounds enable, or `DX12StateManager` state). A barrier elsewhere only reruns `TransitionAttachments`; a compute dispatch or command-list reset rebinds the cached `_graphicsPipeline`. Pipeline buckets store the key hash. `DX12StateManager::Apply` converts RHI descriptors only after a setter ran and compares with the device only when `_stateSerial` moved; `SetRawBlendState` bumps that serial.

UAV writes are hazard-tracked per resource with `UAVPendingEpoch` (command-list epoch). `Dispatch` and `FinishDraw` mark written UAVs instead of issuing a UAV barrier each time. `UAVTable` emits one batched UAV barrier when a pending resource is bound as a UAV again in the same list; a full transition out of `UNORDERED_ACCESS` clears the mark because the transition already orders the writes. `PrepareUpscale` emits the barrier before external upscalers write a pending output. Command-list boundaries are fully synchronized, so marks from older epochs are ignored.

Occlusion queries record `EndQuery` only; `Submit` resolves all slots ended in the list with one `ResolveQueryData` per contiguous run. A slot restarted in the same list is resolved before its new `BeginQuery`. Default-heap buffer addresses are cached for the vertex/index path, and depth bounds are reapplied only when they change.

Validation: compare D3D12 frame time and the `D3D12 ... barrier ... pso` counter line before and after in the same loaded scene; check SSR/GTAO/compute chains, upscalers, occlusion-culled lights and depth-bounds lights in RenderDoc or with `-dxdebug`. Not compiled or measured yet.

The 2026-10-03 font batching change passed a Debug translation-unit compile with no warnings or errors. Runtime FPS and rendered output remain unverified. Check console-open versus console-closed frame times in the same loaded scene, and inspect text order, colors, selection, scrolling and long lines. A RenderDoc capture should show font draws split at batch capacity rather than at every string. Compilation alone does not establish the performance fix.

## Constant buffers

Engine-owned `b0`–`b5` layouts and the matrix encode/decode live in `gamedata/shaders/d3d11/shared/fixed_cb.hlsli`. C++ includes that file; dynamic, static and editor declarations expand the same field lists. The R1-on-D3D11 declarations (`r1_decl.hlsli`) keep the shorter `cb_frame` prefix without `hud_rain` and `m_digiclock`. CPU `static_assert`s lock sizes and offsets. `encode_float4x4` / `encode_float3x4` are the shared transpose; a 3x4 decode restores an affine matrix (`0,0,0,1` on the dropped row).

`FixedConstants` owns the CPU shadow and the six dynamic buffers. Writes compare the shadow and skip the upload when the bytes match. A failed map leaves the buffer dirty. D3D12 upload alignment stays in the RHI and does not change these offsets.

Texture and sampler slots come from the pass. `dx10texture` / `r_dx10Texture` assign `t0` upward in call order, or the slot passed as the third Lua argument. `dx10sampler` / `r_dx10Sampler` assign `s0` upward. The shader compiler rewrites those declarations to `register(tN)` / `register(sN)` before compilation. Only pixel and compute passes bind textures, at most 16 per pass. A pass that requests more must drop unused ones (`r_dx10Unbind`), as the terrain blender does. Shader load does not call D3D shader reflection. `m_affects` is written once per frame. Bones, bloom, tonemap, wind and trample constants are written by name into their explicit buffers. Other loose `$Globals` have no reflected offset anymore.

D3D12 reads resource binding metadata from DXBC/DXIL when creating or replacing an RHI shader. This metadata supplies CBV/sampler/UAV masks and SRV/UAV dimensions, return types and buffer kinds; it does not restore renderer constant-offset reflection. Missing or feedback-bound SRVs use null descriptors matching the shader declaration, including `TextureCube`. Replacement refreshes this metadata and its null-descriptor cache together, and preserves the old shader if reflection fails. The 2026-10-03 change addresses GPU validation error 940 at level startup and awaits Debug/RelWithDebInfo runtime verification.

Slots `b6`–`b10` are explicit pass buffers, reused only by shaders that do not declare the same slot together:

| Slot | Buffers |
| --- | --- |
| b6 | skin, trample, volumetric lights, fluid sim, fluid render, bloom, tonemap |
| b7 | detail wind, fluid AABB |
| b8 | fluid emitter |
| b9 | fluid OOBB clip planes |
| b10 | dynamic OOBB, static box bounds |

Bone and grass values are written by name into their explicit buffers. `DISABLE_MOTION_VECTORS` still drops `sbones_array_old`. Fluid and editor-brush globals have no upload path yet. UI icon lighting, visor/gas-mask, `m_tc_xform` and editor constants live in `shared/extra_cb.hlsli` (`b11`-`b13`); those buffers stay bound, so a shader reading one that was never written sees zeros. No frame-time measurement is recorded here.

## Visibility and shader options

Sectors and portals are traversed from the camera sector. HOM rejects hidden objects before drawing. Deferred renderers use occlusion queries for lights.

Grass uses `CDetailManager` and `ps_r__detail_*` cvars. Renderer cvars register in `src/Layers/xrRender/xrRender_console.h`; presets are `rspec_*.ltx` console scripts. Shader permutations belong to material blenders and renderer shader options.

R4 initializes `o.dx11_disable_motion_vectors` in `r4.cpp` from the `DISABLE_MOTION_VECTORS` entry in `EngineExternal().ShadersOptions`. It is not derived by a `NeedMotionVectors()` helper from AA, upscaler, or motion-blur cvars in this branch. Check the option, scene shader permutations, and `rt_Velocity` when debugging motion vectors; inspect the target with a windowed RenderDoc capture. Autotest launch details are in [lifecycle.md](lifecycle.md).

Sources: [renderer loading](../../src/xrEngine/EngineAPI.cpp), [R4 frame](../../src/Layers/xrRenderPC_R4/r4_R_render.cpp), [R1 frame](../../src/Layers/xrRenderPC_R1/FStaticRender.cpp), [shader options](../../src/xrCore/EngineExternal.cpp).

## Scope postprocess effects

`scope_nightvision = scope_contrast` and `scope_nightvision = scope_nightvision_gauss` load the configured `.ppe` animation in both 2D and R4 3D scopes. Installed scope effects are reapplied after addon initialization. For 3D scopes, the animator supplies an independent postprocess state; R4 processes the world snapshot in `$user$accum` before rendering the lens, detector frames and reticle. Separate color-map aliases and noise timing leave the actor camera postprocess independent. Zooming out, alternate aim and grenade mode disable the lens effect. `SPPInfo::lerp` adds to color-map influence and interpolation, gray, blur and duality, so `Device.ScopePostprocess` is reset to `pp_zero` before every `Process`, as `CCameraManager::UpdatePPEffectors` does for camera effectors; otherwise the color-map influence grows every frame and the lens turns white.

D3D11 postprocess shaders sample color lookup tables and noise at mip level zero. Lookup-table sampling must not depend on screen-space brightness derivatives: selecting different mip levels across image edges produces bands. Noise coordinates use floating-point image-to-texture ratios to preserve grain size across resolutions.

At zero brightness, the 2D weapon postprocess stays disabled. Recreating an expired zero-factor animator at full strength causes alternating bright/dark frames; enabled animators receive their factor immediately on creation. Brightness upgrades initialize their new control range and retain installed parameters across addon reloads. Saved brightness is consumed only after a nonzero control range is available.

The 3D postprocess shaders convert the HDR world snapshot to display colors using the current exposure before evaluating the `.ppe` color table. They then invert tone mapping and exposure before returning the snapshot to the lens shader. This prevents the scene combine from applying exposure twice to display colors and washing out the scope. The conversion matches `combine_2` without the bloom term: exposure is divided by `bloom_amount + 1` (its square root with legacy lighting). Legacy lighting squares colors only to mix bloom and tone-maps the unsquared color, so the scope shaders must not square or take the square root around tone mapping: at high night exposure that brightens the lens to white.

## Rain puddles

`r4_rain_puddles` enables noise-based terrain puddles only when R4 uses `USE_LEGACY_LIGHT`. The option is off initially and requires a game restart. `CRender::o.RainPuddles` captures it at renderer creation/reset; the shader cache adds a separate `USE_RAIN_PUDDLES` permutation only for that legacy path. PBR retains its existing masked wet-surface calculation, shader permutations and SSLR/VSLR processing.

The terrain shader samples three shared textures for Perlin contours, surface waves and rain-impact ripples, using terrain UVs and the upward geometric normal. Coverage follows `CEnvironment::wetness_factor`; no level-specific puddle mask is required. The enabled legacy path uses an RGBA surface buffer and stores coverage in its third channel. The legacy wet-surface compute shader preserves that channel when repacking the material; its PBR branch remains unchanged. Disabled legacy lighting retains the compact R8 surface format.

`CRenderTarget::PhaseRainPuddles` blends reflections into the combined opaque scene before forward rendering. `rain_puddles.ps.hlsl` calls the water shader's existing `ScreenSpaceLocalReflections`, controlled by `r4_sslr_water`, with sky-map fallback. It does not enable the world's SSLR/VSLR passes on legacy lighting. `r4_puddles` continues to control the separate level-authored volumetric geometry.

Configuration, assets, wetness timing and troubleshooting: [English guide](../docs/main/en/graphics/weather/getting-wet.md) / [Russian guide](../docs/main/ru/graphics/weather/getting-wet.md). Sources: [terrain bindings](../../src/Layers/xrRender/Blender_BmmD.cpp), [reflection pass](../../src/Layers/xrRenderPC_R4/RainPuddles.cpp), [reflection bindings](../../src/Layers/xrRenderPC_R4/BlenderRainPuddles.cpp).

## Visor glass and rain drops

Full equipment configuration, condition mapping, accumulation/reset rules, wiping animation setup, assets and troubleshooting: [English guide](../docs/main/en/gameplay/equipment/visor-glass.md) / [Russian guide](../docs/main/ru/gameplay/equipment/visor-glass.md).

R2 and R4 use `r2_use_rain_drops` to enable visor droplets. `r2_rain_drops_mode texture` selects the existing normal-map effect; `procedural` selects animated procedural drop layers. Both appear in advanced video options and can be changed without restarting. Texture droplets remain the default. R1 does not draw these effects.

An equipped helmet or outfit must have `glass_present = true`. A helmet with glass takes precedence over an outfit with glass. Glass is active only for the living actor in first-person view. `hud_gas_mask_avaliable` and `hud_rain_drops_avaliable` retain their existing spelling and independently control the glass and droplets. The base scientific outfit and protective helmet declare glass in the corresponding game packages.

`hud_gas_mask_type = flat` is the default and selects ordinary flat glass. R4 uses ten normal maps selected by the equipped item's condition, plus cracks, refraction and reflections. `hud_gas_mask_type = curved` selects hemispherical glass with distortion towards the edges: the original four-stage `effector_gasmask` shader and its condensation effect. `r4_use_gasmask` controls both glass implementations. Flat glass draws the selected droplet implementation in its own pass; curved glass and disabled glass postprocess use a separate droplet pass. `VisorGlassType` carries the selected `Flat` or `Curved` type from equipment to the renderer.

`IGame_Persistent::VisorRain` owns the accumulation state for both drop modes. The primary render thread checks for overhead cover once a second with a 50-metre upward ray, then changes coverage by 0.05 per second, capped by rain density. `CActor::ClearMaskCB`, called by the `Left` motion mark of `anm_gasmask` or the configured clear-mask animator, clears coverage. The dedicated item animator also invokes an unconsumed callback at natural completion; the HUD-item animation requires its motion mark. Drops accumulate again in rain and dry gradually under cover while rain continues. Rain stopping clears them immediately. Disabling droplets, removing or replacing the glass item, changing the view entity and disconnecting reset the state. This state is independent of weapon droplets and world wetness.

The D3D11 shaders calculate procedural normals with finite differences. R2 computes height once and uses screen derivatives, scaled to the same normal strength, to fit within the minimum Shader Model 3 instruction limit. The shared algorithm is in `shaders/{r2,d3d11}/shared/VisorDrops.hlsli`; flat-glass damage textures are in `textures/shaders/gasmasks/`. The curved-glass shader and textures remain available.
