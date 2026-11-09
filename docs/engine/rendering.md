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

With `r__mt_texture_load on` (D3D11; D3D12 and the editors load synchronously), creating a texture returns its logical `CTexture` at once and starts its DDS load as soon as the render device is ready, including during deferred resource creation. Each `CTexture` owns a task group allocated only while a load is pending. Repeated requests find the same `CTexture` in the manager and reuse that task. The task reads the file, decodes the DDS and creates the D3D11 surface and view (the D3D11 device is free-threaded); it writes only that texture and then sets a release flag. Worker tasks never change the bind delegate or the views a draw reads. D3D11 staging uploads and binding remain on the rendering thread. Video textures and user surfaces use the synchronous path.

`CResourceManager::OnDeviceCreate` loads `ed\ed_not_existing_texture.dds` once, synchronously, as the shared placeholder; it is never unloaded. A texture that starts a background load keeps a reference to it until destruction, so the placeholder lives while anything can show it. A draw of a texture whose load is still running binds the placeholder view instead of waiting; once the task has finished, the next bind adopts the result on the rendering thread and all shaders, passes and visuals that already hold the texture use the loaded content. `GfxCmdList::SetTextures` keeps such slots marked and binds them again until the texture is no longer pending. The placeholder is a 2D texture, so it stands in only for textures a pixel shader declares as `Texture2D`: pixel shader reflection records those registers, and creating a pass enables the placeholder for the textures in them. Textures bound only to cube, volume, array, compute or vertex registers, render targets and `$user$` surfaces never show it; their first draw waits for the load as before. If the file fails to load, `texture_load` reports it as before (missing files already fall back to `ed_not_existing_texture`), and a 2D texture left without a view binds the placeholder.

Accessors that need real data (`get_Width`, `get_Height`, `get_Format`, `isUser`, `surface_get`, `GetView`) still wait for the load. `Unload` and destruction cancel a load that has not started and wait for a running one, so a late result never touches a destroyed texture; level unload therefore discards queued loads of released textures. `CResourceManager::DeferredUpload` only starts loads and returns; level start no longer waits for all textures. Memory statistics skip textures whose load is pending. Device reset/destruction and texture-description reload still wait before changing the resources used by the loader. Shader alpha-test setup (`uber_deffer`) still reads the DDS header of a pending texture through `CRender::PeekTextureFormat` when the load has not finished. Turning multithreaded loading off retains synchronous or lazy loading according to `bDeferredLoad`; `r__deferred_texture_load` controls bulk upload/unload on R4. R1 and R2 ignore it and always unload/upload: Direct3D 9 textures live in `D3DPOOL_DEFAULT`, and `IDirect3DDevice9::Reset` fails while any of them exists. `InternalDevice9::ResizeBuffers` retries a reset only while the device is lost and exits with an error on any other result.

The R4 `render.experemental.target_res` command is registered during renderer console initialization, before any background texture load. Texture loading never registers console commands from a worker.

## Skeleton updates

The renderer calculates skeletons in parallel (`SkeletonBatch`, `src/Layers/xrRenderPC_R4/SkeletonBatch.cpp`). `render_main` (deferred pass) and `r_dsgraph_render_subspace` (light and sun shadows) collect visible dynamic objects before static geometry, add their `CKinematics` to `R_dsgraph_structure::SkeletonUpdates`, and launch the batch. Worker threads calculate the bones while the primary thread traverses static geometry; `Wait` lets the primary thread take the remaining skeletons. Wallmarks, lights, glows and `renderable_Render` then run on the primary thread in the original traversal order.

`Launch` calls `CKinematics::PrepareBones` on the calling thread first: animation tracks and their blend callbacks (game and script code) never run on a worker. Bone hierarchies, bone callbacks and visual callbacks (IK ray picks, attachments) run on workers. `r__mt_skeletons off` keeps the same order and calculates every skeleton on the primary thread.

`UCalc_Mutex` is the only skeleton lock. A thread that is calculating a skeleton never waits for another one: a nested `CalculateBones` or `Bone_Calculate` on a skeleton owned by another thread is skipped, and `LL_GetBoneLocalPosition` / `LL_GetBoneLocalTransform` read the current matrices unlocked. This avoids cycles between neighbouring NPCs whose IK rays hit each other's collision forms. For the same reason `CCF_Skeleton::BuildState` copies the bone matrices before it takes its build lock: a thread that waits for a skeleton must not hold a lock that the skeleton's own ray picks need. Physics shell activation in `CPHElement::BonesCallBack` is serialized, because it moves ODE bodies in a shared space.

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
7. `render_main(true)` collects visible geometry of both priorities; `r_dsgraph_render_graph` submits it.
8. G-buffer: `phase_scene_begin`, HUD, scope, graph 0, LODs, details, `phase_scene_end`. `R2FLAG_EXP_SPLIT_SCENE` draws graph 0 early; a scope mask disables splitting.
9. `phase_occq` for lights, wallmarks, sun cascades (`render_sun_cascades`), light accumulation, and `phase_combine`.
10. Within combine, the renderer calls `render_forward` for forward geometry, puddles, and volumetric combine. Postprocessing is in `r4_rendertarget_phase_*.cpp`.

`PreRenderThread` runs `seqParallelRender`, the `CollectGraphs` delegate, rain item updates, and particle updates. GPU submission and swapchain operations stay on the primary thread.

### Render graphs

A graph (`R_dsgraph_structure`) belongs to one view. `GraphView` describes it: phase, sector, frustum, transform, origin, pass mask, and a copy of what the collection reads (camera, FOV, fog, SSA thresholds). A new pass needs a description, not a new collection path:

| Graph | Phase | Collected | Thread |
| --- | --- | --- | --- |
| `GraphMain` | normal | `render_main`, particles before forward | primary |
| `GraphReflection[6]` | reflect | static, `Build` | `PreRenderThread` |
| `GraphRain` | shadow map | static, `Build` | `PreRenderThread` |
| `GraphSun[3]` | shadow map | static with `Build`, dynamic in `append_sun_dynamics` | `PreRenderThread`, then primary |
| `GraphPass` | any | `BuildFull`: spot shadows, planar puddles, static-lighting dynamic lights | primary, one pass at a time |
| `GraphUI` | live | `UI3dStatic`, one icon at a time | primary |

`Build` is `Begin`, `Traverse`, `CaptureStatic`. `BuildFull` adds `CollectDynamic` before the static geometry, so worker threads calculate skeletons meanwhile, and `CaptureDynamic` after it. Traversal results stay in the graph's `CPortalTraverser`; `CSector` and `CPortal` hold no per-frame state.

`on_idle` calls `BeginGraphCollect` before starting `PreRenderThread`. It fills `GraphJobs` in the order the frame draws them, so these views use the camera of the previous frame. Shadow accumulation samples with the matrices used for capture: only coverage lags by a frame. The primary thread calls `WaitGraphJobs` with the jobs it is about to draw. `Render` and `render_rain` collect on the primary thread in the editors and on frames `CanCollectSceneOnWorker` did not foresee. `level_Unload` and `reset_begin` wait for `PreRenderThread`: both can run inside `FrameMove` and delete what the collection reads.

The main scene is collected once per frame: priority 0, priority 1 and wallmarks together. `render_forward` only adds particles through `CaptureMainParticles`, because they are simulated in parallel with the frame. The editors keep two collections. `BeginMainCollect` gives `GraphMain` a `GraphView` copy for each collection. All graphs deduplicate through `private_visuals`. `GraphUI` holds the 3D icons and is the only graph that reads the live camera, because `UI3dStatic` replaces it while it submits an icon.

The game sees a graph through `IDSGraphManager` (`src/xrEngine/Render.h`). The renderer passes it to `renderable_Render(DM)`, `CCustomHUD::Render_First(DM)`, `Render_Last(DM)` and `OnHUDDraw(hud, DM)`, and objects add their visuals with `DM->add_Dynamic(visual, &xform)`. The parameter is required, so an object always adds to the graph that is collecting it. `::Render->GetMainGraph()` and `GetUIGraph()` return the graphs for code outside these calls: the editors and `UI3dStatic`.

## 3D scope target detector

With `g_3d_scopes on` on R4, a lensed scope enables target frames through `scope_alive_detector = scope_detector` in its scope section (or a weapon upgrade). The detector also works when there is no 2D `scope_texture`. The section `scope_detector` supplies `vis_frame_speed`, `vis_frame_color`, `found_snd`, and `catch_snd`, as in the 2D detector.

`CActor::RenderScopeUI` routes the active first-person weapon to `TBinocularsVision::DrawScopeUI`. Visual-memory selection stays in the main frame update; rendering updates only target projection with the current world camera once per frame. The regular 2D UI query stays separate; switching to alternate aim, grenade mode, zooming out, or removing the scope leaves the shared texture clear of detector frames.

Detector upgrades are applied after HUD/addon initialization. `LoadScopeEffects` resolves weapon and scope defaults, then installed upgrades, so `up_sect_wpn_gauss_detector` remains active after addon reloads and save loading. Upgrade previews do not apply the detector.

`rt_ui_pda` (`$user$ui_pda`) uses RGBA16F to retain coverage alpha. `hud_scope_detector` writes linear premultiplied color with `ONE / INVSRCALPHA` blending, preserving overlapping frames. The scope shaders sample it in normalized screen coordinates, recover straight color and apply `lerp(World, UI.xyz, UI.w)` before scope fade and the reticle. Normalized coordinates keep the overlay aligned when render resolution differs from display resolution. Ordinary, variable-reticle, night-vision and Gauss scope materials bind `s_scope_ui` to the shared texture.

## R4 rain streaks

`CEnvironment::RenderLast` draws `CEffect_Rain` after sorted geometry in `render_forward` (and in `render_static`). `dxRainRender` uses the `effects\rain_streaks` shader (`effects_rain_streaks.*`) with the `fx\rain_streak_drop` texture and its own vertex layout: a point on the streak axis, the offset to the sprite edge and `CEffect_Rain::SpriteMotion`, the drop's world-space displacement over the last frame. One drop fills a sprite, so the sprites are narrowed to `rain_width` times `r4_rain_width_scale` (0.1 by default); the vertex shader widens streaks thinner than about a pixel and fades them. The splash model `dm\rain.dm` keeps its own shader.

`fx\rain_streak_drop` is `rain_drop` from Screen Space Shaders (used with the author's permission). It is an atlas; its top-left 7x7 texels hold one lens-shaped drop (r - normal across the streak, g - along it, a - thickness), stretched over the whole sprite and sampled at mip 0 because the streaks are a few pixels wide.

`CRenderTarget::PhaseRainStreaksBegin` blurs `rt_Generic_0` into `RainRefraction` (`$user$rain_refraction`, 1/4 resolution, `rain_refraction_blur`; `rmNormal` sets the viewport for the smaller target) and binds `rt_Generic_0`, `rt_Velocity` (omitted with `DISABLE_MOTION_VECTORS`) and `ReactiveMask` (`$user$reactive`, R8); the static frame binds only `rt_Generic_0`. It first copies the depth buffer into `RainDepth` (`$user$rain_depth`): forward geometry is included, and the static frame has no `rt_Position`. `CreateRainStreakTargets` creates these targets for both frames. The streak shows the blurred scene offset by the lens normal (up to 1.25% of the screen height times `r4_rain_refraction`, 0 disables it; `RainDepth` rejects geometry in front of the drop), 10% of a fake reflection taken far away in the same buffer and sky-tinted scattered light of 15% of the background luminance, so it reads as a transparent distortion over both sky and shaded ground and dense rain does not add up to a white sheet. PBR and legacy lighting share the shader. All targets blend `SRCALPHA / INVSRCALPHA` with the alpha of their own output: the velocity alpha is the share of the drop's own light, so the refracted background keeps the motion already in `rt_Velocity`, and the reactive alpha is the streak coverage. `PhaseRainStreaksEnd` restores the frame targets.

Streak details: the vertex shader varies width and opacity per drop (hashed from its fall direction, stable for the drop's life); the trail moves by the camera velocity over a 1/30 s exposure (at most half the streak length), so streaks smear when the camera moves. The pixel shader fades streaks near geometry (`RainDepth`) and closer than 0.3 m to the camera, and lights them with up to 8 nearest lamps from `Lights.package` (`rain_lights_*`, forward scattering when the lamp is behind the drop; none in the static frame) and with forward-scattered sunlight, which also picks up lightning flashes (`CEffect_Thunderbolt` raises `sun_color`).

`CRenderTarget::PhaseRainLayers` draws distant rain into the same targets: `rain_layers` scrolls streak columns on four cylinders around the camera (9, 18, 36 and 72 m), tilted by the fall direction, shifted by the camera position across the view so they stay in the world, and filtered to their average where a column is thinner than a pixel. A layer hides behind `RainDepth` and in fog; layers closer than about 40 m fade with `CEffect_Rain::SkyVisibility` (the camera's sky hemisphere factor), so they vanish under a roof. Layers leave `rt_Velocity` unchanged and write half their coverage into `ReactiveMask`. `rain_common.hlsli` holds the shared sky tint and sun scattering.

## R4 reactive mask

`ReactiveMask` (`$user$reactive`, R8, render resolution) marks translucent pixels whose content changes every frame and must not be taken from history. `phase_combine` clears it before forward rendering; translucent forward effects blend their coverage into it (rain streaks and distant rain layers). TAA (`taa_render`) lowers the history weight and narrows K-DOP clipping by it, FSR receives it as the reactive mask and DLSS as `pInBiasCurrentColorMask`.

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
| b6 | trample, volumetric lights, fluid sim, fluid render, bloom, tonemap (CBSkin is an empty placeholder) |
| b7 | detail wind, fluid AABB |
| b8 | fluid emitter |
| b9 | fluid OOBB clip planes |
| b10 | dynamic OOBB, static box bounds |

Grass values are written by name into their explicit buffers. Skinned meshes no longer upload bones per draw: `SkinningPass` skins every requested skeleton child once per frame in `skin.cs.hlsl` into `g_SkinnedVertices` (VS t58), with the previous position as a half delta for motion vectors. Fluid and editor-brush globals have no upload path yet. UI icon lighting, visor/gas-mask, `m_tc_xform` and editor constants live in `shared/extra_cb.hlsli` (`b11`-`b13`); those buffers stay bound, so a shader reading one that was never written sees zeros. No frame-time measurement is recorded here.

## Visibility and shader options

Sectors and portals are traversed from the camera sector. HOM rejects hidden objects before drawing. Deferred renderers use occlusion queries for lights.

Grass uses `CDetailManager` and `ps_r__detail_*` cvars. Renderer cvars register in `src/Layers/xrRender/xrRender_console.h`; presets are `rspec_*.ltx` console scripts. Shader permutations belong to material blenders and renderer shader options.

R4 initializes `o.dx11_disable_motion_vectors` in `r4.cpp` from the `DISABLE_MOTION_VECTORS` entry in `EngineExternal().ShadersOptions`. It is not derived by a `NeedMotionVectors()` helper from AA, upscaler, or motion-blur cvars in this branch. Check the option, scene shader permutations, and `rt_Velocity` when debugging motion vectors; inspect the target with a windowed RenderDoc capture. Autotest launch details are in [lifecycle.md](lifecycle.md).

Sources: [renderer loading](../../src/xrEngine/EngineAPI.cpp), [R4 frame](../../src/Layers/xrRenderPC_R4/r4_R_render.cpp), [R1 frame](../../src/Layers/xrRenderPC_R1/FStaticRender.cpp), [shader options](../../src/xrCore/EngineExternal.cpp).

## Scope lens chromatic aberration

R4 lens materials share `scope_lens.hlsli`. `SampleScopeScene` reads the scene snapshot and separates its red and blue samples in opposite diagonal directions using the Gunslinger edge-distance formula. The existing `m_hud_params.z` channel carries `scope_abberation`; both legacy constant binding and fixed constant buffers export the same value. No new GPU constant is introduced. Permanent scopes read the weapon's setting; detachable scopes use their own setting with the weapon as fallback. Installed upgrades are reapplied on scope reload.

Sampling uses the source texture dimensions and clamps coordinates to its texel centers. At zero strength, the helper retains the original scene load and skips the extra texture samples. Scene sampling precedes material night vision, the reticle and detector UI. The helper is included by all four lens shaders in every game package.

## Scope postprocess effects

`scope_nightvision = scope_contrast` and `scope_nightvision = scope_nightvision_gauss` load the configured `.ppe` animation for the weapon. Installed scope effects are reapplied after addon initialization. For R4 3D scopes, the animator supplies an independent postprocess state; the lens material chooses whether R4 processes the world snapshot in `$user$accum` before rendering the lens, detector frames and reticle. Separate color-map aliases and noise timing leave the actor camera postprocess independent. The lens PPE stays active throughout normal zoom-in and zoom-out animations, until the zoom rotation factor reaches zero; switching implementations only after zoom completion would produce a bright material-NVG flash during the transition. Detector UI still requires fully completed zoom. Alternate aim and grenade mode disable the lens effect. `SPPInfo::lerp` adds to color-map influence and interpolation, gray, blur and duality, so `Device.ScopePostprocess` is reset to `pp_zero` before every `Process`, as `CCameraManager::UpdatePPEffectors` does for camera effectors; otherwise the color-map influence grows every frame and the lens turns white.

The `models_zoomnight` lens receives the same configured PPE profile through the scope snapshot. Its `ScopePostprocessActive` constant is bound from the current renderer state when the lens pass is selected. When PPE is active, `model_scope_lense_night` skips its additional material night-vision amplification; applying both effects compounds the configured profile with the material's brightness and green gain before scene exposure. The material effect remains available when no PPE pass is active, and zero material brightness leaves the snapshot unchanged.

The Lua material option `:scope_postprocess(false)` requests a raw world snapshot for the HUD scope; the default is `true`. R4 checks this flag on the queued HUD materials before preparing the snapshot. No weapon or profile names are checked in the game component or renderer. For the Gauss material, use `false` to let `model_scope_gauss` provide its own grayscale gain, blue tint, brightness jitter and electronics-interference noise. It uses the existing `s_tonemap` texture and tonemap functions to restore HDR color before composing the lens; no additional shader constant or binding is needed. The existing brightness steps remain active, and the 2D scope continues to use `nightvision_gauss.ppe`. Setting `true` selects the snapshot PPE when available; the Gauss shader uses the existing `ScopePostprocessActive` binding to skip its own amplification and tint, while retaining electronics-interference noise.

D3D11 postprocess shaders sample color lookup tables and noise at mip level zero. Lookup-table sampling must not depend on screen-space brightness derivatives: selecting different mip levels across image edges produces bands. Noise coordinates use floating-point image-to-texture ratios to preserve grain size across resolutions.

At zero brightness, the 2D weapon postprocess stays disabled. Recreating an expired zero-factor animator at full strength causes alternating bright/dark frames; enabled animators receive their factor immediately on creation. Brightness upgrades initialize their new control range and retain installed parameters across addon reloads. Saved brightness is consumed only after a nonzero control range is available.

The 3D postprocess shaders convert the HDR world snapshot to display colors using the current exposure before evaluating the `.ppe` color table. They then invert tone mapping and exposure before returning the snapshot to the lens shader. This prevents the scene combine from applying exposure twice to display colors and washing out the scope. The conversion matches `combine_2` without the bloom term: exposure is divided by `bloom_amount + 1` (its square root with legacy lighting). Legacy lighting squares colors only to mix bloom and tone-maps the unsquared color, so the scope shaders must not square or take the square root around tone mapping: at high night exposure that brightens the lens to white.

## Rain puddles

`r4_rain_puddles` controls rain puddles in both PBR and legacy lighting. `r3_dynamic_wet_surfaces` separately controls ordinary wet-surface shading, splashes and streaks. The PBR rain compute pass runs when either effect needs it; `RainFeatures` lets it apply puddles while skipping general wetness, or apply general wetness without puddles. Procedural resources are created when the renderer starts with puddles enabled, so enabling the option requires a restart; disabling it is also checked at runtime.

`CRender::UpdateRainPuddleMode` checks `$level$/rain_mask.dds`, including mounted level archives. The shared `textures/rain_mask.dds` placeholder does not count as a current-level mask. PBR retains the existing masked calculation when a level mask exists; otherwise `o.ProceduralRainPuddles` selects the shared Perlin/slope/wave algorithm for terrain only (`TERRAIN_ID`). Ordinary static geometry (`BASE_ID`) is excluded from procedural coverage. Objects, foliage and HUD geometry are excluded; slope and rain-shadow checks remain active. Legacy lighting always selects procedural terrain puddles. Mask detection, the selected shader permutation and the cached mask texture are refreshed on level transitions and device resets by `CreateRainShaders`. Supplied-mask PBR and disabled procedural paths do not gain a procedural shader-cache suffix. The log reports the selected PBR algorithm and, for a level mask, its path.

`CBlender_rain::Compile` enables `USE_RAIN_MASK` and binds `s_mask` only when PBR has a real level mask. `_ParseList` replaces an absent texture list with `$null`, so checking only `C.L_textures.empty()` incorrectly selects a mask shader with an unbound texture. Its zero samples become full puddle coverage. `CreateRainShaders` clears shader options before compilation to keep a previous level's mask define from leaking into the procedural variant.

The PBR procedural slope uses `1 - smoothstep(0.05, 0.2, max(abs(WorldNormal.x), abs(WorldNormal.z)))` after contour smoothing. `GetRainPuddleFlatNormal` reuses the same four neighboring depth samples over a projected 25 cm radius (at least one texel, at most 2% of the viewport per axis), softening coverage changes across nearby triangle slopes. The supplied-mask PBR path retains its one-texel geometric normal reconstruction, and legacy procedural shading retains its original slope calculation.

Procedural PBR water normals follow that geometric normal, with wave/ripple perturbations applied in its tangent frame. A fixed world-up reflection normal on sloped receivers can direct grazing SSR rays back into the ground, producing a second contour inside the puddle. This adjustment changes only the procedural material normal and adds no texture samples or reflection passes; the native SSLR/VSLR code, supplied-mask shading and legacy water normal remain unchanged.

`shared/RainPuddles.hlsli` supports both pixel and compute shader sampling. Legacy `deffer_impl.ps.hlsl` uses terrain UVs and stores coverage in the third channel of its enabled RGBA surface buffer; its rain compute shader preserves that channel. Disabled legacy procedural shading retains the compact R8 format. PBR evaluates the fallback in `rain_render.cs.hlsl` using reconstructed geometric normals and the existing level-projection coordinates, then modifies native roughness, specular, color, AO and normals. It keeps native G-buffer packing: the PBR specular channel is not reused for puddle coverage. Both supplied-mask and procedural PBR puddles feed the existing SSLR/VSLR material pipeline. `getShaderParams` includes the procedural permutation in the in-memory resource name as well as the compiled shader cache, preventing reuse of another level's shader variant.

`RainPuddleWaterMap` builds a 2048-square R16G16_UNORM height/coverage map from passable liquid collision triangles for procedural modes. Hardware filtering samples both channels together, and dividing out coverage keeps empty cells from corrupting height. Puddle visibility fades between 2 and 25 cm above the water surface and across footprint edges after contour smoothing. The GPU map occupies 16 MiB, is rebuilt on level load/reset without per-frame collision queries, and is released when a supplied PBR mask is selected. Levels without liquid collision geometry use a neutral one-pixel map.

Only legacy lighting calls `PhaseRainPuddles` before forward rendering. Its overlay uses the water shader's existing `ScreenSpaceLocalReflections`, controlled by `r4_sslr_water`, with sky-map fallback. PBR does not use this overlay. `r4_puddles` continues to control independent level-authored volumetric geometry.

Configuration, resources, option combinations and troubleshooting: [English guide](../docs/main/en/graphics/weather/getting-wet.md) / [Russian guide](../docs/main/ru/graphics/weather/getting-wet.md). Sources: [mode selection and shader resources](../../src/Layers/xrRenderPC_R4/RainPuddles.cpp), [terrain bindings](../../src/Layers/xrRenderPC_R4/Blender_BmmD.cpp), [rain compute bindings](../../src/Layers/xrRenderPC_R4/DX10%20Rain/dx10RainBlender.cpp), [water exclusion map](../../src/Layers/xrRenderPC_R4/RainPuddleWaterMap.cpp).

## Visor glass and rain drops

Full equipment configuration, condition mapping, accumulation/reset rules, wiping animation setup, assets and troubleshooting: [English guide](../docs/main/en/gameplay/equipment/visor-glass.md) / [Russian guide](../docs/main/ru/gameplay/equipment/visor-glass.md).

R2 and R4 use `r2_use_rain_drops` to enable visor droplets. `r2_rain_drops_mode texture` selects the existing normal-map effect; `procedural` selects animated procedural drop layers. Both appear in advanced video options and can be changed without restarting. Texture droplets remain the default. R1 does not draw these effects.

An equipped helmet or outfit must have `glass_present = true`. A helmet with glass takes precedence over an outfit with glass. Glass is active only for the living actor in first-person view. `hud_gas_mask_avaliable` and `hud_rain_drops_avaliable` retain their existing spelling and independently control the glass and droplets. The base scientific outfit and protective helmet declare glass in the corresponding game packages.

`hud_gas_mask_type = flat` is the default and selects ordinary flat glass. R4 uses ten normal maps selected by the equipped item's condition, plus cracks, refraction and reflections. `hud_gas_mask_type = curved` selects hemispherical glass with distortion towards the edges: the original four-stage `effector_gasmask` shader and its condensation effect. `r4_use_gasmask` controls both glass implementations. Flat glass draws the selected droplet implementation in its own pass; curved glass and disabled glass postprocess use a separate droplet pass. `VisorGlassType` carries the selected `Flat` or `Curved` type from equipment to the renderer.

`IGame_Persistent::VisorRain` owns the accumulation state for both drop modes. The primary render thread checks for overhead cover once a second with a 50-metre upward ray, then changes coverage by 0.05 per second, capped by rain density. `CActor::ClearMaskCB`, called by the `Left` motion mark of `anm_gasmask` or the configured clear-mask animator, clears coverage. The dedicated item animator also invokes an unconsumed callback at natural completion; the HUD-item animation requires its motion mark. Drops accumulate again in rain and dry gradually under cover while rain continues. Rain stopping clears them immediately. Disabling droplets, removing or replacing the glass item, changing the view entity and disconnecting reset the state. This state is independent of weapon droplets and world wetness.

The D3D11 shaders calculate procedural normals with finite differences. R2 computes height once and uses screen derivatives, scaled to the same normal strength, to fit within the minimum Shader Model 3 instruction limit. The shared algorithm is in `shaders/{r2,d3d11}/shared/VisorDrops.hlsli`; flat-glass damage textures are in `textures/shaders/gasmasks/`. The curved-glass shader and textures remain available.

## Actor night vision

R4's `nvg_render.ps.hlsl` uses three profiles selected by `shader_effect_range` in the equipped item's `nightvision_sect`. Profile color/gain and postprocessing parameters come from `shader_nvg_color` and `shader_nvg_postprocessing`; an explicit armor `nightvision_color` takes precedence. Profile changes through upgrades take effect on the next activation. Generation zero keeps the PPE route.

`CNightVisionEffector` retains logical device state while electronics interference suppresses the shader. Actor updates reset only visor fields in `ShaderParams`; the night-vision profile persists between frames. Clearing the whole structure would zero its radial coefficient and turn the image black. `PhaseEffectSQ` derives a locally rate-limited `NightVisionInterference` from the current counter exported as `m_affects.x`, including immediate script updates when loading an emission. Initial activation uses the current counter directly. The shader fades through `smoothstep(0.02, 0.4, ...)`, with small downward pulses during the transition, and returns the original scene at complete suppression. Falling interference restores the effect. The exported electronics interface and other devices are unchanged.

`CUIGameCustom::Render` maintains the single-instance `nv_tunnel` custom static with `hud/nvg_vignette.dds` while the actor is alive, in first-person view and has logically enabled, active night vision. Shader suppression leaves this UI mask visible. Manual switch-off, battery depletion and equipment removal use the existing device lifecycle; death and leaving first-person view hide the mask. Caption clearing recreates it on the next draw. No custom-static pointer is retained across UI resets. Weapon/scope night vision remains separate.

Configuration, default coefficients, resource setup and manual verification: [English guide](../docs/main/en/gameplay/equipment/night-vision.md), [Russian guide](../docs/main/ru/gameplay/equipment/night-vision.md).

Sources: [game effector](../../src/xrGame/EffectorNightVision.cpp), [armor profile](../../src/xrGame/ArmorBase.cpp), [portable device](../../src/xrGame/nvg.cpp), [UI lifecycle](../../src/xrGame/ui/UIGameCustom.cpp), [R4 pass](../../src/Layers/xrRenderPC_R4/r4_rendertarget_phase_screen_postprocess.cpp), [shader](../../gamedata/shaders/d3d11/nvg_render.ps.hlsl), [UI definition](../../gamedata/configs/ui/ui_custom_msgs.xml).
