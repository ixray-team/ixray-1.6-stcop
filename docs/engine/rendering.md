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

## Visibility and shader options

Sectors and portals are traversed from the camera sector. HOM rejects hidden objects before drawing. Deferred renderers use occlusion queries for lights.

Grass uses `CDetailManager` and `ps_r__detail_*` cvars. Renderer cvars register in `src/Layers/xrRender/xrRender_console.h`; presets are `rspec_*.ltx` console scripts. Shader permutations belong to material blenders and renderer shader options.

R4 initializes `o.dx11_disable_motion_vectors` in `r4.cpp` from the `DISABLE_MOTION_VECTORS` entry in `EngineExternal().ShadersOptions`. It is not derived by a `NeedMotionVectors()` helper from AA, upscaler, or motion-blur cvars in this branch. Check the option, scene shader permutations, and `rt_Velocity` when debugging motion vectors; inspect the target with a windowed RenderDoc capture. Autotest launch details are in [lifecycle.md](lifecycle.md).

Sources: [renderer loading](../../src/xrEngine/EngineAPI.cpp), [R4 frame](../../src/Layers/xrRenderPC_R4/r4_R_render.cpp), [R1 frame](../../src/Layers/xrRenderPC_R1/FStaticRender.cpp), [shader options](../../src/xrCore/EngineExternal.cpp).

## Scope postprocess effects

`scope_nightvision = scope_contrast` and `scope_nightvision = scope_nightvision_gauss` load the configured `.ppe` animation in both 2D and R4 3D scopes. Installed scope effects are reapplied after addon initialization. For 3D scopes, the animator supplies an independent postprocess state; R4 processes the world snapshot in `$user$accum` before rendering the lens, detector frames and reticle. Separate color-map aliases and noise timing leave the actor camera postprocess independent. Zooming out, alternate aim and grenade mode disable the lens effect. `SPPInfo::lerp` adds to color-map influence and interpolation, gray, blur and duality, so `Device.ScopePostprocess` is reset to `pp_zero` before every `Process`, as `CCameraManager::UpdatePPEffectors` does for camera effectors; otherwise the color-map influence grows every frame and the lens turns white.

The `models_zoomnight` lens receives the same configured PPE profile through the scope snapshot. Its `ScopePostprocessActive` constant is bound from the current renderer state when the lens pass is selected. When PPE is active, `model_scope_lense_night` skips its additional material night-vision amplification; applying both effects compounds the configured profile with the material's brightness and green gain before scene exposure. The material effect remains available when no PPE pass is active, and zero material brightness leaves the snapshot unchanged.

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
