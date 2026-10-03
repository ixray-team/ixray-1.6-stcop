# Reflections

The R4 dynamic renderer uses deferred screen-space local reflections (SSLR/SSR), offscreen cubemap reflections (VSLR), and a forward water SSR path. Both D3D11 and D3D12 use `shaders/d3d11` through `CRender::getShaderPath`. Static lighting and legacy lighting do not run the deferred SSR pipeline.

Status, 2026-10-03: after the current-frame source and sky fixes, the user reported "Works now." This confirms the latest implementation in the user's tested scenario. The report does not specify build configuration, active overlay, settings, captures or timings; it does not establish coverage across both APIs/all game sets or a measured speedup. Earlier revisions had incorrect history reprojection, excessive VSLR rejection, incorrect long-ray sky fallback and stale SSR source color. Their causes, fixes and maintenance constraints are recorded below. The subsequent nearby-foliage report led to the bounded neighboring-cube fallback described below; that follow-up awaits user validation. Static checks passed; the agent did not run local builds, shader compilation, runtime captures or GPU timings.

## Source map

| Responsibility | Source |
| --- | --- |
| Frame snapshot and worker launch | [device.cpp](../../src/xrEngine/device.cpp) |
| Async collection, cubemap capture, compute dispatch | [r4_rendertarget_phase_sslr.cpp](../../src/Layers/xrRenderPC_R4/r4_rendertarget_phase_sslr.cpp) |
| Shader elements and SRV bindings | [blender_sslr.cpp](../../src/Layers/xrRenderPC_R4/blender_sslr.cpp), [blender_combine.cpp](../../src/Layers/xrRenderPC_R4/blender_combine.cpp), [effects_water.lua](../../gamedata/shaders/d3d11/effects_water.lua) |
| Current-frame lighting source and combine order | [r4_rendertarget_phase_combine.cpp](../../src/Layers/xrRenderPC_R4/r4_rendertarget_phase_combine.cpp) |
| Target formats, allocation and history clears | [r4_rendertarget.cpp](../../src/Layers/xrRenderPC_R4/r4_rendertarget.cpp) |
| Capture/history constants and CPU layout | [dx10FixedConstants.h](../../src/Layers/xrRenderPC_R4/dx10FixedConstants.h), [dx10FixedConstants.cpp](../../src/Layers/xrRenderPC_R4/dx10FixedConstants.cpp) |
| HLSL pass layout | [common_decl.hlsli](../../gamedata/shaders/d3d11/common_decl.hlsli) |
| Shared SSR/VSLR intersection and reprojection | [reflections.hlsli](../../gamedata/shaders/d3d11/reflections.hlsli) |
| Tile reduction, trace, filter, history | [sslr_depth_min.cs.hlsl](../../gamedata/shaders/d3d11/sslr_depth_min.cs.hlsl), [sslr_render.cs.hlsl](../../gamedata/shaders/d3d11/sslr_render.cs.hlsl), [sslr_filter.cs.hlsl](../../gamedata/shaders/d3d11/sslr_filter.cs.hlsl), [sslr_temporal.cs.hlsl](../../gamedata/shaders/d3d11/sslr_temporal.cs.hlsl) |
| Water reflection and velocity | [water.ps.hlsl](../../gamedata/shaders/d3d11/water.ps.hlsl) |
| Deferred color consumer, camera velocity, forward VSLR map | [combine_1.ps.hlsl](../../gamedata/shaders/d3d11/combine_1.ps.hlsl), [combine_velocity.ps.hlsl](../../gamedata/shaders/d3d11/combine_velocity.ps.hlsl), [combine_vslr.ps.hlsl](../../gamedata/shaders/d3d11/combine_vslr.ps.hlsl) |

Maintain reflection changes in `gamedata`, `gamedata_soc`, `gamedata_cs`, and `gamedata_coc`. The shared helper and four compute shaders are byte-identical across these sets. Water and combine shaders have game-specific differences: apply targeted edits, do not replace whole variants. `common_decl.hlsli` is supplied by the base shader set and inherited through overlays.

## Reported failures and implemented changes

| Failure or source defect | Change |
| --- | --- |
| D3D12 access violation inside `ShaderElement::passes[0]`, with invalid `this = 0x1C` | `phase_sslr` requested element 4 while the blender only built elements 0-3. Added depth-min element 4 and alternating-history element 5, with matching shaders and bindings. |
| Partial optimization: CPU targets/dispatch and old shader contracts disagreed | Completed tile reduction, RGB trace output, trace/filter inputs, alternating histories, and direct final/history writes. |
| Water discontinuities when pitching; discarded SSR samples around the weapon | Shared perspective-correct tracing, near-plane and screen clipping, finite checks, view-space thickness, hit refinement, and explicit HUD/world hit categories. Preserve the world hit UV/category through HUD fallback. |
| VSLR did not align with SSR during movement | Use the asynchronous capture camera for point/direction conversion, radial comparisons, accepted hit conversion, and forward-map lookup. Include camera translation. |
| Unstable filter values | Guard VNDF/PDF and half-vector arithmetic; encode PDF logarithmically in half-float data; reject invalid/category-mismatched samples before weighting; fall back to the center if total weight is too small. |
| Stale or mismatched history | Store signed view depth and paired surface metadata; reject depth/category/normal/material changes; use consecutive-frame validity, actual prior SSR jitter, a complete clamped 3x3 neighborhood, and motion/color-dependent history weight. |
| Color clipping or double conversion | Keep trace/filter/history in linear HDR; convert once at the existing gamma-space ambient API boundary. |
| Empty cubemap faces retained previous contents | Clear color and distance on all six faces, even when no geometry is drawn; invalidate capture and clear the forward fallback map when no sector exists. |
| Glossy history moved with the receiver instead of its reflected image | Reproject the reflected hit mirrored across the local receiver plane; validate each bilinear history tap against that plane and matching metadata. |
| Tight VSLR validation discarded the original gap-filling samples | Restore nearest valid captured-surface fallback when strict refinement misses, with finite forward-ray bounds and the original distance fade. |
| Full-distance sky rays picked earlier terrain or cleared cube color | Require terminal radial coverage for nearest fallback; uncovered rays use rotated weather sky. The capture cube contains no sky. |
| SSR visibly lagged behind VSLR even after reprojection changes | Replace previous resolved scene color with a current-frame opaque source shared by deferred SSR and water. Reprojection cannot update old scene color. |

The user's final working report follows these combined fixes. It does not isolate each change or provide a performance comparison; retain the regression checks below for future changes.

## Current-frame reflection color

Deferred SSR and water read `rt_sslr_scene` (`$user$sslr_scene`), a render-resolution RGBA16_FLOAT texture. Before tracing, copy this frame's sky/cloud background from `rt_Generic_0`, then shade opaque geometry using combine element 4. Its `SSLR_SOURCE_PASS` variant shares the regular combine shader, fog blend, stencil, lighting/AO and sun constants, but excludes deferred SSR sampling. This avoids consuming an old or unfinished SSR result while preparing its own source.

The order is AO, sky/cloud background, source copy and opaque lighting, four deferred SSR compute stages when enabled, regular combine element 0, then forward rendering including water. Restore/apply main graphics targets before compute reads the source. Keep the source immutable through forward rendering; sampling the same target water writes would create feedback. The source pass and allocation are gated to deferred SSR or water SSR. Source contents exclude later forward transparency and postprocessing; reflection history remains separate.

Current screen hits sample `Hit.UV` directly, including the current raster jitter used by the G-buffer. Do not subtract motion, current jitter, or project through a previous matrix when reading this texture. `ReflectionPreviousUV` was removed; only temporal history performs previous-frame reprojection. The old `r2_RT_generic` source contained previous resolved color, so moving objects/lighting and upstream temporal filtering could remain stale even after camera reprojection. The user authorized the extra pass, background copy and HDR texture to remove that source delay.

The extra "lighting pass" is specifically one additional full-screen opaque combine draw. Geometry/G-buffer rendering, shadow maps and individual light accumulation still run once. Combine element 4 and regular element 0 both read the existing light accumulator and compute material/ambient/fog color; the optional static-sun calculation inside combine also runs in both. The source variant retains the existing sky/VSLR ambient fallback, so "without SSR" does not mean diffuse-only or reflection-free lighting.

```text
current G-buffer + accumulated lights + AO
                      |
current sky/clouds -> background copy -> combine element 4 -> rt_sslr_scene
                                                           |          |
                                      depth-min/trace/filter/temporal  |
                                                           |          |
                                                     rt_sslr          |
                                                           |          |
                                             combine element 0        |
                                                           |          |
                                                   forward + water <--+
                                                           |
                                                   postprocessing
```

The background copy is required by both stencil coverage and the existing fog blend: opaque combine only shades stencil-marked pixels, and its alpha preserves part of the destination background. Clearing the source to black would lose the uncovered sky and change fogged opaque color. `ResolveSurface` uses the existing RHI surface-copy operation here; this is a same-size/same-format copy, not an additional multisample resolve. Source and `rt_Generic_0` use the same HDR format. The source has ordinary RT/SRV usage and does not need a UAV.

Water binds the source directly in every game overlay. `LuaGetShaderOption` queries external shader options, not all engine-generated macros, so it cannot reliably gate this binding on `USE_SSLR_ON_WATER`. `r_dx10Texture` already skips resources absent from compiled shader reflection; shaders without water SSR do not acquire the inactive source binding.

## Deferred passes and dispatch

`phase_sslr` returns for `dx11_use_legacy_light` or disabled `deffered_reflecitons`. All compute shaders use `numthreads(8, 8, 1)`. Trace, filter and temporal reject out-of-bounds threads. Tile reduction contributes depth 1 for out-of-bounds pixels so every thread still reaches the group barriers.

| Order | Blender element | Shader | UAV outputs | Dispatch groups |
| --- | --- | --- | --- | --- |
| 1 | 4 | `sslr_depth_min` | u0: tile minimum | Tile texture width x height |
| 2 | 0 | `sslr_render` | u0: RGB trace; u1: trace point/PDF | ceil(render width / 8) x ceil(render height / 8) |
| 3 | 1 | `sslr_filter` | u0: filtered color/path length | Same render groups |
| 4 | 2 or 5 | `sslr_temporal` | u0: final; u1: next history; u2: next surface metadata | Same render groups |

Each depth-min group reduces one 8x8 render tile. Do not divide the already reduced texture dimensions by eight again. This is a tile minimum optimization, not a full hierarchical-Z structure or a spatial jumping traversal.

Trace reads the G-buffer, current-frame opaque scene (`r2_RT_sslr_scene`), tile minima, environment color/distance, sky and noise. Filter reads trace RGB and point/PDF data. Temporal reads filtered RGB, current trace point/PDF data (`s_refl_data`), and the matching previous color/depth and surface history.

With `sslr_history_flip == false`, element 2 reads `old`/`old_surface` and writes `hist`/`hist_surface`. With it true, element 5 reads `hist`/`hist_surface` and writes `old`/`old_surface`. Both write stable `rt_sslr` directly; no history `CopySurface` is needed. Flip only after dispatch. Unbind compute UAVs and the 16 compute SRV slots after each stage; never alias a history input with its output.

## Resource formats and contents

Dimensions below are the render dimensions, not necessarily the presentation resolution.

| Target | Format | Contents |
| --- | --- | --- |
| `rt_sslr_scene` | RGBA16_FLOAT, render resolution | Current-frame sky/cloud background plus opaque lighting without deferred SSR |
| `rt_sslr_depth_min` | R32_FLOAT, ceil(width/8) x ceil(height/8) | Minimum raw G-buffer depth per tile |
| `rt_sslr_trace` | R11G11B10_FLOAT | Linear HDR reflection RGB; no alpha/category flag |
| `rt_sslr_data` | RGBA16_FLOAT | xyz: selected point in current view; w: signed biased logarithmic PDF |
| `rt_sslr_temp` | RGBA16_FLOAT | Linear filtered RGB; w: receiver distance plus weighted ray-length estimate |
| `rt_sslr` | RGBA16_FLOAT | Final linear RGB; w: signed linear view-z |
| `rt_sslr_old`, `rt_sslr_hist` | RGBA16_FLOAT each | Alternating linear RGB and signed linear view-z |
| `rt_sslr_old_surface`, `rt_sslr_hist_surface` | RGBA8_UNORM each | RG: encoded world normal; B: roughness; A: material value from `s_surface.x` (metalness in modern lighting) |
| `rt_Reflection` | R11G11B10_FLOAT, 256x256x6 | Captured linear cubemap color |
| `rt_Reflection_temp` | R16_FLOAT, 256x256x6 | Linear radial distance from captured camera; invalid clear = -1 |
| `rt_Depth` | D16_UNORM, 256x256 | Shared cubemap-face depth; cleared to 1 before drawing |
| `rt_Reflection_forward` | RGBA8_UNORM_SRGB, 512x512 with mips | Octahedral forward environment encoding and fallback blend |

History color/depth targets clear to zero. History w = 0 means invalid/sky, w > 0 means world view-z, and w < 0 means HUD view-z. Negative history depth is valid HUD data: do not restore -1 as an invalid-history sentinel. Surface metadata is read only after color/depth validity succeeds. Temporal writes all three outputs for every in-bounds pixel, including sky.

Trace-data w uses a separate encoding:

```text
w = (24 - clamp(log2(PDF), -23.5, 23.5)) * (HUD receiver ? -1 : +1)
inverse PDF = exp2(abs(w) - 24)
```

Zero is reserved for skipped trace data. Its sign classifies the receiver, independently of the selected hit category. Storing raw inverse PDF in RGBA16_FLOAT can overflow; preserve the encoder/decoder pair. Trace RGB is bounded to 64000 for finite R11G11B10 storage and is not saturated to [0,1].

## Camera spaces and asynchronous VSLR

Keep asynchronous collection. `on_idle` snapshots camera position, direction, top/right, FOV, near plane, sector, environment far plane and VSLR distance before launching `PreRenderThread` and then running `FrameMove`. The latter updates the render camera. `collect_reflections` uses the snapshot, six private graphs and thread-local `PHASE_REFLECT` culling. Ticket completion uses release/acquire synchronization; `render_reflections` waits before primary-thread GPU submission.

The capture view and its inverse are exported separately from current view constants. For a point and direction:

```text
capturePoint = captureView * currentInverseView * float4(currentPoint, 1)
currentHit   = currentView * captureInverseView * float4(captureHit, 1)
captureDir   = captureViewRotation * currentInverseViewRotation * currentDir
```

Directions use rotation only. Radial tests and cubemap lookup use captured-view points. Accepted VSLR hits are transformed back to current view before filtering. Replacing these with current-camera orientation or rotation-only point conversion reintroduces motion misalignment. Matching coordinates does not make the earlier scene/camera snapshot identical to the current frame.

Face directions are captured right +/-X, top +/-Y and forward +/-Z. Up is captured top on +/-X/+/-Z; it is negative captured forward on +Y and positive captured forward on -Y. Each face projects at 90 degrees, aspect 1, with captured near and `capturedFar * capturedDistance` far clip. `r4_vslr_distance` ranges from 0.4 to 1.0, defaults to 0.7, and now controls collection and drawing rather than leaving the old hardcoded 0.4 clip. The exported radial bound is face far clip * sqrt(3), further limited by fog in the tracer.

Capture draws supported reflection shader graph 0, graph 1 and sorted geometry, restores the current matrices, converts element 3 into the forward octahedral map and generates mips. Color clears to zero and radial distance to -1 on every face. With no captured sector, export an invalid identity capture and clear the forward map to (0,0,0,1), then generate mips; shaders select environment fallback rather than stale geometry.

`CBPass`/b5 contains `m_reflectionV[3]`, `m_invReflectionV[3]`, `reflection_params` (radial radius, capture-valid flag), and `reflection_history_jitter` (previous SSR jitter xy, consecutive-history-valid z). Keep CPU field order and HLSL layout identical. `SetReflectionCapture` and `SetReflectionHistory` mark pass constants dirty. Prior SSR jitter is saved after its temporal dispatch, not inferred from arbitrary `UpdateView` calls: cubemap rendering temporarily clears jitter and changes view state several times.

## Shared tracing, water and filtering

`TraceScreenReflection` receives an unjittered current-view point. It uses the appropriate world or HUD projection, clips toward-camera rays against the near plane, clips the projected segment to the screen, and adds current raster jitter for depth lookup. Interpolate point/w and 1/w, then divide; linear interpolation of view depth along screen UV is incorrect under perspective.

The march uses at most 64 main samples, with up to five binary refinement steps for a candidate. Long projected rays use strided samples; this is not guaranteed contiguous per-pixel traversal and can miss thin geometry. Only the deferred trace compiles tile-min rejection. Depth samples are point-filtered. World rays skip HUD samples and continue. Intersection compares view-z intervals using thickness `max(world 0.05 / HUD 0.005, sceneZ * 0.01)`, refines in the correct increasing/decreasing-z direction, and rejects residual error, nonfinite points and self-intersections. Screen misses retain zero confidence.

`ReflectionHit` carries point, UV, raw depth and confidence. Receiver category and hit category must remain distinct. On a HUD local miss, including screen exit, a forward-going ray may use the retained world-direction fallback. Keep its original world UV/depth; do not overwrite them with a HUD reprojection. This fallback is directional and does not prove a finite ray intersection from the weapon surface.

Current scene lookup rejects invalid confidence/UV before sampling the current hit UV directly. Deferred SSR and water share the same current-frame source. This color lookup no longer depends on motion vectors or previous-scene visibility; temporal accumulation still has its own disocclusion and motion limits.

VSLR runs only when SSR confidence is below one. It uses the captured-space sphere bound, 20 geometrically growing steps (factor 1.25), point radial samples, and five refinement steps. Invalid radial coverage resets the bracket. A refined crossing within `max(0.05, depth * 0.01)` snaps to the captured radial surface and returns full confidence, provided it lies forward of the ray origin and within the trace bound.

When strict refinement misses, preserve the original VSLR gap-filling role. Track the valid captured surface closest to the finite reflection ray, ranked by squared perpendicular distance divided by captured depth squared. Snap the march direction to its positive finite radial depth, reject backward/self candidates (forward distance <= 0.025) and candidates beyond the trace bound, then return it with the original distance/fog fade `1 - saturate(2.5 * depth * fog_params.w + fog_params.x)`. This is an approximate captured-color fallback, not a proven ray intersection. Uncovered/invalid samples still cannot produce a fallback. After a full march without a refined hit, require positive finite radial coverage at the finite ray endpoint before using nearest geometry. An uncovered endpoint is a sky/environment miss; returning an earlier terrain candidate there incorrectly replaces sky. Refined nearer hits return before this endpoint check. Capture transforms and deterministic marching remain in effect.

The nearby-foliage follow-up adds a bounded neighboring-cube fallback for a specific occluded miss: strict tracing found no hit, no nearest forward candidate survived, the terminal direction has valid radial coverage, and that covered surface is behind/self-relative to the reflection ray origin while lying in front of the endpoint. A close leaf can hide a distant water reflection in both the screen depth and the camera-centered single-layer capture. Rejecting that leaf is correct, but searching only the exact march directions can leave no gap-filling geometry.

On this case only, build a tangent basis around the captured endpoint direction and search eight neighboring directions at nominal one-, two- and four-texel offsets (24 extra point depth reads maximum). Texture dimensions determine the angular step; cube sampling crosses face seams without a face-UV clamp. Neighbors must lie farther than the blocking surface, be positive/finite, project forward onto the finite reflection ray, and fall within the bounded depth-normalized perpendicular error. Select the lowest-error candidate and reduce its confidence toward the search bound, then apply the existing distance/fog fade. The returned captured surface point selects its matching color through the existing caller; this does not sample screen color from the wrong pixel.

Strict hits and existing nearest candidates retain their prior behavior. Uncovered terminal directions still return weather sky before this search, so the previous terrain-over-sky regression is not reopened. SSR intersection validation, asynchronous collection, targets and history layouts are unchanged. The search is an approximation: it can borrow adjacent visible scenery at small foliage gaps, but it cannot recover scenery fully hidden throughout the neighborhood. Wide leaf silhouettes can still fall back to sky. This follow-up is implemented pending user shader compilation and runtime verification; the earlier "Works now" report predates it.

The capture cubemap contains geometry only: uncovered color is zero and distance is -1. Sky comes from the weather textures, never from those cleared color texels. Deferred sky fallback rotates the world reflection direction by `L_sky_color.w` before the existing sky sampling policy and uses full sky coverage with offscreen reflections, matching the original offscreen hemisphere behavior. Water retains its existing rotated/weather-blended sky path. The radial coverage test cannot distinguish missing geometry from true sky in a single-layer capture; preserving real nearest hits and avoiding sky smearing still requires runtime checks at the horizon.

The first 2026-10-03 revision removed this fallback and required a tight crossing for every VSLR contribution. The user reported excessive sample loss and clarified that the original nearest captured samples intentionally filled gaps; the strict-only policy was a regression.

Water calls the same screen tracer instead of its old fixed-point/clamped-UV loop. Its normal offset transforms the world normal into view space (`m_V * Nw`, scale 0.07), and its VSLR lookup uses capture coordinates. Removed receiver shell shrinking does not move the ray origin toward the camera. Water exports camera motion from current projection and previous world clip, with finite/w checks and alpha-derived velocity coverage; it no longer writes zero camera velocity.

The deferred filter uses 16 disk samples. Reject skipped, nonfinite or wrong-receiver-category data before arithmetic; guard half-vector normalization and decoded weights, and retain the center color when the denominator is too small. RGB stays linear through trace, filter and temporal. `combine_1` converts it to gamma for the existing ambient interface, whose implementation performs its own gamma-to-linear conversion. Preserve this boundary when changing either consumer.

The forward VSLR map decodes an octahedral world direction and rotates it into captured view. Its color retains the existing gamma-domain `r / (1 + r)` packing expected by the ambient consumer; its fallback alpha treats invalid radial coverage or capture as environment. Do not apply deferred linear-history assumptions to this packed map.

## Temporal validity and motion

Temporal reconstructs the receiver at the same jittered pixel center as trace. Rough receivers and HUD use receiver motion. With motion vectors, remove current jitter, apply velocity, then add the jitter saved by the previous SSR dispatch. Without vectors, world receivers use previous camera projection; HUD history is disabled. History is valid only if SSR ran on the immediately preceding `Device.dwFrame`.

For world receivers with roughness <= 0.1, mirror the selected current trace hit across the receiver's local plane, transform that virtual point into world space, project it through `m_VP_old`, and add previous SSR jitter:

```text
virtualPoint = hitPoint - 2 * normal * dot(hitPoint - receiverPoint, normal)
previousUV = project(previousVP * currentInverseView * virtualPoint) + previousSSRJitterUV
```

This replaces the first revision's receiver-only glossy reprojection. It uses the existing `rt_sslr_data` SRV and adds no history target or constant-buffer fields. Skip virtual-hit reprojection for skipped/nonfinite data or the fog-distance miss point. Roughness selection retains the original 0.1 threshold. The old total-path-length scalar is not treated as a fabricated world hit.

Gather the four bilinear history taps explicitly. Validate each tap's finite color, nonzero matching depth sign, surface normal and material before it contributes; renormalize valid color weights and retain the surviving weight as confidence. Do not validate one point-sampled depth and then bilinearly blend unchecked neighboring colors.

For each world tap, intersect its previous unjittered camera ray with the current local receiver plane. Transform the homogeneous world plane by `m_invVP_old`, solve its raw clip depth at that tap, and recover expected linear view-z as the reciprocal inverse-projected homogeneous w. Reject near/far-invalid, parallel or nonfinite intersections. This provides a separate expected depth for each tap, including slopes and the displaced glossy history UV. HUD retains current receiver z as an approximation. Require depth agreement within `max(world 0.05 / HUD 0.005, expectedZ * 0.01)`.

Decode previous world normals and reduce tap weight between dot 0.9 and 1.0; roughness/material differences also reduce it. Clamp accepted history against all nine same-category finite colors in the current 3x3 neighborhood, with edge coordinates clamped. Base history weight ranges from 0.85 to 0.95 with roughness, then decreases with surviving tap confidence, relative color change and pixel motion (`exp2(-motion * 0.1)`). Remove the current/previous jitter difference before measuring motion so a static camera's jitter does not count as movement.

The local mirror/receiver plane is an approximation for curved or strongly bump-mapped receivers, stochastic glossy rays, moving reflectors and moving reflected objects. HUD pose and rough-receiver moving-object depth also remain approximate. The original revision's single-depth rejection plus unchecked bilinear color and receiver-only glossy motion were source defects. The user has since reported that the combined implementation works; these approximations still require attention when extending it to new receiver or motion cases.

SSR itself does not enable motion vectors. `NeedMotionVectors()` is snapshotted on level load; set AA/upscaler/motion-blur settings before `start`, as described in [rendering.md](rendering.md) and [lifecycle.md](lifecycle.md).

## Maintenance traps

Blender element numbers are local to each shader, not global pass IDs. `s_combine->E[4]` is the current-frame source pixel pass; `s_sslr->E[4]` is depth-min compute. `s_sslr->E[3]` converts the captured cube for forward use. A valid element number in the CPU caller is insufficient if the blender never creates its passes. The original `this = 0x1C` crash was a CPU-side missing-element dereference, despite being observed on D3D12; changing `FixedVector` bounds behavior or adding a blanket null skip would leave the unfinished pipeline unresolved.

`SSLR_SOURCE_PASS` must be added before compiling combine element 4 and cleared after that element. `CRender::getShaderParams` includes option names/values in cache identity, so the source and regular combine must remain distinct permutations. If the option leaks into element 0, normal combine loses deferred SSR; if absent from element 4, preparation reads the result it is about to produce. The recorder's unconditional `s_refl` binding line is harmless for the source permutation because `r_dx10Texture` first checks compiled resource reflection. Inspect the compiled resource list rather than inferring an active read from that binding line alone.

Graphics output changes are queued by the RHI render-view manager. `CBackend::Compute` applies resources/state/constants and dispatches, but does not apply queued graphics targets. Preserve the explicit `ApplyRenderTargetChange` after restoring `rt_Generic_0` and before SSR compute. Unbind outputs before the background copy and before consuming the source as an SRV. Existing RHI code handles backend resource transitions; this change did not introduce a separate D3D12 barrier path. Water must keep reading the separate immutable source even though regular combine has already updated the main color target.

Similar-looking depth/alpha values have different contracts:

| Value | Meaning | Invalid/category rule |
| --- | --- | --- |
| G-buffer `O.Depth`, tile minimum, `ReflectionHit.Depth` | Raw hardware depth | Sky >= 1; HUD < 0.02 |
| Capture distance | Linear radial distance from captured origin | -1 clear; require positive finite coverage |
| Final/history w | Signed linear view-z of receiver | 0 invalid; negative HUD; positive world |
| Trace-data w | Signed biased log-PDF | 0 skipped; sign is receiver category |
| Filter w | Receiver distance plus weighted ray-length estimate | Not a stored reflected world point |
| `ReflectionHit.Confidence` / VSLR return w | Contribution weight | 0 is a miss; nearest VSLR is approximate |
| Forward-map alpha | Environment fallback blend | Invalid capture selects fallback |

Do not use a trace hit's category as the receiver's history/filter category. Do not compare raw depth, view-z and radial length without conversion, or replace stored radial length with squared distance just because the crossing test squares it. Formats, UAV declarations, binding counts and consumers must change together; removing trace alpha requires retaining category information in point/PDF data.

Jitter constants are in clip/NDC units. Their UV conversion is `float2(0.5, -0.5)`; velocity uses its own NDC-to-UV sign conversion. Current source lookup already uses the jittered hit UV. History lookup adds the saved previous SSR jitter, while per-tap plane validation removes it to reconstruct the previous camera ray. Cubemap view updates must not overwrite this saved history state. Measuring motion without subtracting the jitter difference makes a stationary receiver look mobile.

The strict VSLR intersection and nearest fallback serve different purposes. Keep finite/category/coverage guards, but do not turn the fallback into another tight intersection test: that recreates the user-reported holes. Conversely, searching nearest captured geometry without terminal coverage can smear terrain across sky. Distance uses point sampling to avoid interpolating geometry with the -1 sentinel; captured color uses linear filtering. Color filtering can still blend valid geometry with a cleared neighboring texel at coverage edges. This residual edge risk has not been separately measured or eliminated.

## Diagnosing similar artifacts

| Symptom | Check first |
| --- | --- |
| Screen reflections trail moving geometry while VSLR responds promptly | Source age and bindings: trace/water must read `rt_sslr_scene` at current hit UV. Previous matrices cannot repair stale object color. |
| Glossy reflections drift with camera translation despite current source color | Virtual-hit reprojection and per-tap receiver-plane depth; receiver motion alone does not describe reflected-image motion. |
| Water changes abruptly when pitching through the horizon | Perspective Q/K interpolation, near/screen clipping, current jitter, capture-origin conversion and the sky/nearest-fallback boundary. |
| Holes or loss around weapon outlines | Distinguish HUD receiver from world hit; preserve the fallback hit's UV/depth and category; verify filter/history signs. |
| Offscreen terrain disappears after stricter rejection | Nearest-surface fallback must still run on strict misses with valid endpoint coverage. Missing grass requires a capture implementation, not relaxed depth tests. |
| Sky contains terrain, black patches or wrong orientation | Endpoint radial coverage, geometry-only cube clears, weather sky rotation and offscreen hemisphere policy. |
| Intermittent history failures after reset, skipped frames or camera state changes | Paired history selection, zero clears, consecutive-frame flag, saved SSR jitter and all three UAV outputs. |
| One game variant behaves differently | Active shader overlay and compiled permutations; shared shader equality does not cover intentional water/combine differences. |

First distinguish trace coverage, current source color, history accumulation and offscreen capture coverage. Increasing history weight or loosening every depth test can hide one symptom while retaining the wrong source or accepting another surface. The earlier attempts showed why these contracts need to be checked separately.

## Costs and remaining limits

The user confirmed working rendering, but no speedup has been measured. The added source work is one full-screen combine plus one background copy, not a second geometry/shadow/per-light render. RGB trace storage, tile rejection and removed history copy can save work, while the larger screen march, extra metadata, expanded capture coverage, additional reflection draw lists, nearest VSLR search, up to 24 neighboring depth reads on foreground-occluded misses, four validated temporal taps and current-frame source pass/copy can add work. The RGBA16 current-frame source adds eight nominal bytes per render pixel; two RGBA8 surface histories add another eight, excluding allocation alignment. The 64 main screen steps and refinement are also used by water, whose old loop had 15 iterations. Compare equivalent resolution, scene, camera, settings and API before reporting a performance result.

Detail grass still has no VSLR capture path. `render_reflections` does not draw `Details`. Its worker collection uses the main-camera frustum/cache and double buffers; non-normal rendering selects detail shadow shader variants in `dx10DetailManager_VS.cpp`. Adding `Details->Render` in the cube loop is insufficient. It needs reflection color/radial-distance shaders and an agreed per-face or all-direction collection/ownership design that preserves asynchronous cache safety. That extension has not been implemented or settled.

Other limits are screen visibility and strided SSR misses, 256x256 single-layer radial cubemap coverage, reflection shader/SSA culling coverage, approximate HUD direction fallback, local-plane temporal approximation, missing forward-source contributions, and temporal disocclusion. These are distinct from camera-space alignment. Do not claim that alignment alone provides complete offscreen geometry or eliminates every discarded sample.

## Validation record and regression checks

On 2026-10-03 the user reported "Works now" after the latest source/sky changes. No capture, benchmark or per-API/overlay/configuration matrix accompanied that report. Treat it as user runtime confirmation for the tested scenario, not exhaustive verification. The checks below describe future regression coverage; they are not a record of checks already executed.

Use Debug for diagnosis and Debug plus RelWithDebInfo for final checks; never Release. Do not add temporary probe switches or instrumentation. Compile the affected renderer and shader permutations for the active game overlay. Use a loaded level and the existing [autotest](lifecycle.md) for repeatable output/timings; a menu-only pass does not exercise reflections. Inspect actual rendered captures before marking the visual fix complete.

1. Reproduce the supplied water/weapon view with the same resolution, settings and API. Slowly pitch up/down through the horizon and shoreline; inspect water continuity and SSR samples near the weapon silhouette. Repeat pure rotation and translation separately, then combined motion, to distinguish capture orientation from capture-origin errors. Check glossy and rough receivers and screen edges.
2. In RenderDoc, confirm `sslr_scene` copy/lighting precedes the four compute events and regular combine. Element 4 must exclude SSR reads; deferred trace and water must bind the same current-frame source SRV. Its copy must match dimensions/format, and it must be unbound as an output before compute or forward sampling. Confirm the four compute events and dispatch sizes. Tile reduction dispatch equals tile dimensions; other groups equal ceil(render dimensions/8). Outputs per stage are 1, 2, 1 and 3 UAVs. Check the last partial tiles at a resolution not divisible by eight.
3. Across adjacent frames, verify element 2/5 alternation, matched surface-history selection, stable final target and no simultaneous input/output use of one history. First frame, reset/reallocation and a frame gap must reject history. Examine signed linear view-z independently of raw hardware depth; zero is invalid, negative is HUD.
4. Inspect all six cubemap faces: empty faces must clear, radial distance must be linear with -1 uncovered pixels, face orientation must match the captured basis, and color/distance must describe the same capture. Compare the pass's capture matrix/origin against the earlier snapshot rather than the updated main view. Check no-sector fallback and normal current-view restoration after capture.
5. Inspect trace/filter/history values for finite linear HDR color, valid point/PDF encoding, and finite filter weights. Bright reflections may exceed one. Check same-category filtering, per-tap depth/normal/material rejection on slopes, changing camera jitter, glossy virtual-hit UVs under translation, motion-vector UVs and the water velocity coverage. For strict VSLR misses with valid captured geometry, inspect the nearest-surface fallback and its distance fade; invalid radial coverage must remain rejected. Reproduce the nearby-foliage view with the camera close to and then clear of the bush; check recovered reflection continuity, leaf-color bleed, wide hidden regions and cube-face seams. Confirm the neighbor search only runs on the documented occluded miss and costs at most 24 extra depth reads. Check long sky rays and horizon transitions: uncovered endpoints must use weather sky rather than earlier terrain, and deferred sky rotation must follow the visible sky. Verify the forward octahedral map uses the capture rotation and its separate packed color contract.
6. Test settings with motion vectors enabled before level load and settings without them. World camera reprojection should remain valid; disabled-vector HUD history should reject. Check moving geometry and disocclusion for the documented approximate-history limits rather than assuming they are solved.
7. Check moving reflected objects and camera translation against VSLR to confirm the stale scene-source delay is removed; distinguish any remaining temporal ghosting from an old input texture. Compare D3D11 and D3D12 at identical workloads, then each game overlay used in distribution. Measure GPU events and frame times after warmup before calling the optimization complete. Record capture evidence, settings, configuration and outstanding artifacts here when results are available.

Static verification to date: whitespace diff checks, UTF-8/CRLF and document-link checks, byte equality for the five shared reflection shaders across all four game sets, inspection of CPU/HLSL bindings and layouts, and source-order/binding checks for copy -> source combine -> SSR -> regular combine -> forward. These are separate from the user's runtime confirmation and do not establish cross-configuration resource correctness or performance.
