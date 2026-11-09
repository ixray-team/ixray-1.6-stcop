# Plan: unifying render graph building

Reference: `D:\ixray-team\ixray-render-graph-manager`, `09b81bde4..1b880ce3b`.
Target branch: `feature/parallel-model-load` (`ad7e2da039`, 2026-11-06). R4 only: R1 and R2 are no longer in it.
Paths below are for this branch, with the `src/Layers/xrRenderPC_R4/` prefix omitted. Everything was checked by reading the code; nothing was run in game.

## Status

| Stage | State |
| --- | --- |
| 1–2. View snapshot, cascades without waiting | Committed (`00e3592d27`) |
| 3–6. `Begin`, `Build`, `CollectGraphs`, `GraphRain` on the worker, each pass has its own graph, one main-scene collection | Committed (`d425ce2772`) |
| Icons in `GraphUI`, view snapshot for `GraphMain` | Committed (`8c55fdea9f`) |
| `IDSGraphManager` interface for `xrGame` | Committed (`42357ce0ea`) |
| 7. Stable shadow statics | Not done: no measurable benefit |
| 8. More work on workers | Not committed: `r__mt_main_static`, no gain so far |

Differences from what is described below:

- One `GraphPass` for all sequential main-thread passes instead of `GraphShadow` and separate graphs: projectors, puddles, dynamic lights of the static mode.
- `smapvis` removed: `svis.begin()` was placed after the build, and `end()` dropped the feedback before the next insertion, so `invisible` was always empty.
- `ZPrefill` is stored in `GraphView`, not in a global flag.
- `GraphMain` gets its view snapshot in `BeginMainCollect`; deduplication for all graphs goes through `private_visuals`. Only `GraphUI` reads the live camera: `UI3dStatic` substitutes it while drawing an icon.
- The editor keeps two collections. It was not checked: `xrECore` does not build on this branch (`EStats.cpp`).

**Stage 7.** `r4_spot_shadow_cache` already stores the list of statics without portal traversal, and the depth. With the flashlight in modes 0, 1 and 2 the frame takes 1.68, 1.69 and 1.78 ms. A separate CPU graph cache would duplicate this. A scene with static lamps was not measured, so the decision can be revisited on one.

**Stage 8.** The main scene needs the current camera, so its statics cannot start before `FrameMove`, and dynamics cannot be collected during `OnFrame` (section 4). The only remaining option was tested: the main scene's statics are collected on a worker into a separate graph in parallel with dynamics on the main thread, then `mapNormalPasses` are swapped and the sorted lists are merged. Alternating measurement against `42357ce0ea`:

| Scene | Runs | `calc`, ms | `total`, ms | p95, ms |
| --- | --- | --- | --- | --- |
| Zaton | 6 and 6 | 0.318 → 0.392 | 1.50 → 1.57 | 2.42 → 2.68 |
| Limansk, CS | 3 and 3 | 0.457 → 0.432 | 2.82 → 2.61, ranges overlap | 3.68 → 3.62 |
| Army Warehouses, CS | 3 and 3 | 0.222 → 0.249 | 1.04 → 1.07 | 1.46 → 1.82 |

Collection already takes a small share of the frame, and the task launch and merge eat up the savings.

Second option: statics start right after `Calculate`, alongside the cascades on `PreRenderThread` and everything the main thread does before the main scene. Command `r__mt_main_static`, measured in one binary, `total` on / off, markup: median and p95, ms.

| Scene | Runs | Median | p95 |
| --- | --- | --- | --- |
| Zaton | 6 and 6 | 1.224 / 1.233 | 1.67 / 1.77 |
| Limansk, CS | 3 and 3 | 3.03 / 2.37 | 3.92 / 2.86 |
| Army Warehouses, CS | 3 and 3 | 1.00 / 1.00 | 2.22 / 1.30 |
| Hospital, CS | 3 and 2 | 0.47 / 0.49 | 0.67 / 0.65 |

No gain; worse on Limansk and the warehouses, cause not found. Not checked: `IXRAY_ASAN`, Debug, render scenarios with the second option, projector shadows.

**Check of stages 3–6** on Zaton: draw calls and polygons match `594f1f4cc2` in the scenarios clear weather, rain, sun cache, `r2_zfill`, without `r2_exp_splitscene`, flashlight, shadow cache 1 and 2, static lighting with and without the flashlight. `disconnect` and `vid_restart` without crashes. `total_ms` 1.26–1.46 ms versus 1.11–1.18 ms. Not checked in the autotest: 3D scope, puddle reflections (`R4FLAG_PUDDLES`), `decor_object`, a heavy scene.

## 1. Goal

Graph building must be **one mechanism that works for anything**: the main scene, cascades, rain, SSLR, projector shadows. A new pass is a new view description, not a new copy of the code.

In `RenderGraphManager` this works as follows: a graph is a view object with the same `traverse` → `capture_*` → `render_*` loop, and the differences are set by parameters (`CDSGraphManager GMRainWet`, `GMBase`, `GMRefl`, `cascade.GMCascade`, `light.GMLight`).

## 2. Where we are now

The branch has already gone part of the way. The reference did not have the following, and it must be kept:

| Already present | Where |
| --- | --- |
| Graph objects: `GraphMain`, `GraphSun[3]`, `GraphReflection[6]` | `r4.h:118-120` |
| Each graph has its own `CPortalTraverser` and local `own_clips` | `r__portal_traverser.h` |
| Real multithreading: cascades and reflections are collected on `PreRenderThread` | `R_sun.cpp:213`, `r4_rendertarget_phase_sslr.cpp` |
| `renderable_Render(void* graph)`, `::Render->add_Visual(..., graph)` | `IRenderable.h:25`, `xrEngine/Render.h` |
| `SkeletonBatch`, compute skinning, `GfxCmdList`, MU LODs, shadow caches | |

What is missing, that is, where unification has not been done:

| Problem | Where |
| --- | --- |
| Rain, projectors, `decor_object`, shadow cache, ZPrefill and static lighting go through `GraphMain` and take turns using its maps | `r4_R_rain.cpp:203-226`, `lights_render.cpp:115-213` |
| View setup is copied by hand in every place | `R_sun.cpp:242-252`, `:476-486`, `r4_rendertarget_phase_sslr.cpp:254-260` |
| Two traversal modes: `own_clips` and the global `CSector::r_frustums`, with branching in every consumer | `r__sector.cpp:89-110`, `r__dsgraph_render.cpp:442-456, 479-481` |
| Two deduplication modes: the shared `marker` and `private_visuals` | `r__dsgraph_build.cpp:420-436` |
| View parameters live in TLS, and not all of them: `R_CullTLS` covers the phase and SSA, while the camera, FOV and fog are read globally | `r__dsgraph_structure.h:18-27`, `r__dsgraph_build.cpp:208, 222, 362, 929, 966` |
| Each pass has its own launch protocol: `sun_ticket`, `reflection_ticket`, `Collect*` delegates | `r4.h:246-257`, `EngineThreading.cpp` |
| `ViewBase` is substituted in the non-isolated mode, although `ParticleEffect.cpp:151` reads it from the particle simulation | `r__dsgraph_render.cpp:447-452` |
| Collection is not separated from traversal: `append_sun_dynamics` traverses the portals of the same cascades again | `R_sun.cpp:487` |
| Two main-scene collections per frame | `r4_R_render.cpp:620, 879` |
| Cascades wait for the main thread (see section 4) | `R_sun.cpp:216-220` |

## 3. Target model

A view description is a snapshot of everything the build needs. After `Begin`, the build does not read `Device`, `Environment` or `RImplementation.phase`, and does not depend on which thread it runs on.

```cpp
struct GraphView
{
	u32 Phase;                      // PHASE_NORMAL, PHASE_SMAP, PHASE_REFLECT
	IRender_Sector* Sector;
	CFrustum Frustum;
	Fmatrix Transform;
	Fvector Origin;                 // portal traversal origin
	u32 TraverseOptions;            // VQ_HOM, VQ_SSA, VQ_FADE
	ESPATIAL_TYPE DynamicTypes;
	bool PassMask[3];               // deferred, forward, wallmarks
	bool DepthOnly;                 // currently the global RImplementation.ZPrefill
	CObject* Ignore;

	// Camera and environment snapshot, replaces R_CullTLS and reads of Device
	Fvector CameraPosition;
	float Fov, FogDistance, FarPlane;
	float SsaDiscard, SsaLodA, SsaLodB;
};

class R_dsgraph_structure
{
public:
	void Begin(const GraphView& View);   // clear and unified view setup
	void Traverse();                     // portals, once per build
	void CollectDynamic();               // objects, SkeletonBatch launch
	void CaptureStatic();
	void CaptureDynamic();               // Wait, renderable_Render(this)
	bool IsSectorVisible(const CSector* Sector) const;

	void r_dsgraph_render_static(u32 Priority, bool Clear = true);
	void r_dsgraph_render_dynamic(u32 Priority, bool Clear = true);
	void r_dsgraph_render_graph(u32 Priority, bool Clear = true);   // both parts
};
```

Instances differ only in their description:

| Graph | Phase | Geometry | Build thread |
| --- | --- | --- | --- |
| `GraphMain` | NORMAL | everything, HUD | main |
| `GraphSun[3]` | SMAP | statics on the worker, dynamics later | `PreRenderThread` |
| `GraphReflection[6]` | REFLECT | statics | `PreRenderThread` |
| `GraphRain` | SMAP | statics | `PreRenderThread` |
| `GraphShadow` | SMAP | everything | main, can move to the worker later |
| `GraphZPrefill` | SMAP, `DepthOnly` | priority 0 | main |

**Launching on the worker** is also unified: `CollectGraphs` walks a list of "graph + description" requests, instead of `CollectReflections` and `CollectSunCascades` with their tickets. Before rendering, one join point remains: that is the end of the build, not idle time.

**Interface with the game.** Passing `graph` is already done, so we keep it. Separately, `void*` can be replaced with a typed pointer and the `nullptr` default removed, since it silently sends the object to `GraphMain`. Currently only the HUD and UI pass `nullptr` (`player_hud.cpp`, `CutsceneItem.cpp`, `UI3dStatic.cpp`), so this is not urgent.

## 4. Remove the cascade wait

In the profiler, `Collect Sun Cascades` sits idle for the whole of `OnFrame`. The cause: `PreRenderThread` starts before `FrameMove`, and the cascade matrices are published later, from `CRender::Render` (`r4_R_render.cpp:507`). Reflections do not do this: `BeginReflectionCollect` snapshots the camera before the thread starts (`device.cpp:237`).

**What we do:** compute the matrices and publish the request before `PreRenderThread.Run()`, as reflections do. `sun_ticket`, `EnsureSunCollect` and the `wait` at the start of `collect_sun_cascades` are removed.

What to take into account when moving it; all items were found in the code:

1. **The build reads the global camera.** `CalcSSA`, `IsValuableToRender`, `FullDetailRejectStatic`, fog and MU LOD selection take `Device.vCameraPosition`, `Device.fFOV` and `CurrentEnv` directly during `OnFrame`, which changes them. So the `GraphView` snapshot (stage 1) comes **before** the wait is removed. Reflections already read these fields in parallel with `OnFrame`, and the snapshot fixes that as well.
2. **`dwFrame`.** `FrameMove` increments it after publication. `prepare_sun_cascade_xforms` compares `LastSunUpdateFrame != Device.dwFrame` (`R_sun.cpp:337`), and `LastSunUpdateFrame` is written during rendering (`:580`). With a plain move the condition becomes false and the sun cache stops updating. The target frame number has to be passed in.
3. **`BeginSunShadowFrame`** (`r4_R_render.cpp:505`) moves together with publication.
4. **Early exits from `Render`.** The menu, `m_bFirstFrameAfterReset` and static lighting are now known only after the build has started. The launch condition has to be checked in advance. An extra build is harmless: the next one clears the graph.
5. **Matrices lag the camera by one frame.** Accum uses the same `Cascade.xform`, so there is no mismatch; only the coverage shifts. The sun cache already lives with old matrices. Check a sharp turn and a teleport.
6. **`g_StaticTraversalStats.Current`** is read in the build from any thread (`r__dsgraph_build.cpp:930, 1039`). Build counters from the worker must not end up in the main view's statistics.

**Unload and reset.** The current wait also protected against `disconnect` and `vid_restart`: both run inside `FrameMove` and delete what the build reads. Instead of a per-frame lock, `level_Unload` and `reset_begin` call `PreRenderThread.Wait()` once. Frames without an unload wait for nothing.

**Lock with the destruction stage.** Not introduced in stages 1–2. `CObjectList::Update` destroys objects in `xr_object_list.cpp:263-317`, and once the worker walks dynamics, that section will have to take an `xrSRWLock` exclusively, and the build will take it for reading while it works with objects.

The current cascade build on the worker is statics only: sectors, portals and level visuals outlive objects. So the lock protects nothing there, and there is no reason to hold it for the whole build; otherwise frames with destruction would wait for the worker. Models and particles are already cleaned up before the thread starts (`device.cpp:205-215`).

**Open question about dynamics.** The lock protects against deletion, but not against modification. `CObjectList::Update` in the same `OnFrame` moves objects and animation, so collecting dynamics in parallel with it can read `XFORM` and bones in the middle of an update. Therefore `append_sun_dynamics` stays after `OnFrame` until a separate measurement and analysis in stage 8 show otherwise.

## 5. What not to take from the reference

| Defect in the reference | What we do |
| --- | --- |
| Sorted lists keyed by `dxRender_Visual*` are walked in insertion order, back-to-front is lost (`r__dsgraph_build.cpp`, `r_dsgraph_render_graph_sorted`) | Key `distSQ` and `traverseRL`, as now |
| `SNODE` dangles after `Realloc` at 64 nodes, because recursion inserts into the same map (`r__sector.cpp:98,106,165`) | `local_frustums` by sector index, as now |
| Light statics are frozen together with camera filters until the light moves (`m_moving_frames`) | Stage 7 |
| No invalidation on shaders and textures | Revisions from the shadow cache |
| `o.distortion == false` at collection time, the Distort lists are empty (`r4_R_render.cpp:259,285,418`) | With a single collection, the condition depends on `distortion_enabled` |
| `Fmatrix*` in the item | A copy of the matrix, as now |
| A single `DSGraphItem` | `_NormalItem` with resolved geometry, as now |
| A light query and spin in `vis_update` | `R_occlusion`, as now |
| The old portal selection | `OnPath`, `Visits`, `IsNearPortal`, as now |

## 6. Stages

Each stage builds, runs and produces the same picture, except where noted.

### Stage 0. Baseline

Capture with `-autotest` and fixed cameras: frame time, a profiler capture, draw calls per pass, screenshots. Scenarios are in section 7.

### Stage 1. View snapshot

**Done, not committed.** `GraphView` stores the view snapshot; `R_CullTLS` is removed. The build reads it through accessors that check `OwnsView`. `GraphMain` has no snapshot yet and reads live values: `UI3dStatic` substitutes the camera while drawing icons. This branch goes away in stage 6. `ZPrefill` stays global until stage 5: only `GraphMain` reads it.

**Done when:** build functions do not read `Device`, `CurrentEnv` or `RImplementation.phase`.

### Stage 2. Remove the cascade wait

**Done, not committed.** Section 4. Cascades are published before `PreRenderThread.Run()`; the only wait is the join in `render_sun_cascades`. The editor and unexpected frames are built synchronously on the main thread.

Measured with the built-in profiler, compared with `ad7e2da039`, Zaton, clear weather, median over 6 runs:

| Metric, ms | Before | After |
| --- | --- | --- |
| Wait for cascade start | 0.268 | 0 |
| `Collect Sun Cascades` | 0.396 | 0.116 |
| Whole frame | 2.297 (range 2.21–2.56) | 2.258 (range 2.22–2.33) |

The worker idle time is gone, and the build itself did not get more expensive. On this light scene the frame did not change beyond the noise: the join barely waited before either. Shadow coverage: with a camera jump of 0.2 rad per frame the difference is within noise; at 1 rad per frame there are no holes, and bushes near the camera differ. Not checked: a heavy scene, editor mode (`xrECore` does not build on the branch).

**Done when:** the profiler shows no idle time in `Collect Sun Cascades`, shadows match the baseline with a static camera and do not lose coverage on a sharp turn. The picture in motion may differ by one frame.

### Stage 3. Unified build: `Begin` and `CollectGraphs`

Replace the manual setup and the tickets of cascades and reflections with a single launch. Split `render_subspace` into `Traverse`, `CollectDynamic`, `CaptureStatic` and `CaptureDynamic`, and keep the old overloads as wrappers until stage 6. `append_sun_dynamics` uses the traversal that is already done instead of doing a new one.

**Done when:** cascades and reflections are described only through `GraphView`, and portals for a cascade are traversed once per frame.

### Stage 4. Rain as a test of unification

`GraphRain`: split `render_rain` into computing the matrices before the worker starts and rendering; the build goes through `CollectGraphs`. This is the simplest view and a test of the goal: adding a pass should be almost nothing but a new description.

**Done when:** rain does not touch `GraphMain`, is built on the worker, and no changes to the build code were needed.

### Stage 5. The remaining passes and one mode

`GraphShadow` for projectors, `decor_object` and the shadow cache. `GraphZPrefill`. Static lighting (`r1_LightPPA.cpp`) mechanically, through its own graph.

After this there is no non-isolated mode any more, and the following can be removed:

- `own_clips` as a mode: clips always belong to the graph.
- `CSector::r_frustums`, `r_marker`, `r_scissors`, the per-frame fields of `CPortal`, `VQ_SCISSOR`.
- The `ViewBase` substitution.

Consumers of `r_marker` (`Light_DB.cpp:224`, `light.cpp:228`, `DetailManager.cpp:218`, `DetailManager_Render.cpp:356`) move to `GraphMain.IsSectorVisible`. Currently shadows overwrite the `GraphMain` traversal; after this stage the answer will always be about the main camera. This is a behaviour change and must be checked separately.

Reduce deduplication to one mechanism: for statics, a per-graph bit array by visual index; for dynamics, `private_visuals`. Move or remove the shared `marker` and `light_smapvis`. `add_static` is the heaviest area in the profiler, so measure the cost of `private_visuals` (a hash per leaf) separately.

**Done when:** no pass except the main one writes to `GraphMain`.

### Stage 6. The main scene as an ordinary view, one collection per frame

`render_main` becomes `Begin` + `Traverse` + `Capture*`. The collection includes priorities 0 and 1 and wallmarks at once, so `render_forward` no longer traverses the scene (`r4_R_render.cpp:879`). Particles are collected separately before forward, the HUD once. `mapHUDScopeMask` must be known before the `split_the_scene` decision. Inserting Distort depends on `distortion_enabled`. Remove the `r_dsgraph_render_subspace` wrappers.

This is the riskiest stage: the two `render_main` calls with different masks carry many implicit dependencies. Move them one at a time and compare draw calls.

**Done when:** one traversal per frame, the picture matches the baseline, `RenderCALC` has dropped.

### Stage 7. Separate and stable statics

`r_dsgraph_render_static`, `r_dsgraph_render_dynamic`, `ClearStatic`, `ClearDynamic`. Rendering with `Clear=false` must not change the graph, so the graph can be reused. Do not split the sorted lists: statics and dynamics are sorted together.

The stable graph is stored in `SpotShadowCacheEntry`, which already has a memory limit and classification. Only fixed geometry goes into it, collected without camera filters. The `MT_MESH_LODS` and `MT_LOD` groups stay per-frame: LOD selection depends on the camera. Invalidation: light movement, sector change, level load, device reset, texture and shader revisions. Decide from measurements whether `smapvis` is still needed. Cascades likewise, if a measurement shows a gain.

**Done when:** there is no repeated traversal for a static light, and shadows are correct when the camera moves and on LOD transitions.

### Stage 8 (optional). More work on workers

`CaptureStatic` of the main scene and of projector shadows can be parallelised. For dynamics on the worker, first resolve the open question from section 4. Check `renderable_Render` for purity: `CalculateWallmarks`, `CROS_impl::update`, `hom_frame`, `Skinning.Request`, `Lights.add_light`, `L_Shadows`. Rendering stays on the main thread.

**Done when:** a clean run under `IXRAY_ASAN` and a measurable gain. Without a gain the stage is not merged.

## 7. Verification

The autotest from `docs/engine/lifecycle.md` and manual scenarios with a fixed camera:

- Portals: on a threshold, through several sectors, underground, `rs_render_portals`.
- Order and transparency: sorted, Distort, emissive, wallmarks, particles, including the HUD.
- HUD and scopes: `mapHUDScopeMask`, HUD light, 3D icons.
- Lights: omni with shadow, `decor_object`, `ignore_object`, `r4_spot_shadow_cache` 0–2, `r4_sun_shadow_cache` 0–2.
- Cascades in motion: sharp turn, teleport, level change.
- LODs: `MT_MESH_LODS` transitions, billboards, shadows at the moment of transition.
- Modes: static lighting, editor, `R2FLAG_ZFILL`, `R2FLAG_EXP_SPLIT_SCENE`, `r__mt_skeletons off`.
- Lifecycle: device reset, level change, save/load, destroying an object during the build. For the last one, `IXRAY_ASAN`.

## 8. Out of scope

Not carried over from the reference: asynchronous cform loading, `Feel_Vision` and the AI changes; they are not related to the graph. A separate light database `g_SpatialSpaceLights` is an independent optimisation; it can be considered after stage 5.
