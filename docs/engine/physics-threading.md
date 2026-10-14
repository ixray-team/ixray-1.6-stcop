# Physics threading

`CPHWorld::Step` runs on the physics owner thread, normally `GameThread` when `mt_physics` is enabled. It uses the shared task scheduler for read-only spatial queries and independent ODE worlds. Worker tasks never call `CPHObject::Collide`, triangle callbacks, debug drawing or game code.

## Collision order

The owner thread walks the live `m_objects` list in its original order. For each object it executes the complete virtual `Collide`: character initialization, ray motions, dynamic collision, static collision, material processing, callbacks, joint creation and dirty-flag updates. Newly activated objects remain eligible for the same live traversal. Static shells and sleeping objects are filtered by their current flags, without snapshot indices.

Narrowphase remains sequential because the triangle collider changes geometry userdata (`pushing_neg`, `neg_tri`, `last_pos`, `tri_material`) and invokes game callbacks. Precomputing all contacts and applying them later changes these dependencies even with object locks. Ray-motion geometry is configured and consumed immediately, so no deferred pair retains a pointer to a reused ray.

The contact buffer is local to each `CollideIntoGroup` call. Its lifetime ends after applying that pair. The level owns one triangle collider, and its scratch storage is used only by the owner thread. No worker-local triangle colliders or whole-step contact arrays are retained.

## Bounded broadphase

For at least eight active objects, `ProcessCollisions` prepares a window of at most 64 objects (four per requested worker). Workers call `ISpatial_DB::QueryBoxLimited`; each result holds at most 256 spatial references. This bounds the cached reference payload to 256 KiB on x64, plus query metadata and the normal serial query buffer. The default worker limit is eight, which uses a window of at most 32 objects.

The limited query reports overflow and clears its partial result. The owner then uses the ordinary full query, preserving all candidates and traversal order. Before using a cached result, it checks its owner, exact query bounds and spatial revision. Register, unregister, move (including movement within the same node), database initialization and registered-owner reset invalidate the revision. A mismatch triggers a full query. Collision flags and validators are evaluated only when the live object is processed.

The owner reserves query buffers before dispatch, so workers do not allocate them. The shared allocation counter uses atomic increments and an atomic exchange in frame statistics. Cached references are released after each window. Smaller windows destroy unused query buffers; switching to serial broadphase frees the cache. A serial query buffer that grew beyond 256 entries is released at the end of the collision phase. The 256 KiB limit applies to speculative cached results, not the game's total physics memory or the full query required for an overflowing scene.

## Island solver

After collision and `PhTune`, only active `CPHIsland` worlds enter the solver task list. Expensive worlds start first and tasks fetch the remaining worlds dynamically. Workers change bodies, joints and geometry belonging to that world; the owner waits before `IslandReinit`, `PhDataUpdate`, spatial updates and clearing `ContactGroup`.

QuickStep uses a local generator seeded from `dxWorld::QuickStepSeed`, advanced by that world. Its sequence is independent of worker identity and scheduling order. Global ODE parameters must remain unchanged while solver tasks run. Debug hooks and island statistics execute on the owner thread. Recursive world `Step` / `StepTouch` / `FrameStep` requests during an active step are ignored so callbacks cannot clear its contact group.

## Controls and verification

| Command | Meaning |
| --- | --- |
| `ph_threads 0` | Automatic, at most eight workers and no more than scheduler concurrency |
| `ph_threads 1` | Serial queries and solver; original live collision order |
| `ph_threads 2` ... `64` | Requested task count, limited by scheduler concurrency |
| `ph_parallel_broadphase false` | Disable query prefetch independently |
| `ph_parallel_islands false` | Disable parallel island stepping independently |

Profiler zones: `PH Collide Collect`, `PH Collide Apply`, `PH Island Step`, `PH Island Step Serial`, and `Physics World Step`. The Apply zone includes sequential narrowphase. Measure the wall time of the complete physics step and frame, including p95/p99, rather than summing concurrent zone durations.

Runtime coverage should include a dense pile, ray-motion objects, sleeping shells, ladders, passable materials and Debug visualization. Compilation alone does not establish scene-wide equivalence or a performance gain.

### Runtime check, October 15, 2026

The rebuilt engine was run with `D:\ixray-team\cop` as its working directory and the installed addons. A temporary filesystem configuration beside the game's `fsgame.ltx` redirected only `$app_data_root$` to separate test profiles. The filesystem configuration's directory determines `$fs_root$`, so moving it into the build directory changes archive mount paths even when the process working directory is correct.

Both RelWithDebInfo runs loaded a new game (`all/single/alife/new`), created a dense pile of 128 `wpn_abakan` objects after loading, verified that all 128 remained online after 360 actor updates, and completed 900 measured autotest frames with exit code zero and `RESULT: PASS`.

| Requested threads | Peak private memory, MiB | Peak working set, MiB |
| --- | ---: | ---: |
| 1 | 3243.6 | 1584.2 |
| 8 | 3285.1 | 1578.7 |

Debug also completed 1200 measured frames with `ph_threads 64`, both parallel stages enabled, all 128 stress objects online and `errors=0`, `RESULT: PASS`. The requested task count is still limited by scheduler concurrency. This run loaded sounds with `ALSOFT_DRIVERS=null`; an earlier Debug attempt with `-nosound` produced missing-sound script assertions and is not counted as a passing run. RelWithDebInfo used `-nosound`, so Debug and release memory totals are not directly comparable.

Process memory was sampled every 500 ms from launch through exit. These are total engine-process measurements with addons, not isolated physics allocations. They show similar resident memory in these two runs; they do not establish the reduction from the previous implementation or exclude leaks in longer sessions. The runs also do not establish an FPS improvement or cover ladder and ray-motion behavior.
