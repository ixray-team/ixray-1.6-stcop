# Physics threading

`CPHWorld::Step` runs on the physics owner thread, normally `GameThread` when `mt_physics` is enabled. A persistent physics worker pool executes spatial queries, static triangle narrowphase on private geometry copies, and independent ODE worlds. Workers never call `CPHObject::Collide`, game triangle callbacks, debug drawing or game code.

## Parallel steps and worker lifetime

Consecutive `CPHWorld::Step` calls inside `FrameStep` represent successive simulation times. The next step needs the previous step's poses, velocities, collision history and callbacks; they cannot execute concurrently on the same bodies. Their contact joints, feedback and effector storage are also shared. Parallelism is inside each step: different active `CPHIsland::Step` calls operate on disjoint body/joint lists, and the owner joins them before publishing results and starting the next simulation time. A single connected island remains one solver job.

`CPHWorkerPool` creates named `Physics Worker N` threads lazily through the engine's thread API. Each phase publishes a borrowed context and a function pointer, then wakes only its participating workers. There are no per-window PPL tasks or task-functor allocations. The owner processes the same dynamic job queue directly unless it is running admitted scheduler work; after doing useful work it waits with an atomic wait rather than yielding in a spin loop. Small broadphase windows use at most one worker per four queries. Idle threads sleep, and the same pool is reused across collision windows, island solves and consecutive fixed steps.

Automatic mode requests at most eight participants, including the owner when it contributes physics work. Explicit requests are limited to 64 and the CPU's reported logical thread count. Scheduler overlap may use that many background solver workers while the owner does independent work. Created threads are retained up to the largest request, released when a subsequent `FrameStep` selects `ph_threads 1` or disables every parallel physics stage, and joined before world teardown. The extra thread stacks must be included when comparing memory; reusable job storage does not imply zero memory overhead. Failure to create a background worker leaves the owner processing the shared queue. A phase waits for all published work before rethrowing a captured exception on the owner, so no worker retains a pointer to an expired phase context. MSVC builds enable `/EHsc` for `xrPhysics` in every configuration so phase guards and private collision recording scopes unwind correctly; the global release settings previously omitted this option.

## Collision order

The owner thread walks the live `m_objects` list in its original order. For each object it executes the complete virtual `Collide`: character initialization, ray motions, dynamic collision, static collision, material processing, callbacks, joint creation and dirty-flag updates. Newly activated objects remain eligible for the same live traversal. Static shells and sleeping objects are filtered by their current flags, without snapshot indices.

Dynamic narrowphase and contact application remain sequential. Ray-motion geometry is configured and consumed immediately, so no deferred pair retains a pointer to a reused ray. Static narrowphase is prefetched as described below; it never changes the live geometry or userdata on a worker.

The ordinary contact buffer is local to each `CollideIntoGroup` call. Its lifetime ends after applying that pair. The level's triangle collider remains exclusive to the owner; each static worker has a separate collider and query scratch. No whole-step contact arrays are retained.

## Static narrowphase

For each collision window, the owner captures at most 32 supported primitive leaves (sphere, box, custom cylinder). It traverses spaces and transforms, computes each leaf's world pose using ODE's transform multiplication, and prepares private ODE shapes and detached bodies. Body rotation is copied exactly after the ODE setter to avoid quaternion normalization changing the collision matrix. The collider reads the cloned pose, dimensions and linear velocity, and writes a private copy of geometry userdata, including triangle history. The static model, vertices, triangles and material flags are read-only while these tasks run.

Up to eight workers fetch primitive jobs dynamically and generate contacts with their own `dcTriListCollider`. They call its primitive methods directly, avoiding ODE's shared lazy collider dispatch table and temporary modifications of live transform children. The owner waits for completion, then executes the original live object traversal. At the actual static leaf collision, it checks the spatial owner's identity, body identity, exact pose, dimensions, linear velocity and original userdata/history before using the cached result. An earlier dynamic collision or callback that changed these inputs causes an ordinary serial collision instead. Unsupported shapes, arbitrary triangle callbacks, deep hierarchies and leaves outside the bounded window also use that path.

Game initialization explicitly admits only the default `ContactShotMark` and `CharacterContactShotMark` callbacks. They create effects without modifying collision inputs. Workers record their triangle/contact arguments; matching results replay these callbacks on the owner in their original order before `ApplyContacts`. Contacts are remapped to live geometry before replay and application; ODE's enclosing transform and argument-reversal handling still executes normally. Custom triangle callbacks always run through the ordinary collider. Object contact callbacks, materials, island merges, wakeups, joint creation and virtual character collision initialization retain their original owner-thread order.

Each speculative leaf has 128 contact slots and 128 callback-event slots, with two triangle-history copies limited to 4096 triangle IDs. Contact truncation, event overflow or a larger triangle query invalidates the whole result; partial contacts are never applied, and the original collider recomputes them with its normal budget. Worker scratch above 4096 entries is released after each job, and oversized temporary history capacity is released after each window. Query allocation may transiently exceed that bound before detecting overflow; the normal serial collider still handles unrestricted scenes. Pools are bounded by 32 leaves and eight workers, retained for reuse, and released when narrowphase prefetch is disabled, physics becomes serial, or the world is destroyed. This bounds speculative storage independently of the total number of physics objects.

In Debug, workers skip global triangle counters and drawing. Accepted results update the live saved-triangle count on the owner; detailed per-triangle counters describe only the ordinary serial path. Enabling any physics debug draw flag selects the ordinary static collider for complete visualization and counters.

## Bounded broadphase

For at least eight active objects, `ProcessCollisions` prepares a window of at most 64 objects (four per requested worker). Workers call `ISpatial_DB::QueryBoxLimited`; each result holds at most 256 spatial references. This bounds the cached reference payload to 256 KiB on x64, plus query metadata and the normal serial query buffer. The default worker limit is eight, which uses a window of at most 32 objects.

The limited query reports overflow and clears its partial result. The owner then uses the ordinary full query, preserving all candidates and traversal order. Before using a cached result, it checks its owner, exact query bounds and spatial revision. Register, unregister, move (including movement within the same node), database initialization and registered-owner reset invalidate the revision. A mismatch triggers a full query. Collision flags and validators are evaluated only when the live object is processed.

The owner reserves query buffers before dispatch, so workers do not allocate them. The shared allocation counter uses atomic increments and an atomic exchange in frame statistics. Cached references are released after each window, while the query slots and their bounded buffers remain available for the next window and step. A short final window does not destroy buffers that the following step would allocate again. Reducing the requested window size removes unused slots; selecting serial collision processing frees the query cache. The common unchanged live-object order uses a direct slot lookup, with a bounded identity search when callbacks change that order. A serial query buffer that grew beyond 256 entries is released at the end of the collision phase. The 256 KiB limit applies to speculative cached results, not the game's total physics memory or the full query required for an overflowing scene.

## Island solver

After collision and `PhTune`, only active `CPHIsland` worlds enter a reusable solver list. Two or more independent islands can execute concurrently when the thread limit permits; the previous four-island threshold is removed. Expensive worlds start first and workers fetch the remaining worlds dynamically. Workers change bodies, joints and geometry belonging to that world; the owner waits before `IslandReinit`, `PhDataUpdate`, spatial updates and clearing `ContactGroup`. The phase snapshots its step duration, and `CPHIsland::Step` passes its actual `step` argument to ODE instead of reading `fixed_step` again. Inactive merged islands do not emit misleading `Physics World Step` events.

QuickStep uses a local generator seeded from `dxWorld::QuickStepSeed`, advanced by that world. Its sequence is independent of worker identity and scheduling order. Global ODE parameters must remain unchanged while solver tasks run. Debug hooks and island statistics execute on the owner thread. Recursive world `Step` / `StepTouch` / `FrameStep` requests during an active step are ignored so callbacks cannot clear its contact group.

## Scheduler overlap

The ordinary scheduler callbacks retain their order and finish before physics: AI, Lua binders, forces, spawning and body destruction are not moved across the step. Only work explicitly submitted with `CSheduler::QueuePhysicsIndependent` may overlap the island solver. Its `ISheduled::ExecutePhysicsIndependent` implementation runs on the game thread, must not access ODE state, call Lua, change object lifetimes or launch further tasks.

The first client is `vision_client` (actor vision). Its ray-test job captures the eye position, elapsed time and transparency threshold during the normal scheduler callback. It then runs while workers solve islands, before their completion barrier and before `PhDataUpdate`. Frustum collection and visual-memory updates remain in the original scheduler callback. Ray results still feed the normal subsequent memory update; deferring the ray job changes when its results become available within the frame. This does not parallelize NPC logic or the entire scheduler, and the ray job was already asynchronous in the previous path, so a performance gain must be measured.

With pending independent work, even a single active island can be dispatched to a worker, provided `ph_threads` permits more than one worker and `ph_parallel_islands` is enabled. The owner drains independent work while that island runs. The pool joins the phase before its borrowed task state goes out of scope, including when a callback or solver throws.

The queue holds at most 64 owner pointers (512 bytes of pointer payload on x64), with storage reserved during scheduler initialization. Unregistering or destroying an owner cancels its queued work; resetting vision cancels its old request. With overlap disabled, no MT physics world, or a full queue, vision uses its existing `SecondaryTasks` path. If no parallel solve runs (including no fixed step, a frozen world or serial physics), `GameThread` drains queued work after `seqFrameMT` in the same frame. Work is also drained before physics-world teardown while the object space still exists.

## Controls and verification

| Command | Meaning |
| --- | --- |
| `ph_threads 0` | Automatic, at most eight participants and no more than logical CPU threads |
| `ph_threads 1` | Serial queries and solver; original live collision order |
| `ph_threads 2` ... `64` | Requested participant count, limited by logical CPU threads |
| `ph_parallel_broadphase false` | Disable query prefetch independently |
| `ph_parallel_narrowphase true` / `false` | Enable (default) or disable static narrowphase prefetch independently |
| `ph_parallel_islands false` | Disable parallel island stepping independently |
| `ph_scheduler_overlap true` / `false` | Enable (default) or disable explicitly admitted scheduler work during the solver |

Profiler zones: `PH Collide`, `PH Collide Prefetch`, `PH Collide Collect`, `PH Collide Snapshot`, `PH Collide Narrowphase`, `PH Static Collision Job`, `PH Static Cache Hit`, `PH Static Narrowphase Serial`, `PH Collide Apply`, `PH Collide Dynamic Apply`, `PH Collide Static Apply`, `PH Island Step`, `PH Island Step Serial`, and `Physics World Step`. `PH Collide` measures collision wall time on the owner, and `PH Collide Prefetch` includes snapshot preparation, task dispatch and the completion wait. `PH Collide Apply` still contains dynamic narrowphase, cache validation, callback replay and joint creation. It is expected to remain on the owner thread. Actual triangle work should appear under worker `PH Collide Narrowphase` zones, with `PH Static Cache Hit` recording results accepted on the owner. `PH Static Narrowphase Serial` shows ordinary leaf collision calls, including unsupported or invalidated cached work.

`PH Island Solve` measures the complete solver phase on the owner, including concurrent scheduler work and the final `PH Worker Wait`. Inspect simultaneous `Physics World Step` events on the named physics threads, rather than expecting successive `CPHWorld::Step` events to overlap. The supplied Optick screenshot also shows significant Concurrency Runtime scheduling/wait paths and allocation/stack-walk samples. The persistent pool removes PPL submission from physics; the screenshot alone cannot quantify how much time a new implementation saves. The Optick `PROF_EVENT` wrapper now initializes its description with a C++ guarded local static, avoiding the package macro's unsynchronized shared pointer initialization when several workers first enter one call site. The pool and profiler changes have not been built or run by the assistant at the user's request.

Compare `ph_parallel_narrowphase false` and `true` with the same scene, thread count and broadphase/island settings. Measure the wall time of the complete physics step and frame, including p95/p99, rather than summing concurrent zone durations. The supplied Tracy CSV before this change reports 6422.23 ms in `PH Collide Apply` across 1233 frames (about 5.21 ms/frame), 14470.34 ms in `CPHWorld::OnFrame` (11.74 ms/frame), and 17152.22 ms summed across `PH Collide Collect` workers. The CSV contains aggregate statistics, so it cannot establish worker wall time, cache coverage or a speedup from this change. The new static narrowphase path has not been built or run at the user's request.

Overlap zones are `Scheduler Physics Independent` and `Vision Physics Independent`. Compare `ph_scheduler_overlap false` and `true` with the same scene and thread count. The solver wall-time zone includes the owner's independent work and wait; do not add that time to concurrent island durations. Check visibility updates, pause/unpause, level reload and serial mode as well as timing. The scheduler-overlap changes have not been built or run at the user's request.

Runtime coverage should include a dense pile, ray-motion objects, sleeping shells, ladders, passable materials and Debug visualization. Compilation alone does not establish scene-wide equivalence or a performance gain.

### Runtime check before scheduler overlap and static narrowphase prefetch, October 15, 2026

The rebuilt engine was run with `D:\ixray-team\cop` as its working directory and the installed addons. A temporary filesystem configuration beside the game's `fsgame.ltx` redirected only `$app_data_root$` to separate test profiles. The filesystem configuration's directory determines `$fs_root$`, so moving it into the build directory changes archive mount paths even when the process working directory is correct.

Both RelWithDebInfo runs loaded a new game (`all/single/alife/new`), created a dense pile of 128 `wpn_abakan` objects after loading, verified that all 128 remained online after 360 actor updates, and completed 900 measured autotest frames with exit code zero and `RESULT: PASS`.

| Requested threads | Peak private memory, MiB | Peak working set, MiB |
| --- | ---: | ---: |
| 1 | 3243.6 | 1584.2 |
| 8 | 3285.1 | 1578.7 |

Debug also completed 1200 measured frames with `ph_threads 64`, both parallel stages enabled, all 128 stress objects online and `errors=0`, `RESULT: PASS`. The requested task count is still limited by scheduler concurrency. This run loaded sounds with `ALSOFT_DRIVERS=null`; an earlier Debug attempt with `-nosound` produced missing-sound script assertions and is not counted as a passing run. RelWithDebInfo used `-nosound`, so Debug and release memory totals are not directly comparable.

Process memory was sampled every 500 ms from launch through exit. These are total engine-process measurements with addons, not isolated physics allocations. They show similar resident memory in these two runs; they do not establish the reduction from the previous implementation or exclude leaks in longer sessions. The runs also do not establish an FPS improvement or cover ladder and ray-motion behavior.
