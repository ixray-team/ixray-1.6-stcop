# Profiler

In-game flame graph of the last frames: **Debug -> Profiler** (requires a build with debug drawing). `Alt` toggles mouse capture.

## What it shows

- A history bar of the last 255 frames; the height of a column is the frame time.
- One track per thread and one for the GPU, each a flame graph of the labelled blocks of the selected frame. GPU blocks arrive a few frames late. A track is as tall as its deepest drawn label, at most rows/track, so an idle thread takes one row and every track fits on the screen more often; tracks are ordered by the number of active labels, so a thread with few labels (the submit thread has three) sits low in the list.
- The average and worst frame time over the last second.

## Controls

| Control | Effect |
| --- | --- |
| Instant / Smooth / Worst | Show the newest frame, the average of the last half second, or the slowest frame of the last second |
| Freeze | Keep the selected frame |
| rows/track | The most nested levels a track shows; scroll a track for deeper ones |
| Sort | Re-sort tracks by the number of active labels |
| Export | Write every frame of the history to a `.csv` |
| Click a history column | Select and freeze that frame |
| Click a block | Zoom the graph to it |

## Export

**Export** writes `profiler_<unix time>.csv` to the logs folder and prints the file name to the log. One row per label that ran in a frame:

| Column | Meaning |
| --- | --- |
| `frame` | Frame number |
| `frame_ms` | Duration of that frame |
| `track` | Thread name, or `GPU` |
| `depth` | Nesting level, 1 for a top-level label |
| `path` | Labels from the top of the track down to this one, separated by `/` |
| `calls` | How many times the label ran in the frame |
| `ms` | Total time of the label in the frame, children included |

## GPU counters per pass

`util/nsight_passes.ps1` traces one frame of a saved game with NVIDIA Nsight Graphics, without opening its window, and prints a row per GPU pass: time, how busy the shader units, texture cache and memory were, how many shader threads were active, and how full the register file of compute shaders was. It needs Nsight Graphics, Python and an NVIDIA GPU; the counters exist for RTX 20 / GTX 16 series and newer, older GPUs get times only.

```
util\nsight_passes.ps1 -Save <save name> -Game <game folder> -Exe <xrEngine.exe>
```

| Parameter | Meaning |
| --- | --- |
| `-Save` | Saved game to load (required) |
| `-Game` | Game folder, the one with `fsgame.ltx`; or the `IXRAY_GAME_DIR` variable |
| `-Exe` | Engine executable; or `IXRAY_ENGINE_EXE`; otherwise the newest one under the repository's `build` folder |
| `-Ngfx` | `ngfx.exe` of Nsight; or `NSIGHT_NGFX`; otherwise the `PATH`, then the newest installed version |
| `-Passes` | GPU labels joined by commas; a label can be a path such as `DEFER_SUN/accum_direct_volumetric` |
| `-Commands` | Console commands to run before the save loads, each ending in `;` |
| `-Runs` | How many traces to take (default 3); each pass shows its fastest |
| `-StartMs` | When the trace starts (default 75000); raise it if the level loads slower |
| `-Architecture` | Nsight's name of the GPU generation, when the guess from the adapter name is wrong |
| `-All` | Also print every counter of each pass |

## Limitations

- Only labelled blocks are measured; time between them shows as a gap.
- The history holds 255 frames; an export covers at most those.
- GPU rows of a frame belong to a frame rendered a few frames earlier.
- A label's time includes its children: do not sum a parent with its children.

## Check after a build

- Debug -> Profiler opens the overlay and tracks appear.
- Export prints the file name to the log, and the file has the header row plus rows for CPU threads and `GPU`.
- Freezing, zooming and sorting still work.
- Idle tracks take one row; a deep track stops at rows/track and scrolls.
- On D3D12 with `r_submit_thread 1` the `X-Ray Submit Thread` track shows `SubmitThread`; with 0 those labels are under `NriEndFrame` on `Primary`.

## Capture between two commands

`profiler_start` begins a capture and `profiler_end` ends it; the profiler window does not have to be open, and the capture may last as long as needed (the window's history holds 255 frames, a capture sums every frame). `profiler_end` prints the number of frames, the seconds, the average and the worst frame time and the frames per second to the log, and writes `profiler_capture_<unix time>.csv` to the logs folder: one row per label that ran, with its track, depth, path, calls per frame, average milliseconds per frame and the peak of one frame; the first row is the frame itself. Two captures of the same walk, before and after a change, compare by these averages. Calling `profiler_start` again starts over.

## GPU spike watch

`profiler_watch on` keeps the profiler and the GPU timings running without the window and watches every GPU pass. A pass that takes more than three times its usual time, and at least a millisecond more, is reported in the log:

```
! GPU spike at frame 705, 6.43 ms the frame:
!   Frame/CRender_Render/FORWARD_LEVEL_FRAME/ScenePrepass/SceneDraw: 138.68 ms, usually 0.91 ms
```

The report is followed by the output of `render_memory_stats` (video memory, the texture streaming pools, what the forward cull drew), so the state at the moment of the spike is in the log beside it. A pass has to be seen for 120 frames before it can be reported, and there is at most one report every 120 frames. `profiler_watch off` stops it. The watch costs what the open profiler window costs; leave it off when measuring performance.

## CPU spike reports

While `profiler_watch` is on, a thread samples the primary thread's call stack about every two milliseconds of every frame. A frame that ends late gets a report: `cpu_spike_<time>_<frame ms>ms.txt` in the logs folder and a `! CPU spike` line in the log. Late is more than 8 ms and more than three times the usual frame, which is tracked while the watch runs. The report lists:

- the functions the thread was in (top of the stack), by share of the samples;
- the functions it was under (anywhere in the stack), by share of the samples: read this list from the top down to see which call the time was spent beneath;
- the whole stack of the middle sample.

A report whose top entry is a wait (`XRayWorkerThread::Wait`, `FramePacer`, `InternalDevice12::Wait` inside `PrepareSlot`) means the primary thread was not the slow one: a worker thread or the GPU was. The two threads the frame waits for (`X-Ray Game Thread`, `X-Ray PreRender Thread`) are sampled at the same moments and follow in the same report, each under its own `========` line with the same three lists; a worker that only waited shows `NtWaitForAlertByThreadId`. Script code has no names in a report: `lua51!...` entries are the nearest exported name, not the function, and the caller below them (`CScriptBinder::shedule_Update`, the Lua collector step of `CLevel`) tells what ran the script.

A frame longer than its sample buffer keeps every second sample and halves the rate, so the report covers the whole frame. Reports are written by a separate low-priority thread; a run writes at most 64. On a 3.9 ms frame the watch cost 0.1 ms (2026-11-19). Function names need the build's symbol files beside the binaries (Debug or RelWithDebInfo). Windows x64 only.

## Zones inside the scene's passes

The scene's passes (`SceneDraw`, `SceneCull`, `SceneGrass`, `SceneHiZ`, `SceneDepth`) are GPU events with a CPU zone of the same name. On the GPU side `SceneDraw` holds one event per pipeline, named by its pixel shader. On the CPU side these zones split the time a pass takes on the primary thread:

| Zone | What it covers |
| --- | --- |
| `CollectMain` | gathering what the main view draws through the old path: its portals, then `add_static` inline (the few statics the scene leaves, `GpuScene::GetMainStatics`) when the scene draws every static of the level; otherwise `add_static` walks the sectors on the task pool and `join add_static` waits for it |
| `ForwardDecals`, `ForwardCull`, `SceneCullPrepare` | building the decal records, the cull of the main view, the part of a cull before its dispatch |
| `ForwardDepthGraph` | the old path's draws of the depth pass |
| `ForwardCreate`, `ScenePipelineCreate` | making the forward layout, making one pipeline: a pipeline made in a frame is a stall of its own |
| `ForwardFrameSet` | the frame's descriptor set |
| `ForwardStreaming`, `FeedbackReadback` | the streaming step and, in it, reading back what the cull asked for: a map of a buffer the GPU still writes waits for the GPU |
| `TextureStreaming`, `TextureLoaded`, `TexturePoolResize`, `TextureHeadRead` | choosing heads, copying a read head into its pool, moving a pool into a new array, reading a head (on the loader thread) |
| `ForwardRecord` | recording the pass's draws, to its end |
| `CullStatsReadback` | reading the cull's counters back for `render_memory_stats` |
| `GrassReserve`, `GrassBladesUpload`, `ForwardGrassSamples` | growing the grass buffers (with a flush), uploading changed blades, remaking grass pipelines for another sample count |
| `NriBegin`, `NriEnd` | handing the command list to NRI and back; on D3D11 `NriEnd` submits |
| `D3D12: BeginExternalPass`, `D3D12: EndExternalPass`, `D3D12: ReadExternalBuffer`, `D3D11: BeginExternalPass`, `D3D11: EndExternalPass` | the old backend's side of the same hand-over; `ReadExternalBuffer` is the geometry pages (the scene's passes, and each skinning dispatch once a page); the skinned output is NRI's and needs none |
| `NriEndFrame` (in `D3D12: Present`, or the D3D11 present) | on D3D12 with `r_submit_thread 1` the frame handed to the submit thread; on D3D11, and on D3D12 with `r_submit_thread 0` (the default), the frame's one submit and its present, run in place (`SubmitThread`, `QueueSubmit`, `QueuePresent` inside) |
| `SubmitWait` | the primary thread waiting for the submit thread to finish the frame before: in `NriEndFrame`, and at every stop point, `Flush` (ImGui's own windows) and upload; normally near zero |
| `SubmitThread` (track `X-Ray Submit Thread`, D3D12 with `r_submit_thread 1`; the track keeps its labels with no time while the variable is 0), `QueueSubmit`, `QueuePresent` inside | the frame's one queue submit with its signal, then the present and the swap chain's next texture |
| `D3D12: Flush`, `NriDrainAll` | a stop point: the submit thread waited for, what is recorded submitted and the GPU waited for until idle |
| `D3D12: WaitFence` | the backend taking a frame slot again (`PrepareSlot`) before frame N - 3 is done: the pacer did not wait for it |

Time of a pass that none of its zones holds is the set-up before `ForwardRecord`: wrapping the frame's textures and targets.

## Zones of the shadow views

`ForwardLampShadows` and `RenderSunCascade` split into what collects the moving objects and what records the draws:

| Zone | What it covers |
| --- | --- |
| `FrameObjects`, `SceneBuildObjects` | inside `CollectMain`: the moving objects the main view keeps, posed and called once each, their parts written as records of the frame's table as they are added (the scene's collector); then the model shaders that join, the leftovers handed to `GraphMain`, the rigid parts' poses and the records' light |
| `LampPrepare` (`SpotShadowCache` inside, its `::Classify`, `::Build`) | every lamp part's cache entry looked up, classified and built before anything is drawn |
| `LampQueries` | one sphere query a shadowed lamp and the drawn cascades' queries, on the workers |
| `LampCollect` (`Traverse` inside) | the portals of each part that casts what moves or draws the level's batch, the part's only traversal of the frame (the reached sectors kept for its batch), and the objects of its lamp's query it would call |
| `ShadowCollect` (`SunCascadeWait`, `SceneCollectCasters`, `SceneUploadModels` inside) | the moving objects of every shadow view at once, once a frame (in `ForwardLampShadows` when the frame has shadowed lamps, else in `RenderSunCascade`): those only they see called once, the table's casters flagged, what the batch cannot draw handed to the graphs of the cascades and lamp parts, the one skinning pass and the table's upload |
| `LampBake` (`SpotShadowCache::Bake` inside) | a cached part's moving casters tested against the table and hashed; drawn into its depth once they stay the same |
| `LampRestore` | the cached depths of a page: one NRI pass, a scissored triangle each part (`SpotShadowCacheRestore` on the GPU) |
| `LampPortals`, `LampDepthBatch`, `SpotShadowCache::DirectStatic`, `::PerFrameStatic` | a part drawn whole: `LampPortals` only when the batch leaves the level's statics to the lamp views (its portals again, `Traverse`, and `add_static`), the level's batch kept for the page, culled by the sectors `LampCollect` reached (its cull block, `SceneCullPrepare`); a cached part's statics from its entry (`::DirectStatic` culls by the same sectors; `::PerFrameStatic` only for an entry with per-frame statics) |
| `LampGraph`, `LampGrass`, `LampTranslucent` | a part's graph, only when it holds packets of the first pass (statics left, the table's parts the batch cannot draw), its grass, its translucent pass |
| `LampTiles` | the moving casters of a page's parts, up to 16 at once, kept for the page as one batch: one cull over the table and one pair of draws |
| `LampRecordViews` (`SceneViewsPrepare` is just before it) | recording every page's batches: on D3D12 the wait for the workers (`LampPageRecord` on each, a page a worker) and `ViewsChain` (`NriDevice::Chain`: the frame's command buffer closed, the buffers queued behind it: no submit); on D3D11 the batches on the primary thread, `SceneDepth` and `SceneTiles` on the GPU |
| `SunCascadeBuild` (task pool) | a cascade's portals (`Traverse`); `add_static` inside only when the batch leaves the cascades any static |
| `SunCascadeRecordViews`, `SunCascadeGrass`, `SunCascadeDraw` | recording the cascades' culls and draws (on D3D12 the wait for the workers, `SunCascadeRecord` on each, and `ViewsChain`), their grass, what their graphs draw |

On D3D12 the batches the workers record have no GPU zone of their own: they reach the queue with the frame's one submit (`QueueSubmit` on the submit thread), in the same call as the buffers around them, so their time is inside `ForwardLampShadows` and `RenderSunCascade` on the GPU with no gap between submits, and a capture names their buffers `LampPage` and `SunCascade`. The bake (`LampBake`) and a cache build still record their batch at once, `SceneTiles` and `SceneDepth` on the GPU.

`render_memory_stats` prints the table's lines with the forward cull's: `frame objects in the frame before` (objects of the main view and of the shadow views alone, the `renderable_Render` calls of each view, main, lamps, decor and sun cascades, against the objects they were made for: every object once a frame; then the objects only the shadow views see: called, left out because their shadow cannot reach the screen, and skeletons that kept their pose, far from the eye) and `frame objects' parts` (records by kind, casters).
