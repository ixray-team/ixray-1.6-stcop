# In-game frame profiler

Open **Debug → Profiler** in the ImGui menu to collect frame timings. The frame history lets you inspect a frame or average a range of frames. Hover over a block to inspect its time and call count. Close the profiler to stop collection.

## Script timings

Use the Lua Framework markers in a script. Completed samples appear on the **Lua** track with their names, nesting, elapsed time and call counts. Repeated calls at the same position in the tree are combined within the frame. A parent's time includes its children and any engine functions called inside the marked block.

```lua
local result = PROF_EVENT_CLOSURE("Update quest state", function()
    PROF_EVENT_BEGIN("Find active tasks")
    local tasks = find_active_tasks()
    PROF_EVENT_END()
    return update_tasks(tasks)
end)
```

The overlay reads markers directly from scripts. There is no LTX configuration or external profiler connection required. Use a stable name per operation. Named markers are collected in Debug and RelWithDebInfo with the overlay open. The existing Tracy connection can collect the same markers.

Keep a begin/end pair within one frame and one coroutine. Unfinished samples are dropped at the frame boundary. Use the closure form around code that may raise an error, so its open markers are closed before the error is propagated. Collection is limited to 8,192 Lua scopes per frame and 32 levels of nesting. Marker names and tree nodes also have bounded storage.

## Post-build checks

- In Debug, load a level in Shadow of Chernobyl or Clear Sky and open **Debug → Profiler**. Trigger a marked callback and confirm the **Lua** track reports its name, time and count.
- Use two nested markers and call the inner marker twice in one frame. Inspect that frame and confirm the inner marker appears below its parent with two calls.
- Catch an error thrown inside a marked closure, then record another marker. Confirm it appears at the expected depth. Close and reopen the overlay and confirm live samples resume.
- Repeat the basic collection check in RelWithDebInfo.
