# IX-Ray SDK Macro MCP

To generate an AI map, prepare the AI Map tool's Snap List in Level Editor, then
chain `Add ai node` → `Generate ai map`. Pass world coordinates `X Y Z` as the
first argument of `Add ai node`. `Generate ai map` takes no arguments and returns
the resulting node count. Both commands use the configured Snap List as-is.

The `run_sdk_macro` tool runs with the configured Windows SDK. With `keep_open` enabled it reuses a compatible editor window when available. It runs a Macro Editor JSON graph or a text command-list macro through Level Editor's `ExecCommand`.

Build the `LevelEditor` target and put the matching editor binaries and dependencies in your SDK.

To include MCP in a Windows SDK build, configure the editor preset with
`cmake --preset Editors-x64-Windows -DIXRAY_SDK_MCP=ON` and build the
`LevelEditor` target. CMake stages `mcp/` in the editor's configuration output.
When assembling the SDK, place that directory beside `bin/` and `fs.ltx`.
The option defaults to off.

## Connect a packaged SDK

Use a system Python installation with `python` available on `PATH`. Install the
server dependencies in PowerShell, replacing `D:/SDK` with the folder where you
extracted the SDK.

```powershell
python -m pip install -r "D:/SDK/mcp/requirements.txt"
```

Use this entry in your MCP client's stdio server configuration.

```json
{
  "command": "python",
  "args": ["D:/SDK/mcp/server.py"]
}
```

The MCP client must be able to resolve the same `python` on its `PATH`. The server
finds `bin/LevelEditor.exe` and `fs.ltx` relative to the SDK root and uses `import/`
for macros. After switching Python versions, run the dependency installation
command again with the new interpreter.

Put trusted macro files in the configured directory and call `run_sdk_macro` with their filename. For a smoke test, put `COMMAND_TOGGLE_GRID()` in `smoke.txt` and call the tool with `{"name":"smoke.txt"}`. The result reflects the command handlers' return values. A command that starts a background task can return before the task finishes.

Call `list_sdk_macros` to discover `.json` graphs and `.txt` command lists in the configured directory. Each entry returns the `name` to pass to `run_sdk_macro` and the file's `format`. Listing reads filenames only. Level Editor validates the graph when it runs.

To run a node graph, save it in Macro Editor and copy `editor_macros.json` from the SDK's `$app_data_root$` to the configured macro directory. Call `run_sdk_macro` with `{"name":"editor_macros.json"}`. The response includes `success`, `exit_code`, and a `nodes` array in execution order. Each executed command has an `id`, `command`, `success`, and `result`. Start is the entry point. A failed command (integer result `0`) stops the entire graph, including any remaining branches. Re-entering an already visited node stops execution with an error on that node.

Direct editor invocation is available for troubleshooting:

```text
LevelEditor.exe --run-sdk-macro "D:\SDK\import\smoke.txt" "D:\SDK\result.json" -fsltx "D:\SDK\fs.ltx"
```

For the strict AI pipeline, add `--xrai D:/SDK/bin/xrLevelBuilder.exe` to the MCP server arguments. The compiler must support `--headless-ai`. Set `--timeout` for the expected build duration. Link `Start` → `Build ai map` → `Verify ai map` → `Build ai spawn`. Set the first argument of each command to the level directory name and the second to an absolute output directory. The compiler writes `level.ai` in the configured SDK and writes the spawn and dependent graphs in a new `spawn` subdirectory of the output directory. The same output directory contains `build.log`, `verify.log` and `spawn.log` for the stages that ran.

Each AI node also returns `compiler_exit_code`, `compiler_error_code`, `log_path` and `artifacts`. The compiler exit code and log path are null if unavailable. The error code is null on success and contains the compiler diagnostic when present. The artifact list contains files produced by that stage, including partial output on failure. Verification reads `level.ai` and returns an empty artifact list.

With `"keep_open": true`, the editor stays open after the graph finishes. The response includes `editor_pid` and a null `exit_code` while the editor is open. Keep-open mode reuses a compatible open Level Editor for the same SDK configuration. If none is available, it launches one through the interactive Windows desktop. An editor running a build from before this change needs to be restarted once. It requires pywin32 and an active Windows desktop. The editor inherits its environment from Explorer. Use the default batch mode for graphs that need the server option --xrai.

To create an editable scene, link `Start` → `Clear` → `Add library object` → `Save new level`. Add one node per object. Set its first argument to the SDK object reference without the `.object` suffix and its second argument to three space-separated coordinates `X Y Z` (Y up). The result is the new object's scene name. `Save new level` takes an unused absolute `.level` path in an existing directory and returns that path. The editor saves the companion scene parts beside the level. To extend a scene, use `Load` with the existing `.level` path in place of `Clear` and save to a new path.

Use `Search library models` to find SDK model references by a case-insensitive substring. It returns `models`, `total`, and `truncated`, with up to 100 matches. `Move scene object` and `Rotate scene object` take the exact scene instance name and an absolute `X Y Z` vector separated by spaces. Rotation uses degrees around the editor axes. Both commands return the instance name. To save edits as a separate copy, finish the graph with `Save new level`.

`Find scene model usage` takes an exact model reference without the `.object` suffix. Its read-only result lists matching instance names, models, positions, rotations in radians, and scales.

`Replace scene models` takes the expected model reference as its first argument. Its second argument is a JSON object such as `{"replacement":"props/fence_new","objects":["checkpoint_fence_0","checkpoint_fence_1"]}`. Use SDK library references as returned by the editor. The node checks that every listed instance can be edited and still uses the expected model before replacing any references. It keeps instance names, positions, rotations, and scales, and uses the new model's surfaces. Run `Save new level` after replacement to save a separate scene copy.

`List scene objects` returns the loaded scene objects as an array with `name` and `position` (`[X, Y, Z]`). `Delete scene object` takes the instance name and returns the removed name. Missing, locked, grouped, and read-only objects cause the node to fail. Finish with `Save new level` to write a separate copy.

`Duplicate scene object` takes the source instance name and a unique new name made of letters, digits or underscores. It copies the static object's model, transform and serialized properties and returns the new name. `Scale scene object` takes the instance name and an absolute positive scale vector `X Y Z`. For an object at unit scale, `2 2 2` doubles its size.

`Rename scene object` takes the current name and a new name. `Group scene objects` takes a new group name and a JSON array of at least two distinct scene object names as its second argument. Each command returns the resulting name. New names use letters, digits, or underscores and are stored in lowercase. Group members must be editable, unlocked, and outside existing groups.

`Create waypoint route` takes a unique route name and a JSON array of 1–256 world positions, such as `[[-3,0,-6],[-3,0,-2]]`. It links each point to the next and closes the loop. A single point has no links and can serve as a stationary post.

`Set level mode` sets the scene's map usage to `single`, `deathmatch` (also `mp`), or `freemp`. In a new scene, call it after `Clear`. Add an `actor` and a `graph_point` with `Add spawn entity` for Single. For Deathmatch and FreeMP, use `Add player respawn` with the mode and three world coordinates `X Y Z`. Each call creates one team-neutral respawn point. Add geometry and save with `Save new level`. The existing `Play in editor` command starts Single. Multiplayer playtesting uses a multiplayer game session.

Place `Validate level mode` before build or play nodes and pass the mode as its first argument. The node returns `ready`, `mode_mask`, object counts and an `issues` array with issue codes and object names. A false `ready` stops the graph.

For a standalone build, follow validation with `Export level build`. Set the copy's `level_path` to a unique directory name before exporting. The node writes `build.prj` and the game data, including respawn points in `level.game`, under the SDK's `$game_levels$` directory. Run xrLC in xrLevelBuilder against that project to compile the render geometry and collision data.

`Assign npc logic` takes the exact spawn object name and an existing LTX path relative to `gamedata/configs`, such as `scripts/checkpoint/guard.ltx`. The file supplies a `[logic]` section and can reference a route with `path_walk`. The command adds `[logic] cfg = ...` to the spawn object's custom data. Repeating the same assignment is safe. A different existing logic section causes the node to fail.

`Inspect scene object` takes the exact scene object name. It returns the object type, position, rotation in radians and scale. Static objects include their model reference, waypoint routes include their ordered positions, and spawn objects include their spawn section and LTX logic reference when present.

To build a test copy and start Play in Editor, link `Start` → `Load` → `Save new level` → `Build pie collision` → `Build pie ai map` → `Build pie game graph` → `Play in editor`. Set the first two command arguments to the source `.level` and the unused destination `.level` respectively. Run the graph with `"keep_open": true` from an idle editor. The three build nodes prepare collision and navigation for the embedded game. The final node builds the spawn and starts the game. The MCP response includes each executed node's result and the editor session's `log_path`.
