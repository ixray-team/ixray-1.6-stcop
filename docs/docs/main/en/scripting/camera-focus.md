# Camera focus

In single player, scripts can smoothly turn the first-person camera toward a point or object, reduce its FOV and track the target. The actor does not move. No binocular item, scope overlay or camera animation file is required.

## API

```lua
local params = camera_focus_params()

-- Return true on success, false when the request is rejected.
db.actor:actor_focus_point(point, params)
db.actor:actor_focus_object(object, offset, params)

-- -1 uses the configured blend_out; 0 ends immediately.
db.actor:actor_stop_focus(-1)

local active = db.actor:actor_focus_active()
local state = db.actor:actor_focus_state()
```

`point` and `offset` are `vector` values; `object` is an online `game_object`. The offset is added to the object's position **in world coordinates**. For example, `vector():set(0, 1.6, 0)` aims above an NPC's origin. This is a fixed offset, not a head bone attachment.

Starting requires a living, player-controlled actor using the first-person camera. Multiplayer, dedicated servers, conversations, vehicles, absolute camera effectors and `demo_record` reject the request. The actor cannot target itself.

Invalid parameters, a `nil` object or a call on another object type return `false`, preserving any existing focus. Parameters are copied at start; later edits to the Lua `params` object do not affect the running session.

## Parameters

| Field | Default | Meaning |
| --- | --- | --- |
| `fov` | `35` | Target field of view in degrees, from 5 to 170. Smaller values zoom in further. |
| `blend_in` | `1` | Turn and FOV transition duration, in seconds. |
| `hold_time` | `-1` | Hold duration after the entry transition, in seconds. `-1` waits for cancellation; `0` starts leaving immediately. |
| `blend_out` | `0.5` | Exit transition duration, in seconds. |
| `tracking_speed` | `8` | Tracking smoothing rate after entry, in 1/s. Higher values follow moving targets faster. |
| `lock_input` | `true` | Block actor gameplay input until the exit transition completes. |
| `track` | `true` | Update the object's position each frame. `false` captures its position at start. Ignored for a point target. |
| `restore_direction` | `false` | Smoothly restore the original viewing direction on exit. Otherwise leave the view facing its last direction. |

Numbers must be finite. Durations must be nonnegative, except for `hold_time = -1`. `tracking_speed` must be positive. A zero duration performs an instant transition.

Entry and exit use smoothstep. Tracking uses time-based exponential smoothing and the shortest turn across the angle seam. Camera turn limits apply. A target coinciding with the camera position preserves the last direction.

## Patrol waypoint

```lua
function show_waypoint()
    if not db.actor then return false end

    local point = patrol("quest_view_point"):point(0)
    local params = camera_focus_params()
    params.fov = 30
    params.blend_in = 1.2
    params.hold_time = 3
    params.blend_out = 0.8
    params.restore_direction = true
    return db.actor:actor_focus_point(point, params)
end
```

Waypoint indices start at zero. The path and index must exist; validation belongs to the `patrol` API rather than camera focus.

## NPC tracking

```lua
function watch_npc(npc)
    if not db.actor or not npc then return false end

    local params = camera_focus_params()
    params.fov = 40
    params.hold_time = -1
    params.track = true
    params.lock_input = true
    return db.actor:actor_focus_object(npc, vector():set(0, 1.6, 0), params)
end

function stop_watching()
    if db.actor then db.actor:actor_stop_focus(-1) end
end
```

Set `lock_input = false` to allow movement while tracking. Focus still controls camera direction and FOV. On exit, direction is released unless `restore_direction` is requested.

## State and completion

| `actor_focus_state()` | State |
| --- | --- |
| `0` | Idle |
| `1` | Entering |
| `2` | Holding / tracking |
| `3` | Leaving |

`actor_focus_active()` remains `true` during exit. Observe the transition to state `0` from a regular script update to detect completion. Camera updates do not call Lua callbacks.

A successful repeated start replaces the target and transitions from the current viewing direction and visible FOV. `restore_direction` retains the direction from before the **first** start in that sequence.

`actor_stop_focus(seconds)` overrides the exit duration for the current session. `-1` uses `blend_out`. Repeated cancellation does not restart an exit in progress. Other negative values and nonfinite numbers are ignored. `0` releases input immediately and restores direction immediately when requested.

Deleting a tracked object starts a normal exit. With `track = false`, no object reference is retained, so deletion does not interrupt viewing the captured point. NPC death alone does not cancel tracking while its object still exists.

Actor death, save loading, actor destruction/respawn, camera changes, conversations, vehicles or transfer of control to another entity reset the session. Absolute camera effects and `demo_record` have priority. These resets skip the exit transition and direction restoration. Focus is not serialized into save files; quests can restart it after loading when appropriate.

A normal exit restores the **current** gameplay FOV, including sprint and weapon aiming. The user's FOV setting and global `g_fov` are not overwritten. Regular camera effects (bobbing, recoil and the camera rig) continue to apply on top of focus.

Locking input stops movement, firing and aiming. Button release events are still processed to avoid stuck actions. Menus and the console remain accessible. The system never calls `level.enable_input()` and does not release input restrictions owned by other scripts. After exit, input is processed normally again, including movement keys that remain held.

The system does not hide the HUD or weapon, move the character, guarantee target visibility through obstacles or change game time.
