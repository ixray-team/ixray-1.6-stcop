# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR TRIGGERS Module
A module that allows you to place a spherical script trigger zone on a location, which will call a script method when an NPC, monster, or the player enters the zone (useful for story branching, tracking when alife objects enter an area). It is a lightweight version of space restrictors.

Works only for online objects.

Trigger zones are saved in the game save.

```lua
--// Create a trigger (polling interval 100ms, reacts to NPC/actor/monster entering the zone)
TriggerCreate(trigger_name, trigger_level_name, trigger_center_position, trigger_radius, callable_fn)
args:
  trigger_name (string)(required) - unique trigger name.
  trigger_level_name (string)(required) - level (map) name.
  trigger_center_position (vector)(required) - trigger center position.
  trigger_radius (int)(required) - trigger zone radius.
  callable_fn (function)(required) - callback (should return boolean: true to stop/delete trigger, false to keep listening).

closure(callable_fn): - callback arguments.
  The `callable_fn` is called on trigger events with:
  - level_name (string): level name
  - trigger_name (string): trigger name
  - trigger_center (vector): trigger center
  - trigger_radius (int): trigger radius
  - game_object_entity (entity): object that triggered
  - contact_position (vector): contact position
  - is_inside (bool): object entered zone
  - in_zone (bool): object is in zone
  - is_outside (bool): object left zone
  - source (string): object type (monster|npc|actor)

retval: (bool) - success.

--// Check if a trigger exists
TriggerExists(trigger_name)
args:
  trigger_name (string)(required) - trigger name.
retval: (bool) - does the trigger exist.

--// Remove a trigger
TriggerRemove(trigger_name)
args:
  trigger_name (string)(required) - trigger name.
retval: (bool) - success.
```

Examples:
```lua
--// wait for new game event
function on_game_start(callbackRegistrator)
  RegisterScriptCallback("new_game_created", this.on_new_game_started)
end

function on_new_game_started()
  --// Create a trigger with a 5 meter radius for the 'cordon' location (do this once, e.g. at new game start, as triggers are saved in the game save)
  TriggerCreate(
    "test-trg", 
    "l01_escape", 
    vector():set(-92, 0.67176, 677), 
    5.0, 
    function (level_name, trigger_name, trigger_center, trigger_radius, game_object_entity, contact_position, is_inside, in_zone, is_outside, source)
      ...
    end
  )

--// other triggers as needed
end

--// Check if a trigger exists
if TriggerExists("test-trg") then
  ...
end

--// Remove a trigger
if TriggerRemove("test-trg") then
  ...
end
```
