# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_object_utils: `\gamedata\scripts\ixr_framework\utils\ffx_object_utils.script`
Utilities for client and server game objects: validation, conversion, condition management and bone positions:
* `has_actor`
* `se_object_by_id_or_false`
* `se_object_or_false`
* `game_object_or_false`
* `is_actor_alive`
* `safe_release`
* `release`
* `create`
* `get_inv_name`
* `safe_bone_pos`
* `safe_release_by_id`
* `set_condition_by_id`
* `set_condition`
* `to_game_object`
* `test_to_game_object`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_object_utils.script]
--// Check whether the actor exists, with caching.
has_actor()
args:
  (none)
retval: (boolean) - true if the actor exists; otherwise false.

--// Get a server object by ID, or false if not found.
se_object_by_id_or_false(id)
args:
  id (number)(required) - server object ID.
retval: (server_object|false) - the server object if valid; otherwise false.

--// Return a server object if valid; otherwise false.
se_object_or_false(se_obj)
args:
  se_obj (userdata)(required) - server object to check.
retval: (server_object|false) - the original object if valid; otherwise false.

--// Return a client game object if valid; otherwise false.
game_object_or_false(g_obj)
args:
  g_obj (userdata)(required) - client object to check.
retval: (game_object|false) - the original object if valid; otherwise false.

--// Check whether the actor exists and alive() is true.
is_actor_alive()
args:
  (none)
retval: (boolean) - true if the actor exists and is alive; otherwise false.

--// Safely release a server object using a table containing its ID. With force = true, release immediately; otherwise try to switch it offline first.
safe_release(p, force)
args:
  p (table)(required) - table with the object ID at index 1, such as {id}.
  force (boolean)(optional) - if true, force release; otherwise release after switching offline (default false).
retval: (boolean) - true if the object was released successfully or was already released; false if it remains online and could not be switched offline.

--// Release a server object directly.
release(se_obj)
args:
  se_obj (userdata)(required) - server object to release.
retval: (none)

--// Create an object through ALife.
create(tbl)
args:
  tbl (table)(required) - table of parameters: {section, position_vector, level_vertex_id, game_vertex_id}.
retval: (none)

--// Get a section inventory name from inv_name in the system INI. Return the section name if not found.
get_inv_name(section)
args:
  section (string)(required) - section name.
retval: (string) - localized inventory name or section name.

--// Safely get an NPC bone position. If the bone is missing or the object is not a stalker, return a position above the current position.
safe_bone_pos(npc, bone)
args:
  npc (userdata)(required) - client object (NPC).
  bone (string)(optional) - bone name (default "bip01_spine").
retval: (vector) - position vector.

--// Safely release a server object by ID.
safe_release_by_id(id)
args:
  id (number)(required) - ID of the object to release.
retval: (none)

--// Set a client object condition by server ID. Uses client_spawn_manager to wait for the object to appear online.
set_condition_by_id(id, float_condition)
args:
  id (number)(required) - server object ID.
  float_condition (number)(required) - condition value, clamped to 0..1.
retval: (none)

--// Set a client object condition directly, clamping the value to 0..1.
set_condition(gobj_client, float_condition)
args:
  gobj_client (userdata)(required) - client object.
  float_condition (number)(required) - condition value, clamped to 0..1.
retval: (none)

--// Wrap a server or client object in a uniform interface with safe methods. Invalid input produces a dummy wrapper whose methods return the default value.
to_game_object(obj, def_value)
args:
  obj (userdata|table)(required) - server or client object to wrap.
  def_value (any)(optional) - value returned by dummy wrapper methods (default nil).
retval: (table) - wrapper table with methods: id(), section(), name(), alive(), level_vertex_id(), game_vertex_id(), position(), is_valid(), is_client_object(), is_server_object(), await_online(callback).

--// Test function for to_game_object that prints various wrapped object properties for debugging.
test_to_game_object(_gobj)
args:
  _gobj (userdata)(optional) - object to test; uses the actor if omitted.
retval: (none)
```
:::

### Usage examples:
```lua
--// Check whether the actor exists.
if ffx_object_utils.has_actor() then
    SemiLog("Actor exists")
end

--// Get a server object by ID.
local se_obj = ffx_object_utils.se_object_by_id_or_false(12345)
if se_obj then
    SemiLog(string.format("Server object found, ID: %d", se_obj.id))
end

--// Validate a client object.
local g_obj = ffx_object_utils.game_object_or_false(db.actor)
if g_obj then
    SemiLog("Actor is a valid client object")
end

--// Check whether the actor is alive.
if ffx_object_utils.is_actor_alive() then
    SemiLog("Actor is alive")
end

--// Safely release an object using a table containing its ID.
local obj_table = { se_obj_id }
ffx_object_utils.safe_release(obj_table, true)  --// force release

--// Create an object.
local position = vector():set(100, 0, 50)
ffx_object_utils.create({"ammo_5.56x45_ss109", position, 0, 0})

--// Get the inventory name.
local inv_name = ffx_object_utils.get_inv_name("wpn_ak74")
SemiLog(string.format("Name: %s", inv_name))

--// Get a bone position.
local pos = ffx_object_utils.safe_bone_pos(db.actor, "bip01_head")
SemiLog(string.format("Head position: %.2f, %.2f, %.2f", pos.x, pos.y, pos.z))

--// Set condition (health) by ID.
ffx_object_utils.set_condition_by_id(actor_id, 0.8)

--// Use the uniform to_game_object wrapper.
local wrapped = ffx_object_utils.to_game_object(db.actor)
if wrapped:is_valid() then
    SemiLog(string.format("Object: %s, ID: %d", wrapped:section(), wrapped:id()))
end

--// Wait for the object to appear online through await_online.
wrapped:await_online(function(gobj)
    SemiLog("Object is now online")
end)

--// Test function for debugging.
ffx_object_utils.test_to_game_object(db.actor)
```
