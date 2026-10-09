# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_spawn_utils: `\gamedata\scripts\ixr_framework\utils\ffx_spawn_utils.script`
Utilities for spawning objects (creating game entities):
* `spawn_on_ground_by_actor_pos`
* `actor_multiple_spawn_to_backpack`
* `create_item_on_story_object`
* `actor_spawn_random_section_to_backpack`
* `spawn_by_section`
* `spawn_on_ground_for_current_level`
* `actor_spawn_to_backpack`
* `npc_spawn_to_backpack`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_spawn_utils.script]
--// Create the given number of objects from a section on the ground at the actor position.
spawn_on_ground_by_actor_pos(section, count)
args:
  section (string)(required) - object section name.
  count (number)(optional) - number of objects (default 1).
retval: (table|false) - table of created server objects (se_object), or false on error.

--// Create the given number of items from a section in the actor inventory.
actor_multiple_spawn_to_backpack(section, count)
args:
  section (string)(required) - item section name.
  count (number)(optional) - number of items (default 1).
retval: (table|false) - table of server objects, or false on error.

--// Create items at a story object identified by story_id. With spawn_into = true, place them inside its inventory.
create_item_on_story_object(section, story_id, count, spawn_into)
args:
  section (string)(required) - item section name.
  story_id (string)(required) - story ID of the target object.
  count (number)(optional) - number of items (default 1).
  spawn_into (boolean)(optional) - if true, create inside the object using its parent ID; otherwise create at its position (default false).
retval: (table|false) - table of server objects, or false on error.

--// Create a random item from a list of sections in the actor inventory.
actor_spawn_random_section_to_backpack(items_sections)
args:
  items_sections (table)(required) - array of section name strings.
retval: (server_object|false) - created server object, or false if the list is empty or an error occurs.

--// Basic spawning. Create an object from a section at the given position and vertices, with an optional parent ID.
spawn_by_section(section, position, lv_id, gv_id, id)
args:
  section (string)(required) - section name.
  position (vector)(required) - position vector.
  lv_id (number)(required) - level vertex ID (level_vertex_id).
  gv_id (number)(required) - game vertex ID (game_vertex_id).
  id (number)(optional) - parent object ID for an inventory or container.
retval: (server_object|false) - server object on success; otherwise false.

--// Create an object on the ground at the given position in the current level, using actor vertices.
spawn_on_ground_for_current_level(section, position)
args:
  section (string)(required) - section name.
  position (vector)(required) - position vector.
retval: (server_object|false) - server object or false.

--// Create an item in the actor inventory.
actor_spawn_to_backpack(section)
args:
  section (string)(required) - item section name.
retval: (server_object|false) - server object or false on error.

--// Create an item in an NPC inventory using its server ID.
npc_spawn_to_backpack(section, npc_id)
args:
  section (string)(required) - item section name.
  npc_id (number)(required) - server ID of the NPC.
retval: (server_object|false) - server object or false on error.
```
:::

### Usage examples:
```lua
--// Create 3 medkits on the ground near the actor.
local spawned = ffx_spawn_utils.spawn_on_ground_by_actor_pos("medkit", 3)
if spawned then
    SemiLog(string.format("Objects created: %d", #spawned))
end

--// Add 5 ammunition items to the actor backpack.
local items = ffx_spawn_utils.actor_multiple_spawn_to_backpack("ammo_5.56x45_ss109", 5)

--// Create an item inside a story object (a crate).
local box_story = "st_weapon_box_1"
local result = ffx_spawn_utils.create_item_on_story_object("wpn_ak74", box_story, 1, true)

--// Random item from a list.
local random_item = ffx_spawn_utils.actor_spawn_random_section_to_backpack({"medkit", "bandage", "antirad"})

--// Basic spawn at a specific position.
local pos = vector():set(100, 0, 50)
local obj = ffx_spawn_utils.spawn_by_section("ammo_9x19_fmj", pos, actor:level_vertex_id(), actor:game_vertex_id())

--// Spawn on the ground at the supplied position in the current level.
local obj2 = ffx_spawn_utils.spawn_on_ground_for_current_level("grenade_f1", pos)

--// Add a grenade to the actor backpack.
local grenade = ffx_spawn_utils.actor_spawn_to_backpack("grenade_f1")

--// Add an item to an NPC backpack by ID.
local npc_id = 12345
local item = ffx_spawn_utils.npc_spawn_to_backpack("medkit", npc_id)
```
