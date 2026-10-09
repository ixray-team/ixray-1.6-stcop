# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_particles_utils: `\gamedata\scripts\ixr_framework\utils\ffx_particles_utils.script`
Utilities for particle systems (.pg effects):
* `play_async`
* `destroy`
* `set_pos_and_dir_x_bazis`
* `set_pos_and_dir_y_bazis`
* `set_pos_and_dir_z_bazis`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_particles_utils.script]
--// Create or reuse a particle system. The current implementation ignores id, creates a new object with an automatic index and plays it at the given position.
play_async(id, pg_path, pos)
args:
  id (number)(required) - identifier (unused; overwritten internally).
  pg_path (string)(required) - particle system file path (.pg).
  pos (vector)(required) - position at which to play the effect.
retval: (none)

--// Stop and delete the particle system with the given id.
destroy(id)
args:
  id (number)(required) - particle system identifier.
retval: (none)

--// Set the particle object position and orientation using an X-axis direction (X basis).
set_pos_and_dir_x_bazis(pg_object, position, direction)
args:
  pg_object (userdata)(required) - particle system object.
  position (vector)(required) - new position.
  direction (vector)(required) - direction vector for the X basis.
retval: (none)

--// Set the particle object position and orientation using a Y-axis direction (Y basis).
set_pos_and_dir_y_bazis(pg_object, position, direction)
args:
  pg_object (userdata)(required) - particle system object.
  position (vector)(required) - new position.
  direction (vector)(required) - direction vector for the Y basis.
retval: (none)

--// Set the particle object position and orientation using a Z-axis direction (Z basis).
set_pos_and_dir_z_bazis(pg_object, position, direction)
args:
  pg_object (userdata)(required) - particle system object.
  position (vector)(required) - new position.
  direction (vector)(required) - direction vector for the Z basis.
retval: (none)
```
:::

### Usage examples:
```lua
--// Play a smoke effect at the actor position.
local actor_pos = db.actor:position()
ffx_particles_utils.play_async(1, "effects\\smoke.pg", actor_pos)

--// Stop and delete the effect by id.
ffx_particles_utils.destroy(1)

--// Create a particle object and set its orientation.
local pg_obj = particles_object("effects\\fire.pg")
local pos = vector():set(0, 0, 0)
local dir = vector():set(1, 0, 0)
ffx_particles_utils.set_pos_and_dir_x_bazis(pg_obj, pos, dir)

--// Set position and direction along Y.
ffx_particles_utils.set_pos_and_dir_y_bazis(pg_obj, pos, vector():set(0, 1, 0))

--// Set position and direction along Z.
ffx_particles_utils.set_pos_and_dir_z_bazis(pg_obj, pos, vector():set(0, 0, 1))
```
