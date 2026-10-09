# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_vector_utils: `\gamedata\scripts\ixr_framework\utils\ffx_vector_utils.script`
Utilities for 3D vectors: positions, directions, cloning, normalization and rotation:
* `mul_in_direction`
* `clone`
* `get_device_position`
* `get_device_direction`
* `normalize`
* `rotate_quaternion`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_vector_utils.script]
--// Move a position in the given direction by the specified distance. Return a new vector.
mul_in_direction(position, direction, length)
args:
  position (vector)(required) - initial position (vector object).
  direction (vector)(required) - direction vector.
  length (number)(required) - distance.
retval: (vector) - new position (clone).

--// Create a deep copy of a vector as a new vector object.
clone(vec)
args:
  vec (vector)(required) - vector to clone.
retval: (vector) - new vector with the same coordinates.

--// Get the current camera position as a new vector.
get_device_position()
args: (none)
retval: (vector) - camera position.

--// Get the current camera direction as a new vector.
get_device_direction()
args: (none)
retval: (vector) - camera direction.

--// Normalize a vector, returning a table with x,y,z fields rather than a vector object.
normalize(v)
args:
  v (table|vector)(required) - vector with x,y,z components.
retval: (table) - normalized vector as {x=..., y=..., z=...}, or a zero vector if the length is 0.

--// Rotate a vector around base_vector by an angle in radians using quaternion rotation.
rotate_quaternion(vector_a, base_vector, angle)
args:
  vector_a (table|vector)(required) - vector to rotate.
  base_vector (table|vector)(required) - rotation axis, normalized internally.
  angle (number)(required) - rotation angle in radians.
retval: (table) - rotated vector as a table {x, y, z}.
```
:::

### Usage examples:
```lua
--// Move forward by 10 meters.
local pos = db.actor:position()
local dir = db.actor:direction()
local new_pos = ffx_vector_utils.mul_in_direction(pos, dir, 10)
db.actor:set_position(new_pos)

--// Clone a vector.
local original = vector():set(1, 2, 3)
local copy = ffx_vector_utils.clone(original)
copy.x = 5
--// original remains (1,2,3)

--// Get camera position/direction.
local cam_pos = ffx_vector_utils.get_device_position()
local cam_dir = ffx_vector_utils.get_device_direction()
SemiLog(string.format("Camera: %.2f,%.2f,%.2f", cam_pos.x, cam_pos.y, cam_pos.z))

--// Normalization.
local raw = {x=3, y=4, z=0}
local norm = ffx_vector_utils.normalize(raw)
--// norm = {x=0.6, y=0.8, z=0}

--// Rotate a vector around the Y axis by 90 degrees (PI/2).
local v = {x=1, y=0, z=0}
local axis = {x=0, y=1, z=0}
local rotated = ffx_vector_utils.rotate_quaternion(v, axis, math.pi/2)
--// rotated ~= {x=0, y=0, z=-1} (depending on the coordinate system)
```
