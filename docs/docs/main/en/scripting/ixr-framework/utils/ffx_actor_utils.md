# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0


### ffx_actor_utils: \gamedata\scripts\ixr_framework\utils\ffx_actor_utils.script
Actor utilities:
* `is_in_crouch`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_actor_utils.script]
--// Check whether the actor is crouching.
is_in_crouch()
args:
  (none)
retval: (boolean) - true if the actor exists and is crouching; otherwise false.
```
:::

### Usage examples:
```lua
if ffx_actor_utils.is_in_crouch() then
  SemiLog("Actor is crouching")
else
  SemiLog("Actor is not crouching")
end
```
