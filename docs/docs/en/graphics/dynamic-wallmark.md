# Dynamic Wallmarks
Dynamic wallmarks are like regular SDK wallmarks but can be toggled at runtime. They let you add/remove decals during the story. Unlike bullet/grenade wallmarks that fade on a timer, these disappear only when a function is called, and you have more control over placement.
Use cases:

- A stalker camp is normal at game start; an offline attack happens in the story. When the player returns, blood and bullet holes appear alongside bodies.
- An empty area becomes occupied by a faction; their graffiti and signage appear on walls.

# Creating a dynamic wallmark

## SDK

1. Place the spawn object `ai/dynamic_wallmark` where needed.
2. Configure the object:

> - It has the same settings as a space restrictor plus wallmark options and two control buttons.
> - You can attach logic to it. For example, several dynamic wallmarks can enable themselves when an infoportion is received; issuing one infoportion can toggle many wallmarks at once.

```ini
[logic]
active = sr_idle@off

[sr_idle@off]
on_info = {+info_to_activate_wallmarks} sr_idle@on %=switch_wallmark(true)%

[sr_idle@on]
on_info = {-info_to_activate_wallmarks} sr_idle@off %=switch_wallmark(false)%
```

> - Wallmark settings match the Wallmarks tab. Avoid changing the default shader unless you know why.
> - The flag in the spawn object center represents the normal of the surface the wallmark will be placed on.
> - Two buttons are available: **Update** and **Convert Wallmark to Static**.

>> - **Update** refreshes the preview of how the wallmark will be placed. The preview is not saved or compiled.
>> - **Convert Wallmark to Static** generates a static wallmark with identical parameters at the same spot without deleting the dynamic object, so you can quickly stamp identical static wallmarks using the dynamic spawn object.

3. To add a dynamic wallmark to the game, only the spawn needs to be built.

## Game

Dynamic wallmarks are off by default. To toggle them use:

```ini
level.switch_wallmark("client object", "true|false")
```

To call this from logic, register the function in `xr_effects.script`, for example:

```lua
function switch_wallmark(actor, npc, p) -- called from logic of the dynamic wallmark object
	local isOn = p ~= nil
	if isOn then
		isOn = p[1] == "true"
	end
	level.switch_wallmark(npc, isOn)
end
```
