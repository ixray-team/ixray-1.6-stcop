# First-Person Animation System (Hud Animator)
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

A system for playing first-person animations.

## Item Usage

Usage Example: Find the section of the item you need to add an animator to. In the item section, you need to add the line `animator_sect`, followed by the equal sign and the name of the HUD animator section. For example: `animator_sect = conserva_eat_animator`.

## Headlamp/Night Vision Device Usage

**Usage Example:** Find the `game_global.ltx` file in `gamedata/configs`. It contains two relevant sections: `night_vision` and `headlamp`. These sections have parameters that control the animator for turning the Headlamp/NVD on and off: `headlamp_animator` for the headlamp and `night_vision_animator` for the NVD, respectively. By default, these parameters point to the `nvs_animator` section, which does not exist by default. You can either override these parameters to point to a different section or create the `nvs_animator` section.

## Usage via Scripts

**Usage Example:** Functions for running the HUD animator via scripts have been exported, for example, for cutscenes. HUD animator functions are directly linked to the actor, so you need to call them through the actor using `db.actor`. For instance, if you need to play a cutscene where the protagonist uses an item or looks at a document.

```lua
function main_story_look_doc()
   db.actor:start_hud_animator("look_doc_animator")
end
```

## HUD Animator LUA Functions

```lua
   start_hud_animator(string) - Starts the HUD animator. The argument should specify the name of the section to launch.
   stop_hud_animator() - Stops the HUD animator playback.
   is_hud_animator_active() - Returns a boolean indicating if the HUD animator is currently active.
   get_hud_animator_section() - Returns a string, the name of the current HUD animator section.
   get_hud_animator_restored_slot() - Returns u8, the slot of the active item that was hidden to launch the HUD animator.
   get_hud_animator_force_hide_items() - Returns a boolean indicating if instant weapon hiding before launching the HUD animator is enabled.
   set_hud_animator_force_hide_items(boolean) - The argument specifies whether to instantly hide the weapon before launching the HUD animator.
```

## HUD Animator Section Contents
Since the HUD animator does not require a fake item or a dedicated slot, all it needs to function is a section with specific parameters.

```ini
[antirad_use]
item_visual = dynamics\weapons\wpn_eat\injectors\wpn_injector_antirad_hud.ogf ; Path to the visual to display (Optional. If only hands are needed, this line should be absent)

;Position and rotation (similar to weapons)
hands_orientation			 = 0.000000,0.000000,0.000000 ; Hand rotation
hands_orientation_16x9			 = 0.000000,0.000000,0.000000

hands_position	                         = 0.000000,0.011600,0.015000 ; Hand position
hands_position_16x9                      = 0.000000,0.011600,0.015000

; Sounds (similar to weapons)
sound_1 = interface\item_usage\antirad_use ; Path to the sound to play (Optional. If no sound is needed, this line should be absent)
; If there are multiple sounds:
sound_1 = interface\item_usage\antirad_use
sound_11 = interface\item_usage\antirad_use1
sound_12 = interface\item_usage\antirad_use2
sound_13 = interface\item_usage\antirad_use3

;Animation (similar to weapons)
anm_show = antirad ; Specifies both the hand animation and the item animation. The order is the same as for weapons: `anm_show = hand animation, item animation`. If the hand and item animation names are the same, it's enough to write it once, as in this example. Animation speed can also be specified with a comma after the animation names: `anm_show = antirad, 3.2`

;Sprint restriction
can_sprint = true ; Allows/forbids sprinting during the playback of THIS animator, depending on true or false. false forbids sprinting.

;Animation blending
blend = true ; Enables blending with the previous HUD animator animation. Visible changes will be minimal.

;HUD FOV change
hud_fov = 55 ; Specifies a float value that is set as the hud fov for the duration of the HUD animator playback.

; NOTE. All subsequent described LUA callbacks default to null. If you need to nullify a script call reference, write null.

;LUA. HUD animator section modification (section substitution)
modify_sect_lua_callback = animator_callbacks.modify_exo_sect ; Script.Function. The function must always have an argument (any name), into which the current HUD animator section name will be passed. In this example, we substitute the antirad section if an exoskeleton is equipped:

;LUA. Usage availability check
precondition_functor = safe_zones.in_safe_zone ; A script function that returns true/false. If it returns false, the HUD animator will not start because some condition described in the called callback is not met. Primarily used to block quick melee attacks or grenade throws in safe zones.

;LUA. Various callbacks during the HUD animator's operation
;Note: The names left, left2, right, right2 have nothing to do with hands. By default in the stalker motion marks, only these four names exist in the SDK.

start_lua_callback = null ; Calls a script at the very start of the HUD animator
end_lua_callback = null ; Calls a script at the very end of the HUD animator animation
left_lua_callback = null ; Calls a script when the required frame in the "Left" motion mark is reached
left2_lua_callback = null ; Calls a script when the required frame in the "Left2" motion mark is reached
right_lua_callback = null ; Calls a script when the required frame in the "Right" motion mark is reached
right2_lua_callback = null ; Calls a script when the required frame in the "Right2" motion mark is reached
```

```lua
--The function must always return a string!!!
function modify_exo_sect(section)
	if db.actor then
		local outfit = db.actor:item_in_slot(7)
		if outfit and (outfit:section() == "exo_outfit" or outfit:section() == "svoboda_exo_outfit") and ini:line_exist(section, "modify_exo_sect") then
			return ini:r_string(section, "modify_exo_sect") -- Read from the HUD animator section the parameter `modify_exo_sect`, which points to the section for the antirad animator in an exoskeleton
		end
	end
	
	return section
end
```

## Adding Motion Marks to an Animation
This example uses the fan-made software [OMF Editor](https://github.com/VaIeroK/Omf-Editor).
Open the animation in any software that supports plugins for Stalker's animation format, such as the common ones: 3Ds Max, Blender, and Maya, which have such plugins. Since the example focuses on Blender, a link to [PavelBlend's plugin for Blender](https://github.com/PavelBlend/blender-xray) is provided.

I won't describe the entire process of loading animations, etc. It's only important to look at the animation and find the needed frame where we will place the mark/marks.

In my case, it's the 60th frame when the pills are behind the camera. This frame will be used, for example, to call the required script.

![image](https://github.com/user-attachments/assets/63825218-256c-42cf-9e9b-77480c13734b)
![image](https://github.com/user-attachments/assets/a6258c01-55f9-4510-b019-577cc7e1460f)

Open the OMF file for hand animations and find the needed animation. For convenience, select the `Keys` format in the `Motion time format` option.

![image](https://github.com/user-attachments/assets/3c270fd2-83f4-4dda-b662-3a139e4b8710)
![image](https://github.com/user-attachments/assets/fbe1532f-5a95-473d-81e3-f4c802fd5606)

A bit lower, find the column labeled `Motion Marks`. If it's grayed out and the buttons in it are also gray, just above the `Motion time format` option there is a column with checkboxes. We need the Has `Motion Marks` checkbox; enable it.

![image](https://github.com/user-attachments/assets/e038981a-bf54-451d-a73f-80d51b54783f)

In the `Motion Marks` section there are two additional columns with `Add` and `Del` buttons. In the leftmost column, press `Add`. Because the HUD animator is designed for the original motion marks `Left`, `Left2`, and so on, create only marks from the menu that appears after clicking `Add`. I need `Left`, so I create that one.

![image](https://github.com/user-attachments/assets/93fc0a45-9a7c-4311-a25d-23366b024263)

Our mark appears in the left column. Now press the `Add` button in the rightmost column. If this is our first and only mark, an element named `0_mark0` will be created in the right column. A bit to the right of the mark columns, there are `Start` and `End` rows with fields below them. By default, the start field is zero, and the end field equals the animation length (118 frames in my case). The end field must not be changed under any circumstances, or the mark will not work correctly! We only edit the start. Earlier, I found the frame I needed in the animation—the 60th frame.

**Important note: Near the very top, there is a `Speed` field. It controls the animation speed, and the speed affects the motion mark trigger. If speed = 1, we enter the desired frame and don't worry. If `Speed` is greater or less than 1, we divide the desired frame by the animation speed. If the animation speed were 2, we would have to set the start time to 30, not 60. If the animation was mistakenly made at 60fps or higher, and a speed of 2 was chosen specifically to hide this flaw, that's a completely different situation not covered in this example.**

![image](https://github.com/user-attachments/assets/11c8effd-3395-4065-890a-7316b4a12775)
![image](https://github.com/user-attachments/assets/c8a4247d-042e-492f-b647-83581a5dd921)

Work in the OMF Editor is finished. Save and close. Since I chose the `Left` mark and want to make a script call on it, I need to specify `left_lua_callback` in the HUD animator config.

**Important note!! Motion marks are used directly by the engine to call special functions. For example, the NVD animator uses the `Left` mark to trigger the NVD activation at the right frame. Engine callbacks are hardcoded only for consumable animators, the headlamp, and NVD, and they all use the `Left` mark by default. At the moment, other marks are not used for engine callbacks.**

## anim_fake system (legacy)
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.1 <br>
> **Maximum Version**: 1.2.2

The system is based on a fully separate slot (this method solves many problems with detectors/weapon hiding/etc), which is why a base slot item is created: `anim_fake`. This item needs to be placed in the actor's inventory (can be done at spawn).

### anim_fake
* `misc/anim_slot.ltx`
```ini
[anim_fake]:identity_immunities
class = F_ANMITM
slot = 22
can_trade = false
can_take = true
cost = 0
inv_weight = 0
inv_name = "animfake"
inv_name_short = "animfake"
default_to_ruck = false
sprint_allowed = true

; hud sect
animation_slot = 2
control_inertion_factor = 1
hud_fov = 45
hud = anim_fake

; For Debug
inv_grid_width		= 1
inv_grid_height		= 1
inv_grid_x		= 11
inv_grid_y		= 9

visual = dynamics\devices\dev_aptechka\dev_aptechka_low.ogf ; unused

; Hud attach
item_visual = dynamics\devices\dev_aptechka\dev_aptechka_low.ogf ; unused
attach_place_idx = 0

hands_position = 0, 0, 0
hands_orientation = 0, 0, 0
hands_position_16x9 = 0, 0, 0
hands_orientation_16x9 = 0, 0, 0

aim_hud_offset_pos = 0, 0, 0
aim_hud_offset_rot = 0, 0, 0
aim_hud_offset_pos_16x9 = 0, 0, 0
aim_hud_offset_rot_16x9 = 0, 0, 0

gl_hud_offset_pos = 0, 0, 0
gl_hud_offset_rot = 0, 0, 0
gl_hud_offset_pos_16x9 = 0, 0, 0
gl_hud_offset_rot_16x9 = 0, 0, 0

item_position = 0, 0, 0
item_orientation = 0, 0, 0
```
The interesting parameter here is `hud_fov`, as most of the other parameters are overridden in the animation sections.

## Animation Parameters
A section is constructed as follows:
```ini
[my_hud_anim]
hands_position         = -0.0055,-0.05, 0.32         ; Hand position
hands_orientation      = -0.500000,1.000000,1.149999 ; Hand rotation
hands_position_16x9    = -0.0055,-0.05, 0.32         ; Hand position (16x9)
hands_orientation_16x9 = 2.349998,2.749999,2.849998  ; Hand rotation (16x9)

item_position          = 0.000075,-0.004612,0.001283 ; Item position
item_orientation       = 0.045015,1.047468,0.056850  ; Item rotation

item_visual            = dynamics\weapons\wpn_eat\injectors\wpn_injector_big_antirad_hud.ogf ; HUD animation model
anm_use                = antirad_hands_eat_invtrans, antirad_wpn_eat                         ; Animation to play (for non-food items, you can specify your own name)
attach_place_idx       = 0
```
> * Support for item usage animations is also available. To use it, simply move the contents of the section above into the item's section.

## Playing an Animation
If you need to play an animation (aside from the moment of using an item), you can call the Lua function:
```lua 
animslot.play("my_hud_anim", "anm_use")
```
