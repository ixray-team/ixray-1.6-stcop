# Weapon Classes
## CHudItem

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

* Sounds

```ini
snd_switch_device = "snd" ; Sound for turning on/off headlamp/NVG
snd_headlamp_on = "snd" ; Sound for turning on headlamp
snd_headlamp_off = "snd" ; Sound for turning off headlamp
snd_nv_on = "snd" ; Sound for turning on NVG
snd_nv_off = "snd" ; Sound for turning off NVG
snd_gasmask = "snd" ; Sound of gas mask wiping
snd_prepare_detector = "snd" ; Sound of taking detector with main item (only works if anm_prepare_detector animation exists)
snd_finish_detector = "snd" ; Sound of putting detector with main item (only works if anm_finish_detector animation exists)
```

In addition to all the listed sounds, it is possible to play a sound from an animation in the HUD section, example:

```ini
anm_reload = "anim"
snd_anm_reload = "snd"

Important note. Regular sounds will also play if there is a special sound for them. In case of reloading, both `snd_reload` and `snd_anm_reload` will play, in other words, they are not interchangeable.
```

## CMissile

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.1

* General

```ini
checkout_bones = idle, idle2 ; List of bones that will be visible only when in use
```

* Sounds

```ini
snd_draw = "snd" ; Sound of drawing
snd_holster = "snd" ; Sound of holstering
snd_throw_begin = "snd" ; Sound of throwing windup
snd_throw = "snd" ; Sound of throwing
```

## CWeapon

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.1

* General

```ini
use_condition = false; Does the item have wear
```

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

* Updating bone visibility

```ini
Write in main section:
def_hide_bones = tac_foregrip, tac_handguard, tac_block
def_show_bones = def_foregrip, def_ironsight, def_handguard
def_hide_bones_override_when_gl_attached = muzzle, knife, tac_foregrip
no_scope_overriding_hide_bones = launcher_scope_static
no_scope_overriding_show_bones = launcher_scope
collimator_sights_bones = colim_sights

Write in scope section (from scopes parameter):
bones = scope1
hide_bones = mount
overriding_hide_bones = ironsight, ironsight_sight
overriding_show_bones = ironsight_rail

Write in upgrade section:
hide_bones_override = def_foregrip
hide_bones_override_when_silencer_attached = muzzle, knife
hide_bones_override_when_gl_attached = knife
hide_bones_override_when_scope_attached = tac_block
hide_bones = ironsight
show_bones = alt_colim_scope
```

* Custom magazine size

```ini
ammo_classes = 20x70_buck, 12x70_buck ; 20x70 is type zero, 12x70 is type one
ammo_mag_size_for_type_0 = 3 ; For 20x70 specify magazine size 3
ammo_mag_size_for_type_1 = 5 ; For 12x70 specify magazine size 5
```

If there is no `ammo_mag_size_for_type_` line for the ammo type, the default `ammo_mag_size` parameter will be used.
This only works correctly with `tri_state_reload` disabled!!! Does not work for underbarrel grenade launchers and double barrels.

* Ammo in chamber

```ini
ammo_in_chamber = false ; Is ammo in the chamber needed
```

Does not depend on `ammo_mag_size`, so it doesn't need to be changed, one round is automatically added.
Features:

1. Of course, under certain conditions the ammo counter will display one round more;
2. The ammo icon on the HUD will correspond to the type of ammo in the chamber, not in the magazine.

Do not use on RPGs and double barrels.

* Input blockers

```ini
block_reload = true ; Should reload be blocked if the fire animation hasn't finished (pump or bolt action)
block_firemode_glm = true ; Should fire mode animation be blocked when in underbarrel grenade launcher mode
restricted_gl_and_sil = false ; Will the silencer be removed if grenade launcher is attached, and vice versa
```

A new external `EnableDelayedWeaponActions` has been added, which will prevent sprint animation from being interrupted to start an action animation.
If the reload button was pressed (fire, aim, change fire mode, etc.) while running, we will wait for the run to end and only then reload, makes sense if there are in/out sprint animations.

* Jam system

The advanced jam system is enabled with the `EnableImproveWeaponMisfire` external, which allows using new parameters.

```ini
no_jam_fire = false ; Is jam needed before or during fire? If during fire, then false. Write in HUD section.
no_jam_in_first_shot = false ; Should jam be blocked on first shot?
actor_can_shoot = true ; Can the actor shoot from the weapon?

use_light_misfire = false ; Should light misfire system be used
disable_light_misfires_with_detector = false ; Should light misfires be disabled if detector is active
light_misfire_start_condition = 1.0 ; Weapon condition when light misfires may start
light_misfire_end_condition = 0.0 ; Weapon condition when light misfires become constant
light_misfire_start_probability = 1.0 ; Light misfire chance in range between start and end condition
light_misfire_end_probability = 0.0 ; Light misfire chance in range below end condition
```

* Weapon device breakdown (weapon condition)

```ini
collimator_breaking_params = -1, -1, 0 ; Built-in collimator sight breakdown, when in bad condition collimator_sights_bones are hidden
torch_breaking_params = -1, -1, 0 ; Tactical flashlight breakdown, when in bad condition flashlight flickers
```

First value - weapon condition when problems start.
Second value - weapon condition when device stops working completely.
Third value - chance of device "lag" when weapon is at the condition from first value.

* Weapon device breakdown (electronics problems)

```ini
misfire_after_problems_level = 10.0 ; Electronics problem level at which weapon jam triggers
collimator_problems_level = 0.0 ; Electronics problem level at which collimator lags, collimator_sights_bones are hidden
```

Important to note that electronics problems won't work if your surge_manager.script doesn't accumulate problem level from 0 to 5.

* Scope system

```ini
snd_scope_brightness_plus = "snd" ; Sound of increasing scope brightness
snd_scope_brightness_minus = "snd" ; Sound of decreasing scope brightness
snd_scope_zoom_plus = "snd" ; Sound of increasing scope magnification
snd_scope_zoom_minus = "snd" ; Sound of decreasing scope magnification
snd_scope_zoom_gyro = "snd" ; Sound of smooth scope magnification change

min_lens_factor = 1.0 ; Minimum scope magnification value
max_lens_factor = 1.0 ; Maximum scope magnification value
lens_speed = 0.0 ; Magnification change speed
lens_gyro_sound_period = 0.0 ; Delay before playing snd_scope_zoom_gyro sound
lens_factor_levels_count = 5.0 ; Number of magnification positions
force_zoom_sound = false ; Play magnification sounds even if at minimum/maximum limit
```

* Strafes (SWM)

```ini
strafe_enabled = true; Are weapon strafes allowed
strafe_aim_enabled = false; Are weapon strafes allowed in aim mode
strafe_hud_offset_pos = 0.015, 0, 0; Axes and strength of weapon position change in strafes
strafe_hud_offset_rot = 0, 0, 4.5; Axes and strength of weapon rotation change in strafes
strafe_aim_hud_offset_pos = 0.005, 0, 0; Axes and strength of weapon position change in aim strafes
strafe_aim_hud_offset_rot = 0, 0, 2.5; Axes and strength of weapon rotation change in aim strafes
strafe_transition_time = 0.5; Time for weapon to move to strafe_hud_offset_pos and strafe_hud_offset_rot coordinates
strafe_aim_transition_time = 0.15; Time for weapon to move to strafe_aim_hud_offset_pos and strafe_aim_hud_offset_rot coordinates
```

* HUD collision (SWM)

```ini
nearwall_dist_min = float       ; Minimum distance when HUD collision starts working
nearwall_dist_max = float       ; Maximum distance when HUD collision works
nearwall_target_hud_fov = float ; How much the HUD FOV is offset at maximum
nearwall_speed_mod = float      ; Offset speed
```

* Inertia (SWM)

```ini
inertion_tendto_speed = float         ; Speed at which inertia tends to return to original position after movement
inertion_tendto_aim_speed = float     ; Speed of inertial return while aiming
inertion_tendto_ret_speed = float     ; Speed at which inertia returns to original position
inertion_tendto_ret_aim_speed = float ; Speed of inertial return to original position after aiming ends

inertion_min_angle = float       ; Minimum angle affected by inertia
inertion_min_angle_aim = float   ; Minimum inertia angle while aiming

inertion_offset_LRUD = float     ; Amount of inertia offset left, right, up and down
inertion_offset_LRUD_aim = float ; Amount of inertia offset left, right, up and down while aiming
```

> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.2

* Scope system by Mortan

### How it works

The following parameters are written in the main weapon section:

```ini
parent_section = string ; Replace string with the original weapon section name without quotes (example: wpn_ak74)
scopes = scope1, scope2, etc ; Scope sections that can be attached to the weapon.
```

Then a special weapon section is created for a specific scope named as `weaponname_scopename` (example: wpn_ak74_ekp), inheriting the original weapon section. There you set separate visuals, HUD, scope coordinates, etc.

## CWeaponKnife

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

```ini
snd_draw = "snd" ; Sound of drawing
snd_holster = "snd" ; Sound of holstering
snd_kick_1 = "snd" ; Sound of attack on LMB
snd_kick_2 = "snd" ; Sound of attack on RMB

Important to note. The presence of `snd_kick_1` and `snd_kick_2` excludes the playback of the `snd_shoot` sound.
```

## CWeaponMagazined

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.1

* Sounds

```ini
snd_reload_misfire = "snd" ; Sound of reloading with jam
snd_reload_empty = "snd" ; Sound of reloading empty magazine
snd_aim = "snd" ; Sound of entering aim
snd_aim_out = "snd" ; Sound of exiting aim
```

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

* Sounds

```ini
snd_aim_start = "snd" ; Sound of entering aim (renamed from snd_aim to snd_aim_start)
snd_aim_end = "snd" ; Sound of exiting aim (renamed from snd_aim_out to snd_aim_end)
snd_reload_jammed | snd_reload_misfire = "snd" ; Sound of reloading with jam (snd_reload_misfire support hasn't disappeared! This is not a rename!)
snd_reload_jammed_last | snd_reload_misfire_last = "snd" ; Sound of reloading with jam when out of ammo
snd_reload_jammed_detector | snd_reload_misfire_detector = "snd" ; Sound of reloading with jam, with detector active
snd_reload_jammed_last_detector | snd_reload_misfire_last_detector = "snd" ; Sound of reloading with jam when out of ammo, with detector active
snd_changecartridgetype = "snd" ; Sound of reload when changing ammo type (doesn't work when out of ammo)
snd_changefiremode = "snd" ; Sound of fire mode selector
snd_torch_on = "snd" ; Sound of turning on tactical flashlight
snd_torch_off = "snd" ; Sound of turning off tactical flashlight
snd_breechblock = "snd" ; Sound of pump or bolt action
snd_jam = "snd" ; Sound of jammed pump or bolt action
snd_light_misfire = "snd" ; Sound of light misfire (only works with light misfire enabled and EnableImproveWeaponMisfire external enabled)
snd_shoot_actor = "snd" ; Sound of fire by actor
snd_shot_last_actor = "snd" ; Sound of fire with last round by actor
snd_silncer_shot_actor = "snd" ; Sound of fire with silencer by actor
snd_silencer_shot_last = "snd" ; Sound of fire with silencer with last round
snd_silencer_shot_last_actor = "snd" ; Sound of fire with silencer with last round by actor
```

### CWeaponMagazinedWGrenade

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

* Sounds

```ini
snd_shoot_grenade_actor = "snd" ; Sound of underbarrel grenade launcher fire by actor
snd_load_grenade | snd_reload_grenade = "snd" ; Sound of underbarrel grenade launcher reload (alternative, doesn't replace snd_reload_grenade)
snd_switch_g = "snd" ; Sound of exiting underbarrel grenade launcher
```

### CWeaponBM16

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

* Sounds

```ini
snd_reload_only = "snd" ; Sound of reload with one round of loading type in inventory
snd_reload_only_ammochange = "snd" ; Sound of reload with one round of loading type in inventory, when changing type
snd_changecartridgetype_one = "snd" ; Sound of reload when changing type with not fully loaded weapon
snd_changecartridgetype_only = "snd" ; Sound of reload when changing type with one round of loading type in inventory and fully loaded weapon
snd_changecartridgetype_one_only = "snd" ; Sound of reload when changing type with one round of loading type in inventory and not fully loaded weapon

Important note. The above sounds only work with `use_alt_reload_system` enabled in the weapon section. Also, in addition to these sounds, `snd_changecartridgetype` and `snd_reload_empty` sounds are used, `snd_reload_1` sound is disabled.
```

### CWeaponShotgun | CWeaponAutomaticShotgun

> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.3

```ini
add_cartridge_in_open = false; Should a round be added in open animations, write in HUD section
```

* Sounds

```ini
snd_open_weapon_empty = "snd" ; Sound of starting empty shotgun reload
snd_add_cartridge_empty = "snd" ; Sound of inserting round in empty shotgun
snd_add_cartridge_preloaded = "snd" ; Sound of inserting round in shotgun after chamber insertion
snd_close_weapon_empty = "snd" ; Sound of ending empty shotgun reload (ammo counter should be 0)
snd_close_weapon_preloaded = "snd" ; Sound of ending shotgun reload after chamber insertion

Important note. Some of the above sounds only work with the add_cartridge_in_open parameter.
```
