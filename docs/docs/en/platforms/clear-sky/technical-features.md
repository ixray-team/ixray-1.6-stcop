# Clear Sky (1.5.10)
## General features

* Uses the same `xrGame.dll` module as CoP, so most CoP engine features work in CS and vice versa.
* Knife config (`gamedata\configs\weapons\w_knife.ltx`) uses CoP parameters.
* Anomaly detection is now by sections, not classes (`gamedata\configs\misc\radiation_counter.ltx`).
* Veles detector uses CoP artefact detection (`gamedata\configs\ui\ui_detector_artefact.xml`).
* Scripts ported from CoP: `level_weathers.script`, `ph_door.script`, `sr_no_weapon.script`, `xr_sound.script`.
* Using CoP `xrGame.dll` enables the CoP UI.
* Multiplayer includes many CoP UI fixes.
* `eat_portions_num` for food set from `-1` to `1` to avoid infinite food.
* Rain sound is stereo like CoP (`gamedata\sounds\ambient\rain.ogg`).
* UI hints (`gamedata\configs\ui\hint_item.xml`) taken from CoP.
* NPC state fixes added in `gamedata\scripts\state_mgr_direction.script`.
* Crouch/stand toggled via checkbox in options (not a separate key), as in CoP.
* Added text `Press any key to start the game` to `gamedata\configs\ui\game_tutorials.xml`.
* Stalker fire behavior controlled by `fire_queue_params` section, as in CoP.
* Inventory shows item condition, as in CoP.
* Language switching added to main menu (`gamedata\configs\ui\ui_mm_main.xml`).

## Default values in `engine_external.ltx`

CS `gamedata` differs from CoP in default `engine_external.ltx` settings:

```ini
[general]
Platform = cs ; Platform in use; cs = Clear Sky
title = st_ixray_title_cs ; Discord Rich Presence name, unique for CS

[ui]
WeaponIconScale = 0.65 ; Ammo icon size in maingame.xml (CoP default 0.8, CS 0.65)
ShowLoadingStages = true ; Loading stages such as "Client: Sync"
DisableMotionIcon = true ; Disable noise/visibility meters (not present in CS)
PdaRearrangeTabButtons = true ; Old PDA tab sizing formula (Faction War style)

[gameplay]
EnableInventoryPistolSlot = true ; Second slot only for pistols

[render]
DisableLoadScreenTips = true ; Disable load-screen tips
```
