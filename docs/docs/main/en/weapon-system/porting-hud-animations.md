# Animation and sound alias renames

## Weapon animations

Keep the intermediate keys the engine checks when selecting a compound alias. For example, `anm_shoot_aim_last` needs `anm_shoot_aim`. Trailing numbers apply to classes that select an animation by the round count.

| Old alias | New alias |
| --- | --- |
| `anm_shot_l` | `anm_shoot_last` |
| `anm_shoot_l` | `anm_shoot_last` |
| `anm_shots` | `anm_shoot` |
| `anm_shot_aim` | `anm_shoot_aim` |
| `anm_shots_aim` | `anm_shoot_aim` |
| `anm_shots_when_aim` | `anm_shoot_aim` |
| `anm_shoot_when_aim` | `anm_shoot_aim` |
| `anm_shots_aim_scope` | `anm_shoot_aim_scope` |
| `anm_shots_last_aim` | `anm_shoot_aim_last` |
| `anm_shoot_last_aim` | `anm_shoot_aim_last` |
| `anm_shot_l_aim` | `anm_shoot_aim_last` |
| `anm_shot_1` | `anm_shoot_1` |
| `anm_shot_2` | `anm_shoot_2` |
| `anm_shot_aim_1` | `anm_shoot_aim_1` |
| `anm_shot_aim_2` | `anm_shoot_aim_2` |
| `anm_shots_jammed` | `anm_shoot_jammed` |
| `anm_shoots_jammed` | `anm_shoot_jammed` |
| `anm_shots_g` | `anm_shoot_g` |
| `anm_shots_aim_g` | `anm_shoot_aim_g` |
| `anm_shots_w_gl` | `anm_shoot_w_gl` |
| `anm_shoot_when_aim_w_gl` | `anm_shoot_aim_w_gl` |
| `anm_shoot_w_gl_when_aim` | `anm_shoot_aim_w_gl` |
| `anm_shot_l_w_gl` | `anm_shoot_last_w_gl` |
| `anm_shoot_w_gl_l` | `anm_shoot_last_w_gl` |
| `anm_shoot_last_aim_w_gl` | `anm_shoot_aim_last_w_gl` |
| `anm_reload_misfire` | `anm_reload_jammed` |
| `anm_reload_misfire_w_gl` | `anm_reload_jammed_w_gl` |
| `anm_reload_w_gl_misfire` | `anm_reload_jammed_w_gl` |
| `anm_changefiremode` | `anm_firemode` |
| `anm_changefiremode_aim` | `anm_firemode_aim` |
| `anm_changefiremode_empty` | `anm_firemode_empty` |
| `anm_changefiremode_jammed` | `anm_firemode_jammed` |
| `anm_changefiremode_w_gl` | `anm_firemode_w_gl` |
| `anm_switch_mode` | `anm_firemode` |
| `anm_switch_mode_empty` | `anm_firemode_empty` |
| `anm_switch_mode_jammed` | `anm_firemode_jammed` |
| `anm_idle_g_aim` | `anm_idle_aim_g` |
| `anm_idle_w_gl_aim` | `anm_idle_aim_w_gl` |
| `anm_idle_moving_slow_crouch` | `anm_idle_moving_crouch_slow` |
| `anm_idle_moving_slow_crouch_empty` | `anm_idle_moving_crouch_slow_empty` |
| `anm_idle_moving_slow_crouch_jammed` | `anm_idle_moving_crouch_slow_jammed` |
| `anm_idle_moving_slow_crouch_g` | `anm_idle_moving_crouch_slow_g` |
| `anm_idle_moving_slow_crouch_w_gl` | `anm_idle_moving_crouch_slow_w_gl` |
| `anm_kick_1` | `anm_kick` |
| `anm_kick_2` | `anm_kick2` |
| `anm_shots_l` | `anm_shoot_last` |
| `anm_shots_w_gl_l` | `anm_shoot_last_w_gl` |
| `anm_shot_w_gl_l` | `anm_shoot_last_w_gl` |
| `anm_shoot_aim_l` | `anm_shoot_aim_last` |
| `anm_shots_aim_l` | `anm_shoot_aim_last` |
| `anm_shots_w_gl_when_aim` | `anm_shoot_aim_w_gl` |
| `anm_shots_aim_w_gl` | `anm_shoot_aim_w_gl` |
| `anm_shots_aim_scope_w_gl` | `anm_shoot_aim_scope_w_gl` |
| `anm_shots_last_aim_scope` | `anm_shoot_aim_scope_last` |
| `anm_shoot_last_aim_scope` | `anm_shoot_aim_scope_last` |
| `anm_shots_last_aim_scope_w_gl` | `anm_shoot_aim_scope_last_w_gl` |
| `anm_shoot_last_aim_scope_w_gl` | `anm_shoot_aim_scope_last_w_gl` |
| `anm_shots_jammed_w_gl` | `anm_shoot_jammed_w_gl` |
| `anm_reload_misfire_empty` | `anm_reload_jammed_last` |
| `anm_reload_jammed_empty` | `anm_reload_jammed_last` |
| `anm_changefiremode_w_gl_aim` | `anm_firemode_aim_w_gl` |
| `anm_firemode_w_gl_aim` | `anm_firemode_aim_w_gl` |
| `anm_changefiremode_w_gl_jammed` | `anm_firemode_jammed_w_gl` |
| `anm_firemode_w_gl_jammed` | `anm_firemode_jammed_w_gl` |
| `anm_idle_moving_slow_crouch_jammed_w_gl` | `anm_idle_moving_crouch_slow_jammed_w_gl` |
| `anm_idle_moving_slow_crouch_empty_w_gl` | `anm_idle_moving_crouch_slow_empty_w_gl` |
| `anm_idle_moving_slow_crouch_jammed_g` | `anm_idle_moving_crouch_slow_jammed_g` |
| `anm_idle_moving_slow_crouch_empty_g` | `anm_idle_moving_crouch_slow_empty_g` |
| `anm_idle_moving_empty_slow` | `anm_idle_moving_slow_empty` |
| `anm_idle_moving_slow_crouch_0` | `anm_idle_moving_crouch_slow_0` |
| `anm_idle_moving_slow_crouch_1` | `anm_idle_moving_crouch_slow_1` |
| `anm_idle_moving_slow_crouch_2` | `anm_idle_moving_crouch_slow_2` |
| `anm_hand_kick_1` | `anm_kick` |
| `anm_hand_kick_2` | `anm_kick2` |

## Dry fire

| Old alias | New alias |
| --- | --- |
| `anm_fakeshoot_0` | `anm_empty_click_0` |
| `anm_fakeshoot_1` | `anm_empty_click_1` |
| `anm_fakeshoot_2` | `anm_empty_click_2` |
| `anm_fakeshoot_aim_0` | `anm_empty_click_aim_0` |
| `anm_fakeshoot_aim_auto_empty` | `anm_empty_click_aim_auto` |
| `anm_fakeshoot_aim_auto_empty_g` | `anm_empty_click_aim_auto_g` |
| `anm_fakeshoot_aim_auto_empty_w_gl` | `anm_empty_click_aim_auto_w_gl` |
| `anm_fakeshoot_aim_auto_g` | `anm_empty_click_aim_auto_g` |
| `anm_fakeshoot_aim_auto_jammed` | `anm_empty_click_aim_auto_jammed` |
| `anm_fakeshoot_aim_auto_jammed_g` | `anm_empty_click_aim_auto_jammed_g` |
| `anm_fakeshoot_aim_auto_jammed_w_gl` | `anm_empty_click_aim_auto_jammed_w_gl` |
| `anm_fakeshoot_aim_empty` | `anm_empty_click_aim` |
| `anm_fakeshoot_aim_empty_g` | `anm_empty_click_aim_g` |
| `anm_fakeshoot_aim_empty_w_gl` | `anm_empty_click_aim_w_gl` |
| `anm_fakeshoot_aim_g` | `anm_empty_click_aim_g` |
| `anm_fakeshoot_aim_jammed` | `anm_empty_click_aim_jammed` |
| `anm_fakeshoot_aim_jammed_0` | `anm_empty_click_aim_jammed_0` |
| `anm_fakeshoot_aim_jammed_1` | `anm_empty_click_aim_jammed_1` |
| `anm_fakeshoot_aim_jammed_2` | `anm_empty_click_aim_jammed_2` |
| `anm_fakeshoot_aim_jammed_g` | `anm_empty_click_aim_jammed_g` |
| `anm_fakeshoot_aim_jammed_w_gl` | `anm_empty_click_aim_jammed_w_gl` |
| `anm_fakeshoot_aim_triple_empty` | `anm_empty_click_aim_triple_empty` |
| `anm_fakeshoot_aim_triple_jammed` | `anm_empty_click_aim_triple_jammed` |
| `anm_fakeshoot_auto_empty` | `anm_empty_click_auto` |
| `anm_fakeshoot_auto_empty_g` | `anm_empty_click_auto_g` |
| `anm_fakeshoot_auto_empty_w_gl` | `anm_empty_click_auto_w_gl` |
| `anm_fakeshoot_auto_g` | `anm_empty_click_auto_g` |
| `anm_fakeshoot_auto_jammed` | `anm_empty_click_auto_jammed` |
| `anm_fakeshoot_auto_jammed_g` | `anm_empty_click_auto_jammed_g` |
| `anm_fakeshoot_auto_jammed_w_gl` | `anm_empty_click_auto_jammed_w_gl` |
| `anm_fakeshoot_empty` | `anm_empty_click` |
| `anm_fakeshoot_empty_g` | `anm_empty_click_g` |
| `anm_fakeshoot_empty_w_gl` | `anm_empty_click_w_gl` |
| `anm_fakeshoot_g` | `anm_empty_click_g` |
| `anm_fakeshoot_jammed` | `anm_empty_click_jammed` |
| `anm_fakeshoot_jammed_0` | `anm_empty_click_jammed_0` |
| `anm_fakeshoot_jammed_1` | `anm_empty_click_jammed_1` |
| `anm_fakeshoot_jammed_2` | `anm_empty_click_jammed_2` |
| `anm_fakeshoot_jammed_g` | `anm_empty_click_jammed_g` |
| `anm_fakeshoot_jammed_w_gl` | `anm_empty_click_jammed_w_gl` |
| `anm_fakeshoot_triple_empty` | `anm_empty_click_triple_empty` |
| `anm_fakeshoot_triple_jammed` | `anm_empty_click_triple_jammed` |

## Headlamp switching

Merge on and off keys when they use the same motion. Weapon-mounted flashlights and lasers have their own keys.

| Old alias | New alias |
| --- | --- |
| `anm_headlamp_off` | `anm_switch_device` |
| `anm_headlamp_off_0` | `anm_switch_device_0` |
| `anm_headlamp_off_1` | `anm_switch_device_1` |
| `anm_headlamp_off_2` | `anm_switch_device_2` |
| `anm_headlamp_off_auto` | `anm_switch_device_auto` |
| `anm_headlamp_off_auto_empty` | `anm_switch_device_auto_empty` |
| `anm_headlamp_off_auto_empty_g` | `anm_switch_device_auto_empty_g` |
| `anm_headlamp_off_auto_empty_w_gl` | `anm_switch_device_auto_empty_w_gl` |
| `anm_headlamp_off_auto_g` | `anm_switch_device_auto_g` |
| `anm_headlamp_off_auto_jammed` | `anm_switch_device_auto_jammed` |
| `anm_headlamp_off_auto_jammed_g` | `anm_switch_device_auto_jammed_g` |
| `anm_headlamp_off_auto_jammed_w_gl` | `anm_switch_device_auto_jammed_w_gl` |
| `anm_headlamp_off_auto_w_gl` | `anm_switch_device_auto_w_gl` |
| `anm_headlamp_off_empty` | `anm_switch_device_empty` |
| `anm_headlamp_off_empty_g` | `anm_switch_device_empty_g` |
| `anm_headlamp_off_empty_w_gl` | `anm_switch_device_empty_w_gl` |
| `anm_headlamp_off_first` | `anm_switch_device_first` |
| `anm_headlamp_off_g` | `anm_switch_device_g` |
| `anm_headlamp_off_jammed` | `anm_switch_device_jammed` |
| `anm_headlamp_off_jammed_1` | `anm_switch_device_jammed_1` |
| `anm_headlamp_off_jammed_2` | `anm_switch_device_jammed_2` |
| `anm_headlamp_off_jammed_g` | `anm_switch_device_jammed_g` |
| `anm_headlamp_off_jammed_w_gl` | `anm_switch_device_jammed_w_gl` |
| `anm_headlamp_off_triple` | `anm_switch_device_triple` |
| `anm_headlamp_off_triple_empty` | `anm_switch_device_triple_empty` |
| `anm_headlamp_off_triple_jammed` | `anm_switch_device_triple_jammed` |
| `anm_headlamp_off_w_gl` | `anm_switch_device_w_gl` |
| `anm_headlamp_on` | `anm_switch_device` |
| `anm_headlamp_on_0` | `anm_switch_device_0` |
| `anm_headlamp_on_1` | `anm_switch_device_1` |
| `anm_headlamp_on_2` | `anm_switch_device_2` |
| `anm_headlamp_on_auto` | `anm_switch_device_auto` |
| `anm_headlamp_on_auto_empty` | `anm_switch_device_auto_empty` |
| `anm_headlamp_on_auto_empty_g` | `anm_switch_device_auto_empty_g` |
| `anm_headlamp_on_auto_empty_w_gl` | `anm_switch_device_auto_empty_w_gl` |
| `anm_headlamp_on_auto_g` | `anm_switch_device_auto_g` |
| `anm_headlamp_on_auto_jammed` | `anm_switch_device_auto_jammed` |
| `anm_headlamp_on_auto_jammed_g` | `anm_switch_device_auto_jammed_g` |
| `anm_headlamp_on_auto_jammed_w_gl` | `anm_switch_device_auto_jammed_w_gl` |
| `anm_headlamp_on_auto_w_gl` | `anm_switch_device_auto_w_gl` |
| `anm_headlamp_on_empty` | `anm_switch_device_empty` |
| `anm_headlamp_on_empty_g` | `anm_switch_device_empty_g` |
| `anm_headlamp_on_empty_w_gl` | `anm_switch_device_empty_w_gl` |
| `anm_headlamp_on_first` | `anm_switch_device_first` |
| `anm_headlamp_on_g` | `anm_switch_device_g` |
| `anm_headlamp_on_jammed` | `anm_switch_device_jammed` |
| `anm_headlamp_on_jammed_1` | `anm_switch_device_jammed_1` |
| `anm_headlamp_on_jammed_2` | `anm_switch_device_jammed_2` |
| `anm_headlamp_on_jammed_g` | `anm_switch_device_jammed_g` |
| `anm_headlamp_on_jammed_w_gl` | `anm_switch_device_jammed_w_gl` |
| `anm_headlamp_on_triple` | `anm_switch_device_triple` |
| `anm_headlamp_on_triple_empty` | `anm_switch_device_triple_empty` |
| `anm_headlamp_on_triple_jammed` | `anm_switch_device_triple_jammed` |
| `anm_headlamp_on_w_gl` | `anm_switch_device_w_gl` |

## Gauss display

The `_auto` replacements apply to the legacy scheme where this suffix denoted an active display.

| Old alias | New alias |
| --- | --- |
| `anm_changefiremode_from_1_to_a` | `anm_mui_off` |
| `anm_changefiremode_from_1_to_a_empty` | `anm_mui_off_empty` |
| `anm_changefiremode_from_1_to_a_jammed` | `anm_mui_off_jammed` |
| `anm_changefiremode_from_a_to_1` | `anm_mui_on` |
| `anm_changefiremode_from_a_to_1_empty` | `anm_mui_on_empty` |
| `anm_changefiremode_from_a_to_1_jammed` | `anm_mui_on_jammed` |
| `anm_empty_click_aim_auto` | `anm_empty_click_aim_mui` |
| `anm_empty_click_aim_auto_jammed` | `anm_empty_click_aim_mui_jammed` |
| `anm_empty_click_auto` | `anm_empty_click_mui` |
| `anm_empty_click_auto_jammed` | `anm_empty_click_mui_jammed` |
| `anm_hide_auto` | `anm_hide_mui` |
| `anm_hide_auto_empty` | `anm_hide_mui_empty` |
| `anm_hide_auto_jammed` | `anm_hide_mui_jammed` |
| `anm_idle_aim_auto` | `anm_idle_aim_mui` |
| `anm_idle_aim_auto_empty` | `anm_idle_aim_mui_empty` |
| `anm_idle_aim_auto_jammed` | `anm_idle_aim_mui_jammed` |
| `anm_idle_aim_end_auto` | `anm_idle_aim_end_mui` |
| `anm_idle_aim_end_auto_empty` | `anm_idle_aim_end_mui_empty` |
| `anm_idle_aim_end_auto_jammed` | `anm_idle_aim_end_mui_jammed` |
| `anm_idle_aim_moving_back_auto` | `anm_idle_aim_moving_back_mui` |
| `anm_idle_aim_moving_back_auto_empty` | `anm_idle_aim_moving_back_mui_empty` |
| `anm_idle_aim_moving_back_auto_jammed` | `anm_idle_aim_moving_back_mui_jammed` |
| `anm_idle_aim_moving_forward_auto` | `anm_idle_aim_moving_forward_mui` |
| `anm_idle_aim_moving_forward_auto_empty` | `anm_idle_aim_moving_forward_mui_empty` |
| `anm_idle_aim_moving_forward_auto_jammed` | `anm_idle_aim_moving_forward_mui_jammed` |
| `anm_idle_aim_moving_left_auto` | `anm_idle_aim_moving_left_mui` |
| `anm_idle_aim_moving_left_auto_empty` | `anm_idle_aim_moving_left_mui_empty` |
| `anm_idle_aim_moving_left_auto_jammed` | `anm_idle_aim_moving_left_mui_jammed` |
| `anm_idle_aim_moving_right_auto` | `anm_idle_aim_moving_right_mui` |
| `anm_idle_aim_moving_right_auto_empty` | `anm_idle_aim_moving_right_mui_empty` |
| `anm_idle_aim_moving_right_auto_jammed` | `anm_idle_aim_moving_right_mui_jammed` |
| `anm_idle_aim_scope_auto` | `anm_idle_aim_scope_mui` |
| `anm_idle_aim_scope_auto_empty` | `anm_idle_aim_scope_mui_empty` |
| `anm_idle_aim_scope_auto_jammed` | `anm_idle_aim_scope_mui_jammed` |
| `anm_idle_aim_start_auto` | `anm_idle_aim_start_mui` |
| `anm_idle_aim_start_auto_empty` | `anm_idle_aim_start_mui_empty` |
| `anm_idle_aim_start_auto_jammed` | `anm_idle_aim_start_mui_jammed` |
| `anm_idle_auto` | `anm_idle_mui` |
| `anm_idle_auto_empty` | `anm_idle_mui_empty` |
| `anm_idle_auto_jammed` | `anm_idle_mui_jammed` |
| `anm_idle_crouch_auto` | `anm_idle_crouch_mui` |
| `anm_idle_crouch_auto_empty` | `anm_idle_crouch_mui_empty` |
| `anm_idle_crouch_auto_jammed` | `anm_idle_crouch_mui_jammed` |
| `anm_idle_crouch_slow_auto` | `anm_idle_crouch_slow_mui` |
| `anm_idle_crouch_slow_auto_empty` | `anm_idle_crouch_slow_mui_empty` |
| `anm_idle_crouch_slow_auto_jammed` | `anm_idle_crouch_slow_mui_jammed` |
| `anm_idle_moving_auto` | `anm_idle_moving_mui` |
| `anm_idle_moving_auto_empty` | `anm_idle_moving_mui_empty` |
| `anm_idle_moving_auto_jammed` | `anm_idle_moving_mui_jammed` |
| `anm_idle_moving_crouch_auto` | `anm_idle_moving_crouch_mui` |
| `anm_idle_moving_crouch_auto_empty` | `anm_idle_moving_crouch_mui_empty` |
| `anm_idle_moving_crouch_auto_jammed` | `anm_idle_moving_crouch_mui_jammed` |
| `anm_idle_moving_crouch_slow_auto` | `anm_idle_moving_crouch_slow_mui` |
| `anm_idle_moving_crouch_slow_auto_empty` | `anm_idle_moving_crouch_slow_mui_empty` |
| `anm_idle_moving_crouch_slow_auto_jammed` | `anm_idle_moving_crouch_slow_mui_jammed` |
| `anm_idle_moving_slow_auto` | `anm_idle_moving_slow_mui` |
| `anm_idle_moving_slow_auto_empty` | `anm_idle_moving_slow_mui_empty` |
| `anm_idle_moving_slow_auto_jammed` | `anm_idle_moving_slow_mui_jammed` |
| `anm_idle_slow_auto` | `anm_idle_slow_mui` |
| `anm_idle_slow_auto_empty` | `anm_idle_slow_mui_empty` |
| `anm_idle_slow_auto_jammed` | `anm_idle_slow_mui_jammed` |
| `anm_idle_sprint_auto` | `anm_idle_sprint_mui` |
| `anm_idle_sprint_auto_empty` | `anm_idle_sprint_mui_empty` |
| `anm_idle_sprint_auto_jammed` | `anm_idle_sprint_mui_jammed` |
| `anm_idle_sprint_end_auto` | `anm_idle_sprint_end_mui` |
| `anm_idle_sprint_end_auto_empty` | `anm_idle_sprint_end_mui_empty` |
| `anm_idle_sprint_end_auto_jammed` | `anm_idle_sprint_end_mui_jammed` |
| `anm_idle_sprint_start_auto` | `anm_idle_sprint_start_mui` |
| `anm_idle_sprint_start_auto_empty` | `anm_idle_sprint_start_mui_empty` |
| `anm_idle_sprint_start_auto_jammed` | `anm_idle_sprint_start_mui_jammed` |
| `anm_reload_auto` | `anm_reload_mui` |
| `anm_reload_auto_empty` | `anm_reload_mui_empty` |
| `anm_shoot_aim_auto` | `anm_shoot_aim_mui` |
| `anm_shoot_aim_auto_last` | `anm_shoot_aim_mui_last` |
| `anm_shoot_aim_scope_auto` | `anm_shoot_aim_mui_scope` |
| `anm_shoot_aim_scope_auto_last` | `anm_shoot_aim_mui_scope_last` |
| `anm_shoot_auto` | `anm_shoot_mui` |
| `anm_shoot_auto_last` | `anm_shoot_mui_last` |
| `anm_shoot_auto_suicide` | `anm_shoot_mui_suicide` |
| `anm_show_auto` | `anm_show_mui` |
| `anm_show_auto_empty` | `anm_show_mui_empty` |
| `anm_show_auto_jammed` | `anm_show_mui_jammed` |
| `anm_stop_suicide_auto` | `anm_stop_suicide_mui` |
| `anm_suicide_auto` | `anm_suicide_mui` |
| `anm_switch_device_auto` | `anm_switch_device_mui` |
| `anm_switch_device_auto_empty` | `anm_switch_device_mui_empty` |
| `anm_switch_device_auto_jammed` | `anm_switch_device_mui_jammed` |

## Removing aliases from LTX

Check direct script calls, references in other sections, and HUD inheritance. If the key is defined in a parent section, removing the child assignment restores the inherited value.

| Alias | When to remove it |
| --- | --- |
| `anm_reload_w_gl_empty` | Remove the redundant empty reload key. For `_w_gl` mode, move the value to `anm_reload_empty_w_gl` if that key is missing. |
| `anm_shoot_variant1`, `anm_reload_variant1`, `anm_reload_empty_variant1`, `anm_reload_empty_variant2`, `anm_reload_empty_variant3` | Remove them if they were defined only for random selection. Random variants use one base alias, and the loader looks for numbered motion names in the OMF. If a variant uses a separate item motion, check its playback before merging. |
| `anm_idle_aim_moving_slow*`, `anm_idle_aim_moving_crouch*`, `anm_idle_aim_scope_moving_slow*`, `anm_idle_aim_scope_moving_crouch*` | Remove `_slow`, `_crouch`, and `_crouch_slow` variants appended after `_moving` for the standard aimed weapon selector. It uses `_moving` and directional variants. |
| `anm_shoot_last_aim_scope`, `anm_shoot_last_aim_scope_w_gl` | Remove the old suffix order after moving the value to `anm_shoot_aim_scope_last` or `anm_shoot_aim_scope_last_w_gl`. If scoped and unscoped shots use the same motion, `anm_shoot_aim_last` or `anm_shoot_aim_last_w_gl` is enough. |
| `anm_shoot_aim_last_jammed`, `anm_shoot_last_aim_jammed` | Remove these from the standard firing path. On a regular magazine-fed weapon, misfires and last shots use separate branches. With an underbarrel launcher attached in bullet mode, the combination uses `_jammed_last`, for example `anm_shoot_aim_jammed_last_w_gl`. |
| `anm_shoot_aim_scope`, `anm_shoot_aim_scope_w_gl`, `anm_shoot_aim_scope_last`, `anm_shoot_aim_scope_last_w_gl` | Remove them only when they match the ordinary versions without `_scope` or when you intentionally use one motion for both cases. Keep an intermediate key if it selects a more specific variant. For example, `anm_shoot_aim_scope` can be required to select `anm_shoot_aim_scope_last`. |
| `anm_idle_aim_moving` and its supported state or direction variants, `anm_idle_aim_scope_moving` | Remove these only when dropping the corresponding behavior. The engine checks these keys. The presence of `anm_idle_aim_scope_moving` affects selection with a scope attached. |
| `anm_shoot_aim_l` | Remove it after moving the desired motion to `anm_shoot_aim_last`. The `_l` suffix still serves as a fallback. |
| `anm_bore`, `anm_bore_empty`, `anm_bore_jammed` | Remove placeholders that repeat the regular idle motion. If the weapon has no inspect animation, set `disable_bore = true` in the HUD section. |
| Duplicate `anm_*` assignments in a child HUD section | Remove the duplicate if the parent defines the same alias with the same clips, speed, and bone parts. |

## Sounds

The `snd_checkout` replacement applies to the sound at the start of a HUD throw. You can rename `snd_reload_grenade` in the base weapon section. Keep `snd_reload_grenade` in upgrades that change the launcher reload sound.

Keep the built-in firing keys `snd_silncer_shot` and `snd_shot_last` as spelled. `snd_empty` sets the event sound. `snd_anm_empty_click` sets an additional sound for that animation.

| Old alias | New alias |
| --- | --- |
| `snd_reload_misfire` | `snd_reload_jammed` |
| `snd_reload_misfire_last` | `snd_reload_jammed_last` |
| `snd_reload_misfire_empty` | `snd_reload_jammed_last` |
| `snd_reload_jammed_empty` | `snd_reload_jammed_last` |
| `snd_reload_misfire_detector` | `snd_reload_jammed_detector` |
| `snd_reload_misfire_last_detector` | `snd_reload_jammed_last_detector` |
| `snd_draw_misfire` | `snd_draw_jammed` |
| `snd_draw_misfire_g` | `snd_draw_jammed_g` |
| `snd_draw_misfire_w_gl` | `snd_draw_jammed_w_gl` |
| `snd_holster_misfire` | `snd_holster_jammed` |
| `snd_holster_misfire_g` | `snd_holster_jammed_g` |
| `snd_holster_misfire_w_gl` | `snd_holster_jammed_w_gl` |
| `snd_bore_misfire` | `snd_bore_jammed` |
| `snd_reload_grenade` | `snd_load_grenade` |
| `snd_checkout` | `snd_throw_begin` |
| `snd_anm_<old_alias>` | `snd_anm_<new_alias>` |

## Gauss display switching sounds

Merge the listed variants for each direction if they use the same sound.

| Old alias | New alias |
| --- | --- |
| `snd_anm_changefiremode_from_1_to_a` | `snd_mui_off` |
| `snd_anm_changefiremode_from_1_to_a_empty` | `snd_mui_off` |
| `snd_anm_changefiremode_from_1_to_a_jammed` | `snd_mui_off` |
| `snd_anm_changefiremode_from_a_to_1` | `snd_mui_on` |
| `snd_anm_changefiremode_from_a_to_1_empty` | `snd_mui_on` |
| `snd_anm_changefiremode_from_a_to_1_jammed` | `snd_mui_on` |
