# PDA statistics

Statistics track weapons and killed mutants by their full configuration section names. A new weapon is not grouped with a stock weapon simply because it uses the same engine class. Mutants can have their own artwork and strength priority configured in LTX.

This article describes the implementation in the current branch and the common `gamedata/scripts` set. Scripts in `gamedata_coc`, `gamedata_cs`, and `gamedata_soc` have separate implementations; changing a common script does not automatically add the feature to those game data sets.

## Installation

Start a new game when switching to this implementation: migration of statistics from the previous save layout is not supported. Restart the game after changing scripts or LTX files. The grenade kill attribution fix also requires an updated `xrGame.dll` for the build configuration you run.

## Strongest killed mutant

A mutant is considered when its kill is credited to the player. Stalkers are excluded from this statistic. The script stores the full section name of the server ALife object, such as `m_controller_normal` or `bloodsucker_custom`.

Priority is determined as follows:

1. If the section in `pda_statistics.ltx` has a `rank` key, that value is used.
2. Otherwise, the server object's `rank()` is used.
3. The first credited mutant is selected even when its rank is `0`.
4. A subsequent mutant replaces it only if its rank is strictly greater. Equal ranks keep the first kill.

This compares ranks, not health, damage dealt, or kill counts. If a mutant with an equal or higher rank was killed before a controller, the PDA artwork will not change, although the total mutant kill counter will increase.

### Configuration

Entries belong in `gamedata/configs/misc/pda_statistics.ltx`. Each section name in this file must exactly match the mutant's section in the game configuration.

| Key | Type | When absent |
| --- | --- | --- |
| `back` | String: UI texture ID for the background | Stock artwork selected by class and the original ALife rank, when known |
| `icon` | String: UI texture ID for the overlay | Stock overlay artwork, when known |
| `rank` | Integer from `0` to `4294967295` | The server ALife object's rank |

An absent key and an empty value have different meanings. `icon =` explicitly hides the overlay; omitting `icon` selects the fallback artwork. The same rule applies to `back`. Known stock profiles usually use only `back`.

An unknown mutant class no longer aborts statistics processing. It can become the strongest mutant by rank, but its artwork will be empty unless textures are configured or a fallback profile is known.

Settings are read from the exact section in `pda_statistics.ltx`. Mutant section inheritance in `system.ltx` does not establish matching inheritance in the statistics file.

### Adding a mutant

Suppose the creature configuration already contains this mutant:

```ini
[bloodsucker_custom]:bloodsucker_strong
; Other parameters of the new mutant go here.
```

Add this entry to `pda_statistics.ltx`:

```ini
[bloodsucker_custom]
back = ui_inGame2_Krovosos_2
icon =
rank = 20
```

The example uses the existing strong bloodsucker texture and sets a priority of `20`. Remove the `rank` line to use the original ALife rank. A rank set here affects only statistics selection, not the mutant's gameplay parameters.

### Custom artwork

Create the texture `gamedata/textures/ui/my_mod_pda_mutants.dds` and its XML description at `gamedata/configs/ui/textures_descr/my_mod_pda_mutants.xml`:

```xml
<w>
    <file name="ui\my_mod_pda_mutants">
        <texture id="my_mod_pda_bloodsucker" x="0" y="0" width="393" height="200" />
    </file>
</w>
```

Dimensions and coordinates must match the actual artwork. `393 × 200` is the size of the stock mutant background. XML files in `ui/textures_descr` are loaded when the main menu is initialized.

Then change the LTX entry:

```ini
[bloodsucker_custom]
back = my_mod_pda_bloodsucker
icon =
rank = 20
```

Both `back` and `icon` refer to IDs from the XML, not DDS paths. Use a unique ID for your modification.

## Favorite weapon

Each full section gets its own counter. For example, `wpn_ak74` and `wpn_ak74_custom` are tracked independently even if they share a CLSID. Weapons do not need to be added to a separate statistics list.

The favorite weapon is selected by the accumulated sum of positive damage values passed to `set_best_weapon(amount)`. This is neither the number of shots nor the number of kills. Zero and negative values are ignored. Stock death handlers also generate a synthetic hit with an amount of `1`, so this statistic should not be treated as an exact measurement of health lost by targets.

If two weapons have equal totals, the current favorite remains selected. Until statistics are available, the PDA returns the `wpn_knife` section.

### Grouping variants with parent_section

To count a scoped variant together with its base weapon, set `parent_section` in the variant's main game configuration section:

```ini
[wpn_ak74_custom_scope]:wpn_ak74_custom
parent_section = wpn_ak74_custom
```

Hits with this variant increase the `wpn_ak74_custom` counter, and the PDA displays the base section. Without `parent_section`, the variant is tracked separately.

The value is used only when it is nonempty and the referenced section exists in `system_ini()`. An empty or unknown parent keeps the weapon's own section. Only one redirect is resolved: a chain of `parent_section` entries is not followed recursively.

An inherited `parent_section` is also read from the resolved game section. To give a child variant its own counter, override it with an empty value:

```ini
[wpn_ak74_custom_scope]:wpn_ak74_custom
parent_section =
```

### Weapon artwork

The PDA receives the full section of the favorite weapon and uses its existing upgrade icon parameters: `upgr_icon_x`, `upgr_icon_y`, `upgr_icon_width`, `upgr_icon_height`, and the optional `upgr_icons_texture`. In 3D icon mode, visual parameters are read from the same section.

2D rendering requires all four `upgr_icon_*` parameters. When using `parent_section`, the selected parent section must contain the display parameters. For atlas configuration, see [Custom icon atlases](/en/interface/custom-icon-atlases).

### 3D icon in statistics

In `gamedata/configs/ui/pda_ranking.xml` and its widescreen variant `pda_ranking_16.xml`, enable the `use_3d` attribute on the existing element:

```xml
<favorite_weapon_icon x="775" y="625" width="230" height="80" alignment="c" align="c" use_3d="true"/>
```

`use_3d="true"` enables a model for this element only; `false` keeps the 2D icon. When the attribute is absent, the default is the global `Enable3DIcons` setting in `[gameplay]` of `engine_external.ltx`. If an addon overrides the PDA XML, edit its active layout instead.

The model is selected from `3d_static_visual_name`, falling back to `visual` when the key is absent. The selected weapon section also supports:

```ini
3d_static_visual_name = dynamics\weapons\wpn_p90\wpn_p90.ogf
3d_static_rotate = 0, 90, 0
3d_static_scale = 1.0
3d_static_offset = 0.0, 0.0
```

This example assumes the P90 model is installed. Rotation is in degrees; scale multiplies the model size after fitting it into the UI element; offset is relative to the element's width and height. `def_hide_bones` and `def_show_bones` are applied; the `wpn_launcher` and `wpn_silencer` bones are hidden for the base weapon representation. Set `3d_static_visual_name` to use a separate icon model.

3D rendering does not require upgrade atlas coordinates. If the model is missing or unsuitable for a skeletal icon, rendering falls back to 2D when all `upgr_icon_*` parameters are present. Without either a model or a complete 2D parameter set, the field remains empty.

Counting checks the active item's type through `game_object:is_weapon()`, which returns `true` for weapons and hand grenades. It does not depend on the old startup callback populating the Lua `weapon_classes` table.

## Grenades: kill credit and favorite weapon

Kill credit uses the explosion initiator. A grenade preserves the thrower's ID and passes it to explosion hits upon detonation. Killing a mutant with the player's grenade can increase the total kill counter and update the strongest mutant according to the usual rank comparison rules. An NPC's grenade should not credit its kill to the player.

Favorite weapon counting currently uses the player's active item **at the time of impact**, not the source of the specific projectile. If the player switches to a rifle after throwing a grenade, the grenade's damage may be added to the rifle's counter. If no weapon is active, that damage is not added to weapon statistics. Preserving the explosion initiator fixes lost kill credit, but does not resolve this weapon attribution limitation.

## Saves and configuration changes

Saves store the strongest mutant's section, selected rank, fallback artwork profile, favorite weapon section, and counters by section. Both serialization paths support this data layout. The weapon entry count is stored as `u16`, removing the previous 255-section limit; the format supports up to 65535 entries.

Changing `rank` in LTX does not recalculate the strongest mutant's already saved rank. Changing `parent_section` does not merge previously accumulated counters. These settings apply to subsequent events; use a new game to test changes to the balance.

## Verification and troubleshooting

| Observation | What to check |
| --- | --- |
| A killed mutant did not replace the artwork | The kill was credited to the player; its rank is strictly greater than the saved rank |
| The kill counter increases, but artwork is empty | The exact section name, `back`/`icon` values, XML and DDS files; an unknown class needs custom settings |
| A new weapon is displayed as the base weapon | Its own or inherited `parent_section` |
| A weapon is counted, but its icon does not change | In 2D mode, the complete `upgr_icon_*` set; in 3D mode, the active XML, model, and model settings |
| A grenade kill is not credited to the player | `xrGame.dll` is updated, the player threw the grenade, and the target died from its hit |
| Grenade damage increases another weapon's counter | The active item changed between throwing and impact; this is a current limitation |
| Statistics are incorrect after loading | The save was created with the new layout; installed scripts are matching versions |

To verify the feature, start a new game and deal different damage totals with stock and custom weapons, then repeat with a variant that has `parent_section`. Kill mutants with lower, equal, and higher priorities; check empty texture values and grenade kills. Save and reload: the selected sections and counters should persist.
