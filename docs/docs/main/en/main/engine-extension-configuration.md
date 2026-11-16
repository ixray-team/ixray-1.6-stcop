# Engine configuration — engine_external.ltx

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 0.7

## Overview

The engine reads `gamedata/configs/engine_external.ltx` from the active game data. The repository ships separate files for CoP (`gamedata`), CS (`gamedata_cs`), SoC (`gamedata_soc`), and CoC (`gamedata_coc`). CoC uses the `cop` platform mode.

The tables list C++ fallback values for missing keys. Shipped LTX files can override them. Boolean options fall back to `false` and are cached by section. Restart the game after editing the file.

See [shader options](../graphics/shader-options.md) for `[shaders_options]` and per-texture settings.

## Syntax

Place keys in the sections shown below. This example uses the CoP data set.

```ini
[general]
Platform = cop

[gameplay]
BalanceMode = auto
```

### [general]

| Key | Fallback | Effect |
| --- | --- | --- |
| `Platform` | `cop` | Platform mode. Accepted values are `cop`, `cs`, and `soc`. The `[general]` section is required to initialize the mode. |
| `title` | `IX-Ray Platform` | Title or localization key, also used for Discord status. |
| `SaveImageSize` | `128, 128` | Save thumbnail width and height in pixels. |

### [ui]

| Key | Fallback | Effect |
| --- | --- | --- |
| `DisableCharacterInfo` |  | Hides character information. |
| `DisableInventoryGrid` |  | Uses the alternate inventory grid texture. |
| `ShowLoadingStages` |  | Displays loading stages on the loading screen. |
| `DisableMotionIcon` |  | Hides the motion indicator. |
| `PdaRearrangeTabButtons` |  | Repositions PDA tabs based on their width. |
| `UseSavedGameStatic` |  | Uses a UI static for the saved-game notification. |
| `DisableHudRenderingOnMaster` |  | Hides the HUD on Master difficulty. |
| `UseCompassBar` |  | Selects the initial compass navigation mode for compatibility. See the HUD documentation for runtime switching. |
| `WeaponIconScale` | `0.8` | Weapon icon size multiplier. Shipped values vary by platform. |
| `InventoryItemCountPrefix` | `x` | Prefix for stack counters and quick-slot consumable counts. |

### [physics]

| Key | Fallback | Effect |
| --- | --- | --- |
| `DeadBodyRagdoll` |  | Enables ragdoll for bodies. |
| `DisableCameraMagicField` |  | Disables special handling of camera collision with dynamic objects. |

### [gameplay]

| Key | Fallback | Effect |
| --- | --- | --- |
| `BalanceMode` | `auto` | Selects balance formulas. Values are `auto`, `soc`, `cs`, and `cop`. |
| `EnableThirst` |  | Enables thirst. |
| `EnableSleepiness` |  | Enables sleepiness. |
| `EnableMedIntoxication` |  | Enables medication intoxication. |
| `EnableAiDieInAnomaly` |  | Enables AI death handling in anomalies. |
| `EnableNPCLookAtActor` |  | Enables NPCs looking at the actor. |
| `EnableBlockSprintInReload` |  | Blocks sprinting during reload. |
| `EnableAutoreload` |  | Enables automatic reloading. |
| `EnableMonstersInventory` |  | Enables monster inventories. |
| `EnableWeaponInertion` |  | Enables weapon inertia. |
| `EnableActorStepWallmarks` |  | Enables actor footprints using `[step_wallmark]`. |
| `DisableSprintWhileOverweight` |  | Blocks sprinting beyond the weight threshold. |
| `EnableAlternateZoomFovCalc` |  | Enables alternate aiming FOV calculation. |
| `EnableImproveWeaponMisfire` |  | Selects extended misfire handling. |
| `EnableDelayedWeaponActions` |  | Enables delayed weapon actions. |
| `EnableLegacyUpgradeSystem` |  | Selects the legacy Clear Sky upgrade path. |
| `EnableEngineArtefactSpawn` |  | Reads artifact spawn parameters from SoC anomaly configurations. |
| `Enable3DPDA` |  | Enables the 3D PDA. |
| `EnableTorchOnlyInOutfit` |  | Restricts torch use to when an outfit is equipped. |
| `EnableBurerShieldPenetrationWithGauss` |  | Lets Gauss rifle damage penetrate a Burer's shield. |
| `EnableWeaponAlternateRPMSystem` | `false` | With `true`, the selected interval replaces the shot timer. [Weapon keys and examples](../weapon-system/weapon-config-options.md). |
| `EnableInventoryVolume` |  | Enables the inventory volume system. |
| `EnableRealBulletPos` |  | Uses the actual firing position in first person. |
| `EnableWeaponAffectsOnMouseSensitivity` |  | Applies the weapon's `control_inertion_factor` to mouse sensitivity. |
| `EnableMainMenuForDisabledControls` |  | Allows opening the main menu when game controls are disabled. |
| `NewDemoRecordInputSchema` |  | Uses alternate movement controls in demorecord. |
| `EnablePolterStaminaLooseOnHit` |  | Declared in the enum, with no flag readers found in this `default` snapshot. |
| `EnableWindEffectOnABullet` |  | Enables experimental wind influence on bullets. |
| `EnableSaveOnNewGame` |  | Creates a save when starting a new game. |
| `EnableSuicideByController` |  | Enables the actor suicide sequence under a Controller's influence. |
| `Enable3DIcons` |  | Enables 3D item icons. |
| `DialogFovScale` | `0.7` | Camera FOV multiplier during dialogue. |
| `TalkDof` | `0.0, 0.5, 5.0, 0.0` | Four components of the dialogue depth-of-field parameter. |
| `SprintFovFactor` | `7.0` | Factor for the sprint FOV effect. |

### [spawn_supplies]

| Key | Fallback | Effect |
| --- | --- | --- |
| `EnableLoadoutsSupplies` |  | Enables supplies from NPC loadouts. |
| `EnableSpawnFullRandomLoadout` |  | Gives all items from one random loadout. |
| `EnableSpawnOnceRandomItemPerEachLoadouts` |  | Gives one random item from each loadout. |
| `EnableSpawnOnceRandomitemByRandomLoadout` |  | Gives one random item from one random loadout. |

### [render]

| Key | Fallback | Effect |
| --- | --- | --- |
| `DisableLoadScreenTips` |  | Hides loading-screen tips. |
| `UseDynamicSnowMask` |  | Enables a shader variant using the `*_snowmask.dds` texture. |
| `UseLegacyParticleLoader` |  | Selects the legacy action parser for SoC-format `particles.xr`. |
| `FontAtlasSize` | `4096` | Font atlas side length in pixels. |

### [environment]

| Key | Fallback | Effect |
| --- | --- | --- |
| `ReadSunConfig` |  | Reads sun position parameters from weather configurations. |

### [sound]

| Key | Fallback | Effect |
| --- | --- | --- |
| `EnableWorldAmbientOcclusion` |  | Enables occlusion for sounds of type `SOUND_TYPE_WORLD_AMBIENT`. |

### [localization]

| Key | Fallback | Effect |
| --- | --- | --- |
| `PreferedFallbackLanguage` | `eng` | Fallback string-table locale. Use the exact key spelling shown here. |

### [system]

| Key | Fallback | Effect |
| --- | --- | --- |
| `EngineScriptStoryID` |  | Enables engine handling of string story IDs and the corresponding Lua exports. |
| `CustomMessageInClipboardOnCrash` |  | Enables a custom crash clipboard message. |
| `DisablePause` |  | Blocks pausing through the `pause` input action. |
| `DisableConsole` |  | Blocks opening the console with its bound key in game and in the main menu. |

### [player_hud]

| Key | Fallback | Effect |
| --- | --- | --- |
| `PlayerHudOmfAdditional` | empty string | Paths or patterns for additional HUD hand animation OMF files. |

### [step_wallmark]

| Key | Fallback | Effect |
| --- | --- | --- |
| `materials` | empty string | Comma-separated material names. |
| `left_mark` | empty string | Section name for the left footprint. |
| `right_mark` | empty string | Section name for the right footprint. |

## Examples

### Selecting balance formulas

This example runs Clear Sky in the CS platform mode with CoP balance formulas.

```ini
[general]
Platform = cs

[gameplay]
BalanceMode = cop
```

`auto` uses the platform formulas and retains the existing compatibility rules for actor and NPC protection. Explicit `soc`, `cs`, or `cop` also selects that game's actor and NPC armor formulas. Item, ammunition, immunity, and armor data retain their configured values. An unknown value is logged and falls back to `auto`.

## Reading the file from Lua

The global function `external_ini()` returns an `ini_file` for reading the configuration. Check if a key exists when supporting multiple game data sets.

```lua
local options = external_ini()
local enabled = options:line_exist("gameplay", "Enable3DIcons")
    and options:r_bool("gameplay", "Enable3DIcons")
```

`is_cop()`, `is_cs()`, and `is_soc()` check the `Platform` mode.

## Recommendations and limitations

- `EnableInventoryPistolSlot` was replaced by [inventory slot settings](../interface/inventory-slots.md) in `mod_system_ixray.ltx`.
- `EnableWeaponCollision` and `EnablePolterDrop` appear in shipped LTX files. No engine or shipped Lua readers were found for these names in this snapshot.
- `EnablePolterStaminaLooseOnHit` is declared in a C++ enum without a flag reader in the current sources.

## See also

- [Shader options](../graphics/shader-options.md)
- [Launch arguments](launch-keys.md)
- [Console commands](console-commands.md)
