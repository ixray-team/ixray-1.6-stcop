# Engine Extension Configuration

## Overview

The configuration file allows you to configure various aspects of the game and engine through a system of sections and parameters. The file uses INI format with grouping of settings by functional categories

## Main Sections

### [general]

**Basic engine settings**

| Parameter | Values | Description |
|----------|----------|----------|
| **Platform** | `cop`, `cs`, `soc` | Platform mode: cop - Call of Pripyat, cs - Clear Sky, soc - Shadow of Chernobyl |
| **title** | text | Name of the modification |
| **SaveImageSize** | 128,128 | Size of save screenshots |


### [gameplay]: balance formulas

| Parameter | Values | Description |
|----------|----------|----------|
| **BalanceMode** | `auto`, `soc`, `cs`, `cop` | Select damage, protection, artifact and condition formulas. `auto` preserves the platform defaults and existing armor/material compatibility rules. |

For example, to use Call of Pripyat calculations with the Clear Sky platform:

```ini
[general]
Platform = cs

[gameplay]
BalanceMode = cop
```

The setting is read on startup; restart the game after changing it. Explicit `soc`, `cs` and `cop` also select the corresponding actor and NPC armor formulas. In `auto`, actor armor still follows its configuration and NPC armor follows its bone protection settings and `game_materials` version. The engine does not convert item, ammunition, immunity or armor values: using a different formula can require rebalancing those configurations. Test existing saves after changing modes. An unknown value is logged and falls back to `auto`.

### [shaders_options]

**Shader and graphics settings**

| Parameter | Values | Description |
|----------|----------|----------|
| USE_LEGACY_LIGHT | 0/1 | 1 - legacy pipeline, 0 - PBR pipeline |
| USE_BGRA_SKYCOLOR | 0/1 | Sky color format BGRA |
| USE_LEGACY_SKY_TONEMAP | 0/1 | Original ToneMapping format for sky |
| USE_CGIM_SKY_TWEAK | 0/1 | CGIM2 compatibility |
| USE_CGIM_WHITE_TWEAK | 0/1 | White color settings for CGIM2 |

### [ui]

**User interface**

| Parameter | Values | Description |
|----------|----------|----------|
| **DisableCharacterInfo** | true/false | Disable character information |
| **DisableInventoryGrid** | true/false | Use alternate inventory grid |
| **WeaponIconScale** | 0.65-0.9 | Weapon icon scale (CoP: 0.8, CS: 0.65, SoC: 0.9) |
| **ShowLoadingStages** | true/false | Show loading stages from CS/SoC |
| **DisableMotionIcon** | true/false | Disable motion icon |
| **PdaRearrangeTabButtons** | true/false | CS-style buttons with size depending on text |
| **UseSavedGameStatic** | true/false | Use static save screenshots |

### [physics]

**Physics and interactions**

| Parameter | Values | Description |
|----------|----------|----------|
| **DeadBodyRagdoll** | true/false | Enable ragdoll physics for bodies |

### [gameplay]

**Gameplay and mechanics**

#### Basic Mechanics

| Parameter | Values | Description |
|----------|----------|----------|
| **EnableThirst** | true/false | Enable thirst mechanic |
| **EnableSleepiness** | true/false | Enable sleepiness mechanic |
| **EnableAiDieInAnomaly** | true/false | AI death in anomalies |
| **EnableNPCLookAtActor** | true/false | NPCs look at the player |
| **EnableAutoreload** | true/false | Automatic weapon reload |
| **EnableMonstersInventory** | true/false | Inventory for monsters |

#### Weapon System

| Parameter | Values | Description |
|----------|----------|----------|
| **EnableWeaponInertion** | true/false | Weapon inertia (Gunslinger style) |
| **EnableWeaponCollision** | true/false | Weapon collision (Shoker style) |
| **EnableImproveWeaponMisfire** | true/false | Improved misfire system (Gunslinger style) |
| **EnableDelayedWeaponActions** | true/false | Delayed weapon actions (Gunslinger style) |

#### Damage and Accuracy

Enabled in `gamedata_soc` and `gamedata_cs`, disabled in Call of Pripyat. Vanilla config values are not changed: each option reads only its own keys, so `false` restores the original balance.

| Parameter | Values | Description |
|----------|----------|----------|
| **EnableImprovedBulletDamage** | true/false | Stalker bone armor stops a share of a bullet instead of being subtracted from it: damage = power × max(1 − armor × (1 − ap), `hit_fraction`). Armor piercing (`k_ap`) lowers the armor the same way it lowers the SoC actor outfit armor. If a bone table `[X]` has an `[X_improved]` section, that one is used (`stalker_damage_improved`: chest 1.0 in SoC / 0.8 in CS, head 6 / 4). Finishing off wounded stalkers and actor armor are unchanged |
| **EnableImprovedWeaponDispersion** | true/false | Actor weapons use `fire_dispersion_base_improved` (as a factor of `fire_dispersion_base`, so dispersion upgrades keep their share), the actor uses `disp_aim_improved`. Values are chosen so that 90% of bullets land in a 4 cm circle at 30–55 m when aiming standing (sniper rifles up to 100 m). Stalkers keep the original spread |
| **EnableDifficultyHitProbability** | true/false | Weapon `hit_probability_<difficulty>` of stalkers follows the game difficulty. The original code always used the novice value. In SoC: beyond `hit_probability_max_dist`, 20/30/40/50% of bullets hit the actor (Novice … Master) instead of 20% at any difficulty. CS weapons have the same value for every difficulty, so nothing changes there |

#### Additional Features

| Parameter | Values | Description |
|----------|----------|----------|
| **EnableActorStepWallmarks** | true/false | Footprints on surfaces (AIW style) |
| **EnableAlternateZoomFovCalc** | true/false | Alternate FOV calculation on zoom (Gunslinger style) |
| **EnableInventoryPistolSlot** | true/false | Separate slot for pistols (like CS/SoC) |

#### Dialogs and Movement

| Parameter | Values | Description |
|----------|----------|----------|
| **DialogFovScale** | number | FOV increase strength during dialogs |
| **TalkDof** | numbers | Background blur degree in dialog |
| **EnableBlockSprintInReload** | true/false | Block sprint during reload |
| **DisableSprintWhileOverweight** | true/false | Disable sprint when overweight |
| **SprintFovFactor** | number | FOV increase strength while running |

### [render]

**Visual effects and rendering**

| Parameter | Values | Description |
|----------|----------|----------|
| **DisableLoadScreenTips** | true/false | Disable loading screen tips |
| **UseDynamicSnowMask** | true/false | Use procedural snow |
| [FontAtlasSize](/interface/fonts#sinographic-writing-system-family) | 4096 | Font texture atlas size (power of two) |

### [environment]

**Environment**

| Parameter | Values | Description |
|----------|----------|----------|
| **ReadSunConfig** | true/false | Regulate sun position via configs|

## Advanced Sections

### [player_hud]

**Player interface**

| Parameter | Description |
|----------|----------|
| **PlayerHudOmfAdditional** | Paths to OMF animation files for hand on HUD, comma-separated listing is supported |

### [step_wallmark]

**Footprints** (requires `EnableActorStepWallmarks`)

| Parameter | Description |
|----------|----------|
| **materials** | Materials on which footprints are displayed |
| **left_mark** | Section for left foot mark |
| **right_mark** | Section for right foot mark |

## Recommendations

- ⚠ **Warnings**:  
  The `[step_wallmark]` section requires activation of `EnableActorStepWallmarks` <br>
  `EnableAlternateZoomFovCalc` breaks the original weapon system  <br>
  Weapon settings from different mods may conflict  <br>
  Changing `Platform` affects mod compatibility <br>

- ✖ **Not Recommended**:  
  Changing `FontAtlasSize` without necessity (only for special fonts)  <br>
  Using commented options without understanding their purpose <br>
