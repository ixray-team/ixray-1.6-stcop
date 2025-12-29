# Engine Extension Configuration

## Overview

The configuration file allows you to configure various aspects of the game and engine through a system of sections and parameters. The file uses INI format with grouping of settings by functional categories

## Main Sections

### [general]

**Basic engine settings**

| Parameter | Values | Description |
|----------|----------|----------|
| **Platform** | `cop`, `cs` | Platform mode. cop - Call of Pripyat, cs - Clear Sky |
| **title** | text | Name of the modification |
| **SaveImageSize** | 128,128 | Size of save screenshots |

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
