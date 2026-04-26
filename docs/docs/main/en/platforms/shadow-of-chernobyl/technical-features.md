> [!IMPORTANT]
> **Status**: WIP  
> **Minimum version**: 2.0

## Overview

- The platform uses the same `xrGame.dll` used by the Call of Pripyat (CoP) branch, enabling many CoP engine features in Shadow of Chernobyl (SoC) and vice versa.
- Anomaly detection is now defined by sections instead of classes (see `gamedata/configs/misc/radiation_counter.ltx`).
- Using `xrGame.dll` from CoP allows using CoP UI elements in SoC.
- The UI is adapted for 16:9 aspect ratio.
- `eat_portions_num` default values were changed from `-1` to `1` to prevent infinite food.
- For better compatibility with unified addons, the `config` folder was renamed to `configs`.
- Fixed missing animation flags for trimmed animations.
- Added NPC state fixes in `gamedata/scripts/state_mgr_direction.script`.
- The sit/stand toggle is now an option checkbox instead of a separate key, matching CoP behavior.
- Added "Press any key to continue" text to `gamedata/configs/ui/game_tutorials.xml`.
- Inventory now shows item condition similar to CoP.
- Added language switching via the main menu (`gamedata/configs/ui/ui_mm_main.xml`).
- Combined scope handling: the engine first tries to load `scopes.xml` and locate a matching section; if not found, the scope is used as a fullscreen texture as in SoC.

## engine_external.ltx defaults

Some defaults in SoC's `engine_external.ltx` differ from the CoP defaults. Notable differences:

```ini
[general]
Platform = soc ; platform identifier (SoC)
title = st_ixray_title_soc ; Discord Rich Presence title for SoC

[ui]
WeaponIconScale = 0.9 ; ammo icon size in maingame.xml (CoP default 0.8, CHN 0.65, SoC 0.9)
ShowLoadingStages = true ; show loading stages (e.g., "Client: Synchronizing")
PdaRearrangeTabButtons = true ; use legacy PDA tab sizing formula

[gameplay]
EnableInventoryPistolSlot = true ; enable second slot for pistols only
EnableAiDieInAnomaly = true ; allow NPCs to die in anomalies
EnableLegacyUpgradeSystem = true ; use legacy upgrade system (no two upgrades in same category)
EnableEngineArtefactSpawn = true ; read artifact spawn params from anomaly config

[render]
DisableLoadScreenTips = true ; disable tips on the loading screen
UseLegacyParticleLoader = true ; use SoC particle format for particles.xr
+```

## Info portions system

SoC includes an "info portion" system configured via XML and registered in `system.ltx` under the `info_portions` section.

Example:

```xml
    <info_portion id="test_info_2"> <!-- info portion identifier -->
        <task>storyline_eliminate_gunslinger</task> <!-- grant a quest (SoC only) -->
        <dialog>actor_break_dialog</dialog> <!-- grant a dialog -->
        <article>about_enciclopedia</article> <!-- grant an encyclopedia article -->
        <article_disable>test_article</article_disable> <!-- remove an article from the encyclopedia -->
        <disable>test_info_1</disable> <!-- disable a previously granted info portion -->
    </info_portion>
```
