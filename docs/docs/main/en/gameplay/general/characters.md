# Characters
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.1

## Random NPC Rank

You need to open the NPC profile and specify the minimum and maximum value for the `rank` tag

```xml
   <rank min="0" max="100" />
```

## Random NPC Reputation

You need to open the NPC profile and specify the minimum and maximum value for the `reputation` tag

```xml
   <reputation min="-100" max="100" />
```

## Random Loot
A random loot system from CoC has been added. For this, you need to specify `spawn_loadout` in the NPC profile.

**Example:**
```ini
[spawn_loadout] \n
wpn_ak74 = 1 \n
wpn_spas12 = 1 \n

[spawn_loadout2] \n
wpn_pb = 1, silencer \n
wpn_pm = 1 \n
wpn_sig220 = 1 \n
```

## Supplies from an LTX section

A `<supplies>` line of the form `@section` is replaced by the lines of that section from `system.ltx`, parents included. The section can be changed with [DLTX](../../configs/dltx.md), so one loadout serves many profiles and platforms.

```ini
; configs/misc/my_loadouts.ltx, included from system.ltx
[my_loadout_stalker_1]
wpn_ak74 = 1, scope_prob=0.3
ammo_5.45x39_fmj = 2
```

```xml
<supplies>
	[spawn] \n
	@my_loadout_stalker_1 \n
	#include "gameplay\character_items.xml"
</supplies>
```

A missing section logs `! Specific character '...': supplies section '...' not found`. Keys in an LTX section are unique: write two of the same item as `medkit = 2`, not as two lines.

## Improved Behavior Schemes 

- Improved indoor combat behavior scheme: NPCs will no longer look at the floor or ceiling if the enemy is on another floor
- Improved NPC enemy detection scheme, fixed the issue of premature enemy detection
- Added a vehicle bypass scheme for NPCs
- Added the ability to enable NPC reaction to anomalies (`EnableAiDieInAnomaly`)
- Added the ability to enable NPC tracking of the main character within a certain radius (`NPCLookAtActor`)
