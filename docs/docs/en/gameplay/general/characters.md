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

## Improved Behavior Schemes 

- Improved indoor combat behavior scheme: NPCs will no longer look at the floor or ceiling if the enemy is on another floor
- Improved NPC enemy detection scheme, fixed the issue of premature enemy detection
- Added a vehicle bypass scheme for NPCs
- Added the ability to enable NPC reaction to anomalies (`EnableAiDieInAnomaly`)
- Added the ability to enable NPC tracking of the main character within a certain radius (`NPCLookAtActor`)
