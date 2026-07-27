# Medication Intoxication

> [!IMPORTANT] Status: Supported Minimum version: IX-Ray 1.X
>
>  Last updated: 2026-07-28

## Overview

Medication intoxication is an optional actor condition system that models body overload from drugs and chemical items.

The intoxication scale ranges from `0.0` (clean) to `1.0` (maximum). Items with the `eat_intoxication` parameter raise or lower the scale on use. The higher the value, the stronger the stamina penalty, the sooner health damage starts, and the weaker medkit healing becomes.

The system is disabled by default. For the player it should read as a clear role split:

- medkits heal and do not have to be an intoxication source;
- drugs and antirad give a fast effect at the cost of stacking intoxication;
- antidote and drinks clear intoxication;
- a medic through the "I need medical help" dialogue fully clears intoxication together with normal treatment.

One-line usage example: enable `EnableMedIntoxication`, set `eat_intoxication` on drugs and `intoxication_heal_min` on medkits via DLTX.

## Enabling

The feature is activated by a flag in `engine_external.ltx` or a DLTX override on top of it:

```ini
![gameplay]
EnableMedIntoxication = true
```

When `false`:

- `actor_condition` intoxication parameters are not used by the active path;
- `ChangeIntoxication` does nothing;
- penalties to healing, stamina, HP, sprint, and limping are not applied;
- intoxication UI is not created and not updated, even if XML nodes are present.



## Gameplay model



### Scale


| Value `I`         | Meaning                                                            |
| ----------------- | ------------------------------------------------------------------ |
| `0.0`             | No intoxication                                                    |
| `0.0 .. critical` | Light load: stamina drain is active, no heal penalty and no HP DoT |
| `> critical`      | Heal efficiency penalty and HP DoT                                 |
| `>= 0.7`          | Heavy overdose: sprint blocked, cam-effector                       |
| `> 0.9`           | Critical overdose: limping, PP-effector, stronger DoT              |


`critical` is configurable (`intoxication_critical`, default `0.35`).
Thresholds `0.7` and `0.9`, as well as DoT multipliers `1.5` / `2.5`, are hardcoded and cannot be changed via LTX.

### How it rises and falls

Intoxication changes as follows:

1. On item use: `I += eat_intoxication`.
2. Every frame: `I += intoxication_v * dt` (usually a negative decay).
3. Optionally with thirst enabled: extra detox from `eat_thirst` (see the thirst section).
4. Value is always clamped to `[0.0 .. 1.0]`.
5. It is saved in the actor save.



### Recommended item roles


| Role                             | Typical setup                                             | Gameplay meaning                                            |
| -------------------------------- | --------------------------------------------------------- | ----------------------------------------------------------- |
| Medkits                          | `eat_intoxication = 0`, different `intoxication_heal_min` | Always heal; cheap ones work worse under heavy intoxication |
| Chemicals / stimulants / antirad | `eat_intoxication > 0`                                    | Fast effect at the cost of stacking                         |
| Antidote                         | `eat_intoxication = -1.0`                                 | Full clear                                                  |
| Drinks                           | `eat_intoxication < 0`                                    | Cheap slow detox                                            |
| Bandages                         | no parameter or `0`                                       | Do not participate in the chemical stack                    |


This split helps the player understand the system quickly: healing with medkits is safe for intoxication, while abusing stimulants is not.

## Effects by system



### Stamina (power)

For any `I > 0`:

```text
delta_power += intoxication_power_v * I * dt
```

`intoxication_power_v` is usually negative. The higher the intoxication, the faster stamina falls relative to this formula, even at rest.

Example with `intoxication_power_v = -0.015`:

- `I = 0.5` -> about `-0.0075` power per second;
- `I = 1.0` -> about `-0.015` power per second.



### Health

Only when `I > critical`:

```text
excess = (I - critical) / (1 - critical)
healthMul = 1.0
if I >= 0.7: healthMul = 1.5
if I > 0.9: healthMul = 2.5
delta_health += intoxication_health_v * excess * healthMul * dt
```

`intoxication_health_v` is usually negative.

### Healing efficiency

The penalty applies only to positive HP healing:

- `eat_health > 0` in `ApplyInfluence`;
- `boost_health_restore > 0` in `ApplyBooster` (important for mods like Gunslinger, where medkits heal through a booster).

Not affected:

- bleeding (`boost_bleeding_restore` / bleeding);
- radiation;
- power / food / thirst and other item parameters.

Formula:

```text
if I <= critical:
    factor = 1.0
else:
    t = saturate((I - critical) / (1 - critical))
    kMin = item intoxication_heal_min (default 0.25, clamp 0..1)
    factor = 1.0 + (kMin - 1.0) * t
```

At maximum intoxication `factor = kMin`.
At `I = critical` the penalty has not started yet.
At `I = 1.0` healing is cut down to the item minimum.

Example with `critical = 0.35` and `I = 1.0`:


| Item                   | `intoxication_heal_min` | Heal efficiency |
| ---------------------- | ----------------------- | --------------- |
| Regular medkit         | `0.30`                  | 30%             |
| Army medkit            | `0.60`                  | 60%             |
| Scientific medkit      | `0.90`                  | 90%             |
| Item without parameter | default `0.25`          | 25%             |




### Movement and effectors


| Condition  | Effect                                                |
| ---------- | ----------------------------------------------------- |
| `I >= 0.7` | Cannot sprint                                         |
| `I >= 0.7` | Cam-effector from section `[effector_intoxication]`   |
| `I > 0.9`  | Limping                                               |
| `I > 0.9`  | PP-effector from section `[effector_intoxication_pp]` |


Effector intensity is tied to the current `GetIntoxication()` value.

### Medic

The standard help dialogue (`dialogs.medic_magic_potion` / `actor_needs_bless`) can account for intoxication:

- the help line is available if `db.actor.intoxication > 0`, even at full HP;
- medic treatment clears intoxication via `db.actor.intoxication = -1` (full clear, same pattern as radiation).

This is done through a script.

## Syntax and parameters



### Global flag

File: `engine_external.ltx` / DLTX

```ini
![gameplay]
EnableMedIntoxication = true
```



### Actor section `![actor_condition]`


| Parameter               | Type         | Engine default | Meaning                                              |
| ----------------------- | ------------ | -------------- | ---------------------------------------------------- |
| `intoxication_critical` | float `0..1` | `0.35`         | Threshold where heal penalty and HP DoT start        |
| `intoxication_v`        | float / sec  | `-0.00025`     | Natural scale change every frame                     |
| `intoxication_power_v`  | float / sec  | `-0.005`       | Stamina drain multiplier: `* I`                      |
| `intoxication_health_v` | float / sec  | `-0.0015`      | HP DoT multiplier: `* excess * healthMul`            |
| `intoxication_thirst_k` | float        | `0.3`          | Extra detox from `eat_thirst` when thirst is enabled |


Example:

```ini
![actor_condition]
intoxication_v          = -0.00025
intoxication_critical   = 0.35
intoxication_health_v   = -0.0012
intoxication_power_v    = -0.015
intoxication_thirst_k   = 0.0
```

> [!NOTE]
> For drink detox independent of thirst, use `intoxication_thirst_k = 0` and a direct `eat_intoxication < 0` on drinks. Then behavior does not depend on `EnableThirst`.



### Item parameters

Read from the used item section:


| Parameter               | Type         | Default | Meaning                              |
| ----------------------- | ------------ | ------- | ------------------------------------ |
| `eat_intoxication`      | float        | `0.0`   | How much to add/remove on use        |
| `intoxication_heal_min` | float `0..1` | `0.25`  | Minimum heal efficiency at `I = 1.0` |


Examples:

```ini
![medkit]
eat_intoxication = 0.0
intoxication_heal_min = 0.30

![drug_booster]
eat_intoxication = 0.15

![drug_antidot]
eat_intoxication = -1.0

![water]
eat_intoxication = -0.10
```



### Link to thirst (optional)

If all of the following are true:

- `EnableMedIntoxication = true`;
- `EnableThirst = true`;
- the item has `eat_thirst > 0`;
- `intoxication_thirst_k > 0`;

then on apply additionally:

```text
eat_intoxication_effective -= eat_thirst * intoxication_thirst_k
```

If a drink already has negative `eat_intoxication` and `intoxication_thirst_k > 0`, detox can stack. Usually you pick one path.

### Effectors

Regular sections without `!` (created once; do not duplicate them across multiple LTX files):

```ini
[effector_intoxication]
cam_eff_name   = camera_effects\drunk.anm
cam_eff_cyclic = 1

[effector_intoxication_pp]
pp_eff_name     = alcohol.ppe
pp_eff_cyclic   = 1
pp_eff_overlap  = true
```

> [!WARNING]
> Duplicating a regular `[effector_intoxication]` section in base `gamedata` and an addon causes a fatal error on LTX load. Keep effectors in one place.



## UI and strings



### HUD

Node `indicator_intoxication` in maingame XML (via XML Override). Display thresholds:


| `I`       | Icon behavior        |
| --------- | -------------------- |
| `<= 0.05` | Hidden               |
| `< 0.35`  | Green, slow blink    |
| `< 0.7`   | Yellow, medium blink |
| `>= 0.7`  | Red, fast blink      |




### Actor inventory

Node `intoxication_state` in actor menu XML. Three icon levels using the same logical thresholds.

### Item tooltip

Node `boost_intoxication` in `booster_params` XML.
The tooltip **does not show exact numbers** for `eat_intoxication`. It shows a qualitative description:


| Value condition | String                            |
| --------------- | --------------------------------- |
| `<= -0.9`       | `ui_inv_intoxication_clear`       |
| `< 0`           | `ui_inv_intoxication_reduce`      |
| `<= 0.12`       | `ui_inv_intoxication_raise_light` |
| `<= 0.20`       | `ui_inv_intoxication_raise`       |
| `> 0.20`        | `ui_inv_intoxication_raise_heavy` |


Base strings are also required:

- `ui_inv_intoxication`
- `st_ui_intoxication_sensor`



## Lua API

Actor property:

```lua
local i = db.actor.intoxication      -- get, 0..1
db.actor.intoxication = -1           -- ChangeIntoxication(-1), full clear
db.actor.intoxication = 0.2          -- add 0.2
```

The property setter works like `Change*`, not as an absolute set. For a full clear use `-1`.

Also exported on `CEntityCondition`:

- `ChangeIntoxication(value)`
- `GetIntoxication()`

UI expression:

- `fltPlayerIntoxication`

Medic wrapper example:

```lua
function on_game_start()
	local original_potion = dialogs.medic_magic_potion
	local original_needs = dialogs.actor_needs_bless

	dialogs.medic_magic_potion = function(first_speaker, second_speaker)
		original_potion(first_speaker, second_speaker)
		if db.actor then
			db.actor.intoxication = -1
		end
	end

	dialogs.actor_needs_bless = function(first_speaker, second_speaker)
		if original_needs(first_speaker, second_speaker) then
			return true
		end
		return db.actor ~= nil and db.actor.intoxication > 0
	end

	dialogs.actor_is_damn_healthy = function(first_speaker, second_speaker)
		return not dialogs.actor_needs_bless(first_speaker, second_speaker)
	end
end
```



## Usage examples



### Scenario 1: Minimal enable

```ini
; mod_engine_external_my_intox.ltx
![gameplay]
EnableMedIntoxication = true

; mod_system_my_intox.ltx
![actor_condition]
intoxication_v = -0.00025
intoxication_critical = 0.35
intoxication_health_v = -0.0012
intoxication_power_v = -0.015
intoxication_thirst_k = 0.0

![drug_booster]
eat_intoxication = 0.15

![drug_antidot]
eat_intoxication = -1.0

![medkit]
eat_intoxication = 0.0
intoxication_heal_min = 0.30
```

Result: chemicals stack intoxication, the medkit does not poison, under heavy intoxication it heals weakly, antidote clears fully.

### Scenario 2: Medkit tiers

```ini
![medkit]
eat_intoxication = 0.0
intoxication_heal_min = 0.30

![medkit_army]
eat_intoxication = 0.0
intoxication_heal_min = 0.60

![medkit_scientic]
eat_intoxication = 0.0
intoxication_heal_min = 0.90
```

Result: at max intox the scientific medkit almost keeps its effect, while the regular one becomes weak. This justifies tier pricing.

### Scenario 3: Drinks as detox without thirst

```ini
![water]
eat_intoxication = -0.10

![energy_drink]
eat_intoxication = -0.04

![actor_condition]
intoxication_thirst_k = 0.0
```

Result: water always reduces intoxication, even if `EnableThirst = false`.

### Scenario 4: Hard mode

```ini
![actor_condition]
intoxication_critical = 0.25
intoxication_v = -0.00010
intoxication_power_v = -0.025
intoxication_health_v = -0.0020

![antirad]
eat_intoxication = 0.20

![drug_anabiotic]
eat_intoxication = 0.40
```

Result: you hit penalties faster, recover naturally slower, and stamina/HP pressure harder.

## Recommendations

Correct usage:

- Balance only through DLTX and XML Override. Do not edit legacy `gamedata` files.
- Give medkits `eat_intoxication = 0` and set tiers via `intoxication_heal_min`.
- Give chemicals a positive `eat_intoxication`.
- Give antidote `-1.0` if you want a full emergency clear.
- Give drinks a direct negative `eat_intoxication` if detox is part of this system.
- Keep effectors in a single addon LTX file.

Limitations:

- Thresholds `0.7` / `0.9` and DoT multipliers `1.5` / `2.5` are hardcoded in C++.
- The heal penalty does not affect bleeding/radiation restore.
- `db.actor.intoxication = X` applies a delta, not an absolute current value.
- Without UI Override and strings the player will not see indicators and tooltip descriptions, even though logic already works.
- CoP is the target branch. CS/SOC are not a priority for this feature.

Anti-patterns:

- Giving all medkits a high `eat_intoxication`. The player stops understanding what they are being punished for.
- Enabling thirst detox and a strong negative `eat_intoxication` on the same drinks without recalculating.
- Duplicating `[effector_intoxication]` across multiple files.
- Showing raw intoxication percentages in the item tooltip. Use qualitative strings.
- Editing vanilla `dialogs.script` in `gamedata`. Wrap the functions from an addon script.



## What the system intentionally does not do

- Does not change shooting accuracy with a separate multiplier.
- Is not a replacement for alcohol. Alcohol remains a separate scale.
- Does not require a `medic` community. Existing medical help dialogues are used.
- Does not clear intoxication just because HP was restored. You need antidote, a drink, natural decay, or a medic.



## Related sections

- Flag: `gamedata/configs/engine_external.ltx` -> `EnableMedIntoxication`
- Actor condition: `src/xrGame/ActorCondition.cpp` (`UpdateIntoxication`, `GetMedicineEfficiencyFactor`, `ApplyInfluence`, `ApplyBooster`)
- `eat_intoxication` read path: `src/xrGame/EntityCondition.cpp`
- Tooltip UI: `src/xrGame/ui/UIBoosterInfo.cpp`
- HUD: `src/xrGame/ui/UIMainIngameWnd.cpp`
- Inventory: `src/xrGame/ui/UIActorStateInfo.cpp`
- Lua property: `db.actor.intoxication`
- Reference addon balance and UI: `RenewedPerceptionGunslingerMod` (`mod_system_med_intoxication.ltx`, XML Override, `ixr_med_intoxication.script`)

