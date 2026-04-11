> [!IMPORTANT]
> **Status**: Supported  
> **Minimum version**: IX-Ray Platform 1.4  

## Overview

> *Original*

![оригинал](https://github.com/user-attachments/assets/603e36a8-f603-4ed5-a495-71c4692cde6a)  

> *Pattern*

![паттерн](https://github.com/user-attachments/assets/2759dd1c-3df8-4ad3-84d4-329361272fa0)  

When a weapon has a recoil pattern defined, the system enables a spring-pendulum style recoil mechanic. Each shot shifts the camera target, and the camera follows it with inertia — producing a realistic kick and recovery.

> [!NOTE]
> The pattern system is activated by adding the suffix `_hipfire_pattern` to the weapon section name. Example: if the weapon section is `wpn_ak74`, use `wpn_ak74_hipfire_pattern` to enable the new pattern recoil for the AK-74.

## Defining a weapon pattern

Pattern entries use `bullet_XXX` keys, where `XXX` is the shot index. Each value is two numbers in the format `X, Y` representing cumulative angles in degrees.

Pattern logic: a positive `X` shifts left, negative `X` shifts right; a positive `Y` shifts up, negative `Y` shifts down.

```ini
[wpn_ak74_hipfire_pattern]
bullet_1 = 0.009, 0.157
bullet_2 = -0.070, 0.136
bullet_3 = -0.080, 0.169
bullet_4 = -0.088, 0.124
bullet_5 = -0.084, 0.180
bullet_6 = 0.121, 0.180
bullet_7 = -0.035, 0.270
bullet_8 = -0.134, 0.267
bullet_9 = -0.192, 0.293
bullet_10 = -0.146, 0.319
; ...
```

> [!WARNING]
> Pattern values are cumulative: each shot starts from the last bullet position.

## Weapon config parameters

Set pattern parameters in the weapon config `wpn_XXX` (where `XXX` is the weapon name).

Parameters:
```ini
pattern_factor = 0.048
pattern_factor_agility = 1
pattern_factor_agility_vel = 2
pattern_factor_agility_accel = 1
pattern_factor_agility_crouch = 0.950
pattern_factor_agility_crouch_no_acc = 0.910
pattern_stiffness = 1000
pattern_damping = 45
pattern_impulse = 90
pattern_loop = 1
pattern_return_speed = 3.500
pattern_return_enable = 1
pattern_random_enable = 1
pattern_random_x = -0.2, 0.2
pattern_random_y = -0.05, 0.2
```

> [!WARNING]
> Zoom (aiming) parameters use the `zoom_` prefix.

```ini
zoom_pattern_factor = 0.044
zoom_pattern_factor_agility = 1
zoom_pattern_factor_agility_vel = 3.700
zoom_pattern_factor_agility_accel = 1
zoom_pattern_factor_agility_crouch = 0.870
zoom_pattern_factor_agility_crouch_no_acc = 0.770
zoom_pattern_stiffness = 950
zoom_pattern_damping = 45
zoom_pattern_impulse = 85
zoom_pattern_loop = 1
zoom_pattern_return_speed = 3
zoom_pattern_return_enable = 1
zoom_pattern_random_enable = 1
zoom_pattern_random_x = -0.2, 0.2
zoom_pattern_random_y = -0.05, 0.2
```
### Parameter descriptions

`pattern_factor` / `zoom_pattern_factor`
- Default: `0.035`, `0.025` (zoom)
- Purpose: base multiplier for pattern strength. Higher values increase the effect of `bullet_` offsets.

`pattern_factor_agility` / `zoom_pattern_factor_agility`
- Default: `1.0`, `1.0` (zoom)
- Purpose: overall agility multiplier applied to weapon recoil.

`pattern_factor_agility_vel` / `zoom_pattern_factor_agility_vel`
- Default: `3.0`, `3.5` (zoom)
- Purpose: multiplier that scales pattern effect with player movement speed.

`pattern_factor_agility_accel` / `zoom_pattern_factor_agility_accel`
- Default: `1.0`, `1.0` (zoom)
- Purpose: additional multiplier applied while sprinting.

`pattern_factor_agility_crouch` / `zoom_pattern_factor_agility_crouch`
- Default: `0.95`, `0.9` (zoom)
- Purpose: multiplier for crouching movement.

`pattern_factor_agility_crouch_no_acc` / `zoom_pattern_factor_agility_crouch_no_acc`
- Default: `0.95`, `0.85` (zoom)
- Purpose: multiplier for full crouch (no sprint) movement.

`pattern_stiffness` / `zoom_pattern_stiffness`
- Default: `800.0`, `800.0` (zoom)
- Purpose: spring stiffness — higher values make the camera snap faster to the target.

`pattern_damping` / `zoom_pattern_damping`
- Default: `40.0`, `40.0` (zoom)
- Purpose: damping of the spring — higher values reduce oscillation.

`pattern_impulse` / `zoom_pattern_impulse`
- Default: `35.0`, `35.0` (zoom)
- Purpose: instantaneous impulse applied on shot — higher = stronger kick.

`pattern_loop` / `zoom_pattern_loop`
- Default: `1`, `1` (zoom)
- Purpose: whether to loop the pattern. `1` loops back to the first bullet after the last.

`pattern_return_speed` / `zoom_pattern_return_speed`
- Default: `5.0`, `5.0` (zoom)
- Purpose: speed of return toward the previous camera position after recoil.

`pattern_return_enable` / `zoom_pattern_return_enable`
- Default: `1`, `1` (zoom)
- Purpose: whether the camera returns to the previous position after recoil ends.

`pattern_random_enable` / `zoom_pattern_random_enable`
- Default: `0`, `0` (zoom)
- Purpose: enables randomization on top of pattern points.

`pattern_random_x` / `zoom_pattern_random_x`
- Format: `min, max`
- Purpose: horizontal random offset range.

`pattern_random_y` / `zoom_pattern_random_y`
- Format: `min, max`
- Purpose: vertical random offset range.

## Recoil settings for attachments

Attachment parameters are set in the scope, silencer and underbarrel grenade sections.

Scope section:
```ini
scope_attached_recoil_factor = 0.93
scope_attached_recoil_reduction = 0.81
```

`scope_attached_recoil_factor`
- Default: `1.0` — multiplies recoil when a scope is attached.

`scope_attached_recoil_reduction`
- Default: `1.0` — additional divisor applied in aiming mode.

Silencer:
```ini
attached_recoil_f = 0.85
```

`attached_recoil_f`
- Default: `1.0` — recoil multiplier when a silencer is attached.

Underbarrel grenade launcher:
```ini
grenade_attached_recoil = 0.95
```

`grenade_attached_recoil`
- Default: `1.0` — recoil multiplier when an underbarrel grenade launcher is attached.

## Global recoil parameters

These go into the `[actor]` section and act as global multipliers influenced by player movement (`agility`). Useful for mod-wide tuning.

```ini
agility_vel_factor = 2
agility_accel_factor = 1.05
agility_crouch_factor = 0.98
agility_crouch_no_acc_factor = 0.96
```

`agility_vel_factor`
- Default: `2.0` — main movement multiplier affecting `pattern_factor_agility` and zoom equivalents.

`agility_accel_factor`
- Default: `1.05` — multiplier when the player is running.

`agility_crouch_factor`
- Default: `0.98` — multiplier for crouch movement.

`agility_crouch_no_acc_factor`
- Default: `0.96` — multiplier for full crouch (no sprint).

## Examples

### Example 1: basic pattern setup

Recoil Pattern Editor demo:

```ini
[wpn_mp5_hipfire_pattern]
bullet_1 = 0.088, 0.263
bullet_2 = -0.044, 0.263
bullet_3 = 0.044, 0.175
bullet_4 = 0.035, 0.175
bullet_5 = -0.044, 0.175
bullet_6 = 0.000, 0.132
bullet_7 = 0.088, 0.132
bullet_8 = -0.175, 0.175
bullet_9 = 0.225, 0.234
bullet_10 = 0.175, -0.175
bullet_11 = 0.009, 0.175
bullet_12 = 0.000, 0.175
bullet_13 = 0.053, 0.132
bullet_14 = 0.219, -0.088
bullet_15 = 0.096, 0.289
bullet_16 = -0.175, 0.263
bullet_17 = -0.013, 0.290
bullet_18 = 0.158, 0.259
bullet_19 = 0.086, 0.259
bullet_20 = 0.175, -0.132
bullet_21 = -0.026, 0.263
bullet_22 = 0.088, 0.219
bullet_23 = 0.161, 0.297
bullet_24 = -0.384, 0.130
bullet_25 = 0.162, 0.256
bullet_26 = 0.270, 0.135
bullet_27 = 0.243, -0.068
bullet_28 = -0.040, 0.216
bullet_29 = -0.068, 0.296
bullet_30 = -0.168, 0.175
```

Weapon parameters for `wpn_mp5`:
```ini
pattern_factor = 0.049
pattern_factor_agility = 1
pattern_factor_agility_vel = 1.500
pattern_factor_agility_accel = 1
pattern_factor_agility_crouch = 0.890
pattern_factor_agility_crouch_no_acc = 0.810
pattern_stiffness = 1500
pattern_damping = 30
pattern_impulse = 60
pattern_loop = 1
pattern_return_speed = 5
pattern_return_enable = 1
pattern_random_enable = 0
zoom_pattern_factor = 0.042
zoom_pattern_factor_agility = 1
zoom_pattern_factor_agility_vel = 3.300
zoom_pattern_factor_agility_accel = 1
zoom_pattern_factor_agility_crouch = 0.850
zoom_pattern_factor_agility_crouch_no_acc = 0.770
zoom_pattern_stiffness = 1500
zoom_pattern_damping = 35
zoom_pattern_impulse = 55
zoom_pattern_loop = 1
zoom_pattern_return_speed = 5
zoom_pattern_return_enable = 1
zoom_pattern_random_enable = 0

![MP5 без глушителя](https://github.com/user-attachments/assets/63b68c84-654a-4295-b414-5d4c8ceca46a)
```

### Example 2: silencer reduces recoil
Add `attached_recoil_f` in the silencer addon section:

```ini
[wpn_addon_silencer]
attached_recoil_f = 0.5
```

* This reduces overall weapon recoil by 50% (applies both when hipfiring and aiming).

![MP5 с глушителем](https://github.com/user-attachments/assets/24682a38-bf36-4fbd-af6e-822b6d2eed8c)

### Example 3: disable movement influence for `wpn_lr300`

![lr300 с учетом движения](https://github.com/user-attachments/assets/efb7e860-b960-442d-817f-0a2343e65ace)
![lr300 зум с учетом движения](https://github.com/user-attachments/assets/bc255f3c-4734-4128-a1ae-9525bd3229bc)

In `[actor]` set `agility_vel_factor` to `1.0` to remove movement influence:

```ini
[actor]
agility_vel_factor = 1.0
```

Also set both `pattern_factor_agility_vel` and `zoom_pattern_factor_agility_vel` to `1.0` in `wpn_lr300`.

Result: movement no longer affects LR-300 recoil.

![lr300 без учета движения](https://github.com/user-attachments/assets/1a467686-22a9-4048-89b9-f89f256588a1)
![lr300 зум без учета движения](https://github.com/user-attachments/assets/3f382433-dcde-4235-ac4f-10d5120cdcf6)

## Recommendations and limitations

- Recoil multipliers can be set below 1.0 to reduce recoil or above 1.0 to increase it.
- Use `pattern_stiffness` / `zoom_pattern_stiffness`, `pattern_damping` / `zoom_pattern_damping`, and `pattern_impulse` / `zoom_pattern_impulse` to tune the physical feel.
- You can inherit patterns for variant weapons instead of duplicating them:

```ini
[wpn_mp5_nimble_hipfire_pattern]:wpn_mp5_hipfire_pattern
```

- Limitations:
- In single-shot mode with very high RPM values the pattern may be applied but immediately cleared due to the shot queue ending, producing no visible effect. To mitigate, ensure at least one `bullet_1` entry and consider enabling `pattern_random_enable` / `zoom_pattern_random_enable` and adjust offsets.
- Final recoil is the product of all multipliers (weapon params, `bullet_` points, attachments, actor values). Avoid setting all multipliers to `0.0` as it may break gameplay balance.

## See also

- [DLTX](https://github.com/ixray-team/ixray-1.6-stcop/wiki/DLTX)
