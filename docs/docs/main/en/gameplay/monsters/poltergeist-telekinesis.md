# Poltergeist Telekinesis — mod_system_ixray.ltx

Section: `![m_poltergeist_normal_tele]`

---

## Weapon Shooting

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `Tele_Shooting_From_Weapon_Enable` | bool | off | Enables shooting from telekinetically held weapons at enemies. `on`/`off` |
| `Tele_Max_Pickuped_Weapons` | int | 3 | Maximum number of firearms the poltergeist can hold simultaneously |

## Grenades

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `Tele_Activate_N_Throw_Grenade` | bool | on | Enables pulling grenade pins and throwing them at enemies. `on`/`off` |

## Object Holding

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `Tele_Time_Object_Keep` | int (ms) | 10000 | Time an object is held in the air before being thrown |
| `Tele_Object_Min_Mass` | float | 0 | Minimum mass of an object eligible for telekinesis pickup |
| `Tele_Object_Max_Mass` | float | 10000 | Maximum mass of an object eligible for telekinesis pickup |
| `Particle_Tele_Object` | string | `static\fire_distort` | Particle effect played under the held object (visual only) |

## Thrown Object Damage

Damage formula:
```
final_damage = hit_factor - (strike_protection * outfit_condition)
```
If the result is `< 0`, damage = 0 (object does not penetrate armor).

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `Novice_Difficulty_Throwed_Object_Hit_Factor` | float | 0.25 | Damage multiplier on Novice difficulty. Value = % of MaxHP (0.25 = 25% HP per hit) |
| `Stalker_Difficulty_Throwed_Object_Hit_Factor` | float | 0.35 | Damage multiplier on Stalker difficulty (0.35 = 35% HP) |
| `Veteran_Difficulty_Throwed_Object_Hit_Factor` | float | 0.45 | Damage multiplier on Veteran difficulty (0.45 = 45% HP) |
| `Master_Difficulty_Throwed_Object_Hit_Factor` | float | 0.55 | Damage multiplier on Master difficulty (0.55 = 55% HP) |

> Damage is also affected by the outfit's `strike_immunity` and belt artifacts with `strike` protection.
> Valid range: `[0.0 .. 1.0]`. Setting `0.0` disables thrown object damage entirely.

**Damage calculation examples:**

| factor | Outfit | strike_protection | Final damage |
|--------|--------|-------------------|--------------|
| 0.25 | Novice | 0.1 | 0.25 - 0.1 = **0.15** (15% HP) |
| 0.35 | Stalker | 0.15 | 0.35 - 0.15 = **0.20** (20% HP) |
| 0.45 | Exo | 0.4 | 0.45 - 0.4 = **0.05** (5% HP) |
| 0.25 | Exo | 0.4 | 0.25 - 0.4 = **-0.15** → **0** (not penetrated) |

---

## Weapon Auto-Aim

### Angular Speed (deg/s)

The weapon rotates toward the target at a limited speed rather than instantly.

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `Novice_Difficulty_Angular_Speed` | float | 180.0 | Weapon rotation speed toward target on Novice (deg/s) |
| `Stalker_Difficulty_Angular_Speed` | float | 200.0 | Weapon rotation speed toward target on Stalker (deg/s) |
| `Veteran_Difficulty_Angular_Speed` | float | 240.0 | Weapon rotation speed toward target on Veteran (deg/s) |
| `Master_Difficulty_Angular_Speed` | float | 280.0 | Weapon rotation speed toward target on Master (deg/s) |

> Valid range: `(0 .. 360]`. Value `360` = instant aim (turret mode).

### Aim Error Angle (deg)

The angle of deviation of the weapon axis from the target direction at which firing is permitted.

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `Novice_Difficulty_Error_Angle` | float | 30.0 | Aim error tolerance on Novice (deg) |
| `Stalker_Difficulty_Error_Angle` | float | 20.0 | Aim error tolerance on Stalker (deg) |
| `Veteran_Difficulty_Error_Angle` | float | 15.0 | Aim error tolerance on Veteran (deg) |
| `Master_Difficulty_Error_Angle` | float | 8 | Aim error tolerance on Master (deg) |

> Valid range: `(0 .. 180]`.
> - `1°` — weapon fires only when perfectly aimed at the center of the enemy model.
> - `180°` — weapon fires even when facing the opposite direction.
> - Lower difficulties combine larger error angles with slower angular speed, resulting in wasted ammo.
