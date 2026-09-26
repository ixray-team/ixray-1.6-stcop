# 3D Shells

The engine supports two methods for displaying shell casings:
- **Particles** — the default method. Shells are particle effects from `shell_particles`.
- **3D Model** — a physically simulated 3D shell that bounces off surfaces.

To enable 3D shells, add `shell_section` to the weapon section.

---

## Weapon Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `shell_section` | string | — | 3D shell definition section. If set — a 3D shell is **always** used instead of particles, for every ammo type |
| `shell_section_0` | string | — | 3D shell section for ammo type with index 0 |
| `shell_section_1` | string | — | 3D shell section for ammo type with index 1 |
| `shell_section_N` | string | — | 3D shell section for ammo type with index N. Indices start at 0 and follow the same order as ammo types in `ammo_class` |
| `shell_point` | vector3 | `0, 0, 0` | Shell ejection point: offset from the weapon origin in its local coordinate system, **in meters** (X, Y, Z). Configured separately for world and HUD (see "Where to Put Parameters") |
| `shell_dir` | vector3 | `1, 0, 0` | Ejection direction in weapon-local axes: X = right, Y = up, Z = forward. Normalized automatically, so a rough direction like `1, 0, 0` is enough. Rotates together with the weapon |
| `shell_ejection_speed` | float | 20.0 | Shell ejection speed along `shell_dir`, in meters per second. The movement speed of the weapon (and its holder) is added to it — a shell ejected by a running character is carried along with them |
| `shell_ejection_dispersion_angle` | float | 30.0 | Random spread of the ejection direction in degrees: `0` — strictly along `shell_dir`, `30` — a 30° cone, `90` — the maximum possible spread. Values above 90 are clamped to 90 |
| `spawn_shell_on_last_shot` | bool | `true` | Whether to eject a 3D shell on the last shot — when no cartridges remain in the magazine and chamber afterwards. When `false`, the last shot only plays a particle from `shell_particles` and no 3D shell appears: used for pump-action weapons and bolt-action rifles, where the casing should "wait" for the bolt to be cycled |
| `shell_time` | float | `0.0` | Delay before the 3D shell is ejected after a shot, **in seconds** (fractional values allowed: `0.15` = 150 ms). At `0` — ejection happens at the moment of the shot. The deferred ejection is cancelled if, before the delay expires, the weapon stops belonging to its holder (e.g. is dropped). Particles ignore `shell_time` |

### Where to Put Parameters

- `shell_section`, `shell_section_N`, `shell_dir`, `shell_ejection_speed`, `shell_ejection_dispersion_angle`, `spawn_shell_on_last_shot`, `shell_time` — read **only from the weapon section** (e.g. `[w_ak74]:wpn_ak74`). The HUD section ignores them
- `shell_point` — set independently in two places: in the weapon section (world: NPCs, third-person view) and in the HUD section (first person view). The value is not carried over between sections — define it in both, otherwise in one of the modes the shell will eject from `0, 0, 0`

### Search Priority

- If `shell_section` is set — used for **all** ammo types
- If not — searches `shell_section_0`, `shell_section_1`, ... by ammo type index
- If the shot has no dedicated section but other `shell_section_N` are set — any of the configured sections is used by default
- If nothing found — uses particles from `shell_particles`

The shell section is chosen by the ammo type of the shot being fired: chamber first, then magazine, then the weapon's current ammo type.

### Ejection Conditions

A 3D shell appears only when all of the following hold:
- the weapon did not misfire;
- at least one `shell_section`/`shell_section_N` section is set.

Particles from `shell_particles` play only within 5 m of the camera, while 3D shells work at any distance.

### Delayed Ejection

- `shell_time` only affects 3D shells (when `shell_section`/`shell_section_N` is set): particles are always emitted at the moment of the shot
- The delay is counted from the moment of the shot, not from the actual bolt movement — tune the value to match the length of the cycling animation
- If, before the delay expires, the weapon stops belonging to its holder, the deferred ejection is cancelled (no shell appears)

---

## Shell Section Definition

Each 3D shell is a separate section:

```ini
[shell_5.45x39]
class     = SHELL_S_
$spawn    = "physics\shell"
$prefetch = 8
visual    = dynamics\weapons\wpn_shells\5_45x39.ogf
```

| Parameter | Description |
|-----------|-------------|
| `class` | Required: `SHELL_S_` |
| `$spawn` | Required: `"physics\shell"` |
| `$prefetch` | Number of prefetched objects in the pool |
| `visual` | Path to the 3D shell model (`.ogf`) |

Sections can inherit from each other:

```ini
[shell_12g_blue]:shell_12g
visual = dynamics\weapons\wpn_shells\12g_blue.ogf
```

---

## Example: Weapon with 3D Shells

```ini
; Weapon section (for NPC and world)
[w_ak74]:wpn_ak74
shell_point = 0.017, 0.054, 0.17
shell_dir = 1.0, 0.0, 0.0
shell_section_0 = shell_5.45x39
shell_section_1 = shell_5.45x39

; HUD section (for first person view)
[w_ak74_hud]:wpn_ak74_hud
shell_point = 0.017, 0.054, 0.17
```

> `shell_point` is configured separately for HUD and World. HUD position is for first person view, world position is for NPCs.

## Example: Delayed Ejection (Bolt/Pump)

For bolt-action rifles and pump-action shotguns: the casing is not ejected at the moment of the shot, but slightly later — visually when the bolt is cycled.

```ini
[w_mp43]:wpn_mp43
shell_section = shell_12g
shell_point = 0.02, 0.05, 0.22
shell_dir = 1.0, 0.15, 0.0
shell_ejection_speed = 12.0
shell_time = 0.4 ; ejection 0.4 s after the shot — timed to the cycling animation
spawn_shell_on_last_shot = false ; the last casing is not ejected by the shot itself
```

## Example: Custom Shell

```ini
[shell_9x19_custom]
class     = SHELL_S_
$spawn    = "physics\shell"
$prefetch = 8
visual    = dynamics\weapons\wpn_shells\9x19.ogf

[w_pm]:wpn_pm
shell_section = shell_9x19_custom
```
