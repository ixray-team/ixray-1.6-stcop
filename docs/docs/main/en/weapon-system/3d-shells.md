# 3D Shells

The engine supports two methods for displaying shell casings:
- **Particles** — the default method. Shells are particle effects from `shell_particles`.
- **3D Model** — a physically simulated 3D shell that bounces off surfaces.

To enable 3D shells, add `shell_section` to the weapon section.

---

## Weapon Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `shell_section` | string | — | 3D shell definition section. If set, enables 3D shells instead of particles |
| `shell_section_0` | string | — | 3D shell section for ammo type 0 |
| `shell_section_1` | string | — | 3D shell section for ammo type 1 |
| `shell_section_N` | string | — | 3D shell section for ammo type N |
| `shell_point` | vector3 | `0, 0, 0` | Shell ejection point offset from the weapon in local coordinates (X, Y, Z) |
| `shell_dir` | vector3 | `1, 0, 0` | Shell ejection direction in weapon-local coordinates (X = right, Y = up, Z = forward) |
| `shell_ejection_speed` | float | 20.0 | Shell ejection speed in meters per second |
| `shell_ejection_dispersion_angle` | float | 30.0 | Random spread of the shell ejection direction in degrees (max 90). Higher value — more random. At 0 — always along shell_dir, at 30 — spreads within a 30° cone |

### Search Priority

- If `shell_section` is set — used for **all** ammo types
- If not — searches `shell_section_0`, `shell_section_1`, ... for each ammo type
- If nothing found — uses particles from `shell_particles`

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
