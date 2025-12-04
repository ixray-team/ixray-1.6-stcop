# Sound Layers

> [!IMPORTANT]  
> **Status**: Supported  
> **Minimal version**: 1.2

## Overview

The **Layered Sound System** lets you build multi-layer sound composites for weapon shots. Instead of one file, combine several tracks that play together with per-layer volume and delay.

## Syntax

### Basic layer syntax

Use numbered layers with this naming scheme:

```ini
snd_N_layer = path_to_sound, volume, delay
```

Where:

- `N` - layer index (starting at 1)
- `path_to_sound` - path to the sound file
- `volume` - optional volume (default 1.0)
- `delay` - optional delay in seconds (default 0.0)

### Layer variations

Each layer can have multiple random variations:

```ini
snd_N_layer  = sound_path1, volume, delay
snd_N_layer1 = sound_path2, volume, delay  
snd_N_layer2 = sound_path3, volume, delay
```

## Supported sound params

### Primary shot sounds

> [!NOTE]
> These parameters support the layer system:

- `snd_shoot` - main shot sound
- `snd_silncer_shot` - silenced shot

### Actor shot variants

> [!NOTE]  
> These parameters also work with Sound Layers:

- `snd_shoot_actor` - actor shot
- `snd_shot_last_actor` - actor last shot
- `snd_silncer_shot_actor` - actor silenced shot
- `snd_silencer_shot_last` - last silenced shot
- `snd_silencer_shot_last_actor` - actor last silenced shot

## Examples

### Example 1: Basic layered shot

```ini
; Weapon section
[wpn_abakan]
snd_shoot = layered_shot_abakan

; Layered sound section
[layered_shot_abakan]
snd_1_layer = weapons\abakan_shot_core, 1.0, 0.0
snd_1_layer1 = weapons\abakan_shot_variant1, 1.0, 0.0
snd_1_layer2 = weapons\abakan_shot_variant2, 1.0, 0.0

snd_2_layer = weapons\mech_action, 0.8, 0.05

snd_3_layer = weapons\shell_echo, 0.6, 0.1
```

### Example 2: Silenced shot

```ini
[wpn_val]
snd_shoot = layered_shot_val
snd_silncer_shot = layered_silenced_shot_val

[layered_shot_val]
snd_1_layer = weapons\val_shot_main
snd_2_layer = weapons\bolt_mechanism, 0.7, 0.08

[layered_silenced_shot_val]
snd_1_layer = weapons\val_silenced_main, 0.9, 0.0
snd_2_layer = weapons\silenced_mech, 0.6, 0.1
snd_3_layer = weapons\bullet_crack, 0.4, 0.15
```

### Example 3: Complex composition with variations

```ini
[layered_shot_shotgun]
; Main shot (3 variants)
snd_1_layer  = weapons\shotgun_blast1, 1.0, 0.0
snd_1_layer1 = weapons\shotgun_blast2, 1.0, 0.0
snd_1_layer2 = weapons\shotgun_blast3, 1.0, 0.0

; Mechanical sounds (2 variants)
snd_2_layer  = weapons\pump_action1, 0.8, 0.3
snd_2_layer1 = weapons\pump_action2, 0.8, 0.3

; Echo and reverb
snd_3_layer = weapons\shotgun_echo, 0.5, 0.5
snd_4_layer = weapons\environment_reverb, 0.3, 0.7
```

## **Guidelines**

### ✔ **Good usage**

- **Layer numbering**: Start with `snd_1_layer`, increment consecutively
- **Variations**: Add variants to avoid repetitiveness
- **Volume balance**: Tune per-layer volume for a natural result
- **Delays**: Use delays to mimic real mechanical timing

### ⚠ **Limitations**

- **Order**: Layers play concurrently; numbering doesn’t change playback order
- **Performance**: Many layers can affect performance
- **Compatibility**: Engine feature must be enabled

### ✖ **Anti-patterns**

- **Skipping numbers**: Don’t skip layer indices (1, 2, 3...)
- **Too many layers**: Avoid overly complex stacks
- **Frequency clashes**: Ensure layers don’t fight each other

## Debugging / troubleshooting

> [!WARNING]
> If sounds don’t play:

1. Check sound file paths
2. Verify parameters are supported
3. Confirm layer name syntax (case-sensitive)
4. Ensure the feature is enabled in engine settings
