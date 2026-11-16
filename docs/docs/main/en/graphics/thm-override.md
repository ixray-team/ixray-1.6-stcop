# THM override

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

THM override defines shader macros for an individual base texture. Bump, detail, and material metadata come from the THM. The engine loads macros from an LTX with the same base name. This lets you apply an effect to one material.

## Syntax

For `gamedata/textures/prop/window.dds` and `gamedata/textures/prop/window.thm`, create `gamedata/textures/prop/window.ltx`.

```ini
[shaders_options]
USE_PARRALAX_INTERIOR = 1
```

The loader looks for THM and LTX pairs in the same directory under `$game_textures$` or `$level$`. It supports Image, Terrain, and Normal Map THMs. Macros are passed to shader passes compiled through `uber_deffer` with this texture as their base texture. A local value replaces an existing value for the same macro in that pass.

## Examples

### Window interiors

Set `USE_PARRALAX_INTERIOR = 1` for a window texture, run R4, and load a scene where the material uses the high-quality `uber_deffer` variant. The presence of the macro adds an interior pass. For comparison, capture the same view with the entry present and removed, restarting the game between runs.

### Parallax through textures.ltx

The legacy metadata format separately accepts `parallax[yes]` in `[specification]` in the shared `gamedata/textures/textures.ltx`.

```ini
[specification]
prop\wall = bump_mode[use:prop\wall_bump], material[1.0], parallax[yes]
```

The engine loads `textures.ltx` before the THMs. A supported THM for the same texture replaces the specification from `textures.ltx`.

## Recommendations and limitations

- Entries without a value are skipped and logged. A local value of `0` still defines the macro. Both `#ifdef` and presence checks see it. Remove or comment out the local entry to omit the definition.
- Set global startup options such as `DISABLE_MOTION_VECTORS` in `engine_external.ltx`.
- A texture listed only in `[association]` can retain detail-texture settings. Local shader macros require a supported THM and the matching LTX.
- Use the exact spelling `USE_PARRALAX_INTERIOR` from the source.

## See also

- [Global shader options](shader-options.md)
- [PBR and texture materials](pbr.md)
- [Engine configuration](../main/engine-extension-configuration.md)
