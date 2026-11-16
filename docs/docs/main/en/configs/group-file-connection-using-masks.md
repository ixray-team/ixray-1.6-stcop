# Including resources by pattern

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: IX-Ray Platform 1.2

## Overview

Wildcard patterns let one entry include several resources. LTX and XML use an `#include` directive. An OGF model can use a wildcard in its reference list to load multiple OMF animation files.

## Syntax

| Format | Example | Base directory |
| --- | --- | --- |
| LTX | `#include "weapons\w_*.ltx"` | Current LTX directory |
| XML | `#include "ui\hud_widgets\*.xml"` | File-system alias used to load the XML |
| OGF | `dynamics\weapons\wpn_hand\*.omf` | `$game_meshes$` and `$level$` |

## Examples

### Weapon configurations

```ini
#include "weapons\w_*.ltx"
```

Use this pattern for the `w_` prefix. The file system compares the pattern with the full path relative to `weapons`.

To include all LTX files inside `weapons` and its subdirectories, use:

```ini
#include "weapons\*.ltx"
```

Keep the base directory literal. In `#include "weapons\*\*.ltx"`, the LTX parser tries to open `weapons\*\` as the base directory.

### HUD widgets

```xml
<w>
  #include "ui\hud_widgets\*.xml"
</w>
```

The XML parser inserts the included contents at the directive. Each file in this example supplies child elements of `w`.

### OMF animations

```text
dynamics\weapons\wpn_hand\*.omf
```

The OGF reference selects OMF files under `wpn_hand`, including its subdirectories. The pattern `dynamics\weapons\*\*.omf` requires a subdirectory under `weapons`. To match files directly under `weapons` as well, use `dynamics\weapons\*.omf`.

## Recommendations and limitations

- Use a pattern containing `*.xml` to include multiple XML files. An exact path includes one XML file.
- List files explicitly when inclusion order affects the result.
- Keep animation sets that match the model skeleton under the same OMF pattern.

## See also

- [DLTX](dltx.md)
- [XMLOverride](xml-override.md)
- [Animation](../animation-system/general-information.md)
