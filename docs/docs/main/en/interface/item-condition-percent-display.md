> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4

# Item condition and percent display

## Overview

`CUIItemStateDisplay` supports two basic modes: a bar and text percent display.

## Modes

1. Bar:
   1. `percent_display` is not set.
   2. Uses `CUIProgressBar`.
2. Text:
   1. `percent_display` is enabled.
   2. Text is configured via `percent_display:text`.

## Output formats

1. `percent` — a number with a percent sign.
2. `number` — a number without a percent sign.
3. `fraction` — a fraction using `fraction_max`.
4. `portion` — a part counter for stackable items.

## Example

```xml
<percent_display>
  <background stretch="1">
    <texture>ui_inv_condition_back</texture>
  </background>
  <text format="percent" align="c" vert_align="c" font="letterica16"/>
</percent_display>
```

## Usage

1. Add the node to the desired UI element.
2. Choose the `format`.
3. If needed, set `min_color`, `middle_color`, `max_color`.

Related material: [item parameters](item-parameters.md), [UI overview](ui-advanced-features.md).
