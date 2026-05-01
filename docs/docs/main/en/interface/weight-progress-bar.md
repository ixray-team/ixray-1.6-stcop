> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum version**: 1.4

# Weight progress bar

## Overview

In `actor_menu.xml`, you can enable the weight row using the `actor_weight_row` container. Inside the row, there are two possible modes: a weight progress bar or a text weight value.

## Full actor_weight_row logic

1. The engine first searches for the `actor_weight_row` node.
2. If the node is found:
   1. it creates the weight row container
   2. it always creates `actor_weight_row:actor_weight_caption`
   3. it then checks `actor_weight_row:weight_status_bar`
3. If `weight_status_bar` is found:
   1. it creates `CUIProgressBar`
   2. the progress bar is enabled and shown
   3. the text `actor_weight_row:actor_weight` is not created
4. If `weight_status_bar` is not found:
   1. it creates `actor_weight_row:actor_weight` as a fallback
5. In both cases, `actor_weight_row:actor_weight_max` is always created.
6. If `actor_weight_row` is missing entirely, the engine falls back to the old layout and searches for root nodes `actor_weight_caption`, `actor_weight`, `actor_weight_max`.

## Tooltip logic for weight_status_bar

1. The tooltip works only if `weight_status_bar` exists.
2. The tooltip appears when the cursor hovers over the bar.
3. There is a 700 ms delay before display after focus.
4. If another hint is already open, the new one is not shown.
5. The tooltip text contains:
   1. current total inventory weight
   2. maximum carry weight
6. Tooltip position is chosen near the cursor within the screen bounds.

## Ready XML

```xml
<actor_weight_row x="810" y="736" width="210" height="16">
  <actor_weight_caption x="0" y="0" width="71" height="16">
    <text font="ui_font_arial_14" align="l" color="ui_1">ui_inv_weight</text>
  </actor_weight_caption>

  <weight_status_bar x="73" y="4" width="90" height="8" horz="1" min="0" max="100" pos="0">
    <background><texture>ui_inGame2_hint_wnd_bar_16</texture></background>
    <progress><texture a="180">ui_inGame2_hint_wnd_bar_alfa_line_16</texture></progress>
    <color_less color="pda_green"/>
    <color_more color="pda_red"/>
  </weight_status_bar>

  <actor_weight_max x="166" y="0" width="44" height="16">
    <text font="ui_font_arial_14" align="r" color="ui_1"/>
  </actor_weight_max>
</actor_weight_row>
```

## XML fallback without progress bar

```xml
<actor_weight_row x="810" y="736" width="210" height="16">
  <actor_weight_caption x="0" y="0" width="71" height="16">
    <text font="ui_font_arial_14" align="l" color="ui_1">ui_inv_weight</text>
  </actor_weight_caption>

  <actor_weight x="73" y="0" width="44" height="16">
    <text font="ui_font_arial_14" align="r" color="ui_1"/>
  </actor_weight>

  <actor_weight_max x="121" y="0" width="44" height="16">
    <text font="ui_font_arial_14" align="r" color="ui_1"/>
  </actor_weight_max>
</actor_weight_row>
```

## Usage

1. Add the block to `configs/ui/actor_menu.xml`.
2. For the progress bar mode, add `weight_status_bar` and check `min` and `max` ranges.
3. For text mode, do not add `weight_status_bar`; leave `actor_weight`.
4. Verify:
   1. normal display
   2. overload display
   3. tooltip on hover over the progress bar

Related material: [UI overview](ui-advanced-features.md).
