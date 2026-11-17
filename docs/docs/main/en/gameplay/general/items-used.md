# Used Items

## Custom inventory menu actions

In an item section, `default_use_text` supplies the text of the regular use action. Standard consumables have their own `use_text`, with `st_use` as fallback. Medkits, antirad, and bottles use their standard labels when `default_use_text` is absent.

Each extra action takes a label and a Lua function name. Four pairs are supported: `use1_text` and `use1_functor`, `use2_text` and `use2_functor`, `use3_text` and `use3_functor`, `use4_text` and `use4_functor`. The `*_text` value is a localization key. The function receives the item’s `game_object` and returns `true` to run regular item use afterward, or `false` to finish without that step.

```ini
use1_text = st_open
use1_functor = my_item.on_open
```

If `my_item.on_open` handles the entire action itself, return `false`.

## Portions, weight, and the last animation

In a consumable item section, `eat_portions_num` takes precedence over `max_uses`. The fallback for `max_uses` is `1`. The loader then divides the use count by `eat_condition` and stores the integer part. Use a positive `eat_condition`, which defaults to `1`. `remove_after_use` controls removal after the last portion and defaults to `true`. `empty_weight` sets the empty item weight and defaults to `0.0`.

When the item has an `animator_sect`, put `last_use_section` in that animator section. It names the animation section for the final portion of a multi-use item.
