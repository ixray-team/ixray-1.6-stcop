> [!IMPORTANT]
> **Status**: In development

# PDA "Reference" tab

## Overview

The reference builds its articles itself from what the game configs already have: name, description, icon, price and item properties. A hand-written article is needed only where the standard description is not enough.

The tab runs on the same engine as [Records](pda-records.md): sections are groups, an article is a record card. It is enabled by a button with `id="eptReference"` in `pda*.xml`; the layout lives in `pda_reference*.xml`. The vanilla encyclopedia (`eptEncyclopedia`) stays a separate tab and does not change.

## What the player sees

1. Sections on the left: "Artefacts", "Weapons", "Equipment", "Mutants", "Zone". Clicking a section caption collapses or expands it.
2. Sections list only what the player has already met. Items appear once they are in the inventory. A mutant appears after the first kill. "Zone" articles are visible at once or after a given info portion.
3. A new article is highlighted until it is opened.
4. Card: a path like `#/Artefacts/Jellyfish`, the name, the price on the right, the description, the item icon and properties. Bonuses are green, penalties red.

## What comes automatically

| Section | Source | Properties |
| --- | --- | --- |
| Artefacts | class `ARTEFACT`, `SCRPTART` | protection from `hit_absorbation_sect` (scaled by the `actor_condition` limits), restoration, carry weight |
| Weapons | class `WP_*` | caliber from the first `ammo_class` entry, damage, rate of fire, magazine |
| Equipment | class `E_STLK`, `E_HLMET` | `*_protection` values, carry weight, artefact slots |
| Mutants | kills by the player | description and kill count |

For every item: `inv_name`, `description`, the icon from `inv_grid_*`, `cost`, `inv_weight`.

## Custom article

A `pda_reference_<item section>` section extends the automatic article: its text goes after the properties, its picture is shown on top of the card.

```ini
[pda_reference_af_medusa]
text = st_ref_af_medusa_long
image = ui\reference\medusa
```

For mutants the section is `pda_reference_mon_<species>`.

Articles without an item are listed in `[pda_reference_articles]`, each with its own section:

```ini
[pda_reference_articles]
my_article

[pda_reference_my_article]
group = zone
name = st_my_article
text = st_my_article_text
image = ui\my_mod\picture
icon = ui_my_icon
info = my_info_portion
```

`group` is the section, `zone` by default. With `info` the article appears once that info portion is given.

The section order is `groups` in `[pda_reference]`. Mutant icons come from `[pda_reference_mutants]`: `species = texture`.

## pda_reference.xml

Built like `pda_records.xml`, without category tabs: `main_wnd` sets `category="reference"`. What matters for the reference:

| Node | Attribute | Effect |
| --- | --- | --- |
| `group_header` | `collapsible`, `prefix_open`, `prefix_closed` | Collapsible sections with `[-]` / `[+]` marks |
| `record_item:icon`, `details:icon` | `keep_aspect` | The icon fits its frame keeping proportions |
| `details:icon` | `push_body="0"` | The icon does not push the text down and sits on the side |
| `details` | `auto_heading="0"` | No automatic heading: the provider adds the path and heading as `path` and `heading` lines |
| `details:subtitle` | | The price in the top right corner |

Line styles in HD HUD: `path`, `heading`, `text`, `stat`, `caption`, `hint`, `info`.

## Limitations

1. Items seen only at a trader or on the ground do not appear until they are in the inventory.
2. On first opening with an old save the current inventory is added without the "new" mark. Mutants killed earlier are not counted.
3. Weapon properties are simplified: damage is the first `hit_power` value x 100.
4. Collapsed sections are not kept between sessions.

## Post-build checks

1. The "Reference" tab opens; without an `eptReference` button the PDA looks as before.
2. A picked-up artefact appears under "Artefacts" with the new mark; the card shows the path, price, an unstretched icon and colored properties.
3. Clicking a section caption collapses it, clicking again expands it.
4. After killing a mutant it appears under "Mutants" with the kill count.
5. An article with a `pda_reference_<item>` section shows the extra text and picture.
6. "Zone" articles are visible at once.
