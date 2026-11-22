> [!IMPORTANT]
> **Status**: In development

# PDA map legend, spot filters and hotkeys

## Overview

The PDA map can hide spots by category, manage personal marks from the keyboard and show a hint bar with the current key bindings. Everything is driven by addon data: categories in `ltx`, the legend and the hint bar in `pda_tasks*.xml`, default binds in `mod_default_controls_*.ltx`. Without these nodes the PDA works as in the original game.

## Spot categories

A category is a group of spots that can be hidden as a whole. A hidden spot disappears from the PDA map, the minimap and the compass. The state is stored in the save.

Categories are defined in the `[map_spot_categories]` section of the system config, for example in `configs/mod_system_my_addon.ltx`:

```ini
[map_spot_categories]
main_tasks     = storyline_task*, ui_storyline_task_blink
stashes        = treasure*
personal_spots = $user
traders        = ui_pda2_trader_location
```

1. The key is the category id.
2. The value is a comma-separated list of spot types.
3. A trailing `*` is a prefix: `treasure*` matches `treasure`, `treasure_rare` and so on.
4. `$user` means the player's personal marks.

A spot is hidden if at least one of its categories is hidden.

The filter buttons at the bottom of the map (`filter_treasures`, `filter_primary_objects`, `filter_quest_npcs`, `filter_personal_spots`, `filter_secondary_tasks`) toggle the `pda_filter_treasures`, `pda_filter_primary_objects`, `pda_filter_quest_npcs`, `pda_filter_personal_spots`, `pda_filter_secondary_tasks` categories. The state lives in the category itself, so a button, a legend item with the same `category` and Lua always agree.

The filter buttons and the `btn_nav_parent` panel (legend, personal mark, center, zoom reset and task buttons) are optional and can be removed from `pda_tasks*.xml` completely:

1. Without a filter button its category stays as it is: shown until the legend or Lua hides it, and saved with the game.
2. Without `btn_nav_parent` the same actions are available from the keyboard: `pda_tasks_toggle_legend`, `pda_tasks_personal_spot_cursor`, `pda_tasks_map_show_me`, `pda_tasks_map_show_target`, the mouse wheel for zoom.

## Map legend

The `map_legend_wnd` node in `pda_tasks*.xml`. Clicking an item hides or shows its category, clicking again restores it.

```xml
<map_legend_wnd x="780" y="150" width="150" height="470">
	<btn_toggle_all x="10" y="434" width="130" height="30" frame_mode="1" text_show="pda_legend_show_all" text_hide="pda_legend_hide_all">
		<texture>ui_inGame2_pda_button_new_task</texture>
		<text align="c" vert_align="c" font="font_rubik_16">pda_legend_hide_all</text>
	</btn_toggle_all>
	<legend_list x="3" y="40" width="200" height="386" item_height="36" hidden_alpha="70">
		<item x="0" y="0" width="200" height="36" category="stashes">
			...
		</item>
	</legend_list>
</map_legend_wnd>
```

| Attribute | Node | Meaning |
| --- | --- | --- |
| `category` | `item` | category id from `[map_spot_categories]` |
| `spots`, `user_spots` | `item` | old way: patterns right in the XML, the category is created automatically |
| `hidden_alpha` | `legend_list` | alpha of a hidden item, 0..255, default 80 |
| `text_show`, `text_hide` | `btn_toggle_all` | "Show all" and "Hide all" button texts |

An item without `category` and `spots` is not clickable, as in the original game.

## Hint bar

> The shared hotkey system, chip styles and the `hotkey` / `hotkey_bar` nodes are described in a [separate page](hotkeys.md). Below is the bar format itself.

The `hotkeys_hint` node in `pda_tasks*.xml`. The engine builds the line from the current bindings, skips actions without a key and drops items from the end if the line does not fit. In gamepad mode the bar is hidden when `gamepad_legend` exists.

```xml
<hotkeys_hint x="49" y="708" width="790" height="35" separator=" | " key_format="[%s]">
	<text align="l" vert_align="c" font="font_rubik_14"/>
	<item action="pda_tasks_toggle_list">pda_hint_tasks</item>
	<item action="pda_tasks_personal_spot_prev,pda_tasks_personal_spot_next">pda_hint_spot_browse</item>
</hotkeys_hint>
```

1. `action` is one action or several separated by commas.
2. The item text is the id of the label string.
3. `separator` (between items), `key_format` (key format, for example `[%s]`) and `label_separator` (between the key and the label, for example ` - `) are optional.
4. The item `key` attribute sets fixed key text instead of the binding, for example `<item action="quit" key="Esc">`.
5. There can be several `hotkeys_hint` nodes, for example the left and right parts of the bottom bar. The same works for several rows in the marks panel.
6. `hotkeys_hint` also works in `pda.xml`, for example key caps for tab switching: `<item action="ui_tab_left"/>` and `<item action="ui_tab_right"/>`. The item label can be omitted.
7. The `btn_second_task` node is optional: without it the task list opens only by hotkey.

### Key caps

When `hotkeys_hint` contains a `key` node, the bar is drawn as a row of caps instead of a text line: every key in its own frame, the label to the right. Without `key` the bar works as before.

```xml
<hotkeys_hint x="49" y="708" width="700" height="35" key_padding="9" key_spacing="4" label_spacing="9" item_spacing="22" chips_align="l">
	<key_back type="line" x="0" y="0" width="26" height="26">
		<texture>ui_inGame2_pda_new_tab_btn_e</texture>
	</key_back>
	<key x="0" y="0" width="26" height="26">
		<text align="c" vert_align="c" font="font_rubik_14" r="225" g="200" b="145"/>
	</key>
	<label x="0" y="0" width="10" height="26">
		<text align="l" vert_align="c" font="font_rubik_14" r="190" g="190" b="190"/>
	</label>
	<item action="pda_tasks_map_show_me">pda_hint_show_me</item>
</hotkeys_hint>
```

| Node or attribute | Effect |
| --- | --- |
| `key` | Key text template. Its height sets the cap height; the width fits the text. It can have its own texture with `stretch="1"`. |
| `key_back` | Optional frame under the key. `type`: `static` (stretched texture), `line` (three-part texture `_b`, `_back`, `_e`), `frame` (nine-part frame). A `static` can take `<svg>file.svg</svg>` instead of a texture (with `<texture>` as the fallback) and `stretch="1"`: HD HUD draws the PDA chips from `hotkey_frame.svg` this way. On a wide key the SVG is stretched as a whole, corners included. |
| `label` | Label template. The width fits the text. |
| `key_padding` | Text padding inside the cap. Default 8. |
| `key_min_width` | Minimum cap width. Defaults to the cap height. |
| `key_spacing` | Gap between caps of one item. Default 4. |
| `label_spacing` | Gap between the cap and the label. Default 8. |
| `item_spacing` | Gap between items. Default 20. |
| `chips_align` | Row alignment: `l`, `c`, `r`. Default `l`. |

Every bound key gets its own cap, up to four per item. In this mode `key_format` defaults to `%s`, and `separator` and `label_separator` are not used. Items that do not fit are dropped from the end, as in text mode.

## Task list tabs

Tabs are buttons in `second_task_wnd:task_filter_tabs`. The mode is chosen by the button `id`:

| `id` | Shows |
| --- | --- |
| `all` | All tasks in progress |
| `story` | Story tasks in progress |
| `side` | Side tasks in progress |
| `active` | All tasks in progress |
| `completed` | Completed tasks, newest first |
| `failed` | Failed tasks, newest first |
| `archive` | Completed and failed tasks together, newest first |

```xml
<task_filter_tabs x="0" y="470" width="344" height="24" counters="1">
	<button id="active" x="0" y="0" width="172" height="24">...</button>
	<button id="archive" x="172" y="0" width="172" height="24">...</button>
</task_filter_tabs>
<t_empty x="15" y="230" width="310" height="60" text_active="pda_tasks_empty_active" text_archive="pda_tasks_empty_archive">
	<text align="c" vert_align="c" font="font_rubik_16"/>
</t_empty>
```

1. `counters="1"` appends the task count to the tab name: "Active (5)".
2. `t_empty` is shown instead of an empty list. The `text_<tab id>` attribute (`text_active`, `text_archive` and so on) sets the text for its tab, `text` sets a common one.
3. Finished task colours are the `task_item:completed` and `task_item:failed` nodes, `read` is used by default.
4. The optional `task_item:st_status` shows a status label in the status colour on a finished task. Its texts are the `text_completed` and `text_failed` attributes. The task name is narrowed so it does not overlap the label.
5. A finished task cannot be made active, its focus and map buttons are hidden.
6. List tabs are switched by the PDA tab keys with Alt held (Alt+Q and Alt+E by default) or by the `ui_tab_sec_left` / `ui_tab_sec_right` actions. If the list is closed, it opens.
7. The optional `tab_hint_prev` and `tab_hint_next` nodes show key hints at the edges of the tab bar. Each is a hint bar with one item: `<item action="ui_tab_left" gamepad_action="ui_tab_sec_left"/>`, the key format is `key_format="[Alt + %s]"`, and `gamepad_key_format` for a gamepad.

## Personal marks panel

The `personal_spots_wnd` node in `pda_tasks*.xml`. The panel lists all personal marks: the current level first, then the others. It is opened by the `pda_tasks_personal_spots_list` action.

| Node | Purpose |
| --- | --- |
| `background_frame`, `t_caption`, `btn_close` | Frame, caption, close button |
| `t_counter` | Counter "3 / 5", just "3" without a limit |
| `spot_list` | The list. `distance_format` is the id of a string like `%s · %d m` (level and distance) |
| `t_empty` | Text shown when there are no marks |
| `t_message` | Limit message, `text_limit` is the id of a string with `%u` |
| `t_tip` | Permanent tip |
| `hotkeys_hint` | Hint bar, same as on the map |
| `spot_item` | Row template: `name`, `info`, `btn_track`, `btn_remove`, `highlight` (shown for the tracked mark), colours `normal` and `tracked` |

In a row, a click shows the mark on the map, a double click opens renaming, the buttons toggle tracking and remove the mark.

Only one side panel is open on the map at a time: the task list, the legend or the marks panel.

The mark creation dialog (`pda_spot.xml`) can contain a `t_hint` static. If the name is left empty, the mark gets a default name like "Mark 3 · Zaton".

## Personal mark limit

```ini
[pda_map]
personal_spot_limit = 5
```

0 or a missing line removes the limit. When the limit is reached, no new mark is created: on the map the marks panel opens with a message, in game the quick mark shows a notification. The limit also applies to Lua `AddPersonalSpot`.

## Active target card

The optional `active_task_card` node in `pda_tasks*.xml`. The card over the map shows the current target: the tracked geomark if navigation is on, otherwise the active story task, and if there is none, the active side task. It refreshes twice per second. A click on the card or the `pda_tasks_map_show_target` action shows the target on the map.

| Attribute or node | Purpose |
| --- | --- |
| `text_story`, `text_side`, `text_mark` | Target type labels. With several story or side tasks "· 1 / 3" is appended |
| `text_none` | Text when there is no target. Without it the card hides |
| `distance_format` | "level · distance" format, as in the geomarks panel |
| `hide_with_task_list` | 1 (default): while the task list is open the card hides, or collapses to the header if there is one |
| `header_height` | Header height, the card collapses to it |
| `header_text`, `header_counter` | Header text and active task counter ("ALL TASKS · 5") |
| `button_show`, `button_hide`, `button_format` | List button labels and the format with the key, for example `%s  [%s]` |
| `icon_story`, `icon_side`, `icon_mark` | Icons used when the task has no icon of its own |
| `header_frame`, `header_icon`, `t_header`, `btn_list` | Header. A click on the header or the button shows or hides the task list |
| `body_frame`, `t_icon` | Body background and task icon (`m_icon_texture_name`) |
| `background_frame` or `background` | Background |
| `t_type`, `t_title`, `t_objective`, `t_info` | Type, task title, current objective, level and distance |
| `btn_focus` | "Show on map" button, hidden if the target has no map spot |

## Actions

| Action | Where | What it does |
| --- | --- | --- |
| `ui_tab_left`, `ui_tab_right` | PDA | Previous and next tab; with Alt they switch task list tabs |
| `ui_tab_sec_left`, `ui_tab_sec_right` | PDA | Task list and diary tabs |
| `pda_tasks_toggle_list` | PDA map | Show or hide the task list |
| `pda_tasks_map_show_me` | PDA map | Show the player on the map |
| `pda_tasks_map_show_target` | PDA map | Show the current target on the map |
| `pda_tasks_toggle_legend` | PDA map | Show or hide the legend |
| `pda_tasks_personal_spot_cursor` | PDA map | Mark at the cursor; on an existing mark opens renaming |
| `pda_tasks_personal_spot_actor` | PDA map | Mark at the player position |
| `pda_tasks_personal_spot_remove` | PDA map | Remove the mark under the cursor |
| `pda_tasks_personal_spot_track` | PDA map | Track the mark under the cursor, press again to stop |
| `pda_tasks_personal_spot_next`, `_prev` | PDA map | Next and previous personal mark |
| `pda_tasks_personal_spots_list` | PDA map | Show or hide the personal marks panel |
| `personal_spot_quick` | In game | Quick mark at the player position without a dialog |

The quick mark notification icon is set by `quick_spot_news_icon` in the `[pda_map]` section of `game_global.ltx`.

## Default binds from an addon

After the main `default_controls.ltx`, the `default_controls` command loads every `configs/ixray_settings/mod_default_controls_*.ltx` file in name order. These are plain console commands:

```ini
bind pda_tasks_toggle_legend kL
bind pda_tasks_toggle_list kTAB
```

One key can be bound in different action groups: in game `kL` toggles the torch, in the PDA it opens the legend.

## Lua

`CMapManager` methods (`level.map_manager()`):

| Method | Description |
| --- | --- |
| `AddPersonalSpot(level_name, position, hint)` | Create a personal mark |
| `RemovePersonalSpot(location)` | Remove a personal mark |
| `TogglePersonalSpotNavigation(location)` | Track a mark or stop tracking |
| `GetActiveUserNavigationLocation()` | Current navigation target |
| `GetAdjacentPersonalSpot(id, next)` | Neighbour personal mark by id |
| `RegisterSpotCategory(id, patterns)` | Add a spot category |
| `SetSpotCategoryHidden(id, hidden)` | Hide or show a category |
| `IsSpotCategoryHidden(id)` | Whether a category is hidden |
| `GetPersonalSpotLimit()`, `GetPersonalSpotCount()`, `CanAddPersonalSpot()` | Personal mark limit |

Related: [navigation personal marker](map-personal-navigation-marker.md), [UI overview](ui-advanced-features.md).
