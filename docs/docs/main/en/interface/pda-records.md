> [!IMPORTANT]
> **Status**: In development

# PDA "Records" tab

## Overview

A tab for things the player wants to remember: stash tips, conversations, achievements, notes and any mod categories. Categories on top, a grouped list on the left, a card with a picture, text and buttons on the right.

The engine only draws the records. Lua provides the data: every category is a provider that fills the list when the tab updates. Mod records can be stored with a ready API, and the save format does not change.

The tab is enabled by a button with `id="eptRecords"` in `pda*.xml`. Without that button the window is not created. The layout lives in `pda_records*.xml`.

## What the player sees

1. Categories with a count of new records, switched with A and D.
2. A list with groups, a new-record mark and a crosshair for records with a map target.
3. A card: picture, heading, text lines in different styles, "Show on map", "Track" and an action button when a mod provides one.
4. "Track" puts a personal mark named after the record on the target and turns on navigation to it. An existing mark at that place is reused.
5. Selecting a record clears its "new" mark.

## Controls

| Action | Default | Gamepad | Effect |
| --- | --- | --- | --- |
| `pda_records_category_prev` | A | D-pad left | Previous category |
| `pda_records_category_next` | D | D-pad right | Next category |
| `pda_records_prev` | Up | D-pad up | Previous record |
| `pda_records_next` | Down | D-pad down | Next record |
| `pda_records_show_on_map` | M | X | Show on map |
| `pda_records_track` | F | Y | Track |
| `pda_records_action` | E | A | Mod action |

The actions have their own conflict group, like the task tab. Alt+Q / Alt+E and the triggers also switch categories. Default binds: `configs/ixray_settings/mod_default_controls_*.ltx`; the options group: `mod_ui_keybinding_*.xml`.

## Lua API

### Stored records

The `pda_records` module keeps records in actor data, so a mod needs no save/load code.

```lua
pda_records.add("my_mod.note_01", {
	category = "notes",
	title = "st_my_note_title",
	subtitle = "st_my_note_short",
	icon = "ui_my_note_icon",
	image = "ui\\my_mod\\note_01",
	group = "jupiter",
	group_caption = "st_my_group_jupiter",
	level = "jupiter",
	map_level = "jupiter",
	position = vector():set(100, 0, 200),
	trackable = true,
	lines = {
		{"hint", "st_my_note_hint"},
		{"text", "st_my_note_text"},
	},
	action = "read",
	action_caption = "st_my_note_read",
	callback = "my_mod.on_note_action",
})

pda_records.update("my_mod.note_01", {state = "done", subtitle = "st_my_note_done"})
pda_records.set_new("my_mod.note_01", true)
pda_records.remove("my_mod.note_01")
pda_records.exists("my_mod.note_01")
local record = pda_records.get("my_mod.note_01")
```

| Field | Meaning |
| --- | --- |
| `category` | Category id, as the button id in `category_tabs`. `notes` by default. |
| `title`, `subtitle` | Heading and the short label in the list row. String ids work. |
| `icon`, `image` | Row icon and card picture. |
| `state` | Any state. The title color comes from `record_item:colors:<state>`. |
| `group`, `group_caption` | Group key and its caption. Records of one group go together. |
| `level` | Record level, used to find the loading screen picture. |
| `map_level`, `position`, `trackable` | Map target. With `trackable = true` the "Track" button is available. |
| `lines` | Card lines: `{style, text}` pairs. |
| `action`, `action_caption`, `callback` | Action button. Pressing it calls `callback(id, action)`. |
| `new` | New-record mark, `true` by default. |
| `time` | Time in game minutes since the game start, the current time by default. Newer records go first. |

Stored records are sorted by group, then from newest to oldest.

### Providers

A provider builds records from live game data on every tab update. It stores nothing, so it suits data the game already has: stashes, tasks, statistics.

```lua
function collect(list)
	local entry = list:add("my_mod.item_01")
	entry:set_title("st_my_title")
	entry:set_subtitle("st_my_state")
	entry:set_state("known")
	entry:set_icon("ui_my_icon")
	entry:set_image("ui\\my_mod\\picture")
	entry:set_group("zaton", "st_my_group_zaton")
	entry:set_level("zaton")
	entry:set_new(true)
	entry:set_map("zaton", vector():set(100, 0, 200))
	entry:set_trackable(true)
	entry:add_line("text", "st_my_text")
	entry:set_action("open", "st_my_open")
end

function on_select(id)
end

function on_action(id, action)
end

function on_game_start()
	pda_records.register_provider("my_category", {collect = collect, on_select = on_select, on_action = on_action})
end
```

Records appear in the order the provider adds them. Stored records of the same category follow them. `pda_records_talk_partner()` returns the id of the actor's conversation partner during a dialog, otherwise 65535.

Other entry methods:

| Method | Effect |
| --- | --- |
| `set_icon_rect(x, y, w, h)` | Part of the icon texture, for example a portrait from an atlas. |
| `set_time(minutes)`, `time()` | Record time in game minutes since the game start. It is part of the hash, so a new time redraws the tab. |
| `id()` | Record id. |
| `set_scroll_end(true)` | The card opens scrolled to the end. |
| `set_item_icon(section)` | The item icon from its section (`inv_grid_*`, `icons_texture`). |
| `set_image("none")` | A card without a picture. If the record has an icon, the `details:icon` node is shown instead. |

`list:get(index)` returns the entry at a 0-based index, `list:count()` the number of entries. `pda_records_fill_contacts(list, max_lines)` adds the contact cards (see below); `max_lines` limits the lines per card, 0 means no limit.

## pda_records.xml

| Node | Meaning |
| --- | --- |
| `main_wnd` | Window. `provider`, `on_select`, `on_action` name Lua functions, `pda_records` ones by default. `refresh_ms` is the update rate, 2000 by default. Nested `auto_static`, `auto_frameline` are decoration. |
| `category_tabs` | Category buttons; the button `id` is the category id. `counters="1"` appends the new-record count. Without the node the tab shows one category from the `category` attribute of `main_wnd`. |
| `category_hint_prev`, `category_hint_next`, `hotkeys_hint` | Key hint bars. |
| `record_list` | List. |
| `group_header` | Group caption. Without it there are no groups. With `collapsible="1"` a click collapses the group; `prefix_open` and `prefix_closed` put a mark before the caption. |
| `record_item` | Row: `selection`, `new_mark`, `icon`, `title`, `subtitle`, `btn_focus`, colors `colors:<state>`, `colors:new`, `colors:normal`. |
| `t_empty` | Empty list: `text` and `text_<category>`. |
| `details` | Card. |

Card: `empty`, `image` (with `crop_rect`), `icon` (portrait for records without a picture, `gap` is the space before the body), optional `title` and `subtitle`, `body`, icon attributes `keep_aspect` (keep proportions) and `push_body="0"` (do not move the body), the `auto_heading="0"` attribute of `details` (no heading line), buttons `btn_map`, `btn_track`, `btn_action` with a key cap `hint` inside. `image_level_intro` and `image_default` work as in the task tab. Without a `title` node the heading becomes the first body line through the `line_heading` template and moves with the body.

Body lines use `line_<style>` templates; an unknown style falls back to `line_text`. HD HUD defines `heading`, `text`, `hint`, `info`, `status`, `caption`, `item`, plus `day`, `npc`, `actor` for contacts.

The `record_item:icon_<category>` and `details:icon_<category>` nodes give one category its own position and size for the row icon and the card portrait (`x`, `y`, `width`, `height`). Everything else comes from the `icon` node. HD HUD uses this for `achievements`, whose icons are portrait-shaped.

## Contacts

The `contacts` category is built from the dialog history the game already keeps for the talk window. The save gets no new data except "read" marks.

1. One record per conversation partner: portrait, name, time of the last talk. Today's talks show the time, older ones the day.
2. Groups by the day of the last talk, newest first.
3. Card: portrait, "Last conversation: Day N, HH:MM", then the lines in order with day separators. Partner and player lines have different colors. The card opens on the last line.
4. A player line belongs to the partner who answers next. A pause longer than 5 game minutes starts a new conversation.
5. A partner with new lines since the last view is marked new. On the first run with the mod installed all contacts count as read.
6. A card shows the last 150 lines; the number is `MAX_LINES` in `pda_records_contacts.script`.

## Achievements

The `achievements` category in HD HUD is built from the `achievements` section of `system.ltx`: name, icon, description and reward come from there. The provider checks the achievement conditions every 5 seconds, so achievements are granted without a separate statistics tab.

1. Groups: "In progress", "Earned", "Not started", "Path closed", each with a count. Earned ones go from newest to oldest.
2. Row: icon, name, and on the right the progress like `2/3`, "earned" or "closed".
3. Card: icon, name, state (progress, time earned or "Path closed"), goal and reward. The reward stays hidden until the first step.
4. The time an achievement was earned is stored from this version on. Older ones say "Earned before the journal was installed".
5. A newly earned achievement is marked new until its card is opened.

## Stashes

The `stashes` category in HD HUD is built from the vanilla `treasure_manager`, which stays unchanged:

1. Name: the nearest named place from `smart_names`, for example "Stash: Skadovsk".
2. Tip: distance and direction from that place, for example "Skadovsk: about 120 m to the north-east".
3. Source: the conversation partner at the moment of the tip, or "Coordinates from a PDA", with the time. Tips from older saves show "Coordinates received earlier".
4. State: new, not opened, partial, emptied, empty (someone got there first), outdated (the stash closed again).
5. After opening, the card lists what was inside.
6. Groups by level with "found X of Y known".

Data for a single stash comes from a `pda_records_stash_<stash id>` section:

```ini
[pda_records_stash_zat_hiding_place_7]
place = st_my_place_name
hint  = st_my_stash_hint
image = ui\my_mod\stash_photo_07
```

## Limitations

1. Mod records live in actor data and survive saves, but not new games.
2. "Track" creates a personal mark, so the personal mark limit applies.
3. A provider runs on every update, so it should be fast.
4. Tips given before installation show no source and no found items.
5. Contacts see only what is left in the dialog history: the game limits its size, so old lines drop out over time. System talk messages (item transfers and the like) do not appear in the card.
6. Partners with the same name and portrait merge into one contact.
7. Achievements earned before this version have no known time.
8. HD HUD has no `notes` tab. Records of that category, including `pda_records.add` records without a `category` field, stay hidden until a mod adds a button with `id="notes"` to `category_tabs`.

## Post-build checks

1. Without an `eptRecords` button the PDA looks as before.
2. The "Records" tab opens; A and D switch "Stashes", "Contacts" and "Achievements".
3. A new stash tip appears in its category with a count, source and direction hint.
4. M opens the map on the stash, F places a mark and turns on navigation.
5. After opening a stash the card lists found items and the group count updates.
6. A record added through `pda_records.add` appears in its category and is saved.
7. The actions are listed in the control settings under "PDA: records".
8. After talking to an NPC, "Contacts" shows the portrait with a new mark, the card opens on the last line and player lines are labelled "You".
9. Talking to the same NPC on another day adds a day separator and moves the contact to the top.
10. "Achievements" shows groups with counts; icons are not stretched in the row or the card.
11. A freshly earned achievement is marked new, its card shows the time earned, and opening the card clears the mark.
12. An achievement whose condition is met is granted within a few seconds, even if the PDA was never opened.
