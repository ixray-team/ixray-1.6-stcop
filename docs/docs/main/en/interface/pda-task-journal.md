> [!IMPORTANT]
> **Status**: In development

# PDA task tab

## Overview

A separate tab with the task journal: a filtered list on the left and the selected task card with a picture on the right. The task window on the map (`second_task_wnd`) stays as it was.

The tab is enabled by a button with `id="eptTaskJournal"` in `pda*.xml`. Without that button the window is not created and the PDA works as before. The layout lives in `pda_task_journal*.xml`, and almost every node is optional.

## What the player sees

1. "In progress", "Completed" and "Cancelled" filters with key hints on both sides.
2. The list is split into "Story" and "Side" groups. Inside a group, current tasks show the tracked task first, then the rest by priority. Finished lists show the latest first.
3. A click selects a task. A double click, Enter or the crosshair icon in the row shows it on the map.
4. The card shows a picture, a heading, receive or finish time, the target level and stages with their state.
5. "Show on map" and "Track" buttons with key caps.
6. The list refreshes by itself when tasks change.

A CoP task has three states: in progress, completed, failed. A script "cancel" (`reversed`) is also a failure in the engine, so the `failed` filter shows both.

## Controls

| Action | Default | Gamepad | Effect |
| --- | --- | --- | --- |
| `pda_task_journal_filter_prev` | A | D-pad left | Previous filter |
| `pda_task_journal_filter_next` | D | D-pad right | Next filter |
| `pda_task_journal_prev` | Up | D-pad up | Previous task |
| `pda_task_journal_next` | Down | D-pad down | Next task |
| `pda_task_journal_show_on_map` | M | X | Show on map |
| `pda_task_journal_track` | F | Y | Track |

The actions have their own conflict group, so A and D do not clash with walking and M and F do not clash with map binds. Filters also switch with Alt+Q / Alt+E and the gamepad triggers, like the task list tabs on the map. Enter and the arrow keys always work, even without binds.

Default binds come from the addon file `configs/ixray_settings/mod_default_controls_*.ltx`:

```ini
bind pda_task_journal_filter_prev kA
bind_gamepad pda_task_journal_filter_prev cDPAD_LEFT
```

The engine loads such files at start-up and when controls are reset to defaults. To list the actions in the options, add a group to `mod_ui_keybinding_*.xml`:

```xml
<group name="kb_grp_pda_task_journal" override="add">
	<command id="kb_pda_tj_show_on_map" exe="pda_task_journal_show_on_map"/>
</group>
```

If a player already has saved controls and the binds are missing, press "Default" in the control settings.

## Replacing the map task window

By default the tab works next to the old task window on the map. To make the tab replace it completely, enable it explicitly with an attribute on `main_wnd` in `pda_task_journal.xml`:

```xml
<main_wnd x="0" y="0" width="1024" height="768" replace_task_list="1">
```

Then:

1. The `btn_second_task` button on the map is hidden and the task list popup never opens.
2. Every way to open that list leads to the "Tasks" tab instead: the `pda_tasks_toggle_list` hotkey, the list button of the active task card, the `eptTaskList` alias tab, a Lua call.
3. Alt+Q / Alt+E and the gamepad triggers on the map tab no longer switch list filters, since there is no list.

Without an `eptTaskJournal` tab in `pda*.xml` the attribute has no effect.

With the replacement on, the `btn_second_task` and `second_task_wnd` nodes can be removed from `pda_tasks*.xml`: without `second_task_wnd` the map does not create the list window at all. HD HUD does this.

## Tab button

```xml
<button x="171" y="3" width="90" height="35" id="eptTaskJournal" hint="pda_tab_task_journal_hint" frame_mode="1">
	<text align="c" vert_align="c" font="font_oswald_20">pda_tab_task_journal</text>
</button>
```

The tab background is the `eptTaskJournal` node in `tab_backgrounds`. The id can be remapped through `[pda_tab_aliases]`.

## pda_task_journal.xml

| Node | Meaning |
| --- | --- |
| `main_wnd` | Tab window. Nested `auto_static`, `auto_frameline`, `auto_framewindow` are created automatically: panel captions, frames, background. |
| `filter_tabs` | Filter tabs. `counters="1"` appends the task count to each label. |
| `filter_hint_prev`, `filter_hint_next` | Key caps next to the filters, a hint bar with one item. |
| `hotkeys_hint` | Any extra hint bars; there can be several. |
| `task_list` | Task list. |
| `group_header` | Group caption in the list. Without it there are no groups. |
| `task_item` | List row template. |
| `t_empty` | Text for an empty list. |
| `details` | Task card. |

Hint bars are described in [map legend and hotkeys](pda-map-legend-hotkeys.md): the `key` node turns on key cap mode, an item is `<item action="..." gamepad_action="..."/>`. An item without a bound key is hidden.

### Filters

| Button id | Tasks |
| --- | --- |
| `active` | Current |
| `completed` | Completed |
| `failed` | Failed and cancelled |
| `archive` | All finished |
| `story`, `side` | Current story or side tasks |
| `all` | All current |

The first button is open by default.

### Groups

```xml
<group_header x="0" y="0" width="300" height="20" text_story="pda_tj_group_story" text_side="pda_tj_group_side">
	<text x="8" align="l" vert_align="c" font="font_rubik_12" r="110" g="110" b="110"/>
</group_header>
```

With this node the list is split into story and side tasks, each part under a caption row.

### List row

```xml
<task_item x="0" y="0" width="300" height="38" padding="10">
	<selection x="0" y="0" width="300" height="38" stretch="1"><texture a="110">ui_inGame2_pda_background_text</texture></selection>
	<active_mark x="0" y="0" width="2" height="38" stretch="1"><texture r="220" g="190" b="80">ui_inGame2_hint_wnd_bar</texture></active_mark>
	<icon x="4" y="8" width="22" height="22" stretch="1"/>
	<title x="32" y="11" width="216" height="16"><text font="font_rubik_14"/></title>
	<status x="180" y="11" width="90" height="16" text_completed="pda_tj_item_completed" text_failed="pda_tj_item_failed"><text align="r" font="font_rubik_12"/></status>
	<btn_focus x="276" y="11" width="14" height="17" stretch="1"><texture>ui_inGame2_pda_center_on_mission_button</texture></btn_focus>
	<colors>
		<normal r="210" g="210" b="210"/>
		<active r="235" g="215" b="140"/>
		<unread r="245" g="245" b="245"/>
		<completed r="130" g="190" b="120"/>
		<failed r="205" g="115" b="95"/>
	</colors>
</task_item>
```

| Node | Meaning |
| --- | --- |
| `selection` | Background of the selected row. |
| `active_mark` | Mark of the tracked task. |
| `icon` | Task icon. |
| `type_icon` | Type icon, textures in `texture_story` and `texture_side`. |
| `title` | Title. The row grows when it wraps; `padding` is the space below it. |
| `time` | Receive or finish time. |
| `status` | Label of a finished task. |
| `btn_focus` | "Show on map" button, visible only for current tasks with a map target. |
| `colors` | Title colors: `normal`, `active`, `unread`, `completed`, `failed`. |

### Empty list

`t_empty` with `text` (default) and `text_<filter id>` attributes.

### Card

```xml
<details x="366" y="117" width="635" height="628" heading_source="description"
	image_level_intro="1" image_default="intro\intro_no_start_picture"
	received_format="pda_tj_received" finished_format="pda_tj_finished" location_format="pda_tj_location"
	objectives_caption="pda_tj_objectives">
	<empty x="14" y="12" width="600" height="20"><text font="font_rubik_14">pda_tj_select_task</text></empty>
	<image x="1" y="0" width="633" height="186" stretch="1"/>
	<body x="0" y="194" width="635" height="356" vert_interval="4"/>
	<btn_map x="8" y="562" width="300" height="26">
		<text x="32" align="l" vert_align="c" font="font_rubik_14">pda_tj_show_on_map</text>
		<hint x="0" y="1" width="26" height="24">...<item action="pda_task_journal_show_on_map"/></hint>
	</btn_map>
	<heading x="0" y="0" width="615" height="24" complex_mode="1"><text font="font_oswald_24"/></heading>
	<info x="0" y="0" width="615" height="16"><text font="font_rubik_12"/></info>
	<objective x="12" y="0" width="600" height="16" complex_mode="1"><text font="font_rubik_14"/></objective>
</details>
```

| Attribute | Effect |
| --- | --- |
| `heading_source` | `title` or `description`: what the `heading` line shows. When the description is used, it is not repeated in `descr`. |
| `image_section` | ltx section with pictures, `task_journal_images` by default. |
| `image_level_intro` | `1`: when the section has no picture, use the loading screen of the target level `intro\intro_<level>`. |
| `image_default` | Picture when nothing else is found. |
| `received_format`, `finished_format`, `location_format` | String ids for time and place, `{value}` is replaced with the value. |
| `objectives_caption` | Caption above the stage list. |
| `text_story`, `text_side`, `text_in_progress`, `text_active`, `text_completed`, `text_failed` | Labels for the optional `type` and `status` nodes. |

Child nodes: `empty`, `image`, `icon`, `title`, `type`, `status`, `body`, `btn_map`, `btn_activate`. The optional `hint` inside a button is its key cap; it hides together with the button. Body line templates: `heading`, `descr`, `info`, `objectives_caption`, `objective`; stage colors in `colors` (`progress`, `completed`, `failed`).

`image` may have `crop_rect="x,y,w,h"`: a texture area in pixels when the picture should be cropped instead of stretched. Without a picture the body moves up into its place.

### Task pictures

The picture is looked up in this order:

1. A key with the task id in the `image_section` section.
2. A key with the target level name in the same section.
3. The target level loading screen, if `image_level_intro="1"` and the texture exists.
4. The `default` key in the section.
5. The `image_default` attribute.

```ini
[task_journal_images]
zat_b14_stalker_task  = ui\task_journal\zat_b14
jupiter               = ui\task_journal\jupiter_common
default               = ui\task_journal\placeholder
```

Any addon can extend the section through DLTX.

## Limitations

1. "Show on map" needs a map tab, `eptTasks` or `eptTaskList`, in the PDA.
2. The engine does not tell cancelled tasks from failed ones.
3. The target level is known while the task has a map spot. Finished tasks get their picture by id or by default.
4. Selecting a task marks it as read.

## Post-build checks

1. Without an `eptTaskJournal` button the PDA looks as before.
2. The tab opens, tasks are split into "Story" and "Side", the tracked task is first in its group.
3. A and D switch filters, the arrows move through the list, the key caps show the current binds.
4. M, Enter, a double click and the row crosshair open the map on the target. F moves tracking.
5. The card shows the target level loading screen, the heading from the description, time and place.
6. After a task completes or fails it moves to the right filter by itself.
7. The actions are listed in the control settings under "PDA: tasks".
8. Without `replace_task_list` the task window on the map works as before.
9. With `replace_task_list="1"` the map has no list button and the list hotkey opens the "Tasks" tab.
