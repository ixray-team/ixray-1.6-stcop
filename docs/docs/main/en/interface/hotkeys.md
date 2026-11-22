> [!IMPORTANT]
> **Status**: In development

# Hotkeys: actions, contexts and hints

## Overview

One system for every interface and game key:

1. An **action** is described once in `ltx`: where it works, its default key, label and group in the controls menu.
2. A **context** defines where an action works: `game`, `ui`, `pda`, `pda.map` and so on. Contexts are nested.
3. **Hints** are drawn from a shared style: a window layout needs one line.
4. **Lua** can add its own actions and catch their presses without engine changes.

Built-in engine actions (`ui_tab_left`, `pda_tasks_*` and the rest) work as before and take part in the system. Binds are stored in `user.ltx` as usual: `bind action_name key`.

## Action

A `[hotkey.<name>]` section in any system config, for example `configs/mod_system_my_addon.ltx`:

```ini
[hotkey.my_addon_scanner]
context  = game
key      = kJ
gamepad  = cY
label    = st_my_addon_scanner
options  = kb_grp_my_addon
hint     = 10
```

| Field | Meaning |
| --- | --- |
| `context` | Where the action works. `game` by default. Ignored for built-in actions. |
| `key`, `key2` | Default primary and secondary key |
| `gamepad`, `gamepad2` | Default gamepad buttons |
| `label` | String id of the caption in the controls menu and in automatic hint bars |
| `options` | String id of the controls menu group. Without it the action is not listed in the menu. |
| `hint` | Order in automatic hint bars, 0 means hidden |

A section can also be written for a built-in action: then it only sets default keys, label, group and order. HD HUD uses this to list the map actions in the controls menu.

Default keys are applied by the `default_controls` command (at start and by the "Default" button in the settings). The player's binds in `user.ltx` override them.

Up to 256 custom actions can be added.

## Contexts

| Context | Parent | Active |
| --- | --- | --- |
| `game` | | In game with no interface open |
| `game.vehicle`, `game.aiming` | `game` | Vehicle, aiming |
| `ui` | | Any open window |
| `pda` | `ui` | PDA |
| `pda.map`, `pda.journal`, `pda.records`, `pda.reference`, `pda.contacts`, `pda.logs` | `pda` | PDA tabs |

Custom contexts are declared in `[hotkey_contexts]`: `name = parent`.

```ini
[hotkey_contexts]
my_addon.menu = ui
```

How they work:

1. A key press is looked up in the active context first, then in its parents. A tab action overrides a PDA action on the same key.
2. Two actions of one context cannot share a key: binding it removes it from the old action. In different contexts the same key works independently.
3. With no interface open `game` is active. An open window enables its own context; other windows give `ui`.

## Layers

Layers let one key do different things in different windows: F is "Use" in game, "Track mark" on the map and "Track" in Records. The topmost layer gets a key press first; the others get only what it did not take.

Layers are off by default. Without the flag windows receive key presses in the old order and everything works as before.

```ini
[hotkey_layers]
enabled = true

[hotkey_layer.my_addon.menu]
parent   = ui
priority = 200
block    = all
label    = st_my_addon_menu_layer
```

| Field | Meaning |
| --- | --- |
| `parent` | Parent context |
| `priority` | Higher layers get the press first. On a tie the layer enabled later goes first. |
| `block` | `all`: a press the layer did not take is not passed to lower layers (the window's old handling still runs). `none` (default): it is passed. `ui` is `all` by default: while a window is open the game layer gets no presses. When the field is absent the layer keeps its previous value. |
| `label` | String id of the layer name in the controls menu, for example "PDA: records". Without it the layer is not named in the menu. |

Built-in layer priorities: `game` 0, `ui` 50, `pda` 100, PDA tabs 110.

Layers work across the whole game, not only in the PDA:

1. The `game` layer is active while a level is loaded.
2. Every window opened as a dialog (inventory, trade, talk, PDA, mod windows) enables its root layer, `ui` by default. A window can set its own context.
3. Presses go only to the top window's layers and to layers enabled after it. A window opened over another one fully covers its layers: typing in an edit box does not switch the PDA tabs below it.
4. Inside a window layers refine each other: the PDA handles `pda` and `ui` (Q/E tabs), the map `pda.map`, Records `pda.records`, Journal `pda.journal`.
5. A press goes through the layers from top to bottom. A layer takes it when the key has an action from its contexts and the layer performed it. Custom (Lua) actions fire in the layers where they are visible.
6. If no layer takes the press, it goes the old way.
7. One key in different contexts is not a conflict. The controls menu treats only a match inside one context as a conflict.

A layer from C++: `CHotkeyContextScope` with `SetContext`, `SetHandler(contexts, handler)` and `SetActive`. A window has `SetHotkeyContext` and `GetHotkeyLayer`.

Example: E is bound to "Tab right" (`ui`) and to the Records "Action" (`pda.records`). On the Records tab E runs the action; on the other tabs it switches tabs.

### Controls menu

With layers on, the controls menu shows where each key works. Without the flag the menu is unchanged.

1. An action whose layer has a `label` gets the layer name after its caption: "Track  (PDA: records)".
2. An asterisk after the key ("F *") means the same key is bound in a related layer, a parent or a child one. For example, F in game and F in Records. This is not an error: in a window the top layer's action runs, in game the game action.
3. Binding a key that is already taken in the same layer shows "E? - Tab right" in the field. Pressing the same key again confirms the replacement and removes the key from the old action. Another key or leaving the field cancels it.
4. Custom actions without `options` whose layer has a `label` are grouped under the layer name.

HD HUD names the `ui`, `pda`, `pda.map`, `pda.journal`, `pda.records` and `pda.reference` layers.

On a gamepad the PDA tabs are still switched by the old handler (with repeat on hold). Tab layers work on a gamepad too.

## Hints

### Styles

`configs/ui/hotkey_styles.xml` holds chip templates. Mods extend it with `mod_hotkey_styles_*.xml`.

```xml
<w>
	<style id="pda" key_padding="6" key_min_width="22" key_spacing="3" label_spacing="6" item_spacing="16">
		<key_back type="line" x="0" y="0" width="22" height="22">
			<texture>ui_RP2_HotKey</texture>
		</key_back>
		<key x="0" y="0" width="22" height="22">
			<text align="c" vert_align="c" font="font_rubik_12" r="225" g="225" b="225"/>
		</key>
		<label x="0" y="0" width="10" height="22">
			<text align="l" vert_align="c" font="font_rubik_12" r="190" g="190" b="190"/>
		</label>
	</style>
</w>
```

The `key_back`, `key`, `label` and spacing attributes are described in the [map legend](pda-map-legend-hotkeys.md#key-caps) page.

### Single chip

```xml
<hotkey x="252" y="39" width="22" height="22" chips_align="c" action="ui_tab_left" style="pda"/>
```

### Bar

Explicit list:

```xml
<hotkey_bar x="50" y="710" width="470" height="22" style="pda">
	<item action="pda_tasks_toggle_legend" label="pda_hint_legend"/>
	<item action="pda_tasks_map_zoom_in,pda_tasks_map_zoom_out">pda_hint_zoom</item>
</hotkey_bar>
```

Automatic, every action of the context with `hint > 0` in `hint` order:

```xml
<hotkey_bar x="50" y="710" width="900" height="22" style="pda" context="pda.map"/>
```

1. `context` takes several contexts separated by commas. Explicit and automatic items can be mixed; automatic mode shows at most 16 items.
2. Attributes of the node override the style. A node with its own `key` needs no style.
3. The bar refreshes itself when a bind, the input device or the set of actions changes.
4. Actions without a key are not shown. Items that do not fit are dropped from the end.
5. The old `hotkeys_hint` node works as `hotkey_bar`.

The `hotkey`, `hotkey_bar` and `hotkeys_hint` nodes are read by the PDA windows: `pda.xml`, `pda_tasks.xml`, `pda_task_journal.xml`, `pda_records.xml`, `pda_reference.xml`, the personal marks panel.

## Lua

The `hotkeys` module:

```lua
function on_game_start()
	hotkeys.register("my_addon.scanner", {
		context = "game",
		key = "kJ",
		gamepad = "cY",
		label = "st_my_addon_scanner",
		options = "kb_grp_my_addon",
		hint = 10,
	})
	hotkeys.on("my_addon.scanner", function(name)
		my_addon.toggle_scanner()
	end)
end
```

| Function | Effect |
| --- | --- |
| `register(name, def)` | Registers an action. `def` takes the same fields as `ltx`. Keys are applied only if the action is not bound yet. |
| `register_context(name, parent)` | Adds a context |
| `register_layer(name, def)` | Adds a layer: `parent`, `priority`, `block` |
| `layers_enabled()` | Whether layers are on |
| `on(name, fn)`, `off(name, fn)` | Subscribes to presses. `fn(name)` may return `false` to let the press through. |
| `push_context(name)`, `pop_context(name)` | Enables and disables a custom context, for example for a custom menu |
| `active_context()` | Current context |
| `key_name(name)` | Key text for the current device: `J` or `Y` |
| `is_bound(name)` | Whether a key is bound |
| `context_of(name)` | Context of an action |
| `action_at(dik, context)` | Which action a key triggers in a context, the current one by default |

1. `hotkeys.on` receives presses of custom actions only. Built-in actions are handled by the engine.
2. A handler fires only when its action is visible from the active context.
3. An action registered from Lua keeps the player's bind: the `bind` line from `user.ltx` is remembered and applied on registration. For default keys and a controls menu entry an `ltx` section is more reliable: it exists before scripts load.
4. `$$ACTION_name$$` in strings still works, custom actions included.

## Limitations

1. At most 256 custom actions.
2. Contexts are not saved: a window enables its context every time it is shown.
3. Hints and contexts are wired into the PDA windows only for now. Other windows give the shared `ui` context.
4. Lua handlers work on a level only: presses in the main menu do not reach them.

## Post-build checks

1. Q and E next to the PDA tabs and the map hotkey bars show keys and update without a restart after a bind changes in the settings.
2. The controls menu has a "PDA: map" group with the map actions and their keys.
3. An action from `[hotkey.*]` with `key` gets its key after "Default" in the settings and is saved to `user.ltx`.
4. `hotkeys.register` and `hotkeys.on` from Lua: the press calls the handler in game, and does not call it with the PDA open when the context is `game`.
5. `<hotkey_bar context="pda.map"/>` lists the map actions with `hint` in order.
6. Built-in actions behave as before: tab switching, map controls, Records and Journal.
7. Without `[hotkey_layers] enabled`, E on the Records tab switches tabs as before. With the flag E runs the record action, and Q/E switch tabs on the other tabs.
8. One key in two different layers can be bound in the controls menu without clearing the other field.
9. With HD HUD the controls menu shows the layer name for the map and Records actions, and F in game and in Records has an asterisk.
10. Binding E to a `ui` layer action where E is taken first shows "E? - ...", a second E confirms, another key cancels.
11. After "Default" and after a bind change the asterisks update without reopening the menu.
12. Without `[hotkey_layers] enabled` there are no layer names, asterisks or confirmation, and a taken key is removed from the old action at once.
