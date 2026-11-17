# Windows, widgets, and HUD

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

This section lists UI and HUD registrations. In-game windows are available after creation and depend on the game mode.

## Syntax

| Call | Arguments and result |
| --- | --- |
| `window:FindWindow(name)` | Searches the current node and its descendants by window name. Returns a `CUIWindow` or `nil`. |
| `window:FindStatic(name)` | Performs the same search and returns a `CUIStatic`, or `nil` if the node is missing or has another type. |
| `view:HasWindow(item)` | Checks whether the window belongs to the contents of a `CUIScrollView`. |
| `view:ScrollToItem(item, offset)` | Scrolls to the given child window, adding the integer vertical offset. |
| `bar:ShowBackground(enabled)` | Shows or hides the background of a `CUIProgressBar`. |
| `bar:SnapProgressPos()` | Moves the displayed fill to the latest position set by `SetProgressPos`. |

## Examples

### Update a named widget

Pass the dialog root containing a `CUIStatic` named `status_label`.

```lua
local function set_status(window, text)
    local label = window:FindStatic("status_label")
    if not label then return false end
    label:SetText(text)
    return true
end
```

### Reveal a row

Pass an initialized `CUIScrollView` and a window already added to it.

```lua
local function reveal_item(view, item)
    if not view:HasWindow(item) then return false end
    view:ScrollToItem(item, 0)
    return true
end
```

### Update the fill immediately

Pass an initialized `CUIProgressBar`. Use a value in the bar’s range, such as `40` for a `0–100` bar.

```lua
local function update_bar(bar, value)
    bar:ShowBackground(true)
    bar:SetProgressPos(value)
    bar:SnapProgressPos()
end
```

## Registrations

### Global scope

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `abort_sleep()` | Function | Ends active sleep early. | [UIGameCustom_script.cpp:213](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L213) |
| `cancel_sleep_dialog()` | Function | Closes the sleep dialog and cancels the selection. | [UIGameCustom_script.cpp:210](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L210) |
| `confirm_sleep()` | Function | Confirms the selected duration and starts sleep when allowed. | [UIGameCustom_script.cpp:211](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L211) |
| `CUIActionRepeatersManager` | Class | Manages action repeats for held inputs. | [UIActionRepeaters_script.cpp:22](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L22) |
| `CUIActionRepeatersOwner` | Class | Identifies an owner of action repeaters. | [UIActionRepeaters_script.cpp:13](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L13) |
| `CUICaption` | Class | Widget for named text messages. | [ui_export_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L96) |
| `CUICompassBar()` | Class | HUD compass bar widget. | [UIActorMenu_script.cpp:465](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L465) |
| `CUIZoneMap()` | Class | HUD minimap for the current zone. | [UIActorMenu_script.cpp:458](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L458) |
| `EDDListType` | Class | Inventory drag-and-drop list type enumeration. | [UIActorMenu_script.cpp:379](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L379) |
| `FactionState` | Class | Faction state for the faction screen. | [FactionState.cpp:65](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L65) |
| `FitInRect(window, visible_rect, border, widescreen_x_offset)` | Function | Positions a hint window near the cursor using the visible rectangle, border and widescreen X offset. Returns `false` if the cursor is outside the rectangle. | [UIWindow_script.cpp:128](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L128) |
| `force_sleep(hours)` | Function | Forces sleep for the given number of game hours. | [UIGameCustom_script.cpp:212](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L212) |
| `get_sleep_phase()` | Function | Returns the numeric phase of the current sleep session. | [UIGameCustom_script.cpp:219](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L219) |
| `get_sleep_selected_hours()` | Function | Returns the duration selected in the sleep dialog in game hours. | [UIGameCustom_script.cpp:216](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L216) |
| `GetCursorPosition()` | Function | Returns the cursor position. Result `vector2`. Implementation `return GetUICursor().GetCursorPosition();`. | [UIWindow_script.cpp:126](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L126) |
| `GetFontArial14()` | Function | Returns the `CGameFont` object for Arial 14. | [UIWindow_script.cpp:117](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L117) |
| `GetFontArial21()` | Function | Returns the `CGameFont` object for Arial 21. | [UIWindow_script.cpp:118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L118) |
| `GetTextureInfo(name, fallback_name)` | Function | Returns the texture ID’s `TEX_INFO` record, looking up the fallback ID when the primary one is missing. | [UIWindow_script.cpp:137](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L137) |
| `GetTextureName(texture_id)` | Function | Returns the file path for a registered texture ID. | [UIWindow_script.cpp:135](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L135) |
| `GetTextureRect(texture_id)` | Function | Returns the texture region rectangle for the given ID. | [UIWindow_script.cpp:136](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L136) |
| `hide_sleep_dialog()` | Function | Hides the sleep dialog. | [UIGameCustom_script.cpp:209](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L209) |
| `is_actor_sleeping()` | Function | Checks whether the actor is asleep. | [UIGameCustom_script.cpp:218](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L218) |
| `is_sleep_dialog_ready()` | Function | Checks whether the sleep dialog is ready. | [UIGameCustom_script.cpp:214](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L214) |
| `is_sleep_dialog_shown()` | Function | Checks whether the sleep dialog is visible. | [UIGameCustom_script.cpp:215](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L215) |
| `set_sleep_blocked(blocked [, warning_text])` | Function | Blocks or allows sleep. The second overload accepts a warning message. | [UIGameCustom_script.cpp:220](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L220), [UIGameCustom_script.cpp:221](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L221) |
| `set_sleep_selected_hours(hours)` | Function | Sets the duration selected in the sleep dialog. | [UIGameCustom_script.cpp:217](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L217) |
| `SetARGB(rgba, a)` | Function | Sets the argb. Arguments `rgba` (`number`), `a` (`number`). Result `number`. Implementation `return subst_alpha(rgba, a);`. | [UIWindow_script.cpp:129](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L129) |
| `SetCursorPosition(pos)` | Function | Sets the cursor position. Arguments `pos` (`vector2`). Implementation `GetUICursor().SetUICursorPosition(pos);`. | [UIWindow_script.cpp:127](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L127) |
| `show_sleep_dialog()` | Function | Attempts to open the sleep duration dialog. | [UIGameCustom_script.cpp:207](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L207) |
| `show_sleep_dialog_at_hour(hours)` | Function | Opens the sleep dialog with the given initial duration when its layout is loaded. | [UIGameCustom_script.cpp:208](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L208) |
| `system_depends` | Class | Lua type for system depends, backed by `enum_exporter`. | [UIOptionsManagerScript.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L59) |
| `TEX_INFO` | Class | File and region metadata for a registered UI texture. | [UIWindow_script.cpp:131](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L131) |

### `ActorMenu`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `ActorMenu.get_actor_menu()` | Function | Returns the actor menu. Result `CUIActorMenu`. Implementation `return CurrentGameUI()->ActorMenu();`. | [UIActorMenu_script.cpp:494](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L494) |
| `ActorMenu.get_maingame()` | Function | Returns the maingame. Result `CUIMainIngameWnd`. Implementation `if (CUIGameCustom* ui = CurrentGameUI()) return ui->UIMainIngameWnd; return nullptr;`. | [UIActorMenu_script.cpp:496](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L496) |
| `ActorMenu.get_menu_mode()` | Function | Returns the menu mode. Result `number`. Implementation `return (u8)(CurrentGameUI()->ActorMenu()->GetMenuMode());`. | [UIActorMenu_script.cpp:495](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L495) |
| `ActorMenu.get_pda_menu()` | Function | Returns the pda menu. Result `CUIPdaWnd`. Implementation `return CurrentGameUI()->PdaMenu();`. | [UIActorMenu_script.cpp:493](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L493) |

### `CDialogHolder`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CDialogHolder:SetMainInputReceiver(ir, find_remove)` | Method | Sets the main input receiver. Arguments `ir` (`CUIDialogWnd`), `find_remove` (`boolean`). | [UIWindow_script.cpp:205](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L205) |
| `CDialogHolder:start_stop_menu(pDialog, bDoHideIndicators)` | Method | Starts the stop menu. Arguments `pDialog` (`CUIDialogWnd`), `bDoHideIndicators` (`boolean`). | [UIWindow_script.cpp:203](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L203) |

### `CGameFont`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CGameFont.alJustify` | Constant | `CGameFont` enumeration value for al justify. `CGameFont::alJustify`. | [ui_export_script.cpp:93](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L93) |

### `CScriptXmlInit`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CScriptXmlInit:GetColor(path, index)` | Method | Returns the color. Arguments `path` (`string`), `index` (`number`). Result `number`. Implementation `return CUIXmlInit::GetColor(m_xml, path, index, 0xFFFFFFFF);`. | [ScriptXMLInit.cpp:373](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L373) |
| `CScriptXmlInit:GetNodesNum(path, index, tag_name)` | Method | Returns the nodes num. Arguments `path` (`string`), `index` (`number`), `tag_name` (`string`). Result `number`. Implementation `return m_xml.GetNodesNum(path, index, tag_name);`. | [ScriptXMLInit.cpp:366](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L366) |
| `CScriptXmlInit:InitArrow(path, parent)` | Method | Initializes the arrow. Arguments `path` (`string`), `parent` (`CUIWindow`). Result `CUIArrow`. Implementation `CUIArrow* pWnd = new CUIArrow(); pWnd->init_from_xml (m_xml, path, parent); return pWnd;`. | [ScriptXMLInit.cpp:405](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L405) |
| `CScriptXmlInit:InitArrowStepper(path, parent)` | Method | Initializes the arrow stepper. Arguments `path` (`string`), `parent` (`CUIWindow`). Result `CUIArrowStepper`. | [ScriptXMLInit.cpp:396](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L396) |
| `CScriptXmlInit:InitGamepadLegend(path, parent)` | Method | Initializes the gamepad legend. Arguments `path` (`string`), `parent` (`CUIWindow`). Result `CUIGamepadLegend`. | [ScriptXMLInit.cpp:382](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L382) |
| `CScriptXmlInit:InitLabel(path, parent)` | Method | Initializes the label. Arguments `path` (`string`), `parent` (`CUIWindow`). Result `CUIFrameLineWnd`. | [ScriptXMLInit.cpp:378](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L378) |
| `CScriptXmlInit:InitList(path, parent)` | Method | Initializes the list. Arguments `path` (`string`), `parent` (`CUIWindow`). Result `CUIListWnd`. | [ScriptXMLInit.cpp:403](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L403) |
| `CScriptXmlInit:InitStackPanel(path, parent)` | Method | Initializes the stack panel. Arguments `path` (`string`), `parent` (`CUIWindow`). Result `CUIStackPanel`. | [ScriptXMLInit.cpp:381](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L381) |
| `CScriptXmlInit:NavigateToNode(path, index)` | Method | Returns the navigate to node. Arguments `path` (`string`), `index` (`number`). Result `boolean`. | [ScriptXMLInit.cpp:367](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L367) |
| `CScriptXmlInit:NavigateToNode_ByAttribute(tag_name, attrib_name, attrib_value)` | Method | Returns the navigate to node by attribute. Arguments `tag_name` (`string`), `attrib_name` (`string`), `attrib_value` (`string`). Result `boolean`. | [ScriptXMLInit.cpp:368](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L368) |
| `CScriptXmlInit:NavigateToNode_ByPath(path, index, tag_name, attrib, attrib_value_pattern)` | Method | Returns the navigate to node by path. Arguments `path` (`string`), `index` (`number`), `tag_name` (`string`), `attrib` (`string`), `attrib_value_pattern` (`string`). Result `boolean`. | [ScriptXMLInit.cpp:369](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L369) |
| `CScriptXmlInit:NavigateToRoot()` | Method | Resets the XML navigation context to the document root. Implementation `m_xml.SetLocalRoot(m_xml.GetRoot());`. | [ScriptXMLInit.cpp:370](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L370) |
| `CScriptXmlInit:NodeExist(path, index)` | Method | Returns the node exist. Arguments `path` (`string`), `index` (`number`). Result `boolean`. Implementation `if (m_xml.NavigateToNode(path, index)) { return true; } return false;`. | [ScriptXMLInit.cpp:365](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L365) |
| `CScriptXmlInit:ParseDirFile(xml_dir, xml_file)` | Method | Loads XML from the specified config subdirectory. Arguments `xml_dir` (`string`), `xml_file` (`string`). Implementation `m_xml.Load(CONFIG_PATH, xml_dir, xml_file);`. | [ScriptXMLInit.cpp:363](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L363) |
| `CScriptXmlInit:ReadAttribute(path, index, attrib)` | Method | Reads the attribute. Arguments `path` (`string`), `index` (`number`), `attrib` (`string`). Result `string`. Implementation `return m_xml.ReadAttrib(path, index, attrib, "");`. | [ScriptXMLInit.cpp:372](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L372) |
| `CScriptXmlInit:ReadValue(path, index)` | Method | Reads the value. Arguments `path` (`string`), `index` (`number`). Result `string`. Implementation `return m_xml.Read(path, index, "");`. | [ScriptXMLInit.cpp:371](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ScriptXMLInit.cpp#L371) |

### `CUI3tButton`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUI3tButton:OnClick()` | Method | Handler for the click event. Implementation `CUIButton::OnClick (); PlaySoundT ();`. | [UIButton_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L54) |
| `CUI3tButton:SetHighlighted(val)` | Method | Sets the highlighted. Arguments `val` (`boolean`). Implementation `m_bForceHighlight = val;`. | [UIButton_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L55) |

### `CUIActionRepeatersManager`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIActionRepeatersManager:CanRepeatActionNow(owner, actionId)` | Method | Checks whether the owner’s action is due to repeat. Arguments `owner` (`ActionRepeatersOwnerKey`), `actionId` (`number`). Result `boolean`. | [UIActionRepeaters_script.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L29) |
| `CUIActionRepeatersManager:IsActionStarted(owner, actionId)` | Method | Checks whether an action repeat has started for the given owner. Arguments `owner` (`ActionRepeatersOwnerKey`), `actionId` (`number`). Result `boolean`. | [UIActionRepeaters_script.cpp:31](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L31) |
| `CUIActionRepeatersManager:Reset(actionId)`, `CUIActionRepeatersManager:Reset(owner, actionId)` | Method | Resets repeats for the action. The overload with an owner limit resets that owner’s registration. | [UIActionRepeaters_script.cpp:27](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L27), [UIActionRepeaters_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L28) |
| `CUIActionRepeatersManager:ResetAll(skipOneOwner)` | Method | Resets the all. Arguments `skipOneOwner` (`CUIActionRepeatersOwner`). | [UIActionRepeaters_script.cpp:26](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L26) |
| `CUIActionRepeatersManager:SetActionStarted(owner, actionId)` | Method | Sets the action started. Arguments `owner` (`ActionRepeatersOwnerKey`), `actionId` (`number`). | [UIActionRepeaters_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L30) |
| `CUIActionRepeatersManager:UnregisterOwner(owner)` | Method | Unregisters the owner. Arguments `owner` (`ActionRepeatersOwnerKey`). | [UIActionRepeaters_script.cpp:25](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIActionRepeaters_script.cpp#L25) |

### `CUIActorMenu`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIActorMenu:CallMessageBoxOK(text)` | Method | Shows a text message with an OK button. Arguments `text` (`string`). Implementation `m_bShowInfoWnds = false; m_message_box_ok->SetText(text); m_message_box_ok->ShowDialog(false);`. | [UIActorMenu_script.cpp:402](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L402) |
| `CUIActorMenu:CallMessageBoxYesNo(text)` | Method | Shows a Yes/No confirmation dialog. Arguments `text` (`string`). | [UIActorMenu_script.cpp:401](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L401) |
| `CUIActorMenu:get_drag_item()` | Method | Returns the drag item. Result `game_object`. | [UIActorMenu_script.cpp:396](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L396) |
| `CUIActorMenu:GetInvBox()` | Method | Returns the inventory box. Result `game_object`. | [UIActorMenu_script.cpp:411](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L411) |
| `CUIActorMenu:GetInventorySortSystem()` | Method | Returns the inventory sort system. Result `string`. | [UIActorMenu_script.cpp:417](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L417) |
| `CUIActorMenu:GetMenuMode()` | Method | Returns the menu mode. Result `EMenuMode`. | [UIActorMenu_script.cpp:409](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L409) |
| `CUIActorMenu:GetPartner()` | Method | Returns the partner. Result `game_object`. | [UIActorMenu_script.cpp:410](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L410) |
| `CUIActorMenu:highlight_for_each_in_slot(functor, type, slot_id)` | Method | Highlights the for each in slot. Arguments `functor` (`function`), `type` (`number`), `slot_id` (`number`). | [UIActorMenu_script.cpp:398](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L398) |
| `CUIActorMenu:highlight_section_in_slot(section, type, slot_id)` | Method | Highlights the section in slot. Arguments `section` (`string`), `type` (`number`), `slot_id` (`number`). | [UIActorMenu_script.cpp:397](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L397) |
| `CUIActorMenu:refresh_current_cell_item()` | Method | Refreshes the current cell item. | [UIActorMenu_script.cpp:399](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L399) |
| `CUIActorMenu:SetActor(GO)` | Method | Sets the actor. Arguments `GO` (`game_object`). Implementation `menu->SetActor(Actor()->cast_inventory_owner());`. | [UIActorMenu_script.cpp:414](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L414) |
| `CUIActorMenu:SetInvBox(GO)` | Method | Sets the inventory box. Arguments `GO` (`game_object`). Implementation `CInventoryBox* inv_box = GO->object().cast_inventory_box(); if (inv_box) { menu->SetInvBox(inv_box); }`. | [UIActorMenu_script.cpp:413](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L413) |
| `CUIActorMenu:SetInventorySortSystem(system)` | Method | Sets the inventory sort system. Arguments `system` (`string`). | [UIActorMenu_script.cpp:416](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L416) |
| `CUIActorMenu:SetMenuMode(mode)` | Method | Sets the menu mode. Arguments `mode` (`EMenuMode`). | [UIActorMenu_script.cpp:408](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L408) |
| `CUIActorMenu:SetPartner(GO)` | Method | Sets the partner. Arguments `GO` (`game_object`). Implementation `CInventoryOwner* io = GO->object().cast_inventory_owner(); if (io) menu->SetPartner(io);`. | [UIActorMenu_script.cpp:412](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L412) |
| `CUIActorMenu:ShowGamepadLegend(b)` | Method | Shows the gamepad legend. Arguments `b` (`boolean`). Implementation `if (!menu) { return; } menu->m_should_show_gamepad_legend = b;`. | [UIActorMenu_script.cpp:415](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L415) |
| `CUIActorMenu:ToBelt(GO, b_use_cursor_pos)` | Method | Returns the to belt. Arguments `GO` (`game_object`), `b_use_cursor_pos` (`boolean`). Result `boolean`. | [UIActorMenu_script.cpp:407](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L407) |
| `CUIActorMenu:ToSlot(GO, force_place, slot_id)` | Method | Returns the to slot. Arguments `GO` (`game_object`), `force_place` (`boolean`), `slot_id` (`number`). Result `boolean`. | [UIActorMenu_script.cpp:406](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L406) |
| `CUIActorMenu:UpdateConditionProgressBars()` | Method | Updates the condition progress bars. | [UIActorMenu_script.cpp:400](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L400) |

### `CUIArrow`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIArrow:GetPos()` | Method | Returns the pos. Result `number`. | [UIArrow.cpp:80](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIArrow.cpp#L80) |
| `CUIArrow:SetNewValue(new_value)` | Method | Sets the new value. Arguments `new_value` (`number`). | [UIArrow.cpp:78](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIArrow.cpp#L78) |
| `CUIArrow:SetPos(pos)` | Method | Sets the pos. Arguments `pos` (`number`). Implementation `m_pos = pos; inherited::SetHeading( m_angle_begin + m_pos * m_angle_range );`. | [UIArrow.cpp:79](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIArrow.cpp#L79) |

### `CUIArrowStepper`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIArrowStepper:GetCheck()` | Method | Returns the check. Result `boolean`. | [UIButton_script.cpp:107](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L107) |
| `CUIArrowStepper:GetFValue()` | Method | Returns the floating-point value. Result `number`. | [UIButton_script.cpp:110](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L110) |
| `CUIArrowStepper:GetIValue()` | Method | Returns the integer value. Result `number`. | [UIButton_script.cpp:109](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L109) |
| `CUIArrowStepper:SetCheck(b)` | Method | Sets the check. Arguments `b` (`boolean`). | [UIButton_script.cpp:108](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L108) |
| `CUIArrowStepper:SetCurrentValue()` | Method | Sets the current value. | [UIButton_script.cpp:111](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L111) |

### `CUIButton`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIButton:EnableTextHighlighting(enabled)` | Method | Enables button text highlighting with `true` and disables it with `false`. | [UIButton_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L49) |

### `CUICaption`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUICaption:addCustomMessage(msg_name, x, y, font_size, pFont, al, color, def_str)` | Method | Adds the custom message. Arguments `msg_name` (`string`), `x` (`number`), `y` (`number`), `font_size` (`number`), `pFont` (`CGameFont`), `al` (`CGameFont::EAligment`), `color` (`number`), `def_str` (`string`). | [ui_export_script.cpp:97](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L97) |
| `CUICaption:setCaption(msg_name, message_to_out, color, replaceColor)` | Method | Sets the caption. Arguments `msg_name` (`string`), `message_to_out` (`string`), `color` (`number`), `replaceColor` (`boolean`). | [ui_export_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L98) |

### `CUICheckButton`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUICheckButton:AssignProps(entry, group)` | Method | Assigns the props. Arguments `entry` (`string`), `group` (`string`). Implementation `btn->AssignProps(entry, group);`. | [UIButton_script.cpp:64](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L64) |
| `CUICheckButton:SetSystemDepends(depend)` | Method | Sets the system depends. Arguments `depend` (`number`). Implementation `btn->SetSystemDepends((CUIOptionsItem::ESystemDepends)depend);`. | [UIButton_script.cpp:65](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L65) |
| `CUICheckButton:Toggle()` | Method | Toggles the checkbox state. Implementation `btn->SetCheck(!btn->GetCheck()); btn->SendClickCallback();`. | [UIButton_script.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L66) |

### `CUIComboBox`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIComboBox:AssignProps(entry, group)` | Method | Assigns the props. Arguments `entry` (`string`), `group` (`string`). Implementation `cb->AssignProps(entry, group);`. | [UIComboBox_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L48) |
| `CUIComboBox:GetCurrentIdx()` | Method | Returns the current item index. Result `number`. Implementation `return m_list_box.GetSelectedIDX();`. | [UIComboBox_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L47) |
| `CUIComboBox:GetListBox()` | Method | Returns the list box widget. Result `CUIListBox`. | [UIComboBox_script.cpp:50](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L50) |
| `CUIComboBox:GetSize()` | Method | Returns the size. Result `number`. Implementation `return m_list_box.GetSize();`. | [UIComboBox_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L55) |
| `CUIComboBox:GetValueOf(index)` | Method | Returns the value of. Arguments `index` (`number`). Result `string`. Implementation `if (u32(index) >= GetSize()) return ""; auto token = GetOptToken(); return token[index].name;`. | [UIComboBox_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L42) |
| `CUIComboBox:IsListShown()` | Method | Returns true when the dropdown is expanded. Result `boolean`. Implementation `return m_list_frame.IsShown();`. | [UIComboBox_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L53) |
| `CUIComboBox:OnListItemSelect()` | Method | Handler for the list item select event. | [UIComboBox_script.cpp:51](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L51) |
| `CUIComboBox:SetCurrentIdx(idx)` | Method | Sets the current item index. Arguments `idx` (`number`). Implementation `m_list_box.SetSelectedIDX(idx);`. | [UIComboBox_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L46) |
| `CUIComboBox:SetHighlighted(val)` | Method | Sets the highlighted. Arguments `val` (`boolean`). | [UIComboBox_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L54) |
| `CUIComboBox:SetItemIDX(idx)` | Method | Sets the item idx. Arguments `idx` (`number`). | [UIComboBox_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L56) |
| `CUIComboBox:SetSystemDepends(depend)` | Method | Sets the system depends. Arguments `depend` (`number`). Implementation `cb->SetSystemDepends((CUIOptionsItem::ESystemDepends)depend);`. | [UIComboBox_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L49) |
| `CUIComboBox:ShowList(bShow)` | Method | Shows the list. Arguments `bShow` (`boolean`). | [UIComboBox_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIComboBox_script.cpp#L52) |

### `CUICompassBar`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUICompassBar:Background()` | Method | Returns the background. Result `CUIStatic`. | [UIActorMenu_script.cpp:468](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L468) |
| `CUICompassBar:GetFrame()` | Method | Returns the frame. Result `CUIWindow`. | [UIActorMenu_script.cpp:469](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L469) |

### `CUIGameCustom`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIGameCustom:AbortSleep()` | Method | Ends active sleep early. | [UIGameCustom_script.cpp:189](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L189) |
| `CUIGameCustom:AddCustomMessage(id, x, y, font_size, pFont, alignment, color, flicker)` | Method | Adds the custom message. Arguments `id` (`string`), `x` (`number`), `y` (`number`), `font_size` (`number`), `pFont` (`CGameFont`), `alignment` (`number`), `color` (`number`), `flicker` (`number`). | [UIGameCustom_script.cpp:160](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L160), [UIGameCustom_script.cpp:161](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L161) |
| `CUIGameCustom:AddHudMessage(text, text2, id, trnslate_second_text, time, bSingleInstance)` | Method | Adds the hud message. Arguments `text` (`string`), `text2` (`string`), `id` (`string`), `trnslate_second_text` (`boolean`), `time` (`number`), `bSingleInstance` (`boolean`). Result `SDrawStaticStruct`. | [UIGameCustom_script.cpp:165](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L165) |
| `CUIGameCustom:CancelSleepDialog()` | Method | Closes the sleep dialog and cancels the selection. | [UIGameCustom_script.cpp:186](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L186) |
| `CUIGameCustom:ClearSleepAllowBleeding()` | Method | Clears the allow-sleep-while-bleeding rule. Implementation `if (SleepWnd != nullptr) SleepWnd->ClearSleepAllowBleeding();`. | [UIGameCustom_script.cpp:197](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L197) |
| `CUIGameCustom:ClearSleepHourPresets()` | Method | Clears the sleep duration presets. Implementation `if (SleepWnd != nullptr) SleepWnd->ClearHourPresets();`. | [UIGameCustom_script.cpp:182](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L182) |
| `CUIGameCustom:ClearSleepHoursRange()` | Method | Clears the sleep duration range. Implementation `if (SleepWnd != nullptr) SleepWnd->ClearSleepHoursRange();`. | [UIGameCustom_script.cpp:195](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L195) |
| `CUIGameCustom:ClearSleepMute()` | Method | Clears the muting during sleep. Implementation `if (SleepWnd != nullptr) SleepWnd->ClearSleepMute();`. | [UIGameCustom_script.cpp:201](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L201) |
| `CUIGameCustom:ClearSleepRestorePower()` | Method | Clears the stamina recovery during sleep. Implementation `if (SleepWnd != nullptr) SleepWnd->ClearSleepRestorePower();`. | [UIGameCustom_script.cpp:199](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L199) |
| `CUIGameCustom:ClearSleepSessionOverrides()` | Method | Clears the sleep session overrides. Implementation `if (SleepWnd != nullptr) SleepWnd->ClearSleepSessionOverrides();`. | [UIGameCustom_script.cpp:202](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L202) |
| `CUIGameCustom:ConfirmSleep()` | Method | Confirms the selected duration and starts sleep when allowed. | [UIGameCustom_script.cpp:187](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L187) |
| `CUIGameCustom:CurrentItemAtCell()` | Method | Returns the current item at cell. Result `game_object`. | [UIGameCustom_script.cpp:171](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L171) |
| `CUIGameCustom:CustomMessageOut(id, msg, color)` | Method | Displays the identified message using the given text and color. Arguments `id` (`string`), `msg` (`string`), `color` (`number`). Implementation `GameCaptions()->setCaption(id,msg,color,true);`. | [UIGameCustom_script.cpp:162](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L162) |
| `CUIGameCustom:ForceSleep(hours)` | Method | Forces sleep for the given number of game hours. | [UIGameCustom_script.cpp:188](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L188) |
| `CUIGameCustom:GetSleepPhase()` | Method | Returns the numeric phase of the current sleep session. | [UIGameCustom_script.cpp:193](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L193) |
| `CUIGameCustom:GetSleepSelectedHours()` | Method | Returns the duration selected in the sleep dialog in game hours. | [UIGameCustom_script.cpp:190](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L190) |
| `CUIGameCustom:HideSleepDialog()` | Method | Hides the sleep dialog. | [UIGameCustom_script.cpp:185](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L185) |
| `CUIGameCustom:IsActorSleeping()` | Method | Checks whether the actor is asleep. | [UIGameCustom_script.cpp:192](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L192) |
| `CUIGameCustom:IsSleepDialogReady()` | Method | Checks whether the sleep dialog is ready. | [UIGameCustom_script.cpp:183](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L183) |
| `CUIGameCustom:IsSleepDialogShown()` | Method | Checks whether the sleep dialog is visible. | [UIGameCustom_script.cpp:184](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L184) |
| `CUIGameCustom:RemoveCustomMessage(id)` | Method | Removes the custom message. Arguments `id` (`string`). Implementation `GameCaptions()->removeCustomMessage(id);`. | [UIGameCustom_script.cpp:163](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L163) |
| `CUIGameCustom:SetMainInputReceiver(ir, find_remove)` | Method | Sets the main input receiver. Arguments `ir` (`CUIDialogWnd`), `find_remove` (`boolean`). | [UIGameCustom_script.cpp:153](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L153) |
| `CUIGameCustom:SetSleepAllowBleeding(allow)` | Method | Sets the allow-sleep-while-bleeding rule. Arguments `allow` (`boolean`). Implementation `if (SleepWnd != nullptr) SleepWnd->SetSleepAllowBleeding(allow);`. | [UIGameCustom_script.cpp:196](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L196) |
| `CUIGameCustom:SetSleepBlocked(blocked [, warning_text])` | Method | Blocks or allows sleep. The second overload accepts a warning message. | [UIGameCustom_script.cpp:203](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L203), [UIGameCustom_script.cpp:204](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L204) |
| `CUIGameCustom:SetSleepHourPresets(tbl)` | Method | Sets the sleep duration presets. Arguments `tbl` (`Lua value`). | [UIGameCustom_script.cpp:181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L181) |
| `CUIGameCustom:SetSleepHoursRange(minHours, maxHours)` | Method | Sets the sleep duration range. Arguments `minHours` (`number`), `maxHours` (`number`). Implementation `if (SleepWnd != nullptr) SleepWnd->SetSleepHoursRange(minHours, maxHours);`. | [UIGameCustom_script.cpp:194](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L194) |
| `CUIGameCustom:SetSleepMute(muteMusic, muteEffects)` | Method | Sets the muting during sleep. Arguments `muteMusic` (`boolean`), `muteEffects` (`boolean`). Implementation `if (SleepWnd != nullptr) SleepWnd->SetSleepMute(muteMusic, muteEffects);`. | [UIGameCustom_script.cpp:200](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L200) |
| `CUIGameCustom:SetSleepRestorePower(power)` | Method | Sets the stamina recovery during sleep. Arguments `power` (`number`). Implementation `if (SleepWnd != nullptr) SleepWnd->SetSleepRestorePower(power);`. | [UIGameCustom_script.cpp:198](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L198) |
| `CUIGameCustom:SetSleepSelectedHours(hours)` | Method | Sets the duration selected in the sleep dialog. | [UIGameCustom_script.cpp:191](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L191) |
| `CUIGameCustom:ShowActorMenu()` | Method | Shows the actor menu. Result `boolean`. | [UIGameCustom_script.cpp:169](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L169) |
| `CUIGameCustom:ShowSleepDialog()` | Method | Attempts to open the sleep duration dialog. | [UIGameCustom_script.cpp:179](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L179) |
| `CUIGameCustom:ShowSleepDialogAtHour(hours)` | Method | Opens the sleep dialog with the given initial duration when its layout is loaded. | [UIGameCustom_script.cpp:180](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L180) |
| `CUIGameCustom:UpdateActorMenu()` | Method | Updates the actor menu. | [UIGameCustom_script.cpp:170](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIGameCustom_script.cpp#L170) |

### `CUIGamepadLegend`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIGamepadLegend:ReloadLegend()` | Method | Reloads the legend. Implementation `for (CUIWindow* W : m_ChildWndList) { if (!W \|\| !W->ui_cast_static()) { continue; } W->ui_cast_static()->ReloadText(); }`. | [UIWindow_script.cpp:234](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L234) |

### `CUIHudStatesWnd`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIHudStatesWnd.m_back` | Read-only field | HUD component or setting for back. Read-only. | [UIActorMenu_script.cpp:476](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L476) |
| `CUIHudStatesWnd.m_fire_mode` | Read-only field | HUD component or setting for fire mode. Read-only. | [UIActorMenu_script.cpp:483](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L483) |
| `CUIHudStatesWnd.m_radia_damage` | Read-only field | HUD component or setting for radia damage. Read-only. | [UIActorMenu_script.cpp:488](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L488) |
| `CUIHudStatesWnd.m_ui_grenade` | Read-only field | HUD component or setting for ui grenade. Read-only. | [UIActorMenu_script.cpp:484](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L484) |
| `CUIHudStatesWnd.m_ui_health_bar` | Read-only field | HUD component or setting for ui health bar. Read-only. | [UIActorMenu_script.cpp:486](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L486) |
| `CUIHudStatesWnd.m_ui_stamina_bar` | Read-only field | HUD component or setting for ui stamina bar. Read-only. | [UIActorMenu_script.cpp:487](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L487) |
| `CUIHudStatesWnd.m_ui_weapon_ammo_color_active` | Read-only field | HUD component or setting for ui weapon ammo color active. Read-only. | [UIActorMenu_script.cpp:477](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L477) |
| `CUIHudStatesWnd.m_ui_weapon_ammo_color_inactive` | Read-only field | HUD component or setting for ui weapon ammo color inactive. Read-only. | [UIActorMenu_script.cpp:478](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L478) |
| `CUIHudStatesWnd.m_ui_weapon_ap_ammo` | Read-only field | HUD component or setting for ui weapon ap ammo. Read-only. | [UIActorMenu_script.cpp:481](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L481) |
| `CUIHudStatesWnd.m_ui_weapon_cur_ammo` | Read-only field | HUD component or setting for ui weapon cur ammo. Read-only. | [UIActorMenu_script.cpp:479](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L479) |
| `CUIHudStatesWnd.m_ui_weapon_fmj_ammo` | Read-only field | HUD component or setting for ui weapon fmj ammo. Read-only. | [UIActorMenu_script.cpp:480](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L480) |
| `CUIHudStatesWnd.m_ui_weapon_icon` | Read-only field | HUD component or setting for ui weapon icon. Read-only. | [UIActorMenu_script.cpp:485](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L485) |
| `CUIHudStatesWnd.m_ui_weapon_third_ammo` | Read-only field | HUD component or setting for ui weapon third ammo. Read-only. | [UIActorMenu_script.cpp:482](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L482) |

### `CUIListBox`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIListBox:GetItemHeight()` | Method | Returns the item height. Result `number`. | [UIListBox_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L42) |
| `CUIListBox:NextItem(selectOnly)` | Method | Advances to the next value. Arguments `selectOnly` (`boolean`). | [UIListBox_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L49) |
| `CUIListBox:PrevItem(selectOnly)` | Method | Moves to the previous value. Arguments `selectOnly` (`boolean`). | [UIListBox_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L48) |
| `CUIListBox:SetItemHeight(h)` | Method | Sets the item height. Arguments `h` (`number`). Implementation `m_def_item_height = h;`. | [UIListBox_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L41) |
| `CUIListBox:SetSelectedIndex(idx)` | Method | Sets the selected item index. Arguments `idx` (`number`). Implementation `SetSelected(GetItemByIDX(idx));`. | [UIListBox_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListBox_script.cpp#L40) |

### `CUIListItemEx`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIListItemEx:SetSelectionColor(dwColor)` | Method | Sets the selection color. Arguments `dwColor` (`number`). Implementation `m_dwSelectionColor = dwColor;`. | [UIListWnd_script.cpp:74](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L74) |

### `CUIListWnd`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIListWnd:ActivateList(activity)` | Method | Enables or disables list activity. Arguments `activity` (`boolean`). Implementation `m_bListActivity = activity;`. | [UIListWnd_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L56) |
| `CUIListWnd:AddItem(item)` | Method | Adds the item. Arguments `item` (`CUIListItem`). Result `boolean`. Implementation `return AddItem(item, -1);`. | [UIListWnd_script.cpp:37](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L37) |
| `CUIListWnd:EnableScrollBar(enable)` | Method | Enables the scrollbar. Arguments `enable` (`boolean`). Implementation `m_ScrollBar->SetEnabled(enable); UpdateScrollBar();`. | [UIListWnd_script.cpp:43](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L43) |
| `CUIListWnd:GetFocusedItem()` | Method | Returns the focused list item. Result `number`. | [UIListWnd_script.cpp:61](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L61) |
| `CUIListWnd:GetItem(index)` | Method | Returns the item. Arguments `index` (`number`). Result `CUIListItem`. | [UIListWnd_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L48) |
| `CUIListWnd:GetItemPos(pItem)` | Method | Returns the item pos. Arguments `pItem` (`CUIListItem`). Result `number`. | [UIListWnd_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L49) |
| `CUIListWnd:GetSelectedItem()` | Method | Returns the selected list item. Result `number`. | [UIListWnd_script.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L66) |
| `CUIListWnd:GetSize()` | Method | Returns the list item count. Result `number`. Implementation `return m_ItemList.size();`. | [UIListWnd_script.cpp:50](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L50) |
| `CUIListWnd:GetVertFlip()` | Method | Returns the vert flip. Result `boolean`. | [UIListWnd_script.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L59) |
| `CUIListWnd:IsListActive()` | Method | Checks whether the list is active. Result `boolean`. | [UIListWnd_script.cpp:57](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L57) |
| `CUIListWnd:IsScrollBarEnabled()` | Method | Checks whether the list scrollbar is enabled. Result `boolean`. Implementation `return m_ScrollBar->GetEnabled();`. | [UIListWnd_script.cpp:44](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L44) |
| `CUIListWnd:NextItem(selectOnly, loop)` | Method | Advances to the next value. Arguments `selectOnly` (`boolean`), `loop` (`boolean`). Result `boolean`. | [UIListWnd_script.cpp:64](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L64) |
| `CUIListWnd:PrevItem(selectOnly, loop)` | Method | Moves to the previous value. Arguments `selectOnly` (`boolean`), `loop` (`boolean`). Result `boolean`. | [UIListWnd_script.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L63) |
| `CUIListWnd:ReinitScrollBar()` | Method | Reinitializes the scrollbar. Result `boolean`. | [UIListWnd_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L42) |
| `CUIListWnd:RemoveAll()` | Method | Removes all rows from the list. | [UIListWnd_script.cpp:39](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L39) |
| `CUIListWnd:RemoveItem(index)` | Method | Removes the item. Arguments `index` (`number`). | [UIListWnd_script.cpp:38](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L38) |
| `CUIListWnd:ResetFocusCapture()` | Method | Resets the focus capture. Implementation `m_bForceFocusedItem = false;`. | [UIListWnd_script.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L67) |
| `CUIListWnd:ScrollBar()` | Method | Scrolls the bar. Result `CUIScrollBar`. | [UIListWnd_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L41) |
| `CUIListWnd:ScrollToBegin()` | Method | Scrolls the to begin. Implementation `m_ScrollBar->SetScrollPos((s16)m_ScrollBar->GetMinRange()); m_iFirstShownIndex = m_ScrollBar->GetScrollPos(); UpdateList();`. | [UIListWnd_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L45), [UIListWnd_script.cpp:51](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L51) |
| `CUIListWnd:ScrollToEnd()` | Method | Scrolls the to end. | [UIListWnd_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L46), [UIListWnd_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L52) |
| `CUIListWnd:ScrollToPos(position)` | Method | Scrolls the to pos. Arguments `position` (`number`). | [UIListWnd_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L53) |
| `CUIListWnd:SetFocusedItem(iNewFocusedItem)` | Method | Sets the focused list item. Arguments `iNewFocusedItem` (`number`). | [UIListWnd_script.cpp:60](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L60) |
| `CUIListWnd:SetItemHeight(iItemHeight)` | Method | Sets the item height. Arguments `iItemHeight` (`number`). Implementation `m_iItemHeight = iItemHeight; m_iRowNum = iFloor(GetHeight() / iItemHeight);`. | [UIListWnd_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L47) |
| `CUIListWnd:SetScrollBarProfile(profile)` | Method | Sets the scrollbar profile. Arguments `profile` (`string`). Implementation `m_scrollbar_profile = profile;`. | [UIListWnd_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L40) |
| `CUIListWnd:SetVertFlip(vert_flip)` | Method | Sets the vert flip. Arguments `vert_flip` (`boolean`). Implementation `m_bVertFlip = vert_flip;`. | [UIListWnd_script.cpp:58](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L58) |
| `CUIListWnd:SetWidth(width)` | Method | Sets the width. Arguments `width` (`number`). Implementation `inherited::SetWidth(width);`. | [UIListWnd_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L54) |
| `CUIListWnd:ShowSelectedItem(show)` | Method | Shows the selected list item. Arguments `show` (`boolean`). Implementation `m_bShowSelectedItem = show; if (show) { create_active_back(); } else destroy_active_back();`. | [UIListWnd_script.cpp:62](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIListWnd_script.cpp#L62) |

### `CUIMainIngameWnd`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIMainIngameWnd.m_ind_bleeding` | Read-only field | HUD component or setting for ind bleeding. Read-only. | [UIActorMenu_script.cpp:441](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L441) |
| `CUIMainIngameWnd.m_ind_boost_chem` | Read-only field | HUD component or setting for ind boost chem. Read-only. | [UIActorMenu_script.cpp:451](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L451) |
| `CUIMainIngameWnd.m_ind_boost_health` | Read-only field | HUD component or setting for ind boost health. Read-only. | [UIActorMenu_script.cpp:454](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L454) |
| `CUIMainIngameWnd.m_ind_boost_power` | Read-only field | HUD component or setting for ind boost power. Read-only. | [UIActorMenu_script.cpp:455](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L455) |
| `CUIMainIngameWnd.m_ind_boost_psy` | Read-only field | HUD component or setting for ind boost psy. Read-only. | [UIActorMenu_script.cpp:449](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L449) |
| `CUIMainIngameWnd.m_ind_boost_rad` | Read-only field | HUD component or setting for ind boost rad. Read-only. | [UIActorMenu_script.cpp:456](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L456) |
| `CUIMainIngameWnd.m_ind_boost_radia` | Read-only field | HUD component or setting for ind boost radia. Read-only. | [UIActorMenu_script.cpp:450](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L450) |
| `CUIMainIngameWnd.m_ind_boost_weight` | Read-only field | HUD component or setting for ind boost weight. Read-only. | [UIActorMenu_script.cpp:453](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L453) |
| `CUIMainIngameWnd.m_ind_boost_wound` | Read-only field | HUD component or setting for ind boost wound. Read-only. | [UIActorMenu_script.cpp:452](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L452) |
| `CUIMainIngameWnd.m_ind_helmet_broken` | Read-only field | HUD component or setting for ind helmet broken. Read-only. | [UIActorMenu_script.cpp:445](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L445) |
| `CUIMainIngameWnd.m_ind_outfit_broken` | Read-only field | HUD component or setting for ind outfit broken. Read-only. | [UIActorMenu_script.cpp:446](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L446) |
| `CUIMainIngameWnd.m_ind_overvolume` | Read-only field | HUD component or setting for ind overvolume. Read-only. | [UIActorMenu_script.cpp:448](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L448) |
| `CUIMainIngameWnd.m_ind_overweight` | Read-only field | HUD component or setting for ind overweight. Read-only. | [UIActorMenu_script.cpp:447](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L447) |
| `CUIMainIngameWnd.m_ind_radiation` | Read-only field | HUD component or setting for ind radiation. Read-only. | [UIActorMenu_script.cpp:442](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L442) |
| `CUIMainIngameWnd.m_ind_starvation` | Read-only field | HUD component or setting for ind starvation. Read-only. | [UIActorMenu_script.cpp:443](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L443) |
| `CUIMainIngameWnd.m_ind_weapon_broken` | Read-only field | HUD component or setting for ind weapon broken. Read-only. | [UIActorMenu_script.cpp:444](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L444) |
| `CUIMainIngameWnd.m_ui_hud_states` | Read-only field | HUD component or setting for ui hud states. Read-only. | [UIActorMenu_script.cpp:440](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L440) |
| `CUIMainIngameWnd.UICompassBar` | Read-only field | HUD component or setting for ui compass bar. Read-only. | [UIActorMenu_script.cpp:437](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L437) |
| `CUIMainIngameWnd.UIMotionIcon` | Read-only field | HUD component or setting for ui motion icon. Read-only. | [UIActorMenu_script.cpp:435](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L435) |
| `CUIMainIngameWnd.UIStaticDiskIO` | Read-only field | HUD component or setting for ui static disk io. Read-only. | [UIActorMenu_script.cpp:433](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L433) |
| `CUIMainIngameWnd.UIStaticQuickHelp` | Read-only field | HUD component or setting for ui static quick help. Read-only. | [UIActorMenu_script.cpp:434](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L434) |
| `CUIMainIngameWnd.UIZoneMap` | Read-only field | HUD component or setting for ui zone map. Read-only. | [UIActorMenu_script.cpp:436](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L436) |
| `CUIMainIngameWnd:IsCompassBarMode()` | Method | Checks whether the HUD uses the compass navigation mode. Result `boolean`. Implementation `return m_navigationHud->IsCompassBarMode();`. | [UIActorMenu_script.cpp:439](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L439) |
| `CUIMainIngameWnd:SetNavigationMode(compassBar)` | Method | Sets the navigation mode. Arguments `compassBar` (`boolean`). Implementation `m_navigationHud->SetNavigationModeBool(compassBar);`. | [UIActorMenu_script.cpp:438](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L438) |

### `CUIMessageBoxEx`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIMessageBoxEx:Init(xml_template)` | Method | Initializes the message box from the named XML template. Arguments `xml_template` (`string`). | [UIMessageBox_script.cpp:22](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMessageBox_script.cpp#L22) |

### `CUIMMShniaga`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIMMShniaga:GetPage()` | Method | Returns the current main-menu page ID. | [UIMMShniaga.cpp:476](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMMShniaga.cpp#L476) |

### `CUIPdaWnd`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIPdaWnd:GetActiveDialog()` | Method | Returns the active dialog. Result `CUIWindow`. | [UIActorMenu_script.cpp:426](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L426) |
| `CUIPdaWnd:GetActiveSection()` | Method | Returns the active section. Result `string`. Implementation `return m_sActiveSection.c_str();`. | [UIActorMenu_script.cpp:427](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L427) |
| `CUIPdaWnd:GetTabControl()` | Method | Returns the tab control. Result `CUITabControl`. | [UIActorMenu_script.cpp:428](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L428) |
| `CUIPdaWnd:SetActiveDialog(pUI)` | Method | Sets the active dialog. Arguments `pUI` (`CUIWindow`). Implementation `m_pActiveDialog = pUI;`. | [UIActorMenu_script.cpp:425](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L425) |
| `CUIPdaWnd:SetActiveSubdialog(section)` | Method | Sets the active subdialog. Arguments `section` (`string`). Implementation `SetActiveSubdialog((const shared_str&)section);`. | [UIActorMenu_script.cpp:424](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L424) |
| `CUIPdaWnd:SwitchZoom()` | Method | Switches the zoom. | [UIActorMenu_script.cpp:429](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L429) |

### `CUIProgressBar`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIProgressBar:GetProgressStatic()` | Method | Returns the static progress fill widget. Result `CUIStatic`. | [UIProgressBar_script.cpp:25](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L25) |
| `CUIProgressBar:SetColor(color)` | Method | Sets the color. Arguments `color` (`number`). Implementation `m_UIProgressItem.SetTextureColor(color);`. | [UIProgressBar_script.cpp:20](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L20) |
| `CUIProgressBar:SetMaxColor(color)` | Method | Sets the max color. Arguments `color` (`number`). Implementation `m_maxColor.set(color);`. | [UIProgressBar_script.cpp:24](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L24) |
| `CUIProgressBar:SetMiddleColor(color)` | Method | Sets the middle color. Arguments `color` (`number`). Implementation `m_middleColor.set(color);`. | [UIProgressBar_script.cpp:23](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L23) |
| `CUIProgressBar:SetMinColor(color)` | Method | Sets the min color. Arguments `color` (`number`). Implementation `m_minColor.set(color);`. | [UIProgressBar_script.cpp:22](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L22) |
| `CUIProgressBar:ShowBackground(enabled)` | Method | Shows the bar background with `true` and hides it with `false`. | [UIProgressBar_script.cpp:19](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L19) |
| `CUIProgressBar:SnapProgressPos()` | Method | Immediately sets the displayed fill to the last position supplied to `SetProgressPos`. | [UIProgressBar_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L28) |
| `CUIProgressBar:UseColor(status)` | Method | Toggles value-based coloring of the progress fill. Arguments `status` (`boolean`). Implementation `m_bUseColor = status;`. | [UIProgressBar_script.cpp:21](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIProgressBar_script.cpp#L21) |

### `CUIPropertiesBox`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIPropertiesBox:GetSelectedItem()` | Method | Returns the selected list item. Result `CUIListBoxItem`. Implementation `return m_UIListWnd.GetSelectedItem();`. | [UIPropertiesBox_script.cpp:21](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIPropertiesBox_script.cpp#L21) |
| `CUIPropertiesBox:InitPropertiesBox(pos, size)` | Method | Initializes the properties box. Arguments `pos` (`vector2`), `size` (`vector2`). | [UIPropertiesBox_script.cpp:24](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIPropertiesBox_script.cpp#L24) |

### `CUIScriptWnd`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIScriptWnd:GetCheck(name)` | Method | Looks up a registered control by name and returns a `CUICheckButton` when the type matches. | [UIScriptWnd_script.cpp:62](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L62) |
| `CUIScriptWnd:GetComboBox(name)` | Method | Looks up a registered control by name and returns a `CUIComboBox` when the type matches. | [UIScriptWnd_script.cpp:61](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L61) |
| `CUIScriptWnd:GetDialogWnd(name)` | Method | Looks up a registered control by name and returns a `CUIDialogWnd` when the type matches. | [UIScriptWnd_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L54) |
| `CUIScriptWnd:GetEditBox(name)` | Method | Looks up a registered control by name and returns a `CUIEditBox` when the type matches. | [UIScriptWnd_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L53) |
| `CUIScriptWnd:GetFrameLineWnd(name)` | Method | Looks up a registered control by name and returns a `CUIFrameLineWnd` when the type matches. | [UIScriptWnd_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L56) |
| `CUIScriptWnd:GetFrameWindow(name)` | Method | Looks up a registered control by name and returns a `CUIFrameWindow` when the type matches. | [UIScriptWnd_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L55) |
| `CUIScriptWnd:GetListWnd(name)` | Method | Looks up a registered control by name and returns a `CUIListWnd` when the type matches. | [UIScriptWnd_script.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L59) |
| `CUIScriptWnd:GetProgressBar(name)` | Method | Looks up a registered control by name and returns a `CUIProgressBar` when the type matches. | [UIScriptWnd_script.cpp:57](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L57) |
| `CUIScriptWnd:GetStatic(name)` | Method | Looks up a registered control by name and returns a `CUIStatic` when the type matches. | [UIScriptWnd_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L52) |
| `CUIScriptWnd:GetTabControl(name)` | Method | Looks up a registered control by name and returns a `CUITabControl` when the type matches. | [UIScriptWnd_script.cpp:58](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L58) |
| `CUIScriptWnd:GetTrackBar(name)` | Method | Looks up a registered control by name and returns a `CUITrackBar` when the type matches. | [UIScriptWnd_script.cpp:60](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIScriptWnd_script.cpp#L60) |
| `CUIScriptWnd:OnGamepadKey(key, action)` | Method | Handles a gamepad button. `key` is a `gamepad_keys` code and `action` is the UI press or release event. Return `true` when the window handles the event. | [uiscriptwnd_script2.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L29) |
| `CUIScriptWnd:OnGamepadKeyHold(key)` | Method | Handles a held gamepad button. Return `true` when the window handles the event. | [uiscriptwnd_script2.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L30) |
| `CUIScriptWnd:OnGamepadStick(key, value, action)` | Method | Handles a gamepad stick. `value` is a two-component vector and `action` is the UI event code. Return `true` when the window handles the event. | [uiscriptwnd_script2.cpp:31](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L31) |
| `CUIScriptWnd:OnGyroscope(value)` | Method | Handles gyroscope data supplied as a three-component vector. Return `true` when the window handles the event. | [uiscriptwnd_script2.cpp:32](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L32) |
| `CUIScriptWnd:OnKeyboardHold(key)` | Method | Handles a held keyboard key by input code. Return `true` when the window handles the event. | [uiscriptwnd_script2.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L28) |
| `CUIScriptWnd:OnTouchpad(value)` | Method | Handles touchpad data supplied as a two-component vector. Return `true` when the window handles the event. | [uiscriptwnd_script2.cpp:33](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/uiscriptwnd_script2.cpp#L33) |

### `CUIScrollBar`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIScrollBar.fixed` | Constant | `CUIScrollBar` enumeration value for fixed. `1`. | [UIScrollBar_script.cpp:60](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L60) |
| `CUIScrollBar.stretch` | Constant | `CUIScrollBar` enumeration value for stretch. `0`. | [UIScrollBar_script.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L59) |
| `CUIScrollBar:GetEnabled()` | Method | Returns the enabled. Result `boolean`. | [UIScrollBar_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L56) |
| `CUIScrollBar:GetMaxRange()` | Method | Returns the max range. Result `number`. | [UIScrollBar_script.cpp:43](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L43) |
| `CUIScrollBar:GetMinRange()` | Method | Returns the min range. Result `number`. | [UIScrollBar_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L42) |
| `CUIScrollBar:GetPageSize()` | Method | Returns the page size. Result `number`. | [UIScrollBar_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L47) |
| `CUIScrollBar:GetScrollPos()` | Method | Returns the scroll position. Result `number`. Implementation `return std::max(_minPos, _scrollPos);`. | [UIScrollBar_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L45) |
| `CUIScrollBar:GetStepSize()` | Method | Returns the step size. Result `number`. | [UIScrollBar_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L49) |
| `CUIScrollBar:InitScrollBarFixed(x, y, isHorizontal, profile)` | Method | Initializes the scroll bar fixed. Arguments `x` (`number`), `y` (`number`), `isHorizontal` (`boolean`), `profile` (`string`). Result `boolean`. Implementation `const Fvector2 pos = Fvector2().set(x, y); return self->InitScrollBar(pos, isHorizontal, profile);`. | [UIScrollBar_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L40) |
| `CUIScrollBar:InitScrollBarStretch(x, y, length, isHorizontal, profile)` | Method | Initializes the scroll bar stretch. Arguments `x` (`number`), `y` (`number`), `length` (`number`), `isHorizontal` (`boolean`), `profile` (`string`). Result `boolean`. Implementation `const Fvector2 pos = Fvector2().set(x, y); return self->InitScrollBar(pos, length, isHorizontal, profile);`. | [UIScrollBar_script.cpp:39](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L39) |
| `CUIScrollBar:IsFixedLayout()` | Method | Checks whether the scrollbar has fixed layout. Result `boolean`. Implementation `return _layoutMode == ScrollLayoutMode::Fixed;`. | [UIScrollBar_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L53) |
| `CUIScrollBar:IsInitialized()` | Method | Checks whether scrollbar initialization is complete. Result `boolean`. | [UIScrollBar_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L54) |
| `CUIScrollBar:Refresh()` | Method | Refreshes the scrollbar after its scroll state changes. Implementation `SyncThumbFromScrollPos();`. | [UIScrollBar_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L52) |
| `CUIScrollBar:SetEnabled(enabled)` | Method | Sets the enabled. Arguments `enabled` (`boolean`). Implementation `_enabled = enabled; if (!_enabled) { Show(_enabled); }`. | [UIScrollBar_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L55) |
| `CUIScrollBar:SetPageSize(page)` | Method | Sets the page size. Arguments `page` (`number`). Implementation `_pageSize = std::max(0, page); UpdateScrollBar();`. | [UIScrollBar_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L46) |
| `CUIScrollBar:SetRange(minPos, maxPos)` | Method | Sets the range. Arguments `minPos` (`number`), `maxPos` (`number`). | [UIScrollBar_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L41) |
| `CUIScrollBar:SetScrollPos(pos)` | Method | Sets the scroll position. Arguments `pos` (`number`). Implementation `SetScrollPosClamped(pos); UpdateScrollBar();`. | [UIScrollBar_script.cpp:44](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L44) |
| `CUIScrollBar:SetStepSize(step)` | Method | Sets the step size. Arguments `step` (`number`). Implementation `_stepSize = step; UpdateScrollBar();`. | [UIScrollBar_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L48) |
| `CUIScrollBar:TryScrollDec(byScrollbox)` | Method | Attempts to decrement the scroll position. The argument selects page or step scrolling. Arguments `byScrollbox` (`boolean`). Implementation `if (ScrollDec(byScrollbox)) { NotifyScrollChanged(); }`. | [UIScrollBar_script.cpp:51](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L51) |
| `CUIScrollBar:TryScrollInc(byScrollbox)` | Method | Attempts to increment the scroll position. The argument selects page or step scrolling. Arguments `byScrollbox` (`boolean`). Implementation `if (ScrollInc(byScrollbox)) { NotifyScrollChanged(); }`. | [UIScrollBar_script.cpp:50](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIScrollBar_script.cpp#L50) |

### `CUIScrollView`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIScrollView:GetPadSize()` | Method | Returns the scroll content size. Result `vector2`. Implementation `if(m_flags.test (eNeedRecalc) ) RecalcSize (); return m_pad->GetWndSize();`. | [UIWindow_script.cpp:258](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L258) |
| `CUIScrollView:HasWindow(item)` | Method | Returns `true` when the window belongs to the scrollable contents. | [UIWindow_script.cpp:259](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L259) |
| `CUIScrollView:ReinitScrollBar()` | Method | Reinitializes the scrollbar. Result `boolean`. | [UIWindow_script.cpp:256](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L256) |
| `CUIScrollView:ScrollBar()` | Method | Scrolls the bar. Result `CUIScrollBar`. | [UIWindow_script.cpp:255](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L255) |
| `CUIScrollView:ScrollToItem(item, offset)` | Method | Scrolls to a child window with an integer vertical offset. | [UIWindow_script.cpp:260](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L260) |
| `CUIScrollView:SetFixedScrollBar(b)` | Method | Sets the fixed scroll bar. Arguments `b` (`boolean`). Implementation `m_flags.set(eFixedScrollBar, b);`. | [UIWindow_script.cpp:253](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L253) |
| `CUIScrollView:SetScrollBarProfile(profile)` | Method | Sets the scrollbar profile. Arguments `profile` (`string`). Implementation `m_scrollbar_profile = profile;`. | [UIWindow_script.cpp:254](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L254) |

### `CUIStackPanel`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIStackPanel:IsAlignRight()` | Method | Checks whether the panel aligns its items to the right. Result `boolean`. Implementation `return SPAlignment == eRight;`. | [UIWindow_script.cpp:230](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L230) |
| `CUIStackPanel:SetRightAlign(Val)` | Method | Sets the right align. Arguments `Val` (`boolean`). Implementation `SPAlignment = Val ? eRight : eLeft;`. | [UIWindow_script.cpp:229](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L229) |

### `CUIStatic`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIStatic:AdjustHeightToText()` | Method | Adjusts the height to text. | [UIStatic_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L28) |
| `CUIStatic:AdjustWidthToText()` | Method | Adjusts the width to text. | [UIStatic_script.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L29) |
| `CUIStatic:GetColor()` | Method | Returns the texture color. Result `number`. Implementation `return m_UIStaticItem.GetTextureColor();`. | [UIStatic_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L46) |
| `CUIStatic:GetFont()` | Method | Returns the widget font. Result `CGameFont`. Implementation `return TextItemControl()->GetFont();`. | [UIStatic_script.cpp:80](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L80) |
| `CUIStatic:GetOriginalRect()` | Method | Returns the texture rect. Result `Frect`. Implementation `return &m_UIStaticItem.GetTextureRect();`. | [UIStatic_script.cpp:64](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L64) |
| `CUIStatic:GetStretchTexture()` | Method | Returns the stretch texture. Result `boolean`. | [UIStatic_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L30), [UIStatic_script.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L67) |
| `CUIStatic:GetText()` | Method | Returns the text. Result `string`. Implementation `return TextItemControl()->GetText();`. | [UIStatic_script.cpp:36](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L36) |
| `CUIStatic:GetTextAlign()` | Method | Returns the text alignment. Result `number`. Implementation `return static_cast<u32>(TextItemControl()->GetTextAlignment());`. | [UIStatic_script.cpp:70](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L70) |
| `CUIStatic:GetTextColor()` | Method | Returns the text color. Result `number`. Implementation `return TextItemControl()->GetTextColor();`. | [UIStatic_script.cpp:50](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L50) |
| `CUIStatic:GetTextureColor()` | Method | Returns the texture color. Result `number`. Implementation `return m_UIStaticItem.GetTextureColor();`. | [UIStatic_script.cpp:27](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L27) |
| `CUIStatic:GetTextX()` | Method | Returns the text x. Result `number`. Implementation `return TextItemControl()->m_TextOffset.x;`. | [UIStatic_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L42) |
| `CUIStatic:GetTextY()` | Method | Returns the text y. Result `number`. Implementation `return TextItemControl()->m_TextOffset.y;`. | [UIStatic_script.cpp:43](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L43) |
| `CUIStatic:InitTextureEx(texture, shader, fatal)`, `CUIStatic:InitTextureEx(texture, shader)` | Method | Initializes the texture ex. Result `boolean`. | [UIStatic_script.cpp:54](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L54), [UIStatic_script.cpp:55](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L55) |
| `CUIStatic:ResetOriginalRect()` | Method | Resets the original rect. Implementation `m_UIStaticItem.ResetOriginalRect();`. | [UIStatic_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L56) |
| `CUIStatic:SetColor(color)` | Method | Sets the texture color. Arguments `color` (`number`). Implementation `m_UIStaticItem.SetTextureColor(color);`. | [UIStatic_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L45) |
| `CUIStatic:SetElipsis(pos, indent)` | Method | Sets the elipsis. Arguments `pos` (`number`), `indent` (`number`). Implementation `TextItemControl()->SetEllipsis(pos != 0);`. | [UIStatic_script.cpp:75](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L75) |
| `CUIStatic:SetEllipsis(mode)` | Method | Sets the ellipsis. Arguments `mode` (`boolean`). Implementation `TextItemControl()->SetEllipsis(mode);`. | [UIStatic_script.cpp:76](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L76) |
| `CUIStatic:SetOriginalRect(pr)` | Method | Sets the original rect. Arguments `pr` (`Frect`). Implementation `m_UIStaticItem.SetTextureRect(*pr);`. | [UIStatic_script.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L63) |
| `CUIStatic:SetTextAlign(align)` | Method | Sets the text alignment. Arguments `align` (`number`). | [UIStatic_script.cpp:69](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L69) |
| `CUIStatic:SetTextAlignment(al)` | Method | Sets the text alignment. Arguments `al` (`ETextAlignment`). Implementation `TextItemControl()->SetTextAlignment(al);`. | [UIStatic_script.cpp:77](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L77) |
| `CUIStatic:SetTextComplexMode(mode)` | Method | Sets the multiline text layout mode. Arguments `mode` (`boolean`). Implementation `TextItemControl()->SetTextComplexMode(mode);`. | [UIStatic_script.cpp:79](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L79) |
| `CUIStatic:SetTextOffset(x, y)` | Method | Sets the text offset. Arguments `x` (`number`), `y` (`number`). Implementation `TextItemControl()->m_TextOffset.x = x; TextItemControl()->m_TextOffset.y = y;`. | [UIStatic_script.cpp:81](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L81) |
| `CUIStatic:SetTextST(txt)` | Method | Sets the text st. Arguments `txt` (`string`). Implementation `TextItemControl()->SetTextST(txt);`. | [UIStatic_script.cpp:34](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L34) |
| `CUIStatic:SetTextureOffset(x, y)` | Method | Sets the texture offset. Arguments `x` (`number`), `y` (`number`). Implementation `m_TextureOffset.set(x, y);`. | [UIStatic_script.cpp:58](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L58) |
| `CUIStatic:SetTextX(x)` | Method | Sets the text x. Arguments `x` (`number`). Implementation `TextItemControl()->m_TextOffset.x = x;`. | [UIStatic_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L40) |
| `CUIStatic:SetTextY(y)` | Method | Sets the text y. Arguments `y` (`number`). Implementation `TextItemControl()->m_TextOffset.y = y;`. | [UIStatic_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L41) |
| `CUIStatic:SetVTextAlignment(al)` | Method | Sets the vertical text alignment. Arguments `al` (`EVTextAlignment`). Implementation `TextItemControl()->SetVTextAlignment(al);`. | [UIStatic_script.cpp:78](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIStatic_script.cpp#L78) |

### `CUITabControl`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUITabControl:GetActiveIndex()` | Method | Returns the active index. Result `number`. | [UITabControl_script.cpp:23](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L23) |
| `CUITabControl:GetButtonByIndex(index)` | Method | Returns the button by index. Arguments `index` (`number`). Result `CUITabButton`. | [UITabControl_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L28) |
| `CUITabControl:NextTab(bAllowLoop)` | Method | Advances to the next value. Arguments `bAllowLoop` (`boolean`). | [UITabControl_script.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L29) |
| `CUITabControl:PrevTab(bAllowLoop)` | Method | Moves to the previous value. Arguments `bAllowLoop` (`boolean`). | [UITabControl_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L30) |
| `CUITabControl:RemoveItem(index)`, `CUITabControl:RemoveItem(id)` | Method | Removes the item. | [UITabControl_script.cpp:19](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L19), [UITabControl_script.cpp:20](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L20) |
| `CUITabControl:SetNewActiveTab(index)` | Method | Sets the new active tab. Arguments `index` (`number`). | [UITabControl_script.cpp:26](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UITabControl_script.cpp#L26) |

### `CUITrackBar`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUITrackBar:AssignProps(entry, group)` | Method | Assigns the props. Arguments `entry` (`string`), `group` (`string`). Implementation `track->AssignProps(entry, group);`. | [UIButton_script.cpp:93](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L93) |
| `CUITrackBar:CurrentID()` | Method | Returns the current value ID. Result `number`. Implementation `return (m_i_val - 1);`. | [UIButton_script.cpp:91](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L91) |
| `CUITrackBar:GetInvert()` | Method | Returns the invert. Result `boolean`. | [UIButton_script.cpp:97](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L97) |
| `CUITrackBar:SetCurrentID(val_id)` | Method | Sets the current value ID. Arguments `val_id` (`number`). Implementation `m_i_val = val_id + 1;`. | [UIButton_script.cpp:92](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L92) |
| `CUITrackBar:SetDrawingValue(enabled)` | Method | Controls whether the current value is drawn next to the slider. | [UIButton_script.cpp:99](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L99) |
| `CUITrackBar:SetFValue(val)` | Method | Sets the floating-point value. Arguments `val` (`number`). Implementation `m_f_val = val; UpdatePos();`. | [UIButton_script.cpp:87](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L87) |
| `CUITrackBar:SetHighlighted(value)` | Method | Sets the highlighted. Arguments `value` (`boolean`). Implementation `m_pSlider->SetHighlighted(value);`. | [UIButton_script.cpp:103](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L103) |
| `CUITrackBar:SetInvert(v)` | Method | Sets the invert. Arguments `v` (`boolean`). Implementation `m_b_invert=v;`. | [UIButton_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L98) |
| `CUITrackBar:SetIValue(val)` | Method | Sets the integer value. Arguments `val` (`number`). Implementation `m_i_val = val; UpdatePos();`. | [UIButton_script.cpp:85](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L85) |
| `CUITrackBar:SetMagnitude(magnitude)` | Method | Sets the magnitude. Arguments `magnitude` (`number`). Implementation `m_f_magnitude = magnitude;`. | [UIButton_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L95) |
| `CUITrackBar:SetNumOfSigns(num_of_signs)` | Method | Sets the displayed decimal places. Arguments `num_of_signs` (`number`). Implementation `m_i_num_of_signs = num_of_signs;`. | [UIButton_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L96) |
| `CUITrackBar:SetStep(step)` | Method | Sets the step. Arguments `step` (`number`). Implementation `if (IsFltMode()) m_f_step = step; else m_i_step = iFloor(step);`. | [UIButton_script.cpp:94](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L94) |
| `CUITrackBar:SetSystemDepends(depend)` | Method | Sets the system depends. Arguments `depend` (`number`). Implementation `track->SetSystemDepends((CUIOptionsItem::ESystemDepends)depend);`. | [UIButton_script.cpp:100](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L100) |
| `CUITrackBar:StepLeft()` | Method | Moves the slider one step to the left. | [UIButton_script.cpp:101](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L101) |
| `CUITrackBar:StepRight()` | Method | Moves the slider one step to the right. | [UIButton_script.cpp:102](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIButton_script.cpp#L102) |

### `CUIWindow`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIWindow:FindStatic(name)` | Method | Searches this node and its descendants by name. Returns a `CUIStatic`, or `nil` if missing or of another type. | [UIWindow_script.cpp:196](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L196) |
| `CUIWindow:FindWindow(name)` | Method | Searches this node and its descendants by name. Returns a `CUIWindow` or `nil`. | [UIWindow_script.cpp:195](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L195) |
| `CUIWindow:FocusReceiveTime()` | Method | Returns the focus receive time. Result `number`. | [UIWindow_script.cpp:147](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L147) |
| `CUIWindow:GetAbsoluteRect(out_rect)` | Method | Writes the window’s absolute bounds into the supplied `Frect`. Returns no value. | [UIWindow_script.cpp:148](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L148) |
| `CUIWindow:GetFont()` | Method | Returns the widget font. Result `CGameFont`. Implementation `if(m_pFont) return m_pFont; if(m_pParentWnd== NULL) return m_pFont; else return m_pParentWnd->GetFont();`. | [UIWindow_script.cpp:188](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L188) |
| `CUIWindow:Init(x, y, width, height)`, `CUIWindow:Init(rect)` | Method | Initializes window coordinates and size. An overload takes a rectangle. Implementation `self->SetWndPos(Fvector2().set(x, y)); self->SetWndSize(Fvector2().set(width, height));`. | [UIWindow_script.cpp:150](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L150), [UIWindow_script.cpp:155](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L155) |
| `CUIWindow:SetHeight(height)` | Method | Sets the window’s height in UI coordinates. | [UIWindow_script.cpp:181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L181) |
| `CUIWindow:SetWidth(width)` | Method | Sets the window’s width in UI coordinates. | [UIWindow_script.cpp:179](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L179) |

### `CUIZoneMap`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `CUIZoneMap.disabled` | Read/write field | HUD component or setting for disabled. Readable and writable. | [UIActorMenu_script.cpp:460](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L460) |
| `CUIZoneMap:Background()` | Method | Returns the background. Result `CUIStatic`. | [UIActorMenu_script.cpp:463](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L463) |
| `CUIZoneMap:MapFrame()` | Method | Returns the map frame. Result `CUIWindow`. | [UIActorMenu_script.cpp:462](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L462) |

### `EDDListType`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `EDDListType.iActorBag` | Constant | Drag-and-drop list type `iActorBag` in the inventory UI. | [UIActorMenu_script.cpp:382](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L382) |
| `EDDListType.iActorBelt` | Constant | Drag-and-drop list type `iActorBelt` in the inventory UI. | [UIActorMenu_script.cpp:383](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L383) |
| `EDDListType.iActorSlot` | Constant | Drag-and-drop list type `iActorSlot` in the inventory UI. | [UIActorMenu_script.cpp:384](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L384) |
| `EDDListType.iActorTrade` | Constant | Drag-and-drop list type `iActorTrade` in the inventory UI. | [UIActorMenu_script.cpp:385](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L385) |
| `EDDListType.iDeadBodyBag` | Constant | Drag-and-drop list type `iDeadBodyBag` in the inventory UI. | [UIActorMenu_script.cpp:386](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L386) |
| `EDDListType.iInvalid` | Constant | Drag-and-drop list type `iInvalid` in the inventory UI. | [UIActorMenu_script.cpp:387](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L387) |
| `EDDListType.iPartnerTrade` | Constant | Drag-and-drop list type `iPartnerTrade` in the inventory UI. | [UIActorMenu_script.cpp:388](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L388) |
| `EDDListType.iPartnerTradeBag` | Constant | Drag-and-drop list type `iPartnerTradeBag` in the inventory UI. | [UIActorMenu_script.cpp:389](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L389) |
| `EDDListType.iQuickSlot` | Constant | Drag-and-drop list type `iQuickSlot` in the inventory UI. | [UIActorMenu_script.cpp:390](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L390) |
| `EDDListType.iTrashSlot` | Constant | Drag-and-drop list type `iTrashSlot` in the inventory UI. | [UIActorMenu_script.cpp:391](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIActorMenu_script.cpp#L391) |

### `FactionState`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `FactionState.actor_goodwill` | Property | Property on `FactionState`. Returns the actor goodwill. Result `number`. | [FactionState.cpp:72](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L72) |
| `FactionState.bonus` | Read/write field | Stores the bonus. Readable and writable. | [FactionState.cpp:69](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L69) |
| `FactionState.faction_id` | Property | Property on `FactionState`. Returns the faction id. Result `string`. | [FactionState.cpp:71](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L71) |
| `FactionState.icon_big` | Property | Property on `FactionState`. Returns the icon big. Result `string`. | [FactionState.cpp:76](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L76) |
| `FactionState.location` | Property | Property on `FactionState`. Returns the location. Result `string`. | [FactionState.cpp:79](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L79) |
| `FactionState.member_count` | Read/write field | Stores the member count. Readable and writable. | [FactionState.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L66) |
| `FactionState.power` | Read/write field | Stores the power. Readable and writable. | [FactionState.cpp:68](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L68) |
| `FactionState.resource` | Read/write field | Stores the resource. Readable and writable. | [FactionState.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L67) |
| `FactionState.target_desc` | Property | Property on `FactionState`. Returns the target desc. Result `string`. | [FactionState.cpp:78](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L78) |
| `FactionState.war_state1` | Property | Property on `FactionState`. Returns the war state1. Result `string`. | [FactionState.cpp:81](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L81) |
| `FactionState.war_state2` | Property | Property on `FactionState`. Returns the war state2. Result `string`. | [FactionState.cpp:82](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L82) |
| `FactionState.war_state3` | Property | Property on `FactionState`. Returns the war state3. Result `string`. | [FactionState.cpp:83](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L83) |
| `FactionState.war_state4` | Property | Property on `FactionState`. Returns the war state4. Result `string`. | [FactionState.cpp:84](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L84) |
| `FactionState.war_state5` | Property | Property on `FactionState`. Returns the war state5. Result `string`. | [FactionState.cpp:85](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L85) |
| `FactionState.war_state_hint1` | Property | Property on `FactionState`. Returns the war state hint1. Result `string`. | [FactionState.cpp:87](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L87) |
| `FactionState.war_state_hint2` | Property | Property on `FactionState`. Returns the war state hint2. Result `string`. | [FactionState.cpp:88](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L88) |
| `FactionState.war_state_hint3` | Property | Property on `FactionState`. Returns the war state hint3. Result `string`. | [FactionState.cpp:89](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L89) |
| `FactionState.war_state_hint4` | Property | Property on `FactionState`. Returns the war state hint4. Result `string`. | [FactionState.cpp:90](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L90) |
| `FactionState.war_state_hint5` | Property | Property on `FactionState`. Returns the war state hint5. Result `string`. | [FactionState.cpp:91](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/FactionState.cpp#L91) |

### `GAME_TYPE`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `GAME_TYPE.eGameIDFreeMp` | Constant | `GAME_TYPE` enumeration value for game id free mp. `eGameIDFreeMP`. | [UIMapList.cpp:553](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMapList.cpp#L553) |
| `GAME_TYPE.GAME_ARTEFACTHUNT` | Constant | `GAME_TYPE` enumeration value for game artefacthunt. `eGameIDArtefactHunt`. | [UIMapList.cpp:551](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMapList.cpp#L551) |
| `GAME_TYPE.GAME_DEATHMATCH` | Constant | `GAME_TYPE` enumeration value for game deathmatch. `eGameIDDeathmatch`. | [UIMapList.cpp:549](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMapList.cpp#L549) |
| `GAME_TYPE.GAME_TEAMDEATHMATCH` | Constant | `GAME_TYPE` enumeration value for game teamdeathmatch. `eGameIDTeamDeathmatch`. | [UIMapList.cpp:550](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/UIMapList.cpp#L550) |

### `main_menu`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `main_menu.in_controller_mode()` | Function | Returns the in controller mode. Result `boolean`. Implementation `return pInput->GetControllerMode();`. | [ui_export_script.cpp:122](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L122) |

### `system_depends`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `system_depends.sdApplyOnChange` | Constant | Option application mode `sdApplyOnChange` for a UI control. | [UIOptionsManagerScript.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L66) |
| `system_depends.sdNothing` | Constant | Option application mode `sdNothing` for a UI control. | [UIOptionsManagerScript.cpp:62](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L62) |
| `system_depends.sdSndRestart` | Constant | Option application mode `sdSndRestart` for a UI control. | [UIOptionsManagerScript.cpp:65](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L65) |
| `system_depends.sdSystemRestart` | Constant | Option application mode `sdSystemRestart` for a UI control. | [UIOptionsManagerScript.cpp:64](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L64) |
| `system_depends.sdVidRestart` | Constant | Option application mode `sdVidRestart` for a UI control. | [UIOptionsManagerScript.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIOptionsManagerScript.cpp#L63) |

### `TEX_INFO`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `TEX_INFO:get_file_name()` | Method | Returns the file name. Result `string`. | [UIWindow_script.cpp:132](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L132) |
| `TEX_INFO:get_rect()` | Method | Returns the rect. Result `Frect`. | [UIWindow_script.cpp:133](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L133) |

### `ui`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `ui.gamepad_prefix()` | Function | Returns the gamepad prefix. Result `string`. Implementation `return pInput->GamepadPrefix();`. | [ui_export_script.cpp:128](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L128) |
| `ui.get_current_kx()` | Function | Returns the current kx. Result `number`. Implementation `return UI().get_current_kx();`. | [ui_export_script.cpp:126](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L126) |
| `ui.is_widescreen()` | Function | Checks whether the UI uses its widescreen layout. Result `boolean`. Implementation `return UI().is_widescreen();`. | [ui_export_script.cpp:127](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L127) |
| `ui.QueryScrollBarProfileLayout(profile, isHorizontal)` | Function | Queries the scroll bar profile layout. Arguments `profile` (`string`), `isHorizontal` (`boolean`). Result `number`. | [ui_export_script.cpp:129](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/ui/ui_export_script.cpp#L129) |

### `ui_events`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `ui_events.LIST_ITEM_UNSELECT` | Constant | UI widget event code for list item unselect. | [UIWindow_script.cpp:301](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L301) |
| `ui_events.TRACK_VALUE_CHANGED` | Constant | UI widget event code for track value changed. | [UIWindow_script.cpp:319](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L319) |

### `UIHint`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `UIHint:GetHintText()` | Method | Returns the hint text. Result `string`. Implementation `return m_text->GetText();`. | [UIWindow_script.cpp:241](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L241) |
| `UIHint:SetHeight(height)` | Method | Sets the window’s height in UI coordinates. | [UIWindow_script.cpp:239](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L239) |
| `UIHint:SetHintText(text)` | Method | Sets the hint text. Arguments `text` (`string`). | [UIWindow_script.cpp:240](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L240) |
| `UIHint:SetWidth(width)` | Method | Sets the window’s width in UI coordinates. | [UIWindow_script.cpp:238](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrUI/Widgets/UIWindow_script.cpp#L238) |

## See also

- [New Lua exports](../lua-exports.md)
