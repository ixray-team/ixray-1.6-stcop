# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

## IXR OPTIONS Module

A system for centralized publication of your script's settings in a custom settings menu in the main menu:
* Uses a configuration store independent from game saves, alongside game options.
* Synergizes with the callback system and publishes an event so any script participating in autoload can publish its settings to the shared settings area.
* Structured, sectioned options storage.
* Supports setting descriptions and choosing the appropriate control type for UI.
* The script that published the option is personally notified when the user changes it.
* Default values are supported.
* Settings are loaded before the main autoload, ensuring correct initialization order for all dependent scripts using options.
* Options are strictly isolated by the script instance that created them. Multiple scripts can have identically named options without conflict; both will appear in the main menu.

```lua
--// Check if a variable exists in options
HasOptionsVar(var_name)
args:
  var_name (string)(required) - key to search for (unique within the script's namespace).
retval: (bool) - does the value exist for the key.

--// Get a variable from options
GetOptionsVar(var_name, default_value)
args:
  var_name (string)(required) - key to get value for.
  default_value (mixed)(optional) - default value if key not found.
retval: (nil|bool|int|float|string|table|function) - value for key or default_value.

--// Register an option (available only within the closure called by "on_init_ixr_options")
_ref.register_option(group_name, variable_name, display_text, controll_data, _callback)
args:
  group_name (string|nil)(required) - group options by name, nil for no group.
  variable_name (string)(required) - system name, unique within the script.
  display_text (string)(required) - description text for the option.
  controll_data (table)(required) - table defining the control to render in the options menu (see wrappers below).
  _callback (callable)(required) - function called on change/load.
retval: (void)
```

Control variations for options:
```lua
--// Create TrackBar control (slider)
_ref.track_bar(default_value, min_value, max_value, value_offset)
args:
  default_value (number)(required) - initial value.
  min_value (number)(required) - minimum value.
  max_value (number)(required) - maximum value.
  value_offset (number)(required) - step size.
retval: (table) - control data for _ref.register_option.

--// Create NumericUpDown control
_ref.numeric_up_down(default_value, min_value, max_value, value_offset)
args:
  default_value (number)(required) - initial value.
  min_value (number)(required) - minimum value.
  max_value (number)(required) - maximum value.
  value_offset (number)(required) - step size.
retval: (table) - control data for _ref.register_option.

--// Create ComboBox control (dropdown)
_ref.combo_box(default_selected_index, list_values_array)
args:
  default_selected_index (number)(required) - default selected index (starting from 1).
  list_values_array (table)(required) - array of string values for the list.
retval: (table) - control data for _ref.register_option.

--// Create TextBox control
_ref.text_box(def_value)
args:
  def_value (string)(required) - default value.
retval: (table) - control data for _ref.register_option.

--// Create CheckBox control
_ref.check_box(def_value)
args:
  def_value (boolean)(required) - default value.
retval: (table) - control data for _ref.register_option.
```

Examples:
```lua
--// Automatically called by the ixr autoloader
function on_game_start()
  --// Subscribe to the options registration event
  RegisterScriptCallback("on_init_ixr_options", function (_ref)
    --// Create a TextBox
    _ref.register_option(nil, "gg_name", "TextBox example", _ref.text_box("Marked One"), this.on_change_value)

    --// Create a TrackBar
    _ref.register_option(nil, "float_track_bar", "TrackBar example", _ref.track_bar(23.0, 0.0, 100.0, 1.0), this.on_change_value)

    --// Create a NumericUpDown
    _ref.register_option(nil, "float_picker", "NumericUpDown example", _ref.numeric_up_down(0.0, -1.0, 1.0, 0.25), this.on_change_value)

    --// Create a ComboBox
    _ref.register_option(nil, "color_picker", "ComboBox example", _ref.combo_box(1, {
      { ["option_display_name"] = "Not set",   ["option_return_value"] = {255, 255, 255}},
      { ["option_display_name"] = "Red",       ["option_return_value"] = {255, 0, 0}},
      { ["option_display_name"] = "Green",     ["option_return_value"] = {0, 255, 0}},
      { ["option_display_name"] = "Blue",      ["option_return_value"] = {0, 0, 255}},
    }), this.on_change_value)

    --// Create a CheckBox
    _ref.register_option(nil, "var_4", "CheckBox 4", _ref.check_box(false), this.on_change_value)

    --// Group several checkboxes
    _ref.register_option("Group 1", "var_5", "CheckBox 5", _ref.check_box(false), this.on_change_value)
    _ref.register_option("Group 1", "var_6", "CheckBox 6", _ref.check_box(true), this.on_change_value)
    _ref.register_option("Group 1", "var_7", "CheckBox 7", _ref.check_box(true), this.on_change_value)
    _ref.register_option("Group 1", "var_8", "CheckBox 8", _ref.check_box(false), this.on_change_value)

    _ref.register_option("Group 2", "var_9", "CheckBox 9", _ref.check_box(true), this.on_change_value)
    _ref.register_option("Group 2", "var_10", "CheckBox 10", _ref.check_box(true), this.on_change_value)
    _ref.register_option("Group 3", "var_11", "CheckBox 11", _ref.check_box(false), this.on_change_value)
  end)
end

--// Callback for option changes in this script file
function on_change_value(group, name, value)
  --// print to log
  SemiLog(tostring(script_name()) .. ".script [option change]: group:" .. tostring(group) .." name:".. tostring(name) .. " value:" .. tostring(ffx_json_lib.json_encode(value)))
end

--// Check if a variable exists in options
if HasOptionsVar("my-option") then
  ...
end

--// Get a variable from options
local option_value = GetOptionsVar("my-option", nil)
```
