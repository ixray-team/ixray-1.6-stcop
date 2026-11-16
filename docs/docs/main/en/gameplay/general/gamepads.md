# Gamepads

> [!IMPORTANT]
> **Scope**: Single-player <br>
> **Minimum version**: 1.4

## Lua vibration and adaptive triggers

These functions are global. All durations are in seconds.

| Function | Parameters and effect |
|:---|:---|
| `gamepad_feedback(low, high, time)` | Controller vibration. `low` and `high` are the low- and high-frequency motor intensities, from 0 to 1. Use a duration from 0.001 to 65.535 seconds. |
| `gamepad_trigger_shot(right, time)` | Single-shot trigger effect with fixed parameters. |
| `gamepad_trigger_resistance(right, start_position, force, time)` | Trigger resistance. `start_position` ranges from 0 to 9 and `force` from 0 to 8. |
| `gamepad_trigger_machine(right, start_zone, behavior, force, amplitude, period, frequency, time)` | Repeating trigger effect. `start_zone`, `period`, and `frequency` are bytes. `behavior` is a boolean mode selector. `force` and `amplitude` each use the low four bits (0–15). |

Vibration requires gamepad input mode. The SDL vibration path respects `gamepad_vibration`. Call `gamepad_feedback(0, 0, 0)` to stop vibration.

Trigger effects require a DualSense whose HID device the engine has opened. Pass `true` for R2 or `false` for L2. A positive duration schedules an automatic reset. A duration of zero leaves the reset unscheduled. These effect functions return no value.

```lua
gamepad_feedback(0.6, 0.3, 0.15)
gamepad_trigger_shot(true, 0.12)
gamepad_trigger_resistance(false, 4, 6, 0.5)
gamepad_trigger_machine(true, 127, true, 15, 15, 60, 255, 0.6)
```

## Weapon configuration

Set these optional keys in the weapon's LTX section. The values below are the engine defaults.

```ini
vibration_time = 0.1
vibration_factor_left = 1.0
vibration_factor_right = 1.0
trigger_effect_time = 1.0
```

- `vibration_time` is the vibration duration in seconds. `vibration_factor_left` and `vibration_factor_right` set the motor intensities from 0 to 1. Magazine-fed weapons and stationary machine guns read all three keys.
- `trigger_effect_time` sets the duration in seconds of the DualSense R2 effect in single-shot mode for magazine-fed weapons. The repeating trigger effect in other fire modes uses parameters computed by the engine.

The bundled LTX files set `vibration_time = 2.0` for `wpn_gauss`. The other three keys use the engine defaults unless a weapon config overrides them.

## Other Lua functions

- `gamepad_led(r, g, b)` sets the controller light color. Each RGB component is an integer from 0 to 255.
- `gyroscope_supported()` returns `true` if the current gamepad reports a gyroscope to SDL.
- `any_binded_key_for_action_pressed_c(action_id)` checks whether a key bound to the action is pressed.

## Gamepad input

The Lua `gamepad_keys` table exposes button IDs. The face-button IDs `BUTTON_A`, `BUTTON_B`, `BUTTON_X`, and `BUTTON_Y` correspond to Cross, Circle, Square, and Triangle on PlayStation controllers. `BUTTON_LSHOULDER` and `BUTTON_RSHOULDER` correspond to LB/RB or L1/R1. `BUTTON_LSTICK` and `BUTTON_RSTICK` represent stick clicks. Stick directions use `BUTTON_LSTICK_*` and `BUTTON_RSTICK_*`, and D-pad directions use `BUTTON_DPAD_*`.

`IInputReceiver` receives button events through `IR_GamepadKeyPress`, `IR_GamepadKeyRelease`, and `IR_GamepadKeyHold`, and analog input through `IR_GamepadUpdateStick`. `CUIWindow` exposes `OnGamepadKeyAction`, `OnGamepadKeyHold`, and `OnGamepadStickAction`. Lua windows derived from `CUIScriptWnd` can handle the corresponding callbacks.

```lua
function MyWindow:OnGamepadKey(id, gamepad_action)
    return false
end

function MyWindow:OnGamepadKeyHold(id)
    return false
end

function MyWindow:OnGamepadStick(key, value, gamepad_action)
    return false
end
```

For analog input, `key` is `0` for the left stick, `1` for the right stick, and `2` for the triggers. With triggers, `value.x` is the left trigger and `value.y` is the right trigger.

## Gamepad legend

`CUIGamepadLegend` is a stack panel for control hints. It appears in gamepad mode and can lay out text and icons along its width.

```xml
<gamepad_legend x="0" y="724" width="1024" height="30" spacing="5">
    <auto_static x="0" y="0" width="22" height="30">
        <text font="graffiti19" vert_align="c">ui_mm_back</text>
    </auto_static>
</gamepad_legend>
```

```lua
self.gamepad_legend = xml:InitGamepadLegend("gamepad_legend", self)
```
