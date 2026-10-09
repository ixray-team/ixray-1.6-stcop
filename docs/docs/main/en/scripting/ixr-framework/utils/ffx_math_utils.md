# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

### ffx_math_utils: `\gamedata\scripts\ixr_framework\utils\ffx_math_utils.script`
Utilities for mathematical operations:
* `classic_round`
* `clamp_in_range`
* `scaled_random`
* `safe_divide`

#### Method descriptions:

::: code-group
```lua [gamedata\scripts\ixr_framework\utils\ffx_math_utils.script]
--// Round a number using the conventional rule: 0.5 and above rounds up; otherwise round down.
classic_round(value)
args:
  value (number)(required) - number to round.
retval: (number) - rounded integer.

--// Clamp a value to [min_value, max_value].
clamp_in_range(min_value, current_value, max_value)
args:
  min_value (number)(required) - minimum allowed value.
  current_value (number)(required) - value to check.
  max_value (number)(required) - maximum allowed value.
retval: (number) - value clamped to [min_value, max_value].

--// Generate a random number between min_value and max_value * multiplier_coeff, with the upper bound at least min_value. If min and max are integers, round the result to an integer.
scaled_random(min_value, max_value, multiplier_coeff)
args:
  min_value (number)(required) - minimum value.
  max_value (number)(required) - maximum value.
  multiplier_coeff (number)(required) - scaling coefficient (0..1).
retval: (number) - random value in the computed range, or 0 on error.

--// Safely divide two numbers, returning 0 if either the dividend or divisor is 0/nil.
safe_divide(first_vale, second_value)
args:
  first_vale (number)(required) - dividend.
  second_value (number)(required) - divisor.
retval: (number) - division result, or 0 if division is unsafe.
```
:::

### Usage examples:
```lua
--// Rounding.
local rounded = ffx_math_utils.classic_round(3.5)  --// 4
local rounded2 = ffx_math_utils.classic_round(3.49) --// 3
SemiLog(string.format("Rounded results: %d, %d", rounded, rounded2))

--// Clamp a value.
local clamped = ffx_math_utils.clamp_in_range(0, 15, 10)  --// 10
local clamped2 = ffx_math_utils.clamp_in_range(5, 3, 10)  --// 5
SemiLog(string.format("Clamped values: %d, %d", clamped, clamped2))

--// Generate a scaled random number.
local random1 = ffx_math_utils.scaled_random(10, 100, 0.5) --// random integer from 10 to 50, because the bounds are integers
local random2 = ffx_math_utils.scaled_random(1.5, 10.0, 0.7) --// fractional value from 1.5 to 7.0
SemiLog(string.format("Random values: %f, %f", random1, random2))

--// Safe division.
local div1 = ffx_math_utils.safe_divide(10, 2)   --// 5
local div2 = ffx_math_utils.safe_divide(10, 0)   --// 0
local div3 = ffx_math_utils.safe_divide(0, 5)    --// 0
SemiLog(string.format("Division results: %d, %d, %d", div1, div2, div3))
```
