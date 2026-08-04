# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_math_utils: `\gamedata\scripts\ixr_framework\utils\ffx_math_utils.script`
Утилиты для математических операций:
* `classic_round`
* `clamp_in_range`
* `scaled_random`
* `safe_divide`

#### Описание методов:

```lua
--// Округлить число по классическому правилу: 0.5 и выше — вверх, иначе — вниз.
classic_round(value)
args:
  value (number)(required) - число для округления.
retval: (number) - округлённое целое число.

--// Ограничить значение заданным диапазоном [min_value, max_value].
clamp_in_range(min_value, current_value, max_value)
args:
  min_value (number)(required) - минимальное допустимое значение.
  current_value (number)(required) - проверяемое значение.
  max_value (number)(required) - максимальное допустимое значение.
retval: (number) - значение, ограниченное диапазоном [min_value, max_value].

--// Сгенерировать случайное число в диапазоне от min_value до max_value * multiplier_coeff (но не менее min_value). Если min и max — целые, результат округляется до целого.
scaled_random(min_value, max_value, multiplier_coeff)
args:
  min_value (number)(required) - минимальное значение.
  max_value (number)(required) - максимальное значение.
  multiplier_coeff (number)(required) - коэффициент масштабирования (0..1).
retval: (number) - случайное значение в рассчитанном диапазоне, или 0 при ошибке.

--// Безопасно разделить два числа, возвращая 0, если делимое равно 0/nil или делитель равен 0/nil.
safe_divide(first_vale, second_value)
args:
  first_vale (number)(required) - делимое.
  second_value (number)(required) - делитель.
retval: (number) - результат деления, или 0 при небезопасном делении.
```

### Примеры использований:
```lua
--// Округление
local rounded = ffx_math_utils.classic_round(3.5)  --// 4
local rounded2 = ffx_math_utils.classic_round(3.49) --// 3
SemiLog(string.format("Результат округления: %d, %d", rounded, rounded2))

--// Ограничение значения
local clamped = ffx_math_utils.clamp_in_range(0, 15, 10)  --// 10
local clamped2 = ffx_math_utils.clamp_in_range(5, 3, 10)  --// 5
SemiLog(string.format("Зажатые значения: %d, %d", clamped, clamped2))

--// Генерация случайного числа с масштабированием
local random1 = ffx_math_utils.scaled_random(10, 100, 0.5) --// случайное от 10 до 50 (целое, т.к. границы целые)
local random2 = ffx_math_utils.scaled_random(1.5, 10.0, 0.7) --// дробное от 1.5 до 7.0
SemiLog(string.format("Случайные: %f, %f", random1, random2))

--// Безопасное деление
local div1 = ffx_math_utils.safe_divide(10, 2)   --// 5
local div2 = ffx_math_utils.safe_divide(10, 0)   --// 0
local div3 = ffx_math_utils.safe_divide(0, 5)    --// 0
SemiLog(string.format("Результаты деления: %d, %d, %d", div1, div2, div3))
```
