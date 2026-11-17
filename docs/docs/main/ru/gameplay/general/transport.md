# Транспорт

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: IX-Ray Platform 1.1

## Обзор

В конфигурации транспорта задаются место водителя, камеры, приборная панель, используемые кости и багажник. Для машины со Story ID используйте класс `SCRPTCAR`.

```ini
class = SCRPTCAR
story_id = aiw_closed
```

## Синтаксис

### Позиция в машине
Позиция настраивается в секции `car_definition`. Есть несколько режимов:
1. Через кость (оригинал)
```ini
driver_place = seat_left
```
2. Без указания. Берётся root кость (оригинал)
3. Через прямое указание позиций (Lost Alpha)
```ini
driver_position  = 10,10,10 ; Позиция
driver_direction = 1,0,1    ; Направление камеры
```

## Камеры водителя

В `[car_definition]` укажите `camera_pos` для камеры первого лица. `camera_pos2` задаёт смещение при выходе из камеры первого лица в другой режим. Если ключ отсутствует, используется `camera_pos`.

## Двигатель и коробка передач

В таблице приведены параметры пользовательских данных модели автомобиля для IX-Ray Platform 1.4.

| Секция и ключ | Действие |
| --- | --- |
| `[car_definition] auto_transmission` | Включает автоматическое переключение передач. |
| `[car_definition] power_increment_factor`, `rpm_increment_factor` | Коэффициенты сглаживания роста мощности и оборотов. Начальное значение обоих — `0.5`. |
| `[car_definition] power_decrement_factor`, `rpm_decrement_factor` | Коэффициенты сглаживания снижения мощности и оборотов. Берут соответствующее значение для роста, если ключ пропущен. |
| `[car_definition] power_neutral_factor` | Множитель мощности без нажатия газа. Начальное значение `0.25`. Допустимы значения строго между `0.1` и `1.0`. |
| `[car_definition] fuel_tank`, `fuel_consumption` | Ёмкость бака и коэффициент расхода топлива. Движок заполняет бак при загрузке параметров и делит коэффициент расхода на `100000`. |
| `[car_definition] main_gear_ratio` | Передаточное число главной пары. Умножается на передаточное число каждой ступени. |
| `[transmission_gear_ratio] R`, `N1`, `N2` и далее | Вектор из трёх чисел для задней и каждой передней передачи. Для передних ступеней задайте передаточное число и две границы в об/мин. Номера передних ступеней должны идти подряд. |

## Приборная панель
Машина поддерживает приборную панель. Для этого в конфигурации модели машины необходимо создать секцию `dashboard`

```ini
[dashboard]
rpm_bone = rpm_strelka                   ; Кость тахометра
fuel_bone = fuel_strelka                 ; Кость датчика уровня топлива
speed_bone = speed_strelka               ; Кость спидометра
rpm_angle = 5.0, -180.0, -240.0, 5.0     ; [NET Online] zero_rpm, max_rpm, min_angle, max_angle
fuel_angle = 5.0, -180.0, -240.0, 5.     ; [NET Online] zero_fuel, max_fuel, min_angle, max_angle
speed_angle = 15.0, -133.0, -250.0, 15.0 ; [NET Online] 0 km/h, 100 km/h, min_angle, max_angle
```
Для корректного вращения или движения кости, необходимо настроить `bind_rotation`

![image](https://github.com/user-attachments/assets/1b802575-632d-4364-b286-d7ac90eac133)

### Примеры настройки:
Сейчас доступны для настройки `rpm_*`, `fuel_*`, `speed_*`
```ini
*_bone = bone_name
*_angle = zero_value, max_value, min_angle, max_angle
```
Для удобства углы подбираются так-же через `bind_rotation`

::: details Инструкция по настройке

#### Настройка `zero_value` и `max_value`

Для настройки `zero_value` подбираем значение при котором стрелочка датчика указывает на ноль. Для настройки `max_value` (для стрелок тахометра и датчика топлива это значение задаётся в конфиге, поэтому вы сами выбираете какое значение максимальное, для спидометра это значение фиксированное - 100 км/ч) подбираем необходимое значение.

![image](https://github.com/user-attachments/assets/255a37dd-b615-4205-8f32-5916123184e5)

#### Настройка `min_angle` и `max_angle`
Для настройки пределов счётчика необходимо подобрать крайнее левое и крайнее правое положение стрелок и записать в порядке возрастания в конфиг.

![image](https://github.com/user-attachments/assets/c6b953c1-8db5-4079-8b6b-451d7fcb07f3)

:::

## Usable Bones 
В одиночной игре обработчик `Use()` может вызывать Lua-функцию для кости, на которую смотрит игрок. В `[car_definition]` задайте два списка одинаковой длины.

Пары сопоставляются по порядку. Если функция возвращает `true`, движок пропускает стандартное взаимодействие. При `false` или отсутствующем обработчике продолжается обычная обработка.

```ini
[car_definition]
usable_bones = back_wheel, steering_wheel
usable_bones_callback = test_car.fuel_test, test_car.steering_test
```

`gamedata/scripts/test_car.script`

```lua
function fuel_test(car)
    car:add_fuel(10)
    return true
end

function steering_test(car)
    return false
end
```

Текст подсказки задаётся строкой `car_use`.

```xml
<string id="car_use">
	<text>Использовать ($$ACTION_USE$$)</text>
</string>
```
> Подробнее о новых скриптовых экспортах можно прочесть [тут](vehicle-lua.md).
## UI Индикаторы
![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/d9b17009-669a-431b-953f-3b44d606f8b8)
### Файлы: 
* configs/ui/car_panel.xml <-- XML Описание окна 
* configs/ui/textures_descr/ui_car_panel.xml <-- Регистрация иконок на текстуре 
* textures/ui/car_panel.dds <-- Текстура с иконками

### XML окно 

::: details Нажмите на меня, чтобы переключить код

``` xml
<?xml version='1.0' encoding="UTF-8"?>
<w>
	<car_panel x="0" y="0" width="1024" height="768"> <!-- Само окно -->
	    <car_static x="840" y="525" width="175" height="100" stretch="1"> <!-- Задний фон -->
			<texture>ui_car_panel_back</texture>
		</car_static>
	     <car_health_progress_bar x="45" y="12" width="114" height="12" horz="1" min="0" max="1" pos="0"> <!-- Индикатор здоровья -->
		    	    <progress stretch="1">
		        		<texture r="194" g="8" b="8" a="200">ui_inGame2_inventory_progress_bar</texture>
			        </progress>
		</car_health_progress_bar>
	     <car_fuel_progress_bar x="45" y="30" width="114" height="12" horz="1" min="0" max="100" pos="0"> <!-- Индикатор топлива -->
		    	    <progress stretch="1">
		        		<texture r="8" g="122" b="122" a="200">ui_inGame2_inventory_progress_bar</texture>
			        </progress>
		</car_fuel_progress_bar>

		<car_engine_lamp> <!-- Индикатор двигателя -->
			<on x="26" y="47" width="16" height="16" stretch="1">
				<texture r="122" g="122" b="8">ui_car_panel_engine</texture>
			</on>
			<off x="26" y="47" width="16" height="16" stretch="1">
				<texture a="120">ui_car_panel_engine</texture>
			</off>
		</car_engine_lamp>
		
		<car_light_lamp> <!-- Индикатор фар -->
			<on x="44" y="47" width="14" height="12" stretch="1">
				<texture r="122" g="122" b="8">ui_car_panel_light</texture>
			</on>
			<off x="44" y="47" width="14" height="12" stretch="1">
				<texture>ui_car_panel_light</texture>
			</off>
		</car_light_lamp>

		<car_speed_mode x="30" y="77" width="16" height="16" stretch="1"> <!-- Индикатор текущей передачи -->
			<text font="font_graffiti" />
		</car_speed_mode>
	</car_panel>
</w>
```
:::

## Багажник 
### Регистрация 
В **userdata** конфиге нужно указать название кости в секции `car_definition`
```ini
trunk_bone = back_wheel
```
![image](https://github.com/ixray-team/ixray-1.6-stcop/assets/13867290/86361179-a30f-432a-b8a8-b40acde72e62)
### Текстовая подсказка
```xml
<string id="car_trunk_use">
 <text>Открыть багажник ($$ACTION_USE$$)</text>
</string>
```
### Иконка и имя 
Данный функционал является опциональным. Для активации нужно прописать следующие строки в основной конфиг автомобиля: 
```ini
name = "niva" ; Имя авто (Имеется поддержка секций из text)
icon = ui_npc_monster_pseudodog ; иконка авто
```

## См. также

- [Транспорт в Lua](vehicle-lua.md)
- [Геймпады](gamepads.md)

## Предметы с посадочным местом

> **Минимальная версия**: 1.4

У объекта класса `CHolderEntityObject` параметры читаются из его собственной LTX-секции. Пример такой секции есть в CoC под именем `holder_stul` в `configs/models/dynamic_objects.ltx`.

| Ключ | Тип и резерв | Назначение |
| --- | --- | --- |
| `allow_weapon` | обязательный bool | Разрешает обработку действий оружия, пока актёр занимает место. |
| `lock_enter`, `lock_exit` | обязательные bool | Блокируют посадку и выход соответственно. |
| `camera_pos` | вектор `0, 0, 0` | Позиция камеры в локальных координатах объекта. Движок переводит её в мировые координаты. |
| `camera_angle` | вектор `0, 0, 0` | Вектор, который загрузчик передаёт обновлению камеры вместе с позицией. |
| `exit_pos` | вектор `0, 0, 0` | Позиция, в которую устанавливается физический контроллер актёра при выходе. Этот загрузчик возвращает значение напрямую, поэтому задавайте координаты на уровне. |
| `use_action_hint` | строка, необязательно | Ключ текста подсказки при наведении на объект. |

Для отдельного экземпляра на известной локации разместите точку выхода на свободном участке рядом с местом. Например, при тестировании объекта в точке `100, 2, 40` используйте `exit_pos = 101, 2, 40`, если эта точка свободна в геометрии уровня.
