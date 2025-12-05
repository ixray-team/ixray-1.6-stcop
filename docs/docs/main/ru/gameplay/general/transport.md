# Транспорт
## Общее
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.1

* Восстановлен базовый функционал машин из SoC
* Исправлены полёты в воздухе при спавне
* Исправлены переходы по локациям 
* Добавлен класс **`SCRPTCAR`** из 1.5.10
```ini
class = SCRPTCAR
```
* Поддержка `Story ID` (для класса **`SCRPTCAR`**):
```ini
story_id = aiw_closed
```

## Позиция в машине
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
Данная система позволяет устанавливать скриптовые коллбеки на Use() по названию кости. Для этого нужно зарегистрировать список костей и функций в `car_definition`
```ini
; Custom usable 
usable_bones = back_wheel, left_door
usable_bones_callback = test_car.fuel_test, test_car.block_left_door
```
Пример скрипта:
```lua
function fuel_test(obj_car)
    obj_car.fuel = 10
    return false -- Передаём дальнейшее управление движку 
end

function block_left_door(obj_car)
    return true -- Блокируем дальнейшую обработку Use() события для движка 
end
```
* Текстовая подсказка 
```xml
<string id="car_use">
	<text>Использовать ($$ACTION_USE$$)</text>
</string>
```
> Подробнее о новых скриптовых экспортах можно прочесть [тут](/scripting/new-functions#ccar).
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
