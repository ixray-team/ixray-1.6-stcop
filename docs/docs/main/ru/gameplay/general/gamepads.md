# Геймпады
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4

## Скриптовые экспорты
На данный момент имеется экспорт всех существующих кнопок из SDL3 и движка.

### gamepad_keys
| ID кнопки | Кнопка на Xbox | Кнопка на PlayStation | Заметки |
|:---|:---:|:---:|---:|
| BUTTON_A | A | ✕ |  |
| BUTTON_B | B | ○ |  |
| BUTTON_X | X | □ |  |
| BUTTON_Y | Y | △ |  |
| BUTTON_BACK | Back | Сенсорная панель |  |
| BUTTON_GUIDE | Логотип Xbox | PS |  |
| BUTTON_START | Start | Options |  |
| BUTTON_LSTICK | LS | L3 |  |
| BUTTON_RSTICK | RS | R3 |  |
| BUTTON_LSHOULDER | LB | L1 |  |
| BUTTON_RSHOULDER | RB | R1 |  |
| BUTTON_LTRIGGER | LT | L2 | Левый триггер |
| BUTTON_RTRIGGER | RT | R2 | Правый триггер |
| BUTTON_DPAD_UP | D-Pad вверх | D-Pad вверх |  |
| BUTTON_DPAD_DOWN | D-Pad вниз | D-Pad вниз |  |
| BUTTON_DPAD_LEFT | D-Pad влево | D-Pad влево |  |
| BUTTON_DPAD_RIGHT | D-Pad вправо | D-Pad вправо |  |
| BUTTON_LSTICK_UP | LS вверх | L3 вверх | Левый стик вверх |
| BUTTON_LSTICK_DOWN | LS вниз | L3 вниз | Левый стик вниз |
| BUTTON_LSTICK_LEFT | LS влево | L3 влево | Левый стик влево |
| BUTTON_LSTICK_RIGHT | LS вправо | L3 вправо | Левый стик вправо |
| BUTTON_RSTICK_UP | RS вверх | R3 вверх | Правый стик вверх |
| BUTTON_RSTICK_DOWN | RS вниз | R3 вниз | Правый стик вниз |
| BUTTON_RSTICK_LEFT | RS влево | R3 влево | Правый стик влево |
| BUTTON_RSTICK_RIGHT | RS вправо | R3 вправо | Правый стик вправо |

### Вибрация и адаптивные триггеры

Функции доступны глобально в Lua. Длительность эффекта задаётся в секундах.

| Вызов | Параметры и действие |
|:---|:---|
| `gamepad_feedback(low, high, time)` | Вибрация корпуса. `low` и `high` — интенсивность низкочастотного и высокочастотного моторов от 0 до 1. Для запуска задайте `time` от 0,001 до 65,535 секунды. |
| `gamepad_trigger_shot(right, time)` | Эффект одиночного выстрела с фиксированными параметрами. |
| `gamepad_trigger_resistance(right, start_position, force, time)` | Сопротивление нажатию. `start_position` — позиция начала эффекта от 0 до 9, `force` — сила от 0 до 8. |
| `gamepad_trigger_machine(right, start_zone, behavior, force, amplitude, period, frequency, time)` | Повторяющийся эффект. `start_zone`, `period` и `frequency` передаются как байты. `behavior` — логический переключатель режима. Для `force` и `amplitude` используются младшие четыре бита каждого значения (0–15). |

Вибрация вызывается в режиме ввода с геймпада. Путь через SDL учитывает настройку `gamepad_vibration`. Для остановки вибрации вызовите `gamepad_feedback(0, 0, 0)`.

Для адаптивных триггеров нужен DualSense с открытым HID-устройством. `right = true` выбирает R2, `right = false` — L2. Указывайте положительную длительность для автоматического сброса эффекта. При `time = 0` сброс по таймеру не запланирован. Функции эффектов не возвращают результат.

```lua
-- Примеры отдельных вызовов
gamepad_feedback(0.6, 0.3, 0.15)
gamepad_trigger_shot(true, 0.12)
gamepad_trigger_resistance(false, 4, 6, 0.5)
gamepad_trigger_machine(true, 127, true, 15, 15, 60, 255, 0.6)
```

## Настройки оружия

Эти необязательные ключи задаются в LTX-секции оружия. Ниже приведены значения по умолчанию.

```ini
vibration_time = 0.1
vibration_factor_left = 1.0
vibration_factor_right = 1.0
trigger_effect_time = 1.0
```

- `vibration_time` — длительность вибрации в секундах. `vibration_factor_left` и `vibration_factor_right` — интенсивность моторов от 0 до 1. Все три ключа читаются для магазинного оружия и стационарных пулемётов.
- `trigger_effect_time` — длительность эффекта на R2 DualSense в секундах при стрельбе одиночными из магазинного оружия. В остальных режимах параметры повторяющегося эффекта вычисляет движок.

В штатных LTX для `wpn_gauss` задано `vibration_time = 2.0`. Остальные три ключа берут значения по умолчанию, если конфиг оружия их не переопределяет.

### Другие функции

- `gamepad_led(r, g, b)` задаёт цвет подсветки. Каждый компонент RGB — целое число от 0 до 255.
- `gyroscope_supported()` возвращает `true`, если текущий геймпад сообщает SDL о наличии гироскопа.
- `any_binded_key_for_action_pressed_c(action_id)` проверяет, нажата ли клавиша, назначенная на действие.

## Скриптовые и движковые функции
На данный момент имеются следующие функции в движке:
```c++
// IInputReceiver
void IR_GamepadKeyPress(int key); // key - Нажимаемая клавиша.
void IR_GamepadKeyRelease(int key); // key - Отжимаемая клавиша.
void IR_GamepadKeyHold(int key); // key - Зажимаемая клавиша.

void IR_GamepadUpdateStick(int key, Fvector2 value); // key - Тип стика 
                                                     // (0 - левый стик, 1 - правый стик, 2 - триггеры), 
                                                     // value - Значения x, y 
                                                     // (в случае с триггерами: x - сила нажатия на левый триггер
                                                     // y - сила нажатия на правый триггер), 
// CUIWindow
bool OnGamepadKeyAction(int key, EUIMessages gamepad_action); // key - Нажимаемая клавиша, 
                                                              // gamepad_action - Действие в окне.

bool OnGamepadKeyHold(int key); // key - Зажимаемая клавиша

bool OnGamepadStickAction(int key, Fvector2 value, EUIMessages gamepad_action); // key - Тип стика 
                                                                                // (0 - левый стик, 1 - правый стик, 2 - триггеры), 
                                                                                // value - Значения x, y 
                                                                                // (в случае с триггерами: x - сила нажатия на левый триггер
                                                                                // y - сила нажатия на правый триггер), 
                                                                                // gamepad_action - Действие в окне.
```

и в скриптах:
```lua
bool CUIScriptWnd:OnGamepadKey(int id, int gamepad_action) -- id - нажатая клавиша, gamepad_action - действие внутри окна (CUIWindow)
bool CUIScriptWnd:OnGamepadKeyHold(int id) -- id - нажатая клавиша
bool CUIScriptWnd:OnGamepadStick(int key, Fvector2 value, int gamepad_action) -- key - тип стика (0 - левый стик, 1 - правый стик, 2 - триггеры), value - значения X и Y, gamepad_action - действие внутри окна (CUIWindow)
```

## Легенда (CUIGamepadLegend)

Отдельный класс, базирующийся на CUIStackPanel. Отличается тем, что автоматически скрывается при вводе с мыши/клавиатуры, а также имеет автоматический подгон текста по ширине. 

Пример реализации в XML:
```xml
        <!-- Параметры идентичные, за исключением отсутствия режима 
             (всегда стоит горизонтальный по центру) -->
	<gamepad_legend x="0" y="724" width="1024" height="30" spacing="5">
		<!-- Текст, идущий после кнопки. Обычный CUIStatic с автоподгоном по ширине. Может содержать в себе иконку -->
		<auto_static x="0" y="0" width="22" height="30">
			<text font="graffiti19" vert_align="c">ui_mm_back</text>
		</auto_static>
	</gamepad_legend>
```

Пример реализации в Lua:

Init:

```lua
	self.gamepad_legend = xml:InitGamepadLegend("gamepad_legend", self)
```

Update:

```lua
	if self.gamepad_legend then
		local text_select		= self:GetStatic("gp_select")	
		if text_select then
			if self.shniaga:GetPage() ~= CUIMMShniaga.epi_new_game then
				text_select:SetTextST("ui_mm_select")
			else
				text_select:SetTextST("ui_mm_select_difficulty")
			end
		end
	end
```

## Выбор цели при входе в прицел

При обработке действия прицеливания с геймпада движок может довести камеру к видимому живому противнику. Он выбирает кандидата с наибольшим совпадением направления на цель с направлением камеры. Секция `[auto_aiming]` загружается через `system.ltx`. Во всех четырёх штатных комплектах она находится в `mod_system_ixray.ltx`.

| Ключ | Резерв в коде | Действие |
| --- | --- | --- |
| `distance_b` | `5.1` | Расстояние перехода между ближним и дальним диапазонами. |
| `distance_c` | `200.1` | Предельная дальность захвата. Цели на этой границе и дальше отбрасываются. |
| `dotp_a_norm`, `dotp_a_easy` | `0.5` | Минимальное скалярное произведение направлений на нулевой дистанции. |
| `dotp_b_norm`, `dotp_b_easy` | `0.97` | Порог на расстоянии `distance_b`. |
| `dotp_c_norm`, `dotp_c_easy` | `0.97` | Порог на дальней границе. |
| `easiness` | `1.0` | Доля профиля `easy` при смешивании порогов. `0` выбирает `norm`, `1` — `easy`. |
| `height_fraction` | `0.7` | Высота точки наведения как доля высоты ограничивающего объёма цели, считая от его нижней грани. |

Все значения дробные. Для угловых порогов используйте `-1…1`. Чем ближе значение к `1`, тем меньше допустимый угол. Задавайте расстояния в порядке `0 < distance_b < distance_c`. Между нулём и `distance_b` порог интерполируется по синусу, между `distance_b` и `distance_c` — линейно. Например, измените значения в существующей секции, чтобы ограничить захват дистанцией 35 м.

```ini
[auto_aiming]
distance_b = 5
distance_c = 35
easiness = 0
height_fraction = 0.72
```
