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

Также имеются вспомогательные функции:
```lua
void gamepad_feedback(float left, float right, float time) -- Вибрация геймпада
bool any_binded_key_for_action_pressed_c(int action_id) -- Произведено ли действие с геймпада или нет
```

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

