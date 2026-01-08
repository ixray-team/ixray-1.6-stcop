
# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

## Модуль опций IXR OPTIONS

  Система централизованной публикации настроек вашего скрипта в кастомное меню настроек в главном меню:
  * Использует независимое от сохранений игры хранилище конфигурации на ряду с игровыми опциями.
  * Синергирует с системой коллбеков и публикует событие подписавшись на которое любой скрипт участвующий в автозагрузке может опубликовать свои настройки в общую область настроек.
  * Структурированное по разделам хранилище опций.
  * Поддерживается установка описаний своих настроек и выбор подходящего типа контрола для визуализации управления данными.
  * Скрипт опубликовавший настройку пурсональноу ведомляется о её изменении при сохранении пользователем.
  * Поддердживаются значения по умолчанию.
  * Настройки загружаются перед выполнением основной автозагрузки что гарантирует правильный порядок инициализации всех зависимых скриптов использующих опции.
  * Опции строго изолированы экземпляром скрипта их породившим. т.е может быть несколько разных скриптаов где опции названы полностью одинаково, это не будет вызывать конфликты, оба варианта опций появятся в главном меню.

```lua
--// Проверить наличие переменной в настройках
HasOptionsVar(var_name)
args: 
  var_name (string)(required) - ключ для поиска значения (уникален в пространстве имен скрипта).
retval: (bool) - существует ли значение по ключу.

--// Получить переменную из настроек
GetOptionsVar(var_name, default_value)
args:
  var_name (string)(required) - ключ для получения значения.
  default_value (mixed)(optional) - значение по умолчанию, если ключ не найден.
retval: (nil|bool|int|float|string|table|function) - значение по ключу или default_value.


--// Метод регистрации настройки (доступен только в рамках замыкания вызываемого событием "on_init_ixr_options")
 _ref.register_option(group_name, variable_name, display_text, controll_data, _callback)
args:
  group_name (string|nil)(required) - группировка опций по какому либо общему наименованию, если передать nil опция не будет сгруппирована.
  variable_name (string)(required) - системное имя опции должно быть уникальным в рамках скрипта в котором регистрируется опция.
  display_text (string)(required) - Текст отображаемый как описание пункта настройки.
  controll_data (table)(required) - таблица определяющая контролл который будет отрисован в меню опций (задается через обертку об этом будет далее).
  _callback (callable)(required) - функция вызываемая при изменении и загрузке настроек.
retval: (void)
```

Вариации контроллов для опций.
```lua
--// Создать контрол TrackBar (ползунок)
_ref.track_bar(default_value, min_value, max_value, value_offset)
args:
  default_value (number)(required) - начальное значение.
  min_value (number)(required) - минимальное значение.
  max_value (number)(required) - максимальное значение.
  value_offset (number)(required) - шаг изменения значения.
retval: (table) - данные контрола для  _ref.register_option.

--// Создать контрол NumericUpDown (числовой переключатель)
_ref.numeric_up_down(default_value, min_value, max_value, value_offset)
args:
  default_value (number)(required) - начальное значение.
  min_value (number)(required) - минимальное значение.
  max_value (number)(required) - максимальное значение.
  value_offset (number)(required) - шаг изменения значения.
retval: (table) - данные контрола для  _ref.register_option.

--// Создать контрол ComboBox (выпадающий список)
_ref.combo_box(default_selected_index, list_values_array)
args:
  default_selected_index (number)(required) - индекс выбранного элемента по умолчанию (начиная с 1).
  list_values_array (table)(required) - массив строковых значений для списка.
retval: (table) - данные контрола для  _ref.register_option.

--// Создать контрол TextBox (текстовое поле)
_ref.text_box(def_value)
args:
  def_value (string)(required) - значение по умолчанию.
retval: (table) - данные контрола для  _ref.register_option.

--// Создать контрол CheckBox (флажок)
_ref.check_box(def_value)
args:
  def_value (boolean)(required) - значение по умолчанию.
retval: (table) - данные контрола для  _ref.register_option.
```


Примеры:
```lua

--// вытоматически вызываемый метод системой ixr autoloader
function on_game_start()
  --// Подписываемся на событие регистрации опций
	RegisterScriptCallback("on_init_ixr_options", function (_ref)
    --// создаем TextBox (текстовое поле)
		_ref.register_option(nil, "gg_name", "Пример текстового поля ввода", _ref.text_box("Меченый"), this.on_change_value)

    --// создаем TrackBar (ползунок)
		_ref.register_option(nil, "float_track_bar", "Пример трек бара", _ref.track_bar(23.0, 0.0, 100.0, 1.0), this.on_change_value)
		
    --// создаем NumericUpDown (числовой переключатель)
		_ref.register_option(nil, "float_picker", "Пример числового пикера", _ref.numeric_up_down(0.0, -1.0, 1.0, 0.25), this.on_change_value)
    
    --// создаем ComboBox (выпадающий список)
		_ref.register_option(nil, "color_picker", "Пример комбо бокса", _ref.combo_box(1, {
			{["option_display_name"] = "Не задан", 	["option_return_value"] = {255, 255, 255}}, --// в возвращаемое значение можно положить любой lua тип даже функцию
			{["option_display_name"] = "Красный", 	["option_return_value"] = {255, 0, 0}}, 	--// в возвращаемое значение можно положить любой lua тип даже функцию
			{["option_display_name"] = "Зеленый", 	["option_return_value"] = {0, 255, 0}}, 	--// в возвращаемое значение можно положить любой lua тип даже функцию
			{["option_display_name"] = "Синий", 	["option_return_value"] = {0, 0, 255}}, 	--// в возвращаемое значение можно положить любой lua тип даже функцию
		}), this.on_change_value)
		
    --// создаем CheckBox (флажок)
		_ref.register_option(nil, "var_4", "Чекбокс 4", _ref.check_box(false), this.on_change_value)

    --// группируем несколько чек боксов
		_ref.register_option("Группа 1", "var_5", "Чекбокс 5", _ref.check_box(false), this.on_change_value)
		_ref.register_option("Группа 1", "var_6", "Чекбокс 6", _ref.check_box(true), this.on_change_value)
		_ref.register_option("Группа 1", "var_7", "Чекбокс 7", _ref.check_box(true), this.on_change_value)
		_ref.register_option("Группа 1", "var_8", "Чекбокс 8", _ref.check_box(false), this.on_change_value)
		
    --// группируем несколько чек боксов
		_ref.register_option("Группа 2", "var_9", "Чекбокс 9", _ref.check_box(true), this.on_change_value)
		_ref.register_option("Группа 2", "var_10", "Чекбокс 10", _ref.check_box(true), this.on_change_value)
		_ref.register_option("Группа 3", "var_11", "Чекбокс 11", _ref.check_box(false), this.on_change_value)
	end)
end

--// определили функцию выступающую в роли коллбека реагирующего на изменение опций этого файла скрипта
function on_change_value(group, name, value)
	--// печатаем в лог
  SemiLog(tostring(script_name()) .. ".script [option change]: group:" .. tostring(group) .." name:".. tostring(name) .. " value:" .. tostring(ffx_json_lib.json_encode(value)))
end

--// Проверить наличие переменной в настройках
if HasOptionsVar("my-option") then
  ...
end

--// Получить значение переменной из настроек
local option_value = GetOptionsVar("my-option", nil)
```
