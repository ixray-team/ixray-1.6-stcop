
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
--// (Функция работает только опции связанные с контекстом имени файла скрипты откуда произведен вызов)
--// Настройки каждого скрипта изолированы друг от друга (для проброса значений между файлами пишите собственные геттеры и сеттеры)
GetOptionsVar(var_name, default_value)
args:
  var_name (string)(required) - ключ для получения значения.
  default_value (mixed)(optional) - значение по умолчанию, если ключ не найден.
retval: (nil|bool|int|float|string|table|function) - значение по ключу или default_value.

--// Установить значение переменной в настройках с перерисовкой и событием изменения в связанный скрипт
--// (Функция работает только опции связанные с контекстом имени файла скрипты откуда произведен вызов)
--// Настройки каждого скрипта изолированы друг от друга (для проброса значений между файлами пишите собственные геттеры и сеттеры)
SetOptionsVar(var_name, var_value)
args:
  var_name (string)(required) - ключ для получения значения.
  var_value (mixed)(required) - значение (зависит от контролла - для списков передавать индекс - для прочих вещественные значения  строк или булева или целых или дробей).
retval: (bool) - удалось ли сменить значение.

--// Установить видимость опции настройках с перерисовкой на следующем кадре
--// (Функция работает только опции связанные с контекстом имени файла скрипты откуда произведен вызов)
SetOptionVisible(var_name, is_visible)
args:
  var_name (string)(required) - ключ для получения значения.
  is_visible (bool)(required) - управляет видимостью контролла в опциях.
retval: (bool) - удалось ли сменить значение.


--// Установить заголовок для вкладки опций
--// (Функция работает только опции связанные с контекстом имени файла скрипты откуда произведен вызов)
SetOptionTitle(title_text)
args:
  title_text (string)(required) - Текст заголовка (поддерживает перевод).
retval: (bool) - удалось ли сменить значение.

--// Установить название аддона для вкладки опций (по именам аддонов происзводится их сортировка для удобного отображения рядом)
--// (Функция работает только опции связанные с контекстом имени файла скрипты откуда произведен вызов)
SetOptionAddonName(addon_name_text)
args:
  addon_name_text (string)(required) - Текст название аддона (поддерживает перевод).
retval: (bool) - удалось ли сменить значение.

--// Установить название аддона для вкладки опций
--// (Функция работает только опции связанные с контекстом имени файла скрипты откуда произведен вызов)
SetOptionScriptVersion(addon_script_version)
args:
  addon_script_version (string)(required) - Текст версия аддона например 1.2.16
retval: (bool) - удалось ли сменить значение.

--// Установить перечень авторов для вкладки опций
--// (Функция работает только опции связанные с контекстом имени файла скрипты откуда произведен вызов)
SetOptionAuthors(addon_authors)
args:
  addon_authors (string)(required) - Текст перечисление авторов аддона (имеет ограничение длинны текста указывайте первых 2 - 3)
retval: (bool) - удалось ли сменить значение.

--// Установить иконку аддона для вкладки опций 64х64
--// (Функция работает только опции связанные с контекстом имени файла скрипты откуда произведен вызов)
SetOptionIcon(dds_icon_path)
args:
  dds_icon_path (string)(required) - Путь до dds иконки аддона отображаемый размер 64х64 рекомендуемый размер для файла 128х128 
retval: (bool) - удалось ли сменить значение.


--// Метод регистрации настройки (доступен только в рамках замыкания вызываемого событием "on_init_ixr_options")
 _ref.register_option(group_name, variable_name, display_text, controll_data, _callback)
args:
  group_name (string|nil)(required) - группировка опций по какому либо общему наименованию, если передать nil опция не будет сгруппирована.
  variable_name (string)(required) - системное имя опции должно быть уникальным в рамках скрипта в котором регистрируется опция.
  display_text (string)(required) - Текст отображаемый как описание пункта настройки.
  controll_data (table)(required) - таблица определяющая контролл который будет отрисован в меню опций (задается через обертку об этом будет далее).
  _callback (callable)(required) - функция вызываемая при изменении и загрузке настроек.
retval: (void)

--// Заблокировать к выбору элемент комбо бокса по индексу
DisableComboBoxIndex (variable_name, index)
Parameters:
  var_name (string)(required) - key name for index value in options (uique name for caller script namespace) not conflicts between other scripts,
  index (mixed)(required) - combo box item index
Returns: (void)

--// Разблокировать к выбору элемент комбо бокса по индексу
EnableComboBoxIndex(variable_name, index)
Parameters:
  var_name (string)(required) - key name for index value in options (uique name for caller script namespace) not conflicts between other scripts,
  index (mixed)(required) - combo box item index
Returns: (void)

```

Информация о испускаемых системой опций событиях на которые можно подписать функцию скрипта.
```lua
  --// Сигнатура метода обработчика событий настроек
  function (group, name, value, is_value_changed_by_code)
    args: 
      var_name (group)(string) - текстовое представление переданное в опцию в качестве имени группы (может помочь группировать логику обработки на основе группы если нужно).
      var_name (name)(string) - имя опции (имя переменной).
      var_name (value)(mixed) - значение выбранное в контроле настроек или связанное со сзначением свойство или метод (все зависит от типа контролла).
      var_name (is_value_changed_by_code)(bool) - указывает на то была ли настройка изменена через метод SetOptionsVar(...) т.е обработчик для системы пресетов.
    
```

Вариации контроллов для опций.
```lua
--// Создать контрол TrackBar (ползунок по умолчанию имеет дробное представление данных)
_ref.track_bar(default_value, min_value, max_value, value_offset, use_ceil)
args:
  default_value (number)(required) - начальное значение.
  min_value (number)(required) - минимальное значение.
  max_value (number)(required) - максимальное значение.
  value_offset (number)(required) - шаг изменения значения.
  use_ceil (bool) - использовать округление значения.
retval: (table) - данные контрола для  _ref.register_option.

--// Создать контрол TrackBar (ползунок по умолчанию имеет целочисленное представление данных) аналог предыдущего метода с предустановленным округлением
_ref.ceil_track_bar(default_value, min_value, max_value, value_offset)
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
  list_values_array (table)(required) - массив строковых значений для списка (заполнять через ref.combo_box_item).
retval: (table) - данные контрола для  _ref.register_option.

--// Итем для контролла ComboBox (элемент выпадающего списка)
_ref.combo_box_item(option_display_name, is_visible, option_return_value)
args:
  option_display_name (string)(required) - видимый текст элемента (имеет автоперевод по string table).
  is_visible (bool)(required) - видимость значения по умолчанию.
  option_return_value (mixed|nil|bool|string|int|float|function|table)(required) - возвращаемое значение отправляемое в событие смены значения контролла.
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

Базовые контроллы:
```lua

--// автоматически вызываемый метод системой ixr autoloader
function on_game_start()

  -- Регистрируем данные своего аддона перед инициализацией настроек
	SetOptionTitle("st_my_addon_title") -- Задаём заголовок для вкладки опций (создаем в xml строку перевода и указываем её id)
	SetOptionAddonName("st_my_addon") -- Задаём название аддона для вкладки опций (создаем в xml строку перевода и указываем её id)
	SetOptionScriptVersion("1.0.2") -- Задаём версию скрипта для вкладки опций
	SetOptionAuthors("User8912") -- Задаём авторов аддона для вкладки опций
	SetOptionIcon("addons_icons\\my_addon_icon") -- Задаём путь до иконки вкладки опций (dds файл указывается без расширения полный путь с двойными слешами относительно папки текстур)
	

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
function on_change_value(group, name, value, is_value_changed_by_code)
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


Пример реализации пресетов опций на базе имеющихся механик:
```lua
--// автоматически вызываемый метод системой ixr autoloader
function on_game_start()
  --// Подписываемся на событие регистрации опций
	RegisterScriptCallback("on_init_ixr_options", function (_ref)
	  
	  --// Будем использовать как основу для пресетов контролл комбо бокс (особенность его работы в том что мы можем в качестве значения на каждый элемент списка опций 
	  --// указывать лямбду скриптового замыкания на метод как хранимое значение) таким образом можно более наглядно связывать конкрутный пункт опции пресета 
	  --// с действиями которые нужно выполнить при переключении на пункт пресета
	  _ref.register_option("Пример пресета опций", "preset_picker", "Пример комбо бокса с пресетами настроек", _ref.combo_box(1, {
        _ref.combo_box_item("Пользовательский пресет", true, function () 
          -- оставляем здесь пустой метод чтобы не зациклилось когда пееходим на первый пресет при изменении любого элемента в ручную
        end),
        _ref.combo_box_item("1 Пресет", false, function () 
          SetOptionsVar("var_test_int_track_bar", 5)
          SetOptionsVar("var_test_01", true)
          SetOptionsVar("var_test_02", false)
          SetOptionsVar("var_test_03", false)
          SetOptionsVar("var_test_text", "preset first")
          SetOptionsVar("var_test_float_picker", 0.12)
          SetOptionsVar("var_test_color_picker", 1)
        end),
        _ref.combo_box_item("2 Пресет", true, function () 
          SetOptionsVar("var_test_int_track_bar", 15)
          SetOptionsVar("var_test_01", false)
          SetOptionsVar("var_test_02", true)
          SetOptionsVar("var_test_03", false)
          SetOptionsVar("var_test_text", "preset two")
          SetOptionsVar("var_test_float_picker", 0.26)
          SetOptionsVar("var_test_color_picker", 2)
        end),
        _ref.combo_box_item("3 Пресет", true, function () 
          SetOptionsVar("var_test_int_track_bar", 25)
          SetOptionsVar("var_test_01", false)
          SetOptionsVar("var_test_02", false)
          SetOptionsVar("var_test_03", true)
          SetOptionsVar("var_test_text", "preset three")
          SetOptionsVar("var_test_float_picker", 0.3)
          SetOptionsVar("var_test_color_picker", 3)
        end),
      }), function (group, name, value_fn) value_fn() end) 
      --// в последнем аргументе метода комбо бокса указываем что в качестве функции обрабатывающей событие мы будем задавать
      --// анонимную функцию которая сразу же при смене опции в комбо боксе выполнить привязанный к опции анонимный метод 
      --// для смены данных в подчинённых контроллах
      
      --// здесь разместим тестовые контроллы которыми будем управлять при переключении пресетов
      _ref.register_option("Пример пресета опций", "var_test_int_track_bar", "Пример трек бара целочисленный", _ref.track_bar(1.0, 0.0, 100.0, 1.0, true), this.on_change_value_preset)
      _ref.register_option("Пример пресета опций", "var_test_01", "Чекбокс 1", _ref.check_box(true), this.on_change_value_preset)
      _ref.register_option("Пример пресета опций", "var_test_02", "Чекбокс 2", _ref.check_box(true), this.on_change_value_preset)
      _ref.register_option("Пример пресета опций", "var_test_03", "Чекбокс 3", _ref.check_box(true), this.on_change_value_preset)
      _ref.register_option("Пример пресета опций", "var_test_text", "Пример текстового поля ввода", _ref.text_box("1234567890"), this.on_change_value_preset)
      _ref.register_option("Пример пресета опций", "var_test_float_picker", "Пример числового пикера", _ref.numeric_up_down(0.0, -1.0, 1.0, 0.25), this.on_change_value_preset)
      _ref.register_option("Пример пресета опций", "var_test_color_picker", "Пример комбо бокса", _ref.combo_box(1, {
        _ref.combo_box_item("%c[255, 255, 255, 255]Не задан (белый)", true, {255, 255, 255}), 
        _ref.combo_box_item("%c[255, 255, 0, 0]Красный", false, {255, 0, 0}),
        _ref.combo_box_item("%c[255, 0, 255, 0, 255]Зеленый", true, {0, 255, 0}),
        _ref.combo_box_item("%c[255, 0, 0, 255]Синий", true, {0, 0, 255}),
      }), this.on_change_value_preset)
	
	end)
end

--// здесь мы развестим функцию для контретного пресета рекомендую делать по уникальному методу в настройказ для упрощения взаимодействий если пресетов на странице настроек несколько одновременно
function on_change_value_preset(group, name, value, is_value_from_code)
	if not is_value_from_code then --// если опция была изменена не через смену ппресета настроек кодом то переключаем пресет на пользовательский 
		SetOptionsVar("preset_picker", 1) -- сбрасываем пресет на пользовательский (это нужно для того чтобы была возможность выбирать настройки не только через пресет а и в ручную докликивать нужные значения просто меняя пресет на пользовательский)
	end
end
```

Пример реализации со скрытием опций в зависимости от состояния дочернего контрола:
```lua
--// автоматически вызываемый метод системой ixr autoloader
function on_game_start()
  --// Подписываемся на событие регистрации опций
	RegisterScriptCallback("on_init_ixr_options", function (_ref)
	  
	  --// Будем использовать как основу чекбокс для простоты реализации
	  _ref.register_option("Группа c зависимым параметром", "var_2_a", "Включить отображение опции находящейся ниже", _ref.check_box(false), function (group, name, value, is_value_from_code)
			SetOptionVisible("int_track_bar_test_switch", value) -- сама функция позволяющая задавать видимость контролов
		end)
		
		-- трек бар который меняет доступность при переключении чекбокса управляющего его видимостью
		_ref.register_option("Группа c зависимым параметром", "int_track_bar_test_switch", "Пример трек бара целочисленный который вкл выкл чекбоксом выше", _ref.ceil_track_bar(1.0, 0.0, 100.0, 1.0), this.on_change_value)
	
	end)
end
```
