# Базовые типы, конфиги и движок

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Здесь собраны регистрации базовых типов и функций движка. Утилиты и редакторы могут подключать отдельные наборы экспортов.

## Синтаксис

| Вызов | Аргументы и результат |
| --- | --- |
| `get_cached_ini(path)` | Открывает LTX относительно `$game_config$`. Повторное чтение по той же строке возвращает общий `ini_file`. |
| `external_ini()` | Возвращает `ini_file` для текущего `engine_external.ltx` или `nil`, если конфигурация недоступна. |
| `ini:section_for_each(callback)` | Вызывает `callback(section_name)` для каждой секции. `true` завершает обход; `false` продолжает его. |

## Примеры

### Прочитать настройку

Проверка ключа позволяет использовать тот же скрипт с разными конфигами. После переинициализации ALife получайте кэшированный объект заново.

```lua
local function use_3d_icons()
    local ini = get_cached_ini("engine_external.ltx")
    return ini:line_exist("gameplay", "Enable3DIcons")
        and ini:r_bool("gameplay", "Enable3DIcons")
end
```

### Найти секцию

Колбэк завершает обход, как только встречает `shaders_options`.

```lua
local function find_shader_section()
    local ini = external_ini()
    if not ini then return nil end

    local found
    ini:section_for_each(function(name)
        if name == "shaders_options" then
            found = name
            return true
        end
        return false
    end)
    return found
end
```

### Создать LTX и прочитать его обратно

Пример создаёт `example_mod_preferences.ltx` в каталоге `$app_data_root$`, записывает две настройки и читает обратно `110`. При повторном запуске он заново записывает этот файл. `alias` — зарегистрированный FS-алиас со знаками `$` по краям, а `filename` — путь относительно него.

```lua
local function save_mod_preferences()
    local alias = "$app_data_root$"
    local filename = "example_mod_preferences.ltx"
    local ini = create_ini(alias, filename)
    ini:save_at_end(false)

    ini:w_string("preferences", "theme", "dark", "")
    ini:w_u32("preferences", "scale", 110, "")
    assert(ini:save_as(""), "Could not save " .. filename)

    local loaded = read_ini(alias, filename)
    assert(loaded:line_exist("preferences", "scale"))
    return loaded:r_u32("preferences", "scale")
end
```

Обе функции возвращают отдельный объект, и их регистрации не передают Lua владение выделенной памятью. При частых чтениях неизменяемого конфига из `$game_config$` используйте `get_cached_ini`. Функция `create_ini_file(text)` читает текст LTX из памяти и передаёт объект во владение Lua.

### Записать и прочитать двоичные данные

Пример записывает локальный пакет и читает поля в том же порядке. Результат — `4, -7`.

```lua
local function packet_round_trip()
    local packet = net_packet()
    packet:w_begin(0)
    packet:w_vec4(vector4():set(1, 2, 3, 4))
    packet:w_s8(-7)

    packet:r_begin()
    local v = packet:r_vec4()
    return v.w, packet:r_s8()
end
```

## Регистрации

### Глобальная область

| Имя в Lua | Тип | Поведение | Регистрация |
| --- | --- | --- | --- |
| `create_ini(alias, filename)` | Функция | Возвращает пустой, доступный для записи `ini_file` с именем файла относительно указанного FS-алиаса. Если файл уже существует, его содержимое не загружается. Сохранение через `save_as("")` запишет новое содержимое поверх него. | [script_ini_file_script.cpp:183](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L183) |
| `debug_print(message)` | Функция | Пишет непустую строку в журнал с отметкой `DBG` и текущим игровым временем. | [script_engine_script.cpp:350](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L350) |
| `device()` | Функция | Возвращает объект устройства игры `Device`. | [script_engine_script.cpp:357](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L357) (`#ifdef XRGAME_EXPORTS`) |
| `editor()` | Функция | Возвращает `true` в сборке без `XRGAME_EXPORTS` и `false` в игровой сборке. | [script_engine_script.cpp:346](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L346) |
| `error_log(message)` | Функция | Пишет ошибку и стек Lua в журнал, затем вызывает assert движка. | [script_engine_script.cpp:342](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L342) |
| `external_ini()` | Функция | Возвращает `ini_file` для текущего `engine_external.ltx` или `nil`, если конфигурация недоступна. | [script_ini_file_script.cpp:181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L181) |
| `flush()` | Функция | Сбрасывает буферы журналов движка и Lua в сборке `DEBUG`. | [script_engine_script.cpp:343](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L343) |
| `get_cached_ini(filename)` | Функция | Открывает LTX относительно `$game_config$` и возвращает общий `ini_file` из кеша по той же строке пути. | [script_ini_file_script.cpp:188](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L188) |
| `help()` | Функция | Печатает в журнал классы текущей VM Lua, их свойства, константы и перегрузки методов. | [script_engine_script.cpp:354](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L354) |
| `is_cop()` | Функция | Проверяет режим «Зов Припяти». Возвращает `boolean`. | [script_ini_file_script.cpp:176](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L176) |
| `is_cs()` | Функция | Проверяет режим «Чистое небо». Возвращает `boolean`. | [script_ini_file_script.cpp:177](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L177) |
| `is_soc()` | Функция | Проверяет режим «Тень Чернобыля». Возвращает `boolean`. | [script_ini_file_script.cpp:178](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L178) |
| `IsEditor()` | Функция | Проверяет режим редактора у устройства движка. Возвращает `false`, если устройство недоступно. | [script_engine_script.cpp:353](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L353) |
| `IsSupportMP()` | Функция | Возвращает `true` в сборке с `XR_MP_BUILD`. | [script_engine_script.cpp:352](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L352) |
| `IsTracyConnected()` | Функция | Проверяет подключение клиента Tracy. Возвращает `false` в сборке без профилировщика Tracy. | [script_engine_script.cpp:363](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L363) |
| `load_script_to_global(filename)` | Функция | Выполняет скрипт из `$game_scripts$` в глобальной среде текущей VM. Результат загрузки в Lua не возвращается. | [script_engine_script.cpp:358](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L358) (`#ifdef XRGAME_EXPORTS`) |
| `log(message)` | Функция | Записывает сообщение Lua в журнал. Эта регистрация выполняется в режиме редактора, вывод исключён в `MASTER_GOLD`. | [script_engine_script.cpp:370](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L370) |
| `log1(message)` | Функция | Пишет строку в журнал движка. | [script_engine_script.cpp:341](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L341) |
| `prefetch(module_name)` | Функция | Запускает обработку модуля скриптовым движком по имени. | [script_engine_script.cpp:344](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L344) |
| `r_file_as_string(path)` | Функция | Читает весь файл в строку Lua. Путь должен указывать на существующий доступный для чтения файл. Экспорт не проверяет ошибку открытия. | [script_reader_script.cpp:118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L118) |
| `read_ini(alias, filename)` | Функция | Загружает LTX относительно указанного FS-алиаса и возвращает `ini_file` только для чтения. Если файл открыть не удалось, объект будет пуст. Проверяйте нужные секции и ключи перед чтением. | [script_ini_file_script.cpp:184](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L184) |
| `reader_base` | Класс | Базовый тип бинарного чтения. Объект предоставляет движок. | [script_reader_script.cpp:89](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L89) |
| `reload_system_ini()` | Функция | Перечитывает глобальный `system.ltx` из `$game_config$` и возвращает обёртку `ini_file` для текущих настроек. Уже загруженные объекты могут хранить значения, считанные при инициализации. | [script_ini_file_script.cpp:182](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L182) |
| `SemiLog(message)` | Функция | Записывает сообщение через `Msg`. Строка служит форматом, поэтому литеральный `%` передавайте как `%%`. | [script_engine_script.cpp:349](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L349) |
| `time_global()` | Функция | Возвращает игровое время в миллисекундах. В сборке без `XRGAME_EXPORTS` возвращает `0`. | [script_engine_script.cpp:348](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L348) |
| `time_global_async()` | Функция | Возвращает показание независимого от игрового времени таймера устройства в миллисекундах. В сборке без `XRGAME_EXPORTS` возвращает `0`. | [script_engine_script.cpp:351](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L351) |
| `TinyLog(message)` | Функция | Пишет строку в журнал движка. | [script_engine_script.cpp:359](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L359) (`#ifdef XRGAME_EXPORTS`) |
| `trigger_assert(message)` | Функция | Принудительно вызывает assert движка. Переданное сообщение реализация игнорирует. | [script_engine_script.cpp:360](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L360) (`#ifdef XRGAME_EXPORTS`) |
| `trigger_vs_log(message)` | Функция | На Windows отправляет строку в вывод отладчика. На других платформах вызов ничего не делает. | [script_engine_script.cpp:361](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L361) (`#ifdef XRGAME_EXPORTS`) |
| `try_load_file(path)` | Функция | Если файла ещё нет в индексе FS и он есть на диске, добавляет его в индекс. В Lua результат не возвращается. | [script_engine_script.cpp:355](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L355) |
| `user_name()` | Функция | Возвращает имя пользователя, сохранённое ядром движка. | [script_engine_script.cpp:347](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L347) |
| `vector4()` | Класс | Создаёт вектор из четырёх нулевых компонентов. Используйте `v:set(x, y, z, w)` для инициализации. | [script_fvector_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L95) |
| `verify_if_thread_is_running()` | Функция | Проверяет наличие активного потока скрипта. Используется в проверке контекста для `coroutine.yield()`. | [script_engine_script.cpp:345](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L345) |

### `FS`

| Имя в Lua | Тип | Поведение | Регистрация |
| --- | --- | --- | --- |
| `FS:rescan_path(alias)` | Метод | Отмечает каталог зарегистрированного FS-алиаса для повторного сканирования. | [fs_registrator_script.cpp:230](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/fs_registrator_script.cpp#L230) |

### `ini_file`

Методы `w_*` принимают все четыре аргумента и ничего не возвращают. Передайте пустую строку в `comment`, если комментарий не задан. Текущий сериализатор это поле не сохраняет. Для замены существующего ключа сначала вызовите `ini:set_override_names(true)`.

| Имя в Lua | Тип | Поведение | Регистрация |
| --- | --- | --- | --- |
| `ini_file:remove_line(section, key)` | Метод | Удаляет существующий ключ из доступного для записи INI. Возвращаемых значений нет. | [script_ini_file_script.cpp:155](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L155) |
| `ini_file:save_as(filename)` | Метод | Записывает текущее содержимое на диск. При непустом `filename` передайте полный путь для сохранения под новым именем. Пустая строка использует уже заданный путь. Возвращает `true` при успехе и `false`, если открыть файл для записи не удалось. | [script_ini_file_script.cpp:153](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L153) |
| `ini_file:save_at_end(enabled)` | Метод | Управляет сохранением доступного для записи объекта при уничтожении. Для немедленной записи вызовите `save_as`. Возвращаемых значений нет. | [script_ini_file_script.cpp:154](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L154) |
| `ini_file:section_count()` | Метод | Возвращает число секций. | [script_ini_file_script.cpp:157](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L157) |
| `ini_file:section_for_each(callback)` | Метод | Вызывает `callback(section_name)` для каждой секции. Верните `true`, чтобы закончить обход, или `false`, чтобы продолжить. Сам метод ничего не возвращает. | [script_ini_file_script.cpp:158](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L158) |
| `ini_file:set_override_names(enabled)` | Метод | При `true` разрешает повторную запись уже существующего ключа через методы `w_*`. Возвращаемых значений нет. | [script_ini_file_script.cpp:156](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L156) |
| `ini_file:set_readonly(enabled)` | Метод | Включает режим только для чтения при `true` и разрешает изменение при `false`. Возвращаемых значений нет. | [script_ini_file_script.cpp:159](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L159) |
| `ini_file:w_bool(section, key, value, comment)` | Метод | Записывает булево значение. Секция и ключ должны существовать. | [script_ini_file_script.cpp:137](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L137) |
| `ini_file:w_color(section, key, value, comment)` | Метод | Записывает упакованный цвет ARGB. Секция и ключ должны существовать. | [script_ini_file_script.cpp:138](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L138) |
| `ini_file:w_fcolor(section, key, value, comment)` | Метод | Записывает цвет `fcolor`. Секция и ключ должны существовать. | [script_ini_file_script.cpp:139](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L139) |
| `ini_file:w_float(section, key, value, comment)` | Метод | Записывает число с плавающей точкой. Секция и ключ должны существовать. | [script_ini_file_script.cpp:140](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L140) |
| `ini_file:w_fvector2(section, key, value, comment)` | Метод | Записывает двумерный вектор `vector2`. Секция и ключ должны существовать. | [script_ini_file_script.cpp:141](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L141) |
| `ini_file:w_fvector3(section, key, value, comment)` | Метод | Записывает трёхмерный вектор `vector`. Создаёт отсутствующую секцию или ключ. | [script_ini_file_script.cpp:142](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L142) |
| `ini_file:w_fvector4(section, key, value, comment)` | Метод | Записывает четырёхмерный вектор `vector4`. Создаёт отсутствующую секцию или ключ. | [script_ini_file_script.cpp:143](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L143) |
| `ini_file:w_s16(section, key, value, comment)` | Метод | Записывает знаковое 16-битное целое. Создаёт отсутствующую секцию или ключ. | [script_ini_file_script.cpp:144](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L144) |
| `ini_file:w_s32(section, key, value, comment)` | Метод | Записывает знаковое 32-битное целое. Создаёт отсутствующую секцию или ключ. | [script_ini_file_script.cpp:145](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L145) |
| `ini_file:w_s64(section, key, value, comment)` | Метод | Записывает знаковое 64-битное целое. Создаёт отсутствующую секцию или ключ. | [script_ini_file_script.cpp:146](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L146) |
| `ini_file:w_s8(section, key, value, comment)` | Метод | Записывает знаковое 8-битное целое. Создаёт отсутствующую секцию или ключ. | [script_ini_file_script.cpp:147](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L147) |
| `ini_file:w_string(section, key, value, comment)` | Метод | Записывает строку. Создаёт отсутствующую секцию или ключ. | [script_ini_file_script.cpp:148](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L148) |
| `ini_file:w_u16(section, key, value, comment)` | Метод | Записывает беззнаковое 16-битное целое. Создаёт отсутствующую секцию или ключ. | [script_ini_file_script.cpp:149](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L149) |
| `ini_file:w_u32(section, key, value, comment)` | Метод | Записывает беззнаковое 32-битное целое. Создаёт отсутствующую секцию или ключ. | [script_ini_file_script.cpp:150](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L150) |
| `ini_file:w_u64(section, key, value, comment)` | Метод | Записывает беззнаковое 64-битное целое. Секция и ключ должны существовать. | [script_ini_file_script.cpp:151](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L151) |
| `ini_file:w_u8(section, key, value, comment)` | Метод | Записывает беззнаковое 8-битное целое. Секция и ключ должны существовать. | [script_ini_file_script.cpp:152](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_ini_file_script.cpp#L152) |

### `net_packet`

| Имя в Lua | Тип | Поведение | Регистрация |
| --- | --- | --- | --- |
| `net_packet:r_object_id()` | Метод | Считывает числовой ID объекта и двигает позицию чтения. | [script_net_packet_script.cpp:155](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L155) |
| `net_packet:r_vec4()` | Метод | Читает четыре числа и возвращает `vector4`. | [script_net_packet_script.cpp:137](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L137) |
| `net_packet:w_object_id(id)` | Метод | Записывает числовой ID объекта. | [script_net_packet_script.cpp:126](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L126) |
| `net_packet:w_s8(value)` | Метод | Записывает знаковое 8-битное целое. | [script_net_packet_script.cpp:114](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L114) |
| `net_packet:w_vec4(value)` | Метод | Записывает четыре компонента объекта `vector4`. | [script_net_packet_script.cpp:105](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_net_packet_script.cpp#L105) |

### `reader`

| Имя в Lua | Тип | Поведение | Регистрация |
| --- | --- | --- | --- |
| `reader:r_object_id()` | Метод | Читает числовой ID объекта из бинарных данных. | [script_reader_script.cpp:115](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L115) |
| `reader:r_vec4(out)` | Метод | Записывает считанные четыре компонента в переданный `vector4`. Возвращаемых значений нет. | [script_reader_script.cpp:110](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L110) |

### `reader_base`

| Имя в Lua | Тип | Поведение | Регистрация |
| --- | --- | --- | --- |
| `reader_base:r_angle16()` | Метод | Читает 16-битный квантованный угол и возвращает его в радианах в диапазоне от `0` до `2π`. | [script_reader_script.cpp:101](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L101) |
| `reader_base:r_angle8()` | Метод | Читает 8-битный квантованный угол и возвращает его в радианах в диапазоне от `0` до `2π`. | [script_reader_script.cpp:102](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L102) |
| `reader_base:r_dir(out)` | Метод | Записывает распакованное направление в переданный `vector`. | [script_reader_script.cpp:103](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L103) |
| `reader_base:r_float()` | Метод | Читает число с плавающей точкой одинарной точности. | [script_reader_script.cpp:90](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L90) |
| `reader_base:r_float_q16(min, max)` | Метод | Читает 16-битное значение и восстанавливает число в диапазоне `min`–`max`. | [script_reader_script.cpp:99](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L99) |
| `reader_base:r_float_q8(min, max)` | Метод | Читает 8-битное значение и восстанавливает число в диапазоне `min`–`max`. | [script_reader_script.cpp:100](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L100) |
| `reader_base:r_s16()` | Метод | Читает знаковое 16-битное целое. | [script_reader_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L96) |
| `reader_base:r_s32()` | Метод | Читает знаковое 32-битное целое. | [script_reader_script.cpp:94](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L94) |
| `reader_base:r_s64()` | Метод | Читает знаковое 64-битное целое. | [script_reader_script.cpp:92](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L92) |
| `reader_base:r_s8()` | Метод | Читает знаковое 8-битное целое. | [script_reader_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L98) |
| `reader_base:r_sdir(out)` | Метод | Читает сжатое направление и масштаб, затем записывает результат в переданный `vector`. | [script_reader_script.cpp:104](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L104) |
| `reader_base:r_u16()` | Метод | Читает беззнаковое 16-битное целое. | [script_reader_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L95) |
| `reader_base:r_u32()` | Метод | Читает беззнаковое 32-битное целое. | [script_reader_script.cpp:93](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L93) |
| `reader_base:r_u64()` | Метод | Читает беззнаковое 64-битное целое. | [script_reader_script.cpp:91](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L91) |
| `reader_base:r_u8()` | Метод | Читает беззнаковое 8-битное целое. | [script_reader_script.cpp:97](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_reader_script.cpp#L97) |

### `save`

| Имя в Lua | Тип | Поведение | Регистрация |
| --- | --- | --- | --- |
| `save.call_error()` | Функция | Вызывает assert для ошибки размера сохранения. В сообщение попадает этап из `save.set_stage`. | [script_engine_script.cpp:376](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L376) |
| `save.set_stage(name)` | Функция | Сохраняет имя текущего этапа для диагностики ошибки размера сохранения. | [script_engine_script.cpp:377](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/script_engine_script.cpp#L377) |

### `vector4`

| Имя в Lua | Тип | Поведение | Регистрация |
| --- | --- | --- | --- |
| `vector4.w` | Поле для чтения и записи | Четвёртый компонент. Доступен для чтения и записи. | [script_fvector_script.cpp:99](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L99) |
| `vector4.z` | Поле для чтения и записи | Третий компонент. Доступен для чтения и записи. | [script_fvector_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L98) |
| `vector4:add(value)`, `vector4:add(a, value)` | Метод | Покомпонентно вычисляет `self + value` или `a + value`, записывает результат в текущий вектор и возвращает его. `value` может быть вектором или числом. | [script_fvector_script.cpp:103](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L103), [script_fvector_script.cpp:104](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L104), [script_fvector_script.cpp:105](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L105), [script_fvector_script.cpp:106](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L106) |
| `vector4:div(value)`, `vector4:div(a, value)` | Метод | Покомпонентно вычисляет `self / value` или `a / value`, записывает результат в текущий вектор и возвращает его. `value` может быть вектором или числом. Делители должны быть ненулевыми. | [script_fvector_script.cpp:115](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L115), [script_fvector_script.cpp:116](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L116), [script_fvector_script.cpp:117](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L117), [script_fvector_script.cpp:118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L118) |
| `vector4:lerp(a, b, t)` | Метод | Записывает покомпонентную интерполяцию от `a` к `b` с коэффициентом `t` и возвращает этот же вектор. | [script_fvector_script.cpp:120](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L120) |
| `vector4:magnitude()` | Метод | Возвращает длину четырёхмерного вектора. | [script_fvector_script.cpp:121](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L121) |
| `vector4:magnitude_sqr()` | Метод | Возвращает квадрат длины вектора. | [script_fvector_script.cpp:122](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L122) |
| `vector4:mul(value)`, `vector4:mul(a, value)` | Метод | Покомпонентно вычисляет `self * value` или `a * value`, записывает результат в текущий вектор и возвращает его. `value` может быть вектором или числом. | [script_fvector_script.cpp:111](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L111), [script_fvector_script.cpp:112](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L112), [script_fvector_script.cpp:113](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L113), [script_fvector_script.cpp:114](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L114) |
| `vector4:normalize()` | Метод | Делит все компоненты на длину ненулевого вектора и возвращает его. | [script_fvector_script.cpp:123](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L123) |
| `vector4:normalize_as_plane()` | Метод | Делит все четыре компонента на длину нормали `(x, y, z)` и возвращает вектор. Нормаль должна быть ненулевой. | [script_fvector_script.cpp:124](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L124) |
| `vector4:similar(other, epsilon)` | Метод | Возвращает `true`, если модуль разности каждой пары компонентов меньше `epsilon`. | [script_fvector_script.cpp:119](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L119) |
| `vector4:sub(value)`, `vector4:sub(a, value)` | Метод | Покомпонентно вычисляет `self - value` или `a - value`, записывает результат в текущий вектор и возвращает его. `value` может быть вектором или числом. | [script_fvector_script.cpp:107](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L107), [script_fvector_script.cpp:108](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L108), [script_fvector_script.cpp:109](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L109), [script_fvector_script.cpp:110](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrScripts/exports/script_fvector_script.cpp#L110) |

## См. также

- [Новые экспорты Lua](../lua-exports.md)
