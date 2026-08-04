# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

### ffx_cache_utils: `\gamedata\scripts\ixr_framework\utils\ffx_cache_utils.script`
Утилиты для кеширования данных:
* `get_cached`
* `invalidate_cache`

#### Описание методов:

```lua
--// Получить значение из кеша. Если ключ отсутствует или записи устарела, вызывается fn_closure для получения свежего значения, которое затем сохраняется и возвращается.
get_cached(key_name, fn_closure, time_invalidate, default_value, skip_errors)
args:
  key_name (string)(required) - ключ кеша.
  fn_closure (function)(required) - функция, возвращающая значение для кеширования.
  time_invalidate (number)(optional) - время жизни кеша в миллисекундах (по умолчанию 60000).
  default_value (any)(optional) - значение по умолчанию, возвращаемое при ошибке, если skip_errors = true.
  skip_errors (boolean)(optional) - если true, подавляет ошибки валидации (по умолчанию false).
retval: (any) - закешированное или свежевычисленное значение; при ошибке и skip_errors = true возвращает default_value.

--// Инвалидировать (удалить) закешированное значение по ключу.
invalidate_cache(key_name)
args:
  key_name (string)(required) - ключ кеша для удаления.
retval: (none)
```

Примеры использований:
```lua
--// Получаем данные, кешируем на 30 секунд (30000 мс)
local data = ffx_cache_utils.get_cached("user_profile", function()
    return load_user_profile()
end, 30000)

--// Инвалидируем кеш после обновления профиля
ffx_cache_utils.invalidate_cache("user_profile")

--// Использование с обработкой ошибок (подавляем ошибки, возвращаем "default")
local result = ffx_cache_utils.get_cached("expensive_calc", expensive_function, nil, 0, true)

--// Получаем данные с явным указанием skip_errors = false (по умолчанию)
local value = ffx_cache_utils.get_cached("settings", load_settings, 60000, nil, false)
```
