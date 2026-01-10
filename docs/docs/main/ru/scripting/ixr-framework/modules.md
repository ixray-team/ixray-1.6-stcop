# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

Система фреймворка поддерживает модульную архитектуру, каждый модуль хранит в себе самодостаточную к выполнению логику и не ссылается внутри на другие модули для атомарности и независимости.

Для того чтобы созданный скрипт считался модулем он должен соответствовать интерфейсу:
```lua
function get_module_info()
	return {
		-- (string) имя модуля по которому к нему можно будет обратиться через фреймворк
		alias_name = "modulename",
		-- (string) категория для информативности: ["ixr_system", "sub_system", "sub_gameplay", "sub_utils"] or custom
		category = "sub_system",
		-- (float) версия
		version = 1.1,
		-- (string) имя метода выступающего в качестве точки входа в модуль
		init_function_name = "init",
		-- (string|string[]) строка или массив перечней авторов модуля
		authors = "nickname",
		-- (string) короткое описание модуля для информативности
		description = "text",
	}
end

local is_init = false --// флаг инициализации по умочланию false

--// метод который вызовет фреймворк согласно информации о модуле (здесь можно подписываться на события или заниматься получением данных из маршара или хранилища в зависимости от порядка инициализации модуля)
function init()
  if is_initialized() then
    return true --// предотсвращение повторной инициализации
  end

	is_init = true
  return is_initialized() --// отдаем текущее состояние инициализированности для информирования системы
end

--[[
Description: Проверка инициализации.
Return: (bool) - true если модуль инициализирован
]]
function is_initialized()
	return is_init
end

```

Далее чтобы фреймворк увидел наш модуль его нужно явным образом прописать в переопределяемый файл настроек совместимый с системой аддонов __ixr_override_framework_load_sub_modules.script
```lua
function configure(_ref_ixr_framework)
	-- use concrete script names for include to framework, after module allow by alias name included in module info in module code or script name is included
	--------------------------------------------
	-- Модули загружаемые в первую очередь ->
	--------------------------------------------
	_ref_ixr_framework.load_module_by_script_name("ixr_module_signals") -- alias:[ixr_signals]
	_ref_ixr_framework.load_module_by_script_name("ixr_module_global_registry") -- alias:[ixr_registry]
	_ref_ixr_framework.load_module_by_script_name("ixr_module_options") -- alias:[ixr_options]
	_ref_ixr_framework.load_module_by_script_name("ixr_module_storage") -- alias:[ixr_storage]
	
	--------------------------------------------
	-- Модули загружаемые в середине ->
	--------------------------------------------
	_ref_ixr_framework.load_module_by_script_name("ixr_module_timers") -- alias:[ixr_timers]
	_ref_ixr_framework.load_module_by_script_name("ixr_module_triggers") -- alias:[ixr_triggers]
	
	--------------------------------------------
	-- Модули загружаемые в последнюю очередь ->
	--------------------------------------------
	_ref_ixr_framework.load_module_by_script_name("ixr_module_autoloader") -- alias:[ixr_autoloader] !!! required register this module latest (autoload scripts entry points after register other modules)
end


--// где в качестве названия мы передаем полное имя файла скрипта - далее этот модуль будет доступен через фреймворк по алиасу прописанному внутри метода с информацией о модуле
```

Методы фреймворка для обрщения к модулям выведенные глобально
```lua
--// Проверить загружен ли модуль
IsModuleLoaded(script_or_alias_name)
args:
  script_or_alias_name (string)(required) - имя скрипта или алиас модуля.
retval: (bool) - загружен ли модуль.

--// Получить ссылку на модуль
GetModule(script_or_alias_name)
args:
  script_or_alias_name (string)(required) - имя скрипта или алиас модуля.
retval: (reference|exception) - ссылка на модуль или исключение.

--// Замыкающий вызов при существовании модуля
ClosureModuleIsExists(script_or_alias_name, callback_fn, def_value)
args:
  script_or_alias_name (string)(required) - имя скрипта или алиас модуля.
  callback_fn (function)(required) - функция для вызова если модуль существует.
  def_value (mixed)(required) - значение по умолчанию если модуль не существует.
retval: (mixed|false) - результат callback_fn или def_value.
```

Примеры работы с модулями по именам:
```lua
if IsModuleLoaded("my-module") then
    GetModule("my-module").my_method_in_module() --// какой то известный нам метод внутри модуля
end

--// Замыкающий вызов при существовании модуля
ClosureModuleIsExists("my-module",
  function(module)
    module.my_method_in_module() --// какой то известный нам метод внутри модуля
  end
)

```
