# Система скриптовых коллбэков
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.0

## Система скриптовых коллбэков

* Таблица `intercepts` принимает в себя название функций, активируемых по коллбэку;
* Функция `RegisterScriptCallback` регистрирует коллбэк;
* Функция `UnregisterScriptCallback` отменяет регистрацию коллбэка;
* Функция `SendScriptCallback` посылает коллбэк.

Примерный сценарий использования:

```lua
-- доступные типы коллбеков
local intercepts = {
	save = {},
	load = {},
	update = {},

	save_state = {},
	load_state = {}
}
```

```lua
-- скрипт
function DoSomething()
    -- ...
    SendScriptCallback("save_state")
end 

function DoSomething2()
    -- ...
    SendScriptCallback("load_state")
end 
```

```lua
-- биндер
function actor_binder:reinit()
    -- ...
    RegisterScriptCallback("save_state", self)
end

function actor_binder:net_destroy()
    -- ...	
    UnregisterScriptCallback("save_state", self)
end

function actor_binder:load(reader)
    -- ...
    self:load_state() -- фейковый вызов ввиду неготовности биндера во время совершения коллбэка
end

function actor_binder:save_state()
    -- ...
end

function actor_binder:load_state()
    -- ...
end
```
