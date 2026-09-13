
# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

## Модуль таймеров IXR THROTTLERS
Позволяет ограничивать переодичность вызовов для оптимизации кода (заменяет простейшие таймеры отсечки кода)

```lua
--// Проверка запрещён ли вызов с учетом тротлинга
IsActionThrottled(name, interval_ms)
args:
  name (string)(required) - Уникальное имя тротлера.
  interval_ms (int)(required) - Тайминг замера повторов в милисекундах
retval: (bool) (возвращает true если вызов производится чаще чем задано вторым аргументом)


--// Проверка разрешен ли вызов с учетом тротлинга (метод аналогичный тому что выше но инвертированный)
IsNotActionThrottled(name, interval_ms)
args:
  name (string)(required) - Уникальное имя тротлера.
  interval_ms (int)(required) - Тайминг замера повторов в милисекундах
retval: (bool) (возвращает true если вызов производится чаще чем задано вторым аргументом)
```

Примеры:
```lua
--// Подписываемся на обдейт актора в качестве примера
function on_game_start(callbackRegistrator)
	RegisterScriptCallback("actor_on_update", actor_on_update)
end

--// Обработчик обновления актора (в качестве примера сделаем метод который тикает раз в 250 мс)
function actor_on_update()
  if IsActionThrottled("my_test_throttler_01", 250) then
    return --// Прерываем метод если часотота обновлений чаще чем раз в 250 милисекунд
  end
  
  lazy_tick()
end

--// Пример вызова который выполняется раз в 250 мс
local n=0
function lazy_tick()
  n = n +1
  SemiLog("TICK:"..tostring(n))
end
```
