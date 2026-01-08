
# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

## Модуль триггеров IXR TRIGGERS
Модуль позволяющий установить на локации сферическую скриптовую зону триггера которая будет реагировать установленным в неё скриптовым методом на вхождение НПЦ Монстров или ГГ в область действия зоны (может быть использовано для сюжетных ветвлений где требуется отследить попадание алайф обьектов в какую то область на локации) служит облегченной версией спейс рестрикторов.

Работает только на обьектах находящихся в онлайне.

Зона триггера записывается в сохранении игры.

```lua
--// Создать триггер (скорость опроса тригера 100мс реагирует на вхождение в область НПЦ или ГГ или Монстров)
TriggerCreate(trigger_name, trigger_level_name, trigger_center_position, trigger_radius, callable_fn)
args: 
  trigger_name (string)(required) - уникальное имя триггера.
  trigger_level_name (string)(required) - имя уровня (карты).
  trigger_center_position (vector)(required) - центральная позиция триггера.
  trigger_radius (int)(required) - радиус триггерной зоны.
  callable_fn (function)(required) - функция обратного вызова (ожидается возвращаемое boolean значение в случае true тригер будет остановлен и удален, в случае false регистрация событий будет продолжаться).
  
closure(callable_fn): - аргументы функция обратного вызова.
  Функция `callable_fn` вызывается при событиях в триггерной зоне с аргументами:
  - level_name (string): имя уровня
  - trigger_name (string): имя триггера
  - trigger_center (vector): центр триггера
  - trigger_radius (int): радиус триггера
  - game_object_entity (entity): объект, вызвавший событие
  - contact_position (vector): позиция контакта
  - is_inside (bool): объект вошел в зону
  - in_zone (bool): объект находится в зоне
  - is_outside (bool): объект вышел из зоны
  - source (string): тип объекта (monster|npc|actor)

retval: (bool) - успешность создания триггера.

--// Проверить существование триггера
TriggerExists(trigger_name)
args:
  trigger_name (string)(required) - имя триггера.
retval: (bool) - существует ли триггер.

--// Удалить триггер
TriggerRemove(trigger_name)
args:
  trigger_name (string)(required) - имя триггера.
retval: (bool) - успешность удаления.
```

Примеры:
```lua

--// выставляем ожидание события новой игры
function on_game_start(callbackRegistrator)
	RegisterScriptCallback("new_game_created", this.on_new_game_started)
end

function on_new_game_started()
  --// Создаем триггер радиусом 5 метров для локации кордон (делать это достаточно 1 раз например в момент начала новой игры т.к триггеры сохраняются в сейв игры и не требуют постоянного повторого обьявления)
  TriggerCreate(
    "test-trg", 
    "l01_escape", 
    vector():set(-92, 0.67176, 677), 
    5.0, 
    function (level_name, trigger_name, trigger_center, trigger_radius, game_object_entity, contact_position, is_inside, in_zone, is_outside, source)
      ...
    end
  )

--// прочие триггеры далее
end

--// Проверить существование триггера
if TriggerExists("test-trg") then
  ...
end

--// Удалить триггер
if TriggerRemove("test-trg") then
  ...
end


```
