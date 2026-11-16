# Транспорт в Lua

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Методы `game_object` управляют посадкой актёра и выходом из объекта с `CHolderCustom`. Класс `CCar` предоставляет доступ к топливу и состоянию самой машины.

## Синтаксис

| Вызов | Результат |
| --- | --- |
| `actor:attach_vehicle(object, force)` | Посадка в живой объект с `CHolderCustom`. |
| `actor:detach_vehicle(force)` | Выход из текущего транспорта. |
| `actor:get_attached_vehicle()` | Возвращает `game_object` транспорта или `nil`. |
| `object:cast_Car()` | Возвращает `CCar` для автомобиля или `nil`. |
| `car:add_fuel(amount)` | Добавляет топливо до ёмкости бака, возвращает фактически добавленное количество. |
| `car.fuel`, `car.fuel_tank` | Текущее топливо и ёмкость бака. Доступны чтение и запись. |

## Примеры

### Дозаправка машины по ID

```lua
local function refuel_car(car_id, amount)
    local object = level.object_by_id(car_id)
    local car = object and object:cast_Car()
    if car then
        return car:add_fuel(amount)
    end
    return 0
end
```

## Рекомендации и ограничения

- Проверяйте результат `level.object_by_id` перед посадкой. Реализация `attach_vehicle` разыменовывает переданный указатель.
- Используйте `add_fuel` с положительным количеством для дозаправки с учётом ёмкости. Прямая запись `fuel` обходит ограничение бака.

## См. также

- [Настройка транспорта](transport.md)
- [Lua API](../../scripting/new-functions.md)
