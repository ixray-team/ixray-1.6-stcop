# Взрывчатка
## CExplosive
### Следы от взрыва
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.1

```ini
wallmark_section = explosion_marks; Установка wallmark'и от взрыва (указывается секция из конфига)
```

* Следующие настройки звуков работаю в системе `Sound Layers`:
* * `snd_explode`

### Газовые гранаты
> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.4
```ini
; Основной флаг газового взрыва
is_gas_explosive           = true/false    ; Включить газовый взрыв (по умолчанию false)

; Тайминги (в миллисекундах)
blast_update_time          = 2000          ; Интервал поиска новых целей для газового взрыва
particles_update_time      = 500           ; Интервал обновления частиц

; Callback-функции для актора
actor_blast_begin_callback = callback_func ; Функция вызываемая в начале обработки актора
actor_blast_end_callback   = callback_func ; Функция вызываемая в конце обработки актора
```

::: details Примеры callback'ов 
```lua
-- Функция проверки в начале воздействия
-- Должна возвращать true/false - разрешить/запретить дальнейшую обработку
function check_gas_mask()
    local actor = db.actor
    if actor:item_in_slot(6) ~= nil then  -- Проверка противогаза в слоте 6
        local helmet = actor:item_in_slot(6)
        if helmet:section() == "helm_gasmask" then
            return false  -- Противогаз есть, отменить эффект
        end
    end
    return true  -- Продолжить обработку
end

-- Функция применяемая в конце воздействия
function apply_gas_effect()
    local actor = db.actor
    actor:radiation_inc(0.05)  -- Небольшая доза радиации
    actor:set_health(actor:get_health() - 0.1)  -- Урон здоровью
    
    -- Можно добавить визуальные эффекты
    local ps = get_console():execute("ps")  -- Получить партикл-систему
    -- ... логика эффектов ...
end

-- Полная обработка взрыва на акторе
function actor_gas_poison()
    -- Проверка защиты
    if not check_gas_mask() then
        return
    end
    
    -- Применение эффектов
    apply_gas_effect()
    
    -- Дополнительная логика
    printf("[GAS] Актор отравлен газом!")
end
```
:::

## CGrenade
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.1
```ini
explosion_on_hit = true; Взрыв при получении урона
explosion_hit_types = 6, 8 ; Типы урона 
```
