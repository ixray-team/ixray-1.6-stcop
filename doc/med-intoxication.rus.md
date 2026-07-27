# Медикаментозная интоксикация

> [!IMPORTANT] Статус: Поддерживается Минимальная версия: IX-Ray 1.X
>
>  Последнее обновление: 2026-07-28

## Обзор

Медикаментозная интоксикация это опциональная система состояния актора, которая моделирует перегрузку организма препаратами и химией.

Шкала интоксикации лежит в диапазоне от `0.0` (чисто) до `1.0` (максимум). Предметы с параметром `eat_intoxication` поднимают или снижают шкалу при использовании. Чем выше значение, тем сильнее штраф к стамине, тем раньше начинается урон по здоровью и тем слабее работает лечение аптечками.

Система выключена по умолчанию. Для игрока она должна читаться как разделение ролей:

- аптечки лечат и не обязаны быть источником интоксикации;
- препараты и антирад дают быстрый эффект ценой накопления интоксикации;
- антидот и напитки снимают интоксикацию;
- медик через диалог "мне нужна медицинская помощь" полностью очищает интоксикацию вместе с обычным лечением.

Пример использования в одном предложении: включите `EnableMedIntoxication`, задайте `eat_intoxication` на препаратах и `intoxication_heal_min` на аптечках через DLTX.

## Включение

Механика активируется флагом в `engine_external.ltx` или DLTX поверх него:

```ini
![gameplay]
EnableMedIntoxication = true
```

При `false`:

- параметры `actor_condition` для интоксикации не читаются в рабочий путь;
- `ChangeIntoxication` ничего не делает;
- штрафы к лечению, стамине, HP, спринту и хромоте не применяются;
- UI интоксикации не создается и не обновляется, даже если XML-ноды присутствуют.



## Геймплейная модель



### Шкала


| Значение `I`      | Смысл                                                              |
| ----------------- | ------------------------------------------------------------------ |
| `0.0`             | Нет интоксикации                                                   |
| `0.0 .. critical` | Легкая нагрузка: есть сток стамины, нет штрафа лечения и DoT по HP |
| `> critical`      | Штраф эффективности лечения и DoT по HP                            |
| `>= 0.7`          | Тяжелая передозировка: блок спринта, cam-effector                  |
| `> 0.9`           | Критическая передозировка: хромота, PP-effector, усиленный DoT     |


`critical` настраивается (`intoxication_critical`, по умолчанию `0.35`).
Пороги `0.7` и `0.9`, а также множители DoT `1.5` / `2.5`, заданы в коде и через LTX не меняются.

### Как растет и падает

Интоксикация меняется так:

1. При использовании предмета: `I += eat_intoxication`.
2. Каждый кадр: `I += intoxication_v * dt` (обычно отрицательный спад).
3. Опционально при жажде: дополнительный детокс от `eat_thirst` (см. раздел про thirst).
4. Значение всегда clamp в `[0.0 .. 1.0]`.
5. Сохраняется в сейв актора.



### Рекомендуемые роли предметов


| Роль                          | Типичная настройка                                     | Игровой смысл                                                |
| ----------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| Аптечки                       | `eat_intoxication = 0`, разный `intoxication_heal_min` | Лечат всегда, при сильной интоксикации дешевые работают хуже |
| Химия / стимуляторы / антирад | `eat_intoxication > 0`                                 | Быстрый эффект ценой накопления                              |
| Антидот                       | `eat_intoxication = -1.0`                              | Полный сброс                                                 |
| Напитки                       | `eat_intoxication < 0`                                 | Дешевый медленный детокс                                     |
| Бинты                         | без параметра или `0`                                  | Не участвуют в химии                                         |


Такое разделение помогает игроку быстро понять систему: лечиться аптечками безопасно по интоксикации, а злоупотреблять стимуляторами уже нет.

## Эффекты по системам



### Стамина (сила)

При любом `I > 0`:

```text
delta_power += intoxication_power_v * I * dt
```

`intoxication_power_v` обычно отрицательный. Чем выше интоксикация, тем быстрее падает стамина даже в покое относительно этой формулы.

Пример при `intoxication_power_v = -0.015`:

- `I = 0.5` -> примерно `-0.0075` силы в секунду;
- `I = 1.0` -> примерно `-0.015` силы в секунду.



### Здоровье

Только при `I > critical`:

```text
excess = (I - critical) / (1 - critical)
healthMul = 1.0
если I >= 0.7: healthMul = 1.5
если I > 0.9: healthMul = 2.5
delta_health += intoxication_health_v * excess * healthMul * dt
```

`intoxication_health_v` обычно отрицательный.

### Эффективность лечения

Штраф применяется только к положительному лечению HP:

- `eat_health > 0` в `ApplyInfluence`;
- `boost_health_restore > 0` в `ApplyBooster` (важно для модов вроде Gunslinger, где аптечки лечат бустером).

Не затрагиваются:

- кровотечение (`boost_bleeding_restore` / bleeding);
- радиация;
- сила / еда / жажда и прочие параметры предмета.

Формула:

```text
если I <= critical:
    factor = 1.0
иначе:
    t = saturate((I - critical) / (1 - critical))
    kMin = intoxication_heal_min предмета (дефолт 0.25, clamp 0..1)
    factor = 1.0 + (kMin - 1.0) * t
```

При максимальной интоксикации `factor = kMin`.
При `I = critical` штраф еще не начинается.
При `I = 1.0` лечение урезано до минимума предмета.

Пример при `critical = 0.35` и `I = 1.0`:


| Предмет               | `intoxication_heal_min` | Эффективность heal |
| --------------------- | ----------------------- | ------------------ |
| Обычная аптечка       | `0.30`                  | 30%                |
| Армейская             | `0.60`                  | 60%                |
| Научная               | `0.90`                  | 90%                |
| Предмет без параметра | дефолт `0.25`           | 25%                |




### Передвижение и эффекторы


| Условие    | Эффект                                             |
| ---------- | -------------------------------------------------- |
| `I >= 0.7` | Нельзя спринтать                                   |
| `I >= 0.7` | Cam-effector из секции `[effector_intoxication]`   |
| `I > 0.9`  | Хромота                                            |
| `I > 0.9`  | PP-effector из секции `[effector_intoxication_pp]` |


Интенсивность эффекторов завязана на текущее значение `GetIntoxication()`.

### Медик

Стандартный диалог помощи (`dialogs.medic_magic_potion` / `actor_needs_bless`) может учитывать интоксикацию:

- фраза помощи доступна, если `db.actor.intoxication > 0`, даже при полном HP;
- лечение медиком сбрасывает интоксикацию через `db.actor.intoxication = -1` (полный clear, аналогично радиации).

Это делается через скрипт.

## Синтаксис и параметры



### Глобальный флаг

Файл: `engine_external.ltx` / DLTX

```ini
![gameplay]
EnableMedIntoxication = true
```



### Секция актора `![actor_condition]`


| Параметр                | Тип          | Дефолт движка | Смысл                                            |
| ----------------------- | ------------ | ------------- | ------------------------------------------------ |
| `intoxication_critical` | float `0..1` | `0.35`        | Порог начала штрафа лечения и DoT по HP          |
| `intoxication_v`        | float / сек  | `-0.00025`    | Естественное изменение шкалы каждый кадр         |
| `intoxication_power_v`  | float / сек  | `-0.005`      | Множитель стока стамины: `* I`                   |
| `intoxication_health_v` | float / сек  | `-0.0015`     | Множитель DoT HP: `* excess * healthMul`         |
| `intoxication_thirst_k` | float        | `0.3`         | Доп. детокс от `eat_thirst` при включенной жажде |


Пример:

```ini
![actor_condition]
intoxication_v          = -0.00025
intoxication_critical   = 0.35
intoxication_health_v   = -0.0012
intoxication_power_v    = -0.015
intoxication_thirst_k   = 0.0
```

> [!NOTE]
> Для независимого детокса напитками рекомендуется `intoxication_thirst_k = 0` и прямой `eat_intoxication < 0` на напитках. Тогда поведение не зависит от `EnableThirst`.



### Параметры предметов

Читаются из секции используемого предмета:


| Параметр                | Тип          | Дефолт | Смысл                                        |
| ----------------------- | ------------ | ------ | -------------------------------------------- |
| `eat_intoxication`      | float        | `0.0`  | Сколько добавить/снять при use               |
| `intoxication_heal_min` | float `0..1` | `0.25` | Минимальная эффективность heal при `I = 1.0` |


Примеры:

```ini
![medkit]
eat_intoxication = 0.0
intoxication_heal_min = 0.30

![drug_booster]
eat_intoxication = 0.15

![drug_antidot]
eat_intoxication = -1.0

![water]
eat_intoxication = -0.10
```



### Связь с жаждой (опционально)

Если одновременно:

- `EnableMedIntoxication = true`;
- `EnableThirst = true`;
- у предмета `eat_thirst > 0`;
- `intoxication_thirst_k > 0`;

то при apply дополнительно:

```text
eat_intoxication_effective -= eat_thirst * intoxication_thirst_k
```

Если напиток уже имеет отрицательный `eat_intoxication`, и `intoxication_thirst_k > 0`, детокс может суммироваться. Обычно выбирают один путь.

### Эффекторы

Обычные секции без `!` (создаются один раз, не дублируйте их в нескольких LTX):

```ini
[effector_intoxication]
cam_eff_name   = camera_effects\drunk.anm
cam_eff_cyclic = 1

[effector_intoxication_pp]
pp_eff_name     = alcohol.ppe
pp_eff_cyclic   = 1
pp_eff_overlap  = true
```

> [!WARNING]
> Дубликат обычной секции `[effector_intoxication]` в базовом `gamedata` и аддоне приводит к fatal при загрузке LTX. Держите эффекторы в одном месте.



## UI и строки



### HUD

Нода `indicator_intoxication` в maingame XML (через XML Override). Пороги отображения:


| `I`       | Поведение иконки           |
| --------- | -------------------------- |
| `<= 0.05` | Скрыта                     |
| `< 0.35`  | Зеленая, медленное мигание |
| `< 0.7`   | Желтая, среднее мигание    |
| `>= 0.7`  | Красная, быстрое мигание   |




### Инвентарь актора

Нода `intoxication_state` в actor menu XML. Три уровня иконки по тем же смысловым порогам.

### Тултип предмета

Нода `boost_intoxication` в `booster_params` XML.
В тултипе **не показываются точные цифры** `eat_intoxication`. Показывается качественное описание:


| Условие значения | Строка                            |
| ---------------- | --------------------------------- |
| `<= -0.9`        | `ui_inv_intoxication_clear`       |
| `< 0`            | `ui_inv_intoxication_reduce`      |
| `<= 0.12`        | `ui_inv_intoxication_raise_light` |
| `<= 0.20`        | `ui_inv_intoxication_raise`       |
| `> 0.20`         | `ui_inv_intoxication_raise_heavy` |


Также нужны базовые строки:

- `ui_inv_intoxication`
- `st_ui_intoxication_sensor`



## Lua API

Свойство актора:

```lua
local i = db.actor.intoxication      -- get, 0..1
db.actor.intoxication = -1           -- ChangeIntoxication(-1), полный clear
db.actor.intoxication = 0.2          -- добавить 0.2
```

Сеттер свойства работает как `Change*`, не как абсолютный set. Для полного сброса используйте `-1`.

На `CEntityCondition` также экспортированы:

- `ChangeIntoxication(value)`
- `GetIntoxication()`

Выражение UI:

- `fltPlayerIntoxication`

Пример обертки медика:

```lua
function on_game_start()
	local original_potion = dialogs.medic_magic_potion
	local original_needs = dialogs.actor_needs_bless

	dialogs.medic_magic_potion = function(first_speaker, second_speaker)
		original_potion(first_speaker, second_speaker)
		if db.actor then
			db.actor.intoxication = -1
		end
	end

	dialogs.actor_needs_bless = function(first_speaker, second_speaker)
		if original_needs(first_speaker, second_speaker) then
			return true
		end
		return db.actor ~= nil and db.actor.intoxication > 0
	end

	dialogs.actor_is_damn_healthy = function(first_speaker, second_speaker)
		return not dialogs.actor_needs_bless(first_speaker, second_speaker)
	end
end
```



## Примеры использования



### Сценарий 1: Минимальное включение

```ini
; mod_engine_external_my_intox.ltx
![gameplay]
EnableMedIntoxication = true

; mod_system_my_intox.ltx
![actor_condition]
intoxication_v = -0.00025
intoxication_critical = 0.35
intoxication_health_v = -0.0012
intoxication_power_v = -0.015
intoxication_thirst_k = 0.0

![drug_booster]
eat_intoxication = 0.15

![drug_antidot]
eat_intoxication = -1.0

![medkit]
eat_intoxication = 0.0
intoxication_heal_min = 0.30
```

Результат: химия копит интоксикацию, аптечка не травит, при сильной интоксикации лечит слабо, антидот очищает полностью.

### Сценарий 2: Тир аптечек

```ini
![medkit]
eat_intoxication = 0.0
intoxication_heal_min = 0.30

![medkit_army]
eat_intoxication = 0.0
intoxication_heal_min = 0.60

![medkit_scientic]
eat_intoxication = 0.0
intoxication_heal_min = 0.90
```

Результат: при max intox научная почти сохраняет эффект, обычная становится слабой. Это оправдывает стоимость тиров.

### Сценарий 3: Напитки как детокс без жажды

```ini
![water]
eat_intoxication = -0.10

![energy_drink]
eat_intoxication = -0.04

![actor_condition]
intoxication_thirst_k = 0.0
```

Результат: вода всегда снимает интоксикацию, даже если `EnableThirst = false`.

### Сценарий 4: Жесткий режим

```ini
![actor_condition]
intoxication_critical = 0.25
intoxication_v = -0.00010
intoxication_power_v = -0.025
intoxication_health_v = -0.0020

![antirad]
eat_intoxication = 0.20

![drug_anabiotic]
eat_intoxication = 0.40
```

Результат: быстрее доходите до штрафов, медленнее выходите естественно, стамина и HP давят сильнее.

## Рекомендации

Правильное использование:

- Баланс только через DLTX и XML Override. Legacy файлы `gamedata` не редактировать.
- Аптечкам задавайте `eat_intoxication = 0` и тир через `intoxication_heal_min`.
- Химии задавайте положительный `eat_intoxication`.
- Антидоту давайте `-1.0`, если хотите полный экстренный сброс.
- Напиткам давайте прямой отрицательный `eat_intoxication`, если детокс часть этой механики.
- Эффекторы держите в одном LTX-файле аддона.

Ограничения:

- Пороги `0.7` / `0.9` и множители DoT `1.5` / `2.5` зашиты в C++.
- Штраф лечения не трогает bleeding/radiation restore.
- `db.actor.intoxication = X` это изменение на дельту, не абсолютная установка текущего значения.
- Без UI Override и строк игрок не увидит индикаторы и описания в тултипе, хотя логика уже работает.
- CoP является целевой веткой. CS/SOC не являются приоритетом этой механики.

Анти-паттерны:

- Давать всем аптечкам высокий `eat_intoxication`. Игрок перестает понимать, за что его наказывают.
- Одновременно включать thirst-детокс и сильный отрицательный `eat_intoxication` на тех же напитках без пересчета.
- Дублировать `[effector_intoxication]` в нескольких файлах.
- Показывать сырые проценты интоксикации в тултипе предмета. Используйте качественные строки.
- Править vanilla `dialogs.script` в `gamedata`. Оборачивайте функции из аддон-скрипта.



## Что система намеренно не делает

- Не меняет точность стрельбы отдельным множителем.
- Не является заменой алкоголю. Алкоголь остается отдельной шкалой.
- Не требует community `medic`. Используются существующие диалоги медпомощи.
- Не лечит интоксикацию фактом восстановления HP. Нужен антидот, напиток, естественный спад или медик.



## Связанные разделы

- Флаг: `gamedata/configs/engine_external.ltx` -> `EnableMedIntoxication`
- Состояние актора: `src/xrGame/ActorCondition.cpp` (`UpdateIntoxication`, `GetMedicineEfficiencyFactor`, `ApplyInfluence`, `ApplyBooster`)
- Чтение `eat_intoxication`: `src/xrGame/EntityCondition.cpp`
- UI тултипа: `src/xrGame/ui/UIBoosterInfo.cpp`
- HUD: `src/xrGame/ui/UIMainIngameWnd.cpp`
- Инвентарь: `src/xrGame/ui/UIActorStateInfo.cpp`
- Lua-свойство: `db.actor.intoxication`
- Эталонный аддонный баланс и UI: `RenewedPerceptionGunslingerMod` (`mod_system_med_intoxication.ltx`, XML Override, `ixr_med_intoxication.script`)

