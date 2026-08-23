# LTX Логика в квестах
> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.5

Добавлен механизм, при котором квест можно разбить на несколько секций с поддержкой .ltx логики. Это будет полезно при создании большого квеста, который можно логически разбить на набор состояний.

Для активации фичи необходимо в конфиге квеста выставить ключ `use_quest_logic = true`, а после по ключу `logic` вписать имя первой секции.
Пример квеста:

```ini
[quest_logic]
icon = some_icon
prior = 100
use_quest_logic = true
storyline = true
logic = state_1

[state_1]
title = {+title_3} state_3_title, state_1_title ; можно как и в оригинальных делать такой кондлист
descr = state_1_descr
target = state_1_target
condlist_0 = {+some_info_2} complete ; можно прописать условия выхода
on_info = {+some_info_1} state_2
on_timer = 1000 | state_2
; Поддерживаются on_info, on_timer, on_actor_in_zone, on_actor_not_in_zone, on_npc_in_zone, on_npc_not_in_zone

[state_2]
title = state_2_title
descr = state_2_descr
target = state_2_target
condlist_0 = {+some_info_2} complete
```
