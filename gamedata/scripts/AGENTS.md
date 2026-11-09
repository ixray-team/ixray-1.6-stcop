# Карта игровых Lua-скриптов IX-Ray

Эта инструкция охватывает `gamedata/scripts/` и вложенные папки. Это карта существующей реализации для ИИ-агентов: изучение кода не является запросом на его исправление или переработку. Для конкретной области читай вложенные инструкции перед изменениями.

## Пользовательская документация и примеры

- [Правила написания и обновления документации Framework](../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/AGENTS.md) — оформление функций, аргументы и результаты, примеры, версии и навигация.

- [Общее описание](../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/general-info.md)
- [Структура файлов](../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/files.md)
- [Шаблон и подключение модулей](../../IX-RAY/docs/docs/main/ru/scripting/ixr-framework/modules.md)

Начинай с документации нужного API и примеров, затем сверяй сигнатуры, значения и жизненный цикл с текущей реализацией. Если старая формулировка расходится с актуальными правилами владельца проекта (например, допускает имя файла модуля вместо alias), соблюдай актуальные правила из родительских AGENTS.md. Само обнаружение расхождения не является задачей на изменение скриптов или документации. Минимальную версию проверяй на конкретной странице.

## Расширение без изменения ядра Framework

Модифицировать существующий код Framework **не рекомендуется**. Если прикладная задача, на первый взгляд, требует такой правки, сначала пересмотри решение: вероятнее всего, нужно использовать публичное API/события, создать подходящий управляемый модуль либо новое событие, зарегистрировав его в предназначенном для этого публичном файле `__ixr_override_signals_intercepts.script`.

Порядок выбора решения:

1. Используй существующие глобальные функции и API, явно предоставленный обратными вызовами.
2. Для отдельной ответственности создай модуль по принятому шаблону и подключи его в нужном месте `__ixr_override_framework_load_sub_modules.script`.
3. Если требуется новое имя события, зарегистрируй его только в `__ixr_override_signals_intercepts.script`.
4. Если решение всё ещё требует правки ядра, проверь постановку и архитектуру задачи. Не выбирай изменение существующих модулей, компонентов, глобальных обёрток или внутренних контрактов как обычный способ реализации игровой функции.

Это рекомендация по архитектуре, а не абсолютный запрет: обоснованная задача на развитие или исправление самого Framework может требовать таких изменений. В этом случае объясни, почему публичных точек расширения недостаточно, и ограничь правку необходимым объёмом. Само наличие такой необходимости не создаёт дополнительного требования запрашивать разрешение, если работа уже явно поставлена пользователем.

## Граница доступа игрового кода к Framework

В игровых скриптах вне `ixr_framework/` доступ к сервисам Framework предоставляется только через:

1. Глобальные методы, определённые в `ixray_system/global.lua`.
2. Глобальные методы системы событий из `ixray_system/dynamic_callbacks.lua`.
3. Данные и API, явно предоставленные реализацией через обратные вызовы: их аргументы и предусмотренные контрактом результаты. Например, `on_init_ixr_options` передаёт `_ref` с API регистрации опций.

Если для задачи есть готовая глобальная обёртка (`TimerCreate`, `GetStorageVar`, `GetOptionsVar`, `IXRLog` и т. п.), используй её либо API, явно переданный предусмотренным callback. Наличие глобального аналога не требует заменять легитимную работу через API события. Не обходи публичный вход прямым вызовом `ixr_framework.get_module`, `get_framework`, обращением к неймспейсу реализации модуля, `_G[script_name]`, приватным локалкам или внутренним таблицам. Техническая доступность этих деталей не делает их пользовательским API. Не расширяй полученный в callback доступ исследованием других внутренних полей.

### Пример легитимного доступа: регистрация опций

Образец — [ixr_options_template.script](example_options/ixr_options_template.script). В `on_game_start` скрипт задаёт метаданные страницы глобальными `SetOption*` и устанавливает замыкание через `RegisterScriptCallback("on_init_ixr_options", function(_ref) ... end)`.

После окончания автозагрузки Framework отправляет событие регистрации опций. Замыкание получает `_ref` и использует предоставленные методы `_ref.register_option`, `_ref.check_box`, `_ref.combo_box` и другие. Вложенные обработчики могут захватывать `_ref` и использовать предусмотренное API, как это делает реализация опций. Это полноценный публичный путь, а не попытка получить модуль в обход глобалок. Не заменяй такой пример прямым поиском модуля или требованием вызывать исключительно глобальные аналоги.

### Управляемые модули: только alias

Когда публичный контракт предусматривает получение или проверку модуля, используй только `alias_name` из `get_module_info()`:

```lua
local timers = GetModule("ixr_timers")
local loaded = IsModuleLoaded("ixr_timers")
```

Правило действует и для `ClosureModuleIsExists`. `GetModule("ixr_module_timers")` и прямое `ixr_module_timers.timer_create(...)` категорически запрещены. Наличие `GetModule` не является поводом обходить готовую глобальную обёртку без необходимости.

Имена файлов используются для подключения модулей в `__ixr_override_framework_load_sub_modules.script` через предоставленный `configure` API `load_module_by_script_name`. Сам системный глобальный слой, движковый мост и явно предусмотренные конфигурационные callbacks реализуют эти точки доступа; их внутренние вызовы не являются образцом для копирования в обычный игровой код. Контракты общего API утилит описаны отдельно в соответствующих вложенных инструкциях.

## Навигация

| Инструкция | Область |
| --- | --- |
| [Системные глобалки](ixray_system/AGENTS.md) | Загрузка глобального API движком, доступ к модулям и событиям, LuaSocket, LuaPanda. |
| [Корень Framework](ixr_framework/AGENTS.md) | Полная карта фреймворка, его запуск и связи между каталогами; содержит ссылки на все вложенные инструкции. |
| [Модули](ixr_framework/modules/AGENTS.md) | Обязательный шаблон, состав/порядок инициализации, автозагрузка, сигналы, хранилище, таймеры, триггеры, опции. |
| [Расширение событий](ixr_framework/extend_callbacks/AGENTS.md) | Новые и составные события на базе существующих, ожидание условий и общие результаты. |
| [Публичные компоненты](ixr_framework/public_components/AGENTS.md) | Помощники для разработчиков модов, контекстное логирование и Tracy. |
| [Системные компоненты](ixr_framework/system_components/AGENTS.md) | Внутренние помощники фреймворка и оригинальных скриптов, движковые callbacks биндеров. |
| [Настройки команды IX-Ray](ixr_framework/ixray_options/AGENTS.md) | Опции самого проекта; пользовательский шаблон находится отдельно. |
| [Переиспользуемое API утилит](ixr_framework/utils/AGENTS.md) | Общие решения для модов, оригинальных скриптов и фреймворка. |
| [Библиотеки утилит](ixr_framework/utils/libs/AGENTS.md) | Готовые JSON и Base64 API, правила совместимости форматов. |

Для общего знакомства начни с системных глобалок и корня Framework, затем пройди дочерние инструкции. Ссылки выше позволяют открыть каждую существующую инструкцию непосредственно из этой карты.

## Запуск и точки настройки Framework

- `../configs/alife.ltx`, секция `[alife]`: `start_game_callback = ixr_on_start_game_callback` переопределяет входную точку после загрузки локации.
- `ixray_system/global.lua:ixr_on_start_game_callback()` сначала вызывает оригинальную `_G.start_game_callback()`, затем `ixr_framework.init()`. Сохраняй порядок и оригинальный вызов.
- `__ixr_override_framework_load_sub_modules.script` определяет **набор и порядок инициализации** модулей. Мододел может вставить собственный модуль в нужное место списка. Автозагрузчик подключается последним.
- `__ixr_override_autoload_system.script` задаёт фильтр первичного сканирования скриптов: исключает ненужные и опасные для проверки файлы до обращения к `_G[script_name]` и проверки `on_game_start`.
- Обращение к `_G` может запустить загрузку скрипта и привести к критическому сбою для отдельных файлов с классами. Известный пример от владельца проекта — `ce_new_attachable_item.script`; сохраняй базовые исключения, включая `ce_*`, и пользовательские фильтры. Для анализа читай исходник, не загружай его пробным обращением.
- `__ixr_override_signals_intercepts.script` — **единственное место регистрации новых имён коллбеков**. В других файлах используй существующие глобальные функции подписки/отписки/отправки; не расширяй default-каталог или локальные реестры новыми именами.
- `__storage_mod_version.script` предоставляет `get_version()` для данных хранилища. Не меняй версию или формат данных при составлении карты.
- `example_options/ixr_options_template.script` — пример опций для разработчиков модов. `ixr_framework/ixray_options/ixray_global_options.script` — опции команды проекта.

## Обвязки аддонов над ванильными скриптами через callbacks

Система коллбеков используется не только для уведомлений. Она предоставляет интеграционные точки для изменения поведения: общий игровой код отправляет событие в нужном месте, а отдельный скрипт аддона подписывается на него, меняет переданный контекст по ссылке или выполняет дополнительную логику.

Цель — сохранить общую кодовую базу: в ванильной функции остаётся небольшая универсальная точка расширения, а специфические условия и реализация аддона не попадают в её тело и не требуют копировать всю функцию в отдельную модифицированную версию.

### Пример: изменение цены улучшения

В `inventory_upgrades.script:get_upgrade_cost` после расчёта цены уже есть такая точка:

```lua
local flags_table = {new_price = price}
SendScriptCallback("inv_upgrades_get_upgrade_cost_override_price", param_ini, char_ini, cur_price_percent, section, flags_table)
price = flags_table.new_price
```

Отдельный скрипт аддона устанавливает обвязку один раз при автозагрузке:

```lua
function on_game_start()
	RegisterScriptCallback("inv_upgrades_get_upgrade_cost_override_price", function(param_ini, char_ini, cur_price_percent, section, flags_table)
		-- Дополнительная скидка 10% к рассчитанной цене.
		flags_table.new_price = math.floor(flags_table.new_price * 0.9)
	end)
end
```

Исходная функция продолжает работу и использует изменённую цену. Событие в этом примере уже зарегистрировано. Если подписчиков нет, `new_price` сохраняет исходное значение и стандартная логика продолжается. Наличие точки расширения не обязывает аддон получать внутренний модуль или читать приватные локалки Framework.

### Какие варианты уже используются

| Исходник | Интеграция |
| --- | --- |
| `inventory_upgrades.script` | Подписчик меняет `new_price`, `new_money`, `new_value` или `new_perc`; код после отправки использует изменённое поле. Отдельное событие после списания денег позволяет выполнить дополнительный код. |
| `death_manager.script` | В `overriden_iterator_functor`/`override_spawn_items_functor` можно записать функцию. Исходный код выбирает её вместо стандартной ветки и передаёт контекст, локальные данные и ссылки на необходимые функции-помощники. |
| `xr_motivator.script` | `override_position_logic` позволяет предоставить функцию вместо стандартной установки позиции NPC; она получает биндер, smart terrain, игровой и серверный объекты. |
| `surge_manager.script` | `on_before_surge` передаёт `{allow = true}`; изменение `allow` на false запрещает соответствующую обработку. |
| `xr_danger.script` | `npc_on_eval_danger` передаёт `{ret_value = true}`; false используется вызывающим кодом для отказа от стандартной оценки опасности. |
| `bind_anomaly_zone.script` | Перед проверкой вероятности спавна артефакта подписчик может изменить `spawn_chance`. Имя события копируй точно из источника/каталога. |

### Правила интеграционных обвязок

Канонический шаблон добавления вызова или дополнительного кода аддона: в нужном месте общей функции отправить событие со **всеми необходимыми аргументами**, а реализацию разместить в отдельном обработчике аддона через `RegisterScriptCallback`. Передавай нужные объекты, значения, таблицы контекста и, если требуются, ссылки на функции-помощники; не заставляй аддон искать приватные данные или обращаться к внутренностям Framework. Общий исходник не должен напрямую зависеть от имени скрипта аддона, проверять наличие его конкретного метода или содержать его специфическую логику. При отсутствии подписчика дополнительный вызов просто не выполняется, и ванильная функция продолжает стандартную работу без ожидания ответа. Если требуется не дополнение, а замена ветки, явно предусмотри соответствующий слот функции/флаг и исходную ветку по умолчанию. Новое имя события регистрируется только в `__ixr_override_signals_intercepts.script`; каталог описывает порядок и назначение переданного контекста.

Канонический шаблон изменения значения через аддон: **до события** создать таблицу с исходным значением по умолчанию → передать её в событие → подписчик при необходимости меняет поле этой же таблицы → после отправки исходный код читает поле. Значение по умолчанию соответствует ванильному расчёту, а не произвольной заглушке. Если подписчика нет, таблица остаётся неизменной и используется ванильный результат. Пример существующего контракта: `local flags_table = {new_price = price}` перед событием изменения цены; после события — `price = flags_table.new_price`. Для такого механизма не передавай только отдельное число/строку/boolean в надежде изменить переменную вызывающего кода: изменяемым контейнером служит таблица.

**Главный инвариант: если аддон отсутствует, отключён или удалён, ванильное поведение не меняется.** Подписчика и его ответа может не быть вообще. Общий исходник обязан завершить текущую операцию и работать дальше самостоятельно со стандартными значениями и стандартной веткой.

- Контекст до отправки события заполняется исходным рассчитанным результатом либо значениями, выбирающими стандартную логику. Для опционального функтора отсутствие значения должно сохранять исходную ветку.
- Не ставь ванильный код в постоянное ожидание ответа, ответного события, заполнения поля или инициализации аддона. Не вводи для исходной механики обязательные флаги готовности, таймеры или последующие стадии, которыми управляет только необязательный подписчик.
- Сама отправка `SendScriptCallback` не гарантирует наличие обработчика и не создаёт обязанность аддона что-либо вернуть. Код после неё должен быть корректен без действий подписчиков.
- Если результат аддона используется позже, отсутствие сопутствующей части не должно блокировать ванильную операцию. Не оставляй устойчивое состояние, из которого может вывести только удалённый аддон.
- Проверяй отдельно запуск и работу без аддона, с аддоном и после его отключения/удаления. При удалении после сохранения или смены конфигурации отсутствие ожидаемой части не должно создавать новую зависимость или ломать стандартный путь.

- Сначала ищи существующий `SendScriptCallback` возле нужной операции и прочитай код до и после него: уведомление о свершившемся действии, изменение вычисленного значения и замена ветки имеют разную семантику.
- Предпочитай готовую интеграционную точку копированию или переписыванию всей ванильной функции. Новый управляемый модуль нужен для соответствующей ответственности, а локальная игровая обвязка может оставаться отдельным скриптом аддона.
- Если задача действительно требует отсутствующую точку расширения, в общей базе оставляй минимальную нейтральную интеграцию с сохранением стандартного поведения без подписчика. Специфическую логику аддона размещай в его скрипте, а не в ядре Framework или универсальном игровом коде. Описание этого подхода не является самостоятельным заданием добавлять точки в существующие файлы.
- Новое имя события регистрируется только в `__ixr_override_signals_intercepts.script`. Там описываются позиционные аргументы и назначение полей контекста; функции-замены требуют отдельного описания их будущих аргументов.
- `SendScriptCallback` выполняется синхронно. Вызывающий код читает изменённый контекст или вызывает переданную функцию только после завершения подписчиков.
- Меняй поля исходной таблицы. Присваивание `flags_table = {...}` внутри обработчика заменяет лишь его локальный параметр и не меняет таблицу вызывающей стороны.
- Шина не собирает `return` подписчиков. Для управления исходной логикой используй именно предусмотренные поля, например `allow`, `ret_value` или слот функции. Не рассчитывай, что `return false` из обычного подписчика отменит действие.
- Если подписчик установил функцию, её отдельно вызывает исходная функция с предусмотренными аргументами. Переданные локальные данные/помощники — явный контракт; автоматической привязки её лексических переменных к исходному файлу здесь нет.
- Дополнительный код без изменения таблицы также допустим, если точка вызова предоставляет нужный момент и контекст. Не дублируй побочный эффект исходной функции случайно.
- У нескольких подписчиков общий контекст. `pairs` не гарантирует порядок вызова: не обещай приоритет по времени подписки, не рассчитывай на обязательную предварительную правку другого аддона. Замену одного слота функции несколькими аддонами нужно учитывать как совместное использование одной точки.
- Этот механизм обвязок через события отличается от движкового `override_or_create_method`: он расширяет явно предусмотренное место выполнения, а не заменяет произвольную функцию и её сохранённые ссылки.

## Как читать каталог аргументов событий

`__ixr_override_signals_intercepts.script` — публичный справочник для мододелов, а не только файл регистрации. Мододел находит нужное событие, копирует список аргументов в свой обработчик и сохраняет их позиции.

Второй аргумент `add_intercept` — упорядоченный список строк-подсказок. Они могут обозначать имя, роль, область применения, предполагаемый тип или допустимость отсутствия значения. Это не строгие имена игровых/C++ типов и не автоматически создаваемые переменные. Например, `nil_or_device_torch_game_object` описывает возможный объект фонаря либо `nil`, `ref_ixr_options` — переданный API опций, `extend_description_table` — таблицу дополнений к описанию.

- Данные приходят позиционно из `SendScriptCallback(event_name, ...)`, а не по совпадению имён параметров.
- Сохраняй порядок и состав фактических аргументов. Параметрам обработчика можно дать удобные локальные имена; ассоциативные обозначения не нужно искать как реальные Lua/C++ классы.
- Если встречаются повторяющиеся обозначения вроде `game_object` или `number`, при переносе в сигнатуру дай параметрам различимые имена по их назначению. `...` означает переменную часть, а не имя обычного параметра.
- По строкам этого списка не выполняются валидация типов, преобразование значений или генерация вызова. Они также используются при выводе описания регистрации в лог.
- Для точной семантики проверяй источник отправки: что реально передано, допускается ли `nil`, какие поля есть в таблице, ожидается ли изменение общей таблицы результата.
- При добавлении нового события в этот файл делай обозначения полезными для копирования и понимания обработчика: важнее передать назначение и порядок аргументов, чем изобразить строгую систему типов. Не переименовывай уже существующие подсказки только ради соответствия названию игрового типа.

## Скрипты с префиксом `___`

По соглашению проекта файлы с тремя начальными подчёркиваниями относятся к Framework. В текущем корне присутствует `___ixr_engine_callbacks.script`: это сборник входных функций, вызываемых движком, а не обычный модуль с `get_module_info` и не скрипт пользовательской конфигурации `__ixr_override_*`.

### `___ixr_engine_callbacks.script`

| Входная функция/группа | Назначение |
| --- | --- |
| `on_frame()` | Вызывает framework frame hook; через него обновляется, в частности, буфер файловых логов. |
| `CInventory__eat` | Перед использованием предмета отправляет событие актора либо общее событие; возвращает изменённый подписчиками `flags.ret_value`. |
| `CActor__BeforeHitCallback` | Отправляет `actor_on_before_hit` перед обработкой попадания и возвращает `flags.ret_value`. |
| `CSE_ALifeDynamicObject_on_unregister` | Существующая движковая точка освобождения объекта; сейчас тело пустое. Не удаляй публичный вход только из-за отсутствия тела. |
| `CALifeUpdateManager__on_before_change_level` | Передаёт пакет в `on_before_level_changing`. |
| `CUIActorMenu_CanMoveToPartner`, `CInventoryBox_*`, `CInventory_ItemAvailableToTrade` | Передают проверку действия подписчикам через общую таблицу `flags` и возвращают результат движку. |
| `CUIActorMenu_OnItemDropped` | Разделяет drag/drop по отсутствию/совпадению предметов и принадлежности актору/NPC, отправляет общие и совместимые уведомления. |
| `CUIActorMenu_OnItemFocusReceive`, `CUIActorMenu_OnItemFocusLost` | Отправляют события фокуса с сохранением существующих имён совместимости. |
| `level_on_key_press` | При наличии уровня и актора отправляет `level_input_on_key_press`, возвращает предусмотренное текущим контрактом значение. |
| `get_near_object_info` | Делегирует сбор сведений системному компоненту `ixr_npc_info`. |
| `load_screen_press_any_key` | Отправляет `on_load_screen_await_any_key_pressed`, используемое составными событиями и таймерами. |

Имена этих функций, позиции аргументов и результаты возврата являются контрактом с C++. Не переименовывай их по стилистическим причинам. Сигнатура движкового входа и аргументы отправленного Lua-события могут различаться: сверяй обе стороны.

`flags` передаётся подписчикам той же ссылкой; возврат значения самим обработчиком шины не заменяет изменение `flags.ret_value`. Не копируй таблицу и не меняй синхронную отправку на отложенную для таких проверок. Наличие входа в этом файле само по себе не регистрирует новое имя события: каталог расширяется только в `__ixr_override_signals_intercepts.script`.

## Общие правила работы со скриптами

Подбирай готовые API по поведению задачи, а не только по совпадению названий. Например, если функция уже вызывается на update и её тяжёлую часть нужно выполнять реже, проверка `if IsActionThrottled("уникальное_имя_задачи", 500) then return end` может заменить отдельный таймер. Она лишь пропускает часть поступающих вызовов, а не планирует новый вызов. Модуль таймеров предназначен прежде всего для сюжетных игровых действий с сохраняемым состоянием, а не для технического управления частотой кода: у сохранения есть заметная стоимость. Модуль триггеров предназначен для геймплейных/сюжетных пространственных зон, не для произвольных внутренних проверок скрипта. Выбирай эти сервисы по назначению и необходимому жизненному циклу. Не добавляй лишнее состояние и инфраструктуру, когда существующая комбинация глобальных функций решает задачу.

- Сохраняй публичные контракты, используемые существующими модами, оригинальными скриптами и Framework. Следуй стилю соседнего кода; не переформатируй весь файл ради небольшой задачи.
- Общие алгоритмы, парсеры, проверки, работу с файлами, таблицами и кешем сначала ищи в `ixr_framework/utils/`. Не размазывай копии одной реализации по проекту.
- Инициализация и подписки выполняются в предусмотренных точках входа, а не заново в update. Автозагрузка `on_game_start` и управляемая инициализация модуля — разные механизмы.
- Контекст владельца для логов, опций и Storage определяется по стеку вызовов. При переносе в обёртку проверь глубину caller lookup и сохранность области данных.
- Не добавляй постоянные игровые тесты, логирование каждого кадра или запуск отладчика ради анализа. Текущая задача по картам изменяет только `AGENTS.md`, не игровые скрипты, конфиги и журналы.
- Рабочая `gamedata` и исходники в соседнем `IX-RAY/gamedata` могут различаться. Не синхронизируй их автоматически без задачи и проверки различий.

## Текущие скрипты непосредственно в корне

Основа этого каталога — ванильные игровые скрипты. Скрипты с нестандартными для исходной игры именами могут относиться к дополнительному контенту мододелов. Отдельно выделены описанные выше служебные файлы Framework с префиксами `___` и `__`.

Имя — ориентир, а не доказательство происхождения или неизменности: ванильный скрипт может содержать правки, а дополнительный контент — использовать привычное имя. Для точной атрибуции нужны исходник, места подключения и сравнение с соответствующей базовой версией игры. Не считай весь корень реализацией Framework и не переписывай игровые скрипты при составлении его карты.

Ниже перечислены все 374 файлов `.script`/`.lua`, находившихся непосредственно в `gamedata/scripts/` на момент составления списка. Вложенные каталоги в этот перечень не включены; их инструкции доступны через раздел навигации. При добавлении или удалении корневых скриптов актуализируй перечень.

```text
___ixr_engine_callbacks.script
__ixr_override_autoload_system.script
__ixr_override_framework_load_sub_modules.script
__ixr_override_signals_intercepts.script
__storage_mod_version.script
_g.script
actor_menu.script
actor_proxy.script
alife_storage_manager.script
anabiotic_vanilla.script
benchmark.script
bind_anomaly_field.script
bind_anomaly_zone.script
bind_artefact.script
bind_camp.script
bind_campfire.script
bind_crow.script
bind_door_labx8.script
bind_faction.script
bind_heli.script
bind_level_changer.script
bind_monster.script
bind_physic_object.script
bind_restrictor.script
bind_signal_light.script
bind_smart_cover.script
bind_smart_terrain.script
bind_stalker.script
cam_effector_sets.script
ce_new_attachable_item.script
ce_new_game_dm.script
ce_switcher.script
check_logic_path.script
class_registrator.script
combat_restrictor.script
cover_manager.script
db.script
death_manager.script
dialog_manager.script
dialogs.script
dialogs_jupiter.script
dialogs_pripyat.script
dialogs_zaton.script
dumpling_patch.script
game_registrator.script
game_relations.script
game_stats.script
game_types.script
gulag_general.script
heli_combat.script
heli_fire.script
heli_fly.script
heli_look.script
heli_move.script
heli_snd.script
inventory_upgrades.script
ixr_weather_options.script
ixray_global.script
jump_level.script
lain_test_bl.script
level_psy_antenna.script
level_weathers.script
loadscreen.script
lua_help.script
luaxml.script
memusage.script
mob_camp.script
mob_combat.script
mob_death.script
mob_home.script
mob_jump.script
mob_remark.script
mob_state_mgr.script
mob_walker.script
modules.script
move_mgr.script
news_manager.script
object_collection.script
outro_bad.script
outro_balanced_good.script
outro_cond.script
outro_good.script
pda.script
ph_appforce.script
ph_button.script
ph_code.script
ph_death.script
ph_door.script
ph_hit.script
ph_idle.script
ph_minigun.script
ph_on_hit.script
ph_oscillate.script
ph_sound.script
phantom_manager.script
post_combat_idle.script
profiler.script
ranks.script
release_body_manager.script
restrictor_manager.script
se_actor.script
se_artefact.script
se_car.script
se_heli.script
se_item.script
se_level_changer.script
se_monster.script
se_new_attachable_item.script
se_smart_cover.script
se_squad_group.script
se_stalker.script
se_zones.script
sim_board.script
sim_faction_brain_human.script
sim_faction_brain_mutant.script
sim_objects.script
sim_squad_actions.script
sim_squad_generic.script
sim_squad_scripted.script
simulation_objects.script
smart_covers.script
smart_covers_anim_pri_a22.script
smart_covers_animpoint_pri_a15.script
smart_covers_animpoint_sit_ass.script
smart_covers_animpoint_sit_high.script
smart_covers_animpoint_sit_knee.script
smart_covers_animpoint_sit_low.script
smart_covers_animpoint_sit_normal.script
smart_covers_animpoint_stay_table.script
smart_covers_animpoint_stay_wall.script
smart_covers_combat.script
smart_covers_combat_front.script
smart_covers_combat_prone.script
smart_covers_cover_barricade_0.script
smart_covers_cover_barricade_1.script
smart_covers_cover_barricade_2.script
smart_covers_cover_barricade_3.script
smart_covers_cover_barricade_commander.script
smart_covers_cover_lesnik.script
smart_covers_cover_loophole_1.script
smart_covers_cover_loophole_10.script
smart_covers_cover_loophole_10_11.script
smart_covers_cover_loophole_10_8.script
smart_covers_cover_loophole_10_8_11.script
smart_covers_cover_loophole_11.script
smart_covers_cover_loophole_1_2.script
smart_covers_cover_loophole_1_2_3.script
smart_covers_cover_loophole_1_2_3_jump.script
smart_covers_cover_loophole_1_2_jump.script
smart_covers_cover_loophole_1_3.script
smart_covers_cover_loophole_1_3_jump.script
smart_covers_cover_loophole_1_3_test.script
smart_covers_cover_loophole_1_jump.script
smart_covers_cover_loophole_2.script
smart_covers_cover_loophole_2_3.script
smart_covers_cover_loophole_2_3_jump.script
smart_covers_cover_loophole_2_jump.script
smart_covers_cover_loophole_3.script
smart_covers_cover_loophole_3_jump.script
smart_covers_cover_loophole_4.script
smart_covers_cover_loophole_4_5.script
smart_covers_cover_loophole_5.script
smart_covers_cover_loophole_6.script
smart_covers_cover_loophole_6_7.script
smart_covers_cover_loophole_7.script
smart_covers_cover_loophole_8.script
smart_covers_cover_loophole_8_11.script
smart_covers_cover_loophole_9.script
smart_covers_cover_loophole_lead_forester_idle_talk.script
smart_covers_cover_loophole_lead_sit_sleep_mechanic.script
smart_covers_cover_loophole_lead_sit_sleep_write.script
smart_covers_cover_loophole_lead_sit_sleep_write_temp.script
smart_covers_cover_loophole_lead_sit_strong.script
smart_covers_cover_loophole_lead_stand_hit_tv.script
smart_covers_cover_loophole_lead_stand_lean_on_bar.script
smart_covers_cover_loophole_lead_stand_look_at_table.script
smart_covers_cover_loophole_lead_stand_look_down_2.script
smart_covers_cover_loophole_lead_stand_look_down_3.script
smart_covers_cover_loophole_lead_stand_look_in_window.script
smart_covers_cover_loophole_lead_stand_look_up.script
smart_covers_cover_loophole_minigunner.script
smart_covers_loophole_1.script
smart_covers_loophole_10.script
smart_covers_loophole_11.script
smart_covers_loophole_2.script
smart_covers_loophole_3.script
smart_covers_loophole_4.script
smart_covers_loophole_5.script
smart_covers_loophole_6.script
smart_covers_loophole_7.script
smart_covers_loophole_8.script
smart_covers_loophole_9.script
smart_covers_loophole_anim_pri_a22.script
smart_covers_loophole_animpoint_pri_a15.script
smart_covers_loophole_animpoint_sit_ass.script
smart_covers_loophole_animpoint_sit_high.script
smart_covers_loophole_animpoint_sit_knee.script
smart_covers_loophole_animpoint_sit_low.script
smart_covers_loophole_animpoint_sit_normal.script
smart_covers_loophole_animpoint_stay_table.script
smart_covers_loophole_animpoint_stay_wall.script
smart_covers_loophole_barricade_0.script
smart_covers_loophole_barricade_1.script
smart_covers_loophole_barricade_2.script
smart_covers_loophole_barricade_3.script
smart_covers_loophole_barricade_commander.script
smart_covers_loophole_crouch_back.script
smart_covers_loophole_crouch_front.script
smart_covers_loophole_crouch_front_left.script
smart_covers_loophole_crouch_front_right.script
smart_covers_loophole_crouch_left.script
smart_covers_loophole_crouch_right.script
smart_covers_loophole_lead_forester_idle.script
smart_covers_loophole_lead_forester_talk.script
smart_covers_loophole_lead_sit_sleep_mechanic.script
smart_covers_loophole_lead_sit_sleep_write.script
smart_covers_loophole_lead_sit_strong.script
smart_covers_loophole_lead_stand_hit_tv.script
smart_covers_loophole_lead_stand_lean_on_bar.script
smart_covers_loophole_lead_stand_look_at_table.script
smart_covers_loophole_lead_stand_look_down_2.script
smart_covers_loophole_lead_stand_look_down_3.script
smart_covers_loophole_lead_stand_look_in_window.script
smart_covers_loophole_lead_stand_look_up.script
smart_covers_loophole_lesnik.script
smart_covers_loophole_minigunner.script
smart_covers_loophole_prone.script
smart_covers_loophole_stand_back.script
smart_covers_loophole_stand_front_left.script
smart_covers_loophole_stand_front_right.script
smart_covers_loophole_stand_left.script
smart_covers_loophole_stand_right.script
smart_covers_stc_cover_1.script
smart_covers_stc_cover_2.script
smart_covers_stc_cover_3.script
smart_names.script
smart_terrain.script
smart_terrain_control.script
sound_manager.script
sound_theme.script
spawn_b103.script
spawn_items.script
spawn_nimble_items.script
spawn_quest_items.script
spawn_upgrades.script
spawner.script
sr_camp.script
sr_crow_spawner.script
sr_cutscene.script
sr_deimos.script
sr_idle.script
sr_light.script
sr_monster.script
sr_no_weapon.script
sr_particle.script
sr_postprocess.script
sr_psy_antenna.script
sr_silence.script
sr_teleport.script
sr_timer.script
stalker_generic.script
state_lib.script
state_lib_animpoint.script
state_mgr.script
state_mgr_animation.script
state_mgr_animation_list.script
state_mgr_animation_list_animpoint.script
state_mgr_animstate.script
state_mgr_animstate_list.script
state_mgr_animstate_list_animpoint.script
state_mgr_bodystate.script
state_mgr_direction.script
state_mgr_goap.script
state_mgr_mental.script
state_mgr_movement.script
state_mgr_pri_a15.script
state_mgr_scenario.script
state_mgr_smartcover.script
state_mgr_weapon.script
story_objects.script
surge_manager.script
task_functor.script
task_manager.script
task_objects.script
test_pp.script
test_pp_stop.script
trade_manager.script
travel_manager.script
treasure_manager.script
ui_freeplay_dialog.script
ui_load_dialog.script
ui_main_menu.script
ui_mm_dof_options.script
ui_mm_mp_demo.script
ui_mm_mp_gamespy.script
ui_mm_mp_join.script
ui_mm_mp_localnet.script
ui_mm_mp_options.script
ui_mm_mp_profile.script
ui_mm_mp_server.script
ui_mm_opt_con_cmd.script
ui_mm_opt_controller_layout.script
ui_mm_opt_controller_layout_captions.script
ui_mm_opt_controls.script
ui_mm_opt_controls_gamepad.script
ui_mm_opt_gameplay.script
ui_mm_opt_main.script
ui_mm_opt_sound.script
ui_mm_opt_video.script
ui_mm_opt_video_adv.script
ui_mp_main.script
ui_numpad.script
ui_registrator.script
ui_save_dialog.script
ui_scenes.script
ui_si.script
ui_sleep_dialog.script
ui_wpn_params.script
utils.script
version.script
xr_abuse.script
xr_actions_id.script
xr_animpoint.script
xr_animpoint_predicates.script
xr_box.script
xr_camper.script
xr_combat.script
xr_combat_camper.script
xr_combat_ignore.script
xr_combat_monolith.script
xr_combat_zombied.script
xr_comm_ptr.script
xr_companion.script
xr_conditions.script
xr_corpse_detection.script
xr_cover.script
xr_danger.script
xr_death.script
xr_detector.script
xr_dialogs.script
xr_effects.script
xr_evaluators_id.script
xr_gather_items.script
xr_guide.script
xr_gulag.script
xr_hear.script
xr_heli_hunter.script
xr_help_wounded.script
xr_hit.script
xr_info.script
xr_kamp.script
xr_logic.script
xr_meet.script
xr_motivator.script
xr_patrol.script
xr_reach_task.script
xr_remark.script
xr_s.script
xr_sleeper.script
xr_smartcover.script
xr_sound.script
xr_sounds.script
xr_sounds_id.script
xr_spawner.script
xr_state.script
xr_statistic.script
xr_walker.script
xr_wounded.script
xr_zones.script
xr_zones_sound.script
xrs_dyn_music.script
xrs_dynamic_music_themes.script
xrs_stereo.script
zone_no_weapon.script
```
