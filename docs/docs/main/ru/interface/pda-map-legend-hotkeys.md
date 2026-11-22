> [!IMPORTANT]
> **Статус**: В разработке

# Легенда карты, фильтры спотов и хоткеи ПДА

## Обзор

Карта ПДА умеет скрывать споты по категориям, управлять личными метками с клавиатуры и показывать полосу подсказок с текущими биндами. Всё настраивается данными аддона: категории в `ltx`, легенда и полоса подсказок в `pda_tasks*.xml`, бинды по умолчанию в `mod_default_controls_*.ltx`. Если узлов нет, ПДА работает как в оригинале.

## Категории спотов

Категория описывает группу спотов, которую можно скрыть целиком. Скрытый спот пропадает на карте ПДА, на миникарте и на компасе. Состояние сохраняется в сейв.

Категории задаются в секции `[map_spot_categories]` системного конфига, например в `configs/mod_system_my_addon.ltx`:

```ini
[map_spot_categories]
main_tasks     = storyline_task*, ui_storyline_task_blink
stashes        = treasure*
personal_spots = $user
traders        = ui_pda2_trader_location
```

1. Ключ это id категории.
2. Значение это список типов спотов через запятую.
3. `*` в конце означает префикс: `treasure*` совпадает с `treasure`, `treasure_rare` и т.д.
4. `$user` означает личные метки игрока.

Спот скрыт, если скрыта хотя бы одна категория, в которую он попадает.

Кнопки фильтров внизу карты (`filter_treasures`, `filter_quest_npcs` и другие) тоже работают через категории `pda_filter_*`. Если кнопки нет в XML, её категория всегда включена.

## Легенда карты

Узел `map_legend_wnd` в `pda_tasks*.xml`. Клик по пункту скрывает или показывает его категорию, повторный клик возвращает.

```xml
<map_legend_wnd x="780" y="150" width="150" height="470">
	<btn_toggle_all x="10" y="434" width="130" height="30" frame_mode="1" text_show="pda_legend_show_all" text_hide="pda_legend_hide_all">
		<texture>ui_inGame2_pda_button_new_task</texture>
		<text align="c" vert_align="c" font="font_rubik_16">pda_legend_hide_all</text>
	</btn_toggle_all>
	<legend_list x="3" y="40" width="200" height="386" item_height="36" hidden_alpha="70">
		<item x="0" y="0" width="200" height="36" category="stashes">
			...
		</item>
	</legend_list>
</map_legend_wnd>
```

| Атрибут | Где | Значение |
| --- | --- | --- |
| `category` | `item` | id категории из `[map_spot_categories]` |
| `spots`, `user_spots` | `item` | старый способ: шаблоны прямо в XML, категория создаётся автоматически |
| `hidden_alpha` | `legend_list` | прозрачность скрытого пункта, 0..255, по умолчанию 80 |
| `text_show`, `text_hide` | `btn_toggle_all` | тексты кнопки «Показать все» и «Скрыть все» |

Пункт без `category` и `spots` не кликается, как в оригинале.

## Полоса подсказок

> Общая система хоткеев, стили плашек и узлы `hotkey` / `hotkey_bar` описаны в [отдельной статье](hotkeys.md). Ниже — формат самой полосы.

Узел `hotkeys_hint` в `pda_tasks*.xml`. Движок сам собирает строку из текущих биндов, пропускает действия без клавиши и убирает пункты с конца, если строка не влезает по ширине. В режиме геймпада полоса скрыта, если есть `gamepad_legend`.

```xml
<hotkeys_hint x="49" y="708" width="790" height="35" separator=" | " key_format="[%s]">
	<text align="l" vert_align="c" font="font_rubik_14"/>
	<item action="pda_tasks_toggle_list">pda_hint_tasks</item>
	<item action="pda_tasks_personal_spot_prev,pda_tasks_personal_spot_next">pda_hint_spot_browse</item>
</hotkeys_hint>
```

1. `action` это одно действие или несколько через запятую.
2. Текст пункта это id строки подписи.
3. `separator` (между пунктами), `key_format` (формат клавиши, например `[%s]`) и `label_separator` (между клавишей и подписью, например ` - `) необязательны.
4. Атрибут `key` у пункта задаёт фиксированный текст клавиши вместо бинда, например `<item action="quit" key="Esc">`.
5. Узлов `hotkeys_hint` может быть несколько, например левая и правая части нижней панели. Так же можно сделать несколько строк в панели меток.
6. `hotkeys_hint` работает и в `pda.xml`, например плашки клавиш переключения вкладок: `<item action="ui_tab_left"/>` и `<item action="ui_tab_right"/>`. Подпись у пункта можно не задавать.
7. Узел `btn_second_task` необязателен: если его нет, список заданий открывается только хоткеем.

### Клавиши-плашки

Если внутри `hotkeys_hint` есть узел `key`, полоса рисуется не строкой, а рядом плашек: каждая клавиша в своей рамке, подпись справа. Без `key` полоса работает как раньше.

```xml
<hotkeys_hint x="49" y="708" width="700" height="35" key_padding="9" key_spacing="4" label_spacing="9" item_spacing="22" chips_align="l">
	<key_back type="line" x="0" y="0" width="26" height="26">
		<texture>ui_inGame2_pda_new_tab_btn_e</texture>
	</key_back>
	<key x="0" y="0" width="26" height="26">
		<text align="c" vert_align="c" font="font_rubik_14" r="225" g="200" b="145"/>
	</key>
	<label x="0" y="0" width="10" height="26">
		<text align="l" vert_align="c" font="font_rubik_14" r="190" g="190" b="190"/>
	</label>
	<item action="pda_tasks_map_show_me">pda_hint_show_me</item>
</hotkeys_hint>
```

| Узел или атрибут | Действие |
| --- | --- |
| `key` | Шаблон текста клавиши. Высота задаёт высоту плашки, ширина подгоняется под текст. Может иметь свою текстуру со `stretch="1"`. |
| `key_back` | Необязательная рамка под клавишей. `type`: `static` (растянутая текстура), `line` (трёхчастная текстура `_b`, `_back`, `_e`), `frame` (девятичастная рамка). В `static` вместо текстуры можно указать `<svg>файл.svg</svg>` (с `<texture>` как запасной) и `stretch="1"`: так HD HUD рисует плашки ПДА из `hotkey_frame.svg`. При растяжении на широкую клавишу SVG тянется целиком, вместе со скруглениями. |
| `label` | Шаблон подписи. Ширина подгоняется под текст. |
| `key_padding` | Отступ текста от краёв плашки. По умолчанию 8. |
| `key_min_width` | Минимальная ширина плашки. По умолчанию равна её высоте. |
| `key_spacing` | Расстояние между плашками одного пункта. По умолчанию 4. |
| `label_spacing` | Расстояние от плашки до подписи. По умолчанию 8. |
| `item_spacing` | Расстояние между пунктами. По умолчанию 20. |
| `chips_align` | Выравнивание ряда: `l`, `c`, `r`. По умолчанию `l`. |

Каждая назначенная клавиша получает свою плашку, у одного пункта их не больше четырёх. В этом режиме `key_format` по умолчанию равен `%s`, `separator` и `label_separator` не используются. Пункты, которые не влезают по ширине, убираются с конца, как в текстовом режиме.

## Вкладки списка заданий

Вкладки задаются кнопками в `second_task_wnd:task_filter_tabs`. Режим выбирается по `id` кнопки:

| `id` | Что показывает |
| --- | --- |
| `all` | Все задания в процессе |
| `story` | Сюжетные задания в процессе |
| `side` | Побочные задания в процессе |
| `active` | Все задания в процессе |
| `completed` | Выполненные задания, сначала самые свежие |
| `failed` | Проваленные задания, сначала самые свежие |
| `archive` | Выполненные и проваленные задания вместе, сначала самые свежие |

```xml
<task_filter_tabs x="0" y="470" width="344" height="24" counters="1">
	<button id="active" x="0" y="0" width="172" height="24">...</button>
	<button id="archive" x="172" y="0" width="172" height="24">...</button>
</task_filter_tabs>
<t_empty x="15" y="230" width="310" height="60" text_active="pda_tasks_empty_active" text_archive="pda_tasks_empty_archive">
	<text align="c" vert_align="c" font="font_rubik_16"/>
</t_empty>
```

1. `counters="1"` дописывает к названию вкладки число заданий: «Активные (5)».
2. `t_empty` показывается вместо пустого списка. Атрибут `text_<id вкладки>` (`text_active`, `text_archive` и т.д.) задаёт текст для своей вкладки, `text` задаёт общий текст.
3. Цвета завершённых заданий задают узлы `task_item:completed` и `task_item:failed`, по умолчанию берётся цвет `read`.
4. Необязательный `task_item:st_status` показывает у завершённого задания метку статуса цветом статуса. Тексты задают атрибуты `text_completed` и `text_failed`. Название задания сужается, чтобы не наезжать на метку.
5. Завершённое задание нельзя сделать активным, у него скрыты кнопки фокуса и показа на карте.
6. Вкладки списка переключаются клавишами вкладок ПДА с зажатым Alt (по умолчанию Alt+Q и Alt+E) или действиями `ui_tab_sec_left` / `ui_tab_sec_right`. Если список закрыт, он открывается.
7. Необязательные узлы `tab_hint_prev` и `tab_hint_next` показывают подсказки клавиш по краям панели вкладок. Это полоса подсказок с одним пунктом: `<item action="ui_tab_left" gamepad_action="ui_tab_sec_left"/>`, формат клавиши задаёт `key_format="[Alt + %s]"`, для геймпада `gamepad_key_format`.

## Панель личных меток

Узел `personal_spots_wnd` в `pda_tasks*.xml`. Панель показывает все личные метки: сначала на текущем уровне, потом на остальных. Открывается действием `pda_tasks_personal_spots_list`.

| Узел | Назначение |
| --- | --- |
| `background_frame`, `t_caption`, `btn_close` | Рамка, заголовок, кнопка закрытия |
| `t_counter` | Счётчик «3 / 5», без лимита просто «3» |
| `spot_list` | Список. `distance_format` задаёт id строки вида `%s · %d м` (уровень и расстояние) |
| `t_empty` | Текст, когда меток нет |
| `t_message` | Сообщение о лимите, `text_limit` задаёт id строки с `%u` |
| `t_tip` | Постоянная подсказка |
| `hotkeys_hint` | Полоса подсказок, как на карте |
| `spot_item` | Шаблон строки: `name`, `info`, `btn_track`, `btn_remove`, `highlight` (показан у метки с навигацией), цвета `normal` и `tracked` |

В строке клик показывает метку на карте, двойной клик открывает переименование, кнопки включают навигацию и удаляют метку.

На карте одновременно открыта только одна боковая панель: список заданий, легенда или панель меток.

Окно создания метки (`pda_spot.xml`) может содержать статик `t_hint`. Если имя оставить пустым, метка получит имя по умолчанию вида «Метка 3 · Затон».

## Лимит личных меток

```ini
[pda_map]
personal_spot_limit = 5
```

Значение 0 или отсутствие строки снимает ограничение. При достижении лимита новая метка не создаётся: на карте открывается панель меток с сообщением, в игре быстрая метка показывает уведомление. Лимит действует и для Lua `AddPersonalSpot`.

## Карточка активной цели

Необязательный узел `active_task_card` в `pda_tasks*.xml`. Карточка поверх карты показывает текущую цель: если включена навигация к геометке, то её, иначе активное сюжетное задание, а если его нет, то активное побочное. Обновляется раз в полсекунды. Клик по карточке или действие `pda_tasks_map_show_target` показывает цель на карте.

| Атрибут или узел | Назначение |
| --- | --- |
| `text_story`, `text_side`, `text_mark` | Подписи типа цели. Если сюжетных или побочных заданий несколько, добавляется «· 1 / 3» |
| `text_none` | Текст, когда цели нет. Без него карточка скрывается |
| `distance_format` | Формат строки «уровень · расстояние», как у панели геометок |
| `hide_with_task_list` | 1 (по умолчанию): пока открыт список заданий, карточка скрывается, а при наличии шапки сворачивается до неё |
| `header_height` | Высота шапки, до неё карточка сворачивается |
| `header_text`, `header_counter` | Текст шапки и счётчик активных заданий («ВСЕ ЗАДАНИЯ · 5») |
| `button_show`, `button_hide`, `button_format` | Подписи кнопки списка и формат с клавишей, например `%s  [%s]` |
| `icon_story`, `icon_side`, `icon_mark` | Иконки, если у задания нет своей иконки |
| `header_frame`, `header_icon`, `t_header`, `btn_list` | Шапка. Клик по шапке или кнопке открывает и скрывает список заданий |
| `body_frame`, `t_icon` | Фон тела и иконка задания (`m_icon_texture_name`) |
| `background_frame` или `background` | Фон |
| `t_type`, `t_title`, `t_objective`, `t_info` | Тип, название задания, текущая цель, уровень и расстояние |
| `btn_focus` | Кнопка «показать на карте», скрыта, если у цели нет метки |

## Действия

| Действие | Где работает | Что делает |
| --- | --- | --- |
| `ui_tab_left`, `ui_tab_right` | ПДА | Предыдущая и следующая вкладка, с Alt переключают вкладки списка заданий |
| `ui_tab_sec_left`, `ui_tab_sec_right` | ПДА | Вкладки списка заданий и дневника |
| `pda_tasks_toggle_list` | Карта ПДА | Открыть или скрыть список заданий |
| `pda_tasks_map_show_me` | Карта ПДА | Показать игрока на карте |
| `pda_tasks_map_show_target` | Карта ПДА | Показать текущую цель на карте |
| `pda_tasks_toggle_legend` | Карта ПДА | Открыть или скрыть легенду |
| `pda_tasks_personal_spot_cursor` | Карта ПДА | Метка под курсором, на существующей метке открывает переименование |
| `pda_tasks_personal_spot_actor` | Карта ПДА | Метка на позиции игрока |
| `pda_tasks_personal_spot_remove` | Карта ПДА | Удалить метку под курсором |
| `pda_tasks_personal_spot_track` | Карта ПДА | Вести к метке под курсором, повторно снять |
| `pda_tasks_personal_spot_next`, `_prev` | Карта ПДА | Следующая и предыдущая личная метка |
| `pda_tasks_personal_spots_list` | Карта ПДА | Открыть или скрыть панель личных меток |
| `personal_spot_quick` | В игре | Быстрая метка на месте игрока без диалога |

Иконку уведомления быстрой метки задаёт `quick_spot_news_icon` в секции `[pda_map]` файла `game_global.ltx`.

## Бинды по умолчанию из аддона

Команда `default_controls` после основного `default_controls.ltx` загружает все файлы `configs/ixray_settings/mod_default_controls_*.ltx` по алфавиту. Это обычные консольные команды:

```ini
bind pda_tasks_toggle_legend kL
bind pda_tasks_toggle_list kTAB
```

Одна клавиша может быть назначена в разных группах действий: в игре `kL` включает фонарь, а в ПДА открывает легенду.

## Lua

Методы `CMapManager` (`level.map_manager()`):

| Метод | Описание |
| --- | --- |
| `AddPersonalSpot(level_name, position, hint)` | Создать личную метку |
| `RemovePersonalSpot(location)` | Удалить личную метку |
| `TogglePersonalSpotNavigation(location)` | Вести к метке или снять навигацию |
| `GetActiveUserNavigationLocation()` | Текущая цель навигации |
| `GetAdjacentPersonalSpot(id, next)` | Соседняя личная метка по id |
| `RegisterSpotCategory(id, patterns)` | Добавить категорию спотов |
| `SetSpotCategoryHidden(id, hidden)` | Скрыть или показать категорию |
| `IsSpotCategoryHidden(id)` | Скрыта ли категория |
| `GetPersonalSpotLimit()`, `GetPersonalSpotCount()`, `CanAddPersonalSpot()` | Лимит личных меток |

Смежный материал: [навигационная личная метка](map-personal-navigation-marker.md), [обзор UI](ui-advanced-features.md).
