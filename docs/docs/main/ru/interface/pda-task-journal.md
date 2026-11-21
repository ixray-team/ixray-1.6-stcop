> [!IMPORTANT]
> **Статус**: В разработке

# Вкладка заданий ПДА

## Обзор

Отдельная вкладка с журналом заданий: слева список с фильтрами, справа карточка выбранного задания с картинкой. Окно заданий на карте (`second_task_wnd`) остаётся как было.

Вкладка включается кнопкой с `id="eptTaskJournal"` в `pda*.xml`. Если такой кнопки нет, окно не создаётся и ПДА работает как раньше. Раскладка задаётся в `pda_task_journal*.xml`, почти каждый узел необязателен.

## Что видит игрок

1. Фильтры «В процессе», «Выполнено», «Отменено» с подсказками клавиш по краям.
2. Список разбит на группы «Сюжетные» и «Дополнительные». Внутри группы в текущих заданиях сначала отслеживаемое, потом по приоритету. В завершённых сначала самые свежие.
3. Клик выбирает задание. Двойной клик, Enter или значок-прицел в строке показывают его на карте.
4. Карточка: картинка, заголовок, время получения или завершения, уровень цели, этапы с цветом состояния.
5. Кнопки «Показать на карте» и «Отслеживать» с плашками клавиш.
6. Список обновляется сам, когда меняются задания.

В CoP у задания три состояния: в процессе, выполнено, провалено. «Отмена» из скрипта (`reversed`) в движке тоже провал, поэтому фильтр `failed` показывает и то и другое.

## Управление

| Действие | По умолчанию | Геймпад | Что делает |
| --- | --- | --- | --- |
| `pda_task_journal_filter_prev` | A | крестовина влево | Предыдущий фильтр |
| `pda_task_journal_filter_next` | D | крестовина вправо | Следующий фильтр |
| `pda_task_journal_prev` | ↑ | крестовина вверх | Предыдущее задание |
| `pda_task_journal_next` | ↓ | крестовина вниз | Следующее задание |
| `pda_task_journal_show_on_map` | M | X | Показать на карте |
| `pda_task_journal_track` | F | Y | Отслеживать |

Действия в своей группе конфликтов, поэтому A и D не мешают ходьбе, а M и F биндам карты. Фильтры также переключают Alt+Q / Alt+E и курки геймпада, как вкладки списка на карте. Enter и стрелки работают всегда, даже без биндов.

Бинды по умолчанию задаёт файл `configs/ixray_settings/mod_default_controls_*.ltx` аддона:

```ini
bind pda_task_journal_filter_prev kA
bind_gamepad pda_task_journal_filter_prev cDPAD_LEFT
```

Движок подхватывает такие файлы при запуске и при сбросе управления по умолчанию. Чтобы действия появились в настройках, добавьте группу в `mod_ui_keybinding_*.xml`:

```xml
<group name="kb_grp_pda_task_journal" override="add">
	<command id="kb_pda_tj_show_on_map" exe="pda_task_journal_show_on_map"/>
</group>
```

Если у игрока уже сохранено управление и бинды не появились, нажмите «По умолчанию» в настройках управления.

## Замена окна заданий на карте

По умолчанию вкладка работает рядом со старым окном заданий на карте. Чтобы вкладка полностью его заменила, включите это явно атрибутом на `main_wnd` в `pda_task_journal.xml`:

```xml
<main_wnd x="0" y="0" width="1024" height="768" replace_task_list="1">
```

Тогда:

1. Кнопка `btn_second_task` на карте скрыта, всплывающий список заданий не открывается.
2. Все способы открыть этот список ведут на вкладку «Задания»: хоткей `pda_tasks_toggle_list`, кнопка списка в карточке активной цели, вкладка-алиас `eptTaskList`, вызов из Lua.
3. Alt+Q / Alt+E и курки геймпада на вкладке карты больше не переключают фильтры списка, потому что списка нет.

Если вкладки `eptTaskJournal` нет в `pda*.xml`, атрибут ни на что не влияет.

При замене узлы `btn_second_task` и `second_task_wnd` в `pda_tasks*.xml` можно удалить: без `second_task_wnd` окно списка на карте не создаётся вовсе. HD HUD так и делает.

## Кнопка вкладки

```xml
<button x="171" y="3" width="90" height="35" id="eptTaskJournal" hint="pda_tab_task_journal_hint" frame_mode="1">
	<text align="c" vert_align="c" font="font_oswald_20">pda_tab_task_journal</text>
</button>
```

Фон вкладки задаёт узел `eptTaskJournal` в `tab_backgrounds`. Id можно переназначить через `[pda_tab_aliases]`.

## pda_task_journal.xml

| Узел | Что это |
| --- | --- |
| `main_wnd` | Окно вкладки. Вложенные `auto_static`, `auto_frameline`, `auto_framewindow` создаются сами: подписи панелей, рамки, фон. |
| `filter_tabs` | Вкладки фильтров. `counters="1"` дописывает к подписи число заданий. |
| `filter_hint_prev`, `filter_hint_next` | Плашки клавиш у фильтров, полоса подсказок с одним пунктом. |
| `hotkeys_hint` | Любые дополнительные полосы подсказок, их может быть несколько. |
| `task_list` | Список заданий. |
| `group_header` | Заголовок группы в списке. Без узла групп нет. |
| `task_item` | Шаблон строки списка. |
| `t_empty` | Текст для пустого списка. |
| `details` | Карточка задания. |

Полосы подсказок описаны в [легенде карты и хоткеях](pda-map-legend-hotkeys.md): режим плашек включается узлом `key`, пункт задаётся `<item action="..." gamepad_action="..."/>`. Пункт без назначенной клавиши не показывается.

### Фильтры

| Id кнопки | Задания |
| --- | --- |
| `active` | Текущие |
| `completed` | Выполненные |
| `failed` | Проваленные и отменённые |
| `archive` | Все завершённые |
| `story`, `side` | Текущие сюжетные или дополнительные |
| `all` | Все текущие |

Первая кнопка открыта по умолчанию.

### Группы

```xml
<group_header x="0" y="0" width="300" height="20" text_story="pda_tj_group_story" text_side="pda_tj_group_side">
	<text x="8" align="l" vert_align="c" font="font_rubik_12" r="110" g="110" b="110"/>
</group_header>
```

С этим узлом список делится на сюжетные и дополнительные задания, над каждой частью строка заголовка.

### Строка списка

```xml
<task_item x="0" y="0" width="300" height="38" padding="10">
	<selection x="0" y="0" width="300" height="38" stretch="1"><texture a="110">ui_inGame2_pda_background_text</texture></selection>
	<active_mark x="0" y="0" width="2" height="38" stretch="1"><texture r="220" g="190" b="80">ui_inGame2_hint_wnd_bar</texture></active_mark>
	<icon x="4" y="8" width="22" height="22" stretch="1"/>
	<title x="32" y="11" width="216" height="16"><text font="font_rubik_14"/></title>
	<status x="180" y="11" width="90" height="16" text_completed="pda_tj_item_completed" text_failed="pda_tj_item_failed"><text align="r" font="font_rubik_12"/></status>
	<btn_focus x="276" y="11" width="14" height="17" stretch="1"><texture>ui_inGame2_pda_center_on_mission_button</texture></btn_focus>
	<colors>
		<normal r="210" g="210" b="210"/>
		<active r="235" g="215" b="140"/>
		<unread r="245" g="245" b="245"/>
		<completed r="130" g="190" b="120"/>
		<failed r="205" g="115" b="95"/>
	</colors>
</task_item>
```

| Узел | Что это |
| --- | --- |
| `selection` | Подложка выбранной строки. |
| `active_mark` | Метка отслеживаемого задания. |
| `icon` | Иконка задания. |
| `type_icon` | Значок типа, текстуры в `texture_story` и `texture_side`. |
| `title` | Название. Высота строки растёт, если название переносится, `padding` это отступ под ним. |
| `time` | Время получения или завершения. |
| `status` | Метка завершённого задания. |
| `btn_focus` | Кнопка «показать на карте», видна только у текущих заданий с целью на карте. |
| `colors` | Цвета названия: `normal`, `active`, `unread`, `completed`, `failed`. |

### Пустой список

`t_empty` с атрибутами `text` (по умолчанию) и `text_<id фильтра>`.

### Карточка

```xml
<details x="366" y="117" width="635" height="628" heading_source="description"
	image_level_intro="1" image_default="intro\intro_no_start_picture"
	received_format="pda_tj_received" finished_format="pda_tj_finished" location_format="pda_tj_location"
	objectives_caption="pda_tj_objectives">
	<empty x="14" y="12" width="600" height="20"><text font="font_rubik_14">pda_tj_select_task</text></empty>
	<image x="1" y="0" width="633" height="186" stretch="1"/>
	<body x="0" y="194" width="635" height="356" vert_interval="4"/>
	<btn_map x="8" y="562" width="300" height="26">
		<text x="32" align="l" vert_align="c" font="font_rubik_14">pda_tj_show_on_map</text>
		<hint x="0" y="1" width="26" height="24">...<item action="pda_task_journal_show_on_map"/></hint>
	</btn_map>
	<heading x="0" y="0" width="615" height="24" complex_mode="1"><text font="font_oswald_24"/></heading>
	<info x="0" y="0" width="615" height="16"><text font="font_rubik_12"/></info>
	<objective x="12" y="0" width="600" height="16" complex_mode="1"><text font="font_rubik_14"/></objective>
</details>
```

| Атрибут | Что делает |
| --- | --- |
| `heading_source` | `title` или `description`: что показывать в строке `heading`. Если выбрано описание, оно не повторяется в `descr`. |
| `image_section` | Секция ltx с картинками, по умолчанию `task_journal_images`. |
| `image_level_intro` | `1`: если картинки нет в секции, взять экран загрузки уровня цели `intro\intro_<уровень>`. |
| `image_default` | Картинка, если ничего не нашлось. |
| `received_format`, `finished_format`, `location_format` | Id строк для времени и места, `{value}` заменяется значением. |
| `objectives_caption` | Заголовок списка этапов. |
| `text_story`, `text_side`, `text_in_progress`, `text_active`, `text_completed`, `text_failed` | Подписи для необязательных узлов `type` и `status`. |

Дочерние узлы: `empty`, `image`, `icon`, `title`, `type`, `status`, `body`, `btn_map`, `btn_activate`. Внутри кнопки необязательный `hint` это плашка клавиши: она прячется вместе с кнопкой. Шаблоны строк тела: `heading`, `descr`, `info`, `objectives_caption`, `objective`, цвета этапов в `colors` (`progress`, `completed`, `failed`).

`image` может иметь `crop_rect="x,y,w,h"`: область текстуры в пикселях, если картинку нужно обрезать, а не растягивать. Если картинки нет, тело поднимается на её место.

### Картинки заданий

Картинка ищется по порядку:

1. Ключ с id задания в секции `image_section`.
2. Ключ с именем уровня цели в той же секции.
3. Экран загрузки уровня цели, если `image_level_intro="1"` и текстура есть.
4. Ключ `default` в секции.
5. Атрибут `image_default`.

```ini
[task_journal_images]
zat_b14_stalker_task  = ui\task_journal\zat_b14
jupiter               = ui\task_journal\jupiter_common
default               = ui\task_journal\placeholder
```

Секцию можно дополнять из любого аддона через DLTX.

## Ограничения

1. «Показать на карте» работает, если в ПДА есть вкладка карты `eptTasks` или `eptTaskList`.
2. Отменённые и проваленные задания движок не различает.
3. Уровень цели известен, пока у задания есть метка на карте. У завершённых заданий картинка берётся по id или по умолчанию.
4. Выбор задания отмечает его прочитанным.

## Проверка после сборки

1. Без кнопки `eptTaskJournal` ПДА выглядит как раньше.
2. Вкладка открывается, задания разбиты на «Сюжетные» и «Дополнительные», отслеживаемое первое в своей группе.
3. A и D переключают фильтры, стрелки листают список, плашки показывают текущие бинды.
4. M, Enter, двойной клик и прицел в строке открывают карту на цели. F переносит отслеживание.
5. Карточка показывает экран загрузки уровня цели, заголовок из описания, время и место.
6. После выполнения или провала задание само уходит в нужный фильтр.
7. Действия видны в настройках управления в группе «ПДА: задания».
8. Без `replace_task_list` окно заданий на карте работает как раньше.
9. С `replace_task_list="1"` кнопки списка на карте нет, а хоткей списка открывает вкладку «Задания».
