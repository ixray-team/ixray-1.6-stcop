# Lua в инструментах SDK

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Эта страница описывает Lua API редактора свойств серверных объектов. Он регистрируется в сборке `xrSE_Factory` и вызывается в `FillProps(prefix, items)` при построении панели свойств. [Регистрация в SDK](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrServerEntities/script_engine_export.cpp#L126-L128)

## Синтаксис

`properties_helper()` возвращает общий объект `properties_list_helper`. Методы ниже вызываются через двоеточие, например `helper:create_float(...)`. `prop_item_vec` — список `items`, который передаёт редактор. `prop_value` — тип аргумента `sender` для обработчиков. Типы результатов в таблицах представляют свойства, созданные редактором. Их конструкторы и методы в этой регистрации отсутствуют. [Типы свойств и фабрика](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L44-L74)

`key` задаёт путь в панели свойств с разделителем `\`. `owner` — серверная сущность, обычно `self` в `FillProps`. Поле `owner[field]` должно существовать до вызова и иметь указанный в таблице тип. Квадратные скобки обозначают последовательные перегрузки с дополнительными аргументами.

### Добавление свойств

| Вызов | Результат | Назначение | Код |
| --- | --- | --- | --- |
| `create_caption(items, key, text)` | `caption_value` | Строка с подписью. | [111](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L111) |
| `create_canvas(items, key, text, height)` | `canvas_value` | Добавляет область для отрисовки. `height` — высота области. Регистрация Lua не предоставляет обработчик рисования. | [112](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L112) |
| `create_button(items, key, labels, flags)` | `button_value` | Добавляет кнопки с подписями из строки через запятую. Передайте `0` для обычного ряда кнопок. Через этот вызов обработчик нажатия не назначается. | [113](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L113) |
| `create_choose(items, key, owner, field, mode [, path [, fill_param [, max_items]]])` | `choose_value` | Окно выбора ресурса для строкового поля. `path` — начальный выбор для пустого поля, `fill_param` — строка для обработчика выбранного режима, `max_items` — максимум выбираемых элементов, по умолчанию `1`. | [115–118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L115-L118) |
| `create_s16(items, key, owner, field [, min [, max [, step]]])` | `s16_value` | Знаковое 16-битное целое. | [125–128](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L125-L128) |
| `create_s32(items, key, owner, field [, min [, max [, step]]])` | `s32_value` | Знаковое 32-битное целое. | [130–133](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L130-L133) |
| `create_u8(items, key, owner, field [, min [, max [, step]]])` | `u8_value` | Беззнаковое 8-битное целое. | [135–138](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L135-L138) |
| `create_u16(items, key, owner, field [, min [, max [, step]]])` | `u16_value` | Беззнаковое 16-битное целое. | [140–143](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L140-L143) |
| `create_u32(items, key, owner, field [, min [, max [, step]]])` | `u32_value` | Беззнаковое 32-битное целое. | [145–148](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L145-L148) |
| `create_float(items, key, owner, field [, min [, max [, step [, decimals]]]])` | `float_value` | Число с плавающей точкой. | [150–154](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L150-L154) |
| `create_bool(items, key, owner, values, field)` | `bool_value` | Флажок для существующего значения `values[field]` типа boolean. `owner` — серверный объект, `values` — таблица с полем. | [156](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L156) |
| `create_vector(items, key, owner, field [, min [, max [, step [, decimals]]]])` | `vector_value` | Редактор трёх компонентов объекта `vector`. Границы общие для всех осей. | [158–162](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L158-L162) |
| `create_flag8(items, key, owner, field, mask [, off [, on [, flags]]])` | `flag8_value` | Флажок для битовой маски в `Flags8`. Для вызова нужен объект `Flags8` от C++. Конструктор `flags8()` в Lua не зарегистрирован. | [164–167](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L164-L167) |
| `create_flag16(items, key, owner, field, mask [, off [, on [, flags]]])` | `flag16_value` | Флажок для битовой маски в объекте `flags16`. | [169–172](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L169-L172) |
| `create_flag32(items, key, owner, field, mask [, off [, on [, flags]]])` | `flag32_value` | Флажок для битовой маски в объекте `flags32`. | [174–177](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L174-L177) |
| `create_token8(items, key, owner, field, tokens)` | `token8_value` | Показывает имена из `token_list`. Хранит ID как `u8`. | [179](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L179) |
| `create_token16(items, key, owner, field, tokens)` | `token16_value` | Показывает имена из `token_list`. Хранит ID как `u16`. | [180](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L180) |
| `create_token32(items, key, owner, field, tokens)` | `token32_value` | Показывает имена из `token_list`. Хранит ID как `u32`. | [181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L181) |
| `create_list(items, key, owner, field, options)` | `list_value` | Показывает строки из `rtoken_list`. Хранит выбранную строку. | [187](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L187) |
| `create_color(items, key, owner, field)` | `u32_value` | Выбор цвета, упакованного в 32-битное целое ARGB. | [189](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L189) |
| `create_fcolor(items, key, owner, field)` | `color_value` | Выбор цвета из объекта `fcolor` с четырьмя компонентами. | [190](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L190) |
| `create_vcolor(items, key, owner, field)` | `vector_value` | Выбор цвета, хранящегося в `vector` с компонентами RGB от `0` до `1`. | [191](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L191) |
| `create_text(items, key, owner, field)` | `text_value` | Редактируемая строка. | [193](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L193) |
| `create_time(items, key, owner, field [, min [, max]])` | `float_value` | Редактирует секунды в формате `ЧЧ:ММ:СС`. Границы по умолчанию `0` и `86400`. | [195–197](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L195-L197) |
| `create_angle(items, key, owner, field [, min [, max [, step [, decimals]]]])` | `float_value` | Угол и его границы задаются в радианах. Интерфейс показывает градусы. | [199–203](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L199-L203) |
| `create_vangle(items, key, owner, field [, min [, max [, step [, decimals]]]])` | `vector_value` | Три угла в объекте `vector`. Действуют те же единицы, что у `create_angle`. | [205–209](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L205-L209) |

Для целых чисел значения `min, max, step` по умолчанию равны `0, 100, 1`. Для `create_float` и `create_vector` значения `min, max, step, decimals` равны `0, 1, 0.01, 2`. У угловых свойств границы по умолчанию соответствуют полному диапазону `float`, шаг равен `0.01`, точность — два знака после запятой. `off` и `on` задают подписи состояний битовой маски. `flags` — маска параметров отображения свойства, по умолчанию `0`. [Значения по умолчанию](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Editors/Public/PropertiesListHelper.h)

### Обработчики редактирования

При создании угловых свойств соответствующие обработчики уже подключаются на стороне SDK. Для прямого вызова нужен `sender`, переданный редактором с правильным типом свойства.

| Вызов | Результат | Назначение | Код |
| --- | --- | --- | --- |
| `vector_on_before_edit(sender, value)` | — | Преобразует переданный `vector` и границы свойства из радиан в градусы. | [102](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L102) |
| `vector_on_after_edit(sender, value)` | `boolean` | Преобразует переданный `vector` и границы свойства из градусов в радианы. Возвращает `true`. | [101](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L101) |
| `float_on_before_edit(sender, value)` | `number` | Возвращает угол в градусах и переводит границы свойства в градусы. | [105](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L105) |
| `float_on_after_edit(sender, value)` | `boolean, number` | Возвращает `true` и угол в радианах. Границы свойства тоже переводятся в радианы. | [104](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L104) |
| `name_before_edit(sender)` | `string` | Внутренний обработчик выделяет последний компонент имени. Lua-регистрация не принимает исходное имя, поэтому вызов из Lua не выполняет это преобразование для заданной строки. | [108](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L108) |
| `name_after_edit(sender)` | `boolean, string` | Внутренний обработчик проверяет новое имя элемента списка. Регистрация скрывает аргумент с вводом и передаёт пустое значение в проверку. Вызывать этот обработчик напрямую из Lua небезопасно. | [107](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L107) |

### Режимы выбора ресурсов

| Режим | Что выбирает |
| --- | --- |
| `choose_type.custom` | Режим пользовательского заполнения списка. `create_choose` не принимает Lua-функцию заполнения. |
| `choose_type.sound_source` | Источник звука. |
| `choose_type.sound_environment` | Звуковое окружение. |
| `choose_type.library_object` | Объект из библиотеки SDK. |
| `choose_type.engine_shader` | Шейдер движка. |
| `choose_type.compiler_shader` | Шейдер компилятора. |
| `choose_type.particle_effect` | Эффект партиклов. |
| `choose_type.particle_system` | Система партиклов. |
| `choose_type.texture` | Текстура. |
| `choose_type.entity` | Тип сущности. |
| `choose_type.spawn_item` | Спавн-элемент Level Editor. В `fill_param` можно задать ключ группы, например `spawn_group`. |
| `choose_type.light_animation` | Анимация источника света. |
| `choose_type.visual` | Визуальная модель. |
| `choose_type.skeleton_animations` | Анимация скелета. Укажите относительный путь к модели в `fill_param`. |
| `choose_type.skeleton_bones` | Кость скелета. Укажите относительный путь к модели в `fill_param`. |
| `choose_type.material` | Игровой материал. |
| `choose_type.game_animation` | Игровая анимация. |
| `choose_type.game_motion` | Движение игрового объекта. |
| `choose_type.particle_anim_curve` | Кривая анимации партиклов. |

Список ресурсов для режима предоставляет конкретный редактор. Вызов `create_choose` регистрирует строковое свойство, а диалог открывается при его редактировании. [Регистрация режимов](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/utils/xrSE_Factory/properties_list_helper_script.cpp#L74-L99)

### Списки значений

Создайте и заполните списки перед передачей в `create_token8`, `create_token16`, `create_token32` или `create_list`. Храните их в полях сущности и не меняйте, пока редактор использует свойства. Свойства используют данные списка по указателю.

| Вызов | Назначение |
| --- | --- |
| `token_list()` | Создаёт соответствие имён и числовых ID. |
| `token_list:add(name, id)` | Добавляет пару. Имя и ID должны быть уникальными. |
| `token_list:remove(name)` | Удаляет запись с существующим именем. |
| `token_list:clear()` | Удаляет все записи. |
| `token_list:id(name)` | Возвращает числовой ID существующего имени. |
| `token_list:name(id)` | Возвращает имя существующего ID. |
| `rtoken_list()` | Создаёт список строк. |
| `rtoken_list:add(value)` | Добавляет строку в конец списка. |
| `rtoken_list:remove(index)` | Удаляет строку по индексу с нуля. Выход за границы ничего не меняет. |
| `rtoken_list:clear()` | Очищает список. |
| `rtoken_list:count()` | Возвращает число строк. |
| `rtoken_list:get(index)` | Возвращает строку по индексу с нуля или `nil` за пределами списка. |

## Примеры

### Число, флажок и выбор анимации

Вызовите `fill_editor_fields(prefix, items, self)` из `FillProps` вашей сущности после её базовой реализации. До вызова задайте числовое `self.intensity`, строковое `self.animation` и таблицу `self.preview` с булевым полем `enabled`. В панели появятся три свойства под группой `Light`.

```lua
local function fill_editor_fields(prefix, items, self)
    local helper = properties_helper()
    local key = prefix .. "\\Light\\"

    helper:create_float(items, key .. "Intensity", self, "intensity", 0, 10, 0.1, 2)
    helper:create_bool(items, key .. "Enabled", self, self.preview, "enabled")
    helper:create_choose(items, key .. "Animation", self, "animation", choose_type.light_animation)
end
```

### Выбор числа по подписи и выбор строки

Вызовите `fill_editor_choices(prefix, items, self)` из `FillProps` вместо предыдущего примера или вместе с ним. Инициализируйте `self.quality` значением `0` или `1`, а `self.preview_mode` — строкой `"Disabled"` или `"Enabled"`. Первое свойство сохраняет ID, второе — текст.

```lua
local function fill_editor_choices(prefix, items, self)
    if not self.quality_modes then
        self.quality_modes = token_list()
        self.quality_modes:add("Low", 0)
        self.quality_modes:add("High", 1)

        self.preview_modes = rtoken_list()
        self.preview_modes:add("Disabled")
        self.preview_modes:add("Enabled")
    end

    local helper = properties_helper()
    helper:create_token8(items, prefix .. "\\Quality", self, "quality", self.quality_modes)
    helper:create_list(items, prefix .. "\\Preview", self, "preview_mode", self.preview_modes)
end
```

## См. также

- [Пример `FillProps` в ресурсах ЗП](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/gamedata/scripts/se_new_attachable_item.script#L331)
- [Новые экспорты Lua](../lua-exports.md)
