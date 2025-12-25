# Оружейные классы
## CHudItem

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

* Звуки

```ini
snd_switch_device = "snd" ; Звук вкл./выкл. налобного фонарика/ПНВ
snd_headlamp_on = "snd" ; Звук вкл. налобного фонарика
snd_headlamp_off = "snd" ; Звук выкл. налобного фонарика
snd_nv_on = "snd" ; Звук вкл. ПНВ
snd_nv_off = "snd" ; Звук выкл. ПНВ
snd_gasmask = "snd" ; Звук протирания маски
snd_prepare_detector = "snd" ; Звук доставания детектора с основным итемом (работает только при наличии анимации anm_prepare_detector)
snd_finish_detector = "snd" ; Звук прятанья детектора с основным итемом (работает только при наличии анимации anm_finish_detector)
```

Помимо всех перечисленных звуков есть возможность проигрывать звук от имени анимации, в худовой секции, пример:

```ini
anm_reload = "anim"
snd_anm_reload = "snd"

Важное уточнение. Обычные звуки тоже будут проигрываться, если на них есть особый звук, в случае с перезарядкой будет проигрываться `snd_reload` и `snd_anm_reload`, проще говоря, они не взаимозаменяемы.
```

## CMissile

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.1

* Общее

```ini
checkout_bones = idle, idle2 ; Список костей, которые будут видны только при использовании
```

* Звуки

```ini
snd_draw = "snd" ; Звук доставания
snd_holster = "snd" ; Звук прятанья
snd_throw_begin = "snd" ; Звук замаха перед броском
snd_throw = "snd" ; Звук броска
```

## CWeapon

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.1

* Общее

```ini
use_condition = false; Имеет ли предмет износ
```

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

* Обновление видимости костей

```ini
Пропись в основную секцию:
def_hide_bones = tac_foregrip, tac_handguard, tac_block
def_show_bones = def_foregrip, def_ironsight, def_handguard
def_hide_bones_override_when_gl_attached = muzzle, knife, tac_foregrip
no_scope_overriding_hide_bones = launcher_scope_static
no_scope_overriding_show_bones = launcher_scope
collimator_sights_bones = colim_sights

Пропись в секцию прицела (из параметра scopes):
bones = scope1
hide_bones = mount
overriding_hide_bones = ironsight, ironsight_sight
overriding_show_bones = ironsight_rail

Пропись в секцию апгрейда:
hide_bones_override = def_foregrip
hide_bones_override_when_silencer_attached = muzzle, knife
hide_bones_override_when_gl_attached = knife
hide_bones_override_when_scope_attached = tac_block
hide_bones = ironsight
show_bones = alt_colim_scope
```

* Кастомный размер магазина

```ini
ammo_classes = 20x70_buck, 12x70_buck ; 20х70 это нулевой тип, 12х70 первый тип
ammo_mag_size_for_type_0 = 3 ; Для 20х70 указываем размер магазина 3
ammo_mag_size_for_type_1 = 5 ; Для 12х70 указываем размер магазина 5
```

Если нет строки `ammo_mag_size_for_type_` под тип патрона, то будет использоваться дефолтный параметр `ammo_mag_size`.
Корректно работает работает только с выключенным `tri_state_reload`!!! Не работает для подствольного гранатомета и двустволок.

* Патрон в патроннике

```ini
ammo_in_chamber = false ; Нужен ли патрон в патроннике
```

Не зависит от `ammo_mag_size`, поэтому трогать его не нужно, один патрон автоматически прибавится.
Особенности:

1. Само собой при определенных условиях на счётчике патронов будет отображаться на один патрон больше;
2. Иконка патронов на худе будет соответствовать типу патрона в патроннике, а не в магазине.

Не использовать на РПГ и двустволках.

* Блокировщики инпута

```ini
block_reload = true ; Нужно ли запрещать перезарядку, если не успели закончить анимацию выстрела (передёргивание помпы или затвора)
block_firemode_glm = true ; Нужно ли запрещать анимацию смены режима огня, если находимся в режиме подствольного гранатомета
restricted_gl_and_sil = false ; Будет ли сниматься глушитель, если установлен гранатомет, и наоборот
```

Добавлен новый экстернал `EnableDelayedWeaponActions`, он не даст прервать анимацию спринта, чтобы начать анимационное действие.
Если была нажата кнопка перезарядки (выстрел, прицеливание, смена режима огня и т.д.) во время бега, тогда будем ждать конца бега и только тогда перезаряжаться, имеет смысл, если есть in/out спринт анимации.

* Система клина

Продвинутая система клина включается экстерналом `EnableImproveWeaponMisfire`, что позволит использовать новые параметры.

```ini
no_jam_fire = false ; Нужен клин перед выстрелом или во время выстрела? Если нужен во время выстрела, то false. Пропись в худовую секцию.
no_jam_in_first_shot = false ; Нужно ли блокировать клин при первом выстреле?
actor_can_shoot = true ; Может ли актор стрелять из пушки?

use_light_misfire = false ; Нужно ли использовать систему осечки
disable_light_misfires_with_detector = false ; Нужно ли отключать осечки, если активен детектор
light_misfire_start_condition = 1.0 ; Состояние пушки при котором могут начаться осечки
light_misfire_end_condition = 0.0 ; Состояние пушки при котором осечки будут константны
light_misfire_start_probability = 1.0 ; Шанс осечки в диапазоне между start и end condition
light_misfire_end_probability = 0.0 ; Шанс осечки в диапазоне ниже end condition
```

* Поломка девайсов оружия (состояние оружия)

```ini
collimator_breaking_params = -1, -1, 0 ; Поломка встроенного коллиматорного прицела, при плохом состоянии скрываются кости collimator_sights_bones
torch_breaking_params = -1, -1, 0 ; Поломка тактического фонарика, при плохом состоянии фонарик мерцает
```

Первое значение - состояние оружия при котором начнутся проблемы.
Второе значение - состояние оружия при котором девайс перестанет работать совсем.
Третье значение - шанс "пролагивания" девайса при состоянии оружия из первого значения.

* Поломка девайсов оружия (проблемы с электроникой)

```ini
misfire_after_problems_level = 10.0 ; Уровень проблем с электроникой на котором срабатывает клин оружия
collimator_problems_level = 0.0 ; Уровень проблем с электроникой на котором лагает коллиматор, скрываются кости collimator_sights_bones
```

Важно учесть, что проблемы с электроникой не будут работать, если в вашем surge_manager.script нет накопления уровня проблем от 0 до 5.

* Система прицелов

```ini
snd_scope_brightness_plus = "snd" ; Звук увеличения яркости прицела
snd_scope_brightness_minus = "snd" ; Звук уменьшения яркости прицела
snd_scope_zoom_plus = "snd" ; Звук увеличения кратности прицела
snd_scope_zoom_minus = "snd" ; Звук уменьшения кратности прицела
snd_scope_zoom_gyro = "snd" ; Звук плавной смены кратности прицела

min_lens_factor = 1.0 ; Минимальное значение кратности прицела
max_lens_factor = 1.0 ; Максимальное значение кратности прицела
lens_speed = 0.0 ; Скорость смены кратности
lens_gyro_sound_period = 0.0 ; Задержка перед проигрыванием звук snd_scope_zoom_gyro
lens_factor_levels_count = 5.0 ; Число позиций кратности прицела
force_zoom_sound = false ; Играть звуки смены кратности, даже если упёрлись в минимум/максимум
```

* Стрейфы (SWM)

```ini
strafe_enabled = true; Разрешены ли стрейфы оружия
strafe_aim_enabled = false; Разрешены ли стрейфы оружия в режиме прицеливания
strafe_hud_offset_pos = 0.015, 0, 0; По каким осям и насколько сильно оружие будет менять позицию в стрейфах
strafe_hud_offset_rot = 0, 0, 4.5; По каким осям и насколько сильно оружие будет менять ротацию в стрейфах
strafe_aim_hud_offset_pos = 0.005, 0, 0; По каким осям и насколько сильно оружие в режиме прицеливания будет менять позицию в стрейфах
strafe_aim_hud_offset_rot = 0, 0, 2.5; По каким осям и насколько сильно оружие в режиме прицеливания будет менять ротацию в стрейфах
strafe_transition_time = 0.5; Время за которое оружие сместится до координат strafe_hud_offset_pos и strafe_hud_offset_rot
strafe_aim_transition_time = 0.15; Время за которое оружие сместится до координат strafe_aim_hud_offset_pos и strafe_aim_hud_offset_rot
```

* Коллизия худа (SWM)

```ini
nearwall_dist_min = float       ; Минимальная дистанция при которой начинает работать коллизия
nearwall_dist_max = float       ; Максимальная дистанция при которой работает коллизия
nearwall_target_hud_fov = float ; Насколько сильно максимально смещается худ фов
nearwall_speed_mod = float      ; Скорость смещения
```

* Инерция (SWM)

```ini
inertion_tendto_speed = float         ; Скорость, с которой инерция стремится вернуться к исходному положению после движения
inertion_tendto_aim_speed = float     ; Скорость инерционного возвращения при прицеливании
inertion_tendto_ret_speed = float     ; Скорость, с которой инерция возвращается в исходное положение
inertion_tendto_ret_aim_speed = float ; Скорость инерционного возвращения в исходное положение после окончания прицеливания

inertion_min_angle = float       ; Минимальный угол, на который воздействует инерция
inertion_min_angle_aim = float   ; Минимальный угол инерции при прицеливании

inertion_offset_LRUD = float     ; Величина смещения инерции влево, вправо, вверх и вниз
inertion_offset_LRUD_aim = float ; Величина смещения инерции влево, вправо, вверх и вниз при прицеливании
```

> [!IMPORTANT]  
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.2

* Система прицелов от Mortan

### Принцип работы

В основную секцию оружия прописываются следующие параметры:

```ini
parent_section = string ; Вместо string написать изначальную секцию оружия без кавычек (например: wpn_ak74)
scopes = scope1, scope2, etc ; Секции прицелов, которые можно нацепить на оружие.
```

Далее создаётся специальная секция оружия под определённый прицел по принципу `названиеоружия_названиеприцела` (пример: wpn_ak74_ekp), наследующая оригинальную секцию оружия. Там задаётся отдельный визуал, HUD, координаты прицела и.т.д.

## CWeaponKnife

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

```ini
snd_draw = "snd" ; Звук доставания
snd_holster = "snd" ; Звук прятанья
snd_kick_1 = "snd" ; Звук удара на ЛКМ
snd_kick_2 = "snd" ; Звук удара на ПКМ

Важно учесть. Наличие `snd_kick_1` и `snd_kick_2` исключает проигрывание звука `snd_shoot`.
```

## CWeaponMagazined

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.1

* Звуки

```ini
snd_reload_misfire = "snd" ; Звук перезарядки при клине
snd_reload_empty = "snd" ; Звук при перезарядке пустого магазина
snd_aim = "snd" ; Звук входа в прицеливание
snd_aim_out = "snd" ; Звук выхода из прицеливания
```

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

* Звуки

```ini
snd_aim_start = "snd" ; Звук входа в прицеливание (переименование с snd_aim в snd_aim_start)
snd_aim_end = "snd" ; Звук выхода из прицеливания (переименование с snd_aim_out в snd_aim_end)
snd_reload_jammed | snd_reload_misfire = "snd" ; Звук перезарядки при клине (поддержка snd_reload_misfire не исчезла! Это не переименование!)
snd_reload_jammed_last | snd_reload_misfire_last = "snd" ; Звук перезарядки при клине, при нуле патронов
snd_reload_jammed_detector | snd_reload_misfire_detector = "snd" ; Звук перезарядки при клине, с активным детектором
snd_reload_jammed_last_detector | snd_reload_misfire_last_detector = "snd" ; Звук перезарядки при клине, при нуле патронов, с активным детектором
snd_changecartridgetype = "snd" ; Звук перезарядки, при смене типа патронов (не работает при нуле патронов)
snd_changefiremode = "snd" ; Звук переводчика режима огня
snd_torch_on = "snd" ; Звук вкл. тактического фонарика
snd_torch_off = "snd" ; Звук выкл. тактического фонарика
snd_breechblock = "snd" ; Звук помпы или затвора болтовки
snd_jam = "snd" ; Звук заклинившей помпы или заклинившего затвора болтовки
snd_light_misfire = "snd" ; Звук лёгкой осечки (работает только при включенной лёгкой осечке и включенным экстерналом EnableImproveWeaponMisfire)
snd_shoot_actor = "snd" ; Звук выстрела актором
snd_shot_last_actor = "snd" ; Звук выстрела с последним патроном актором
snd_silncer_shot_actor = "snd" ; Звук выстрела с глушителем актором
snd_silencer_shot_last = "snd" ; Звук выстрела с глушителем, с последним патроном
snd_silencer_shot_last_actor = "snd" ; Звук выстрела с глушителем, с последним патроном актором
```

### CWeaponMagazinedWGrenade

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

* Звуки

```ini
snd_shoot_grenade_actor = "snd" ; Звук выстрела из подствольного гранатомёта актором
snd_load_grenade | snd_reload_grenade = "snd" ; Звук перезарядки подствольного гранатомёта (альтернативный, не заменяет snd_reload_grenade)
snd_switch_g = "snd" ; Звука выхода из подствольного гранатомёта
```

### CWeaponBM16

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

* Звуки

```ini
snd_reload_only = "snd" ; Звук перезарядки с одним патроном в инвентаре заряжаемого типа
snd_reload_only_ammochange = "snd" ; Звук перезарядки с одним патроном в инвентаре заряжаемого типа, при смене типа
snd_changecartridgetype_one = "snd" ; Звук перезарядки при смене типа с не полностью заряженным оружием
snd_changecartridgetype_only = "snd" ; Звук перезарядки при смене типа с одним патроном в инвентаре заряжаемого типа и с полностью заряженным оружием
snd_changecartridgetype_one_only = "snd" ; Звук перезарядки при смене типа с одним патроном в инвентаре заряжаемого типа и с не полностью заряженным оружием

Важное уточнение. Вышеуказанные звуки работают только с включенным `use_alt_reload_system` в секции оружия. Также, помимо этих звуков используются звуки `snd_changecartridgetype` и `snd_reload_empty`, отключается звук `snd_reload_1`.
```

### CWeaponShotgun | CWeaponAutomaticShotgun

> [!IMPORTANT]
> **Статус**: Поддерживается <br>
> **Минимальная версия**: 1.3

```ini
add_cartridge_in_open = false; Нужно ли добавлять патрон в open анимациях, пропись в худовую секцию
```

* Звуки

```ini
snd_open_weapon_empty = "snd" ; Звук начала перезарядки пустого дробовика
snd_add_cartridge_empty = "snd" ; Звук вставки патрона в пустой дробовик
snd_add_cartridge_preloaded = "snd" ; Звук вставки патрона в дробовик после вставки патрона в патронник
snd_close_weapon_empty = "snd" ; Звук конца перезарядки пустого дробовика (счётчик патронов должен быть 0)
snd_close_weapon_preloaded = "snd" ; Звук конца перезарядки дробовика после вставки патрона в патронник

Важное уточнение. Некоторые вышеуказанные звуки работают только с параметром add_cartridge_in_open.
```
