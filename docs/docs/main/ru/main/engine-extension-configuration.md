# Настройки движка — engine_external.ltx

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 0.7

## Обзор

Файл `gamedata/configs/engine_external.ltx` относится к текущему набору игровых ресурсов. В репозитории отдельные файлы поставляются для ЗП (`gamedata`), ЧН (`gamedata_cs`), ТЧ (`gamedata_soc`) и CoC (`gamedata_coc`). У CoC используется режим платформы `cop`.

В таблицах указан резерв из C++ для отсутствующего ключа. В поставляемом LTX могут быть выставлены другие значения. Булевы опции читаются с резервом `false` и кэшируются по секциям. После изменения файла перезапустите игру.

Параметры из `[shaders_options]` и локальные настройки текстур описаны в [опциях шейдеров](../graphics/shader-options.md).

## Синтаксис

Указывайте параметры в секции, указанной в таблице. Для примера ниже нужен комплект ресурсов ЗП.

```ini
[general]
Platform = cop

[gameplay]
BalanceMode = auto
```

### [general]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `Platform` | `cop` | Режим платформы. Допустимы `cop` (ЗП), `cs` (ЧН) и `soc` (ТЧ). Секция `[general]` нужна для инициализации режима. |
| `title` | `IX-Ray Platform` | Название или ключ локализации, в том числе для статуса Discord. |
| `SaveImageSize` | `128, 128` | Ширина и высота миниатюр сохранений в пикселях. |

### [ui]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `DisableCharacterInfo` | `false` | Скрывает информацию о персонаже. |
| `DisableInventoryGrid` | `false` | Использует альтернативную текстуру сетки инвентаря. |
| `ShowLoadingStages` | `false` | Показывает стадии загрузки в интерфейсе загрузочного экрана. |
| `DisableMotionIcon` | `false` | Скрывает индикатор движения. |
| `PdaRearrangeTabButtons` | `false` | Пересчитывает расположение вкладок КПК с учётом их ширины. |
| `UseSavedGameStatic` | `false` | Использует статический элемент интерфейса для уведомления о сохранении. |
| `DisableHudRenderingOnMaster` | `false` | Скрывает HUD на сложности «Мастер». |
| `UseCompassBar` | `false` | Задаёт совместимый стартовый режим навигации с компасом. Для переключения во время игры см. документацию по HUD. |
| `WeaponIconScale` | `0.8` | Множитель размера иконки оружия. Штатные значения отличаются по платформам. |
| `InventoryItemCountPrefix` | `x` | Префикс счётчика стопки и расходников в быстрых слотах. |

### [physics]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `DeadBodyRagdoll` | `false` | Включает ragdoll для тел. |
| `DisableCameraMagicField` | `false` | Отключает специальную обработку столкновений камеры с динамическими объектами. |

### [gameplay]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `BalanceMode` | `auto` | Выбирает формулы баланса. Варианты `auto`, `soc`, `cs`, `cop`. |
| `EnableThirst` | `false` | Включает жажду. |
| `EnableSleepiness` | `false` | Включает сонливость. |
| `EnableMedIntoxication` | `false` | Включает медицинскую интоксикацию. |
| `EnableAiDieInAnomaly` | `false` | Включает ветку гибели AI в аномалиях. |
| `EnableNPCLookAtActor` | `false` | Включает поворот взгляда NPC на актёра. |
| `EnableBlockSprintInReload` | `false` | Запрещает спринт при перезарядке. |
| `EnableAutoreload` | `false` | Включает автоматическую перезарядку. |
| `EnableMonstersInventory` | `false` | Включает инвентарь монстров. |
| `EnableWeaponInertion` | `false` | Включает инерцию оружия. |
| `EnableActorStepWallmarks` | `false` | Включает следы шагов актёра по настройкам `[step_wallmark]`. |
| `DisableSprintWhileOverweight` | `false` | Запрещает спринт при превышении порога веса. |
| `EnableAlternateZoomFovCalc` | `false` | Включает альтернативный расчёт FOV при прицеливании. |
| `EnableImproveWeaponMisfire` | `false` | Выбирает расширенную обработку осечек. |
| `EnableDelayedWeaponActions` | `false` | Включает отложенные действия оружия. |
| `EnableLegacyUpgradeSystem` | `false` | Выбирает legacy-ветку улучшений, совместимую с ЧН. |
| `EnableEngineArtefactSpawn` | `false` | Включает чтение параметров спавна артефактов из конфигураций аномалий ТЧ. |
| `Enable3DPDA` | `false` | Включает 3D-КПК. |
| `EnableTorchOnlyInOutfit` | `false` | Ограничивает использование фонаря наличием экипировки. |
| `EnableBurerShieldPenetrationWithGauss` | `false` | Разрешает урон от гаусс-пушки через щит бюрера. |
| `EnableWeaponAlternateRPMSystem` | `false` | При `true` таймер выстрела получает выбранный интервал заново. [Ключи оружия и примеры](../weapon-system/weapon-config-options.md). |
| `EnableInventoryVolume` | `false` | Включает систему объёма предметов. |
| `EnableRealBulletPos` | `false` | Использует фактическую позицию выстрела при виде от первого лица. |
| `EnableWeaponAffectsOnMouseSensitivity` | `false` | Применяет `control_inertion_factor` оружия к чувствительности мыши. |
| `EnableMainMenuForDisabledControls` | `false` | Разрешает открыть главное меню при отключённом управлении. |
| `NewDemoRecordInputSchema` | `false` | Использует альтернативную схему движения камеры в demorecord. |
| `EnablePolterStaminaLooseOnHit` | `false` | Опция объявлена в перечислении, в срезе `default` вызовов чтения этого флага нет. |
| `EnableWindEffectOnABullet` | `false` | Включает экспериментальное влияние ветра на пули. |
| `EnableSaveOnNewGame` | `false` | Создаёт сохранение при старте новой игры. |
| `EnableSuicideByController` | `false` | Включает сценарий самоубийства актёра под действием контролёра. |
| `Enable3DIcons` | `false` | Включает поддержку 3D-иконок предметов. |
| `DialogFovScale` | `0.7` | Множитель FOV камеры в диалоге. |
| `TalkDof` | `0.0, 0.5, 5.0, 0.0` | Четыре компонента параметра глубины резкости в диалоге. |
| `SprintFovFactor` | `7.0` | Коэффициент эффекта FOV при спринте. |

### [spawn_supplies]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `EnableLoadoutsSupplies` | `false` | Разрешает выдачу предметов NPC из loadout. |
| `EnableSpawnFullRandomLoadout` | `false` | Выдаёт все предметы одного случайного loadout. |
| `EnableSpawnOnceRandomItemPerEachLoadouts` | `false` | Выдаёт по одному случайному предмету из каждого loadout. |
| `EnableSpawnOnceRandomitemByRandomLoadout` | `false` | Выдаёт один случайный предмет из одного случайного loadout. |

### [render]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `DisableLoadScreenTips` | `false` | Скрывает подсказки на экране загрузки. |
| `UseDynamicSnowMask` | `false` | Разрешает шейдерный вариант с текстурой `*_snowmask.dds`. |
| `UseLegacyParticleLoader` | `false` | Выбирает legacy-разбор действий из `particles.xr`, используемый в ТЧ. |
| `FontAtlasSize` | `4096` | Размер стороны атласа шрифтов в пикселях. |

### [environment]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `ReadSunConfig` | `false` | Включает чтение параметров положения солнца из конфигов погоды. |

### [sound]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `EnableWorldAmbientOcclusion` | `false` | Включает окклюзию звуков категории `SOUND_TYPE_WORLD_AMBIENT`. |

### [localization]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `PreferedFallbackLanguage` | `eng` | Резервная локаль таблицы строк. Используйте написание ключа из таблицы. |

### [system]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `EngineScriptStoryID` | `false` | Включает обработку строковых story ID движком и соответствующие Lua-экспорты. |
| `CustomMessageInClipboardOnCrash` | `false` | Разрешает пользовательский текст в буфере обмена при аварийном завершении. |
| `DisablePause` | `false` | Запрещает паузу по игровому действию `pause`. |
| `DisableConsole` | `false` | Блокирует открытие консоли по назначенной клавише в игровом интерфейсе и главном меню. |

### [player_hud]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `PlayerHudOmfAdditional` | пустая строка | Список путей или масок к дополнительным OMF анимаций рук HUD. |

### [step_wallmark]

| Ключ | Резерв | Действие |
| --- | --- | --- |
| `materials` | пустая строка | Имена материалов через запятую. |
| `left_mark` | пустая строка | Имя секции следа левой ноги. |
| `right_mark` | пустая строка | Имя секции следа правой ноги. |

## Примеры

### Выбор формул баланса

Пример для платформы ЧН с формулами ЗП.

```ini
[general]
Platform = cs

[gameplay]
BalanceMode = cop
```

`auto` выбирает платформенные формулы и сохраняет штатные правила совместимости защиты актёра и NPC. При явном выборе `soc`, `cs` или `cop` формулы защиты актёра и NPC переключаются на соответствующий режим. Данные предметов, патронов, иммунитетов и брони сохраняют значения из своих конфигураций. Ошибочное значение логируется, применяется `auto`.

## Чтение из Lua

Глобальная функция `external_ini()` возвращает `ini_file` для чтения конфигурации. Проверяйте наличие ключа для совместимости с разными наборами ресурсов.

```lua
local options = external_ini()
local enabled = options:line_exist("gameplay", "Enable3DIcons")
    and options:r_bool("gameplay", "Enable3DIcons")
```

`is_cop()`, `is_cs()` и `is_soc()` проверяют режим `Platform`.

## Рекомендации и ограничения

- `EnableInventoryPistolSlot` заменён настройкой [слотов инвентаря](../interface/inventory-slots.md) в `mod_system_ixray.ltx`.
- `EnableWeaponCollision` и `EnablePolterDrop` встречаются в поставляемых LTX. Для этих имён не найдено чтение в движке и поставляемых Lua-скриптах среза.
- `EnablePolterStaminaLooseOnHit` имеет запись в C++ enum без потребителя флага в текущих исходниках.

## См. также

- [Опции шейдеров](../graphics/shader-options.md)
- [Ключи запуска](launch-keys.md)
- [Консольные команды](console-commands.md)
