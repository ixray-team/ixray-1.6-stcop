# Техническая спецификация: перенос транспорта X-Ray-Cars-Ext в IXRAY 1.6 STCOP

Статус: утверждена  
Дата: 2026-11-01  
Источник функциональности: `H:\S.T.A.L.K.E.R. - Lost Alpha DC\X-Ray-Cars-Ext`  
Текущий проект: `H:\IXRAY\ixray-1.6-stcop`  
Ресурсы текущего аддона: `H:\IXRAY\ixr_addons\carpack`  
Ресурсы источника: `H:\S.T.A.L.K.E.R. - Lost Alpha DC\pack\gamedata_mod`

## 1. Обзор

Текущий `CCar` в IXRAY умеет базовый транспорт: физический shell, колёса, двери, одну турель, фары, выхлоп, повреждения, взрыв, топливо и простой HUD. X-Ray-Cars-Ext содержит существенно более полную реализацию: модель ДВС с кривой мощности, КПП/CVT, ABS, дифференциал, подвеску, дрифт/nitro, рекурсивные двери, несколько турелей, пять групп света, расширенную звуковую state-machine, пять камер, 3D-приборы на костях и детальную конфигурацию через OGF user data.

Задача — заменить ядро транспорта реализацией Cars-Ext, сохранив архитектурные контракты IXRAY: `xrGame`-only, текущий Lua API, нативный UI, `bind_physic_object`, текущий MP-слой и кросс-платформенную сборку. Первая итерация — одиночная игра на трёх текущих машинах `carpack`; полноценная клиент-серверная синхронизация выделена в отдельную фазу.

## 2. Архитектурное решение

### 2.1. Принципы

1. **Cars-Ext — эталон поведения.** Формулы, порядок обновления, параметры и структура классов берутся из Cars-Ext. Отклонения допускаются только там, где API IXRAY отсутствует или несовместим, и фиксируются в реестре отклонений.
2. **`CCar` остаётся единым data-driven классом.** Декомпозиция на ECS/компоненты не выполняется: это уменьшает риск расхождения с источником и сохраняет все текущие `cast_car()`-контракты.
3. **Ядро не меняет движковые модули.** `xrEngine`, `xrPhysics`, `xrRender`, `xrSound` не изменяются. Если фича требует правки движка, она упрощается, опускается или уходит в extras.
4. **Сначала SP, затем MP.** Ядро проектируется с точками расширения для сети, но не расширяет текущий `SyncRead/SyncWrite`. Текущий MP-слой должен продолжать компилироваться и работать в прежнем объёме.
5. **Новые сейвы.** Формат сохранения меняется; миграция старых сохранений не выполняется.
6. **Контент — только `carpack`.** Три текущие машины; меши/конфиги LA не копируются. Иконки и локализация LA не переносятся.

### 2.2. Модули и файлы

| Файл/группа | Действие | Примечание |
| --- | --- | --- |
| `src/xrGame/Car.h`, `Car.cpp` | Заменить ядром Cars-Ext, вернуть IXRAY-специфику | 13 базовых классов, `XREFLECT`, MP-методы, `cast_car()` |
| `src/xrGame/CarWheels.cpp` | Заменить | Hinge2, подвеска, ABS, дифференциал, slip |
| `src/xrGame/CarDoors.cpp` | Заменить | `SDoor` вместо `CCarDoor`; рекурсивные двери, багажник |
| `src/xrGame/CarDoors.h` | Удалить или оставить пустой shim | Внешних пользователей `CCarDoor` нет; проверить CMake |
| `src/xrGame/CarLights.h`, `CarLights.cpp` | Заменить | Пять групп света; flare-эффекты упрощаются |
| `src/xrGame/CarWeapon.h`, `CarWeapon.cpp` | Заменить | `CCarTurret` + `CCarWeapon`, ракеты, слоты |
| `src/xrGame/CarSound.cpp` | Заменить | `SCarSound` state-machine; OpenAL/`ref_sound` |
| `src/xrGame/CarInput.cpp` | Заменить, сохранить MP-отправку | `GAME_EVENT_MP_CAR_INPUT`, текущие биндинги |
| `src/xrGame/CarExhaust.cpp` | Заменить | Частицы выхлопа, nitro/pop |
| `src/xrGame/CarDamageParticles.h`, `.cpp` | Заменить | Партиклы повреждений |
| `src/xrGame/CarCameras.cpp` | Заменить | Пять камер, FOV-эффекты, инерция |
| `src/xrGame/car_memory.h`, `.cpp` | Добавить | AI-видимость для турелей и `isObjectVisible` |
| `src/xrGame/CarScript.cpp` | Не переносить | Lua-контракт реализуется через `XREFLECT` в `Car.h` |
| `src/xrGame/DBG_Car.cpp` | Опционально | Только DEBUG; не влияет на приёмку |
| `src/xrGame/ActorVehicle.cpp`, `ActorInput.cpp`, `ActorAnimation.*` | Адаптировать | Вход/выход, анимации, ввод, `use_Holder` |
| `src/xrGame/Actor.*` | Минимально адаптировать | `m_holder`, `m_holderID`, HUD, тени |
| `src/xrGame/Level.cpp` | Добавить проход 3D-приборов | `CCar::Draw_3D_UI` из `CLevel::Render` |
| `src/xrGame/ui/UICarPanel.*` | Расширить | Скорость, RPM, передача, топливо, здоровье |
| `src/xrGame/ui/UICarBodyWnd.*` | Сохранить/адаптировать | Нативный багажник |
| `src/xrGame/UIMainIngameWnd.*`, `UIGameCustom.cpp`, `UIActorMenu.cpp` | Адаптировать | HUD, окно багажника, иконка/имя машины |
| `src/xrGame/HUDManager.*` | Адаптировать при необходимости | Скрытие HUD, звуковые/light-эффекты в xrGame |
| `src/xrGame/Inventory.cpp` | Проверить/адаптировать | Инвентарь машины, `trunk_capacity` |
| `src/xrGame/console_commands.cpp` | Опционально | Отладочные `rs_car_spring`/`rs_car_damping` |
| `src/xrGame/CMakeLists.txt` | Проверить | Новые `car_memory.*`, удалённый `CarDoors.h` |
| `src/xrServerEntities/...` | Не менять в ядре | `CSE_ALifeCar` и Sync остаются текущими |
| `gamedata/configs/misc/vehicles.ltx` | Оставить/минимально править | Три item-секции |
| `gamedata/configs/models/vehicles/*.ltx` | Оставить | Три модели, подключаются через OGF `#include` |

### 2.3. Иерархия и контракты классов

`CCar` сохраняет роль «god object» и наследует:

- `CEntity`
- `CScriptEntity`
- `CPHUpdateObject`
- `CHolderCustom`
- `CPHSkeleton`
- `CDamagableItem`
- `CPHDestroyable`
- `CPHCollisionDamageReceiver`
- `CHitImmunity`
- `CExplosive`
- `CRocketLauncher`
- `CDelayedActionFuse`
- `CInventoryOwner`

Ключевые контракты:

- `cast_car()` возвращает `CCar*`; все текущие вызовы в `Actor`, `HUDManager`, `PDA`, `burer`, `CHolderCustom` сохраняются.
- `CHolderCustom` по-прежнему поддерживает **одного водителя**. Пассажирские места не добавляются.
- `CCar` остаётся `final`; отдельный MP-класс в ядре не создаётся.
- `CCar::allowWeapon()` и `CCar::HUDView()` сохраняют текущую семантику; `HUDView()` — только первая камера.
- `CCar::GetInventory()` и инвентарь машины сохраняются; `CUICarBodyWnd` продолжает работать.
- `CCar::Use()` приоритизирует ближайший интерактивный элемент: `usable_bones`, багажник, дверь; вход выполняется только если интерактивных элементов нет.
- `CCar::DoExit()` выбирает ближайшую/открытую дверь, затем `ExitPosition()`.
- `CCar::attach_Actor()` отказывает при занятом месте; `detach_Actor()` очищает владельца, камеру, HUD, видимость актёра и звуковой фактор.
- `m_sits_transforms` не растёт при повторных входах: используется фиксированная трансформация водителя, очищаемая в `detach_Actor()`/`net_Destroy()`.

Новые/заменяемые вспомогательные типы:

- `SWheel`, `SWheelDrive`, `SWheelSteer`, `SWheelBreak`
- `SDoor` (заменяет `CCarDoor`), состояния `opening/closing/opened/closed/broken`
- `SCarSound`
- `SExhaust`
- `CCarLights`, `SCarLight`
- `CCarTurret`, `CCarWeapon`
- `CCarDamageParticles`
- `car_memory`
- массив из пяти камер (`EctFirstCam`, `EctSecondCam`, `EctThridCam`, `EctFourthCam`, `EctFiveCam`)

### 2.4. Конфигурация и поток данных

Источник истины — OGF user data, но фактически это цепочка:

```
spawn-секция (class=SCRPTCAR, visual=...ogf)
  -> OGF chunk 0x11 (USERDATA)
     -> #include "models\vehicles\<model>.ltx"
        -> CInifile
           -> CCar::ParseDefinitions()
           -> CCar::Init()
```

Проверено: OGF-меши `carpack` идентичны LA, user data содержит `#include` на `configs/models/vehicles/*.ltx`. Переэкспорт OGF не требуется.

Два независимых источника параметров:

1. **`pSettings` (item-секция `vehicles.ltx`)** — параметры экземпляра:
   `fuel_tank`, `fuel_consumption`, `fuel_speed_factor`, `fuel_speed_optimal`, `fuel_rpm_factor`, `fuel_gas_factor`, `trunk_capacity`, `trunk_distance`, `hit_stall_threshold`, `handbrake_rpm_threshold`, `slip_sliding_threshold`, `vel_sliding_threshold`, `friction_*`, `steer_*`, `drift_*`, `brake_boost`, `switch_shake_boost`, `engine_shake_boost`, `wheels_spin_factor_*`, `wheels_sound_delay`, `tire_*`, `icon`, `damage`, `immunities`, `collide`, `physics_common`.
2. **`LL_UserData` (модельный ltx)** — параметры машины:
   `[car_definition]`, `[transmission_gear_ratio]`, `[lights]` + секции ламп, `[car_sound]`, `[doors]` + секции дверей, `[wheels_params]` + секции колёс, `[damage_particles]`, `[damage_items]`, `[air_resistance]`, `[explosion]`, `[destroyed]`, `[mounted_weapon_definition]`, `[animations]`, секции приборов (`[speedometer]`, `[tachometer]`, `[fuel]`, `[temp]`, `[gear]`, `[fan]`, `[clock_*]`), `[car_firsteye_cam]`, `[car_look_cam]`, `[car_free_cam]`, `[disable]`.

Правила:

- Отсутствующий необязательный ключ → дефолт Cars-Ext + предупреждение в лог.
- Отсутствующая кость/колесо/дверь → подсистема отключается, машина спавнится.
- Отсутствующий `visual` → фатальная ошибка спавна.
- `[grass_coll_spheres]` читается, но игнорируется (grass collision — out of scope).
- `gyroscopic_mode` принимается, но игнорируется (нет `dBodySetGyroscopicMode`).
- `flare*` принимается, но lens flare не создаётся; остаётся light/glow.

### 2.5. Lua-контракт

Сохраняется текущий `XREFLECT`-API:

- свойства: `fuel`, `fuel_tank`
- методы: `add_fuel`, `Action`, `SetParam`, `HasWeapon`, `CanHit`, `FireDirDiff`, `IsObjectVisible`, `CurrentVel`, `GetfHealth`, `SetfHealth`, `SetExplodeTime`, `ExplodeTime`, `CarExplode`
- константы: `CCar.eWpnDesiredDir`, `eWpnDesiredPos`, `eWpnActivate`, `eWpnFire`, `eWpnAutoFire`, `eWpnToDefaultDir`
- конструктор `CCar()`

Добавляются совместимые алиасы Cars-Ext:

- `GetFuel()`
- `Refuel()`
- `ShowTrunk()`
- `OpenTrunkBone()`
- `CloseTrunkBone()`
- `TrunkBoneWorldPos()`
- `engaged()` (через `CHolderCustom`/`holder`)

`CarScript.cpp` из Cars-Ext не переносится; то же поведение реализуется через `XPROPERTY(LuaCallable)`.

Сохраняются:

- `usable_bones` / `usable_bones_callback`;
- `on_use_trunk` как опциональный алиас, если секция присутствует;
- `holder`-методы `engaged`, `Action`, `SetParam`, `SetEnterLocked`, `SetExitLocked`;
- `get_car`, `get_current_holder`, `attach_vehicle`, `detach_vehicle` — аудит и сохранение текущих реализаций.

### 2.6. Сеть и MP-слой

Ядро сохраняет текущий контракт:

- `CCar::SyncRead` / `SyncWrite` — без расширения полей.
- `CCar::net_Relevant()` = `IsMyCar() || OnServer()`.
- `CCar::net_Export` / `net_Import` — текущие пустые реализации.
- `GAME_EVENT_MP_CAR_INPUT` — сохраняется во всех обработчиках ввода при `OnClient()`; формат `ID машины + dik/id + pressed` не меняется.
- `CCar::IsMyCar()`, `m_CarNetUpdates`, `Interpolate()` — сохраняются.
- `CSE_ALifeCar` — без новых полей в ядре.
- `FreeMP/game_sv_freemp.cpp` — не меняется.

Фаза MP (после ядра) будет проектироваться как **owner-authoritative + server validation**: владелец симулирует и отправляет состояние, сервер хранит копию, валидирует и ретранслирует, остальные клиенты интерполируют. В ядре только фиксируются точки расширения; новые состояния (двери, топливо, оружие, здоровье) в `Sync*` не добавляются.

### 2.7. Потоки и рендер

- `PhDataUpdate` — только физика, топливо, КПП, двери, руль. Запрещены звук, свет, частицы, UI, Lua и любые вызовы, требующие `!physics_world()->Processing()`.
- Все звуковые/световые/UI-события из физического шага ставятся в очередь `EAsyncCalls` и выполняются в `UpdateCL`/`VisualUpdate` на основном потоке.
- `cam_Update` вызывается только из `UpdateEx`, не из физического шага.
- `renderable_Render` — базовый рендер, турели, 3D-приборы; без Lua и без создания ресурсов.
- `CCar::Draw_3D_UI` вызывается из `CLevel::Render` (xrGame), использует `UIRender`; никаких правок `xrRender`.
- `VisualUpdate` обновляет интерполяцию shell, посадку актёра, звук, HUD, выхлоп, свет, 3D-приборы.
- `UpdateCL` без владельца выполняет `VisualUpdate(90)` и `CScriptEntity::ProcessScripts`; с владельцем — `UpdateEx`.

### 2.8. Физика

- Машина — rigid-body shell, собранный из bone shapes: `P_build_Shell(this, false, &bone_map)`.
- Колёса — Hinge2 (`jtWheel`), двери — Hinge (`jtJoint`).
- Используются `CPHSkeleton`, `CPHUpdateObject`, `CPhysicsShell`, `CPhysicsElement`, `CPhysicsJoint`; изменения в `xrPhysics` не требуются.
- `SetPrefereExactIntegration`, `SetAirResistance`, `set_DynamicLimits`, `ApplySpawnIniToPhysicShell` для `[physics_common]`, `[collide]`, `[animated_object]`.
- `EffectiveGravity`/`PhTune` сохраняют поведение Cars-Ext; двойная гравитация не допускается.
- `dBodySetGyroscopicMode` отсутствует — `gyroscopic_mode` игнорируется с однократным предупреждением.
- `character_scraper` — best-effort: если реализуется на существующих xrPhysics API, включается; иначе пропускается с предупреждением.

### 2.9. Сохранение и загрузка

Новый формат; старые сейвы не поддерживаются.

Сохраняются:

- позиция/угол;
- здоровье машины;
- топливо;
- состояние и здоровье дверей;
- здоровье колёс.

Не сохраняются (сбрасываются при загрузке):

- двигатель вкл/выкл, RPM, передача, сцепление;
- свет, турели, боезапас, fire mode;
- камера, ввод, nitro, частицы, damage particles;
- очередь `m_CarNetUpdates`.

При загрузке: двигатель выключен, свет выключен, турели деактивированы, двери/колёса восстановлены, топливо/здоровье восстановлены.

Если меняется layout `CSE_ALifeCar::STATE_*`, поднимается `SPAWN_VERSION`; если только `CCar::save/load`, вводится версия формата CCar. Точный механизм — на этапе реализации; миграция не предусмотрена.

## 3. Ограничения и краевые случаи

### 3.1. Поддерживается

- Полная ходовая модель Cars-Ext: ДВС, КПП/CVT, ABS, дифференциал, подвеска, дрифт, nitro, топливо.
- Колёса, двери, багажник, рекурсивные двери, повреждения колёс/дверей/кузова.
- Турели/оружие/ракеты, несколько слотов `mounted_weapon_definition_N`.
- Пять групп света: headlights, stoplights, indoorlights, reverselights, enginelights.
- Выхлоп, nitro/pop-частицы, damage-частицы.
- Взрыв, destroyed-состояние, отложенный fuse, `CPHDestroyable`.
- Звук двигателя/КПП/тормозов/дверей/сигнала; OpenAL/`ref_sound`.
- Пять камер, FOV-эффекты, инерция, свободная камера.
- Нативный HUD: здоровье, топливо, скорость, RPM, передача, лампы.
- 3D-приборы на костях: speedometer, tachometer, fuel, temp, gear, fan, clock.
- Нативный багажник `CUICarBodyWnd`.
- Текущий Lua API + алиасы Cars-Ext.
- Текущий MP-слой в прежнем объёме.
- Три текущие машины `carpack`: `veh_zaz968`, `veh_zaz968_2`, `veh_niva_green`.

### 3.2. Не поддерживается (out of scope)

- Полноценная MP-синхронизация новых состояний (двери, топливо, оружие, здоровье, места) — отдельная фаза.
- Старые сохранения и миграция.
- `bind_car.script`, `ph_car.script`, `ui_car_buy/ui_car_trunk/ui_car_info`, `la_utils` и прочий LA-скриптовый слой.
- AI-вождение (`ph_car`), патрули, стрельба AI с турели.
- Покупка через UI/диалоги; ремонт и заправка через инвентарь машины реализованы (`repair_item` / `refuel_item` / `refuel_amount` в `vehicles.ltx`).
- Пассажирские места, несколько владельцев, пассажирские камеры/анимации.
- FMOD-радио и DSP-плейлист.
- Collision-сферы травы (`Device.m_detcollslots`) и деформация травы.
- 3D UI text/video на костях (`3d_ui_text_*`, `3d_ui_video_*`); в трёх текущих конфигах не используется.
- Полный контент LA: 59 машин, 90 модельных ltx, 350 МБ мешей, 3.4 ГБ текстур.
- Иконки `ui_veh_*`, атласы `ui_trunk_vehicles*`, `string_table_enc_vehicles.xml`.
- `CarConfig`/`CarEditor` как рабочий инструмент; остаются компилируемыми, но нерабочими.
- Правки `xrEngine`, `xrPhysics`, `xrRender`, `xrSound`.
- Lens flare, `gyroscopic_mode`, глобальный `psSoundCarFactor`.
- `DBG_Car.cpp` и отладочные команды — опционально, не критерий приёмки.

### 3.3. Краевые случаи и поведение

| Ситуация | Поведение |
| --- | --- |
| Нет OGF user data / `#include` | Дефолты, предупреждение, машина спавнится с базовым поведением |
| Нет `visual` | Фатальная ошибка спавна, объект не создаётся |
| Нет кости колеса | Колесо пропускается, предупреждение; машина остаётся управляемой на остальных |
| Нет кости двери/багажника | Дверь/багажник пропускается, предупреждение |
| Нет `driver_place` | Посадка невозможна, предупреждение; машина спавнится |
| Нет `transmission_gear_ratio` | Одна передача по умолчанию |
| Нет `[mounted_weapon_definition]` | Турель отсутствует |
| Нет `[car_sound]` | Машина без звука |
| Нет `[lights]` | Машина без света |
| `[damage_items]` содержит не колесо/дверь | Предупреждение, запись игнорируется |
| `fuel_tank <= 0` | Расход отключён, предупреждение |
| `reference_radius <= 0` | Защита от деления на ноль, дефолт |
| `max_rpm <= min_rpm` | Клэмп, предупреждение |
| `Device.dwFPS <= 0` | Клэмп инерции камеры |
| Повторный вход/выход | `m_sits_transforms` не растёт; владелец/камера/HUD очищаются |
| Смерть/удаление актёра в машине | `net_Relcase`/`net_Destroy` отсоединяют водителя, без dangling `OwnerActor` |
| Уничтожение машины с водителем | Водитель высаживается/эвакуируется, взрыв, без зависания |
| Save/load в движении | Позиция/здоровье/топливо/двери/колёса восстанавливаются; transient state сбрасывается |
| Старый сейв | Понятная ошибка/отказ загрузки, без краша |
| MP без новых полей | Машины могут расходиться по дверям/топливу/оружию; известно и документировано до фазы MP |
| `SyncWrite` > 255 байт | Не допускается; новые поля в ядре не добавляются |
| Отсутствующие иконки/локализация | Принято: машина может показывать `Vehicles`/пустую иконку; не блокер |

## 4. Критерии приёмки (Definition of Done)

Проверка — **сборка + ручной прогон**. Unit/integration/benchmarks не обязательны по решению; они могут быть добавлены позже.

### 4.1. Сборка

- [ ] CMake configure/build x64 Debug и Release, `xrGame.dll` собирается без unresolved externals.
- [ ] `IXRAY_MP`/FreeMP-сборка компилируется; текущие `SyncRead/SyncWrite` и `GAME_EVENT_MP_CAR_INPUT` не сломаны.
- [ ] Новый код не использует WinAPI, FMOD, MSVC-only расширения; типы `u32`/`xr_string`/`shared_str`; стиль `.clang-format`.
- [ ] `CarDoors.h`/`CCarDoor` не оставляют висячих включений; CMake видит `car_memory.*`.
- [ ] `CarConfig`/`CarEditor` компилируются (функциональность не проверяется).

### 4.2. Одиночная игра

- [ ] Спавн `veh_zaz968`, `veh_zaz968_2`, `veh_niva_green`; каждая машина создаётся, лог показывает загрузку конфига.
- [ ] Вход/выход водителя; камеры first/chase/free/free2/free-fly; актёр скрывается/показывается; HUD появляется/скрывается.
- [ ] Запуск/остановка двигателя, холостой ход, газ, задний ход, нейтраль, ручная/авто/CVT КПП, переключения, stall, limiter.
- [ ] Nitro, расход топлива, пустой бак → двигатель глохнет; `Refuel`/`AddFuel` работают.
- [ ] AWD/RWD, руль, тормоза, ручник, ABS, дифференциал, подвеска, burnout/drift/wheelie.
- [ ] Колёса: звук, частицы, wallmarks, повреждение, два уровня здоровья.
- [ ] Двери: open/close/use/switch, broken handle, spring/lamba/vertical, рекурсивные двери, авто-закрытие.
- [x] Анимация водителя включается и выключается сразу по итоговому состоянию руления, включая комбинации клавиш.
- [x] Звук шин останавливается для каждого колеса отдельно; состояние заноса сбрасывается при потере контакта.
- [ ] Багажник: открытие/закрытие, `CUICarBodyWnd`, `trunk_capacity`/`trunk_distance`, `ShowTrunk`/`OpenTrunkBone`/`CloseTrunkBone`/`TrunkBoneWorldPos`.
- [x] Ремонт и заправка из инвентаря машины: `repair_item` / `refuel_item` / `refuel_amount` в `vehicles.ltx`; только когда актёр находится в машине и скорость не выше 1 км/ч; полный ремонт дверей и колёс при каждом использовании; SP, MP-события запланированы.
- [ ] Свет: headlights/stoplights/reverselights/indoorlights/enginelights; переключение; свечение; отсутствие flare не считается багом.
- [ ] Турель: активация, наведение, fire, auto fire, боезапас, ракеты (если есть в конфиге); машина без турели работает.
- [ ] Урон: кузов/колесо/дверь; уровни повреждений; damage particles; взрыв; destroyed; водитель не застревает.
- [ ] Звук: engine start/stop/idle/drive, transmission, gas/brake/valve/turbo/decel/heavy/blower/nitro/whine/horn/pop, двери; интерьерная громкость.
- [ ] HUD: здоровье, топливо, скорость, RPM, передача, лампы; 3D-приборы (speedometer/tachometer/fuel/temp/gear/clock) анимируются.
- [x] Прицел в машине скрыт, пока не активна турель с оружием.
- [ ] Save/load: позиция, здоровье, топливо, двери, колёса; transient state сброшен; старый сейв отклонён.
- [ ] Lua: текущий API и алиасы; `ph_minigun`-подобный сценарий турели; `usable_bones`; `on_use_trunk` при наличии.
- [ ] Краевые конфиги: удалить `[car_sound]`, `[lights]`, `[mounted_weapon_definition]`, `[damage_particles]`, `[transmission_gear_ratio]` по одному; машина спавнится, пишет предупреждения, не падает.
- [ ] Удалить/переименовать кость колеса/двери; подсистема отключается, машина спавнится.
- [ ] 10-минутный прогон с 3 машинами, светом, турелью и частицами: без крашей, зависаний и утечек; нет `R_ASSERT`/`VERIFY`/`BEER`.
- [ ] Регрессия: актор, инвентарь, оружие, стационарные holder'ы, `CUICarBodyWnd`, `cast_car()`-вызовы работают как до изменений.

### 4.3. MP-сборка (без расширения)

- [ ] FreeMP-сборка запускается; машина спавнится; текущий `SyncRead/SyncWrite` работает в прежнем объёме (двигатель, свет, владелец, передача, физика).
- [ ] `GAME_EVENT_MP_CAR_INPUT` доходит до сервера; ввод применяется как раньше.
- [ ] Новые поля (двери, топливо, оружие, здоровье) в сеть не отправляются; расхождения документированы.

### 4.4. Документация и отклонения

- [ ] Заполнен реестр отклонений от Cars-Ext: FMOD-радио, lens flare, gyroscopic mode, глобальный sound factor, grass collision, 3D text/video, AI/purchase/repair UI, passenger seats, старые сейвы, иконки/локализация.
- [ ] В логе нет ошибок missing texture, кроме принятых иконок.
- [ ] Известные ограничения MP и контента описаны в отчёте.

## 5. Риски и открытые вопросы

1. **MP-фаза** — owner-authoritative + server validation ещё не спроектирована детально; текущий гибридный Sync остаётся техническим долгом.
2. **Слияние `XREFLECT` и Cars-Ext** — возможны конфликты имён/перегрузок `Action`/`SetParam`; требует аккуратного маппинга.
3. **Отсутствующие LA API** — `psSoundCarFactor`, `dBodySetGyroscopicMode`, `HUD().Add_LensFlare`, `roll_lock`, `Draw_3D_UI`; часть реализуется в xrGame, часть упрощается.
4. **Физика** — ODE IXRAY может дать отличные от LA ощущения; отклонения фиксируются, а не маскируются.
5. **Контент** — `veh_niva_green` без `inv_name`/`icon`; `veh_zaz968_2` использует тот же ltx, что и `veh_zaz968`; `kamaz`/`super_niva` не подключены.
6. **Сеть** — `SyncWrite` ограничен 255 байтами; новые поля нельзя добавлять без пересмотра протокола.
7. **Сохранения** — старые сейвы несовместимы; это осознанное решение.
8. **Редактор** — `CarConfig`/`CarEditor` остаются нерабочими; при поломке компиляции допускается минимальный shim, но не развитие функциональности.
