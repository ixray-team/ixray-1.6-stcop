# План разработки: перенос транспорта X-Ray-Cars-Ext в IXRAY 1.6 STCOP

Статус: на утверждении  
Дата: 2026-11-01  
Спецификация: `docs/engine/transport-cars-ext-spec.md`  
Ветка: `feature/transport-cars-ext` (создаётся на этапе A)  
Режим: поэтапный, с обязательными сборочными гейтами

## 0. Правила выполнения

1. Каждый этап — отдельный коммит (или серия коммитов) в ветке `feature/transport-cars-ext`. Основная ветка не меняется до финального ревью.
2. Полная сборка выполняется на гейтах, указанных в таблице ниже. Между гейтами допускаются промежуточные коммиты, которые не линкуются, но каждый заменяемый файл проверяется синтаксически (`clang -fsyntax-only` по `compile_commands.json`, если доступно).
3. Запрещено менять `xrEngine`, `xrPhysics`, `xrRender`, `xrSound`. Если без этого не собирается — остановка, изменение спецификации, отдельное согласование.
4. Запрещено добавлять FMOD, WinAPI, MSVC-only расширения, `std::mutex`/`std::lock_guard`.
5. Контент не расширяется: только `veh_zaz968`, `veh_zaz968_2`, `veh_niva_green`.
6. Любое отклонение от Cars-Ext фиксируется в реестре отклонений (раздел 6).
7. После изменений кода — `graphify update .`, если инструмент доступен в окружении; иначе зафиксировать это в отчёте.

## 1. Этап A. Базовая линия и страховка

**Цель:** зафиксировать текущее состояние, чтобы отделить новые дефекты от существующих.

### A1. Ветка и базовые метрики
- Создать ветку `feature/transport-cars-ext` от текущей.
- Зафиксировать текущий commit hash и версию `SPAWN_VERSION` (сейчас 135).
- Сохранить хеши трёх OGF и трёх модельных ltx из `carpack`.

**Файлы:** без изменений кода.

**Валидация:**
- `cmake` configure + build `Debug` и `Release` для `xrGame`.
- Сборка `IXRAY_MP` (если включена) — сохранить лог.
- Запуск текущей игры, спавн одной машины, сохранение/загрузка — сохранить лог и поведение как baseline.

**Выход:** известны текущие ошибки/варнинги; есть baseline-логи и хеши контента.

### A2. Тестовый сценарий
- Зафиксировать карту/точку спавна и способ спавна каждой из трёх машин (консоль/скрипт/уровень).
- Создать шаблон ручного чеклиста из раздела 4 спецификации.
- Зафиксировать команды сборки и путь к `compile_commands.json`.

**Валидация:** тестовый сценарий воспроизводится на текущей сборке.

**Выход:** любой разработчик может повторить baseline и ручной прогон.

## 2. Этап B. Ядро транспорта (замена Car*)

**Цель:** заменить текущую реализацию `CCar` на Cars-Ext, сохранив компилируемость и базовый спавн.

**Риск:** высокий; это самый крупный этап. Промежуточные коммиты могут не линковаться.

### B1. Новый заголовок класса
- `src/xrGame/Car.h` — заменить объявлением `CCar` из Cars-Ext.
- Сохранить `XREFLECT(LuaExport, Bases = "CGameObject,CHolderCustom")`, `XGENERATED_BODY()`, `final`.
- Добавить `CRocketLauncher` в базовые классы.
- Сохранить текущие MP-объявления: `SyncRead/SyncWrite`, `net_Relevant`, `IsMyCar`, `m_CarNetUpdates`, `Interpolate`, `net_Export/net_Import`.
- Сохранить `cast_car()`, `GetInventory()`, `TryTrunk()`, `StartCarBody()`, `usable_bones`.
- Оставить `CCar.eWpn*`-константы через `CarWeapon.h`.
- `src/xrGame/CarDoors.h` — удалить или оставить пустой shim; `CCarDoor` заменяется на `SDoor`.
- `src/xrGame/car_memory.h` — добавить из Cars-Ext.

**Проверка:** `clang -fsyntax-only` для `Car.h` через тестовый TU, если возможно; иначе визуальная проверка зависимостей.

### B2. Колёса и подвеска
- `src/xrGame/CarWheels.cpp` — заменить реализацией Cars-Ext.
- Адаптировать отсутствующие ODE-вызовы: `dBodySetGyroscopicMode` → игнор `gyroscopic_mode` + однократное предупреждение.
- Проверить `BuildWheelJoint`/`P_build_Shell` в текущем `xrPhysics`.

**Проверка:** `clang -fsyntax-only` для `CarWheels.cpp`.

### B3. Двери и багажник
- `src/xrGame/CarDoors.cpp` — заменить; `SDoor` вместо `CCarDoor`.
- Удалить/адаптировать `CarDoors.h`; обновить включения в `Car.h` и CMake.
- Сохранить текущий контракт багажника: `m_bone_trunk`, `TryTrunk`, `StartCarBody`.

**Проверка:** `clang -fsyntax-only`; поиск оставшихся `CCarDoor`.

### B4. Свет, выхлоп, damage-частицы
- `src/xrGame/CarLights.h/.cpp` — заменить.
- `src/xrGame/CarExhaust.cpp` — заменить.
- `src/xrGame/CarDamageParticles.h/.cpp` — заменить.
- Убрать `m_detcollslots`/grass collision; `flare*` читать, но lens flare не создавать.
- `CCar::UpdateCL`/`VisualUpdate` — точки вызова света/частиц.

**Проверка:** `clang -fsyntax-only` для трёх модулей.

### B5. Турели и оружие
- `src/xrGame/CarWeapon.h/.cpp` — заменить; добавить `CCarTurret`.
- Сохранить `CCar.eWpn*`-константы и текущие имена методов `Action`/`SetParam`/`HasWeapon`/`CanHit`/`FireDirDiff`.
- Проверить `CRocketLauncher`, `GE_LAUNCH_ROCKET`, `CShootingObject`, `CCartridge` в текущем `xrGame`.
- `[mounted_weapon_definition_N]` — поддержать; при отсутствии секции турель не создаётся.

**Проверка:** `clang -fsyntax-only` для `CarWeapon.cpp`.

### B6. Звук
- `src/xrGame/CarSound.cpp` — заменить `SCarSound`.
- Заменить глобальный `psSoundCarFactor` на локальный фактор `CCar`/`SCarSound`; применить к двигателю, дверям, сигналу.
- Убедиться, что звук не вызывается из `PhDataUpdate`.

**Проверка:** `clang -fsyntax-only`; поиск `psSoundCarFactor` (не должно остаться внешних ссылок).

### B7. Камеры
- `src/xrGame/CarCameras.cpp` — заменить; пять камер.
- `roll_lock` реализовать в `CCar`/`CCameraLook2` (xrGame), без правок `CCameraBase`.
- `apply_inert`, `rot_speed`, `lim_zoom` — читать из модельных секций и применять в `CCar`.
- `cam_Update` вызывать только из `UpdateEx`.

**Проверка:** `clang -fsyntax-only`; поиск `EctFiveCam`/`roll_lock`.

### B8. Ввод
- `src/xrGame/CarInput.cpp` — заменить обработчики Cars-Ext.
- Сохранить текущую отправку `GAME_EVENT_MP_CAR_INPUT` при `OnClient()` для клавиатуры и геймпада.
- Сохранить текущие биндинги `kCAM_1..3`, `kFWD/kBACK`, `kL_STRAFE/kR_STRAFE`, `kTORCH`, `kDETECTOR`, `kTRANSMISSION_UP/DOWN`, `kBRAKE`, `kENGINE`; добавить новые действия Cars-Ext.
- `OnKeyboardHold` — свободная камера.

**Проверка:** `clang -fsyntax-only`; сравнить блоки MP-отправки с текущими.

### B9. Основная реализация
- `src/xrGame/Car.cpp` — заменить реализацией Cars-Ext.
- Перенести/адаптировать:
  - `ParseDefinitions`, `Init`, `CreateSkeleton`, `SpawnInitPhysics`;
  - `net_Spawn`, `net_Destroy`, `net_Save`, `SaveNetState`, `RestoreNetState`, `SetDefaultNetState`;
  - `save/load/Serialize`;
  - `UpdateCL`, `UpdateEx`, `VisualUpdate`, `shedule_Update`, `PhDataUpdate`, `PhTune`;
  - `renderable_Render`, `Draw_3D_UI`;
  - `Hit`, `Die`, `OnEvent`, `net_Relcase`, `OnBeforeExplosion`, `OnAfterExplosion`;
  - `attach_Actor`, `detach_Actor`, `Use`, `Enter`, `Exit`, `DoExit`;
  - `usable_bones`, `TryTrunk`, `StartCarBody`;
  - `m_sits_transforms` — фиксированная трансформация водителя, без роста.
- Вернуть IXRAY-специфику:
  - `IsMyCar`, `m_CarNetUpdates`, `Interpolate`;
  - `SyncRead/SyncWrite` (текущий формат);
  - `net_Relevant` = `IsMyCar() || OnServer()`;
  - `net_Export/net_Import` — пустые;
  - `cast_*`-методы;
  - `GetInventory`, `AlwaysTheCrow`, `UsedAI_Locations`, `is_ai_obstacle`.
- Заглушки отсутствующих API:
  - `HUD().Add_LensFlare` → нет flare, только glow;
  - `m_detcollslots` → нет;
  - `dBodySetGyroscopicMode` → нет;
  - `psSoundCarFactor` → локально.

**Проверка:** `clang -fsyntax-only` для `Car.cpp`; поиск оставшихся LA-специфичных символов.

### B10. `car_memory`
- `src/xrGame/car_memory.cpp` — добавить.
- Использовать для `isObjectVisible` и турелей.
- CMake: убедиться, что файл включён в `xrGame` (глоб или явный список).

**Проверка:** `clang -fsyntax-only`; `isObjectVisible` работает.

### B11. Сборочный гейт B
- Полная сборка `xrGame` Debug.
- Исправить ошибки компиляции/линковки.
- Запуск: спавн одной машины (`veh_zaz968`), проверка отсутствия краша при создании shell.
- Если машина не спавнится — разрешено временно отключить необязательные подсистемы (турель, свет, звук) с `// TODO` и логом, но не отключать физику/двери/колёса.

**Выход:** `xrGame` собирается; машина создаётся; есть список нерешённых runtime-проблем для следующих этапов.

## 3. Этап C. Actor, holder и посадка

**Цель:** водитель снова может сесть в машину, камеры/HUD/анимации работают.

### C1. Actor/holder
- `src/xrGame/ActorVehicle.cpp` — адаптировать `attach_Vehicle`, `detach_Vehicle`, `use_Vehicle`.
- `src/xrGame/ActorInput.cpp` — маршрутизация ввода в `m_holder`.
- `src/xrGame/Actor.*` — `m_holder`, `m_holderID`, HUD, тени, `OnHUDDraw`.
- `src/xrGame/ActorAnimation.*` — анимация водителя (`driver_animation_type`).
- Убедиться, что `cast_car()`-проверки не сломаны.

### C2. Посадка/высадка
- `attach_Actor`: один водитель, `driver_place`/`driver_direction`/`driver_position`.
- `detach_Actor`: очистка владельца, камеры, HUD, видимости, звукового фактора.
- `DoExit`: ближайшая/открытая дверь, `ExitPosition` fallback.
- `net_Relcase`/`net_Destroy`: не оставлять dangling `OwnerActor`.

**Проверка:**
- Вход/выход, повторный вход 10 раз — `m_sits_transforms` не растёт.
- Камеры first/chase/free/free2/free-fly.
- Актёр скрывается в first-eye, возвращается в chase.
- HUD появляется/скрывается.
- Смерть актёра в машине и удаление машины — без краша.

**Гейт C:** сборка `xrGame` Debug; ручной smoke посадки.

## 4. Этап D. Нативный UI

**Цель:** HUD и багажник соответствуют новым данным, без LA-скриптов.

### D1. HUD
- `src/xrGame/ui/UICarPanel.h/.cpp`:
  - реализовать `SetSpeed`, `SetRPM`, `SetGear`, `SetFuel`, `SetHealth`;
  - исправить шкалу топлива (литры vs 0–100);
  - включить `SetAutoDelete` для fuel bar.
- `gamedata/configs/ui/car_panel.xml`, `car_panel_16.xml`:
  - добавить текстовые поля скорости/RPM/передачи;
  - использовать существующие шрифты, без новых текстур/атласов.
- `src/xrGame/UIMainIngameWnd.*` — обновление панели.

### D2. Багажник и инвентарь
- `src/xrGame/ui/UICarBodyWnd.*` — сохранить; адаптировать под новый `CCar`, `GetInventory`, `TryTrunk`.
- `src/xrGame/UIGameCustom.cpp` — `StartCarBody`, `cast_car()`.
- `src/xrGame/UIActorMenu.cpp` — иконка/имя машины; отсутствие иконки не должно крашить.
- `src/xrGame/Inventory.cpp` — проверить `CanTakeItem` для машины; `trunk_capacity` — если текущий API позволяет, ограничить; иначе зафиксировать отклонение.

**Проверка:**
- HUD отображает здоровье/топливо/скорость/RPM/передачу.
- Багажник открывается/закрывается, предметы перемещаются.
- Нет missing texture spam, кроме принятых иконок.

**Гейт D:** сборка + ручной HUD/trunk smoke.

## 5. Этап E. 3D-приборы

**Цель:** стрелки/индикаторы на костях работают.

### E1. Парсинг и отрисовка
- `src/xrGame/Car.h/.cpp`:
  - парсинг секций `[speedometer]`, `[tachometer]`, `[fuel]`, `[temp]`, `[gear]`, `[fan]`, `[clock_*]`;
  - `CCar::Draw_3D_UI`;
  - обновление углов/текста в `VisualUpdate`.
- `src/xrGame/Level.cpp`:
  - вызвать `Draw_3D_UI` для машин с приборами из `CLevel::Render`;
  - без per-frame allocations.

### E2. Проверка
- Для `veh_niva_green` (есть speedometer/tachometer/gear) и `veh_zaz968` (speedometer/gear/clock).
- Стрелки двигаются, часы идут, топливо/температура соответствуют состоянию.
- Отсутствие секции → прибор не рисуется, без краша.

**Гейт E:** сборка + визуальная проверка.

## 6. Этап F. Lua-контракт

**Цель:** текущие скрипты работают, LA-алиасы доступны.

### F1. API
- `src/xrGame/Car.h`:
  - `XPROPERTY(LuaCallable)` для `GetFuel`, `Refuel`, `ShowTrunk`, `OpenTrunkBone`, `CloseTrunkBone`, `TrunkBoneWorldPos`;
  - `engaged` — через `CHolderCustom`/`holder`; проверить `holder_custom_script.cpp`.
- Сохранить `wpn_action`-константы и текущие имена.
- `src/xrGame/holder_custom_script.cpp` — при необходимости добавить/сохранить `engaged`, `Action`, `SetParam`, `SetEnterLocked`, `SetExitLocked`.
- `src/xrGame/script_game_object*` — аудит `get_car`, `get_current_holder`, `attach_vehicle`, `detach_vehicle`; ничего не ломать.
- `usable_bones`/`usable_bones_callback` — сохранить; `on_use_trunk` — опциональный алиас.

### F2. Проверка
- Вызвать текущий API из Lua: `fuel`, `add_fuel`, `Action`, `SetParam`, `HasWeapon`, `CanHit`, `IsObjectVisible`, `GetfHealth`, `SetfHealth`, `SetExplodeTime`, `ExplodeTime`, `CarExplode`, `CurrentVel`.
- Вызвать алиасы: `GetFuel`, `Refuel`, `ShowTrunk`, `OpenTrunkBone`, `CloseTrunkBone`, `TrunkBoneWorldPos`, `engaged`.
- `ph_minigun`-подобный сценарий: `get_car()`, `Action(CCar.eWpn*)`, `SetParam`, `CanHit`.

**Гейт F:** сборка + Lua smoke.

## 7. Этап G. Сохранение MP-слоя

**Цель:** текущий FreeMP-слой компилируется и работает в прежнем объёме.

### G1. Клиентские методы
- `src/xrGame/Car.cpp`:
  - `SyncRead` — текущий формат: engine/light flags, owner, transmission, state count + positions/quaternions;
  - `SyncWrite` — тот же формат;
  - `IsMyCar`, `m_CarNetUpdates`, `Interpolate`;
  - `net_Relevant` = `IsMyCar() || OnServer()`;
  - `net_Export/net_Import` — пустые.
- `src/xrGame/CarInput.cpp`:
  - сохранить отправку `GAME_EVENT_MP_CAR_INPUT` для клавиатуры и геймпада;
  - не менять формат события.
- `src/xrServerEntities` — не менять.
- `src/xrGame/FreeMP/game_sv_freemp.cpp` — не менять.

### G2. Проверка
- Сборка с `IXRAY_MP=ON`.
- FreeMP smoke (если доступен сервер): спавн машины, посадка, движение, свет, передача; два клиента видят позицию.
- Зафиксировать известные ограничения: двери/топливо/оружие/здоровье не синхронизируются; listen-server ввод имеет текущий баг; `SyncWrite` ≤ 255 байт.

**Гейт G:** сборка `IXRAY_MP` + MP smoke или документированный отказ от smoke.

## 8. Этап H. Save/load и устойчивость

**Цель:** новые сейвы работают, старые отклоняются, краевые конфиги не крашат.

### H1. Формат сохранения
- `src/xrGame/Car.cpp`:
  - версия формата CCar (если нужно) или `SPAWN_VERSION`;
  - сохранять: позицию/угол, здоровье, топливо, состояние/здоровье дверей, здоровье колёс;
  - не сохранять: двигатель, RPM, передачу, свет, турели, боезапас, камеру, nitro, частицы;
  - при загрузке сбрасывать transient state.
- `src/xrServerEntities/xrServer_Objects_ALife.*` — только если layout `STATE_*` меняется; иначе не трогать.
- Старый сейв → понятная ошибка/отказ, без краша.

### H2. Краевые случаи
- `Car.cpp`:
  - missing key/bone → defaults + warning;
  - `fuel_tank <= 0`, `reference_radius <= 0`, `max_rpm <= min_rpm` → guards;
  - `RestoreNetState` — проверка количества door/wheel states;
  - `net_Relcase`/`net_Destroy` — detach водителя;
  - `m_sits_transforms` — очистка.
- `CarWheels.cpp`, `CarDoors.cpp` — null/empty joint guards.

### H3. Проверка
- Save/load в движении, после повреждений, с открытыми дверями, с пустым/полным баком.
- Старый сейв → отказ.
- Удалить `[car_sound]`, `[lights]`, `[mounted_weapon_definition]`, `[damage_particles]`, `[transmission_gear_ratio]` по одному → спавн без краша, warnings.
- Переименовать кость колеса/двери → подсистема отключается.

**Гейт H:** сборка + save/edge smoke.

## 9. Этап I. Аудит потоков и звука

**Цель:** нет вызовов звука/света/UI/Lua из физического шага.

### I1. Очередь событий
- `Car.h`/`Car.cpp`:
  - расширить `EAsyncCalls` для звуков дверей/КПП/турели/света;
  - `PhDataUpdate` только ставит события;
  - `UpdateCL`/`VisualUpdate` выполняет.
- `CarCameras.cpp`:
  - `cam_Update` только из `UpdateEx`.
- `CarSound.cpp`:
  - локальный sound factor;
  - никаких `VERIFY(!physics_world()->Processing())`-нарушений.

### I2. Проверка
- Драйв 10 минут с переключениями, дверями, светом, турелью.
- Лог без `VERIFY`/`R_ASSERT`/`BEER`.
- Нет звуковых артефактов при паузе/низком FPS.

**Гейт I:** сборка + лог-аудит.

## 10. Этап J. Финальная валидация

### J1. Сборки
- `Debug` x64.
- `Release` x64.
- `IXRAY_MP=ON` (если применимо).
- Кросс-платформенная проверка Clang/Linux — если есть CI; иначе grep на WinAPI/FMOD/MSVC-only.

### J2. Ручной чеклист
- Полный чеклист из спецификации, раздел 4.2, для всех трёх машин.
- 10-минутный прогон стабильности.
- Регрессия: актор, инвентарь, оружие, стационарные holder'ы, `CUICarBodyWnd`, `cast_car()`.

### J3. Документация
- Заполнить реестр отклонений (раздел 11).
- Обновить `docs/engine/transport-cars-ext-spec.md` при изменении решений.
- `graphify update .` при доступности.
- Подготовить итоговый отчёт: сборки, чеклист, логи, известные ограничения.

### J4. Ревью и merge
- Код-ревью по `AGENTS.md`/`docs/engine/style.md`.
- Убедиться, что нет изменений в `xrEngine/xrPhysics/xrRender/xrSound`.
- Merge в основную ветку после утверждения отчёта.

## 11. Реестр отклонений от Cars-Ext

Заполняется по мере реализации; на старте зафиксированы:

| Отклонение | Причина | Влияние |
| --- | --- | --- |
| Нет FMOD-радио | В проекте OpenAL; FMOD — новая зависимость | Радио отсутствует |
| Нет lens flare | Нет `HUD().Add_LensFlare`; xrGame-only | Только light/glow |
| `gyroscopic_mode` игнорируется | Нет `dBodySetGyroscopicMode` | Поведение колёс чуть проще |
| Глобальный `psSoundCarFactor` заменён локальным | Нет глобального фактора; xrGame-only | Интерьерная громкость приблизительная |
| Нет grass collision | `Device.m_detcollslots` вне xrGame | Трава не деформируется |
| Нет 3D text/video | Не используется в трёх конфигах | Только 3D-приборы |
| Нет AI-вождения | `ph_car` — LA-скрипты | Машины без AI-патрулей |
| Нет покупки/ремонта UI | LA-скрипты/диалоги | Нет игрового ремонта/покупки |
| Нет пассажирских мест | В Cars-Ext их тоже нет | Только водитель |
| Нет иконок/локализации | Решение: только carpack | Возможны `Vehicles`/пустые иконки |
| Старые сейвы несовместимы | Новый формат | Нужен новый старт |
| `CarConfig`/`CarEditor` нерабочие | Отложено | Редактор не использовать |

## 12. Сборочные гейты (сводка)

| Гейт | Что собирается | Что проверяется |
| --- | --- | --- |
| A | Debug/Release + MP baseline | Текущее состояние, логи, хеши |
| B | `xrGame` Debug | Спавн одной машины, shell, отсутствие краша |
| C | `xrGame` Debug | Вход/выход, камеры, HUD, анимация |
| D | `xrGame` Debug | HUD, багажник, инвентарь |
| E | `xrGame` Debug | 3D-приборы |
| F | `xrGame` Debug | Lua API и алиасы |
| G | `xrGame` + `IXRAY_MP` | Текущий Sync и ввод |
| H | `xrGame` Debug/Release | Save/load, краевые конфиги |
| I | `xrGame` Debug | Потоки, звук, логи |
| J | Debug/Release/MP | Полный чеклист, регрессия, отчёт |

## 13. Что не делаем в этом плане

- Не расширяем MP-синхронизацию (двери/топливо/оружие/здоровье).
- Не переносим LA-скрипты и UI.
- Не добавляем пассажиров, AI-вождение, покупку/ремонт.
- Не меняем движковые модули.
- Не добавляем контент LA и иконки.
- Не чиним ImUtils-редактор.
- Не делаем unit/integration/benchmarks обязательными.
