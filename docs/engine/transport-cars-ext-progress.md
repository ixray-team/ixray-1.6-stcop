# Прогресс переноса транспорта Cars-Ext

Дата: 2026-11-01  
Ветка: `feature/transport-cars-ext`  
Спецификация: `docs/engine/transport-cars-ext-spec.md`  
План: `docs/engine/transport-cars-ext-plan.md`

## Этап A — завершён

- Ветка создана от `develop` (`c005ee2e3`).
- Baseline `RelWithDebInfo`, `Debug`, `Release` собран.
- Зафиксированы хеши трёх OGF/ltx и `SPAWN_VERSION=135`.
- Подготовлен тестовый стенд `build/bin/<Config>` с junction'ами на `H:\IXRAY`.
- Baseline-документ: `docs/engine/transport-cars-ext-baseline.md`.

## Этап B — компиляционный checkpoint

`xrGame` Debug собирается. Перенесены:

- `Car.h`, `Car.cpp`, `CarWheels.cpp`, `CarDoors.cpp`, `CarLights.*`, `CarWeapon.*`, `CarSound.cpp`, `CarInput.cpp`, `CarExhaust.cpp`, `CarDamageParticles.*`, `CarCameras.cpp`;
- `car_memory.*`;
- интеграционные правки `holder_custom.h`, `cameralook.h`, `RocketLauncher.h`, `UICarPanel.h`, `CMakeLists.txt`.

Сохранены:

- `XREFLECT`-контракт `CCar`, включая `fuel`, `fuel_tank`, `add_fuel`, `Action`, `SetParam`, `HasWeapon`, `CanHit`, `FireDirDiff`, `IsObjectVisible`, `CurrentVel`, `GetfHealth`, `SetfHealth`, `SetExplodeTime`, `ExplodeTime`, `CarExplode`;
- новые Lua-алиасы: `GetFuel`, `Refuel`, `ShowTrunk`, `OpenTrunkBone`, `CloseTrunkBone`, `TrunkBoneWorldPos`;
- `CCar.eWpn*`-константы;
- MP-методы `IsMyCar`, `SyncRead`, `SyncWrite`, `m_CarNetUpdates`, `Interpolate`;
- `GAME_EVENT_MP_CAR_INPUT` в `CarInput.cpp`;
- `TryTrunk`/`TryUsableBones` и нативный багажник.

## Отклонения и совместимые правки

| Отклонение/правка | Причина |
| --- | --- |
| `CPhysicsJoint::GetDJoint()` добавлен как non-pure accessor | Cars-Ext активно использует ODE joint ID; IXRAY скрывал метод в `CPHJoint` |
| `CPhysicsElement::get_body()` добавлен как non-pure accessor | То же для ODE body ID |
| `CRocketLauncher::getRocketCount/getCurrentRocket/dropCurrentRocket` сделаны public | `CCarWeapon` не является наследником `CRocketLauncher` |
| `CHolderCustom::Owner/OwnerActor` сделаны public | `car_memory` использует их |
| `CCar` наследует `CRocketLauncher` | Требуется для ракет Cars-Ext |
| FMOD-радио удалено | Решение: без FMOD |
| Lens flare отключён | Нет `HUD().Add_LensFlare`; остаются light/glow |
| 3D text/video UI удалён | В трёх конфигах не используется; 3D-приборы остаются |
| Экранная отрисовка стрелок приборов удалена | Используются повороты gauge-костей; UI API IXRAY отличается |
| `CPHActivationShape` в `RestoreNetState` не используется | Класс не экспортируется из `xrPhysics.dll`. Попытка использовать экспортируемый `ActivateShapePhysShellHolder` привела к вылету в `CODEGeom::geometry_bt()` (`m_geom_transform == nullptr`) при загрузке сейва с машиной, поэтому восстановление идёт без penetration-разрешения |
| `default_l_scale`/`default_w_scale` заменены на `1.01f` | Символы не экспортируются из `xrPhysics.dll` |
| `ph_world` заменён на `physics_world()` | Символ не экспортируется |
| `TDamageManager` вместо `CDamageManager` | ECS-компонент IXRAY |
| `TParticlesPlayer`-компонент вместо `StartParticles`/`StopParticles` | ECS-компонент IXRAY |
| `CPHContact` вместо `dContact` в callbacks | IXRAY physics API |
| `CarWeapon` адаптирован под `CShootingObject` IXRAY | Поля/методы LA (`fTime`, `Light_Create`, `StartShotParticles`, rocket-by-section) отсутствуют |
| `m_transmission_stage` (`0/1/2/10/20`) заменён на `ETransmissionStage` (`Drive`/`SwitchUp`/`SwitchDown`) | Убраны магические числа. Промежуточные состояния 1/2 стали параметром `SwitchRequest` у `Transmission()`; проверки `< 10`, `>= 10`, `> 2` заменены на `IsTransmissionSwitching()`, а `rs_car_info` показывает имя состояния |

## Текущий runtime-статус

- `xrEngine.exe` и `xrGame` Debug/Release собираются без ошибок; предупреждения в `Car.cpp` устранены.
- Autotest `start server(jupiter/single/alife/new) client(localhost)` без спавна машины: PASS.
- Autotest со спавном каждой из трёх машин (`veh_zaz968`, `veh_zaz968_2`, `veh_niva_green`), 300 кадров после прогрева: `errors=0`, PASS.
- Для загрузки уровня потребовался `gamedata/spawns/jupiter.spawn`; он создан как копия `all.spawn` в тестовом game data (вне репозитория, `gamedata/spawns` исключён из git).
- Спавн машины в точке актёра приводил к гибели актёра и авто-загрузке; тестовый спавн перенесён на 8 м вперёд. Это особенность теста, не дефект машины.

### Исправления, найденные runtime-прогоном

| Проблема | Исправление |
| --- | --- |
| Падение `GetGlobalTransformDynamic` после спавна | Двери при инициализации деактивируют ODE-элементы (body уничтожается). ODE-реализация `CPHElement::GetGlobalTransformDynamic` теперь пропускает элемент без body, как Box3D; `CPHShell::GetGlobalTransformDynamic` выбирает корнем элемент с body |
| `load/save mismatch` для `veh_zaz968` | `CSE_ALifeCar` получил `fuel`; `data_load`/`data_save` читают и пишут его. `CCar::net_Spawn` берёт `m_fuel` из серверной сущности |
| `Object exceeds limit of 24 renderable parts/materials` | Лимит отладочной проверки в R4 light shadows поднят до 128 под детализированные модели машин |
| Отсутствующие партиклы нитро/выхлопа | Пустые имена и отсутствующие эффекты теперь безопасно пропускаются |
| Ошибки `C4804`/`C4715` | Исправлено сравнение `CurrentTransmission()` и добавлены возвраты в `WheelsGetSpring`/`WheelsGetDamping` |

### Проверка Lua-контракта

В рантайме через `cast_Car()` проверено: `GetFuel`, `fuel_tank`, `Refuel`, `HasWeapon`, `CanHit`, `FireDirDiff`, `TrunkBoneWorldPos`, `OpenTrunkBone`, `CloseTrunkBone` — ошибок Lua нет, значения корректны (топливо 40/40, координаты багажника возвращаются).

### Гейт C: исправление посадки в машину

Ручная проверка выявила: двери открываются/закрываются, но актёр не садится. Причины и исправления:

| Проблема | Исправление |
| --- | --- |
| `closed_door_form_in_object` хранил мировой трансформ элемента, а затем умножался на `XFORM()` (двойное преобразование) | Форма закрытой двери сохраняется в локальных координатах машины (`inverse(XFORM) * door_transform`) |
| `IsInArea` Cars-Ext требует нахождения камеры в узком секторе между плоскостями двери; камера IXRAY туда не попадает | `CanEnter` проверяет `TestPass` (как рабочий baseline IXRAY); отклонение задокументировано в коде |
| `TryTrunk` срабатывал по любому попаданию луча, в том числе сквозь открытую боковую дверь | Учитывается только ближайшее попадание (`OPT_ONLYNEAREST`), дистанция 2 м |
| `IsDistOfSight` использовал глобальную камеру вместо переданной позиции взаимодействия | Метод принимает `pos`; вызовы передают позицию из `Use`/`UpdateRecursiveDoors` |

Дополнительно, из автоматического smoke входа/выхода:

| Проблема | Исправление |
| --- | --- |
| `CObjectContactCallback::Add`: `callback != c` при повторном входе в машину | Добавление object-contact callback сделано идемпотентным |
| `_vector3::normalize` на нулевых векторах после detach/в покое | Добавлен `safe_normalize`; им заменены нормализации скорости, подвески, дверей и камерных направлений в `Car.cpp`/`CarDoors.cpp` |

Автоматический smoke: `Use` → `attach_vehicle` → `detach_vehicle`, 300 кадров, `errors=0`, PASS.

### Гейт C: камера и управление

| Проблема | Исправление |
| --- | --- |
| После входа камера остаётся на месте: `CCar::UpdateEx` нигде не вызывался | В `CActor::UpdateCL` добавлен вызов `m_holder->UpdateEx(currentFOV())`, как в LA; камера машины обновляется через `CCar::cam_Update` |
| Сигнал на `show_detector`, двигатель не запускается | Сигнал перенесён на `kDROP` (press/release/hold); запуск двигателя добавлен на `kDETECTOR` (`show_detector`); привязка `kENGINE` сохранена |
| Изнутри нельзя открыть дверь и выйти | `GetNearestBone` больше не требует начального попадания луча в одностороннюю коллизию машины: изнутри кости сканируются напрямую. Из первого лица наведение на дверь открывает/закрывает её, наведение на свободное пространство проёма — выход (как в LA) |
| Вылет при выходе от 3-го лица, если дверь уже открыта | `CActor::use_Vehicle` повторно проверяет `m_holder` перед `Use` (как в LA): `Use` может синхронно выполнить `detach_Vehicle` и обнулить `m_holder` |
| Снаружи при приоткрытой двери игрок садился вместо закрытия | Наведение на дверь (`GetNearestDoor`) теперь обрабатывается до `Enter`: дверь открывается/закрывается, посадка — только через свободное пространство проёма |
| Сигнал не играет на `drop` (`kG`) | `get_binded_action(kG)` возвращал посторонний id 154; для машин ввод резолвит `kDROP`/`kDETECTOR` через `is_binded`, включая press/release/hold |
| При входе в 3-м лице модель актёра не появлялась до смены камеры | Видимость актёра теперь выставляется по целевой камере (`type != EctFirstCam`), а не по предыдущей |
| Багажник открывался сразу с инвентарём либо закрывался повторным нажатием | Первое использование открывает багажник без UI; повторное на открытом — инвентарь; закрытие инвентаря вызывает `CloseTrunkBone()` и багажник закрывается |
| Предметы не переносились в багажник: зависали в воздухе | Cars-Ext `CCar::OnEvent` не обрабатывал `GE_TRADE_BUY`/`GE_TRADE_SELL`. Добавлена обработка (как в baseline IXRAY), перенос в машину и обратно проверен по `parent_id` |
| Камера при выходе кратко показывала старое место | В `CActor::detach_Vehicle` актёр сразу ставится в `ExitPosition()`, затем обновляются движение и `cam_Set(eacFirstEye)` — без кадра со старой позицией |
| `use_distance` из ltx машины не влиял на открытие двери/багажника | Дистанция двери проверяется через `IsDistOfSight(pos)` до переключения; `TryTrunk` использует `dist_to_door_avlbl` секции `trunk_bone` из ltx модели |
| `trunk_distance`/`has_trunk` читались из pSettings (`vehicles.ltx`) | Чтение `trunk_distance` удалено вместе с полем; `has_trunk` в коде отсутствует. Параметры берутся из секции кости в ltx машины |
| Переключение передач срабатывало дважды и путалось с другими действиями | `kACCEL`/`kCROUCH` снова переключают передачи (accel — вверх, crouch — вниз) и следуют за настройками управления. `kTRANSMISSION_UP`/`kTRANSMISSION_DOWN` работают только как отдельный алиас на клавишах, не занятых другими действиями: `sprint_toggle` остаётся нитро и передачи не трогает |
| Нет `rs_car_info` из LA | Добавлена консольная команда `rs_car_info` и отладочная панель телеметрии машины (скорость, передачи, RPM, топливо, downforce и т.д.) на `pFontStat` |
| Нет следов колёс (wallmark) из LA | В stock `gamemtl.xr` у пар `objects\car_wheel` с поверхностями пустые `CollideMarks`. Добавлены отдельные массивы `wm_tire_trail_1`, `wm_tire_trail_dirt_1`, `wm_tire_trail_grass_1` (переопределяются через `tire_trail_wallmark_name*` в `vehicles.ltx`) и выбор текстуры по материалу поверхности; если у material pair есть свои метки, используются они. В `vehicles.ltx` прописаны `tire_trail_wallmark_name*`, `tire_trail_similars`, `tire_trail_notsimilars`, `tire_trails` и `tire_trail_wallmark_size` |
| В `rs_car_info` не было данных по колёсам | Добавлена LA-панель по каждому активному колесу: состояние, линейная/угловая скорость, torque/diff, скорости и углы руления, тормозной момент, friction/slip/spring/damping, traction и ABS. Колонки по два колеса, как в LA |
| Звуки колёс брались из `gamemtl.xr` (человеческие шаги) | Добавлена загрузка `tire_sound_*` и `tire_skid_sound_*` из `vehicles.ltx` с LA-дефолтами (`car\wheel_road/dirt/grass/water`, `car\handbrake`, `car\dirt_skid`). Для каждой поверхности можно задать свой loop-звук и свой skid-звук. Fallback на `StepSounds`/`CollideSounds` material pair полностью убран, чтобы человеческие шаги не попадали в звук машины; water-skid по умолчанию `car\dirt_skid` |
| В глубокой воде слышны шаги персонажа | Основная причина: при пересечении колесом/кузовом поверхности воды physics contact (`physics_game.cpp`) играл `CollideSounds` пары water/car, а это human-шаги. Теперь для объектов `veh_*` liquid-contact звук пропускается. Дополнительно убран fallback колёсных звуков на material pair. Ранние попытки через actor step manager оказались не причиной и убраны |
| Нет консольных команд ремонта/заправки машины | Добавлены `rs_car_repair` и `rs_car_refuel`; работают только когда актёр сидит в машине |
| Урон от `collision_damage` не проходил | В `CPHCollisionDamageReceiver::CollisionHit` был жёстко зашит порог 5.0, а `collision_hit_threshold` из модели не читался. Добавлен настраиваемый порог; `CCar::Init` читает его из `car_definition` (по умолчанию 1.0 для моделей без ключа). Это отсекает слабые контакты, но не является причиной полного отсутствия урона |
| Урон не проходил при ударах о землю, стены и другую статику | `CollisionHit` отправлял `GE_HIT` с `whoID = source_id`; для статической геометрии источник — `ALife::INVALID_OBJECT_ID`, и single-player сервер отбрасывал такое событие до `CCar::Hit`. Теперь при невалидном источнике в `whoID` подставляется ID самой машины (как в LA), а исходный источник остаётся в `weaponID`. Удары о другие объекты по-прежнему сохраняют корректного инициатора |
| После загрузки сохранения, сделанного с взглядом на юг, езда вперёд/назад инвертировалась | В `CActor::on_requested_spawn` после `attach_Vehicle` восстанавливается `r_torso.yaw` из `car->XFORM().getXYZi(xyz)`, как в LA. LA-позиционирование через `ActivateShapePhysShellHolder` не переносится: оно вызывает вылет при загрузке сейва с машиной |
| После загрузки сейва машина ехала назад при переднем ходе | `SWheelDrive::Init` вычислял `pos_fvd` из `element->mXFORM.k.x`; этот transform уже повёрнут вместе с машиной, поэтому при yaw π знак менялся. Теперь используется bind-space ориентация `bones_bind_forms[bone_id].k.x`, не зависящая от мирового поворота |
| Детектор оставался в руках при посадке в машину | В `CActor::attach_Vehicle` после `SetWeaponHideState(INV_STATE_BLOCK_ALL, true)` текущий `CCustomDevice` при необходимости переводится в `HideDetector(true, true)`. Это повторяет явное скрытие детектора из LA `CInventory::SetSlotsBlocked` |
| Фонарь не выключался при посадке и не следовал за машиной | При посадке `CTorch::Switch(false)`. При `kTORCH` и `[lights] torchlight = true` фонарь переключается вместе с фарами через `CActor::UseTorch`. В `CTorch::UpdateCL` направление берётся из камеры машины в первом лице или из матрицы кузова в остальных камерах |
| Первый выход после загрузки сейва с машиной: урон и отскок | В `CActor::detach_Vehicle` после `detach_Actor` персонаж создаётся/позиционируется в точке выхода и повторно активируется `ActivateBoxDynamic(0)`, как в LA. Без второго шага character оставался внутри оболочки машины |
| Багажник не всегда открывался сбоку | `TryTrunk` использует `GetNearestBone(..., trunk_test=true)`: кость багажника находится сканированием костей и trunk-лучом, поэтому работает сбоку, но не срабатывает при наведении на другую дверь или кузов. Дистанция ограничена `use_distance` из ltx |
| Зум камеры машины не работал через `cam_zoom_in`/`cam_zoom_out` и колесо | Зум идёт через `CCameraBase::Move(kCAM_ZOOM_IN/OUT)`: у `CCameraLook` меняется `dist`, поэтому камера приближается/отдаляется, FOV не меняется. `kCAM_ZOOM_IN/OUT` обрабатываются в `OnKeyboardHold`, колесо — через `CActor::IR_OnMouseWheel` → `CCar::OnMouseWheel` (10 шагов, как у актёрской камеры) |
| Нет команды `rs_car_steer_factor` | Добавлена консольная команда `rs_car_steer_factor` (0.5–3.0), управляющая глобальным `rs_car_steer_factor` из `cb_Steer_factor` |
| Нет отдельного бессмертия машины | Добавлена команда `rs_car_god` и глобальный `rs_car_god`. При включённом флаге и актёре внутри машины `CCar::Hit`, `ChangeCondition`, `PHHit` и `CarExplode` игнорируют урон/взрыв. `g_god` с этой командой не связан |
| Отсутствующая/пустая модель из `[destroyed]` вызывает fatal error | `CPHDestroyable::GenSpawnReplace` проверяет наличие OGF через `FS.exist` и наличие physics shapes у скелета. При отсутствии пишет `! [destroyed] ...` в лог и пропускает spawn, не прерывая игру |
| Вылет при таране одной машины другой в `CKinematics::CalculateWallmarks` | Следы колёс создавались как skeleton wallmarks на динамическом объекте (другой машине) и могли пережить его visual. Теперь следы колёс ставятся только на статическую геометрию; в R4 `CalculateWallmarks` добавлена защита от null и удаление просроченных меток |
| `anim kils bone matrix` при спавне `veh_brdm_nogun` | У BRDM bind-матрица `left_caliper`/`right_caliper` и трансформ передней оси почти компенсируются, и произведение матриц уходит в субнормальные значения (`7.3e-40`, `-1.8e-43`). `_valid` считает денормали невалидными, поэтому срабатывал assert. В R4 `CLBone` субнормальные элементы матрицы теперь сбрасываются в 0 перед проверкой (только в конфигурациях, где проверка активна); NaN и Inf по-прежнему обнаруживаются |
| Вылет `can not deactivate physics character shell during physics processing` при выходе из машины/загрузке сейва | `CCar::PhDataUpdate` вызывал `CActor::detach_Vehicle()` внутри шага физики. Если `ActivateBoxDynamic` не удавался, `DestroyCharacter` пытался удалить физическую оболочку персонажа во время `CPHWorld::Step` и срабатывал assert. Выход перенесён в `CCar::UpdateCL`; задержка 400 мс после открытия двери реализована через `m_exit_car_time`, без отдельного потока с сырым указателем на машину |
| Эффекты повреждения машины не убирались при ремонте | `CDamagableItem::HitEffect` применял только уровни урона, но не понижал `m_level_applied` при росте здоровья. Добавлен `CCar::UpdateDamageEffects`, который при ремонте вызывает `ApplyDamage` для нового уровня; `ApplyDamage` теперь останавливает `car_damage_particles2` при уровне ниже 2 и `car_damage_particles1` при уровне ниже 1. `CCarDamageParticles` хранит состояние Play/Stop и не запускает/не останавливает эффект повторно |
| Громкость двигателя проседала при нажатии на газ | `snd_engine` в `UpdateDrive` останавливался через `stop_deffered` на холостом ходу. В новом звуковом микшере `stop_deffered` растягивал затухание на весь остаток файла, поэтому при возврате газа громкость сначала падала и только после остановки слота восстанавливалась. Основной loop двигателя теперь остаётся играть на нулевой громкости, а отложенная остановка в микшере делает короткий фейд (~100 мс), как в LA |
| Детектор снова появлялся в машине | Принудительное скрытие при посадке теперь сбрасывает `m_bNeedActivation`, а `CCustomDevice::ShowDetector` и `switch_device` не работают, пока `CActor::Holder()` не пуст. Это закрывает восстановление детектора через `OnMovementChanged`, горячую клавишу, UI и Lua. `need_restore_detector` по-прежнему выставляется при посадке |
| Звуки тяжёлого дыхания/кровотечения не прекращались в машине после лечения | В `CActor::shedule_Update` при `m_holder` был ранний `return` до блока обновления звуков, поэтому в машине циклы не обновлялись вообще. Логика вынесена в `CActor::UpdateWoundedSounds()` и вызывается как в ветке `m_holder`, так и в обычном пути. Теперь в машине дыхание играет при низком здоровье/хромоте и останавливается после лечения |
| `steer_boost` всегда показывал 0 в `rs_car_info` при езде | `CCar::OnKeyboardHold` сбрасывал `m_steer_boost` в начале каждого вызова, а `KeyboardUpdate` присылает hold для всех зажатых клавиш. Клавиша газа/другая клавиша с большим DIK обрабатывалась после A/D и обнуляла значение. Сброс перенесён на кадр через `m_steer_boost_frame`; сам импульс применяется через локальное значение steering-вызова, поэтому удержание газа не применяет boost повторно. При одновременно зажатых `kL_STRAFE` и `kR_STRAFE` boost не применяется. `UpdateCL` очищает устаревшее значение, если ввода руления в кадре не было |
| `steer_spin_boost` показывался на скорости, где не применялся | В `CCar::DrawCarInfo` выводятся только фактически активные значения: `steer_boost` при `GetSpeedKmH() > 10` и без `b_burnout`, `spin_boost` при `GetSpeedKmH() <= 10` и `b_burnout`. Неактивное значение отображается как `0`, сырое значение берётся только из текущего кадра |
| `torque_by_engine` и `breaking_torque` всегда показывали одинаковые значения | Оба читали один параметр ODE `dParamFMax2`: у hinge2 один мотор оси привода, поэтому момент двигателя и тормозной момент не могут действовать одновременно. Теперь `SWheelDrive::UpdatePower` пишет запрошенный момент двигателя в `SWheel::EngineTorque`, а `SWheelBreak::Break`/`HandBreak` — тормозной в `SWheel::BrakeTorque`; оба поля сбрасываются в начале `PhDataUpdate`. Фактически применённый лимит по-прежнему виден в новой строке `applied_torque_limit` (это и есть `dParamFMax2`) |
| Колесо без контакта с землёй раскручивалось двигателем и не тормозилось | В `CCar::PhDataUpdate` для колеса с `collision_params.traction == false` (нет контакта дольше 50 мс) и без торможения вызывается `SWheel::Neutral()`: мотор привода получает цель `0` и силу `m_axle_friction`, поэтому `axle_friction` работает и в воздухе. Флаг `SWheel::AirborneFriction` запоминает это состояние; при восстановлении контакта `SWheelDrive::Drive()` возвращает мотору цель скорости привода, иначе нулевая цель тормозила бы колесо после приземления. При активных тормозах `Neutral()` не вызывается, так как тормозной момент сильнее |
| Нет LA-логики удара по двигателю машины в аномалии | Восстановлен `CCar::HitEngine` и `m_hit_engine_time`, проверка в `UpdatePower`. `CMosquitoBald` вызывает `HitEngine` из `Affect`, если у машины `hit_stall_threshold != 0`. `HitEngine` не меняет мощность/обороты первые 1000 мс после запуска двигателя; физический импульс при этом применяется. Урон кузову считается как `power * (mass / 1000) * hit_anomaly_scale`, после чего `CCar::Hit` применяет `strike_immunity` так же, как для collision damage. `hit_anomaly_scale` читается из `vehicles.ltx` и по умолчанию равен 1. Машины исключены из `ShouldIgnoreObject` для `disable_time`. В `CCar::Hit` добавлена проверка `hit_stall_threshold`: сильный неаномальный удар вызывает `Stall()`; старый `CCar::PHHit(float...)` в IXRAY больше не вызывается базовым классом |

Примечание: при тесте перемещения машины обнаружено отсутствие модели разрушения `physics\vehicles\veh_empty.ogf`; это относится к будущему блоку destruction, не к текущему переносу.

Autotest LA-сценария: посадка → `Use` в дверь (открыть) → `Use` в дверь (закрыть) → `Use` в дверь (открыть) → `Use` в свободный проём (выход), `errors=0`, PASS.

Autotest attach/detach с обновлением камеры: 300 кадров, `errors=0`, PASS.

Autotest следов колёс: текстуры `wm_tire_trail_1/_dirt_1/_grass_1` загружаются, вызов `add_StaticWallmark` выполняется через отдельные массивы (в stock `gamemtl.xr` метки для `objects\car_wheel` пустые), `errors=0`, PASS.

Autotest `rs_car_info 1` с посадкой в машину: панель по каждому колесу отрисовывается 190 кадров, loop-звуки `wheel_road/dirt/grass/water` и skid-звуки `handbrake` для road, `dirt_skid` для dirt/grass/water создаются, `errors=0`, PASS.

Autotest `steer_boost`: при вызове hold для A/D и W в одном кадре значение остаётся ненулевым (`-0.099` для A), при следующем кадре только с W сбрасывается в `0`. До исправления W после A обнулял boost. `errors=0`, PASS.

Autotest удара о статику: `veh_zaz968` сброшен с 18 м в стороне от актёра — health 1.0 → 0.246, PASS. Для машины с актёром внутри урон также доходит до `CCar::Hit`; ограничение — отсутствующая модель `veh_empty` при разрушении.

Autotest фонаря: включённый `device_torch` выключается при посадке в машину (`torch_enabled()` было `true`, стало `false`), `errors=0`, PASS.

Autotest детектора: `detector_simple` в руках (`state=eShowing`) после посадки в машину уходит в `state=eHidden`, `errors=0`, PASS.

Autotest детектора после усиления защиты: `state=1 (eShowing)` → посадка → `state=2 (eHiding)`; вызов `db.actor:switch_detector()` внутри машины состояние не меняет; после выхода `state=3 (eHidden)`, `errors=0`, PASS.

Autotest звуков ранения в машине: health `0.05` → тяжёлое дыхание играет; `set_health_ex(1.0)` при `IsLimping() = false` → дыхание останавливается, `holder=true`; повторное понижение здоровья → снова играет. `errors=0`, PASS.

`CMosquitoBald` проверен сборкой Debug/Release/RelWithDebInfo; отдельный runtime-тест с зоной не выполнялся. `hit_anomaly_scale` читается из секции машины в `vehicles.ltx`, значение по умолчанию — 1.

Autotest `hit_stall_threshold`: `veh_niva_green` с запущенным двигателем (`engine=true`), удар `power=10`, `type=strike` → `engine=false`, health 1.0 → 0.84; слабый удар `power=0.5` двигатель не заглушил. `errors=0`, PASS.

Autotest задержки `HitEngine`: сразу после запуска `m_hit_engine_time = 0`; первый допустимый урон двигателю — через ~1039 мс, двигатель остаётся запущенным. `errors=0`, PASS.

Autotest сохранения и загрузки сейва с машиной: `errors=0`, PASS.

Autotest трансмиссии: машина сохранена повёрнутой на юг (`yaw≈π`). Перед сохранением передний ход давал `dot(CurrentVel, XFORM.k)` +2.34/+4.78, после загрузки +2.42/+4.62 — направление сохранилось.

Autotest выхода после загрузки сейва с машиной: `actor health=1`, `car health=1`, без отскока и урона, PASS.

Autotest багажника: наведение на багажник сзади, слева и справа даёт `TryTrunk=true`; наведение на переднюю часть кузова и на боковую дверь — `false`.

Зум камеры теперь не меняет FOV: `kCAM_ZOOM_IN/OUT` и колесо вызывают `CCameraLook::Move`, который двигает камеру через `dist` в пределах `lim_zoom`.

Autotest `rs_car_god`: при включённой команде урон `-0.4` не изменил health (0.6), после выключения health уменьшился до 0.2; команда читается и сбрасывается корректно.

Autotest destruction: `CarExplode` для `veh_niva_green` с `veh_empty` в `[destroyed]` пишет `! [destroyed] visual has no physics shapes: dynamics\vehicles\veh_empty` и продолжает работу без `[error]`, PASS.

Autotest спавна BRDM: загрузка сейва с `veh_niva_green` и `g_spawn veh_brdm_nogun` (один и несколько раз подряд) не даёт `anim kils bone matrix` и `[error]`. До фикса на `left_caliper`/`right_caliper` воспроизводился тот же assert с субнормальными элементами матрицы.

Autotest выхода из машины: сейв с актёром в `veh_niva_green`, `kCAM_2` + `kUSE`, дверь открывается, через 400 мс `detach_Vehicle` выполняется в `UpdateCL`, `holder` после выхода `nil`, `errors=0`, PASS.

Autotest эффектов повреждения: `SetfHealth(0.25)` запускает `Play1`/`Play2`, затем `SetfHealth(0.60)` останавливает только `Play2`, `SetfHealth(1.0)` — только `Play1`; повторных запусков/остановок нет, `errors=0`, PASS.

Звук машины: отложенная остановка в микшере теперь делает фейд ~100 мс, а loop `snd_engine` не останавливается на холостом ходу и остаётся на нулевой громкости. Debug/Release/RelWithDebInfo пересобраны, autotest без `[error]`.

`axle_friction` в воздухе: Debug собран, общий autotest `RESULT: PASS`, `errors=0`. Отдельный сценарий с подвешенным колесом требует ручной проверки: в автоматическом прогоне машина не создаётся и двигатель не запускается.

### Этап 1: безопасность вычислений и жизненный цикл

- `EngineCurTorque`, `DriveWheelsMeanAngleRate`, `RefWheelMaxSpeed` и разностный множитель защищены от деления на ноль и пустых списков колёс/передач.
- `Gauge_struct` и `Suspension_struct` теперь принадлежат `CCar` через `xr_unique_ptr`; `net_Destroy` сбрасывает callbacks, очищает параметры, турели и все контейнеры, включая `m_doors_recurs`.
- `thread_spawn` для drag-callback больше не передаёт сырой `this`: используется ID машины и повторный поиск объекта после задержки.
- Удалены мёртвые `b_transmission_switching` и `model_cache`; `fixed_step * 100.f` вынесен в `FixedStepCompensation`; `m_last_cotact_time` переименован в `m_last_contact_time`.

Autotest stage 1: общий прогон и спавн `veh_zaz968` через `-autotest_post_cmd` — `RESULT: PASS`, `errors=0`.

### Этап 2: структурный рефакторинг

- `cb_Gauge` разбит на диспетчер и отдельный `Update*Gauge`-метод для каждого прибора.
- `SpawnInitPhysics` разбит на `CreateCharacterScraper`, `CreateCarTurrets`, `ApplyBonePhysicsParams` и `BindGaugeAndSuspensionCallbacks`; три scraper-сферы создаются циклом по z-смещениям.
- `ParseDefinitions` разбит на загрузчики driver/camera, handling/camera, engine, sound, transmission и visual definitions.
- `PhDataUpdate` разбит на упорядоченные этапы: objects, engine/brakes, doors, transmission, slip, wheelie, drift, engine shake, steering, wheel physics, traction/suspension. Порядок вызовов сохранён.

Проверки stage 2: Debug/Release/RelWithDebInfo собраны; общий autotest и спавн `veh_zaz968` — `RESULT: PASS`, `errors=0`.

### Этап 3: гигиена кода

- Из `Car.cpp` удалены блоки `// UNUSED`, закомментированный код и комментарии в битой кодировке; из `CarWheels.cpp` удалены закомментированные `Msg`-строки.
- В `Car.cpp` убраны дублирующий include `xr_input.h` и mid-file include `EnnumerateVertices.h`; include перенесён в начальный блок.
- Ручные iterator-циклы по `m_wheels_map`, `m_doors`, `m_driving_wheels`, `m_steering_wheels`, `m_breaking_wheels`, `m_exhausts`, `m_car_weapons`, `bone_map` и `LL_Bones()` заменены на range-for.
- Циклы с `erase` (`m_doors_update`), `find` (`is_Door`, `Get_Door` и т.п.) и параллельные итераторы `RestoreNetState` оставлены без изменений.

Проверки stage 3: Debug/Release/RelWithDebInfo собраны; общий autotest и спавн `veh_zaz968` — `RESULT: PASS`, `errors=0`.

### Этап 4: разбиение оставшихся крупных функций

- `BindGaugeAndSuspensionCallbacks` стал диспетчером; добавлены `BindGaugeCallback`, отдельные `Bind*Gauge`-методы для каждого прибора и `BindSuspension`. Порядок привязки и `pribor_arrows` сохранены.
- `UpdateWheelPhysics` разбит на `UpdateWheelSuspension`, `UpdateWheelDownforce`, `UpdateWheelTireSound`, `UpdateWheelTraction`, `UpdateWheelTractionTimeout`, `UpdateWheelAirborneFriction`; rain-фактор считается один раз через `CalculateRainFrictionFactor`.
- Удалены оставшиеся обрывки закомментированного кода в wheel-физике.

Проверки stage 4: Debug/Release/RelWithDebInfo собраны; общий autotest и спавн `veh_zaz968` — `RESULT: PASS`, `errors=0`.

### Этап 5: исправления поведения

- Зарегистрирована консольная команда `rs_car_unlimitedfuel` (флаг уже проверялся в `UpdateFuel`).
- `interior_volume` теперь применяется глобально к 3D-звукам в новом микшере через `psSoundCarFactor`; в первом лице фактор выставляется в `CCar::OnCameraChange`, сбрасывается при выходе из машины и её удалении. Погодные 2D-звуки (`ambient\rain` и капли по крыше) масштабируются в `CEffect_Rain::OnFrame`, дождь по шлему — в `CActor::PlayRainOnHelmetSound`. Для коробки передач, whine и звуков дверей добавлен флаг `sm_NoCarFactor`, поэтому они не глушатся вместе с остальными звуками.
- `UpdateWheelAirborneFriction` больше не вызывает `Neutral()` для ведущих колёс при нажатом газе (`e_state_drive == drive`, двигатель и сцепление включены), поэтому колёса раскручиваются в воздухе. При отпускании газа колесо снова нейтрализуется.
- `rs_car_info` показывает массу машины.
- `doors_use_recursive` открывает двери строго по порядку: следующая дверь запускается только после полного открытия предыдущей, а прямой обход через багажник заблокирован до наступления его очереди (`TryTrunk`). Повторное использование переключает цепочку на закрытие в обратном порядке. Добавлены `AreRecursiveDoorsOpened()` / `AreRecursiveDoorsClosed()` и защита итераторов `m_doors_recurs`.

Проверки stage 5: Debug/Release/RelWithDebInfo собраны; общий autotest, спавн `veh_zaz968` и прогон с `rs_car_unlimitedfuel 1` + `rs_car_info 1` — `RESULT: PASS`, `errors=0`.

### Этап 6: отображение громкости и окклюзия машины

- `rs_car_info` показывает настроенный `interior_volume` и текущий активный `psSoundCarFactor`.
- Звуки машины (`SCarSound`, двери) создаются с флагом `sm_NoOCC`. Микшер для таких слотов не считает окклюзию от кустов и препятствий: пропускаются `get_occlusion_to()` при рендере, плавное обновление `Volume.z` и пересчёт окклюзии при обновлении позиции. Радиус слышимости при этом по-прежнему учитывается.
- Это исключает просадку громкости двигателя, дверей, сигнала и остальных звуков машины, когда камера или сама машина находится в кустах.

Проверки stage 6: Debug/Release/RelWithDebInfo собраны; общий autotest и прогон с `rs_car_unlimitedfuel 1` + `rs_car_info 1` (со спавном `veh_zaz968`) — `RESULT: PASS`, `errors=0`.

### Этап 7: громкость салона при открытых дверях

- Активный `psSoundCarFactor` теперь пересчитывается каждый кадр для машины с водителем (`CCar::UpdateInteriorSoundFactor`), поэтому он реагирует не только на смену камеры, но и на состояние дверей.
- Если активна первая камера и открыта хотя бы одна обычная дверь (`!non_enterable && !non_exitable`, состояние `state != closed`), фактор становится средним между `interior_volume` и `1.0`:
  ```cpp
  psSoundCarFactor = 0.5f * (interior_volume + 1.0f);
  ```
  Это включает состояния `opening`, `opened` и `closing`; полностью закрытая дверь возвращает обычный `interior_volume`.
- Двери с `non_enterable` или `non_exitable` в расчёте не участвуют.
- `rs_car_info` дополнительно показывает `Open cabin door: yes/no`, чтобы видеть причину изменения `Active car volume`.

Проверки stage 7: Debug/Release/RelWithDebInfo собраны; общий autotest и прогон с `rs_car_unlimitedfuel 1` + `rs_car_info 1` (со спавном `veh_zaz968`) — `RESULT: PASS`, `errors=0`.

### Этап 8: физическая турель и сон машины

- Причина прыжков `veh_brdm` — физическая турель: её моторы постоянно удерживали цель и создавали реактивный момент на корпусе, а `CPHElement::Enable()` не давал элементам турели уснуть. Из-за этого корпус не проходил в auto-disable и дрожал на месте.
- В `CCarTurret` добавлены `WakePhysics()` и `ParkPhysics()`:
  - если турель неактивна и машина стоит (`< 0.15 м/с` и `< 0.15 рад/с`) дольше `0.75 с`, моторы турели обнуляются, скорости тел сбрасываются, а тела `machine_gun`/`machine_gun_tower` переводятся в disabled;
  - при движении машины (`> 0.25 м/с` или `> 0.25 рад/с`), активации турели, движении мыши или `SetParam()` тела снова включаются;
  - если оболочка включается целиком (посадка в машину, восстановление save), неактивная турель остаётся припаркованной до начала движения или активации.
- После парковки корпус машины получает возможность полностью уйти в auto-disable: в проверке `veh_brdm` скорость и угловая скорость становятся нулевыми, корневой элемент переходит в disabled вместе с турелью.

Проверки stage 8: Debug/Release/RelWithDebInfo собраны; общий autotest и отдельный спавн `veh_brdm` — `RESULT: PASS`, `errors=0`.

### Этап 9: оружие физической турели после парковки

- Нашлась вторая причина отсутствия стрельбы и типа патронов у `veh_brdm`: слот `wpn_section = btr60_mgun` ссылается на секцию, которой нет в базовом `gamedata`. Старый цикл создания оружия удалял невалидный слот, делал `--i` у `u16` и из-за переполнения больше не доходил до валидного `wpn_section_1 = wpn_pkm`. В итоге у физической турели было `0` орудий: `m_cur_gun == nullptr`, HUD не получал тип патронов, `Action(eWpnFire)` ничего не запускал.
- `CCarTurret` теперь перебирает все слоты `wpn_section` / `wpn_section_N`, пропускает отсутствующие секции и сохраняет последующие валидные орудия. `m_guns_size` выставляется по фактическому числу созданных орудий. Для `veh_brdm` без `btr60_mgun` остаётся рабочий `wpn_pkm`; при наличии основной секции работают оба орудия.
- `WakePhysics()` при выходе из парковки включает всю физическую оболочку машины, а не только тела турели, и обновляет список патронов текущего орудия. Это гарантирует, что после auto-disable корпус, fire bone и состояние боеприпасов согласованы.
- `eWpnFire`, `eWpnAutoFire` и выбор орудия `SetGunCircle()` теперь также запрашивают пробуждение физики.

Проверки stage 9: Debug/Release/RelWithDebInfo собраны; общий autotest, спавн `veh_brdm` и stage 3 со спавном `veh_zaz968` — `RESULT: PASS`, `errors=0`. Целевой прогон с посадкой в `veh_brdm`, парковкой турели, активацией и стрельбой: HUD получает тип патронов, `OnShot` уменьшает боезапас, после деактивации турель снова паркуется.

### Этап 10: запуск двигателя при повреждениях и без топлива

- `CCar::StartEngine()` теперь запускает попытку даже при нулевом топливе: проигрывается звук стартера, но двигатель не заводится.
- Если игрок находится в машине (`OwnerActor()`), успешность попытки случайная:
  - играет `damage_particles1` — шанс равен нормированному порогу здоровья начала этой стадии (`2/3`);
  - играет `damage_particles2` — шанс равен нормированному порогу здоровья начала этой стадии (`1/3`); при обеих стадиях приоритет у `damage_particles2`.
- Для запуска сторонним методом без игрока вероятность не применяется: при наличии топлива двигатель запускается как раньше.
- При неудачной попытке звук глушения запускается сразу после `engine_sound_start_dellay`, не дожидаясь окончания клипа стартера; сам звук стартера в этот момент останавливается через `stop_deffered(CarDeferredStopFade)`, как остальные звуки машины. `b_starting` сбрасывается в этот же момент.
- Пока идёт неудачная попытка, циклы работающего двигателя не включаются, топливо не расходуется и не уходит в минус.
- Для машин без звука остановки двигателя состояние звука теперь корректно завершается, а не остаётся в `sndStoping`.

Проверки stage 10: Debug/Release/RelWithDebInfo собраны; общий autotest, спавн `veh_brdm` и stage 3 — `RESULT: PASS`, `errors=0`. Целевой прогон запуска: при здоровье `0.5` шанс `0.667`, при `0.2` — `0.333`, без топлива двигатель не запускается, а звук глушения запускается после `engine_sound_start_dellay`.

### Этап 11: индикаторы HUD и firsteye_panel

- В `CCar::VisualUpdate()` восстановлено обновление ламп `CUICarPanel`:
  - лампа двигателя показывает `b_engine_on`;
  - лампа света показывает `m_lights.IsOn()` (фары).
- При инициализации HUD обе лампы выставляются в выключенное состояние, поэтому пары `on/off` больше не рисуются одновременно.
- `CUICarPanel::ShowPanel()` теперь скрывает содержимое панели (`car_static`), а не вызывает `Show()` у всего окна. Благодаря этому `firsteye_panel = false` действительно убирает HUD в виде от первого лица; при других камерах он возвращается. `CCar::VisualUpdate()` уже вызывал `ShowPanel()` и `Show(true)` в правильном порядке.
- Для `firsteye_panel` используется существующая настройка машины (`veh_brdm` — `false`); в коде она читается в `CCar::Load`.

Проверки stage 11: Debug/Release/RelWithDebInfo собраны; общий autotest — `RESULT: PASS`, `errors=0`.

### Этап 12: восстановление damage_particles после загрузки

- При загрузке `CDamagableItem::RestoreEffect()` больше не применяет заново уже активные стадии урона. Раньше он проходил с `1` до текущего уровня и при уровне `2` вызывал `Stop2()` и `Play2()` в одном кадре. Остановка партиклов срабатывала позже, из-за чего после загрузки оставался только `damage_particles1`.
- Теперь восстанавливаются только недостающие стадии начиная с `m_level_applied + 1`; уже работающий `damage_particles2` не перезапускается.

Проверки stage 12: Debug/Release/RelWithDebInfo собраны; общий autotest — `RESULT: PASS`, `errors=0`.

### Этап 13: визуальная ось зависимой подвески

- В `CCar::cb_Suspension()` для зависимой подвески (`first_wheel` + `second_wheel`) раньше перезаписывалась только ось `i` матрицы кости. Оси `j`/`k` оставались прежними, из-за чего матрица переставала быть ортонормированной: скиннинг воспринимал это как сдвиг/масштаб, и визуальная ось «растягивалась».
- Теперь строится согласованный ортонормированный базис:
  - `i` направляется по линии между колёсами;
  - `j` берётся из исходной оси вверх и ортогонализуется к `i`;
  - `k` вычисляется как `i × j`.
  Длины исходных осей `i`/`j`/`k` сохраняются.
- Смещение `c.y` по-прежнему равно средней высоте колёс плюс `suspension_offset`; независимая подвеска не менялась.

Проверки stage 13: Debug собран; общий autotest — `RESULT: PASS`, спавн `veh_brdm` проходит без ошибок, связанных с подвеской.

### Этап 14: освобождение машины после выхода из Mincer

- `CBaseGraviZone::feel_touch_delete()` теперь освобождает объект, который удерживается телекинезом, перед удалением его из `m_ObjectInfoMap`. Раньше зона только убирала запись об объекте, а `CTelekinesis` продолжал держать машину: гравитация оставалась выключенной, и машина висела в воздухе после выхода из аномалии.
- Для освобождения вызывается `telekinesis()->deactivate(GO)`, а партиклы зоны останавливаются через `StopTeleParticles(GO)`.
- `CTelekinesis::clear_deactivate()` дополнительно восстанавливает гравитацию удерживаемым объектам. Этот путь используется, когда выброс Mincer заканчивается и объекты перестают удерживаться без импульса; без явного `set_ApplyByGravity(true)` они оставались висеть в воздухе.
- Проверено временным autotest-сценарием с `zone_mine_gravitational_strong` и `veh_zaz968`: после переноса зоны в сторону машина падает, после отключения зоны во время удержания — тоже. Общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 15: корректная позиция трупа при смерти в машине

- При смерти актёра `CEntity::KillEntity()` отцепляет его от машины через `use_HolderEx(nullptr, true)` → `CCar::detach_Actor()` → `CActor::detach_Vehicle()`. Позиция выхода бралась из `m_exit_position`, которая заполняется только при обычном выходе. Если игрок вошёл в машину и сразу погиб, там оставался `(0,0,0)`, и труп появлялся в начале координат; после предыдущего выхода могла использоваться старая точка на другом конце карты.
- Добавлен `CCar::RecalculateExitPosition()`: перед отцеплением точка пересчитывается от текущего положения машины по выбранной двери (или первой двери), а при отсутствии дверей/костей — по позиции машины.
- `CCar::detach_Actor()` вызывает пересчёт до отсоединения, поэтому `CActor::detach_Vehicle()` получает актуальную точку и создаёт персонажа/рагдолл рядом с машиной.
- Проверено временным autotest-сценарием: спавн `veh_zaz968`, посадка актёра и `kill`. До исправления позиция перед смертью была `(0,00 0,00 0,00)`, после — рядом с машиной (примерно `(264,33 19,66 551,96)` при машине в `(264,24 19,33 550,82)`). Общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 16: разбор ODE `LCP internal error` при спавне машины

- Сообщение воспроизводилось нестабильно: временный autotest спавнил `veh_zaz968` и примерно в части запусков получал один `LCP internal error, s <= 0`.
- В ODE добавлена диагностика решателя. В момент ошибки: `n=33, i=24, nub=20, cmd=5, si=21, si_lo=-3.0, si_hi=3.0, si_x=-3.0, si_w=0, si_dw=1.75e-4`, остров `veh_zaz968...`. Это не лимит колёсного шарнира и не нулевая масса: блокирующая строка — **friction-строка контакта** с пределом `±mu` (`mu=3`), уже стоящая точно на нижней границе.
- `dSolveLCP()` считал нулевой шаг `s == 0` внутренней ошибкой, прерывал решение и обнулял оставшиеся строки. Теперь нулевой шаг при уже активной границе применяет переключение состояния и продолжает решение; на один ведущий индекс допускается не более 8 таких переключений, после чего используется прежний аварийный выход.
- Дополнительно:
  - `CCar::SWheel::applywheelCollisionParams()` проецирует `fdir1` на плоскость контакта и нормализует его. ODE требует единичный касательный вектор; на уклонах оси машины не касательны, из-за чего friction-строки вырождались.
  - `characters_scraper_shell` получает `XFORM()` машины до `Activate()`, а не активируется в начале координат.
- Проверки stage 16: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`; 10 повторных запусков спавна `veh_zaz968` — ни одного `LCP internal error`.

### Этап 17: проверка высоты потолка при вставании в машине

- В LA `CPHMovementControl::ActivateBoxDynamic()` перед сменой стойки проверяет свободное место над головой. Этот код не был перенесён: в текущей ветке проверка сразу переходила к динамической активации бокса.
- Добавлена проверка для боксов `id == 0` и `id == 1`:
  - вокруг целевого бокса строится фрустум и через `g_SpatialSpacePhysic->q_frustum()` ищутся ближайшие физические объекты (кроме персонажа актёра);
  - если такие объекты есть, вверх из центра бокса пускается луч `GetPickResult()` с длиной радиуса бокса;
  - при попадании ближе радиуса вставание отменяется, и актёр остаётся в приседе.
- Это не даёт встать внутри кузова RAF/PAZ и толкнуть крышу/перевернуть машину.
- Проверено временным autotest-сценарием: спавн `veh_raf` над актёром; после запроса приседа и затем вставания состояние `mcCrouch` осталось `true`. В том же сценарии без машины в открытом месте после запроса вставания `mcCrouch` стало `false`.
- Проверки stage 17: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 18: сцепление и flotation по поверхностям для всей машины

- В `vehicles.ltx` добавлена секция `surface_params`, ссылка на неё задаётся в `m_car_base`; конкретная машина может переопределить секцию.
- Формат строки: `материал = friction, flotation`. `friction` — абсолютное базовое трение до множителей колеса; `flotation` — `0..1`, где `1` — без дополнительного сопротивления, `0` — максимальное; `-1` — оставить значение из `gamemtl.xr`.
- `CCar::LoadSurfaceParams()` резолвит имена материалов через `GMLib.GetMaterialIt()` и кэширует параметры по `SGameMtl::ID`. Неизвестные имена логируются и пропускаются.
- Колёса: `applywheelCollisionParams()` применяет `friction` до `mu_factor`, дождя, нитро и slip-множителей, а для `flotation` ставит override на тело колеса.
- Кузов/крыша: `CarBodySurfaceCallback` на основном шелле применяет те же значения к контактам со статической геометрией уровня. Колёса пропускаются, чтобы не перетереть их множители.
- В `CPHContactBodyEffector` добавлен flotation override. `CPHContact::SetFlotation()` создаёт или обновляет effector даже для материалов без `flSlowDown` (например, песок), поэтому торможение работает и для колёс, и для кузова/крыши, и в воде.
- В wheel info добавлены `surface_material`, `surface_friction` и `surface_flotation`; в общем info показывается последняя поверхность под кузовом.
- Конфиг: `ixr_addons/carpack/configs/misc/vehicles.ltx`, секция `[car_surface_params_default]`. Начальные значения: песок `0.55/0.88`, земля `0.85/0.95`, вода `-1/0.80`.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Временный лог подтвердил для `materials\earth`: `surface_friction=0.85`, `surface_flotation=0.95`, flotation override применён (`recip=0.05`).

### Этап 19: общие параметры колёс и направление `bind_position_add`

- В `[wheels_params]` теперь можно задавать значения по умолчанию сразу для всех колёс: `radius_factor`, `velocity_factor`, `torque_factor`, `gyroscopic_mode`, `bind_position_add`, `bind_direction_add`. Секции конкретных колёс переопределяют эти значения.
- `radius_factor` теперь применяется в `SWheel::Init()` после `e->getRadius()`. Раньше он умножал неинициализированное поле `radius` в `SWheel::Load()`, а затем результат перезаписывался, поэтому параметр фактически не работал.
- `bind_position_add` теперь трактуется в модельном пространстве и переводится в пространство родительской кости через обратную bind-ориентацию. Это исправляет рулящие колёса: их родитель (`left_caliper`/`right_caliper`) повёрнут, и старое сложение напрямую в `bind_transform.c` смещало колесо не в ту сторону. Временный лог: для задних колёс offset `(0, 0.1, 0)` остался `(0, 0.1, 0)`, для передних с учётом родителя получился `(0, 0, -0.1)`.
- `gyroscopic_mode` читается для совместимости, но по-прежнему игнорируется: в ODE-форке нет `dBodySetGyroscopicMode`.
- Из wheel info убраны `material_pair_friction` и `material_flotation`; остались `surface_material`, `surface_friction`, `surface_flotation`.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Временный конфиг с defaults в `[wheels_params]` подтвердил `radius_factor = 1.2` (`0.303 → 0.363`), `velocity_factor = 1.1`, `torque_factor = 1.2` и применение `bind_position_add` ко всем колёсам.

### Этап 20: взаимодействие с дверью через дочернюю кость

- Добавлен `CCar::GetDoorBone()`: от кости, на которую смотрит игрок, он поднимается по `CBoneData::GetParentID()` до первой кости, зарегистрированной в `m_doors`.
- `Get_Door()`, обе перегрузки `is_Door()`, `GetNearestDoor()` и `TryTrunk()` теперь используют этот резолвер. Поэтому стекло, ручка или другая дочерняя кость двери открывает/закрывает саму дверь.
- Проверка багажника дополнительно требует `m_bone_trunk != BI_NONE`, чтобы отсутствие багажника не совпадало с `BI_NONE` при промахе.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Временный лог подтвердил корректный обход иерархии дверей; в использованных тестовых моделях стекло не было отдельной дочерней костью, поэтому наведение на стекло нужно проверить в игре.

### Этап 21: подсказка двери только при наведении на дверь

- Раньше `CActor::UpdateCL()` выставлял `car_character_use` («Открыть дверь») при любом взгляде на машину в пределах `PICKUP_DISTANCE`, даже если под прицелом был только кузов.
- Добавлен `CCar::IsDoorInSight()`: через `GetNearestBone()` и `GetDoorBone()` проверяет, что под прицелом дверь или её дочерняя кость, и что дверь находится в пределах `use_distance`.
- В `Actor.cpp` подсказка `car_character_use` теперь выставляется только при `IsDoorInSight()`. Багажник и `usable_bones` обрабатываются как раньше; для не-машин поведение не менялось.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 22: выход через открытую дверь с прозрачной верхней вставкой

- `CCar::SDoor::CanExit()` для открытой, открывающейся или сломанной двери разрешает выход только если луч проходит через `TestPass()` или через область, где дверь находилась в закрытом положении. Область кэшируется в `SDoor::ExitArea` методом `BuildExitArea()`: он собирает вершины визуальной геометрии двери и её дочерних костей через `IKinematics::EnumBoneVertices()` и строит AABB в bind-пространстве двери. Поэтому прозрачная верхняя вставка входит в проём, даже если у неё нет отдельной physics-кости/OBB, а выйти, глядя в любое место машины, по-прежнему нельзя.
- `BuildExitArea()` вызывается в `SpawnInitPhysics()` после `CalculateBones()`. Кэш хранится в bind-пространстве двери и при проверке трансформируется через `XFORM() * bones_bind_forms[bone_id]`, поэтому после загрузки сохранения с уже открытой дверью проём остаётся на месте закрытой двери. Если `EnumBoneVertices()` не вернул вершины, используется прежний fallback по closed-bind OBB.
- Логика `Use()` не менялась: нажатие на саму дверь по-прежнему переключает/закрывает её. Выход срабатывает, когда прицел не попадает в дверь, а проходит через свободное пространство открытого проёма.
- `CCar::Exit()` теперь выбирает не первую подходящую дверь в порядке `m_doors`, а лучшую по направлению взгляда: учитывается `TestPass()` и скалярное произведение направления на дверь с направлением камеры. Это не даёт выйти через ранее открытую левую дверь, когда игрок смотрит в правый или другой проём.
- `CCar::Exit()` записывает `m_exit_door_bone` выбранной двери. `detach_Actor()` вызывает `RecalculateExitPosition()`, и без этого поля она откатывалась к первой двери в `m_doors` — обычно задней, из-за чего игрок появлялся сзади машины вместо открытой двери.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 23: сопротивление воды для машин с высокой подвеской

- Контактная flotation в ODE применяется к телу, которое пересекает воду. Если кузов находится выше поверхности, а в воде только колёса, вода не замедляла машину.
- Добавлен `CCar::UpdateWaterDrag()`; вызывается из `PhDataUpdate()` на каждом физическом шаге. Для каждого колеса вертикальный луч по статической геометрии ищет первый liquid-треугольник, вычисляет долю погружения колеса и drag-фактор.
- Drag применяется к физической оболочке по той же формуле, что `CPHContactBodyEffector::Apply()`: `10000 * (1 - flotation)^2 * drag_factor`, с ограничением `mass / step`. Flotation берётся из `surface_params` машины (в `vehicles.ltx` для `materials\water` задано `0.10`), иначе из `fFlotationFactor` материала.
- Drag-фактор линейно зависит от погружения колеса: `0` — колесо только касается поверхности, `0.5` — погружено наполовину, `1.0` — полностью в воде. Сопротивление растёт плавно вместе с погружением.
- Если колёса касаются воды, `UpdateWaterDrag()` становится единственным источником сопротивления: контактная flotation кузова и колёс для liquid-материалов подавляется через `SetFlotation(..., 1.0)` (в `CarBodySurfaceCallback()` и `applywheelCollisionParams()`), поэтому у поверхности нет конфликта двух drag-источников и полного замедления из `vehicles.ltx` при касании. Когда колёса не в воде, контактная flotation кузова работает как раньше.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Временный тест на Jupiter: машина, поставленная в воду, определила уровень воды и погружение колёс при кузове выше поверхности; drag применялся.

### Этап 24: удержание spring_loaded дверей после загрузки

- В `CCar::SDoor::RestoreNetState()` для открытых, открывающихся и закрывающихся дверей теперь заново выставляются `open_time`, `real_open_time` и `real_close_time`.
- Раньше эти таймеры не сохранялись в сейве. После загрузки `Update()` мог сразу переключить `spring_loaded` дверь на слабый удерживающий момент или сработать по устаревшему `real_open_time`, из-за чего открытая дверь переставала удерживаться.
- `open_time` инициализирован в конструкторе `SDoor`.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 25: ремонт и заправка из инвентаря машины

- В окне инвентаря машины (`mmDeadBodySearch`, партнёр — `CCar`) добавлены действия контекстного меню:
  - «Починить» — для предмета из `repair_item` в секции машины (`vehicles.ltx`);
  - «Заправить» — для предмета из `refuel_item`; объём одной заправки задаёт `refuel_amount`.
- В `[m_car_base]` (`vehicles.ltx`) заданы тестовые значения: `repair_item = medkit_scientic`, `refuel_item = vodka`, `refuel_amount = 10`. Их можно переопределять в секциях конкретных машин.
- Если `repair_item`/`refuel_item` не заданы, соответствующие действия недоступны. `refuel_amount` задаёт литров на один предмет; при отсутствии используется 10 л, а `refuel_amount = 0` отключает заправку.
- Действия доступны только пока актёр находится в машине: в инвентаре машины партнёр должен совпадать с текущей машиной, в инвентаре игрока используется машина из `Actor()->Holder()`. Снаружи, в том числе при открытом багажнике, действия скрыты.
- Ремонт и заправка блокируются, когда скорость машины выше 1 км/ч.
- Машина для действий определяется через `Actor()->Holder()`, а не через `GetPartner()`; это устраняет вылет при использовании предметов рядом с машиной при устаревшем или уже удалённом партнёре.
- Прицел в машине скрыт, пока не активна турель с оружием (`CCar::HasWeaponActive()`).
- `last_contact_time` — абсолютное время `Device.dwTimeGlobal` последнего контакта колеса с землёй, а не таймер; в `car_info` не выводится.
- Предмет не нужно перекладывать в багажник: действие доступно из инвентаря игрока, даже если багажник заполнен. Предметы в багажнике машины также поддерживаются.
- Ремонт: здоровье кузова восстанавливается на `repair_amount` от максимума (значение из секции машины в `vehicles.ltx`), а двери и колёса при любом использовании ремонтного предмета восстанавливаются полностью. Если кузов уже цел, но есть повреждённые двери/колёса, ремонт всё равно доступен.
- Заправка: один предмет из `refuel_item` добавляет `refuel_amount` литров. При полном баке действие скрыто и предмет не расходуется.
- Расход предмета в SP выполняется через удаление объекта и обновление UI; для MP действия пока скрыты, в коде оставлены точки для серверного события.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 26: коллизия с трупами и characters scraper

- `characters_scraper_shell` создавался с `SetIgnoreDynamic()`. В ODE это выставляет `NCClassDynamic`, поэтому scraper не сталкивался с персонажами и ragdoll-трупами: их оболочки тоже относятся к dynamic class. В результате трупы не отталкивались, а основной кузов и колёса могли их придавливать.
- `SetIgnoreDynamic()` убран, `SetIgnoreStatic()` оставлен. `CollisionDetectionCallback` теперь отсекает контакты с самой машиной, другими машинами, актёром и предметами инвентаря, но пропускает живых NPC и трупы.
- Коллизия основного кузова и колёс с трупами не отключается: `SetIgnoreRagDoll()` не вызывается, требование сохранить коллизию выполнено.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Нужен ручной прогон: переехать труп и проверить, что машина не застревает, а труп отталкивается.

### Этап 27: синхронизация анимации водителя с рулём

- Раньше `steer_Vehicle()` вызывался напрямую из `CarInput` на каждое нажатие/отпускание клавиши. При одновременном нажатии двух клавиш направление анимации расходилось с фактическим состоянием руля. На геймпаде `OnGamepadAxisMove` дополнительно вызывал `PlayCycle` почти каждый кадр, перезапуская цикл.
- Добавлен `CCar::UpdateSteerAnimation()`, который вызывается из `CarInput` сразу после изменения руления и из `CCar::UpdateEx()`. Направление и idle берутся из итогового `e_state_steer`, поэтому анимация включается и выключается с прежней скоростью, а комбинация клавиш больше не рассинхронизирует её.
- Прямые вызовы `steer_Vehicle()` из обработчиков клавиш и оси геймпада заменены на общий вызов `UpdateSteerAnimation()`. `m_steer_anim_state` сбрасывается при посадке актёра.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Нужен визуальный прогон от третьего лица: поворот, удержание и возврат руля.

### Этап 28: звук шин и отдельный контакт каждого колеса

- `UpdateWheelTireSound()` останавливал `tire_drive` только по глобальному `mb_traction`. Если одно колесо оставалось на земле, звук висящего в воздухе колеса продолжал играть.
- После включения `characters_scraper_shell` для коллизии с трупами scraper начал контактировать и с собственной машиной. Эти контакты попадали в `WheellCollisionCallback`, обновляли `traction`/`last_contact_time` и запускали звук шин. Теперь `WheellCollisionCallback` отбрасывает контакты со scraper по `CollisionDetectionCallback`.
- Теперь остановка проверяет контакт конкретного колеса (`last_contact_time`) и низкую скорость, а `tire_drive` не остаётся играть на месте. При потере контакта сбрасываются `buxe`/`breaking`/`slide`, чтобы занос не перезапускал звук после приземления. Минимальная частота звука поднята с `0` до `0.1`.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Нужен ручной прогон: прыжок/отрыв колёс, приземление, разные поверхности.

### Этап 29: непрерывные брызги от колёс в воде

- Контакт колеса с поверхностью воды существует только при пересечении поверхности. Поэтому `SWheel::Play()` запускал частицы `hit_water` только при въезде в воду; когда колесо уже под поверхностью, новых контактных событий нет.
- `CCar::UpdateWaterDrag()` и так каждый физический шаг находит liquid-треугольник и вычисляет погружение каждого колеса. В него добавлен периодический вызов пары материалов `objects\car_wheel`/жидкость: позиция берётся на уровне воды под колесом, направление — по скорости колеса, размер — от скорости машины или окружной скорости колеса.
- Эффект создаётся не чаще одного раза в 100–150 мс на колесо, только при погружении выше 0.02, в радиусе 70 м от камеры и когда движется машина или вращается само колесо (это важно после полного погружения, когда сопротивление воды быстро гасит скорость кузова). Первая камера и настройки звука шин на это не влияют.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Нужен ручной прогон на воде: въезд, движение, поворот и задний ход.

### Этап 30: камера третьего лица не уходит под воду

- В `CCar::cam_Update()` для `EctSecondCam`, `EctThridCam` и `EctFourthCam` добавлена проверка уровня воды. Она выполняется и для точки обзора до расчёта дистанции, и для итоговой позиции камеры.
- Вода ищется вертикальным лучом по статической геометрии: среди попаданий выбирается ближайший liquid-треугольник выше камеры. Луч начинается выше машины и заканчивается чуть ниже камеры, поэтому вода под мостом или берегом не поднимает камеру сквозь перекрытие.
- Если камера ниже поверхности воды, она поднимается на `0.3 м` над ней. Первая камера и свободная `EctFiveCam` не затрагиваются.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Нужен ручной прогон: въезд в воду, движение вдоль берега, камеры 2/3/4.

### Этап 31: компактный character scraper

- После включения коллизии scraper’а с ragdoll-трупами трупы начали отскакивать от машины на расстоянии нескольких метров. Причина — геометрия scraper’а: три сферы радиусом `0.5 * Radius()` со смещениями `±0.7 * Radius()` по длине выходили примерно на метр за пределы кузова.
- Затем выяснилось, что один OBB по визуальному bounding box тоже слишком велик: в него попадают части модели в bind-позе, выходящие за реальный кузов. Для ZAZ визуальный box был `2.89 × 2.26 × 4.06 м`, а объединение костных боксов кузова — около `1.62 × 1.55 × 3.98 м`.
- Scraper теперь состоит из набора OBB по костям `bone_map`: бокс каждой кости берётся через `LL_GetBox()`, переводится в пространство машины через `bones_bind_forms` и получает отступ `0.05 м`. Если у машины нет подходящих костных боксов, используется прежний визуальный AABB как fallback.
- `SetIgnoreStatic()`, динамические контакты с персонажами/ragdoll-трупами и отсечение самой машины, других машин, актёра и предметов сохранены.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Нужен ручной прогон: труп рядом с машиной, под машиной, проезд через труп.

### Этап 32: demo_record в машине

- `CCar::cam_Update()` всегда применял камеру машины напрямую через `OwnerActor()->Cameras()`, поэтому камера `demo_record` из `Level().Cameras()` не могла перехватить управление. Ввод при этом уже был захвачен записью, из-за чего управление казалось зависшим.
- Теперь при наличии `cefDemo` камера машины обновляется через `Level().Cameras().UpdateFromCamera()`, как это уже делает обычная камера актёра. То же добавлено для камеры активной турели `CCarTurret::cam_Update()`.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Нужен ручной прогон: `demo_record` в машине, движение камеры мышью и клавишами, выход по ESC, обычная камера после записи.

### Этап 33: защита R4 при смене камеры в demo_record

- При переключении камеры от третьего лица в `demo_record` R4 падал в `CRender::apply_lmaterial()`: `RCache.get_ActiveTexture()` мог вернуть `nullptr`, после чего код читал `T->m_material`.
- В `apply_lmaterial()` добавлен fallback на нейтральный материал, если sampler остался без текстуры. Это не даёт смене камеры или неполному набору render state уронить весь рендер.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Нужен ручной прогон: demo_record и переключение камер 1/2/3/free.

### Этап 34: запрет укусов мутантов в машине

- В `vehicles.ltx` в `[m_car_base]` добавлен параметр `allow_mutant_bite_in_car = false`.
- В движке значение по умолчанию — `false`; если параметра нет или он выключен, мутанты не наносят укус игроку, находящемуся в машине.
- Фильтр добавлен в `CBaseMonster::HitEntity()`, `CBaseMonster::Hit_Wound()` и в контроллере; отдельная проверка добавлена для крыс, которые отправляют `GE_HIT` напрямую.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`. Нужен ручной прогон: крыса, собака и другой кусающий мутант при игроке в машине; при `allow_mutant_bite_in_car = true` урон должен вернуться.

### Этап 35: стабильное направление нелинейного руля после загрузки

- `SWheelSteer::Init` вычислял `pos_right` из мировой ориентации physics-элемента колеса (`element->mXFORM.i.y`). Если машина загружалась из сейва в перевёрнутом положении, компонента `i.y` меняла знак, `GetSteerAngle()` инвертировался, и `steer_nonlinear` начинал выворачивать колесо быстрее, а возвращать медленнее: ветка `back = 1.5` срабатывала не с той стороны, а `SteerIdle` возвращал колесо через ограничитель.
- Теперь `pos_right` берётся из bind-ориентации `bones_bind_forms[bone_id].i.y`, как уже сделано для `pos_fvd` в `SWheelDrive`. Знак не зависит от мирового поворота машины.
- Проверка: autotest со спавном `veh_zaz968` вверх колёсами (`o_Angle.x = pi`). До исправления `SteerLeft` уводил угол в `lo_limit`, после — в `hi_limit`, а `SteerIdle` плавно возвращает его к нулю. Загрузка сейвов, сделанных в машине с вывернутым влево и вправо рулём, восстанавливает угол с правильным знаком.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 36: стандартизация параметров дверей

- Общие множители дверей теперь: `open_torque_factor` (момент) и `open_vel_factor` (скорость). Оба читаются из секции `[doors]` модели и могут быть переопределены одноимённым ключом в user data модели в секции с именем кости двери.
- Удалены устаревшие per-direction параметры `torque_factor_opening`, `a_vel_factor_opening`, `torque_factor_closing`, `a_vel_factor_closing`. Открытие и закрытие больше не различаются по моменту и скорости: используется один `torque` и один `a_vel`.
- `a_vel` конкретной двери теперь вычисляется как `M_PI * open_vel_factor` в `SDoor::Init()`; `open_torque_factor` переведён с `r_u16` на `r_float`.
- В `veh_base.ltx` в `[veh_base_doors]` добавлен `open_vel_factor = 1`.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 37: дождевое трение не действует на колесо в воде

- У `CCar::SWheel` появился флаг `InWater`: он выставляется, когда колесо касается жидкости или погружено в неё.
- Контакт с liquid-материалом сразу помечает колесо как `InWater`, а `CCar::UpdateWaterDrag()` каждый шаг уточняет флаг лучом вниз и глубиной погружения. Поэтому флаг работает и для полностью погружённого колеса, которое касается дна, а не поверхности воды.
- В `applywheelCollisionParams()` множитель `friction_rain_factor` больше не применяется к `sp.mu`, если колесо в воде. `friction_nitro_factor` продолжает применяться.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest и stage 3 — `RESULT: PASS`, `errors=0`.

### Этап 38: сила возврата костей в нейтральное положение

- Добавлен per-bone параметр `return_force` (момент на радиан) в user data модели, в секцию с именем кости. Пример: `[kabina]` в `veh_kamaz_u_05.ltx`.
- `ApplyBonePhysicsParams()` собирает список таких костей, а `UpdateReturnBones()` каждый физический шаг читает углы шарнира и применяет момент `-angle * return_force` вокруг мировой оси. Один `return_force` используется для всех осей кости.
- Поддерживаются `hinge` и `full_control`: у `full_control` обрабатываются три AMotor-оси, у `hinge` — одна. `hinge2`, `slider` и `ball` пропускаются, так как не дают подходящего угла возврата.
- Добавлена мёртвая зона `0.001` рад, чтобы кость не будила физику на околонулевом шуме.
- В `veh_kamaz_u_05.ltx` в `[kabina]` добавлен пример `return_force = 10000`.
- Проверки: Debug/Release/RelWithDebInfo собраны; общий autotest, stage 3 и спавн `veh_kamaz` — `RESULT: PASS`, `errors=0`.

## Следующие шаги

1. Гейт C: повторная ручная проверка камеры, посадки/высадки, дверей и багажника.
2. UI, 3D-приборы, Lua, MP, save/load.
