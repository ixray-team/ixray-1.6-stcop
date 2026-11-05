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

## Следующие шаги

1. Гейт C: повторная ручная проверка камеры, посадки/высадки, дверей и багажника.
2. UI, 3D-приборы, Lua, MP, save/load.
3. Настройка физики: единичное сообщение ODE `LCP internal error` при спавне машины; не фатально, требует проверки параметров колёсных контактов.
