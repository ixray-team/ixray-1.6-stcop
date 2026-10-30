# FFxExtendedEntityesLogic

Отдельная DLL расширенной логики мутантов (`FFxExtendedEntityesLogic.dll`). Здесь находятся подключение к движку, компоненты, общие службы и реализации категорий.

Текущие файлы:

- `EntryPoint.h/.cpp` — единственные внешние вызовы модуля; кадр, события, очистка; [MAIN/ENTITY].
- `CELBaseAdapter.h/.cpp` — реализует IELAdapter; общая база адаптеров владельца; [ENTITY].
- `CELSharedPerceptionBudget.h/.cpp` — реализует IELBudget; общая служба; [ENTITY].
- `CELSharedSettings.h/.cpp` — общие неизменяемые настройки, профили и версия режима; [INIT/READ].

Перенос в отдельную DLL выполнен. Готовность и оставшиеся механики: [архитектура](../../doc/extended-entityes-architecture.md).

Игровые обновления и события подключаются через EntryPoint.h; команды регистрируются заголовочным регистратором рендера, игровые типы — через API DLL. Обработчики проверяют аргументы и режимы внутри модуля; снаружи остаются события и краткий возврат при обработке. Регистрация команд — прямой CELSharedDebugCommands::Register до загрузки user.ltx, без связи рендера с игровой DLL. В EntryPoint не добавлять условные прослойки ради объединения регистраторов. Подробная карта внешних точек — в архитектуре. Новые параметры LTX сопровождать русскими комментариями и одновременно обновлять реальные секции и empty_example.ltx.

## Граница DLL

Все `.cpp` этого каталога собираются целью `FFxExtendedEntityesLogic`. `xrGame` подключает интерфейсы событий и регистрации типов и вызывает модуль через версионированную таблицу `ModuleAPI.h`; загрузчик находится в `src/xrGame/FFxExtendedEntityesModule.cpp`. Прямой зависимости линковки `xrGame → FFxExtendedEntityesLogic` нет. Модуль линкуется с игровым хостом и его библиотеками; Windows-экспорты хоста формирует `cmake/FFxGameHostExports.cmake`: из определений `xrGame` выбираются только символы, на которые ссылаются объектные файлы модуля. Для глобальных данных используются явные импорты `FFxGameHostAPI.h`. На MSVC хост и модуль компилируются с `/GL-`, чтобы сборщик экспортов мог прочитать обычные COFF-символы объектных файлов; оптимизация компилятора сохраняется, межмодульная оптимизация LTCG для этих исходников отключена. Это C++ ABI одного комплекта сборки: DLL должны пересобираться и распространяться вместе.

Компонент `TExtendedEntityesLogic` создаётся и читается только внутри модуля. Генератор идентификаторов ECS находится в `xrCore`, поэтому идентификаторы разных DLL не пересекаются. При завершении игры сначала освобождаются регистрации фабрики, затем очереди и ECS-хранилище расширения, и после этого выгружается DLL. Поддерживается переключение логики через `monster_logic`, но горячая замена DLL во время уровня не предусмотрена.

Сборка: `cmake --build build --config RelWithDebInfo --target FFxExtendedEntityesLogic`. Цель `xrEngine` также собирает модуль. Результат размещается рядом с `xrGame.dll` в `build/bin/<Config>/`.

## Тактика и восприятие — 28 октября 2026

- `Perceptions/CELSharedEnvironment.h`: суточные/погодные коэффициенты, порог дождя для нюха и ближнего обнаружения. Нативное зрение получает коэффициент через версионированный ModuleAPI v2.
- `Perceptions/CELSharedWeaponThreat.h`: наблюдаемая угроза оружия, отдельная досягаемость ножа; скрытые команды ввода не читаются.
- `Perceptions/CELSharedHuntGroups.h/.cpp`: общий снимок ролей с интервалом 100 мс, поколения владельцев, свежесть по стамине и момент атаки союзника. Полные списки, без первых 32 записей.
- `Ground/Behaviors/CELGroundCombatTactics.h/.cpp`: временное боевое намерение, уворот/ложный выпад/контратака, парный манёвр, спринт и штатный прыжок. Исполнение пути остаётся в GroundAttackBehavior и GroundMovement.
- `Ground/Behaviors/CELGroundFlyingGrab.h/.cpp`: максимум восемь одновременных физических захватов после подтверждённого смертельного попадания. Записи содержат ID и проверку жизненного цикла; тела не телепортируются, коллизии не выключаются.
- `Flying/.../Behaviors/CELFlyingSearchBehavior.h/.cpp`: конечный подход/поиск по сохранённой позиции и следу, включая интерес к шумовой позиции без врага.
- `CELSharedHearing`: ограниченные списки ожидающих и завершённых интересов, естественные сроки, запрет повторного открытия старым пересказом.

Профили и `empty_example.ltx` обновляются вместе. Состояния тактики, стамина, причины запрета прыжка, роль группы, очередь интересов, погода и активный захват выведены в существующую русскую отладку. При выключенной отладке строки/события не собираются. Новых тестовых целей в CMake нет; обычный список источников подхватывается существующим GLOB/сборкой.

Граница готовности и оставшиеся сценарии: [контроль механик](../../doc/monster-behavior-audit.md). Наличие нового класса не означает игровую приёмку соответствующей схемы.

Продолжение тактики: GroundApproach хранит только наблюдаемую скорость выбранной цели; HuntGroups передаёт краткоживущие блокирующие позиции с идентификатором цели. Скрытость кандидата проверяется не более одним лучом на допустимое решение в общей квоте. Контрольные сборки после передачи сборки пользователю не запускаются; см. более поздний статус в контрольном документе.

### Shared configuration source

`CELSharedSettings` reads the merged `system.ltx` section selected by `engine_external.ltx` `[gameplay] DeathOrDieMonstersLogicSectionName` (default `monsters_logic`). The monster addon supplies it through `configs/mod_system_new_monsters_logic.ltx`. No standalone INI filename is opened by the module. The pointer to `pSettings` is borrowed and never deleted. Personal entity fields override the selected shared section. See `doc/monster-behavior-scenarios.md` and `doc/monster-behavior-audit.md` for action dependencies and acceptance limits.

Crow shared memory uses the same merged system settings: `[gameplay] DeathOrDieCrowSharedMemorySectionName` selects its section (default `crow_shared_memory`). The addon places this section after `[monsters_logic]` in the same `configs/mod_system_new_monsters_logic.ltx`, with parameter descriptions. There is no separate crow INI file loader. Missing sections retain the SSettings defaults and log a diagnostic. Species flight_* and crow_* settings remain in entity sections.

### Aim evasion controls

Ground tactics estimate their owner's actual planar motion and distinguish an aim cone over the current body from a gun leading that motion. Escape goals include a full-body cone boundary and a configurable margin; committed turns can reverse on a leading aim after a bounded interval. The five controls are `monster_dodge_max_width`, `monster_dodge_cone_margin`, `monster_dodge_jump_max_angle`, `monster_dodge_prediction_time`, and `monster_dodge_direction_interval`; defaults and dog_cute overrides are recorded in the scenario/audit documents and the addon example. Evasive point jumps pass a separate optional heading limit to the native controller and validate the ballistic corridor; ordinary calls retain their species heading limit. A distant post-dodge counter window does not suppress ongoing aim evasion. Native game and module binaries must be rebuilt together after the signature change.

All addon LTX parameters have inline explanations. Obsolete keys were removed only after checking both native/new logic and available scripts, preserving dynamically named table rows and generated key families. The audit document lists removed names and the unchanged-value verification.

Current firearm evasion is independently controlled by `monster_dodge_fire_enabled`; disabling it preserves melee/feint/dive permissions. Jump chance rolls use species retry windows of 2–4 seconds and landing cooldowns of 5–10 seconds. Jump angles sample the configured 15–40 degree forward cone; `monster_dodge_jump_width_min/max` sample a desired lateral displacement within native reach and safety constraints. The current config-only per-section values are in `doc/monster-evasion-settings.md`.

### Evasive jump ownership

Ready firearm jump evasion preempts step evasion without sharing its cooldown. `monster_dodge_jump_on_aim` permits broad aim-triggered attempts; `monster_dodge_jump_distance_max` caps evasive travel independently of native attacking jumps. Candidate angles are relative to the target, not the current heading. Landing clears the actual gun ray with projected body support and configured margin. Native `eEvasiveJump` plays the existing landing motion without the attack follow-through path, releases by animation end or its duration deadline, and recognizes physical ground support after flight time. Debug records retain the last evasive jump rejection reason.

### Personal target utility

`CELSharedTargets` evaluates enemy distance, confirmation, damage, proximity, bounded ally load, pursuit commitment and decaying per-enemy interest in one cost. It applies no actor-specific bonus. Confirmed combat excludes unrelated unconfirmed noise without erasing memories. `RecordHit` adds personal interest only for real health loss, symmetrically for attacker and victim. The per-owner cache is generation-scoped, limited to 64 entries and pruned with perception memory. Debug shows selected/previous cost and interest. All tuning lives in the perception profile and the two example configuration sections.

Short evasive flight uses `monster_dodge_jump_animation_speed_max` in both geometric prediction and execution. Global glide starts apply one physical impulse; duplicate part-start events cannot restart flight. Native attack timing remains unchanged.

Tactical native jump requests carry a scope-bound permission marker. The chimera attack state accepts that explicit request and yields its own sequence while an evasive jump owns the controller. This integrates native state permissions without species-specific tuning in the module. Evasion length has configurable horizontal minimum/maximum bounds; debug shows actual versus planned displacement and whether the single impulse started.

Active point-directed evasive flight bypasses grounded path steering in CPHMovementControl while retaining physical position/velocity updates, contacts and gravity. The old ground acceleration is cleared before launch. Jump animation stages opt out of locomotion phase inheritance; normal locomotion transitions retain phase synchronization. This is shared executor behavior, without species-based tuning.

Evasive touchdown now returns directly to locomotion: the native moving-ground animation must not be played stationary. Physical touchdown requires elapsed flight time, contact, non-jumping PH state and ground environment. Flight duration is retained separately for diagnostics; phase, actual velocities, physics state, scale and loaded bounds are shown. Animation ownership resets clear pending completion flags and stale fading blends cannot complete the current global motion.

Touchdown and diagnostics read `CPHCharacter::HasGroundSupport() const`. `ContactWas()` consumes a one-shot control-contact event and must not be used as a support predicate or by diagnostic drawing. The persistent getter uses the physical support state and upward contact normal; it is independent of gameplay/debug read order. The new virtual interface method requires rebuilding the physics runtime and its dependent consumers together.

Jump startup requires both an idle controller and physical ground support with no pending/active physical jump. Evasive bounds are revalidated natively at request and takeoff. Ballistic prediction returns a local duration and cannot modify an already running jump timer.

Accepted firearm evasions register a single already-confirmed target for bounded facing-independent tracking. Observe refreshes LOS within the shared perception budget; adapter/debug queries read the cache only. Real occlusion, range, weather, generation and expiry remain effective. Native post-evasion recovery also gates attacking jumps through monster_dodge_attack_jump_delay, preventing an immediate second jump without blocking locomotion or melee.

Retained target contact is not firearm-evasion authorization. Weapon threat checks require the enemy inside the observer actual eye frustum as well as weapon aim/lead alignment. Grounded firearm evasion is cancelled when the observer faces away; target memory remains, and an already launched physical jump is not interrupted midair.
