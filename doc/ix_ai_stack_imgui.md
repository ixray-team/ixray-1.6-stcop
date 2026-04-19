IX AI Stack отладочная панель ImGui

Раздел 1. Назначение

Документ перечисляет элементы окна IX AI Stack в отладочном интерфейсе редактора. Окно подписывается в CImGuiManager под именем IX AI Stack и рисуется функцией RenderIxAiStackWindow. Видимость окна управляется флагом редактора Game_IxAiStackManager. Параметры слайдеров и флагов пишутся в глобальную структуру g_ixAiRuntimeTuning и применяются сразу при работающем стеке, если не указано иное. Кнопка Reset tuning defaults вызывает IxAiRuntimeTuningResetDefaults и сбрасывает значения к заводским из кода.

Раздел 2. Общая панель над вкладками

Reset tuning defaults. Сбрасывает все поля тюнинга к дефолтам из IxAiRuntimeTuning в коде. Рядом пояснение что изменения действуют немедленно при запущенном стеке.

Раздел 3. Вкладка Overview

Строка состояния стека. RUNNING если IxAiStackApi активен, иначе OFFLINE и краткий текст что тюнинг ниже всё равно меняется, а агенты и буферы требуют загруженный одиночный уровень с EnableIxAiStack в engine_external и перезагрузку.

Activation checklist. Набор буллетов только для чтения. Dedicated server не должен быть выделенным сервером. Target game mode должен соответствовать целевому режиму движка. EnableIxAiStack в EngineExternal должен быть true. Singleplayer. Указатель на игровой уровень не nullptr. Если уровень есть, дополнительно строка Level ready bReady да или нет.

Telemetry при живом стеке. Last manager update в миллисекундах за последний кадр менеджера. Visual probes last frame число визуальных зондов за кадр. Corpse probes last frame число зондов трупов за кадр.

При мёртвом стеке. Строка что менеджер не обновлялся.

Счётчики последнего кадра только для чтения. bridge сколько раз мост пушил сигналы. sound ingests сколько приёмов звука в восприятие. tactic danger сколько пушей опасности от тактики. cover danger сколько пушей от cover подсказки.

Frame delta. Текущий Device.fTimeDelta в миллисекундах для ориентира по кадру.

Раздел 4. Вкладка Agents

При отсутствии стека. Текст что реестр агентов пуст пока стек не запущен.

Agent count. Число агентов в менеджере.

Таблица ix_ai_agents с колонками только для чтения. id числовой идентификатор объекта сталкера. alert строка уровня тревоги Vigilant Suspicious Search Combat None. suspicion число подозрения. profile вид поведения GuardBasic FlankerLite. sensory число слотов памяти в счётчике слотов агента. working число рабочих belief в модели памяти. focus yes или no есть ли последний фокус. tactical yes или no есть ли тактическая подсказка.

Раздел 5. Вкладка Perception

При отсутствии стека. Текст что глобальный буфер недоступен.

При живом стеке. Строка Buffered events с текущим числом событий, потолком из констант, временем удержания и размером ячейки xz сетки.

Таблица до 64 первых событий глобального буфера. Колонка t время метки события. type имя типа восприятия. I интенсивность. R радиус. pos тройка координат позиции.

Подзаголовок Visual probe tuning applies when stack runs. Visual probe interval frames интервал в кадрах от 1 до 20. Visual probe intensity от 0.1 до 3. Visual probe radius от 0.5 до 15 метров. Max visual probes per frame от 1 до 128.

Подзаголовок IX vision LOS FOV plus RayPick. FOV fallback half-angle deg запасной половинный угол сектора зрения в градусах от 5 до 85. Ray range past target m дополнительная дальность луча за цель от 0 до 3. Occlusion depth epsilon m порог глубины окклюзии от 0 до 0.5. Aim chest height m высота точки прицела по груди от 0 до 1.8. Max distance vs eye_range масштаб максимальной дистанции относительно eye_range сталкера от 0.5 до 2.

Подзаголовок Corpse probe tuning VisualCorpse events near actor. Corpse probe interval frames от 1 до 60. Corpse probe radius от 5 до 120. Corpse event intensity от 0.05 до 2.

Подзаголовок Squad channel ally wound combat fan-out. Squad channel enabled включает канал отряда. Squad max distance ноль отключает ограничение по дистанции иначе метры до 400. Ally wound intensity от 0.1 до 4. Ally wound radius от 1 до 40. Combat engaged intensity от 0.1 до 4. Combat engaged radius от 1 до 60. Squad suspicion scale от 0 до 1.5. Squad focus intensity min от 0 до 2.

Раздел 6. Вкладка Stealth

Silenced shot power cutoff от 0.01 до 1 порог мощности для классификации глушителя.

Suspicion to Suspicious порог перевода в подозрительную ступень от 0.01 до 2.

Suspicion to Search порог перевода в поиск от 0.1 до 6.

Suspicion to Combat порог перевода в бой от 0.5 до 12.

Global suspicion decay scale множитель глобального затухания подозрения от 0.1 до 4.

Подзаголовок Memory per agent. Memory decay per sec затухание памяти в секунду от 0.1 до 4. Memory suspicion leak scale утечка подозрения из памяти от 0 до 0.5. Memory sample weight scale вес выборки памяти от 0.05 до 1. Memory strength epsilon порог силы слота от 0.001 до 0.1.

Раздел 7. Вкладка Tactics

Guard hold distance дистанция удержания охраны от 1 до 12 метров.

Flank side scale масштаб бокового фланга от 0.05 до 1.

Подзаголовок Experimental movement bias danger at tactical hint. Feed tactical hint as EnemySound danger подаёт тактическую подсказку как звук врага в legacy опасность. Tactic hint danger cooldown ms кулдаун от 200 до 5000 миллисекунд.

Подзаголовок Experimental cover bias lateral danger nudge. Feed cover-side danger Search Combat боковой пуш опасности для укрытия на ступенях Search и Combat. Cover hint interval frames интервал кадров от 1 до 60. Cover hint danger cooldown ms кулдаун от 200 до 6000.

Раздел 8. Вкладка Bridge

Кнопка Reload misc ix_ai_stack.ltx defaults plus file вызывает IxAiStackApi ReloadRuntimeConfig и перечитывает файл конфигурации поверх дефолтов.

Enable legacy bridge danger enemy nudges включает мост в память сталкера для опасности и лёгких толчков врага.

IX memory authoritative Suspicious Search danger focus from slots включает режим когда сильнейший слот IX памяти задаёт фокус опасности в legacy на ступенях Suspicious и Search.

Suspicious danger cooldown s интервал между пушами подозрительной опасности от 0.5 до 12 секунд.

Search danger cooldown s интервал для ступени поиска от 0.5 до 12 секунд.

Combat push cooldown s интервал для боевого пуша от 0.2 до 8 секунд.

Блок текста что мост кормит CDangerObject make_object_visible_somewhen set_enemy и рекомендуется держать выключенным при настройке и включать для экспериментов.

Раздел 9. Вкладка Debug draw

Draw perception spheres рисует точки глобальных событий восприятия в мире.

Draw tactical hints agent to hint рисует линию от позиции агента к позиции тактической подсказки.

Оба оверлея требуют живой стек и загруженный уровень с bReady. Фактическая отрисовка через DBG доступна только в сборке DEBUG без MASTER_GOLD иначе показывается пояснение что нужна такая сборка.

Раздел 10. Что в панели не выводится

Параметры локальности locality из g_ixAiRuntimeTuning и секции ix_ai_locality в misc ix_ai_stack.ltx в этом окне не редактируются, только через конфиг и перезагрузку. Остальные поля структуры тюнинга без соответствующего виджета в ImGui здесь не перечислены как элементы панели.
