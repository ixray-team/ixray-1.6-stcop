# Текущий статус Tiramisu Render

> Актуальность: 20 августа 2026 года. Recovery baseline пересозданной ветки
> `dev/tiramisu` повторно пройден на свежих normal и ASan build-каталогах.

## Профилирование полного Zaton

Для производительности добавлен отдельный скрытый сценарий
`-legacy-zaton-runtime-profile`. Он обязательно запускается с
`-rdbg -render-deterministic -editor-test-hidden`, загружает
`rawdata/levels/!FinalSP/zaton.level`, прогревает 60 кадров и сохраняет
60-кадровый Optick capture. Vulkan проверяется первым, затем D3D12.

Профилировщик подтвердил несколько независимых CPU-проблем переходного editor
bridge:

- полное хеширование всех vertices/indices каждой legacy-сетки на каждом
  redraw;
- повторный material resolve для всех 568 видимых material slots до проверки
  неизменного source revision;
- отсутствие frustum culling для legacy static meshes и вспомогательных
  объектов;
- повторная генерация и копирование примерно 27 тысяч AI-map triangles при
  неизменных карте и камере;
- отдельные vertex/index buffers и прямой draw для каждой секции legacy-сцены;
- повторная сортировка неизменного scene layout и слишком узкая очередь
  подготовки material slots, хотя фактическая DXC-компиляция уже дедуплицирована.

Исправлены generation revision сеток, ранний material source cache, frustum
culling, revisioned AI-map cache и delta-передача debug draw через mailbox.
Статическая и skeletal geometry теперь собирается в общий GPU arena, scene layout
кешируется, а одинаковые mesh sections объединяются в instances. Видимый кадр
Zaton содержит 3296 логических draws, которые сведены в 900 indexed indirect
команд и две API-группы. Vulkan использует нативную 20-байтную команду, D3D12 —
28-байтный NRI emulation layout с продублированными base vertex/base instance;
оба ABI покрыты CPU-тестом и реальным GPU smoke.

До исправлений автоматический Vulkan-замер показывал steady p50
`1748,65 ms`. После исправлений normal Debug с NRI validation и точным
`-rdbg` показывает `31,26 ms` на Vulkan и `30,81 ms` на D3D12, то есть примерно
`32,0` и `32,5 FPS`. Это 56–57-кратное сокращение CPU frame time относительно
исходного steady-state. Предыдущий запуск на тех же исходниках давал
`28,31/27,11 ms`, поэтому Debug-цифры используются как regression checkpoint,
а не как production benchmark. Финальная ASan-проверка дала соответственно
`94,20 ms` и `88,94 ms`; sanitizer и NRI/API validation ошибок нет.

Число `89836 ms` на пользовательском снимке относилось к кадру, внутри
которого синхронно завершалась старая 88,7-секундная загрузка уровня, но
steady-state просадка также была реальной и устранена отдельно. Текущие
примерно 32 FPS в Debug `-rdbg` ещё не являются production-целью. Общий geometry arena
и indirect submission уже работают, но GPU Hi-Z/async compute occlusion,
production allocator без переходного дублирования mesh buffers, GPU timestamps
и основной игровой scene submission остаются задачами следующих этапов.

Captures сохраняются раздельно:

- `build/test-results/tiramisu/profiles/zaton-runtime-vulkan.opt`;
- `build/test-results/tiramisu/profiles/zaton-runtime-d3d12.opt`;
- `build/test-results/tiramisu/profiles/zaton-runtime-vulkan-asan.opt`;
- `build/test-results/tiramisu/profiles/zaton-runtime-d3d12-asan.opt`.

## Восстановление после пересоздания ветки

Текущий `HEAD` содержит renderer/editor checkpoint, исправление LevelEditor, build fix
и последующий рефакторинг. Модули `xrRenderTiramisu`,
`xrTiramisuMaterialCore`, `xrTiramisuSceneCore`,
`xrTiramisuMaterialEditorCore` и editor integration восстановлены и проверены
не только по истории, но и свежей полной сборкой, CPU-тестами и GPU smoke.

Сравнение верхнего коммита `bada9b6537` с `d77b8a710c` показывает
330 изменённых файлов. После `--ignore-all-space` изменёнными остаются 278 файлов,
поэтому диапазон не является только массовым форматированием. Recovery audit
подтвердил наличие содержательных изменений renderer, editor, material, scene, core
и build systems; утерянной целиком editor integration не обнаружено.

Итог recovery baseline:

- 🟢 LevelEditor и Material Editor — свежие normal/ASan сборки и совместный GPU smoke подтверждены;
- 🟢 Vulkan/D3D12 GPU smoke — пройден на обоих backend;
- 🟢 ASan и RenderDoc — пройдены в матрице normal/ASan × Vulkan/D3D12;
- 🟢 CPU/compiler/cooker tests — 48/48 в normal и 48/48 в ASan, все команды используют точный `-rdbg`;
- 🟢 R4 — должен оставаться рабочим игровым fallback и не входит в editor recovery rewrite.

### Обязательный recovery checklist

1. [x] Проверить активную ветку, историю и наличие основных Tiramisu/editor модулей.
2. [x] Сравнить текущий `HEAD` с последним рабочим checkpoint и классифицировать изменения.
3. [x] Сконфигурировать свежие normal/ASan editor builds актуальным CMake.
4. [x] Собрать `ALL_BUILD` normal и ASan параллельно, без `/MP1`.
5. [x] Проверить CTest-команды на точный `-rdbg`, затем пройти весь набор normal и ASan.
6. [x] Запустить LevelEditor с `-rdbg` на Vulkan, затем D3D12; проверить viewport, Material Editor и shutdown.
7. [x] Повторить GPU smoke под ASan и с RenderDoc через `-renderdoc -renderdoc-capture -rdbg`.
8. [x] Проверить Zaton: sibling `.part`, auto-conversion, JSON/BIN, MaterialInstance, lights и dumps.
9. [x] Записать свежие результаты в документацию перед продолжением feature roadmap.

Исторический checkpoint ниже сохранён для сравнения. Результаты recovery от
19 августа 2026 года имеют приоритет над старыми числами.

## Последний выполненный пакет: OGF/OMF animation и GPU skinning

- [x] Введён `FEditorModelInstance`: LevelEditor передаёт только asset name,
  transform, object ID и selection flags.
- [x] Standalone OGF parser и cache находятся в `xrRenderTiramisu`, а не в
  LevelEditor; GPU buffers создаются renderer-side.
- [x] Поддержаны static, progressive, embedded hierarchy и skeletal draw-parts
  с 1–4 weights.
- [x] Skeletal loader сохраняет bone indices/weights для каждой вершины,
  проверяет их диапазоны и сумму; `startup_animation` копируется через
  renderer-neutral model instance и owned mailbox без `IKinematics` в editor.
- [x] Loader читает bone hierarchy и bind transforms, вычисляет inverse bind
  и строит renderer-owned skinning palette из local pose. Synthetic test
  проверяет двухкостную hierarchy, identity bind pose, animated child offset,
  неверный размер pose и out-of-range bone index; real OGF проверяет hierarchy.
- [x] После skeletal-palette slice повторно пройдены 47/47 normal и ASan
  CTest, а также normal/ASan × Vulkan/D3D12 GPU и RenderDoc smoke с
  `models=1/1/0/1`. Для реально загруженного RenderDoc deterministic smoke
  использует 120-секундный deadline вместо обычных 60 секунд, потому что
  synchronous D3D12/ASan capture может задержать particle readiness.
- [x] После добавления animation contract полный CTest прошёл 47/47 в normal
  и ASan. GPU smoke и RenderDoc 1.45/API 1.7 capture повторены в матрице
  normal/ASan × Vulkan/D3D12 с `models=1/1/0/1`, exit 0 и без
  NRI/API validation либо sanitizer errors.
- [x] Добавлены synthetic negative/positive tests, real fixture
  `dynamics/scene_objects/part/part_none.ogf` и mailbox lifetime/validation
  tests; targeted normal/ASan CTest прошёл 2/2.
- [x] LevelEditor normal/ASan собран; Vulkan и D3D12 smoke с точным `-rdbg`
  прошёл в обеих конфигурациях: `models=1/1`, `draws=7`, `gpu-draws=187`, без
  NRI и sanitizer errors.
- [x] RenderDoc 1.45/API 1.7 capture повторён в normal/ASan × Vulkan/D3D12;
  четыре `.rdc` читаются `renderdoccmd`.
- [x] Attached Spawn shape повторно использует общий renderer-neutral shape
  packet; idle particle/group передаётся через renderer-owned particle
  library/simulation. Tiramisu path больше не создаёт для них legacy
  `IRenderVisual`.
- [x] Normal/ASan × Vulkan/D3D12/RenderDoc smoke проверяет attached sphere
  (`396` triangles) и реальный idle `explosions\\campfire` (`1` instance,
  `18–20` simulated particles, `1` billboard draw), без validation/sanitizer
  errors.
- [x] OGF I/O/parse перенесён в bounded worker queue. Scene packet хранит
  pending count, повторно разрешается после publication, а GPU buffers
  создаются только на render thread; smoke требует `models=1/1/0`.
- [x] После OGF publication развёрнутый owned packet обновляет общий
  `TiramisuEditorViewportScenePicker`; real skeletal fixture проверяется лучом
  из triangle centroid/normal, а GPU smoke требует `ModelPickingReady` и
  публикует `models=1/1/0/1`.
- [x] OGF/OMF loader читает embedded motions, старые refs/REFS2 и внешние OMF,
  поддерживает compressed/uncompressed rotation/translation, constant tracks,
  loop и stop-at-end sampling.
- [x] Material GPU ABI v4 хранит current/previous palette offsets, bone count,
  inverse view-projection и данные projective decal draw;
  отдельный bindless palette buffer заполняется только render thread.
- [x] Общий `MaterialSkeletalVertexFactory` зарегистрирован в material manifest,
  имеет отдельный deterministic pipeline key и компилируется в DXIL/SPIR-V.
- [x] GPU smoke использует `stalker_bandit_1.ogf`, внешний
  `stalker_animation.omf` и `norm_walk_fwd_0`: 2 draw-part, 47 current и 47
  previous matrices, palette действительно меняется между кадрами.
- [x] Normal/ASan × Vulkan/D3D12 и RenderDoc 1.45/API 1.7 прошли с `-rdbg`;
  четыре animated-skinning `.rdc` читаются `renderdoccmd`.
- [x] Выбранный legacy `CSector` публикует world-space bounds через общий
  renderer-neutral debug-line packet; portal traversal и OCC остаются
  отдельными runtime-задачами.
- [x] Legacy `CPuddle` повторно использует общий shape packet. Временный
  selected puddle добавлен в GPU acceptance; normal/ASan × Vulkan/D3D12
  требуют `legacy-gizmos=2/420/408`.
- [x] ASan deterministic GPU smoke получил явный 120-секундный deadline;
  обычная сборка сохраняет 60 секунд. После холодной загрузки shader/OGF cache
  normal/ASan × Vulkan/D3D12 повторно прошли с exit 0 и точным `-rdbg`.
- [ ] Следующий editor slice: остальные специализированные object/gizmo packets,
  restart/device-loss и тройные immutable UI packets; остальные vertex
  factories остаются отдельной geometry-задачей. Standalone legacy
  ShaderEditor в Tiramisu composition root не включается.

> Рабочий checkpoint на 24 июля 2026 года. Это состояние стартового прототипа, а не отметка о готовности renderer к использованию по умолчанию.

## Коротко: последний рабочий checkpoint

Tiramisu по-прежнему включается только через `-r5`. R4 не изменяется и остаётся основным renderer/fallback.

`TiramisuRenderDeferredPass` сейчас **не является полноценным deferred renderer**. Он использует временный single-target geometry path, необходимый для вывода первой статической геометрии. Shader-side G-buffer ABI, GGX/Smith/Schlick direct lighting и tone mapping уже компилируются, но MRT resources, deferred/clustered GPU passes, shadows и render-graph wiring ещё не реализованы.

Целевые backend равноправны: Vulkan и D3D12. Материалы используют Descriptor Heap Indexing; draw record выбирается через `baseInstance`, а HLSL получает индекс через переносимый `NRI_INSTANCE_ID_OFFSET`.

## Состояние подсистем

| Подсистема | Статус | Что есть сейчас |
| --- | --- | --- |
| Документация и baseline | ✅ Этап 1 закрыт | Обзор, архитектура, материалы, [R4 feature matrix](./r4-feature-matrix.md), [representative scenes](./representative-scenes.md), roadmap, deterministic policy, диагностика и этот checkpoint |
| `xrTiramisuMaterialCore` | 🟡 Рабочая основа | Assets, master/instance inheritance, dynamic instances, generation-counted handles, legacy fallback, parameter layout |
| HLSL/graph compiler | 🟡 Рабочая основа | Typed graph, diagnostics, constant folding, DCE, Static Switch, ограниченный Custom HLSL, общий каталог из 21 node type, HLSL generation |
| Cooker и shader bundle | 🟡 Рабочая основа | Bundle v2, flattened instances, deterministic output, 216 DXIL/SPIR-V blobs для обоих backend/stage, runtime shader library и CPU pipeline cache |
| GPU material ABI | 🟡 Подключён к static/skeletal editor geometry, lights и decals | ABI v4: material/draw buffers, current/previous skinning offsets, inverse view-projection, bindless descriptor indices, 64-байтные light/palette records и общий C++/HLSL contract |
| Projective decals | 🟡 Editor path готов | Отдельные `Decal` pass/domain и `decal_projector`, box-volume rasterization, conservative renderer-side frustum culling, native RenderScene v3 component, bindless depth SRV, live legacy adapter и persistent Wallmark migration с audit dump готовы; production DBuffer/G-buffer, lifetime/occlusion и игровой scene wiring ещё отсутствуют |
| Material pass proxies | 🟡 Частично | Generation-counted NRI pipeline registry и `ResolvePass`; default material пока регистрирует старые prototype pipelines |
| Static/skeletal geometry | 🟡 Частично | Использует `FMeshBatch`, LOD/section/material-slot model и draw-record index через `baseInstance`; renderer-owned OGF/OMF path читает static/progressive/hierarchy, external motions и рисует animated 1–4-weight geometry через GPU palette. Visibility, LOD/SWI и остальные production vertex factories ещё прототипные |
| UI rendering | ⬜ Старый путь | Пока использует legacy texture-index/baseInstance path и не переведён на material ABI |
| `xrEUI`/`xrECore` | 🟡 Backend contracts готовы | ImGui frontend отвязан от встроенного DX9 backend, viewport capture/resize/surface и `IMaterialPreviewRenderer` renderer-neutral; legacy adapters сохранены на время миграции |
| LevelEditor | 🟡 Базовый native authoring работает | `-tiramisu-editor` устанавливает NRI ImGui/swapchain и `IEditorRenderBackend`; native `*.static-mesh.json`/`*.render-scene.json` открываются и передаются в NRI viewport. StaticMesh и Directional/Point/Spot Light создаются и редактируются через native Outliner/Details. Старые `.object`/`.level` автоматически конвертируются с MaterialInstance и audit dumps; Spawn visual передаётся как neutral OGF instance, а optional startup animation семплируется и деформируется в `xrRenderTiramisu`. Полный набор editor tools ещё не перенесён |
| Material Editor | 🟡 Рабочий asset/GPU preview slice | Master/instance open/save, parent chains, ImNodes canvas, typed properties/pins/links, undo/redo, copy/paste, autosave/recovery, diagnostics и generated HLSL/JSON; dependency watcher запускает безопасный live rebuild, а Tiramisu показывает sphere/cube/plane preview с настоящими Texture2D/TextureCube и environment lighting; production IBL и полный scene workflow ещё не завершены |
| Deferred PBR | 🟡 Shader/editor Forward foundation | Есть octahedral G-buffer pack/unpack и общий GGX/Smith/Schlick include. Editor Forward pass читает до 64 Directional/Point/Spot lights из bindless buffer; Vulkan/D3D12 GPU smoke проверяет два источника. MRT deferred passes, clustered lists, game-scene light upload и shadows отсутствуют |
| Renderer foundation | 🟡 Начат | Adapter/queue selection, `TRenderGraph`, NRI barrier translation и executor для привязанных resources с graphics/compute/copy submissions, timeline fences и тремя frame contexts готовы. RenderDoc загружается до NRI device через `-renderdoc`, F12 capture, передача exception handling в `xrDebug`/native debugger и безопасная `-rdbg` policy работают; transient allocation, main-path wiring и resize/device-loss ещё нужны |
| Диагностика renderer | ✅ Baseline | NRI validation управляется общей `-rdbg` policy; game/editor passes размечены GPU annotations. Общий versioned snapshot публикует CPU frame time, passes, draws, triangles/lines, uploads и tracked buffers/textures/pipelines/descriptors/bytes. GPU timestamp и driver VRAM пока явно помечены как не измеряемые |

## Что уже реализовано и проверено

- `xrTiramisuMaterialCore` отделён от NRI и используется runtime/compiler/cooker/tests.
- Общий `xrCore` RenderDoc bootstrap работает для игры и редакторов: DLL загружается до Vulkan/D3D12 device, capture сохраняется в `logs/renderdoc`, обязательный `-rdbg` сохраняет shader debug info без конфликтующих NRI/API validation layers, а внутренний RenderDoc crash handler отключается в пользу `xrDebug`/Rider.
- Master materials и instances читаются из versioned JSON; inheritance flattening и cycle/type validation покрыты тестами.
- Параметры scalar, vector, texture и sampler имеют deterministic layout и загружаются из `ByteAddressBuffer`.
- Общий HLSL ABI использует `ResourceDescriptorHeap` и `SamplerDescriptorHeap` для D3D12/Vulkan.
- Material GPU ABI v4 добавляет отдельные bindless `ByteAddressBuffer` света
  и skinning palettes: constants передают descriptor indices и inverse
  view-projection, draw record — current/previous palette offsets, bone count
  либо world-to-decal/depth descriptor для decal pass. Каждая light/matrix
  запись занимает 64 байта.
- Legacy static vertex/pixel shaders компилируются в DXIL и SPIR-V с новым draw/material ABI.
- Cooker создаёт deterministic bundle v2 с парными canonical vertex/pixel stages; incomplete shader set не выдаётся за production-ready.
- Shader library индексирует blobs по material, permutation, pass, vertex factory, render-pass signature и backend.
- CPU pipeline cache публикует только полный набор на границе кадра и сохраняет старый snapshot при ошибке reload.
- Renderer владеет material instance/parameter/draw upload buffers и NRI pipeline registry.
- Static geometry получает material pass proxy, но фактический pipeline пока остаётся prototype fallback.
- `xrECore` больше не требует прямого обращения `UIRenderForm` к legacy `GRHI` для capture/resize/surface.
- UI-independent `TiramisuMaterialEditorDocument` владеет целым master asset: metadata, runtime/static parameters, graph, typed node properties, undo/redo, compile, open/save.
- `TiramisuMaterialInstanceEditorDocument` редактирует parent и типизированные runtime/static overrides, запрещая менять domain/blend/shading model через instance.
- В LevelEditor добавлено окно `Windows → Material Editor`, построенное на ImNodes из `xrEUI` и UX-схеме ShaderEditor.
- Material Editor открывает и сохраняет настоящие `*.material.json` и `*.material-instance.json`, показывает master details и instance inspector.
- Compiler diagnostics могут выделить и центрировать конкретный ImNodes node.
- Новые GUID assets, nodes, pins и links имеют UUID-форму; factory pins получают deterministic GUID.
- JSON parsers переведены на non-throwing syntax parsing, чтобы invalid external JSON не завершал process при сборке nlohmann/json без exceptions.
- Material/instance/graph/legacy-map readers проверяют JSON field types и numeric ranges до чтения; wrong-type inputs покрыты regression-тестами.
- Master и instance assets сохраняются через temporary file в той же папке и atomic replace; ошибка не очищает dirty state и не оставляет temp-файл.
- Parameter metadata, default/min/max и свойства Constant, Parameter, Static Switch, Texture Sample и Custom HLSL редактируются через типизированный document API.
- Texture Sample может напрямую ссылаться на `Texture2D` parameter; подключённый texture pin имеет приоритет над этим свойством.
- Copy/paste сохраняет только выбранные nodes и внутренние links, создаёт новые GUID и оформляется одной undo-операцией.
- Autosave/recovery создаёт sidecar-файлы, не меняет source path и dirty state; мигрированные assets остаются dirty до сохранения в текущей версии schema.
- Material Editor показывает статистику static permutations с признаком lower bound/overflow.
- Preview показывает requested/accepted revision, фактический pipeline key, backend, pass и vertex factory; при сборке или ошибке явно указывает активную last-good revision.
- Dependency watcher отслеживает master/instance JSON, всю parent chain, HLSL template/implementation и asset dependencies по timestamp, размеру и content hash. Чистый документ обновляется автоматически, а локальные dirty-изменения нельзя затереть без явного выбора пользователя.
- Главный editor viewport использует отдельный объединённый dependency set для `legacy-map.json`, master/parent assets, HLSL template/implementation, vertex factory, Forward pass и lighting include. Изменение запускает background reload resolver и принудительную сборку затронутых scene permutations; неуспешный resolver/DXC результат не заменяет last-good pipelines.
- Добавлен renderer-neutral `IMaterialPreviewRenderer`: generation-counted handles, master/instance/HLSL source, sphere/cube/plane, environment, revision и состояния unavailable/compiling/ready/error.
- Tiramisu backend реализует `IMaterialPreviewRenderer`: асинхронно компилирует master/instance/HLSL, создаёт NRI pipeline и offscreen target, рисует sphere/cube/plane и публикует renderer-owned ImGui surface. При неудачном rebuild ранее рабочий pipeline остаётся активным.
- Material instance inspector разрешает цепочки parent instance → parent instance → master через общую `TiramisuMaterialLibrary`, проверяет циклы и передаёт в preview flattened runtime/static overrides.
- Preview-вкладка LevelEditor использует backend через renderer-neutral contract. Texture2D/TextureCube читаются из material asset references, декодируются через RedImage, загружаются в NRI resource heap и кэшируются по типу/пути; missing/invalid assets получают типобезопасный 2D/cube fallback и diagnostic.
- Studio/Neutral/Outdoor разрешаются в настоящие game TextureCube assets. Preview shader получает environment descriptor через per-draw record и использует cube для diffuse/specular environment lighting. Это рабочее editor IBL-приближение, но ещё не production prefiltered irradiance/specular pipeline.
- Главный NRI viewport разрешает legacy material slot через `TiramisuEditorViewportMaterialResolver`: `legacy-map.json` выбирает заранее созданный instance со static switches, runtime texture overrides flatten поверх parent chain, а общий material compiler собирает выбранный Forward pass для Vulkan или D3D12.
- Material scene pipeline учитывает two-sided и blend mode, использует GGX/Smith/Schlick lighting для bindless Directional/Point/Spot lights и сохраняет debug shader как безопасный fallback только на время первой сборки или при ошибке. При нулевом `LightCount` остаётся временный hardcoded sun; старый рабочий pipeline не заменяется неуспешной компиляцией.
- Editor scene ABI использует Descriptor Heap Indexing. `NRI_BASE_INSTANCE` передаёт абсолютный индекс `FMaterialDrawGpuData`; draw record ссылается на `FMaterialInstanceGpuData`, а instance — на упакованный parameter block с bindless texture/sampler indices.
- Исправлена граница matrix ABI, проявлявшаяся растянутой геометрией Zaton после vertex shader: X-Ray row-vector `LocalToWorld` транспонируется только перед записью в построчно загружаемый material draw `ByteAddressBuffer`. Cbuffer/root constants не транспонируются повторно. Regression-тест проверяет translation, совпадение CPU/HLSL transform и `clip.w = 1`. Полный legacy Zaton после исправления визуально проверен в LevelEditor: геометрия отображается корректно.
- Draw, instance, parameter data и viewport constants разделены по трём frame contexts. Pipeline publication и CPU→GPU upload выполняются только после fence повторно используемого context; preview и main scene используют непересекающиеся диапазоны.
- Tiramisu перечисляет все NRI adapters, отбрасывает устройства без выбранного Vulkan/D3D12 или graphics queue и детерминированно предпочитает discrete GPU; `SupportsRendering()` использует ту же проверку вместо безусловного `true`.
- Неизвестный `RedTexturePixelFormat` теперь возвращает `nri::Format::UNKNOWN`, а загрузка texture завершается типизированным отказом вместо undefined return.
- `FRenderMeshBath` заменён на `FMeshBatch`; static mesh render data разделён на LOD resources, sections и material slots.
- `TiramisuLegacyScene` сохранён как content adapter старых `.level`/OGF. Новый scene format остаётся целевой архитектурой и не зависит от legacy portal/visual layout.
- Добавлены legacy opaque/masked/emissive masters и заранее созданные instances `default/vertex/lmap/*_aref/selflight`. `legacy-map.json` выбирает parent instance со static switches, а loader создаёт поверх него кэшируемый `TMaterialInstanceDynamic`.
- Legacy dynamic cache учитывает parent instance, нормализованное shader name и весь texture set. Текущий временный GPU proxy передаёт только первую texture; lightmap и полный parameter block ещё не подключены к production pipeline.
- Начальный R5 shader foundation адаптирует проверенные идеи R4 без переноса legacy bindings: octahedral normals, PBR direct lighting, point attenuation и commerce tone mapping. Все resources выбираются через Descriptor Heap Indexing.
- NRI pipeline initialization legacy parent materials теперь ставится render command; game-thread loader не вызывает `CreateGraphicsPipeline`. В material proxies, pipeline registry, render passes, resource/scene proxies и GPU storage добавлены явные thread-affinity checks. Worker thread больше не считается game thread; thread IDs и флаг остановки render thread атомарны.
- Добавлен CPU-компилятор `TRenderGraph`: generation-counted handles, read/write dependencies, stable topological order, RAW/WAR/WAW hazards, resource barriers, compute/graphics queue transfers, transient lifetimes и compatibility-class aliasing.
- Граф формирует детерминированный submission plan с cross-queue waits. NRI translation централизованно создаёт texture/buffer barriers, texture ownership transfer и состояние `ARGUMENT_BUFFER/INDIRECT` для indirect draws.
- Render-graph тест моделирует будущую цепочку `DepthPrepass → BuildHiZ → Cull → DrawVisible → Present`, проверяет compute → graphics синхронизацию, read-only queue transfer, NRI state mapping, barrier bindings, aliasing, read-before-write, циклы и протухшие handles.
- Tiramisu обнаруживает отдельные NRI compute/copy queues; при их отсутствии будущие async passes должны использовать graphics fallback. При полностью равных адаптерах наличие compute/copy queues является дополнительным детерминированным критерием выбора.
- Включён игровой NRI ImGui draw path: ImGui frame формируется на game thread, а render thread выполняет `CmdCopyImguiData`/`CmdDrawImgui`. До тройных frame packets используется безопасная синхронная передача, не позволяющая начать следующий ImGui frame во время чтения NRI. Для `ImGui::Image` NRI принимает shader-resource `nri::Descriptor*`; editor adapter и render-graph transition preview texture ещё не подключены.
- Основной command path использует три frame contexts. Добавлена fence-aware deferred deletion queue со stable ordering и shutdown flush после `QueueWaitIdle`; release owned material pipelines уже переведён на неё. Остальные texture/buffer/descriptor destroy paths ещё требуют поэтапной миграции.
- `TRenderGraphNriExecutor` записывает скомпилированные passes в graphics/compute/copy command buffers, применяет NRI barriers, выполняет cross-queue timeline waits/signals и принимает external acquire/release synchronization. Автоматическое создание transient resources и подключение executor к основному frame loop ещё не выполнены.
- `xrEUI` использует сменный `IXrUIRendererBackend`; встроенный DX9 остался fallback, а frontend больше не вызывает `ImGui_ImplDX9_*` напрямую.
- Main/Top toolbars, Content Browser, asset chooser, thumbnail classes, icon picker, texture viewer, object/library previews, Detail Object Shuffle, Light Animation и Image Editor больше не создают `ref_texture`/`IRHISurface` через `EDevice->Resources` или `GRHI`. Именованные icons проходят через `TUI::LoadTexture`, изменяемые pixels — через `FEditorTextureHandle`; GPU creation/upload/deferred deletion выполняет выбранный backend. `TUI` больше не держит параллельные legacy/new texture caches, `SChooseTexture` содержит только generation-counted handle, а служебные Search/Null icons удалены из `CEditorRenderDevice`. По контрольному поиску `ref_texture|IRHISurface|GRHI->CreateTexture` прямые editor texture/surface обращения сокращены примерно со 117 до 4. Они находятся только в built-in legacy backend и старом `CRender::texture_load`; в LevelEditor и специализированных UI tools таких обращений больше нет.
- LevelEditor получает `TiramisuEditorRenderBridge` из `xrRenderTiramisu`: Vulkan/D3D12 swapchain, ImGui instance, три command contexts, resize/out-of-date handling и детерминированный frame scheduler используют единый renderer-owned NRI device и streamer.
- Для backend, владеющего главным swapchain, Tiramisu redraw ответвляется до `CEditorRenderDevice::Begin`, `RCache` и legacy `Scene->Render`. LevelEditor обновляет CPU camera, формирует renderer-neutral scene/debug packet и передаёт ImGui submit/present установленному backend из `xrRenderTiramisu`. Существующий legacy frame path остаётся только для старых editor backends.
- Добавлен renderer-neutral scene snapshot: camera, стабильные session IDs mesh/object, changed mesh uploads, material-slot sections, instances/transforms, Directional/Point/Spot lights и явное удаление mesh. Thread-safe mailbox копирует входные данные, объединяет одинаковые revisions и транзакционно отклоняет некорректные snapshots. Один legacy object может законно публиковать несколько mesh instances с одинаковым `ObjectId`; Light ID при этом обязан быть уникальным и не пересекаться с object ID. Переходный `EScene` bridge теперь также преобразует активное legacy-направление солнца и видимые `CLight` с `m_UseInD3D` в те же light records, включая color/brightness/range, spot cone, selection и cast-shadow flag. Legacy Terrain передаёт сгенерированный `CEditableObject` через общий static-mesh upload, material slots, object transform и selection flags; отдельного Terrain/NRI renderer в LevelEditor нет. Legacy Detail Objects сохраняют старую CPU-декомпрессию slot database и привязку к snap geometry, но не вызывают `hw_Render`: видимые экземпляры каждой detail-модели объединяются в один временный static-mesh batch и проходят через mailbox/material pipeline `xrRenderTiramisu`. `details\blend` явно разрешается в masked master. Это переходный editor path без wind, slot/base-texture overlay и нативного GPU instancing. Legacy `CEditShape`, `ESoundEnvironment` и Fog Volume emitter/occlusion sphere/box преобразуются в ограниченные renderer-neutral line/triangle lists с object transform и authoring colors. Fog Volume здесь является только authoring volume: физический volumetric fog pass не реализован. Выбранный `ESoundSource` публикует min/max distance spheres, невыбранный — компактный icon sphere. Legacy `CPortal` публикует замкнутый contour, двухсторонний полупрозрачный triangle fan и normal marker. Legacy `CGlow` публикует camera-facing mesh instance с `editor\glow_sprite`, texture material slot и selection box. Legacy `RCache`, `CTLSprite` и `ref_shader` для этих объектов в Tiramisu frame не вызываются.
- Legacy `CSpawnPoint` публикует fallback icon, selection bounds и overlay label для RPoint, EnvMod и entity records; выбранный spherical EnvMod дополнительно публикует radius sphere. Entity visual, attached shape и idle particle пока не подключены и остаются незакрытой частью Spawn packet.
- AI Map публикует plane-projected node triangles и уникальные link lines через renderer-neutral debug lists. Видимый набор берётся из spatial hash с `m_VisRadius` и жёстким лимитом; старые `m_RGeom`, `m_Shader` и `RCache.Vertex` в Tiramisu frame не используются.
- Legacy WayPoint path публикует point crosses, направленные/reciprocal links и выбранные labels через renderer-neutral packets. Двусторонняя link рисуется один раз, односторонняя сохраняется независимо от адресов объектов.
- Group path не рендерит дочерние объекты повторно: они уже находятся в class lists `EScene`. Bridge учитывает visibility всей owner chain и tool visibility, а selected Group добавляет только bounds. Smoke временно включает тестируемые tools и восстанавливает пользовательские visibility flags сразу после submit.
- Legacy LevelEditor `EScene/CSceneObject/CEditableMesh` преобразуется в этот snapshot без изменения формата старых сцен. Общие meshes дедуплицируются, legacy shader/texture дают стабильный material-slot ID, а выбранность и object transform сохраняются в instance.
- Добавлен независимый `xrTiramisuSceneCore` с versioned assets `StaticMesh` и
  `RenderScene`. StaticMesh v2 хранит параметры, sections и прямые material
  references в компактном JSON, а vertices/indices — в соседнем versioned BIN
  с magic/endian/offset/stride/count/size/hash validation. Inline JSON v1
  остаётся читаемым только для миграции. RenderScene v3 добавляет native
  projective Decal components к static meshes и Directional/Point/Spot lights;
  RenderScene v1 без lights и v2 без decals остаются читаемыми.
- Content Browser открывает native `*.static-mesh.json` и `*.render-scene.json`. Открытие старого `.object` создаёт native static mesh, открытие `.level` — native render scene и все требуемые static meshes. Исходные legacy-файлы не изменяются.
- Legacy surfaces преобразуются в дедуплицированные `MaterialInstance` через стабильную migration database. Отличающиеся surface-параметры конкретного `CSceneObject` сохраняются как per-component material overrides, а не размножают геометрию. В Tiramisu-режиме Properties старого Static Mesh больше не показывает редактируемые `Tex/Shader/Compile/Mtl`: группа `Materials` содержит разрешённый путь `Material Instance`, `Two Sided` и команду открытия instance в Material Editor. Viewport bridge пакетно создаёт/переиспользует эти assets, один раз атомарно публикует migration database и передаёт явный material override каждому mesh instance.
- Каждая попытка конвертации публикует детерминированный audit sidecar: успешный `<target>.migration.json`, неуспешный `<target>.migration.failed.json`. Dump v2 содержит version/importer, source hash, status, target GUID, metadata/payload paths, asset/material mappings, created/reused counts и diagnostics. Ошибка открытия или загрузки `.level` тоже создаёт failed dump.
- Полный `rawdata/levels/!FinalSP/zaton.level` повторно прошёл автоматическую конвертацию: 426 уникальных meshes, 5536 StaticMesh components и 753 light components. Импортер v3 переносит сохранённое направление legacy-солнца в Directional Light и 752 явных Point Light. В текущем workspace все 15 883 material bindings переиспользованы из stable migration database; быстрый clean-root smoke отдельно проверяет создание и последующее переиспользование MaterialInstance.
- Full-level acceptance повторно пройден во всех четырёх сочетаниях normal/ASan × Vulkan/D3D12 с обязательным `-rdbg` уже на Tiramisu-only startup без legacy D3D11/CRHI. Native `RenderScene`, пары StaticMesh JSON/BIN и migration dump повторно загружены после записи; diagnostics dump пуст, sanitizer и NRI/API validation errors отсутствуют. Результаты сохраняются в `build/test-results/tiramisu/zaton-<timestamp>/`.
- `xrRenderTiramisu` теперь владеет read-only particle catalog и читает как бинарный `particles.xr`, так и loose `.pe/.pg/.pac`, не создавая legacy shaders. Loader различает original и extended layout библиотеки; на текущем контенте найдено 1277 effect/group/curve assets. Для effect сохраняются compiled PAPI actions, max particle count, flags, sprite, frame, time limit, velocity scale и align-to-path rotation. Для group сохраняются flags, time limit, root effects, `time0/time1` и ссылки `on play/birth/death`. LevelEditor chooser, Content Browser и `EParticlesObject` используют копируемый renderer-neutral snapshot вместо `RImplementation.PSLibrary`; particle instances проходят в owned scene mailbox. Render thread создаёт независимые PAPI simulation states, планирует enabled root entries группы по `time0/time1` и deferred-stop flag, формирует sprite quads с UV/frame и vertex color, загружает texture через DescriptorHeapIndexing и рисует их общими additive/translucent material pipelines. Реализованы legacy group callbacks: `on birth` и `on death` создают свободные finite child effects, `on play` держит related child на индексе PAPI particle, корректно повторяет fast-erase при смерти и поддерживает rewind. Immediate/deferred stop распространяется на дочерние states. Frame animation поддерживает animated/random frame/random playback; billboard builder поддерживает velocity scale, align-to-path, world align и face align. `CViewportParticle` отправляет particle-only snapshot в отдельный renderer-owned viewport surface; отдельный GPU acceptance проверяет его размер, simulation и draw. Position markers остаются диагностикой. До полного паритета ещё нужны collision/culling/distortion/soft-particle варианты.
- Level importer использует batched migration transaction: тысячи component source/material updates накапливаются в памяти, а `legacy-object-migration.json` атомарно публикуется один раз после обхода сцены.
- Добавлен renderer-neutral CPU picker с persistent mesh cache: nearest triangle, transforms, material section, backface culling, max distance, mesh removal и instance-only update покрыты отдельным regression-тестом. Legacy backend сохраняет существующий точный CPU picking старого редактора.
- Выбранные instances получают отдельный NRI wireframe overlay с depth test без depth write. Совместный GPU smoke требует корректный pick результата и ровно три selection draws для opaque, translucent и additive material sections.
- Сохранённые points, lines, wire/solid faces и OBB из LevelEditor `m_DebugDraw` преобразуются в renderer-neutral revisioned line/triangle packet. Mailbox проверяет finite vertex data, NRI backend создаёт host-upload vertex buffer и рисует отдельные depth-tested line-list/triangle-list passes с alpha blending; replacement buffer освобождается через deferred deletion.
- На Tiramisu redraw `xrECore` открывает transient CPU capture без legacy GPU draw. Selection rectangle заранее добавляется как две alpha-blended screen-space triangles, editor grid копируется в depth-tested line packet, а сохранённые `m_DebugDraw` line/triangle primitives добавляются непосредственно при сборке snapshot. Capture закрывается при scene submission; для editor states без submission он явно отбрасывается, поэтому данные не переходят в следующий кадр. Есть отдельный лимит в 1 048 576 элементов каждого типа на redraw. Begin/end/discard lifecycle и владение временными строками проверяются `xrEditorRenderBackendTests`.
- NRI viewport создаёт `RGBA8` color и `D32` depth targets, загружает vertex/index buffers и рисует indexed instances с camera/object matrices. Для material slots асинхронно собирается настоящий Forward pass с общим material HLSL contract; он читает native lights из тройно буферизованного bindless light buffer. Debug shader остаётся только last-resort fallback. Заменённые buffers и pipelines освобождаются после timeline fence. Это первый editor material/light path, но ещё не G-buffer/deferred renderer.

## Отдельные задачи по редактору

1. [x] **Editor 1 — renderer-neutral `xrEUI`.** Сменный ImGui backend, корректный SDL platform init и отключение unsupported multi-viewports; есть отдельный contract test.
2. [x] **Editor 2 — NRI ImGui presenter.** Backend, swapchain, тройной frame scheduler, двухфазный present и startup по умолчанию подключены в LevelEditor (`-dx12` выбирает D3D12; прежний `-tiramisu-editor` допустим, но больше не обязателен). Runtime GPU acceptance вынесен в Editor 6.
3. [ ] 🟡 **Editor 3 — Tiramisu viewport и native scene workflow.** Главный viewport, renderer-owned editor textures, picking/selection/debug draw, Forward materials и native Light готовы. `xrTiramisuSceneCore`, native static-mesh/render-scene assets, Content Browser open и автоматический `.object`/`.level` importer с дампами подключены. Native lifecycle включает point/rectangle selection, Focus Selected/Zoom All, move/rotate/scale, StaticMesh drag-and-drop, создание Directional/Point/Spot Light, Cut/Copy/Paste с cross-scene GUID remap, delete/duplicate, invert selection, Hide Selected/Unselected/All, transaction undo/redo и atomic Save/Save As. Outliner объединяет StaticMesh и Light; Light Details редактирует type, transform, HDR color/intensity, range, spot cones, visibility и cast-shadows. Остались остальные editor object types/tools, а затем удаление переходной `EScene` модели. `TiramisuLegacyScene` сохраняется для старого игрового контента.
4. [x] **Editor 4 — базовый material GPU preview.** Tiramisu реализует sphere/cube/plane, offscreen render target, environment selection, асинхронную DXC/NRI pipeline сборку и безопасную ImGui presentation. Vulkan и D3D12 smoke пройдены с `-rdbg`.
5. [ ] 🟡 **Editor 5 — Material Editor authoring.** Node canvas, assets, parent instance chains, diagnostics, generated HLSL, фактический preview pipeline key/backend/pass/vertex-factory и GPU preview с настоящими Texture2D/TextureCube/environment lighting готовы. Остаются production IBL и статистика полного production permutation set.
6. [ ] 🟡 **Editor 6 — live workflow и тесты.** Dependency watcher preview и основной сцены, background compile, безопасная publication/last-good pipeline, autosave/migration integration, общий render-thread submit, normal/ASan CTest, resize/recreate и совместный preview+scene reload Vulkan/D3D12 GPU smoke готовы. Остались restart/device-loss, тройные immutable UI packets и automated flythrough.

NRI presenter является default для LevelEditor, но не меняет игровые renderer selection и остальные legacy editor executables. Editor images передаются через renderer-owned texture handles и mailbox; оставшиеся незарегистрированные legacy user-image команды безопасно заменяются white descriptor и не попадают в NRI как raw DX9 pointers. Submit, resize, создание и удаление editor GPU resources выполняются выделенным общим render thread `xrRenderTiramisu`; lifecycle status и resize smoke проверяют ненулевой ID этого потока. Временной остаётся синхронная передача живого `ImDrawData`, которую позже заменят тройные immutable UI packets.

Первый viewport Editor 3 уже не использует DX9 texture pointer: `TiramisuEditorRenderBridge` одновременно реализует `IEditorRenderBackend`, создаёт device-local color/depth targets, переводит color `COLOR_ATTACHMENT → SHADER_RESOURCE` и удаляет descriptor из registry до уничтожения GPU resource. Native document и переходная legacy-сцена поступают через один snapshot/mailbox; material slots разрешаются в instances, а общий compiler создаёт Forward permutation. Native Light records размещаются в отдельном тройно буферизованном диапазоне bindless buffer, поэтому Directional/Point/Spot lighting не требует менять pipeline при изменении runtime radiometry. CPU picker, wireframe selection, debug/overlay/text paths и transient editor meshes используют тот же renderer-neutral packet. Dependency-driven reload основной сцены сохраняет last-good pipeline и проверяется отдельным reload counter в GPU smoke. Resize atomically публикует новый renderer-owned surface: сохранённый текущим `ImDrawData` старый descriptor перенаправляется на новый до NRI draw, поэтому resize-кадр не превращается в white fallback. Native document выполняет общий selection/transform/visibility/Cut/Copy/Paste/duplicate/delete/undo/save lifecycle для StaticMesh и Light; cross-scene paste назначает новый GUID, сохраняет Light parameters и входит в одну undo-запись. StaticMesh дополнительно поддерживает material overrides и path rebasing. Focus получает renderer-neutral world bounds из mesh AABB и 0,5-unit bounds light icon. Native Object List не обращается к `CCustomObject`: он объединяет StaticMesh и Light, виртуализирует строки и выполняет atomic range selection/bulk visibility через document. Это всё ещё не готовый редактор: остальные object types/tools и restart/device-loss acceptance отсутствуют.

## Material Editor: что ещё требуется до отметки «готов»

Material Editor пока **не готов**. Готов рабочий asset-based slice, включая parent chains и GPU asset preview, но он ещё не закрывает полный authoring/preview workflow. Для завершения обязательны:

- production prefiltered environment/IBL и расширенная статистика реальных shader permutations;
- production-wide material hot reload и render-thread deferred destruction без `QueueWaitIdle` на обычном пути; focused Material Editor live preview уже работает;
- завершение Tiramisu viewport workflow LevelEditor: остальные object types/tools и restart/device-loss validation; resize/recreate, point/rectangle/invert selection, Focus Selected/Zoom All, Cut/Copy/Paste, duplicate/delete, visibility commands, single- и multi-component Details/material overrides, native Outliner с bulk visibility, пустая native scene, create/edit/save StaticMesh lifecycle, native asset open/legacy auto-conversion, renderer-neutral picking и debug/overlay paths уже готовы.

Фраза **«Material Editor готов»** допустима только после production IBL/permutation acceptance и завершения полного Tiramisu-only LevelEditor workflow с native scene authoring. Базовый цикл открыть asset → изменить `FMaterialGraph` → собрать HLSL → показать диагностику/preview → сохранить → повторно открыть уже работает.

## Проверки checkpoint

> Все тесты Tiramisu без исключений запускаются с `-rdbg`: CPU/CTest, compiler/cooker, LevelEditor, ShaderEditor, Vulkan/D3D12 GPU smoke и будущие automated flythrough. Результат без `-rdbg` не считается валидной проверкой и не закрывает задачу или acceptance criterion.

Автоматические LevelEditor smoke дополнительно запускаются через `Start-Process -WindowStyle Hidden` и получают `-editor-test-hidden`. В этом режиме splash отключён, test HWND создаётся `NOT_FOCUSABLE`, показывается без активации за пределами рабочего стола и не может забрать фокус. Полностью hidden HWND не используется: DXGI может перестать выдавать кадры. Parser запрещает `-editor-test-hidden` без валидного `-render-deterministic`.

Test-only положение `-32000/-32000`, размеры и maximized state не записываются
в пользовательский `level.json`. Обычный запуск дополнительно проверяет
сохранённую верхнюю область окна по рабочим областям всех SDL displays. Если
старый smoke уже оставил off-screen координаты, LevelEditor автоматически
центрирует окно на основном дисплее и исправляет placement при следующем
обычном сохранении.

После подключения Terrain это поведение повторно проверено foreground-PID probe во всех четырёх сочетаниях normal/ASan × Vulkan/D3D12: каждый процесс завершился с кодом `0`, ни один LevelEditor не стал foreground process, sanitizer и критические NRI/API diagnostics отсутствуют.

После подключения Detail Objects та же матрица повторена с реальным `rawdata/objects/detail/det_hvosh.object`: bridge сообщил `models=1, instances=1`, renderer выполнил один дополнительный GPU draw, четыре процесса завершились с кодом `0`, фокус не захватывался. `xrLegacyMaterialBridgeTests` проверяет masked mapping `details\blend`; полный normal CTest прошёл 48/48, targeted ASan test — 147 checks.

Fog Volume smoke создаёт emitter и occlusion и требует `legacy-gizmos=2/444/432`: по сравнению с предыдущим checkpoint добавляются ровно 24 линии и 24 треугольника двух box volumes. Normal/ASan × Vulkan/D3D12 завершились с кодом `0`, без захвата фокуса, sanitizer и критических NRI/API diagnostics.

Прямой `CTerrain` smoke строит сетку 2×2 через `GenerateMeshByHeightmap`, передаёт один mesh/selected instance и требует дополнительный Tiramisu GPU draw и selection overlay (`legacy-glow=5/2`). Тест выявил и исправил лишние normal/adjacency ref-counts: `CEditableMesh::Create` уже создаёт derived data, поэтому повторные `GenerateFNormals`/`GenerateVNormals`/`GenerateAdjacency` удалены. В Tiramisu mode heightmap generator также больше не вызывает legacy `CSurface::OnDeviceCreate`. Повторная normal/ASan × Vulkan/D3D12 матрица завершилась с `FATAL=0`, `Assert=0`, без sanitizer/NRI/API ошибок и захвата фокуса.

Authoring-флаг Details `Draw slot boxes` переведён на общий line debug packet: bridge повторяет 2×2-метровый slot bounds, quantized Y range, selected/inactive colors и ограничение 75 м. Synthetic selected slot добавляет ровно 12 линий и проверяется через `legacy-gizmos=2/456/432` в normal/ASan × Vulkan/D3D12. `Draw base texture` также перенесён: `CCustom2DProjector` публикует только CPU triangle/UV span и в Tiramisu mode не создаёт `ref_geom`, `ref_shader` или legacy shader resources. Bridge создаёт один textured static-mesh packet, а `xrRenderTiramisu` применяет per-draw clip-space depth bias через новый renderer-neutral flag. Smoke требует `textured=1, depth-bias=1`, шестой legacy draw, отдельный bindless texture descriptor и проходит normal/ASan × Vulkan/D3D12 без critical diagnostics.

Resize/recreate проверяется отдельным `-editor-resize-smoke`. После готовности scene он меняет размер главного SDL window и renderer-owned viewport `512×512 → 704×416`, затем требует увеличения swapchain/surface/resource revisions, нового successful present, перенаправления старого ImGui descriptor и сохранения material accepted revision/pipeline key. Normal и ASan на Vulkan и D3D12 завершились с кодом `0`; sanitizer/NRI/API errors и захват фокуса отсутствуют. Эта проверка закрывает resize, но не выдаётся за device-loss: restart и искусственное/реальное удаление device остаются отдельным acceptance gate.

После переключения LevelEditor на `xrRenderTiramisu` по умолчанию та же
normal/ASan × Vulkan/D3D12 матрица повторена без аргумента
`-tiramisu-editor`. Все четыре скрытых запуска с точными
`-rdbg -render-deterministic -editor-test-hidden` завершились с кодом `0`, в
логе выбрали Tiramisu presenter, опубликовали ненулевой render-thread ID и
прошли resize/material reload acceptance без sanitizer или validation errors.
Старый аргумент остаётся допустимым, но больше не определяет выбор renderer.

Аудит `EScene::CreateSceneTools` подтверждает renderer-neutral представление всех сохраняемых классов `OBJCLASS_GROUP..OBJCLASS_TERRAIN`. `ESceneDummyTool` имеет `OBJCLASS_DUMMY`, не создаёт объекты и не требует render packet. Это доказывает покрытие типов сцены, но не означает полный паритет каждого режима инструмента: у Details ещё отсутствуют wind/base overlay/native instancing, а Fog Volume пока показывает authoring shape без volumetric simulation.

19 августа 2026 года после пересоздания ветки выполнено повторно:

Свежие build-каталоги `intermediate/recovery-editors-normal` и
`intermediate/recovery-editors-asan` полностью собраны через Debug `ALL_BUILD`
обычной параллельной сборкой без `/MP1`. Для GPU smoke LevelEditor дополнительно
собран в `RelWithDebInfo`. Старые build artifacts не использовались как доказательство.

```text
ctest --test-dir intermediate/recovery-editors-normal -C Debug --output-on-failure
ctest --test-dir intermediate/recovery-editors-asan -C Debug --output-on-failure
100% tests passed, 0 tests failed out of 43
```

Все прямые CTest executables зарегистрированы с отдельным аргументом `-rdbg`. Два wrapper-теста cooker отклоняют любое значение кроме `TEST_DEBUG_FLAG=-rdbg` и передают флаг каждому вложенному process. Полные наборы прошли 48/48 в normal и 48/48 в ASan. CMake доставляет MSVC ASan runtime рядом с binaries; `detect_leaks=1` не используется, потому что MSVC AddressSanitizer не поддерживает LeakSanitizer.

Набор включает:

- material/compiler/cooker tests, включая 11 masters, 9 instances и
  deterministic bundle из 216 DXIL/SPIR-V blobs;
- `xrSceneAssetTests` — StaticMesh v2 JSON/BIN round-trip, legacy inline v1 read, missing/corrupt/hash-mismatch payload rejection, invalid topology, RenderScene v1 compatibility/v2 Light round-trip, invalid light type/range/color/cones, global GUID uniqueness и conversion dump v2 round-trip;
- `xrEditorNativeSceneDocumentTests` — selection, grouped transform transactions, undo/redo, atomic Save/Save As, StaticMesh lifecycle и native Light create/details/bounds/visibility/Cut/Copy/Paste/duplicate/remove/reload с cross-scene GUID remap и автоматическим upgrade RenderScene v1 → v2;
- `xrLegacyMaterialBridgeTests` и `xrTiramisuShaderFoundationTests`;
- `xrTiramisuAdapterSelectionTests`;
- `xrTiramisuStaticMeshTypesTests`;
- `xrTiramisuThreadAffinityTests` и `xrTiramisuRenderCommandQueueTests`;
- `xrTiramisuDeferredDeletionTests`;
- `xrTiramisuRenderGraphTests`;
- `xrTiramisuRenderGraphExecutorTests`;
- `xrEditorRenderBackendTests`;
- `xrMaterialPreviewRendererTests`;
- `xrUIRenderBackendTests`;
- `xrEditorNriFrameSchedulerTests`;
- `xrEditorNriStartupTests`;
- `xrEditorNriTextureRegistryTests`;
- `xrEditorViewportSceneMailboxTests` — deep-copy и validation mesh/material/instance/light/debug packet, max 64 lights, radiometry/range/cone/GUID checks и regression для одного multi-mesh legacy object с повторяющимся `ObjectId`;
- `xrEditorViewportScenePickerTests` — nearest transformed triangle, section/material, backface culling, max distance, instance-only update, mesh removal и invalid ray;
- `xrEditorTextureMailboxTests`;
- `xrEditorViewportSceneShaderTests` — scene/selection и debug line/triangle VS/PS компилируются в DXIL и SPIR-V;
- `xrEditorViewportMaterialResolverTests` — legacy shader/texture разрешаются в pre-authored instance со static switches и flatten runtime overrides;
- `xrMaterialEditorDocumentTests`;
- `xrMaterialInstanceEditorDocumentTests`;
- `xrMaterialPreviewCompilerTests`;
- `xrMaterialDependencyWatcherTests` — deduplication, same-size content change, creation, deletion и однократная доставка события.
- `xrLegacyObjectMaterialMigrationTests` — stable key/GUID, instance reuse, two-sided/static metadata и migration database.

Совместный Material Preview + editor scene reload GPU smoke пройден исключительно с `-rdbg -render-deterministic` в четырёх вариантах: normal/ASan × Vulkan/D3D12. Preview загружает `kung` как Texture2D 1024×1024 с 11 mip и `sky_10_cube#small` как TextureCube 32×32 через прямой descriptor heap; scene smoke создаёт synthetic indexed mesh с тремя material sections и non-identity translation в `LocalToWorld`, Directional и Point Light, renderer-owned effect и group instances из реального каталога, проверяет CPU pick смещённого instance (`distance=1.000`, object/material IDs), разрешает legacy `default`/`textures/kung`, `editor\spawn_icon`/default-white и particle materials, собирает отдельные opaque, translucent/unlit и additive/unlit Forward pipelines, запускает background resolver reload и ждёт `ReloadCount = 1/1/1/1`. Group для smoke выбирается только при наличии включённого child callback; acceptance ждёт не только `particle-children=1`, но и отдельный дочерний billboard draw. Дополнительный particle-only viewport проверяет тот же путь, который использует `CViewportParticle`: surface `384×384`, один effect instance, ненулевая simulation и один draw batch. Отдельный legacy bridge viewport создаёт временные selected Spot `CLight`, `CEditShape`, `ESoundSource`, `ESoundEnvironment`, `CPortal` и `CGlow`, добавляет legacy sun, синхронно копирует snapshot и проверяет `legacy-gizmos=2/229/212`, `legacy-glow=1/1`: два light records, один glow draw, один selection draw, 229 линий и 212 solid triangles. Временные объекты удаляются сразу после submit и не меняют пользовательскую сцену. Во всех четырёх вариантах нижняя точка acceptance — `draws=6`, `selection=3`, `debug-lines>=22`, `particle-instances=2`, `particle-groups=1`, `particle-children=1`, `simulated-particles>=5`, `particle-billboards>=5`, `particle-billboard-draws=3`, `particle-preview=384x384/>=2`, `legacy-gizmos=2/229/212`, `legacy-glow=1/1`, `overlay-text=1`, `lights=2`, renderer snapshot содержит `passes=5`, `gpu-draws>=186`, `triangles>=7604` и ненулевой tracked resource census; exit 0, нет `FATAL ERROR`, ASan, NRI/API validation errors. Более медленный ASan Vulkan может успеть симулировать больше частиц до опроса, но проходит те же нижние acceptance bounds. Значение `gpu-timing=not-collected` ожидаемо: timestamp queries ещё не реализованы. На D3D12 остаётся только NRI warning о неподдержанном optional `options22` (`E_INVALIDARG`), это не validation error. Скрытый Win32-запуск работает, однако это ещё не headless runner: content flythrough и image-diff capture runner остаются следующими задачами.

Актуальный Puddle checkpoint заменяет предыдущие значения legacy viewport: normal/ASan × Vulkan/D3D12 с `-rdbg` проверяют `legacy-gizmos=2/420/408`, `legacy-glow=2/1` и `legacy-labels=4`. К общим Shape/Sound/Portal/Spawn/AI Map/WayPoint/Group пакетам добавлены 12 линий и 12 треугольников временного selected `CPuddle`; все четыре запуска завершены с exit 0 без ASan и NRI/API validation errors.

Projective Decal checkpoint добавляет отдельные `EMaterialPass::Decal` и
`decal_projector`, RenderScene v3 `decal_components`, bindless D32 depth SRV и
ABI v4. Deterministic smoke содержит одну native decal и один настоящий legacy
`wm_slot`; второй преобразуется из positions/UV/width/height в projector volume,
а старые clipped triangles при Tiramisu backend не рисуются. Normal Vulkan и
D3D12 reload smoke прошли с точным `-rdbg`: native `instances=1/draws=1`,
legacy viewport `draws=3`, `DecalReady=1`, validation errors отсутствуют.
Первая Vulkan-проверка отдельно обнаружила обновление общего descriptor set
после bind; исправление публикует все depth descriptors до начала command
buffer и ждёт idle при создании/resize viewport. D3D12 затем обнаружил неверную
комбинацию depth access/layout при возврате из decal pass; исправленная barrier
использует attachment-write layout. Полный ASan CTest прошёл 48/48, а Vulkan и
D3D12 ASan smoke подтвердили `instances=1/draws=1` без sanitizer и NRI/API
validation errors. Normal RenderDoc 1.45/API 1.7 capture также повторён для
обоих API с `-rdbg`; в обоих захватах decal pass дошёл до успешного acceptance.
Для холодного D3D12 pipeline cache RenderDoc deadline увеличен до 240 секунд,
при этом обычный smoke сохраняет 60 секунд, а ASan — 120 секунд. Общий
renderer-neutral converter теперь используется и live bridge, и persistent
legacy level importer. Importer создаёт/переиспользует decal `MaterialInstance`,
пишет native `decal_components`, material mappings и диагностический итог в
`.migration.json`; повторный импорт требует стабильные component/material GUID.
Full-level Zaton acceptance с точным `-rdbg` пройден на Vulkan и D3D12: 426
mesh assets, 5536 StaticMesh components, 753 lights, 220 decals, 0 пропусков и
6509 компонентов суммарно. После этого fullscreen triangle на каждую декаль
заменён canonical box volume из 12 треугольников, генерируемых по `SV_VertexID`
без vertex buffer; front-face culling оставляет одну дальнюю границу convex
volume. Pixel pass восстанавливает surface normal из depth derivatives и
использует angle fade против растягивания на перпендикулярные поверхности;
`Modulate` смешивается к нейтральному белому. DXIL/SPIR-V compiler tests содержат
205 проверок, normal/ASan Vulkan и D3D12 smoke и RenderDoc API 1.7 capture прошли
без validation/sanitizer errors.
Renderer-side conservative frustum test проверяет восемь углов projector box,
строит compact visible-index list и отдельно публикует instance/draw/culled
counts. Deterministic smoke требует `instances=2/draws=1/culled=1`; normal и
ASan Vulkan/D3D12 прошли без validation/sanitizer errors. Во время проверки
также исправлены два startup edge case: симметричное закрытие ImGui child/menu
window при неуспешном `BeginMenuBar` и пропуск zero-size ImGui frame до NRI,
который запрещает Vulkan viewport нулевой ширины. Production DBuffer/G-buffer,
lifetime/occlusion и игровой scene wiring всё ещё нужны до полного закрытия
строки world parity.

Отдельный RenderDoc smoke также пройден в normal/ASan × Vulkan/D3D12 с `-rdbg -renderdoc -renderdoc-capture`. Установлен RenderDoc 1.45, in-application API сообщает 1.7.0. Во всех четырёх вариантах DLL загружена до NRI device, конфликтующие NRI/API validation layers подавлены, shader debug info сохранена, material preview, scene viewport, отдельный particle preview, дочерний particle draw и legacy Light/Shape/Sound/Puddle/Portal/Glow/Spawn/AI Map/WayPoint/Group viewport завершились с exit 0. Явный one-frame capture оборачивает renderer-owned внешний ImGui present и передаёт настоящий HWND, поэтому все четыре `.rdc` читаются `renderdoccmd thumb`; актуальные захваты и PNG thumbnails проверены в `build/test-results/tiramisu/renderdoc-legacy-puddle/`. D3D12 ожидаемо отключает NVAPI при активном RenderDoc. Интерактивный F12 остаётся доступен.

Legacy conversion GPU/editor smoke также пройден исключительно с `-rdbg` в четырёх вариантах normal/ASan × Vulkan/D3D12. Он проверяет failed dump для незагружаемого `.level`, двукратную конвертацию реального `.object` с тем же asset GUID и повторным использованием MaterialInstance, парные StaticMesh JSON/BIN, payload path в dump, конвертацию самодостаточного старого `.level`, `asset_mappings` и последующую загрузку native `RenderScene`. Отдельный full-level smoke конвертирует Zaton вместе со всеми `.part` в timestamp-каталог; полный D3D12-вариант также завершён под ASan. Неполный legacy level с отсутствующими library objects обязан завершаться ошибкой и dump, а не создавать видимость успешной пустой сцены.

## Ближайший порядок работы

1. Довести native LevelEditor workflow: добавить остальные object types/tools и постепенно убрать переходную `EScene`. Локальный legacy `RImplementation` runtime и D3D11 `CRHI` уже удалены из Tiramisu composition root; старые `.object`/`.level` остаются CPU-only import sources с обязательными dumps.
2. Довести уже подключённый renderer-neutral particle contract: catalog, chooser, Content Browser, legacy object validation, owned scene packets, PAPI simulation state, расписание root entries, group child callbacks, frame animation, velocity/path/world/face alignment, position markers, textured billboard draw и отдельный particle preview surface готовы; далее нужны collision/culling/distortion/soft-particle варианты. Legacy `CPSLibrary` GPU initialization в Tiramisu-only LevelEditor не возвращается.
3. Подключить готовое ядро NRI executor к основному frame loop и добавить физическое создание/aliasing transient resources и frame statistics.
4. Подключить G-buffer MRT/depth resources, deferred directional/point lighting и tone-map shaders через первый исполняемый граф.
5. Завершить аудит thread-affinity и перевести оставшиеся texture/buffer/descriptor destroy paths на fenced deferred deletion queue.
6. Выполнить restart/device-loss validation первого scene viewport на Vulkan и D3D12; resize/recreate уже закрыт normal/ASan GPU smoke. Затем добавить representative scene flythrough.
7. Расширить focused dependency watcher/live preview до production material pipeline set, добавить production IBL и статистику всех permutations.
8. Перевести UI/остальные domains на единый material/draw ABI и заменить JSON records в bundle v2 на binary flattened records cooked runtime.

## Состояние Git для продолжения

- Активная ветка: `dev/tiramisu`.
- Ветка была пересоздана; старые build artifacts не считаются подтверждением текущего состояния.
- В истории присутствуют checkpoint `5cbb22b024`, исправление редактора `85aa66bed2`, build fix `f32719e771` и последующий рефакторинг `d77b8a710c`.
- Recovery audit текущего `HEAD` завершён; feature work можно продолжать, не используя старые build artifacts как доказательство.
- В рабочем дереве находится большой объём untracked content/build/generated data; recovery не должен удалять его через `clean`, `reset` или массовое перемещение.

## Решение по совместимости редакторов

Игровой R4 остаётся доступным и не изменяется. LevelEditor с `-tiramisu-editor` уже использует Tiramisu-only GPU composition root без legacy D3D11 device. Standalone legacy ShaderEditor к нему не подключается и не является целью этой миграции; новый Material Editor находится внутри LevelEditor и использует старое окно только как UX-референс. Это не отменяет совместимость контента: `TiramisuLegacyScene` остаётся в Tiramisu как адаптер старых локаций `.level`/OGF параллельно новому scene format.

## Checkpoint рефакторинга 24 июля 2026

### Выполнено

- [x] `xrMaterialCore` переименован в `xrTiramisuMaterialCore`; публичный `Tiramisu::Material` удалён, material API и owned data переведены на engine-типы.
- [x] `xrSceneCore` переименован в `xrTiramisuSceneCore`, включая каталог, CMake target, зависимости и документацию.
- [x] `xrMaterialEditorCore` переименован в `xrTiramisuMaterialEditorCore`; исходники перенесены в `src/Editors/TiramisuMaterialEditor`.
- [x] `xrTiramisuSceneCore` и `xrTiramisuMaterialEditorCore` переведены на `xr_string`, `xr_vector`, `xr_hash_map`, `xr_span`, `xr_optional`, `u32`, `u64` и остальные аналоги движка. `std::filesystem`, iostream и JSON используют явные преобразования только на внешней границе.
- [x] После удаления `Tiramisu::Material` concrete-классы material, scene и editor подсистем получили полный префикс `Tiramisu`: в том числе `TiramisuMaterialLibrary`, `TiramisuMaterialEditorDocument`, `TiramisuEditorViewportSceneMailbox` и `TiramisuEditorNativeSceneDocument`. `F` сохранён только у структур данных, `I` — у интерфейсов, `T` — у настоящих шаблонов.
- [x] Concrete-классы Tiramisu renderer переименованы из сокращённых `T...` в полные `Tiramisu...` имена. Настоящие шаблоны `THandle`, `TEnqueueRenderCommand` и `TEditorBoundedAsyncQueue` сохранены.
- [x] Renderer-neutral Editor API, Tiramisu viewport, native scene document, legacy importers и Material Editor UI согласованы с engine-типами. `xr_hash_map` теперь поддерживает custom hasher тем же третьим template-параметром, что и `std::unordered_map`.
- [x] Удалён неиспользуемый дублирующий `IMaterialPreviewRenderer` из material runtime; редактор использует единый узкий интерфейс из renderer-neutral Editor API.
- [x] Русские контрактные комментарии расставлены в новых модулях `xrTiramisuMaterialCore`, `xrTiramisuSceneCore`, `xrTiramisuMaterialEditorCore` и `xrRenderTiramisu`.
- [x] Комментариями покрыты public types, material/scene compiler API, render graph/NRI translation, thread ownership, GPU lifetime, editor bridge/mailboxes и новые legacy adapters. Raw-string HLSL и сторонние headers не переписывались.
- [x] В code style закреплено: комментировать контракт, поток, владение, ABI и причину решения; не дублировать очевидные имена getters/setters.
- [x] Полный `LevelEditor` собирается в Debug через `intermediate/codex-tiramisu-editor-cmake` обычной параллельной сборкой, без `/MP1`.
- [x] После полного переименования 42 material/scene/Tiramisu/editor теста прошли через CTest. Все 42 команды явно запущены с обязательным аргументом `-rdbg`.
- [x] Ранее подтверждены 12 material tests и 7 renderer tests, также исключительно с `-rdbg`.

### Следующие задачи рефакторинга

- [x] Остаточные concrete `TEditor...` файлы переименованы в `TiramisuEditor...`; единственный сохранённый `TEditorBoundedAsyncQueue` является настоящим шаблоном.
- [ ] Продолжить аудит нового и изменённого Tiramisu-кода: naming классов/структур проверен; остаются минимизация namespaces, engine-типы и осмысленные переносы без искусственного ограничения длины строки.
- [ ] Обновить остальные страницы документации после окончательной фиксации имён и выполнить `npm run docs:build`.
- [x] Выполнен Debug `ALL_BUILD` обычной параллельной сборкой без `/MP1`; затем 48/48 material/scene/Tiramisu/editor тестов прошли исключительно с `-rdbg`.
- [x] Выполнить отдельную ASan-конфигурацию и ASan-тесты с `-rdbg`.
- [x] После build/test regression выполнить runtime smoke Vulkan/D3D12 и RenderDoc normal/ASan с обязательным `-rdbg`.

### Последние подтверждённые проверки

- `xrTiramisuMaterialCore`, `xrMaterialCooker`, `xrRenderTiramisu`, `xrTiramisuSceneCore`, `xrTiramisuMaterialEditorCore`: Debug build — успешно.
- Debug `ALL_BUILD`, включая `xrEngine`, R4, Tiramisu и все editor targets: успешно без `/MP1`.
- `LevelEditor`: Debug build — успешно.
- Material test suite: 12/12 — успешно с `-rdbg`.
- Renderer test suite: 7/7 — успешно с `-rdbg`.
- Scene/Material Editor core suite: 7/7 — успешно с `-rdbg`.
- Renderer-neutral editor/Tiramisu suite: 13/13 — успешно с `-rdbg`.
- Совокупный material/scene/Tiramisu/editor CTest block: 48/48 normal и 48/48 ASan — успешно с `-rdbg`.
- Git merge не находится в незавершённом состоянии. После успешных build/test проверок владелец запросил отдельный WIP checkpoint-коммит с пометкой `needs refactor`.
## Исправление границы LevelEditor и renderer — 24 июля 2026

- [x] GPU backend, frame scheduler, texture/scene mailboxes, picker и editor
  shader support физически перенесены из `LevelEditor/Renderer` в
  `src/Layers/xrRenderTiramisu/Editor`.
- [x] LevelEditor больше не создаёт `nri::Device`, graphics queue или streamer
  и не линкует `NRI.lib` напрямую.
- [x] `TiramisuEditorRenderBridge` использует единый `TiramisuRenderDevice`
  основного renderer module и отдаёт редактору только renderer-neutral API.
- [x] Минимальный `IXrUIRendererBackend` вынесен из внутреннего `xrEUI` header
  в публичный renderer contract; обратная зависимость `xrRenderTiramisu` от
  `xrEUI` удалена.
- [x] Выполнена полная Debug `ALL_BUILD` без `/MP1`.
- [x] Повторно пройдены 47/47 CTest-тестов, каждый с обязательным `-rdbg`.
- [ ] Перевести специализированные editor passes на общий
  `TiramisuRenderGraph` при сохранении текущего публичного API.
