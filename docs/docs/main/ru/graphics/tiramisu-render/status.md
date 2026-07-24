# Текущий статус Tiramisu Render

> Рабочий checkpoint на 24 июля 2026 года. Это состояние стартового прототипа, а не отметка о готовности renderer к использованию по умолчанию.

## Коротко

Tiramisu по-прежнему включается только через `-r5`. R4 не изменяется и остаётся основным renderer/fallback.

`TiramisuRenderDeferredPass` сейчас **не является полноценным deferred renderer**. Он использует временный single-target geometry path, необходимый для вывода первой статической геометрии. Shader-side G-buffer ABI, GGX/Smith/Schlick direct lighting и tone mapping уже компилируются, но MRT resources, deferred/clustered GPU passes, shadows и render-graph wiring ещё не реализованы.

Целевые backend равноправны: Vulkan и D3D12. Материалы используют Descriptor Heap Indexing; draw record выбирается через `baseInstance`, а HLSL получает индекс через переносимый `NRI_INSTANCE_ID_OFFSET`.

## Состояние подсистем

| Подсистема | Статус | Что есть сейчас |
| --- | --- | --- |
| Документация и baseline | ✅ Этап 1 закрыт | Обзор, архитектура, материалы, [R4 feature matrix](./r4-feature-matrix.md), [representative scenes](./representative-scenes.md), roadmap, deterministic policy, диагностика и этот checkpoint |
| `xrTiramisuMaterialCore` | 🟡 Рабочая основа | Assets, master/instance inheritance, dynamic instances, generation-counted handles, legacy fallback, parameter layout |
| HLSL/graph compiler | 🟡 Рабочая основа | Typed graph, diagnostics, constant folding, DCE, Static Switch, ограниченный Custom HLSL, общий каталог из 21 node type, HLSL generation |
| Cooker и shader bundle | 🟡 Рабочая основа | Bundle v2, flattened instances, deterministic output, 200 DXIL/SPIR-V blobs для обоих backend/stage, runtime shader library и CPU pipeline cache |
| GPU material ABI | 🟡 Подключён к статике и editor lights | ABI v2: material/draw buffers, bindless descriptor indices, `FMaterialInstanceGpuData`, `FMaterialDrawGpuData`, 64-байтный `FMaterialLightGpuData` и общий C++/HLSL contract |
| Material pass proxies | 🟡 Частично | Generation-counted NRI pipeline registry и `ResolvePass`; default material пока регистрирует старые prototype pipelines |
| Static geometry | 🟡 Частично | Использует `FMeshBatch`, LOD/section/material-slot model и draw-record index через `baseInstance`; visibility и production pipeline paths ещё прототипные |
| UI rendering | ⬜ Старый путь | Пока использует legacy texture-index/baseInstance path и не переведён на material ABI |
| `xrEUI`/`xrECore` | 🟡 Backend contracts готовы | ImGui frontend отвязан от встроенного DX9 backend, viewport capture/resize/surface и `IMaterialPreviewRenderer` renderer-neutral; legacy adapters сохранены на время миграции |
| LevelEditor | 🟡 Базовый native authoring работает | `-tiramisu-editor` устанавливает NRI ImGui/swapchain и `IEditorRenderBackend`; native `*.static-mesh.json`/`*.render-scene.json` открываются и передаются в NRI viewport. StaticMesh и Directional/Point/Spot Light создаются и редактируются через native Outliner/Details с selection, transforms, visibility, duplicate/delete, undo/redo, dirty prompt и атомарными Save/Save As. Старые `.object`/`.level` автоматически конвертируются с MaterialInstance и audit dumps. Полный набор editor tools ещё не перенесён |
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
- Material GPU ABI v2 добавляет отдельный bindless `ByteAddressBuffer` света: constants передают только descriptor index, frame-relative offset и count, а каждый 64-байтный record хранит position/range, direction/type, HDR color/intensity, spot cone и flags.
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
- LevelEditor получает `TiramisuEditorRenderBridge` из `xrRenderTiramisu`: Vulkan/D3D12 swapchain, ImGui instance, три command contexts, resize/out-of-date handling и детерминированный frame scheduler используют единый renderer-owned NRI device и streamer.
- Для backend, владеющего главным swapchain, `ImGui::Render()` отделён от GPU submit/present: legacy scene завершается без второго present, затем вызывается внешний presenter. Существующий DX9 путь сохраняет прежний порядок.
- Добавлен renderer-neutral scene snapshot: camera, стабильные session IDs mesh/object, changed mesh uploads, material-slot sections, instances/transforms, Directional/Point/Spot lights и явное удаление mesh. Thread-safe mailbox копирует входные данные, объединяет одинаковые revisions и транзакционно отклоняет некорректные snapshots. Один legacy object может законно публиковать несколько mesh instances с одинаковым `ObjectId`; Light ID при этом обязан быть уникальным и не пересекаться с object ID.
- Legacy LevelEditor `EScene/CSceneObject/CEditableMesh` преобразуется в этот snapshot без изменения формата старых сцен. Общие meshes дедуплицируются, legacy shader/texture дают стабильный material-slot ID, а выбранность и object transform сохраняются в instance.
- Добавлен независимый `xrTiramisuSceneCore` с versioned assets `StaticMesh` и `RenderScene`. StaticMesh v2 хранит параметры, sections и прямые material references в компактном JSON, а vertices/indices — в соседнем versioned BIN с magic/endian/offset/stride/count/size/hash validation. Inline JSON v1 остаётся читаемым только для миграции. RenderScene v2 хранит static-mesh components и native Directional/Point/Spot lights со stable GUID, transform, HDR color/intensity, range, spot cones, visibility и cast-shadows metadata; RenderScene v1 без lights остаётся читаемым.
- Content Browser открывает native `*.static-mesh.json` и `*.render-scene.json`. Открытие старого `.object` создаёт native static mesh, открытие `.level` — native render scene и все требуемые static meshes. Исходные legacy-файлы не изменяются.
- Legacy surfaces преобразуются в дедуплицированные `MaterialInstance` через стабильную migration database. Отличающиеся surface-параметры конкретного `CSceneObject` сохраняются как per-component material overrides, а не размножают геометрию. В Tiramisu-режиме Properties старого Static Mesh больше не показывает редактируемые `Tex/Shader/Compile/Mtl`: группа `Materials` содержит разрешённый путь `Material Instance`, `Two Sided` и команду открытия instance в Material Editor. Viewport bridge пакетно создаёт/переиспользует эти assets, один раз атомарно публикует migration database и передаёт явный material override каждому mesh instance.
- Каждая попытка конвертации публикует детерминированный audit sidecar: успешный `<target>.migration.json`, неуспешный `<target>.migration.failed.json`. Dump v2 содержит version/importer, source hash, status, target GUID, metadata/payload paths, asset/material mappings, created/reused counts и diagnostics. Ошибка открытия или загрузки `.level` тоже создаёт failed dump.
- Полный `rawdata/levels/!FinalSP/zaton.level` прошёл автоматическую конвертацию: 426 уникальных meshes, 5536 components, 396 созданных и 15487 переиспользованных material bindings. `zaton_terrain` metadata JSON занимает 1362 байта вместо примерно 160 МБ inline JSON; geometry BIN занимает 19 917 768 байт. Полный каталог результата — около 258 МБ против почти 2 ГБ незавершённого текстового прогона на сопоставимом этапе.
- Тот же full-level acceptance повторно пройден ASan-сборкой LevelEditor на D3D12 с обязательным `-rdbg`: native `RenderScene` и все 426 пар StaticMesh JSON/BIN загружены после записи, sanitizer и NRI/API validation errors отсутствуют. В результате 427 BIN-файлов с учётом служебного smoke mesh занимают 244 794 264 байта, а 427 metadata JSON — 595 393 байта.
- Level importer использует batched migration transaction: тысячи component source/material updates накапливаются в памяти, а `legacy-object-migration.json` атомарно публикуется один раз после обхода сцены.
- Добавлен renderer-neutral CPU picker с persistent mesh cache: nearest triangle, transforms, material section, backface culling, max distance, mesh removal и instance-only update покрыты отдельным regression-тестом. Legacy backend сохраняет существующий точный CPU picking старого редактора.
- Выбранные instances получают отдельный NRI wireframe overlay с depth test без depth write. Совместный GPU smoke требует корректный pick результата и ровно три selection draws для opaque, translucent и additive material sections.
- Сохранённые points, lines, wire/solid faces и OBB из LevelEditor `m_DebugDraw` преобразуются в renderer-neutral revisioned line/triangle packet. Mailbox проверяет finite vertex data, NRI backend создаёт host-upload vertex buffer и рисует отдельные depth-tested line-list/triangle-list passes с alpha blending; replacement buffer освобождается через deferred deletion.
- На Tiramisu redraw `xrECore` открывает transient capture вокруг legacy `CDrawUtilities`. Common world-space `DU_impl` calls параллельно сохраняют depth-tested line/triangle primitives, object-axis — screen-space lines и owned text labels, а selection rectangle заранее добавляется как две alpha-blended screen-space triangles до scene submission. Старый D3D9 draw не отключается. Capture закрывается после `Scene->Render`, cursor и `m_DebugDraw`, затем все пять списков добавляются в один revisioned snapshot. Есть отдельный лимит в 1 048 576 элементов каждого типа на redraw. Inactive capture ничего не накапливает, begin/end lifecycle и владение временными строками проверяются `xrEditorRenderBackendTests`.
- NRI viewport создаёт `RGBA8` color и `D32` depth targets, загружает vertex/index buffers и рисует indexed instances с camera/object matrices. Для material slots асинхронно собирается настоящий Forward pass с общим material HLSL contract; он читает native lights из тройно буферизованного bindless light buffer. Debug shader остаётся только last-resort fallback. Заменённые buffers и pipelines освобождаются после timeline fence. Это первый editor material/light path, но ещё не G-buffer/deferred renderer.

## Отдельные задачи по редактору

1. [x] **Editor 1 — renderer-neutral `xrEUI`.** Сменный ImGui backend, корректный SDL platform init и отключение unsupported multi-viewports; есть отдельный contract test.
2. [x] **Editor 2 — NRI ImGui presenter.** Backend, swapchain, тройной frame scheduler, двухфазный present и opt-in startup подключены через `-tiramisu-editor` (`-dx12` для D3D12). Runtime GPU acceptance вынесен в Editor 6.
3. [ ] 🟡 **Editor 3 — Tiramisu viewport и native scene workflow.** Главный viewport, renderer-owned editor textures, picking/selection/debug draw, Forward materials и native Light готовы. `xrTiramisuSceneCore`, native static-mesh/render-scene assets, Content Browser open и автоматический `.object`/`.level` importer с дампами подключены. Native lifecycle включает point/rectangle selection, Focus Selected/Zoom All, move/rotate/scale, StaticMesh drag-and-drop, создание Directional/Point/Spot Light, Cut/Copy/Paste с cross-scene GUID remap, delete/duplicate, invert selection, Hide Selected/Unselected/All, transaction undo/redo и atomic Save/Save As. Outliner объединяет StaticMesh и Light; Light Details редактирует type, transform, HDR color/intensity, range, spot cones, visibility и cast-shadows. Остались остальные editor object types/tools, а затем удаление переходной `EScene` модели. `TiramisuLegacyScene` сохраняется для старого игрового контента.
4. [x] **Editor 4 — базовый material GPU preview.** Tiramisu реализует sphere/cube/plane, offscreen render target, environment selection, асинхронную DXC/NRI pipeline сборку и безопасную ImGui presentation. Vulkan и D3D12 smoke пройдены с `-rdbg`.
5. [ ] 🟡 **Editor 5 — Material Editor authoring.** Node canvas, assets, parent instance chains, diagnostics, generated HLSL, фактический preview pipeline key/backend/pass/vertex-factory и GPU preview с настоящими Texture2D/TextureCube/environment lighting готовы. Остаются production IBL и статистика полного production permutation set.
6. [ ] 🟡 **Editor 6 — live workflow и тесты.** Dependency watcher preview и основной сцены, background compile, безопасная publication/last-good pipeline, autosave/migration integration, normal/ASan CTest и совместный preview+scene reload Vulkan/D3D12 GPU smoke готовы. Остались resize/restart/device-loss и automated flythrough.

NRI presenter намеренно не является default. Editor images передаются через renderer-owned texture handles и mailbox; оставшиеся незарегистрированные legacy user-image команды безопасно заменяются white descriptor и не попадают в NRI как raw DX9 pointers. Перенос submit на выделенный editor render thread и полная runtime-проверка scene viewport учитываются в общей задаче Editor 6.

Первый viewport Editor 3 уже не использует DX9 texture pointer: `TiramisuEditorRenderBridge` одновременно реализует `IEditorRenderBackend`, создаёт device-local color/depth targets, переводит color `COLOR_ATTACHMENT → SHADER_RESOURCE` и удаляет descriptor из registry до уничтожения GPU resource. Native document и переходная legacy-сцена поступают через один snapshot/mailbox; material slots разрешаются в instances, а общий compiler создаёт Forward permutation. Native Light records размещаются в отдельном тройно буферизованном диапазоне bindless buffer, поэтому Directional/Point/Spot lighting не требует менять pipeline при изменении runtime radiometry. CPU picker, wireframe selection, debug/overlay/text paths и transient editor meshes используют тот же renderer-neutral packet. Dependency-driven reload основной сцены сохраняет last-good pipeline и проверяется отдельным reload counter в GPU smoke. Native document выполняет общий selection/transform/visibility/Cut/Copy/Paste/duplicate/delete/undo/save lifecycle для StaticMesh и Light; cross-scene paste назначает новый GUID, сохраняет Light parameters и входит в одну undo-запись. StaticMesh дополнительно поддерживает material overrides и path rebasing. Focus получает renderer-neutral world bounds из mesh AABB и 0,5-unit bounds light icon. Native Object List не обращается к `CCustomObject`: он объединяет StaticMesh и Light, виртуализирует строки и выполняет atomic range selection/bulk visibility через document. Это всё ещё не готовый редактор: остальные object types/tools и restart/device-loss acceptance отсутствуют.

## Material Editor: что ещё требуется до отметки «готов»

Material Editor пока **не готов**. Готов рабочий asset-based slice, включая parent chains и GPU asset preview, но он ещё не закрывает полный authoring/preview workflow. Для завершения обязательны:

- production prefiltered environment/IBL и расширенная статистика реальных shader permutations;
- production-wide material hot reload и render-thread deferred destruction без `QueueWaitIdle` на обычном пути; focused Material Editor live preview уже работает;
- завершение Tiramisu viewport workflow LevelEditor: остальные object types/tools и resize/restart/device-loss validation; point/rectangle/invert selection, Focus Selected/Zoom All, Cut/Copy/Paste, duplicate/delete, visibility commands, single- и multi-component Details/material overrides, native Outliner с bulk visibility, пустая native scene, create/edit/save StaticMesh lifecycle, native asset open/legacy auto-conversion, renderer-neutral picking и debug/overlay paths уже готовы.

Фраза **«Material Editor готов»** допустима только после production IBL/permutation acceptance и завершения полного Tiramisu-only LevelEditor workflow с native scene authoring. Базовый цикл открыть asset → изменить `FMaterialGraph` → собрать HLSL → показать диагностику/preview → сохранить → повторно открыть уже работает.

## Проверки checkpoint

> Все тесты Tiramisu без исключений запускаются с `-rdbg`: CPU/CTest, compiler/cooker, LevelEditor, ShaderEditor, Vulkan/D3D12 GPU smoke и будущие automated flythrough. Результат без `-rdbg` не считается валидной проверкой и не закрывает задачу или acceptance criterion.

23 июля 2026 года выполнено:

Оба editor preset (`Editors-x64-Windows` и `Editors-x64-Windows-ASan`) полностью пересобраны через `ALL_BUILD --clean-first` без `/MP1`. После чистой сборки normal и ASan наборы, а также GPU smoke, были запущены заново.

```text
ctest --test-dir build/x64/Editors-Windows -C RelWithDebInfo --output-on-failure
100% tests passed, 0 tests failed out of 41
```

Все прямые CTest executables зарегистрированы с отдельным аргументом `-rdbg`. Два wrapper-теста cooker запускают CMake script, который отклоняет любое значение кроме `TEST_DEBUG_FLAG=-rdbg` и передаёт этот флаг каждому cooker/inspector process. Полные наборы прошли 42/42 в normal и 42/42 в `Editors-Windows-ASan`; CMake автоматически доставляет MSVC ASan runtime рядом с test/editor binaries. `detect_leaks=1` не используется, потому что MSVC AddressSanitizer на Windows не поддерживает LeakSanitizer и завершает процесс до входа в тест.

Набор включает:

- material/compiler/cooker tests, включая 9 masters, 9 instances и deterministic bundle из 200 DXIL/SPIR-V blobs;
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

Совместный Material Preview + editor scene reload GPU smoke пройден исключительно с `-rdbg -render-deterministic` в четырёх вариантах: normal/ASan × Vulkan/D3D12. Preview загружает `kung` как Texture2D 1024×1024 с 11 mip и `sky_10_cube#small` как TextureCube 32×32 через прямой descriptor heap; scene smoke создаёт synthetic indexed mesh с тремя material sections и non-identity translation в `LocalToWorld`, Directional и Point Light, проверяет CPU pick смещённого instance (`distance=1.000`, object/material IDs), разрешает legacy `default`/`textures/kung`, `editor\spawn_icon`/default-white и `editor\particle_additive`/default-white, собирает отдельные opaque, translucent/unlit и additive/unlit Forward pipelines, выполняет три обычных и три wireframe selection draws, по одной world-space debug line/triangle, screen-space overlay line/triangle и owned text label, запускает background resolver reload и ждёт `ReloadCount = 1/1/1`. Во всех четырёх вариантах — `draws=3`, `selection=3`, `overlay-text=1`, `lights=2`, renderer snapshot содержит `passes=3`, `gpu-draws=164`, `triangles=6560` и ненулевой tracked resource census; exit 0, нет `FATAL ERROR`, ASan, NRI/API validation errors. Значение `gpu-timing=not-collected` ожидаемо: timestamp queries ещё не реализованы. DLTX override `rain` исправлен на `![rain]`. На D3D12 остаётся только NRI warning о неподдержанном optional `options22` (`E_INVALIDARG`), это не validation error. Скрытый Win32-запуск работает, однако это ещё не headless runner: content flythrough и image-diff capture runner остаются следующими задачами.

Отдельный RenderDoc smoke также пройден в normal/ASan × Vulkan/D3D12 с `-rdbg -renderdoc`. Установлен RenderDoc 1.45, in-application API сообщает 1.7.0. Во всех четырёх вариантах DLL загружена до NRI device, конфликтующие NRI/API validation layers подавлены, shader debug info сохранена, material preview и viewport reload завершились с exit 0. D3D12 ожидаемо отключает NVAPI при активном RenderDoc. Capture по F12 сохраняется в абсолютный путь `<workspace>/logs/renderdoc/LevelEditor*.rdc`.

Legacy conversion GPU/editor smoke также пройден исключительно с `-rdbg` в четырёх вариантах normal/ASan × Vulkan/D3D12. Он проверяет failed dump для незагружаемого `.level`, двукратную конвертацию реального `.object` с тем же asset GUID и повторным использованием MaterialInstance, парные StaticMesh JSON/BIN, payload path в dump, конвертацию самодостаточного старого `.level`, `asset_mappings` и последующую загрузку native `RenderScene`. Отдельный full-level smoke конвертирует Zaton вместе со всеми `.part` в timestamp-каталог; полный D3D12-вариант также завершён под ASan. Неполный legacy level с отсутствующими library objects обязан завершаться ошибкой и dump, а не создавать видимость успешной пустой сцены.

## Ближайший порядок работы

1. Довести native LevelEditor workflow: добавить остальные object types/tools, затем убрать переходную `EScene` модель из editor composition root. Point/rectangle/invert selection, Focus Selected/Zoom All, Cut/Copy/Paste, duplicate/delete, visibility commands, single- и multi-component Details/material overrides, native Outliner с bulk visibility, пустая native scene и базовый create/edit/save StaticMesh lifecycle уже работают; старые `.object`/`.level` остаются только import sources с обязательными dumps.
2. Подключить готовое ядро NRI executor к основному frame loop и добавить физическое создание/aliasing transient resources и frame statistics.
3. Подключить G-buffer MRT/depth resources, deferred directional/point lighting и tone-map shaders через первый исполняемый граф.
4. Завершить аудит thread-affinity и перевести оставшиеся texture/buffer/descriptor destroy paths на уже добавленную fenced deferred deletion queue.
5. Выполнить resize/restart/device-loss validation первого scene viewport на Vulkan и D3D12, затем добавить representative scene flythrough.
6. Расширить готовый focused dependency watcher/live preview до production material pipeline set, добавить production IBL и статистику всех собираемых permutations.
7. Перевести UI/остальные domains на единый material/draw ABI и заменить JSON records в bundle v2 на binary flattened records cooked runtime.

## Состояние Git для продолжения

- Активная ветка: `dev/viberender`.
- Незавершённого merge нет.
- Последний завершённый merge: `7bd826800a3c0b8b40991a27eaecf50cdabfc379`.
- Текущий большой checkpoint зафиксирован как WIP с обязательной пометкой `needs refactor`; дальнейшие изменения следует разбивать по подсистемам.
- Существующее пользовательское изменение `src/xrGame/ActorCameras.cpp` не относится к Tiramisu и должно быть сохранено.
- Существующее пользовательское изменение `src/xrGame/ai/monsters/controller/controller.h` не относится к Tiramisu и должно быть сохранено.

## Решение по совместимости редакторов

Игровой R4 остаётся доступным и не изменяется. Для LevelEditor и ShaderEditor обратная совместимость с legacy D3D9 renderer больше не является требованием: целевой editor build переводится полностью на Tiramisu. Временный renderer adapter может быть удалён из editor composition root после переноса прямых `RDevice`/`RImplementation` участков. Это не отменяет совместимость контента: `TiramisuLegacyScene` остаётся в Tiramisu как адаптер старых локаций `.level`/OGF параллельно новому scene format.

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
- [x] Выполнен Debug `ALL_BUILD` обычной параллельной сборкой без `/MP1`; затем 43/43 material/scene/Tiramisu/editor теста прошли исключительно с `-rdbg`.
- [ ] Выполнить отдельную ASan-конфигурацию и ASan-тесты с `-rdbg`.
- [ ] После build/test regression выполнить runtime smoke для Vulkan и D3D12 с `-rdbg`; RenderDoc запускать без конфликтующего renderer debug/validation режима согласно описанной политике.

### Последние подтверждённые проверки

- `xrTiramisuMaterialCore`, `xrMaterialCooker`, `xrRenderTiramisu`, `xrTiramisuSceneCore`, `xrTiramisuMaterialEditorCore`: Debug build — успешно.
- Debug `ALL_BUILD`, включая `xrEngine`, R4, Tiramisu и все editor targets: успешно без `/MP1`.
- `LevelEditor`: Debug build — успешно.
- Material test suite: 12/12 — успешно с `-rdbg`.
- Renderer test suite: 7/7 — успешно с `-rdbg`.
- Scene/Material Editor core suite: 7/7 — успешно с `-rdbg`.
- Renderer-neutral editor/Tiramisu suite: 13/13 — успешно с `-rdbg`.
- Совокупный material/scene/Tiramisu/editor CTest block: 43/43 — успешно с `-rdbg`.
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
- [x] Повторно пройдены 43/43 CTest-теста, каждый с обязательным `-rdbg`.
- [ ] Перевести специализированные editor passes на общий
  `TiramisuRenderGraph` при сохранении текущего публичного API.