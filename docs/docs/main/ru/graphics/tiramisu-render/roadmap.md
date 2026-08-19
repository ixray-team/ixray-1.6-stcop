# Roadmap Tiramisu Render

> Статус: рабочий план полной замены R4. Обновлено 19 августа 2026 года после пересоздания ветки `dev/tiramisu`.

Цель — довести Tiramisu до feature и performance parity с R4 на Vulkan и D3D12. До прохождения acceptance gates renderer включается только через `-r5`. R4 сохраняется без изменения и после rollout остаётся fallback минимум один стабильный релиз.

## Статус реализации

> Важно: recovery baseline пересозданной ветки закрыт свежими normal/ASan
> сборками, CTest, Vulkan/D3D12 и RenderDoc. Последующие feature-пункты всё равно
> требуют собственных проверок и не наследуют готовность автоматически.

Обозначения:

- `[x]` — выполнено и проверено;
- `[ ]` — не выполнено;
- 🟡 — задача начата, но её acceptance criteria ещё не закрыты.

| Этап | Статус | Фактический результат |
| --- | --- | --- |
| 0. Восстановление после пересоздания ветки | ✅ Выполнено | История и состав исходников проверены; свежие normal/ASan сборки, 48/48 CTest с `-rdbg`, GPU smoke Vulkan/D3D12, RenderDoc и запуск/закрытие LevelEditor подтверждены |
| 1. Baseline и документация | ✅ Выполнено | Документация, полная R4 feature matrix, versioned representative-scenes suite, deterministic GPU mode, актуальный CMake preset, NRI validation, GPU markers и базовая frame/resource statistics проверены |
| 2. Renderer foundation | 🟡 Частично | Adapter/queue selection, `SupportsRendering`, `TRenderGraph` и NRI executor для bound resources/трёх queues готовы; transient resources и main frame integration пока не подключены |
| 3. Material runtime | 🟡 Частично | CPU runtime, deterministic parameter layout, общий GPU ABI, runtime bundle lookup и validation NRI pipeline готовы; production pass set и lifetime ещё не подключены |
| 4. Graph compiler и инструменты | 🟡 Частично | Compiler/cooker, ImNodes Material Editor, parent chains, dependency-driven focused live workflow и Tiramisu GPU preview с Texture2D/TextureCube/environment lighting собраны; normal/ASan Vulkan/D3D12 preview+scene smoke пройден, но production IBL и renderer-wide hot reload ещё нужны |
| 5. Scene и geometry pipeline | 🟡 Частично | `xrTiramisuSceneCore` StaticMesh v2 JSON/BIN и RenderScene v2 с native Light, per-component material overrides, `.object`/`.level` auto-import с dumps, `FMeshBatch`, `TiramisuLegacyScene` и renderer-owned standalone OGF bind-pose path готовы; StaticMesh/Light create/edit/save, Outliner, Details и undo работают, но остальные object types, animation и vertex factories ещё не завершены |
| 6. Deferred PBR и lighting | 🟡 Editor Forward foundation | G-buffer ABI и direct PBR компилируются в DXIL/SPIR-V; editor Forward pass читает до 64 Directional/Point/Spot lights из bindless buffer. `TiramisuRenderDeferredPass` всё ещё не является deferred renderer, clustered lists и shadows отсутствуют |
| 7. World features | ⬜ Не начат | Заглушки прототипа |
| 8. Postprocessing и R4 effects | 🟡 Shader foundation | Tone mapping адаптирован из R4 и компилируется; HDR/exposure/bloom passes ещё не подключены |
| 9. Parity и rollout | ⬜ Не начат | Tiramisu остаётся opt-in через `-r5` |

### Исторически выполнено и проверено до пересоздания ветки

Пункты ниже описывают последний известный рабочий checkpoint. Их код не считается
утраченным, однако результаты необходимо воспроизвести на текущем `HEAD` в этапе 0.

- [x] `xrTiramisuMaterialCore` собирается как независимая static library;
- [x] Debug target `xrRenderTiramisu` собирается с подключённым `xrTiramisuMaterialCore`;
- [x] CTest material suite: 14/14 passed;
- [x] `xrMaterialCooker --validate-only`: 6 master materials и 9 instances;
- [x] общий DXC path компилирует семь pixel pass templates в DXIL и SPIR-V с Shader Model 6.6;
- [x] Descriptor Heap Indexing проверен компиляцией обращений к `ResourceDescriptorHeap` и `SamplerDescriptorHeap` для D3D12/Vulkan;
- [x] cooker сохраняет 200 проверенных DXIL/SPIR-V blobs: парные vertex/pixel stages для известных permutations и двух backend;
- [x] bundle v2 сохраняет pass, shader stage, vertex factory и render-pass signature для каждого blob;
- [x] CPU pipeline cache атомарно публикует наборы на границе кадра, отклоняет неполный reload и возвращает старые snapshots для deferred deletion;
- [x] два независимых development bundle с shader blobs имеют одинаковый SHA-256;
- [x] deterministic parameter layout генерирует общий C++/HLSL contract и загружает scalar/vector/texture/sampler параметры из `ByteAddressBuffer`;
- [x] renderer создаёт базовые draw/material-instance/material-parameter buffers и публикует их через bindless descriptor heap;
- [x] material GPU ABI v5 добавляет 64-байтные light/palette records,
  `LightDataBufferIndex/Offset/Count`, current/previous skinning offsets,
  inverse view-projection и decal draw metadata; editor viewport загружает
  данные в тройно буферизованные `ResourceDescriptorHeap` ranges;
- [x] static level geometry передаёт индекс draw record через `baseInstance`, а HLSL читает его через переносимый `NRI_INSTANCE_ID_OFFSET`;
- [x] legacy static vertex/pixel shaders компилируются для DXIL и SPIR-V с новым material/draw ABI;
- [x] `UIRenderForm` использует renderer-neutral capture/resize/surface contract с legacy adapter по умолчанию;
- [x] `xrECore`, LevelEditor и ShaderEditor собраны в `Editors-x64-Windows` RelWithDebInfo;
- [x] полный CTest в editor preset: 47/47 passed с обязательным `-rdbg`, включая particle catalog/simulation, OGF/OMF loader, renderer statistics, scene assets/conversion dump, native scene document и legacy object MaterialInstance migration;
- [x] тот же полный набор прошёл 47/47 в ASan; прямые test executables получают отдельный `-rdbg`, а два CMake cooker wrapper жёстко проверяют `TEST_DEBUG_FLAG=-rdbg` и передают его каждому вложенному процессу;
- [x] совместный Material Preview + viewport material GPU smoke с `-rdbg -render-deterministic` пройден в normal/ASan × Vulkan/D3D12: реальные Texture2D/TextureCube, Directional+Point bindless lights, Forward scene pipeline, GPU markers, frame/resource snapshot, 0 ASan errors и 0 NRI/API validation errors;
- [x] VitePress production build документации завершён успешно;
- [x] Tiramisu по-прежнему включается только через `-r5`, R4 не изменён.

## Этапы

### 0. Восстановление baseline после пересоздания ветки

- [x] Подтвердить, что в `dev/tiramisu` присутствуют checkpoint Tiramisu, исправление запуска LevelEditor и последующий рефакторинг.
- [x] Подтвердить наличие `xrRenderTiramisu`, `xrTiramisuMaterialCore`, `xrTiramisuSceneCore`, `xrTiramisuMaterialEditorCore`, LevelEditor integration и CMake presets normal/ASan.
- [x] Зафиксировать точный diff текущей ветки относительно последнего рабочего checkpoint и отделить намеренный рефакторинг от случайной потери файлов.
- [x] Создать свежие normal и ASan build-каталоги через `Editors-x64-Windows` и `Editors-x64-Windows-ASan`; старые build-каталоги использовать только для диагностики.
- [x] Выполнить `ALL_BUILD` для normal и ASan обычной параллельной сборкой без `/MP1`.
- [x] Получить фактический список CTest и проверить, что каждая команда содержит точный `-rdbg`.
- [x] Пройти весь CTest в normal и ASan; фактический результат после OGF/OMF GPU-skinning tests — 47/47 в обеих конфигурациях.
- [x] Пройти LevelEditor smoke с `-rdbg` на Vulkan, затем на D3D12: startup, material preview, viewport material reload и чистый shutdown; та же матрица пройдена под ASan.
- [x] Повторить Vulkan/D3D12 smoke под ASan и проверить журналы на `FATAL ERROR`, ASan, NRI и API validation errors.
- [x] Повторить RenderDoc smoke через `-renderdoc -rdbg` на Vulkan и D3D12, отложить capture до ready material/reload frame, проверить native MCP replay RenderDoc 1.45 и зафиксировать пути `.rdc`.
- [x] Открыть или автоматически сконвертировать полноценный Zaton с sibling-файлами, проверить native scene, материалы, освещение и conversion dumps.
- [x] Обновить `status.md`, количество тестов и таблицу паритета по новым результатам; только после этого вернуть editor-пунктам статус «подтверждено».

Критерий завершения этапа 0: LevelEditor запускается, показывает native/legacy scene и
Material Editor, корректно закрывается на Vulkan и D3D12, normal/ASan проверки зелёные,
а все команды тестов содержат `-rdbg`.

### 1. Baseline и документация

- [x] Зафиксировать фактический статус прототипа в документации.
- [x] Составить полную [R4 feature matrix](./r4-feature-matrix.md) с source anchors, текущим статусом Tiramisu и gate для каждой строки.
- [x] Выбрать [representative scenes](./representative-scenes.md), сохранить versioned suite manifest и реализовать общий `-render-deterministic` GPU test policy для game/editor.
- [x] Получить чистую сборку актуальным CMake preset.
- [x] Добавить NRI validation, GPU markers и базовую frame/resource statistics: game/editor публикуют pass/draw/triangle/upload/CPU-time counters и tracked resource census; GPU timestamps и driver VRAM намеренно остаются следующими задачами.
- [x] Добавить скрытый full-level Optick profile Zaton с 60-кадровым warmup и capture; устранить повторное хеширование geometry, material resolve, отсутствие frustum culling и полную передачу неизменного AI/debug draw. Общий geometry arena, cached scene layout и indexed indirect batching сводят 3296 логических draws к 900 командам и двум API-группам. Свежий normal `-rdbg` p50 снижен с 1748,65 мс до 31,26 мс на Vulkan и 30,81 мс на D3D12; финальный ASan p50 — 94,20 и 88,94 мс без sanitizer/NRI validation errors.

### 2. Renderer foundation

- [x] Исправить adapter selection и `SupportsRendering`: полное NRI enumeration, API/graphics-queue filtering и deterministic discrete/integrated priority.
- [x] Вынести legacy material pipeline creation из game-thread constructor в render command; добавить thread-affinity checks и тесты очереди.
- [x] Уточнить thread contract до точных `GGameThreadId`/`GRenderThreadId`: посторонний worker thread больше не считается game thread; идентификатор render thread и флаг остановки являются атомарными.
- [x] Добавить render-thread assertions в material/texture/resource proxies, scene proxies, descriptor allocator, render commands и legacy scene upload; исправить захват viewport reset по ссылке.
- [ ] Завершить thread-affinity аудит всех NRI create/destroy/update paths и добавить validation build checks.
- [ ] 🟡 Ввести три frame contexts: три command allocator/buffer contexts основного path готовы; per-frame transient descriptors/uploads/queries ещё не разнесены полностью.
- [ ] 🟡 Реализовать безопасную остановку render thread и deferred resource deletion: fence-aware queue, shutdown flush и deferred material pipeline release готовы; остальные resource destroy paths ещё мигрируют.
- [x] Реализовать CPU-компилятор `TRenderGraph`: generation-counted resources/passes, dependencies, RAW/WAR/WAW hazards, stable topological order, barriers, queue transfers, transient lifetimes и aliasing.
- [x] Добавить детерминированный per-pass submission plan, cross-queue waits и NRI translation для texture/buffer barriers, ownership transfer и indirect argument state.
- [x] Обнаруживать NRI compute/copy queues и детерминированно оставлять graphics fallback, если отдельная queue отсутствует.
- [ ] 🟡 Завершить NRI executor `TRenderGraph`: command allocators/buffers, graphics/compute/copy submissions, timeline waits/signals, external sync и bound resource barriers готовы; остались физические transient resources, frame statistics и main-path wiring.
- [ ] Стабилизировать swapchain, resize, fullscreen, VSync и device-loss paths.

### 3. Material runtime

- [x] Реализовать независимый `xrTiramisuMaterialCore`, versioned JSON schema и generation-counted handles.
- [x] Реализовать master/instance inheritance, flattening и CPU dynamic parameters.
- [x] Реализовать deterministic parameter layout, HLSL loader и общий versioned GPU ABI.
- [x] Реализовать базовые renderer-owned material instance/parameter/draw buffers и bindless descriptor upload.
- [ ] Перевести GPU storage на frame-context allocations, deferred deletion и безопасное повторное использование диапазонов.
- [x] Добавить начальные hand-written HLSL masters и deterministic pipeline keys.
- [x] Вынести общий NRI-независимый DXC compiler и собрать validation permutations для D3D12/Vulkan.
- [x] Добавить versioned pass manifest и pixel templates для Depth, Shadow, GBuffer, Forward, UI, PostProcess и Validation.
- [x] Реализовать CPU pipeline cache со staging, frame-boundary publication и сохранением старого snapshot при ошибке.
- [ ] 🟡 Создавать NRI pipelines из bundle и разрешать material pass proxies непосредственно в renderer: immutable bundle lookup и validation pipeline на render thread готовы; production Depth/Shadow/GBuffer/Forward set ждёт MRT/render graph.
- [x] Добавить standard/error materials и legacy adapter/fallback.
- [x] Добавить legacy opaque/masked/emissive masters, pre-authored static instances и cached dynamic instance bridge из `legacy-map.json`.
- [ ] Передавать полный legacy texture/parameter set через общий GPU parameter layout; сейчас prototype bridge передаёт только первую texture.
- [ ] 🟡 Перевести static geometry и UI на material pass proxies; static geometry уже использует per-draw/material buffer ABI, но всё ещё выбирает legacy pipeline, UI остаётся на старом texture-index path.

### 4. Graph compiler и инструменты

- [x] Реализовать versioned graph model, typed pins/links и diagnostics.
- [x] Реализовать type checking, cycle detection, constant folding, DCE и HLSL generation.
- [x] Реализовать Static Switch и ограниченный Custom HLSL node.
- [x] Добавить общий initial node catalog для compiler/editor: 21 versioned node type с typed pins и factory.
- [x] Добавить deterministic golden HLSL tests.
- [x] Добавить bytecode-тест эквивалентности hand-written и graph-generated material.
- [x] Реализовать cooker validation, flattening и deterministic development bundle.
- [x] Подключить общий DXC к cooker и записывать требуемые production pixel passes плюс Validation для всех известных static permutations.
- [x] Добавить bundle v2 с versioned pass metadata и детерминированной сериализацией.
- [x] Добавить CPU-часть safe publication: новый полный набор активируется на границе кадра, ошибочный набор не заменяет старый.
- [x] Добавить canonical vertex-factory/WPO shader, paired vertex/pixel blobs и development renderer bundle consumption.
- [ ] Добавить binary flattened records, полный domain/pass set и строгую runtime validation; только после этого выставлять `CompleteShaderSet = true`.
- [ ] 🟡 Расширить dependency tracking и background rebuild до production game renderer: Material Editor preview и основная editor scene уже отслеживают master/instance parent chain, legacy map и HLSL/pass dependencies, создавая NRI pipelines с last-good fallback.
- [x] Провести аудит `ShaderEditor`, `UIRenderForm` и legacy renderer-кода внутри `xrECore`.
- [x] Выделить базовый renderer-neutral viewport presentation contract без D3D9/NRI типов в `UIRenderForm`.
- [x] Обернуть legacy capture, resize и ImGui surface presentation в adapter по умолчанию.
- [x] Включить базовый NRI ImGui draw path в Tiramisu с game-thread ImGui ownership и синхронной безопасной передачей draw data на общий render thread `xrRenderTiramisu`.
- [x] Выделить узкий NRI-независимый `IMaterialPreviewRenderer` с generation-counted handles, revisioned source, sphere/cube/plane/environment и состояниями compile/ready/error.
- [x] Добавить Preview-вкладку Material Editor, которая передаёт master/instance/generated HLSL через нейтральный контракт и явно показывает отсутствие backend.
- [x] **Editor 1:** отвязать `xrEUI` от встроенного DX9 ImGui backend и покрыть сменный renderer contract тестом.
- [x] **Editor 2:** подключить NRI ImGui presenter/swapchain LevelEditor: backend, три frame contexts, scheduler, single-present contract и Tiramisu composition root по умолчанию готовы; `-dx12` выбирает D3D12, прежний `-tiramisu-editor` сохранён как необязательный совместимый аргумент.
- [x] Добавить `-editor-test-hidden` для автоматических LevelEditor smoke: отключить splash/focus activation, использовать non-focusable off-screen HWND для совместимости с DXGI и проверять parser contract вместе с `-render-deterministic`.
- [ ] 🟡 **Editor 3:** viewport/picking/debug draw и native asset open/render готовы. Добавлены `xrTiramisuSceneCore`, native StaticMesh/RenderScene v2 Light, per-component material overrides и `.object`/`.level` auto-import с audit dumps. StaticMesh и Directional/Point/Spot Light участвуют в selection, transforms, Focus, visibility, Cut/Copy/Paste с cross-scene GUID remap, duplicate/delete, undo/redo и atomic Save/Save As; общий Outliner и Light Details редактируют type/radiometry/range/cones/shadow metadata. StaticMesh дополнительно поддерживает drag-and-drop, material overrides и bulk Details. Live legacy `CLight` и сохранённое направление солнца уже конвертируются переходным bridge в те же renderer-neutral light records. Остались остальные editor tools и удаление переходной `EScene` модели; `TiramisuLegacyScene` сохраняется для старого игрового контента.
- [x] **Editor 4:** реализован базовый Tiramisu GPU backend `IMaterialPreviewRenderer`: async compiler, NRI pipelines/targets, sphere/cube/plane, environment selection, safe old-pipeline fallback, ImGui surface и Vulkan/D3D12 smoke с `-rdbg`.
- [ ] 🟡 **Editor 5:** завершить material authoring: Master Material Editor и Material Instance Editor разделены на самостоятельные окна и маршрутизируются Content Browser по типу asset; parent instance chains, GPU preview с реальными texture/environment assets и feedback фактического preview pipeline key/backend/pass/vertex-factory готовы. Остаются production IBL и статистика полного production permutation set.
- [ ] 🟡 **Editor 6:** dependency watcher preview/основной сцены, background compile, безопасная publication/last-good pipeline, autosave/migration, общий render-thread submit, resize/recreate, normal/ASan CTest и совместный preview+scene reload GPU smoke готовы; остаются restart/device-loss, тройные immutable UI packets и representative scene tests.
- [x] Сделать `xrRenderTiramisu` единственным GPU composition root режима `-tiramisu-editor` в LevelEditor без изменения legacy CPU authoring model; игровой R4 остаётся отдельным и не затрагивается. Standalone legacy ShaderEditor не подключать.
- [ ] 🟡 Перенести render-target presentation, picking/debug draw и ImGui texture presentation LevelEditor на Tiramisu: основной redraw обходит `CEditorRenderDevice::Begin`, `RCache` и legacy `Scene->Render`; startup/shutdown не создаёт D3D11 `CRHI`/`CResourceManager`/legacy render targets, а globals остаются реализациями `xrRenderTiramisu`. Submit, resize и lifecycle editor GPU resources уже выполняются общим render thread. Renderer-neutral CPU picking, NRI selection overlay, grid, `m_DebugDraw`, selection rectangle, corner axis, owner/tool visibility и live legacy Terrain/Detail Objects/Fog Volume/Light/Shape/Sound Source/Sound Environment/Puddle/Portal/Glow/AI Map/WayPoint/Group/Sector packets готовы. Terrain использует общий static-mesh packet из своего сгенерированного `CEditableObject`, без отдельного NRI renderer в редакторе; direct heightmap-generated CTerrain smoke проверяет mesh draw, selection и чистое освобождение derived data. Detail Objects используют старую CPU slot-декомпрессию только как import/placement adapter и объединяют видимые экземпляры по модели в общий Tiramisu static-mesh batch; `hw_Render` не вызывается. Selected/inactive slot boxes публикуются через общий line debug packet, а base texture projector — через textured static-mesh packet с renderer-owned bindless texture и per-draw depth bias. Остались wind и нативный GPU-instanced detail vertex factory. Fog Volume emitter/occlusion использует общий shape debug packet; реальный volumetric fog render pass остаётся отдельной world-feature задачей. Spawn visual передаётся по renderer-neutral OGF contract; OGF/OMF motion sampling, current/previous GPU palette и skeletal material vertex factory работают. Attached shape использует общий shape packet, idle particle/group — общий renderer-owned particle path, OGF draw-parts публикуются в общий CPU picker, fallback icon/selection/label сохранён. Все сохраняемые `OBJCLASS_GROUP..OBJCLASS_TERRAIN` представлены в bridge; остаются restart/device-loss acceptance и недостающие native services.
- [x] Перевести main/top toolbars, Content Browser, icon picker, texture viewer, object/library previews, Detail Object Shuffle, Light Animation и Image Editor с прямых `ref_texture`/`IRHISurface` на `TUI::LoadTexture` и `FEditorTextureHandle`; normal/ASan Vulkan/D3D12 smoke пройден с `-rdbg`.
- [x] Перевести asset chooser и thumbnail classes на renderer-neutral texture API: `SChooseTexture` хранит только `FEditorTextureHandle`, UI не создаёт параллельный `IRHISurface`.
- [x] Удалить legacy D3D11 `CRHI`, `CResourceManager`, render targets/shaders и локальный `RImplementation` runtime из Tiramisu LevelEditor startup/frame/shutdown; legacy backend остаётся только для пока не переведённых editor executables.
- [ ] 🟡 Завершить renderer-neutral particle library/preview API в `xrRenderTiramisu`: original/extended catalog binary/loose loading, chooser/Content Browser, legacy object validation, owned scene packets, render-thread PAPI simulation, `time0/time1` scheduling root entries, `on play/birth/death` child callbacks, frame animation, velocity/path/world/face alignment, position markers, textured billboard draw через общий material pipeline и отдельный particle preview surface готовы; collision/culling/distortion/soft-particle варианты остаются. Legacy `CPSLibrary` shader initialization в LevelEditor не возвращать.
- [ ] 🟡 Завершить Material Editor в LevelEditor на базе UX/окон `src/Editors/ShaderEditor`: отдельные master/instance окна, Content Browser open/create, parent chains, full parameter metadata/default/min/max, typed overrides, undo/redo, copy/paste, atomic save, autosave/recovery и focused dependency-driven hot reload готовы; остались production IBL и полный Tiramisu scene workflow.
- [ ] 🟡 Завершить ImNodes tooling: canvas, typed links/properties, generated HLSL, diagnostics с переходом к node, Tiramisu GPU preview с реальными Texture2D/TextureCube и feedback текущего pipeline готовы; остаётся статистика production permutation set.
- [x] Добавить UUID-форму для asset/node/link GUID и deterministic GUID для factory pins.
- [x] Завершить hardening JSON readers: invalid syntax, wrong field types и out-of-range numeric values в master/instance/graph/legacy-map возвращают diagnostics без process abort.
- [x] Перевести master/instance save на temporary file и atomic replace с cleanup при ошибке.
- [x] Добавить property editors для Parameter, Texture Sample, Static Switch, Custom HLSL и constants, включая прямую ссылку Texture Sample на `Texture2D` parameter.
- [x] Добавить редактор сигнатуры Custom HLSL: тип Result, до 16 именованных typed inputs, deterministic pin GUID, безопасную инвалидизацию links, undo и copy/paste.
- [x] Добавить типизированные Constant/Make/Break/Swizzle nodes для `float2/3/4`, проверку swizzle-шаблонов, HLSL generation и сохранение типа при copy/paste.
- [x] Добавить copy/paste с GUID remap и одной undo-операцией, autosave/recovery sidecars и migration dirty-state.
- [x] Добавить deterministic static permutation statistics с lower-bound и overflow diagnostics.

### 5. Scene и geometry pipeline

- [x] Заменить `FRenderMeshBath` на `FMeshBatch`.
- [x] Добавить начальный static mesh render-data model: LOD resources, sections и material slots.
- [x] Сохранить `TiramisuLegacyScene` как adapter `.level`/OGF в `FMeshBatch`; новый scene format остаётся отдельным целевым path.
- [x] Добавить LevelEditor migration bridge `EScene → static mesh uploads + instances` и первый NRI viewport geometry/depth pass без NRI типов в editor scene model.
- [x] Добавить renderer-owned standalone OGF/OMF path для Spawn visual: neutral model instance с optional `startup_animation`, owned mailbox, static/progressive/embedded hierarchy, skeletal 1–4 weights, bone hierarchy, bind/inverse-bind transforms, embedded/external motion sampling, current/previous GPU palette, `skeletal` material permutation, stable draw-part/material IDs, bounded background I/O/parse и normal/ASan Vulkan/D3D12/RenderDoc smoke на реальном actor с 47 костями.
- [x] Добавить независимый `xrTiramisuSceneCore`: versioned native `StaticMesh`/`RenderScene` JSON, прямые material asset references, transforms и per-component material overrides.
- [x] Добавить RenderScene v2 native Light: Directional/Point/Spot, stable GUID/name, transform, HDR color/intensity, range, spot cones, visibility и cast-shadows metadata; сохранить чтение RenderScene v1 и automatic editor upgrade v1 → v2.
- [x] Вынести StaticMesh bulk geometry из JSON в versioned `*.static-mesh.bin`; оставить в JSON параметры/sections/material slots и проверять magic/endian/offsets/strides/counts/size/hash. Inline JSON v1 оставить read-only migration path.
- [x] Добавить Content Browser open native assets и однократную auto-конвертацию старых `.object`/`.level` без изменения исходников.
- [x] При legacy-конвертации создавать/переиспользовать дедуплицированные `MaterialInstance` и сохранять stable migration database.
- [x] Заменить legacy `Surfaces/Tex/Shader/Compile/Mtl` в Properties старого `CSceneObject` на единое поле `Material`: master и instance назначаются через внутренний searchable modal с группами `Materials`/`Instances`, `Open/Select/Reset`, persistent per-surface override и маршрутизацией в соответствующий редактор; explicit asset не получает повторный legacy texture override, а исходный surface остаётся только входом автоконвертации.
- [x] Публиковать обязательный deterministic success/failure dump с source hash, target GUID, asset/material mappings, counts и diagnostics, включая ошибку до загрузки `EScene`.
- [x] Добавить в dump v2 путь binary payload и выполнить full-level acceptance на Zaton; migration database при level import публиковать одной batched транзакцией.
- [x] Перенести при legacy level import сохранённое направление солнца и явные Directional/Point/Spot lights в native `light_components`; regression smoke требует Directional light с cast shadows.
- [x] Перенести legacy Wallmarks в native RenderScene v3 `decal_components`: использовать общий renderer-neutral positions/UV converter, создавать или переиспользовать decal `MaterialInstance`, сохранять material mappings и итог в обязательном migration dump. Full-level Zaton acceptance подтверждает 220 декалей без пропусков и стабильные GUID между Vulkan/D3D12.
- [x] Повторить full-level Zaton acceptance в normal/ASan × Vulkan/D3D12 с `-rdbg`; проверить 426 meshes, 5536 StaticMesh components, 753 lights, повторную загрузку assets и отсутствие sanitizer/NRI validation errors.
- [ ] 🟡 Завершить прямой native scene lifecycle: общий StaticMesh/Light selection, Focus, transforms, visibility, Cut/Copy/Paste, duplicate/delete, undo/redo, Save/Save As, Outliner и Light Details готовы; StaticMesh add и single-/multi-component material Details также готовы. Остались остальные editor object types/tools. Legacy `.object`/`.level` оставить только import sources.
- [ ] 🟡 Добавить vertex factories для static, progressive, dynamic, skinned, trees/details, particles и UI: `level_static` и общий `skeletal` material vertex factory готовы; остальные типы отсутствуют.
- [ ] 🟡 Добавить transforms, previous transforms, LOD/SWI, sorting и visibility lists: transforms/previous transforms, cached material/pipeline sorting, общий static/skeletal geometry arena и CPU visibility list готовы; LOD/SWI и GPU visibility list отсутствуют.
- [x] Добавить indexed indirect submission для editor scene: одинаковые mesh sections объединяются в instances, 20-байтный Vulkan и 28-байтный D3D12 NRI-emulation ABI проверены CPU-тестом, normal/ASan Vulkan/D3D12 Zaton profile не содержит validation errors.
- [ ] Исправить обнаруженный RenderDoc MCP дефект API parity: в synthetic ready-frame D3D12 skeletal `ExecuteIndirect` выполняется, но первый actor draw не меняет render target, тогда как Vulkan рисует модель; после этого ввести допустимый image-diff threshold вместо одного status-based smoke.
- [ ] Реализовать GPU frustum/occlusion culling: object/section bounds, Hi-Z pyramid и compact visibility lists.
- [ ] Выполнять Hi-Z occlusion и построение indirect draw arguments на async compute через `TRenderGraph`; при отсутствии отдельной compute queue использовать тот же pass на graphics queue.
- [ ] Добавить явную compute → graphics синхронизацию, conservative temporal policy для camera cut/resize и debug-режим визуализации culled bounds.
- [ ] Покрыть culling CPU reference tests, deterministic indirect-list tests и GPU validation сценами без queue/barrier errors.
- [ ] Подключить actors, NPC, HUD-модели и оружие.

### 6. Deferred PBR и lighting

- [ ] 🟡 Реализовать G-buffer: shader pack/unpack с BaseColor/AO, octahedral Normal/Roughness/Metallic, Emissive/Flags и Velocity готов; MRT/depth resources и pass wiring отсутствуют.
- [ ] 🟡 Реализовать GGX/Smith/Schlick BRDF, sun, point/spot lights и IBL: editor Forward HLSL и bindless runtime upload Directional/Point/Spot lights готовы и проверены normal/ASan × Vulkan/D3D12; игровой scene upload, deferred pass и IBL отсутствуют.
- [ ] Реализовать clustered light lists.
- [ ] Реализовать cascaded sun/local shadows и masked depth/shadow passes.
- [ ] Реализовать forward transparency/additive/modulate с теми же clustered lights.

### 7. World features

- [ ] 🟡 Реализовать decals: отдельные domain/pass `Decal`,
  `decal_projector`, depth reconstruction и native RenderScene v3 component
  готовы для LevelEditor. Legacy Wallmarks в Tiramisu уже преобразуются в
  projector packets, постоянно мигрируют в scene v3 с audit dump и не рисуются
  старыми clipped triangles. Projector rasterization ограничена canonical box
  volume из 12 треугольников без отдельного vertex buffer; fullscreen triangle
  на каждую декаль удалён. Surface normal восстанавливается из depth derivatives,
  а angle fade отсекает перенос на почти перпендикулярные поверхности.
  Renderer-side conservative frustum culling строит compact visible list и
  публикует visible/culled statistics. Остались production DBuffer/G-buffer
  composition, lifetime/occlusion и игровой scene wiring.
- [ ] Реализовать details/grass/foliage, particles, glows и progressive LOD.
- [ ] Реализовать sky, clouds, weather, rain, flares, thunderbolts и water.
- [ ] 🟡 Добавить domains Decal/UI/PostProcess и shading models
  Unlit/Foliage/Hair в runtime passes: отдельный Decal pass готов; остальные
  production domains/models и игровой pass wiring не завершены.

### 8. Postprocessing и R4 effects

- [ ] 🟡 Реализовать HDR, exposure, tone mapping, gamma и bloom: resource-free tone-map library и bindless fullscreen pass готовы; HDR targets, exposure и bloom chain отсутствуют.
- [ ] Реализовать GTAO/SSAO, SSR/SSLR, TAA, FXAA/SMAA, motion blur и DOF.
- [ ] Реализовать NVG, gas mask, rain effects, CAS, DLSS, FSR и XeSS.
- [ ] 🟡 Реализовать screenshots, video UI, debug draw, statistics и RenderDoc workflow: ранний RenderDoc bootstrap, F12 capture, абсолютный output path и безопасная совместимость с `-rdbg` готовы; screenshots, video UI и production statistics остаются.

### 9. Parity и rollout

- [ ] Закрыть feature matrix и legacy mapping.
- [ ] Пройти acceptance gates на Vulkan и D3D12.
- [ ] Сделать Tiramisu renderer по умолчанию.
- [ ] Сохранить `-r4` fallback минимум один стабильный релиз после rollout.
- [ ] Удалять R4 только отдельным изменением после подтверждения контента и производительности.

## Матрица паритета

Полная построчная инвентаризация R4 находится на странице [R4 feature matrix](./r4-feature-matrix.md). Таблица ниже остаётся краткой сводкой по областям; её строки не считаются закрытыми до выполнения соответствующих gate полной матрицы.

| Область | Прототип | Требование для parity |
| --- | --- | --- |
| Backend | Vulkan/D3D12 device и базовый present | Одинаковая функциональность, validation без ошибок |
| Geometry | Часть static level geometry | Все vertex factories, LOD, culling и dynamic/skinned content |
| Materials | Legacy masters, pre-authored instances и dynamic bridge; editor scene передаёт texture/parameter block через material ABI, игровой prototype bridge пока передаёт только первую texture | Полный runtime parameter set, bundle pipelines, graph/HLSL, cooker и legacy coverage |
| Lighting | Native editor Directional/Point/Spot lights и bindless Forward PBR до 64 records; G-buffer foundation без MRT/deferred pass | Игровой light upload, deferred PBR, clustered lights, IBL и shadows |
| Transparency | Нет общего material path | Forward translucent/additive/modulate |
| World | Большинство методов заглушено | Weather, vegetation, particles, decals, water и sky |
| Postprocess | Bindless tone-map HLSL без runtime pipeline | HDR pipeline и все обязательные R4 effects |
| Tools | Cooker/compiler готовы, editor renderer жёстко связан с legacy D3D9/R4 | Renderer-neutral xrECore, Tiramisu viewport, node editor, preview, diagnostics, bundles и reports |
| Stability | Resize/recreate проверен normal/ASan на Vulkan/D3D12 | Restart/device-loss и 30 минут flythrough без ошибок |

## Автоматические проверки

> Обязательное правило: каждый тестовый запуск движка, LevelEditor, ShaderEditor и отдельного GPU runner выполняется только с `-rdbg`. Для D3D12 к той же команде добавляется `-dx12`; запуск без `-rdbg` не закрывает acceptance criteria. CPU test executables также получают `-rdbg`, даже если конкретный тест пока не использует этот аргумент.

CPU CTest target `xrRenderTiramisuTests` не вводит новую внешнюю test dependency.

- [x] JSON parsing, version migration и invalid assets.
- [x] Parent cycles, override type mismatches и instance inheritance.
- [x] Generation-counted handles и dynamic/static update rules.
- [x] Deterministic pipeline keys и material bundle serialization.
- [x] Graph link/type validation, cycle detection, constant folding и DCE.
- [x] Missing/error material и legacy fallback order.
- [x] Stable parameter GUID при rename master parameter.
- [x] Master/instance editor document round-trip, typed override validation и undo/redo.
- [x] Invalid JSON syntax regression tests для master, instance, graph и legacy map.
- [x] Wrong-type JSON regression tests и atomic master/instance save replacement/failure tests.
- [x] Golden HLSL generation test.
- [x] Hand-written/graph bytecode equivalence tests для одинакового `EvaluateMaterial` contract.
- [x] Компиляция всех текущих validation permutations в DXIL и SPIR-V.
- [x] Компиляция всех текущих production pixel pass templates в DXIL и SPIR-V.
- [x] Проверка bundle v2 metadata и полного cooker-набора из 200 shader blobs.
- [x] Legacy bridge: выбор pre-authored parent instance, static switches, dynamic inheritance и deterministic cache key.
- [x] Shader foundation: G-buffer, Directional/Point/Spot Forward PBR
  lighting, tone mapping, `skeletal` и `decal_projector` vertex factories
  компилируются в DXIL и SPIR-V; source tests фиксируют ABI v5, 160-byte draw
  record, 64-byte palette/light strides и bindless loads.
- [x] Thread-affinity contract: game/render/worker cases, FIFO/drain/clear и concurrent producers render-command queue.
- [x] Static mesh CPU model: LOD resources, sections, material slots и проверка `FMeshBatch` ranges.
- [x] Standalone OGF/OMF CPU loader: synthetic static/progressive/hierarchy/motion, invalid FVF/index/type/payload, real bind-pose fixture и обязательный `stalker_bandit_1.ogf` + `stalker_animation.omf`; проверяется изменение walk palette между двумя timestamps.
- [x] Model-aware picker: renderer повторно публикует expanded OGF packet в общий scene picker; real skeletal fixture ray test и normal/ASan Vulkan/D3D12/RenderDoc smoke требуют `ModelPickingReady`.
- [x] CPU hot reload publication success/failure с сохранением старого pipeline snapshot.
- [x] Проверка точных размеров/offsets C++ GPU ABI, включая `FMaterialLightGpuData == 64`, и deterministic parameter packing/HLSL field generation.
- [x] Компиляция legacy static geometry shaders с `NRI_INSTANCE_ID_OFFSET` и Descriptor Heap Indexing для D3D12/Vulkan.
- [x] Editor backend contract: default legacy fallback, install/restore, viewport forwarding и opaque surface (`xrEditorRenderBackendTests`).
- [x] Material preview contract: unavailable fallback, generation-counted handle lifecycle, source/update/resize/render forwarding и opaque surface (`xrMaterialPreviewRendererTests`).
- [x] GPU adapter policy: API/graphics-queue filtering, architecture/memory priority и stable tie break (`xrTiramisuAdapterSelectionTests`).
- [x] Все CPU/compiler/cooker/editor executables запускаются с `-rdbg`; два CMake wrapper требуют и пересылают `TEST_DEBUG_FLAG=-rdbg`. Normal и ASan CTest прошли 48/48.
- [x] Native scene tests: StaticMesh v2 JSON/BIN и legacy v1 round-trip, corrupt/missing/hash-mismatch payload, RenderScene v1 compatibility/v2 Light round-trip, invalid type/range/color/cones, cross-type duplicate GUID и deterministic conversion dump v2.
- [x] Native scene document tests: point/rectangle/range/invert selection, bounds, visibility, transforms, undo/save, StaticMesh clipboard/material lifecycle и native Light create/details/continuous edit/Cut/Copy/Paste/duplicate/remove/reload с cross-scene GUID remap и automatic v1 → v2 upgrade.
- [x] Legacy MaterialInstance migration tests: stable key/GUID, relative asset path, reuse после перезапуска service и migration database.
- [x] Legacy conversion editor smoke normal/ASan × Vulkan/D3D12: failed load dump, real `.object` dedup/reimport и `.level` → native RenderScene.
- [x] Полный `zaton.level` со всеми sibling `.part` сконвертирован и повторно загружен в D3D12 ASan-сборке: 426 meshes, 5536 components, 396 created/15487 reused material bindings.
- [x] Совместный Material Preview + viewport material reload GPU smoke в normal/ASan на Vulkan и D3D12 с `-rdbg`; CPU pick, opaque/translucent/additive materials, реальный `stalker_bandit_1.ogf` с `norm_walk_fwd_0`, 2 skeletal draw-part, 47 current + 47 previous GPU matrices и изменившаяся palette, Directional+Point light, overlays и reload подтверждены, exit 0, ASan и NRI/API validation errors отсутствуют.
- [x] RenderDoc 1.45/API 1.7 bootstrap через `-renderdoc` проверен в normal/ASan на Vulkan и D3D12 с обязательным `-rdbg`; конфликтующие NRI/API validation layers подавляются, внутренний Breakpad handler уступает exception handling `xrDebug`/Rider, shader debug info сохраняется, F12 output направлен в `logs/renderdoc`.
- [x] Focused editor-preview hot reload через dependency watcher, DXC worker и render-thread NRI pipeline creation с сохранением last-good pipeline.
- [ ] Production material hot reload для полного renderer pass/pipeline set.
- [ ] 🟡 GPU scenes: static, skinned, masked, translucent, emissive, foliage и UI; static и animated skinned editor smoke уже покрыты, остальные reference scenes не закрыты.
- [ ] Vulkan/D3D12 API validation runner.
- [ ] Automated flythrough, weather transitions и restart; отдельный
  resize/recreate smoke уже пройден normal/ASan на Vulkan/D3D12.

## Acceptance gates

Перед включением Tiramisu по умолчанию одновременно выполняются условия:

- [ ] Cooked build не содержит runtime JSON/HLSL compilation.
- [ ] Штатный контент не содержит unmapped legacy materials или error material.
- [ ] P95 CPU/GPU frame time не хуже R4 более чем на 10% при одинаковых настройках.
- [ ] VRAM не превышает R4 более чем на 15%.
- [ ] Deterministic material scenes совпадают между Vulkan и D3D12 в утверждённом image-diff допуске.
- [ ] 30-минутные automated flythrough завершаются без crash, hang и validation errors.

## Ограничения roadmap

- первая документация выпускается на русском;
- node editor обязателен, но следует после runtime compiler и cooker;
- graph ограничен material expressions и не является full shader graph;
- Vulkan и D3D12 равноправны;
- render materials не заменяют физическую библиотеку game materials;
- `TiramisuLegacyScene` остаётся адаптером старых `.level`/OGF для Tiramisu и не заменяет целевой новый scene format;
- совместимость режима `-tiramisu-editor` LevelEditor с legacy D3D9 renderer не требуется, но игровой R4 остаётся fallback;
- legacy content мигрирует постепенно через adapter/fallback.
- standalone legacy ShaderEditor не подключается к Tiramisu; новый Material Editor живёт в LevelEditor и использует его только как UX-референс.
