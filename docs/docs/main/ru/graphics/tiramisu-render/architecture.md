# Архитектура Tiramisu Render

> Статус: описание прототипа и целевой архитектуры. Обновлено 23 июля 2026 года.

## Границы подсистем

Tiramisu остаётся renderer-модулем, а material asset model и compiler выделяются в независимый от NRI модуль `xrTiramisuMaterialCore`. Этим модулем совместно пользуются renderer DLL, cooker, Level Editor и автоматические тесты.

```text
Game thread
  -> render commands / scene updates
Render thread
  -> scene proxies
  -> material pass proxies
  -> render graph
  -> NRI resources and pipelines
  -> Vulkan or D3D12

Level Editor ----\
Cooker -----------+-> xrTiramisuMaterialCore -> typed IR -> HLSL
Renderer DLL ----/
```

NRI-типы не должны попадать в публичное API `xrTiramisuMaterialCore`, assets или editor model.

## Интеграция xrECore и LevelEditor

До начала миграции editor renderer не являлся сменяемым backend: `xrECore` компилировал части D3D9/R1/R4 renderer прямо в библиотеку, `CEditorRenderDevice` публиковал D3D9 resources, а `UIRenderForm` напрямую показывал legacy render target через raw ImGui texture. Поэтому Tiramisu не встраивается как ещё одна ветка внутри legacy `CRender`.

Миграция выполняется через renderer-neutral editor contract:

```text
LevelEditor / ShaderEditor UI
           |
           v
  xrECore editor render contract
       /                 \
legacy adapter       Tiramisu adapter
EDevice/R4            renderer DLL/NRI
       \                 /
        viewport surface + picking/debug API
```

Первый срез уже реализован: `UIRenderForm` получает opaque ImGui surface и вызывает capture/resize через `IEditorRenderBackend`, а встроенный legacy adapter сохраняет прежний `EDevice`/R4 путь по умолчанию. Контракт допускает временную установку другого backend с явным восстановлением предыдущего; это проверяется отдельным CTest.

Для material preview добавлен отдельный `IMaterialPreviewRenderer`. Он принимает сериализованные master/instance assets и generated HLSL только на время вызова, использует generation-counted handle и возвращает opaque ImGui surface вместе с revision/state/diagnostic. Default implementation безопасно сообщает `Unavailable`; forwarding и lifecycle проверяет `xrMaterialPreviewRendererTests`. Реальная NRI-реализация Tiramisu подключена в LevelEditor: background DXC, NRI pipeline/offscreen target, bindless Texture2D/TextureCube cache, environment lighting и безопасное сохранение старого pipeline при ошибке rebuild.

Общие renderer-neutral интерфейсы viewport и material preview находятся в `src/Include/xrRender/EditorRenderer.h`. Поэтому renderer DLL может реализовать их без зависимости от `xrECore`, а editor не включает NRI headers.

Renderer-neutral CPU picking уже переведён на contract и использует persistent CPU mesh cache из тех же scene snapshots; legacy backend сохраняет старый точный picking. Тот же contract передаёт depth-tested world-space debug lines/triangles, screen-space overlay lines/triangles в NDC и owned text labels. `xrECore` открывает capture на время editor redraw и параллельно с неизменённым legacy draw собирает common `DU_impl` primitives: lines/crosses, point/line/triangle lists, strips и fans, indexed faces, grid, selection boxes, prebuilt sphere/box/cone/cylinder gizmos, object-axis lines и подписи. `DrawEntity` дополнительно создаёт owned transient static mesh со стабильными mesh/material IDs, texture name и transform: migration bridge объединяет его с обычными static meshes, поэтому spawn icon проходит через тот же mailbox, material resolver, DescriptorHeapIndexing и Forward pipeline. Glow использует общий immutable quad и меняет camera-facing instance transform; editor particle renderer копирует уже рассчитанные CPU billboard vertices до unlock legacy dynamic VB. Оба пути публикуют стабильный object ID и bindless texture override в том же transient mesh packet. Selection rectangle добавляется до scene submission, потому что его legacy draw расположен позже `Tools->Render()`. После `Scene->Render`, cursor и сохранённого `m_DebugDraw` capture закрывается, migration bridge добавляет все списки в один owned scene snapshot; NRI backend рисует world-space primitives с depth test, overlays — после них без depth test, а UI копирует labels и композит их ImGui поверх renderer-owned image. Corner axis использует ImGuizmo и не зависит от legacy model. Далее переводятся полный lifecycle render targets и целевой новый scene path. После этого Tiramisu становится единственным composition root для LevelEditor/ShaderEditor; сохранять legacy D3D9 renderer в editor build не требуется, тогда как игровой R4 остаётся отдельным fallback. Editor-код не получает D3D9 или NRI handles; для ImGui он использует opaque viewport surface handle, создаваемый Tiramisu adapter.

Material Editor использует layout и workflow существующего `src/Editors/ShaderEditor`, но не наследует `CSHEngineTools`, `IBlender` или legacy shader serialization. Его данные — `FMaterialGraph` и material assets из `xrTiramisuMaterialCore`; preview подключается через `IMaterialPreviewRenderer`.

`xrEUI` теперь разделяет ImGui frontend и renderer backend через `IXrUIRendererBackend`. Встроенный DX9 backend остаётся временным fallback. Реализация LevelEditor `TiramisuEditorRenderBridge` находится в `xrRenderTiramisu`, использует единый `TiramisuRenderDevice` и общий streamer, а сам владеет только editor swapchain, ImGui instance, тремя command contexts, timeline fence и ресурсами editor surfaces. Для Vulkan и D3D12 используется тот же официальный NRI ImGui path, что и в игровом Tiramisu.

External presenter работает в две фазы: `EndFrame` завершает построение `ImDrawData`, legacy scene закрывается без DX9 present, затем `PresentMainFrame` записывает NRI commands и выполняет единственный present главного окна. Путь устанавливается opt-in флагом `-tiramisu-editor`; Vulkan является default API, `-dx12` выбирает D3D12. Viewport, thumbnails и icons на новом пути публикуются через renderer-owned texture handles/registry. Оставшиеся незарегистрированные legacy user-image commands безопасно заменяются white descriptor и никогда не передаются GPU как raw DX9 pointers; подмена типов через cast запрещена.

Первый scene-срез этой границы реализован для главного viewport. Тот же backend реализует `IEditorRenderBackend`, принимает resize/capture и renderer-neutral snapshot без NRI типов. Snapshot содержит camera, changed static-mesh uploads, sections/material-slot IDs, visible instances/transforms, Directional/Point/Spot lights, removed mesh IDs и revisioned debug line/triangle lists; backend обязан скопировать spans до возврата. Thread-safe mailbox проверяет finite geometry/radiometry, max 64 lights, positive local-light range, spot cone и global object/light GUID constraints, валидирует пакет транзакционно и передаёт owned data стороне записи GPU commands. Повторяющийся instance `ObjectId` допустим для нескольких editable meshes одного legacy object.

Legacy `EScene` остаётся загрузчиком старого editor content и через migration bridge преобразует `CSceneObject/CEditableMesh` в тот же контракт, который выдаёт новый scene/static-mesh/light component path. NRI backend лениво создаёт device-local `RGBA8` color target, `D32` depth target и views, загружает изменённые meshes, разрешает legacy shader/texture через pre-authored material instances и асинхронно создаёт настоящий Forward pipeline. Pass учитывает two-sided/blend mode и использует общий GGX/Smith/Schlick BRDF. До 64 Directional/Point/Spot lights загружаются в отдельный bindless `ByteAddressBuffer`; point использует smooth range falloff, spot — inner/outer cone. При отсутствии lights остаётся временный hardcoded sun fallback. Shadow flag переносится в GPU record, но shadow maps/passes пока отсутствуют. Debug shader остаётся fallback при первой сборке или ошибке. `editor\spawn_icon` разрешается в translucent/unlit master; `editor\particle_translucent`, `editor\particle_additive` и `editor\glow_sprite` — в translucent/additive unlit masters, а texture path передаётся как bindless `BaseTexture` override. Выбранные instances повторно рисуются отдельным wireframe pipeline с depth test без depth write. CPU picker использует persistent mesh cache, поддерживает transforms и возвращает ближайший triangle вместе с object/mesh/material IDs. Сохранённый `m_DebugDraw` и common world-space `DU_impl` primitives преобразуются в цветные line/triangle vertex lists и рисуются depth-tested NRI pipelines с alpha blending; selection rectangle и object-axis lines используют следующие screen-space pipelines без depth test. Text mailbox валидирует finite position/colors, ограничивает длину, владеет строками и отдаёт UI безопасную копию для ImGui composition. Общий GPU buffer заменяется по deterministic revision и удаляется deferred. Color переводится в `SHADER_RESOURCE` для ImGui, а заменённые mesh buffers/pipelines освобождаются после timeline fence. При resize выполняется ожидание graphics queue, unregister и пересоздание targets. Restart/device-loss acceptance ещё не завершён.

Editor material GPU ABI v2 разделён на три frame regions. `NRI_BASE_INSTANCE` передаёт абсолютный индекс draw record в `FMaterialDrawGpuData`; record ссылается на `FMaterialInstanceGpuData`, тот — на упакованный parameter block с bindless indices из `ResourceDescriptorHeap` и `SamplerDescriptorHeap`. Отдельный `LightDataBufferIndex` выбирает `ByteAddressBuffer`, а `LightDataOffset`/`LightCount` адресуют диапазон текущего viewport/frame. Каждый `FMaterialLightGpuData` занимает 64 байта. Draw/instance/parameter/light данные и viewport constants обновляются только после ожидания fence переиспользуемого frame context. Preview и каждый main viewport используют непересекающиеся диапазоны. X-Ray `Fmatrix` остаётся row-vector типом на CPU; только current/previous transform, записываемый в построчно восстанавливаемый HLSL `ByteAddressBuffer`, транспонируется через общий `xrTiramisuMaterialCore` helper. Cbuffer и root constants сохраняют исходную память `Fmatrix`, поскольку их column-major packing выполняет DXC.

Текущий editor NRI backend является scene/presentation foundation, а не завершённой render-thread моделью. Scene submission уже копируется в immutable owned packet, однако запись NRI commands пока выполняется внешним presenter path. Перед включением по умолчанию consumer packet должен быть перенесён на editor render thread; NRI create/destroy/submit будут проверяться теми же thread-affinity правилами, что игровой renderer.

## Threads и владение ресурсами

Game thread управляет игровыми объектами и публикует неизменяемые данные для renderer через render commands. Render thread единолично создаёт и уничтожает GPU resources, descriptor allocations и pipelines.

В текущем migration slice legacy material loader выполняет parsing/resolution и создаёт CPU dynamic instance на game thread. Конструктор render proxy только ставит команду и не вызывает NRI. Создание/регистрация pipeline выполняется `Initialize_RenderThread`; пока команда не выполнена, `ResolvePass` возвращает отсутствие готового pass. Registry, material GPU storage и render passes проверяют thread affinity через `CheckIsRenderThread`, а loader/managers — через `CheckIsGameThread`. Контракт и FIFO/drain semantics очереди покрыты отдельными CTest.

Проверка game thread использует точное сравнение с `GGameThreadId`, а не условие «любой поток, кроме render thread». Поэтому background compiler/worker не может случайно вызвать game-thread API. `GRenderThreadId` и флаг остановки render thread атомарны. NRI create/destroy/update paths в texture/material/resource proxies и scene upload дополнительно проверяют `CheckIsRenderThread`; при отключённом отдельном render thread его роль выполняет только game thread.

Публичные material API используют generation-counted handles. Raw render-thread pointers не передаются game/editor code. Удаление GPU resources выполняется отложенно, после завершения всех кадров, которые могли на них ссылаться.

Основной Tiramisu path уже содержит три command frame context и ждёт соответствующее fence value перед повторным использованием allocator/buffer. Добавлена общая fence-aware deferred deletion queue; первым реальным потребителем стал release owned material pipelines. Целевая завершённая модель дополнительно переносит в каждый context transient descriptor ranges, upload allocations и query data, а все texture/buffer/descriptor destroy paths выполняет только после безопасного fence.

## Validation, GPU markers и статистика

Общая debug policy требует точный аргумент `-rdbg`. Без RenderDoc он включает NRI/API validation; при активном `-renderdoc` конфликтующие validation hooks подавляются до создания device, но DXC debug info сохраняется. Такое подавление является отдельным явно логируемым режимом, а не успешным validation run.

Основные игровые command buffers размечены annotations `Main`, `DeferredPass`, `UI`, `ImGui` и `SwapChain`. Editor backend использует `Editor.SceneViewport`, `Editor.MaterialPreview` и `Editor.ImGui`. Маркеры не меняют владение ресурсами и доступны одинаково Vulkan/D3D12 через NRI.

`FRenderStatisticsTracker` — NRI-независимый versioned контракт. Каждый завершённый кадр публикует CPU duration, pass/draw/dispatch count, triangles, lines и upload bytes. Resource census содержит только явно отслеживаемые buffers, textures, pipelines, descriptors, deferred resources и известные allocation bytes. Эти поля намеренно называются `Tracked`: они не являются значением driver VRAM/residency. `GpuTimingValid=false` означает, что timestamp queries ещё не собраны; в таком snapshot GPU time равен нулю и не должен использоваться для performance acceptance.

Игровой renderer формирует snapshot только на render thread и публикует копию под mutex для game thread. Editor backend возвращает renderer-neutral snapshot через `IEditorRenderBackend::GetRenderStatistics`; NRI pointers через этот API не выходят. Отдельный CTest проверяет reset/accumulation/validity semantics, а deterministic GPU smoke требует ненулевые revision, passes, draws и resource census до объявления успеха.

## Scene proxies

Игровой объект не рендерится напрямую. Для него создаётся компактный scene proxy с необходимыми renderer данными. Геометрия публикуется как `FMeshBatch` с vertex factory, material handle, transforms, visibility flags и диапазоном primitives.

Per-draw data содержит current/previous transform, material instance index, object ID, vertex factory, geometry ranges и flags для depth, shadow, G-buffer или forward pass.

Первый переход уже выполнен: прежний `FRenderMeshBath` заменён на `FMeshBatch`, а static mesh render data содержит LOD resources, sections и material slots. Это пока минимальный CPU model: vertex factories, transforms, LOD selection и production visibility lists ещё не завершены.

`TiramisuLegacyScene` остаётся content adapter для старых `.level`, OGF, sectors/portals и legacy visual hierarchy. Он преобразует старые render items в `FMeshBatch` и не является целевой scene architecture. Игровой R4 продолжает читать старый контент своим независимым путём.

Целевой editor content path начат в независимом `xrTiramisuSceneCore`. Versioned `StaticMesh` v2 состоит из текстового `*.static-mesh.json` и binary `*.static-mesh.bin`: JSON хранит GUID, material slots, sections и описание geometry payload, а BIN — плотные vertices/indices. Binary header фиксирует magic/version/endian, offsets, strides, counts, file size и hash; loader сверяет header, metadata и фактический payload hash. Старый inline JSON v1 читается только для миграции. `RenderScene` v2 хранит static-mesh components и native Directional/Point/Spot Light: stable GUID/name, transform, HDR linear color/intensity, range, inner/outer cone, visibility и cast-shadows metadata. V1 без lights остаётся читаемым; первая Light-операция в editor переводит документ на v2. Native document формирует тот же renderer-neutral snapshot без NRI типов.

Старые `.object`/`.level` в Tiramisu-only editor считаются import sources. Конвертер создаёт native assets и дедуплицированные MaterialInstance, не изменяя source. Каждая попытка обязана создать deterministic audit dump: success `<target>.migration.json` или failure `<target>.migration.failed.json`. В dump входят importer/version, source path/hash, target metadata/payload paths, target ID, asset/material mappings, created/reused counts и diagnostics; даже отказ `EScene` открыть или распознать level проходит через failure-dump path. При импорте level MaterialInstance создаются сразу, но растущая migration database публикуется один раз после обхода всех components. Native document поддерживает point/rectangle/range/invert selection, Focus Selected/Zoom All, transform transactions и common selection/visibility/Cut/Copy/Paste/duplicate/remove/undo/save для StaticMesh и Light. Cross-scene clipboard хранит resolved mesh data либо полный Light record, назначает новый stable GUID/name и публикует Paste одной undo-транзакцией; StaticMesh reference дополнительно rebases относительно target scene. `GetWorldBounds` кеширует local mesh AABB и использует небольшие icon bounds для Light. Native Outliner объединяет оба типа без `CCustomObject*`; Light Details редактирует type, position, HDR color/intensity, range, spot cones, visibility и cast shadows одной document-транзакцией. Остальные editor tools ещё не подключены, поэтому переходный legacy bridge сохраняется.

## Resources и descriptors

Material instance parameters размещаются в индексируемом GPU material buffer. Texture parameters хранят bindless descriptor indices, а sampler выбирается из ограниченного набора presets. Renderer отвечает за lifetime descriptors и безопасное обновление buffer между кадрами.

Shader ABI использует HLSL Descriptor Heap Indexing: resources читаются через `ResourceDescriptorHeap[resourceIndex]`, samplers — через `SamplerDescriptorHeap[samplerIndex]`. Это относится и к material textures/records, и к scene light buffer; register bindings не умножаются с числом источников света. `xrTiramisuMaterialCore` хранит типизированные indices и 64-байтный light record как backend-neutral ABI, но allocation и NRI descriptors принадлежат render thread.

Фиксированный heap на 2048 descriptors, используемый прототипом, должен быть заменён управляемыми диапазонами, диагностикой исчерпания и deferred reuse.

## Shader compiler и pipelines

Обе формы material implementation — ручной HLSL и node graph — сводятся к общему HLSL contract. Затем единый compiler выполняет DXC compilation, reflection и проверку bindings. Pipeline key детерминированно строится из material/template/implementation hashes, static parameters, vertex factory, render pass signature, backend, shader model, compiler options и hashes includes.

CPU-часть parsing, type checking, IR и DXC может работать в background threads. Создание NRI pipeline выполняется только на render thread.

Для deferred foundation добавлены engine-owned HLSL libraries: versioned G-buffer pack/unpack с octahedral normal, GGX/Smith/Schlick direct BRDF, point-light attenuation и tone mapping, адаптированный из R4. R4 resource declarations и register bindings не переносятся: новые fullscreen passes получают descriptor indices через собственный constants contract и читают `ResourceDescriptorHeap`/`SamplerDescriptorHeap`. DXIL и SPIR-V компиляция проверена тестами, но эти shaders ещё не подключены к MRT и render graph.

## Render graph

Существующие ручные barriers являются временным решением. Первый CPU-срез `TRenderGraph` уже описывает passes через читаемые и записываемые resources и вычисляет stable topological order, RAW/WAR/WAW dependencies, logical barriers, compute/graphics queue transfers, lifetime transient resources и безопасный compatibility-class aliasing памяти. Он также формирует per-pass submission plan с cross-queue waits. NRI-слой уже переводит абстрактные access states в единые для Vulkan/D3D12 `AccessLayoutStage`, texture/buffer barriers, queue ownership transfer и indirect argument state. Следующий срез создаёт реальные transient resources, command buffers и queue submissions/fences; до этого существующие GPU passes продолжают использовать ручные barriers.

Первый полноценный граф включает depth/prepass при необходимости, G-buffer, clustered light preparation, deferred lighting, forward transparency, postprocessing, UI и present.

## Безопасный hot reload

После успешной фоновой компиляции новый material proxy и pipelines подготавливаются render thread и атомарно публикуются только на границе кадра. Старый proxy удаляется отложенно. При ошибке компиляции старый рабочий proxy остаётся активным, а diagnostics возвращаются editor/console.

В текущем Material Editor этот контракт реализован и для preview, и для основной editor scene. Polling watcher preview следит за JSON/HLSL и parent-chain dependencies; scene watcher объединяет `legacy-map.json`, master/parent assets, HLSL implementation/template и engine pass includes. Resolver reload и DXC работают в background jobs, а NRI backend принимает готовый pipeline только на безопасной границе frame context. Dirty-документ защищён от неявной перезаписи; при неудачном rebuild preview и scene продолжают использовать accepted last-good revision. Подключение той же цепочки к полному игровому runtime pass set и общей deferred deletion queue остаётся отдельной задачей renderer foundation.

## Backend parity

Vulkan и D3D12 равноправны. Feature не считается готовой, пока она не проходит одинаковые material tests, representative scenes и API validation на обоих backend. Backend-specific code допускается только под общим renderer contract.

RenderDoc загружается общим `xrCore` bootstrap по `-renderdoc` до создания любого graphics device, поэтому одна реализация работает для игры и редакторов. `xrRenderDoc` не передаёт NRI-типы: он предоставляет доступ к RenderDoc API, путь capture и запрос следующего кадра. Общая `FRenderDebugPolicy` разделяет shader debug info и validation layers. При активном RenderDoc обязательный `-rdbg` сохраняет DXC debug info, но NRI/API validation автоматически подавляется, чтобы не накладывать конфликтующие Vulkan/D3D12 hooks. После получения API bootstrap отключает внутренний crash handler RenderDoc: обработку исключений сохраняют `xrDebug` и подключённый native debugger, поэтому завершение capture из `Present` не конкурирует с RenderDoc Breakpad.

Целевой visibility path строит Hi-Z из depth, выполняет occlusion culling и compact/indirect draw generation на async compute. `TRenderGraph` владеет зависимостями depth/Hi-Z/visibility/indirect buffers и синхронизацией compute → graphics. Async compute является оптимизацией, а не обязательным условием: если adapter не предоставляет подходящую отдельную queue, те же passes исполняются на graphics queue без изменения результата.
