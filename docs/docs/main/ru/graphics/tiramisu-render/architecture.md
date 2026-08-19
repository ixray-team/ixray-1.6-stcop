# Архитектура Tiramisu Render

> Статус: описание прототипа и целевой архитектуры. Обновлено 19 августа 2026 года.

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
       LevelEditor UI
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

Renderer-neutral CPU picking использует persistent CPU mesh cache из тех же scene snapshots. Тот же contract передаёт depth-tested world-space debug lines/triangles, screen-space overlay triangles в NDC и owned text labels. В `-tiramisu-editor` redraw ответвляется до `CEditorRenderDevice::Begin`, `RCache`, `CRender::Render` и `EScene::Render`: LevelEditor обновляет CPU camera, добавляет grid, сохранённый `m_DebugDraw` и selection rectangle в snapshot, а NRI backend `xrRenderTiramisu` выполняет viewport draw, ImGui submit и единственный Present. Незавершённый capture явно отбрасывается при смене editor state.

Startup этого режима также не вызывает `InitRenderDeviceEditor` и не создаёт legacy D3D11 `CRHI`, `CResourceManager`, render targets, vertex buffers или editor shaders. Загрузка старых `.object`/`.level` остаётся CPU-only import path: surface metadata, mesh geometry, details и wallmark slots читаются без legacy GPU allocation, после чего importer пишет native assets. Глобальные `RenderFactory`, `UIRender` и `DRender` остаются реализациями `xrRenderTiramisu`, а не перезаписываются `xrECore`.

Decal path также принадлежит `xrRenderTiramisu`, а не LevelEditor. Общий
scene contract передаёт `FEditorDecalInstance`: stable object/material IDs,
канонический projector transform, sort order и selection flag. Renderer
создаёт depth SRV, резервирует отдельный bindless descriptor каждому viewport,
публикует draw record до записи command buffer и выполняет порядок
`geometry → depth SRV → decals → depth attachment → editor overlays`.
Legacy `ESceneWallmarkTool` вычисляет projector basis из сохранённых позиций и
UV только в adapter; его старые clipped triangles в Tiramisu не отрисовываются.
Native schema хранит тот же смысл напрямую как `decal_components` RenderScene
v3. Полная миграция Wallmarks в эти компоненты с audit dump остаётся отдельным
шагом importer, поэтому runtime adapter пока не считается завершённой
контентной миграцией.

Оставшаяся работа относится не к параллельному renderer, а к editor services. Первый particle service уже находится в `xrRenderTiramisu`: он различает original/extended `particles.xr`, читает loose `.pe/.pg/.pac`, публикует owned snapshot, принимает particle instances через scene mailbox и не создаёт `CPSLibrary` shaders. Effect definition сохраняет compiled PAPI actions и sprite metadata; group definition — расписание root effects и child references. Render thread владеет отдельным simulation state каждого object ID, выполняет ограниченный fixed-step update, запускает enabled group entries по `time0/time1`, создаёт related/free child states для `on play/birth/death`, обновляет frame animation и строит vertices с velocity/path/world/face alignment, UV и vertex color. Texture использует тот же DescriptorHeapIndexing material ABI, buffer заменяется renderer-ом и удаляется deferred. `CViewportParticle` использует отдельный viewport ID и renderer-owned surface без legacy model pool. До полного particle parity остаются collision/culling/distortion/soft-particle варианты. Остальные gizmo/object packets должны переходить через тот же контракт LevelEditor. Игровой R4 остаётся отдельным fallback.

Material Editor находится в LevelEditor и использует существующий
`src/Editors/ShaderEditor` только как визуальный ориентир для layout и
workflow. Сам legacy ShaderEditor не подключается к Tiramisu и не входит в
composition root нового редактора. Material Editor не наследует
`CSHEngineTools`, `IBlender` или legacy shader serialization: его данные —
`FMaterialGraph` и material assets из `xrTiramisuMaterialCore`, а preview
подключается через `IMaterialPreviewRenderer`.

`xrEUI` теперь разделяет ImGui frontend и renderer backend через `IXrUIRendererBackend`. Встроенный DX9 backend остаётся временным fallback. Реализация LevelEditor `TiramisuEditorRenderBridge` находится в `xrRenderTiramisu`, использует единый `TiramisuRenderDevice` и общий streamer, а сам владеет только editor swapchain, ImGui instance, тремя command contexts, timeline fence и ресурсами editor surfaces. Для Vulkan и D3D12 используется тот же официальный NRI ImGui path, что и в игровом Tiramisu. Zero-size ImGui frames при создании, минимизации или восстановлении окна отбрасываются до записи command buffer, поскольку Vulkan/NRI не допускает viewport нулевой ширины. При resize bridge ждёт graphics queue, пересоздаёт swapchain/targets и атомарно перенаправляет descriptor старого viewport surface в уже собранном `ImDrawData` на новый descriptor. Renderer-neutral lifecycle status публикует только размеры и revision/counters; NRI-объекты остаются внутри DLL. Автоматический `-editor-test-hidden` не создаёт splash и не активирует главное окно: non-focusable HWND расположен за пределами рабочего стола, но остаётся видимым для DXGI/Vulkan swapchain.

Presentation работает в две фазы: `EndFrame` на game thread завершает
построение `ImDrawData`, legacy scene закрывается без DX9 present, затем
`PresentMainFrame` синхронно передаёт draw data общей очереди render commands.
Выделенный render thread `xrRenderTiramisu` обрабатывает scene/mailbox,
записывает NRI commands и выполняет единственный present главного окна. До
введения тройного immutable UI packet game thread ждёт завершения этой команды,
поэтому следующий ImGui frame не может перезаписать используемый `ImDrawData`.
LevelEditor устанавливает этот путь по умолчанию; прежний флаг
`-tiramisu-editor` сохранён как совместимый необязательный аргумент существующих
launch-конфигураций. Vulkan является default API, `-dx12` выбирает D3D12.
Viewport, thumbnails и icons на новом пути
публикуются через renderer-owned texture handles/registry. Оставшиеся
незарегистрированные legacy user-image commands безопасно заменяются white
descriptor и никогда не передаются GPU как raw DX9 pointers; подмена типов
через cast запрещена.

Первый scene-срез этой границы реализован для главного viewport. Тот же backend реализует `IEditorRenderBackend`, принимает resize/capture и renderer-neutral snapshot без NRI типов. Snapshot содержит camera, changed static-mesh uploads, sections/material-slot IDs, visible instances/transforms, standalone OGF model instances, Directional/Point/Spot lights, removed mesh IDs и revisioned debug line/triangle lists; backend обязан скопировать spans до возврата. Thread-safe mailbox проверяет finite geometry/radiometry, model asset name, max 64 lights, positive local-light range, spot cone и global object/light GUID constraints, валидирует пакет транзакционно и передаёт owned data стороне записи GPU commands. Повторяющийся instance `ObjectId` допустим для нескольких editable meshes одного legacy object.

Standalone OGF остаётся renderer-owned asset path: LevelEditor передаёт только
имя visual, необязательный `startup_animation`, transform, object ID и flags.
`xrRenderTiramisu` нормализует путь, bounded worker читает и разбирает
static/progressive/embedded hierarchy либо skeletal 1–4-weight draw-parts,
а render thread публикует cache entry, создаёт stable mesh/material IDs и NRI
buffers. Для skeletal vertices loader сохраняет bone indices и проверенные
нормализованные weights, читает bone hierarchy/bind transforms и вычисляет
inverse-bind matrices. Renderer-owned pose builder уже строит конечную
`current-model × inverse-bind` palette из массива local transforms. Pending
instance сохраняется в owned scene packet и автоматически разворачивается
после готовности job. OGF/OMF parser читает embedded/external motions и
семплирует current/previous pose. Render thread упаковывает обе палитры в
bindless `ByteAddressBuffer`, записывает offsets/count в `FMaterialDrawGpuData`
ABI v5 и выбирает отдельную `skeletal` material permutation. Общий
`MaterialSkeletalVertexFactory` выполняет до четырёх bone influences и
предыдущую деформацию для будущего velocity pass; LevelEditor не видит NRI
buffer, descriptor index или внутренний формат skeletal vertex.

Expanded OGF mesh updates/instances также атомарно передаются в существующий
`TiramisuEditorViewportScenePicker`. Он хранит CPU mesh cache под mutex и
возвращает тот же Spawn object ID, поэтому LevelEditor не содержит отдельной
логики ray intersection для OGF.

Остальные Spawn-представления не вводят собственных passes: attached
`CEditShape` проходит через общий shape debug packet, а `idle_particles` — через
`FEditorParticleInstance` и renderer-owned particle catalog/simulation. На
Tiramisu path LevelEditor хранит только source names и не создаёт legacy
`IRenderVisual` для OGF или idle particle. Незакрытой Spawn-частью остаются
специализированные object/gizmo packets и полный authoring UI анимаций, а не
базовая OGF/OMF GPU-деформация.

Legacy `EScene` остаётся загрузчиком старого editor content и через migration bridge преобразует `CSceneObject/CEditableMesh` в тот же контракт, который выдаёт новый scene/static-mesh/light component path. Тот же bridge преобразует сохранённое направление legacy-солнца и видимые `CLight` с включённым `m_UseInD3D` в Directional/Point/Spot records; spot cone переводится из полного D3D-угла в half angle, selection и cast-shadow metadata сохраняются. Legacy `CEditShape`, `ESoundEnvironment` и `CPuddle` sphere/box используют общие `du_sphere/du_box` topology данные только на CPU: bridge применяет shape и object transforms, ограничивает объём пакета и передаёт wire/solid vertices в renderer-neutral debug lists. Для `ESoundSource` в тот же line packet попадают min/max distance spheres либо компактный icon sphere. `CPortal` публикует замкнутый wire contour, двухсторонний полупрозрачный triangle fan и normal marker; приватные legacy sector colors в новый render contract не протекают. NRI backend лениво создаёт device-local `RGBA8` color target, `D32` depth target и views, загружает изменённые meshes, разрешает legacy shader/texture через pre-authored material instances и асинхронно создаёт настоящий Forward pipeline. Pass учитывает two-sided/blend mode и использует общий GGX/Smith/Schlick BRDF. До 64 Directional/Point/Spot lights загружаются в отдельный bindless `ByteAddressBuffer`; point использует smooth range falloff, spot — inner/outer cone. При отсутствии lights остаётся временный hardcoded sun fallback. Shadow flag переносится в GPU record, но shadow maps/passes пока отсутствуют. Debug shader остаётся fallback при первой сборке или ошибке. `editor\spawn_icon` разрешается в translucent/unlit master; `editor\particle_translucent`, `editor\particle_additive` и `editor\glow_sprite` — в translucent/additive unlit masters, а texture path передаётся как bindless `BaseTexture` override. Выбранные instances повторно рисуются отдельным wireframe pipeline с depth test без depth write. CPU picker использует persistent mesh cache, поддерживает transforms и возвращает ближайший triangle вместе с object/mesh/material IDs. Сохранённый `m_DebugDraw` и common world-space `DU_impl` primitives преобразуются в цветные line/triangle vertex lists и рисуются depth-tested NRI pipelines с alpha blending; selection rectangle и object-axis lines используют следующие screen-space pipelines без depth test. Text mailbox валидирует finite position/colors, ограничивает длину, владеет строками и отдаёт UI безопасную копию для ImGui composition. Общий GPU buffer заменяется по deterministic revision и удаляется deferred. Color переводится в `SHADER_RESOURCE` для ImGui, а заменённые mesh buffers/pipelines освобождаются после timeline fence. Resize/recreate acceptance пройден на normal/ASan Vulkan/D3D12; restart/device-loss acceptance ещё не завершён.

Editor material GPU ABI v5 разделён на три frame regions.
`NRI_BASE_INSTANCE` передаёт абсолютный индекс draw record в
`FMaterialDrawGpuData`; 160-байтный record ссылается на material instance,
current/previous palette offsets и bone count, а decal record переиспользует
неактивные skinning поля для depth descriptor и world-to-decal. Отдельные
descriptor indices выбирают parameter, light и skinning `ByteAddressBuffer`;
scene constants дополнительно содержат inverse view-projection. Каждая
light/palette matrix запись занимает 64 байта. Draw/instance/parameter/light/
palette данные и viewport constants обновляются только после ожидания fence
переиспользуемого frame context. Preview и каждый main viewport используют
непересекающиеся диапазоны. X-Ray `Fmatrix` остаётся row-vector типом на CPU;
current/previous transform, world-to-decal и bone matrices транспонируются
только при записи в построчно восстанавливаемый HLSL buffer. Cbuffer и root
constants сохраняют исходную память `Fmatrix`, поскольку их column-major
packing выполняет DXC.

Editor NRI backend использует общий render thread `xrRenderTiramisu`. До запуска
выделенного потока game thread временно выполняет роль render thread и создаёт
shared NRI device вместе с базовыми ресурсами самого `Render->create()`. После
запуска renderer вызов
`InitializeRendererResources` ставит команду создания swapchain, frame
contexts, ImGui backend, viewport/preview pipelines и GPU resources на render
thread. Каждый editor frame и resize выполняются там же с
`CheckIsRenderThread`. SDL window handle и pixel size считываются только game
thread и передаются consumer через атомарное presentation state.

Остановка выполняется в обратном порядке: editor GPU resources удаляются
render command, затем останавливается общий renderer, и только после этого
`FinalizeRendererShutdown` освобождает shared device. Scene submission до
возврата копируется в immutable owned packet. Текущая синхронная передача
`ImDrawData` уже соблюдает thread contract, но остаётся временной до введения
трёх независимых immutable UI packets.

Legacy `CGlow` не вызывает `ref_shader`, `RCache` или `CTLSprite` в Tiramisu composition root. Bridge строит общий unit quad, передаёт texture через material slot `editor\glow_sprite`, а camera-facing transform, fixed/world size, selection и object ID хранит в обычном scene instance. NRI mesh, pipeline и descriptors создаются и удаляются только renderer thread.

Legacy `CSpawnPoint` публикует renderer-neutral fallback icon, overlay label,
selection bounds и EnvMod radius. Entity visual передаётся как standalone OGF
instance, attached shape использует общий shape packet, а idle particle —
общий particle packet. OGF/OMF animation выполняется renderer-side; остаются
специализированные authoring controls и остальные object packets.

AI Map больше не требует `ref_geom`, `ref_shader` и dynamic legacy vertex stream в Tiramisu viewport. `ESceneAIMapTool` отдаёт ограниченный `m_VisRadius` набор из spatial hash, bridge строит plane-projected node quads и уникальные link lines, а renderer получает обычные debug triangles/lines. Линейный fallback разрешён только малому ещё не синхронизированному набору узлов.

Legacy `CWayObject/CWayPoint` публикует point crosses, одно отображение reciprocal link и labels через те же renderer-neutral line/text lists. Односторонняя связь не теряется из-за pointer ordering, а приватные authoring collections открыты bridge только через read-only accessors.

Grouped objects остаются в обычных `EScene` class lists, поэтому bridge не дублирует их geometry. Общая проверка видимости проходит цепочку `GetOwner()` и учитывает `ESceneToolBase::flVisible`; скрытая Group или скрытый tool исключает child packets. Выбранная `CGroupObject` отдельно публикует только renderer-neutral bounds.

Выбранный legacy `CSector` также не создаёт editor-local GPU resources. Bridge читает его итоговый world-space `Fbox`, публикует 12 renderer-neutral линий и включает их в deterministic debug revision. Цвет выбранного sector box остаётся белым, locked box — красным; приватный `sector_color` через границу редактора не передаётся. Portal contour и sector bounds уже видны в Tiramisu viewport, но runtime portal traversal, indoor/outdoor visibility и OCC этим диагностическим пакетом не реализуются.

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
