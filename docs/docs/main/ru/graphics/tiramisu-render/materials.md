# Material-система Tiramisu

> Статус: целевой контракт; первая реализация развивается в `xrTiramisuMaterialCore`. Обновлено 23 июля 2026 года.

Material-система следует модели Unreal Engine: master material определяет shader contract и статическую конфигурацию, а instances переопределяют разрешённые параметры. Master можно реализовать вручную на HLSL или собрать в node editor. Graph не исполняется runtime: он типизируется, оптимизируется и генерирует HLSL для того же engine template.

## Source assets

Исходные assets хранятся в `$game_data$/render_materials/`:

- `*.material.json` — master material;
- `*.material-instance.json` — instance;
- `legacy-map.json` — соответствия старых shader names заранее подготовленным material instances или masters.

HLSL templates и implementations находятся в `gamedata/shaders/r5/materials/`.

Минимальная структура master material:

```json
{
  "asset_version": 1,
  "guid": "8f46e935-289d-4a8f-85d8-d882b69af78d",
  "name": "Standard Surface",
  "domain": "surface",
  "blend_mode": "opaque",
  "shading_model": "default_lit",
  "two_sided": false,
  "template": "materials/MaterialTemplate.hlsl",
  "implementation": {
    "type": "hlsl",
    "source": "materials/StandardSurface.hlsl"
  },
  "parameters": [],
  "static_parameters": [],
  "dependencies": []
}
```

Каждый asset, parameter, graph node и pin имеет стабильный GUID. Переименование отображаемого имени не изменяет identity и не ломает instance overrides.

Instance хранит parent GUID или path, runtime overrides и static overrides. Domain, blend mode и shading model наследуются и через instance не меняются. Parent chains разрешены; loader/cooker обнаруживает циклы и сохраняет flattened representation.

## Parameters и permutations

Runtime parameters: scalar, `float2`, `float3`, `float4`, color, `texture2D`, `textureCube` и sampler preset. Их изменение обновляет material buffer или descriptor index и не создаёт новый pipeline.

Static bool/enum parameters входят в `FMaterialPipelineKey` и создают отдельную shader permutation. Изменить static parameter у уже созданного dynamic instance нельзя: API возвращает типизированную ошибку.

На GPU per-instance параметры лежат в индексируемом material buffer. Texture fields содержат bindless indices. Per-draw data ссылается на material instance index.

Tiramisu shader ABI использует Descriptor Heap Indexing. Generated и hand-written HLSL получают `uint` resource/sampler indices из `MaterialParameters`, затем обращаются к `ResourceDescriptorHeap` и `SamplerDescriptorHeap`. Graph node не получает raw descriptor и не объявляет собственный register.

Текущая базовая реализация строит deterministic parameter layout в `xrTiramisuMaterialCore`, генерирует для него HLSL loader и хранит параметры в renderer-owned `ByteAddressBuffer`. Draw record содержит current/previous transform, material instance index и object ID. Для indexed draw первый draw record передаётся через `baseInstance`; shader использует `NRI_INSTANCE_ID_OFFSET`, поэтому один ABI работает с Vulkan и D3D12, включая NRI draw-parameter emulation.

Матрицы требуют явного контракта на границе CPU/GPU. `Fmatrix` X-Ray хранит row-vector transform (`[position, 1] * M`), а material vertex factory вычисляет `mul(M, position)`. Матрицы draw record загружаются из `ByteAddressBuffer` явными HLSL-строками, поэтому CPU перед записью применяет `MakeMaterialDrawBufferMatrix` и транспонирует current/previous transform. Эта операция относится только к draw buffer: обычные constant/root buffers используют column-major packing DXC и дополнительного transpose не требуют.

Material GPU ABI v4 также определяет независимые от NRI 64-байтные light
records и skinning matrices. Scene constants передают descriptor indices
light/palette buffers и inverse view-projection; `FMaterialDrawGpuData` хранит
current/previous palette offsets и bone count. Для `decal_projector` поле
previous transform содержит заранее вычисленный world-to-decal, а свободный
skinning descriptor slot — bindless index scene depth. HLSL выбирает ресурсы
через `ResourceDescriptorHeap`, а не через фиксированные registers.
Runtime-изменение света, bone palette или projector transform не создаёт
material permutation; vertex factory (`level_static`, `skeletal` либо
`decal_projector`) входит в deterministic pipeline key. Editor Forward pass
обходит не более 64 Directional/Point/Spot lights; отдельный Decal pass читает
depth и композитит color до editor overlays. Это не замена будущих clustered
lists, shadow passes и production DBuffer/G-buffer decals.

Renderer уже создаёт три начальных bindless buffer: draw data, material instance table и material parameter data. Изменённые parameter ranges обновляются отдельно. Это пока baseline, а не финальный allocator: storage должен перейти на frame contexts, fenced reuse и deferred deletion.

Static level geometry уже читает draw/material records по этому ABI, но pipeline всё ещё выбирается старым прототипным способом. UI пока передаёт texture index через старый `baseInstance` contract и будет переведён отдельно.

## HLSL contract

Engine template владеет entry points, render pass logic, register bindings и resource declarations. Material implementation реализует только вычисление material inputs:

```hlsl
void EvaluateMaterial(
    in MaterialContext Context,
    in MaterialParameters Parameters,
    out MaterialInputs Result);
```

Начальный `MaterialInputs`: BaseColor, Normal, Roughness, Metallic, AmbientOcclusion, Emissive, Opacity, OpacityMask и WorldPositionOffset.

`MaterialContext` уже содержит `TexCoord0`, `TexCoord1`, vertex color, world normal/position, camera data и time. Второй UV нужен pre-authored legacy lightmap instance; наличие lightmap и vertex color задаётся static switches parent instance, а texture indices остаются runtime parameters.

Ручная HLSL implementation подключается include-файлом. Graph implementation проходит type checking, constant folding, dead-code elimination и HLSL generation. После этого обе формы используют одинаковые DXC compilation, reflection и pipeline cache.

## Material graph

Graph описывает expressions, а не произвольные shader stages или render passes. Он не может назначать registers и создавать глобальные GPU resources.

Начальный набор nodes включает constants/parameters, arithmetic/vector operations, texture/sample/UV, vertex color, normals, world/camera data, time, lerp, clamp, Fresnel, static switch, Custom HLSL и Material Output.

Custom HLSL node обязан явно объявлять typed inputs/outputs. Объявления глобальных resources, registers, entry points и include с произвольными bindings запрещены.

Graph JSON содержит versioned node type, стабильные node/pin GUID, links, значения и editor positions. Compiler добавляет node GUID в diagnostics и `#line`, чтобы Level Editor мог перейти к проблемному node.

Node UI строится поверх `FMaterialGraph` из `xrTiramisuMaterialCore` и использует ImNodes только для отображения/взаимодействия. Существующий `CNodeViewport` не становится semantic model материала.

## Runtime API

Независимый от NRI модуль `xrTiramisuMaterialCore` определяет material IDs/enums, parameter definitions, static sets и pipeline keys, master/instance/dynamic instance, generation-counted handles, proxy contracts, asset validation, graph compiler и legacy resolver.

Renderer адаптирует CPU contract к NRI через собственные `TiramisuMaterialRenderProxy` и `FMaterialPassProxy`. Level Editor использует узкий `IMaterialPreviewRenderer`, поэтому NRI types не проходят в editor code.

## Legacy compatibility

Старый материал разрешается в строгом порядке:

1. явный material GUID в новом необязательном chunk уровня/OGF;
2. запись `legacy-map.json`;
3. standard material, автоматически заполненный shader name, первой texture и texture metadata;
4. диагностический error material.

Для совместимости с UE-подобной моделью таблица не должна изменять static parameters во время загрузки уровня. Базовые `Legacy Opaque`, `Legacy Masked` и `Legacy Emissive` являются masters. Заранее созданные instances `default`, `vertex`, `lmap`, варианты `*_aref` и `selflight` фиксируют static switches и blend/shading contract. `legacy-map.json` выбирает такой parent instance, после чего loader создаёт кэшируемый `TMaterialInstanceDynamic` только с runtime texture/scalar overrides.

Ключ dynamic cache включает parent reference, нормализованное legacy shader name и полный список textures. Это исключает instance на каждый draw и не создаёт новую permutation. Текущий renderer bridge уже выбирает parent instance и откладывает NRI pipeline initialization на render thread, но prototype GPU parameter path передаёт только первую texture. Lightmap slot, texture metadata и остальные overrides должны быть подключены через общий material parameter layout до объявления legacy migration завершённой.

`TiramisuLegacyScene` остаётся content adapter старых `.level`/OGF и преобразует их геометрию в `FMeshBatch`. Новый scene format развивается параллельно и не обязан повторять layout старых sectors/visuals. Форматы R4 остаются читаемыми игровым R4; постепенная миграция не требует массовой конвертации контента.

При открытии старой `.level` в Tiramisu legacy `CSceneObject` выполняет first-load material migration без изменения исходного уровня. Его `CSurface` используется только как вход: shader, texture, two-sided и остальные сохранённые поля разрешаются в заранее созданный parent и дедуплицированный `generated/legacy_objects/*.material-instance.json`. Properties показывает уже `Material Instance` и позволяет открыть его в Material Editor. Общая mesh geometry остаётся одна, а отличия конкретного объекта поступают в viewport как per-instance material-slot overrides. Игровой R4 и его старый property path этим не изменяются.

## Cooker и hot reload

Development cache key учитывает hashes material/template/graph, static parameters, vertex factory, pass signature, backend, shader model, options и includes. Целевая схема выполняет parsing, IR и DXC в фоне, а NRI pipelines создаёт только на render thread.

`xrMaterialCooker` использует тот же compiler и создаёт versioned binary bundle с flattened instances, dependency table и DXIL/SPIR-V blobs. Cooked runtime не читает JSON и не компилирует HLSL.

Текущий checkpoint валидирует 11 masters и 9 instances. Для всех известных
static permutations cooker создаёт 216 DXIL/SPIR-V blobs: парные canonical
vertex и pixel stages для production/validation passes двух backend, включая
две legacy decal masters. Vertex factory получает draw record через
`NRI_INSTANCE_ID_OFFSET`; surface factories вызывают тот же `EvaluateMaterial`
для `WorldPositionOffset`, а `decal_projector` создаёт fullscreen triangle и
передаёт draw index pixel pass. Bundle v2 хранит material/pipeline key, backend
format, pass, stage, entry point, vertex factory и render-pass signature. Все
pixel templates компилируются для Vulkan и D3D12 с одинаковыми
descriptor-heap bindings.

Renderer умеет загрузить development bundle, выбрать нужный backend и material permutation и создать validation NRI pipeline только на render thread. Legacy G-buffer pipeline остаётся отдельным fallback до подключения настоящих MRT. Полный production pipeline set, binary flattened records без JSON и frame-safe hot reload ещё не завершены. Поэтому bundle намеренно сохраняет `CompleteShaderSet = false`, а строгий cooked runtime обязан его отклонять.

CPU-часть `TiramisuMaterialPipelineCache` уже принимает только полный типизированный набор pass proxies, проверяет revision/duplicates/required passes и атомарно публикует pending snapshots на границе кадра. Ошибочный набор не заменяет активный, а заменённые snapshots возвращаются renderer для deferred GPU deletion.

Focused editor workflow уже работает end-to-end. `TiramisuMaterialDependencyWatcher` отслеживает active master/instance, всю parent chain, HLSL template/implementation и объявленные dependencies по timestamp, размеру и content hash. Чистый asset перезагружается автоматически; если документ dirty, editor не затирает локальную работу и предлагает либо явно применить внешний вариант, либо оставить локальный. Parsing/IR/DXC выполняются в фоне, NRI pipeline устанавливается render thread, а при ошибке предыдущая accepted revision остаётся активной. Preview показывает requested/accepted revisions, pipeline key, backend, pass и vertex factory.

Hot reload подключён к Material Editor preview и Forward material path основной editor scene. Scene watcher отслеживает legacy map, master/parent assets, implementation/template и engine pass includes; resolver reload и DXC выполняются в фоне, неуспешный результат сохраняет last-good pipeline. Production `TiramisuMaterialPipelineCache` ещё не подключён ко всему игровому renderer frame loop и полному Depth/Shadow/GBuffer/Forward pipeline set; binary cooked runtime также остаётся незавершённым.

## Material Editor

За основу компоновки и пользовательского workflow берётся существующий `src/Editors/ShaderEditor`: отдельные panels, item browser, properties/details и preview viewport. Его legacy semantic model (`IBlender`, `CSHEngineTools`, временная сериализация shaders и прямой `EDevice->Reset`) в новую систему не переносится.

Новый editor хранит единственную semantic model в `FMaterialGraph`/material assets из `xrTiramisuMaterialCore`. ImNodes отвечает только за view/controller: позиции, выбор, создание links и команды редактирования. Компиляция, type checking и diagnostics всегда идут через общий material compiler.

Для полного перехода редакторов требуется отделить `xrECore` от встроенного legacy renderer. `xrECore` всё ещё компилирует D3D9/R1/R4 renderer sources внутрь своей библиотеки, поэтому миграционная граница разделена на следующие части:

- renderer-neutral viewport/editor render contract в `xrECore`;
- временный legacy adapter для работы текущих редакторов во время миграции;
- Tiramisu composition root для режима `-tiramisu-editor` в LevelEditor;
- material preview только через `IMaterialPreviewRenderer`, без NRI типов в editor code.

Базовый scene/presentation-срез уже готов: `UIRenderForm` использует `IEditorRenderBackend` для capture, resize, scene snapshots, CPU picking и opaque ImGui surface, а legacy adapter временно сохраняет прежнее поведение. NRI presenter LevelEditor имеет собственный swapchain и три frame contexts; создание ресурсов, запись команд, resize, present и удаление выполняются общим render thread `xrRenderTiramisu`. Viewport рисует static/transient meshes, selection и debug/overlay primitives; legacy surfaces разрешаются через pre-authored `MaterialInstance`, parent flattening и bindless texture overrides. Общий compiler асинхронно собирает Forward pass для Vulkan/D3D12, а неуспешный rebuild сохраняет last-good pipeline. `NRI_BASE_INSTANCE` индексирует `FMaterialDrawGpuData`; draw/instance/parameter records и отдельные light records тройно буферизованы. Forward pass учитывает two-sided/blend mode и вычисляет GGX/Smith/Schlick lighting для native Directional/Point/Spot lights из Descriptor Heap Indexing buffer. Совместный normal/ASan GPU smoke на Vulkan и D3D12 проверяет два lights, три opaque/translucent/additive draws, selection, debug/overlay paths и safe reload с обязательным `-rdbg`. Material preview отдельно реализует background DXC, offscreen sphere/cube/plane, Texture2D/TextureCube cache и environment lighting. Production prefiltered IBL, clustered lights/shadows, игровой renderer-wide hot reload и полный новый scene path ещё не подключены. Standalone legacy ShaderEditor не подключается; новый Material Editor работает внутри Tiramisu-only режима LevelEditor. Игровой R4 остаётся отдельным.

Текущий GUI уже включает master/instance assets и parent chains, searchable palette, typed pins, details panel, undo/redo, copy/paste, autosave/recovery, migrations, diagnostics с переходом к node, generated HLSL preview, permutation statistics, preview primitive/environment controls, instance inspector и безопасный dependency-driven live preview. До готовности остаются production IBL, renderer-wide pipeline hot reload и завершение Tiramisu scene viewport workflow.
