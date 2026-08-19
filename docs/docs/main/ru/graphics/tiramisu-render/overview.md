# Tiramisu Render

> Статус: стартовый прототип, opt-in. Обновлено 19 августа 2026 года.

Tiramisu Render (`xrRenderTiramisu`) — экспериментальный renderer IX-Ray для Vulkan и Direct3D 12 поверх NRI. Он создаёт устройство, swapchain, базовые GPU-ресурсы, загружает часть статической геометрии уровня и умеет выводить её вместе с UI. Это техническая основа будущей замены R4, но ещё не готовый игровой renderer.

## Запуск

Tiramisu выбирается явно параметром командной строки:

```text
-r5
```

По умолчанию используется Vulkan. Direct3D 12 выбирается дополнительным параметром:

```text
-r5 -dx12
```

До выполнения критериев из [roadmap](./roadmap.md) игровой Tiramisu остаётся opt-in через `-r5`, а R4 не изменяется. LevelEditor уже выбирает `xrRenderTiramisu` по умолчанию; старый аргумент `-tiramisu-editor` разрешён в существующих launch-конфигурациях, но не обязателен. Для сборки следует использовать актуальный CMake preset и новый build-каталог: сохранённые build-каталоги могут содержать пути к уже удалённой версии Visual Studio.

## Обязательный режим тестирования

Все тестовые запуски движка и редакторов Tiramisu выполняются исключительно с параметром `-rdbg`. Это правило распространяется на ручные smoke-тесты, автоматизированные GPU-сцены, flythrough, проверки resize/restart/device loss и запуск под отладчиком. Результат запуска без `-rdbg` не засчитывается как acceptance-проверка.

Базовые команды GPU smoke:

```text
LevelEditor.exe -tiramisu-editor -rdbg -render-deterministic -editor-test-hidden -material-preview-smoke -viewport-material-reload-smoke
LevelEditor.exe -tiramisu-editor -dx12 -rdbg -render-deterministic -editor-test-hidden -material-preview-smoke -viewport-material-reload-smoke
LevelEditor.exe -tiramisu-editor -rdbg -render-deterministic -editor-test-hidden -legacy-conversion-smoke
LevelEditor.exe -tiramisu-editor -dx12 -rdbg -render-deterministic -editor-test-hidden -legacy-conversion-smoke
LevelEditor.exe -tiramisu-editor -rdbg -render-deterministic -editor-test-hidden -legacy-zaton-conversion-smoke
LevelEditor.exe -tiramisu-editor -dx12 -rdbg -render-deterministic -editor-test-hidden -legacy-zaton-conversion-smoke
```

Первая пара проверяет material preview/viewport hot reload, bindless upload двух scene lights и non-identity `LocalToWorld`, проходящий через material draw-buffer ABI; CPU pick использует тот же смещённый instance. Вторая пара проверяет быстрый legacy `.object`/`.level` conversion и обязательные dumps. Последняя пара загружает полноценный `rawdata/levels/!FinalSP/zaton.level` вместе с соседними `.part` и сохраняет native assets и dumps в `build/test-results/tiramisu/zaton-<timestamp>/`. Импортер переносит legacy-солнце и явные light objects в native `light_components`. Без `-dx12` используется Vulkan, с `-dx12` — D3D12. CPU-тестовые executable также запускаются с `-rdbg`; после этого весь набор дополнительно прогоняется через CTest для проверки регистрации и зависимостей тестов.

Обычный импорт через Content Browser создаёт файлы относительно `$game_data$`: StaticMesh metadata/BIN — в `render_static_meshes/imported/`, RenderScene — в `render_scenes/imported/`, автоматически созданные legacy MaterialInstance и migration database — в `render_materials/generated/legacy_objects/`. Для Zaton целевой scene path имеет вид `gamedata/render_scenes/imported/!FinalSP/zaton.render-scene.json`. Каталоги создаются лениво при первом успешном импорте.

Для проверки памяти добавлен отдельный preset AddressSanitizer:

```text
cmake --preset Editors-x64-Windows-ASan
cmake --build build/x64/Editors-Windows-ASan --config Debug --target ALL_BUILD
ctest --test-dir build/x64/Editors-Windows-ASan -C Debug --output-on-failure
```

ASan не заменяет NRI/API validation: полный CPU/compiler/cooker/editor набор сначала проходит под sanitizer, а затем ASan-сборка `LevelEditor.exe` отдельно запускает обе совместные preview+scene GPU smoke-команды выше. Для немедленной остановки на дефекте задаётся `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1`. MSVC AddressSanitizer на Windows не поддерживает LeakSanitizer, поэтому `detect_leaks=1` использовать нельзя: runtime завершит процесс ещё до тестового кода. Все CTest-команды и оба GPU запуска сохраняют обязательный `-rdbg`. Автоматический LevelEditor smoke также обязан передавать `-editor-test-hidden`: splash не создаётся, test HWND получает `NOT_FOCUSABLE`, показывается через `SW_SHOWNOACTIVATE` за границами рабочего стола и не забирает пользовательский фокус. Флаг отклоняется без валидного `-render-deterministic`. CMake автоматически копирует `clang_rt.asan_dynamic-x86_64.dll` из каталога активного MSVC toolset рядом с бинарниками; отсутствие runtime считается ошибкой конфигурации. Обычный deterministic GPU smoke ограничен 60 секундами, ASan — 120 секундами, RenderDoc — 240 секундами: instrumentation, shader permutations, OGF/OMF и синхронная сериализация capture не должны давать ложный timeout.

Recovery baseline на 19 августа 2026 года: normal CTest `47/47`, ASan CTest `47/47`, material/viewport GPU smoke `4/4`, RenderDoc capture `4/4` и полный Zaton conversion `4/4` для normal/ASan × Vulkan/D3D12. Два CMake wrapper-теста cooker жёстко требуют `TEST_DEBUG_FLAG=-rdbg` и передают его каждому вложенному executable.

## RenderDoc

RenderDoc подключается ранним bootstrap до создания Vulkan/D3D12 NRI device. Для LevelEditor используются команды:

```text
LevelEditor.exe -tiramisu-editor -rdbg -renderdoc
LevelEditor.exe -tiramisu-editor -dx12 -rdbg -renderdoc
LevelEditor.exe -tiramisu-editor -rdbg -render-deterministic -editor-test-hidden -material-preview-smoke -viewport-material-reload-smoke -renderdoc -renderdoc-capture
LevelEditor.exe -tiramisu-editor -dx12 -rdbg -render-deterministic -editor-test-hidden -material-preview-smoke -viewport-material-reload-smoke -renderdoc -renderdoc-capture
```

`-renderdoc` принимает уже инжектированный `renderdoc.dll` либо ищет его рядом с executable, по пути из переменной `RENDERDOC_DLL`, через `PATH` и в стандартном `Program Files\RenderDoc`. Версия API и полный путь DLL выводятся в лог. Интерактивный захват выполняется клавишей `F12`. Для автоматического smoke используется дополнительный `-renderdoc-capture`: capture gate ждёт готовности material pipelines и reload acceptance, после чего явно оборачивает следующий внешний present через `StartFrameCapture`/`EndFrameCapture` и передаёт HWND главного окна. Редактор завершается только после успешного `EndFrameCapture`; самый первый стартовый present больше не считается RenderDoc acceptance. Имя содержит backend и PID (`LevelEditor_vulkan_<pid>_capture.rdc` или `LevelEditor_d3d12_<pid>_capture.rdc`), поэтому параллельные normal/ASan запуски не перезаписывают друг друга. Файлы сохраняются в `logs/renderdoc/`; для текущего workspace это `G:/GameDev/Engine/XRay/ixray-1.6-stcop/logs/renderdoc/`.

`-rdbg` остаётся обязательным и продолжает включать debug-информацию DXC. Когда RenderDoc активен, конфликтующие graphics API validation и NRI validation layers автоматически отключаются до создания device; это явно отмечается в логе. Для узкой диагностики доступен опасный override `-renderdoc-validation`, принудительно совмещающий RenderDoc с validation layers; он может быть нестабилен и не используется в штатных тестах.

У X-Ray есть собственный crash handler, а при запуске из Rider исключениями владеет LLDB. Поэтому bootstrap после получения in-application API отключает внутренний Breakpad handler RenderDoc и явно включает подавление API debug output. Это исключает конкуренцию обработчиков во время завершения D3D12/Vulkan capture в `Present`; сам `.rdc` при этом сохраняется обычным способом. Проверить файл без открытия GUI можно командой `renderdoccmd thumb --out=<preview.png> <capture.rdc>`.

Текущий smoke можно запускать со скрытым Win32-окном, но это всё ещё не headless GPU runner: процесс создаёт HWND. В `-tiramisu-editor` LevelEditor больше не вызывает `InitRenderDeviceEditor`, не создаёт legacy D3D11 `CRHI`, `CResourceManager`, editor render targets или shaders и не поднимает локальный `RImplementation` runtime. Единственный GPU device принадлежит `xrRenderTiramisu`; старые editor backends сохраняют прежний lifecycle только для других пока не переведённых executables.

## Текущий кадр

Сейчас кадр проходит по упрощённой цепочке:

1. Engine формирует render commands и синхронизируется с render thread.
2. Renderer создаёт offscreen color target `RGBA8_UNORM` и depth/stencil `D24_UNORM_S8_UINT`.
3. `TiramisuRenderDeferredPass` напрямую рисует видимую статическую геометрию в этот color target.
4. UI рисуется поверх результата.
5. Полноэкранный треугольник копирует offscreen color в swapchain.
6. Изображение передаётся на present.

`TiramisuRenderDeferredPass` **пока не является deferred renderer**. В нём нет G-buffer, material resolve, отдельного lighting pass или полноценной PBR-модели. Название описывает предполагаемое направление, а не текущую реализацию.

## Что уже работает

- создание NRI device для Vulkan и D3D12;
- swapchain и базовая обработка кадра;
- опциональный render thread и очередь render commands;
- загрузка общих vertex/index buffers из `level.geom`;
- базовый frustum/portal/SSA отбор статической геометрии;
- два используемых формата level vertices;
- минимальный набор shaders для сцены, UI и вывода в swapchain;
- bindless-доступ в HLSL через `ResourceDescriptorHeap[index]` и `SamplerDescriptorHeap[index]`;
- bindless descriptor heap фиксированного размера;
- UI для triangle list/strip, scissor и шрифтов;
- базовые NRI debug/validation параметры запуска;
- GPU annotations для основных game/editor passes и общий versioned statistics snapshot: CPU frame time, pass/draw/triangle/line/upload counters и tracked buffers/textures/pipelines/descriptors/bytes; GPU timestamp и driver VRAM пока не измеряются;
- ранний RenderDoc bootstrap через `-renderdoc`, F12 capture и безопасная policy совместного запуска с обязательным `-rdbg`;
- LevelEditor NRI viewport с material slot resolution, pre-authored instances, Forward material permutations и runtime Directional/Point/Spot PBR lighting;
- отдельный LevelEditor Tiramisu redraw, который обходит legacy `CEditorRenderDevice::Begin`/`RCache`/`Scene->Render` и передаёт scene snapshot, ImGui submit и Present в `xrRenderTiramisu`;
- тройная буферизация editor draw/instance/parameter/light/skinning records:
  `NRI_BASE_INSTANCE` индексирует ABI v5 `FMaterialDrawGpuData`, а scene
  constants задают bindless light, palette buffers и inverse view-projection;
- отдельный projective Decal pass LevelEditor: scene depth читается через
  `ResourceDescriptorHeap`, мировая позиция восстанавливается в pixel shader,
  а `world-to-decal` берётся из draw record. Native RenderScene v3 хранит
  projector transform/material/sort order; старый Wallmark служит только
  входом автоматического adapter, а не готовой render geometry;
- renderer-owned OGF/OMF path для Spawn/actor visuals: hierarchy, 1–4 weights, embedded/external motions, current/previous palette upload и отдельная `skeletal` material permutation;
- renderer-neutral CPU picking статических mesh instances с возвратом object/mesh/material IDs и NRI wireframe overlay выбранных объектов;
- renderer-neutral debug packet и depth-tested NRI line/triangle passes для сохранённых LevelEditor `m_DebugDraw` и common world-space `DU_impl` primitives: lines/crosses, lists/strips/fans, indexed faces, grid, selection boxes, sphere/box/cone/cylinder gizmos; отдельные screen-space passes без depth test для selection rectangle и object-axis lines; owned text labels и ImGuizmo corner axis композятся поверх viewport image;
- native `StaticMesh`/`RenderScene` assets из `xrTiramisuSceneCore`, открываемые LevelEditor напрямую; StaticMesh v2 разделён на компактный текстовый `*.static-mesh.json` с параметрами и versioned binary `*.static-mesh.bin` с вершинами/индексами, RenderScene v2 добавляет native Directional/Point/Spot Light;
- базовое редактирование native RenderScene в LevelEditor: общий selection/transform/visibility/Cut/Copy/Paste/duplicate/delete/undo/save lifecycle для StaticMesh и Light, cross-scene GUID remap, StaticMesh drag-and-drop/material overrides и Light Details с type, HDR radiometry, range, spot cones и cast-shadows metadata;
- автоматическая конвертация старых `.object`/`.level` в native assets с дедуплицированными MaterialInstance и per-component overrides;
- обязательные детерминированные `<target>.migration.json` и `<target>.migration.failed.json`, включая ошибку открытия/загрузки старого level.

StaticMesh binary имеет magic, version, endian tag, offsets, vertex/index strides, counts, file size и payload hash. JSON хранит имя payload, форматы, strides, counts, sections, material slots и тот же hash. Loader проверяет соответствие двух файлов до публикации mesh. Inline geometry JSON v1 остаётся только читаемым migration source; новый importer всегда пишет v2. На полном Zaton `zaton_terrain.static-mesh.json` уменьшился примерно со 160 МБ до 1,3 КБ, а 19,9 МБ bulk geometry перешли в `.bin`.

## Ограничения и заглушки

Следующие подсистемы отсутствуют, реализованы частично или пока представлены пустыми совместимыми методами:

- настоящий deferred PBR, MRT G-buffer и deferred/clustered lighting; editor Forward path уже принимает до 64 native lights;
- production-ready material bundles, полный pass set и renderer-wide hot reload; базовые assets/instances/permutations/parameter buffers уже реализованы;
- полный игровой dynamic/progressive/skinned pipeline для actors, NPC, HUD-моделей и оружия; LevelEditor actor smoke уже использует animated OGF/OMF GPU skinning, но игровой scene integration, LOD/SWI и shadows отсутствуют;
- production particles, trees/details, grass и foliage; editor particles,
  glows и projective legacy-wallmark adapter уже работают;
- game-scene light upload, clustered lists, shadows, production DBuffer
  decals и transparency; native editor lights, projective color decals и
  базовые translucent/additive materials уже проходят Tiramisu path;
- sky, weather, rain, water и связанные environment effects;
- postprocessing, exposure, tone mapping и temporal effects;
- screenshots, gamma control и video/sequence output; selection wireframe, сохранённый `m_DebugDraw`, common world-space `DU_impl`, screen-space selection rectangle/object-axis lines, labels, corner axis, spawn icons, glow sprites, PAPI particle position markers и базовые textured particle billboards уже подключены через Tiramisu;
- безопасный device-loss path, полноценный resize/fullscreen lifecycle;
- cooked material bundles и отсутствие runtime shader compilation;
- полная representative-scene пригодность backend; adapter/API/graphics-queue filtering уже реализован, но acceptance ещё не закрыт.
- полный native LevelEditor authoring: point/rectangle/range/invert selection, Focus Selected/Zoom All, native Outliner с bulk visibility, Cut/Copy/Paste, duplicate/delete, visibility commands, single- и multi-component Details/material overrides, пустая RenderScene и базовый StaticMesh component lifecycle уже работают, но остальные editor tools ещё не перенесены с переходной legacy-модели.

Игровой prototype bridge статической геометрии пока использует только первую texture материала; editor viewport уже передаёт flatten parameter block через общий material ABI. Progressive LOD фактически не подключён. UI line primitives не выводятся. Фиксированный descriptor heap и текущая синхронизация предназначены только для прототипа.

## Целевое направление

Цель — deferred PBR для opaque/masked geometry и forward-проход для translucent и специальных материалов. Центральной частью станет общая material-система с master materials, instances, ручным HLSL и node graph, который генерирует HLSL для того же engine template.

Подробности:

- [подробное описание текущей реализации](./current-implementation.md);
- [архитектура renderer](./architecture.md);
- [material-система и node graph](./materials.md);
- [полная матрица возможностей R4](./r4-feature-matrix.md);
- [representative scenes и deterministic GPU mode](./representative-scenes.md);
- [этапы замены R4 и критерии готовности](./roadmap.md).
