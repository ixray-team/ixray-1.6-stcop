# Стиль C++ кода Tiramisu Render

> Статус: обязательный стандарт для нового и изменяемого кода Tiramisu Render, `xrTiramisuMaterialCore`, material cooker, renderer-интеграции LevelEditor и относящихся к ним тестов. Первая редакция от 23 июля 2026 года.

## Область действия

Этот документ задаёт целевой стиль для рефакторинга уже написанного кода Tiramisu и для всей дальнейшей разработки renderer. Старые подсистемы X-Ray не переименовываются массово только ради соответствия этому документу. При переносе legacy-кода в новый pipeline перенесённая часть сразу приводится к новому стилю.

За основу взят [Epic C++ Coding Standard для Unreal Engine 5](https://dev.epicgames.com/documentation/unreal-engine/epic-cplusplus-coding-standard-for-unreal-engine?lang=en-US), но правила проекта имеют приоритет. Основные отличия от UE5:

- используются типы и allocator-ы `xrCore`, а не `TArray`, `FString` и другие типы Unreal;
- в проекте нет системы `UObject`, поэтому префиксы `U`, `A` и `S` не переносятся;
- имя Tiramisu не сокращается до `T`: у нешаблонных renderer-классов пишется полное `Tiramisu`;
- глобальные namespace используются строже и реже, чем допускает современный UE5.

## Главные правила

1. Новый код использует публичные типы `xrCore`, если у стандартного типа есть корректный engine-аналог.
2. Все имена типов, функций и переменных пишутся в `PascalCase`, кроме существующих имён внешних API.
3. Любая новая `struct` начинается с `F`.
4. Префикс `T` разрешён только настоящему шаблонному типу.
5. Любой нешаблонный concrete-класс нового Tiramisu pipeline начинается с полного слова `Tiramisu`; префикс `F` для `class` запрещён.
6. Namespace создаётся только при наличии реальной границы, а не для группировки нескольких файлов.
7. Нельзя менять ABI, формат файла или внешний API слепой заменой `std::*` на `xr_*`.
8. Жёсткого ограничения длины строки нет.

## Длина строки и переносы

В Tiramisu-коде нет лимита в 80, 100, 120 или другое фиксированное количество символов. Formatter, linter и review не должны требовать перенос только потому, что строка достигла выбранной колонки.

Строка переносится, когда это улучшает чтение структуры выражения:

- длинная цепочка условий разделяется по логическим частям;
- аргументы сложного вызова можно расположить по одному на строке;
- initializer list можно разбить по элементам;
- длинное имя типа, функция с понятными аргументами, include, URL или диагностическая строка могут оставаться одной строкой.

Нельзя переносить короткое цельное выражение только ради соответствия ширине окна. Нельзя использовать «лесенку» из переносов, которая скрывает основную операцию и увеличивает вертикальный размер кода.

```cpp
const EMaterialResolveResult ResolveResult = MaterialManager.ResolvePass(MaterialHandle, VertexFactory, RenderPassSignature, OutPassProxy);
```

Если выражение действительно состоит из нескольких смысловых частей, перенос отражает именно их:

```cpp
const bool bCanPublishPipeline =
    CompileResult.bSucceeded &&
    CompileResult.Diagnostics.empty() &&
    PipelineCache.IsCompatible(CompileResult.PipelineKey);
```

## Типы xrCore вместо STL

### Базовые числовые типы

| Назначение | Использовать | Не использовать в новом engine-коде |
| --- | --- | --- |
| Знаковые целые фиксированной ширины | `s8`, `s16`, `s32`, `s64` | `std::int8_t` … `std::int64_t` |
| Беззнаковые целые фиксированной ширины | `u8`, `u16`, `u32`, `u64` | `std::uint8_t` … `std::uint64_t` |
| Числа с плавающей точкой | `f32`, `f64` | Локальные aliases для `float`/`double` |
| Логическое значение | `bool` | Числовой тип в роли boolean |
| Размер и индекс памяти | `size_t` | Принудительное сужение до `u32` |
| Время C runtime | `xr_time_t` | Прямой `std::time_t` в публичном engine API |

`float` и `double` допустимы в сигнатурах внешних библиотек и там, где существующий математический API X-Ray уже использует их напрямую. Для сериализуемых и GPU ABI структур ширина каждого поля должна быть явной и проверяться `static_assert`.

### Последовательные контейнеры и адаптеры

| Стандартный тип | Engine-тип | Примечание |
| --- | --- | --- |
| `std::vector<T>` | `xr_vector<T>` | Основной динамический массив с allocator-ом X-Ray |
| `std::deque<T>` | `xr_deque<T>` | Двусторонняя очередь |
| `std::list<T>` | `xr_list<T>` | Использовать только при доказанной необходимости стабильных узлов |
| `std::array<T, N>` | `xr_array<T, N>` | Массив фиксированного размера |
| `std::queue<T>` | `xr_queue<T>` | По умолчанию построен на `xr_deque` |
| `std::stack<T>` | `xr_stack<T>` | По умолчанию построен на `xr_deque` |
| `std::span<T>` | `xr_span<T>` | Невладеющее представление непрерывных данных |
| `std::pair<K, V>` | `xr_pair<K, V>` | Пара значений |
| `std::tuple<T...>` | `xr_tuple<T...>` | Для коротких внутренних результатов; публичному API предпочтительнее именованная `F`-структура |

`xr_vector` поддерживает `fast_erase`, когда порядок элементов не важен. Выбор этой операции должен быть заметен из контекста: она переносит последний элемент на место удалённого.

### Ассоциативные контейнеры

| Стандартный тип | Engine-тип |
| --- | --- |
| `std::set<K>` | `xr_set<K>` |
| `std::multiset<K>` | `xr_multiset<K>` |
| `std::map<K, V>` | `xr_map<K, V>` |
| `std::multimap<K, V>` | `xr_multimap<K, V>` |
| `std::unordered_map<K, V>` | `xr_hash_map<K, V>` |
| `std::unordered_set<K>` | `xr_hash_set<K>` |

Для deterministic cooker output нельзя полагаться на порядок обхода `xr_hash_map` или `xr_hash_set`. Перед сериализацией ключи сортируются либо сразу используется упорядоченный контейнер.

`xr_string_map<K, V>` существует как legacy-обёртка над `std::unordered_map`, но не использует `xalloc`. В новом Tiramisu-коде следует использовать `xr_hash_map`, а `xr_string_map` оставлять только в существующих совместимых путях.

### Строки и пути

| Задача | Engine-тип | Правило |
| --- | --- | --- |
| Владеющая UTF-8/ANSI строка X-Ray | `xr_string` | Основной строковый тип engine-кода |
| Невладеющая строка | `xr_string_view` | Входные параметры, когда вызывающая сторона гарантирует lifetime |
| Невладеющая wide-строка | `xr_wstring_view` | Граница Windows API и других wide API |
| Interned неизменяемая строка | `shared_str` | Повторяющиеся engine-идентификаторы; сравнение использует общий string pool |
| Строка фиксированной ёмкости | `xr_stack_string<N>` | Локальное форматирование без heap allocation |
| Wide-строка фиксированной ёмкости | `xr_stack_wstring<N>` | Wide API с известным ограничением |
| Platform string фиксированной ёмкости | `xr_stack_tstring<N>` | Только platform boundary |
| Путь | `xr_path` | Engine filesystem и asset/tool paths |
| Элемент каталога | `xr_dir_entry` | Аналог `std::filesystem::directory_entry` |
| Итератор каталога | `xr_dir_iter` | Аналог `std::filesystem::directory_iterator` |
| Рекурсивный итератор каталога | `xr_dir_recursive_iter` | Аналог `std::filesystem::recursive_directory_iterator` |

Готовые stack aliases: `xr_stack_string_path`, `xr_stack_string16`, `32`, `64`, `128`, `256`, `512`, `1024`, `2048`, `4096` и соответствующие `xr_stack_wstring_path`, `xr_stack_wstring16` … `xr_stack_wstring4096`.

Старые массивы `string16` … `string4096`, `string_path`, `wstring16` … `wstring4096` и `wstring_path` остаются только для совместимости старого API. В новом коде используются `xr_string`, view или `xr_stack_*`, в зависимости от владения и требуемой ёмкости.

`shared_str` не является общей заменой `xr_string`: его следует выбирать только для часто повторяющихся неизменяемых имён с lifetime общего string container. Для JSON/HLSL текста, диагностики и редактируемых значений используется `xr_string`.

### Optional, владение и callbacks

| Стандартный механизм | Engine-механизм | Правило |
| --- | --- | --- |
| `std::optional<T>` | `xr_optional<T>` | Необязательное значение |
| `std::unique_ptr<T>` | `xr_unique_ptr<T>` | Единоличное владение объектом, выделенным через X-Ray memory API |
| `std::shared_ptr<T>` | `xr_shared_ptr<T>` | Разделяемое владение только при реальной необходимости |
| `std::weak_ptr<T>` | `xr_weak_ptr<T>` | Невладеющая ссылка на объект под `xr_shared_ptr` |
| `std::make_unique<T>` | `xr_make_unique<T>` | Создание `xr_unique_ptr` |
| `std::make_shared<T>` | `xr_make_shared<T>` | Создание `xr_shared_ptr` |
| Intrusive reference counting | `intrusive_ptr<T>` и `intrusive_base` | Только для существующей intrusive ownership-модели |
| Простой delegate | `xr_delegate<Signature>` и `xr_make_delegate` | Engine callback с совместимым lifetime |
| `std::scope_exit` | `xr_scope_exit` | Локальная очистка ресурса при выходе из scope |

Raw pointer не выражает владение. Он допустим как:

- невладеющая ссылка с lifetime, определённым владельцем;
- handle внешнего API, например NRI, DXC, RenderDoc или Win32;
- участок существующего ABI, который пока нельзя изменить.

В каждом таком случае владелец должен быть понятен из типа, имени поля или комментария к нетривиальному lifetime. Нельзя помещать объект, выделенный внешним API, в `xr_unique_ptr` без подходящего deleter.

`xr_delegate` не является полной заменой любого owning callable. `std::function` допустим для владеющего type-erased callback с captures, если `xr_delegate` не предоставляет нужную семантику. Такое исключение не должно попадать в hot path без измерения allocation.

### Concurrency

| Назначение | Engine-тип или функция |
| --- | --- |
| 8-битный atomic | `xr_atomic_u8` |
| 32-битный unsigned atomic | `xr_atomic_u32` |
| 32-битный signed atomic | `xr_atomic_s32` |
| Atomic boolean | `xr_atomic_bool` |
| Atomic float | `xr_atomic_float` |
| Группа задач | `xr_task_group` |
| Concurrent hash map | `xr_concurrent_unordered_map<K, V>` |
| Concurrent vector | `xr_concurrent_vector<T>` |
| Параллельный цикл | `xr_parallel_for` |
| Параллельный обход | `xr_parallel_foreach` |
| Максимальная concurrency | `xr_max_concurrency()` |
| Потокобезопасная комбинация результатов | `xr_combinable<T>` |

У `std::atomic<T>`, `std::mutex`, `std::condition_variable`, `std::thread`, `std::future`, `std::promise` и `std::chrono` сейчас нет полного общего набора `xrCore`-аналогов. Они разрешены, когда существующий engine primitive не выражает требуемую операцию. Локальные aliases с новым именем `xr_*` создавать нельзя: общий engine-аналог добавляется централизованно в `xrCore` отдельным изменением.

Независимо от выбранного типа, Tiramisu сохраняет thread-affinity contract:

- game-thread API проверяет `CheckIsGameThread`;
- render-thread API и все NRI create/destroy/update/submit пути проверяют `CheckIsRenderThread`;
- background compiler не получает доступ к game/render-thread объектам;
- данные между потоками передаются owned immutable packet-ами или явно синхронизированными структурами.

### Специализированные контейнеры

| Тип | Использование |
| --- | --- |
| `buffer_vector<T>` | Vector поверх предоставленного снаружи фиксированного buffer |
| `associative_vector` | Отсортированный vector с ассоциативным интерфейсом; только при подходящем профиле вставок/поиска |
| `xr_rtree2d<T>` | Пространственный R-tree для 2D bounds |
| `xr_combinable<T>` | Локальные для worker-ов значения с последующим combine |

Legacy aliases `FvectorVec`, `U16Vec`, `U32Vec`, `FloatVec`, `LPSTRVec`, `LPCSTRVec` и соответствующие iterator aliases не применяются в новом Tiramisu-коде. Пишется полный тип, например `xr_vector<u32>`, чтобы владение, allocator и element type читались непосредственно.

## Когда `std::*` разрешён

Наличие engine-аналогов не означает запрет стандартной библиотеки. Разрешены:

- algorithms, ranges, type traits, concepts и `std::numeric_limits`;
- типы без общего engine-аналога, например `std::variant`;
- synchronization/time primitives без полного аналога в `xrCore`;
- внешний ABI, который требует точный `std`-тип;
- generated и third-party код, который проект не поддерживает как собственный;
- короткий adapter на границе NRI, DXC, JSON, RenderDoc, ImGui или OS API.

Для владеющего контейнера в основном engine-коде отсутствие аналога не является причиной заводить локальный alias. Если тип нужен системно, сначала добавляется и тестируется единый wrapper в `xrCore`.

Нельзя менять тип в:

- экспортируемой DLL-сигнатуре без проверки ABI;
- сериализуемой структуре без version migration;
- GPU buffer layout без общей C++/HLSL проверки размера и offsets;
- callback внешней библиотеки;
- стороннем или generated исходнике.

На границе данные преобразуются один раз. Внутри одного subsystem нельзя постоянно чередовать `std::vector` и `xr_vector` либо `std::string` и `xr_string`.

## Именование типов

### Таблица префиксов

| Префикс | Назначение | Пример |
| --- | --- | --- |
| `F` | Любая `struct`: value type, descriptor, handle, packet или простой data/helper type | `FMaterialTextureParameterBinding`, `FMeshBatch`, `FMaterialAssetId` |
| `E` | `enum` и `enum class` | `EMaterialBlendMode` |
| `I` | Чистый интерфейс с virtual contract | `IMaterialPreviewRenderer` |
| `T` | Только шаблонный тип | `TMaterialHandle<TTag>` |
| `Tiramisu` | Любой нешаблонный concrete-класс нового renderer/editor/scene/material pipeline | `TiramisuLegacyScene`, `TiramisuMaterialEditorDocument` |
| `b` | Boolean variable или поле | `bTwoSided`, `bIsReady` |

`F` обязателен для всех новых структур, даже если структура используется только внутри одного `.cpp`. Безымянная структура допустима только как деталь внешнего C API.

Префикс выбирается по семантике типа:

- пассивные данные с публичными полями оформляются как `struct F...`;
- concrete `class`, включая helper и document/controller, называется `Tiramisu...`; `F...` для `class` не используется;
- размер и роль concrete-класса не меняют правило полного префикса `Tiramisu...`;
- abstract contract называется `I...`;
- шаблон начинается с `T...`.

Префиксы `C`, `S`, `U` и `A` не используются в новом Tiramisu-коде. Существующие legacy-типы с такими именами не переименовываются вне отдельной задачи миграции.

### `T` не является сокращением Tiramisu

Текущие нешаблонные имена с `T` должны быть развёрнуты при последовательном рефакторинге:

| Было | Должно стать | Причина |
| --- | --- | --- |
| `TLegacyScene` | `TiramisuLegacyScene` | Concrete renderer class, не template |
| `TRenderDevice` | `TiramisuRenderDevice` | Владелец renderer lifecycle |
| `TRenderGraph` | `TiramisuRenderGraph` | Renderer subsystem, не template |
| `TRenderDeferredPass` | `TiramisuDeferredPass` | Renderer pass, не template |
| `TiramisuEditorRenderBridge` | `TiramisuEditorRenderBridge` | Адаптер основного Tiramisu renderer для редактора |
| `MaterialTextureParameterBinding` | `FMaterialTextureParameterBinding` | Структура данных |
| `THandle<TTag>` | `THandle<TTag>` | Это настоящий template |

Таблица задаёт направление именования, а не разрешение на немедленное массовое переименование. Перед каждым rename проверяются public headers, exports, serialization, CMake targets, тесты и editor integration.

### Enum

Используется `enum class EName : u8/u16/u32` с явно выбранным underlying type, если значение сериализуется, попадает в GPU ABI или binary bundle.

```cpp
enum class EMaterialBlendMode : u8
{
    Opaque,
    Masked,
    Translucent,
    Additive
};
```

Элементы enum не повторяют имя enum: `EMaterialBlendMode::Opaque`, а не `EMaterialBlendMode::BlendModeOpaque`.

### Функции, поля и параметры

- Функции и методы: `PascalCase` — `ResolvePass`, `CreateDynamicInstance`.
- Локальные переменные, параметры и поля нового кода: `PascalCase`.
- Boolean: префикс `b` — `bHasPipeline`, `bEnableValidation`.
- Выходной параметр: `OutResult`; входной-выходной: `InOutGraph`.
- Счётчик: `MaterialCount`; индекс: `MaterialIndex`; идентификатор: `MaterialId`.
- Число байт: `BufferSizeBytes`; offset в байтах: `DataOffsetBytes`.
- Сырые suffix-ы `m_`, `_`, `p_` и Hungarian notation в новом Tiramisu-коде не используются.

Предпочтителен return value. Output parameter применяется для нескольких результатов, переиспользования storage или требования внешнего API.

### Аббревиатуры

Аббревиатура внутри собственного имени рассматривается как слово:

- `Gpu`, `Cpu`, `Nri`, `Hlsl`, `Json`, `Guid`, `Ui`;
- `FMaterialGpuData`, `GenerateHlsl`, `ParseJson`;
- не `FMaterialGPUData`, `GenerateHLSL`, `ParseJSON`.

Официальное написание сохраняется у внешних типов, constants и protocol names: `nri::Device`, `ID3D12Device`, `DXC_OUT_ERRORS`, `NRI_BASE_INSTANCE`.

## Namespace

Tiramisu не оборачивается целиком в `namespace Tiramisu` и не создаёт глубокие namespace вроде `Tiramisu::Renderer::Material::Runtime`.

Разрешены:

- anonymous namespace внутри `.cpp` для приватных функций, constants и helper types;
- короткий named namespace для реальной независимой границы: generated code, parser implementation, backend-specific adapter или зеркало внешнего API;
- существующий namespace внешней библиотеки.

Запрещены:

- `using namespace ...` в header;
- глобальный `using namespace ...` в `.cpp`;
- namespace только для компенсации слишком общего имени типа;
- объявление engine API в `std`, кроме разрешённых стандартом специализаций для пользовательского типа;
- namespace alias в публичном header без необходимости ABI/API.

Внутри функции допустим точечный `using std::swap;` или аналогичный import для ADL. Если нужен namespace, его назначение должно быть понятно без изучения всех файлов модуля.

## Struct или class

Используется `struct`, когда тип:

- является пассивным набором данных;
- не охраняет сложный invariant;
- допускает публичные поля;
- удобен для serialization, command packet или GPU layout.

Используется `class`, когда тип:

- управляет lifetime ресурса;
- скрывает invariant;
- выполняет orchestration;
- владеет thread-affine состоянием;
- представляет polymorphic interface.

GPU ABI структура должна быть trivially copyable, иметь фиксированные типы полей и проверки `sizeof`, `alignof` и значимых offsets. В неё нельзя помещать `xr_string`, container, smart pointer или virtual method.

## Пример целевого кода

```cpp
struct FMaterialTextureParameterBinding
{
    FMaterialParameterId ParameterId;
    u32 DescriptorIndex = 0;
    u32 SamplerIndex = 0;
};

enum class EMaterialResolveResult : u8
{
    Ready,
    Pending,
    InvalidHandle
};

class IMaterialPassResolver
{
public:
    virtual ~IMaterialPassResolver() = default;
    virtual EMaterialResolveResult ResolvePass(
        FMaterialHandle Material,
        FMaterialPassProxy& OutPass) = 0;
};

class TiramisuLegacyScene final
{
public:
    void SubmitMeshes(xr_span<const FMeshBatch> MeshBatches);

private:
    xr_vector<FMeshBatch> PendingMeshBatches;
    xr_hash_map<FMaterialAssetId, FMaterialHandle> MaterialHandles;
    xr_atomic_bool bIsUploadPending = false;
};
```

## Комментарии и контракты

Комментарии нового Tiramisu-кода пишутся на русском языке. Английские термины API (`render thread`, `material`, `pass`, `fence`, `descriptor`) допустимы, когда перевод ухудшает связь с именами в коде.

Обязательный комментарий ставится у:

- каждого нового публичного типа и нетривиальной структуры данных;
- функции с требованиями к `game thread`/`render thread` или другому thread-affine состоянию;
- операции, которая передаёт владение, публикует snapshot, использует deferred deletion или зависит от завершения GPU fence;
- ABI, serialization, shader contract, fallback и migration boundary;
- алгоритма, invariant или причины, которые нельзя надёжно восстановить только по сигнатуре.

Комментарий описывает контракт, владение, поток или причину решения. Он не должен пересказывать имя функции. Тривиальные getters/setters, очевидные constructors и destructors не требуют отдельного комментария. Устаревший legacy-код не комментируется массово, но каждый новый adapter между legacy content и Tiramisu получает явное описание границы совместимости.

## Include и границы модулей

- Header включает то, что нужно его собственным declarations; нельзя зависеть от случайного precompiled header include.
- Forward declaration используется, если полный тип не нужен для layout, inheritance или inline-кода.
- Порядок: парный header, project headers, third-party headers, standard headers.
- Public API `xrTiramisuMaterialCore` не включает NRI, D3D12, Vulkan, ImGui или editor headers.
- NRI-типы не выходят из renderer implementation и renderer-owned adapters.
- Generated/third-party headers изолируются adapter-ом; их стиль не переписывается.

## Порядок рефакторинга

Рефакторинг выполняется по subsystem, а не глобальной текстовой заменой:

1. Зафиксировать public API и serialization/GPU ABI выбранного subsystem.
2. Переименовать типы по правилам `F/E/I/T/Tiramisu`.
3. Заменить владеющие STL containers и строки на engine-типы.
4. Удалить лишние namespace и глобальные `using namespace`.
5. Обновить includes, CMake source lists, tests и документацию.
6. Собрать все затронутые targets без `/MP1`.
7. Запустить все renderer/editor тесты с точным аргументом `-rdbg`.
8. Повторить доступные CPU-тесты и smoke runs под ASan.
9. Не смешивать функциональные изменения с механическим rename, если это мешает review или поиску regression.

Новый код не должен увеличивать количество нарушений. При изменении старого файла исправляется затронутая область, но соседний несвязанный legacy-код не переписывается без отдельной задачи.

## Checklist для review

- [ ] Каждая новая структура имеет префикс `F`.
- [ ] Каждый enum имеет префикс `E` и, где требуется, явный underlying type.
- [ ] Каждый interface имеет префикс `I`.
- [ ] `T` используется только шаблонным типом.
- [ ] Каждый concrete `class` нового Tiramisu pipeline использует полное `Tiramisu`; `class F...` отсутствует.
- [ ] Нет нового `C`-префикса и нет сокращения Tiramisu до одной буквы.
- [ ] Вместо доступного `std` owning type используется engine-аналог.
- [ ] Исключение для `std` находится на понятной границе и не протекает по subsystem.
- [ ] Нет глобального `using namespace`.
- [ ] Нет искусственных переносов ради ограничения в 80/100/120 символов.
- [ ] Serialization, DLL ABI и C++/HLSL layouts не изменились случайно.
- [ ] Thread-affine методы содержат соответствующую проверку.
- [ ] Tests запущены с `-rdbg`; доступная ASan-конфигурация также проверена.
