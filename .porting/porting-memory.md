# Porting memory — IX-Ray → macOS Metal

> Файл «памяти порта» по методологии Apple (`porting-methodology`): то, что должно быть в голове в начале каждой сессии.
> Обновлять в конце каждой сессии/вехи.

## Context

- **Цель проекта:** нативная macOS/arm64 версия IX-Ray 1.6, рендерер **R4** через новый **Metal-бэкенд** в `src/xrRHI`.
- **Репозиторий:** `~/Hermes/ixray-mac-port/ixray-src`, ветка **`macos-metal`** (от `default` = `478899e68`, 13.10.2026).
- **План:** `~/Hermes/ixray-mac-port/.hermes/plans/2026-10-16_124500-ixray-macos-dxvk-native-port.md`.
- **Артефакты:** `ixray-src/.porting/` (этот файл, `discovery-report.md`, далее goal/handoff).
- **Инструменты (проверено 16.10):** Xcode 27.1 (лицензия принята) · MetalToolchain 27.1.266.1 · `metal` 32023.921 (target `air64-apple-darwin27`) · `metallib` · `metal-shaderconverter` **4.0.3** (`/usr/local/bin`, triple `air64-apple-macosx15`) · заголовки `/usr/local/include/metal_irconverter/`.
- **GPTK-скиллы:** `~/Hermes/ixray-mac-port/gptk/game-porting-skills/skills/` (25 шт.) + `docs/metal-shader-converter/` + `samples/StartingAGamePortWithMetal`.
- **Не трогать:** репозиторий OpenXRay (решение Степана), оригинальная папка игры `~/Hermes/OpenXRay/game` (только копии).

## Решения проекта

| Решение | Статус |
|---|---|
| Цель v1 — только **R4** (DX11); R1/R2 (DX9) вне области | зафиксировано |
| Маршрут **B**: нативный Metal-бэкенд в слое `xrRHI` (не DXVK Native + MoltenVK) | зафиксировано |
| Шейдеры — **офлайн-печь** `HLSL → DXC(DXIL) → metal-shaderconverter → metallib` + кеш | зафиксировано |
| metal-cpp (C++), не ObjC/Swift | зафиксировано (стек проекта — C++, платформенный код в Platform-слоях) |
| DLSS / FSR3 / XeSS на mac **выключены**; в перспективе FSR → MetalFX | зафиксировано |
| `IXRAY_MP` (мультиплеер) вне v1 | зафиксировано |
| Baseline меряем в **Evaluation Environment** (Wine + D3DMetal), не кодом порта | зафиксировано |

## ЖЕЛЕЗНОЕ ПРАВИЛО: не ломать другие платформы

**Чужой код не удаляем и не переписываем** (требование Степана, 16.10.2026). Это форк живой игры: **Windows-сборка обязана продолжать работать**, Linux — тоже. Любая правка аддитивная; ветки `MSVC` / `WIN32` / `IXR_WINDOWS` / `IXR_LINUX` не трогаем. Перед каждым коммитом — сверка, что `git diff` не меняет поведение Windows/Linux-ветвей.

**Аудит коммита `1f938e58c`** (проведён по требованию):
- ветки `MSVC` (`xrCore/CMakeLists.txt`), `WIN32` (`cmake/clang.cmake`), `#include <malloc.h>` (`trivial_encryptor.h`), `wchar_t` (`stack_string.h`) — **целы** ✓
- ссылок на старое имя `_types.h` в коде не осталось ✓; 75 проектных файлов VS в репозитории этот файл не упоминают ✓; обновлены две ссылки в документации ✓
- правки, затрагивающие все платформы, — только два случая, и оба платформенно-нейтральны: переименование файла (см. выше) и `.hpp`→`.h` в `sse2neon.cmake` (на `.hpp` не ссылался никто, а все четыре платформы включают `.h`)

## Watch list (что держать в голове)

1. **Комбинации шейдеров.** Рантайм-компиляция через `D3DCompile` (`src/xrRHI/RHIShaderCompiler.cpp:24`) с макро-комбинациями. Полнота офлайн-печи критична: промах = розовая заглушка. Инструмент dump-комбинаций — обязателен.
2. **DXC на macOS** — официального билда нет; цепочка решения в Task 2.5.2 (сборка из исходников → Wine-dxc офлайн → CI).
3. **`UNIX AND NOT APPLE`** в `src/xrRHI/CMakeLists.txt:52` — «замок», который надо открыть новой веткой `elseif(APPLE)`.
4. **Утечка D3D11:** `xrRenderDX10` — 231 упоминание / 7 файлов; в R4 128/21, но значительная часть — обёртки апскейлеров, которые выключаем.
5. **Правила кода IX-Ray:** табы + Allman, UTF-8 **+ CRLF**, PascalCase без `m_`, комментарии по-английски, платформенный код только в Platform-модулях, макрос `IXR_APPLE_SERIES`.
6. **Автотест** `xrEngine -autotest … -autotest_cmd <cmds>` — обязательный регресс после каждого таска Phase 2.

## Feature status

| Фича | Статус | Где |
|---|---|---|
| Ветка + артефакты | ✅ сделано | Task 0.1 |
| Замер утечки D3D11 + контракт RHI (~153 метода) | ✅ сделано | Task 0.2 |
| Инвентарь шейдеров (2172 HLSL; 209 под R4) | ✅ сделано | Task 0.3 |
| Baseline в Evaluation Environment | ⚠️ частично (см. журнал) | Task 0.4 |
| Сборка ядра под macOS | ✅ **`libxrCore.dylib` собран и слинкован** (Task 1.3) | Phase 1 |
| Платформенная ветка под macOS | ✅ сделано (в коде `IXR_MACOS`; в доке/плане — `IXR_APPLE_SERIES`, расхождение свести) | Task 1.2 |
| Звук / сеть / скрипты / зависимости | ✅ `xrSound`, `xrNetServer`, `luabind`, `imgui`, `SDL3` собраны. `xrPhysics`/`xrScripts` отдельными целями не собирались | Task 1.4 (частично) |
| RHI с родными D3D-бэкендами | ✅ **`libxrRHI.dylib`** — родные бэкенды D3D9 и D3D11 IX-Ray собираются против заголовков DXVK Native | Task 1.2/2.1′ |
| Ядро движка | ✅ **`libxrEngineCore.dylib` собран и слинкован** | Task 1.5 (частично) |
| **Родной рендер R4 через DXVK Native** | 🟡 **компилируется целиком, линковка блокирована**: нет `D3DCompile`/`D3DReflect`/`D3DDisassemble`/`D3DGetInputSignatureBlob` и `DirectX::CreateTextureEx` | Task 2.5′ (= Task 0.3/2.5 плана) |
| `xrPlay` (exe) + автотест | ⬜ не собрано | Task 1.5/1.6 |
| Metal-бэкенд в `xrRHI` | ⛔ **снят**: решением Степана от 14:12 рендер берём родной (R4 + DXVK Native), свой не пишем | Phase 2 (заменена) |
| Игра на Mac | ⬜ не начато | Phase 3 |

> **Смена маршрута (16.10, 14:12).** Phase 2 исходного плана (свой Metal-бэкенд) **заменена**:
> рендер IX-Ray (R4/D3D11) берётся как есть и работает через **DXVK Native** — тот же путь, что у самого
> проекта на Linux. Замер Task 0.2 (утечка D3D11 выше RHI) закрыт не развязкой интерфейса, а тем, что
> RHI получил настоящие D3D-заголовки из DXVK. Подробности — раздел «Рабочий рецепт» ниже.
> **Следствие:** Task 1.5/1.6 в исходном виде («движок без рендерера») не выполнялись — рендерер у нас
> есть; но `xrPlay` и автотест всё равно понадобятся как регресс-тест.

## Коммиты ветки `macos-metal`

| Коммит | Что |
|---|---|
| `1f938e58c` | первый прорыв: PCH собирается, xrCore компилируется (11 правок) |
| `234bb9ed4` | правило «не ломать другие платформы» + дочистка ссылок на переименованный файл |
| `502a8a69b` | **`xrCore` собран и слинкован на arm64** (слой памяти + переименование `thread_t`) |

## Правки второго коммита сборки (`502a8a69b`)

| Файл | Что сделано | Затрагивает другие платформы? |
|---|---|---|
| `memory/memory_usage.cpp` | `<malloc.h>` под `#ifndef IXR_MACOS` | нет — Win/Linux видят тот же инклюд |
| `memory/xrMemory_align.cpp` | то же | нет |
| `memory/xrMemory_pso_Fill32.cpp` | добавлена ветка `#if defined(IXR_ARM64)` с портативной заливкой; их `IXR_X64` стала `#elif` | нет — x64 и x86-ветки не тронуты |
| `memory/xrMemory_pso_Copy.cpp` | то же (портативный `memcpy`) | нет |
| `Platform/MacOS/OSMemory.h` | добавлен `malloc_usable_size` (на macOS это `malloc_size`) | нет — файл только для macOS |
| `_math.h`, `_math.cpp` | тип движка `thread_t` переименован в `xr_thread_entry_t` (на macOS `thread_t` — системный `mach_port_t`) | только переименование, семантика та же; остальных использований нет (проверено grep) |

## Рабочая команда конфигурации (проверена, Task 1.1 + OpenAL)

```bash
cd ~/Hermes/ixray-mac-port/ixray-src
cmake -S . -B build-macos -G Ninja \
      -DCMAKE_BUILD_TYPE=Debug \
      -DCMAKE_OSX_ARCHITECTURES=arm64 \
      -DIXRAY_USE_R1=OFF -DIXRAY_USE_R2=OFF \
      -DIXRAY_PROFILER=OFF \
      -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
      -DCMAKE_FIND_FRAMEWORK=LAST \
      -DCMAKE_PREFIX_PATH=/opt/homebrew/opt/openal-soft \
      -DCMAKE_CXX_FLAGS="-I/opt/homebrew/opt/openal-soft/include" \
      -DCMAKE_C_FLAGS="-I/opt/homebrew/opt/openal-soft/include"
```

Что откуда:
1. **`-DCMAKE_POLICY_VERSION_MINIMUM=3.5`** — иначе CMake 4.x отвергает старые `cmake_minimum_required` в зависимостях.
2. **`ispc`** (`brew install ispc`) — для `redimage`/`ispc_texcomp` (сжатие текстур BC7).
3. **`ninja`** (`brew install ninja`).
4. **`-DCMAKE_FIND_FRAMEWORK=LAST`** + `CMAKE_PREFIX_PATH` + явный `-I…/openal-soft/include` — иначе `find_package(OpenAL)` берёт **системный Apple-фреймворк** (в нём заголовки лежат как `<OpenAL/…>` и нет EFX). Причём Homebrew-овский `openal-soft` — keg-only, и CMake находит его заголовки как `include/AL`, а код включает `<AL/al.h>`, поэтому нужен родительский `include` (на Linux этого не требуется — там заголовки и так в системном пути).

## Рабочий рецепт: родной рендер IX-Ray на macOS через DXVK Native

Решение Степана (16.10, 14:12): **своего рендера не пишем** — на macOS должен работать **родной рендер IX-Ray (R4/D3D11)**. Путь — тот же, что у проекта на Linux: DXVK Native (Vulkan через MoltenVK, окно через SDL3). Это подтверждает и их `AGENTS.md`: «`xrRHI` — D3D9 and D3D11 backends; **Linux uses DXVK Native**».

Что выяснено и сделано:

- **R4, `xrGame`, `xrPlay` включаются флагом `-DIXR_TEST_CI=ON`** (иначе в сборке их нет вообще).
- **Upstream DXVK master на macOS не собирается:** Apple-линкер не знает GNU-опцию `--version-script`, плюс ошибка `call to 'tzcnt' is ambiguous` в `dxvk_bind_mask.h`. Компиляция при этом проходит целиком — падает только линковка (`libdxvk_dxgi.0.dylib`).
- **Фиксы живут в форке.** Прописан Apple-only пин: `generalsx-project/dxvk` @ `f66da559fd48cd55ddfebca38ed0f708c0f76a24` (форк рабочий, из проекта GeneralsX — там на нём собирают macOS-порт). Windows/Linux продолжают качать upstream master.
- **Опции meson различаются по версии:** у форка (DXVK 2.6) — `-Ddxvk_native_wsi=sdl3`, а старые `-Dnative_sdl3=enabled/-Dnative_sdl2/-Dnative_glfw` в нём отсутствуют (meson падает с `Unknown option`). Сделана Apple-only подстановка (`DXVK_WSI_ARGS`), Linux-набор не тронут.
- **Vulkan SDK от LunarG для сборки не нужен:** хватает заголовков Homebrew (`vulkan-headers`, `vulkan-loader`, `glslang`, `meson`). Он нужен только в рантайме, и там достаточно `MoltenVK` (есть готовый в CrossOver по пути `lib64/libMoltenVK.dylib`, либо релизы KhronosGroup/MoltenVK).
- **D3D-заголовки для компиляции рендерера** берутся из дерева DXVK: `${DXVK_SOURCE_DIR}/include/native` (там `windows/` и `directx/`) — это то, чего не хватало для `xrRHI`/`D3D11`; путь надо добавить в инклюды после успешной сборки DXVK (проверить фактически).
- Их стиль (их же `AGENTS.md`): **комментарии на английском**, хак помечается `// HACK:`. Все мои комментарии приведены к английскому.

## ПРАВИЛО КОММЕНТАРИЕВ (Степан, 16.10)

- Любой комментарий в правках начинается с **`AI:`** (тегирую каждую строку многострочных блоков — так сформулировано правило, и так не остаётся двусмысленности; если Степан захочет тег только на первой строке блока — это правится одной правкой).
- Стиль — как у разработчиков IX-Ray: в C/C++ комментарии английские и короткие, в cmake — русские (по языку соседних комментариев в файле).
- Если код или подход взят из **OpenXRay** — это указывается в комментарии (пример: ключевая функция `IReaderBase` — подход подсмотрен в OpenXRay, где интерфейс `IReader` помечен `XRCORE_API`).
- Аудит (16.10): 47 комментариев в 29 файлах, без тега `AI:` — **ноль**.

## Системные правки вместо заплаток

- **`<malloc.h>`**: его включают **24 файла** движка (на Linux он есть в glibc, поэтому автор кода включал его свободно), плюс скачиваемый DirectXMesh. Вместо того чтобы латать каждый файл, добавлен совместимый заголовок `src/3rd-party/macos-compat/malloc.h`, и его каталог подключён к сборке только на macOS (`if(APPLE)` в верхнем `CMakeLists.txt`). Ранее сделанные точечные заплатки `#ifndef IXR_MACOS` вокруг `#include <malloc.h>` **откачены** — чужой код вернулся в исходный вид.
- **`ODE`**: в `3rd-party/ode/include/ode/config.h` для Apple берётся `<alloca.h>` и `<stdlib.h>` вместо `<malloc.h>` (там же лежат `malloc`/`realloc`).

## Последний блокер до рендера

- **GameSpy** (`src/xrGameSpy/gamespy/`) — платформенная развилка `gsPlatform.h` не знает Apple → `#error "The GameSpy SDKs do not support this operating system"`, плюс `sockaddr_in` (`<netinet/in.h>`) и `GSICriticalSection`. Это часть **мультиплеера**, а `IXRAY_MP` по умолчанию **OFF**; но заголовки GameSpy подтягиваются кодом xrGame. Чинится добавлением ветки Apple в их же развилку (третья сторона, правка аддитивная).
- Всё остальное в xrGame/xrUI/xrPhysics/xrScripts компилируется; упор остаётся в `xrRHI/RHI.cpp` — точке создания рендер-устройства (STOP: решение DXVK или DXMT).

## ⚠️ НЕ полагаться на Linux-ветку как на образец (напоминание Степана, 16.10)

- **Linux-версия IX-Ray недоделана и не работает.** Поэтому: (1) нельзя копировать файлы `Platform/Linux/*` в `Platform/MacOS/*` — именно так в mac-платформе и появились glibc-инклюды (`malloc.h`, `/proc/self/exe`, `<bits/local_lim.h>`), которые я потом вычищал; (2) нельзя считать «Linux собирает D3D11 через DXVK» доказательством, что наш macOS-путь верный; (3) для не-рендерных частей эталон — **работающий macOS-слой OpenXRay** (разрешение Степана 13:30), а не Linux-ветка IX-Ray.

## Правила проекта о платформах (их же документация)

`docs/docs/main/ru/main/coding-guidelines.md:113`:
- платформозависимый код должен жить в соответствующих `Platform`-реализациях (**`Windows`, `Linux`, `macOS`** — macOS заявлен как платформа);
- `#ifdef` допустим только внутри платформенных модулей, и в условии используется один из макросов **`IXR_WINDOWS`, `IXR_LINUX`, `IXR_APPLE_SERIES`, `IXR_BSD_SERIES`**.
- Отсюда важное расхождение: документация называет макрос **`IXR_APPLE_SERIES`**, а код определяет и использует **`IXR_MACOS`** (в `Platform.h`), при этом `IXR_APPLE_SERIES` и `IXR_APPLE` не определены нигде. Свести их надо будет позже (определить документированный макрос рядом с рабочим), не ломая существующий код.
- Мои правки сейчас вынужденно стоят в общих файлах (`FS.h`, `stack_string.h`, …) с `#ifdef IXR_MACOS`. По стандарту проекта их место — в `Platform/MacOS/`; это кандидат на аккуратную доработку после того, как сборка встанет целиком.

## КЛЮЧЕВАЯ НАХОДКА: слой RHI завязан на D3D-типы

Проверено (16.10):

- Из **всех** модулей движка RHI нужен только **`xrEngine`** — 4 файла (`EngineAPI.h`, `device.h`, `Autotest.cpp`, `StatGraph.cpp`), и им достаточно **чистых** заголовков `RHI.h` и `RHIEnums.h`. `xrGame`, `xrUI`, `xrSound`, `xrPhysics`, `xrScripts` RHI **не касаются вообще** — они падают лишь транзитивно, из-за сборки самого `xrRHI`.
- Но `xrRHI` **не является чистым швом**: его реализация включает D3D-заголовки напрямую — `RHI.cpp` → `D3D9/Device.h`, `D3D11/Device.h`, `D3DX*`; `Private/RHIStateCache.h` → `<d3d11.h>`; `RHIShaderCompiler.cpp` → `<d3dcompiler.h>`. На Windows они из Windows SDK, на Linux — из DXVK-Native, на macOS их нет.
- Пробовали взять заголовки из mingw-w64 (их же путь на Linux): `_mingw.h` даёт `#error Only Win32 target is supported!` без `_WIN32`; с `-D_WIN32 -fms-extensions` конфликтуют `_exception_code`/`_CRTIMP`. **На macOS этот путь не работает** — заголовки только для Win32-таргета. Ставить костыль (подделка D3D-заголовков) не стали — это как раз тот случай, где нужен «стоп/спросить».
- **Следствие для плана:** порядок фаз скорректирован — «окно на Mac» (Task 1.5) недостижимо раньше, чем `xrRHI` начнёт собираться на macOS, а это и есть работа Phase 2 (Metal-бэкенд + развязка интерфейса RHI от D3D-типов). Phase 1 останавливается на `xrCore` ✅ + `xrSound` ✅ + всех зависимостях ✅.

## Статус сборки модулей (на 16.10, 13:30)

| Модуль | Состояние | Что мешает |
|---|---|---|
| `xrCore` | ✅ **собран и слинкован** (`libxrCore.dylib`, 6.4 МБ) | — |
| `xrSound` | 🟡 **компилируется целиком** (452 объекта), линковка падает | `Undefined symbols: vtable for IReaderBase` (см. ниже) |
| `xrScripts`, `xrPhysics`, `xrEngine`, `xrUI` | 🔴 падают на общей зависимости | `build-macos/_deps/directxmesh-src/DirectXMesh/DirectXMeshP.h:142: 'malloc.h' file not found` — третьесторонняя библиотека Microsoft (DirectXMesh), тянется самим проектом |
| `xrNetServer` | ⚪ вне v1 | своя ошибка `_localtime64` (MSVC-специфика); нужен только для `IXRAY_MP` |

**Про DirectXMesh:** правильный путь — не править скачанный код, а добавить шаг патча в `cmake/modules/DirectXMesh.cmake` — проект **сам так делает** для других зависимостей (в логе конфигурации видно `-- Added pragma once to nvapi_lite_salend.h`), то есть механизм в `cmake/functions.cmake` уже есть.

## Известные пробелы macOS-платформы (не блокируют сборку, но починить до запуска)

- `Platform/MacOS/OSMemory.h`: `Platform::LoadLibrary` — копия linux-версии: ищет `/proc/self/exe` (на macOS его нет, нужен `_NSGetExecutablePath`) и добавляет суффикс `.so` вместо `.dylib`.
- **`xrSound` не линкуется:** `Undefined symbols: "vtable for IReaderBase"` (ссылка из `IReaderBase::IReaderBase()` в `SoundRender_Core.cpp.o` — там есть `new IReader(_data, …)`, а `IReader` наследует `IReaderBase`).
  **Что измерено (не догадки):**
  - в `libxrCore` есть **обе** vtable, но с разной линковкой: `S __ZTV12CMultiReader` и `S __ZTV11CFileReader` — **экспортные**, а `s __ZTV11IReaderBase` — **локальная** (см. `nm`);
  - у экспортных классов есть **ключевая функция** (первый не-inline не-чистый виртуальный метод, определённый вне класса) — clang кладёт vtable в один TU и экспортирует её; у `IReaderBase` все виртуальные либо inline, либо чистые (`= 0`), то есть ключевой функции нет → vtable получается локальной;
  - пометка класса `XRCORE_API` (как у их же `IReader`/`CMultiReader`) **не помогла** — символ остался локальным. Значит дело не в видимости, а в отсутствии ключевой функции.
  **Следующий шаг:** не гадать, а поставить чистый эксперимент — собрать крошечную dylib+exe с такой же иерархией классов и посмотреть, какой вариант экспортирует vtable (варианты: ключевая функция вне класса; `-fvisibility=default` для конкретных единиц; явный анкор). Только после этого править код движка.
  **Важно:** правки в чужие классы делать нельзя, поэтому ищем вариант, который ничего не ломает на Windows.

## Грабли окружения (записать и не наступать повторно)

- **Инструмент правки файлов переводит файл в CRLF целиком**, из-за чего `git diff` показывает изменённым весь файл, а не одну строку. Если файл в репозитории был с LF — diff превращается в шум на сотни строк. Лечится так: восстановить файл из индекса (`git checkout -- <файл>`), определить перевод строки в файле и дописать нужный блок скриптом, а не редактором. В спорных случаях сверять `git diff --numstat` перед коммитом.

1. **CrossOver игнорирует `WINEPREFIX`** — бутылка выбирается через **`CX_BOTTLE`** (иначе `Unable to find the 'default' bottle`).
2. **CrossOver игнорирует `WINEDLLOVERRIDES`** — override'ы только через реестр бутылки (`user.reg`, секция `[Software\Wine\DllOverrides]`) или `wine reg add`.
3. **IX-Ray требует настоящий VC++ Runtime** (использует `concrt140`/Concurrency Runtime). Без него: `unimplemented function concrt140.dll.??0_TaskCollection... aborting`. Лечение: `vc_redist.x64.exe` в бутылку + CRT в `DllOverrides` = `native`.
4. **D3DMetal умеет только DirectX 11/12** — DX9-рендереры (R1/R2) в baseline рисовать не смогут.
5. Аварийный диалог Wine закрывается **клавиатурой** (активировать окно `xrEngine.exe` → Enter) — kill не нужен.

## Рабочая команда конфигурации (проверена, Task 1.1)

```bash
cd ~/Hermes/ixray-mac-port/ixray-src
cmake -S . -B build-macos -G Ninja \
      -DCMAKE_BUILD_TYPE=Debug \
      -DCMAKE_OSX_ARCHITECTURES=arm64 \
      -DIXRAY_USE_R1=OFF -DIXRAY_USE_R2=OFF \
      -DIXRAY_PROFILER=OFF \
      -DCMAKE_POLICY_VERSION_MINIMUM=3.5
```

Три условия, добытых опытом:
1. **`-DCMAKE_POLICY_VERSION_MINIMUM=3.5`** — иначе CMake 4.x отвергает старые `cmake_minimum_required` в зависимостях (libogg, redimage/nvtt/libsquish).
2. **`ispc`** (`brew install ispc`, 1.32.0) — нужен `redimage`/`ispc_texcomp` для сжатия текстур (BC7).
3. **`ninja`** (`brew install ninja`).

Итог: `Configuring done` / `Generating done` ✓ — конфигурация под arm64 проходит.

## Правки для сборки под macOS — коммит `1f938e58c` (ветка `macos-metal`)

| # | Файл | Что сделано |
|---|---|---|
| 1 | `cmake/clang.cmake` | `_GNU_SOURCE`, `-fuse-ld=lld` и Linux-пути — только не для Apple |
| 2 | `src/xrCore/Platform/Platform.h` | на Apple Silicon определяется `IXR_ARM64` (раньше всегда `IXR_X64` — движок считал M4 «интеловским») |
| 3 | **`src/xrCore/Platform/PlatformAPI.h`** | **ключевой баг:** ветка macOS проверяла `IXR_APPLE`, а движок определяет `IXR_MACOS` → на macOS не подключался **ни один** платформенный заголовок |
| 4 | **`src/xrCore/_types.h` → `xr_core_types.h`** | **ключевой баг:** имя `_types.h` перехватывало системный заголовок Apple `<_types.h>` (его включает `<wchar.h>`), из-за чего рассыпался весь SDK |
| 5 | `Platform/MacOS/PlatformInit.h` | убраны `<linux/limits.h>` и glibc-шный `<bits/local_lim.h>`; ARM-ветка через `<arm_neon.h>` |
| 6 | `Platform/MacOS/OSFile.h` | macOS не даёт `LOGIN_NAME_MAX`/`HOST_NAME_MAX` → фолбэки `MAXLOGNAME`/`MAXHOSTNAMELEN` из `<sys/param.h>` |
| 7 | `Platform/MacOS/OSThread.cpp` | `pthread_setname_np` на macOS принимает **один** аргумент (на Linux — два) |
| 8 | `stack_string.h` | `xr_stack_wstring` не был определён ни для macOS, ни для BSD → ветка `#else` с `char16_t` |
| 9 | `Crypto/trivial_encryptor.h` | `<malloc.h>` — glibc-only, у macOS такого заголовка нет |
| 10 | `src/xrCore/CMakeLists.txt` | платформы разведены только для MSVC и Linux → для Darwin собирались исходники **BSD и macOS сразу**; добавлена ветка Darwin |
| 11 | `cmake/modules/sse2neon.cmake` | качал `sse2neon.hpp`, хотя все четыре платформы включают `sse2neon.h` |

**Результат:** PCH собирается ✓, `xrCore` компилируется по-настоящему (было «0 файлов», стало «12/21»).

## Открытые ошибки: слой памяти xrCore

| Файл | Ошибка | Что нужно |
|---|---|---|
| `memory/memory_usage.cpp` | нет `<malloc.h>` | на macOS брать `<malloc/malloc.h>` (`malloc_size` вместо `malloc_usable_size`) |
| `memory/xrMemory_align.cpp` | нет `<malloc.h>` | то же |
| `memory/xrMemory_pso_Copy.cpp` | `unsupported architecture 'arm64' for MS-style inline assembly` | MS-style `__asm`-блоки → заменить на NEON/`memcpy` под `IXR_ARM64` |
| `memory/xrMemory_pso_Fill32.cpp` | то же | то же |

## Системная закономерность (важно для остальных модулей)

В проекте **13 мест**, где ветки написаны только под `IXR_WINDOWS` и `IXR_LINUX`, а Apple не учтён. В пути сборки xrCore задействованы: `xrCore.h`, `xrSyncronize.h`, `_thread_types.h`, `PlatformAPI.h` (исправлен), `stack_string.h` (исправлен). Остальные — в `xrGameSpy` и `Editors` (вне v1).

Ещё одна находка: в проекте **три разных имени** для одной платформы — `IXR_MACOS` (реально определяется в `Platform.h`), `IXR_APPLE` (используется 1 раз, не определён нигде), `IXR_APPLE_SERIES` (упомянут в их же документации и в `Editors/xrEUI`, но нигде не определён). Пока ориентируемся на `IXR_MACOS`.

## Журнал (append-only)

- **2026-10-16, сессия 1** — Phase 0 начат и в основном закрыт:
  - создана ветка `macos-metal`, заведён `.porting/`;
  - Task 0.2: замер утечки D3D11 (`xrRenderDX10` 231/7 файлов, `xrRenderPC_R4` 128/21, R1/R2 чисто) + измерен контракт RHI (**≈153 виртуальных метода**);
  - Task 0.3: инвентарь шейдеров — **2172 HLSL всего, 209 под R4** (`gamedata/shaders/d3d11`), компиляция **в рантайме** через `D3DCompile` (`src/xrRHI/RHIShaderCompiler.cpp:24`);
  - оформлен `discovery-report.md`;
  - Task 0.4 (baseline): скачана и распакована сборка r1.3.6 в `cop-baseline/`, создана бутылка CrossOver `ixray`, снят баг с `concrt140` (см. грабли). **Открыто:** окно «IX-Ray Engine» открывается, но экран чёрный (CPU 0%). Отложено — не блокирует Phase 1.
  - **Следующий шаг: Phase 1, Task 1.1** (пресет сборки macOS + `brew install ninja`).

### 2026-10-16, сессия 2 — Task 2.3′ (компилятор шейдеров) и разбор Task 2.4′

**vkd3d собран нативно на macOS/arm64 — ✅**
- Источник: **официальный релиз 2.1** (tarball от 24.08.2026):
  `https://dl.winehq.org/vkd3d/source/vkd3d-2.1.tar.xz` → `/tmp/vkd3d-2.1`, out-of-tree сборка в `build-probe`.
- **git-master НЕ собирается:** `configure` проходит, но `libs/vkd3d/device.c` требует свежие D3D12-фичи
  (`D3D12_FEATURE_D3D12_OPTIONS19/21`), которых нет в сгенерированных `vkd3d_d3d12.h`; в git-дереве нет
  widl-заголовков, а `widl` на macOS отсутствует. Рабочий путь — только релизный tarball.
- Потребовалось: `brew install bison` (системный bison 2.3 не понимает `%code`, нужен
  `/opt/homebrew/opt/bison/bin` **в PATH на этапе make**), `vulkan-headers` + `spirv-headers`
  (`CPPFLAGS="-I/opt/homebrew/opt/vulkan-headers/include -I/opt/homebrew/opt/spirv-headers/include"`),
  `--disable-demos --disable-tests --without-spirv-tools`. `-Wl,--version-script` в Makefile.am уже под
  `HAVE_LD_VERSION_SCRIPT` — на Darwin не применяется сам.
- Артефакты: `libvkd3d-utils.dylib`, `libvkd3d-shader.dylib`, `libvkd3d.dylib`, `vkd3d-compiler`, `vkd3d-dxbc`.
- **Проверено `nm`:** `libvkd3d-utils` экспортирует ровно то, что просит R4 — `D3DCompile`, `D3DReflect`,
  `D3DDisassemble`, `D3DGetInputSignatureBlob` (+ `D3DCompile2`, `D3DPreprocess`, `D3DCreateBlob`, …).

**БЛОКЕР: vkd3d не умеет HLSL member-функции (`this`) — проверено на 2.1 И на master**
- Тест `/tmp/vkd3d-test/compile_test.c` (линк с `libvkd3d-utils`, свой `ID3DInclude`) реально компилирует
  шейдер R4: `D3DPreprocess` проходит, `D3DCompile` падает на
  `E5030: Unknown modifier "Lerp"` / `E5030: Unknown modifier "IXRayMaterial"` / `E5000: syntax error, unexpected '('`.
- Причина: `gamedata/shaders/d3d11/metalic_roughness_base.hlsli:33-58` — метод внутри `struct IXRayMaterial`
  (`IXRayMaterial Lerp(...) { IXRayMaterial O = this; ... }`). В лексере vkd3d `this` в списке `RESERVED3`
  (`libs/vkd3d-shader/hlsl.l:57`) — просто не реализовано; в `hlsl_struct_field` нет понятия «метод».
- Масштаб: этот заголовок тянут ~11 файлов R4 (все `metalic_roughness*`).
- Разведка готового патча: issues/MR в GitLab vkd3d по «member function»/«method» — **0**; поиск по коммитам
  через API — 401 (нужен токен). Готового патча не нашли.

**Task 2.4′ (`DirectX::CreateTextureEx`) — корень найден; «SAL-заглушек» недостаточно**
- Воспроизведено в изоляции: `wsl/wrladapter.h: unknown type name '_In_opt_'`, `'_Inout_'`,
  `declaration of 'U' shadows template parameter`, `expected ')'`.
- Разбор цепочки (`-H`): `DirectXTexP.h:141-143` (ветка `!WIN32`) включает `<wsl/winadapter.h>` →
  `<unknwn.h>`, и он разрешается в **DXVK-заголовок** (`dxvk/unknwn.h` → `windows_base.h`), где SAL не
  определён; заглушки DirectX-Headers (`wsl/stubs/basetsd.h`, где `_In_opt_`/`_Inout_` есть) в цепочку
  не попадают. Плюс на не-Windows ветке `DirectXTexP.h` **не включает `<d3d11.h>`** (на Windows его даёт
  ветка `_WIN32_WINNT >= WIN10`) — файл рассчитан на Windows-стек заголовков.
- **Главный факт (проверено компиляцией `/tmp/probe/guards.cpp`):** два стека Windows-заголовков
  **несовместимы в одном TU** — DXVK (`windows_base.h`: `typedef INT BOOL;`, `unsigned long DWORD`) и
  WSL-заглушки DirectX-Headers (`basetsd.h`: `typedef uint32_t UINT, ULONG, DWORD, BOOL;`) дают
  `error: typedef redefinition with different types`. Порядок инклюдов это не лечит.
- Следствие: подход плана «дать файлу SAL-заглушки» **не сработает**. Нужно решение Степана:
  (а) маленький macOS-шим с реализацией `CreateTextureEx` против DXVK-заголовков (по образцу
  `src/3rd-party/macos-compat/`), либо (б) другой способ получить символ (пока не найден).

**Состояние 2.2′ (перепроверено сборкой):** `libxrRender_R4.dylib` не слинкован, незакрытых символов 5 групп:
`D3DCompile`, `D3DDisassemble`, `D3DGetInputSignatureBlob`, `D3DReflect` (все из `r4.cpp`) +
`DirectX::CreateTextureEx` (из `dx11Texture.cpp`). Компиляция — 0 ошибок.

**ИТОГ сессии 2 — `libxrRender_R4.dylib` СЛИНКОВАН ✅**
- **Task 2.4′ — закрыта.** `DirectX::CreateTextureEx` портирован в
  `src/Layers/xrRenderPC_R4/DirectXTexD3D11_macos.cpp` (MIT). Тело — **дословно из апстрима**
  (`DirectXTex/DirectXTexD3D11.cpp`, строки 445-637) ради возможности диффать с оригиналом; отличия только
  два: внутренний макрос `HRESULT_E_NOT_SUPPORTED` и отсутствующий в DXVK-заголовках `E_UNEXPECTED` заменены
  локальными константами. Комментарии апстрима внутри тела оставлены как есть (это чужой код, как в
  `DDSTextureLoader9.cpp`); наши пояснения — в шапке файла, каждая строка с `AI:`. Файл добавлен в цель
  `xrRender_R4` **только под `if(APPLE)`**.
- **Task 2.3′ (линковка) — закрыта.** Новый `cmake/modules/VKD3D.cmake` собирает релизный vkd3d 2.1 под
  macOS на этапе конфигурации (FetchContent + autotools, `bison` из Homebrew в PATH, Vulkan/SPIR-V заголовки,
  `--disable-demos --disable-tests --without-spirv-tools`), ребейзит install_name на `@rpath/...` и отдаёт
  `VKD3D_LIBRARIES`; `xrRender_R4` линкует `libvkd3d-utils` **только под `if(APPLE)`**.
- **Проверено:** `cmake --build build-macos --target xrRender_R4` → **EXIT=0**, незакрытых символов **0**;
  `nm -gU` видит определённый `DirectX::CreateTextureEx`; `otool -L` — зависимость
  `@rpath/libvkd3d-utils.1.dylib`; четыре шейдерных символа резолвятся ею.
- **Открыто (линковку не блокирует):** member-функции HLSL в vkd3d — рантайм-компиляция ~11 шейдеров R4
  упадёт. Решение Степана пока не принято; смотреть по факту на автотесте (Task 2.5′).
- **TODO к Task 2.6′:** положить `libvkd3d-utils.1.dylib`, `libvkd3d-shader.1.dylib`, `libvkd3d.1.dylib`
  рядом с бинарником (`bin/Debug` уже в rpath цели) — список отдаётся переменной `VKD3D_DYLIBS`.
  **Сделано:** модуль сам копирует их в `bin/<config>` на этапе конфигурации.

**Task 2.5′ — exe собран, начат рантайм (первые два бага macOS-слоя закрыты)**
- `bin/Debug/xrEngine` собран и слинкован (Task 2.5′ шаг 1) — цель называется **`xrEngine`**, не `xrPlay`.
- Площадка запуска: `~/Hermes/ixray-mac-port/run-macos/` — бинарники + `fsgame.ltx` + **симлинки** на ассеты
  (`gamedata`, `levels`, `resources`, `localization`, `mp`, `patches`) + копия `_appdata_`; рантайм-библиотеки:
  `libsteam_api.dylib` (из `build-macos/dep/steamworks_sdk_164/sdk/redistributable_bin/osx/`), **MoltenVK 1.4.2**
  (официальный релиз `MoltenVK-macos.tar`) + его `MoltenVK_icd.json`; запуск с `VK_ICD_FILENAMES=$PWD/MoltenVK_icd.json`.
- **Баг 1 (mac-слой, `Platform/MacOS/OSFile.h`):** `Platform::GetAbsolutePath` — копия linux-версии,
  `strcpy(dest, realpath(path, 0))`; `realpath` возвращает NULL для пустого/несуществующего пути
  (`LocatorAPI::setup_fs_path` зовёт его с пустой строкой, когда в имени нет разделителя) → SIGSEGV на старте
  (`EXC_BAD_ACCESS` в `_platform_strcpy`, подтверждено отчётом о краше). Починено по образцу Windows:
  результат `realpath` используется, когда он есть, иначе путь строится от текущего каталога (как `_fullpath`).
- **Баг 2 (общий код, правка платформенно-нейтральна):** `CLocatorAPI::file_find_it` нормализует **ключ
  поиска** через `Platform::ValidPath`, а на macOS/Linux она переводит `\` → `/`; записи же в `m_files`
  хранят разделители из конфигов (`\`). Имя, зарегистрированное с `\`, поэтому не находилось, и `Register`
  падал на `R_ASSERT(I_.second)` — запись признавалась дубликатом самой себя. Правка: если нормализованный
  ключ не найден, искать исходный. **На Windows `ValidPath` — тождественная функция**, поведение там то же.
- Итог: FS инициализируется (~0.6 с), движок доходит до чтения `gamedata/configs/system.ltx`.
- **Текущий упор (следующая сессия):** `InitSettings()` (`src/xrEngine/x_ray.cpp:136`) →
  `CHECK_OR_EXIT(0 != pSettings->section_count())` → «Cannot find file …/system.ltx» + выход с кодом 1
  (место выхода поймано lldb: breakpoint на `exit`, кадр `xrDebug::do_exit` ← `InitSettings`). Файл в логе
  помечен как найденный и загружаемый, но секций 0 → смотреть `check_for_file` / `file_from_cache`
  в `r_open_impl` (вероятно, `FS.r_open` не находит запись и возвращает пустой читатель).

**Рантайм, продолжение: движок доходит до загрузки рендерера**
- **Ассеты.** У `~/Hermes/OpenXRay/game` **неполный** набор: в `gamedata/configs/` только `models`,
  `scripts`, `text`, `ui` и `openxray.ltx` — **нет `system.ltx`** (в CoP он лежит в архиве
  `resources/configs.db`). Полный набор — `~/Hermes/ixray-mac-port/cop-baseline` (4.8 ГБ, с архивами
  `resources/*.db*`, `patches/xpatch_02.db`, `mp/mp_limansk.db`). Площадка запуска
  (`~/Hermes/ixray-mac-port/run-macos/`) теперь ссылается на него; правило «ассеты не трогать» соблюдено:
  только симлинки + своя копия `_appdata_`.
- **Рантайм-обвязка:** `bin/Debug/xrEngine` + dylibs, `libsteam_api.dylib` (из Steamworks SDK, ветка `osx`),
  **MoltenVK 1.4.2** (официальный релиз) + его `MoltenVK_icd.json`, запуск с `VK_ICD_FILENAMES`.
- **Грабли тестового запуска (важно):** в DEBUG-сборке фатальная ошибка показывает **модальное окно SDL** и
  ждёт клика — прогон висит и блокирует Степана. Решение: запускать прогоны с `SDL_VIDEODRIVER=dummy`
  (окно не создаётся, процесс не блокируется). Флага «тихого» режима у движка нет: `SilentErrorMode`
  ставится только в dedicated-режиме, `ignore_error_window` в этой ветке не выставляется нигде.
- **Закрытые баги этого шага:**
  1. `FS_Path::_set`/`_set_root` (`LocatorAPI_defs.cpp`) собирали `m_Path` **без** `ValidPath`, а
     `FS_Path::_update` (поиск) — с ним. Сканирование регистрировало «смешанные» имена
     (`…/run-macos/gamedata\configs/…`), которые не находил ни один поиск. Теперь путь нормализуется при
     сборке, как в `_update`. (Правка платформенно-нейтральна: на Windows `ValidPath` — тождественна.)
  2. `LoadArchive` (`LocatorAPI.cpp`) регистрировал имена файлов **из архивов** с виндовыми `\` (они так и
     записаны внутри архива) → `system.ltx` из `resources/configs.db` не находился. Нормализация добавлена
     при регистрации.
  3. `CEngineAPI::CreateRendererList` (`EngineAPI.cpp`) искал `libxrRender_R4.so` (linux-имя, зашито в общей
     ветке) → список рендереров пустой, `renderer renderer_r4` падал, движок уходил в R1/R2 и умирал на
     «Render factory is empty!». Добавлена ветка `IXR_MACOS` с именами `.dylib`.
  4. `cmake/modules/VKD3D.cmake`: ребейзится не только `-id`, но и **зависимости** между тремя библиотеками
     (`-change /usr/local/lib/libvkd3d-shader.1.dylib → @rpath/…`); иначе `dlopen` рендерера падал на
     отсутствующем `/usr/local/lib/libvkd3d-shader.1.dylib`. Ребейз и копирование рядом с бинарником
     вынесены из `if(NOT vkd3d_POPULATED)` — выполняются на каждой конфигурации (идемпотентно).
- **Состояние:** FS поднимается, `system.ltx`/`game.ltx` читаются, лог движка пишется, **`xrRender_R4`
  загружается успешно** (в логе `Loading DLL: xrRender_R4` без отката на R2/R1).
- **Следующий упор:** `xrGame` — `EngineAPI.cpp:136` «Game DLL raised exception during loading or there is no game DLL
  at all»: модуль игры ещё не собран (в `bin/Debug` нет `libxrGame.dylib`); идёт сборка цели `xrGame`.

**`xrGame` собран, и вскрылся АРХИТЕКТУРНЫЙ БЛОКЕР маршрута DXVK → MoltenVK**
- `libxrGame.dylib` собран и слинкован (108 МБ, ошибок 0). Движок с ним проходит всё до создания D3D11-устройства.
- **Блокер (проверено опросом устройства, `/tmp/vkfeat/vkfeat.c`):** MoltenVK 1.4.2 на Apple M4 отдаёт
  `geometryShader = 0` (у Metal нет стадии геометрических шейдеров) и `transformFeedback = 0`
  (расширения `VK_EXT_transform_feedback` нет вообще). DXVK 2.6 в `D3D11Device::GetDeviceFeatures()`
  помечает **обе как обязательные** для уровня 10_1, `DxvkAdapter::checkFeatureSupport()` их не находит → 
  `D3D11InternalCreateDevice: Maximum supported feature level: 0` → `Minimum required feature level
  D3D_FEATURE_LEVEL_10_1 not supported` → `E_INVALIDARG`. Рендерер после этого падает на
  `xrRHI/D3D11/DX11Buffer.cpp:91` (`pDevice`).
- Что при этом **работает** (из лога): DXVK 2.6 стартует, находит Vulkan-загрузчик, MoltenVK создаёт VkInstance
  (Vulkan 1.3.357), физическое устройство Apple M4 (16 ГБ, типы памяти), 154 расширения; всё остальное из
  обязательного набора 10_1 на месте (`tessellationShader`, `depthClamp`, `imageCubeArray`,
  `textureCompressionBC`, `multiViewport`, `fillModeNonSolid`, `samplerAnisotropy`, `independentBlend` и т.д.).
  Не хватает ровно: `geometryShader`, `transformFeedback` (а `logicOp` = 0 нужен только для 11_1 — у DXVK
  он берётся как необязательный).
- **Масштаб проблемы в самой игре:** геометрических шейдеров в R4 всего **4 из ~206**
  (`fluid_array*.gs.hlsl` — объёмный эффект 3DFluid, и `test.gs.hlsl`), против 63 `vs`, 135 `ps`, 4 `cs`.
  То есть GS — периферия, основная часть рендера его не требует.
- **Варианты (нужно решение Степана):**
  (а) патч форка DXVK: для macOS снять обязательность `geometryShader`/`transformFeedback` (правка ~10 строк,
      под флагом сборки) → устройство создастся, игра пойдёт; эффекты на GS (объёмный дым/fluid) будут
      отключены или сломаны;
  (б) проверить альтернативный Vulkan-драйвер под Metal (Mesa KosmicKrisp) — вероятно, та же нехватка;
  (в) сменить цель на R2/D3D9 через DXVK (там требования к фичам ниже) — но это другой рендер, не то, что нужно.
