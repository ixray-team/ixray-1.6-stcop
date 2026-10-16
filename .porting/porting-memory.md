# Porting memory — IX-Ray → macOS Metal

> Файл «памяти порта» по методологии Apple (`porting-methodology`): то, что должно быть в голове в начале каждой сессии.
> Обновлять в конце каждой сессии/вехи.

## Context

- **Цель проекта:** нативная macOS/arm64 версия IX-Ray 1.6, рендерер **R4** через новый **Metal-бэкенд** в `src/xrRHI`.
- **Репозиторий:** `~/Hermes/ixray-mac-port/ixray-src`, ветка **`macos-metal`** (от `default` = `478899e68`, 13.10.2026).
- **План:** `~/Hermes/ixray-mac-port/.hermes/plans/2026-10-16_124500-ixray-macos-metal-port.md`.
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
| Metal-бэкенд | ⬜ не начато | Phase 2 |
| Игра на Mac | ⬜ не начато | Phase 3 |

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
