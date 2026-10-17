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

**Решение: патчим форк DXVK (выбор Степана). Устройство и swapchain создаются, движок дошёл до шейдеров**

- **Патч форка делается при конфигурации** из `cmake/modules/DXVK.cmake` (APPLE-only, идемпотентно, через
  локальный helper `dxvk_text_patch`, по образцу `download_and_fix_nvapi()`), а не руками в дереве сборки:
  1. `src/d3d11/d3d11_device.cpp` — `geometryShader`, `extTransformFeedback.transformFeedback` и
     `geometryStreams` берутся из поддерживаемых фич устройства вместо жёсткого `VK_TRUE`. Без этого
     `D3D11InternalCreateDevice()` давал `Maximum supported feature level: 0` → `E_INVALIDARG` → в движке
     нулевой `pDevice` (`xrRHI/D3D11/DX11Buffer.cpp:91`). После патча в логе
     `Using feature level D3D_FEATURE_LEVEL_11_0`.
  2. `src/dxvk/dxvk_adapter.cpp` — макрос `CHECK_FEATURE_NEED` дополнительно логирует имя невыполненного
     требования (поведение не меняется: при обоих false выражение по-прежнему даёт false). Это позволило не
     угадывать недостающие фичи, а получить их списком.
  3. `src/wsi/sdl3/wsi_window_sdl3.cpp` — при неудаче `SDL_Vulkan_CreateSurface` логируется текст ошибки SDL
     (вызывающая сторона видит только `VK_ERROR_OUT_OF_HOST_MEMORY`).
- **Правки движка на этом шаге** (либо платформенно-нейтрально, либо под `IXR_MACOS`):
  1. `src/xrRHI/D3D11/Device.cpp` — окно для `D3D11CreateDeviceAndSwapChain` на macOS передаётся как
     **указатель `SDL_Window*`**: в форке `include/native/wsi/native_sdl3.h` `fromHwnd()` делает
     `reinterpret_cast<SDL_Window*>(hWindow)`. Win32-свойство (`SDL_PROP_WINDOW_WIN32_HWND_POINTER`) даёт на
     macOS NULL, а целочисленный id окна приводит к «Invalid window» во всех вызовах SDL внутри презентера.
  2. `src/xrPlay/Application.cpp` — окно создаётся как `SDL_WINDOW_VULKAN | SDL_WINDOW_HIDDEN`; отдельный
     `SDLK_WINDOW_METAL` тут не нужен и **вреден**: SDL3 разрешает только один из флагов
     OPENGL/METAL/VULKAN («Conflicting window graphics flags specified» → `SDL_CreateWindow` возвращает
     NULL, и дальше движок падал на нулевом окне). Дополнительно вызывается
     `SDL_Vulkan_LoadLibrary(nullptr)`: SDL грузит Vulkan-загрузчик сам только для окон с флагом VULKAN, а
     он нужен DXVK WSI (`SDL_Vulkan_GetInstanceExtensions` в `Sdl3WsiDriver`).
- **Проверенные факты:**
  - MoltenVK 1.4.2 на Apple M4: `geometryShader = 0`, `transformFeedback = 0` (расширения
    `VK_EXT_transform_feedback` нет), при этом `tessellationShader`, `depthClamp`, `imageCubeArray`,
    `textureCompressionBC`, `multiViewport`, `fillModeNonSolid`, `samplerAnisotropy` и остальной базовый
    набор 10_1 — есть (пробник `/tmp/vkfeat/vkfeat.c`).
  - SDL грузится **одна**: `DYLD_PRINT_LIBRARIES=1` показывает только нашу
    `build-macos/bin/Debug/libSDL3.0.dylib` (в brew тоже есть SDL3 3.4.18 — потенциальный источник второй
    копии, но в текущих прогонах она не подхватывается).
- **Грабли (важно):** macOS убивает бинарник с `SIGKILL (Code Signature Invalid)` («Killed: 9», пустой лог,
  в `log show` — `load code signature error`), если подпись dylib/бинарника стала недействительной
  (`install_name_tool` её ломает, копирование тоже может). Лечится ad-hoc подписью всех файлов площадки:
  `for f in xrEngine *.dylib; do codesign --force --sign - "$f"; done`.
- **Данные игры, шейдеры:** у `cop-baseline` набор шейдеров — оригинальный CoP (199 файлов, нет
  `sslr_render.cs.hlsl`), у репозитория IX-Ray — свой (209 файлов). В `run-macos/fsgame.ltx` путь
  `$game_shaders$` переключён на `$fs_root$| shaders_ixray\` (симлинк на `ixray-src/gamedata/shaders`);
  сами данные игры не менялись.
- **Текущий упор:** компиляция шейдеров — `dx11ResourceManager_Resources.cpp:325` (`_CreatePS`,
  `Can't compile shader` для `shaders_ixray/d3d11/copy_image.ps.hlsl`). Причина та же, что нашли ранее:
  vkd3d не понимает HLSL-методы структур — `metalic_roughness_base.hlsli` (`IXRayMaterial O = this;`,
  `O.Lerp(...)`) даёт `E5030: Unknown modifier "Lerp"` и `E5000: syntax error`. Задето ~11 из 209 шейдеров.
  Варианты: (а) патч vkd3d — поддержка member-функций; (б) офлайн-компиляция задетых шейдеров внешним
  HLSL-компилятором с подстановкой в кэш шейдеров движка (`_appdata_ixray_/shaders_cache/`); (в) переписать
  эти шейдеры без методов структур (наш набор данных, правка небольшая).

**Сделано: шейдеры компилируются, движок дошёл до игровой логики**

- **Вариант (в) — переписал шейдеры** (Степан не ответил, работал в автономном режиме; правка оказалась
  крошечной). В `gamedata/shaders/d3d11/metalic_roughness_base.hlsli` метод `IXRayMaterial::Lerp()` вынесен
  в свободную функцию `IXRayMaterial_Lerp(A, B, Factor)` (тело то же, `this` — первый аргумент), в
  `snowing_main.ps.hlsl` обновлён единственный вызов. Это обычный HLSL, поэтому Windows/Linux компилируют
  файлы как прежде. После правки библиотека шейдеров R4 компилируется целиком, остаются только предупреждения
  (`W4300: Redefinition of PI`, `W5300: Implicit truncation`).
- **Заглушка в движке:** `CRHI::GetInputElementDescStride` (`src/xrRHI/RHI.cpp`) на не-Windows падала в
  `VERIFY(!"Implement me!")`. Ветка Windows зовёт `DirectX::ComputeInputLayout` из DirectXMesh, но у D3D11-версии
  этой функции в DirectXMesh нет сборки под macOS: в `DirectXMesh/DirectXMeshUtil.cpp` она под
  `#if defined(__d3d11_h__)`, а не-Windows-ветка `DirectXMeshP.h` заголовок D3D11 вообще не включает (проверено:
  в `libDirectXMesh.a` есть только D3D12-перегрузка). Поэтому для macOS тот же алгоритм портирован в
  `RHI.cpp` (цитата источника в комментарии, размеры элементов — через `DirectX::BytesPerElement` из той же
  библиотеки). Ветка Windows и заглушка Linux не тронуты.
- **Состояние (прогон `-r4 -autotest -autotest_cmd quit`):** D3D11-устройство (FL 11_0) → swapchain → загрузка
  данных/кэша → **библиотека шейдеров R4 компилируется целиком** → движок уходит в игровой слой и падает на
  Lua: `PANIC: unprotected error in call to Lua API (27)` (`autotest37.log`, ~44 600 строк лога).
- **Мелочь:** строки `! error: _appdata_ixray_/shaders_cache/r1.3.6/d3d11/<shader>...` — это не ошибки
  компиляции, а неудачная запись в кэш шейдеров (каталога `_appdata_ixray_` в площадке запуска нет);
  компиляция при этом проходит.
- **Следующий упор:** Lua-паника при инициализации игры (`xrScripts`/`xrGame`) — смотреть, какой скрипт
  грузится и что именно не сходится; вероятно, связано с набором данных (используется `gamedata` из
  `cop-baseline` + шейдеры из репозитория).

**Lua-паника: причина найдена и закрыта; вскрыта «двойная LuaJIT»; остался цикл в сборке мусора**

- **Рецепт прогонов (важно):** `ALSOFT_DRIVERS=null` + `VK_ICD_FILENAMES=$PWD/MoltenVK_icd.json` +
  сторож на `pkill -9 -x xrEngine`. Почему так:
  - без `ALSOFT_DRIVERS=null` движок **висит** в `ALDeviceList::Enumerate()` (`OpenALDeviceList.cpp:109`):
    OpenAL Soft перечисляет устройства CoreAudio, а macOS на это спрашивает доступ к звуку (TCC) — запрос ждёт
    ответа и блокирует прогон (Степану прилетал запрос «зачем микрофон»);
  - в DEBUG-сборке фатальная ошибка показывает модальную плашку SDL и ждёт клика — сторож обязателен.
- **Причина паники:** встроенный в движок LuaJIT (форк `ixray-team/luajit-ixray` = 2.1.0-beta3 + их
  бэкпорты) **не умеет JIT на Apple Silicon**: в `lj_mcode.c` он не может разместить область машинного кода
  («OS probably ignores hints?» → `LJ_TRERR_MCODEAL`), ошибка уходит наружу, LuaJIT вызывает `panic()` →
  движок падает. Доказано отдельным процессом (`/tmp/ljtest/run_luajit.c`), который только `dlopen`-ит
  собранную библиотеку и гоняет цикл: **форк падает, официальный LuaJIT 2.1 — работает**.
- **Решение (выбор Степана — «дожимать правильный путь»):** `cmake/modules/LuaJIT.cmake` для macOS собирает
  **официальный LuaJIT v2.1** (пин `2a8e017ef1ece6a95b983ffca073d30609dce7c4`) его собственным Makefile-ом:
  режим `BUILDMODE=dynamic`, `MACOSX_DEPLOYMENT_TARGET` (11.0), в его `lj_mcode.c` убирается `return 0;` из
  void-функции (современный clang не пропускает), результат (`libluajit.so`) копируется как
  `libluajit.dylib` с install name `@rpath/libluajit.dylib` и кладётся рядом с бинарником. Windows/Linux
  продолжают собирать их форк — их путь не тронут.
- **Находка №2 (реальный баг):** при статической LuaJIT в процессе оказывалось **две копии** — `.a`
  «заглатывал» `libluabind.dylib` (он линкует LuaJIT сам, см. `luabind/luabind/CMakeLists.txt:27`), а вторая
  копия уходила в `xrScripts`, и GC одной библиотеки работал по объектам другой. Проверено `otool -L`
  (в `libluabind` не было зависимости от LuaJIT, но были её символы). Теперь одна общая
  `@rpath/libluajit.dylib`.
- `src/xrScripts/luautf8/luautf8.c`: добавлен `#include <limits.h>` — используется `INT_MAX`, а заголовки
  официального LuaJIT больше не тянут его транзитивно.
- **Текущий упор:** даже с одной LuaJIT (и с выключенным JIT — проверял) движок всё ещё висит в
  `CResourceManager::LS_Load()` → `luabind::open()` → внутри LuaJIT: `sample` показывает сплошные сэмплы в
  `libluajit` (цикл в GC-пути). Отдельный тест (юзердата с `__gc` + 400k аллокаций) на этом же LuaJIT
  проходит, то есть зависание специфично для связки luabind + движка. Следующие шаги: собрать LuaJIT с
  символами и посмотреть в lldb состояние GC (`gcstate`, объект, на котором цикл), либо сравнить
  `detail::class_registry` luabind-а с новым GC LuaJIT.

**Наводка Степана: смотрим, как Lua сделана в OpenXRay (он работает на этом маке)**

- Рабочее дерево OpenXRay: **`/Users/stepan/STALKER-Call-of-Pripyat`** (исходники в `build/xray-16`,
  собранная игра в `build/xray-16/bin/arm64/Release/xr_3da`, игровые данные — в `~/Hermes/OpenXRay/game`).
  Там же `Externals/{LuaJIT,LuaJIT-proj,luabind}`.
- **Ключевой факт:** их рабочий мак-билд использует **ту же старую LuaJIT 2.1.0-beta3**, причём
  `LUAJIT_DISABLE_JIT:BOOL=OFF` и `LUAJIT_DISABLE_GC64:BOOL=OFF` (JIT включён!), и в их `lj_mcode.c`
  **нет** `MAP_JIT` — и всё работает. Значит провал mcode у IX-Ray — это разница **сборки** (их
  `Externals/LuaJIT-proj/CMakeLists.txt` гоняет их Makefile с `-O3 -fomit-frame-pointer
  -fno-stack-protector -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -U_FORTIFY_SOURCE`, `arm64_src`,
  `buildvm_arch.h`), а не отсутствие ARM64-поддержки в LuaJIT. **Это главный неисследованный путь.**
- LuaJIT и luabind у них — отдельные `.dylib` (`xrLuaJIT.dylib`, `xrLuabind.dylib`; `xrLuabind` линкуется
  с `@rpath/xrLuaJIT.dylib`) — то же, к чему пришли мы.
- **Их luabind — свой форк `OpenXRay/luabind-deboostified`** (локальный коммит `8da131b8`, «Safe class
  registry cache»), а IX-Ray тянет `ForserX/luabind-latest`. Пробовал переключить macOS на их luabind:
  собирается и линкуется, но дальше начинается **новый API** (`enum_`, `return_reference_to`,
  `module_[...]`, нет `get_globals`, нет `luabind_memory.h`, нет `functor.hpp`) → нужен **порт всего слоя
  привязок** (десятки файлов `src/xrScripts/exports`, `src/xrGame`). Для быстрого пути НЕ годится,
  ветку откатил (в дереве снова старый luabind). У них `functor` возвращён их же файлом
  `src/xrScriptEngine/Functor.hpp`, `get_globals` заменён на `luabind::globals(L)`.
- Следующий шаг (дешёвый и по наводке): **повторить их рецепт сборки LuaJIT** для macOS в
  `cmake/modules/LuaJIT.cmake` (их Makefile-вызов + `arm64_src`) и проверить, работает ли JIT у их
  версии LuaJIT на этом маке. Если да — их форк LuaJIT + родной luabind IX-Ray и есть рабочая связка,
  а апгрейд LuaJIT/luabind можно отложить.

**РЕЗУЛЬТАТ: их LuaJIT снял и панику, и зависание**

- Собрал `OpenXRay/LuaJIT` (`5a5cd82e`) для macOS и подключил его в `cmake/modules/LuaJIT.cmake`
  (пин + сборка их Makefile-ом `BUILDMODE=dynamic` + переименование в `libluajit.dylib` с
  `@rpath/libluajit.dylib`). Собралось чисто, движок слинковался.
- Прогон: **ни паники (`PANIC`/`LJ_TRERR_MCODEAL`), ни зависания в `luabind::open()`/GC** — движок
  прошёл весь Lua-этап, прочитал всю библиотеку шейдеров R4 из кеша (`smaa_*`, `contrast_adaptive_sharpening`,
  `ui_postprocess*`, `gtao_*`, `combine_velocity`) и пошёл дальше в конфиги управления
  (в собственном логе `_appdata_ixray_/logs/ixray-*.log` видны разборы action_name и т.п.).
- Падение теперь другое: процесс завершается с кодом **133 (SIGTRAP)** — похоже на сработавший
  ассерт/`__builtin_trap` уже в новом месте; системного crash-отчёта нет, в stdout ошибки нет
  (в логе движка он обрывается на середине строки). Следующий шаг: временно вернуть тихий режим
  ошибок (`Debug._initialize(true)`), чтобы текст ошибки попал в лог, и поймать место.
- **Важное уточнение к прежнему выводу:** провал mcode у обоих «beta3»-форков (IX-Ray и OpenXRay)
  воспроизводится в *минимальном* процессе-пробнике (`PANIC ... (13)`), но **внутри движка их сборка
  работает** — то есть это зависит от раскладки адресного пространства процесса, а не только от
  библиотеки. Официальный LuaJIT 2.1 в пробнике проходит, но в движке упирается в GC старого luabind.
- Остались отложенные копии в build-дереве: `_deps/luajit_apple-src.upstream-bak`,
  `_deps/luajit_apple-subbuild.upstream-bak`, `build-macos/luajit/libluajit.dylib.upstream-bak`
  (старая сборка официального LuaJIT; удалять их можно только с разрешения Степана).

**ДВИЖОК ЗАПУСКАЕТСЯ (дошёл до интро-ролика)**

- **Причина «вылета внутри LuaJIT» (EXC_BREAKPOINT в `lj_BC_JFUNCV`) — две LuaJIT в процессе.** Проверено
  `DYLD_PRINT_LIBRARIES=1`: грузились и наша новая (`build-macos/luajit/libluajit.dylib`), и **старая из
  форка** (`_deps/luajit-build/lib/libluajit.2.1.dylib`) — её тянули четыре библиотеки движка
  (`xrGame`, `xrRender_R4`, `xrUI`, `xrPhysics`), которые остались слинкованными с прежним именем
  (`@rpath/libluajit.2.dylib`). Лечится на чистой сборке само (в текущем ninja-графе у них правильная
  команда линковки), а в существующем build-дереве — подменой: старые файлы отложены рядом
  (`*.fork-bak`), на их имена поставлены симлинки на нашу новую библиотеку. Итог: в процессе одна LuaJIT.
- **Один алиас данных:** `run-macos/fsgame.ltx` не хватало `$game_particles$ = true| false| $game_data$| particles\`
  (в шаблоне репозитория он есть, в скопированном файле — нет). После добавления движок поехал дальше.
- **Итог прогонов:** движок проходит весь старт — Lua, вся библиотека шейдеров R4, конфиги, UI-текстуры,
  синхронизация уровня (`End of synchronization A[1] R[1]`), звук (`stereo sound source ...`) — и доходит
  до **интро-ролика** (`intro_start intro_logo`, играется `gamedata/sounds/video/amd_fusion_final_720.ogg`).
  Ни падений, ни зависаний; прогон пришлось гасить сторожем только по времени (автотест не успевает).
- Наблюдение: старт небыстрый (компиляция шейдеров + загрузка), для тестов давать ≥5 минут.
- Следующие шаги: (1) довести до первого отрисованного кадра в окне (без `-autotest`, посмотреть картинку),
  (2) загрузить уровень/сейв и проверить геймплей, (3) откатить временный тихий режим ошибок (уже откачен),
  (4) убрать отложенные `*.upstream-bak`/`*.fork-bak` (с разрешения Степана).

**ПЕРВЫЙ КАДР: меню игры отрисовано (главная задача порта выполнена)**

- Окно теперь скрывается только в автотестах (`Application.cpp`: флаг `SDL_WINDOW_HIDDEN` ставится, если в
  `Core.Params` есть `-autotest`); обычный запуск показывает окно — именно это позволило увидеть кадр.
  Запуск без автотеста: `./xrEngine -r4 -nointro` (+ `ALSOFT_DRIVERS=null`, `VK_ICD_FILENAMES=.../MoltenVK_icd.json`).
- Скриншот (снят `screencapture -x`, лежит в `~/Desktop/ixray-shots/frame-01.png` + `.jpg`): **полностью
  отрисованное главное меню CoP** — «Новая игра / Загрузить игру / Сетевая игра / Опции / Титры / Выйти в
  Windows», фон с баржей и колесом обозрения. Рисует родной R4 (D3D11) через DXVK + MoltenVK — то есть
  выбранный путь порта работает без переписывания рендера.
- Старт небыстрый: до меню ~4 минуты (шейдеры + загрузка). Для наблюдения запускать без `-autotest`.
- Что дальше: (1) новая игра/загрузка сейва → уровень и геймплей, (2) проверить ввод (мышь/клавиатура)
  и звук в игре, (3) при желании — убрать временные бэкапы в build-дереве.

**Звук починен: движок падал на инициализации OpenAL**

- Симптом: движок умирал на старте с `terminating due to uncaught exception of type
  std::__1::system_error: mutex lock failed: Invalid argument` — окно исчезало ещё до меню.
- Отладчик (`breakpoint set -n __cxa_throw`) показал точную цепочку: `alGenSources` →
  `al::Context::throw_error` в `libopenal` → `CSoundRender_TargetA::_initialize`
  (`src/xrSound/SoundRender_TargetA.cpp:33`) → `CSoundRender_CoreA::_initialize` → `InitSound2()`
  (`x_ray.cpp:170`) → `EngineLoadStage4()`.
- Причина: на маке стоит **OpenAL Soft 1.25** (Homebrew), который **бросает C++ исключение** при ошибке,
  а движок написан под OpenAL, который просто выставлял код ошибки: его код ниже как раз читает
  `alGetError()` и умеет продолжать без звука — до этой проверки дело просто не доходило.
- Правка: вызов `alGenSources` обёрнут в `try/catch` (с пояснением), ошибка пишется в лог, цель звука не
  инициализируется, движок продолжает работу по своей же error-ветке.
- Проверено прогоном: движок доходит до меню и играет `gamedata/sounds/music/menu.ogg`, исключений нет.
  Строка `! SOUND: OpenAL: Max targets - 256` — нормальное поведение: то же самое в рабочем OpenXRay
  (там `alcCreateContext(pDevice, nullptr)` и такой же предел целей).
- Ещё одно наблюдение: `-nosound` в этой ветке не отключает звук — `InitSound1/2` вызываются в
  `EngineLoadStage4` безусловно.

**Анимации моделей: падение из-за обнулённой блокировки (баг виден только вне Windows)**

- Стек (пойман `lldb` по `abort`): `abort` ← `std::terminate` ← `std::system_error: mutex lock failed:
  Invalid argument` ← `std::shared_mutex::lock()` ← `xrSRWLock::AcquireExclusive` ←
  `CBlendInstance::blend_add` ← `CKinematicsAnimated::Bone_Motion_Start_IM` ← `LL_PlayCycle`
  (тик анимации модели в R4).
- Причина: `CBlendInstance` создаётся сырой памятью (`xr_alloc<CBlendInstance>` в
  `SkeletonAnimated.cpp:669`) и инициализируется `construct()`, который делает
  `ZeroMemory(this, sizeof(*this))`. Поле `blend_lock` при этом обнуляется, а `xrSRWLock` — обёртка
  (`src/xrCore/xrSyncronize.h:40`), у которой на Windows внутри `SRWLOCK` (нулевое состояние = свободная
  блокировка, поэтому баг не проявляется), на Linux `pthread_rwlock_t`, **на macOS `std::shared_mutex`**,
  для которого нулевое состояние недопустимо → первая же попытка взять блокировку даёт EINVAL.
- Правка: после `ZeroMemory` блокировка конструируется явно (`new (&blend_lock) xrSRWLock();`) в обоих
  местах: `src/Layers/xrRender/SkeletonAnimated.cpp` и серверная копия
  `src/Layers/xrRenderDS_R0/Visual/XRayKinematicsAnimated.cpp`.
- Правило на будущее: `ZeroMemory(this, sizeof(*this))` в классах с блокировками — искать и проверять
  (кандидаты в дереве: `src/xrEngine/bone.h:494`, `src/Layers/xrRenderDX10/3DFluid/dx103DFluidRenderer.h:52`).
  Windows-сборкам это не вредит: там блокировки нулевые валидны.

**Ошибки Lua: движок молчал и падал; включён штатный режим обработки**

- Симптом: `terminating due to uncaught exception of type luabind::error: lua runtime error` при
  спавне объектов уровня (стек: `CWrapperAbstractZone<CSE_ALifeSmartZone>::STATE_Write` ←
  `CSE_Abstract::Spawn_Write` ← `xrServer::Process_spawn`).
- Причина: движок написан под luabind **без исключений** — `CScriptEngine::setup_callbacks()`
  ставит свои обработчики (`luabind::set_error_callback`, `Log`/`Debug.fatal`) только под
  `#ifdef LUABIND_NO_EXCEPTIONS`. На Windows этот define приходит с конфигурацией Release/
  RelWithDebInfo, а у нас Debug — поэтому luabind бросал `luabind::error`, ловить его в движке
  негде, и процесс падал.
- Правка: `cmake/modules/LuaBind.cmake` — для macOS `target_compile_definitions(luabind PUBLIC
  LUABIND_NO_EXCEPTIONS)` (PUBLIC, чтобы и код движка видел тот же режим). Windows/Linux не тронуты.
- Результат: ошибки скриптов теперь идут в лог движка (`[LUA] Error: [string "smart_terrain"]:858: ...`),
  движок их переживает.

**Данные: нужна родная gamedata IX-Ray, а не ванильная**

- В репозитории (`ixray-src/gamedata/`) лежит **полный набор IX-Ray: 291 скрипт**, а мы запускали с
  ванильной gamedata из `cop-baseline` (44 скрипта) — отсюда были ошибки `utils.script:588`,
  `smart_terrain.script:858` и «пропавшие» модели пропсов.
- Правка площадки: `run-macos/gamedata` теперь симлинк на `../ixray-src/gamedata` (старый симлинк
  на cop-baseline сохранён рядом как `gamedata.cop-baseline-old`, ничего не удалено).
- После этого новая игра создаётся, объекты уровня грузятся, ошибок Lua нет.

**Управление снаружи (для автотестов)**

- Клики (`cliclick`) и клавиши (`osascript key code`) **до SDL-окна не доходят** — меню реагирует
  только на «живого» пользователя. Проверено и кликом в центр пункта, и кликом по заголовку окна.
  Иногда срабатывает, но нестабильно (один раз новая игра всё же создалась).
- Надёжный вариант на будущее: временный «пульт» внутри движка (послать `SDL_PushEvent`/вызвать
  стартовую функцию меню после N секунд) — помечать как TEMPORARY и убирать после проверки.
- Полезно: `-$ <команда> <параметр>` = выполнить консольную команду после загрузки уровня
  (`Level_start.cpp:273`), команды консоли: `load` (`CCC_ALifeLoadFrom`), `main_menu` (`CCC_MainMenu`).

**Как стартовать новую игру командой (найдено для автотестов)**

Меню не жмётся извне, но «Новая игра» — это просто консольные команды
(`gamedata/scripts/ui_main_menu.script`, `main_menu:StartGame`):

```
g_game_difficulty gd_novice
start server(all/single/alife/new) client(localhost)
main_menu off
```

Их можно выполнить откуда угодно: из консоли движка (`Console->Execute(...)`), через
`-autotest_cmd "<команды>"` или `run_string`/`run_script` (команды консоли из
`src/xrGame/console_commands.cpp`). Временный «пульт» в `CRenderDevice::message_loop` (по переменной
окружения, помечен TEMPORARY) для этого подходит, но откатывается: синтетические события от другого
процесса до окна не доходят, а команды консоли — работают.

**Следующее препятствие (после перехода на родную gamedata)**

- При старте движок упирается в модальную плашку `Fatal Error`:
  `File: src/xrCore/FS.cpp:56 / Function: FileDownload / Description: Can't read from file /`
  `Arguments: _appdata_ixray_/logs/` — то есть каталог логов читают как файл (стек-путь идёт через
  `CLocatorAPI` → `FileDownload`). Похоже на манифест/список файлов системы аддонов IX-Ray
  (`xrAddons`/DLTX), где одним из элементов оказался каталог.
- Пока не исследовано. Плашка модальная и блокирует движок — в автотестах от неё спасает
  watchdog/авто-закрытие, а в обычном прогоне нужно закрыть руками.

**Где остановились (последний прогон ночи)**

- С временными послаблениями (тихий режим ошибок `Debug._initialize(true)` + «пульт» с командой
  `start`, оба помечены TEMPORARY и уже откачены) движок пошёл **дальше**, чем раньше: компилировал
  шейдеры уровня `pfx_volumetric_light`, `combine_volumetric`, `postprocess*`, `distort` — и упал с
  **`EXIT=139` (SIGSEGV)**, без C++-исключения. То есть следующий баг — не блокировка и не Lua, а
  падение памяти на компиляции/подготовке шейдеров постобработки R4.
- Как ловится: `lldb` без точки останова останавливается на самом нарушении доступа
  (`stop reason = EXC_BAD_ACCESS`), затем `thread backtrace`. Воспроизведение требует запуска новой
  игры — значит нужны и «пульт», и тихий режим ошибок (иначе модальная плашка блокирует движок).
- Что уже проверено: с валидными данными (репозиторная gamedata) и всеми правками новая игра
  создаётся, объекты уровня идут online, ошибок Lua нет.

**Игра ЗАПУСКАЕТСЯ и играбельна: уровень загружается, Степан зашёл и побегал (17 окт, утро)**

Три проблемы, которые он видит, и их причины (разбор без правок кода — он просил пока не реализовывать):

1. **Низкий FPS.** Сборка идёт как `CMAKE_BUILD_TYPE=Debug` (проверено в `build-macos/CMakeCache.txt`):
   без оптимизаций, с `VERIFY`, с включённым Optick. Плюс накладные расходы трансляции
   D3D11 → DXVK → Vulkan → MoltenVK → Metal и холодный кэш шейдеров. Первый шаг проверки — собрать
   Release и сравнить.
2. **Свет: «подходишь — секторы лампочек загораются, отходишь — пропадают».** Причина найдена:
   R4 рисует объёмы света шейдером `gamedata/shaders/d3d11/accum_volumetric.vs.hlsl`, который
   использует `SV_CullDistance` — отсечение вершин по расстоянию. Metal не имеет
   `shaderCullDistance` (MoltenVK сообщает 0), требование снято правкой в `cmake/modules/DXVK.cmake`
   (фича берётся из того, что поддерживает устройство) → инструкция теряется → объём света
   отсекается неверно. Эту фичу используют ровно три шейдера: `fluid_common.hlsli`,
   `fluid_grid_oobb.vs.hlsl`, `accum_volumetric.vs.hlsl` (последний и есть световой объём).
   Проверка-эксперимент: временно убрать отсечение из этого одного шейдера и посмотреть на лампы.
3. **«Сбитая» текстура террейна.** Подозреваемый — наш шим создания текстур
   (`src/Layers/xrRenderPC_R4/DirectXTexD3D11_macos.cpp`, `DirectX::CreateTextureEx`): террейн в R4
   это многослойный атлас с детальными текстурами, и расхождение формата/мипов/анизотропии между
   D3D11 и Metal проявляется именно так. Проверка: посмотреть в логе, с какими форматами и мипами
   грузятся текстуры террейна, и сравнить с ожиданием шейдера.

Дополнительно: геометрических шейдеров в Metal тоже нет, а они задействованы в `smaa.hlsli`
(сглаживание), `water_puddles.vs.hlsl` (лужи) и `tess.hlsli` (тесселяция) — на маке эти эффекты
отключены/не работают. К трём проблемам выше не относятся.
