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
| Сборка ядра под macOS | 🟢 конфигурация ✅, сборка идёт (311/411) | Phase 1 |
| Metal-бэкенд | ⬜ не начато | Phase 2 |
| Игра на Mac | ⬜ не начато | Phase 3 |

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

## Правки, уже сделанные для сборки под macOS (ветка `macos-metal`)

| # | Файл | Что сделано | Статус |
|---|---|---|---|
| 1 | `cmake/clang.cmake` | `_GNU_SOURCE` и `-fuse-ld=lld` + Linux-пути загорожены `if(NOT APPLE)` | ✅ |
| 2 | `src/xrCore/Platform/Platform.h` | на Apple при `__aarch64__` определяется `IXR_ARM64` (раньше всегда `IXR_X64`) | ✅ |
| 3 | `src/xrCore/Platform/MacOS/PlatformInit.h` | убраны `<linux/limits.h>` и glibc-шный `<bits/local_lim.h>` → `<limits.h>`; для Apple в ARM-ветке `<arm_neon.h>` | ✅ |
| 4 | `cmake/modules/sse2neon.cmake` (новый) + `cmake/modules.cmake` | добавлено скачивание `sse2neon/sse2neon.h` тем же механизмом `download_single_header`, что у `stb`/`nvapi` | ✅ файл скачался (502 КБ) |

Конфигурация: см. выше. Сборка xrCore: **конфигурация ✓**, компиляция упирается в следующие ошибки.

## Открытые ошибки сборки xrCore (шаг 1.3)

| # | Ошибка | Диагноз | Статус |
|---|---|---|---|
| A | `_types.h:2 'cstdint' file not found` при сборке **C**-файла `memory/doug_lea_memory_allocator.c` | `_types.h` — чисто C++-заголовок (`using`, `std::numeric_limits`), а его тянет C-трансляция (вероятно, через форсированный PCH) | ⬜ сделать `_types.h` двуязычным (`__cplusplus`-ветки: `<stdint.h>`/`<limits.h>` + `typedef` вместо `using`) |
| B | каскад в заголовках Apple SDK: `__darwin_wctype_t`, `__darwin_wctrans_t`, `__DARWIN_WEOF`, `char_type` … | похоже, что-то в цепочке платформенных заголовков ломает `<wchar.h>`/`<sys/_types.h>` (тип `__darwin_*` не определяется) | ⬜ диагностика: скомпилировать минимальный .cpp с `MacOS/PlatformInit.h` + `<wchar.h>` и найти виновника |
| C | `_MAX_PATH` undeclared в `_types.h:57` | определение есть в `Platform/MacOS/OSFile.h`, но к моменту `_types.h` платформенные заголовки не подключены/сломаны | ⬜ перепроверить после A и B (вероятно, производное) |

## Замечено попутно (пригодится)

- В `CMakePresets.json` у IX-Ray **уже есть пресет `Engine-x64-Wine`** — то есть авторы сами ходят через Wine; это подтверждает, что macOS-путь они держат в уме.
- `Platform/MacOS/PlatformInit.h` — **практически копия** `Platform/Linux/PlatformInit.h` (отличие: нет `using HDC = void*;`). Папку завели, но под не-Apple её, похоже, никогда не компилировали.
- `src/xrCore/Platform/Platform.h` разводит платформы по `WIN32 / __linux__ / __FreeBSD__ / __APPLE__` — диспетчеризация для Apple есть ✓.

## Журнал (append-only)

- **2026-10-16, сессия 1** — Phase 0 начат и в основном закрыт:
  - создана ветка `macos-metal`, заведён `.porting/`;
  - Task 0.2: замер утечки D3D11 (`xrRenderDX10` 231/7 файлов, `xrRenderPC_R4` 128/21, R1/R2 чисто) + измерен контракт RHI (**≈153 виртуальных метода**);
  - Task 0.3: инвентарь шейдеров — **2172 HLSL всего, 209 под R4** (`gamedata/shaders/d3d11`), компиляция **в рантайме** через `D3DCompile` (`src/xrRHI/RHIShaderCompiler.cpp:24`);
  - оформлен `discovery-report.md`;
  - Task 0.4 (baseline): скачана и распакована сборка r1.3.6 в `cop-baseline/`, создана бутылка CrossOver `ixray`, снят баг с `concrt140` (см. грабли). **Открыто:** окно «IX-Ray Engine» открывается, но экран чёрный (CPU 0%). Отложено — не блокирует Phase 1.
  - **Следующий шаг: Phase 1, Task 1.1** (пресет сборки macOS + `brew install ninja`).
