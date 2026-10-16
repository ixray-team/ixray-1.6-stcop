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
