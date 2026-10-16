# Codebase Discovery Report: IX-Ray 1.6 (ixray-1.6-stcop)

> Артефакт методологии Apple GPTK (`porting-discover`). Проект: `~/Hermes/ixray-mac-port/ixray-src`, ветка `macos-metal`, база `478899e68` (13.10.2026).
> Статус: **Phase 0 частично выполнен** (0.1–0.3 сделаны, 0.4 — в работе).

## Summary

Движок **хорошо подготовлен** к порту на macOS, лучше, чем типичный Windows-only проект:

1. **Есть готовый шов абстракции** — слой `src/xrRHI` с бэкендами `D3D9/` и `D3D11/` (~153 виртуальных метода в 9 интерфейсах). Рендереры R1/R2/R4 сидят над ним. Linux уже ходит через этот шов (DXVK Native) — значит архитектура проверена на не-Windows платформе.
2. **Платформенные предпосылки macOS уже заложены**: макрос `IXR_APPLE_SERIES`, `cmake/clang.cmake` («for Clang or Apple»), ветка `elseif(APPLE)` в `cmake/modules/DXVK.cmake`, macOS в `SteamWorks.cmake`. Но в `src/xrRHI/CMakeLists.txt` стоит жёсткое `if(UNIX AND NOT APPLE)` — Apple явно исключён, бэкенд для него не заведён.
3. **Главная работа — не рендерер, а шейдеры**: движок компилирует HLSL **в рантайме** через `D3DCompile` с макро-комбинациями. На macOS этого API нет → нужен офлайн-конвейер DXIL → metallib с предварительной печью комбинаций.

## Skills Loaded

| Скилл | Что дал для анализа |
|---|---|
| `porting-methodology` | система портирующих целей, жизненный цикл вех, правила артефактов (`.porting/`) |
| `porting-discover` | структура этого отчёта, чек-листы платформенной готовности |

## Platform Readiness

| Аспект | Состояние | Комментарий |
|---|---|---|
| Система сборки | ✅ CMake ≥ 3.26 + пресеты | `CMakeLists.txt`, `CMakePresets.json`, `IXRay.code-workspace` |
| Компилятор | ✅ `cmake/clang.cmake` существует | в корневом `CMakeLists.txt:38` уже есть ветка `Clang OR APPLE` |
| Язык | ⚠️ C++23 | MSVC-измы против clang не проверены (Task 1.3) |
| Окно/ввод | ✅ **SDL3** | `imgui_impl_sdl3.cpp`, окно в `src/xrPlay` |
| Звук | ✅ OpenAL (портабельный) | `src/xrSound` |
| Скрипты | ✅ LuaJIT + luabind | `src/xrScripts` |
| Физика | ✅ ODE (портабельный C) | `src/xrPhysics` |
| UI | ✅ ImGui + XML-виджеты | бэкенд Metal под вопросом (open question #2) |
| Платформенные макросы | ✅ `IXR_WINDOWS`, `IXR_LINUX`, **`IXR_APPLE_SERIES`**, `IXR_BSD_SERIES` | `docs/.../coding-guidelines.md:118` |
| Платформенный слой | ✅ требование проекта | «весь платформозависимый код — в `Platform`-реализациях (`Windows`, `Linux`, **`macOS`**)» |
| Графический API | ⚠️ **нет бэкенда для Apple** | `src/xrRHI/CMakeLists.txt:52`: `if(UNIX AND NOT APPLE)` → DXVK; `elseif(WIN32)` → D3D. Apple не попадает никуда |
| Автотест | ✅ есть | `xrEngine -autotest … -autotest_cmd <cmds>` (`AGENTS.md`) — годится как headless-регресс |

## Graphics Backend Analysis

### Контракт будущего Metal-бэкенда (объём работы, точные цифры)

| Интерфейс | Виртуальных методов |
|---|---|
| `RHITextureInterfaces.h` | **51** |
| `RHIDevice.h` | 23 |
| `RHIShaderResourceCache.h` | 20 |
| `RHIStateManager.h` | 19 |
| `RHIShaderCompiler.h` | 18 |
| `RHIBuffer.h` | 8 |
| `RHIDriversExt.h` | 8 |
| `RHIShaderDeclaration.h` | 5 |
| `RHIGPUMark.h` | 1 |
| **Итого** | **≈153 метода** |

Это и есть измеренный объём Metal-бэкенда (реализация этих интерфейсов, не переписывание рендерера).

### Утечка D3D11 выше RHI

| Слой | Упоминаний D3D11 | Файлов |
|---|---|---|
| `xrRender` (общий) | 14 | 7 |
| `xrRenderDX10` (общий DX10/DX11) | **231** | 7 |
| `xrRenderPC_R4` (наш целевой) | 128 | 21 |
| `xrRenderPC_R2` (DX9) | 0 | 0 |
| `xrRenderPC_R1` (DX9) | 0 | 0 |

**Важное наблюдение:** топ-файлы R4 по упоминаниям D3D11 — это почти целиком **обёртки апскейлеров**: `OverlayAPI/DLSSWrapper.cpp` (11), `OverlayAPI/FSR3Wrapper.h` (11), `OverlayAPI/XESSWrapper.h/.cpp` (+14), и рядом `CSCompiler.cpp` (18), `r4_rendertarget_phase_sslr.cpp` (11), `r4_rendertarget.cpp` (10). Поскольку DLSS/FSR/XeSS на macOS выключаем по решению проекта, **реальная поверхность порта меньше, чем 359 суммарных упоминаний**: значительная часть вычитается вместе с апскейлерами.

`xrRenderDX10` — главный кандидат на «уплотнение» через расширение интерфейса RHI (правило проекта: D3D-типы не тащить в рендерер).

### Шейдерный конвейер

| Факт | Значение |
|---|---|
| HLSL-файлов в репозитории всего | **2172** |
| Набор под R4 (цель) — `gamedata/shaders/d3d11` | **209 файлов** |
| Наборы r1 / r2 (вне области v1) | 135 + 135 на вариант игры |
| Вариантов игр | `gamedata`, `gamedata_soc`, `gamedata_cs`, `gamedata_coc` |
| Компиляция | **в рантайме**: `src/xrRHI/RHIShaderCompiler.cpp:24` → `D3DCompile(srcData, …, defines, include, entryPoint, target, …)` |
| Профили | передаются строкой (`target`), макро-комбинации — через `D3D_SHADER_MACRO*` |

**Следствие:** на macOS нет `d3dcompiler` → рантайм-компиляция невозможна. Нужен офлайн-конвейер: `HLSL → DXC → DXIL → metal-shaderconverter → metallib` + кеш + инструмент перечисления комбинаций (снять список с реального прогона Windows-сборки). Это **риск №1** плана.

### Выбор рендерера (точки правки)

`src/xrEngine/xr_ioc_cmd.cpp` (`rsR2`/`rsR4`) → `EngineAPI.cpp` (`GetAPI()`: D3D11 для `rsR4`, иначе D3D9) → `Device_create_render.cpp` (загрузка `xrRender_R*`). Список рендереров — `CreateRendererList`.

## Feature Coverage Matrix (предварительно)

| Фича R4 | Переносимость | Комментарий |
|---|---|---|
| Deferred G-buffer + освещение | ✅ через RHI | основная работа Phase 2.8 |
| Тени (sun + point) | ✅ через RHI | — |
| SSAO / GTAO | ✅ | — |
| SSLR (отражения) | ✅ | `r4_rendertarget_phase_sslr.cpp` — избавить от прямого D3D11 |
| Постобработка (tonemap, bloom, grading) | ✅ | — |
| PBR | ✅ | математика в шейдерах, переносится конвертером |
| Снежная маска, намокание, wallmarks | ✅ | — |
| **DLSS** | ❌ | NVIDIA-only; выключаем по `IXR_APPLE_SERIES` |
| **FSR3** | ❌ (stretch: MetalFX) | нативных AMD-библиотек на mac нет; замена — MetalFX temporal upscaler |
| **XeSS** | ❌ | Intel-only; выключаем |
| TAA / FXAA / SMAA | ✅ | шейдерные, переносятся |
| ImGui-оверлей | ⚠️ | нативный `imgui_impl_metal` vs `RHIImGuiLayout` — решить в Task 2.7 |
| Мультиплеер (`IXRAY_MP`) | ⛔ вне v1 | выключен в пресете сборки |

## Game Porting Toolkit Evaluation Findings

**Частично выполнено (Task 0.4).** Итог: сборка **стартует, но не рендерит**.

Что сделано:
- Скачана Windows-сборка IX-Ray **r1.3.6** (14.09.2026): `engine-x64-game-cop` (63 МБ) + `assets-cop` (26 МБ) → распакованы в `~/Hermes/ixray-mac-port/cop-baseline/` (поверх копии CoP). В `bin/` есть **все** рендереры: `xrRender_R1/R2/R4.dll` + `xrRender_DS0.dll` ✓
- Создана отдельная бутылка CrossOver **`ixray`** (шаблон win10_64). Бутылка `Steam` не тронута.
- **Найдена и снята проблема №1:** движок падал с `wine: Call to unimplemented function concrt140.dll.??0_TaskCollection@details@Concurrency@@QEAA@XZ, aborting`. Причина: IX-Ray линкуется с **MSVC Concurrency Runtime**, а встроенная в Wine реализация `concrt140` неполна. Лечение: установлен официальный **VC++ Redistributable 2015-2022 x64** в бутылку + прописаны нативные CRT в реестр бутылки (`HKCU\Software\Wine\DllOverrides`: `concrt140`, `msvcp140`, `msvcp140_1/2`, `vcruntime140`, `vcruntime140_1` = `native`). После этого ошибка исчезла.
  Полезно знать: CrossOver **игнорирует** `WINEPREFIX` (бутылка выбирается через `CX_BOTTLE`) и **игнорирует** `WINEDLLOVERRIDES` (override'ы только через реестр бутылки).
- **Проблема №2 (открыта):** после фикса CRT окно **«IX-Ray Engine» открывается, но экран чёрный**, процесс уходит в простой (CPU 0%), собственный лог движка не создаётся. Гипотезы: (а) движок выбирает DX9-рендерер (R2) — а **D3DMetal поддерживает только DirectX 11/12**, поэтому рисовать нечем; (б) особенность связки CrossOver + D3DMetal с этим движком.
- Не проверено: Metal Performance HUD, GPU frame capture, Metal System Trace.

**Вывод:** baseline **желателен, но не блокирует** Phase 1 (ядро всё равно собирается нативно, без Wine). Отложен осознанно по правилу «3 попытки»; к нему стоит вернуться, когда понадобится эталон производительности, либо через CrossOver GUI (установка VC++ «родным» способом + принудительный R4), либо через Whisky/Gcenx-wine.

## Trace Analysis

Трейсов нет (RenderDoc с Windows-платформы отсутствует, `.gputrace`/Metal System Trace появятся после Task 0.4). Рекомендация: снять GPU frame capture и Metal System Trace на шаге 0.4 — они станут эталоном для валидации Phase 2.

## Risk Assessment

| # | Риск | Вероятность | Влияние | Митигация |
|---|---|---|---|---|
| 1 | **Печь комбинаций шейдеров** (рантайм-перечисление неполно → розовые объекты) | высокая | высокое | dump-инструмент на реальном прогоне (Task 0.4/2.5); фолбэк-заглушка + лог ключа → допекаем; версионируем кеш |
| 2 | DXC на macOS (официального mac-билда нет) | средняя | высокое | цепочка: сборка из исходников → Wine-`dxc.exe` офлайн (eval env) → CI-прекомпил. Это офлайн-инструмент, не рантайм-костыль |
| 3 | Утечка D3D11 в `xrRenderDX10` (231) | средняя | среднее | Task 0.2b: разложить по файлам, расширять интерфейс RHI вместо `#ifdef` |
| 4 | Windows-измы ядра (Linux-поддержка у них «basic») | средняя | среднее | итеративно Task 1.3–1.5, правки только в Platform-слоях |
| 5 | MSVC-измы против clang (SAL, `__declspec`, unity-build) | средняя | низкое | `IXRAY_UNITYBUILD=OFF` при отладке |
| 6 | Объём Phase 2 (недели) | — | — | вехи M4–M9: каждая даёт видимый результат |

## Dependency Graph (порядок)

```
CMake preset (1.1) → Platform APPLE (1.2) → xrCore (1.3) → xrSound/xrScripts/xrPhysics (1.4)
   → xrEngine+xrPlay с заглушкой рендера (1.5) → автотест (1.6)
        ↓
   Metal-бэкенд: скелет+CMake (2.1) → выбор API (2.2) → device (2.3) → present (2.4)
        ↓                              ↘ шейдерный офлайн-конвейер (2.5)
   ресурсы/состояния (2.6) → треугольник (2.7) → полный R4 (2.8) → полировка (2.9)
        ↓
   игра: прогон CoP (3.1) → перф vs baseline (3.2) → триж фич (3.3)
```

## Open Questions

1. DXGI-специфика в swapchain R4 (форматы HDR, flip-модель) — что реально нужно для v1.
2. ImGui-бэкенд: нативный `imgui_impl_metal` (upstream) против их `RHIImGuiLayout` — решить в Task 2.7.
3. Формат/расположение кеша metallib — рядом с игрой или в `gamedata/shaders_cache`; важно не сломать модмейкерские механизмы (DLTX/addons).
4. Нужен ли `IXRAY_MP` на mac (сейчас выключен в пресете).

## Артефакты

| Что | Путь |
|---|---|
| Исходники (ветка `macos-metal`) | `~/Hermes/ixray-mac-port/ixray-src` |
| Windows-сборка для baseline | `~/Hermes/ixray-mac-port/win-build/` |
| Копия CoP для baseline | `~/Hermes/ixray-mac-port/cop-baseline/` |
| GPTK-скиллы/дока/сэмпл | `~/Hermes/ixray-mac-port/gptk/` |
| План порта | `~/Hermes/ixray-mac-port/.hermes/plans/2026-10-16_124500-ixray-macos-dxvk-native-port.md` |
