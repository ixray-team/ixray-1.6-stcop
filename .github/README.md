# ixray-1.6-stcop-winefixes

## RU

- Форк для исправления проблем запуска **IX-Ray 1.6 STCoP** в **CrossOver / Wine (D3DMetal/DXMT)**.
- Это не официальный macOS-порт, а практичная compatibility-ветка.

### Модель веток

- `r1.3.3_winefixes` — стабильные и проверенные фиксы для релиза `r1.3.3`.
- `r1.3.3_winefixes_dev` — тестовые/временные изменения до подтверждения.

### Конфигурация окружения

- MacBook Pro 14-inch (2021) (`MacBookPro18,3`)
- Chip: **Apple M1 Pro** (8 cores)
- RAM: **16 GB**
- macOS: **26.2** (build `25C56`)
- CrossOver: **26 beta 2**

- Сборка: **UTM** + **Windows 11 VM** на macOS.
- Рекомендуемый путь: собирать движок в VM (Visual Studio/CMake), запускать игру в CrossOver.

### Рекомендуемые настройки

- В `gamedata/configs/engine_external.ltx`: `USE_LEGACY_LIGHT = 0`
- После правок шейдеров очищать `_appdata_ixray_/shaders_cache/r1.3.3/d3d11`.

### Важно

- Оригинальные версии DX11-шейдеров движка не полностью совместимы с DXMT/D3DMetal, поэтому нужны winefix-правки шейдеров.
- В `r1.3.3_winefixes` уже включены фиксы критичных падений компиляции DX11-шейдеров на D3DMetal/DXMT (включая `deffer_detail` и fluid-пайплайн).
- Если вы воспользовались этим форком и что-то не работает — НЕ мучайте разработчиков основного проекта вопросами и issue.

---

## ENG

- Fork focused on fixing **IX-Ray 1.6 STCoP** issues under **CrossOver / Wine (D3DMetal/DXMT)**.
- This is not an official macOS port; it is a pragmatic compatibility branch.

### Branch model

- `r1.3.3_winefixes` — stable, confirmed fixes for upstream release `r1.3.3`.
- `r1.3.3_winefixes_dev` — in-testing changes before promotion.

### Environment

- MacBook Pro 14-inch (2021) (`MacBookPro18,3`)
- Chip: **Apple M1 Pro** (8 cores)
- RAM: **16 GB**
- macOS: **26.2** (build `25C56`)
- CrossOver: **26 beta 2**

- Build path: **UTM** + **Windows 11 VM** on macOS.
- Recommended workflow: build the engine in VM (Visual Studio/CMake), run the game via CrossOver.

### Recommended settings

- In `gamedata/configs/engine_external.ltx`: `USE_LEGACY_LIGHT = 0`
- After shader edits, clear `_appdata_ixray_/shaders_cache/r1.3.3/d3d11`.

### Note

- Original DX11 shader versions are not fully compatible with DXMT/D3DMetal, so winefix shader adjustments are required.
- `r1.3.3_winefixes` already includes fixes for critical DX11 shader compile crashes on D3DMetal/DXMT (including `deffer_detail` and fluid pipeline cases).
- If you use this fork and something breaks, please do NOT bother upstream `ixray-team/ixray-1.6-stcop` developers with questions or issues.
