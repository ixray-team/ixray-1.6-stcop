# ixray-1.6-stcop-winefixes

## RU

- Форк для исправления проблем запуска **IX-Ray 1.6 STCoP** в **CrossOver / Wine (D3DMetal/DXMT)**.
- Это не официальный macOS-порт, а практичный compatibility-ветвь.

### Модель веток

- `r1.3.3_winefixes` — стабильные и проверенные фиксы для релиза `r1.3.3`.
- `r1.3.3_winefixes_dev` — тестовые/временные изменения до подтверждения.

### Рекомендуемые настройки

- В `gamedata/configs/engine_external.ltx`: `USE_LEGACY_LIGHT = 0`
- После правок шейдеров очищать `_appdata_ixray_/shaders_cache/r1.3.3/d3d11`.

### Важно

- Wine/CrossOver-специфичные проблемы не нужно отправлять в upstream `ixray-team/ixray-1.6-stcop`.

---

## ENG

- Fork focused on fixing **IX-Ray 1.6 STCoP** issues under **CrossOver / Wine (D3DMetal/DXMT)**.
- This is not an official macOS port; it is a pragmatic compatibility branch.

### Branch model

- `r1.3.3_winefixes` — stable, confirmed fixes for upstream release `r1.3.3`.
- `r1.3.3_winefixes_dev` — in-testing changes before promotion.

### Recommended settings

- In `gamedata/configs/engine_external.ltx`: `USE_LEGACY_LIGHT = 0`
- After shader edits, clear `_appdata_ixray_/shaders_cache/r1.3.3/d3d11`.

### Note

- Please do not file Wine/CrossOver-specific issues against upstream `ixray-team/ixray-1.6-stcop`.
