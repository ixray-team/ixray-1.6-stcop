# ixray-1.6-stcop-winefixes

## RU

- Форк для исправления проблем запуска **IX-Ray 1.6 STCoP** в **CrossOver / Wine (D3DMetal/DXMT)**.
- Это не “официальная macOS-версия” движка, а набор практичных совместимых правок.

Если что-то ломается в этом форке — пожалуйста, **не** отправляйте issue в основной `ixray-team/ixray-1.6-stcop` по wine-специфичным проблемам.

### Что исправлено

- Убраны/адаптированы HLSL-конструкции, которые нестабильно обрабатываются D3DMetal:
  - stage-специфичные `register(ps, t0)` -> `register(t0)`
  - проблемные `SV_ClipDistance*` в отдельных шейдерах
  - конфликты семантик `TEXCOORD*`
  - проблемные swizzle-присваивания вида `N.xxx` (заменены на явные `float3(...)`)
  - scope-ошибки цикла в fluid-шейдерах

### Рекомендуемые настройки

- В `gamedata/configs/engine_external.ltx`:
  - `USE_LEGACY_LIGHT = 0`
- После изменения шейдеров очищать:
  - `_appdata_ixray_/shaders_cache/r1.3.3/d3d11`

### Известные ограничения

- Некоторые DX11-фичи могут оставаться нестабильными на отдельных версиях CrossOver/D3DMetal.
- Это ожидаемо для нативно-Windows рендера, запущенного через translation layer.

---

## ENG

- Fork focused on fixing **IX-Ray 1.6 STCoP** startup/runtime issues under **CrossOver / Wine (D3DMetal/DXMT)**.
- This is not an official macOS port; it is a pragmatic compatibility branch.

If something breaks here, please **do not** file Wine-specific issues against upstream `ixray-team/ixray-1.6-stcop`.

### What is fixed

- Patched/adapted HLSL patterns that are unstable under D3DMetal:
  - stage-specific `register(ps, t0)` -> `register(t0)`
  - problematic `SV_ClipDistance*` usage in selected shaders
  - `TEXCOORD*` semantic collisions
  - problematic swizzle-broadcast assignments like `N.xxx` (replaced with explicit `float3(...)`)
  - loop-scope issues in fluid shaders

### Recommended settings

- In `gamedata/configs/engine_external.ltx`:
  - `USE_LEGACY_LIGHT = 0`
- After shader changes, clear:
  - `_appdata_ixray_/shaders_cache/r1.3.3/d3d11`

### Known limitations

- Some DX11 features may still be unstable depending on CrossOver/D3DMetal version.
- This is expected for a Windows-first renderer running through a translation layer.
