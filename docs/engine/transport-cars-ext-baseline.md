# Baseline: перенос транспорта X-Ray-Cars-Ext

Дата: 2026-11-01  
Ветка: `feature/transport-cars-ext`  
Базовая точка: `c005ee2e3` (`develop`) + docs-коммит `024e31803`  
Спецификация: `docs/engine/transport-cars-ext-spec.md`  
План: `docs/engine/transport-cars-ext-plan.md`

## 1. Окружение

| Параметр | Значение |
| --- | --- |
| ОС | Windows |
| CMake | 4.5.0-rc2 |
| Генератор текущей сборки | Visual Studio 18 2026 |
| Установленный VS | `C:\Program Files\Microsoft Visual Studio\18\Community` |
| Build dir | `H:\IXRAY\ixray-1.6-stcop\build` |
| Текущая конфигурация | `RelWithDebInfo` |
| Game root | `H:\IXRAY` |
| Addons root | `H:\IXRAY\ixr_addons` |
| Текущий аддон | `H:\IXRAY\ixr_addons\carpack` |
| `SPAWN_VERSION` | `u16(135)` (`src/xrServerEntities/xrServer_Objects.h:173`) |

## 2. Хеши контента

### 2.1. OGF

| Файл | Размер | SHA-256 |
| --- | ---: | --- |
| `meshes\dynamics\vehicles\zaz968\veh_zaz_968_u_01.ogf` | 4 179 399 | `5DD3C33DDF6D098BDFEA829076025A7C5176E5A3016F3D04B2D6108A8BDDD866` |
| `meshes\dynamics\vehicles\zaz968\veh_zaz_968_u_02.ogf` | 4 179 426 | `6950E6AF5E30AAFC1ED0ECED095E094B68DC94E8814E6DCAEC03D83C0ECC5175` |
| `meshes\dynamics\vehicles\niva\veh_niva_u_01.ogf` | 4 725 435 | `4C1E726BA3016F11C70D706B4D16394BDCCBDA5A31CA0C5FD2CE49774793C58B` |

### 2.2. Конфиги

| Файл | Размер | SHA-256 |
| --- | ---: | --- |
| `configs\models\vehicles\veh_zaz_968_u_03.ltx` | 18 474 | `A57B29E5FE6E829CCB53B069EF2E92550A89B24E09A8F40A97C21D7204111758` |
| `configs\models\vehicles\veh_niva_u_02.ltx` | 17 830 | `EA10D74266D578606F813E1164332FED5E5F30993C01365A1D2C2227CA901CC4` |
| `configs\models\vehicles\veh_niva_u_04.ltx` | 17 958 | `B3F7CE7E8F4F4ECBC88988F5A57EDE96430CA37D36DD13E2332A373AB1D58304` |
| `configs\misc\vehicles.ltx` | 5 027 | `BD78F1288D6112C400E8E7CA96005A72F5027BD6D6399768386E01755354A981` |

### 2.3. OGF user data

| OGF | `#include` |
| --- | --- |
| `veh_zaz_968_u_01.ogf` | `models\vehicles\veh_zaz_968_u_03.ltx` |
| `veh_zaz_968_u_02.ogf` | `models\vehicles\veh_zaz_968_u_03.ltx` |
| `veh_niva_u_01.ogf` | `models\vehicles\veh_niva_u_02.ltx` |

`veh_niva_u_02.ogf` → `veh_niva_u_04.ltx`; `veh_super_niva.ogf` → `veh_super_niva.ltx`, но `veh_super_niva.ltx` в `carpack` отсутствует и не подключён в `vehicles.ltx`.

## 3. Baseline-сборка

### 3.1. RelWithDebInfo

Команда:

```powershell
cmake --build build --config RelWithDebInfo --target xrGame
```

Результат: успешно; `xrGame.dll` собран. В выводе нет ошибок/предупреждений, влияющих на решение.

Артефакты до/после:

| Артефакт | Размер | Timestamp | SHA-256 |
| --- | ---: | --- | --- |
| `build\bin\RelWithDebInfo\xrGame.dll` | 30 486 016 | 2026-11-01T17:13:55 | `B9FFFEFB265D0961C4645FA127068EDCE4C447739B68860F0833F95A809D9194` |
| `build\bin\RelWithDebInfo\xrEngine.exe` | 634 368 | 2026-11-01T17:14:06 | `CE2F23A70C34E19FD8F3C36D2368C3F851C6BFE2CFD357A977878BD1EFBDF385` |
| `build\bin\RelWithDebInfo\xrEngineCore.dll` | 3 693 568 | 2026-11-01T17:10:15 | `41EE85B607A05937D6F69929BAFC18C630161085F1B88295B7AD89E848C86CEB` |

После сборки `xrGame.dll`/`xrEngine.exe` не изменились (инкрементальная сборка без изменений исходников), хеши совпадают с исходными.

### 3.2. Debug

Команда:

```powershell
cmake --build build --config Debug --target xrGame
```

Результат: успешно; `xrGame.dll` собран. Артефакт:

| Артефакт | Размер | Timestamp | SHA-256 |
| --- | ---: | --- | --- |
| `build\bin\Debug\xrGame.dll` | 47 283 712 | 2026-11-01T18:32:53 | `B12DE7A24C00EAB95606E71EAFFDBB72709EA3275DC16E1C52E022E12CE7D65F` |

`xrEngine.exe` в Debug не собирался, так как цель — только `xrGame`; для ручного прогона Debug потребуется отдельная сборка `xrEngine`.

### 3.3. Release

Команда:

```powershell
cmake --build build --config Release --target xrGame
```

Результат: успешно; `xrGame.dll` собран.

| Артефакт | Размер | Timestamp | SHA-256 |
| --- | ---: | --- | --- |
| `build\bin\Release\xrGame.dll` | 13 445 120 | 2026-11-01T18:38:50 | `4DE874254C71510BF4DCFA57AEE94A21A1CF1CA920BF5A4FEF16C059981CA331` |

Все три baseline-конфигурации (`RelWithDebInfo`, `Debug`, `Release`) собираются без ошибок. Debug-сборка `xrEngine.exe` не выполнялась; для ручного прогона Debug потребуется отдельная цель.

## 4. Тестовый стенд

Историческая установка `H:\IXRAY\bin` содержит сторонние бинарники и не используется для проверки новой сборки. Чтобы не перезаписывать её, тестовый стенд собирается в `build\bin\RelWithDebInfo`:

- `gamedata` → junction на `H:\IXRAY\gamedata`;
- `levels` → junction на `H:\IXRAY\levels`;
- `ixr_addons` → junction на `H:\IXRAY\ixr_addons`;
- `resources`, `localization`, `patches`, `mp` → junction на соответствующие каталоги `H:\IXRAY`;
- `fsgame.ltx` копируется из корня репозитория.

`$fs_root$` в `fsgame.ltx` — каталог `xrEngine.exe`, поэтому junction'ы делают тестовый стенд самодостаточным. `carpack` уже лежит в `H:\IXRAY\ixr_addons\carpack` и подключается через `mod_system_carpack.ltx`.

Проверка выполняется вручную; автоматический autotest не является критерием приёмки, но может использоваться как вспомогательный smoke.

### 4.1. Спавн машины

В консоли после загрузки уровня:

```text
run_string alife():create("veh_zaz968",db.actor:position(),db.actor:level_vertex_id(),db.actor:game_vertex_id())
run_string alife():create("veh_zaz968_2",db.actor:position(),db.actor:level_vertex_id(),db.actor:game_vertex_id())
run_string alife():create("veh_niva_green",db.actor:position(),db.actor:level_vertex_id(),db.actor:game_vertex_id())
```

Если `run_string` недоступен в конкретной сборке, используется временный тестовый скрипт в `gamedata\scripts` тестового стенда (не в репозитории).

### 4.2. Baseline-сценарий

1. Загрузить уровень с актёром.
2. Спавнить `veh_zaz968`.
3. Подойти, войти, запустить двигатель, проехать вперёд/назад.
4. Проверить камеры, HUD, свет, двери, багажник.
5. Сохранить/загрузить.
6. Повторить для `veh_zaz968_2` и `veh_niva_green`.

Текущее поведение фиксируется как baseline: базовые колёса/двери/турель/свет/топливо, без КПП/CVT/ABS/подвески/nitro/3D-приборов.

## 5. Известные особенности baseline

- `CCar` в IXRAY — упрощённая реализация; `Car.cpp` ~54 КБ против ~220 КБ в Cars-Ext.
- Текущий `CCar` — `final`, `XREFLECT`, 12 базовых классов; в Cars-Ext — 13 базовых классов, включая `CRocketLauncher`.
- Текущая MP-схема — гибридная: сервер и владелец симулируют параллельно; `SyncWrite` ограничен 255 байтами.
- `CarConfig`/`CarEditor` уже нерабочие (фильтр `C_NIVA`, запись не в тот источник); это не регрессия.
- В `carpack` нет иконок `ui_veh_*` и локализации для трёх машин; это принято.
- `veh_zaz968` и `veh_zaz968_2` используют один модельный конфиг `veh_zaz_968_u_03.ltx`.

## 6. Следующий шаг

После завершения Debug/Release baseline — этап B: замена `Car.h`/`Car.cpp` и модулей `Car*`, первый сборочный гейт B.
