# Классы и события сетевой игры

> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4

## Обзор

Здесь перечислены регистрации подсистемы сетевой игры. Клиентские и серверные классы используются в соответствующих режимах.

## Синтаксис

| Вызов | Аргументы и результат |
| --- | --- |
| `match:GetPlayersCount()` | Возвращает количество записей игроков на клиенте. |
| `match:GetPlayerByOrderID(index)` | Возвращает `game_PlayerState` по индексу `0 … count - 1`. Движок не проверяет границы. |
| `player:getName()` | Возвращает отображаемое имя игрока. `player.kills` и `player.deaths` содержат числовые счётчики. |
| `match:GetLocalPlayer()` | Возвращает состояние локального игрока или `nil`, пока оно недоступно. |
| `match:GetRoundTime()` | Возвращает прошедшее время от начала текущей фазы игры в формате `MM:SS` в сборке `XR_MP_BUILD`. |

## Примеры

### Собрать локальный снимок счёта

Передайте текущий экземпляр `game_cl_mp_script` из клиентского сетевого режима. Результат содержит обычные Lua-таблицы, которые можно использовать при перерисовке счёта.

```lua
local function collect_players(match)
    local result = {}
    for index = 0, match:GetPlayersCount() - 1 do
        local player = match:GetPlayerByOrderID(index)
        result[#result + 1] = {
            name = player:getName(),
            kills = player.kills,
            deaths = player.deaths,
        }
    end
    return result
end
```

### Получить данные локального игрока

Вызовите после синхронизации клиента. Если запись игрока ещё отсутствует, функция вернёт `nil`.

```lua
local function round_status(match)
    local player = match:GetLocalPlayer()
    if not player then return nil end
    return { name = player:getName(), time = match:GetRoundTime() }
end
```

## Регистрации

### Глобальная область

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_cl_GameState` | Класс | Клиентское состояние игры с данными локального игрока. Наследует методы `game_GameState`. | [game_cl_mp_script.cpp:94](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L94) |
| `game_cl_mp` | Класс | Базовое состояние многопользовательской игры на клиенте. Наследует методы `game_cl_GameState`. | [game_cl_mp_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L98) |
| `game_cl_mp_script()` | Класс | Клиентское состояние режима мультиплеера, доступное для расширения из Lua. Наследует методы `WrapType`. | [game_cl_mp_script.cpp:109](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L109) |
| `game_GameState()` | Класс | Общее состояние игрового режима с фазой и номером раунда. Наследует методы `DLL_Pure`. | [game_base_script.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L63) |
| `game_messages` | Класс | Числовые коды событий матча. | [game_sv_base_script.cpp:75](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L75) |
| `game_phases` | Класс | Числовые коды фаз матча. | [game_sv_base_script.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L63) |
| `game_player_flags` | Класс | Маски флагов записи игрока. | [game_sv_base_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L53) |
| `game_PlayerState()` | Класс | Запись игрока со статистикой, командой и флагами. Наследует методы `WrapType`. | [game_base_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L28) |
| `game_sv_Deathmatch()` | Класс | Серверное состояние режима Deathmatch. Наследует методы `WrapType`. | [game_sv_deathmatch_script.cpp:23](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_deathmatch_script.cpp#L23) |
| `game_sv_mp()` | Класс | Серверное состояние сетевой игры. Наследует методы `game_sv_GameState`. | [game_sv_mp_script.cpp:192](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L192) |
| `game_sv_mp_script()` | Класс | Серверное состояние режима мультиплеера, доступное для расширения из Lua. Наследует методы `WrapType`. | [game_sv_mp_script.cpp:209](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L209) |
| `RPoint()` | Класс | Данные точки появления с позицией и углами. | [game_cl_base_script.cpp:27](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L27) |
| `SZoneMapEntityData()` | Класс | Запись маркера объекта на карте зоны. | [game_cl_base_script.cpp:12](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L12) |
| `ZoneMapEntities` | Класс | Список записей маркеров объектов на карте. | [game_cl_base_script.cpp:17](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L17) |

### `game`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game.game_sv_GameState` | Класс | Базовое серверное состояние игрового режима. Наследует методы `game_GameState`. | [game_sv_base_script.cpp:31](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L31) |

### `game.game_sv_GameState`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game.game_sv_GameState:GenerateGameMessage(P)` | Метод | Формирует игровой сообщение. Аргументы `P` (`net_packet`). Реализация `P.w_begin(M_GAMEMESSAGE);`. | [game_sv_base_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L45) |
| `game.game_sv_GameState:get_eid(id)` | Метод | Возвращает `eid`. Аргументы `id` (`number`). Результат `game_PlayerState`. | [game_sv_base_script.cpp:33](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L33) |
| `game.game_sv_GameState:get_id(id)` | Метод | Возвращает идентификатор. Аргументы `id` (`ClientID`). Результат `game_PlayerState`. Реализация `xrClientData* C = (xrClientData*)m_server->ID_to_client (id); if (0==C) return nullptr; else return C->ps;`. | [game_sv_base_script.cpp:34](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L34) |
| `game.game_sv_GameState:get_id_2_eid(id)` | Метод | Возвращает идентификатор 2 `eid`. Аргументы `id` (`ClientID`). Результат `number`. | [game_sv_base_script.cpp:39](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L39) |
| `game.game_sv_GameState:get_name_id(id)` | Метод | Возвращает имя идентификатор. Аргументы `id` (`ClientID`). Результат `string`. Реализация `xrClientData* C = (xrClientData*)m_server->ID_to_client (id); return C == nullptr ? nullptr : C->ps->getName();`. | [game_sv_base_script.cpp:35](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L35) |
| `game.game_sv_GameState:get_option_i(lst, name, def)` | Метод | Возвращает целочисленную настройку. Аргументы `lst` (`string`), `name` (`string`), `def` (`number`). Результат `number`. | [game_sv_base_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L41) |
| `game.game_sv_GameState:get_option_s(lst, name, def)` | Метод | Возвращает строковую настройку. Аргументы `lst` (`string`), `name` (`string`), `def` (`string`). Результат `string`. | [game_sv_base_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L42) |
| `game.game_sv_GameState:get_player_name_id(id)` | Метод | Возвращает идентификатор игрока по имени. Аргументы `id` (`ClientID`). Результат `string`. Реализация `xrClientData* xrCData = m_server->ID_to_client(id); return xrCData == nullptr ? "unknown" : xrCData->ps->getName();`. | [game_sv_base_script.cpp:36](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L36) |
| `game.game_sv_GameState:get_players_count()` | Метод | Возвращает игроков количество. Результат `number`. Реализация `return m_server->GetClientsCount();`. | [game_sv_base_script.cpp:38](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L38) |
| `game.game_sv_GameState:getRP(team_idx, rp_idx)` | Метод | Возвращает `rp`. Аргументы `team_idx` (`number`), `rp_idx` (`number`). Результат `RPoint`. Реализация `if( (team_idx<TEAM_COUNT) && (rp_idx<rpoints[team_idx].size()) ) return rpoints[team_idx][rp_idx]; else return RPoint();`. | [game_sv_base_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L46) |
| `game.game_sv_GameState:getRPcount(team_idx)` | Метод | Возвращает `r` `pcount`. Аргументы `team_idx` (`number`). Результат `number`. Реализация `if ( !(team_idx<TEAM_COUNT) ) return 0; else return (u32)rpoints[team_idx].size();`. | [game_sv_base_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L47) |
| `game.game_sv_GameState:u_EventSend(packet, flags)` | Метод | Рассылает пакет всем подключённым клиентам с указанными сетевыми флагами. | [game_sv_base_script.cpp:43](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L43) |

### `game_cl_GameState`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_cl_GameState.local_player` | Поле для чтения и записи | Хранит локальный игрока. Доступно для чтения и записи. | [game_cl_mp_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L96) |
| `game_cl_GameState.local_svdpnid` | Поле для чтения и записи | Хранит локальный `svdpnid`. Доступно для чтения и записи. | [game_cl_mp_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L95) |

### `game_cl_mp_script`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_cl_mp_script:CanBeReady()` | Метод | Проверяет, может ли клиент перейти в состояние готовности. Результат `boolean`. | [game_cl_mp_script.cpp:125](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L125) |
| `game_cl_mp_script:createGameUI()` | Метод | Создаёт игровой интерфейса. Результат `CUIGameCustom`. | [game_cl_mp_script.cpp:135](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L135) |
| `game_cl_mp_script:createPlayerState(account_info)` | Метод | Создаёт игрока состояние. Аргументы `account_info` (`net_packet`). Результат `game_PlayerState`. Реализация `return inherited::createPlayerState(account_info);`. | [game_cl_mp_script.cpp:134](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L134) |
| `game_cl_mp_script:EventGen(packet, event_type, destination)` | Метод | Записывает заголовок события указанного типа и адресата в пакет для отправки. | [game_cl_mp_script.cpp:117](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L117) |
| `game_cl_mp_script:EventSend(packet)` | Метод | Отправляет подготовленный пакет игрового события. | [game_cl_mp_script.cpp:119](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L119) |
| `game_cl_mp_script:FillMapEntities(dst)` | Метод | Заполняет карты `entities`. Аргументы `dst` (`xr_vector<SZoneMapEntityData>`). | [game_cl_mp_script.cpp:131](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L131) |
| `game_cl_mp_script:GameEventGen(packet, destination)` | Метод | Записывает в пакет заголовок события типа `GE_GAME_EVENT` для указанного объекта. | [game_cl_mp_script.cpp:118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L118) |
| `game_cl_mp_script:GetClientIDByOrderID(idx)` | Метод | Возвращает идентификатор клиента `by` `order` идентификатор. Аргументы `idx` (`number`). Результат `ClientID`. Реализация `PLAYERS_MAP_IT I = players.begin(); std::advance(I,idx); return I->first;`. | [game_cl_mp_script.cpp:115](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L115) |
| `game_cl_mp_script:GetLocalPlayer()` | Метод | Возвращает состояние локального игрока или `nil`, пока он не получен от сервера. | [game_cl_mp_script.cpp:116](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L116) |
| `game_cl_mp_script:GetObjectByGameID(id)` | Метод | Возвращает объекта `by` игровой идентификатор. Аргументы `id` (`number`). Результат `game_object`. | [game_cl_mp_script.cpp:113](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L113) |
| `game_cl_mp_script:GetPlayerByOrderID(index)` | Метод | Возвращает запись игрока по индексу в клиентском списке. Допустимые индексы от `0` до `GetPlayersCount() - 1`. Границы не проверяются. | [game_cl_mp_script.cpp:114](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L114) |
| `game_cl_mp_script:GetPlayersCount()` | Метод | Возвращает число записей игроков в клиентском состоянии матча. | [game_cl_mp_script.cpp:112](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L112) |
| `game_cl_mp_script:GetRoundTime()` | Метод | Возвращает в сборке мультиплеера строку `MM:SS` со временем, прошедшим с начала текущей фазы. | [game_cl_mp_script.cpp:123](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L123) |
| `game_cl_mp_script:Init()` | Метод | Инициализирует клиентскую часть режима. Вызывается при запуске клиента. | [game_cl_mp_script.cpp:126](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L126) |
| `game_cl_mp_script:net_import_state(packet)` | Метод | Читает из сетевого пакета состояние матча, присланное сервером. | [game_cl_mp_script.cpp:129](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L129) |
| `game_cl_mp_script:OnKeyboardPress(key)` | Метод | Обработчик события «клавиатуры нажатия». Аргументы `key` (`number`). Результат `boolean`. | [game_cl_mp_script.cpp:127](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L127) |
| `game_cl_mp_script:OnKeyboardRelease(dik)` | Метод | Обработчик события «клавиатуры отпускания». Аргументы `dik` (`number`). Результат `boolean`. Реализация `if(!local_player \|\| local_player->IsSkip()) return true; else return false;`. | [game_cl_mp_script.cpp:128](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L128) |
| `game_cl_mp_script:shedule_Update(delta)` | Метод | Обрабатывает очередное обновление клиента. Получает интервал от планировщика. | [game_cl_mp_script.cpp:130](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L130) |
| `game_cl_mp_script:TranslateGameMessage(msg, P)` | Метод | Преобразует игровой сообщение. Аргументы `msg` (`number`), `P` (`net_packet`). | [game_cl_mp_script.cpp:132](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L132) |

### `game_GameState`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_GameState.round` | Поле для чтения | Хранит раунда. Доступно для чтения. | [game_base_script.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L67) |
| `game_GameState.start_time` | Поле для чтения | Хранит начала время. Доступно для чтения. | [game_base_script.cpp:68](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L68) |
| `game_GameState.type` | Поле для чтения и записи | Хранит тип. Доступно для чтения и записи. | [game_base_script.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L66) |
| `game_GameState:Phase()` | Метод | Возвращает числовой ID фазы матча. | [game_base_script.cpp:71](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L71) |
| `game_GameState:Round()` | Метод | Возвращает номер раунда. | [game_base_script.cpp:72](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L72) |
| `game_GameState:StartTime()` | Метод | Возвращает время начала текущей фазы. | [game_base_script.cpp:73](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L73) |
| `game_GameState:Type()` | Метод | Возвращает числовой тип игрового режима. | [game_base_script.cpp:70](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L70) |

### `game_messages`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_messages.GAME_EVENT_ARTEFACT_DESTROYED` | Константа | Код события мультиплеера «артефакта `destroyed`». | [game_sv_base_script.cpp:90](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L90) |
| `game_messages.GAME_EVENT_ARTEFACT_DROPPED` | Константа | Код события мультиплеера «артефакта `dropped`». | [game_sv_base_script.cpp:92](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L92) |
| `game_messages.GAME_EVENT_ARTEFACT_ONBASE` | Константа | Код события мультиплеера «артефакта `onbase`». | [game_sv_base_script.cpp:93](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L93) |
| `game_messages.GAME_EVENT_ARTEFACT_SPAWNED` | Константа | Код события мультиплеера «артефакта `spawned`». | [game_sv_base_script.cpp:89](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L89) |
| `game_messages.GAME_EVENT_ARTEFACT_TAKEN` | Константа | Код события мультиплеера «артефакта подбора». | [game_sv_base_script.cpp:91](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L91) |
| `game_messages.GAME_EVENT_BUY_MENU_CLOSED` | Константа | Код события мультиплеера «покупки меню закрытия». | [game_sv_base_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L96) |
| `game_messages.GAME_EVENT_PLAYER_BUY_FINISHED` | Константа | Код события мультиплеера «игрока покупки `finished`». | [game_sv_base_script.cpp:81](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L81) |
| `game_messages.GAME_EVENT_PLAYER_CHANGE_SKIN` | Константа | Код события мультиплеера «игрока изменить облика». | [game_sv_base_script.cpp:82](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L82) |
| `game_messages.GAME_EVENT_PLAYER_CHANGE_TEAM` | Константа | Код события мультиплеера «игрока изменить команды». | [game_sv_base_script.cpp:79](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L79) |
| `game_messages.GAME_EVENT_PLAYER_CONNECTED` | Константа | Код события мультиплеера «игрока `connected`». | [game_sv_base_script.cpp:83](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L83) |
| `game_messages.GAME_EVENT_PLAYER_DISCONNECTED` | Константа | Код события мультиплеера «игрока `disconnected`». | [game_sv_base_script.cpp:84](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L84) |
| `game_messages.GAME_EVENT_PLAYER_ENTER_TEAM_BASE` | Константа | Код события мультиплеера «игрока `enter` команды базы». | [game_sv_base_script.cpp:94](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L94) |
| `game_messages.GAME_EVENT_PLAYER_JOIN_TEAM` | Константа | Код события мультиплеера «игрока `join` команды». | [game_sv_base_script.cpp:86](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L86) |
| `game_messages.GAME_EVENT_PLAYER_KILL` | Константа | Код события мультиплеера «игрока убийства». | [game_sv_base_script.cpp:80](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L80) |
| `game_messages.GAME_EVENT_PLAYER_KILLED` | Константа | Код события мультиплеера «игрока `killed`». | [game_sv_base_script.cpp:85](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L85) |
| `game_messages.GAME_EVENT_PLAYER_LEAVE_TEAM_BASE` | Константа | Код события мультиплеера «игрока `leave` команды базы». | [game_sv_base_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L95) |
| `game_messages.GAME_EVENT_PLAYER_READY` | Константа | Код события мультиплеера «игрока готовности». | [game_sv_base_script.cpp:78](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L78) |
| `game_messages.GAME_EVENT_ROUND_END` | Константа | Код события мультиплеера «раунда конца». | [game_sv_base_script.cpp:88](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L88) |
| `game_messages.GAME_EVENT_ROUND_STARTED` | Константа | Код события мультиплеера «раунда запуска». | [game_sv_base_script.cpp:87](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L87) |
| `game_messages.GAME_EVENT_SCRIPT_BEGINS_FROM` | Константа | Код события мультиплеера «скрипта `begins` `from`». | [game_sv_base_script.cpp:99](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L99) |
| `game_messages.GAME_EVENT_SKIN_MENU_CLOSED` | Константа | Код события мультиплеера «облика меню закрытия». | [game_sv_base_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L98) |
| `game_messages.GAME_EVENT_TEAM_MENU_CLOSED` | Константа | Код события мультиплеера «команды меню закрытия». | [game_sv_base_script.cpp:97](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L97) |

### `game_phases`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_phases.GAME_PHASE_INPROGRESS` | Константа | Код фазы матча «`inprogress`». | [game_sv_base_script.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L67) |
| `game_phases.GAME_PHASE_NONE` | Константа | Код фазы матча «`none`». | [game_sv_base_script.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L66) |
| `game_phases.GAME_PHASE_PENDING` | Константа | Код фазы матча «`pending`». | [game_sv_base_script.cpp:68](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L68) |
| `game_phases.GAME_PHASE_SCRIPT_BEGINS_FROM` | Константа | Код фазы матча «скрипта `begins` `from`». | [game_sv_base_script.cpp:72](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L72) |
| `game_phases.GAME_PHASE_TEAM1_SCORES` | Константа | Код фазы матча «`team1` `scores`». | [game_sv_base_script.cpp:69](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L69) |
| `game_phases.GAME_PHASE_TEAM2_SCORES` | Константа | Код фазы матча «`team2` `scores`». | [game_sv_base_script.cpp:70](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L70) |
| `game_phases.GAME_PHASE_TEAMS_IN_A_DRAW` | Константа | Код фазы матча «`teams` `in` `a` `draw`». | [game_sv_base_script.cpp:71](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L71) |

### `game_player_flags`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_player_flags.GAME_PLAYER_FLAG_LOCAL` | Константа | Маска флага состояния игрока `GAME_PLAYER_FLAG_LOCAL` для `testFlag`, `setFlag` и `resetFlag`. | [game_sv_base_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L56) |
| `game_player_flags.GAME_PLAYER_FLAG_READY` | Константа | Маска флага состояния игрока `GAME_PLAYER_FLAG_READY` для `testFlag`, `setFlag` и `resetFlag`. | [game_sv_base_script.cpp:57](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L57) |
| `game_player_flags.GAME_PLAYER_FLAG_SCRIPT_BEGINS_FROM` | Константа | Маска флага состояния игрока `GAME_PLAYER_FLAG_SCRIPT_BEGINS_FROM` для `testFlag`, `setFlag` и `resetFlag`. | [game_sv_base_script.cpp:60](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L60) |
| `game_player_flags.GAME_PLAYER_FLAG_SPECTATOR` | Константа | Маска флага состояния игрока `GAME_PLAYER_FLAG_SPECTATOR` для `testFlag`, `setFlag` и `resetFlag`. | [game_sv_base_script.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L59) |
| `game_player_flags.GAME_PLAYER_FLAG_VERY_VERY_DEAD` | Константа | Маска флага состояния игрока `GAME_PLAYER_FLAG_VERY_VERY_DEAD` для `testFlag`, `setFlag` и `resetFlag`. | [game_sv_base_script.cpp:58](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L58) |

### `game_PlayerState`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_PlayerState.deaths` | Поле для чтения и записи | Хранит число смертей. Доступно для чтения и записи. | [game_base_script.cpp:32](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L32) |
| `game_PlayerState.flags` | Поле для чтения и записи | Хранит флаги. Доступно для чтения и записи. | [game_base_script.cpp:34](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L34) |
| `game_PlayerState.GameID` | Поле для чтения и записи | Хранит игровой идентификатор. Доступно для чтения и записи. | [game_base_script.cpp:36](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L36) |
| `game_PlayerState.kills` | Поле для чтения и записи | Хранит `rival` число убийств. Доступно для чтения и записи. | [game_base_script.cpp:31](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L31) |
| `game_PlayerState.LastBuyAcount` | Поле для чтения и записи | Хранит последний покупки `acount`. Доступно для чтения и записи. | [game_base_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L45) |
| `game_PlayerState.lasthitter` | Поле для чтения и записи | Хранит `lasthitter`. Доступно для чтения и записи. | [game_base_script.cpp:38](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L38) |
| `game_PlayerState.lasthitweapon` | Поле для чтения и записи | Хранит `lasthitweapon`. Доступно для чтения и записи. | [game_base_script.cpp:39](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L39) |
| `game_PlayerState.money_delta` | Поле для чтения и записи | Хранит деньги `delta`. Доступно для чтения и записи. | [game_base_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L42) |
| `game_PlayerState.money_for_round` | Поле для чтения и записи | Хранит деньги `for` раунда. Доступно для чтения и записи. | [game_base_script.cpp:33](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L33) |
| `game_PlayerState.ping` | Поле для чтения и записи | Хранит пинга. Доступно для чтения и записи. | [game_base_script.cpp:35](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L35) |
| `game_PlayerState.pItemList` | Поле для чтения и записи | Хранит `p` предмета списка. Доступно для чтения и записи. | [game_base_script.cpp:44](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L44) |
| `game_PlayerState.RespawnTime` | Поле для чтения и записи | Хранит возрождения время. Доступно для чтения и записи. | [game_base_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L41) |
| `game_PlayerState.skin` | Поле для чтения и записи | Хранит облика. Доступно для чтения и записи. | [game_base_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L40) |
| `game_PlayerState.team` | Поле для чтения и записи | Хранит команды. Доступно для чтения и записи. | [game_base_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L30) |
| `game_PlayerState:getName()` | Метод | Возвращает отображаемое имя игрока. | [game_base_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L49) |
| `game_PlayerState:net_Export(packet, full)` | Метод | Сериализует запись игрока в сетевой пакет. `full` включает полную запись. | [game_base_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L52) |
| `game_PlayerState:net_Import(packet)` | Метод | Загружает запись игрока из входящего сетевого пакета. | [game_base_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L53) |
| `game_PlayerState:resetFlag(f)` | Метод | Сбрасывает флаг. Аргументы `f` (`number`). Реализация `flags__ &= ~(f);`. | [game_base_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L48) |
| `game_PlayerState:setFlag(f)` | Метод | Задаёт флаг. Аргументы `f` (`number`). Реализация `flags__ \|= f;`. | [game_base_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L47) |
| `game_PlayerState:testFlag(f)` | Метод | Проверяет флаг записи игрока по маске из game_player_flags. Аргументы `f` (`number`). Результат `boolean`. Реализация `return !!(flags__ & f);`. | [game_base_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L46) |

### `game_sv_Deathmatch`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_sv_Deathmatch:GetTeamData(team_id)` | Метод | Возвращает запись команды по ID из состояния сервера Deathmatch. | [game_sv_deathmatch_script.cpp:25](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_deathmatch_script.cpp#L25) |
| `game_sv_Deathmatch:type_name()` | Метод | Возвращает имя серверного игрового режима. Для базовой реализации это `"deathmatch"`. | [game_sv_deathmatch_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_deathmatch_script.cpp#L28) |

### `game_sv_mp`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_sv_mp:KillPlayer(client_id, game_id)` | Метод | Помечает игрока на сервере как убитого. Получает сетевой ID клиента и ID игрового объекта. | [game_sv_mp_script.cpp:195](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L195) |
| `game_sv_mp:SendPlayerKilledMessage(KilledID, KillType, KillerID, WeaponID, SpecialKill)` | Метод | Отправляет игрока `killed` сообщение. Аргументы `KilledID` (`number`), `KillType` (`KILL_TYPE`), `KillerID` (`number`), `WeaponID` (`number`), `SpecialKill` (`SPECIAL_KILL_TYPE`). | [game_sv_mp_script.cpp:196](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L196) |
| `game_sv_mp:signal_Syncronize()` | Метод | Устанавливает флаг принудительной синхронизации состояния сервера. | [game_sv_mp_script.cpp:197](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L197) |

### `game_sv_mp_script`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `game_sv_mp_script:Create(options)` | Метод | Инициализирует серверный режим по строке параметров игры. | [game_sv_mp_script.cpp:221](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L221) |
| `game_sv_mp_script:createPlayerState(account_info)` | Метод | Создаёт игрока состояние. Аргументы `account_info` (`net_packet`). Результат `game_PlayerState`. Реализация `return inherited::createPlayerState(account_info);`. | [game_sv_mp_script.cpp:230](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L230) |
| `game_sv_mp_script:GetHitParamsImpulse(P)` | Метод | Возвращает попадания параметры импульса. Аргументы `P` (`net_packet`). Результат `number`. | [game_sv_mp_script.cpp:216](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L216) |
| `game_sv_mp_script:GetHitParamsPower(P)` | Метод | Возвращает попадания параметры выносливость. Аргументы `P` (`net_packet`). Результат `number`. | [game_sv_mp_script.cpp:215](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L215) |
| `game_sv_mp_script:GetTeamData(Team)` | Метод | Возвращает команды данные. Аргументы `Team` (`number`). Результат `TeamStruct`. | [game_sv_mp_script.cpp:211](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L211) |
| `game_sv_mp_script:net_Export_State(packet, client_id)` | Метод | Записывает серверное состояние игры в пакет для указанного клиента. | [game_sv_mp_script.cpp:229](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L229) |
| `game_sv_mp_script:OnEvent(P, type, time, sender)` | Метод | Обработчик события «события». Аргументы `P` (`net_packet`), `type` (`number`), `time` (`number`), `sender` (`ClientID`). Реализация `inherited::OnEvent(P,type,time,sender);`. | [game_sv_mp_script.cpp:220](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L220) |
| `game_sv_mp_script:OnPlayerHitPlayer(id_hitter, id_hitted, P)` | Метод | Обработчик события «игрока попадания игрока». Аргументы `id_hitter` (`number`), `id_hitted` (`number`), `P` (`net_packet`). | [game_sv_mp_script.cpp:223](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L223) |
| `game_sv_mp_script:OnRoundEnd()` | Метод | Обработчик события «раунда конца». | [game_sv_mp_script.cpp:227](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L227) |
| `game_sv_mp_script:OnRoundStart()` | Метод | Обработчик события «раунда начала». | [game_sv_mp_script.cpp:225](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L225) |
| `game_sv_mp_script:SetHitParams(P, impulse, power)` | Метод | Задаёт попадания параметры. Аргументы `P` (`net_packet`), `impulse` (`number`), `power` (`number`). | [game_sv_mp_script.cpp:214](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L214) |
| `game_sv_mp_script:SpawnPlayer(id, N)` | Метод | Создаёт на уровне игрока. Аргументы `id` (`ClientID`), `N` (`string`). | [game_sv_mp_script.cpp:212](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L212) |
| `game_sv_mp_script:switch_Phase(phase)` | Метод | Переводит сервер в фазу с указанным числовым ID из `game_phases`. | [game_sv_mp_script.cpp:213](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L213) |
| `game_sv_mp_script:type_name()` | Метод | Возвращает строковое имя серверного режима. Может переопределяться скриптом режима. | [game_sv_mp_script.cpp:218](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L218) |

### `RPoint`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `RPoint.A` | Поле для чтения и записи | Хранит `a`. Доступно для чтения и записи. | [game_cl_base_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L30) |
| `RPoint.P` | Поле для чтения и записи | Хранит `p`. Доступно для чтения и записи. | [game_cl_base_script.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L29) |

### `SZoneMapEntityData`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `SZoneMapEntityData.pos` | Поле для чтения и записи | Хранит позицию. Доступно для чтения и записи. | [game_cl_base_script.cpp:14](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L14) |

### `ZoneMapEntities`

| Вызов или имя | Тип | Описание | Регистрация |
| --- | --- | --- | --- |
| `ZoneMapEntities:push_back(entry)` | Метод | Добавляет `SZoneMapEntityData` в конец списка маркеров карты. | [game_cl_base_script.cpp:18](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L18) |

## См. также

- [Новые экспорты Lua](../lua-exports.md)
