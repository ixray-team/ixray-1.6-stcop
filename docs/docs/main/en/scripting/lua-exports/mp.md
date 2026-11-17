# Multiplayer classes and events

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

This section lists multiplayer subsystem registrations. Client and server classes are used in their respective modes.

## Syntax

| Call | Arguments and result |
| --- | --- |
| `match:GetPlayersCount()` | Returns the number of player records on the client. |
| `match:GetPlayerByOrderID(index)` | Returns a `game_PlayerState`. Valid indices run from `0` through `count - 1`. The engine does not check the bounds. |
| `player:getName()` | Returns the player’s display name. `player.kills` and `player.deaths` are numeric counters. |
| `match:GetLocalPlayer()` | Returns the local player state, or `nil` while it is unavailable. |
| `match:GetRoundTime()` | Returns elapsed time since the current game phase started as `MM:SS` in an `XR_MP_BUILD` build. |

## Examples

### Collect a local scoreboard snapshot

Pass the active `game_cl_mp_script` instance from the multiplayer client. The result contains plain Lua tables for redrawing the scoreboard.

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

### Read local player status

Call after client synchronization. The function returns `nil` while the player record is unavailable.

```lua
local function round_status(match)
    local player = match:GetLocalPlayer()
    if not player then return nil end
    return { name = player:getName(), time = match:GetRoundTime() }
end
```

## Registrations

### Global scope

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_cl_GameState` | Class | Client-side state with local player data. Inherits methods from `game_GameState`. | [game_cl_mp_script.cpp:94](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L94) |
| `game_cl_mp` | Class | Base multiplayer state on the client. Inherits methods from `game_cl_GameState`. | [game_cl_mp_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L98) |
| `game_cl_mp_script()` | Class | Multiplayer client state exposed for Lua game modes. Inherits methods from `WrapType`. | [game_cl_mp_script.cpp:109](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L109) |
| `game_GameState()` | Class | Shared game mode state with phase and round data. Inherits methods from `DLL_Pure`. | [game_base_script.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L63) |
| `game_messages` | Class | Numeric match event codes. | [game_sv_base_script.cpp:75](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L75) |
| `game_phases` | Class | Numeric match phase codes. | [game_sv_base_script.cpp:63](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L63) |
| `game_player_flags` | Class | Flag masks for player-state records. | [game_sv_base_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L53) |
| `game_PlayerState()` | Class | Player record with statistics, team, and flags. Inherits methods from `WrapType`. | [game_base_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L28) |
| `game_sv_Deathmatch()` | Class | Server-side Deathmatch state. Inherits methods from `WrapType`. | [game_sv_deathmatch_script.cpp:23](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_deathmatch_script.cpp#L23) |
| `game_sv_mp()` | Class | Multiplayer game state on the server. Inherits methods from `game_sv_GameState`. | [game_sv_mp_script.cpp:192](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L192) |
| `game_sv_mp_script()` | Class | Multiplayer server state exposed for Lua game modes. Inherits methods from `WrapType`. | [game_sv_mp_script.cpp:209](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L209) |
| `RPoint()` | Class | Spawn point data with position and angles. | [game_cl_base_script.cpp:27](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L27) |
| `SZoneMapEntityData()` | Class | Map marker record for an entity. | [game_cl_base_script.cpp:12](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L12) |
| `ZoneMapEntities` | Class | List of entity markers for the zone map. | [game_cl_base_script.cpp:17](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L17) |

### `game`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game.game_sv_GameState` | Class | Base server-side game mode state. Inherits methods from `game_GameState`. | [game_sv_base_script.cpp:31](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L31) |

### `game.game_sv_GameState`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game.game_sv_GameState:GenerateGameMessage(P)` | Method | Generates the game message. Arguments `P` (`net_packet`). Implementation `P.w_begin(M_GAMEMESSAGE);`. | [game_sv_base_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L45) |
| `game.game_sv_GameState:get_eid(id)` | Method | Returns the eid. Arguments `id` (`number`). Result `game_PlayerState`. | [game_sv_base_script.cpp:33](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L33) |
| `game.game_sv_GameState:get_id(id)` | Method | Returns the id. Arguments `id` (`ClientID`). Result `game_PlayerState`. Implementation `xrClientData* C = (xrClientData*)m_server->ID_to_client (id); if (0==C) return nullptr; else return C->ps;`. | [game_sv_base_script.cpp:34](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L34) |
| `game.game_sv_GameState:get_id_2_eid(id)` | Method | Returns the id 2 eid. Arguments `id` (`ClientID`). Result `number`. | [game_sv_base_script.cpp:39](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L39) |
| `game.game_sv_GameState:get_name_id(id)` | Method | Returns the name id. Arguments `id` (`ClientID`). Result `string`. Implementation `xrClientData* C = (xrClientData*)m_server->ID_to_client (id); return C == nullptr ? nullptr : C->ps->getName();`. | [game_sv_base_script.cpp:35](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L35) |
| `game.game_sv_GameState:get_option_i(lst, name, def)` | Method | Returns the integer option. Arguments `lst` (`string`), `name` (`string`), `def` (`number`). Result `number`. | [game_sv_base_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L41) |
| `game.game_sv_GameState:get_option_s(lst, name, def)` | Method | Returns the string option. Arguments `lst` (`string`), `name` (`string`), `def` (`string`). Result `string`. | [game_sv_base_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L42) |
| `game.game_sv_GameState:get_player_name_id(id)` | Method | Returns the player ID by name. Arguments `id` (`ClientID`). Result `string`. Implementation `xrClientData* xrCData = m_server->ID_to_client(id); return xrCData == nullptr ? "unknown" : xrCData->ps->getName();`. | [game_sv_base_script.cpp:36](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L36) |
| `game.game_sv_GameState:get_players_count()` | Method | Returns the players count. Result `number`. Implementation `return m_server->GetClientsCount();`. | [game_sv_base_script.cpp:38](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L38) |
| `game.game_sv_GameState:getRP(team_idx, rp_idx)` | Method | Returns the rp. Arguments `team_idx` (`number`), `rp_idx` (`number`). Result `RPoint`. Implementation `if( (team_idx<TEAM_COUNT) && (rp_idx<rpoints[team_idx].size()) ) return rpoints[team_idx][rp_idx]; else return RPoint();`. | [game_sv_base_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L46) |
| `game.game_sv_GameState:getRPcount(team_idx)` | Method | Returns the r pcount. Arguments `team_idx` (`number`). Result `number`. Implementation `if ( !(team_idx<TEAM_COUNT) ) return 0; else return (u32)rpoints[team_idx].size();`. | [game_sv_base_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L47) |
| `game.game_sv_GameState:u_EventSend(packet, flags)` | Method | Broadcasts a packet to every connected client using the supplied network flags. | [game_sv_base_script.cpp:43](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L43) |

### `game_cl_GameState`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_cl_GameState.local_player` | Read/write field | Stores the local player. Readable and writable. | [game_cl_mp_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L96) |
| `game_cl_GameState.local_svdpnid` | Read/write field | Stores the local svdpnid. Readable and writable. | [game_cl_mp_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L95) |

### `game_cl_mp_script`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_cl_mp_script:CanBeReady()` | Method | Checks whether the client can enter the ready state. Result `boolean`. | [game_cl_mp_script.cpp:125](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L125) |
| `game_cl_mp_script:createGameUI()` | Method | Creates the game ui. Result `CUIGameCustom`. | [game_cl_mp_script.cpp:135](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L135) |
| `game_cl_mp_script:createPlayerState(account_info)` | Method | Creates the player state. Arguments `account_info` (`net_packet`). Result `game_PlayerState`. Implementation `return inherited::createPlayerState(account_info);`. | [game_cl_mp_script.cpp:134](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L134) |
| `game_cl_mp_script:EventGen(packet, event_type, destination)` | Method | Writes the header for the given event type and destination into the packet. | [game_cl_mp_script.cpp:117](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L117) |
| `game_cl_mp_script:EventSend(packet)` | Method | Sends a prepared game event packet. | [game_cl_mp_script.cpp:119](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L119) |
| `game_cl_mp_script:FillMapEntities(dst)` | Method | Fills the map entities. Arguments `dst` (`xr_vector<SZoneMapEntityData>`). | [game_cl_mp_script.cpp:131](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L131) |
| `game_cl_mp_script:GameEventGen(packet, destination)` | Method | Writes a `GE_GAME_EVENT` header for the given destination object into the packet. | [game_cl_mp_script.cpp:118](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L118) |
| `game_cl_mp_script:GetClientIDByOrderID(idx)` | Method | Returns the client id by order id. Arguments `idx` (`number`). Result `ClientID`. Implementation `PLAYERS_MAP_IT I = players.begin(); std::advance(I,idx); return I->first;`. | [game_cl_mp_script.cpp:115](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L115) |
| `game_cl_mp_script:GetLocalPlayer()` | Method | Returns the local player state, or `nil` before it is available from the server. | [game_cl_mp_script.cpp:116](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L116) |
| `game_cl_mp_script:GetObjectByGameID(id)` | Method | Returns the object by game id. Arguments `id` (`number`). Result `game_object`. | [game_cl_mp_script.cpp:113](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L113) |
| `game_cl_mp_script:GetPlayerByOrderID(index)` | Method | Returns the player record at the zero-based client list index. Keep the index below `GetPlayersCount()`. The engine does not check bounds. | [game_cl_mp_script.cpp:114](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L114) |
| `game_cl_mp_script:GetPlayersCount()` | Method | Returns the number of player records in the client match state. | [game_cl_mp_script.cpp:112](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L112) |
| `game_cl_mp_script:GetRoundTime()` | Method | In multiplayer builds, returns elapsed time since the current phase began as an `MM:SS` string. | [game_cl_mp_script.cpp:123](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L123) |
| `game_cl_mp_script:Init()` | Method | Initializes the client-side game mode during client startup. | [game_cl_mp_script.cpp:126](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L126) |
| `game_cl_mp_script:net_import_state(packet)` | Method | Reads the match state received from the server. | [game_cl_mp_script.cpp:129](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L129) |
| `game_cl_mp_script:OnKeyboardPress(key)` | Method | Handler for the keyboard press event. Arguments `key` (`number`). Result `boolean`. | [game_cl_mp_script.cpp:127](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L127) |
| `game_cl_mp_script:OnKeyboardRelease(dik)` | Method | Handler for the keyboard release event. Arguments `dik` (`number`). Result `boolean`. Implementation `if(!local_player \|\| local_player->IsSkip()) return true; else return false;`. | [game_cl_mp_script.cpp:128](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L128) |
| `game_cl_mp_script:shedule_Update(delta)` | Method | Handles a scheduled client update, receiving the elapsed time from the scheduler. | [game_cl_mp_script.cpp:130](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L130) |
| `game_cl_mp_script:TranslateGameMessage(msg, P)` | Method | Translates the game message. Arguments `msg` (`number`), `P` (`net_packet`). | [game_cl_mp_script.cpp:132](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_mp_script.cpp#L132) |

### `game_GameState`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_GameState.round` | Read-only field | Stores the round. Read-only. | [game_base_script.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L67) |
| `game_GameState.start_time` | Read-only field | Stores the start time. Read-only. | [game_base_script.cpp:68](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L68) |
| `game_GameState.type` | Read/write field | Stores the type. Readable and writable. | [game_base_script.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L66) |
| `game_GameState:Phase()` | Method | Returns the numeric match phase ID. | [game_base_script.cpp:71](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L71) |
| `game_GameState:Round()` | Method | Returns the round number. | [game_base_script.cpp:72](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L72) |
| `game_GameState:StartTime()` | Method | Returns the start time for the current phase. | [game_base_script.cpp:73](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L73) |
| `game_GameState:Type()` | Method | Returns the numeric game mode type. | [game_base_script.cpp:70](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L70) |

### `game_messages`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_messages.GAME_EVENT_ARTEFACT_DESTROYED` | Constant | Multiplayer event code for artefact destroyed. | [game_sv_base_script.cpp:90](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L90) |
| `game_messages.GAME_EVENT_ARTEFACT_DROPPED` | Constant | Multiplayer event code for artefact dropped. | [game_sv_base_script.cpp:92](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L92) |
| `game_messages.GAME_EVENT_ARTEFACT_ONBASE` | Constant | Multiplayer event code for artefact onbase. | [game_sv_base_script.cpp:93](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L93) |
| `game_messages.GAME_EVENT_ARTEFACT_SPAWNED` | Constant | Multiplayer event code for artefact spawned. | [game_sv_base_script.cpp:89](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L89) |
| `game_messages.GAME_EVENT_ARTEFACT_TAKEN` | Constant | Multiplayer event code for artefact taken. | [game_sv_base_script.cpp:91](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L91) |
| `game_messages.GAME_EVENT_BUY_MENU_CLOSED` | Constant | Multiplayer event code for buy menu closed. | [game_sv_base_script.cpp:96](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L96) |
| `game_messages.GAME_EVENT_PLAYER_BUY_FINISHED` | Constant | Multiplayer event code for player buy finished. | [game_sv_base_script.cpp:81](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L81) |
| `game_messages.GAME_EVENT_PLAYER_CHANGE_SKIN` | Constant | Multiplayer event code for player change skin. | [game_sv_base_script.cpp:82](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L82) |
| `game_messages.GAME_EVENT_PLAYER_CHANGE_TEAM` | Constant | Multiplayer event code for player change team. | [game_sv_base_script.cpp:79](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L79) |
| `game_messages.GAME_EVENT_PLAYER_CONNECTED` | Constant | Multiplayer event code for player connected. | [game_sv_base_script.cpp:83](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L83) |
| `game_messages.GAME_EVENT_PLAYER_DISCONNECTED` | Constant | Multiplayer event code for player disconnected. | [game_sv_base_script.cpp:84](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L84) |
| `game_messages.GAME_EVENT_PLAYER_ENTER_TEAM_BASE` | Constant | Multiplayer event code for player enter team base. | [game_sv_base_script.cpp:94](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L94) |
| `game_messages.GAME_EVENT_PLAYER_JOIN_TEAM` | Constant | Multiplayer event code for player join team. | [game_sv_base_script.cpp:86](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L86) |
| `game_messages.GAME_EVENT_PLAYER_KILL` | Constant | Multiplayer event code for player kill. | [game_sv_base_script.cpp:80](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L80) |
| `game_messages.GAME_EVENT_PLAYER_KILLED` | Constant | Multiplayer event code for player killed. | [game_sv_base_script.cpp:85](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L85) |
| `game_messages.GAME_EVENT_PLAYER_LEAVE_TEAM_BASE` | Constant | Multiplayer event code for player leave team base. | [game_sv_base_script.cpp:95](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L95) |
| `game_messages.GAME_EVENT_PLAYER_READY` | Constant | Multiplayer event code for player ready. | [game_sv_base_script.cpp:78](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L78) |
| `game_messages.GAME_EVENT_ROUND_END` | Constant | Multiplayer event code for round end. | [game_sv_base_script.cpp:88](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L88) |
| `game_messages.GAME_EVENT_ROUND_STARTED` | Constant | Multiplayer event code for round started. | [game_sv_base_script.cpp:87](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L87) |
| `game_messages.GAME_EVENT_SCRIPT_BEGINS_FROM` | Constant | Multiplayer event code for script begins from. | [game_sv_base_script.cpp:99](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L99) |
| `game_messages.GAME_EVENT_SKIN_MENU_CLOSED` | Constant | Multiplayer event code for skin menu closed. | [game_sv_base_script.cpp:98](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L98) |
| `game_messages.GAME_EVENT_TEAM_MENU_CLOSED` | Constant | Multiplayer event code for team menu closed. | [game_sv_base_script.cpp:97](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L97) |

### `game_phases`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_phases.GAME_PHASE_INPROGRESS` | Constant | Match phase code for inprogress. | [game_sv_base_script.cpp:67](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L67) |
| `game_phases.GAME_PHASE_NONE` | Constant | Match phase code for none. | [game_sv_base_script.cpp:66](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L66) |
| `game_phases.GAME_PHASE_PENDING` | Constant | Match phase code for pending. | [game_sv_base_script.cpp:68](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L68) |
| `game_phases.GAME_PHASE_SCRIPT_BEGINS_FROM` | Constant | Match phase code for script begins from. | [game_sv_base_script.cpp:72](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L72) |
| `game_phases.GAME_PHASE_TEAM1_SCORES` | Constant | Match phase code for team1 scores. | [game_sv_base_script.cpp:69](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L69) |
| `game_phases.GAME_PHASE_TEAM2_SCORES` | Constant | Match phase code for team2 scores. | [game_sv_base_script.cpp:70](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L70) |
| `game_phases.GAME_PHASE_TEAMS_IN_A_DRAW` | Constant | Match phase code for teams in a draw. | [game_sv_base_script.cpp:71](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L71) |

### `game_player_flags`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_player_flags.GAME_PLAYER_FLAG_LOCAL` | Constant | Player-state flag mask for local, used by `testFlag`, `setFlag`, and `resetFlag`. | [game_sv_base_script.cpp:56](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L56) |
| `game_player_flags.GAME_PLAYER_FLAG_READY` | Constant | Player-state flag mask for ready, used by `testFlag`, `setFlag`, and `resetFlag`. | [game_sv_base_script.cpp:57](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L57) |
| `game_player_flags.GAME_PLAYER_FLAG_SCRIPT_BEGINS_FROM` | Constant | Player-state flag mask for script begins from, used by `testFlag`, `setFlag`, and `resetFlag`. | [game_sv_base_script.cpp:60](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L60) |
| `game_player_flags.GAME_PLAYER_FLAG_SPECTATOR` | Constant | Player-state flag mask for spectator, used by `testFlag`, `setFlag`, and `resetFlag`. | [game_sv_base_script.cpp:59](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L59) |
| `game_player_flags.GAME_PLAYER_FLAG_VERY_VERY_DEAD` | Constant | Player-state flag mask for very very dead, used by `testFlag`, `setFlag`, and `resetFlag`. | [game_sv_base_script.cpp:58](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_base_script.cpp#L58) |

### `game_PlayerState`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_PlayerState.deaths` | Read/write field | Stores the deaths. Readable and writable. | [game_base_script.cpp:32](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L32) |
| `game_PlayerState.flags` | Read/write field | Stores the flags. Readable and writable. | [game_base_script.cpp:34](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L34) |
| `game_PlayerState.GameID` | Read/write field | Stores the game id. Readable and writable. | [game_base_script.cpp:36](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L36) |
| `game_PlayerState.kills` | Read/write field | Stores the rival kills. Readable and writable. | [game_base_script.cpp:31](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L31) |
| `game_PlayerState.LastBuyAcount` | Read/write field | Stores the last buy acount. Readable and writable. | [game_base_script.cpp:45](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L45) |
| `game_PlayerState.lasthitter` | Read/write field | Stores the lasthitter. Readable and writable. | [game_base_script.cpp:38](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L38) |
| `game_PlayerState.lasthitweapon` | Read/write field | Stores the lasthitweapon. Readable and writable. | [game_base_script.cpp:39](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L39) |
| `game_PlayerState.money_delta` | Read/write field | Stores the money delta. Readable and writable. | [game_base_script.cpp:42](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L42) |
| `game_PlayerState.money_for_round` | Read/write field | Stores the money for round. Readable and writable. | [game_base_script.cpp:33](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L33) |
| `game_PlayerState.ping` | Read/write field | Stores the ping. Readable and writable. | [game_base_script.cpp:35](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L35) |
| `game_PlayerState.pItemList` | Read/write field | Stores the p item list. Readable and writable. | [game_base_script.cpp:44](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L44) |
| `game_PlayerState.RespawnTime` | Read/write field | Stores the respawn time. Readable and writable. | [game_base_script.cpp:41](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L41) |
| `game_PlayerState.skin` | Read/write field | Stores the skin. Readable and writable. | [game_base_script.cpp:40](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L40) |
| `game_PlayerState.team` | Read/write field | Stores the team. Readable and writable. | [game_base_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L30) |
| `game_PlayerState:getName()` | Method | Returns the player’s display name. | [game_base_script.cpp:49](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L49) |
| `game_PlayerState:net_Export(packet, full)` | Method | Serializes a player record into the packet. Set `full` for the full record. | [game_base_script.cpp:52](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L52) |
| `game_PlayerState:net_Import(packet)` | Method | Loads a player record from the incoming packet. | [game_base_script.cpp:53](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L53) |
| `game_PlayerState:resetFlag(f)` | Method | Resets the flag. Arguments `f` (`number`). Implementation `flags__ &= ~(f);`. | [game_base_script.cpp:48](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L48) |
| `game_PlayerState:setFlag(f)` | Method | Sets the flag. Arguments `f` (`number`). Implementation `flags__ \|= f;`. | [game_base_script.cpp:47](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L47) |
| `game_PlayerState:testFlag(f)` | Method | Checks the player record for a flag mask from game_player_flags. Arguments `f` (`number`). Result `boolean`. Implementation `return !!(flags__ & f);`. | [game_base_script.cpp:46](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_base_script.cpp#L46) |

### `game_sv_Deathmatch`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_sv_Deathmatch:GetTeamData(team_id)` | Method | Returns the server Deathmatch team record for the given ID. | [game_sv_deathmatch_script.cpp:25](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_deathmatch_script.cpp#L25) |
| `game_sv_Deathmatch:type_name()` | Method | Returns the server game mode name. The base implementation uses `"deathmatch"`. | [game_sv_deathmatch_script.cpp:28](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_deathmatch_script.cpp#L28) |

### `game_sv_mp`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_sv_mp:KillPlayer(client_id, game_id)` | Method | Marks a player as killed on the server using the network client ID and game object ID. | [game_sv_mp_script.cpp:195](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L195) |
| `game_sv_mp:SendPlayerKilledMessage(KilledID, KillType, KillerID, WeaponID, SpecialKill)` | Method | Sends the player killed message. Arguments `KilledID` (`number`), `KillType` (`KILL_TYPE`), `KillerID` (`number`), `WeaponID` (`number`), `SpecialKill` (`SPECIAL_KILL_TYPE`). | [game_sv_mp_script.cpp:196](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L196) |
| `game_sv_mp:signal_Syncronize()` | Method | Flags the server state for forced synchronization. | [game_sv_mp_script.cpp:197](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L197) |

### `game_sv_mp_script`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `game_sv_mp_script:Create(options)` | Method | Initializes the server game mode from the game options string. | [game_sv_mp_script.cpp:221](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L221) |
| `game_sv_mp_script:createPlayerState(account_info)` | Method | Creates the player state. Arguments `account_info` (`net_packet`). Result `game_PlayerState`. Implementation `return inherited::createPlayerState(account_info);`. | [game_sv_mp_script.cpp:230](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L230) |
| `game_sv_mp_script:GetHitParamsImpulse(P)` | Method | Returns the hit params impulse. Arguments `P` (`net_packet`). Result `number`. | [game_sv_mp_script.cpp:216](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L216) |
| `game_sv_mp_script:GetHitParamsPower(P)` | Method | Returns the hit params power. Arguments `P` (`net_packet`). Result `number`. | [game_sv_mp_script.cpp:215](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L215) |
| `game_sv_mp_script:GetTeamData(Team)` | Method | Returns the team data. Arguments `Team` (`number`). Result `TeamStruct`. | [game_sv_mp_script.cpp:211](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L211) |
| `game_sv_mp_script:net_Export_State(packet, client_id)` | Method | Writes the server game state into a packet for the given client. | [game_sv_mp_script.cpp:229](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L229) |
| `game_sv_mp_script:OnEvent(P, type, time, sender)` | Method | Handler for the event event. Arguments `P` (`net_packet`), `type` (`number`), `time` (`number`), `sender` (`ClientID`). Implementation `inherited::OnEvent(P,type,time,sender);`. | [game_sv_mp_script.cpp:220](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L220) |
| `game_sv_mp_script:OnPlayerHitPlayer(id_hitter, id_hitted, P)` | Method | Handler for the player hit player event. Arguments `id_hitter` (`number`), `id_hitted` (`number`), `P` (`net_packet`). | [game_sv_mp_script.cpp:223](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L223) |
| `game_sv_mp_script:OnRoundEnd()` | Method | Handler for the round end event. | [game_sv_mp_script.cpp:227](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L227) |
| `game_sv_mp_script:OnRoundStart()` | Method | Handler for the round start event. | [game_sv_mp_script.cpp:225](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L225) |
| `game_sv_mp_script:SetHitParams(P, impulse, power)` | Method | Sets the hit params. Arguments `P` (`net_packet`), `impulse` (`number`), `power` (`number`). | [game_sv_mp_script.cpp:214](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L214) |
| `game_sv_mp_script:SpawnPlayer(id, N)` | Method | Spawns the player. Arguments `id` (`ClientID`), `N` (`string`). | [game_sv_mp_script.cpp:212](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L212) |
| `game_sv_mp_script:switch_Phase(phase)` | Method | Switches the server to the phase with the given numeric ID from `game_phases`. | [game_sv_mp_script.cpp:213](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L213) |
| `game_sv_mp_script:type_name()` | Method | Returns the server mode name as a string. A game mode script may override it. | [game_sv_mp_script.cpp:218](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_sv_mp_script.cpp#L218) |

### `RPoint`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `RPoint.A` | Read/write field | Stores the a. Readable and writable. | [game_cl_base_script.cpp:30](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L30) |
| `RPoint.P` | Read/write field | Stores the p. Readable and writable. | [game_cl_base_script.cpp:29](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L29) |

### `SZoneMapEntityData`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `SZoneMapEntityData.pos` | Read/write field | Stores the pos. Readable and writable. | [game_cl_base_script.cpp:14](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L14) |

### `ZoneMapEntities`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `ZoneMapEntities:push_back(entry)` | Method | Appends an `SZoneMapEntityData` entry to the map marker list. | [game_cl_base_script.cpp:18](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/xrGame/game_cl_base_script.cpp#L18) |

## See also

- [New Lua exports](../lua-exports.md)
