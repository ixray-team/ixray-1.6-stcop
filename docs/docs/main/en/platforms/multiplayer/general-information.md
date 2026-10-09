# Multiplayer

::: important Support
**Status:** WIP · **Minimum version:** 2.0
:::

## ALife

Optional ALife support is available for multiplayer modes. To enable it, place `level.spawn` in the multiplayer level folder.

## Dedicated Server

The dedicated server is provided as a separate executable, `xrServer.exe`. See [Dedicated Server](./dedicated-server.md) for details.

## FreeMP

FreeMP is a multiplayer mode that offers free play similar to the single-player game.

To register a level for FreeMP, add `freemp` to the `[map_usage]` section of `level.ltx`:

```ini [level.ltx]
[map_usage]
ver=1.0
freemp
```

### Network events

`mp_events.script` is one of the core OMP scripts. It sends and processes network packets between the client and server to synchronize objects and states.

**IX-Ray compatibility:** use `script_events.M_SCRIPT_EVENT` for the packet type. The original OMP uses a hardcoded value that is incompatible with IX-Ray.

```lua [mp_events.script]
local M_SCRIPT_CUSTOM_EVENT = script_events.M_SCRIPT_EVENT

local EVENTS =
{
  INIT_EVENT = 1, -- for single actor
  PLAYER_INIT_EVENT = 2,
  DOOR_USE = 3,
  DOOR_CHANGE_SECTION = 4,
  TRADE_CONFIG = 5,
  NPC_SOUND = 6
}

function gen_event(type)
  local P = net_packet()
  P:w_begin(M_SCRIPT_CUSTOM_EVENT)
  P:w_u8(EVENTS[type])
  return P
end

function send_to_server(P)
  script_events.send_to_server(P)
end

function send_to_client(clientId, P)
  script_events.send_to_client(clientId, P)
end

function send_broadcast(P)
  script_events.send_broadcast(P)
end

function cl_send_request_init_event()
  send_to_server(gen_event('INIT_EVENT'))
end

function cl_send_request_player_init_event()
  send_to_server(gen_event('PLAYER_INIT_EVENT'))
end

function process_client_events()
  while script_events.get_size_client_events() > 0 do
    local P = script_events.get_last_client_event()
    local type = P:r_u8()

    if type == EVENTS.INIT_EVENT then
      -- init event for single actor object

    elseif type == EVENTS.PLAYER_INIT_EVENT then
      mp_doors_sync.cl_process_all_door_sections(P)
      mp_trade_sync.cl_process_all_trade_configs(P)

    elseif type == EVENTS.DOOR_CHANGE_SECTION then
      mp_doors_sync.cl_process_door_section(P)

    elseif type == EVENTS.TRADE_CONFIG then
      mp_trade_sync.cl_process_trade_config(P)

    elseif type == EVENTS.NPC_SOUND then
      sound_theme.cl_process_npc_sound(P)
    end

    script_events.pop_last_client_event()
  end
end

function process_server_events()
  while script_events.get_size_server_events() > 0 do
    local e = script_events.get_last_server_event()
    local P = e.Packet
    local type = P:r_u8()

    if type == EVENTS.INIT_EVENT then
      -- init event for single actor object
      local packet = gen_event('INIT_EVENT')
      send_to_client(e.SenderID, packet)

    elseif type == EVENTS.PLAYER_INIT_EVENT then
      local packet = gen_event('PLAYER_INIT_EVENT')
      mp_doors_sync.sv_write_all_door_sections(packet)
      mp_trade_sync.sv_write_all_trade_configs(packet)
      send_to_client(e.SenderID, packet)

    elseif type == EVENTS.DOOR_USE then
      mp_doors_sync.sv_process_use_door_event(e.SenderID, P)
    end

    script_events.pop_last_server_event()
  end
end
```

### Starting inventory

The `mp\fmp_respawn_items.ltx` config defines the items a player receives when spawning on the level.

```ini [mp\fmp_respawn_items.ltx]
[spawn]
wpn_val = 1
ammo_9x39_ap = 1
wpn_pb
ammo_9x18_pmm = 1
grenade_f1 = 3
wpn_binoc = 1
medkit = 1
bandage = 2
device_pda = 1
mp_players_rukzak = 1
```

### Test level

An addon with a test level from [OMP](https://github.com/xray-omp) is available:

1. Download [omp_level.db](https://github.com/ixray-community/ixray-addons/blob/default/omp_level.db).
2. Place the archive in `$fs_root$/ixr_addons/`.
