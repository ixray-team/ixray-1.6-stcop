# Мультиплеер

::: important Поддержка
**Статус:** WIP · **Минимальная версия:** 2.0
:::

## ALife

В мультиплеерных режимах доступна опциональная поддержка ALife. Для её активации поместите `level.spawn` в папку мультиплеерной локации.

## Dedicated Server

Выделенный сервер вынесен в отдельный исполняемый файл `xrServer.exe`.

Подробнее — в разделе [Dedicated Server](./dedicated-server.md).

## FreeMP

FreeMP — мультиплеерный режим свободной игры, приближённый к одиночному.

Чтобы зарегистрировать локацию в FreeMP, добавьте строку `freemp` в секцию `[map_usage]` файла `level.ltx`:

```ini [level.ltx]
[map_usage]
ver=1.0
freemp
```

### Сетевые события

`mp_events` — один из основных скриптов OMP. Он отправляет и обрабатывает сетевые пакеты между клиентом и сервером для синхронизации объектов и состояний.

**Совместимость с IX-Ray:** используйте `script_events.M_SCRIPT_EVENT`. Хардкодное значение из оригинального OMP несовместимо с IX-Ray.

```lua [mp_events.script]
local M_SCRIPT_CUSTOM_EVENT = script_events.M_SCRIPT_EVENT --// Важный момент для совместимости с IXR.
                            --// Оригинальный OMP использует хардкодное значение, которое не совместимо с IX-Ray.

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

### Стартовый набор игрока

Файл `mp\fmp_respawn_items.ltx` определяет предметы, которые игрок получает при появлении на локации.

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

### Тестовая локация

Для проверки FreeMP доступен аддон с тестовой локацией из проекта [OMP](https://github.com/xray-omp).

1. Скачайте [omp_level.db](https://github.com/ixray-community/ixray-addons/blob/default/omp_level.db).
2. Поместите файл в папку `$fs_root$/ixr_addons/`.
