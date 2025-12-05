# Мультиплеер
## Общее сведения
> [!IMPORTANT]  
> **Статус**: WIP <br>
> **Минимальная версия**: 2.0
## Alife
Была добавлена опциональная поддержка alife для мультиплеерных режимов. Для её активации достаточно поместить `level.spawn` в папку с mp локацией

### Dedicated Server
**Dedicated Server** был вынесен в отдельный исполняемый файл, под названием `xrServer.exe`. 

## FreeMP
Новый мультиплеерный режим, представляющий свободный вариант игры, приближённый к одиночному. 
Для регистрации данной локацией в FreeMP нужно прописать в `level.ltx` в секцию `map_usage` строку `freemp`. Пример: 
```ini
[map_usage]
ver=1.0
freemp
```

### mp_events.script
> **mp_events** - один из основных скриптов для работы OMP. Он реализует базовый механизм отправки и обработки сетевых пакетов между клиентом и сервером для синхронизации объектов и состояний.

```lua
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

### mp\\fmp_respawn_items.ltx
> Конфиг, описывающий базовые вещи при спавне игрока на локации. 
```ini
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

### Пример
Аддон с тестовой локацией из проекта [OMP](https://github.com/xray-omp):
https://github.com/ixray-community/ixray-addons/blob/default/omp_level.db

Поместить в папку `$fs_root$/ixr_addons/`
