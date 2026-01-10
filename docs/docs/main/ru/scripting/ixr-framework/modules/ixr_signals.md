
# IXR Framework (LUA Фреймворк)
> [!IMPORTANT]
> **Статус**: Поддерживается<br>
> **Минимальная версия**: 1.4.0

## Модуль сигналов IXR SIGNALS
  Система централизованной рассылки игровых событий в реальном времени, позволяет значительно упросить опыт разработки гемплейных фитч в модификациий структурируя хаос в одном компактном лаконичном решении.
  * Имеет переопределяемые опции интерцептеров для аддон системы
  * Контролирует подключения событий по списку разрешенных интерцепторов
  * Поддерживает редактирование интерцепторов в реальном времени
  * Подписка на события 
  * Рассылка событий
  * Отписка от событий
  * Доступна подписка на замыкания как функторы
  * Возможность легко имплементировать собственные события для своего проекта
  * Синергирует с модулем автозагрузки позволяя подключать события для скриптов безпроводным способом (достаточно просто положить файл скрипта в папку игры и все).
    
 
Внешние перезаписываемые настройки хранятся в файле __ixr_override_signals_intercepts.script совместимо с системой аддонов для переопределения перехватчиков событий
```lua
--// пример конфигурирования перехватчика события для файла __ixr_override_signals_intercepts.script
function configure(_ref_ixr_signals)
  --// пример регистрации перехватчика событий
  _ref_ixr_signals.add_intercept("on_item_take", {"item_game_object", "is_actor_exists_bool"}) -- actor
  ...
end

--// сигнатура метода регистрации перехватчика _ref_ixr_signals.add_intercept
args: 
  event_name (string)(required) - название события
  args_table (table)(required) - перечень аргументов в удобочитаемом формате (используется как документация)
retval: (void)
```


События для которых не установлен перехватчик по названию будут игнорироваться это сделано специально чтобы механизм перехватчиков служил в качестве механизма отключения обработки ненужных событий в системе

При вызове события по имени методом SendScriptCallback происходит широковещательная рассылка этого действия всем подписчикам этого события которые установили обратный вызов методом RegisterScriptCallback 

 ```lua
 --// Добавить перехватчик коллбэка
AddIntercept(name, args_map)
args:
  name (string)(required) - имя коллбэка для перехвата.
  args_map (table)(required) - таблица преобразования аргументов.
retval: (bool) - успешность добавления перехватчика.

--// Удалить перехватчик коллбэка
RemoveIntercept(name)
args:
  name (string)(required) - имя коллбэка.
retval: (bool) - успешность удаления перехватчика.

--// Зарегистрировать скриптовый коллбэк
RegisterScriptCallback(name, func_or_userdata)
args:
  name (string)(required) - имя коллбэка.
  func_or_userdata (function|userdata)(required) - функция или объект для вызова.
retval: (bool) - успешность регистрации.

--// Отменить регистрацию скриптового коллбэка
UnregisterScriptCallback(name, func_or_userdata)
args:
  name (string)(required) - имя коллбэка.
  func_or_userdata (function|userdata)(required) - функция или объект для удаления.
retval: (bool) - успешность отмены регистрации.

--// Вызвать скриптовый коллбэк
SendScriptCallback(name, ...)
args:
  name (string)(required) - имя коллбэка.
  ... (variadic list) - аргументы для передачи в коллбэк.
retval: (mixed) - результат выполнения коллбэка.
 ```

Примеры:
```lua
 --// Добавить перехватчик коллбэка
AddIntercept("my-event-name", {"game_object"})

--// Удалить перехватчик коллбэка
RemoveIntercept("my-event-name")

--// Зарегистрировать скриптовый коллбэк
RegisterScriptCallback("my-event-name", test_event)

--// Отменить регистрацию скриптового коллбэка
UnregisterScriptCallback("my-event-name", test_event)

--// Вызвать скриптовый коллбэк с произвольными аргументами
SendScriptCallback("my-event-name", ...)

--// Пример имплементации совместно с автозагрузчиком
function on_game_start()
  --// подписываемся на событие и устанавливаем функцию замыкание 1-й способ напрямую прописать код метода в замыкании
  RegisterScriptCallback("my-event-name",
    function (obj)
      ...
    end
   )

  --// подписываемся на событие и устанавливаем функцию замыкание 2-й способ указать метод по ссылке
  RegisterScriptCallback("my-event-name", this.test_event)
end

function test_event(obj)
  --// будет вызван когда в системе произойдет вызов SendScriptCallback("my-event-name", ...)
end

```


Перечень событий с аргументами
```ini
("level_input_on_key_press",				{})	
("on_key_press",                         	{"dik_key_number"})	
("on_key_release",                     		{"dik_key_number"})	
("on_key_hold",                          	{"dik_key_number"})	
("on_mouse_move",                        	{"dx_number", "dy_number"})	
("on_mouse_wheel",                      	{"direction_number"})	 
("on_actor_wnd_mode_switch",          		{"wnd_mode_number"}) -- 0 (Undefined) | 1 (Inventory) | 2 (Trade) | 3 (Upgrade) | 4 (DeadBodySearch) | 10 (Talk dialog  show) | 11 (Talk dialog  hide)
("on_actor_wnd_closed",          			{"wnd_mode_number"})
("on_actor_wnd_opened",          			{"wnd_mode_number"})	
("on_actor_inventory_wnd_opened",		{"wnd_mode_number"})
("on_actor_inventory_wnd_closed",     	{"wnd_mode_number"})
("on_actor_trade_wnd_opened",   		{"wnd_mode_number"})
("on_actor_trade_wnd_closed",    		{"wnd_mode_number"})
("on_actor_upgrade_wnd_opened",  		{"wnd_mode_number"})
("on_actor_upgrade_wnd_closed",   		{"wnd_mode_number"})
("on_actor_dead_body_search_wnd_opened", {"wnd_mode_number"})	
("on_actor_dead_body_search_wnd_closed", {"wnd_mode_number"})
("on_actor_dialog_wnd_showed",    		{"wnd_mode_number", "npc_id_number_or_nil"})	
("on_actor_dialog_wnd_closed",  		{"wnd_mode_number", "npc_id_number_or_nil"})	
("on_before_level_changing",       		{"net_packet"}) -- engine
("on_level_changing",              		{}) -- actor binder
("on_item_take",                   		{"item_game_object", "is_actor_exists_bool"}) -- actor
("on_item_drop",             			{"item_game_object", "is_actor_exists_bool"}) -- actor
("actor_on_spawn",                   	{})	
("actor_on_init",                   	{"binder_ref"}) 
("actor_on_reinit",                 	{"binder_ref"}) 
("actor_on_save",                   	{"net_packet"}) 
("actor_on_load",                   	{"net_packet"}) 
("actor_on_level_changing",             {}) 
("actor_on_before_death",          		{"who_id_number", "flags_table"}) -- { ret_value = true } switch to false for ignore death
("actor_on_death",          			{"victim_game_object", "who_game_object"})  
("actor_on_net_destroy",           		{"binder_ref"}) 
("actor_on_first_update",          		{"binder_ref", "delta_number"}) 
("actor_on_update",                 	{"binder_ref", "delta_number"}) 
("actor_on_weapon_fired",          		{"npc_game_object", "wpn_game_object", "iAmmoElapsed_bool", "ammo_type_number"}) 
("actor_on_weapon_jammed",         		{"wpn_game_object"}) 
("actor_on_weapon_no_ammo",        		{"wpn_game_object", "ammo_total_number"}) 
("actor_on_weapon_no_avialable_ammo",	{"wpn_game_object", "suitable_ammo_total_number"}) -- eWeaponNoAmmoAvailable
("actor_on_weapon_reload",         		{"wpn_game_object", "suitable_ammo_total_number"}) -- eWeaponNoAmmoAvailable копия вызова actor_on_weapon_no_avialable_ammo отсутствие патронов не означает что это перезарядка
("actor_on_weapon_zoom_in",        		{"wpn_game_object"}) 
("actor_on_weapon_zoom_out",       		{"wpn_game_object"}) 
("actor_on_item_take",             		{"item_game_object", "before_transfer_parent_id_number", "is_take_from_ground_bool"}) 
("actor_on_item_take_from_box",    		{"box_game_object", "item_game_object"}) 
("actor_on_item_take_from_ground", 		{"item_game_object"}) 
("actor_on_item_drop",             		{"item_game_object"}) 
("on_before_item_use",                  {"npc_game_object", "item_game_object", "table"})  -- { ret_value = true }
("actor_on_item_before_use",       		{"npc_game_object", "item_game_object", "table"}) --  { ret_value = true }
("actor_on_item_use",              		{"item_game_object", "item_section_string"}) 
("actor_item_to_belt",             		{"item_game_object"}) 
("actor_item_to_ruck",             		{"item_game_object"}) 
("actor_item_to_slot",             		{"item_game_object"}) 
("actor_on_trade",                 		{"item_game_object", "sell_bye_bool", "money_number"}) 
("actor_on_info_callback",         		{"npc_game_object", "info_id_number"})  
("actor_on_before_hit",             	{"sHit_struct", "bone_id_number", "flags_table"}) -- { ret_value = true }  switch to false for ignore hit
("actor_on_hit_callback",          		{"game_object", "number", "vector", "game_object", "number"})
("actor_on_torch_enabled",         		{"nil_or_device_torch_game_object"}) 
("actor_on_torch_disabled",        		{"nil_or_device_torch_game_object"}) 
("actor_on_attach_vehicle",        		{"vechicle_game_object"}) 
("actor_on_detach_vehicle",        		{"vechicle_game_object"}) 
("actor_on_use_vehicle",           		{"vechicle_game_object"}) 
("actor_on_hud_animation_end",     		{"item_game_object", "hud_section_string", "m_current_motion_string", "state_number", "animation_slot_number"}) -- не полностью соответствует варианту из anomaly-coc ("game_object", "string", "bool", "bool", "number")
("actor_on_before_sleep",               {"hours_number"}) 
("actor_on_sleep",                  	{"hours_number"}) 
("actor_on_interaction",            	{"string", "game_object", "string"}) 
("actor_on_leave_dialog",           	{"npc_id_number"}) 
("actor_on_foot_step",              	{"alife_game_object", "power_number", "play_bool", "on_ground_bool", "hud_view_bool"}) 
("actor_on_footstep",                	{"alife_game_object", "power_number", "play_bool", "on_ground_bool", "hud_view_bool"})
("npc_on_use",                       	{"target_game_object", "initiator_game_object"}) 
("npc_on_item_take",                 	{"npc_game_object", "item_game_object"}) 
("npc_on_item_take_from_box",        	{"npc_game_object", "box_game_object", "item_game_object"}) 
("npc_on_item_drop",                  	{"npc_game_object", "item_game_object"}) 
("npc_on_net_spawn",                  	{"npc_game_object", "npc_server_object"}) 
("npc_on_net_destroy",                	{"npc_game_object"}) 
("npc_on_update",                     	{"npc_game_object", "db_storage_table"}) 
("npc_on_hit_callback",               	{"npc_game_object", "power_number", "hit_direction_vector", "who_game_object", "bone_index_number"}) 
("npc_on_death_callback",             	{"npc_game_object", "who_game_object"}) 
("npc_on_fighting_actor",             	{"npc_game_object"}) 
("npc_on_weapon_strapped",            	{"npc_game_object", "wpn_game_object"}) 
("npc_on_weapon_unstrapped",          	{"npc_game_object", "wpn_game_object"})
("npc_on_weapon_drop",                 	{"npc_game_object", "wpn_game_object"}) 
("npc_on_hear_callback",              	{"npc_game_object", "who_npc_id_number", "sound_type_number_or_string", "sound_distance_number", "sound_power_number", "sound_position_vector", "raw_sound_type_number"}) 
("npc_on_get_all_from_corpse",        	{"npc_game_object", "corpse_game_object", "item_game_object", "is_in_loot_table_bool"}) 
("npc_on_eval_danger",                 	{"npc_game_object", "flags_table"}) -- {ret_value = true} switch to false for npc ignore danger
("monster_on_update",                 	{"monster_game_object", "self_st_table"}) 
("monster_on_hit_callback",           	{"monster_game_object", "power_number", "hit_direction_vector", "who_game_object", "bone_index_number"}) 
("monster_on_net_spawn",             	{"monster_game_object", "monster_server_object"}) 
("monster_on_net_destroy",            	{"monster_game_object"}) 
("monster_on_death_callback",         	{"monster_game_object", "who_game_object"}) -- without poltergeist
("monster_on_death",         			{"monster_game_object", "who_game_object"}) 
("monster_on_actor_use_callback",     	{"monster_game_object", "who_game_object"}) 
("monster_on_use",     					{"monster_game_object", "who_game_object", "is_alive_bool"}) 
("physic_object_on_death",     			{"game_object", "game_object"}) 
("physic_object_on_use",     			{"physic_game_object", "who_game_object"}) 
("physic_object_on_use_callback",     	{"physic_game_object", "who_game_object"}) 
("physic_object_on_hit",     			{"physic_game_object", "power_number", "hit_direction_vector", "who_game_object", "bone_index_number"}) 
("physic_object_on_hit_callback",     	{"physic_game_object", "power_number", "hit_direction_vector", "who_game_object", "bone_index_number"}) 
("heli_on_hit_callback",              	{"heli_game_object", "power_number", "nil", "who_game_object", "nil"}) 
("squad_on_npc_creation",             	{"squad_server_object", "npc_server_object", "spawn_smart_server_object"}) 
("squad_on_enter_smart",              	{"squad_server_object", "smart_server_object"}) 
("squad_on_leave_smart",              	{"squad_server_object", "old_smart_server_object"}) 
("squad_on_npc_death",                	{"squad_server_object", "npc_server_object", "killer_server_object"})
("squad_on_update",                    	{"squad_server_object"}) 
("squad_on_first_update",              	{"squad_server_object"}) 
("squad_on_add_npc",                   	{"squad_server_object", "npc_server_object", "spawn_section_string", "spawn_position_vector", "lv_id_number", "gv_id_number"})
("squad_on_register",                  	{"squad_server_object"}) 
("squad_on_unregister",                	{"squad_server_object"}) 
("squad_on_after_game_vertex_change",  	{"squad_server_object", "last_gvid_number", "m_game_vertex_id_number", "is_level_changed_bool"}) 
("squad_on_after_level_change",        	{"squad_server_object", "level_name_string", "cur_lvl_string"}) 
("smart_terrain_on_update",            	{"smart_terrain_server_object"}) 
("on_try_respawn",                      {"smart_terrain_server_object", "table"}) -- {disabled = false} switch to true for break
("server_entity_on_register",          	{"server_object", "string"}) -- se_actor, se_artefact, se_car, se_heli, se_outfit, se_helmet, se_weapon, se_weapon_shotgun, se_weapon_automatic_shotgun, se_weapon_magazined, se_weapon_magazined_w_gl, se_item, se_item_torch, se_physic, se_lamp, se_ammo, se_grenade, se_eatable, se_invbox, se_explosive, se_pda, se_detector, se_mgun, se_level_changer, se_monster, se_smart_cover, se_stalker, se_trader, se_zone_anom, se_zone_torrid, se_zone_visual, se_restrictor, sim_squad_scripted, se_smart_terrain
("server_entity_on_unregister",         {"server_object", "string"}) -- se_actor", se_artefact, se_car, se_heli, se_outfit, se_helmet, se_weapon, se_weapon_shotgun, se_weapon_automatic_shotgun, se_weapon_magazined, se_weapon_magazined_w_gl, se_item, se_item_torch, se_physic, se_lamp, se_ammo, se_grenade, se_eatable, se_invbox, se_explosive, se_pda, se_detector, se_mgun, se_level_changer, se_monster, se_smart_cover, se_stalker, se_trader, se_zone_anom , se_zone_torrid, se_zone_visual, se_restrictor, sim_squad_scripted, se_smart_terrain                   
("fill_start_position",                 {}) 
("se_stalker_on_spawn",                 {"server_object"}) 
("se_actor_on_register",                {"server_object"}) 
("se_actor_on_unregister",              {"server_object"}) 
("se_actor_on_STATE_Write",             {"server_object", "net_packet"}) 
("se_actor_on_STATE_Read",              {"server_object", "net_packet", "size_number"}) 
("map_spot_menu_add_property",          {"property_CUIWindow", "id_number", "level_name_string", "hint_string"}) -- возможно есть дуликат вызова
("map_spot_menu_property_clicked",      {"property_CUIWindow", "map_spot_property_id_number", "map_spot_property_level_name_string", "prop_string"}) -- возможно есть дуликат вызова
("main_menu_on_keyboard",               {"dik_number", "keyboard_action_number", "CUIScriptWnd", "is_level_present_bool"}) 
("main_menu_on_init",                   {"CUIScriptWnd"}) -- 1
("main_menu_on_init_controlls",    		{"CUIScriptWnd"}) -- 2
("main_menu_on_init_callbacks",         {"CUIScriptWnd"}) -- 3
("main_menu_on_quit",                   {"CUIScriptWnd"}) -- 4
("on_game_load",                         	 {"binder"}) -- actor net_spawn
("on_console_execute",                    	{"string", "string", "string", "..."})
("on_before_save_input",                	{"number", "number", "table"})
("on_before_load_input",                 	{"number", "number", "table"})
("save_state",                           	{"table"}) 
("load_state",                            	{"table"}) 
("on_pstor_save_all",                     	{"obj_game_object", "net_packet"}) -- is USE_MARSHAL
("on_pstor_load_all",                    	{"obj_game_object", "reader"}) -- is USE_MARSHAL
("on_enemy_eval",                         	{"game_object", "game_object", "table"})
("on_before_surge",                       	{"table"}) -- {allow = true} switch to false to scip surge
("on_before_psi_storm",                   	{"table"}) -- {allow = true} switch to false to scip surge
("opt_menu_on_init",                 		{"CUIScriptWnd"}) 
("opt_menu_on_accept",             			{"CUIScriptWnd", "opt", "console"}) 
("opt_menu_on_set_values",					{"CUIScriptWnd", "opt"})  
("CUIActorMenu_OnItemDropped",           	{"first_item_game_object", "second_item_game_object", "from_slot_number", "to_slot_number"}) -- replicated ActorMenu_on_item_drag_drop
("ActorMenu_on_item_drag_drop",           	{"first_item_game_object", "second_item_game_object", "from_slot_number", "to_slot_number"})	
("actor_on_nil_drag_drop_item_on_item", 	{"first_item_game_object", "second_item_game_object", "from_slot_number", "to_slot_number"})	
("actor_on_npc_drag_drop_item_on_item", 	{"first_item_game_object", "second_item_game_object", "from_slot_number", "to_slot_number"})	
("actor_on_actor_drag_drop_item_on_item",	{"first_item_game_object", "second_item_game_object", "from_slot_number", "to_slot_number"})	
("on_pda_set_active_subdialog",     		{"ref_pda_script", "section_string", "ui_table"}) -- {ui=nil} is need draw new tab ui set it on variavle ui {ui = ui_pda_encyclopedia_tab.get_ui()}
("ActorMenu_on_item_focus_receive",     	{"item_game_object"}) 
("CUIActorMenu_OnItemFocusReceive",      	{"item_game_object"}) 
("ActorMenu_on_item_focus_lost",        	{"item_game_object"}) 
("CUIActorMenu_OnItemFocusLost",         	{"item_game_object"}) 
("CInventory_ItemAvailableToTrade",     	{"npc_game_object", "item_game_object", "flags_table"}) -- { ret_value = true }
("CInventoryBox_CanTake",     				{"box_game_object", "item_game_object", "flags_table"}) -- { ret_value = true } -- !!! not use !!! bugs in ingine - not all events processed
("CUIActorMenu_CanMoveToPartner",     		{"npc_gobj", "item_gobj", "itmWeight", "partner_inv_weight", "partner_max_weight", "flags"}) -- { ret_value = true } -- диолог с нпс
("CInventoryBox_OnInvBoxCanTakeItem",     	{"box_game_object", "item_game_object", "flags_table"}) -- { ret_value = true }
("CInventoryBox_OnInvBoxCanPlaceItem",    	{"box_game_object", "item_game_object", "flags_table"}) -- { ret_value = true }
("main_menu_update", 						{"main_menu_update"})
("on_ixr_autoloader_initialized", 			{"ref_ixr_autoloader"}) -- IXR AUTOLOADER
("on_init_ixr_options", 					{"ref_ixr_options"}) -- IXR OPTIONS
("on_item_draw_description", 				{"item_game_object", "item_game_object_id", "item_game_object_section", "extend_description_table"}) -- IXR Extend description
```
