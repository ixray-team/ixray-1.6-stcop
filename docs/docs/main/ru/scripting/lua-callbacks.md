# Lua: Callbacks
## Binder Callbacks
> [!IMPORTANT]
> **Статус**: Поддерживается  <br>
> **Минимальная версия**: 1.1  <br>

### CGameObject
```lua
item_to_belt(obj) -- предмет переместился на пояс
item_to_slot(obj) -- предмет переместился в слот 
item_to_ruck(obj) -- предмет переместился в инвентарь 

on_foot_step(power, b_play, b_ground, b_hud_view) -- шаг по террейну

weapon_jammed(owner, weapon)
weapon_fired(owner, weapon, ammo_elapsed, ammo_type)
weapon_zoom_in(owner, weapon)
weapon_zoom_out(owner, weapon)

hit(owner, damage, direction, hitter, bone_id) -- получение хита
```

### CActor
```lua
weapon_magazine_empty(weapon, ammo_count)
hud_animation_end(hud_obj, hud_obj_section, current_motion, state, anim_slot)
actor_before_death(killer_id)
```

## Static Callbacks
> [!IMPORTANT]
> **Статус**: Поддерживается  <br>
> **Минимальная версия**: 1.3  <br>

> Статические колбеки - это функции, которые не привязаны к игровым объектам, а являются глобальными.

Для их активации перейдите в файл `game_global.ltx` и раскомментируйте нужные вам

### IX-Ray 
* OnSkipKillActor
```lua
--// see xr_effects.enable_ui
```
* OnStartAttack
```lua
--// see sim_combat.start_attack in "Clear Sky"
```

* OnInvBoxCanTakeItem
```lua
function CInventoryBox_OnInvBoxCanTakeItem(inv_box_game_object, item_game_object)
	return true
end
```

* OnInvBoxCanPlaceItem
```lua
function CInventoryBox_OnInvBoxCanPlaceItem(inv_box_game_object, item_game_object)
	return true
end
```

### Call of Chernobyl
* OnCanTake
```lua
function CInventoryBox_CanTake(inv_box, item)
	return true
end
```
* OnBeforeHit
```lua
-- Called before actor hit callback
-- returning false will ignore the hit completely
function CActor__BeforeHitCallback(actor,shit,bone_id)
	return true
end
```
* OnUnregister
```lua
-- called in CSE_ALifeDynamicObject::on_unregister()
-- good place to remove ids from persistent tables
function CSE_ALifeDynamicObject_on_unregister(id)
end
```
* OnInventoryEat
```lua
-- called when an inventory item is eaten/used
-- returning false will prevent the item from being used
function CInventory__eat(npc,item)
	return true
end
```
* OnBeforeChangeLevel
```lua
function CALifeUpdateManager__on_before_change_level(packet)
	return true
end
```

* OnItemDropped
```lua
--[[
 Refers to when an icon is dragged onto another icon in Actor Inventory Menu
 
 passed parameters:
	itm1 is the object being dragged; may be nil
	itm2 is the object the item is being dragged onto; may be nil
	from_slot is the number value of the slot itm1 is being dragged from;  0 or EDDListType.iInvalid is invalid 
	to_slot is the number value of the slot itm2 is occupying;  0 or EDDListType.iInvalid is invalid 
	
from_slot and to_slot values:
	EDDListType.iInvalid			= 0
	EDDListType.iActorSlot			= 1
	EDDListType.iActorBag			= 2
	EDDListType.iActorBelt			= 3

	EDDListType.iActorTrade			= 4
	EDDListType.iPartnerTradeBag	= 5
	EDDListType.iPartnerTrade		= 6
	EDDListType.iDeadBodyBag		= 7
	EDDListType.iQuickSlot			= 8
	EDDListType.iTrashSlot			= 9
--]]
function CUIActorMenu_OnItemDropped(itm1,itm2,from_slot,to_slot)
	return true
end
```
* OnDonateCurrentItem
```lua
function CUIActorMenu_DonateCurrentItem(parent, itm)
end
```
* OnItemFocusReceive
```lua
function CUIActorMenu_OnItemFocusReceive(itm)
	return true
end
```
* OnItemFocusLost
```lua
function CUIActorMenu_OnItemFocusLost(itm)
	return true
end
```
* OnCanMoveToPartner
```lua
function CUIActorMenu_CanMoveToPartner(parent, itm, r1, r2, item_weight, partner_inv_weight, partner_max_weight)
	return true
end
```
* OnItemAvailableToTrade
```lua
-- useful for when doing npc:use(db.actor) when NPC is alive
-- basically what is available in the corpse loot menu while npc is alive
function CInventory_ItemAvailableToTrade(npc,item)
	return true
end
```
* OnPropertyBoxClicked
```lua
function property_box_clicked(property_ui)
end
```
* OnPropertyBoxAddProperties
```lua
function property_box_add_properties(property_ui,id,level_name,hint)
end
```
* OnSetActiveSubdialog
```lua
-- You can use ActorMenu.get_pda_menu():GetActiveSection() to find out active pda tab
-- UI returned must be CUIScriptWnd
function set_active_subdialog(section)
-- example (or nil)
	return ui_pda_relations_tab.get_ui()
end
```
* OnGetRankingsArraySize
```lua
-- It's how many character rankings to display! u8 (max 255)
function get_rankings_array_size()
	return coc_ranking_array_size
end	
```
* OnKeyPress
```lua
function on_key_press(dik,bind)
	return false
end
```
* OnGetVisibleValue
```lua
-- This occurs during the visible check. If value >= visiblity_threshold then object is considered visible
-- warning npc and who can be nil sometimes
function get_visible_value(npc,who,time_delta,time_quant,luminocity,velocity_factor,velocity,distance,object_distance,always_visible_distance)
	distance = distance <= 0 and 0.00001 or distance
	luminocity = luminocity <= 0 and 0.0001 or luminocity
	
	if (level_weathers.bLevelUnderground) then 
		luminocity = luminocity + 0.35
	end
	
	-- unaltered formula
	-- time_delta / time_quant * luminocity * (1 + velocity_factor*velocity) * (distance - object_distance) / (distance - always_visible_distance)
	
	return  time_delta / time_quant * luminocity * (1 + velocity_factor*velocity) * (distance - object_distance) / distance
end
```
* OnUpdateBestWeapon
```lua
-- if return game_object then it ignores engine. If return nil, then engine tries to find item to kill
function update_best_weapon(npc,cur_wpn)
	return nil
end
```
### Improved Weapon Pack
* OnZoneTouch
```lua
function CZone_Touch(zone_game_object)
	-- if true: play sound
	return false
end
```
### Gunslinger
* OnCanDisassembleItem
```lua
function gunsl_can_disassemble_item(section, condition, partner_profile_name)
	return false
end
```
* OnQuestionDisassembleItem
```lua
function gunsl_question_disassemble_item(section, condition, can_disassemble, partner_profile_name)
	return "some text"
end
```
* OnEffectDisassemble
```lua
function gunsl_effect_disassemble(section, condition, partner_profile_name)
end
```
