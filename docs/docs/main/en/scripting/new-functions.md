# New Functions
## Global Namespace

```lua
--// Script is executed on a dedicated server
IsDedicated()
retval: boolean

--// Script is executed on the client side
OnClient()
retval: boolean

--// Script is executed on the server side
OnServer()
retval: boolean

--// Get cursor position
GetCursorPosition(cursor_pos)
retval: vector2
args: cursor_pos (vector2)

--// Set cursor position
SetCursorPosition(pos)
retval: none
args: pos (vector2)
```

## player_hud

```lua
--// Show/hide legs
player_hud.show_legs(value)
retval: none
args: value (boolean)
```

## CActor

### New

* Actor Status
```lua
--// Is the actor in god mode
db.actor:is_god_mode()
retval: boolean

--// Enable/disable god mode
db.actor:SetInvulnerable(value)
retval: none
args: value (boolean)

--// Check if actor is currently jumping
db.actor:ActorIsJump()
retval: boolean

--// Get max weight the actor can carry
db.actor:GetActorMaxWeight()
retval: number

--// Set max weight the actor can carry
db.actor:SetActorMaxWeight(max_weight)
retval: none
args: max_weight (number)

--// Get max weight at which the actor can walk
db.actor:GetActorMaxWalkWeight()
retval: number

--// Set max weight at which the actor can walk
db.actor:SetActorMaxWalkWeight(max_walk_weight)
retval: none
args: max_walk_weight (number)

--// Get extra carry weight granted by the suit
db.actor:GetAdditionalMaxWeight()
retval: number

--// Set extra carry weight granted by the suit
db.actor:SetAdditionalMaxWeight(add_max_weight)
retval: none
args: add_max_weight (number)

--// Get extra walking weight granted by the suit
db.actor:GetAdditionalMaxWalkWeight()
retval: number

--// Set extra walking weight granted by the suit
db.actor:SetAdditionalMaxWalkWeight(add_max_walk_weight)
retval: none
args: add_max_walk_weight (number)

--// Set sleepiness directly
db.actor:set_actor_sleepiness(value)
retval: none
args: value (number)

--// Set satiety directly
db.actor:set_actor_satiety(value)
retval: none
args: value (number)

--// Set thirst directly
db.actor:set_actor_thirst(value)
retval: none
args: value (number)

--// Set health directly
db.actor:set_actor_health(value)
retval: none
args: value (number)

--// Set stamina directly
db.actor:set_actor_power(value)
retval: none
args: value (number)

--// Set radiation directly
db.actor:set_actor_radiation(value)
retval: none
args: value (number)

--// Set psy health directly
db.actor:set_actor_psy_health(value)
retval: none
args: value (number)

--// Set morale directly
db.actor:set_actor_morale(value)
retval: none
args: value (number)
```
* Camera
```lua
--// Is first-person camera active
db.actor:is_first_person()
retval: boolean

--// Switch to first-person camera
db.actor:set_first_person()
retval: none

--// Switch to third-person camera
db.actor:set_third_person()
retval: none
```
* [Boosters](/scripting/exported-enums#eboostparams)
```lua
--// Check if booster affects the actor (param from EBoostParams)
db.actor:is_booster_influence(boost_param)
retval: boolean
args: boost_param (EBoostParams)

--// Get booster effect time (param from EBoostParams)
db.actor:get_booster_influence_time(boost_param)
retval: number
args: boost_param (EBoostParams)

--// Apply booster (section name with booster params)
db.actor:apply_booster(section_name)
retval: none
args: section_name (string)

--// Set booster effect time (time, param from EBoostParams)
db.actor:set_booster_time(time, boost_param)
retval: none
args: time (number), boost_param (EBoostParams)

--// Get active eBoostPowerRestore duration
db.actor:get_actor_power_boost_time()
retval: number
```
* Actor shadow
```lua
--// Check if actor shadow is enabled
db.actor:is_actor_shadow()
retval: boolean

--// Enable/disable actor shadow
db.actor:set_actor_shadow(value)
retval: none
args: value (boolean)
```
* Movement
```lua
--// Get actor movement state (first: movement type e.g. eWishful; second: command e.g. mcSprint)
db.actor:get_movement_state(movement_type, move_command)
retval: boolean
args: movement_type (EMovementStates), move_command (EMoveCommand)

--// Set actor movement command (movement type, command, state true/false)
db.actor:set_movement_state(movement_type, move_command, status)
retval: none
args: movement_type (EMovementStates), move_command (EMoveCommand), status (boolean)
```
* Inventory
```lua
--// Disable/enable PDA
db.actor:set_pda_disabled(value)
retval: none
args: value (boolean)

--// Is PDA accessible
db.actor:is_pda_disabled()
retval: boolean

--// Disable/enable inventory
db.actor:set_inventory_disabled(value)
retval: none
args: value (boolean)

--// Is inventory accessible
db.actor:is_inventory_disabled()
retval: boolean
```
* Interactions
```lua
--// Put actor into a vehicle
db.actor:attach_vehicle(car, force)
retval: none
args: car (CScriptGameObject), force (boolean)

--// Remove actor from vehicle
db.actor:detach_vehicle(force)
retval: none
args: force (boolean)

--// Get current holder the actor is in
db.actor:get_attached_vehicle()
retval: CScriptGameObject

--// Is actor on a ladder
db.actor:is_ladder()
retval: boolean

--// Get visual name during an active cutscene
db.actor:get_cutscene_visual()
retval: string

--// Set active target
db.actor:set_best_enemy(enemy)
retval: none
args: enemy (CScriptGameObject)
```
## CoC Extended
* Server alife object `local se_obj = alife():object(obj_id)`
```lua
--// Get character icon
se_obj:character_icon()
retval: string

--// Set character rank
se_obj:set_rank(rank)
retval: none
args: rank (number)

--// Set character profile
se_obj:set_profile_name(profile)
retval: none
args: profile (string)

--// Get character name
se_obj:character_name()
retval: string

--// Set character name
se_obj:set_character_name(name)
retval: none
args: name (string)
```

* Client alife object `local npc = level.object_by_id(npc_id) or db.actor`
```lua
--// Set character rank
npc:change_character_rank(char_rank)
retval: none
args: char_rank (number)

--// Iterate objects around actor until found
npc:iterate_feel_touch(callback)
retval: none
args: callback (function) -- function(obj_id: number): boolean
```

* CActor
```lua
--// Hide detector
db.actor:hide_detector()
```

* Weapon `local weapon = db.actor:active_item()`
```lua
--// Get weapon substate
weapon:get_weapon_substate()
retval: number

--// Get ammo count for specific type
weapon:get_ammo_count_for_type(type)
retval: number
args: type (number)

--// Get main weapon type
weapon:get_main_weapon_type()
retval: number

--// Get weapon type
weapon:get_weapon_type()
retval: number

--// Get ammo section for type
weapon:weapon_get_ammo_section(ammo_type)
retval: string
args: ammo_type (number)

--// Attach addon to weapon
weapon:weapon_addon_attach(obj)
retval: none
args: obj (CScriptGameObject)

--// Detach addon from weapon
weapon:weapon_addon_detach(section, bSpawnToInventory)
retval: none
args: section (string), bSpawnToInventory (boolean)

--// Install upgrade
weapon:install_upgrade(name)
retval: boolean
args: name (string)

--// Check if upgrade is installed
weapon:has_upgrade(name)
retval: boolean
args: name (string)

--// Iterate installed upgrades using callback function
weapon:iterate_installed_upgrades(callback)
retval: none
args: callback (function) -- function(upgrade_sect: string, wpn_obj: self): boolean

--// Check item on belt
weapon:IsOnBelt(other_obj)
retval: boolean
args: other_obj (CScriptGameObject)

--// Play HUD animation
weapon:play_hud_motion(name, use_mix, state)
retval: number
args: name (string), use_mix (boolean), state (number)

--// Switch weapon state
weapon:switch_state(state)
retval: none
args: state (number)

--// Get HUD item state
weapon:get_state()
retval: number

--// Get ammo count
weapon:ammo_get_count()
retval: number

--// Set ammo count
weapon:ammo_set_count(count)
retval: none
args: count (number)

--// Get ammo per box
weapon:ammo_box_size()
retval: number

--// Get ammo loaded into magazine and chamber
weapon:get_ammo_in_magazine_and_chamber()
retval: number

--// Does weapon use chamber
weapon:is_weapon_use_chamber()
retval: boolean
```
* Items
```lua
--// Get item by ID
item:item_on_belt(item_id)
retval: CScriptGameObject
args: item_id (number)
```

## Bone Visibility Control
`local obj = level.object_by_id(obj_id) or db.actor`
```lua
--// Is bone visible on world visual
obj:is_world_object_bone_visible(boneName)
retval: boolean
args: boneName (string)

--// Set bone visibility on world visual
obj:set_world_object_bone_visibility(boneName, bVisibility)
retval: boolean
args: boneName (string), bVisibility (boolean)

--// Is bone visible on HUD visual
obj:is_hud_object_bone_visible(boneName)
retval: boolean
args: boneName (string)

--// Set bone visibility on HUD visual
obj:set_hud_object_bone_visibility(boneName, bVisibility)
retval: boolean
args: boneName (string), bVisibility (boolean)
```

## CCar
`local car = level.object_by_id(car_id)`
```lua
--// Add fuel (respecting m_fuel_tank limit)
car:AddFuel(amount)
retval: none
args: amount (number)

--// Get current fuel
car.fuel
--// Set current fuel
car.fuel = value

--// Get fuel tank size
car.fuel_tank
--// Set fuel tank size
car.fuel_tank = value
```

## From Lost Alpha
```lua
--// Get previous weather
level.get_past_wdesc()
retval: string

--// Get next weather
level.get_next_wdesc()
retval: string

--// Get execution time of previous weather
level.get_past_wdesc_execution_time()
retval: number

--// Get execution time of next weather
level.get_next_wdesc_execution_time()
retval: number

--// Get weather game time
level.get_weather_game_time()
retval: number

--// Set previous weather
level.set_past_wdesc(WeatherSection)
retval: none
args: WeatherSection (string)

--// Set next weather
level.set_next_wdesc(WeatherSection)
retval: none
args: WeatherSection (string)
```

## CUIGameCustom
```lua
--// Show a message on screen
get_hud():AddHudMessage(text)
retval: none
args: text (string)
```

## alife_simulator
```lua
--// Teleport actor to level
alife():jump_to_level(name)
retval: none
args: name (string)

--// Teleport alife object
alife():teleport_object(id, gv_id, lv_id, pos)
retval: none
args: id (number), gv_id (number), lv_id (number), pos (vector)

--// Iterate object information
alife():iterate_info(id, callback)
retval: none
args: id (number), callback (function)

--// Reprocess spawn
alife():reprocess_spawn(sobj)
retval: none
args: sobj (CScriptGameObject)

--// Set objects per update
alife():set_objects_per_update(count)
retval: none
args: count (number)

--// Set process time
alife():set_process_time(time)
retval: none
args: time (number)

--// Get child objects
alife():get_children(sobj)
retval: table
args: sobj (CScriptGameObject)

--// Iterator for alife objects (Lost Alpha)
alife():objects()
retval: iterator
-- Example:
-- for id, se_obj in alife():objects() do
--     -- id (number), se_obj (CScriptGameObject)
-- end
```

## CTime
```lua
--// Save time in compressed form (4 bytes)
game.get_game_time():save(packet)
retval: none
args: packet (net_packet)

--// Load time in compressed form (4 bytes)
game.get_game_time():load(packet)
retval: none
args: packet (net_packet)
```

## `save`
```lua
--// Send current chunk name to engine (debug info)
save.set_stage(name)
retval: none
args: name (string)

--// Trigger save error (debug info)
save.call_error()
retval: none
args: none
```

## [animslot](/animation-system/hud-animator)
```lua
--// Play HUD animation
animslot.play(section, anim)
retval: none
args: section (string), anim (string)
```

## [CEatableItem](/gameplay/general/items-used) `local bread = level.object_by_id(item_id)`
```lua
--// Item can no longer be used
bread:Empty()
retval: boolean

--// Item will be deleted if it cannot be used
bread:CanDelete()
retval: boolean

--// Max uses
bread:GetMaxUses()
retval: number

--// Remaining uses
bread:GetRemainingUses()
retval: number

--// Set remaining uses
bread:SetRemainingUses(count)
retval: none
args: count (number)

--// Current item weight
bread:Weight()
retval: number

--// Item cost
bread:Cost()
retval: number

--// Item will be deleted if it cannot be used (property)
bread.m_bRemoveAfterUse
--// Set property value
bread.m_bRemoveAfterUse = value

--// Initial item weight (property)
bread.m_fWeightFull
--// Set property value
bread.m_fWeightFull = value

--// Empty item weight (property)
bread.m_fWeightEmpty
--// Set property value
bread.m_fWeightEmpty = value
```

## CMapManager
```lua
--// Remove specified map location
level.map_manager():RemoveMapLocation(ml)
retval: none
args: ml (CMapLocation)

--// Remove map location by object ID
level.map_manager():RemoveMapLocationByObjectID(id)
retval: none
args: id (number)

--// Disable all pointers on map
level.map_manager():DisableAllPointers()
retval: none
args: none

--// Run functor for each location with given type and ID
level.map_manager():MapLocationsForEach(spot_type, id, functor)
retval: none
args:
  spot_type (string)
  id        (number)
  functor   (function) -> bool
    true  - continue iteration
    false - stop

--// Run functor for all map locations
level.map_manager():AllLocationsForEach(functor)
retval: none
args:
  functor (function) -> bool
    true  - continue iteration
    false - stop
```

::: details Examples

```lua
  -- RemoveMapLocationByObjectID
  mapManager:RemoveMapLocationByObjectID(123)

  -- RemoveMapLocation
  local location = mapManager:GetMapLocation("spot_type", 123)
  mapManager:RemoveMapLocation(location)

  -- DisableAllPointers
  mapManager:DisableAllPointers()

  -- MapLocationsForEach
  mapManager:MapLocationsForEach("spot_type", 123, function(location)
	  print(location:GetHint())
	  return false -- return true to break iteration
  end)

  -- AllLocationsForEach
  mapManager:AllLocationsForEach(function(location)
	  print(location:GetHint())
	  return false -- return true to break iteration
  end)
  ```
:::

## CMapLocation
* Functions
```lua
--// Are map location hints enabled
location:HintEnabled()
retval: boolean

--// Get location hint text
location:GetHint()
retval: string

--// Set location hint text
location:SetHint(hint)
retval: none
args: hint (string)

--// Are map pointers enabled
location:PointerEnabled()
retval: boolean

--// Enable map pointer
location:EnablePointer()
retval: none

--// Disable map pointer
location:DisablePointer()
retval: none

--// Get map location type
location:GetType()
retval: string

--// Get map marker size
location:SpotSize()
retval: Fvector2

--// Is the location user-defined
location:IsUserDefined()
retval: boolean

--// Mark location as user-defined
location:SetUserDefinedFlag(state)
retval: none
args: state (boolean)

--// Highlight map marker
location:HighlightSpot(state, color)
retval: none
args: state (boolean), color (Fcolor)

--// Is the location collidable
location:Collidable()
retval: boolean

--// Is the marker enabled
location:SpotEnabled()
retval: boolean

--// Enable marker
location:EnableSpot()
retval: none

--// Disable marker
location:DisableSpot()
retval: none

--// Get level name for the location
location:GetLevelName()
retval: string

--// Get map position
location:GetPosition()
retval: Fvector2

--// Get map location object ID
location:ObjectID()
retval: number

--// Get last known position
location:GetLastPosition()
retval: Fvector
```
::: details Examples

```lua
  -- HintEnabled
  if location:HintEnabled() then
      print("Hint enabled")
  end

  -- GetHint
  local hint = location:GetHint()
  print("Hint: " .. hint)

  -- SetHint
  location:SetHint("New hint")

  -- PointerEnabled
  if location:PointerEnabled() then
      print("Pointer enabled")
  end

  -- EnablePointer
  location:EnablePointer()

  -- DisablePointer
  location:DisablePointer()

  -- GetType
  local type = location:GetType()
  print("Location type: " .. type)

  -- SpotSize
  local size = location:SpotSize()
  print("Marker size: " .. size.x .. ", " .. size.y)

  -- IsUserDefined
  if location:IsUserDefined() then
      print("User-defined location")
  end

  -- SetUserDefinedFlag
  location:SetUserDefinedFlag(true)

  -- HighlightSpot
  local color = {r = 1, g = 0, b = 0, a = 1}
  location:HighlightSpot(true, color)

  -- Collidable
  if location:Collidable() then
      print("Location is collidable")
  end

  -- SpotEnabled
  if location:SpotEnabled() then
      print("Marker enabled")
  end

  -- EnableSpot
  location:EnableSpot()

  -- DisableSpot
  location:DisableSpot()

  -- GetLevelName
  local levelName = location:GetLevelName()
  print("Level name: " .. levelName)

  -- GetPosition
  local position = location:GetPosition()
  print("Position: " .. position.x .. ", " .. position.y)

  -- ObjectID
  local id = location:ObjectID()
  print("Object ID: " .. id)

  -- GetLastPosition
  local lastPosition = location:GetLastPosition()
  print("Last position: " .. lastPosition.x .. ", " .. lastPosition.y .. ", " .. lastPosition.z)
  ```
:::

## CScriptGameObject

* Properties

```lua
--// Set NPC as mechanic; get - is NPC a mechanic
object.mechanic = true/false
```

* Functions

```lua
--// Enable/disable fire
object:set_fire(bool)
retval: none
args: bool

--// Set max weight InventoryOwner can carry
object:SetCharacterMaxWeight(float)
retval: none
args: float

--// Get total inventory weight of InventoryOwner
object:GetTotalWeight()
retval: float

--// Get weight of specific item
object:Weight()
retval: float

--// Get actor jump speed
object:GetActorJumpSpeed()
retval: float

--// Set actor jump speed
object:SetActorJumpSpeed(float)
retval: none
args: float

--// Get actor sprint coefficient
object:GetActorSprintKoef()
retval: float

--// Set actor sprint coefficient
object:SetActorSprintKoef(float)
retval: none
args: float

--// Get actor run coefficient
object:GetActorRunCoef()
retval: float

--// Set actor run coefficient
object:SetActorRunCoef(float)
retval: none
args: float

--// Get actor backward run coefficient
object:GetActorRunBackCoef()
retval: float

--// Set actor backward run coefficient
object:SetActorRunBackCoef(float)
retval: none
args: float

--// Change entity health directly (with delta)
object:set_health_ex()
retval: none

--// Add custom text to inventory item icon
object:set_sub_inventory_icon_text(text, color, font, offset)
retval: none
args: string, int, string, vector2

--// Add custom texture to inventory item icon
object:set_sub_inventory_icon(mark, offset, size, texture, color)
retval: none
args: bool, vector2, vector2, string, int
```

::: details Examples

```lua
  -- Custom text on inventory icon
  item:set_sub_inventory_icon_text("22123", GetARGB(255, 128, 155, 255), "font_product_sans_14", vector2():set(10, 10))
  
  -- Custom texture on inventory icon
  item:set_sub_inventory_icon(true, vector2():set(3, 3), vector2():set(15, 15), "ui_inGame2_inventory_status_bar", GetARGB(255, 128, 155, 255))

  -- ActorIsJump
  if actor:ActorIsJump() then
      print("Actor is jumping")
  end

  -- GetActorMaxWeight
  local maxWeight = actor:GetActorMaxWeight()
  print("Actor max weight: " .. maxWeight)

  -- SetActorMaxWeight
  actor:SetActorMaxWeight(100.0)

  -- GetActorMaxWalkWeight
  local maxWalkWeight = actor:GetActorMaxWalkWeight()
  print("Max walk weight: " .. maxWalkWeight)

  -- SetActorMaxWalkWeight
  actor:SetActorMaxWalkWeight(80.0)

  -- GetAdditionalMaxWeight
  local additionalWeight = actor:GetAdditionalMaxWeight()
  print("Additional weight: " .. additionalWeight)

  -- SetAdditionalMaxWeight
  actor:SetAdditionalMaxWeight(20.0)

  -- GetAdditionalMaxWalkWeight
  local additionalWalkWeight = actor:GetAdditionalMaxWalkWeight()
  print("Additional walk weight: " .. additionalWalkWeight)

  -- SetAdditionalMaxWalkWeight
  actor:SetAdditionalMaxWalkWeight(15.0)

  -- GetTotalWeight
  local totalWeight = actor:GetTotalWeight()
  print("Total inventory weight: " .. totalWeight)

  -- Weight
  local itemWeight = actor:Weight()
  print("Item weight: " .. itemWeight)

  -- GetActorJumpSpeed
  local jumpSpeed = actor:GetActorJumpSpeed()
  print("Jump speed: " .. jumpSpeed)

  -- SetActorJumpSpeed
  actor:SetActorJumpSpeed(5.0)

  -- GetActorSprintKoef
  local sprintKoef = actor:GetActorSprintKoef()
  print("Sprint coefficient: " .. sprintKoef)

  -- SetActorSprintKoef
  actor:SetActorSprintKoef(1.5)

  -- GetActorRunCoef
  local runCoef = actor:GetActorRunCoef()
  print("Run coefficient: " .. runCoef)

  -- SetActorRunCoef
  actor:SetActorRunCoef(1.2)

  -- GetActorRunBackCoef
  local runBackCoef = actor:GetActorRunBackCoef()
  print("Backward run coefficient: " .. runBackCoef)

  -- SetActorRunBackCoef
  actor:SetActorRunBackCoef(0.8)
  ```
:::

## ActorMenu
```lua
--// Get PDA UI class
ActorMenu.get_pda_menu()
retval: CUIPdaWnd

--// Get Actor UI class
ActorMenu.get_actor_menu()
retval: CUIActorMenu

--// Get current UI ID
ActorMenu.get_menu_mode()
retval: number

--// Get maingame UI class
ActorMenu.get_maingame()
retval: CUIMainIngameWnd
```

## CUIListBox
```lua
--// Select element by index
listbox:SetSelectedIndex(id)
retval: none
args: int
```

## CScriptGameObject
You can now add custom calculated extra text to an item description—useful for auto-generated dynamic stats.

```lua
--// Get additional description text attached to an inventory item
object:get_item_additional_description()
retval: string

--// Set additional description text on an inventory item
object:set_item_additional_description(text)
retval: none
args: string

--// Clear additional description text from an inventory item
object:unset_item_additional_description()
retval: none

--// Is additional description text set
object:is_item_used_additional_description()
retval: bool
```

::: details Example: optimized way to set additional item description via script

```lua
  -- Requires: IXR FRAMEWORK: ^1.0 (or use _G CUIActorMenu_OnItemFocusReceive directly)
  -- Subscribe to mouse-over callback (IXR FRAMEWORK)
	function on_game_start()
		RegisterScriptCallback("CUIActorMenu_OnItemFocusReceive", this.on_item_focus_receive)
	end

-- Set extra description text for a specific section when hovering an item (expand for gameplay logic)
	function on_item_focus_receive(item_game_object)
		local trigger_section = "itm_repair_kit_03" -- Section
		if item_game_object and item_game_object:id() and item_game_object:section() == trigger_section  then
			local min_repair_condition = 30 -- Custom value inserted into translated text
			local characteristics = {
				game.translate_string("st_additional_characteristics"),
				game.translate_string("st_characteristic_category_repair_kit"),
				game.translate_string("st_characteristic_min_condition_repair_kit") .. tostring(min_repair_condition) .. " %",
			}
			
			-- You can pass text directly; here we expand from a string table for convenience
			item_game_object:set_item_additional_description(table.concat(characteristics, ""))
		end
	end

  ```

  Register string fragments in the language file for proper translations:
  ```xml
    <string id="st_additional_characteristics">
		<text> \n%c[255,255,255,255]Characteristics: </text>
	</string>

	<string id="st_characteristic_category_repair_kit">
		<text> \n%c[255,255,255,255]• %c[255,255,255,255] Equipment repair </text>
	</string>

	<string id="st_characteristic_min_condition_repair_kit">
		<text> \n%c[255,255,255,255]• %c[255,255,255,255] Minimum condition to use: </text>
	</string>
  ```
  
  Resulting item description with a script-adjustable value:
  ```xml
	Existing config description ...
	Characteristics: 
		•  Equipment repair 
		•  Minimum condition to use: 30 %
  ```
:::

## level
```lua
--// enable/disable rain effector
level.enable_rain(value)
retval: none
args: value (boolean)

--// Switch wallmark visible
level.switch_wallmark(wallmark_object, status)
retval: none
args: wallmark_object(script game object), status (boolean)

--// Get fog distance from current env
level.get_fog_distance()
retval: float

-- Запретить двигать мышью (независимо от disable input или во время него)
level.disable_mouse_move()
retval: void

-- Разрешить двигать мышью (независимо от disable input или во время него)
level.enable_mouse_move()
retval: void
```
