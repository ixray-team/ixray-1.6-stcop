# New functions
## Global namespace

```cpp
bool IsDedicated(); //-- Script is executed on a dedicated server
bool OnClient();    //-- Script is executed on the client side
bool OnServer();    //-- Script is executed on the server side

Fvector2 GetCursorPosition();     //-- Get cursor position
void SetCursorPosition(Fvector2); //-- Set cursor position
```

## player_hud

```cpp
void show_legs(bool); //-- Show/hide legs
```

## CActor

### New

* Player status
```cpp
bool is_god_mode();
//-- Is the actor in god mode

void SetInvulnerable(bool);
//-- Enable/disable god mode

bool ActorIsJump();
//-- Is the actor currently jumping

float GetActorMaxWeight() const;
//-- Max weight the actor can carry

void SetActorMaxWeight(float max_weight);
//-- Set max weight the actor can carry

float GetActorMaxWalkWeight() const;
//-- Max weight at which the actor can walk

void SetActorMaxWalkWeight(float max_walk_weight);
//-- Set max weight at which the actor can walk

float GetAdditionalMaxWeight() const;
//-- Extra carry weight granted by the suit

void SetAdditionalMaxWeight(float add_max_weight);
//-- Set extra carry weight granted by the suit

float GetAdditionalMaxWalkWeight() const;
//-- Extra walking weight granted by the suit

void SetAdditionalMaxWalkWeight(float add_max_walk_weight);
//-- Set extra walking weight granted by the suit
```
* Camera
```cpp
bool is_first_person();  //-- Is first-person camera active
void set_first_person(); //-- Switch to first-person camera
void set_third_person(); //-- Switch to third-person camera
```
* [Boosters](/scripting/exported-enums#eboostparams)
```cpp
bool is_booster_influence(EBoostParams);        //-- Does the booster affect the actor (param from EBoostParams)
float get_booster_influence_time(EBoostParams); //-- Get booster effect time (param from EBoostParams)

void apply_booster(string);                     //-- Apply booster (section name with booster params)
void set_booster_time(number, EBoostParams);    //-- Set booster effect time (time, param from EBoostParams)
float get_actor_power_boost_time();             //-- Returns active eBoostPowerRestore duration
```
* Actor shadow
```cpp
bool is_actor_shadow();      //-- Is actor shadow enabled
void set_actor_shadow(bool); //-- Enable/disable actor shadow
```
* Movement
```cpp
bool get_movement_state(EMovementStates, EMoveCommand);       //-- Get actor movement state (first: movement type e.g. eWishful; second: command e.g. mcSprint)
void set_movement_state(EMovementStates, EMoveCommand, bool); //-- Set actor movement command (movement type, command, state true/false)
```
* Inventory
```cpp
void set_pda_disabled(bool);
//-- Disable/enable PDA

bool is_pda_disabled();
//-- Is PDA accessible

void set_inventory_disabled(bool);
//-- Disable/enable inventory

bool is_inventory_disabled();
//-- Is inventory accessible
```
* Interactions
```cpp
void attach_vehicle(CScriptGameObject* Car, bool force);
 //-- Put actor into a vehicle

void detach_vehicle(bool force);
//-- Remove actor from vehicle

CScriptGameObject* get_attached_vehicle();
//-- Current holder the actor is in

bool is_ladder();
//-- Is actor on a ladder

string get_cutscene_visual();
//-- Get visual name during an active cutscene

void set_best_enemy(CScriptGameObject*);
//-- Set active target
```
## CoC Extended
* Weapon
```cpp
//-- State
u8 get_weapon_substate();
int get_ammo_count_for_type(type);
u32 get_main_weapon_type();
u32 get_weapon_type();

string weapon_get_ammo_section(ammo_type);
void weapon_addon_attach(obj);
void weapon_addon_detach(obj);

//-- Upgrades
bool install_upgrade(name);
bool has_upgrade(name);
void iterate_installed_upgrades();
```
* Other
```cpp
void set_character_icon(icon);               //-- Set character icon
void change_character_rank(char_rank);       //-- Set character rank
LPCSTR character_name();                     //-- Get character name
LPCSTR character_icon();                     //-- Get character icon
void set_rank(rank);                         //-- Set character rank
void set_profile_name(profile);              //-- Set character profile
void set_character_name(name);               //-- Set character name
void iterate_feel_touch(function bool(id));  //-- Iterate objects around actor until found
void hide_detector();                        //-- Hide detector
bool IsOnBelt(obj);                          //-- Check item on belt
obj item_on_belt(ItemID);                    //-- Get item by ID
u32 play_hud_motion(Name, UseMix, state);    //-- Play HUD animation
void switch_state(state);                    //-- Switch weapon state
u32 get_state();                             //-- Get HUD item state
u16 ammo_get_count();                        //-- Get ammo count
void AmmoSetCount(count);                    //-- Set ammo count
int AmmoBoxSize();                           //-- Get ammo per box
int get_ammo_in_magazine_and_chamber();      //-- Ammo loaded into mag + chamber
bool is_weapon_use_chamber();                //-- Does weapon use chamber
```
## CCar
```cpp
void AddFuel(float);   //-- Add fuel (respecting m_fuel_tank limit)
property fuel;         //-- Current fuel
property fuel_tank;    //-- Fuel tank size
```

### From Lost Alpha
```cpp
LPCSTR get_past_wdesc();                    //-- Get previous weather
LPCSTR get_next_wdesc();                    //-- Get next weather
float get_past_wdesc_execution_time();      //-- Get execution time of previous weather
float get_next_wdesc_execution_time();      //-- Get execution time of next weather
float get_weather_game_time();              //-- Get weather game time
void set_past_wdesc(LPCSTR WeatherSection); //-- Set previous weather
void set_next_wdesc(LPCSTR WeatherSection); //-- Set next weather
```

## CUIGameCustom
```lua
AddHudMessage(string) -- Show a message on screen 
```

## alife_simulator
```lua
jump_to_level(name)                    //-- Teleport actor to level 
teleport_object(id, gv_id, lv_id, pos) //-- Teleport ALife object 
iterate_info(id, function)
reprocess_spawn(sobj) 
set_objects_per_update(count)
set_process_time(time)
get_children(sobj)
```
* Object Iterator (Lost Alpha)
```lua
for id, se_obj in alife():objects() do
    ...
end
```

## game
### CTime
```lua
save(packet) --// Saves time in compressed form (4 bytes) 
load(packet) --// Loads time in compressed form (4 bytes) 
```
## `save`
```lua
set_stage(name) --// Send current chunk name to engine (debug info)
call_error()    --// Trigger save error (debug info)
```
## [animslot](/animation-system/hud-animator)
```lua
animslot.play(section, anim) //-- Play HUD animation 
```

## [CEatableItem](/gameplay/general/items-used)
* Functions
```cpp
bool Empty();              //-- Item can no longer be used
bool CanDelete();          //-- Item will be deleted if it cannot be used
bool GetMaxUses();         //-- Max uses
u8 GetRemainingUses();     //-- Remaining uses
void SetRemainingUses(u8); //-- Set remaining uses
float Weight();            //-- Current item weight
int Cost();                //-- Item cost
```
* Properties 
```cpp
bool m_bRemoveAfterUse;  //-- Remove item if it cannot be used
float m_fWeightFull;     //-- Initial item weight 
float m_fWeightEmpty;    //-- Empty item weight 
```

## CMapManager
* Functions
```cpp
void RemoveMapLocation(CMapLocation* ml); 
//-- Remove specified map location

void RemoveMapLocationByObjectID(u16 id); 
//-- Remove map location by object ID

void DisableAllPointers(); 
//-- Disable all pointers on map

void MapLocationsForEach(LPCSTR spot_type, u16 id, const luabind::functor<bool>& functor); 
//-- Run functor for each location with given type and ID

void AllLocationsForEach(const luabind::functor<bool>& functor); 
//-- Run functor for all locations
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
```cpp
bool HintEnabled(); 
//-- Are map location hints enabled

LPCSTR GetHint(); 
//-- Get location hint text

void SetHint(const shared_str& hint); 
//-- Set location hint text

bool PointerEnabled(); 
//-- Are map pointers enabled

void EnablePointer(); 
//-- Enable map pointer

void DisablePointer(); 
//-- Disable map pointer

LPCSTR GetType() const; 
//-- Get map location type

Fvector2 SpotSize(); 
//-- Get map marker size

bool IsUserDefined() const; 
//-- Is the location user-defined

void SetUserDefinedFlag(BOOL state); 
//-- Mark location as user-defined

void HighlightSpot(bool state, const Fcolor& color); 
//-- Highlight map marker

bool Collidable() const; 
//-- Is the location collidable

bool SpotEnabled(); 
//-- Is the marker enabled

void EnableSpot(); 
//-- Enable marker

void DisableSpot(); 
//-- Disable marker

const shared_str& GetLevelName(); 
//-- Get level name for the location

const Fvector2& GetPosition(); 
//-- Get map position

u16 ObjectID(); 
//-- Get map location object ID

Fvector GetLastPosition(); 
//-- Get last known position
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

```csharp
property bool mechanic //-- set - mark NPC as mechanic; get - is NPC a mechanic
```

* Functions

```cpp
void set_fire(bool);
//-- Enable/disable fire

void SetCharacterMaxWeight(float);
//-- Set max weight InventoryOwner can carry.

float GetTotalWeight() const;
//-- Total inventory weight of InventoryOwner.

float Weight() const;
//-- Weight of the specific item.

float GetActorJumpSpeed() const;
//-- Actor jump speed.

void SetActorJumpSpeed(float jump_speed);
//-- Set actor jump speed.

float GetActorSprintKoef() const;
//-- Actor sprint coefficient.

void SetActorSprintKoef(float sprint_koef);
//-- Set actor sprint coefficient.

float GetActorRunCoef() const;
//-- Actor run coefficient.

void SetActorRunCoef(float run_coef);
//-- Set actor run coefficient.

float GetActorRunBackCoef() const;
//-- Actor backward run coefficient.

void SetActorRunBackCoef(float run_back_coef);
//-- Set actor backward run coefficient.

void set_health_ex();
//-- Change entity health directly (with delta) instead of standard health

void set_sub_inventory_icon_text(LPCSTR m_custom_text, int item_custom_text_clr_inv, LPCSTR item_custom_text_font, Fvector2 m_custom_text_offset);
//-- Add custom text to the inventory item icon

void set_sub_inventory_icon(bool m_custom_mark, Fvector2 m_custom_mark_offset, Fvector2 m_custom_mark_size, LPCSTR m_custom_mark_texture, int m_custom_mark_clr);
//-- Add custom texture to the inventory item icon
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
ActorMenu.get_pda_menu()   //-- Get PDA UI class
ActorMenu.get_actor_menu() //-- Get Actor UI class
ActorMenu.get_menu_mode()  //-- Get current UI ID
ActorMenu.get_maingame()   //-- Get maingame UI ID
```

## CUIListBox
```cpp
void SetSelectedIndex(id); //-- Select element
```

## CScriptGameObject
You can now add custom calculated extra text to an item description—useful for auto-generated dynamic stats.

```lua
-- Get additional description text attached to an inventory item.
string get_item_additional_description()

-- Set additional description text on an inventory item.
void set_item_additional_description(string)

-- Clear additional description text from an inventory item.
void unset_item_additional_description()

-- Is additional description text set.
bool is_item_used_additional_description()
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
