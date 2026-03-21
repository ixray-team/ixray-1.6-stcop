--// General 
jit.opt.start(2)

string.gfind = string.gmatch
math.mod = math.fmod

--// LuaPandas
DebuggerMode = false

function debug_jit_off()
	if DebuggerMode then
		if jit then jit.off() end
	end
end

function debug_jit_on()
	if DebuggerMode then
		if jit then jit.on() end
	end
end

function debugger_attach() 
	if DebuggerMode then
		debug_jit_off()
		LuaPanda.reConnect()
		debug_jit_on()
	else
		debug_jit_off()
		SemiLog('LuaPanda starting...')
		LuaPanda.start("127.0.0.1", 8818)
		DebuggerMode = true
		debug_jit_on()
	end
end


--  ####################################################################################################################
-- 										OVERRIDE ORIGINAL ENTRY POINT
--  ####################################################################################################################

function ixr_on_start_game_callback()
	SemiLog("--[IXR OVERRIDE] _G.start_game_callback()")
	_G.start_game_callback()
	if ixr_framework then ixr_framework.init() end
end

--  ####################################################################################################################
-- 													IXR FRAMEWORK
--  ####################################################################################################################

--[[
Description: Check is module loaded by script name or alias name
Parameters:
  script_or_alias_name (string)(required) 	- Identifier script name or module alias name
Return: (bool) - Return true is module by alias or script name exists.
]]
function IsModuleLoaded(script_or_alias_name)
	return ixr_framework and ixr_framework.is_module_loaded(script_or_alias_name)
end

--[[
Description: Retrive reference to module by alias name or script name
Parameters:
  script_or_alias_name (string)(required) - Identifier script name or module alias name
Return: (reference|exception) - Return reference to callable module or exception is module by alias or script name not exists (use check method before use it function is need skip errors).
]]
function GetModule(script_or_alias_name)
	return ixr_framework.get_module(script_or_alias_name)
end

--[[
Description: Closure call is module exists and return response or default value is module not exists
Parameters:
  script_or_alias_name (string)(required) 	- Identifier script name or module alias name
Return: (mixed|false) - Return true is module by alias or script name exists.
]]
function ClosureModuleIsExists(script_or_alias_name, callback_fn, def_value)
	return ixr_framework.call_module_is_exists(script_or_alias_name, callback_fn, def_value)
end

--  ####################################################################################################################
-- 													IXR STORAGE
--  ####################################################################################################################

--  #########################################################
-- 	#	IXR ISOLATED STORAGE METHODS (by Script Namespace)  #
--  #########################################################
--[[
Description: Check is value saved to isolated by script name storage
Parameters:
  var_name (string)(required) - key name for index value in storage (uique name for caller script namespace) not conflicts between other scripts
Returns: (bool) true is exists
]]
function HasStorageVar(var_name)
	if IsModuleLoaded("ixr_storage") then
		return GetModule("ixr_storage").has_var(ffx_callable_utils.find_caller_source(3), var_name)
	end
	
	return false
end

--[[
Description: Load simple (string|number|bool|table) lua type variable value from isolated by script name storage
Parameters:
  var_name (string)(required) - key name for index value in storage (uique name for caller script namespace) not conflicts between other scripts
  default_value (string|number|bool|table) - default value retrived is storage by variable name is empty
Returns: (string|number|bool|table) - retrive saved or default value
]]
function GetStorageVar(var_name, default_value)
	if IsModuleLoaded("ixr_storage") then
		return GetModule("ixr_storage").get_var(ffx_callable_utils.find_caller_source(3), var_name, default_value)
	end
	
	return default_value
end

--[[
Description: Save simple (string|number|bool|table) lua type variable value to isolated by script name storage
Parameters:
  var_name (string)(required) - key name for index value in storage (uique name for caller script namespace) not conflicts between other scripts
  var_value (string|number|bool|table)(required)  - value for save
  var_type (string) - custom casts non required
Returns: (bool) true is success save
]]
function SetStorageVar(var_name, var_value, var_type)
	if IsModuleLoaded("ixr_storage") then
		return GetModule("ixr_storage").set_var(ffx_callable_utils.find_caller_source(3), var_name, var_value, var_type)
	end
	
	return false
end

--[[
Description: Remove saved variable value from isolated by script name storage
Parameters:
  var_name (string)(required) - key name for index value in storage (uique name for caller script namespace) not conflicts between other scripts
Returns: (void)
]]
function UnsetStorageVar(var_name)
	if IsModuleLoaded("ixr_storage") then
		return GetModule("ixr_storage").remove_var(ffx_callable_utils.find_caller_source(3), var_name)
	end
	
	return false
end

--  ###############################################################################
-- 	#	IXR CHUNK STORAGE METHODS (for netpk) (by Script Namespace + object id)   #
--  ###############################################################################

--[[
Description: Retrieve a saved variable value from the isolated storage by object ID.
Parameters:
  object_id (string)(required) 				- Unique identifier for the object.
  var_name (string)(required) 				- Key name for the variable in the storage.
Returns: (bool) 							- Return true is value exists
]]
function HasStorageObjectVar(object_id, var_name)
	if IsModuleLoaded("ixr_storage") then
		local __key = ffx_callable_utils.find_caller_source(3) .. "_obj_" .. object_id
		local _obj_table_name = "obj_table"
		local obj_table_cache = GetModule("ixr_storage").get_var(__key, _obj_table_name, {})
		
		if obj_table_cache[var_name] == nil then
			return false
		end
			
		return obj_table_cache[var_name]["var_value"] ~= nil
	end
	
	return false
end

--[[
Description: Retrieve a saved variable value from the isolated storage by object ID.
Parameters:
  object_id (string)(required) 				- Unique identifier for the object.
  var_name (string)(required) 				- Key name for the variable in the storage.
  default_value (mixed)(optional) 			- Value to return if the variable is not found.
  retrive_raw_with_type (bool)(optional) - If true, returns the raw value without type encapsulation.
Returns: (mixed) 							- The retrieved variable value or default_value if not found.
]]
function GetStorageObjectVar(object_id, var_name, default_value, retrive_raw_with_type)
	if IsModuleLoaded("ixr_storage") then
		local __key = ffx_callable_utils.find_caller_source(3) .. "_obj_" .. object_id
		local _obj_table_name = "obj_table"
		local obj_table_cache = GetModule("ixr_storage").get_var(__key, _obj_table_name, {})
		if retrive_raw_with_type == nil then
			retrive_raw_with_type = false
		end
		
		if retrive_raw_with_type then
			return obj_table_cache[var_name] or default_value
		else
			if not obj_table_cache[var_name] then
				return default_value
			end
			
			return obj_table_cache[var_name]["var_value"] or default_value
		end
	end
	
	return default_value
end

--[[
Description: Save a variable value in the isolated storage by object ID.
Parameters:
  object_id (string)(required) - Unique identifier for the object.
  var_name (string)(required)   - Key name for the variable in the storage.
  var_value (mixed)(required)      - The value to be saved.
  var_type (string)(optional)    - The type of the variable (default is determined by the type of var_value).
Returns:  (bool) - Returns true if the value is successfully saved, otherwise false.
]]
function SetStorageObjectVar(object_id, var_name, var_value, var_type)
	if IsModuleLoaded("ixr_storage") then
		local _module = GetModule("ixr_storage")
		local __key = ffx_callable_utils.find_caller_source(3) .. "_obj_" .. object_id
		local _obj_table_name = "obj_table"
		local obj_table_cache = _module.get_var(__key, _obj_table_name, {})
		
		if var_type == nil then
			var_type = tostring(type(var_value))
		end
		
		obj_table_cache[var_name] = {
			["var_type"] = var_type,
			["var_value"] = var_value,
		}
		
		return _module.set_var(__key, _obj_table_name, obj_table_cache, "table")
	end
	
	return false
end

--[[
Description: Remove variable value in isolated storage by object ID.
Parameters:
  object_id (string)(required) - Unique identifier for the object.
  var_name (string)(required)   - Key name for the variable in the storage.
Returns:  (bool) - Returns true if the value is successfully saved, otherwise false.
]]
function UnsetStorageObjectVar(object_id, var_name)
	if IsModuleLoaded("ixr_storage") then
		local _module = GetModule("ixr_storage")
		local __key = ffx_callable_utils.find_caller_source(3) .. "_obj_" .. object_id
		local _obj_table_name = "obj_table"
		local obj_table_cache = _module.get_var(__key, _obj_table_name, {})
		
		obj_table_cache[var_name] = nil
		
		return _module.set_var(__key, _obj_table_name, obj_table_cache, "table")
	end
	
	return false
end

--  #################################################################################
-- 	#	IXR SHARED STORAGE METHODS (allowed global by key between another scripts)  #
--  #################################################################################

--[[
Description: Check is value saved to shared storage allowed between other scripts
Parameters:
  var_name (string)(required) - key name for index value in storage (uique name for caller script namespace) shared between other scripts
Returns: (bool) true is exists
]]
function HasStorageSharedVar(var_name)
	if IsModuleLoaded("ixr_storage") then
		return GetModule("ixr_storage").has_var("shared_chunk", var_name)
	end
	
	return false
end

--[[
Description: Load simple (string|number|bool|table) lua type variable value from shared storage allowed between other scripts
Parameters:
  var_name (string)(required) - key name for index value in storage (uique name for caller script namespace) shared between other scripts
  default_value (string|number|bool|table) - default value retrived is storage by variable name is empty
Returns: (string|number|bool|table) - retrive saved or default value
]]
function GetStorageSharedVar(var_name, default_value)
	if IsModuleLoaded("ixr_storage") then
		GetModule("ixr_storage").get_var("shared_chunk", var_name, default_valuec)
	end
	
	return default_value
end

--[[
Description: Save simple (string|number|bool|table) lua type variable value to shared storage allowed between other scripts
Parameters:
  var_name (string)(required) - key name for index value in storage (uique name for caller script namespace) shared between other scripts
  var_value (string|number|bool|table)(required)  - value for save
  var_type (string) - custom casts non required
Returns: (bool) true is success save
]]
function SetStorageSharedVar(var_name, var_value, var_type)
	if IsModuleLoaded("ixr_storage") then
		return GetModule("ixr_storage").set_var("shared_chunk", var_name, var_value, var_type)
	end
	
	return false
end

--[[
Description: Remove saved variable value from shared storage
Parameters:
  var_name (string)(required) - key name for index value in storage (uique name for caller script namespace) shared between other scripts
Returns: (void)
]]
function UnsetStorageSharedVar(var_name)
	if IsModuleLoaded("ixr_storage") then
		return GetModule("ixr_storage").remove_var("shared_chunk", var_name)
	end
	
	return false
end



--  ####################################################################################################################
-- 													IXR REGISTRY 
--  ####################################################################################################################

--[[
Description: Checks if a value exists by key (and subkey if provided).
Parameters:
  key (string)(required) 	- the primary key for the registry.
  subkey (string)(optional) - the subkey to check within the primary key.
Returns: (bool) - returns true if the value exists, otherwise false.
 --]]
function HasRegistryValue(key, subkey)
	if IsModuleLoaded("ixr_registry") then
		return GetModule("ixr_registry").has_value(key, subkey)
	end
	
	return false
end

--[[
Description: Retrieves a value by key (and subkey if provided).
Parameters:
  key (string)(required) - the primary key for the registry.
  subkey (string)(optional) - the subkey to retrieve the value from within the primary key.
  def_value (mixed)(optional) - default value is not exists
Returns: (mixed) - returns the value associated with the key and subkey, or def_value if not found.
--]]
function GetRegistryValue(key, subkey, def_value)	
	if IsModuleLoaded("ixr_registry") then
		local _module = GetModule("ixr_registry")
		if _module.has_value(key, subkey) then
			return _module.get_value(key, subkey)
		else
			return def_value
		end
	end
	
	return def_value
end

--[[
Description: Sets a value by key (and subkey if provided).
Parameters:
  key (string)(required) 	- the primary key for the registry.
  subkey (string)(optional) - the subkey to set the value for within the primary key.
  value (mixed)(required) 	- the value to be set.
Returns: (bool) - returns true if the value was successfully set, otherwise false.
--]]
function SetRegistryValue(key, subkey, value)
	if IsModuleLoaded("ixr_registry") then
		return GetModule("ixr_registry").set_value(key, subkey, value)
	end
	
	return false
end

--[[
Description: Removes a value by key (and subkey if provided).
Parameters:
  key (string)(required) - the primary key for the registry.
  subkey (string)(optional) - the subkey to remove the value from within the primary key.
Returns: (bool) - returns true if the value was successfully removed, otherwise false.
--]]
function UnsetRegistryValue(key, subkey)
	if IsModuleLoaded("ixr_registry") then
		return GetModule("ixr_registry").remove_value(key, subkey)
	end
	
	return false
end

--  ####################################################################################################################
-- 													IXR TIMERS
--  ####################################################################################################################


--[[
Description: Create timer.
Parameters:
  name (string)(required) - name of timer.
  loop (bool)(required) - use auto repeat on left time.
  auto_paly (bool)(required) - use auto start timer after create.
  left_milliseconds (int)(required) - time left to run callable in milliseconds.
  callable_fn (function|string)(required) - callable payload for timer.
  ... (args) - callable args.
Returns: (functions):
	self:name(): string
	self:exists(): bool
	self:play(): bool
	self:stop(): bool
	self:delete(): bool
--]]
function TimerCreate(name, loop, auto_paly, left_milliseconds, callable_fn, ...)
	if IsModuleLoaded("ixr_timers") then
		return GetModule("ixr_timers").timer_create(name, loop, auto_paly, left_milliseconds, callable_fn, ...)
	end
	
	return false
end

--[[
Description: Is timer exists by name.
Parameters:
  name (string)(required) - name of timer.
 Returns: (bool)
--]]
function IsTimerExists(name)
	if IsModuleLoaded("ixr_timers") then
		return GetModule("ixr_timers").timer_exists(name)
	end
	
	return false
end

--[[
Description: Play timer by name.
Parameters:
  name (string)(required) - name of timer.
 Returns: (bool)
--]]
function TimerPlay(name)
	if IsModuleLoaded("ixr_timers") then
		return GetModule("ixr_timers").timer_play(name)
	end
	
	return false
end

--[[
Description: Stop timer by name.
Parameters:
  name (string)(required) - name of timer.
 Returns: (bool)
--]]
function TimerStop(name)
	if IsModuleLoaded("ixr_timers") then
		return GetModule("ixr_timers").timer_stop(name)
	end
	
	return false
end

--[[
Description: Delete timer by name.
Parameters:
  name (string)(required) - name of timer.
 Returns: (bool)
--]]
function TimerDelete(name)
	if IsModuleLoaded("ixr_timers") then
		return GetModule("ixr_timers").timer_delete(name)
	end
	
	return false
end


--  ####################################################################################################################
-- 													IXR TRIGGERS
--  ####################################################################################################################

--[[
Description: Add new trigger by name without overlapping.
	trigger_name : trigger name
	trigger_level_name : level name
	trigger_center_position : vector center position trigger 
	trigger_radius : int distance to center
	callable_fn : function (level_name, trigger_name, trigger_center, trigger_radius, game_object_entity, contact_position, is_inside, in_zone, is_outside, source=monster|npc|actor)
Return: (bool) - Returns true if success register, otherwise false.
]]
function TriggerCreate(trigger_name, trigger_level_name, trigger_center_position, trigger_radius, callable_fn)
	if IsModuleLoaded("ixr_triggers") then
		return GetModule("ixr_triggers").set_trigger(trigger_name, trigger_level_name, trigger_center_position, trigger_radius, callable_fn)
	end
	
	return false
end

--[[
Description: Checks has trigger by name exists.
trigger_name : trigger name
Return: (bool) - Returns true if exists, otherwise false.
]]
function TriggerExists(trigger_name)
	if IsModuleLoaded("ixr_triggers") then
		return GetModule("ixr_triggers").has_trigger(trigger_name)
	end
	
	return false
end

--[[
Description: Remove trigger by name.
trigger_name : trigger name
Return: (bool) - Returns true if success, otherwise false.
]]
function TriggerRemove(trigger_name)
	if IsModuleLoaded("ixr_triggers") then
		return GetModule("ixr_triggers").remove_trigger(trigger_name)
	end
	
	return false
end

--  ####################################################################################################################
-- 													IXR OPTIONS
--  ####################################################################################################################

--  #########################################################
-- 	#		IXR ISOLATED OPTIONS (by Script Namespace)  	#
--  #########################################################
--[[
Description: Check is value exists in options by script namespace
Parameters:
  var_name (string)(required) - key name for index value in options (uique name for caller script namespace) not conflicts between other scripts
Returns: (bool) true is exists
]]
function HasOptionsVar(var_name)
	if IsModuleLoaded("ixr_options") then
		return GetModule("ixr_options").has_variable_value(var_name)
	end
	
	return false
end

--[[
Description: Check is value exists in options by script namespace
Parameters:
  var_name (string)(required) - key name for index value in options (uique name for caller script namespace) not conflicts between other scripts,
  default_value (mixed)(required) - default value is not exists by key name
Returns: (nil|bool|int|float|string|table|function) or false is not exists
]]
function GetOptionsVar(var_name, default_value)
	if IsModuleLoaded("ixr_options") then
		return GetModule("ixr_options").get_variable_value(var_name, default_value)
	end
	
	return false
end

-- ##############################################################
-- #						OTHER								#
-- ##############################################################

function GetARGBColorFromHex(RGBHexColor, alpha)
    RGBHexColor = RGBHexColor:gsub("#", "")
   
    if #RGBHexColor == 8 then
        RGBHexColor = RGBHexColor:sub(3, 8)
    end

    local red = tonumber(RGBHexColor:sub(1, 2), 16)
    local green = tonumber(RGBHexColor:sub(3, 4), 16)
    local blue = tonumber(RGBHexColor:sub(5, 6), 16)
    
    return GetARGB(alpha, red, green, blue)
end

function GetXmlArgbColorFromHex(RGBHexColor, alpha)
    RGBHexColor = RGBHexColor:gsub("#", "")
    if #RGBHexColor == 8 then  RGBHexColor = RGBHexColor:sub(3, 8) end

    return "%c["..tostring(table.concat({ alpha, tonumber(RGBHexColor:sub(1, 2), 16), tonumber(RGBHexColor:sub(3, 4), 16), tonumber(RGBHexColor:sub(5, 6), 16) }, ",")).."]"
end

function clamp(val, min, max)
	return (val < min and min) or (val > max and max) or val
end

function lerp(a, b, t)
    return a + (b - a) * t
end

function trim(s)
	return string.gsub(s, "^%s*(.-)%s*$", "%1")
end

function str_explode(str, div, dont_trim)
	if not (dont_trim) then 
		trim(str)
	end 
	
	local t = {}
	local cpt = string.find (str, div, 1, true)
	local a = ""
	
	if cpt then
		repeat
			if not dont_trim then
				a = trim(string.sub(str, 1, cpt-1))
				table.insert( t, a )
			else
				table.insert( t, string.sub(str, 1, cpt-1) )
			end
			str = string.sub( str, cpt+string.len(div) )
			cpt = string.find (str, div, 1, true)
		until cpt==nil
	end
	
	if not dont_trim then
		a = trim(str)
		table.insert(t, a)
	else
		table.insert(t, str)
	end
	return t
end

local con = nil
local cmd_args = nil

function exec_command(command)
	if not con then
		con = get_console()
	end
	
	con:execute(command)
end

function get_cmd_args()
	if not cmd_args then
		cmd_args = command_line()
	end
	
	return cmd_args
end

function has_cmd_arg(arg_name)
	return string.find(get_cmd_args(), arg_name) or string.find(get_cmd_args(), "-"..arg_name)
end

function run_gui(gui, close_inv) 
    ffx_gui_utils.run_gui(gui, close_inv) 
end

function add_art(zone_name, art_section)
	if zone_name == nil then 
		return 
	end
	
    local binder = db.anomaly_by_name[zone_name] or false
	if binder then
		if art_section then
			binder:set_forced_override(art_section)
		end
		binder:spawn_artefact_randomly()
	end
end