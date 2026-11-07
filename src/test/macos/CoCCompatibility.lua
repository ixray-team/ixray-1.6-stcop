local root = assert(arg[1], "repository path is required")

local failures = 0
local function check(name, test)
    local ok, message = pcall(test)
    print((ok and "PASS: " or "FAIL: ") .. name .. (ok and "" or (": " .. tostring(message))))
    if not ok then failures = failures + 1 end
end

-- Execute the real overlay readers/evaluator with controlled packet and NPC state.
function class(name)
	_G[name] = {}
	return function() end
end
property_evaluator = {}
action_base = {}
IsWounded = function() return false end
IsWeapon = function() return false end
xr_danger = {has_danger = function() return false end}
xr_conditions = {surge_started = function() return false end}
xr_logic = {pick_section_from_condlist = function() return "true" end}
db = {storage = {[1] = {}}, actor = {}}
stalker_ids = {property_enemy = 7}
dofile(root .. "/gamedata_coc/scripts/xr_gather_items.script")

local combat = true
local enemy = {evaluate = function() return combat end}
local planner = {evaluator = function() return enemy end}
local npc = {
	alive = function() return true end,
	best_enemy = function() return nil end,
	active_item = function() return nil end,
	id = function() return 1 end,
	motivation_action_manager = function() return planner end
}
local gather = {object = npc, st = {}, find_valid_item = function() return true end}
check("post-combat gathering", function()
assert(not eva_gather_itm.evaluate(gather), "gathering blocks post-combat planner progress")
combat = false
assert(eva_gather_itm.evaluate(gather), "peaceful gathering is unavailable")

end)

cse_smart_cover = {STATE_Read = function() end}
smart_covers = {descriptions = {test_cover = {loopholes = {{id = "seat"}, {id = "stand"}}}}}
dofile(root .. "/gamedata_coc/scripts/se_smart_cover.script")
local function read_cover(disable_seat)
	local strings = {"test_cover", "seat"}
	local packet = {
		r_stringZ = function() return table.remove(strings, 1) end,
		r_u8 = function() return disable_seat and 1 or 0 end,
		r_bool = function() return false end
	}
	local cover = {script_version = 12, loopholes = {}}
	se_smart_cover.STATE_Read(cover, packet, 0)
	return cover.loopholes
end
check("cover availability defaults", function()
local defaults = read_cover(false)
assert(defaults.seat and defaults.stand, "omitted cover flags lost the description defaults")
local explicit = read_cover(true)
assert(explicit.seat == false and explicit.stand, "explicit disabled loophole was overwritten")

end)

cse_alife_item = {STATE_Read = function() end}
dofile(root .. "/gamedata_coc/scripts/se_item.script")
check("optional eatable spawn data", function()
local item = {}
se_eatable.STATE_Read(item, {r_elapsed = function() return 0 end}, 0)
assert(item.remaining_uses == 0, "base spawn unexpectedly reads a missing byte")
se_eatable.STATE_Read(item, {r_elapsed = function() return 1 end, r_u8 = function() return 3 end}, 0)
assert(item.remaining_uses == 3, "saved remaining uses were not retained")
end)
check("options control before menu creation", function()
    dofile(root .. "/gamedata_coc/scripts/ixr_framework/modules/ixr_module_options.script")
    assert(get_controll("missing") == nil)
    get_menu_ref = function() return {ixr_controlls = {present = 42}} end
    assert(get_controll("present") == 42)
end)
check("unique CoC factory registration", function()
    local entries = {}
    local factory = {register = function(self, ...)
        local arguments = {...}
        local clsid = arguments[#arguments - 1]
        assert(not entries[clsid], "duplicate factory CLSID " .. clsid)
        entries[clsid] = true
    end}
    local function load_namespace(path)
        local env = setmetatable({editor = function() return false end, time_global = function() return 0 end}, {__index = _G})
        env._G = env
        setfenv(assert(loadfile(path)), env)()
        return env
    end
    local base = load_namespace(root .. "/gamedata_coc/scripts/class_registrator.script")
    base.register(factory)
    local overlay = load_namespace(root .. "/gamedata_coc/scripts/ixray_global.script")
    overlay.class_registrator = base
    overlay.register(factory)
    assert(entries.SCRPTCAR, "car registration was lost")
end)
assert(failures == 0, tostring(failures) .. " CoC compatibility checks failed")
