local root = assert(arg[1], "repository path is required")

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
assert(not eva_gather_itm.evaluate(gather), "gathering blocks post-combat planner progress")
combat = false
assert(eva_gather_itm.evaluate(gather), "peaceful gathering is unavailable")

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
local defaults = read_cover(false)
assert(defaults.seat and defaults.stand, "omitted cover flags lost the description defaults")
local explicit = read_cover(true)
assert(explicit.seat == false and explicit.stand, "explicit disabled loophole was overwritten")

cse_alife_item = {STATE_Read = function() end}
dofile(root .. "/gamedata_coc/scripts/se_item.script")
local item = {}
se_eatable.STATE_Read(item, {r_elapsed = function() return 0 end}, 0)
assert(item.remaining_uses == 0, "base spawn unexpectedly reads a missing byte")
se_eatable.STATE_Read(item, {r_elapsed = function() return 1 end, r_u8 = function() return 3 end}, 0)
assert(item.remaining_uses == 3, "saved remaining uses were not retained")
print("PASS: CoC post-combat gathering, cover defaults/disabled flags and optional remaining uses")
