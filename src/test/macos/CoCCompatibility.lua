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
check("develop ALife and skeleton packet fields", function()
    net_packet = function() return {} end
    dofile(root .. "/gamedata_coc/scripts/stpk_utils.script")
    local function packet(fields)
        local index = 0
        local out = {}
        local result = {written = out}
        for _, kind in ipairs({"u8", "u16", "u32", "s32", "float", "stringZ"}) do
            result["r_" .. kind] = function()
                index = index + 1
                local value = assert(fields[index], "packet overread")
                assert(value[1] == kind, "expected " .. value[1] .. ", reader requested " .. kind)
                return value[2]
            end
            result["w_" .. kind] = function(self, value) out[#out + 1] = {kind, value} end
        end
        result.complete = function() assert(index == #fields, "unread engine packet fields") end
        return result
    end
    -- CSE_ALifeObject::STATE_Write includes m_script_story_ID after the two numeric IDs.
    local fields = {{"u16", 1}, {"float", 2}, {"s32", 1}, {"s32", 3}, {"s32", 4},
                    {"stringZ", "custom"}, {"s32", 5}, {"s32", 6}, {"stringZ", "heli_story"}}
    local input = packet(fields)
    local state = parse_cse_alife_object_properties_packet({}, input)
    input.complete()
    local output = packet({})
    fill_cse_alife_object_properties_packet(state, output)
    for index, field in ipairs(fields) do
        assert(output.written[index][1] == field[1] and output.written[index][2] == field[2])
    end
    -- Empty and nonempty story IDs are both part of the current engine format.
    fields[9][2] = ""
    input = packet(fields)
    state = parse_cse_alife_object_properties_packet({}, input)
    input.complete()
    output = packet({})
    fill_cse_alife_object_properties_packet(state, output)
    assert(output.written[9][2] == "")

    -- Current skeleton source ID is u32. Optional SPHBonesData must survive too.
    -- net_Save: mask8 + root2 + min/max24, count2, optional mask8, 8 bytes/bone.
    for _, count in ipairs({-1, 0, 1, 65}) do
        fields = {{"stringZ", "idle"}, {"u8", count < 0 and 0 or 4}, {"u32", 70000}}
        if count >= 0 then
            for i = 1, 34 do fields[#fields + 1] = {"u8", i} end
            fields[#fields + 1] = {"u16", count}
            for i = 1, count * 8 + (count > 64 and 8 or 0) do
                fields[#fields + 1] = {"u8", i % 256}
            end
        end
        input = packet(fields)
        state = parse_cse_ph_skeleton_properties_packet({}, input)
        input.complete()
        output = packet({})
        fill_cse_ph_skeleton_properties_packet(state, output)
        assert(#output.written == #fields, "skeleton payload size changed")
        for index, field in ipairs(fields) do
            assert(output.written[index][1] == field[1] and output.written[index][2] == field[2])
        end
    end
end)
assert(failures == 0, tostring(failures) .. " CoC compatibility checks failed")
