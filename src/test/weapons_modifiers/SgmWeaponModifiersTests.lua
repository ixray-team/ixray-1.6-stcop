local script_path = assert(arg[1], "Pass the SGM script path")
local callbacks = {}
local registrations = 0
local writes = 0
local info_checks = 0
local bonus = 0.20
local actor = { achievement = false, hit = 1.0, rpm = 1.0 }

function actor:has_info(info)
    assert(info == "sgm_achievements_sniper")
    info_checks = info_checks + 1
    return self.achievement
end

function actor:weapons_modifiers(name, value)
    assert(name == "hit_power_coeff")
    self.hit = value
    writes = writes + 1
    return true
end

local environment = setmetatable({
    db = { actor = actor },
    r_mod_params = function(kind, name, fallback)
        assert(kind == "number" and name == "achievements_sniper_bonus" and fallback == 0.20)
        return bonus
    end,
    RegisterScriptCallback = function(name, handler)
        assert(callbacks[name] == nil, "Duplicate callback registration")
        registrations = registrations + 1
        callbacks[name] = handler
    end,
}, { __index = _G })

setfenv(assert(loadfile(script_path)), environment)()
environment.on_game_start("data")
environment.on_game_start("data")
assert(registrations == 2 and callbacks.actor_on_update == nil)
assert(writes == 0 and info_checks == 0, "Startup must not read actor state")

callbacks.actor_on_first_update()
assert(actor.hit == 1.0 and writes == 1)
callbacks.actor_on_info_callback(actor, "unrelated_info")
assert(writes == 1 and info_checks == 1, "Unrelated info must not trigger work")
actor.achievement = true
callbacks.actor_on_info_callback(actor, "sgm_achievements_sniper")
assert(actor.hit == 1.2 and actor.rpm == 1.0)
callbacks.actor_on_info_callback(actor, "sgm_achievements_sniper")
assert(actor.hit == 1.2, "Repeated receipt must not compound the bonus")

actor.hit = 1.0
callbacks.actor_on_first_update()
assert(actor.hit == 1.2, "Existing saves must restore the achievement bonus")
actor.achievement = false
callbacks.actor_on_first_update()
assert(actor.hit == 1.0, "A save without the achievement must reset stale state")
actor.achievement = true
bonus = 0.35
callbacks.actor_on_first_update()
assert(actor.hit == 1.35, "Honor the SGM bonus configuration")

local previous_writes = writes
environment.db.actor = nil
callbacks.actor_on_first_update()
callbacks.actor_on_info_callback(nil, "sgm_achievements_sniper")
environment.db.actor = { has_info = function() error("Old binaries must be skipped") end }
callbacks.actor_on_first_update()
assert(writes == previous_writes)
print("SGM modifier callbacks passed: startup, duplicate registration, first update, info filtering, save/load, idempotence, missing actor and old binaries")
