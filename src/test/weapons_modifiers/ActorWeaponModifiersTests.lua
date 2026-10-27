local script_path = assert(arg[1], "Pass the actor weapon modifiers example path")
local callbacks = {}
local registrations = 0
local writes = 0
local info_checks = 0
local actor = { trained = false, hit_power_coeff = 1.0, recoil_coeff = 1.0, rpm_coeff = 1.0 }

function actor:has_info(info)
    assert(info == "example_weapon_training")
    info_checks = info_checks + 1
    return self.trained
end

function actor:weapons_modifiers(name, value)
    assert(name == "hit_power_coeff" or name == "recoil_coeff")
    self[name] = value
    writes = writes + 1
    return true
end

local environment = setmetatable({
    db = { actor = actor },
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
assert(actor.hit_power_coeff == 1.0 and actor.recoil_coeff == 1.0 and writes == 2)
callbacks.actor_on_info_callback(actor, "unrelated_info")
assert(writes == 2 and info_checks == 1, "Unrelated info must not trigger work")
actor.trained = true
callbacks.actor_on_info_callback(actor, "example_weapon_training")
assert(actor.hit_power_coeff == 1.2 and actor.recoil_coeff == 0.8 and actor.rpm_coeff == 1.0)
callbacks.actor_on_info_callback(actor, "example_weapon_training")
assert(actor.hit_power_coeff == 1.2 and actor.recoil_coeff == 0.8, "Repeated receipt must not compound bonuses")

actor.hit_power_coeff = 1.0
actor.recoil_coeff = 1.0
callbacks.actor_on_first_update()
assert(actor.hit_power_coeff == 1.2 and actor.recoil_coeff == 0.8, "Existing saves must restore bonuses")
actor.trained = false
callbacks.actor_on_first_update()
assert(actor.hit_power_coeff == 1.0 and actor.recoil_coeff == 1.0, "A save without the infoportion must reset stale state")
actor.trained = true
environment.refresh()
actor.trained = false
environment.refresh()
assert(actor.hit_power_coeff == 1.0 and actor.recoil_coeff == 1.0, "Explicit refresh must handle infoportion removal")

local previous_writes = writes
environment.db.actor = nil
callbacks.actor_on_first_update()
callbacks.actor_on_info_callback(nil, "example_weapon_training")
environment.db.actor = { has_info = function() error("Old binaries must be skipped") end }
callbacks.actor_on_first_update()
assert(writes == previous_writes)
print("Actor modifier example passed: startup, registration, first update, info filtering, save/load, idempotence, removal, missing actor and old binaries")
