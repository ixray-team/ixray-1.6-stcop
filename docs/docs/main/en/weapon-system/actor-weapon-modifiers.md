# Actor weapon modifiers

The actor owns runtime weapon coefficients, initially `1.0`. Held firearms read
the current owner's coefficients when firing in single player. NPC weapons,
multiplayer and other shooting objects use neutral coefficients. Dropping or
transferring a weapon does not carry the previous owner's modifiers with it.

`hit_power_coeff` multiplies the current difficulty-specific bullet hit power,
including weapon upgrades, before the existing silencer and cartridge factors.
`rpm_coeff` divides the shot interval in both magazine and chamber firing paths,
including single-shot and initial-burst RPM settings. It does not accelerate
reload/pump animations or change explosive projectile damage or melee attacks.
Stored weapon parameters remain unchanged.

## Lua API

```lua
-- Read by name or enum.
local hit = db.actor:weapons_modifiers("hit_power_coeff")
local rpm = db.actor:weapons_modifiers(weaponsModifiers.RPM_COEFF)

-- Set an absolute coefficient; the setter returns true on success.
local changed = db.actor:weapons_modifiers("hit_power_coeff", 1.20)
db.actor:weapons_modifiers(weaponsModifiers.RPM_COEFF, 1.50)

-- Restore vanilla behavior.
db.actor:weapons_modifiers(weaponsModifiers.HIT_POWER_COEFF, 1.0)
db.actor:weapons_modifiers("rpm_coeff", 1.0)
```

Enum members are `weaponsModifiers.HIT_POWER_COEFF` and
`weaponsModifiers.RPM_COEFF`. Hit power accepts finite values >= 0; RPM accepts
finite values > 0. Invalid names, enum values, coefficients or non-actor objects
are logged; setters return `false` without changing state, and getters return
`1.0`. An unrepresentable shot interval falls back to its original value.

Coefficients reset on actor initialization and are not serialized. Reapply them
from persistent gameplay state on `actor_on_first_update` after each spawn/load.
No weapon switch, inventory scan or per-shot infoportion lookup is needed.

## SGM example

Copy [sgm_weapon_modifiers.script](../../../../examples/sgm_weapon_modifiers.script) into
the SGM addon's `scripts/` directory and fully restart the game to refresh the
Framework autoloader cache. It registers only `actor_on_first_update` and
`actor_on_info_callback`. The latter filters for `sgm_achievements_sniper`.
Both events assign `1 + achievements_sniper_bonus` (normally `1.20`) when the
achievement exists, or `1.0` on the first update without it. The stored
infoportion restores the bonus after loading an existing save. Repeated events
do not multiply an already applied bonus. RPM remains available to other mods.

The script safely does nothing with older binaries lacking `weapons_modifiers`.
The old `actor_base_hit_power_bonus` and `actor_base_hit_power_bonus_info` weapon
keys are no longer read by the new engine. The bonus now scales upgraded damage:
base `0.50`, upgraded `0.70`, coefficient `1.20` gives `0.84`, rather than adding
`0.10` from the original base. Mods displaying damage should use the same formula.

The stock info callback reports receipt, not removal. A mod that revokes this
normally permanent achievement must also set `hit_power_coeff` back to `1.0`,
or reapply its own modifiers; the first-update hook restores it on the next load.
This example owns the hit-power coefficient. Multiple independent bonus providers
must compose their final coefficient before calling the setter.

## Verification

The standalone native tests do not start the engine:

```powershell
cmake -S src/test/weapons_modifiers -B build/weapons-modifiers-tests -A x64
cmake --build build/weapons-modifiers-tests --config Release
ctest --test-dir build/weapons-modifiers-tests -C Release --output-on-failure
luajit src/test/weapons_modifiers/SgmWeaponModifiersTests.lua docs/examples/sgm_weapon_modifiers.script
```

Also build `xrGame` to validate the actor, weapon and Lua binding integration.
In-game acceptance requires a full restart and checks with/without the achievement,
save/load, upgraded weapons, NPC ownership, and both firing paths. Source checks,
the native value tests and mocked Lua callbacks do not replace those game checks.
