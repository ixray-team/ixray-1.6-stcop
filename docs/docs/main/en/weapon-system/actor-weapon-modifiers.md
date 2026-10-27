# Actor weapon modifiers

The actor owns runtime weapon coefficients, initially `1.0`. Held firearms read
the current owner's coefficients when firing in single player. NPC weapons,
multiplayer and other shooting objects use neutral coefficients. Dropping or
transferring a weapon does not carry the previous owner's modifiers with it.

| Lua name | Enum member in `weaponsModifiers` | Effect |
| --- | --- | --- |
| `hit_power_coeff` | `HIT_POWER_COEFF` | Common bullet damage multiplier, with or without a silencer. |
| `rpm_coeff` | `RPM_COEFF` | Rate of fire multiplier. Higher values shorten the shot interval. |
| `silencer_hit_power_coeff` | `SILENCER_HIT_POWER_COEFF` | Additional damage multiplier only when firing with a silencer. |
| `unsilenced_hit_power_coeff` | `UNSILENCED_HIT_POWER_COEFF` | Additional damage multiplier only when firing without a silencer. |
| `hit_impulse_coeff` | `HIT_IMPULSE_COEFF` | Bullet impact impulse transferred to the target, including body knockback; not weapon recoil. |
| `fire_dispersion_base_coeff` | `FIRE_DISPERSION_BASE_COEFF` | Base weapon dispersion multiplier. Lower values improve accuracy. |
| `crosshair_inertion_coeff` | `CROSSHAIR_INERTION_COEFF` | Crosshair settling inertia multiplier (handling). Lower values shorten settling time. |
| `bullet_speed_coeff` | `BULLET_SPEED_COEFF` | Initial bullet speed multiplier (trajectory flatness). Higher values increase speed. |

Damage is the current difficulty-specific hit power (including upgrades), times
`hit_power_coeff`, times exactly one of the silenced/unsilenced coefficients,
times the existing silencer and cartridge factors. Silencer selection follows
the weapon's firing mode, including built-in silencers, and updates when the
attachment changes. The common coefficient retains its previous meaning.

Impact impulse includes the existing silencer and cartridge impulse factors.
Actual body motion also depends on the target's physics and hit location.
Accuracy scales base dispersion including condition, silencer and cartridge
factors; the shooter's additional movement dispersion remains separate. The
initial-burst base-dispersion path uses the same calculation. Handling changes
the existing crosshair interpolation parameter, not recoil, mouse sensitivity
or animation speed. In single player, bullet spread is calculated directly;
crosshair settling does not add a separate delay to shot accuracy.

Bullet speed is scaled after the existing silencer factor, including alternate
initial-burst speeds. Gravity, air resistance and maximum travel distance retain
their configured values; flatness is changed through speed, not a range bonus.

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

-- Independent effects; all start at 1.0.
db.actor:weapons_modifiers("silencer_hit_power_coeff", 1.10)
db.actor:weapons_modifiers("unsilenced_hit_power_coeff", 1.05)
db.actor:weapons_modifiers(weaponsModifiers.HIT_IMPULSE_COEFF, 1.50)
db.actor:weapons_modifiers("fire_dispersion_base_coeff", 0.80)
db.actor:weapons_modifiers("crosshair_inertion_coeff", 0.80)
db.actor:weapons_modifiers("bullet_speed_coeff", 1.20)

-- Reset these two coefficients; reset any other changed coefficients separately.
db.actor:weapons_modifiers(weaponsModifiers.HIT_POWER_COEFF, 1.0)
db.actor:weapons_modifiers("rpm_coeff", 1.0)
```

All coefficients accept finite values >= 0, except RPM and bullet speed, which
require values > 0. The original enum values for hit power and RPM are unchanged.
Invalid names, enum values, coefficients or non-actor objects
are logged; setters return `false` without changing state, and getters return
`1.0`. An unrepresentable shot interval or bullet speed falls back to its original
value. A zero impulse disables the bullet's impact impulse; zero base dispersion
removes only the weapon's dispersion contribution; zero inertion removes the
crosshair settling delay.

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
do not multiply an already applied bonus. All other coefficients remain available
to other mods; the example does not change them or assign extra achievement rewards.

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
save/load, upgraded weapons, NPC ownership, both firing paths, silencer attachment
and removal, built-in silencers, target impact impulse, dispersion, crosshair
settling and bullet trajectories. Source checks,
the native value tests and mocked Lua callbacks do not replace those game checks.
