# Actor weapon modifiers

[Русская версия](/weapon-system/actor-weapon-modifiers)

Weapon modifiers let Lua change the actor's damage, rate of fire, accuracy and other weapon characteristics without rewriting weapon configuration or upgrade values. All coefficients start at **`1.0`**. They belong to the actor and apply to the actor's currently owned weapons in **single player**.

A value of `1.20` means a multiplier of 1.2, not an additive bonus of 1.2. Lower dispersion, inertion and recoil values improve those characteristics. These coefficients are runtime values, not weapon LTX keys.

## Quick start

Run this after the actor exists, for example from `actor_on_first_update`. The guard also supports older binaries that do not expose this API.

```lua
local actor = db.actor
if not actor or not actor.weapons_modifiers then
    return
end

local damage = actor:weapons_modifiers(weaponsModifiers.HIT_POWER_COEFF)
local recoil = actor:weapons_modifiers(weaponsModifiers.RECOIL_COEFF)

assert(actor:weapons_modifiers(weaponsModifiers.HIT_POWER_COEFF, 1.20))
assert(actor:weapons_modifiers(weaponsModifiers.RECOIL_COEFF, 0.75))
```

The method has two forms:

| Call | Result |
| --- | --- |
| `actor:weapons_modifiers(modifier)` | Current coefficient as a number. |
| `actor:weapons_modifiers(modifier, value)` | Assigns an absolute coefficient; returns `true` on success, `false` on rejection. |

Pass a constant from `weaponsModifiers` as the modifier. String names are not supported; prefer symbolic constants over numeric enum IDs. Repeating the same assignment does not stack bonuses. String-based calls no longer have a matching Lua overload; they are not rejected setters returning `false`.

## Coefficient reference

| Enum in `weaponsModifiers` | Effect |
| --- | --- |
| `HIT_POWER_COEFF` | Common bullet damage multiplier. |
| `RPM_COEFF` | Rate of fire multiplier; higher values shorten shot intervals. |
| `SILENCER_HIT_POWER_COEFF` | Additional damage multiplier for shots with a silencer. |
| `UNSILENCED_HIT_POWER_COEFF` | Additional damage multiplier for shots without a silencer. |
| `HIT_IMPULSE_COEFF` | Impact impulse transferred to the target, including body knockback. |
| `FIRE_DISPERSION_BASE_COEFF` | Base weapon dispersion; lower values improve accuracy. |
| `CROSSHAIR_INERTION_COEFF` | Crosshair settling inertia; lower values shorten settling time. |
| `BULLET_SPEED_COEFF` | Initial bullet speed, which affects trajectory flatness. |
| `RECOIL_COEFF` | Vertical and horizontal shot recoil strength; lower values reduce the kick. |

All values must be finite. RPM and bullet speed require **`> 0`**; every other coefficient accepts **`>= 0`**. Negative values, NaN, infinity, invalid enum values and calls on non-actor objects are rejected and logged. Rejected setters preserve the old value; invalid getters return `1.0`. That return value is not a feature-detection mechanism: use the method-presence check shown above.

Zero damage disables that damage contribution. Zero impact impulse removes the bullet's impact impulse. Zero base dispersion removes the weapon's spread contribution, while the actor's extra spread remains. Zero inertion removes crosshair settling delay. Zero recoil suppresses new shot kicks; an existing recoil effect can continue settling.

## Usage examples

The following snippets are independent recipes. Run them when `db.actor` and the API are available, as checked in Quick start. They affect only the parameters shown; other active coefficients remain in effect. Choose values for your mod rather than applying every recipe together.

### Damage with and without a silencer

```lua
local actor = db.actor
actor:weapons_modifiers(weaponsModifiers.HIT_POWER_COEFF, 1.20)
actor:weapons_modifiers(weaponsModifiers.SILENCER_HIT_POWER_COEFF, 1.10)
actor:weapons_modifiers(weaponsModifiers.UNSILENCED_HIT_POWER_COEFF, 1.05)
```

The effective actor damage factor is `HIT_POWER_COEFF × selected_damage_coeff`. Exactly one additional factor is selected by the firing mode, including built-in silencers. With the values above, the actor factors are `1.32` with a silencer and `1.26` without one. Attachment changes are handled by the engine.

For current upgraded hit power `0.70`, the results before existing silencer/cartridge factors are `0.924` and `0.882`. The coefficient scales current difficulty-specific damage, not an old copy of the base damage. Display scripts should use the same composition.

### A precision profile

```lua
local actor = db.actor
actor:weapons_modifiers(weaponsModifiers.FIRE_DISPERSION_BASE_COEFF, 0.80)
actor:weapons_modifiers(weaponsModifiers.CROSSHAIR_INERTION_COEFF, 0.75)
actor:weapons_modifiers(weaponsModifiers.BULLET_SPEED_COEFF, 1.20)
actor:weapons_modifiers(weaponsModifiers.RECOIL_COEFF, 0.50)
```

This reduces base weapon spread by 20%, shortens crosshair settling, increases initial speed by 20% and reduces shot recoil. The actor's additional movement spread is separate. In single player, crosshair settling controls the indicator; it does not delay the bullet-spread calculation.

### Faster fire with a damage tradeoff

```lua
local actor = db.actor
actor:weapons_modifiers(weaponsModifiers.RPM_COEFF, 1.25)
actor:weapons_modifiers(weaponsModifiers.HIT_POWER_COEFF, 0.90)
```

A configured `600 RPM` becomes `750 RPM`: `0.10 / 1.25 = 0.08` seconds per shot. Magazine/chamber firing and alternate single-shot/initial-burst intervals use this factor. Reload and pump animation durations are not accelerated.

When `RPM_COEFF` differs from `1.0`, a valid calculated shot interval is limited to at least **1 ms** (1000 shots per second). This prevents extreme coefficients from stalling the floating-point shot timer or producing an excessive number of catch-up shots in one frame. The neutral coefficient preserves the original interval exactly, including unusually short configured intervals.

### Target impact impulse

```lua
local actor = db.actor
actor:weapons_modifiers(weaponsModifiers.HIT_IMPULSE_COEFF, 1.50)
-- Set to 0.0 to remove the bullet's impact impulse.
-- This does not change RECOIL_COEFF or HIT_POWER_COEFF.
```

This is the impulse delivered on impact, not weapon recoil. Existing silencer and cartridge impulse factors still apply. Actual body movement also depends on physics, hit location and the target.

### Handling a rejected assignment

```lua
local actor = db.actor
local previous = actor:weapons_modifiers(weaponsModifiers.RPM_COEFF)
local accepted = actor:weapons_modifiers(weaponsModifiers.RPM_COEFF, 0.0)
assert(accepted == false)
assert(actor:weapons_modifiers(weaponsModifiers.RPM_COEFF) == previous)

assert(actor:weapons_modifiers(weaponsModifiers.RECOIL_COEFF, -1.0) == false)
```

These deliberate invalid calls write error messages to the script log. In production, validate your mod's input and check the setter result. The setter does not clamp invalid coefficients into the accepted range. An unrepresentable shot interval or bullet speed falls back to its original value.

### Reset one coefficient or the complete profile

```lua
local actor = db.actor
actor:weapons_modifiers(weaponsModifiers.RECOIL_COEFF, 1.0)

-- Full reset: use only when this script owns the entire profile.
local modifiers = {
    weaponsModifiers.HIT_POWER_COEFF, weaponsModifiers.RPM_COEFF,
    weaponsModifiers.SILENCER_HIT_POWER_COEFF, weaponsModifiers.UNSILENCED_HIT_POWER_COEFF,
    weaponsModifiers.HIT_IMPULSE_COEFF, weaponsModifiers.FIRE_DISPERSION_BASE_COEFF,
    weaponsModifiers.CROSSHAIR_INERTION_COEFF, weaponsModifiers.BULLET_SPEED_COEFF, weaponsModifiers.RECOIL_COEFF,
}
for _, modifier in ipairs(modifiers) do
    actor:weapons_modifiers(modifier, 1.0)
end
```

The API has no reset-all method. A full reset can overwrite another system's effects; a script that owns only recoil should reset only `RECOIL_COEFF`.

### Combine several bonus sources without stacking twice

Keep each source's factor under a stable ID and recompute the final product. Updating or removing one source then preserves the others.

```lua
local damage_sources = {}

function set_damage_source(id, coefficient)
    damage_sources[id] = coefficient
    local combined = 1.0
    for _, value in pairs(damage_sources) do
        combined = combined * value
    end

    local actor = db.actor
    if actor and actor.weapons_modifiers then
        actor:weapons_modifiers(weaponsModifiers.HIT_POWER_COEFF, combined)
    end
end

set_damage_source("training", 1.20)
set_damage_source("equipment", 1.10) -- 1.32
set_damage_source("training", 1.20) -- Still 1.32
set_damage_source("temporary", 1.25) -- 1.65
set_damage_source("temporary", nil) -- Back to 1.32
```

Do not repeatedly read the current coefficient and multiply it by the same bonus from update callbacks. That compounds it on every call. A single coordinator should own each final coefficient; the setter replaces the existing value and does not track its sources.

The final damage coefficient is saved by the engine, but the `damage_sources` table above is Lua runtime state and is not saved automatically. Rebuild that table from saved quest, equipment and effect state before changing individual sources after a load; otherwise, the first update can replace the restored product with an incomplete set of bonuses. Call the same source-removal function when a temporary effect expires.

## Saving and loading

The engine saves the final values of all nine coefficients with the actor. A successful setter changes the current session; the next game save records that value. Loading that save restores the coefficients before the actor's Lua binder is loaded. No separate Lua save/load handler is needed just to retain these values.

| Situation | Coefficients |
| --- | --- |
| New actor without saved modifier data | All values start at `1.0`. |
| Load a save containing the modifier block | Stored values are restored. |
| Load an otherwise compatible save created before the modifier block was added | All values remain `1.0`; scripts may assign a profile afterward. |
| Load a stored coefficient outside its allowed range | That coefficient remains `1.0`, with a message in the engine log. |

Compatibility here concerns only the added modifier block; it does not make unrelated save-format changes compatible. Saves written with this block are not supported by older engines that do not recognize it. Keep a separate older save when switching back to such a build.

A method-presence check detects the Lua API, not persistence support: earlier builds exposed `weapons_modifiers` without saving its values. For scripts using the former string syntax, replace the first argument with its `weaponsModifiers.*` constant.

The engine stores the resulting coefficients, not the sources of bonuses, timers or effect ownership. A temporary coefficient remains in a save until a script changes it. Persist the effect's expiry state separately and remove or recompute its contribution when the effect ends.

The event example below recalculates two coefficients from a saved infoportion. This is a gameplay policy for that example, not a requirement for restoring the serialized coefficients. Do not unconditionally reset all coefficients on the first update if their saved values should remain in effect.

## Apply a persistent bonus through events

Coefficients start at `1.0` and are **saved and restored with the actor**. Saves without the modifier block use neutral values of `1.0`. The first-update handler below reconciles the restored coefficients with the training infoportion; afterward, update them only when that state changes. No inventory scan, per-frame polling or per-shot infoportion lookup is required.

The complete example below owns only common damage and recoil: training grants `1.20` damage and `0.80` recoil; otherwise those two values are reset to `1.0`.

1. Save the code below as `actor_weapon_modifiers.script` in your mod's `scripts/` directory.
2. Replace `example_weapon_training` with an infoportion declared by your mod, or declare that ID in an XML file registered under `[info_portions]` in `configs/system.ltx`.
3. Use an IX-Ray Framework setup providing `RegisterScriptCallback`, `actor_on_first_update` and `actor_on_info_callback`, then fully restart to refresh script discovery.
4. Grant the infoportion through your quest logic. Existing saves restore the effect on their first update.

```lua
-- Install in your mod's scripts directory; IX-Ray Framework loads on_game_start.
-- Replace this ID with an infoportion declared by your mod.
local TRAINING_INFO = "example_weapon_training"
local registered = false

function refresh()
    local actor = db.actor
    if not actor or not actor.weapons_modifiers then
        return
    end

    local damage = 1.0
    local recoil = 1.0
    if actor:has_info(TRAINING_INFO) then
        damage = 1.20
        recoil = 0.80
    end

    -- Assign an absolute coefficient: repeated events must not compound the bonus.
    actor:weapons_modifiers(weaponsModifiers.HIT_POWER_COEFF, damage)
    actor:weapons_modifiers(weaponsModifiers.RECOIL_COEFF, recoil)
end

local function on_actor_info(npc, info_id)
    if info_id == TRAINING_INFO then
        refresh()
    end
end

function on_game_start(data)
    if registered then
        return
    end

    RegisterScriptCallback("actor_on_first_update", refresh)
    RegisterScriptCallback("actor_on_info_callback", on_actor_info)
    registered = true
end
```

`on_game_start` registers handlers and does not read actor state. The first-update handler reconciles coefficients with the saved training state. The info handler filters unrelated events; repeated receipt assigns the same absolute values. Registration is guarded against duplicate startup calls.

The stock info callback reports receipt, not removal. Revoke the flag and refresh explicitly:

```lua
local actor = db.actor
actor:disable_info_portion("example_weapon_training")
actor_weapon_modifiers.refresh()
```

If another script also owns damage or recoil, integrate this event handler with the source-composition approach instead of letting both overwrite the same coefficient. Framework callback wiring is a script-layer facility; the engine API itself only stores and applies coefficients.

## Scope and calculation details

- Weapons read their current owner's coefficients. Switching weapons needs no reapplication; dropping or transferring a weapon does not transfer the actor's bonus. NPC weapons, multiplayer and other shooting objects use neutral coefficients.
- Damage and impact impulse keep existing upgrade, silencer and cartridge factors. These bullet modifiers do not change explosive projectile damage or melee hits.
- Accuracy scales base dispersion with condition, silencer and cartridge factors, including the initial-burst base-dispersion path. The actor's additional spread is calculated separately.
- Bullet speed includes the current silencer factor and alternate initial-burst speed. Gravity, air resistance and maximum travel distance retain their configured values; there is no separate flatness coefficient.
- Handling changes crosshair interpolation, not mouse sensitivity, recoil or animation speed.
- Recoil covers pattern and legacy systems, hip-fire and aiming, in both axes. It scales the new shot's kick while retaining existing angle/step/velocity limits. The neutral coefficient preserves the original result, including legacy limit jitter. Return speed, spring/damping settings and weapon animations are independent.

## Verification

Build `xrGame` after C++ or binding changes. Runtime acceptance still requires checks in game: save/load, repeated events, bonus removal, upgraded weapons, silencer attachment/removal, NPC ownership, both firing paths, and pattern/legacy recoil.
