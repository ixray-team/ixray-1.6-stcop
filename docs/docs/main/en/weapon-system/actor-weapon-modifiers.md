# Living entity weapon modifiers

[Русская версия](/weapon-system/actor-weapon-modifiers)

Weapon modifiers let Lua change damage, rate of fire, accuracy and other weapon characteristics without rewriting weapon configuration or upgrade values. Every `CEntityAlive`, including actors and NPCs, owns a separate set of coefficients initialized to **`1.0`**. Bullet damage, speed, impulse, base dispersion and shot intervals use the weapon owner's coefficients. Camera recoil and crosshair inertion use the actor's inherited coefficients.

A value of `1.20` means a multiplier of 1.2, not an additive bonus of 1.2. Lower dispersion, inertion and recoil values improve those characteristics. These coefficients are runtime values, not weapon LTX keys.

## Quick start

Run this when granting or changing a bonus, after the actor exists. The guard also supports older binaries that do not expose this API.

```lua
local actor = db.actor
if not actor or not actor.set_weapon_modifier then
    return
end

local damage = actor:get_weapon_modifier(wpn_modifiers.hit_power)
local recoil = actor:get_weapon_modifier(wpn_modifiers.recoil)

assert(actor:set_weapon_modifier(wpn_modifiers.hit_power, 1.20))
assert(actor:set_weapon_modifier(wpn_modifiers.recoil, 0.75))
```

The API has separate getter and setter methods:

| Call | Result |
| --- | --- |
| `actor:get_weapon_modifier(modifier)` | Current coefficient as a number. |
| `actor:set_weapon_modifier(modifier, value)` | Assigns an absolute coefficient; returns `true` on success, `false` on rejection. |

Pass a constant from `wpn_modifiers` as the modifier. String names are not supported; prefer symbolic constants over numeric enum IDs. Repeating the same assignment does not stack bonuses. String-based calls no longer have a matching Lua overload; they are not rejected setters returning `false`.

## Coefficient reference

| Enum in `wpn_modifiers` | Effect |
| --- | --- |
| `hit_power` | Common bullet damage multiplier. |
| `rpm` | Rate of fire multiplier; higher values shorten shot intervals. |
| `SILENCER_hit_power` | Additional damage multiplier for shots with a silencer. |
| `UNSILENCED_hit_power` | Additional damage multiplier for shots without a silencer. |
| `hit_impulse` | Impact impulse transferred to the target, including body knockback. |
| `fire_dispersion_base` | Base weapon dispersion; lower values improve accuracy. |
| `crosshair_inertion` | Crosshair settling inertia; lower values shorten settling time. |
| `bullet_speed` | Initial bullet speed, which affects trajectory flatness. |
| `recoil` | Vertical and horizontal shot recoil strength; lower values reduce the kick. |

The setter applies `clamp(Value, 0.001f, FLT_MAX)` once before storing the coefficient. Values below `0.001`, including zero and negative values, become `0.001`; values above `FLT_MAX`, including positive infinity, become `FLT_MAX`. The standard clamp does not normalize NaN; callers must not supply NaN. Lua validates that the object derives from `CEntityAlive` and that the enum ID is valid. Invalid calls are logged; invalid getters return `1.0` and invalid setters return `false`.

The same methods work on NPCs:

```lua
db.actor:set_weapon_modifier(wpn_modifiers.rpm, 1.25)
npc_gobj:set_weapon_modifier(wpn_modifiers.rpm, 1.25)
local npc_rpm = npc_gobj:get_weapon_modifier(wpn_modifiers.rpm)
```

`rpm` is a multiplier: `650.0` means 650 times the configured rate, not an absolute rate of 650 rounds per minute.

The minimum coefficient is `0.001`: it reduces the corresponding contribution to one thousandth rather than disabling it. Base dispersion still excludes the actor's extra spread only in the configured initial-burst path; the normal firing path adds actor spread separately.

## Usage examples

The following snippets are independent recipes. Run them when `db.actor` and the API are available, as checked in Quick start. They affect only the parameters shown; other active coefficients remain in effect. Choose values for your mod rather than applying every recipe together.

### Damage with and without a silencer

```lua
local actor = db.actor
actor:set_weapon_modifier(wpn_modifiers.hit_power, 1.20)
actor:set_weapon_modifier(wpn_modifiers.SILENCER_hit_power, 1.10)
actor:set_weapon_modifier(wpn_modifiers.UNSILENCED_hit_power, 1.05)
```

The effective actor damage factor is `hit_power × selected_damage_coeff`. Exactly one additional factor is selected by the firing mode, including built-in silencers. With the values above, the actor factors are `1.32` with a silencer and `1.26` without one. Attachment changes are handled by the engine.

For current upgraded hit power `0.70`, the results before existing silencer/cartridge factors are `0.924` and `0.882`. The coefficient scales current difficulty-specific damage, not an old copy of the base damage. Display scripts should use the same composition.

### A precision profile

```lua
local actor = db.actor
actor:set_weapon_modifier(wpn_modifiers.fire_dispersion_base, 0.80)
actor:set_weapon_modifier(wpn_modifiers.crosshair_inertion, 0.75)
actor:set_weapon_modifier(wpn_modifiers.bullet_speed, 1.20)
actor:set_weapon_modifier(wpn_modifiers.recoil, 0.50)
```

This reduces base weapon spread by 20%, shortens crosshair settling, increases initial speed by 20% and reduces shot recoil. The actor's additional movement spread is separate in the normal firing path; the configured initial-burst base-dispersion path uses only the modified weapon dispersion. In single player, crosshair settling controls the indicator; it does not delay the bullet-spread calculation.

### Faster fire with a damage tradeoff

```lua
local actor = db.actor
actor:set_weapon_modifier(wpn_modifiers.rpm, 1.25)
actor:set_weapon_modifier(wpn_modifiers.hit_power, 0.90)
```

A configured `600 RPM` becomes `750 RPM`: `0.10 / 1.25 = 0.08` seconds per shot. Magazine/chamber firing and alternate single-shot/initial-burst intervals use this factor. Reload and pump animation durations are not accelerated.

The shot timer uses the configured interval divided directly by the owner's `rpm` coefficient, limited to at least **1 ms**. This bounds the interval used by the shot timer; it is not a global limit of 1000 bullets per second, because a shot can emit multiple pellets and the double-barrel path can discharge two rounds in one firing iteration. Actual rate also depends on the firing path, frame timing and weapon state. The minimum also applies to unusually short configured intervals with a neutral coefficient.

### Target impact impulse

```lua
local actor = db.actor
actor:set_weapon_modifier(wpn_modifiers.hit_impulse, 1.50)
-- Use 0.001 for the minimum impact impulse coefficient.
-- This does not change recoil or hit_power.
```

This is the impulse delivered on impact, not weapon recoil. Existing silencer and cartridge impulse factors still apply. Actual body movement also depends on physics, hit location and the target.

### Clamping an assigned value

```lua
local actor = db.actor
assert(actor:set_weapon_modifier(wpn_modifiers.rpm, -1.0))
assert(math.abs(actor:get_weapon_modifier(wpn_modifiers.rpm) - 0.001) < 1e-8)

assert(actor:set_weapon_modifier(wpn_modifiers.recoil, -1.0))
assert(math.abs(actor:get_weapon_modifier(wpn_modifiers.recoil) - 0.001) < 1e-8)
```

Assignments outside the numeric range are clamped rather than rejected.

### Reset one coefficient or the complete profile

```lua
local actor = db.actor
actor:set_weapon_modifier(wpn_modifiers.recoil, 1.0)

-- Full reset: use only when this script owns the entire profile.
local modifiers = {
    wpn_modifiers.hit_power, wpn_modifiers.rpm,
    wpn_modifiers.SILENCER_hit_power, wpn_modifiers.UNSILENCED_hit_power,
    wpn_modifiers.hit_impulse, wpn_modifiers.fire_dispersion_base,
    wpn_modifiers.crosshair_inertion, wpn_modifiers.bullet_speed, wpn_modifiers.recoil,
}
for _, modifier in ipairs(modifiers) do
    actor:set_weapon_modifier(modifier, 1.0)
end
```

The API has no reset-all method. A full reset can overwrite another system's effects; a script that owns only recoil should reset only `recoil`.

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
    if actor and actor.set_weapon_modifier then
        actor:set_weapon_modifier(wpn_modifiers.hit_power, combined)
    end
end

set_damage_source("training", 1.20)
set_damage_source("equipment", 1.10) -- 1.32
set_damage_source("training", 1.20) -- Still 1.32
set_damage_source("temporary", 1.25) -- 1.65
set_damage_source("temporary", nil) -- Back to 1.32
```

Do not repeatedly read the current coefficient and multiply it by the same bonus from update callbacks. That compounds it on every call. A single coordinator should own each final coefficient; the setter replaces the existing value and does not track its sources.

The final damage coefficient is saved by the engine, but the `damage_sources` table above is Lua runtime state and is not saved automatically. Rebuild that table from saved quest, equipment and effect state before changing individual sources after a load; otherwise, the next source change can replace the restored product with an incomplete set of bonuses. Call the same source-removal function when a temporary effect expires.

## Saving and loading

The engine saves all nine coefficients with each living entity. A successful setter changes the current session; the next game save records that value. `CEntityAlive::load` restores them as part of the entity's inherited state. No separate Lua save/load handler is needed just to retain these values.

The save format contains exactly nine consecutive `float` values (36 bytes), in `WeaponModifier` enum order, at the end of `CEntityAlive::save` and `CEntityAlive::load`, after entity conditions. It has no signature or coefficient count. New entities start with all coefficients at `1.0`; loading assigns the stored values directly. `reinit()` preserves them. The actor has no additional copy of the block.

Earlier actor-only saves, saves without these nine values and saves from the signature/count format are incompatible. Legacy save migration is not supported; start a new game with this format. Moving the block between classes or changing the number or order of coefficients changes the save format.

Migrate earlier scripts to separate `get_weapon_modifier` and `set_weapon_modifier` calls and lowercase constants from `wpn_modifiers`. Only the new enum-based syntax is exposed.

The engine stores the resulting coefficients, not the sources of bonuses, timers or effect ownership. A temporary coefficient remains in a save until a script changes it. Persist the effect's expiry state separately and remove or recompute its contribution when the effect ends.

## Apply a persistent bonus through events

Coefficients start at `1.0` and are **saved and restored with the actor**. The handler below changes coefficients when the training infoportion is granted. Loading a save restores them from the entity packet without calling this handler. No inventory scan, per-frame polling or per-shot infoportion lookup is required.

The complete example below owns only common damage and recoil. When `refresh()` runs, it assigns `1.20` damage and `0.80` recoil if the training infoportion is present, or `1.0` for both if it is absent. An absent infoportion alone does not invoke the handler or reset restored coefficients.

1. Save the code below as `actor_weapon_modifiers.script` in your mod's `scripts/` directory.
2. Replace `example_weapon_training` with an infoportion declared by your mod, or declare that ID in an XML file registered under `[info_portions]` in `configs/system.ltx`.
3. Use an IX-Ray Framework setup providing `RegisterScriptCallback` and `actor_on_info_callback`, then fully restart to refresh script discovery.
4. Grant the infoportion through your quest logic to apply the bonus. Subsequent game saves retain the assigned coefficients.

```lua
-- Install in your mod's scripts directory; IX-Ray Framework loads on_game_start.
-- Replace this ID with an infoportion declared by your mod.
local TRAINING_INFO = "example_weapon_training"
local registered = false

function refresh()
    local actor = db.actor
    if not actor or not actor.set_weapon_modifier then
        return
    end

    local damage = 1.0
    local recoil = 1.0
    if actor:has_info(TRAINING_INFO) then
        damage = 1.20
        recoil = 0.80
    end

    -- Assign an absolute coefficient: repeated events must not compound the bonus.
    actor:set_weapon_modifier(wpn_modifiers.hit_power, damage)
    actor:set_weapon_modifier(wpn_modifiers.recoil, recoil)
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

    RegisterScriptCallback("actor_on_info_callback", on_actor_info)
    registered = true
end
```

`on_game_start` registers handlers and does not read actor state. The info handler filters unrelated events. Granting an infoportion already held by the actor does not invoke this callback: the engine rejects duplicate grants. Repeated explicit calls to `refresh()` assign the same absolute values. Registration is guarded against duplicate startup calls.

An infoportion already present in a loaded save does not itself apply the bonus. Loading a compatible save restores the recorded coefficients; it does not recompute them from infoportions.

The stock info callback reports receipt, not removal. Revoke the flag and refresh explicitly:

```lua
local actor = db.actor
actor:disable_info_portion("example_weapon_training")
actor_weapon_modifiers.refresh()
```

If another script also owns damage or recoil, integrate this event handler with the source-composition approach instead of letting both overwrite the same coefficient. Framework callback wiring is a script-layer facility; the engine API itself only stores and applies coefficients.

## Scope and calculation details

- Weapons bind directly to their living owner's coefficient storage when attached. Dropping, transferring or destroying a weapon releases that binding; changes to the entity coefficients are visible immediately. Shots do not look up or cast the owner. Shooting objects without a living owner use shared neutral coefficients.
- Damage and impact impulse keep existing upgrade, silencer and cartridge factors. These bullet modifiers do not change explosive projectile damage or melee hits.
- Accuracy scales base dispersion with condition, silencer and cartridge factors, including the initial-burst base-dispersion path. The normal firing path adds the actor's spread afterward. During the configured initial-burst base-dispersion shots, the weapon uses only modified base dispersion; the crosshair calculation still includes actor spread.
- Bullet speed includes the current silencer factor and alternate initial-burst speed. Gravity, air resistance and maximum travel distance retain their configured values; there is no separate flatness coefficient.
- Handling changes crosshair interpolation, not mouse sensitivity, recoil or animation speed.
- Recoil covers pattern and legacy systems, hip-fire and aiming, in both axes. It scales the new shot's kick while retaining existing angle/step/velocity limits. The neutral coefficient preserves the original result, including legacy limit jitter. Return speed, spring/damping settings and weapon animations are independent.

## Verification

Build `xrGame` after C++ or binding changes. Runtime acceptance still requires checks in game: save/load, repeated events, bonus removal, upgraded weapons, silencer attachment/removal, NPC ownership, both firing paths, and pattern/legacy recoil.
