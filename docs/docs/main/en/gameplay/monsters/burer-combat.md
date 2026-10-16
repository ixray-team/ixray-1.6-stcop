# Gunslinger burer combat

In single player, the presence of `[gunslinger_base]` enables the extended burer combat policy: super stamina hits, close knife protection, tactical shield selection and active grenade handling. Without this section the regular IX-Ray attack policy remains in use.

The behavior previously contained in `gunsl_burer.script` runs in C++. The script is no longer required. Install the engine and resource changes together: `burer_superhealthhit_type` now takes a regular hit-type **token**, such as `wound`, rather than the number `5`.

## Stamina hits and close protection

The normal stamina hit still uses weapon weight and the monster's `weight_to_stamina_hit`. A super hit replaces it when the actor is visible and close, has the inventory open, uses a HUD item (95% chance on Stalker difficulty or higher), carries sniper/explosive weapons, or aims toward the burer. Direct or narrow aim at the burer also permits a super hit. Visibility uses a collision ray. God mode skips the stamina attack.

A super hit damages a magazine-fed weapon by a random amount between `conditiondecmin` and `conditiondecmax`. Weapon dropping uses stamina **before** the hit and respects persistent slots. A left-hand device drops with the weapon.

At close range, an additional regular hit applies the configured damage type, power and impulse through the existing immunity/protection system. Knife protection can hide the actor's hands or play `anm_selfkick` during the knife window. This animation neither traces a normal knife strike nor enters the controller suicide state. Missing self-kick animation falls back to hand hiding.

A prepared grenade is released with reduced throwing force; an idle grenade is dropped from inventory. Native camera reactions distinguish gravi hits, weapon drops, risk under aim and stamina hits without a drop. The blood overlay uses the existing HUD custom static when available.

For 100 ms after a stamina hit, an actor above the configured health threshold can recover the configured stamina floor. Sniper and explosive weapons prevent this recovery. This period is independent of the knife reaction window.

## Attack selection and shield lifetime

The decision matrix considers visibility, actor facing, direct/narrow/wide aim, inventory, HUD item use, ammunition, reloading, remaining shot recovery, sniper weapons and explosive preparation. The weapon's `is_burer_see_sniper_weapon` flag is cached at load time.

Close danger and critical grenades favor the shield; inventory and item use favor anti-aim; a knife outside the closest range favors gravi. Sniper/explosive threats restrict telekinesis. Only ready attacks are selected: control ownership and cooldowns remain effective. A critical grenade can interrupt gravi preparation if the shield is ready.

Close protection and wide aim can bypass the regular shield cooldown. Active grenades, close danger, recent aim and sniper/explosive threats can extend its lifetime. The random aim delay is sampled when aiming starts and counted after aim moves away. The shield state owns its lifetime; a separate frame timer no longer removes an extended shield.

When risk is permitted, the burer lowers its shield and transitions to anti-aim. Risk under aim adds a camera reaction and sideways actor impulse. Risk and random telekinesis-stop decisions run at most once per 500 ms. Tactical timers belong to each burer; actor camera ownership is shared between simultaneous attackers.

## Grenades and telekinesis

Active grenade threats are counted once per object, independently of pickup eligibility. Short-fuse and contact grenades can require protection even when they cannot be picked up.

- `burer_min_gren_timer` is the minimum remaining fuse required for active grenade pickup.
- `burer_critical_gren_timer` triggers urgent telekinesis termination and caps the remaining fuse after a throw.
- Pickup preserves an active grenade's fuse and initiator. A throw can shorten the fuse, never extend it.
- Inactive grenades are activated while held only when `Tele_Activate_N_Throw_Grenade` is enabled. This setting is independent of telekinetic weapon shooting. The burer becomes the initiator of an inactive grenade it activates.
- Active grenades are picked first, ordered by fuse. A normal throw prefers a held grenade with a 90% chance after 100 ms of holding. An urgent throw can occur while raising a grenade.
- Ordinary object mass limits do not exclude grenades. Parented/destroying objects, creatures, cars and `ph_heavy` objects remain excluded.

A final mass throw is permitted after attack expiry, after 500 ms with a grenade threat, on a critical fuse, or after `burer_forcetelefire_min_delta`. Ordinary objects also require a tactical throwing opportunity and the configured probability. Death, burning and external interruptions release objects without a final throw.

Thrown objects retain their collision controllers until normal cleanup. Raising/held counts exclude thrown objects. Picking up or destroying the selected object cancels its throw safely. Selection stores a full object ID and resolves/revalidates it before use.

## `[gunslinger_base]` parameters

Fallback values apply only when a key is missing. Gunslinger resources override them. Configuration is read when the burer loads.

| Parameter | Engine fallback | Gunslinger resources | Meaning |
| --- | ---: | ---: | --- |
| `burer_superstaminahit_dist` | 15 | 10 | Super-hit range, metres |
| `burer_superstaminahit_value` | 1000 | 1000 | Stamina loss |
| `burer_superstaminahit_minimal_stamina` | 0.05 | 0.01 | Recovered stamina floor |
| `burer_superstaminahit_minimal_stamina_health` | 0.5 | 0.9 | Health must exceed this threshold |
| `burer_superhealthhit_power` | -1 | 0.4 | Additional close hit; ≤0 disables it |
| `burer_superhealthhit_impulse` | 10 | 14 | Additional hit impulse |
| `burer_superhealthhit_type` | `wound` | `wound` | Standard hit-type token |
| `burer_superstaminahit_forceitemshideprob` | 1 | 0.1 | Hiding is performed when `random > value` |
| `burer_superstaminahit_conditiondecmin` | 0.05 | 0.02 | Minimum weapon condition loss |
| `burer_superstaminahit_conditiondecmax` | 0.1 | 0.1 | Maximum weapon condition loss |
| `burer_forceantiaim_dist` | 7 | 10 | Close anti-aim range, metres |
| `burer_forceshield_dist` | 3 | 3 | Close protection range, metres |
| `burer_shielded_risky_factor` | 0.02 | 0.18 | Risk decision probability when permitted |
| `burer_min_gren_timer` | 1000 | 1500 | Minimum pickup fuse, ms |
| `burer_critical_gren_timer` | 1000 | 1200 | Critical remaining fuse, ms |
| `burer_forcetelefire_min_delta` | 1000 | 3000 | Delay allowing a final mass throw, ms |
| `burer_gravi_impulse_for_actor` | 1000 | 1000 | Extra impulse when gravi hits the actor |
| `burer_risky_impulse_for_actor` | 1000 | fallback | Sideways impulse during risk under aim |
| `burer_skip_fireall_probability` | 0.7 | 0.7 | Despite its name, probability of **allowing** an ordinary object's final throw |
| `burer_aim_shield_delay_const` | 500 | 2000 | Constant shield delay after aiming, ms |
| `burer_aim_shield_delay_random` | 1000 | 3000 | Random delay addition `[0, value)`, ms |
| `burer_knife_selfkick_window_time` | 500 | 100 | Knife reaction window, ms |

Use probabilities in `[0, 1]`, nonnegative distances/times and `conditiondecmin <= conditiondecmax`.

See [burer telekinesis](burer-telekinesis.md) for ordinary monster-section settings, including mass limits, `Tele_Fly_Velocity`, object capacity and weapon shooting.

## Treasure protection and object IDs

The CoP treasure manager publishes loose treasure item ownership with `level.set_secret_item(id, true)`, clears it on pickup, and restores it through both save readers. AI queries a native ID set; it does not call a Lua functor. SoC/CS treasure items reside in containers and are excluded by the parent check.

IDs use `ALife::_OBJECT_ID` (`u32`) and `ALife::INVALID_OBJECT_ID`, not `u16` or `65535`. Lua uses `alife_const.invalid_object_id`. The save format is unchanged: the legacy reader/writer retains `r_object_id`/`w_object_id`; the chunk serializer retains `s_u32`.

Loose weapons more than 10 metres from the actor are excluded. Mods spawning their own loose treasure items should publish and clear the same native protection flag.

## Manual verification

These are acceptance scenarios, not a report of executed tests.

1. Compare ordinary, sniper and explosive weapons; open inventory; use a medkit. Record stamina and weapon condition before/after the hit.
2. Repeat with low stamina, health above/below the recovery threshold and a persistent slot. Check both weapon/device dropping and the sniper/explosive recovery exclusion.
3. Approach and attack with a knife, turn away and retreat. Check shield, hand hiding and `anm_selfkick`; no ordinary knife strike or controller suicide should result from the reaction animation.
4. Hold aim, move it away and measure shield delay. Repeat while reloading, out of ammunition and preparing a grenade.
5. Throw grenades with more than 1500 ms, 1200–1500 ms and less than 1200 ms remaining. Check pickup, shield and urgent throwing. Remaining fuse must never increase.
6. Test inactive grenades with activation on/off and weapon shooting disabled. Check contact grenades separately.
7. Pick up/destroy the selected object; kill or burn the burer while raising objects. Verify gravity, particles, sounds, weapon fire and absence of final throws on external interruption.
8. Test two burers, save/load, level transitions and a treasure item with ID above 65535. Camera reactions and treasure protection must not confuse objects.
