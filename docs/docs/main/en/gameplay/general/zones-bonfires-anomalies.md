# Zones (Bonfires, Anomalies...)
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.0

## General 
* Volumetric lighting (optional)
```ini
volumetric_blowout = true ; Volumetric light (blowout)
volumetric_quality = 1.0  ; Render quality (light/blowout)
volumetric_distance = 1.0 ; Light distance (light/blowout)
volumetric_intensity = 0.7; Light intensity (light/blowout)
```

## Bolt removal in anomalies

Set `delete_bolts` in a zone section implemented by `CAnomalyZone`. It controls removal of thrown bolts when they enter the zone. This is an engine feature and does not require Gunslinger scripts.

| Parameter | Type | Default | Purpose |
| --- | --- | --- | --- |
| `delete_bolts` | `bool` | `true` | Allows the zone to remove bolts entering it |

### Behavior

With `delete_bolts = true`, the zone compares the bolt's destruction deadline with the current game time plus 100 ms:

- If more than 100 ms remain, the zone immediately requests removal through the standard `Destroy()` mechanism. Actual disappearance depends on destruction event processing.
- If 100 ms or less remain, the existing timer is preserved. An already expired timer is also left unchanged.

**100 ms is a comparison threshold, not a removal delay.** The threshold cannot be configured through LTX. The current implementation differs from original Gunslinger, which shortens the remaining lifetime to 100 ms.

With `delete_bolts = false`, the zone neither changes the bolt's lifetime nor requests its removal. The ordinary `destroy_time` timer from the bolt section still applies. The bolt can affect the zone again if its activation logic permits it.

Only bolts (`CBolt`) are affected. Grenades and other items are unchanged. The `ignore_small`, `ignore_nonalive` and entrance particle settings do not disable removal of a bolt already registered as entering the zone.

The setting applies independently to each zone. In overlapping zones, `delete_bolts = false` in one zone does not prevent another from removing the bolt. Leaving a zone does not cancel a destruction request already sent.

### Example: retain bolts in a minefield

For CoP, CS and CoC, create `configs/zones/mod_zone_minefield_bolts.ltx` as a DLTX patch:

```ini
![zone_mine_field]
delete_bolts = false
```

In SoC, place the corresponding file in `configs/misc/`. The target game must contain the `zone_mine_field` section. When editing the original LTX directly, add the parameter to the existing section without the `!` prefix.

The Gunslinger port already includes this exception in `GunslingerModCop/configs/zones/mod_zone_minefield_zzz_guns.ltx`; it needs no additional patch. Set `true` to allow removal. Omitting the parameter also selects `true`. Section inheritance and DLTX follow their usual rules.

The value is read once when the zone loads and cached in a flag. Restart the game after editing the configuration.

### Manual verification

Use fresh bolts with an ordinary lifetime greater than 100 ms.

| Scenario | Expected result |
| --- | --- |
| Enter an ordinary anomaly without `delete_bolts` | Bolt removal is requested |
| Enter a zone with `delete_bolts = false` | The ordinary lifetime applies; repeated activation depends on zone logic |
| Enter with 100 ms or less remaining | The existing timer is preserved |
| Enter overlapping zones with `true` and `false` | The zone with `true` can remove the bolt |
| Enter with a grenade or another item | This setting does not change its behavior |

## Separate secondary damage type

`secondary_hit_type` selects the additional damage type for zones using `CMosquitoBald::UpdateSecondaryHit`, including thermal and acidic mines. The primary hit continues to use `hit_type`. Enable additional damage with `use_secondary_hit = true`.

| Parameter | Type | Default | Purpose |
| --- | --- | --- | --- |
| `use_secondary_hit` | `bool` | `false` | Enables secondary damage |
| `secondary_hit_power` | `float` | Required when secondary damage is enabled | Sets its power before distance scaling and target protection |
| `secondary_hit_type` | String token | The type from `hit_type` | Selects the secondary hit type only |

### Supported tokens

The parameter uses the same parser as `hit_type`: `burn`, `shock`, `chemical_burn`, `radiation`, `telepatic`, `wound`, `fire_wound`, `strike`, `explosion`, `wound_2`, `light_burn`, `physic_strike`. Names are case-insensitive. Unknown tokens are configuration errors, just like an invalid primary `hit_type`.

When porting original Gunslinger configs, replace `secondary_hit_type = 10` with `secondary_hit_type = light_burn`. Numeric values are not supported.

### Example: primary burn and secondary light burn

For CoP/CoC, create `configs/zones/mod_zone_mine_thermal_secondary.ltx` as a DLTX patch:

```ini
![zone_mine_thermal_weak]
hit_type = burn
use_secondary_hit = true
secondary_hit_power = 0.003
secondary_hit_type = light_burn
```

You can set `secondary_hit_type` in the parent `zone_mine_thermal` section. Derived mines inherit it unless they override the value. The working Gunslinger port uses `GunslingerModCop/configs/zones/mod_zone_mine_thermal_zzz_guns.ltx`.

The setting does not change activation timing, power, impulse, object filters or the primary damage type. The selected type enters the ordinary hit packet and target protection/effect handling. Currently, `burn` and `light_burn` share outfit burn protection and `burn` immunity; distinct type values alone do not imply different final damage. The burning state also depends on hit processing and actor settings.

The type is read once when the zone loads. Omitting the parameter selects the primary `hit_type`. Restart the game after editing LTX.

### Manual verification

In a debugger, compare the `hit_type` argument passed to `CAnomalyZone::CreateHit` from `Affect` and `UpdateSecondaryHit`. Then follow the hit through `CActor::Hit` and `CEntityCondition::ConditionHit`: inspect its type, damage before/after protection and changes to the burning state.

| Scenario | Expected result |
| --- | --- |
| `hit_type = burn`, `secondary_hit_type = light_burn` | Primary hit is `burn`; secondary hit is `light_burn` |
| Parameter omitted | Secondary hit uses the primary type |
| `secondary_hit_type = physic_strike` | Secondary hit is a physical strike; primary type is preserved |
| Unknown token | A configuration error when the zone loads |
| `use_secondary_hit = false` | No secondary hits are created |
