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
