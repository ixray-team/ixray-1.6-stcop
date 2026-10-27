# Shadow of Chernobyl anomaly detectors

> [!IMPORTANT]
> Detectors using the Shadow of Chernobyl configuration must use `class = D_SOCDET`.
> `D_SIMDET` selects the handheld simple artifact detector used by Clear Sky and Call of Pripyat. Update custom detector sections when porting SoC configurations.

The SoC detector works when placed in its inventory slot or on the belt, and turns off in the backpack. It detects enabled anomaly zones by their configured **class**, including sections inherited from a listed zone type. A zone with `visible_by_detector = off` does not produce a sound or map marker.

```ini
[my_detector]:identity_immunities
class = D_SOCDET
slot = 8
radius = 10
ef_detector_type = 1
zone_class_1 = zone_mosquito_bald
zone_sound_1_ = detectors\DA-2_beep1
zone_min_freq_1 = 0.1
zone_max_freq_1 = 40
; Optional map marker and night-vision particle:
; zone_map_location_1 = anomaly_zone_location
; night_vision_particle = damage_fx\heli_burn
```

Retain the other inventory item fields required by the game. Number zone entries consecutively from 1; each needs its own sound and minimum/maximum frequency. Frequencies are in hertz and increase as the actor approaches the zone. The optional particle is shown on nearby enabled zones while the actor uses night vision, as in SoC.

Scripts checking `object:clsid()` must use `clsid.device_detector_soc` for these items; `clsid.device_detector_simple` refers to `D_SIMDET`. Do not hard-code the numeric Lua class ID. The existing item section names remain unchanged. Test existing saves and custom spawn data after changing item class assignments.
