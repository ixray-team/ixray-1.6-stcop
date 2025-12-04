# Weather manager
> [!IMPORTANT]
> **Status**: Supported until 1.1 <br>
> **Minimal version**: 1.0

## What’s new

### Weather cycles and sections

**The modified weather manager introduces a new multi-section flow for weather cycles.** When editing the `weathers` parameter in `game_maps_single.ltx`, you can set the keyword `outdoor` instead of a specific cycle/section.

* With `outdoor` specified for a level, weather becomes multi-sectional and follows this structure:

```ini
; dynamic_weather_graphs

[level_good]           ; “good weather” cycles
[level_bad]            ; “bad weather” cycles
[level_transition]     ; one-hour transition weather between good/bad cycles
[level_pre_blowout]    ; pre-blowout weather, one hour before the blowout
[level_periods_length] ; period lengths for good/bad weather
period_good_length_min = x1
period_good_length_max = y1
period_bad_length_min  = x2
period_bad_length_max  = y2

; Optional settings
; Per-level blowout behavior:
[level_surge_settings]
surge_state = 1 ; 0
```

Example for trilogy surface levels in `outdoor` mode:

```ini
; game_maps_single.ltx

[zaton]
        global_rect                      = 307.0, 90.0, 717.0, 500.000000
        music_tracks                     = zaton_musics
        weathers                         = outdoor

[jupiter]
        global_rect                      = 68.0, 563.0, 478.0, 973.000000
        music_tracks                     = jupiter_musics
        weathers                         = outdoor

[pripyat]
        global_rect                      = 580.0, 564.0, 954.0, 938.000000
        music_tracks                     = pripyat_musics
        weathers                         = outdoor
```

```ini

; environment/dynamic_weather_graphs.ltx

; ZATON
[zaton_good]
clear = 1

[zaton_bad]
rain = 0.4
thunder = 0.3
cloudy = 0.3

[zaton_transition]
cloudy = 1

[zaton_pre_blowout]
cloudy = 0.5
thunder = 0.5

[zaton_periods_length]
period_good_length_min	= 4
period_good_length_max	= 6
period_bad_length_min	= 4
period_bad_length_max	= 6

[zaton_surge_settings]
surge_state = 1

; JUPITER
[jupiter_good]
clear = 1

[jupiter_bad]
rain = 0.4
thunder = 0.3
cloudy = 0.3

[jupiter_transition]
cloudy = 1

[jupiter_pre_blowout]
cloudy = 0.5
thunder = 0.5

[jupiter_periods_length]
period_good_length_min	= 5
period_good_length_max	= 8
period_bad_length_min	= 3
period_bad_length_max	= 4

[jupiter_surge_settings]
surge_state = 1

; PRIPYAT
[pripyat_good]
clear = 1

[pripyat_bad]
rain = 0.4
thunder = 0.3
cloudy = 0.3

[pripyat_transition]
cloudy = 1

[pripyat_pre_blowout]
cloudy = 0.5
thunder = 0.5

[pripyat_periods_length]
period_good_length_min	= 3
period_good_length_max	= 4
period_bad_length_min	= 5
period_bad_length_max	= 8

[pripyat_surge_settings]
surge_state = 1
```

## Blowout manager

### Level registration

**Level registration requirements in the blowout manager were reduced.** The blowout-forbidden check now uses the `indoor` keyword in the weather section instead of listing levels in `surge_manager`; artifact respawn automation works the same way without explicit level registration.

### Fixes

* Fixed double-use of the anesthetic
* Fixed red screen inside shelters
* Optimized final blowout stages
* Minor tweaks
