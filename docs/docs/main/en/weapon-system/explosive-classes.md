# Explosives
## CExplosive
### Explosion marks
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.1

```ini
wallmark_section = explosion_marks; Setting wallmark from explosion (section from config is specified)
```

* The following sound settings work in the `Sound Layers` system:
* * `snd_explode`

### Gas grenades
> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.4
```ini
; Main gas explosion flag
is_gas_explosive           = true/false    ; Enable gas explosion (default false)

; Timings (in milliseconds)
blast_update_time          = 2000          ; Interval for searching new targets for gas explosion
particles_update_time      = 500           ; Particle update interval

; Callback functions for actor
actor_blast_begin_callback = callback_func ; Function called at beginning of actor processing
actor_blast_end_callback   = callback_func ; Function called at end of actor processing
```

::: details Callback examples 
```lua
-- Function to check at beginning of effect
-- Should return true/false - allow/deny further processing
function check_gas_mask()
    local actor = db.actor
    if actor:item_in_slot(6) ~= nil then  -- Check gas mask in slot 6
        local helmet = actor:item_in_slot(6)
        if helmet:section() == "helm_gasmask" then
            return false  -- Gas mask is present, cancel effect
        end
    end
    return true  -- Continue processing
end

-- Function applied at end of effect
function apply_gas_effect()
    local actor = db.actor
    actor:radiation_inc(0.05)  -- Small radiation dose
    actor:set_health(actor:get_health() - 0.1)  -- Health damage
    
    -- Can add visual effects
    local ps = get_console():execute("ps")  -- Get particle system
    -- ... effects logic ...
end

-- Complete processing of explosion on actor
function actor_gas_poison()
    -- Protection check
    if not check_gas_mask() then
        return
    end
    
    -- Apply effects
    apply_gas_effect()
    
    -- Additional logic
    printf("[GAS] Actor poisoned by gas!")
end
```
:::

## CGrenade
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimum Version**: 1.1
```ini
explosion_on_hit = true; Explosion on damage received
explosion_hit_types = 6, 8 ; Damage types 
```
