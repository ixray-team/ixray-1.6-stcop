# Vehicles in Lua

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

The `game_object` methods attach or detach the actor from a `CHolderCustom` object. The `CCar` class exposes vehicle fuel and state.

## Syntax

| Call | Effect |
| --- | --- |
| `actor:attach_vehicle(object, force)` | Attaches the actor to a live `CHolderCustom` object. |
| `actor:detach_vehicle(force)` | Detaches the actor from the current vehicle. |
| `actor:get_attached_vehicle()` | Returns the vehicle as a `game_object`, or `nil`. |
| `object:cast_Car()` | Returns a `CCar` for a car, or `nil`. |
| `car:add_fuel(amount)` | Adds fuel up to tank capacity and returns the amount added. |
| `car.fuel`, `car.fuel_tank` | Current fuel and tank capacity. Both fields are readable and writable. |

## Examples

### Refueling a car by ID

```lua
local function refuel_car(car_id, amount)
    local object = level.object_by_id(car_id)
    local car = object and object:cast_Car()
    if car then
        return car:add_fuel(amount)
    end
    return 0
end
```

## Recommendations and limitations

- Check the result of `level.object_by_id` before attaching the actor. The `attach_vehicle` implementation dereferences the supplied pointer.
- Use `add_fuel` with a positive amount to refuel up to capacity. Writing `fuel` directly bypasses the tank limit.

## See also

- [Vehicle configuration](transport.md)
- [Lua API](../../scripting/new-functions.md)
