# New Artifact Properties Reference

## Scope

This document describes the new artifact properties and their runtime logic.
All effects are taken from artifacts on belt and are scaled by artifact condition.

Condition scale formula:

value_effective = value_config * artifact_condition

artifact_condition is in range from 0.0 to 1.0.

## Property List

1. jump_height_modifier
2. movement_speed_modifier
3. sleepiness_restore_speed
4. equipment_durability_modifier
5. inventory_weight_modifier

## 1. jump_height_modifier

Purpose:
Changes actor jump strength.

Runtime formula:

jump_final = jump_base * (1.0 + sum_modifier_condition_scaled)

Where:
sum_modifier_condition_scaled is sum of all belt artifacts.

Limits:

jump_final is clamped to minimum 0.0.

Neutral value:

0.0

Meaning examples:

0.25 gives about plus 25 percent jump speed component.
0.0 gives no change.
Negative values reduce jump.

## 2. movement_speed_modifier

Purpose:
Changes actor movement acceleration base used for walk run sprint chain.

Runtime formula:

walk_accel_final = walk_accel_base * (1.0 + sum_modifier_condition_scaled)

Then standard run sprint crouch strafe multipliers are applied by existing logic.

Limits:

walk_accel_final is clamped to minimum 0.0.
There is no hard upper clamp in this modifier path.

Neutral value:

0.0

Meaning examples:

0.20 gives about plus 20 percent base movement acceleration.
Negative values reduce acceleration.

High speed protection:

When movement_speed_modifier effect is non zero, extra collision damage filter is enabled for actor movement.
Filter suppresses pseudo fall collision damage from fast horizontal movement.
Real fall and strong downward movement damage are preserved.
A short debounce window is also used to suppress repeated micro impact spam.

## 3. sleepiness_restore_speed

Purpose:
Changes sleepiness stat over time.

Runtime formula per update tick:

delta_sleepiness = sleepiness_restore_speed * artifact_condition * dt

Applied only if engine option EnableSleepiness is enabled.

Tick model:

Artifact effects update in periodic actor loop with base step around 0.1 seconds.

Limits:

Sleepiness stat itself is clamped in actor condition system to range 0.0 to 1.0.

Neutral value:

0.0

Meaning examples:

Positive value increases sleepiness meter.
Negative value decreases sleepiness meter.

## 4. equipment_durability_modifier

Purpose:
Changes equipment wear rate from incoming hits.

Runtime aggregation:

modifier_actor = 1.0 + sum((value_config - 1.0) * artifact_condition)

Then:

modifier_actor is clamped to range 0.0 to 10.0.

Applied in armor hit logic:

hit_power_after_immunity = hit_power_after_immunity * modifier_actor
condition_change = -hit_power_after_immunity

Important behavior:

This property never repairs equipment.
It only scales wear speed.

Neutral value:

1.0

Meaning examples:

0.75 means slower wear by about 25 percent.
1.0 means no change.
1.30 means faster wear by about 30 percent.

## 5. inventory_weight_modifier

Purpose:
Changes weight of inventory items as percentage multiplier.
Does not change carry limit values.

Runtime aggregation:

modifier_actor = 1.0 + sum((value_config - 1.0) * artifact_condition)

Then:

modifier_actor is clamped to range 0.0 to 10.0.

Applied weight logic:

item_weight_effective = item_weight_base * modifier_actor
total_weight = sum(item_weight_effective for all inventory items)

Important behavior:

This property affects item mass contribution.
This property does not change additional_inventory_weight logic.
Carry limits remain controlled by existing max weight and additional weight systems.

Neutral value:

1.0

Meaning examples:

0.75 turns item with 4.0 kg into about 3.0 kg.
1.0 gives no change.
1.20 increases item weight by about 20 percent.

## Recommended Safe Ranges

These ranges are practical recommendations, not hard engine limits.

1. jump_height_modifier from -0.30 to 0.50
2. movement_speed_modifier from -0.20 to 0.40
3. sleepiness_restore_speed small values around gameplay tick scale
4. equipment_durability_modifier from 0.70 to 1.50
5. inventory_weight_modifier from 0.70 to 1.30

To use this feature fully, you will need:
1. Localization files
2. Optional unique icons
