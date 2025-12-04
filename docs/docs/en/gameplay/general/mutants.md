# Mutant Inventory

## Overview

> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimum Version**: 1.0

This feature allows mutants to have an inventory and carry items in it, similar to the mechanic from the original games "Shadow of Chernobyl" (SoC) and "Clear Sky" (CS)

## Enabling the Feature

To activate the mutant inventory system, you need to change the global parameter in the engine configuration.

**Usage Example:**

Open the `engine_external.ltx` file and in the `[gameplay]` section set the parameter `EnableMonstersInventory` to `true`.

```ini
[gameplay]
; ... other parameters ...
EnableMonstersInventory = true ; true - enable inventory, false - disable
```

## Configuring Item Spawn

For a mutant to carry items in its inventory, you need to add special parameters to its configuration file

| Parameter | Type | Description |
| :--- | :--- | :--- |
| `Spawn_Inventory_Item_Section` | string | The item section that will be spawned in the inventory. Supports multi-spawn via comma |
| `Spawn_Inventory_Item_Probability` | float | The probability (from 0.0 to 1.0) that the item will be spawned |

**Usage Example:**

```ini
; Mutant config (e.g., m_boar.ltx)
[m_boar_e]
...
Spawn_Inventory_Item_Section = mutant_boar_leg
Spawn_Inventory_Item_Probability = 1
```

## Configuring Psi-Dog Phantoms

Added the ability to define a custom section for psi-dog phantoms via the `phantom_section` parameter. This allows you to override the default phantom with your own section

**Parameters:**

| Parameter | Type | Description | Default |
| :--- | :--- | :--- | :--- |
| `phantom_section` | string | The section from which phantoms are spawned | `psy_dog_phantom` |

**Usage Example:**

For a psi-dog to spawn phantoms from your own section, specify its name in the mutant config.

**Usage Example:**

```ini
[psy_dog]
phantom_section		= my_custom_phantom ; Using a custom section for phantoms
```
