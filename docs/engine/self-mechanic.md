# Self Mechanic

In a Debug or RelWithDebInfo single-player game, open **Game → Self Mechanic** in ImGui, then press Alt to return input to the game. The `self_mechanic` console command opens the same window. If the inventory is open, Self Mechanic replaces it with the upgrade window. Close any other game dialogs first.

The tool uses the normal upgrade window and the upgrade manager's graph from the loaded configuration, including inherited and addon changes. It makes the selected item eligible without a technician's item list. Unknown upgrades are visible for this session only. Parent requirements, mutually exclusive groups, already installed upgrades, and the item's `legacy_upgrade_mode` remain in effect. Closing the upgrade window clears the session state.

For upgrades using both `inventory_upgrades.precondition_functor_a` and `inventory_upgrades.effect_functor_a`, the tool bypasses the standard availability and money check and invokes the effect with `loading = 1`; the bundled scripts therefore do not charge the actor. Installation and the optional `effect_upgrade_item` callback still follow the normal path. Free repair uses the normal item condition update but does not call the repair effect script.

A mod can replace a function without changing its name, or implement additional conditions and effects in custom Lua functors. Function names alone cannot establish the semantics of that code. Custom functors retain their normal checks and effects, and a replacement of a standard-named function must be checked for compatibility with the `loading` argument. The tool does not promise to bypass arbitrary Lua restrictions or preserve custom side effects suppressed by the free-repair or standard-effect paths.
