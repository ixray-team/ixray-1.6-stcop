# IXR Framework (LUA Framework)
> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4.0

A specially designed Lua framework aimed at greatly simplifying mod development on our platform.
Its key feature is a modular architecture.
Each module is independent and loads before the main game scripts start.
A developer only needs to write a single line to activate the whole system.<br><br>
Out of the box, you get a convenient and optimized toolkit for solving the vast majority of scripting tasks that modders often have to tackle in their projects in non-obvious and suboptimal ways.
Here, we have collected the most useful utilities and libraries for working with strings, vectors, tables, and other data types.
Everything that used to be added chaotically to global scripts is now neatly structured and waiting for you in the utils/ directories.
This is a universal foundation for any modifications, from simple to the most complex.<br><br>

In the Module System, each module has its own unique alias name, which is used to access the module instead of the file path.
This means you can now update a module or connect a module with any file name without having to rewrite all usages, since access is by the internal alias, not the file name.
This allows for flexible versioning of modules without affecting already implemented mods based on them.
