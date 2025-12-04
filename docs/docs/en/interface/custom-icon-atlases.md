# **Support for Custom Icon Atlases**

> [!IMPORTANT]
> **Status**: Supported  <br>
> **Minimum Version**: IX-Ray Platform 1.3

## **Overview**

Added the ability to use **custom icon atlases** for items and upgrades. This simplifies the creation of independent addons and increases the modularity of the display system. Previously, all item icons had to be located in a common atlas `ui_icon_equipment.dds`, which made it difficult to ensure compatibility between multiple modifications. Now each item or upgrade can reference its own **atlas file**, placed in any convenient location within the addon

This allows you to:

- use separate textures without conflicts with other mods;
- simplify the development and testing of individual modules;
- optimize the structure of large modifications (decomposition of icons by category).

## **Usage**

For items, the following parameter has been added to the item section:

```ini
icons_texture = ui\custom_atlas.dds
```

For upgrades, the following parameter has been added to the item section:

```ini
upgr_icons_texture = ui\custom_atlas.dds
```

The path is specified relative to the `textures` directory

## **Recommendations**

* ✔ Group icons by topic in separate atlases;
* ✔ Use unique atlas file names for each addon;
* ⚠ Make sure the icon size matches the grid used in the UI;
* ✖ Don't override system atlases unless necessary.
