# General Information

> [!IMPORTANT]  
> **Status**: Supported  
> **Minimum version**: 1.0  

The addon system is a storage of various `gamedata` and `.db` archives that work without interfering with each other.  

## Basics

### Addons directory

* `$arch_dir_addons$` – folder with addons. By default __`ixr_addons\\`__  

### Operating principle: folder

* An addon in the form of a folder must contain an `addon.init` file inside it.  

![image](https://github.com/user-attachments/assets/5061c665-40e5-4254-86d1-18cd47323e79)

#### addon.init – metadata

* This file may be empty.  
* This file may have the following structure:

::: code-group
```yaml [addon.init]
name: IX-Ray Anim Items
script: test_script.script
platform: [cop, cs]
depends: ixray-anim-items-base
```
:::

`name:` – name of your addon (optional). Other addons refer to it by this name in `depends:`.  

`script:` – entry point of the script systems of your addon (optional). __More details below__!  

`platform:` – platforms the addon mounts on: `cop`, `cs`, `soc`, one value or a list (optional). Without it the addon mounts on every platform.  

`depends:` – addon names, one value or a list, that must mount first (optional). An addon whose dependency is missing is skipped with `! Addon '...' skipped: missing dependencies`. For the same file path, the addon that mounts later wins.  

#### Platform folders

Files in `platforms\<platform>\` inside an addon folder mount after the rest of the addon, as if they were in its root. Only the current platform's folder mounts; the others are skipped. A file there replaces the addon's own file with the same path.

```text
my_addon\
  addon.init
  configs\weapons\w_ak74.ltx                       ; every platform
  platforms\soc\configs\weapons\mod_w_ak74_soc.ltx ; SoC only
  platforms\cs\textures\ui\ui_icons.dds            ; CS only, replaces the root file
```

Use it with [DLTX](../configs/dltx.md) and [XMLOverride](../configs/xml-override.md) to keep one addon with a common base and small platform edits. Archive addons do not support platform folders.

#### script entry

The initialization script file is executed at the moment of engine/location/save loading. Therefore, it should only assign [callbacks](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Lua:-%D0%A1%D0%B8%D1%81%D1%82%D0%B5%D0%BC%D0%B0-%D1%81%D0%BA%D1%80%D0%B8%D0%BF%D1%82%D0%BE%D0%B2%D1%8B%D1%85-%D0%BA%D0%BE%D0%BB%D0%BB%D1%8D%D0%B1%D1%8D%D0%BA%D0%BE%D0%B2) for the further operation of the addon.  
Example:

```lua
SemiLog("Initial test addon script")

RegisterScriptCallback("update", my_script.update) --// calls code during actor:update
RegisterScriptCallback("save", my_script.save)     --// calls code during actor:save
```

### Operating principle: archive

* It is enough to place the archive into the addons directory.  

![image](https://github.com/user-attachments/assets/f0c93315-5efe-4cf1-b953-a893e0d5d45c)

## Systems for working with addons

* [XMLOverride](../configs/xml-override.md)
* [DLTX](../configs/dltx.md)
* [Script callbacks system](../scripting/script-callback-system.md)
