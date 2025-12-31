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

![image](https://github.com/user-attachments/assets/5061c665-40e5-4254-86d1-.init – meta information

* This file may be empty.  
* This file may have the following structure:

```yaml
name: IX-Ray Anim Items
script: test_script.script
```

`name:` – name of your addon (optional).  

`script:` – entry point of the script systems of your addon (optional). __More details below__!  

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

* [XMLOverride](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Addons:-XMLOverride)  
* [DLTX](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Addons:-DLTX)  
* [Script callbacks system](https://github.com/ixray-team/ixray-1.6-stcop/wiki/Lua:-%D0%A1%D0%B8%D1%81%D1%82%D0%B5%D0%BC%D0%B0-%D1%81%D0%BA%D1%80%D0%B8%D0%BF%D1%82%D0%BE%D0%B2%D1%8B%D1%85-%D0%BA%D0%BE%D0%BB%D0%BB%D1%8D%D0%B1%D1%8D%D0%BA%D0%BE%D0%B2)
