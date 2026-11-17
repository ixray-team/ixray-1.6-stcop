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
```
:::

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

* [XMLOverride](../configs/xml-override.md)
* [DLTX](../configs/dltx.md)
* [Script callbacks system](../scripting/script-callback-system.md)

## Swapping gamedata while the game runs

A script can overlay a folder or a `.db` archive on top of `gamedata` without restarting the game, for example to switch textures and sounds after a story event.

```lua
-- the folder must NOT be inside $arch_dir_addons$, otherwise it is mounted at startup
if game.mount_gamedata("$fs_root$", "eras\\era_2") then
    game.reload_gamedata(true, true) -- textures, sounds
end
```

`game.mount_gamedata(path_alias, path)` – queues the folder or archive for mounting. Its files replace the ones with the same relative path. Returns `false` if the alias or the target does not exist.

`game.unmount_gamedata(path_alias, path)` – removes a mount. Mounts added after it are mounted again on top.

`game.reload_gamedata(textures, sounds)` – queues a reload:
* `textures` – full device reset (as `vid_restart`): all textures are re-read.
* `sounds` – all loaded sounds and `sound_env` are re-read. Playing sounds keep playing with the new data.

The work runs at the start of the next frame behind the load screen.

The list of mounts is stored in the save. Loading a save restores its list (a save without one returns to the base gamedata) and reloads textures and sounds if the list changed. A new game starts with the base gamedata.

Limitations:
* `mount_gamedata` / `unmount_gamedata` only change the file list: call `reload_gamedata` to see the new textures and sounds.
* Models, configs, shaders and scripts that are already loaded are not reloaded.
