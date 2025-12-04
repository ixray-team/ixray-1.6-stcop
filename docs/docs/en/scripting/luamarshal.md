# Script data saves (luamarshal)
## Overview
> [!IMPORTANT]  
> **Status**: Not supported <br>
> **Minimal version**: 1.0

**The Lua Marshal-based save/load utility** provides helper functions for saving and loading game state. It abstracts serialization and deserialization so you can persist state between sessions.

## Functions
- `saveToMarshalFile(stateTable)`: serializes the provided Lua table (`stateTable`) and writes it to a file. Useful for storing game state, configs, or any tabular data.

- `loadFromMarshalFile()`: reads the serialized Lua table from file, deserializes it, and returns the Lua table. Useful for loading previously stored state or configs.

## Usage
Similar to the original `save(f)` / `load(r)`, you can expose `save_state()` / `load_state()` in binders to write data into `.dat` files:
```lua
function save_state()
    local gameState = {
        score = 1000,
        level = 5,
        player = {
            name = "Player",
            items = {"x", "y", "z"}
        }
    }
    save_manager.saveToMarshalFile(gameState)
end
```
Loading:
```lua
function load_state()
    local gameState = save_manager.loadFromMarshalFile()

    if gameState then
        score = gameState.score
        level = gameState.level

        if gameState.player then
            player.name = gameState.player.name
            player.items = gameState.player.items
        end
    end
end
```

## Pros
This approach keeps data in a separate `.dat` file, isolating it from the `.scop` save file; write size has no upper limit, and it is more resilient against corrupted saves.

## Cons
Only standard Lua types are supported: numbers, strings, booleans, tables containing valid types, or functions. `userdata` is not supported.
