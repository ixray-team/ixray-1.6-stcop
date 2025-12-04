# Script callback system
> [!IMPORTANT]
> **Status**: Supported <br>
> **Minimal version**: 1.0

## Script callback system

* Table `intercepts` stores function names that fire on callbacks;
* `RegisterScriptCallback` registers a callback;
* `UnregisterScriptCallback` unregisters it;
* `SendScriptCallback` sends a callback.

Example usage:

```lua
-- available callback types
local intercepts = {
	save = {},
	load = {},
	update = {},

	save_state = {},
	load_state = {}
}
```

```lua
-- script
function DoSomething()
    -- ...
    SendScriptCallback("save_state")
end 

function DoSomething2()
    -- ...
    SendScriptCallback("load_state")
end 
```

```lua
-- binder
function actor_binder:reinit()
    -- ...
    RegisterScriptCallback("save_state", self)
end

function actor_binder:net_destroy()
    -- ...	
    UnregisterScriptCallback("save_state", self)
end

function actor_binder:load(reader)
    -- ...
    self:load_state() -- fake call because binder is not ready during callback
end

function actor_binder:save_state()
    -- ...
end

function actor_binder:load_state()
    -- ...
end
```
