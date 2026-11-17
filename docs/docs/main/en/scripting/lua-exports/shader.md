# Shader Lua VM

> [!IMPORTANT]
> **Status**: Supported<br>
> **Minimum version**: 1.4

## Overview

These registrations are for shader `.lua` scripts. References to `dx11ResourceManager_Scripting.cpp` apply to R4, and references to `dx9ResourceManager_Scripting.cpp` apply to Direct3D 9 renderers. Shaders run in their own Lua VM.

## Syntax

| Call | Arguments and result |
| --- | --- |
| `GetShaderOption(name)` | Returns a `boolean` indicating whether `engine_external.ltx` defines the option in its global `[shaders_options]`. |
| `AddShaderOption(name, value)` | Adds a macro name and its string value to the R4 renderer’s current compilation options. |
| `ClearAllShaderOptions()` | Clears the renderer’s current macro set. The global options table remains available. |

## Examples

### Pass a macro to a scope shader pass

An excerpt from the bundled `models_zoom_lerp.lua`. `l_special` runs in the shader VM. Set `USE_LENSE_LERP` before `shader:begin`.

```lua
function l_special(shader, t_base, t_second, t_detail)
    AddShaderOption("USE_LENSE_LERP", "1")
    shader:begin("scope_lense_mask", "scope_lense_mask")
        :zb(false, true):scope(true):fog(false)
    shader:dx10texture("s_position", "$user$position")
    shader:dx10texture("s_base", t_base)
    shader:dx10sampler("smp_base")
end
```

## Registrations

### Global scope

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `AddShaderOption(name, value)` | Function | Adds a macro to the current R4 compilation options. Both name and value are strings. Call before compiling the pass. | [dx11ResourceManager_Scripting.cpp:244](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L244) |
| `ClearAllShaderOptions()` | Function | Clears the current renderer macros. The global `[shaders_options]` section remains available through `GetShaderOption`. | [dx11ResourceManager_Scripting.cpp:246](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L246) |
| `GetShaderOption(name)` | Function | Returns whether the key exists in the global `[shaders_options]` section of `engine_external.ltx`. | [dx11ResourceManager_Scripting.cpp:245](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L245) |
| `log(message)` | Function | Writes a string to the engine log from the shader Lua VM. | [dx11ResourceManager_Scripting.cpp:242](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L242), [dx9ResourceManager_Scripting.cpp:185](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX9/dx9ResourceManager_Scripting.cpp#L185) |

### `_compiler`

| Call or name | Kind | Description | Registration |
| --- | --- | --- | --- |
| `_compiler:iblend(target_index, enabled, src, dst)` | Method | Sets blending for the selected render target. `src` and `dst` are blend factors. Returns the compiler for chaining. | [dx11ResourceManager_Scripting.cpp:187](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L187) |
| `_compiler:scope(enabled)` | Method | Toggles the shader’s scope mask flag and returns the compiler. | [dx11ResourceManager_Scripting.cpp:180](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L180), [dx9ResourceManager_Scripting.cpp:160](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX9/dx9ResourceManager_Scripting.cpp#L160) |
| `_compiler:scope_postprocess(enabled)` | Method | Toggles the shader’s scope postprocess flag and returns the compiler. | [dx11ResourceManager_Scripting.cpp:181](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX10/dx11ResourceManager_Scripting.cpp#L181), [dx9ResourceManager_Scripting.cpp:161](https://github.com/ixray-team/ixray-1.6-stcop/blob/aa90b5cceb412f52b0108150b20b7343f5cd487b/src/Layers/xrRenderDX9/dx9ResourceManager_Scripting.cpp#L161) |

## See also

- [New Lua exports](../lua-exports.md)
- [Shader options](../../graphics/shader-options.md)
