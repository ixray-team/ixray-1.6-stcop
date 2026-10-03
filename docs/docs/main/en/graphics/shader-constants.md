# Shader constants

`b0`–`b5` (`cb_frame`, `cb_view`, `cb_object`, `cb_material`, `cb_light`, `cb_pass`) are declared in `gamedata/shaders/d3d11/shared/fixed_cb.hlsli`. Do not assign those slots in a custom shader that includes `common.hlsli`. Name-based `set_c` and `RegisterConstantSetup` still reach constants the engine reflects, including fields inside the fixed buffers. Custom constants that are not in those six buffers stay on the legacy reflected path.

Pass buffers use `b6`–`b10` and are not all bound at once. Skinning is `b6` (`sbones_array`, and `sbones_array_old` unless `DISABLE_MOTION_VECTORS`). Detail trample is `b6` and wind is `b7`. Bloom and tonemap constants that used to be loose globals are explicit `b6` buffers in those passes (`cb_bloom_down`, `cb_bloom_up`, `cb_bloom_adapt`, `cb_tonemap`).

> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: 1.0

```hlsl
float4 rain_params;
x // rainDensity 
y // rainWetness 
```

> [!IMPORTANT]  
> **Status**: Supported <br>
> **Minimal version**: 1.3

```hlsl
float4 m_timearrow;
float4 m_timearrow2;
```
