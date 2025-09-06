# Vulkan Renderer Traditional Render Pass Implementation

## Implementation Overview

Following community feedback to avoid dynamic rendering and use traditional Vulkan render passes for better performance and mobile compatibility, the Vulkan renderer has been redesigned with a phase-based architecture similar to the existing R2 renderer.

## Architecture Changes

### Traditional Render Passes (No Dynamic Rendering)

The implementation now uses static Vulkan render passes instead of dynamic rendering:

1. **Geometry Pass (`renderPassGeometry`)**: Multi-render-target G-buffer pass
   - Position buffer (R16G16B16A16_SFLOAT)
   - Normal buffer (R16G16B16A16_SFLOAT) 
   - Color buffer (R8G8B8A8_UNORM)
   - Depth/Stencil buffer (D32_SFLOAT_S8_UINT)

2. **Lighting Pass (`renderPassLighting`)**: Deferred lighting accumulation
   - Reads from G-buffer
   - Accumulates lighting contributions
   - Uses depth buffer for light volume culling

3. **Combine Pass (`renderPassCombine`)**: Final output to swapchain
   - Combines lighting with other effects
   - Outputs to swapchain format

4. **Shadow Map Pass (`renderPassShadowMap`)**: Shadow map generation
   - Depth-only rendering
   - 1024x1024 shadow map texture

### Phase-Based Rendering

The renderer follows the established pattern with clear phases:

```cpp
void CRender::Render() {
    // Traditional phase-based rendering (no dynamic rendering)
    phase_smap();      // Shadow map rendering
    phase_scene();     // Geometry pass (G-buffer)
    phase_lighting();  // Lighting accumulation
    phase_combine();   // Final combine and present
}
```

Each phase uses its corresponding traditional render pass:

- `phase_scene()` → `renderPassGeometry`
- `phase_lighting()` → `renderPassLighting`
- `phase_combine()` → `renderPassCombine`
- `phase_smap()` → `renderPassShadowMap`

### Mobile/Android Compatibility

- **Static render passes**: All render passes are created at initialization
- **No dynamic rendering**: Avoids VK_KHR_dynamic_rendering extension
- **Fixed render targets**: Pre-allocated render targets with known layouts
- **Explicit resource management**: Manual resource lifetime management

## Benefits

1. **Better Performance**: Static passes allow for driver optimizations
2. **Mobile Compatible**: Works on Android/mobile Vulkan implementations
3. **Predictable**: Fixed render pass structure is easier to debug and optimize
4. **Driver Friendly**: Traditional approach preferred by most Vulkan drivers

## Implementation Status

- [x] Render pass definitions created
- [x] Framebuffer management implemented
- [x] Render target creation and management
- [x] Phase-based rendering structure
- [x] Integration with main renderer
- [ ] Command buffer recording (TODO)
- [ ] Pipeline state objects (TODO)
- [ ] Shader integration (TODO)
- [ ] Complete lighting implementation (TODO)

This provides a solid foundation for traditional Vulkan rendering that can be extended with full rendering logic while maintaining excellent performance and compatibility.