#pragma once

#include "../xrRender/IRender_Target.h"

// Traditional Vulkan render passes - no dynamic rendering
class VKRenderTarget : public IRender_Target
{
private:
    u32 dwWidth;
    u32 dwHeight;
    
public:
    // Traditional render passes for different phases
    VkRenderPass renderPassGeometry;      // G-buffer pass for deferred rendering
    VkRenderPass renderPassLighting;      // Lighting accumulation pass
    VkRenderPass renderPassCombine;       // Final combine pass
    VkRenderPass renderPassForward;       // Forward rendering pass
    VkRenderPass renderPassShadowMap;     // Shadow map generation
    
    // Framebuffers for each pass
    VkFramebuffer framebufferGeometry;
    VkFramebuffer framebufferLighting;
    VkFramebuffer framebufferCombine;
    VkFramebuffer framebufferShadowMap;
    
    // G-buffer render targets (similar to R2)
    VkImage rt_Position;
    VkImageView rt_Position_view;
    VkDeviceMemory rt_Position_memory;
    
    VkImage rt_Normal;
    VkImageView rt_Normal_view;
    VkDeviceMemory rt_Normal_memory;
    
    VkImage rt_Color;
    VkImageView rt_Color_view;
    VkDeviceMemory rt_Color_memory;
    
    // Depth buffer
    VkImage rt_Depth;
    VkImageView rt_Depth_view;
    VkDeviceMemory rt_Depth_memory;
    
    // Lighting accumulation buffer
    VkImage rt_Accumulator;
    VkImageView rt_Accumulator_view;
    VkDeviceMemory rt_Accumulator_memory;
    
    // Shadow map
    VkImage rt_ShadowMap;
    VkImageView rt_ShadowMap_view;
    VkDeviceMemory rt_ShadowMap_memory;

public:
    VKRenderTarget();
    virtual ~VKRenderTarget();
    
    virtual void set_RT(IUnknown* rt, u32 ID = 0) override;
    virtual void get_RT(u32 ID, IUnknown** ppRT) override;
    virtual void set_ZB(IUnknown* zb) override;
    virtual void get_ZB(IUnknown** ppZB) override;
    
    // Traditional phase-based rendering methods
    void phase_scene_prepare();        // Prepare for scene rendering (clear G-buffer)
    void phase_scene_begin();          // Begin scene rendering (set G-buffer targets)
    void phase_scene_end();            // End scene rendering
    
    void phase_lighting_prepare();     // Prepare for lighting pass
    void phase_lighting_begin();       // Begin lighting accumulation
    void phase_lighting_end();         // End lighting accumulation
    
    void phase_combine_prepare();      // Prepare final combine pass
    void phase_combine_begin();        // Begin final combine
    void phase_combine_end();          // End final combine
    
    void phase_smap_prepare();         // Prepare shadow map rendering
    void phase_smap_begin();           // Begin shadow map rendering
    void phase_smap_end();             // End shadow map rendering
    
    // Vulkan-specific methods
    bool Create(u32 width, u32 height);
    void Destroy();
    
private:
    bool CreateRenderPasses();
    bool CreateFramebuffers();
    bool CreateRenderTargets(u32 width, u32 height);
    void DestroyRenderTargets();
    
    bool CreateImage(u32 width, u32 height, VkFormat format, VkImageUsageFlags usage,
                     VkImage& image, VkDeviceMemory& memory, VkImageView& view);
    void DestroyImage(VkImage& image, VkDeviceMemory& memory, VkImageView& view);
};