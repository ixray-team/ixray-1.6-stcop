#pragma once

#include "../xrRender/r__dsgraph_structure.h"
#include "../xrRender/r__occlusion.h"

#include "../xrRender/PSLibrary.h"
#include "../xrRender/r__types.h"

#include "../xrRender/HOM.h"
#include "../xrRender/DetailManager.h"
#include "../xrRender/ModelPool.h"
#include "../xrRender/WallmarksEngine.h"

#include "../xrRender/Light_DB.h"
#include "../xrRender/LightTrack.h"

#include "../../xrEngine/IRenderable.h"
#include "../../xrEngine/Fmesh.h"

// Forward declarations
class vkRender_Visual;

// Vulkan-specific structures
struct VKDevice {
    VkInstance instance = VK_NULL_HANDLE;
    VkPhysicalDevice physicalDevice = VK_NULL_HANDLE;
    VkDevice device = VK_NULL_HANDLE;
    VkQueue graphicsQueue = VK_NULL_HANDLE;
    VkQueue presentQueue = VK_NULL_HANDLE;
    VkSurfaceKHR surface = VK_NULL_HANDLE;
    VkSwapchainKHR swapchain = VK_NULL_HANDLE;
    VkCommandPool commandPool = VK_NULL_HANDLE;
    
    uint32_t graphicsQueueFamily = UINT32_MAX;
    uint32_t presentQueueFamily = UINT32_MAX;
    
    std::vector<VkImage> swapchainImages;
    std::vector<VkImageView> swapchainImageViews;
    VkFormat swapchainImageFormat;
    VkExtent2D swapchainExtent;
};

// Main Vulkan renderer class
class CRender : public R_dsgraph_structure
{
public:
    enum
    {
        PHASE_NORMAL = 0,
        PHASE_SMAP = 1,
    };

public:
    struct _options
    {
        u32 smapsize : 16;
        u32 HW_smap : 1;
        u32 HW_smap_PCF : 1;
        u32 HW_smap_FETCH4 : 1;
        
        u32 GPU_depth : 1;
        u32 GPU_skinning : 1;
        u32 tessellation : 1;
        u32 mblur : 1;
        u32 sunfilter : 1;
        u32 sunstatic : 1;
        u32 volumetricfog : 1;
        u32 sjitter : 1;
        u32 distortion_enabled : 1;
        u32 distortion : 1;
        u32 mblur_enabled : 1;
    } o;

    struct _stats
    {
        u32 ic_total;
        u32 ic_culled;
        u32 ic_invisible;
        u32 ic_visible;
        
        u32 l_shadowed;
        u32 l_unshadowed;
        u32 r_occluded;
        u32 r_passed;
        
        u32 tris;
        u32 verts;
        u32 calls;
        u32 vs;
        u32 ps;
        u32 cs;
        u32 gs;
        u32 hs;
        u32 ds;
        u32 decals;
        u32 groups;
        u32 batches;
        u32 primitives;
        u32 target_rt;
        u32 target_zb;
    } stat;

public:
    VKDevice vkDevice;
    
    float m_fov = 90.f;
    float m_aspect = 1.f;

    // Resource management
    CResourceManager* Resources = nullptr;
    ref_shader m_WireShader;
    ref_shader m_SelectionShader;

    // Light management
    CLightTracker Lights;
    xr_vector<light*> Lights_LastFrame;
    
    // Statistics
    CStats_timer stat_RenderDUMP_RT;
    CStats_timer stat_RenderDUMP_SKIN;
    CStats_timer stat_RenderDUMP;
    CStats_timer stat_RenderDUMP_Wait;
    CStats_timer stat_RenderDUMP_Wait_S;
    CStats_timer stat_RenderDUMP_HOM;
    CStats_timer stat_RenderDUMP_Gloss;
    CStats_timer stat_RenderDUMP_Alpha;
    CStats_timer stat_RenderDUMP_WM;
    CStats_timer stat_RenderDUMP_DT_VIS;
    CStats_timer stat_RenderDUMP_DT_Render;
    CStats_timer stat_RenderDUMP_DT_Cache;

public:
    CRender();
    virtual ~CRender();

    virtual void create() override;
    virtual void destroy() override;
    virtual void reset_begin() override;
    virtual void reset_end() override;

    virtual void level_Load(IReader*) override;
    virtual void level_Unload() override;

    virtual HRESULT shader_compile(
        LPCSTR name,
        DWORD const* pSrcData,
        UINT SrcDataLen,
        LPCSTR pFunctionName,
        LPCSTR pTarget,
        DWORD Flags,
        void*& result
    ) override;

    virtual void Calculate() override;
    virtual void Render() override;

    virtual void set_Transform(Fmatrix* M) override;
    virtual void set_HUD(BOOL V) override;
    virtual BOOL get_HUD() override;
    virtual void set_Invisible(BOOL V) override;
    virtual void flush() override;
    virtual void set_Object(IRenderable* O) override;
    virtual void add_Occluder(Fbox2& bb_screenspace) override;
    virtual void add_Visual(IRenderVisual* V) override;
    virtual void add_Geometry(IRenderVisual* V) override;

    virtual IRender_ObjectSpecific* ros_create(IRenderable* parent) override;
    virtual void ros_destroy(IRender_ObjectSpecific*&) override;

    virtual IRender_Light* light_create() override;
    virtual void light_destroy(IRender_Light* p_) override;
    virtual IRender_Glow* glow_create() override;
    virtual void glow_destroy(IRender_Glow* p_) override;

    virtual IRenderVisual* model_CreateParticles(LPCSTR name) override;
    virtual IRenderVisual* model_Create(LPCSTR name, IReader* data = 0) override;
    virtual IRenderVisual* model_CreateChild(LPCSTR name, IReader* data) override;
    virtual IRenderVisual* model_Duplicate(IRenderVisual* V) override;
    virtual void model_Delete(IRenderVisual*& V, BOOL bDiscard = FALSE) override;
    virtual void model_Logging(BOOL bEnable) override;
    virtual void models_Prefetch() override;
    virtual void models_Clear(BOOL b_complete) override;

    virtual BOOL occ_visible(vis_data& V) override;
    virtual BOOL occ_visible(Fbox& B) override;
    virtual BOOL occ_visible(sPoly& P) override;

    virtual void rmNear() override;
    virtual void rmFar() override;
    virtual void rmNormal() override;

    virtual u32 memory_usage() override;

    // Vulkan specific methods
    bool InitializeVulkan();
    void DestroyVulkan();
    bool CreateInstance();
    bool SelectPhysicalDevice();
    bool CreateLogicalDevice();
    bool CreateSurface();
    bool CreateSwapchain();
    void CleanupSwapchain();

    // Implementation details
    void apply_object(IRenderable* O);
    void apply_lmaterial();

private:
    BOOL b_loaded = FALSE;
    BOOL b_vis_HUD = TRUE;
    BOOL b_vis_Invisible = FALSE;

    // Vulkan debug/validation layers
    std::vector<const char*> validationLayers;
    std::vector<const char*> deviceExtensions;
    bool enableValidationLayers = false;
};

extern CRender RImplementation;