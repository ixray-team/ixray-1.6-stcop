#include "stdafx.h"
#include "vk_rendertarget.h"
#include "vk.h"

VKRenderTarget::VKRenderTarget()
{
    dwWidth = 0;
    dwHeight = 0;
    
    // Initialize Vulkan objects
    renderPassGeometry = VK_NULL_HANDLE;
    renderPassLighting = VK_NULL_HANDLE;
    renderPassCombine = VK_NULL_HANDLE;
    renderPassForward = VK_NULL_HANDLE;
    renderPassShadowMap = VK_NULL_HANDLE;
    
    framebufferGeometry = VK_NULL_HANDLE;
    framebufferLighting = VK_NULL_HANDLE;
    framebufferCombine = VK_NULL_HANDLE;
    framebufferShadowMap = VK_NULL_HANDLE;
    
    rt_Position = VK_NULL_HANDLE;
    rt_Position_view = VK_NULL_HANDLE;
    rt_Position_memory = VK_NULL_HANDLE;
    
    rt_Normal = VK_NULL_HANDLE;
    rt_Normal_view = VK_NULL_HANDLE;
    rt_Normal_memory = VK_NULL_HANDLE;
    
    rt_Color = VK_NULL_HANDLE;
    rt_Color_view = VK_NULL_HANDLE;
    rt_Color_memory = VK_NULL_HANDLE;
    
    rt_Depth = VK_NULL_HANDLE;
    rt_Depth_view = VK_NULL_HANDLE;
    rt_Depth_memory = VK_NULL_HANDLE;
    
    rt_Accumulator = VK_NULL_HANDLE;
    rt_Accumulator_view = VK_NULL_HANDLE;
    rt_Accumulator_memory = VK_NULL_HANDLE;
    
    rt_ShadowMap = VK_NULL_HANDLE;
    rt_ShadowMap_view = VK_NULL_HANDLE;
    rt_ShadowMap_memory = VK_NULL_HANDLE;
}

VKRenderTarget::~VKRenderTarget()
{
    Destroy();
}

void VKRenderTarget::set_RT(IUnknown* rt, u32 ID)
{
    // For Vulkan, render targets are managed through render passes
    // This is kept for interface compatibility
}

void VKRenderTarget::get_RT(u32 ID, IUnknown** ppRT)
{
    // For Vulkan, render targets are managed through render passes
    // This is kept for interface compatibility
    if (ppRT) *ppRT = nullptr;
}

void VKRenderTarget::set_ZB(IUnknown* zb)
{
    // For Vulkan, depth buffer is managed through render passes
    // This is kept for interface compatibility
}

void VKRenderTarget::get_ZB(IUnknown** ppZB)
{
    // For Vulkan, depth buffer is managed through render passes
    // This is kept for interface compatibility
    if (ppZB) *ppZB = nullptr;
}

bool VKRenderTarget::Create(u32 width, u32 height)
{
    dwWidth = width;
    dwHeight = height;
    
    if (!CreateRenderTargets(width, height)) return false;
    if (!CreateRenderPasses()) return false;
    if (!CreateFramebuffers()) return false;
    
    return true;
}

void VKRenderTarget::Destroy()
{
    VkDevice device = RImplementation.vkDevice.device;
    if (device == VK_NULL_HANDLE) return;
    
    // Wait for device to be idle before destroying
    vkDeviceWaitIdle(device);
    
    // Destroy framebuffers
    if (framebufferGeometry != VK_NULL_HANDLE) {
        vkDestroyFramebuffer(device, framebufferGeometry, nullptr);
        framebufferGeometry = VK_NULL_HANDLE;
    }
    if (framebufferLighting != VK_NULL_HANDLE) {
        vkDestroyFramebuffer(device, framebufferLighting, nullptr);
        framebufferLighting = VK_NULL_HANDLE;
    }
    if (framebufferCombine != VK_NULL_HANDLE) {
        vkDestroyFramebuffer(device, framebufferCombine, nullptr);
        framebufferCombine = VK_NULL_HANDLE;
    }
    if (framebufferShadowMap != VK_NULL_HANDLE) {
        vkDestroyFramebuffer(device, framebufferShadowMap, nullptr);
        framebufferShadowMap = VK_NULL_HANDLE;
    }
    
    // Destroy render passes
    if (renderPassGeometry != VK_NULL_HANDLE) {
        vkDestroyRenderPass(device, renderPassGeometry, nullptr);
        renderPassGeometry = VK_NULL_HANDLE;
    }
    if (renderPassLighting != VK_NULL_HANDLE) {
        vkDestroyRenderPass(device, renderPassLighting, nullptr);
        renderPassLighting = VK_NULL_HANDLE;
    }
    if (renderPassCombine != VK_NULL_HANDLE) {
        vkDestroyRenderPass(device, renderPassCombine, nullptr);
        renderPassCombine = VK_NULL_HANDLE;
    }
    if (renderPassForward != VK_NULL_HANDLE) {
        vkDestroyRenderPass(device, renderPassForward, nullptr);
        renderPassForward = VK_NULL_HANDLE;
    }
    if (renderPassShadowMap != VK_NULL_HANDLE) {
        vkDestroyRenderPass(device, renderPassShadowMap, nullptr);
        renderPassShadowMap = VK_NULL_HANDLE;
    }
    
    // Destroy render targets
    DestroyRenderTargets();
}

bool VKRenderTarget::CreateRenderPasses()
{
    VkDevice device = RImplementation.vkDevice.device;
    
    // Create G-buffer render pass (Geometry pass)
    {
        VkAttachmentDescription attachments[4] = {};
        
        // Position attachment (R16G16B16A16_SFLOAT)
        attachments[0].format = VK_FORMAT_R16G16B16A16_SFLOAT;
        attachments[0].samples = VK_SAMPLE_COUNT_1_BIT;
        attachments[0].loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
        attachments[0].storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        attachments[0].stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
        attachments[0].stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        attachments[0].initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        attachments[0].finalLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        
        // Normal attachment (R16G16B16A16_SFLOAT)
        attachments[1] = attachments[0];
        
        // Color attachment (R8G8B8A8_UNORM)
        attachments[2].format = VK_FORMAT_R8G8B8A8_UNORM;
        attachments[2].samples = VK_SAMPLE_COUNT_1_BIT;
        attachments[2].loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
        attachments[2].storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        attachments[2].stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
        attachments[2].stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        attachments[2].initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        attachments[2].finalLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        
        // Depth attachment
        attachments[3].format = VK_FORMAT_D32_SFLOAT_S8_UINT;
        attachments[3].samples = VK_SAMPLE_COUNT_1_BIT;
        attachments[3].loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
        attachments[3].storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        attachments[3].stencilLoadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
        attachments[3].stencilStoreOp = VK_ATTACHMENT_STORE_OP_STORE;
        attachments[3].initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        attachments[3].finalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        
        VkAttachmentReference colorAttachmentRefs[3] = {};
        colorAttachmentRefs[0].attachment = 0;
        colorAttachmentRefs[0].layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        colorAttachmentRefs[1].attachment = 1;
        colorAttachmentRefs[1].layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        colorAttachmentRefs[2].attachment = 2;
        colorAttachmentRefs[2].layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        
        VkAttachmentReference depthAttachmentRef = {};
        depthAttachmentRef.attachment = 3;
        depthAttachmentRef.layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        
        VkSubpassDescription subpass = {};
        subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
        subpass.colorAttachmentCount = 3;
        subpass.pColorAttachments = colorAttachmentRefs;
        subpass.pDepthStencilAttachment = &depthAttachmentRef;
        
        VkRenderPassCreateInfo renderPassInfo = {};
        renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
        renderPassInfo.attachmentCount = 4;
        renderPassInfo.pAttachments = attachments;
        renderPassInfo.subpassCount = 1;
        renderPassInfo.pSubpasses = &subpass;
        
        if (vkCreateRenderPass(device, &renderPassInfo, nullptr, &renderPassGeometry) != VK_SUCCESS) {
            return false;
        }
    }
    
    // Create lighting render pass
    {
        VkAttachmentDescription attachments[2] = {};
        
        // Accumulator attachment
        attachments[0].format = VK_FORMAT_R16G16B16A16_SFLOAT;
        attachments[0].samples = VK_SAMPLE_COUNT_1_BIT;
        attachments[0].loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
        attachments[0].storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        attachments[0].stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
        attachments[0].stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        attachments[0].initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        attachments[0].finalLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        
        // Depth attachment (read-only for lighting)
        attachments[1].format = VK_FORMAT_D32_SFLOAT_S8_UINT;
        attachments[1].samples = VK_SAMPLE_COUNT_1_BIT;
        attachments[1].loadOp = VK_ATTACHMENT_LOAD_OP_LOAD;
        attachments[1].storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        attachments[1].stencilLoadOp = VK_ATTACHMENT_LOAD_OP_LOAD;
        attachments[1].stencilStoreOp = VK_ATTACHMENT_STORE_OP_STORE;
        attachments[1].initialLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        attachments[1].finalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        
        VkAttachmentReference colorAttachmentRef = {};
        colorAttachmentRef.attachment = 0;
        colorAttachmentRef.layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        
        VkAttachmentReference depthAttachmentRef = {};
        depthAttachmentRef.attachment = 1;
        depthAttachmentRef.layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        
        VkSubpassDescription subpass = {};
        subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
        subpass.colorAttachmentCount = 1;
        subpass.pColorAttachments = &colorAttachmentRef;
        subpass.pDepthStencilAttachment = &depthAttachmentRef;
        
        VkRenderPassCreateInfo renderPassInfo = {};
        renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
        renderPassInfo.attachmentCount = 2;
        renderPassInfo.pAttachments = attachments;
        renderPassInfo.subpassCount = 1;
        renderPassInfo.pSubpasses = &subpass;
        
        if (vkCreateRenderPass(device, &renderPassInfo, nullptr, &renderPassLighting) != VK_SUCCESS) {
            return false;
        }
    }
    
    // Create combine render pass (final output to swapchain)
    {
        VkAttachmentDescription colorAttachment = {};
        colorAttachment.format = RImplementation.vkDevice.swapchainImageFormat;
        colorAttachment.samples = VK_SAMPLE_COUNT_1_BIT;
        colorAttachment.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
        colorAttachment.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        colorAttachment.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
        colorAttachment.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        colorAttachment.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        colorAttachment.finalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        
        VkAttachmentReference colorAttachmentRef = {};
        colorAttachmentRef.attachment = 0;
        colorAttachmentRef.layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        
        VkSubpassDescription subpass = {};
        subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
        subpass.colorAttachmentCount = 1;
        subpass.pColorAttachments = &colorAttachmentRef;
        
        VkRenderPassCreateInfo renderPassInfo = {};
        renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
        renderPassInfo.attachmentCount = 1;
        renderPassInfo.pAttachments = &colorAttachment;
        renderPassInfo.subpassCount = 1;
        renderPassInfo.pSubpasses = &subpass;
        
        if (vkCreateRenderPass(device, &renderPassInfo, nullptr, &renderPassCombine) != VK_SUCCESS) {
            return false;
        }
    }
    
    // Create shadow map render pass
    {
        VkAttachmentDescription depthAttachment = {};
        depthAttachment.format = VK_FORMAT_D32_SFLOAT;
        depthAttachment.samples = VK_SAMPLE_COUNT_1_BIT;
        depthAttachment.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
        depthAttachment.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
        depthAttachment.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
        depthAttachment.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
        depthAttachment.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
        depthAttachment.finalLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL;
        
        VkAttachmentReference depthAttachmentRef = {};
        depthAttachmentRef.attachment = 0;
        depthAttachmentRef.layout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        
        VkSubpassDescription subpass = {};
        subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
        subpass.colorAttachmentCount = 0;
        subpass.pColorAttachments = nullptr;
        subpass.pDepthStencilAttachment = &depthAttachmentRef;
        
        VkRenderPassCreateInfo renderPassInfo = {};
        renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
        renderPassInfo.attachmentCount = 1;
        renderPassInfo.pAttachments = &depthAttachment;
        renderPassInfo.subpassCount = 1;
        renderPassInfo.pSubpasses = &subpass;
        
        if (vkCreateRenderPass(device, &renderPassInfo, nullptr, &renderPassShadowMap) != VK_SUCCESS) {
            return false;
        }
    }
    
    return true;
}

bool VKRenderTarget::CreateFramebuffers()
{
    VkDevice device = RImplementation.vkDevice.device;
    
    // Create geometry framebuffer
    {
        VkImageView attachments[] = {
            rt_Position_view,
            rt_Normal_view,
            rt_Color_view,
            rt_Depth_view
        };
        
        VkFramebufferCreateInfo framebufferInfo = {};
        framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        framebufferInfo.renderPass = renderPassGeometry;
        framebufferInfo.attachmentCount = 4;
        framebufferInfo.pAttachments = attachments;
        framebufferInfo.width = dwWidth;
        framebufferInfo.height = dwHeight;
        framebufferInfo.layers = 1;
        
        if (vkCreateFramebuffer(device, &framebufferInfo, nullptr, &framebufferGeometry) != VK_SUCCESS) {
            return false;
        }
    }
    
    // Create lighting framebuffer
    {
        VkImageView attachments[] = {
            rt_Accumulator_view,
            rt_Depth_view
        };
        
        VkFramebufferCreateInfo framebufferInfo = {};
        framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        framebufferInfo.renderPass = renderPassLighting;
        framebufferInfo.attachmentCount = 2;
        framebufferInfo.pAttachments = attachments;
        framebufferInfo.width = dwWidth;
        framebufferInfo.height = dwHeight;
        framebufferInfo.layers = 1;
        
        if (vkCreateFramebuffer(device, &framebufferInfo, nullptr, &framebufferLighting) != VK_SUCCESS) {
            return false;
        }
    }
    
    // Create shadow map framebuffer
    {
        VkImageView attachments[] = { rt_ShadowMap_view };
        
        VkFramebufferCreateInfo framebufferInfo = {};
        framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        framebufferInfo.renderPass = renderPassShadowMap;
        framebufferInfo.attachmentCount = 1;
        framebufferInfo.pAttachments = attachments;
        framebufferInfo.width = 1024; // Shadow map size
        framebufferInfo.height = 1024;
        framebufferInfo.layers = 1;
        
        if (vkCreateFramebuffer(device, &framebufferInfo, nullptr, &framebufferShadowMap) != VK_SUCCESS) {
            return false;
        }
    }
    
    return true;
}

bool VKRenderTarget::CreateRenderTargets(u32 width, u32 height)
{
    // Create G-buffer targets
    if (!CreateImage(width, height, VK_FORMAT_R16G16B16A16_SFLOAT, 
                     VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
                     rt_Position, rt_Position_memory, rt_Position_view)) return false;
    
    if (!CreateImage(width, height, VK_FORMAT_R16G16B16A16_SFLOAT,
                     VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
                     rt_Normal, rt_Normal_memory, rt_Normal_view)) return false;
    
    if (!CreateImage(width, height, VK_FORMAT_R8G8B8A8_UNORM,
                     VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
                     rt_Color, rt_Color_memory, rt_Color_view)) return false;
    
    // Create depth buffer
    if (!CreateImage(width, height, VK_FORMAT_D32_SFLOAT_S8_UINT,
                     VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
                     rt_Depth, rt_Depth_memory, rt_Depth_view)) return false;
    
    // Create accumulator buffer
    if (!CreateImage(width, height, VK_FORMAT_R16G16B16A16_SFLOAT,
                     VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
                     rt_Accumulator, rt_Accumulator_memory, rt_Accumulator_view)) return false;
    
    // Create shadow map (1024x1024)
    if (!CreateImage(1024, 1024, VK_FORMAT_D32_SFLOAT,
                     VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
                     rt_ShadowMap, rt_ShadowMap_memory, rt_ShadowMap_view)) return false;
    
    return true;
}

void VKRenderTarget::DestroyRenderTargets()
{
    DestroyImage(rt_Position, rt_Position_memory, rt_Position_view);
    DestroyImage(rt_Normal, rt_Normal_memory, rt_Normal_view);
    DestroyImage(rt_Color, rt_Color_memory, rt_Color_view);
    DestroyImage(rt_Depth, rt_Depth_memory, rt_Depth_view);
    DestroyImage(rt_Accumulator, rt_Accumulator_memory, rt_Accumulator_view);
    DestroyImage(rt_ShadowMap, rt_ShadowMap_memory, rt_ShadowMap_view);
}

bool VKRenderTarget::CreateImage(u32 width, u32 height, VkFormat format, VkImageUsageFlags usage,
                                VkImage& image, VkDeviceMemory& memory, VkImageView& view)
{
    VkDevice device = RImplementation.vkDevice.device;
    
    // Create image
    VkImageCreateInfo imageInfo = {};
    imageInfo.sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    imageInfo.imageType = VK_IMAGE_TYPE_2D;
    imageInfo.extent.width = width;
    imageInfo.extent.height = height;
    imageInfo.extent.depth = 1;
    imageInfo.mipLevels = 1;
    imageInfo.arrayLayers = 1;
    imageInfo.format = format;
    imageInfo.tiling = VK_IMAGE_TILING_OPTIMAL;
    imageInfo.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
    imageInfo.usage = usage;
    imageInfo.samples = VK_SAMPLE_COUNT_1_BIT;
    imageInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
    
    if (vkCreateImage(device, &imageInfo, nullptr, &image) != VK_SUCCESS) {
        return false;
    }
    
    // Allocate memory
    VkMemoryRequirements memRequirements;
    vkGetImageMemoryRequirements(device, image, &memRequirements);
    
    VkMemoryAllocateInfo allocInfo = {};
    allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    allocInfo.allocationSize = memRequirements.size;
    
    // Find suitable memory type (device local)
    VkPhysicalDeviceMemoryProperties memProperties;
    vkGetPhysicalDeviceMemoryProperties(RImplementation.vkDevice.physicalDevice, &memProperties);
    
    for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++) {
        if ((memRequirements.memoryTypeBits & (1 << i)) &&
            (memProperties.memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)) {
            allocInfo.memoryTypeIndex = i;
            break;
        }
    }
    
    if (vkAllocateMemory(device, &allocInfo, nullptr, &memory) != VK_SUCCESS) {
        vkDestroyImage(device, image, nullptr);
        return false;
    }
    
    vkBindImageMemory(device, image, memory, 0);
    
    // Create image view
    VkImageViewCreateInfo viewInfo = {};
    viewInfo.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    viewInfo.image = image;
    viewInfo.viewType = VK_IMAGE_VIEW_TYPE_2D;
    viewInfo.format = format;
    viewInfo.subresourceRange.aspectMask = (format == VK_FORMAT_D32_SFLOAT || format == VK_FORMAT_D32_SFLOAT_S8_UINT) ? 
                                          VK_IMAGE_ASPECT_DEPTH_BIT : VK_IMAGE_ASPECT_COLOR_BIT;
    viewInfo.subresourceRange.baseMipLevel = 0;
    viewInfo.subresourceRange.levelCount = 1;
    viewInfo.subresourceRange.baseArrayLayer = 0;
    viewInfo.subresourceRange.layerCount = 1;
    
    if (vkCreateImageView(device, &viewInfo, nullptr, &view) != VK_SUCCESS) {
        vkFreeMemory(device, memory, nullptr);
        vkDestroyImage(device, image, nullptr);
        return false;
    }
    
    return true;
}

void VKRenderTarget::DestroyImage(VkImage& image, VkDeviceMemory& memory, VkImageView& view)
{
    VkDevice device = RImplementation.vkDevice.device;
    if (device == VK_NULL_HANDLE) return;
    
    if (view != VK_NULL_HANDLE) {
        vkDestroyImageView(device, view, nullptr);
        view = VK_NULL_HANDLE;
    }
    if (memory != VK_NULL_HANDLE) {
        vkFreeMemory(device, memory, nullptr);
        memory = VK_NULL_HANDLE;
    }
    if (image != VK_NULL_HANDLE) {
        vkDestroyImage(device, image, nullptr);
        image = VK_NULL_HANDLE;
    }
}

// Phase-based rendering methods (traditional approach)
void VKRenderTarget::phase_scene_prepare()
{
    // Prepare for scene rendering - similar to R2's approach
    // This will be called to prepare G-buffer rendering
}

void VKRenderTarget::phase_scene_begin()
{
    // Begin scene rendering with G-buffer render pass
    // This will begin the geometry render pass
}

void VKRenderTarget::phase_scene_end()
{
    // End scene rendering
    // This will end the geometry render pass
}

void VKRenderTarget::phase_lighting_prepare()
{
    // Prepare for lighting accumulation pass
}

void VKRenderTarget::phase_lighting_begin()
{
    // Begin lighting pass
    // This will begin the lighting render pass
}

void VKRenderTarget::phase_lighting_end()
{
    // End lighting pass
}

void VKRenderTarget::phase_combine_prepare()
{
    // Prepare final combine pass
}

void VKRenderTarget::phase_combine_begin()
{
    // Begin final combine pass
    // This will begin the combine render pass
}

void VKRenderTarget::phase_combine_end()
{
    // End final combine pass
}

void VKRenderTarget::phase_smap_prepare()
{
    // Prepare shadow map rendering
}

void VKRenderTarget::phase_smap_begin()
{
    // Begin shadow map rendering
    // This will begin the shadow map render pass
}

void VKRenderTarget::phase_smap_end()
{
    // End shadow map rendering
}