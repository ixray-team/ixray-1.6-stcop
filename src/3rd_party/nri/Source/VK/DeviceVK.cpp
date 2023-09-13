/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedVK.h"
#include "CommandQueueVK.h"
#include "CommandAllocatorVK.h"
#include "CommandBufferVK.h"
#include "TextureVK.h"
#include "BufferVK.h"
#include "DescriptorVK.h"
#include "FrameBufferVK.h"
#include "FenceVK.h"
#include "SwapChainVK.h"
#include "QueryPoolVK.h"
#include "DescriptorPoolVK.h"
#include "DescriptorSetVK.h"
#include "PipelineLayoutVK.h"
#include "PipelineVK.h"
#include "AccelerationStructureVK.h"
#include "MemoryVK.h"

constexpr uint32_t INVALID_FAMILY_INDEX = uint32_t(-1);

#if _WIN32
    #include <dxgi1_4.h>
#endif

using namespace nri;

inline bool IsExtensionSupported(const char* ext, const Vector<VkExtensionProperties>& list)
{
    for (auto& e : list)
    {
        if (!strcmp(ext, e.extensionName))
            return true;
    }

    return false;
}

void* VKAPI_PTR vkAllocateHostMemory(void* pUserData, size_t size, size_t alignment, VkSystemAllocationScope allocationScope)
{
    MaybeUnused(allocationScope);

    StdAllocator<uint8_t>& stdAllocator = *(StdAllocator<uint8_t>*)pUserData;
    const auto& lowLevelAllocator = stdAllocator.GetInterface();

    return lowLevelAllocator.Allocate(lowLevelAllocator.userArg, size, alignment);
}

void* VKAPI_PTR vkReallocateHostMemory(void* pUserData, void* pOriginal, size_t size, size_t alignment, VkSystemAllocationScope allocationScope)
{
    MaybeUnused(allocationScope);

    StdAllocator<uint8_t>& stdAllocator = *(StdAllocator<uint8_t>*)pUserData;
    const auto& lowLevelAllocator = stdAllocator.GetInterface();

    return lowLevelAllocator.Reallocate(lowLevelAllocator.userArg, pOriginal, size, alignment);
}

void VKAPI_PTR vkFreeHostMemory(void* pUserData, void* pMemory)
{
    StdAllocator<uint8_t>& stdAllocator = *(StdAllocator<uint8_t>*)pUserData;
    const auto& lowLevelAllocator = stdAllocator.GetInterface();

    return lowLevelAllocator.Free(lowLevelAllocator.userArg, pMemory);
}

void VKAPI_PTR vkHostMemoryInternalAllocationNotification(void* pUserData, size_t size, VkInternalAllocationType allocationType,
    VkSystemAllocationScope allocationScope)
{
    MaybeUnused(pUserData);
    MaybeUnused(size);
    MaybeUnused(allocationType);
    MaybeUnused(allocationScope);
}

void VKAPI_PTR vkHostMemoryInternalFreeNotification(void* pUserData, size_t size, VkInternalAllocationType allocationType,
    VkSystemAllocationScope allocationScope)
{
    MaybeUnused(pUserData);
    MaybeUnused(size);
    MaybeUnused(allocationType);
    MaybeUnused(allocationScope);
}

template< typename Implementation, typename Interface, typename ... Args >
Result DeviceVK::CreateImplementation(Interface*& entity, const Args&... args)
{
    Implementation* implementation = Allocate<Implementation>(GetStdAllocator(), *this);
    const Result result = implementation->Create(args...);

    if (result == Result::SUCCESS)
    {
        entity = (Interface*)implementation;
        return Result::SUCCESS;
    }

    Deallocate(GetStdAllocator(), implementation);
    return result;
}

DeviceVK::DeviceVK(const Log& log, const StdAllocator<uint8_t>& stdAllocator) :
    DeviceBase(log, stdAllocator),
    m_PhysicalDevices(GetStdAllocator()),
    m_PhysicalDeviceIndices(GetStdAllocator()),
    m_ConcurrentSharingModeQueueIndices(GetStdAllocator())
{
    if (FillFunctionTable(m_CoreInterface) != Result::SUCCESS)
        REPORT_ERROR(GetLog(), "Failed to get 'CoreInterface' interface in DeviceVK().");
}

DeviceVK::~DeviceVK()
{
    if (m_Device == VK_NULL_HANDLE)
        return;

    for (uint32_t i = 0; i < m_Queues.size(); i++)
        Deallocate(GetStdAllocator(), m_Queues[i]);

    if (m_Messenger != VK_NULL_HANDLE)
    {
        typedef PFN_vkDestroyDebugUtilsMessengerEXT Func;
        Func destroyCallback = (Func)m_VK.GetInstanceProcAddr(m_Instance, "vkDestroyDebugUtilsMessengerEXT");
        destroyCallback(m_Instance, m_Messenger, m_AllocationCallbackPtr);
    }

    if (m_OwnsNativeObjects)
    {
        m_VK.DestroyDevice(m_Device, m_AllocationCallbackPtr);
        m_VK.DestroyInstance(m_Instance, m_AllocationCallbackPtr);
    }

    if (m_Loader)
        UnloadSharedLibrary(*m_Loader);
}

Result DeviceVK::Create(const DeviceCreationDesc& deviceCreationDesc)
{
    m_OwnsNativeObjects = true;
    m_SPIRVBindingOffsets = deviceCreationDesc.spirvBindingOffsets;

    m_AllocationCallbacks.pUserData = &GetStdAllocator();
    m_AllocationCallbacks.pfnAllocation = vkAllocateHostMemory;
    m_AllocationCallbacks.pfnReallocation = vkReallocateHostMemory;
    m_AllocationCallbacks.pfnFree = vkFreeHostMemory;
    m_AllocationCallbacks.pfnInternalAllocation = vkHostMemoryInternalAllocationNotification;
    m_AllocationCallbacks.pfnInternalFree = vkHostMemoryInternalFreeNotification;

    if (deviceCreationDesc.enableAPIValidation)
        m_AllocationCallbackPtr = &m_AllocationCallbacks;

    m_Loader = LoadSharedLibrary(VULKAN_LOADER_NAME);
    if (!m_Loader)
    {
        REPORT_ERROR(GetLog(), "Failed to load Vulkan loader: '%s'.", VULKAN_LOADER_NAME);
        return Result::UNSUPPORTED;
    }

    // Create instance
    Result res = ResolvePreInstanceDispatchTable();
    if (res != Result::SUCCESS)
        return res;

    res = CreateInstance(deviceCreationDesc);
    if (res != Result::SUCCESS)
        return res;

    // Create device
    res = ResolveInstanceDispatchTable();
    if (res != Result::SUCCESS)
        return res;

    res = FindPhysicalDeviceGroup(deviceCreationDesc.physicalDeviceGroup, deviceCreationDesc.enableMGPU);
    if (res != Result::SUCCESS)
        return res;

    m_VK.GetPhysicalDeviceMemoryProperties(m_PhysicalDevices.front(), &m_MemoryProps);
    FillFamilyIndices(false, nullptr, 0);

    res = CreateLogicalDevice(deviceCreationDesc);
    if (res != Result::SUCCESS)
        return res;

    res = ResolveDispatchTable();
    if (res != Result::SUCCESS)
        return res;

    CreateCommandQueues();
    SetDeviceLimits(deviceCreationDesc.enableAPIValidation);

    const uint32_t groupSize = m_DeviceDesc.physicalDeviceNum;
    m_PhysicalDeviceIndices.resize(groupSize * groupSize);
    const auto begin = m_PhysicalDeviceIndices.begin();
    for (uint32_t i = 0; i < groupSize; i++)
        std::fill(begin + i * groupSize, begin + (i + 1) * groupSize, i);

    if (deviceCreationDesc.enableAPIValidation)
        ReportDeviceGroupInfo();

    FindDXGIAdapter();

    return res;
}

Result DeviceVK::Create(const DeviceCreationVulkanDesc& deviceCreationVulkanDesc)
{
    m_OwnsNativeObjects = false;
    m_SPIRVBindingOffsets = deviceCreationVulkanDesc.spirvBindingOffsets;

    const VkPhysicalDevice* physicalDevices = (VkPhysicalDevice*)deviceCreationVulkanDesc.vkPhysicalDevices;
    m_PhysicalDevices.insert(m_PhysicalDevices.begin(), physicalDevices, physicalDevices + deviceCreationVulkanDesc.deviceGroupSize);

    m_AllocationCallbacks.pUserData = &GetStdAllocator();
    m_AllocationCallbacks.pfnAllocation = vkAllocateHostMemory;
    m_AllocationCallbacks.pfnReallocation = vkReallocateHostMemory;
    m_AllocationCallbacks.pfnFree = vkFreeHostMemory;
    m_AllocationCallbacks.pfnInternalAllocation = vkHostMemoryInternalAllocationNotification;
    m_AllocationCallbacks.pfnInternalFree = vkHostMemoryInternalFreeNotification;

    if (deviceCreationVulkanDesc.enableAPIValidation)
        m_AllocationCallbackPtr = &m_AllocationCallbacks;

    const char* loaderPath = deviceCreationVulkanDesc.vulkanLoaderPath ? deviceCreationVulkanDesc.vulkanLoaderPath : VULKAN_LOADER_NAME;
    m_Loader = LoadSharedLibrary(loaderPath);
    if (!m_Loader)
    {
        REPORT_ERROR(GetLog(), "Failed to load Vulkan loader: '%s'.", loaderPath);
        return Result::UNSUPPORTED;
    }

    // Create instance
    Result res = ResolvePreInstanceDispatchTable();
    if (res != Result::SUCCESS)
        return res;

    m_Instance = (VkInstance)deviceCreationVulkanDesc.vkInstance;

    // Create device
    res = ResolveInstanceDispatchTable();
    if (res != Result::SUCCESS)
        return res;

    m_Device = (VkDevice)deviceCreationVulkanDesc.vkDevice;

    res = ResolveDispatchTable();
    if (res != Result::SUCCESS)
        return res;

    m_VK.GetPhysicalDeviceMemoryProperties(m_PhysicalDevices.front(), &m_MemoryProps);

    FillFamilyIndices(true, deviceCreationVulkanDesc.queueFamilyIndices, deviceCreationVulkanDesc.queueFamilyIndexNum);

    // Instance extensions
    {
        uint32_t extensionNum = 0;
        m_VK.EnumerateInstanceExtensionProperties(nullptr, &extensionNum, nullptr);

        Vector<VkExtensionProperties> supportedExts(extensionNum, GetStdAllocator());
        m_VK.EnumerateInstanceExtensionProperties(nullptr, &extensionNum, supportedExts.data());

        if (IsExtensionSupported(VK_EXT_DEBUG_UTILS_EXTENSION_NAME, supportedExts))
            m_IsDebugUtilsSupported = true;
    }

    // Device extensions
    {
        uint32_t extensionNum = 0;
        m_VK.EnumerateDeviceExtensionProperties(m_PhysicalDevices.front(), nullptr, &extensionNum, nullptr);

        Vector<VkExtensionProperties> supportedExts(extensionNum, GetStdAllocator());
        m_VK.EnumerateDeviceExtensionProperties(m_PhysicalDevices.front(), nullptr, &extensionNum, supportedExts.data());

        if (IsExtensionSupported(VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME, supportedExts))
            m_IsRayTracingExtSupported = true;

        if (IsExtensionSupported(VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME, supportedExts))
            m_IsSampleLocationExtSupported = true;

        if (IsExtensionSupported(VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME, supportedExts))
            m_IsConservativeRasterExtSupported = true;

        if (IsExtensionSupported(VK_EXT_HDR_METADATA_EXTENSION_NAME, supportedExts))
            m_IsHDRExtSupported = true;

        if (IsExtensionSupported(VK_EXT_OPACITY_MICROMAP_EXTENSION_NAME, supportedExts))
            m_IsMicroMapSupported = true;

        if (IsExtensionSupported(VK_EXT_MESH_SHADER_EXTENSION_NAME, supportedExts))
            m_IsMeshShaderExtSupported = true;
    }

    CreateCommandQueues();
    SetDeviceLimits(deviceCreationVulkanDesc.enableAPIValidation);

    if (deviceCreationVulkanDesc.enableAPIValidation)
        ReportDeviceGroupInfo();

    FindDXGIAdapter();

    return res;
}

void DeviceVK::FindDXGIAdapter()
{
#if _WIN32
    if (m_LUID == 0)
        return;

    ComPtr<IDXGIFactory4> factory;
    HRESULT result = CreateDXGIFactory2(0, IID_PPV_ARGS(&factory));

    if (FAILED(result))
    {
        REPORT_ERROR(GetLog(), "CreateDXGIFactory2() failed. (result: %d)", (int32_t)result);
        return;
    }

    static_assert(sizeof(LUID) <= sizeof(uint64_t), "invalid sizeof");
    const LUID luid = *(LUID*)&m_LUID;

    result = factory->EnumAdapterByLuid(luid, IID_PPV_ARGS(&m_Adapter));

    if (FAILED(result))
        REPORT_ERROR(GetLog(), "EnumAdapterByLuid() failed. (result: %d)", (int32_t)result);
#endif
}

bool DeviceVK::GetMemoryType(MemoryLocation memoryLocation, uint32_t memoryTypeMask, MemoryTypeInfo& memoryTypeInfo) const
{
    const VkMemoryPropertyFlags host = VK_MEMORY_PROPERTY_HOST_COHERENT_BIT | VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
    const VkMemoryPropertyFlags hostUnwantedFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;

    const VkMemoryPropertyFlags device = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
    const VkMemoryPropertyFlags deviceUnwantedFlags = 0;

    VkMemoryPropertyFlags flags = IsHostVisibleMemory(memoryLocation) ? host : device;
    VkMemoryPropertyFlags unwantedFlags = IsHostVisibleMemory(memoryLocation) ? hostUnwantedFlags : deviceUnwantedFlags;

    memoryTypeInfo.isHostCoherent = IsHostVisibleMemory(memoryLocation);
    memoryTypeInfo.location = (uint8_t)memoryLocation;

    for (uint_fast16_t i = 0; i < m_MemoryProps.memoryTypeCount; i++)
    {
        const bool isMemoryTypeSupported = memoryTypeMask & (1 << i);
        const bool isPropSupported = (m_MemoryProps.memoryTypes[i].propertyFlags & flags) == flags;
        const bool hasUnwantedProperties = (m_MemoryProps.memoryTypes[i].propertyFlags & unwantedFlags) != 0;

        if (isMemoryTypeSupported && isPropSupported && !hasUnwantedProperties)
        {
            memoryTypeInfo.memoryTypeIndex = (uint16_t)i;
            return true;
        }
    }

    // ignore unwanted properties
    for (uint_fast16_t i = 0; i < m_MemoryProps.memoryTypeCount; i++)
    {
        const bool isMemoryTypeSupported = memoryTypeMask & (1 << i);
        const bool isPropSupported = (m_MemoryProps.memoryTypes[i].propertyFlags & flags) == flags;

        if (isMemoryTypeSupported && isPropSupported)
        {
            memoryTypeInfo.memoryTypeIndex = (uint16_t)i;
            return true;
        }
    }

    return false;
}

bool DeviceVK::GetMemoryType(uint32_t index, MemoryTypeInfo& memoryTypeInfo) const
{
    if (index >= m_MemoryProps.memoryTypeCount)
        return false;

    const VkMemoryType& memoryType = m_MemoryProps.memoryTypes[index];

    memoryTypeInfo.memoryTypeIndex = (uint16_t)index;
    memoryTypeInfo.isHostCoherent = memoryType.propertyFlags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;

    const bool isHostVisible = memoryType.propertyFlags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
    memoryTypeInfo.location = isHostVisible ? (uint8_t)MemoryLocation::HOST_UPLOAD : (uint8_t)MemoryLocation::DEVICE;
    static_assert((uint32_t)MemoryLocation::MAX_NUM <= std::numeric_limits<uint8_t>::max(), "Unexpected number of memory locations");

    return true;
}

const char* GetObjectTypeName(VkObjectType objectType)
{
    switch(objectType)
    {
    case VK_OBJECT_TYPE_INSTANCE:
        return "VkInstance";
    case VK_OBJECT_TYPE_PHYSICAL_DEVICE:
        return "VkPhysicalDevice";
    case VK_OBJECT_TYPE_DEVICE:
        return "VkDevice";
    case VK_OBJECT_TYPE_QUEUE:
        return "VkQueue";
    case VK_OBJECT_TYPE_SEMAPHORE:
        return "VkSemaphore";
    case VK_OBJECT_TYPE_COMMAND_BUFFER:
        return "VkCommandBuffer";
    case VK_OBJECT_TYPE_FENCE:
        return "VkFence";
    case VK_OBJECT_TYPE_DEVICE_MEMORY:
        return "VkDeviceMemory";
    case VK_OBJECT_TYPE_BUFFER:
        return "VkBuffer";
    case VK_OBJECT_TYPE_IMAGE:
        return "VkImage";
    case VK_OBJECT_TYPE_EVENT:
        return "VkEvent";
    case VK_OBJECT_TYPE_QUERY_POOL:
        return "VkQueryPool";
    case VK_OBJECT_TYPE_BUFFER_VIEW:
        return "VkBufferView";
    case VK_OBJECT_TYPE_IMAGE_VIEW:
        return "VkImageView";
    case VK_OBJECT_TYPE_SHADER_MODULE:
        return "VkShaderModule";
    case VK_OBJECT_TYPE_PIPELINE_CACHE:
        return "VkPipelineCache";
    case VK_OBJECT_TYPE_PIPELINE_LAYOUT:
        return "VkPipelineLayout";
    case VK_OBJECT_TYPE_RENDER_PASS:
        return "VkRenderPass";
    case VK_OBJECT_TYPE_PIPELINE:
        return "VkPipeline";
    case VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT:
        return "VkDescriptorSetLayout";
    case VK_OBJECT_TYPE_SAMPLER:
        return "VkSampler";
    case VK_OBJECT_TYPE_DESCRIPTOR_POOL:
        return "VkDescriptorPool";
    case VK_OBJECT_TYPE_DESCRIPTOR_SET:
        return "VkDescriptorSet";
    case VK_OBJECT_TYPE_FRAMEBUFFER:
        return "VkFramebuffer";
    case VK_OBJECT_TYPE_COMMAND_POOL:
        return "VkCommandPool";
    case VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION:
        return "VkSamplerYcbcrConversion";
    case VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE:
        return "VkDescriptorUpdateTemplate";
    case VK_OBJECT_TYPE_SURFACE_KHR:
        return "VkSurfaceKHR";
    case VK_OBJECT_TYPE_SWAPCHAIN_KHR:
        return "VkSwapchainKHR";
    case VK_OBJECT_TYPE_DISPLAY_KHR:
        return "VkDisplayKHR";
    case VK_OBJECT_TYPE_DISPLAY_MODE_KHR:
        return "VkDisplayModeKHR";
    case VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT:
        return "VkDebugReportCallbackEXT";
    case VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT:
        return "VkDebugUtilsMessengerEXT";
    case VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR:
        return "VkAccelerationStructureKHR";
    case VK_OBJECT_TYPE_VALIDATION_CACHE_EXT:
        return "VkValidationCacheEXT";
    case VK_OBJECT_TYPE_DEFERRED_OPERATION_KHR:
        return "VkDeferredOperationKHR";
    default:
        return "unknown";
    }
}

VkBool32 VKAPI_PTR DebugUtilsMessenger(
    VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity,
    VkDebugUtilsMessageTypeFlagsEXT messageType,
    const VkDebugUtilsMessengerCallbackDataEXT* callbackData,
    void* userData)
{
    MaybeUnused(messageType);

    bool isError = false;
    bool isWarning = false;

    /*
    // TODO: convert an error to a warning as
    if (callbackData->messageIdNumber == <message ID>)
        messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT;
    */

    const char* type = "unknown";
    switch( messageSeverity )
    {
    case VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT:
        type = "verbose";
        break;
    case VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT:
        type = "info";
        break;
    case VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT:
        type = "warning";
        isWarning = true;
        break;
    case VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT:
        type = "error";
        isError = true;
        break;
    }

    if (!isWarning && !isError)
        return VK_FALSE;

    DeviceVK& device = *(DeviceVK*)userData;

    String message(device.GetStdAllocator());
    message += std::to_string(callbackData->messageIdNumber);
    message += " ";
    message += callbackData->pMessageIdName;
    message += " ";
    message += callbackData->pMessage;

    // vkCmdCopyBufferToImage: For optimal performance VkImage 0x984b920000000104 layout should be VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL instead of GENERAL.
    if (callbackData->messageIdNumber == 1303270965)
        return VK_FALSE;

    if (messageSeverity == VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)
    {
        message += "\nObjectNum: " + std::to_string(callbackData->objectCount);

        char buffer[64];

        for (uint32_t i = 0; i < callbackData->objectCount; i++)
        {
            const VkDebugUtilsObjectNameInfoEXT& object = callbackData->pObjects[i];
            snprintf(buffer, sizeof(buffer), "0x%llx", (unsigned long long)object.objectHandle);

            message += "\n\tObject ";
            message += object.pObjectName != nullptr ? object.pObjectName : "";
            message += " ";
            message += GetObjectTypeName(object.objectType);
            message += " (";
            message += buffer;
            message += ")";
        }

        REPORT_ERROR(device.GetLog(), "DebugUtilsMessenger: %s, %s", type, message.c_str());
    }
    else if (messageSeverity == VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT)
        REPORT_WARNING(device.GetLog(), "DebugUtilsMessenger: %s, %s", type, message.c_str());
    else
        REPORT_INFO(device.GetLog(), "DebugUtilsMessenger: %s, %s", type, message.c_str());

    return VK_FALSE;
}

void DeviceVK::FilterInstanceLayers(Vector<const char*>& layers)
{
    uint32_t layerNum = 0;
    m_VK.EnumerateInstanceLayerProperties(&layerNum, nullptr);

    Vector<VkLayerProperties> supportedLayers(layerNum, GetStdAllocator());
    m_VK.EnumerateInstanceLayerProperties(&layerNum, supportedLayers.data());

    for (size_t i = 0; i < layers.size(); i++)
    {
        bool found = false;
        for (uint32_t j = 0; j < layerNum && !found; j++)
        {
            if (strcmp(supportedLayers[j].layerName, layers[i]) == 0)
                found = true;
        }

        if (!found)
            layers.erase(layers.begin() + i--);
    }
}

Result DeviceVK::CreateInstance(const DeviceCreationDesc& deviceCreationDesc)
{
    Vector<const char*> layers(GetStdAllocator());
    if (deviceCreationDesc.enableAPIValidation)
        layers.push_back("VK_LAYER_KHRONOS_validation");

    FilterInstanceLayers(layers);

    uint32_t extensionNum = 0;
    m_VK.EnumerateInstanceExtensionProperties(nullptr, &extensionNum, nullptr);

    Vector<VkExtensionProperties> supportedExts(extensionNum, GetStdAllocator());
    m_VK.EnumerateInstanceExtensionProperties(nullptr, &extensionNum, supportedExts.data());

    Vector<const char*> extensions(GetStdAllocator());
    for (uint32_t i = 0; i < deviceCreationDesc.vulkanExtensions.instanceExtensionNum; i++)
        extensions.push_back(deviceCreationDesc.vulkanExtensions.instanceExtensions[i]);

    extensions.push_back(VK_KHR_SURFACE_EXTENSION_NAME);
    #ifdef VK_USE_PLATFORM_WIN32_KHR
        extensions.push_back(VK_KHR_WIN32_SURFACE_EXTENSION_NAME);
    #endif
    #ifdef VK_USE_PLATFORM_METAL_EXT
        extensions.push_back(VK_EXT_METAL_SURFACE_EXTENSION_NAME);
    #endif
    #ifdef VK_USE_PLATFORM_XLIB_KHR
        extensions.push_back(VK_KHR_XLIB_SURFACE_EXTENSION_NAME);
    #endif
    #ifdef VK_USE_PLATFORM_WAYLAND_KHR
        extensions.push_back(VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME);
    #endif

    if (IsExtensionSupported(VK_EXT_DEBUG_UTILS_EXTENSION_NAME, supportedExts))
    {
        extensions.push_back(VK_EXT_DEBUG_UTILS_EXTENSION_NAME);
        m_IsDebugUtilsSupported = true;
    }

    const VkApplicationInfo appInfo = {
        VK_STRUCTURE_TYPE_APPLICATION_INFO,
        nullptr,
        nullptr,
        0,
        nullptr,
        0,
        VK_API_VERSION_1_3
    };

    const VkInstanceCreateInfo info = {
        VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        nullptr,
        (VkInstanceCreateFlags)0,
        &appInfo,
        (uint32_t)layers.size(),
        layers.data(),
        (uint32_t)extensions.size(),
        extensions.data(),
    };

    VkResult result = m_VK.CreateInstance(&info, m_AllocationCallbackPtr, &m_Instance);

    RETURN_ON_FAILURE(GetLog(), result == VK_SUCCESS, GetReturnCode(result),
        "Can't create a VkInstance: vkCreateInstance returned %d.", (int32_t)result);

    if (deviceCreationDesc.enableAPIValidation)
    {
        typedef PFN_vkCreateDebugUtilsMessengerEXT Func;
        Func vkCreateDebugUtilsMessengerEXT = nullptr;
        vkCreateDebugUtilsMessengerEXT = (Func)m_VK.GetInstanceProcAddr(m_Instance, "vkCreateDebugUtilsMessengerEXT");

        VkDebugUtilsMessengerCreateInfoEXT createInfo = { VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT };

        createInfo.messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT;
        createInfo.messageSeverity |= VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;

        createInfo.messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT | VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT;
        createInfo.messageType |= VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;

        createInfo.pUserData = this;
        createInfo.pfnUserCallback = DebugUtilsMessenger;

        result = vkCreateDebugUtilsMessengerEXT(m_Instance, &createInfo, m_AllocationCallbackPtr, &m_Messenger);

        RETURN_ON_FAILURE(GetLog(), result == VK_SUCCESS, GetReturnCode(result),
            "Can't create a debug utils messenger callback: vkCreateDebugUtilsMessengerEXT returned %d.", (int32_t)result);
    }

    return Result::SUCCESS;
}

Result DeviceVK::FindPhysicalDeviceGroup(const PhysicalDeviceGroup* physicalDeviceGroup, bool enableMGPU)
{
    uint32_t deviceGroupNum = 0;
    m_VK.EnumeratePhysicalDeviceGroups(m_Instance, &deviceGroupNum, nullptr);

    VkPhysicalDeviceGroupProperties* deviceGroups = STACK_ALLOC(VkPhysicalDeviceGroupProperties, deviceGroupNum);
    VkResult result = m_VK.EnumeratePhysicalDeviceGroups(m_Instance, &deviceGroupNum, deviceGroups);

    RETURN_ON_FAILURE(GetLog(), result == VK_SUCCESS, GetReturnCode(result),
        "Can't enumerate physical devices: vkEnumeratePhysicalDevices returned %d.", (int32_t)result);

    VkPhysicalDeviceIDProperties idProps = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES };
    VkPhysicalDeviceProperties2 props = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 };
    props.pNext = &idProps;

    bool isVulkan11Supported = false;

    uint32_t i = 0;
    for (; i < deviceGroupNum && m_PhysicalDevices.empty(); i++)
    {
        const VkPhysicalDeviceGroupProperties& group = deviceGroups[i];
        m_VK.GetPhysicalDeviceProperties2(group.physicalDevices[0], &props);

        const uint32_t majorVersion = VK_VERSION_MAJOR(props.properties.apiVersion);
        const uint32_t minorVersion = VK_VERSION_MINOR(props.properties.apiVersion);
        isVulkan11Supported = majorVersion > 1 || (majorVersion == 1 && minorVersion >= 1);

        if (physicalDeviceGroup)
        {
            const uint64_t luid = *(uint64_t*)idProps.deviceLUID;
            if (luid == physicalDeviceGroup->luid)
            {
                RETURN_ON_FAILURE(GetLog(), isVulkan11Supported, Result::UNSUPPORTED,
                    "Can't create a device: the specified physical device does not support Vulkan 1.1.");
                break;
            }
        }
        else
        {
            if (isVulkan11Supported)
                break;
        }
    }

    RETURN_ON_FAILURE(GetLog(), i != deviceGroupNum, Result::UNSUPPORTED,
        "Can't create a device: physical device not found.");

    const VkPhysicalDeviceGroupProperties& group = deviceGroups[i];

    m_IsSubsetAllocationSupported = true;
    if (group.subsetAllocation == VK_FALSE && group.physicalDeviceCount > 1)
    {
        m_IsSubsetAllocationSupported = false;
        REPORT_WARNING(GetLog(), "The device group does not support memory allocation on a subset of the physical devices.");
    }

    m_PhysicalDevices.insert(m_PhysicalDevices.begin(), group.physicalDevices, group.physicalDevices + group.physicalDeviceCount);

    if (!enableMGPU)
        m_PhysicalDevices.resize(1);

    return Result::SUCCESS;
}

void DeviceVK::SetDeviceLimits(bool enableValidation)
{
    uint32_t familyNum = 0;
    m_VK.GetPhysicalDeviceQueueFamilyProperties(m_PhysicalDevices.front(), &familyNum, nullptr);

    Vector<VkQueueFamilyProperties> familyProperties(familyNum, m_StdAllocator);
    m_VK.GetPhysicalDeviceQueueFamilyProperties(m_PhysicalDevices.front(), &familyNum, familyProperties.data());

    uint32_t copyQueueTimestampValidBits = 0;
    const uint32_t copyQueueFamilyIndex = m_FamilyIndices[(uint32_t)CommandQueueType::COPY];
    if (copyQueueFamilyIndex != INVALID_FAMILY_INDEX)
        copyQueueTimestampValidBits = familyProperties[copyQueueFamilyIndex].timestampValidBits;

    VkPhysicalDeviceIDProperties deviceIDProps = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES };
    VkPhysicalDeviceProperties2 props = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2, &deviceIDProps };

    m_VK.GetPhysicalDeviceProperties2(m_PhysicalDevices.front(), &props);
    const VkPhysicalDeviceLimits& limits = props.properties.limits;

    VkPhysicalDeviceFeatures features = {};
    m_VK.GetPhysicalDeviceFeatures(m_PhysicalDevices.front(), &features);

    VkPhysicalDeviceVulkan12Features features12 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES };
    VkPhysicalDeviceFeatures2 features2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2, &features12 };

    m_VK.GetPhysicalDeviceFeatures2(m_PhysicalDevices.front(), &features2);

    m_IsDescriptorIndexingSupported = features12.descriptorIndexing ? true : false;
    m_IsBufferDeviceAddressSupported = features12.bufferDeviceAddress ? true : false;

    static_assert(VK_LUID_SIZE == sizeof(uint64_t), "invalid sizeof");
    m_LUID = *(uint64_t*)deviceIDProps.deviceLUID;

    m_DeviceDesc.graphicsAPI = GraphicsAPI::VULKAN;
    m_DeviceDesc.vendor = GetVendorFromID(props.properties.vendorID);
    m_DeviceDesc.nriVersionMajor = NRI_VERSION_MAJOR;
    m_DeviceDesc.nriVersionMinor = NRI_VERSION_MINOR;

    m_DeviceDesc.viewportMaxNum = limits.maxViewports;
    m_DeviceDesc.viewportSubPixelBits = limits.viewportSubPixelBits;
    m_DeviceDesc.viewportBoundsRange[0] = int32_t(limits.viewportBoundsRange[0]);
    m_DeviceDesc.viewportBoundsRange[1] = int32_t(limits.viewportBoundsRange[1]);

    m_DeviceDesc.frameBufferMaxDim = std::min(limits.maxFramebufferWidth, limits.maxFramebufferHeight);
    m_DeviceDesc.frameBufferLayerMaxNum = limits.maxFramebufferLayers;
    m_DeviceDesc.framebufferColorAttachmentMaxNum = limits.maxColorAttachments;

    m_DeviceDesc.frameBufferColorSampleMaxNum = (uint8_t)(limits.framebufferColorSampleCounts);
    m_DeviceDesc.frameBufferDepthSampleMaxNum = (uint8_t)(limits.framebufferDepthSampleCounts);
    m_DeviceDesc.frameBufferStencilSampleMaxNum = (uint8_t)(limits.framebufferStencilSampleCounts);
    m_DeviceDesc.frameBufferNoAttachmentsSampleMaxNum = (uint8_t)(limits.framebufferNoAttachmentsSampleCounts);
    m_DeviceDesc.textureColorSampleMaxNum = (uint8_t)(limits.sampledImageColorSampleCounts);
    m_DeviceDesc.textureIntegerSampleMaxNum = (uint8_t)(limits.sampledImageIntegerSampleCounts);
    m_DeviceDesc.textureDepthSampleMaxNum = (uint8_t)(limits.sampledImageDepthSampleCounts);
    m_DeviceDesc.textureStencilSampleMaxNum = (uint8_t)(limits.sampledImageStencilSampleCounts);
    m_DeviceDesc.storageTextureSampleMaxNum = (uint8_t)(limits.storageImageSampleCounts);

    m_DeviceDesc.texture1DMaxDim = limits.maxImageDimension1D;
    m_DeviceDesc.texture2DMaxDim = limits.maxImageDimension2D;
    m_DeviceDesc.texture3DMaxDim = limits.maxImageDimension3D;
    m_DeviceDesc.textureArrayMaxDim = limits.maxImageArrayLayers;
    m_DeviceDesc.texelBufferMaxDim = limits.maxTexelBufferElements;

    m_DeviceDesc.memoryAllocationMaxNum = limits.maxMemoryAllocationCount;
    m_DeviceDesc.samplerAllocationMaxNum = limits.maxSamplerAllocationCount;
    m_DeviceDesc.uploadBufferTextureRowAlignment = 1;
    m_DeviceDesc.uploadBufferTextureSliceAlignment = 1;
    m_DeviceDesc.typedBufferOffsetAlignment = (uint32_t)limits.minTexelBufferOffsetAlignment;
    m_DeviceDesc.constantBufferOffsetAlignment = (uint32_t)limits.minUniformBufferOffsetAlignment;
    m_DeviceDesc.constantBufferMaxRange = limits.maxUniformBufferRange;
    m_DeviceDesc.storageBufferOffsetAlignment = (uint32_t)limits.minStorageBufferOffsetAlignment;
    m_DeviceDesc.storageBufferMaxRange = limits.maxStorageBufferRange;
    m_DeviceDesc.pushConstantsMaxSize = limits.maxPushConstantsSize;
    m_DeviceDesc.bufferMaxSize = std::numeric_limits<uint64_t>::max();
    m_DeviceDesc.bufferTextureGranularity = (uint32_t)limits.bufferImageGranularity;

    m_DeviceDesc.boundDescriptorSetMaxNum = limits.maxBoundDescriptorSets;
    m_DeviceDesc.perStageDescriptorSamplerMaxNum = limits.maxPerStageDescriptorSamplers;
    m_DeviceDesc.perStageDescriptorConstantBufferMaxNum = limits.maxPerStageDescriptorUniformBuffers;
    m_DeviceDesc.perStageDescriptorStorageBufferMaxNum = limits.maxPerStageDescriptorStorageBuffers;
    m_DeviceDesc.perStageDescriptorTextureMaxNum = limits.maxPerStageDescriptorSampledImages;
    m_DeviceDesc.perStageDescriptorStorageTextureMaxNum = limits.maxPerStageDescriptorStorageImages;
    m_DeviceDesc.perStageResourceMaxNum = limits.maxPerStageResources;

    m_DeviceDesc.descriptorSetSamplerMaxNum = limits.maxDescriptorSetSamplers;
    m_DeviceDesc.descriptorSetConstantBufferMaxNum = limits.maxDescriptorSetUniformBuffers;
    m_DeviceDesc.descriptorSetStorageBufferMaxNum = limits.maxDescriptorSetStorageBuffers;
    m_DeviceDesc.descriptorSetTextureMaxNum = limits.maxDescriptorSetSampledImages;
    m_DeviceDesc.descriptorSetStorageTextureMaxNum = limits.maxDescriptorSetStorageImages;

    m_DeviceDesc.vertexShaderAttributeMaxNum = limits.maxVertexInputAttributes;
    m_DeviceDesc.vertexShaderStreamMaxNum = limits.maxVertexInputBindings;
    m_DeviceDesc.vertexShaderOutputComponentMaxNum = limits.maxVertexOutputComponents;

    m_DeviceDesc.tessControlShaderGenerationMaxLevel = (float)limits.maxTessellationGenerationLevel;
    m_DeviceDesc.tessControlShaderPatchPointMaxNum = limits.maxTessellationPatchSize;
    m_DeviceDesc.tessControlShaderPerVertexInputComponentMaxNum = limits.maxTessellationControlPerVertexInputComponents;
    m_DeviceDesc.tessControlShaderPerVertexOutputComponentMaxNum = limits.maxTessellationControlPerVertexOutputComponents;
    m_DeviceDesc.tessControlShaderPerPatchOutputComponentMaxNum = limits.maxTessellationControlPerPatchOutputComponents;
    m_DeviceDesc.tessControlShaderTotalOutputComponentMaxNum = limits.maxTessellationControlTotalOutputComponents;

    m_DeviceDesc.tessEvaluationShaderInputComponentMaxNum = limits.maxTessellationEvaluationInputComponents;
    m_DeviceDesc.tessEvaluationShaderOutputComponentMaxNum = limits.maxTessellationEvaluationOutputComponents;

    m_DeviceDesc.geometryShaderInvocationMaxNum = limits.maxGeometryShaderInvocations;
    m_DeviceDesc.geometryShaderInputComponentMaxNum = limits.maxGeometryInputComponents;
    m_DeviceDesc.geometryShaderOutputComponentMaxNum = limits.maxGeometryOutputComponents;
    m_DeviceDesc.geometryShaderOutputVertexMaxNum = limits.maxGeometryOutputVertices;
    m_DeviceDesc.geometryShaderTotalOutputComponentMaxNum = limits.maxGeometryTotalOutputComponents;

    m_DeviceDesc.fragmentShaderInputComponentMaxNum = limits.maxFragmentInputComponents;
    m_DeviceDesc.fragmentShaderOutputAttachmentMaxNum = limits.maxFragmentOutputAttachments;
    m_DeviceDesc.fragmentShaderDualSourceAttachmentMaxNum = limits.maxFragmentDualSrcAttachments;
    m_DeviceDesc.fragmentShaderCombinedOutputResourceMaxNum = limits.maxFragmentCombinedOutputResources;

    m_DeviceDesc.computeShaderSharedMemoryMaxSize = limits.maxComputeSharedMemorySize;
    m_DeviceDesc.computeShaderWorkGroupMaxNum[0] = limits.maxComputeWorkGroupCount[0];
    m_DeviceDesc.computeShaderWorkGroupMaxNum[1] = limits.maxComputeWorkGroupCount[1];
    m_DeviceDesc.computeShaderWorkGroupMaxNum[2] = limits.maxComputeWorkGroupCount[2];
    m_DeviceDesc.computeShaderWorkGroupInvocationMaxNum = limits.maxComputeWorkGroupInvocations;
    m_DeviceDesc.computeShaderWorkGroupMaxDim[0] = limits.maxComputeWorkGroupSize[0];
    m_DeviceDesc.computeShaderWorkGroupMaxDim[1] = limits.maxComputeWorkGroupSize[1];
    m_DeviceDesc.computeShaderWorkGroupMaxDim[2] = limits.maxComputeWorkGroupSize[2];

    m_DeviceDesc.timestampFrequencyHz = uint64_t( 1e9 / double(limits.timestampPeriod) + 0.5 );
    m_DeviceDesc.subPixelPrecisionBits = limits.subPixelPrecisionBits;
    m_DeviceDesc.subTexelPrecisionBits = limits.subTexelPrecisionBits;
    m_DeviceDesc.mipmapPrecisionBits = limits.mipmapPrecisionBits;
    m_DeviceDesc.drawIndexedIndex16ValueMax = std::min<uint32_t>(std::numeric_limits<uint16_t>::max(), limits.maxDrawIndexedIndexValue);
    m_DeviceDesc.drawIndexedIndex32ValueMax = limits.maxDrawIndexedIndexValue;
    m_DeviceDesc.drawIndirectMaxNum = limits.maxDrawIndirectCount;
    m_DeviceDesc.samplerLodBiasMin = -limits.maxSamplerLodBias;
    m_DeviceDesc.samplerLodBiasMax = limits.maxSamplerLodBias;
    m_DeviceDesc.samplerAnisotropyMax = limits.maxSamplerAnisotropy;
    m_DeviceDesc.texelOffsetMin = limits.minTexelOffset;
    m_DeviceDesc.texelOffsetMax = limits.maxTexelOffset;
    m_DeviceDesc.texelGatherOffsetMin = limits.minTexelGatherOffset;
    m_DeviceDesc.texelGatherOffsetMax = limits.maxTexelGatherOffset;
    m_DeviceDesc.clipDistanceMaxNum = limits.maxClipDistances;
    m_DeviceDesc.cullDistanceMaxNum = limits.maxCullDistances;
    m_DeviceDesc.combinedClipAndCullDistanceMaxNum = limits.maxCombinedClipAndCullDistances;
    m_DeviceDesc.physicalDeviceNum = (uint8_t)m_PhysicalDevices.size();

    m_DeviceDesc.isAPIValidationEnabled = enableValidation;
    m_DeviceDesc.isTextureFilterMinMaxSupported = features12.samplerFilterMinmax;
    m_DeviceDesc.isLogicOpSupported = features.logicOp;
    m_DeviceDesc.isDepthBoundsTestSupported = features.depthBounds;
    m_DeviceDesc.isProgrammableSampleLocationsSupported = m_IsSampleLocationExtSupported;
    m_DeviceDesc.isComputeQueueSupported = m_Queues[(uint32_t)CommandQueueType::COMPUTE] != nullptr;
    m_DeviceDesc.isCopyQueueSupported = m_Queues[(uint32_t)CommandQueueType::COPY] != nullptr;
    m_DeviceDesc.isCopyQueueTimestampSupported = copyQueueTimestampValidBits == 64;
    m_DeviceDesc.isRegisterAliasingSupported = true;
    m_DeviceDesc.isSubsetAllocationSupported = m_IsSubsetAllocationSupported;
    m_DeviceDesc.isFloat16Supported = features12.shaderFloat16;

    // Conservative raster
    if (m_IsConservativeRasterExtSupported)
    {
        VkPhysicalDeviceConservativeRasterizationPropertiesEXT conservativeRasterProps = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT };

        props.pNext = &conservativeRasterProps;

        m_VK.GetPhysicalDeviceProperties2(m_PhysicalDevices.front(), &props);

        if ( conservativeRasterProps.fullyCoveredFragmentShaderInputVariable && conservativeRasterProps.primitiveOverestimationSize <= (1.0 / 256.0f) )
            m_DeviceDesc.conservativeRasterTier = 3;
        else if ( conservativeRasterProps.degenerateTrianglesRasterized && conservativeRasterProps.primitiveOverestimationSize < (1.0f / 2.0f) )
            m_DeviceDesc.conservativeRasterTier = 2;
        else
            m_DeviceDesc.conservativeRasterTier = 1;
    }

    // Ray tracing
    if (m_IsRayTracingExtSupported)
    {
        VkPhysicalDeviceRayTracingPipelinePropertiesKHR rayTracingProps = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR };
        VkPhysicalDeviceAccelerationStructurePropertiesKHR accelerationStructureProperties = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR };

        props.pNext = &rayTracingProps;
        rayTracingProps.pNext = &accelerationStructureProperties;

        m_VK.GetPhysicalDeviceProperties2(m_PhysicalDevices.front(), &props);

        m_DeviceDesc.rayTracingShaderGroupIdentifierSize = rayTracingProps.shaderGroupHandleSize;
        m_DeviceDesc.rayTracingShaderRecursionMaxDepth = rayTracingProps.maxRayRecursionDepth;
        m_DeviceDesc.rayTracingGeometryObjectMaxNum = (uint32_t)accelerationStructureProperties.maxGeometryCount;
        m_DeviceDesc.rayTracingShaderTableAligment = rayTracingProps.shaderGroupBaseAlignment;
        m_DeviceDesc.rayTracingShaderTableMaxStride = rayTracingProps.maxShaderGroupStride;
    }

    // Mesh shader
    if (!m_IsMeshShaderExtSupported)
    {
        VkPhysicalDeviceMeshShaderPropertiesEXT meshShaderProps = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_EXT };

        props.pNext = &meshShaderProps;

        m_VK.GetPhysicalDeviceProperties2(m_PhysicalDevices.front(), &props);

        m_DeviceDesc.meshTaskWorkGroupInvocationMaxNum = meshShaderProps.maxTaskWorkGroupInvocations;
        m_DeviceDesc.meshTaskWorkGroupMaxDim[0] = meshShaderProps.maxTaskWorkGroupSize[0];
        m_DeviceDesc.meshTaskWorkGroupMaxDim[1] = meshShaderProps.maxTaskWorkGroupSize[1];
        m_DeviceDesc.meshTaskWorkGroupMaxDim[2] = meshShaderProps.maxTaskWorkGroupSize[2];
        m_DeviceDesc.meshTaskTotalMemoryMaxSize = meshShaderProps.maxTaskPayloadAndSharedMemorySize;
        m_DeviceDesc.meshTaskOutputMaxNum = meshShaderProps.maxMeshWorkGroupTotalCount;
        m_DeviceDesc.meshWorkGroupInvocationMaxNum = meshShaderProps.maxMeshWorkGroupInvocations;
        m_DeviceDesc.meshWorkGroupMaxDim[0] = meshShaderProps.maxMeshWorkGroupSize[0];
        m_DeviceDesc.meshWorkGroupMaxDim[1] = meshShaderProps.maxMeshWorkGroupSize[1];
        m_DeviceDesc.meshWorkGroupMaxDim[2] = meshShaderProps.maxMeshWorkGroupSize[2];
        m_DeviceDesc.meshOutputVertexMaxNum = meshShaderProps.maxMeshOutputVertices;
        m_DeviceDesc.meshOutputPrimitiveMaxNum = meshShaderProps.maxMeshOutputPrimitives;
        m_DeviceDesc.meshMultiviewViewMaxNum = meshShaderProps.maxMeshMultiviewViewCount;
        m_DeviceDesc.meshOutputPerVertexGranularity = meshShaderProps.meshOutputPerVertexGranularity;
        m_DeviceDesc.meshOutputPerPrimitiveGranularity = meshShaderProps.meshOutputPerPrimitiveGranularity;
    }
}

void DeviceVK::FillFamilyIndices(bool useEnabledFamilyIndices, const uint32_t* enabledFamilyIndices, uint32_t familyIndexNum)
{
    uint32_t familyNum = 0;
    m_VK.GetPhysicalDeviceQueueFamilyProperties(m_PhysicalDevices.front(), &familyNum, nullptr);

    Vector<VkQueueFamilyProperties> familyProps(familyNum, GetStdAllocator());
    m_VK.GetPhysicalDeviceQueueFamilyProperties(m_PhysicalDevices.front(), &familyNum, familyProps.data());

    memset(m_FamilyIndices.data(), INVALID_FAMILY_INDEX, m_FamilyIndices.size() * sizeof(uint32_t));

    for (uint32_t i = 0; i < familyProps.size(); i++)
    {
        const VkQueueFlags mask = familyProps[i].queueFlags;
        const bool graphics = mask & VK_QUEUE_GRAPHICS_BIT;
        const bool compute = mask & VK_QUEUE_COMPUTE_BIT;
        const bool copy = mask & VK_QUEUE_TRANSFER_BIT;

        if (useEnabledFamilyIndices)
        {
            bool isFamilyEnabled = false;
            for (uint32_t j = 0; j < familyIndexNum && !isFamilyEnabled; j++)
                isFamilyEnabled = enabledFamilyIndices[j] == i;

            if (!isFamilyEnabled)
                continue;
        }

        if (graphics)
            m_FamilyIndices[(uint32_t)CommandQueueType::GRAPHICS] = i;
        else if (compute)
            m_FamilyIndices[(uint32_t)CommandQueueType::COMPUTE] = i;
        else if (copy)
            m_FamilyIndices[(uint32_t)CommandQueueType::COPY] = i;
    }
}

Result DeviceVK::CreateLogicalDevice(const DeviceCreationDesc& deviceCreationDesc)
{
    uint32_t extensionNum = 0;
    m_VK.EnumerateDeviceExtensionProperties(m_PhysicalDevices.front(), nullptr, &extensionNum, nullptr);

    Vector<VkExtensionProperties> supportedExts(extensionNum, GetStdAllocator());
    m_VK.EnumerateDeviceExtensionProperties(m_PhysicalDevices.front(), nullptr, &extensionNum, supportedExts.data());

    Vector<const char*> desiredExts(GetStdAllocator());
    for (uint32_t i = 0; i < deviceCreationDesc.vulkanExtensions.deviceExtensionNum; i++)
        desiredExts.push_back(deviceCreationDesc.vulkanExtensions.deviceExtensions[i]);

    if (IsExtensionSupported(VK_KHR_SWAPCHAIN_EXTENSION_NAME, supportedExts))
        desiredExts.push_back(VK_KHR_SWAPCHAIN_EXTENSION_NAME);

    if (IsExtensionSupported(VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME, supportedExts))
        desiredExts.push_back(VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME);

    if (IsExtensionSupported(VK_KHR_SYNCHRONIZATION_2_EXTENSION_NAME, supportedExts))
        desiredExts.push_back(VK_KHR_SYNCHRONIZATION_2_EXTENSION_NAME);

    if (IsExtensionSupported(VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME, supportedExts))
        desiredExts.push_back(VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME);

    if (IsExtensionSupported(VK_KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME, supportedExts))
        desiredExts.push_back(VK_KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME);

    if (IsExtensionSupported(VK_KHR_RAY_QUERY_EXTENSION_NAME, supportedExts))
        desiredExts.push_back(VK_KHR_RAY_QUERY_EXTENSION_NAME);

    if (IsExtensionSupported(VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME, supportedExts))
    {
        desiredExts.push_back(VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME);
        m_IsRayTracingExtSupported = true;
    }

    if (IsExtensionSupported(VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME, supportedExts))
    {
        desiredExts.push_back(VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME);
        m_IsSampleLocationExtSupported = true;
    }

    if (IsExtensionSupported(VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME, supportedExts))
    {
        desiredExts.push_back(VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME);
        m_IsConservativeRasterExtSupported = true;
    }

    if (IsExtensionSupported(VK_EXT_HDR_METADATA_EXTENSION_NAME, supportedExts))
    {
        desiredExts.push_back(VK_EXT_HDR_METADATA_EXTENSION_NAME);
        m_IsHDRExtSupported = true;
    }

    if (IsExtensionSupported(VK_EXT_OPACITY_MICROMAP_EXTENSION_NAME, supportedExts))
    {
        desiredExts.push_back(VK_EXT_OPACITY_MICROMAP_EXTENSION_NAME);
        m_IsMicroMapSupported = true;
    }

    if (IsExtensionSupported(VK_EXT_MESH_SHADER_EXTENSION_NAME, supportedExts))
    {
        desiredExts.push_back(VK_EXT_MESH_SHADER_EXTENSION_NAME);
        m_IsMeshShaderExtSupported = true;
    }

    VkPhysicalDeviceRayTracingPipelineFeaturesKHR rayTracingFeatures = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR };
    VkPhysicalDeviceAccelerationStructureFeaturesKHR accelerationStructureFeatures = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR };
    VkPhysicalDeviceRayQueryFeaturesKHR rayQueryFeatures = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR };
    VkPhysicalDeviceOpacityMicromapFeaturesEXT micromapFeatures = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_EXT };
    VkPhysicalDeviceMeshShaderFeaturesEXT meshShaderFeatures = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_EXT };
    VkPhysicalDeviceVulkan11Features features11 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES };
    VkPhysicalDeviceVulkan12Features features12 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES };
    VkPhysicalDeviceVulkan13Features features13 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES };
    VkPhysicalDeviceFeatures2 features2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 };

    features2.pNext = &features11;
    features11.pNext = &features12;
    features12.pNext = &features13;

    if (m_IsRayTracingExtSupported)
    {
        rayTracingFeatures.pNext = features2.pNext;
        features2.pNext = &rayTracingFeatures;
        accelerationStructureFeatures.pNext = features2.pNext;
        features2.pNext = &accelerationStructureFeatures;
        rayQueryFeatures.pNext = features2.pNext;
        features2.pNext = &rayQueryFeatures;
    }

    if (m_IsMicroMapSupported)
    {
        micromapFeatures.pNext = features2.pNext;
        features2.pNext = &micromapFeatures;
    }

    if (m_IsMeshShaderExtSupported)
    {
        meshShaderFeatures.pNext = features2.pNext;
        features2.pNext = &meshShaderFeatures;
    }

    m_VK.GetPhysicalDeviceFeatures2(m_PhysicalDevices.front(), &features2);

    Vector<VkDeviceQueueCreateInfo> queues(GetStdAllocator());
    const float priorities = 1.0f;
    for (size_t i = 0; i < m_FamilyIndices.size(); i++)
    {
        if (m_FamilyIndices[i] == INVALID_FAMILY_INDEX)
            continue;

        VkDeviceQueueCreateInfo info = { VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO };
        info.queueCount = 1;
        info.queueFamilyIndex = m_FamilyIndices[i];
        info.pQueuePriorities = &priorities;
        queues.push_back(info);
    }

    VkDeviceCreateInfo deviceCreateInfo = { VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO };
    deviceCreateInfo.pNext = &features2;
    deviceCreateInfo.queueCreateInfoCount = (uint32_t)queues.size();
    deviceCreateInfo.pQueueCreateInfos = queues.data();
    deviceCreateInfo.enabledExtensionCount = (uint32_t)desiredExts.size();
    deviceCreateInfo.ppEnabledExtensionNames = desiredExts.data();

    VkDeviceGroupDeviceCreateInfo deviceGroupInfo = { VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO };
    if (m_PhysicalDevices.size() > 1)
    {
        deviceGroupInfo.pNext = deviceCreateInfo.pNext;
        deviceGroupInfo.physicalDeviceCount = (uint32_t)m_PhysicalDevices.size();
        deviceGroupInfo.pPhysicalDevices = m_PhysicalDevices.data();
        deviceCreateInfo.pNext = &deviceGroupInfo;
    }

    const VkResult result = m_VK.CreateDevice(m_PhysicalDevices.front(), &deviceCreateInfo, m_AllocationCallbackPtr, &m_Device);

    RETURN_ON_FAILURE(GetLog(), result == VK_SUCCESS, GetReturnCode(result), "Can't create a device: "
        "vkCreateDevice returned %d.", (int32_t)result);

    return Result::SUCCESS;
}

void DeviceVK::CreateCommandQueues()
{
    for (uint32_t i = 0; i < m_FamilyIndices.size(); i++)
    {
        if (m_FamilyIndices[i] == INVALID_FAMILY_INDEX)
            continue;

        VkQueue handle = VK_NULL_HANDLE;
        m_VK.GetDeviceQueue(m_Device, m_FamilyIndices[i], 0, &handle);

        m_Queues[i] = Allocate<CommandQueueVK>(GetStdAllocator(), *this, handle, m_FamilyIndices[i], (CommandQueueType)i);
        m_ConcurrentSharingModeQueueIndices.push_back(m_FamilyIndices[i]);
    }
}

void DeviceVK::SetDebugNameToTrivialObject(VkObjectType objectType, uint64_t handle, const char* name)
{
    if (!m_VK.SetDebugUtilsObjectNameEXT)
        return;

    VkDebugUtilsObjectNameInfoEXT info = {
        VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT,
        nullptr,
        objectType,
        (uint64_t)handle,
        name
    };

    const VkResult result = m_VK.SetDebugUtilsObjectNameEXT(m_Device, &info);

    RETURN_ON_FAILURE(GetLog(), result == VK_SUCCESS, ReturnVoid(),
        "Can't set a debug name to an object: vkSetDebugUtilsObjectNameEXT returned %d.", (int32_t)result);
}

void DeviceVK::SetDebugNameToDeviceGroupObject(VkObjectType objectType, const uint64_t* handles, const char* name)
{
    if (!m_VK.SetDebugUtilsObjectNameEXT)
        return;

    const size_t nameLength = strlen(name);
    constexpr size_t deviceIndexSuffixLength = 16; // " (PD%u)"

    char* nameWithDeviceIndex = STACK_ALLOC(char, nameLength + deviceIndexSuffixLength);
    memcpy(nameWithDeviceIndex, name, nameLength);

    VkDebugUtilsObjectNameInfoEXT info = {
        VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT,
        nullptr,
        objectType,
        (uint64_t)0,
        nameWithDeviceIndex
    };

    for (uint32_t i = 0; i < m_DeviceDesc.physicalDeviceNum; i++)
    {
        if (handles[i] != 0)
        {
            info.objectHandle = (uint64_t)handles[i];
            snprintf(nameWithDeviceIndex + nameLength, deviceIndexSuffixLength, " (PD%u)", i);

            const VkResult result = m_VK.SetDebugUtilsObjectNameEXT(m_Device, &info);
            RETURN_ON_FAILURE(GetLog(), result == VK_SUCCESS, ReturnVoid(),
                "Can't set a debug name to an object: vkSetDebugUtilsObjectNameEXT returned %d.", (int32_t)result);
        }
    }
}

void DeviceVK::ReportDeviceGroupInfo()
{
    REPORT_INFO(GetLog(), "Available device memory heaps:");

    for (uint32_t i = 0; i < m_MemoryProps.memoryHeapCount; i++)
    {
        String text(GetStdAllocator());
        if (m_MemoryProps.memoryHeaps[i].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT)
            text += "DEVICE_LOCAL_BIT ";
        if (m_MemoryProps.memoryHeaps[i].flags & VK_MEMORY_HEAP_MULTI_INSTANCE_BIT)
            text += "MULTI_INSTANCE_BIT ";

        const double size = double(m_MemoryProps.memoryHeaps[i].size) / (1024.0 * 1024.0);

        REPORT_INFO(GetLog(), "  Heap%u %.1lfMiB - %s", i, size, text.c_str());

        if (m_DeviceDesc.physicalDeviceNum == 1)
            continue;

        for (uint32_t j = 0; j < m_DeviceDesc.physicalDeviceNum; j++)
        {
            REPORT_INFO(GetLog(), "    PhysicalDevice%u", j);

            for (uint32_t k = 0; k < m_DeviceDesc.physicalDeviceNum; k++)
            {
                if (j == k)
                    continue;

                VkPeerMemoryFeatureFlags flags = 0;
                m_VK.GetDeviceGroupPeerMemoryFeatures(m_Device, i, j, k, &flags);

                text.clear();
                if (flags & VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT)
                    text += "COPY_SRC_BIT ";
                if (flags & VK_PEER_MEMORY_FEATURE_COPY_DST_BIT)
                    text += "COPY_DST_BIT ";
                if (flags & VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT)
                    text += "GENERIC_SRC_BIT ";
                if (flags & VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT)
                    text += "GENERIC_DST_BIT ";

                REPORT_INFO(GetLog(), "      PhysicalDevice%u - %s", k, text.c_str());
            }
        }
    }
}

#define RESOLVE_OPTIONAL_DEVICE_FUNCTION( name ) \
    m_VK.name = (PFN_vk ## name)m_VK.GetDeviceProcAddr(m_Device, "vk" #name)

#define RESOLVE_DEVICE_FUNCTION( name ) \
    RESOLVE_OPTIONAL_DEVICE_FUNCTION(name); \
    if (!m_VK.name) \
    { \
        REPORT_ERROR(GetLog(), "Failed to get device function: '%s'.", #name); \
        return Result::UNSUPPORTED; \
    }

#define RESOLVE_DEVICE_FUNCTION_WITH_OTHER_NAME( functionName, otherName ) \
    m_VK.functionName = (PFN_vk ## functionName)m_VK.GetDeviceProcAddr(m_Device, otherName); \
    if (!m_VK.functionName) \
    { \
        REPORT_ERROR(GetLog(), "Failed to get device function: '" otherName "'."); \
        return Result::UNSUPPORTED; \
    }

#define RESOLVE_INSTANCE_FUNCTION( name ) \
    m_VK.name = (PFN_vk ## name)m_VK.GetInstanceProcAddr(m_Instance, "vk" #name); \
    if (!m_VK.name) \
    { \
        REPORT_ERROR(GetLog(), "Failed to get instance function: '%s'.", #name); \
        return Result::UNSUPPORTED; \
    }

#define RESOLVE_PRE_INSTANCE_FUNCTION( name ) \
    m_VK.name = (PFN_vk ## name)m_VK.GetInstanceProcAddr(VK_NULL_HANDLE, "vk" #name); \
    if (!m_VK.name) \
    { \
        REPORT_ERROR(GetLog(), "Failed to get instance function: '%s'.", #name); \
        return Result::UNSUPPORTED; \
    }

Result DeviceVK::ResolvePreInstanceDispatchTable()
{
    m_VK = {};

    m_VK.GetInstanceProcAddr = (PFN_vkGetInstanceProcAddr)GetSharedLibraryFunction(*m_Loader, "vkGetInstanceProcAddr");
    if (!m_VK.GetInstanceProcAddr)
    {
        REPORT_ERROR(GetLog(), "Failed to get vkGetInstanceProcAddr.");
        return Result::UNSUPPORTED;
    }

    RESOLVE_PRE_INSTANCE_FUNCTION(CreateInstance);
    RESOLVE_PRE_INSTANCE_FUNCTION(EnumerateInstanceExtensionProperties);
    RESOLVE_PRE_INSTANCE_FUNCTION(EnumerateInstanceLayerProperties);

    return Result::SUCCESS;
}

Result DeviceVK::ResolveInstanceDispatchTable()
{
    RESOLVE_INSTANCE_FUNCTION(GetDeviceProcAddr);
    RESOLVE_INSTANCE_FUNCTION(DestroyInstance);
    RESOLVE_INSTANCE_FUNCTION(DestroyDevice);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceMemoryProperties);
    RESOLVE_INSTANCE_FUNCTION(GetDeviceGroupPeerMemoryFeatures);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceFormatProperties);
    RESOLVE_INSTANCE_FUNCTION(CreateDevice);
    RESOLVE_INSTANCE_FUNCTION(GetDeviceQueue);
    RESOLVE_INSTANCE_FUNCTION(EnumeratePhysicalDeviceGroups);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceProperties);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceProperties2);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceFeatures);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceFeatures2);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceQueueFamilyProperties);
    RESOLVE_INSTANCE_FUNCTION(EnumerateDeviceExtensionProperties);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceSurfaceFormatsKHR);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceSurfaceSupportKHR);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceSurfaceCapabilitiesKHR);
    RESOLVE_INSTANCE_FUNCTION(GetPhysicalDeviceSurfacePresentModesKHR);
    RESOLVE_INSTANCE_FUNCTION(DestroySurfaceKHR);

#if VK_USE_PLATFORM_WIN32_KHR
    RESOLVE_INSTANCE_FUNCTION(CreateWin32SurfaceKHR);
#endif
#if VK_USE_PLATFORM_METAL_EXT
    RESOLVE_INSTANCE_FUNCTION(CreateMetalSurfaceEXT);
#endif
#if VK_USE_PLATFORM_XLIB_KHR
    RESOLVE_INSTANCE_FUNCTION(CreateXlibSurfaceKHR);
#endif
#if VK_USE_PLATFORM_WAYLAND_KHR
    RESOLVE_INSTANCE_FUNCTION(CreateWaylandSurfaceKHR);
#endif

    return Result::SUCCESS;
}

Result DeviceVK::ResolveDispatchTable()
{
    RESOLVE_DEVICE_FUNCTION(CreateBuffer);
    RESOLVE_DEVICE_FUNCTION(CreateImage);
    RESOLVE_DEVICE_FUNCTION(CreateBufferView);
    RESOLVE_DEVICE_FUNCTION(CreateImageView);
    RESOLVE_DEVICE_FUNCTION(CreateSampler);
    RESOLVE_DEVICE_FUNCTION(CreateRenderPass);
    RESOLVE_DEVICE_FUNCTION(CreateFramebuffer);
    RESOLVE_DEVICE_FUNCTION(CreateQueryPool);
    RESOLVE_DEVICE_FUNCTION(CreateCommandPool);
    RESOLVE_DEVICE_FUNCTION(CreateSemaphore);
    RESOLVE_DEVICE_FUNCTION(CreateDescriptorPool);
    RESOLVE_DEVICE_FUNCTION(CreatePipelineLayout);
    RESOLVE_DEVICE_FUNCTION(CreateDescriptorSetLayout);
    RESOLVE_DEVICE_FUNCTION(CreateShaderModule);
    RESOLVE_DEVICE_FUNCTION(CreateGraphicsPipelines);
    RESOLVE_DEVICE_FUNCTION(CreateComputePipelines);
    RESOLVE_DEVICE_FUNCTION(CreateSwapchainKHR);

    RESOLVE_DEVICE_FUNCTION(DestroyBuffer);
    RESOLVE_DEVICE_FUNCTION(DestroyImage);
    RESOLVE_DEVICE_FUNCTION(DestroyBufferView);
    RESOLVE_DEVICE_FUNCTION(DestroyImageView);
    RESOLVE_DEVICE_FUNCTION(DestroySampler);
    RESOLVE_DEVICE_FUNCTION(DestroyRenderPass);
    RESOLVE_DEVICE_FUNCTION(DestroyFramebuffer);
    RESOLVE_DEVICE_FUNCTION(DestroyQueryPool);
    RESOLVE_DEVICE_FUNCTION(DestroyCommandPool);
    RESOLVE_DEVICE_FUNCTION(DestroySemaphore);
    RESOLVE_DEVICE_FUNCTION(DestroyDescriptorPool);
    RESOLVE_DEVICE_FUNCTION(DestroyPipelineLayout);
    RESOLVE_DEVICE_FUNCTION(DestroyDescriptorSetLayout);
    RESOLVE_DEVICE_FUNCTION(DestroyShaderModule);
    RESOLVE_DEVICE_FUNCTION(DestroyPipeline);
    RESOLVE_DEVICE_FUNCTION(DestroySwapchainKHR);

    RESOLVE_DEVICE_FUNCTION(AllocateMemory);
    RESOLVE_DEVICE_FUNCTION(MapMemory);
    RESOLVE_DEVICE_FUNCTION(UnmapMemory);
    RESOLVE_DEVICE_FUNCTION(FreeMemory);

    RESOLVE_OPTIONAL_DEVICE_FUNCTION(BindBufferMemory2);
    if (!m_VK.BindBufferMemory2)
        RESOLVE_DEVICE_FUNCTION_WITH_OTHER_NAME(BindBufferMemory2, "vkBindBufferMemory2KHR");

    RESOLVE_OPTIONAL_DEVICE_FUNCTION(BindImageMemory2);
    if (!m_VK.BindImageMemory2)
        RESOLVE_DEVICE_FUNCTION_WITH_OTHER_NAME(BindImageMemory2, "vkBindImageMemory2KHR");

    RESOLVE_OPTIONAL_DEVICE_FUNCTION(GetBufferMemoryRequirements2);
    if (!m_VK.GetBufferMemoryRequirements2)
        RESOLVE_DEVICE_FUNCTION_WITH_OTHER_NAME(GetBufferMemoryRequirements2, "vkGetBufferMemoryRequirements2KHR");

    RESOLVE_OPTIONAL_DEVICE_FUNCTION(GetImageMemoryRequirements2);
    if (!m_VK.GetImageMemoryRequirements2)
        RESOLVE_DEVICE_FUNCTION_WITH_OTHER_NAME(GetImageMemoryRequirements2, "vkGetImageMemoryRequirements2KHR");

    RESOLVE_DEVICE_FUNCTION(QueueWaitIdle);
    RESOLVE_DEVICE_FUNCTION(AcquireNextImageKHR);
    RESOLVE_DEVICE_FUNCTION(QueueSubmit);
    RESOLVE_DEVICE_FUNCTION(QueuePresentKHR);

    RESOLVE_DEVICE_FUNCTION(GetSemaphoreCounterValue);
    RESOLVE_DEVICE_FUNCTION(WaitSemaphores);

    RESOLVE_DEVICE_FUNCTION(ResetCommandPool);
    RESOLVE_DEVICE_FUNCTION(ResetDescriptorPool);
    RESOLVE_DEVICE_FUNCTION(AllocateCommandBuffers);
    RESOLVE_DEVICE_FUNCTION(AllocateDescriptorSets);
    RESOLVE_DEVICE_FUNCTION(FreeCommandBuffers);
    RESOLVE_DEVICE_FUNCTION(FreeDescriptorSets);
    RESOLVE_DEVICE_FUNCTION(UpdateDescriptorSets);

    RESOLVE_DEVICE_FUNCTION(BeginCommandBuffer);
    RESOLVE_DEVICE_FUNCTION(CmdSetDepthBounds);
    RESOLVE_DEVICE_FUNCTION(CmdSetViewport);
    RESOLVE_DEVICE_FUNCTION(CmdSetScissor);
    RESOLVE_DEVICE_FUNCTION(CmdSetStencilReference);
    RESOLVE_DEVICE_FUNCTION(CmdClearAttachments);
    RESOLVE_DEVICE_FUNCTION(CmdClearColorImage);
    RESOLVE_DEVICE_FUNCTION(CmdBeginRenderPass);
    RESOLVE_DEVICE_FUNCTION(CmdBindVertexBuffers);
    RESOLVE_DEVICE_FUNCTION(CmdBindIndexBuffer);
    RESOLVE_DEVICE_FUNCTION(CmdBindPipeline);
    RESOLVE_DEVICE_FUNCTION(CmdBindDescriptorSets);
    RESOLVE_DEVICE_FUNCTION(CmdPushConstants);
    RESOLVE_DEVICE_FUNCTION(CmdDispatch);
    RESOLVE_DEVICE_FUNCTION(CmdDispatchIndirect);
    RESOLVE_DEVICE_FUNCTION(CmdDraw);
    RESOLVE_DEVICE_FUNCTION(CmdDrawIndexed);
    RESOLVE_DEVICE_FUNCTION(CmdDrawIndirect);
    RESOLVE_DEVICE_FUNCTION(CmdDrawIndexedIndirect);
    RESOLVE_DEVICE_FUNCTION(CmdCopyBuffer);
    RESOLVE_DEVICE_FUNCTION(CmdCopyImage);
    RESOLVE_DEVICE_FUNCTION(CmdCopyBufferToImage);
    RESOLVE_DEVICE_FUNCTION(CmdCopyImageToBuffer);
    RESOLVE_DEVICE_FUNCTION(CmdPipelineBarrier);
    RESOLVE_DEVICE_FUNCTION(CmdBeginQuery);
    RESOLVE_DEVICE_FUNCTION(CmdEndQuery);
    RESOLVE_DEVICE_FUNCTION(CmdWriteTimestamp);
    RESOLVE_DEVICE_FUNCTION(CmdCopyQueryPoolResults);
    RESOLVE_DEVICE_FUNCTION(CmdResetQueryPool);
    RESOLVE_DEVICE_FUNCTION(CmdEndRenderPass);
    RESOLVE_DEVICE_FUNCTION(CmdFillBuffer);
    RESOLVE_DEVICE_FUNCTION(EndCommandBuffer);

    RESOLVE_DEVICE_FUNCTION(GetSwapchainImagesKHR);

    if (m_IsDebugUtilsSupported)
    {
        RESOLVE_DEVICE_FUNCTION(SetDebugUtilsObjectNameEXT);
        RESOLVE_DEVICE_FUNCTION(CmdBeginDebugUtilsLabelEXT);
        RESOLVE_DEVICE_FUNCTION(CmdEndDebugUtilsLabelEXT);
    }

    if (m_IsHDRExtSupported)
    {
        RESOLVE_OPTIONAL_DEVICE_FUNCTION(SetHdrMetadataEXT);
        m_IsHDRExtSupported = m_VK.SetHdrMetadataEXT != nullptr;
    }

    if (m_IsRayTracingExtSupported)
    {
        RESOLVE_DEVICE_FUNCTION(CreateAccelerationStructureKHR);
        RESOLVE_DEVICE_FUNCTION(CreateRayTracingPipelinesKHR);
        RESOLVE_DEVICE_FUNCTION(DestroyAccelerationStructureKHR);
        RESOLVE_DEVICE_FUNCTION(GetAccelerationStructureDeviceAddressKHR);
        RESOLVE_DEVICE_FUNCTION(GetAccelerationStructureBuildSizesKHR);
        RESOLVE_DEVICE_FUNCTION(GetRayTracingShaderGroupHandlesKHR);
        RESOLVE_DEVICE_FUNCTION(CmdBuildAccelerationStructuresKHR);
        RESOLVE_DEVICE_FUNCTION(CmdCopyAccelerationStructureKHR);
        RESOLVE_DEVICE_FUNCTION(CmdWriteAccelerationStructuresPropertiesKHR);
        RESOLVE_DEVICE_FUNCTION(CmdTraceRaysKHR);
        RESOLVE_DEVICE_FUNCTION(GetBufferDeviceAddress);
    }

    if (m_IsMeshShaderExtSupported)
    {
        RESOLVE_DEVICE_FUNCTION(CmdDrawMeshTasksEXT);
    }

    return Result::SUCCESS;
}

void DeviceVK::Destroy()
{
    Deallocate(GetStdAllocator(), this);
}

Result CreateDeviceVK(const DeviceCreationDesc& deviceCreationDesc, DeviceBase*& device)
{
    Log log(GraphicsAPI::VULKAN, deviceCreationDesc.callbackInterface);
    StdAllocator<uint8_t> allocator(deviceCreationDesc.memoryAllocatorInterface);

    DeviceVK* implementation = Allocate<DeviceVK>(allocator, log, allocator);

    const Result res = implementation->Create(deviceCreationDesc);

    if (res == Result::SUCCESS)
    {
        device = implementation;
        return Result::SUCCESS;
    }

    Deallocate(allocator, implementation);
    return res;
}

Result CreateDeviceVK(const DeviceCreationVulkanDesc& deviceCreationDesc, DeviceBase*& device)
{
    Log log(GraphicsAPI::VULKAN, deviceCreationDesc.callbackInterface);
    StdAllocator<uint8_t> allocator(deviceCreationDesc.memoryAllocatorInterface);

    DeviceVK* implementation = Allocate<DeviceVK>(allocator, log, allocator);
    const Result res = implementation->Create(deviceCreationDesc);

    if (res == Result::SUCCESS)
    {
        device = implementation;
        return Result::SUCCESS;
    }

    Deallocate(allocator, implementation);
    return res;
}

//================================================================================================================
// NRI
//================================================================================================================

inline void DeviceVK::SetDebugName(const char* name)
{
    SetDebugNameToTrivialObject(VK_OBJECT_TYPE_DEVICE, (uint64_t)m_Device, name);
}

inline Result DeviceVK::GetCommandQueue(CommandQueueType commandQueueType, CommandQueue*& commandQueue)
{
    SharedScope sharedScope(m_Lock);

    if (m_FamilyIndices[(uint32_t)commandQueueType] == INVALID_FAMILY_INDEX)
        return Result::UNSUPPORTED;

    commandQueue = (CommandQueue*)m_Queues[(uint32_t)commandQueueType];

    return Result::SUCCESS;
}

inline Result DeviceVK::CreateCommandAllocator(const CommandQueue& commandQueue, uint32_t physicalDeviceMask, CommandAllocator*& commandAllocator)
{
    return CreateImplementation<CommandAllocatorVK>(commandAllocator, commandQueue, physicalDeviceMask);
}

inline Result DeviceVK::CreateDescriptorPool(const DescriptorPoolDesc& descriptorPoolDesc, DescriptorPool*& descriptorPool)
{
    return CreateImplementation<DescriptorPoolVK>(descriptorPool, descriptorPoolDesc);
}

Result DeviceVK::CreateBuffer(const BufferDesc& bufferDesc, Buffer*& buffer) // TODO: not inline
{
    return CreateImplementation<BufferVK>(buffer, bufferDesc);
}

inline Result DeviceVK::CreateTexture(const TextureDesc& textureDesc, Texture*& texture)
{
    return CreateImplementation<TextureVK>(texture, textureDesc);
}

inline Result DeviceVK::CreateBufferView(const BufferViewDesc& bufferViewDesc, Descriptor*& bufferView)
{
    return CreateImplementation<DescriptorVK>(bufferView, bufferViewDesc);
}

inline Result DeviceVK::CreateTexture1DView(const Texture1DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    return CreateImplementation<DescriptorVK>(textureView, textureViewDesc);
}

inline Result DeviceVK::CreateTexture2DView(const Texture2DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    return CreateImplementation<DescriptorVK>(textureView, textureViewDesc);
}

inline Result DeviceVK::CreateTexture3DView(const Texture3DViewDesc& textureViewDesc, Descriptor*& textureView)
{
    return CreateImplementation<DescriptorVK>(textureView, textureViewDesc);
}

inline Result DeviceVK::CreateSampler(const SamplerDesc& samplerDesc, Descriptor*& sampler)
{
    return CreateImplementation<DescriptorVK>(sampler, samplerDesc);
}

inline Result DeviceVK::CreatePipelineLayout(const PipelineLayoutDesc& pipelineLayoutDesc, PipelineLayout*& pipelineLayout)
{
    return CreateImplementation<PipelineLayoutVK>(pipelineLayout, pipelineLayoutDesc);
}

inline Result DeviceVK::CreatePipeline(const GraphicsPipelineDesc& graphicsPipelineDesc, Pipeline*& pipeline)
{
    return CreateImplementation<PipelineVK>(pipeline, graphicsPipelineDesc);
}

inline Result DeviceVK::CreatePipeline(const ComputePipelineDesc& computePipelineDesc, Pipeline*& pipeline)
{
    return CreateImplementation<PipelineVK>(pipeline, computePipelineDesc);
}

inline Result DeviceVK::CreateFrameBuffer(const FrameBufferDesc& frameBufferDesc, FrameBuffer*& frameBuffer)
{
    return CreateImplementation<FrameBufferVK>(frameBuffer, frameBufferDesc);
}

inline Result DeviceVK::CreateQueryPool(const QueryPoolDesc& queryPoolDesc, QueryPool*& queryPool)
{
    return CreateImplementation<QueryPoolVK>(queryPool, queryPoolDesc);
}

inline Result DeviceVK::CreateFence(uint64_t initialValue, Fence*& fence)
{
    return CreateImplementation<FenceVK>(fence, initialValue);
}

inline Result DeviceVK::CreateSwapChain(const SwapChainDesc& swapChainDesc, SwapChain*& swapChain)
{
    return CreateImplementation<SwapChainVK>(swapChain, swapChainDesc);
}

inline Result DeviceVK::CreatePipeline(const RayTracingPipelineDesc& rayTracingPipelineDesc, Pipeline*& pipeline)
{
    return CreateImplementation<PipelineVK>(pipeline, rayTracingPipelineDesc);
}

inline Result DeviceVK::CreateAccelerationStructure(const AccelerationStructureDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return CreateImplementation<AccelerationStructureVK>(accelerationStructure, accelerationStructureDesc);
}

inline Result DeviceVK::CreateCommandQueue(const CommandQueueVulkanDesc& commandQueueVulkanDesc, CommandQueue*& commandQueue)
{
    const uint32_t commandQueueTypeIndex = (uint32_t)commandQueueVulkanDesc.commandQueueType;

    ExclusiveScope exclusiveScope(m_Lock);

    const bool isFamilyIndexSame = m_FamilyIndices[commandQueueTypeIndex] == commandQueueVulkanDesc.familyIndex;
    const bool isQueueSame = (VkQueue)m_Queues[commandQueueTypeIndex] == (VkQueue)commandQueueVulkanDesc.vkQueue;
    if (isFamilyIndexSame && isQueueSame)
    {
        commandQueue = (CommandQueue*)m_Queues[commandQueueTypeIndex];
        return Result::SUCCESS;
    }

    CreateImplementation<CommandQueueVK>(commandQueue, commandQueueVulkanDesc);

    if (m_Queues[commandQueueTypeIndex] != nullptr)
        Deallocate(GetStdAllocator(), m_Queues[commandQueueTypeIndex]);

    m_FamilyIndices[commandQueueTypeIndex] = commandQueueVulkanDesc.familyIndex;
    m_Queues[commandQueueTypeIndex] = (CommandQueueVK*)commandQueue;

    return Result::SUCCESS;
}

inline Result DeviceVK::CreateCommandAllocator(const CommandAllocatorVulkanDesc& commandAllocatorVulkanDesc, CommandAllocator*& commandAllocator)
{
    return CreateImplementation<CommandAllocatorVK>(commandAllocator, commandAllocatorVulkanDesc);
}

inline Result DeviceVK::CreateCommandBuffer(const CommandBufferVulkanDesc& commandBufferVulkanDesc, CommandBuffer*& commandBuffer)
{
    return CreateImplementation<CommandBufferVK>(commandBuffer, commandBufferVulkanDesc);
}

inline Result DeviceVK::CreateDescriptorPool(NRIVkDescriptorPool vkDescriptorPool, DescriptorPool*& descriptorPool)
{
    return CreateImplementation<DescriptorPoolVK>(descriptorPool, vkDescriptorPool);
}

inline Result DeviceVK::CreateBuffer(const BufferVulkanDesc& bufferDesc, Buffer*& buffer)
{
    return CreateImplementation<BufferVK>(buffer, bufferDesc);
}

inline Result DeviceVK::CreateTexture(const TextureVulkanDesc& textureVulkanDesc, Texture*& texture)
{
    return CreateImplementation<TextureVK>(texture, textureVulkanDesc);
}

inline Result DeviceVK::CreateMemory(const MemoryVulkanDesc& memoryVulkanDesc, Memory*& memory)
{
    return CreateImplementation<MemoryVK>(memory, memoryVulkanDesc);
}

inline Result DeviceVK::CreateGraphicsPipeline(NRIVkPipeline vkPipeline, Pipeline*& pipeline)
{
    PipelineVK* implementation = Allocate<PipelineVK>(GetStdAllocator(), *this);
    const Result result = implementation->CreateGraphics(vkPipeline);

    if (result != Result::SUCCESS)
    {
        pipeline = (Pipeline*)implementation;
        return result;
    }

    Deallocate(GetStdAllocator(), implementation);

    return result;
}

inline Result DeviceVK::CreateComputePipeline(NRIVkPipeline vkPipeline, Pipeline*& pipeline)
{
    PipelineVK* implementation = Allocate<PipelineVK>(GetStdAllocator(), *this);
    const Result result = implementation->CreateCompute(vkPipeline);

    if (result != Result::SUCCESS)
    {
        pipeline = (Pipeline*)implementation;
        return result;
    }

    Deallocate(GetStdAllocator(), implementation);

    return result;
}

inline Result DeviceVK::CreateQueryPool(const QueryPoolVulkanDesc& queryPoolVulkanDesc, QueryPool*& queryPool)
{
    return CreateImplementation<QueryPoolVK>(queryPool, queryPoolVulkanDesc);
}

inline Result DeviceVK::CreateAccelerationStructure(const AccelerationStructureVulkanDesc& accelerationStructureDesc, AccelerationStructure*& accelerationStructure)
{
    return CreateImplementation<AccelerationStructureVK>(accelerationStructure, accelerationStructureDesc);
}

inline void DeviceVK::DestroyCommandAllocator(CommandAllocator& commandAllocator)
{
    Deallocate(GetStdAllocator(), (CommandAllocatorVK*)&commandAllocator);
}

inline void DeviceVK::DestroyDescriptorPool(DescriptorPool& descriptorPool)
{
    Deallocate(GetStdAllocator(), (DescriptorPoolVK*)&descriptorPool);
}

void DeviceVK::DestroyBuffer(Buffer& buffer) // TODO: not inline
{
    Deallocate(GetStdAllocator(), (BufferVK*)&buffer);
}

inline void DeviceVK::DestroyTexture(Texture& texture)
{
    Deallocate(GetStdAllocator(), (TextureVK*)&texture);
}

inline void DeviceVK::DestroyDescriptor(Descriptor& descriptor)
{
    Deallocate(GetStdAllocator(), (DescriptorVK*)&descriptor);
}

inline void DeviceVK::DestroyPipelineLayout(PipelineLayout& pipelineLayout)
{
    Deallocate(GetStdAllocator(), (PipelineLayoutVK*)&pipelineLayout);
}

inline void DeviceVK::DestroyPipeline(Pipeline& pipeline)
{
    Deallocate(GetStdAllocator(), (PipelineVK*)&pipeline);
}

inline void DeviceVK::DestroyFrameBuffer(FrameBuffer& frameBuffer)
{
    Deallocate(GetStdAllocator(), (FrameBufferVK*)&frameBuffer);
}

inline void DeviceVK::DestroyQueryPool(QueryPool& queryPool)
{
    Deallocate(GetStdAllocator(), (QueryPoolVK*)&queryPool);
}

inline void DeviceVK::DestroyFence(Fence& fence)
{
    Deallocate(GetStdAllocator(), (FenceVK*)&fence);
}

inline void DeviceVK::DestroySwapChain(SwapChain& swapChain)
{
    Deallocate(GetStdAllocator(), (SwapChainVK*)&swapChain);
}

inline void DeviceVK::DestroyAccelerationStructure(AccelerationStructure& accelerationStructure)
{
    Deallocate(GetStdAllocator(), (AccelerationStructureVK*)&accelerationStructure);
}

inline Result DeviceVK::GetDisplays(Display** displays, uint32_t& displayNum)
{
    MaybeUnused(displays, displayNum);

#if _WIN32
    if (!m_Adapter)
        return Result::UNSUPPORTED;

    HRESULT result = S_OK;

    if (displays == nullptr || displayNum == 0)
    {
        UINT i = 0;
        for(; result != DXGI_ERROR_NOT_FOUND; i++)
        {
            ComPtr<IDXGIOutput> output;
            result = m_Adapter->EnumOutputs(i, &output);
        }

        displayNum = i;
        return Result::SUCCESS;
    }

    UINT i = 0;
    for(; result != DXGI_ERROR_NOT_FOUND && i < displayNum; i++)
    {
        ComPtr<IDXGIOutput> output;
        result = m_Adapter->EnumOutputs(i, &output);
        if (result != DXGI_ERROR_NOT_FOUND)
            displays[i] = (Display*)(size_t)(i + 1);
    }

    for(; i < displayNum; i++)
        displays[i] = nullptr;

    return Result::SUCCESS;
#else
    return Result::UNSUPPORTED;
#endif
}

inline Result DeviceVK::GetDisplaySize(Display& display, uint16_t& width, uint16_t& height)
{
    MaybeUnused(display, width, height);

#if _WIN32
    if (!m_Adapter)
        return Result::UNSUPPORTED;

    Display* address = &display;
    if (!address)
        return Result::UNSUPPORTED;

    const uint32_t index = (*(uint32_t*)&address) - 1;

    ComPtr<IDXGIOutput> output;
    HRESULT result = m_Adapter->EnumOutputs(index, &output);

    if (FAILED(result))
        return Result::UNSUPPORTED;

    DXGI_OUTPUT_DESC outputDesc = {};
    result = output->GetDesc(&outputDesc);

    if (FAILED(result))
        return Result::UNSUPPORTED;

    MONITORINFO monitorInfo = {};
    monitorInfo.cbSize = sizeof(monitorInfo);

    if (!GetMonitorInfoA(outputDesc.Monitor, &monitorInfo))
        return Result::UNSUPPORTED;

    const RECT rect = monitorInfo.rcMonitor;

    width = uint16_t(rect.right - rect.left);
    height = uint16_t(rect.bottom - rect.top);

    return Result::SUCCESS;
#else
    return Result::UNSUPPORTED;
#endif
}

inline Result DeviceVK::AllocateMemory(uint32_t physicalDeviceMask, MemoryType memoryType, uint64_t size, Memory*& memory)
{
    return CreateImplementation<MemoryVK>(memory, physicalDeviceMask, memoryType, size);
}

inline Result DeviceVK::BindBufferMemory(const BufferMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    if (memoryBindingDescNum == 0)
        return Result::SUCCESS;

    const uint32_t infoMaxNum = memoryBindingDescNum * m_DeviceDesc.physicalDeviceNum;

    VkBindBufferMemoryInfo* infos = STACK_ALLOC(VkBindBufferMemoryInfo, infoMaxNum);
    uint32_t infoNum = 0;

    VkBindBufferMemoryDeviceGroupInfo* deviceGroupInfos = nullptr;
    if (m_DeviceDesc.physicalDeviceNum > 1)
        deviceGroupInfos = STACK_ALLOC(VkBindBufferMemoryDeviceGroupInfo, infoMaxNum);

    for (uint32_t i = 0; i < memoryBindingDescNum; i++)
    {
        const BufferMemoryBindingDesc& bindingDesc = memoryBindingDescs[i];

        MemoryVK& memoryImpl = *(MemoryVK*)bindingDesc.memory;
        BufferVK& bufferImpl = *(BufferVK*)bindingDesc.buffer;

        const MemoryTypeUnpack unpack = { memoryImpl.GetType() };
        const MemoryTypeInfo& memoryTypeInfo = unpack.info;

        const MemoryLocation memoryLocation = (MemoryLocation)memoryTypeInfo.location;

        uint32_t physicalDeviceMask = GetPhysicalDeviceGroupMask(bindingDesc.physicalDeviceMask);
        if (IsHostVisibleMemory(memoryLocation))
            physicalDeviceMask = 0x1;

        if (memoryTypeInfo.isDedicated == 1)
            memoryImpl.CreateDedicated(bufferImpl, physicalDeviceMask);

        for (uint32_t j = 0; j < m_DeviceDesc.physicalDeviceNum; j++)
        {
            if ((1u << j) & physicalDeviceMask)
            {
                VkBindBufferMemoryInfo& info = infos[infoNum++];

                info = {};
                info.sType = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO;
                info.buffer = bufferImpl.GetHandle(j);
                info.memory = memoryImpl.GetHandle(j);
                info.memoryOffset = bindingDesc.offset;

                if (IsHostVisibleMemory(memoryLocation))
                    bufferImpl.SetHostMemory(memoryImpl, info.memoryOffset);

                if (deviceGroupInfos != nullptr)
                {
                    VkBindBufferMemoryDeviceGroupInfo& deviceGroupInfo = deviceGroupInfos[infoNum - 1];
                    deviceGroupInfo = {};
                    deviceGroupInfo.sType = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO;
                    deviceGroupInfo.deviceIndexCount = m_DeviceDesc.physicalDeviceNum;
                    deviceGroupInfo.pDeviceIndices = &m_PhysicalDeviceIndices[j * m_DeviceDesc.physicalDeviceNum];
                    info.pNext = &deviceGroupInfo;
                }
            }
        }
    }

    VkResult result = VK_SUCCESS;
    if (infoNum > 0)
        result = m_VK.BindBufferMemory2(m_Device, infoNum, infos);

    RETURN_ON_FAILURE(GetLog(), result == VK_SUCCESS, GetReturnCode(result),
        "Can't bind a memory to a buffer: vkBindBufferMemory2 returned %d.", (int32_t)result);

    for (uint32_t i = 0; i < memoryBindingDescNum; i++)
    {
        BufferVK& bufferImpl = *(BufferVK*)memoryBindingDescs[i].buffer;
        bufferImpl.ReadDeviceAddress();
    }

    return Result::SUCCESS;
}

inline Result DeviceVK::BindTextureMemory(const TextureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    const uint32_t infoMaxNum = memoryBindingDescNum * m_DeviceDesc.physicalDeviceNum;

    VkBindImageMemoryInfo* infos = STACK_ALLOC(VkBindImageMemoryInfo, infoMaxNum);
    uint32_t infoNum = 0;

    VkBindImageMemoryDeviceGroupInfo* deviceGroupInfos = nullptr;
    if (m_DeviceDesc.physicalDeviceNum > 1)
        deviceGroupInfos = STACK_ALLOC(VkBindImageMemoryDeviceGroupInfo, infoMaxNum);

    for (uint32_t i = 0; i < memoryBindingDescNum; i++)
    {
        const TextureMemoryBindingDesc& bindingDesc = memoryBindingDescs[i];

        const uint32_t physicalDeviceMask = GetPhysicalDeviceGroupMask(bindingDesc.physicalDeviceMask);

        MemoryVK& memoryImpl = *(MemoryVK*)bindingDesc.memory;
        TextureVK& textureImpl = *(TextureVK*)bindingDesc.texture;

        const MemoryTypeUnpack unpack = { memoryImpl.GetType() };
        const MemoryTypeInfo& memoryTypeInfo = unpack.info;

        if (memoryTypeInfo.isDedicated == 1)
            memoryImpl.CreateDedicated(textureImpl, physicalDeviceMask);

        for (uint32_t j = 0; j < m_DeviceDesc.physicalDeviceNum; j++)
        {
            if ((1u << j) & physicalDeviceMask)
            {
                VkBindImageMemoryInfo& info = infos[infoNum++];
                info.sType = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO;
                info.pNext = nullptr;
                info.image = textureImpl.GetHandle(j);
                info.memory = memoryImpl.GetHandle(j);
                info.memoryOffset = bindingDesc.offset;

                if (deviceGroupInfos != nullptr)
                {
                    VkBindImageMemoryDeviceGroupInfo& deviceGroupInfo = deviceGroupInfos[infoNum - 1];
                    deviceGroupInfo = {};
                    deviceGroupInfo.sType = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO;
                    deviceGroupInfo.deviceIndexCount = m_DeviceDesc.physicalDeviceNum;
                    deviceGroupInfo.pDeviceIndices = &m_PhysicalDeviceIndices[j * m_DeviceDesc.physicalDeviceNum];
                    info.pNext = &deviceGroupInfo;
                }
            }
        }
    }

    VkResult result = VK_SUCCESS;
    if (infoNum > 0)
        result = m_VK.BindImageMemory2(m_Device, infoNum, infos);

    RETURN_ON_FAILURE(GetLog(), result == VK_SUCCESS, GetReturnCode(result),
        "Can't bind a memory to a texture: vkBindImageMemory2 returned %d.", (int32_t)result);

    return Result::SUCCESS;
}

inline Result DeviceVK::BindAccelerationStructureMemory(const AccelerationStructureMemoryBindingDesc* memoryBindingDescs, uint32_t memoryBindingDescNum)
{
    if (memoryBindingDescNum == 0)
        return Result::SUCCESS;

    BufferMemoryBindingDesc* infos = ALLOCATE_SCRATCH(*this, BufferMemoryBindingDesc, memoryBindingDescNum);

    for (uint32_t i = 0; i < memoryBindingDescNum; i++)
    {
        const AccelerationStructureMemoryBindingDesc& bindingDesc = memoryBindingDescs[i];
        AccelerationStructureVK& accelerationStructure = *(AccelerationStructureVK*)bindingDesc.accelerationStructure;

        BufferMemoryBindingDesc& bufferMemoryBinding = infos[i];
        bufferMemoryBinding = {};
        bufferMemoryBinding.buffer = (Buffer*)accelerationStructure.GetBuffer();
        bufferMemoryBinding.memory = bindingDesc.memory;
        bufferMemoryBinding.offset = bindingDesc.offset;
        bufferMemoryBinding.physicalDeviceMask = bindingDesc.physicalDeviceMask;
    }

    Result result = BindBufferMemory(infos, memoryBindingDescNum);

    for (uint32_t i = 0; i < memoryBindingDescNum && result == Result::SUCCESS; i++)
    {
        AccelerationStructureVK& accelerationStructure = *(AccelerationStructureVK*)memoryBindingDescs[i].accelerationStructure;
        result = accelerationStructure.FinishCreation();
    }

    FREE_SCRATCH(*this, infos, memoryBindingDescNum);

    return result;
}

inline void DeviceVK::FreeMemory(Memory& memory)
{
    Deallocate(GetStdAllocator(), (MemoryVK*)&memory);
}

inline FormatSupportBits DeviceVK::GetFormatSupport(Format format) const
{
    const VkFormat vulkanFormat = GetVkFormat(format);
    const VkPhysicalDevice physicalDevice = m_PhysicalDevices.front();

    VkFormatProperties formatProperties = {};
    m_VK.GetPhysicalDeviceFormatProperties(physicalDevice, vulkanFormat, &formatProperties);

    constexpr uint32_t transferBits = VK_FORMAT_FEATURE_TRANSFER_DST_BIT | VK_FORMAT_FEATURE_TRANSFER_SRC_BIT;

    constexpr uint32_t textureBits = VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT | transferBits;
    constexpr uint32_t storageTextureBits = VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT | transferBits;
    constexpr uint32_t bufferBits = VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT | transferBits;
    constexpr uint32_t storageBufferBits = VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT | transferBits;
    constexpr uint32_t colorAttachmentBits = VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT | VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT | transferBits;
    constexpr uint32_t depthAttachmentBits = VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT | transferBits;
    constexpr uint32_t vertexBufferBits = VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT | transferBits;

    FormatSupportBits mask = FormatSupportBits::UNSUPPORTED;

    if (formatProperties.optimalTilingFeatures & textureBits)
        mask |= FormatSupportBits::TEXTURE;

    if (formatProperties.optimalTilingFeatures & storageTextureBits)
        mask |= FormatSupportBits::STORAGE_TEXTURE;

    if (formatProperties.optimalTilingFeatures & colorAttachmentBits)
        mask |= FormatSupportBits::COLOR_ATTACHMENT;

    if (formatProperties.optimalTilingFeatures & depthAttachmentBits)
        mask |= FormatSupportBits::DEPTH_STENCIL_ATTACHMENT;

    if (formatProperties.bufferFeatures & bufferBits)
        mask |= FormatSupportBits::BUFFER;

    if (formatProperties.bufferFeatures & storageBufferBits)
        mask |= FormatSupportBits::STORAGE_BUFFER;

    if (formatProperties.bufferFeatures & vertexBufferBits)
        mask |= FormatSupportBits::VERTEX_BUFFER;

    return mask;
}

inline uint32_t DeviceVK::CalculateAllocationNumber(const ResourceGroupDesc& resourceGroupDesc) const
{
    HelperDeviceMemoryAllocator allocator(m_CoreInterface, (Device&)*this, m_StdAllocator);

    return allocator.CalculateAllocationNumber(resourceGroupDesc);
}

inline Result DeviceVK::AllocateAndBindMemory(const ResourceGroupDesc& resourceGroupDesc, nri::Memory** allocations)
{
    HelperDeviceMemoryAllocator allocator(m_CoreInterface, (Device&)*this, m_StdAllocator);

    return allocator.AllocateAndBindMemory(resourceGroupDesc, allocations);
}

#include "DeviceVK.hpp"
