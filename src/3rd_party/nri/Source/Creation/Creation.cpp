/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedExternal.h"
#include "DeviceBase.h"

#define NRI_STRINGIFY(name) #name

using namespace nri;

#if NRI_USE_D3D11
Result CreateDeviceD3D11(const DeviceCreationDesc& deviceCreationDesc, DeviceBase*& device);
Result CreateDeviceD3D11(const DeviceCreationD3D11Desc& deviceDesc, DeviceBase*& device);
#endif

#if NRI_USE_D3D12
Result CreateDeviceD3D12(const DeviceCreationDesc& deviceCreationDesc, DeviceBase*& device);
Result CreateDeviceD3D12(const DeviceCreationD3D12Desc& deviceCreationDesc, DeviceBase*& device);
#endif

#if NRI_USE_VULKAN
Result CreateDeviceVK(const DeviceCreationDesc& deviceCreationDesc, DeviceBase*& device);
Result CreateDeviceVK(const DeviceCreationVulkanDesc& deviceDesc, DeviceBase*& device);
#endif

DeviceBase* CreateDeviceValidation(const DeviceCreationDesc& deviceCreationDesc, DeviceBase& device);

constexpr uint64_t Hash( const char* name )
{
    return *name != 0 ? *name ^ ( 33 * Hash(name + 1) ) : 5381;
}

NRI_API Result NRI_CALL nri::GetInterface(const Device& device, const char* interfaceName, size_t interfaceSize, void* interfacePtr)
{
    const uint64_t hash = Hash(interfaceName);
    size_t realInterfaceSize = size_t(-1);
    Result result = Result::INVALID_ARGUMENT;
    const DeviceBase& deviceBase = (DeviceBase&)device;

    if (hash == Hash( NRI_STRINGIFY(nri::CoreInterface) ))
    {
        realInterfaceSize = sizeof(CoreInterface);
        if (realInterfaceSize == interfaceSize)
            result = deviceBase.FillFunctionTable(*(CoreInterface*)interfacePtr);
    }
    else if (hash == Hash( NRI_STRINGIFY(nri::SwapChainInterface) ))
    {
        realInterfaceSize = sizeof(SwapChainInterface);
        if (realInterfaceSize == interfaceSize)
            result = deviceBase.FillFunctionTable(*(SwapChainInterface*)interfacePtr);
    }
    else if (hash == Hash( NRI_STRINGIFY(nri::WrapperD3D11Interface) ))
    {
        realInterfaceSize = sizeof(WrapperD3D11Interface);
        if (realInterfaceSize == interfaceSize)
            result = deviceBase.FillFunctionTable(*(WrapperD3D11Interface*)interfacePtr);
    }
    else if (hash == Hash(NRI_STRINGIFY(nri::WrapperD3D12Interface)))
    {
        realInterfaceSize = sizeof(WrapperD3D12Interface);
        if (realInterfaceSize == interfaceSize)
            result = deviceBase.FillFunctionTable(*(WrapperD3D12Interface*)interfacePtr);
    }
    else if (hash == Hash( NRI_STRINGIFY(nri::WrapperVKInterface) ))
    {
        realInterfaceSize = sizeof(WrapperVKInterface);
        if (realInterfaceSize == interfaceSize)
            result = deviceBase.FillFunctionTable(*(WrapperVKInterface*)interfacePtr);
    }
    else if (hash == Hash( NRI_STRINGIFY(nri::RayTracingInterface) ))
    {
        realInterfaceSize = sizeof(RayTracingInterface);
        if (realInterfaceSize == interfaceSize)
            result = deviceBase.FillFunctionTable(*(RayTracingInterface*)interfacePtr);
    }
    else if (hash == Hash( NRI_STRINGIFY(nri::MeshShaderInterface) ))
    {
        realInterfaceSize = sizeof(MeshShaderInterface);
        if (realInterfaceSize == interfaceSize)
            result = deviceBase.FillFunctionTable(*(MeshShaderInterface*)interfacePtr);
    }
    else if (hash == Hash( NRI_STRINGIFY(nri::HelperInterface) ))
    {
        realInterfaceSize = sizeof(HelperInterface);
        if (realInterfaceSize == interfaceSize)
            result = deviceBase.FillFunctionTable(*(HelperInterface*)interfacePtr);
    }

    if (result == Result::INVALID_ARGUMENT)
        REPORT_ERROR(deviceBase.GetLog(), "Unknown interface '%s'!", interfaceName);
    else if (interfaceSize != realInterfaceSize)
        REPORT_ERROR(deviceBase.GetLog(), "Interface '%s' has invalid size = %u bytes, while %u bytes expected by the implementation", interfaceName, interfaceSize, realInterfaceSize);
    else if (result == Result::UNSUPPORTED)
        REPORT_WARNING(deviceBase.GetLog(), "Interface '%s' is not supported by the device!", interfaceName);

    return result;
}

template< typename T >
Result FinalizeDeviceCreation(const T& deviceCreationDesc, DeviceBase& deviceImpl, Device*& device)
{
    if (deviceCreationDesc.enableNRIValidation)
    {
        Device* deviceVal = (Device*)CreateDeviceValidation(deviceCreationDesc, deviceImpl);
        if (deviceVal == nullptr)
        {
            nri::DestroyDevice((Device&)deviceImpl);
            return Result::FAILURE;
        }

        device = deviceVal;
    }
    else
        device = (Device*)&deviceImpl;

    return Result::SUCCESS;
}

NRI_API Result NRI_CALL nri::CreateDevice(const DeviceCreationDesc& deviceCreationDesc, Device*& device)
{
    Result result = Result::UNSUPPORTED;
    DeviceBase* deviceImpl = nullptr;

    DeviceCreationDesc modifiedDeviceCreationDesc = deviceCreationDesc;
    CheckAndSetDefaultCallbacks(modifiedDeviceCreationDesc.callbackInterface);
    CheckAndSetDefaultAllocator(modifiedDeviceCreationDesc.memoryAllocatorInterface);

    #if (NRI_USE_D3D11 == 1)
        if (modifiedDeviceCreationDesc.graphicsAPI == GraphicsAPI::D3D11)
            result = CreateDeviceD3D11(modifiedDeviceCreationDesc, deviceImpl);
    #endif

    #if (NRI_USE_D3D12 == 1)
        if (modifiedDeviceCreationDesc.graphicsAPI == GraphicsAPI::D3D12)
            result = CreateDeviceD3D12(modifiedDeviceCreationDesc, deviceImpl);
    #endif

    #if (NRI_USE_VULKAN == 1)
        if (modifiedDeviceCreationDesc.graphicsAPI == GraphicsAPI::VULKAN)
            result = CreateDeviceVK(modifiedDeviceCreationDesc, deviceImpl);
    #endif

    if (result != Result::SUCCESS)
        return result;

    return FinalizeDeviceCreation(modifiedDeviceCreationDesc, *deviceImpl, device);
}

NRI_API Result NRI_CALL nri::CreateDeviceFromD3D11Device(const DeviceCreationD3D11Desc& deviceCreationD3D11Desc, Device*& device)
{
    DeviceCreationDesc deviceCreationDesc = {};
    deviceCreationDesc.callbackInterface = deviceCreationD3D11Desc.callbackInterface;
    deviceCreationDesc.memoryAllocatorInterface = deviceCreationD3D11Desc.memoryAllocatorInterface;
    deviceCreationDesc.graphicsAPI = GraphicsAPI::D3D11;
    deviceCreationDesc.enableNRIValidation = deviceCreationD3D11Desc.enableNRIValidation;
    deviceCreationDesc.enableAPIValidation = deviceCreationD3D11Desc.enableAPIValidation;
    deviceCreationDesc.skipLiveObjectsReporting = true;

    CheckAndSetDefaultCallbacks(deviceCreationDesc.callbackInterface);
    CheckAndSetDefaultAllocator(deviceCreationDesc.memoryAllocatorInterface);

    DeviceCreationD3D11Desc tempDeviceCreationD3D11Desc = deviceCreationD3D11Desc;

    CheckAndSetDefaultCallbacks(tempDeviceCreationD3D11Desc.callbackInterface);
    CheckAndSetDefaultAllocator(tempDeviceCreationD3D11Desc.memoryAllocatorInterface);

    Result result = Result::UNSUPPORTED;
    DeviceBase* deviceImpl = nullptr;

    #if (NRI_USE_D3D11 == 1)
        result = CreateDeviceD3D11(tempDeviceCreationD3D11Desc, deviceImpl);
    #endif

    if (result != Result::SUCCESS)
        return result;

    return FinalizeDeviceCreation(deviceCreationDesc, *deviceImpl, device);
}

NRI_API Result NRI_CALL nri::CreateDeviceFromD3D12Device(const DeviceCreationD3D12Desc& deviceCreationD3D12Desc, Device*& device)
{
    DeviceCreationDesc deviceCreationDesc = {};
    deviceCreationDesc.callbackInterface = deviceCreationD3D12Desc.callbackInterface;
    deviceCreationDesc.memoryAllocatorInterface = deviceCreationD3D12Desc.memoryAllocatorInterface;
    deviceCreationDesc.graphicsAPI = GraphicsAPI::D3D12;
    deviceCreationDesc.enableNRIValidation = deviceCreationD3D12Desc.enableNRIValidation;
    deviceCreationDesc.enableAPIValidation = deviceCreationD3D12Desc.enableAPIValidation;

    CheckAndSetDefaultCallbacks(deviceCreationDesc.callbackInterface);
    CheckAndSetDefaultAllocator(deviceCreationDesc.memoryAllocatorInterface);

    DeviceCreationD3D12Desc tempDeviceCreationD3D12Desc = deviceCreationD3D12Desc;

    CheckAndSetDefaultCallbacks(tempDeviceCreationD3D12Desc.callbackInterface);
    CheckAndSetDefaultAllocator(tempDeviceCreationD3D12Desc.memoryAllocatorInterface);

    Result result = Result::UNSUPPORTED;
    DeviceBase* deviceImpl = nullptr;

    #if (NRI_USE_D3D12 == 1)
        result = CreateDeviceD3D12(tempDeviceCreationD3D12Desc, deviceImpl);
    #endif

    if (result != Result::SUCCESS)
        return result;

    return FinalizeDeviceCreation(deviceCreationDesc, *deviceImpl, device);
}

NRI_API Result NRI_CALL nri::CreateDeviceFromVkDevice(const DeviceCreationVulkanDesc& deviceCreationVulkanDesc, Device*& device)
{
    DeviceCreationDesc deviceCreationDesc = {};
    deviceCreationDesc.callbackInterface = deviceCreationVulkanDesc.callbackInterface;
    deviceCreationDesc.memoryAllocatorInterface = deviceCreationVulkanDesc.memoryAllocatorInterface;
    deviceCreationDesc.spirvBindingOffsets = deviceCreationVulkanDesc.spirvBindingOffsets;
    deviceCreationDesc.graphicsAPI = GraphicsAPI::VULKAN;
    deviceCreationDesc.enableNRIValidation = deviceCreationVulkanDesc.enableNRIValidation;
    deviceCreationDesc.enableAPIValidation = deviceCreationVulkanDesc.enableAPIValidation;

    CheckAndSetDefaultCallbacks(deviceCreationDesc.callbackInterface);
    CheckAndSetDefaultAllocator(deviceCreationDesc.memoryAllocatorInterface);

    DeviceCreationVulkanDesc tempDeviceCreationVulkanDesc = deviceCreationVulkanDesc;

    CheckAndSetDefaultCallbacks(tempDeviceCreationVulkanDesc.callbackInterface);
    CheckAndSetDefaultAllocator(tempDeviceCreationVulkanDesc.memoryAllocatorInterface);

    Result result = Result::UNSUPPORTED;
    DeviceBase* deviceImpl = nullptr;

    #if (NRI_USE_VULKAN == 1)
        result = CreateDeviceVK(tempDeviceCreationVulkanDesc, deviceImpl);
    #endif

    if (result != Result::SUCCESS)
        return result;

    return FinalizeDeviceCreation(deviceCreationDesc, *deviceImpl, device);
}

NRI_API void NRI_CALL nri::DestroyDevice(Device& device)
{
    ((DeviceBase&)device).Destroy();
}

NRI_API Format NRI_CALL nri::ConvertVKFormatToNRI(uint32_t vkFormat)
{
    return VKFormatToNRIFormat(vkFormat);
}

NRI_API Format NRI_CALL nri::ConvertDXGIFormatToNRI(uint32_t dxgiFormat)
{
    return DXGIFormatToNRIFormat(dxgiFormat);
}

NRI_API uint32_t NRI_CALL nri::ConvertNRIFormatToVK(Format format)
{
    MaybeUnused(format);

    #if (NRI_USE_VULKAN == 1)
        return NRIFormatToVKFormat(format);
    #else
        return 0;
    #endif
}

NRI_API uint32_t NRI_CALL nri::ConvertNRIFormatToDXGI(Format format)
{
    MaybeUnused(format);

    #if (NRI_USE_D3D11 == 1)
        return NRIFormatToDXGIFormatD3D11(format);
    #elif(NRI_USE_D3D12 == 1)
        return NRIFormatToDXGIFormatD3D12(format);
    #else
        return 0;
    #endif
}

// C compatible exported functions

static_assert(sizeof(uint8_t) == sizeof(nri::Result));
static_assert(sizeof(uint8_t) == sizeof(nri::Format));

NRIC_API uint8_t NRI_CALL nri_GetInterface(const void* device, const char* interfaceName, size_t interfaceSize, void* interfacePtr)
{
    const uint64_t hash = Hash(interfaceName);
    if (hash == Hash(NRI_STRINGIFY(nri_CoreInterface)))
        return (uint8_t)nri::GetInterface(*(const nri::Device*)device, NRI_STRINGIFY(nri::CoreInterface), interfaceSize, interfacePtr);
    else if (hash == Hash(NRI_STRINGIFY(nri_SwapChainInterface)))
        return (uint8_t)nri::GetInterface(*(const nri::Device*)device, NRI_STRINGIFY(nri::SwapChainInterface), interfaceSize, interfacePtr);
    else if (hash == Hash(NRI_STRINGIFY(nri_WrapperD3D11Interface)))
        return (uint8_t)nri::GetInterface(*(const nri::Device*)device, NRI_STRINGIFY(nri::WrapperD3D11Interface), interfaceSize, interfacePtr);
    else if (hash == Hash(NRI_STRINGIFY(nri_WrapperD3D12Interface)))
        return (uint8_t)nri::GetInterface(*(const nri::Device*)device, NRI_STRINGIFY(nri::WrapperD3D12Interface), interfaceSize, interfacePtr);
    else if (hash == Hash(NRI_STRINGIFY(nri_WrapperVKInterface)))
        return (uint8_t)nri::GetInterface(*(const nri::Device*)device, NRI_STRINGIFY(nri::WrapperVKInterface), interfaceSize, interfacePtr);
    else if (hash == Hash(NRI_STRINGIFY(nri_RayTracingInterface)))
        return (uint8_t)nri::GetInterface(*(const nri::Device*)device, NRI_STRINGIFY(nri::RayTracingInterface), interfaceSize, interfacePtr);
    else if (hash == Hash(NRI_STRINGIFY(nri_MeshShaderInterface)))
        return (uint8_t)nri::GetInterface(*(const nri::Device*)device, NRI_STRINGIFY(nri::MeshShaderInterface), interfaceSize, interfacePtr);
    else if (hash == Hash(NRI_STRINGIFY(nri_HelperInterface)))
        return (uint8_t)nri::GetInterface(*(const nri::Device*)device, NRI_STRINGIFY(nri::HelperInterface), interfaceSize, interfacePtr);

    return (uint8_t)nri::GetInterface(*(const nri::Device*)device, interfaceName, interfaceSize, interfacePtr);
}

NRIC_API uint8_t NRI_CALL nri_GetPhysicalDevices(void* physicalDeviceGroups, uint32_t* physicalDeviceGroupNum)
{
    return (uint8_t)nri::GetPhysicalDevices((nri::PhysicalDeviceGroup*)physicalDeviceGroups, *physicalDeviceGroupNum);
}

NRIC_API uint8_t NRI_CALL nri_CreateDevice(const void* deviceCreationDesc, void** device)
{
    return (uint8_t)nri::CreateDevice(*(const nri::DeviceCreationDesc*)deviceCreationDesc, *(nri::Device**)device);
}

NRIC_API void NRI_CALL nri_DestroyDevice(void* device)
{
    return nri::DestroyDevice(*(nri::Device*)device);
}

NRIC_API uint8_t NRI_CALL nri_CreateDeviceFromD3D11Device(const void* deviceDesc, void** device)
{
    return (uint8_t)nri::CreateDeviceFromD3D11Device(*(const nri::DeviceCreationD3D11Desc*)deviceDesc, *(nri::Device**)device);
}

NRIC_API uint8_t NRI_CALL nri_ConvertDXGIFormatToNRI(uint32_t dxgiFormat)
{
    return (uint8_t)nri::ConvertDXGIFormatToNRI(dxgiFormat);
}

NRIC_API uint32_t NRI_CALL nri_ConvertNRIFormatToDXGI(uint8_t format)
{
    return nri::ConvertNRIFormatToDXGI((Format)format);
}

NRIC_API uint8_t NRI_CALL nri_CreateDeviceFromD3D12Device(const void* deviceDesc, void** device)
{
    return (uint8_t)nri::CreateDeviceFromD3D12Device(*(const nri::DeviceCreationD3D12Desc*)deviceDesc, *(nri::Device**)device);
}

NRIC_API uint8_t NRI_CALL nri_CreateDeviceFromVkDevice(const void* deviceDesc, void** device)
{
    return (uint8_t)nri::CreateDeviceFromVkDevice(*(const nri::DeviceCreationVulkanDesc*)deviceDesc, *(nri::Device**)device);
}

NRIC_API uint8_t NRI_CALL nri_ConvertVKFormatToNRI(uint32_t vkFormat)
{
    return (uint8_t)nri::ConvertVKFormatToNRI(vkFormat);
}

NRIC_API uint32_t NRI_CALL nri_ConvertNRIFormatToVK(uint8_t format)
{
    return nri::ConvertNRIFormatToVK((Format)format);
}

#ifdef _WIN32

#include <dxgi.h>

int SortAdaptersByDedicatedVideoMemorySize(const void* a, const void* b)
{
    DXGI_ADAPTER_DESC1 ad, bd;
    (*(IDXGIAdapter1**)a)->GetDesc1(&ad);
    (*(IDXGIAdapter1**)b)->GetDesc1(&bd);

    if (ad.DedicatedVideoMemory > bd.DedicatedVideoMemory)
        return -1;

    if (ad.DedicatedVideoMemory < bd.DedicatedVideoMemory)
        return 1;

    return 0;
}

NRI_API Result NRI_CALL nri::GetPhysicalDevices(PhysicalDeviceGroup* physicalDeviceGroups, uint32_t& physicalDeviceGroupNum)
{
    IDXGIFactory1* factory = NULL;
    if (FAILED(CreateDXGIFactory1(__uuidof(IDXGIFactory1), (void**)&factory)))
        return Result::UNSUPPORTED;

    uint32_t adaptersNum = 0;
    IDXGIAdapter1* adapters[32];

    for (uint32_t i = 0; ; i++)
    {
        IDXGIAdapter1* adapter;
        HRESULT hr = factory->EnumAdapters1(i, &adapter);
        if (hr == DXGI_ERROR_NOT_FOUND)
            break;

        DXGI_ADAPTER_DESC1 desc;
        if (adapter->GetDesc1(&desc) == S_OK)
        {
            if (desc.Flags == DXGI_ADAPTER_FLAG_NONE)
                adapters[adaptersNum++] = adapter;
        }
    }

    factory->Release();

    if (!adaptersNum)
        return Result::FAILURE;

    if (physicalDeviceGroups)
    {
        qsort(adapters, adaptersNum, sizeof(adapters[0]), SortAdaptersByDedicatedVideoMemorySize);

        if (adaptersNum < physicalDeviceGroupNum)
            physicalDeviceGroupNum = adaptersNum;

        for (uint32_t i = 0; i < physicalDeviceGroupNum; i++)
        {
            DXGI_ADAPTER_DESC1 desc;
            adapters[i]->GetDesc1(&desc);

            PhysicalDeviceGroup& group = physicalDeviceGroups[i];
            memset(&group, 0, sizeof(group));
            wcstombs(group.description, desc.Description, GetCountOf(group.description) - 1);
            group.luid = *(uint64_t*)&desc.AdapterLuid;
            group.dedicatedVideoMemory = desc.DedicatedVideoMemory;
            group.deviceID = desc.DeviceId;
            group.vendor = GetVendorFromID(desc.VendorId);
        }
    }
    else
        physicalDeviceGroupNum = adaptersNum;

    return Result::SUCCESS;
}

#else

#include <vulkan/vulkan.h>

#define GET_VK_FUNCTION(instance, name) \
    const auto name = (PFN_##name)vkGetInstanceProcAddr(instance, #name); \
    if (name == nullptr) \
        return Result::UNSUPPORTED;

int SortAdaptersByDedicatedVideoMemorySize(const void* pa, const void* pb)
{
    PhysicalDeviceGroup* a = (PhysicalDeviceGroup*)pa;
    PhysicalDeviceGroup* b = (PhysicalDeviceGroup*)pb;

    if (a->dedicatedVideoMemory > b->dedicatedVideoMemory)
        return -1;

    if (a->dedicatedVideoMemory < b->dedicatedVideoMemory)
        return 1;

    return 0;
}

NRI_API Result NRI_CALL nri::GetPhysicalDevices(PhysicalDeviceGroup* physicalDeviceGroups, uint32_t& physicalDeviceGroupNum)
{
    Library* loader = LoadSharedLibrary(VULKAN_LOADER_NAME);
    if (!loader)
        return Result::UNSUPPORTED;

    const auto vkGetInstanceProcAddr = (PFN_vkGetInstanceProcAddr)GetSharedLibraryFunction(*loader, "vkGetInstanceProcAddr");
    if (!vkGetInstanceProcAddr)
        return Result::UNSUPPORTED;

    const auto vkCreateInstance = (PFN_vkCreateInstance)vkGetInstanceProcAddr(VK_NULL_HANDLE, "vkCreateInstance");
    if (!vkCreateInstance)
        return Result::UNSUPPORTED;

    // Create instance
    VkApplicationInfo applicationInfo = {};
    applicationInfo.apiVersion = VK_API_VERSION_1_3;

    VkInstanceCreateInfo instanceCreateInfo = {};
    instanceCreateInfo.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    instanceCreateInfo.pApplicationInfo = &applicationInfo;

    VkInstance instance = VK_NULL_HANDLE;
    VkResult result = vkCreateInstance(&instanceCreateInfo, nullptr, &instance);

    nri::Result nriResult = nri::Result::FAILURE;
    if (result == VK_SUCCESS)
    {
        // Get needed functions
        GET_VK_FUNCTION(instance, vkDestroyInstance);
        GET_VK_FUNCTION(instance, vkEnumeratePhysicalDeviceGroups);
        GET_VK_FUNCTION(instance, vkGetPhysicalDeviceProperties2);
        GET_VK_FUNCTION(instance, vkGetPhysicalDeviceMemoryProperties);

        uint32_t deviceGroupNum = 0;
        result = vkEnumeratePhysicalDeviceGroups(instance, &deviceGroupNum, nullptr);

        if (result == VK_SUCCESS && deviceGroupNum)
        {
            if (physicalDeviceGroups)
            {
                // Query device groups
                VkPhysicalDeviceGroupProperties* deviceGroupProperties = STACK_ALLOC(VkPhysicalDeviceGroupProperties, deviceGroupNum);
                vkEnumeratePhysicalDeviceGroups(instance, &deviceGroupNum, deviceGroupProperties);

                // Query device groups properties
                PhysicalDeviceGroup* physicalDeviceGroupsSorted = STACK_ALLOC(PhysicalDeviceGroup, deviceGroupNum);
                for (uint32_t i = 0; i < deviceGroupNum; i++)
                {
                    VkPhysicalDeviceIDProperties deviceIDProperties = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES };
                    VkPhysicalDeviceProperties2 properties2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 };
                    properties2.pNext = &deviceIDProperties;

                    VkPhysicalDevice physicalDevice = deviceGroupProperties[i].physicalDevices[0];
                    vkGetPhysicalDeviceProperties2(physicalDevice, &properties2);

                    VkPhysicalDeviceMemoryProperties memoryProperties = {};
                    vkGetPhysicalDeviceMemoryProperties(physicalDevice, &memoryProperties);

                    PhysicalDeviceGroup& group = physicalDeviceGroupsSorted[i];
                    memset(&group, 0, sizeof(group));
                    strncpy(group.description, properties2.properties.deviceName, sizeof(group.description));
                    group.luid = *(uint64_t*)&deviceIDProperties.deviceLUID[0];
                    group.deviceID = properties2.properties.deviceID;
                    group.vendor = GetVendorFromID(properties2.properties.vendorID);

                    if (properties2.properties.deviceType != VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
                    {
                        /*
                        https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VkPhysicalDeviceMemoryProperties.html
                        In a unified memory architecture (UMA) system there is often only a single memory heap which is considered to
                        be equally "local" to the host and to the device, and such an implementation must advertise the heap as device-local.
                        */

                        // Awful spec leads to awful solutions :)
                        group.dedicatedVideoMemory = 0;
                    }
                    else
                    {
                        for (uint32_t k = 0; k < memoryProperties.memoryHeapCount; k++)
                        {
                            if (memoryProperties.memoryHeaps[k].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT)
                                group.dedicatedVideoMemory += memoryProperties.memoryHeaps[k].size;
                        }
                    }
                }

                // Sort by dedicated video memory
                qsort(physicalDeviceGroupsSorted, deviceGroupNum, sizeof(physicalDeviceGroupsSorted[0]), SortAdaptersByDedicatedVideoMemorySize);

                // Copy to output
                if (deviceGroupNum < physicalDeviceGroupNum)
                    physicalDeviceGroupNum = deviceGroupNum;

                for (uint32_t i = 0; i < physicalDeviceGroupNum; i++)
                    *physicalDeviceGroups++ = *physicalDeviceGroupsSorted++;
            }
            else
                physicalDeviceGroupNum = deviceGroupNum;

            nriResult = nri::Result::SUCCESS;
        }

        if (instance)
            vkDestroyInstance(instance, nullptr);
    }

    UnloadSharedLibrary(*loader);

    return nriResult;
}

#endif
