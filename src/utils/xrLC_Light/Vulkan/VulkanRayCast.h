#pragma once
#include "../xrFace.h"
#include "../../xrCore/Collision/xrCDB.h"
#include <vulkan/vulkan.h>

namespace XRay::RayCast::Vulkan
{
    namespace Internal
    {
        struct Vertex
        {
            Fvector P;
            // ...
        };

        struct Face
        {
            Vertex* v[3];
            u32 dwMaterial;
            // ...
        };

        struct FvectorHash
        {
            size_t operator()(const Fvector& v) const noexcept
            {
                size_t h1 = std::hash<float>{}(v.x);
                size_t h2 = std::hash<float>{}(v.y);
                size_t h3 = std::hash<float>{}(v.z);
                return h1 ^ (h2 << 1) ^ (h3 << 2);
            }
        };

        struct FvectorEqual
        {
            bool operator()(const Fvector& a, const Fvector& b) const noexcept
            {
                return a.similar(b, EPS_L);
            }
        };
    }

    struct MeshBuffers
    {
        VkBuffer vertexBuffer = VK_NULL_HANDLE;
        VkDeviceMemory vertexMemory = VK_NULL_HANDLE;
        uint32_t vertexCount = 0;

        VkBuffer indexBuffer = VK_NULL_HANDLE;
        VkDeviceMemory indexMemory = VK_NULL_HANDLE;
        uint32_t indexCount = 0;

        VkBuffer blasBuffer = VK_NULL_HANDLE;
        VkDeviceMemory blasMemory = VK_NULL_HANDLE;
        VkAccelerationStructureKHR blas = VK_NULL_HANDLE;
    };

    // Функции пребилда
    void BuildStaticGeometry(xr_vector<Face*> const& faces, xr_vector<Fvector>& outVerts, xr_vector<CDB::TRI>& outTris);
    void BuildGeometryFromFaces(const xr_vector<Internal::Face*>& faces, xr_vector<Fvector>& outVerts, xr_vector<CDB::TRI>& outTris);
    void BuildBLASFromTris
    (
        VkDevice device, VkPhysicalDevice physicalDevice, VkCommandPool commandPool, VkQueue queue,
        Fvector* verts, u32 vert_count, CDB::TRI* tris, u32 face_count, MeshBuffers& outBuffers
    );

    // Функции постбилда
    void BuildBLAS(VkDevice device, VkPhysicalDevice physicalDevice, VkCommandPool commandPool, VkQueue queue, MeshBuffers& buffers);
    bool UploadDataToGPUBuffer
    (
        VkDevice device, VkPhysicalDevice physicalDevice, VkCommandPool commandPool, VkQueue queue,
        const void* srcData, VkDeviceSize size, VkBufferUsageFlags usage, VkBuffer& outBuffer, VkDeviceMemory& outMemory
    );

    VkDeviceAddress GetBufferDeviceAddress(VkDevice device, VkBuffer buffer);
};

struct VulkanBuffer
{
    VkBuffer buffer = VK_NULL_HANDLE;
    VkDeviceMemory memory = VK_NULL_HANDLE;
    VkDeviceSize size = 0;

    void Destroy(VkDevice device)
    {
        if (buffer) vkDestroyBuffer(device, buffer, nullptr);
        if (memory) vkFreeMemory(device, memory, nullptr);
        buffer = VK_NULL_HANDLE;
        memory = VK_NULL_HANDLE;
    }
};

struct VulkanAccelerationStructure
{
    VkAccelerationStructureKHR handle = VK_NULL_HANDLE;
    VulkanBuffer buffer;

    void Destroy(VkDevice device)
    {
        if (handle) vkDestroyAccelerationStructureKHR(device, handle, nullptr);
        buffer.Destroy(device);
        handle = VK_NULL_HANDLE;
    }
};
