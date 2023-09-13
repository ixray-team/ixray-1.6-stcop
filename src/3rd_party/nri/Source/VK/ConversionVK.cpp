/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedVK.h"
#include "BufferVK.h"

using namespace nri;

void nri::ConvertGeometryObjectSizesVK(uint32_t physicalDeviceIndex, VkAccelerationStructureGeometryKHR* destObjects, uint32_t* primitiveNums,
    const GeometryObject* sourceObjects, uint32_t objectNum)
{
    for (uint32_t i = 0; i < objectNum; i++)
    {
        const GeometryObject& geometrySrc = sourceObjects[i];

        uint32_t triangleNum = geometrySrc.triangles.indexNum / 3;
        triangleNum += (geometrySrc.triangles.indexNum == 0) ? geometrySrc.triangles.vertexNum / 3 : 0;

        const uint32_t boxNum = geometrySrc.boxes.boxNum;
        primitiveNums[i] = geometrySrc.type == GeometryType::TRIANGLES ? triangleNum : boxNum;

        const VkDeviceAddress transform = GetBufferDeviceAddress(geometrySrc.triangles.transformBuffer, physicalDeviceIndex) +
            geometrySrc.triangles.transformOffset;

        VkAccelerationStructureGeometryKHR& geometryDst = destObjects[i];
        geometryDst = {};
        geometryDst.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR;
        geometryDst.flags = GetGeometryFlags(geometrySrc.flags);
        geometryDst.geometryType = GetGeometryType(geometrySrc.type);
        geometryDst.geometry.instances.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR;
        geometryDst.geometry.aabbs.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR;
        geometryDst.geometry.triangles.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR;
        geometryDst.geometry.triangles.maxVertex = geometrySrc.triangles.vertexNum;
        geometryDst.geometry.triangles.indexType = GetIndexType(geometrySrc.triangles.indexType);
        geometryDst.geometry.triangles.vertexFormat = GetVkFormat(geometrySrc.triangles.vertexFormat);
        geometryDst.geometry.triangles.transformData.deviceAddress = transform;
    }
}

void nri::ConvertGeometryObjectsVK(uint32_t physicalDeviceIndex, VkAccelerationStructureGeometryKHR* destObjects,
    VkAccelerationStructureBuildRangeInfoKHR* ranges, const GeometryObject* sourceObjects, uint32_t objectNum)
{
    for (uint32_t i = 0; i < objectNum; i++)
    {
        const GeometryObject& geometrySrc = sourceObjects[i];

        uint32_t triangleNum = geometrySrc.triangles.indexNum / 3;
        triangleNum += (geometrySrc.triangles.indexNum == 0) ? geometrySrc.triangles.vertexNum / 3 : 0;

        const uint32_t boxNum = geometrySrc.boxes.boxNum;

        ranges[i] = {};
        ranges[i].primitiveCount = geometrySrc.type == GeometryType::TRIANGLES ? triangleNum : boxNum;

        const VkDeviceAddress aabbs = GetBufferDeviceAddress(geometrySrc.boxes.buffer, physicalDeviceIndex) +
            geometrySrc.boxes.offset;

        const VkDeviceAddress vertices = GetBufferDeviceAddress(geometrySrc.triangles.vertexBuffer, physicalDeviceIndex) +
            geometrySrc.triangles.vertexOffset;

        const VkDeviceAddress indices = GetBufferDeviceAddress(geometrySrc.triangles.indexBuffer, physicalDeviceIndex) +
            geometrySrc.triangles.indexOffset;

        const VkDeviceAddress transform = GetBufferDeviceAddress(geometrySrc.triangles.transformBuffer, physicalDeviceIndex) +
            geometrySrc.triangles.transformOffset;

        VkAccelerationStructureGeometryKHR& geometryDst = destObjects[i];
        geometryDst = {};
        geometryDst.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR;
        geometryDst.flags = GetGeometryFlags(geometrySrc.flags);
        geometryDst.geometryType = GetGeometryType(geometrySrc.type);
        geometryDst.geometry.instances.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR;
        geometryDst.geometry.aabbs.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR;
        geometryDst.geometry.aabbs.data.deviceAddress = aabbs;
        geometryDst.geometry.aabbs.stride = geometrySrc.boxes.stride;
        geometryDst.geometry.triangles.sType = VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR;
        geometryDst.geometry.triangles.maxVertex = geometrySrc.triangles.vertexNum;
        geometryDst.geometry.triangles.vertexData.deviceAddress = vertices;
        geometryDst.geometry.triangles.vertexStride = geometrySrc.triangles.vertexStride;
        geometryDst.geometry.triangles.vertexFormat = GetVkFormat(geometrySrc.triangles.vertexFormat);
        geometryDst.geometry.triangles.indexData.deviceAddress = indices;
        geometryDst.geometry.triangles.indexType = GetIndexType(geometrySrc.triangles.indexType);
        geometryDst.geometry.triangles.transformData.deviceAddress = transform;
    }
}

TextureType GetTextureTypeVK(uint32_t vkImageType)
{
    return GetTextureType((VkImageType)vkImageType);
}

QueryType GetQueryTypeVK(uint32_t queryTypeVK)
{
    if (queryTypeVK == VK_QUERY_TYPE_OCCLUSION)
        return QueryType::OCCLUSION;

    if (queryTypeVK == VK_QUERY_TYPE_PIPELINE_STATISTICS)
        return QueryType::PIPELINE_STATISTICS;

    if (queryTypeVK == VK_QUERY_TYPE_TIMESTAMP)
        return QueryType::TIMESTAMP;

    return QueryType::MAX_NUM;
}

constexpr std::array<VkFormat, (uint32_t)Format::MAX_NUM> VK_FORMAT = {
    VK_FORMAT_UNDEFINED,        // UNKNOWN

    VK_FORMAT_R8_UNORM,         // R8_UNORM
    VK_FORMAT_R8_SNORM,         // R8_SNORM
    VK_FORMAT_R8_UINT,          // R8_UINT
    VK_FORMAT_R8_SINT,          // R8_SINT

    VK_FORMAT_R8G8_UNORM,       // RG8_UNORM
    VK_FORMAT_R8G8_SNORM,       // RG8_SNORM
    VK_FORMAT_R8G8_UINT,        // RG8_UINT
    VK_FORMAT_R8G8_SINT,        // RG8_SINT

    VK_FORMAT_B8G8R8A8_UNORM,   // BGRA8_UNORM
    VK_FORMAT_B8G8R8A8_UNORM,   // BGRA8_SRGB

    VK_FORMAT_R8G8B8A8_UNORM,   // RGBA8_UNORM
    VK_FORMAT_R8G8B8A8_SNORM,   // RGBA8_SNORM
    VK_FORMAT_R8G8B8A8_UINT,    // RGBA8_UINT
    VK_FORMAT_R8G8B8A8_SINT,    // RGBA8_SINT
    VK_FORMAT_R8G8B8A8_UNORM,   // RGBA8_SRGB

    VK_FORMAT_R16_UNORM,        // R16_UNORM
    VK_FORMAT_R16_SNORM,        // R16_SNORM
    VK_FORMAT_R16_UINT,         // R16_UINT
    VK_FORMAT_R16_SINT,         // R16_SINT
    VK_FORMAT_R16_SFLOAT,       // R16_SFLOAT

    VK_FORMAT_R16G16_UNORM,     // RG16_UNORM
    VK_FORMAT_R16G16_SNORM,     // RG16_SNORM
    VK_FORMAT_R16G16_UINT,      // RG16_UINT
    VK_FORMAT_R16G16_SINT,      // RG16_SINT
    VK_FORMAT_R16G16_SFLOAT,    // RG16_SFLOAT

    VK_FORMAT_R16G16B16A16_UNORM,   // RGBA16_UNORM
    VK_FORMAT_R16G16B16A16_SNORM,   // RGBA16_SNORM
    VK_FORMAT_R16G16B16A16_UINT,    // RGBA16_UINT
    VK_FORMAT_R16G16B16A16_SINT,    // RGBA16_SINT
    VK_FORMAT_R16G16B16A16_SFLOAT,  // RGBA16_SFLOAT

    VK_FORMAT_R32_UINT,         // R32_UINT
    VK_FORMAT_R32_SINT,         // R32_SINT
    VK_FORMAT_R32_SFLOAT,       // R32_SFLOAT

    VK_FORMAT_R32G32_UINT,      // RG32_UINT
    VK_FORMAT_R32G32_SINT,      // RG32_SINT
    VK_FORMAT_R32G32_SFLOAT,    // RG32_SFLOAT

    VK_FORMAT_R32G32B32_UINT,   // RGB32_UINT
    VK_FORMAT_R32G32B32_SINT,   // RGB32_SINT
    VK_FORMAT_R32G32B32_SFLOAT, // RGB32_SFLOAT

    VK_FORMAT_R32G32B32A32_UINT,    // RGB32_UINT
    VK_FORMAT_R32G32B32A32_SINT,    // RGB32_SINT
    VK_FORMAT_R32G32B32A32_SFLOAT,  // RGB32_SFLOAT

    VK_FORMAT_A2B10G10R10_UNORM_PACK32,     // R10_G10_B10_A2_UNORM
    VK_FORMAT_A2B10G10R10_UINT_PACK32,      // R10_G10_B10_A2_UINT
    VK_FORMAT_B10G11R11_UFLOAT_PACK32,      // R11_G11_B10_UFLOAT
    VK_FORMAT_E5B9G9R9_UFLOAT_PACK32,       // R9_G9_B9_E5_UFLOAT

    VK_FORMAT_BC1_RGBA_UNORM_BLOCK, // BC1_RGBA_UNORM
    VK_FORMAT_BC1_RGBA_SRGB_BLOCK,  // BC1_RGBA_SRGB
    VK_FORMAT_BC2_UNORM_BLOCK,      // BC2_RGBA_UNORM
    VK_FORMAT_BC2_SRGB_BLOCK,       // BC2_RGBA_SRGB
    VK_FORMAT_BC3_UNORM_BLOCK,      // BC3_RGBA_UNORM
    VK_FORMAT_BC3_SRGB_BLOCK,       // BC3_RGBA_SRGB
    VK_FORMAT_BC4_UNORM_BLOCK,      // BC4_R_UNORM
    VK_FORMAT_BC4_SNORM_BLOCK,      // BC4_R_SNORM
    VK_FORMAT_BC5_UNORM_BLOCK,      // BC5_RG_UNORM
    VK_FORMAT_BC5_SNORM_BLOCK,      // BC5_RG_SNORM
    VK_FORMAT_BC6H_UFLOAT_BLOCK,    // BC6H_RGB_UFLOAT
    VK_FORMAT_BC6H_SFLOAT_BLOCK,    // BC6H_RGB_SFLOAT
    VK_FORMAT_BC7_UNORM_BLOCK,      // BC7_RGBA_UNORM
    VK_FORMAT_BC7_SRGB_BLOCK,       // BC7_RGBA_SRGB

    // Depth-specific

    VK_FORMAT_D16_UNORM,            // D16_UNORM
    VK_FORMAT_D24_UNORM_S8_UINT,    // D24_UNORM_S8_UINT
    VK_FORMAT_D32_SFLOAT,           // D32_SFLOAT
    VK_FORMAT_D32_SFLOAT_S8_UINT,   // D32_SFLOAT_S8_UINT_X24_TYPELESS

    VK_FORMAT_X8_D24_UNORM_PACK32,  // D24_UNORM_X8_TYPELESS
    VK_FORMAT_D24_UNORM_S8_UINT,    // X24_TYPLESS_S8_UINT /// TODO: there is no such format in VK
    VK_FORMAT_D32_SFLOAT_S8_UINT,   // X32_TYPLESS_S8_UINT_X24_TYPELESS
    VK_FORMAT_D32_SFLOAT_S8_UINT    // D32_SFLOAT_X8_TYPLESS_X24_TYPELESS
};

uint32_t NRIFormatToVKFormat(Format format)
{
    return (uint32_t)VK_FORMAT[(uint32_t)format];
}
