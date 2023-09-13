/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

#include "NRIMacro.h"

#define NRI_SET_BIT(index) (1 << (index))

NRI_NAMESPACE_BEGIN

NRI_FORWARD_STRUCT(Buffer);
NRI_FORWARD_STRUCT(CommandQueue);
NRI_FORWARD_STRUCT(CommandBuffer);
NRI_FORWARD_STRUCT(CommandAllocator);
NRI_FORWARD_STRUCT(Device);
NRI_FORWARD_STRUCT(Descriptor);
NRI_FORWARD_STRUCT(DescriptorPool);
NRI_FORWARD_STRUCT(DescriptorSet);
NRI_FORWARD_STRUCT(Fence);
NRI_FORWARD_STRUCT(FrameBuffer);
NRI_FORWARD_STRUCT(Memory);
NRI_FORWARD_STRUCT(PipelineLayout);
NRI_FORWARD_STRUCT(Pipeline);
NRI_FORWARD_STRUCT(QueryPool);
NRI_FORWARD_STRUCT(Texture);

static const uint16_t NRI_NAME(REMAINING_ARRAY_LAYERS) = 0;
static const uint16_t NRI_NAME(REMAINING_MIP_LEVELS) = 0;
static const uint16_t NRI_NAME(WHOLE_SIZE) = 0;
static const uint32_t NRI_NAME(WHOLE_DEVICE_GROUP) = 0;
static const bool NRI_NAME(VARIABLE_DESCRIPTOR_NUM) = true;
static const bool NRI_NAME(DESCRIPTOR_ARRAY) = true;

typedef uint32_t NRI_NAME(MemoryType);

NRI_ENUM
(
    Result, RESULT, uint8_t,

    SUCCESS,
    FAILURE,
    INVALID_ARGUMENT,
    OUT_OF_MEMORY,
    UNSUPPORTED,
    DEVICE_LOST,
    SWAPCHAIN_RESIZE,

    MAX_NUM
);

NRI_ENUM
(
    Vendor, VENDOR, uint8_t,

    UNKNOWN,
    NVIDIA,
    AMD,
    INTEL,

    MAX_NUM
);

NRI_ENUM
(
    GraphicsAPI, GRAPHICS_API, uint8_t,

    D3D11,
    D3D12,
    VULKAN,

    MAX_NUM
);

NRI_ENUM
(
    CommandQueueType, COMMAND_QUEUE_TYPE, uint8_t,

    GRAPHICS,
    COMPUTE,
    COPY,

    MAX_NUM
);

NRI_ENUM
(
    MemoryLocation, MEMORY_LOCATION, uint8_t,

    DEVICE,
    HOST_UPLOAD,
    HOST_READBACK,

    MAX_NUM
);

NRI_ENUM
(
    TextureType, TEXTURE_TYPE, uint8_t,

    TEXTURE_1D,
    TEXTURE_2D,
    TEXTURE_3D,

    MAX_NUM
);

NRI_ENUM
(
    Texture1DViewType, TEXTURE_1D_VIEW_TYPE, uint8_t,

    SHADER_RESOURCE_1D,
    SHADER_RESOURCE_1D_ARRAY,
    SHADER_RESOURCE_STORAGE_1D,
    SHADER_RESOURCE_STORAGE_1D_ARRAY,
    COLOR_ATTACHMENT,
    DEPTH_STENCIL_ATTACHMENT,

    MAX_NUM
);

NRI_ENUM
(
    Texture2DViewType, TEXTURE_2D_VIEW_TYPE, uint8_t,

    SHADER_RESOURCE_2D,
    SHADER_RESOURCE_2D_ARRAY,
    SHADER_RESOURCE_CUBE,
    SHADER_RESOURCE_CUBE_ARRAY,
    SHADER_RESOURCE_STORAGE_2D,
    SHADER_RESOURCE_STORAGE_2D_ARRAY,
    COLOR_ATTACHMENT,
    DEPTH_STENCIL_ATTACHMENT,

    MAX_NUM
);

NRI_ENUM
(
    Texture3DViewType, TEXTURE_3D_VIEW_TYPE, uint8_t,

    SHADER_RESOURCE_3D,
    SHADER_RESOURCE_STORAGE_3D,
    COLOR_ATTACHMENT,

    MAX_NUM
);

NRI_ENUM
(
    BufferViewType, BUFFER_VIEW_TYPE, uint8_t,

    SHADER_RESOURCE,
    SHADER_RESOURCE_STORAGE,
    CONSTANT,

    MAX_NUM
);

NRI_ENUM
(
    DescriptorType, DESCRIPTOR_TYPE, uint8_t,

    SAMPLER,
    CONSTANT_BUFFER,
    TEXTURE,
    STORAGE_TEXTURE,
    BUFFER,
    STORAGE_BUFFER,
    STRUCTURED_BUFFER,
    STORAGE_STRUCTURED_BUFFER,
    ACCELERATION_STRUCTURE,

    MAX_NUM
);

NRI_ENUM
(
    VertexStreamStepRate, VERTEX_STREAM_STEP_RATE, uint8_t,

    PER_VERTEX,
    PER_INSTANCE,

    MAX_NUM
);

NRI_ENUM
(
    TextureLayout, TEXTURE_LAYOUT, uint8_t,

    GENERAL,
    COLOR_ATTACHMENT,
    DEPTH_STENCIL,
    DEPTH_STENCIL_READONLY,
    DEPTH_READONLY,
    STENCIL_READONLY,
    SHADER_RESOURCE,
    PRESENT,
    UNKNOWN,

    MAX_NUM
);

NRI_ENUM
(
    ShaderStage, SHADER_STAGE, uint8_t,

    ALL,
    VERTEX,
    TESS_CONTROL,
    TESS_EVALUATION,
    GEOMETRY,
    FRAGMENT,
    COMPUTE,
    RAYGEN,
    MISS,
    INTERSECTION,
    CLOSEST_HIT,
    ANY_HIT,
    CALLABLE,
    MESH_CONTROL,
    MESH_EVALUATION,

    MAX_NUM
);

NRI_ENUM
(
    BarrierDependency, BARRIER_DEPENDENCY, uint8_t,

    ALL_STAGES,
    GRAPHICS_STAGE,
    COMPUTE_STAGE,
    COPY_STAGE,
    RAYTRACING_STAGE,

    MAX_NUM
);

NRI_ENUM
(
    Topology, TOPOLOGY, uint8_t,

    POINT_LIST,
    LINE_LIST,
    LINE_STRIP,
    TRIANGLE_LIST,
    TRIANGLE_STRIP,
    LINE_LIST_WITH_ADJACENCY,
    LINE_STRIP_WITH_ADJACENCY,
    TRIANGLE_LIST_WITH_ADJACENCY,
    TRIANGLE_STRIP_WITH_ADJACENCY,
    PATCH_LIST,

    MAX_NUM
);

NRI_ENUM
(
    FillMode, FILL_MODE, uint8_t,

    SOLID,
    WIREFRAME,

    MAX_NUM
);

NRI_ENUM
(
    CullMode, CULL_MODE, uint8_t,

    NONE,
    FRONT,
    BACK,

    MAX_NUM
);

NRI_ENUM
(
    LogicFunc, LOGIC_FUNC, uint8_t,

    NONE,
    CLEAR,
    AND,
    AND_REVERSE,
    COPY,
    AND_INVERTED,
    XOR,
    OR,
    NOR,
    EQUIVALENT,
    INVERT,
    OR_REVERSE,
    COPY_INVERTED,
    OR_INVERTED,
    NAND,
    SET,

    MAX_NUM
);

NRI_ENUM
(
    CompareFunc, COMPARE_FUNC, uint8_t,

    NONE,
    ALWAYS,
    NEVER,
    LESS,
    LESS_EQUAL,
    EQUAL,
    GREATER_EQUAL,
    GREATER,
    NOT_EQUAL,

    MAX_NUM
);

NRI_ENUM
(
    StencilFunc, STENCIL_FUNC, uint8_t,

    KEEP,
    ZERO,
    REPLACE,
    INCREMENT_AND_CLAMP,
    DECREMENT_AND_CLAMP,
    INVERT,
    INCREMENT_AND_WRAP,
    DECREMENT_AND_WRAP,

    MAX_NUM
);

NRI_ENUM
(
    BlendFactor, BLEND_FACTOR, uint8_t,

    ZERO,
    ONE,
    SRC_COLOR,
    ONE_MINUS_SRC_COLOR,
    DST_COLOR,
    ONE_MINUS_DST_COLOR,
    SRC_ALPHA,
    ONE_MINUS_SRC_ALPHA,
    DST_ALPHA,
    ONE_MINUS_DST_ALPHA,
    CONSTANT_COLOR,
    ONE_MINUS_CONSTANT_COLOR,
    CONSTANT_ALPHA,
    ONE_MINUS_CONSTANT_ALPHA,
    SRC_ALPHA_SATURATE,
    SRC1_COLOR,
    ONE_MINUS_SRC1_COLOR,
    SRC1_ALPHA,
    ONE_MINUS_SRC1_ALPHA,

    MAX_NUM
);

NRI_ENUM
(
    BlendFunc, BLEND_FUNC, uint8_t,

    ADD,
    SUBTRACT,
    REVERSE_SUBTRACT,
    MIN,
    MAX,

    MAX_NUM
);

NRI_ENUM
(
    IndexType, INDEX_TYPE, uint8_t,

    UINT16,
    UINT32,

    MAX_NUM
);

NRI_ENUM
(
    QueryType, QUERY_TYPE, uint8_t,

    TIMESTAMP,
    OCCLUSION,
    PIPELINE_STATISTICS,
    ACCELERATION_STRUCTURE_COMPACTED_SIZE,

    MAX_NUM
);

NRI_ENUM
(
    Filter, FILTER, uint8_t,

    NEAREST,
    LINEAR,

    MAX_NUM
);

NRI_ENUM
(
    FilterExt, FILTER_EXT, uint8_t,

    NONE,
    MIN,
    MAX,

    MAX_NUM
);

NRI_ENUM
(
    AddressMode, ADDRESS_MODE, uint8_t,

    REPEAT,
    MIRRORED_REPEAT,
    CLAMP_TO_EDGE,
    CLAMP_TO_BORDER,

    MAX_NUM
);

NRI_ENUM
(
    BorderColor, BORDER_COLOR, uint8_t,

    FLOAT_TRANSPARENT_BLACK,
    FLOAT_OPAQUE_BLACK,
    FLOAT_OPAQUE_WHITE,
    INT_TRANSPARENT_BLACK,
    INT_OPAQUE_BLACK,
    INT_OPAQUE_WHITE,

    MAX_NUM
);

NRI_ENUM
(
    Format, FORMAT, uint8_t,

    UNKNOWN,

    R8_UNORM,
    R8_SNORM,
    R8_UINT,
    R8_SINT,

    RG8_UNORM,
    RG8_SNORM,
    RG8_UINT,
    RG8_SINT,

    BGRA8_UNORM,
    BGRA8_SRGB,

    RGBA8_UNORM,
    RGBA8_SNORM,
    RGBA8_UINT,
    RGBA8_SINT,
    RGBA8_SRGB,

    R16_UNORM,
    R16_SNORM,
    R16_UINT,
    R16_SINT,
    R16_SFLOAT,

    RG16_UNORM,
    RG16_SNORM,
    RG16_UINT,
    RG16_SINT,
    RG16_SFLOAT,

    RGBA16_UNORM,
    RGBA16_SNORM,
    RGBA16_UINT,
    RGBA16_SINT,
    RGBA16_SFLOAT,

    R32_UINT,
    R32_SINT,
    R32_SFLOAT,

    RG32_UINT,
    RG32_SINT,
    RG32_SFLOAT,

    RGB32_UINT,
    RGB32_SINT,
    RGB32_SFLOAT,

    RGBA32_UINT,
    RGBA32_SINT,
    RGBA32_SFLOAT,

    R10_G10_B10_A2_UNORM,
    R10_G10_B10_A2_UINT,
    R11_G11_B10_UFLOAT,
    R9_G9_B9_E5_UFLOAT,

    BC1_RGBA_UNORM,
    BC1_RGBA_SRGB,
    BC2_RGBA_UNORM,
    BC2_RGBA_SRGB,
    BC3_RGBA_UNORM,
    BC3_RGBA_SRGB,
    BC4_R_UNORM,
    BC4_R_SNORM,
    BC5_RG_UNORM,
    BC5_RG_SNORM,
    BC6H_RGB_UFLOAT,
    BC6H_RGB_SFLOAT,
    BC7_RGBA_UNORM,
    BC7_RGBA_SRGB,

    D16_UNORM,
    D24_UNORM_S8_UINT,
    D32_SFLOAT,
    D32_SFLOAT_S8_UINT_X24,

    R24_UNORM_X8,
    X24_R8_UINT,
    X32_R8_UINT_X24,
    R32_SFLOAT_X8_X24,

    MAX_NUM
);

NRI_ENUM
(
    AttachmentContentType, ATTACHMENT_CONTENT_TYPE, uint8_t,

    COLOR,
    DEPTH,
    STENCIL,
    DEPTH_STENCIL,

    MAX_NUM
);

NRI_ENUM
(
    RenderPassBeginFlag, RENDER_PASS_BEGIN_FLAG, uint8_t,

    NONE,
    SKIP_FRAME_BUFFER_CLEAR,

    MAX_NUM
);

NRI_ENUM
(
    PrimitiveRestart, PRIMITIVE_RESTART, uint8_t,

    DISABLED,
    INDICES_UINT16,
    INDICES_UINT32,

    MAX_NUM
);

NRI_ENUM_BITS
(
    TextureUsageBits, TEXTURE_USAGE_BITS, uint16_t,

    NONE                                = 0,
    SHADER_RESOURCE                     = NRI_SET_BIT(0),
    SHADER_RESOURCE_STORAGE             = NRI_SET_BIT(1),
    COLOR_ATTACHMENT                    = NRI_SET_BIT(2),
    DEPTH_STENCIL_ATTACHMENT            = NRI_SET_BIT(3)
);

NRI_ENUM_BITS
(
    BufferUsageBits, BUFFER_USAGE_BITS, uint16_t,

    NONE                                = 0,
    SHADER_RESOURCE                     = NRI_SET_BIT(0),
    SHADER_RESOURCE_STORAGE             = NRI_SET_BIT(1),
    VERTEX_BUFFER                       = NRI_SET_BIT(2),
    INDEX_BUFFER                        = NRI_SET_BIT(3),
    CONSTANT_BUFFER                     = NRI_SET_BIT(4),
    ARGUMENT_BUFFER                     = NRI_SET_BIT(5),
    RAY_TRACING_BUFFER                  = NRI_SET_BIT(6),
    ACCELERATION_STRUCTURE_BUILD_READ   = NRI_SET_BIT(7)
);

NRI_ENUM_BITS
(
    AccessBits, ACCESS_BITS, uint16_t,

    UNKNOWN                             = 0,
    VERTEX_BUFFER                       = NRI_SET_BIT(0),
    INDEX_BUFFER                        = NRI_SET_BIT(1),
    CONSTANT_BUFFER                     = NRI_SET_BIT(2),
    ARGUMENT_BUFFER                     = NRI_SET_BIT(3),
    SHADER_RESOURCE                     = NRI_SET_BIT(4),
    SHADER_RESOURCE_STORAGE             = NRI_SET_BIT(5),
    COLOR_ATTACHMENT                    = NRI_SET_BIT(6),
    DEPTH_STENCIL_WRITE                 = NRI_SET_BIT(7),
    DEPTH_STENCIL_READ                  = NRI_SET_BIT(8),
    COPY_SOURCE                         = NRI_SET_BIT(9),
    COPY_DESTINATION                    = NRI_SET_BIT(10),
    ACCELERATION_STRUCTURE_READ         = NRI_SET_BIT(11),
    ACCELERATION_STRUCTURE_WRITE        = NRI_SET_BIT(12)
);

// Starts with 1 to unblock "1 << ShaderStage"
NRI_ENUM_BITS
(
    PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, uint16_t,

    NONE                                = 0,
    VERTEX                              = NRI_SET_BIT(1),
    TESS_CONTROL                        = NRI_SET_BIT(2),
    TESS_EVALUATION                     = NRI_SET_BIT(3),
    GEOMETRY                            = NRI_SET_BIT(4),
    FRAGMENT                            = NRI_SET_BIT(5),
    COMPUTE                             = NRI_SET_BIT(6),
    RAYGEN                              = NRI_SET_BIT(7),
    MISS                                = NRI_SET_BIT(8),
    INTERSECTION                        = NRI_SET_BIT(9),
    CLOSEST_HIT                         = NRI_SET_BIT(10),
    ANY_HIT                             = NRI_SET_BIT(11),
    CALLABLE                            = NRI_SET_BIT(12),
    MESH_CONTROL                        = NRI_SET_BIT(13),
    MESH_EVALUATION                     = NRI_SET_BIT(14),

    ALL_GRAPHICS                        = NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, VERTEX) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, TESS_CONTROL) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, TESS_EVALUATION) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, GEOMETRY) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, FRAGMENT) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, MESH_CONTROL) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, MESH_EVALUATION),

    ALL_RAY_TRACING                     = NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, RAYGEN) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, MISS) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, INTERSECTION) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, CLOSEST_HIT) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, ANY_HIT) |
                                          NRI_ENUM_MEMBER(PipelineLayoutShaderStageBits, PIPELINE_LAYOUT_SHADER_STAGE_BITS, CALLABLE)
);

NRI_ENUM_BITS
(
    PipelineStatsBits, PIPELINE_STATS_BITS, uint16_t,

    INPUT_ASSEMBLY_VERTICES             = NRI_SET_BIT(0),
    INPUT_ASSEMBLY_PRIMITIVES           = NRI_SET_BIT(1),
    VERTEX_SHADER_INVOCATIONS           = NRI_SET_BIT(2),
    GEOMETRY_SHADER_INVOCATIONS         = NRI_SET_BIT(3),
    GEOMETRY_SHADER_PRIMITIVES          = NRI_SET_BIT(4),
    CLIPPING_INVOCATIONS                = NRI_SET_BIT(5),
    CLIPPING_PRIMITIVES                 = NRI_SET_BIT(6),
    FRAGMENT_SHADER_INVOCATIONS         = NRI_SET_BIT(7),
    TESS_CONTROL_SHADER_PATCHES         = NRI_SET_BIT(8),
    TESS_EVALUATION_SHADER_INVOCATIONS  = NRI_SET_BIT(9),
    COMPUTE_SHADER_INVOCATIONS          = NRI_SET_BIT(10)
);

NRI_ENUM_BITS
(
    ColorWriteBits, COLOR_WRITE_BITS, uint8_t,

    R                                   = NRI_SET_BIT(0),
    G                                   = NRI_SET_BIT(1),
    B                                   = NRI_SET_BIT(2),
    A                                   = NRI_SET_BIT(3),

    RGBA                                = NRI_ENUM_MEMBER(ColorWriteBits, COLOR_WRITE_BITS, R) |
                                          NRI_ENUM_MEMBER(ColorWriteBits, COLOR_WRITE_BITS, G) |
                                          NRI_ENUM_MEMBER(ColorWriteBits, COLOR_WRITE_BITS, B) |
                                          NRI_ENUM_MEMBER(ColorWriteBits, COLOR_WRITE_BITS, A)
);

NRI_ENUM_BITS
(
    ResourceViewBits, RESOURCE_VIEW_BITS, uint8_t,

    READONLY_DEPTH                      = NRI_SET_BIT(0),
    READONLY_STENCIL                    = NRI_SET_BIT(1)
);

NRI_ENUM_BITS
(
    FormatSupportBits, FORMAT_SUPPORT_BITS, uint8_t,

    UNSUPPORTED                         = 0,
    TEXTURE                             = NRI_SET_BIT(0),
    STORAGE_TEXTURE                     = NRI_SET_BIT(1),
    BUFFER                              = NRI_SET_BIT(2),
    STORAGE_BUFFER                      = NRI_SET_BIT(3),
    COLOR_ATTACHMENT                    = NRI_SET_BIT(4),
    DEPTH_STENCIL_ATTACHMENT            = NRI_SET_BIT(5),
    VERTEX_BUFFER                       = NRI_SET_BIT(6)
);

NRI_ENUM_BITS
(
    DescriptorSetBindingBits, DESCRIPTOR_SET_BINDING_BITS, uint8_t,

    DEFAULT                             = 0,
    PARTIALLY_BOUND                     = NRI_SET_BIT(0)
);

NRI_STRUCT(Rect)
{
    int32_t left;
    int32_t top;
    uint32_t width;
    uint32_t height;
};

NRI_STRUCT(Viewport)
{
    float offset[2];
    float size[2];
    float depthRangeMin;
    float depthRangeMax;
};

NRI_STRUCT(Color32f)
{
    float x, y, z, w;
};

NRI_STRUCT(Color32ui)
{
    uint32_t x, y, z, w;
};

NRI_STRUCT(Color32i)
{
    int32_t x, y, z, w;
};

NRI_STRUCT(DepthStencilClearValue)
{
    float depth;
    uint8_t stencil;
};

NRI_UNION(ClearValueDesc)
{
    NRI_NAME(DepthStencilClearValue) depthStencil;
    NRI_NAME(Color32f) color32f;
    NRI_NAME(Color32ui) color32ui;
    NRI_NAME(Color32i) color32i;
};

NRI_STRUCT(ClearDesc)
{
    NRI_NAME(ClearValueDesc) value;
    NRI_NAME(AttachmentContentType) attachmentContentType;
    uint32_t colorAttachmentIndex;
};

NRI_STRUCT(ClearStorageBufferDesc)
{
    const NRI_NAME(Descriptor)* storageBuffer;
    uint32_t value;
    uint32_t setIndexInPipelineLayout;
    uint32_t rangeIndex;
    uint32_t offsetInRange;
};

NRI_STRUCT(ClearStorageTextureDesc)
{
    const NRI_NAME(Descriptor)* storageTexture;
    NRI_NAME(ClearValueDesc) value;
    uint32_t setIndexInPipelineLayout;
    uint32_t rangeIndex;
    uint32_t offsetInRange;
};

NRI_STRUCT(TextureRegionDesc)
{
    uint16_t offset[3];
    uint16_t size[3];
    uint16_t mipOffset;
    uint16_t arrayOffset;
};

NRI_STRUCT(TextureDataLayoutDesc)
{
    uint64_t offset;
    uint32_t rowPitch;
    uint32_t slicePitch;
};

NRI_STRUCT(QueueSubmitDesc)
{
    const NRI_NAME(CommandBuffer)* const* commandBuffers;
    uint32_t commandBufferNum;
    uint32_t physicalDeviceIndex;
};

NRI_STRUCT(BufferMemoryBindingDesc)
{
    NRI_NAME(Memory)* memory;
    NRI_NAME(Buffer)* buffer;
    uint64_t offset;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(TextureMemoryBindingDesc)
{
    NRI_NAME(Memory)* memory;
    NRI_NAME(Texture)* texture;
    uint64_t offset;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(MemoryDesc)
{
    uint64_t size;
    uint32_t alignment;
    NRI_NAME(MemoryType) type;
    bool mustBeDedicated;
};

NRI_STRUCT(AddressModes)
{
    NRI_NAME(AddressMode) u;
    NRI_NAME(AddressMode) v;
    NRI_NAME(AddressMode) w;
};

NRI_STRUCT(SamplerDesc)
{
    NRI_NAME(Filter) magnification;
    NRI_NAME(Filter) minification;
    NRI_NAME(Filter) mip;
    NRI_NAME(FilterExt) filterExt;
    uint32_t anisotropy;
    float mipBias;
    float mipMin;
    float mipMax;
    NRI_NAME(AddressModes) addressModes;
    NRI_NAME(CompareFunc) compareFunc;
    NRI_NAME(BorderColor) borderColor;
    bool unnormalizedCoordinates;
};

NRI_STRUCT(TextureDesc)
{
    NRI_NAME(TextureType) type;
    NRI_NAME(TextureUsageBits) usageMask;
    NRI_NAME(Format) format;
    uint16_t size[3];
    uint16_t mipNum;
    uint16_t arraySize;
    uint8_t sampleNum;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(BufferDesc)
{
    uint64_t size;
    uint32_t structureStride;
    NRI_NAME(BufferUsageBits) usageMask;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(Texture1DViewDesc)
{
    const NRI_NAME(Texture)* texture;
    NRI_NAME(Texture1DViewType) viewType;
    NRI_NAME(Format) format;
    uint16_t mipOffset;
    uint16_t mipNum;
    uint16_t arrayOffset;
    uint16_t arraySize;
    uint32_t physicalDeviceMask;
    NRI_NAME(ResourceViewBits) flags;
};

NRI_STRUCT(Texture2DViewDesc)
{
    const NRI_NAME(Texture)* texture;
    NRI_NAME(Texture2DViewType) viewType;
    NRI_NAME(Format) format;
    uint16_t mipOffset;
    uint16_t mipNum;
    uint16_t arrayOffset;
    uint16_t arraySize;
    uint32_t physicalDeviceMask;
    NRI_NAME(ResourceViewBits) flags;
};

NRI_STRUCT(Texture3DViewDesc)
{
    const NRI_NAME(Texture)* texture;
    NRI_NAME(Texture3DViewType) viewType;
    NRI_NAME(Format) format;
    uint16_t mipOffset;
    uint16_t mipNum;
    uint16_t sliceOffset;
    uint16_t sliceNum;
    uint32_t physicalDeviceMask;
    NRI_NAME(ResourceViewBits) flags;
};

NRI_STRUCT(BufferViewDesc)
{
    const NRI_NAME(Buffer)* buffer;
    NRI_NAME(BufferViewType) viewType;
    NRI_NAME(Format) format;
    uint64_t offset;
    uint64_t size;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(DescriptorPoolDesc)
{
    uint32_t physicalDeviceMask;
    uint32_t descriptorSetMaxNum;
    uint32_t samplerMaxNum;
    uint32_t constantBufferMaxNum;
    uint32_t dynamicConstantBufferMaxNum;
    uint32_t textureMaxNum;
    uint32_t storageTextureMaxNum;
    uint32_t bufferMaxNum;
    uint32_t storageBufferMaxNum;
    uint32_t structuredBufferMaxNum;
    uint32_t storageStructuredBufferMaxNum;
    uint32_t accelerationStructureMaxNum;
};

NRI_STRUCT(TextureTransitionBarrierDesc)
{
    const NRI_NAME(Texture)* texture;
    uint16_t mipOffset;
    uint16_t mipNum;
    uint16_t arrayOffset;
    uint16_t arraySize;
    NRI_NAME(AccessBits) prevAccess;
    NRI_NAME(AccessBits) nextAccess;
    NRI_NAME(TextureLayout) prevLayout;
    NRI_NAME(TextureLayout) nextLayout;
};

NRI_STRUCT(BufferTransitionBarrierDesc)
{
    const NRI_NAME(Buffer)* buffer;
    NRI_NAME(AccessBits) prevAccess;
    NRI_NAME(AccessBits) nextAccess;
};

NRI_STRUCT(BufferAliasingBarrierDesc)
{
    const NRI_NAME(Buffer)* before;
    const NRI_NAME(Buffer)* after;
    NRI_NAME(AccessBits) nextAccess;
};

NRI_STRUCT(TextureAliasingBarrierDesc)
{
    const NRI_NAME(Texture)* before;
    const NRI_NAME(Texture)* after;
    NRI_NAME(AccessBits) nextAccess;
    NRI_NAME(TextureLayout) nextLayout;
};

NRI_STRUCT(TransitionBarrierDesc)
{
    const NRI_NAME(BufferTransitionBarrierDesc)* buffers;
    const NRI_NAME(TextureTransitionBarrierDesc)* textures;
    uint32_t bufferNum;
    uint32_t textureNum;
};

NRI_STRUCT(AliasingBarrierDesc)
{
    const NRI_NAME(BufferAliasingBarrierDesc)* buffers;
    const NRI_NAME(TextureAliasingBarrierDesc)* textures;
    uint32_t bufferNum;
    uint32_t textureNum;
};

NRI_STRUCT(DescriptorRangeDesc)
{
    uint32_t baseRegisterIndex;
    uint32_t descriptorNum;
    NRI_NAME(DescriptorType) descriptorType;
    NRI_NAME(ShaderStage) visibility;
    bool isDescriptorNumVariable;
    bool isArray;
};

NRI_STRUCT(DynamicConstantBufferDesc)
{
    uint32_t registerIndex;
    NRI_NAME(ShaderStage) visibility;
};

NRI_STRUCT(DescriptorSetDesc)
{
    uint32_t registerSpace;
    const NRI_NAME(DescriptorRangeDesc)* ranges;
    uint32_t rangeNum;
    const NRI_NAME(DynamicConstantBufferDesc)* dynamicConstantBuffers;
    uint32_t dynamicConstantBufferNum;
    NRI_NAME(DescriptorSetBindingBits) bindingMask;
};

NRI_STRUCT(DescriptorRangeUpdateDesc)
{
    const NRI_NAME(Descriptor)* const* descriptors;
    uint32_t descriptorNum;
    uint32_t offsetInRange;
};

NRI_STRUCT(DescriptorSetCopyDesc)
{
    const NRI_NAME(DescriptorSet)* srcDescriptorSet;
    uint32_t baseSrcRange;
    uint32_t baseDstRange;
    uint32_t rangeNum;
    uint32_t baseSrcDynamicConstantBuffer;
    uint32_t baseDstDynamicConstantBuffer;
    uint32_t dynamicConstantBufferNum;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(PushConstantDesc)
{
    uint32_t registerIndex;
    uint32_t size;
    NRI_NAME(ShaderStage) visibility;
};

NRI_STRUCT(SPIRVBindingOffsets)
{
    uint32_t samplerOffset;
    uint32_t textureOffset;
    uint32_t constantBufferOffset;
    uint32_t storageTextureAndBufferOffset;
};

NRI_STRUCT(ShaderDesc)
{
    NRI_NAME(ShaderStage) stage;
    const void* bytecode;
    uint64_t size;
    const char* entryPointName;
};

NRI_STRUCT(VertexAttributeD3D)
{
    const char* semanticName;
    uint32_t semanticIndex;
};

NRI_STRUCT(VertexAttributeVK)
{
    uint32_t location;
};

NRI_STRUCT(VertexAttributeDesc)
{
    NRI_NAME(VertexAttributeD3D) d3d;
    NRI_NAME(VertexAttributeVK) vk;
    uint32_t offset;
    NRI_NAME(Format) format;
    uint16_t streamIndex;
};

NRI_STRUCT(VertexStreamDesc)
{
    uint32_t stride;
    uint16_t bindingSlot;
    NRI_NAME(VertexStreamStepRate) stepRate;
};

NRI_STRUCT(InputAssemblyDesc)
{
    const NRI_NAME(VertexAttributeDesc)* attributes;
    const NRI_NAME(VertexStreamDesc)* streams;
    uint8_t attributeNum;
    uint8_t streamNum;
    NRI_NAME(Topology) topology;
    uint8_t tessControlPointNum;
    NRI_NAME(PrimitiveRestart) primitiveRestart;
};

NRI_STRUCT(SamplePosition)
{
    int8_t x;
    int8_t y;
};

NRI_STRUCT(RasterizationDesc)
{
    uint32_t viewportNum;
    int32_t depthBiasConstantFactor;
    float depthBiasClamp;
    float depthBiasSlopeFactor;
    NRI_NAME(FillMode) fillMode;
    NRI_NAME(CullMode) cullMode;
    uint16_t sampleMask;
    uint8_t sampleNum;
    bool alphaToCoverage;
    bool frontCounterClockwise;
    bool depthClamp;
    bool antialiasedLines;
    bool rasterizerDiscard;
    bool conservativeRasterization;
};

NRI_STRUCT(StencilDesc)
{
    NRI_NAME(CompareFunc) compareFunc;
    NRI_NAME(StencilFunc) fail;
    NRI_NAME(StencilFunc) pass;
    NRI_NAME(StencilFunc) depthFail;
};

NRI_STRUCT(BlendingDesc)
{
    NRI_NAME(BlendFactor) srcFactor;
    NRI_NAME(BlendFactor) dstFactor;
    NRI_NAME(BlendFunc) func;
};

NRI_STRUCT(ColorAttachmentDesc)
{
    NRI_NAME(Format) format;
    NRI_NAME(BlendingDesc) colorBlend;
    NRI_NAME(BlendingDesc) alphaBlend;
    NRI_NAME(ColorWriteBits) colorWriteMask;
    bool blendEnabled;
};

// CompareFunc::NONE = depth/stencil test disabled
NRI_STRUCT(DepthAttachmentDesc)
{
    NRI_NAME(CompareFunc) compareFunc;
    bool write;
};

NRI_STRUCT(StencilAttachmentDesc)
{
    NRI_NAME(StencilDesc) front;
    NRI_NAME(StencilDesc) back;
    uint8_t reference;
    uint8_t compareMask;
    uint8_t writeMask;
};

NRI_STRUCT(OutputMergerDesc)
{
    const NRI_NAME(ColorAttachmentDesc)* color;
    NRI_NAME(DepthAttachmentDesc) depth;
    NRI_NAME(StencilAttachmentDesc) stencil;
    NRI_NAME(Format) depthStencilFormat;
    NRI_NAME(LogicFunc) colorLogicFunc;
    uint32_t colorNum;
    NRI_NAME(Color32f) blendConsts;
};

NRI_STRUCT(PipelineLayoutDesc)
{
    const NRI_NAME(DescriptorSetDesc)* descriptorSets;
    const NRI_NAME(PushConstantDesc)* pushConstants;
    uint32_t descriptorSetNum;
    uint32_t pushConstantNum;
    NRI_NAME(PipelineLayoutShaderStageBits) stageMask;
    bool ignoreGlobalSPIRVOffsets;
};

NRI_STRUCT(GraphicsPipelineDesc)
{
    const NRI_NAME(PipelineLayout)* pipelineLayout;
    const NRI_NAME(InputAssemblyDesc)* inputAssembly;
    const NRI_NAME(RasterizationDesc)* rasterization;
    const NRI_NAME(OutputMergerDesc)* outputMerger;
    const NRI_NAME(ShaderDesc)* shaderStages;
    uint32_t shaderStageNum;
};

NRI_STRUCT(ComputePipelineDesc)
{
    const NRI_NAME(PipelineLayout)* pipelineLayout;
    NRI_NAME(ShaderDesc) computeShader;
};

NRI_STRUCT(FrameBufferDesc)
{
    const NRI_NAME(Descriptor)* const* colorAttachments;
    const NRI_NAME(Descriptor)* depthStencilAttachment;
    const NRI_NAME(ClearValueDesc)* colorClearValues;
    const NRI_NAME(ClearValueDesc)* depthStencilClearValue;
    uint32_t colorAttachmentNum;
    uint32_t physicalDeviceMask;
    uint16_t size[2];
    uint16_t layerNum;
};

NRI_STRUCT(QueryPoolDesc)
{
    NRI_NAME(QueryType) queryType;
    uint32_t capacity;
    NRI_NAME(PipelineStatsBits) pipelineStatsMask;
    uint32_t physicalDeviceMask;
};

NRI_STRUCT(PipelineStatisticsDesc)
{
    uint64_t inputVertices;
    uint64_t inputPrimitives;
    uint64_t vertexShaderInvocations;
    uint64_t geometryShaderInvocations;
    uint64_t geometryShaderPrimitives;
    uint64_t rasterizerInPrimitives;
    uint64_t rasterizerOutPrimitives;
    uint64_t fragmentShaderInvocations;
    uint64_t tessControlInvocations;
    uint64_t tessEvaluationInvocations;
    uint64_t computeShaderInvocations;
};

NRI_STRUCT(DeviceDesc)
{
    // Common
    NRI_NAME(GraphicsAPI) graphicsAPI;
    NRI_NAME(Vendor) vendor;
    uint16_t nriVersionMajor;
    uint16_t nriVersionMinor;

    // Viewports
    uint32_t viewportMaxNum;
    uint32_t viewportSubPixelBits;
    int32_t viewportBoundsRange[2];

    // Framebuffer
    uint32_t frameBufferMaxDim;
    uint32_t frameBufferLayerMaxNum;
    uint32_t framebufferColorAttachmentMaxNum;

    // Multi-sampling
    uint8_t frameBufferColorSampleMaxNum;
    uint8_t frameBufferDepthSampleMaxNum;
    uint8_t frameBufferStencilSampleMaxNum;
    uint8_t frameBufferNoAttachmentsSampleMaxNum;
    uint8_t textureColorSampleMaxNum;
    uint8_t textureIntegerSampleMaxNum;
    uint8_t textureDepthSampleMaxNum;
    uint8_t textureStencilSampleMaxNum;
    uint8_t storageTextureSampleMaxNum;

    // Resource dimensions
    uint32_t texture1DMaxDim;
    uint32_t texture2DMaxDim;
    uint32_t texture3DMaxDim;
    uint32_t textureArrayMaxDim;
    uint32_t texelBufferMaxDim;

    // Memory
    uint32_t memoryAllocationMaxNum;
    uint32_t samplerAllocationMaxNum;
    uint32_t uploadBufferTextureRowAlignment;
    uint32_t uploadBufferTextureSliceAlignment;
    uint32_t typedBufferOffsetAlignment;
    uint32_t constantBufferOffsetAlignment;
    uint32_t constantBufferMaxRange;
    uint32_t storageBufferOffsetAlignment;
    uint32_t storageBufferMaxRange;
    uint32_t bufferTextureGranularity;
    uint64_t bufferMaxSize;
    uint32_t pushConstantsMaxSize;

    // Shader resources
    uint32_t boundDescriptorSetMaxNum;
    uint32_t perStageDescriptorSamplerMaxNum;
    uint32_t perStageDescriptorConstantBufferMaxNum;
    uint32_t perStageDescriptorStorageBufferMaxNum;
    uint32_t perStageDescriptorTextureMaxNum;
    uint32_t perStageDescriptorStorageTextureMaxNum;
    uint32_t perStageResourceMaxNum;

    // Descriptor set
    uint32_t descriptorSetSamplerMaxNum;
    uint32_t descriptorSetConstantBufferMaxNum;
    uint32_t descriptorSetStorageBufferMaxNum;
    uint32_t descriptorSetTextureMaxNum;
    uint32_t descriptorSetStorageTextureMaxNum;

    // Vertex shader
    uint32_t vertexShaderAttributeMaxNum;
    uint32_t vertexShaderStreamMaxNum;
    uint32_t vertexShaderOutputComponentMaxNum;

    // Tessellation control shader
    float tessControlShaderGenerationMaxLevel;
    uint32_t tessControlShaderPatchPointMaxNum;
    uint32_t tessControlShaderPerVertexInputComponentMaxNum;
    uint32_t tessControlShaderPerVertexOutputComponentMaxNum;
    uint32_t tessControlShaderPerPatchOutputComponentMaxNum;
    uint32_t tessControlShaderTotalOutputComponentMaxNum;

    // Tessellation evaluation shader
    uint32_t tessEvaluationShaderInputComponentMaxNum;
    uint32_t tessEvaluationShaderOutputComponentMaxNum;

    // Geometry shader
    uint32_t geometryShaderInvocationMaxNum;
    uint32_t geometryShaderInputComponentMaxNum;
    uint32_t geometryShaderOutputComponentMaxNum;
    uint32_t geometryShaderOutputVertexMaxNum;
    uint32_t geometryShaderTotalOutputComponentMaxNum;

    // Fragment shader
    uint32_t fragmentShaderInputComponentMaxNum;
    uint32_t fragmentShaderOutputAttachmentMaxNum;
    uint32_t fragmentShaderDualSourceAttachmentMaxNum;
    uint32_t fragmentShaderCombinedOutputResourceMaxNum;

    // Compute shader
    uint32_t computeShaderSharedMemoryMaxSize;
    uint32_t computeShaderWorkGroupMaxNum[3];
    uint32_t computeShaderWorkGroupInvocationMaxNum;
    uint32_t computeShaderWorkGroupMaxDim[3];

    // Ray tracing
    uint64_t rayTracingShaderGroupIdentifierSize;
    uint64_t rayTracingShaderTableAligment;
    uint64_t rayTracingShaderTableMaxStride;
    uint32_t rayTracingShaderRecursionMaxDepth;
    uint32_t rayTracingGeometryObjectMaxNum;

    // Mesh shader
    uint32_t meshTaskWorkGroupInvocationMaxNum;
    uint32_t meshTaskWorkGroupMaxDim[3];
    uint32_t meshTaskTotalMemoryMaxSize;
    uint32_t meshTaskOutputMaxNum;
    uint32_t meshWorkGroupInvocationMaxNum;
    uint32_t meshWorkGroupMaxDim[3];
    uint32_t meshOutputVertexMaxNum;
    uint32_t meshOutputPrimitiveMaxNum;
    uint32_t meshMultiviewViewMaxNum;
    uint32_t meshOutputPerVertexGranularity;
    uint32_t meshOutputPerPrimitiveGranularity;

    // Other
    uint64_t timestampFrequencyHz;
    uint32_t subPixelPrecisionBits;
    uint32_t subTexelPrecisionBits;
    uint32_t mipmapPrecisionBits;
    uint32_t drawIndexedIndex16ValueMax;
    uint32_t drawIndexedIndex32ValueMax;
    uint32_t drawIndirectMaxNum;
    float samplerLodBiasMin;
    float samplerLodBiasMax;
    float samplerAnisotropyMax;
    int32_t texelOffsetMin;
    uint32_t texelOffsetMax;
    int32_t texelGatherOffsetMin;
    uint32_t texelGatherOffsetMax;
    uint32_t clipDistanceMaxNum;
    uint32_t cullDistanceMaxNum;
    uint32_t combinedClipAndCullDistanceMaxNum;
    uint8_t conservativeRasterTier;
    uint8_t physicalDeviceNum;

    // Features support
    bool isAPIValidationEnabled;
    bool isTextureFilterMinMaxSupported;
    bool isLogicOpSupported;
    bool isDepthBoundsTestSupported;
    bool isProgrammableSampleLocationsSupported;
    bool isComputeQueueSupported;
    bool isCopyQueueSupported;
    bool isCopyQueueTimestampSupported;
    bool isRegisterAliasingSupported;
    bool isSubsetAllocationSupported;
    bool isFloat16Supported;
};

NRI_NAMESPACE_END
