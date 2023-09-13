/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

NRI_NAMESPACE_BEGIN

NRI_STRUCT(TextureSubresourceUploadDesc)
{
    const void* slices;
    uint32_t sliceNum;
    uint32_t rowPitch;
    uint32_t slicePitch;
};

NRI_STRUCT(TextureUploadDesc)
{
    const NRI_NAME(TextureSubresourceUploadDesc)* subresources;
    NRI_NAME(Texture)* texture;
    NRI_NAME(AccessBits) nextAccess;
    NRI_NAME(TextureLayout) nextLayout;
    uint16_t mipNum;
    uint16_t arraySize;
};

NRI_STRUCT(BufferUploadDesc)
{
    const void* data;
    uint64_t dataSize;
    NRI_NAME(Buffer)* buffer;
    uint64_t bufferOffset;
    NRI_NAME(AccessBits) prevAccess;
    NRI_NAME(AccessBits) nextAccess;
};

NRI_STRUCT(ResourceGroupDesc)
{
    NRI_NAME(MemoryLocation) memoryLocation;
    NRI_NAME(Texture)* const* textures;
    uint32_t textureNum;
    NRI_NAME(Buffer)* const* buffers;
    uint32_t bufferNum;
};

NRI_STRUCT(HelperInterface)
{
    uint32_t (NRI_CALL *CalculateAllocationNumber)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(ResourceGroupDesc) resourceGroupDesc);
    NRI_NAME(Result) (NRI_CALL *AllocateAndBindMemory)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(ResourceGroupDesc) resourceGroupDesc, NRI_NAME(Memory)** allocations);
    NRI_NAME(Result) (NRI_CALL *ChangeResourceStates)(NRI_REF_NAME(CommandQueue) commandQueue, const NRI_REF_NAME(TransitionBarrierDesc) transitionBarriers);
    NRI_NAME(Result) (NRI_CALL *UploadData)(NRI_REF_NAME(CommandQueue) commandQueue, const NRI_NAME(TextureUploadDesc)* textureUploadDescs, uint32_t textureUploadDescNum,
        const NRI_NAME(BufferUploadDesc)* bufferUploadDescs, uint32_t bufferUploadDescNum);
    NRI_NAME(Result) (NRI_CALL *WaitForIdle)(NRI_REF_NAME(CommandQueue) commandQueue);
};

static inline NRI_NAME(Format) NRI_NAME(GetSupportedDepthFormat)(const NRI_REF_NAME(CoreInterface) coreInterface, const NRI_REF_NAME(Device) device, uint32_t minBits, bool stencil)
{
    if (stencil)
    {
        if (minBits <= 24)
        {
            if (NRI_REF_ACCESS(coreInterface)->GetFormatSupport(device, NRI_ENUM_MEMBER(Format, FORMAT, D24_UNORM_S8_UINT)) & NRI_ENUM_MEMBER(FormatSupportBits, FORMAT_SUPPORT_BITS, DEPTH_STENCIL_ATTACHMENT))
                return NRI_ENUM_MEMBER(Format, FORMAT, D24_UNORM_S8_UINT);
        }
    }
    else
    {
        if (minBits <= 16)
        {
            if (NRI_REF_ACCESS(coreInterface)->GetFormatSupport(device, NRI_ENUM_MEMBER(Format, FORMAT, D16_UNORM)) & NRI_ENUM_MEMBER(FormatSupportBits, FORMAT_SUPPORT_BITS, DEPTH_STENCIL_ATTACHMENT))
                return NRI_ENUM_MEMBER(Format, FORMAT, D16_UNORM);
        }
        else if (minBits <= 24)
        {
            if (NRI_REF_ACCESS(coreInterface)->GetFormatSupport(device, NRI_ENUM_MEMBER(Format, FORMAT, D24_UNORM_S8_UINT)) & NRI_ENUM_MEMBER(FormatSupportBits, FORMAT_SUPPORT_BITS, DEPTH_STENCIL_ATTACHMENT))
                return NRI_ENUM_MEMBER(Format, FORMAT, D24_UNORM_S8_UINT);
        }

        if (NRI_REF_ACCESS(coreInterface)->GetFormatSupport(device, NRI_ENUM_MEMBER(Format, FORMAT, D32_SFLOAT)) & NRI_ENUM_MEMBER(FormatSupportBits, FORMAT_SUPPORT_BITS, DEPTH_STENCIL_ATTACHMENT))
            return NRI_ENUM_MEMBER(Format, FORMAT, D32_SFLOAT);
    }

    if (NRI_REF_ACCESS(coreInterface)->GetFormatSupport(device, NRI_ENUM_MEMBER(Format, FORMAT, D32_SFLOAT_S8_UINT_X24)) & NRI_ENUM_MEMBER(FormatSupportBits, FORMAT_SUPPORT_BITS, DEPTH_STENCIL_ATTACHMENT))
        return NRI_ENUM_MEMBER(Format, FORMAT, D32_SFLOAT_S8_UINT_X24);

    return NRI_ENUM_MEMBER(Format, FORMAT, UNKNOWN);
}

static inline NRI_NAME(TextureDesc) NRI_NAME(Texture1D)(NRI_NAME(Format) format, uint16_t width, uint16_t mipNum NRI_DEFAULT_VALUE(1), uint16_t arraySize NRI_DEFAULT_VALUE(1), NRI_NAME(TextureUsageBits) usageMask NRI_DEFAULT_VALUE(NRI_ENUM_MEMBER(TextureUsageBits, TEXTURE_USAGE_BITS, SHADER_RESOURCE)))
{
    NRI_NAME(TextureDesc) textureDesc = NRI_ZERO_INIT;
    textureDesc.type = NRI_ENUM_MEMBER(TextureType, TEXTURE_TYPE, TEXTURE_1D);
    textureDesc.format = format;
    textureDesc.usageMask = usageMask;
    textureDesc.size[0] = width;
    textureDesc.size[1] = 1;
    textureDesc.size[2] = 1;
    textureDesc.mipNum = mipNum;
    textureDesc.arraySize = arraySize;
    textureDesc.sampleNum = 1;

    return textureDesc;
}

static inline NRI_NAME(TextureDesc) NRI_NAME(Texture2D)(NRI_NAME(Format) format, uint16_t width, uint16_t height, uint16_t mipNum NRI_DEFAULT_VALUE(1), uint16_t arraySize NRI_DEFAULT_VALUE(1), NRI_NAME(TextureUsageBits) usageMask NRI_DEFAULT_VALUE(NRI_ENUM_MEMBER(TextureUsageBits, TEXTURE_USAGE_BITS, SHADER_RESOURCE)), uint8_t sampleNum NRI_DEFAULT_VALUE(1))
{
    NRI_NAME(TextureDesc) textureDesc = NRI_ZERO_INIT;
    textureDesc.type = NRI_ENUM_MEMBER(TextureType, TEXTURE_TYPE, TEXTURE_2D);
    textureDesc.format = format;
    textureDesc.usageMask = usageMask;
    textureDesc.size[0] = width;
    textureDesc.size[1] = height;
    textureDesc.size[2] = 1;
    textureDesc.mipNum = mipNum;
    textureDesc.arraySize = arraySize;
    textureDesc.sampleNum = sampleNum;

    return textureDesc;
}

static inline NRI_NAME(TextureDesc) NRI_NAME(Texture3D)(NRI_NAME(Format) format, uint16_t width, uint16_t height, uint16_t depth, uint16_t mipNum NRI_DEFAULT_VALUE(1), NRI_NAME(TextureUsageBits) usageMask NRI_DEFAULT_VALUE(NRI_ENUM_MEMBER(TextureUsageBits, TEXTURE_USAGE_BITS, SHADER_RESOURCE)))
{
    NRI_NAME(TextureDesc) textureDesc = NRI_ZERO_INIT;
    textureDesc.type = NRI_ENUM_MEMBER(TextureType, TEXTURE_TYPE, TEXTURE_3D);
    textureDesc.format = format;
    textureDesc.usageMask = usageMask;
    textureDesc.size[0] = width;
    textureDesc.size[1] = height;
    textureDesc.size[2] = depth;
    textureDesc.mipNum = mipNum;
    textureDesc.arraySize = 1;
    textureDesc.sampleNum = 1;

    return textureDesc;
}

static inline NRI_NAME(TextureTransitionBarrierDesc) NRI_NAME(TextureTransition)(NRI_NAME(Texture)* texture, NRI_NAME(AccessBits) prevAccess, NRI_NAME(AccessBits) nextAccess, NRI_NAME(TextureLayout) prevLayout, NRI_NAME(TextureLayout) nextLayout,
    uint16_t mipOffset NRI_DEFAULT_VALUE(0), uint16_t mipNum NRI_DEFAULT_VALUE(NRI_NAME(REMAINING_MIP_LEVELS)), uint16_t arrayOffset NRI_DEFAULT_VALUE(0), uint16_t arraySize NRI_DEFAULT_VALUE(NRI_NAME(REMAINING_ARRAY_LAYERS)))
{
    NRI_NAME(TextureTransitionBarrierDesc) textureTransitionBarrierDesc = NRI_ZERO_INIT;
    textureTransitionBarrierDesc.texture = texture;
    textureTransitionBarrierDesc.prevAccess = prevAccess;
    textureTransitionBarrierDesc.nextAccess = nextAccess;
    textureTransitionBarrierDesc.prevLayout = prevLayout;
    textureTransitionBarrierDesc.nextLayout = nextLayout;
    textureTransitionBarrierDesc.mipOffset = mipOffset;
    textureTransitionBarrierDesc.mipNum = mipNum;
    textureTransitionBarrierDesc.arrayOffset = arrayOffset;
    textureTransitionBarrierDesc.arraySize = arraySize;

    return textureTransitionBarrierDesc;
}

static inline NRI_NAME(TextureTransitionBarrierDesc) NRI_NAME(TextureTransitionFromUnknown)(NRI_NAME(Texture)* texture, NRI_NAME(AccessBits) nextAccess, NRI_NAME(TextureLayout) nextLayout,
    uint16_t mipOffset NRI_DEFAULT_VALUE(0), uint16_t mipNum NRI_DEFAULT_VALUE(NRI_NAME(REMAINING_MIP_LEVELS)), uint16_t arrayOffset NRI_DEFAULT_VALUE(0), uint16_t arraySize NRI_DEFAULT_VALUE(NRI_NAME(REMAINING_ARRAY_LAYERS)))
{
    NRI_NAME(TextureTransitionBarrierDesc) textureTransitionBarrierDesc = NRI_ZERO_INIT;
    textureTransitionBarrierDesc.texture = texture;
    textureTransitionBarrierDesc.prevAccess = NRI_ENUM_MEMBER(AccessBits, ACCESS_BITS, UNKNOWN);
    textureTransitionBarrierDesc.nextAccess = nextAccess;
    textureTransitionBarrierDesc.prevLayout = NRI_ENUM_MEMBER(TextureLayout, TEXTURE_LAYOUT, UNKNOWN);
    textureTransitionBarrierDesc.nextLayout = nextLayout;
    textureTransitionBarrierDesc.mipOffset = mipOffset;
    textureTransitionBarrierDesc.mipNum = mipNum;
    textureTransitionBarrierDesc.arrayOffset = arrayOffset;
    textureTransitionBarrierDesc.arraySize = arraySize;

    return textureTransitionBarrierDesc;
}

static inline NRI_NAME(TextureTransitionBarrierDesc) NRI_NAME(TextureTransitionFromState)(NRI_REF_NAME(TextureTransitionBarrierDesc) prevState, NRI_NAME(AccessBits) nextAccess, NRI_NAME(TextureLayout) nextLayout,
    uint16_t mipOffset NRI_DEFAULT_VALUE(0), uint16_t mipNum NRI_DEFAULT_VALUE(NRI_NAME(REMAINING_MIP_LEVELS)))
{
    NRI_REF_ACCESS(prevState)->mipOffset = mipOffset;
    NRI_REF_ACCESS(prevState)->mipNum = mipNum;
    NRI_REF_ACCESS(prevState)->prevAccess = NRI_REF_ACCESS(prevState)->nextAccess;
    NRI_REF_ACCESS(prevState)->nextAccess = nextAccess;
    NRI_REF_ACCESS(prevState)->prevLayout = NRI_REF_ACCESS(prevState)->nextLayout;
    NRI_REF_ACCESS(prevState)->nextLayout = nextLayout;

    return *NRI_REF_ACCESS(prevState);
}

NRI_NAMESPACE_END
