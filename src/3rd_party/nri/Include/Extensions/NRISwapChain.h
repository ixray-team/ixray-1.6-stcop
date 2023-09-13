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

NRI_FORWARD_STRUCT(SwapChain);
NRI_FORWARD_STRUCT(Display);

NRI_ENUM
(
    SwapChainFormat, SWAP_CHAIN_FORMAT, uint8_t,

    /* BT.709 - LDR https://en.wikipedia.org/wiki/Rec._709
       BT.2020 - HDR https://en.wikipedia.org/wiki/Rec._2020
       G10 - linear (gamma 1.0)
       G22 - sRGB (gamma ~2.2)
       G2084 - SMPTE ST.2084 (Perceptual Quantization) */

    BT709_G10_8BIT,
    BT709_G10_16BIT,
    BT709_G22_8BIT,
    BT709_G22_10BIT,
    BT2020_G2084_10BIT,

    MAX_NUM
);

NRI_ENUM
(
    WindowSystemType, WINDOW_SYSTEM_TYPE, uint8_t,

    WINDOWS,
    X11,
    WAYLAND,
    METAL,

    MAX_NUM
);

NRI_STRUCT(WindowsWindow)
{
    void* hwnd; // HWND
};

NRI_STRUCT(X11Window)
{
    void* dpy; // Display*
    uint64_t window; // Window
};

NRI_STRUCT(WaylandWindow)
{
    void* display; // wl_display*
    void* surface; // wl_surface*
};

NRI_STRUCT(MetalWindow)
{
    void* caMetalLayer;
};

NRI_UNION(Window)
{
    NRI_NAME(WindowsWindow) windows;
    NRI_NAME(X11Window) x11;
    NRI_NAME(WaylandWindow) wayland;
    NRI_NAME(MetalWindow) metal;
};

// SwapChain buffers will be created as "color attachment" resources
NRI_STRUCT(SwapChainDesc)
{
    NRI_NAME(WindowSystemType) windowSystemType;
    NRI_NAME(Window) window;
    const NRI_NAME(CommandQueue)* commandQueue;
    uint16_t width;
    uint16_t height;
    uint16_t textureNum;
    NRI_NAME(SwapChainFormat) format;
    uint32_t verticalSyncInterval;
    uint32_t physicalDeviceIndex;
    NRI_NAME(Display)* display;
};

NRI_STRUCT(HdrMetadata)
{
    float displayPrimaryRed[2];
    float displayPrimaryGreen[2];
    float displayPrimaryBlue[2];
    float whitePoint[2];
    float luminanceMax;
    float luminanceMin;
    float contentLightLevelMax;
    float frameAverageLightLevelMax;
};

NRI_STRUCT(SwapChainInterface)
{
    NRI_NAME(Result) (NRI_CALL *CreateSwapChain)(NRI_REF_NAME(Device) device, const NRI_REF_NAME(SwapChainDesc) swapChainDesc, NRI_REF_NAME(SwapChain*) swapChain);
    void (NRI_CALL *DestroySwapChain)(NRI_REF_NAME(SwapChain) swapChain);
    void (NRI_CALL *SetSwapChainDebugName)(NRI_REF_NAME(SwapChain) swapChain, const char* name);
    NRI_NAME(Texture)* const* (NRI_CALL *GetSwapChainTextures)(const NRI_REF_NAME(SwapChain) swapChain, NRI_REF(uint32_t) textureNum, NRI_REF_NAME(Format) format);
    uint32_t (NRI_CALL *AcquireNextSwapChainTexture)(NRI_REF_NAME(SwapChain) swapChain);
    NRI_NAME(Result) (NRI_CALL *SwapChainPresent)(NRI_REF_NAME(SwapChain) swapChain);
    NRI_NAME(Result) (NRI_CALL *SetSwapChainHdrMetadata)(NRI_REF_NAME(SwapChain) swapChain, const NRI_REF_NAME(HdrMetadata) hdrMetadata);
    NRI_NAME(Result) (NRI_CALL *GetDisplays)(NRI_REF_NAME(Device) device, NRI_NAME(Display)** displays, NRI_REF(uint32_t) displayNum);
    NRI_NAME(Result) (NRI_CALL *GetDisplaySize)(NRI_REF_NAME(Device) device, NRI_REF_NAME(Display) display, NRI_REF(uint16_t) width, NRI_REF(uint16_t) height);
};

NRI_NAMESPACE_END
