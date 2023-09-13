/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#pragma once

#include <climits>

#include <NRI.h>
#include <sl.h>

namespace slwrap
{
    NRI_API sl::Result NRI_CALL init(const sl::Preferences& pref, uint64_t sdkVersion = sl::kSDKVersion);
    NRI_API sl::Result NRI_CALL shutdown();
    NRI_API sl::Result NRI_CALL isFeatureSupported(sl::Feature feature, const sl::AdapterInfo& adapterInfo);
    NRI_API sl::Result NRI_CALL isFeatureLoaded(sl::Feature feature, bool& loaded);
    NRI_API sl::Result NRI_CALL setFeatureLoaded(sl::Feature feature, bool loaded);
    NRI_API sl::Result NRI_CALL setTag(const sl::ViewportHandle& viewport, const sl::ResourceTag* tags, uint32_t numTags, sl::CommandBuffer* cmdBuffer);
    NRI_API sl::Result NRI_CALL setConstants(const sl::Constants& values, const sl::FrameToken& frame, const sl::ViewportHandle& viewport);
    NRI_API sl::Result NRI_CALL getFeatureRequirements(sl::Feature feature, sl::FeatureRequirements& requirements);
    NRI_API sl::Result NRI_CALL getFeatureVersion(sl::Feature feature, sl::FeatureVersion& version);
    NRI_API sl::Result NRI_CALL allocateResources(sl::CommandBuffer* cmdBuffer, sl::Feature feature, const sl::ViewportHandle& viewport);
    NRI_API sl::Result NRI_CALL freeResources(sl::Feature feature, const sl::ViewportHandle& viewport);
    NRI_API sl::Result NRI_CALL evaluateFeature(sl::Feature feature, const sl::FrameToken& frame, const sl::BaseStructure** inputs, uint32_t numInputs, sl::CommandBuffer* cmdBuffer);
    NRI_API sl::Result NRI_CALL upgradeInterface(void** baseInterface);
    NRI_API sl::Result NRI_CALL getNativeInterface(void* proxyInterface, void** baseInterface);
    NRI_API sl::Result NRI_CALL getFeatureFunction(sl::Feature feature, const char* functionName, void*& function);
    NRI_API sl::Result NRI_CALL getNewFrameToken(sl::FrameToken*& token, const uint32_t* frameIndex = nullptr);
    NRI_API sl::Result NRI_CALL setD3DDevice(void* d3dDevice);
}
