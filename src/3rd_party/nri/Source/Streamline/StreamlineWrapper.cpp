/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include <Extensions/Optional/NRIStreamlineWrapper.h>

#define _STRINGIFY(str) L#str
#define STRINGIFY(str) _STRINGIFY(str)

const wchar_t* pluginPath = STRINGIFY(SL_PLUGINS_DIRECTORY);

sl::Result slwrap::setConstants(const sl::Constants& values, const sl::FrameToken& frame, const sl::ViewportHandle& viewport)
{
    return slSetConstants(values, frame, viewport);
}

sl::Result slwrap::shutdown() { return slShutdown(); }

sl::Result slwrap::init(const sl::Preferences& pref, uint64_t sdkVersion)
{
    sl::Preferences preferences = pref;
    if (preferences.numPathsToPlugins == 0)
    {
        preferences.pathsToPlugins = &pluginPath;
        preferences.numPathsToPlugins = 1;
    }
    return slInit(preferences, sdkVersion);
}

sl::Result slwrap::setTag(const sl::ViewportHandle& viewport, const sl::ResourceTag* tags, uint32_t numTags, sl::CommandBuffer* cmdBuffer)
{
    return slSetTag(viewport, tags, numTags, cmdBuffer);
}

sl::Result slwrap::isFeatureSupported(sl::Feature feature, const sl::AdapterInfo& adapterInfo)
{
    return slIsFeatureSupported(feature, adapterInfo);
}

sl::Result slwrap::isFeatureLoaded(sl::Feature feature, bool& loaded)
{
    return slIsFeatureLoaded(feature, loaded);
}

sl::Result slwrap::setFeatureLoaded(sl::Feature feature, bool loaded)
{
    return slSetFeatureLoaded(feature, loaded);
}

sl::Result slwrap::getFeatureRequirements(sl::Feature feature, sl::FeatureRequirements& requirements)
{
    return slGetFeatureRequirements(feature, requirements);
}

sl::Result slwrap::getFeatureVersion(sl::Feature feature, sl::FeatureVersion& version)
{
    return slGetFeatureVersion(feature, version);
}

sl::Result slwrap::allocateResources(sl::CommandBuffer* cmdBuffer, sl::Feature feature, const sl::ViewportHandle& viewport)
{
    return slAllocateResources(cmdBuffer, feature, viewport);
}

sl::Result slwrap::freeResources(sl::Feature feature, const sl::ViewportHandle& viewport)
{
    return slFreeResources(feature, viewport);
}

sl::Result slwrap::evaluateFeature(sl::Feature feature, const sl::FrameToken& frame, const sl::BaseStructure** inputs, uint32_t numInputs, sl::CommandBuffer* cmdBuffer)
{
    return slEvaluateFeature(feature, frame, inputs, numInputs, cmdBuffer);
}

sl::Result slwrap::upgradeInterface(void** baseInterface)
{
    return slUpgradeInterface(baseInterface);
}

sl::Result slwrap::getNativeInterface(void* proxyInterface, void** baseInterface)
{
    return slGetNativeInterface(proxyInterface, baseInterface);
}

sl::Result slwrap::getFeatureFunction(sl::Feature feature, const char* functionName, void*& function)
{
    return slGetFeatureFunction(feature, functionName, function);
}

sl::Result slwrap::getNewFrameToken(sl::FrameToken*& token, const uint32_t* frameIndex)
{
    return slGetNewFrameToken(token, frameIndex);
}

sl::Result slwrap::setD3DDevice(void* d3dDevice)
{
    return slSetD3DDevice(d3dDevice);
}