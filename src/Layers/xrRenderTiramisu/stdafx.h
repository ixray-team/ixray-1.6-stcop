#pragma once
#include "../../xrEngine/stdafx.h"

#include "../../xrEngine/Render.h"
#include "../../xrEngine/SkeletonMotions.h"
#include "../../xrEngine/SkeletonMotionDefs.h"

#include "../../Include/xrRender/RenderFactory.h"
#include "../../Include/xrRender/UISequenceVideoItem.h"
#include "../../Include/xrRender/StatGraphRender.h"
#include "../../Include/xrRender/EnvironmentRender.h"
#include "../../Include/xrRender/LensFlareRender.h"
#include "../../Include/xrRender/RainRender.h"
#include "../../Include/xrRender/ThunderboltDescRender.h"
#include "../../Include/xrRender/ThunderboltRender.h"
#include "../../Include/xrRender/RenderDeviceRender.h"
#include "../../Include/xrRender/StatsRender.h"
#include "../../Include/xrRender/WallMarkArray.h"
#include "../../Include/xrRender/ObjectSpaceRender.h"
#include "../../Include/xrRender/DrawUtils.h"
#include "../../Include/xrRender/FontRender.h"
#include "../../Include/xrRender/UIShader.h"
#include "../../Include/xrRender/UIRender.h"
#include "../../Include/xrRender/DebugRender.h"
#include "../../xrEngine/Render.h"
#include "../../Include/xrRender/DebugRender.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../../Include/xrRender/Kinematics.h"

#include "Interface/Core/XRayRenderDeviceRender.h"
#include "Interface/Debug/XRayObjectSpaceRender.h"
#include "Interface/Debug/XRayStatGraphRender.h"
#include "Interface/Debug/XRayStatsRender.h"
#include "Interface/Environment/XRayEnvironmentRender.h"
#include "Interface/Environment/XRayFlareRender.h"
#include "Interface/Environment/XRayLensFlareRender.h"
#include "Interface/Environment/XRayRainRender.h"
#include "Interface/Thunderbolt/XRayThunderboltDescRender.h"
#include "Interface/Thunderbolt/XRayThunderboltRender.h"
#include "Interface/UI/XRayFontRender.h"
#include "Interface/UI/XRayUIRender.h"
#include "Interface/UI/XRayUISequenceVideoItem.h"
#include "Interface/WallMark/XRayWallMarkArray.h"
#include "Interface/XRayDebugRender.h"
#include "Interface/XRayDUInterface.h"
#include "Interface/XRayRenderFactory.h"
#include "Interface/XRayRenderInterface.h"
#include "../../xrEngine/Fmesh.h"

#undef Device
#include "NRI.h"

enum :uint32_t {INDEX_NONE	= -1				};
template <typename T>
inline T Align(T x, size_t alignment) {
    return (T)((size_t(x) + alignment - 1) & ~(alignment - 1));
}

#define NRI_CHECK(x) R_ASSERT((x) ==  nri::Result::SUCCESS)

#include "Core/XRayRenderDevice.h"
#include "Resources/Textures/XRayTexture.h"
#include "Resources/Textures/XRayTexture2D.h"
#include "Resources/Textures/XRayTexturesManager.h"
#include "Resources/XRayRenderVertexTypes.h"
#include "Resources/XRayRenderResourcesManager.h"
#include "Resources/XRayRenderDescriptorHeapAllocator.h"
#include "Resources/Shaders/XRayShaderType.h"
#include "Resources/Shaders/Defines/XRayShaderDefinesContainer.h"
#include "Resources/Shaders/Global/XRayGlobalShadersManager.h"


