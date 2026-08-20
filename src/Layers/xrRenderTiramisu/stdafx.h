#pragma once
#include <atomic>
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

#include "Legacy/Interface/Core/XRayRenderDeviceRender.h"
#include "Legacy/Interface/Debug/XRayObjectSpaceRender.h"
#include "Legacy/Interface/Debug/XRayStatGraphRender.h"
#include "Legacy/Interface/Debug/XRayStatsRender.h"
#include "Legacy/Interface/Environment/XRayEnvironmentRender.h"
#include "Legacy/Interface/Environment/XRayFlareRender.h"
#include "Legacy/Interface/Environment/XRayLensFlareRender.h"
#include "Legacy/Interface/Environment/XRayRainRender.h"
#include "Legacy/Interface/Thunderbolt/XRayThunderboltDescRender.h"
#include "Legacy/Interface/Thunderbolt/XRayThunderboltRender.h"
#include "Legacy/Interface/UI/XRayFontRender.h"
#include "Legacy/Interface/UI/XRayUIRender.h"
#include "Legacy/Interface/UI/XRayUISequenceVideoItem.h"
#include "Legacy/Interface/WallMark/XRayWallMarkArray.h"
#include "Legacy/Interface/XRayDebugRender.h"
#include "Legacy/Interface/XRayDUInterface.h"
#include "Legacy/Interface/XRayRenderFactory.h"
#include "Legacy/Interface/XRayRenderInterface.h"
#include "../../xrEngine/Fmesh.h"

#undef Device
#include "NRI.h"

enum : uint32_t
{
	INDEX_NONE = -1
};
template <typename T>
inline T Align(T x, size_t alignment)
{
	return (T)((size_t(x) + alignment - 1) & ~(alignment - 1));
}

#define NRI_CHECK(x) R_ASSERT((x) == nri::Result::SUCCESS)


extern std::atomic_size_t GRenderThreadId;
extern const size_t GGameThreadId;
#include "Core/TThreadAffinity.h"
inline bool IsRenderThreadRunning()
{
	return GRenderThreadId.load(std::memory_order_acquire) != GGameThreadId;
}
#define CheckIsGameThread() VERIFY(Tiramisu::Threading::IsThreadRoleSatisfied(                                                                                      \
	Tiramisu::Threading::EThreadRole::Game, IsRenderThreadRunning(), Platform::GetCurrentThreadId(), GGameThreadId, GRenderThreadId.load(std::memory_order_acquire) \
))
#define CheckIsRenderThread() VERIFY(Tiramisu::Threading::IsThreadRoleSatisfied(                                                                                      \
	Tiramisu::Threading::EThreadRole::Render, IsRenderThreadRunning(), Platform::GetCurrentThreadId(), GGameThreadId, GRenderThreadId.load(std::memory_order_acquire) \
))
#define CheckIsCpuResourceThread() VERIFY( \
	Tiramisu::Threading::IsCpuResourceThreadSatisfied( \
		IsRenderThreadRunning(), Platform::GetCurrentThreadId(), \
		GRenderThreadId.load(std::memory_order_acquire) \
	) \
)

#include "RenderCommandQueue.h"
#include "Core/TiramisuRenderDevice.h"
#include "Resources/Textures/TiramisuRenderTexture.h"
#include "Resources/Textures/TiramisuRenderTexture2D.h"
#include "Resources/Textures/TiramisuRenderTexturesManager.h"
#include "Resources/RenderVertexTypes.h"
#include "Resources/TiramisuRenderResourcesManager.h"
#include "Resources/TiramisuRenderDescriptorHeapAllocator.h"
#include "Resources/Shaders/ShaderType.h"
#include "Resources/Shaders/Defines/TiramisuShaderDefinesContainer.h"
#include "Resources/Shaders/Global/TiramisuGlobalShadersManager.h"
