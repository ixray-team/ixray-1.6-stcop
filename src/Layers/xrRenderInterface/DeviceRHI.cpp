#include "stdafx.h"
#include "linker.h"
#include <d3d11.h>
#include "DeviceRHI.h"

RHI_API IRender_RHI* g_RenderRHI = nullptr;
RHI_API u32 psCurrentVidMode[2] = { 1024,768 };

SPixelFormats g_PixelFormats[FMT_MAX_COUNT];

RHI_API Flags32 psDeviceFlags = { rsDetails | mtPhysics | mtSound | mtNetwork | rsDrawStatic | rsDrawDynamic | rsDeviceActive | mtParticles };
RHI_API Flags32 psGameFlags = { rsActorShadow };