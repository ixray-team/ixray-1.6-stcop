// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently

#pragma once

#include <d3d11_1.h>
#include "../../xrCore/D3DLegacy.h"

#include "../../xrEngine/stdafx.h"

#ifndef NULL
#define NULL nullptr
#endif

// HACK: the D3D-headers _RELEASE expands with NULL, which does not parse here; NULL-free variant.
#ifndef _RELEASE
#define _RELEASE(x) { if (x) { (x)->Release(); (x) = nullptr; } }
#endif

#ifndef _RELEASE
#ifndef ENGINE_API
#define ENGINE_API
#endif
#include "../../xrEngine/defines.h"
#endif

#ifndef _RELEASE
#define _RELEASE(x) { if (x) { (x)->Release(); (x) = nullptr; } }
#endif

#include "imgui.h"

#include <d3d11_1.h>
#include <D3DCompiler.h>

#include "DXCommonTypes.h"

#define MU_LODS_TRUE
#include "particle_core/psystem.h"

#include "HW.h"
#include "Shader.h"
#include "R_Backend.h"
#include "R_Backend_Runtime.h"

#include "ResourceManager.h"

#include "../../xrEngine/vis_common.h"
#include "../../xrEngine/Render.h"
#include "../../xrEngine/_d3d_extensions.h"
#include "../../xrEngine/IGame_Level.h"
#include "blenders/Blender.h"
#include "blenders/Blender_CLSID.h"
#include "xrRender_console.h"
#include "r4.h"

IC	void jitter(CBlender_Compile& C)
{
	C.r_dx10Texture("jitter0", JITTER(0));
	C.r_dx10Texture("jitter1", JITTER(1));
	C.r_dx10Texture("jitter2", JITTER(2));

	C.r_dx10Sampler("smp_jitter");
}
#if defined(IXR_APPLE_SERIES)
#undef NULL
#define NULL 0

#undef _RELEASE
#define _RELEASE(x) { if (x) { (x)->Release(); (x) = nullptr; } }
#endif
#if defined(IXR_APPLE_SERIES)
inline int32_t ffxFsr3UpscalerGetJitterPhaseCount(int32_t, int32_t) { return 1; }
inline void ffxFsr3UpscalerGetJitterOffset(float* x, float* y, int32_t, int32_t) { *x = 0.f; *y = 0.f; }
#endif
