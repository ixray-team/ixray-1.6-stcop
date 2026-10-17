// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently

#pragma once

#include <d3d9.h>

// DXGI joke
#define DXGI_RGBA D3DCOLORVALUE

#include "../../xrEngine/stdafx.h"

// AI: the fluid renderer expands the engine's _RELEASE, which assigns NULL. Neither NULL nor the
// AI: macro itself reaches these translation units - the platform layer is not part of their
// AI: include chain on this host - so both are provided here, for the renderer build only. The
// AI: Windows path defines neither, so this is inert there.
#ifndef NULL
#define NULL nullptr
#endif

#ifndef _RELEASE
#define _RELEASE(x) { if (x) { (x)->Release(); (x) = nullptr; } }
#endif

// AI: the fluid renderer (xrRenderDX10/3DFluid) uses the engine's _RELEASE macro, which on
// AI: Windows arrives transitively through the SDK headers but is missing on the DXVK Native
// AI: hosts. Pulled in explicitly here, and only when it is not defined already.
#ifndef _RELEASE
// AI: xrEngine/defines.h guards _RELEASE and _SHOW_REF behind #ifdef ENGINE_API, which the
// AI: renderer builds do not set. Defining it here, only when the macro is still missing, keeps
// AI: the precompiled header working without touching the Windows path.
#ifndef ENGINE_API
#define ENGINE_API
#endif
#include "../../xrEngine/defines.h"
#endif

#ifndef _RELEASE
// AI: defines.h is protected by #pragma once and may already have been included without
// AI: ENGINE_API by something else above, so the macro is spelled out here as a last resort.
#define _RELEASE(x) { if (x) { (x)->Release(); (x) = nullptr; } }
#endif

#include "imgui.h"

#include <d3d11_1.h>
#include <D3DCompiler.h>

#include "../xrRenderDX10/DXCommonTypes.h"

#define		R_R1	1
#define		R_R2	2
#define		R_R4	4
#define		RENDER	R_R4

#include "../xrRender/particle_core/psystem.h"

#include "../xrRender/HW.h"
#include "../xrRender/Shader.h"
#include "../xrRender/R_Backend.h"
#include "../xrRender/R_Backend_Runtime.h"

#include "../xrRender/ResourceManager.h"

#include "../../xrEngine/vis_common.h"
#include "../../xrEngine/Render.h"
#include "../../xrEngine/_d3d_extensions.h"
#include "../../xrEngine/IGame_Level.h"
#include "../xrRender/blenders/Blender.h"
#include "../xrRender/blenders/Blender_CLSID.h"
#include "../xrRender/xrRender_console.h"
#include "r4.h"

IC	void jitter(CBlender_Compile& C)
{
	C.r_dx10Texture("jitter0", JITTER(0));
	C.r_dx10Texture("jitter1", JITTER(1));
	C.r_dx10Texture("jitter2", JITTER(2));

	C.r_dx10Sampler("smp_jitter");
}
#if defined(IXR_MACOS)
// AI: some of the D3D compatibility headers on this host drop NULL, and several renderer call
// AI: sites pass NULL to the D3D entry points, so it is restored here as plain 0, the way the
// AI: engine's own platform files do it.
#undef NULL
#define NULL 0

// AI: some of the D3D compatibility headers on this host drop NULL, which breaks the engine's
// AI: _RELEASE, because that macro assigns NULL. The renderer builds therefore use an equivalent
// AI: macro that does not depend on it. Windows keeps the engine's own definition untouched.
#undef _RELEASE
#define _RELEASE(x) { if (x) { (x)->Release(); (x) = nullptr; } }
#endif
#if defined(IXR_MACOS)
// AI: FSR3 is disabled on macOS, but the renderer still asks the SDK for its jitter sequence
// AI: when temporal upscaling is off. The vendor header does not exist here, so the two helpers
// AI: are provided as no-ops (no jitter). Windows keeps the real FidelityFX functions.
inline int32_t ffxFsr3UpscalerGetJitterPhaseCount(int32_t, int32_t) { return 1; }
inline void ffxFsr3UpscalerGetJitterOffset(float* x, float* y, int32_t, int32_t) { *x = 0.f; *y = 0.f; }
#endif
