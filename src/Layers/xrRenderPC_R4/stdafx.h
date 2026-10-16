// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently

#pragma once

#include <d3d11_1.h>
#include "../../xrCore/D3DLegacy.h"

#include "../../xrEngine/stdafx.h"

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
