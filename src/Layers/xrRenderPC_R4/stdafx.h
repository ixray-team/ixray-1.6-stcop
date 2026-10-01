// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently

#pragma once

#include <d3d11_1.h>
#include "../../xrCore/D3DLegacy.h"

#include "../../xrEngine/stdafx.h"

#include <imgui.h>

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

#ifndef _EDITOR
#	include "r4.h"
#endif

IC void jitter(CBlender_Compile& C)
{
	C.r_dx10Texture("jitter0", JITTER(0));
	C.r_dx10Texture("jitter1", JITTER(1));
	C.r_dx10Texture("jitter2", JITTER(2));

	C.r_dx10Sampler("smp_jitter");
}
