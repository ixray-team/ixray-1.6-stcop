// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently

#pragma once

#include "../../xrEngine/stdafx.h"

#include "imgui.h"

// Vulkan includes
#define VK_USE_PLATFORM_WIN32_KHR
#include <vulkan/vulkan.h>

#define		R_R1	1
#define		R_R2	2
#define		R_R4	4
#define		R_VK	5
#define		RENDER	R_VK

#include "../../xrParticles/psystem.h"

#include "../xrRender/HW.h"
#include "../xrRender/Shader.h"
#include "../xrRender/R_Backend.h"
#include "../xrRender/R_Backend_Runtime.h"

#include "../xrRender/ResourceManager.h"

#include "../../xrEngine/vis_common.h"
#include "../../xrEngine/Render.h"
#include "../../xrEngine/IGame_Level.h"
#include "../xrRender/blenders/Blender.h"
#include "../xrRender/blenders/Blender_CLSID.h"
#include "../xrRender/xrRender_console.h"
#include "vk.h"