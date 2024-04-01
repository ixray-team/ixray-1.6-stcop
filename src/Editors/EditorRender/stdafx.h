#pragma once
#include <d3d9.h>

#include "../EditorEngineAPI/stdafx.h"
#include "../../xrParticles/psystem.h"

#include <fast_dynamic_cast/fast_dynamic_cast.hpp>
#define smart_cast fast_dynamic_cast
#include "../EditorProps/PropValue.h"

#define R_R1 1
#define RENDER R_R1

#define TEX_POINT_ATT "internal\\internal_light_attpoint"
#define TEX_SPOT_ATT  "internal\\internal_light_attclip"

#include "../../Layers/xrRenderDX9/xrD3DDefs.h"
#include "../../Layers/xrRender/Shader.h"
#include "../../Layers/xrRender/blenders/Blender.h"
#include "../../Layers/xrRender/blenders/Blender_CLSID.h"

#define RContext ((IDirect3DDevice9*)Device.GetRenderContext())
#define RDevice ((IDirect3DDevice9*)Device.GetRenderDevice())
#define RSwapchainTarget ((IDirect3DSurface9*)Device.GetSwapchainTexture())
#define RTarget ((IDirect3DSurface9*)Device.GetRenderTexture())
#define RDepth ((IDirect3DSurface9*)Device.GetDepthTexture())
#define RSwapchain ((IDirect3DDevice9*)Device.GetSwapchain())

#include "../../Layers/xrRender/R_Backend.h"
#include "../../Layers/xrRender/R_Backend_Runtime.h"
#include "../../Layers/xrRender/HWCaps.h"

#include "Resource.h"
#include "render.h"
#include "../../Layers/xrRender/Shader.h"

using ref_sound = shared_str;