#pragma once

// Renderer surface used by the editors, which link xrRender_R4 and run it in editor mode
// (Device.IsEditorMode()). Include after the editor device headers.

#include "../../xrCore/Collision/Frustum.h"
#include "../../xrEngine/vis_common.h"
#include "../../xrEngine/Render.h"
#include "../../xrCore/Kernel/xrAPI.h"
#include <d3dcompiler.h>

#include "../xrRender/HW.h"
#include "../xrRender/Shader.h"
#include "../xrRender/R_Backend.h"
#include "../xrRender/R_Backend_Runtime.h"
#include "../xrRender/ResourceManager.h"
#include "../xrRender/blenders/Blender.h"
#include "../xrRender/blenders/Blender_CLSID.h"
#include "../xrRender/xrRender_console.h"
#include "../xrRender/PSLibrary.h"
#include "../xrRender/IRenderDetailModel.h"
#include "../xrRender/DetailModel.h"
#include "../xrRender/ModelPool.h"
#include "../xrRender/SkeletonCustom.h"
#include "../xrRender/light.h"
#include "../xrRender/dxRenderDeviceRender.h"
#include "../xrRender/EditorRenderHost.h"
#include "../xrRender/EditorDeviceFlags.h"

#include "r4.h"
