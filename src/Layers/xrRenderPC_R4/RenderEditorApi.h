#pragma once

// Renderer surface used by the editors, which link xrRender_R4 and run it in editor mode
// (Device.IsEditorMode()). Include after the editor device headers.

#include "../../xrCore/Collision/Frustum.h"
#include "../../xrEngine/vis_common.h"
#include "../../xrEngine/Render.h"
#include "../../xrCore/Kernel/xrAPI.h"
#include <d3dcompiler.h>

#include "HW.h"
#include "Shader.h"
#include "R_Backend.h"
#include "R_Backend_Runtime.h"
#include "ResourceManager.h"
#include "blenders/Blender.h"
#include "blenders/Blender_CLSID.h"
#include "xrRender_console.h"
#include "PSLibrary.h"
#include "IRenderDetailModel.h"
#include "DetailModel.h"
#include "ModelPool.h"
#include "SkeletonCustom.h"
#include "light.h"
#include "dxRenderDeviceRender.h"
#include "EditorRenderHost.h"
#include "EditorDeviceFlags.h"

#include "r4.h"
