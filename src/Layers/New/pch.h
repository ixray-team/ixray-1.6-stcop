#pragma once
#include "../../xrEngine/stdafx.h"
#include "../../xrEngine/Render.h"
#include "../../Include/xrApi/xrAPI.h"
#include "../xrRender/FVF.h"
#include "../xrRender/Shader.h"
#include "../xrRender/dxUIRender.h"
#include "../xrRender/dxDebugRender.h"
#include "../../Include/xrRender/particles_systems_library_interface.hpp"
#include "../xrRender/ParticleGroup.h"

#include "RenderUI.h"
#include "RenderConsole.h"
#include "RenderTarget.h"
#include "RenderInterface.h"
#include "RenderFactory.h"
#include "RenderDebug.h"

extern Rendering::RenderInterface RenderEngineInterface;
extern Rendering::UIRender UIRenderInterface;
extern Rendering::RenderFactory RenderFactoryInterface;

#include "../../xrEngine/gamefont.h"
#include "../xrRender/D3DUtils.h"
#include "RenderDU.h"

