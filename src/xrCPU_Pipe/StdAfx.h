#pragma once

#ifndef _EDITOR
#ifndef _WIN32_WINNT
	#define _WIN32_WINNT 0x0501
#endif // _WIN32_WINNT
#endif

#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <stdio.h>

#ifndef _EDITOR
#pragma warning(disable:4995)
	#include <intrin.h>
#pragma warning(default:4995)
#endif

#define ENGINE_API
#define ECORE_API

#ifdef _EDITOR
#include "../xrCore/xrCore.h"
#pragma warning(disable:4995)
#include <d3dx9.h>
#pragma warning(default:4995)
#include "../Layers/xrRender/xrD3DDefs.h"
#include "../Layers/xrRender/HW.h"
#include "../Layers/xrRender/Shader.h"
#include "../xrServerEntities/PropertiesListTypes.h"
#include "../Layers/xrRender/R_Backend.h"
#include "../Layers/xrRender/R_Backend_Runtime.h"
#include "../Layers/xrRender/resourcemanager.h"
#include "../xrEngine/Fmesh.h"
#	include "skeletonX.h"
#	include "skeletoncustom.h"
#else // _EDITOR
	#include "../xrCore/xrCore.h"
	#include "../Layers/xrRender/SkeletonXVertRender.h"
	#include "../xrEngine/bone.h"
	#define RENDER 1
	#include "../xrEngine/Render.h"
	#include "../xrEngine/Device.h"
	#include "../Layers/xrRender/light.h"
#endif // _EDITOR

#include "xrCPU_Pipe.h"
#include "ttapi.h"