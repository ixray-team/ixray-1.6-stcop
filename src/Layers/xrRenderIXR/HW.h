// HW.h: interface for the CHW class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "HWCaps.h"

#ifndef _MAYA_EXPORT
#include "stats_manager.h"
#endif

struct SDL_Window;


#define RDepth (GRHI->DevicePtr->RenderDSV)
#define RSwapchainTarget (GRHI->DevicePtr->SwapChainRTV)

#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
#define RTarget (GRHI->DevicePtr->RenderRTV)
#else
#define RTarget (GRHI->DevicePtr->SwapChainRTV)
#endif