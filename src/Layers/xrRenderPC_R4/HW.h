// HW.h: interface for the CHW class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "HWCaps.h"
#include "stats_manager.h"

struct SDL_Window;


#define RFeatureLevel (GRHI->DevicePtr->FeatureLevel)
#define RDepth (GRHI->DevicePtr->RenderDSV)
#define RSwapchainTarget (GRHI->DevicePtr->SwapChainRTV)

#if defined(DEBUG_DRAW) && defined(IXR_WINDOWS)
#define RTarget (GRHI->DevicePtr->RenderRTV)
#else
#define RTarget (GRHI->DevicePtr->SwapChainRTV)
#endif