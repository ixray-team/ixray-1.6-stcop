// HW.h: interface for the CHW class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "HWCaps.h"

#ifndef _MAYA_EXPORT
#include "stats_manager.h"
#endif

struct SDL_Window;

#ifdef USE_DX11
#define RContext ((ID3D11DeviceContext*)GRHI->GetContext())
#define RDevice ((ID3D11Device*)Device.GetRenderDevice())
#define RSwapchain ((IDXGISwapChain*)Device.GetSwapchain())
#else
#define RContext ((IDirect3DDevice9*)GRHI->GetContext())
#define RDevice ((IDirect3DDevice9*)Device.GetRenderDevice())
#define RSwapchain ((IDirect3DDevice9*)Device.GetSwapchain())
#endif

#define RFeatureLevel (GRHI->DevicePtr->FeatureLevel)
#define RDepth (GRHI->DevicePtr->RenderDSV)
#define RSwapchainTarget (GRHI->DevicePtr->SwapChainRTV)

#ifdef DEBUG_DRAW
#define RTarget (GRHI->DevicePtr->RenderRTV)
#else
#define RTarget (GRHI->DevicePtr->SwapChainRTV)
#endif