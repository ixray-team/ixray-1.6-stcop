// HW.h: interface for the CHW class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "hwcaps.h"

#include <renderdoc/api/app/renderdoc_app.h>

#ifndef _MAYA_EXPORT
#include "stats_manager.h"
#endif

struct SDL_Window;

#ifdef USE_DX11
#define RContext ((ID3D11DeviceContext*)Device.GetRenderContext())
#define RDevice ((ID3D11Device*)Device.GetRenderDevice())
#define RSwapchainTarget ((ID3D11RenderTargetView*)Device.GetSwapchainTexture())
#define RTarget ((ID3D11RenderTargetView*)Device.GetRenderTexture())
#define RDepth ((ID3D11DepthStencilView*)Device.GetDepthTexture())
#define RSwapchain ((IDXGISwapChain*)Device.GetSwapchain())
#else
#define RContext (IDirect3DDevice9*)Device.GetRenderContext()
#define RDevice (IDirect3DDevice9*)Device.GetRenderDevice()
#define RSwapchainTarget ((IDirect3DSurface9*)Device.GetSwapchainTexture())
#define RTarget ((IDirect3DSurface9*)Device.GetRenderTexture())
#define RDepth ((IDirect3DSurface9*)Device.GetDepthTexture())
#define RSwapchain ((IDirect3DDevice9*)Device.GetSwapchain())
#endif

#define RFeatureLevel Device.GetFeatureLevel()