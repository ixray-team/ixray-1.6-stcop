// HW.h: interface for the CHW class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "hwcaps.h"

#ifndef _EDITOR
#	include <renderdoc/api/app/renderdoc_app.h>
#endif

#ifndef _MAYA_EXPORT
#include "stats_manager.h"
#endif

struct SDL_Window;

#ifdef USE_DX11
#define RContext ((ID3D11DeviceContext*)g_RenderRHI->GetRenderContext())
#define RDevice ((ID3D11Device*)g_RenderRHI->GetRenderDevice())
#define RSwapchainTarget ((ID3D11RenderTargetView*)g_RenderRHI->GetSwapchainTexture())
#define RTarget ((ID3D11RenderTargetView*)g_RenderRHI->GetRenderTexture())
#define RDepth ((ID3D11DepthStencilView*)g_RenderRHI->GetDepthTexture())
#define RSwapchain ((IDXGISwapChain*)g_RenderRHI->GetSwapchain())
#else
#define RContext ((IDirect3DDevice9*)g_RenderRHI->GetRenderContext())
#define RDevice ((IDirect3DDevice9*)g_RenderRHI->GetRenderDevice())
#define RSwapchainTarget ((IDirect3DSurface9*)g_RenderRHI->GetSwapchainTexture())
#define RTarget ((IDirect3DSurface9*)g_RenderRHI->GetRenderTexture())
#define RDepth ((IDirect3DSurface9*)g_RenderRHI->GetDepthTexture())
#define RSwapchain ((IDirect3DDevice9*)g_RenderRHI->GetSwapchain())
#endif

#define RFeatureLevel ((D3D_FEATURE_LEVEL)g_RenderRHI->GetFeatureLevel())