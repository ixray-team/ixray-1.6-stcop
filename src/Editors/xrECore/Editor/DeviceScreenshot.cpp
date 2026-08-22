//---------------------------------------------------------------------------
#include "stdafx.h"

#include "UI_ToolsCustom.h"
#include "device.h"
#include "ui_main.h"
#include "../Layers/xrRender/ResourceManager.h"
#include "../Layers/xrRender/dxRenderDeviceRender.h"

#include <d3d11.h>

bool CEditorRenderDevice::MakeScreenshot(U32Vec& pixels, u32 width, u32 height)
{
	if (!b_is_Ready) return false;
	if (width == 0 || height == 0) return false;

	// Ensure the output buffer is always valid (the caller ignores the return
	// value and immediately uses pixels.data()). Fill with the clear color so a
	// failed capture never produces a null pointer.
	pixels.resize(width * height, 0);

	// free managed resource
	Resources->Evict();

	// Save current render targets / depth stencil so we can restore them
	IRHIRenderTargetView* pSavedRT[4] = {
		GRHI->GetRenderTargetView(0),
		GRHI->GetRenderTargetView(1),
		GRHI->GetRenderTargetView(2),
		GRHI->GetRenderTargetView(3),
	};
	IRHIDepthStencilView* pSavedDSV = GRHI->GetDepthStencilView();

	// Create offscreen color target and depth-stencil at the requested size
	ref_rt rtColor;
	ref_rt rtDepth;
	rtColor.create("$user$screenshot_color", width, height, ERHI_FORMAT::B8G8R8A8_UNORM);
	rtDepth.create("$user$screenshot_depth", width, height, ERHI_FORMAT::R24G8_TYPELESS);

	// Bind the offscreen targets (unbind the g-buffer MRTs to avoid stale writes)
	RCache.set_RT(rtColor->pRT);
	RCache.set_RT(nullptr, 1);
	RCache.set_RT(nullptr, 2);
	RCache.set_RT(nullptr, 3);
	GRHI->SetDepthStencilView(rtDepth->pZRT);
	GRHI->ApplyRenderTargetChange();

	// Clear with the editor scene clear color
	Clear();

	// Match the normal scene render setup
	RHIViewport VP = { 0, 0, (float)width, (float)height, 0.f, 1.f };
	GRHI->SetViewport(VP);
	RCache.set_Stencil(true, D3DCMP_ALWAYS, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
	ResetMaterial();

	// Render the scene with the currently configured view/projection
	Tools->Render();

	GRHI->ApplyRenderTargetChange();

	// Read back the color target via a staging texture
	bool bResult = false;
	ID3D11Device* pDevice = REDevice;
	ID3D11DeviceContext* pContext = REContext;

	if (pDevice && pContext && rtColor->pSurface)
	{
		ID3D11Resource* pSrc = (ID3D11Resource*)rtColor->pSurface->GetRawTexture();
		if (pSrc)
		{
			D3D11_TEXTURE2D_DESC desc;
			ZeroMemory(&desc, sizeof(desc));
			desc.Width = width;
			desc.Height = height;
			desc.MipLevels = 1;
			desc.ArraySize = 1;
			desc.Format = DXGI_FORMAT_B8G8R8A8_UNORM;
			desc.SampleDesc.Count = 1;
			desc.SampleDesc.Quality = 0;
			desc.Usage = D3D11_USAGE_STAGING;
			desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
			desc.BindFlags = 0;
			desc.MiscFlags = 0;

			ID3D11Texture2D* pStaging = nullptr;
			HRESULT hr = pDevice->CreateTexture2D(&desc, nullptr, &pStaging);
			if (SUCCEEDED(hr) && pStaging)
			{
				pContext->CopyResource(pStaging, pSrc);

				D3D11_MAPPED_SUBRESOURCE mapped;
				hr = pContext->Map(pStaging, 0, D3D11_MAP_READ, 0, &mapped);
				if (SUCCEEDED(hr))
				{
					pixels.resize(width * height);
					const u8* pSrcBits = (const u8*)mapped.pData;
					u32* pDst = pixels.data();
					for (u32 y = 0; y < height; ++y)
					{
						CopyMemory(pDst + y * width, pSrcBits + y * mapped.RowPitch, width * sizeof(u32));
					}
					pContext->Unmap(pStaging, 0);
					bResult = true;
				}
				pStaging->Release();
			}
		}
	}

	// Restore previous render targets / depth stencil
	for (u32 i = 0; i < 4; ++i)
		RCache.set_RT(pSavedRT[i], i);
	GRHI->SetDepthStencilView(pSavedDSV);
	GRHI->ApplyRenderTargetChange();

	return bResult;
}
