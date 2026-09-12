//---------------------------------------------------------------------------
#include "stdafx.h"

#include "UI_ToolsCustom.h"
#include "device.h"
#include "ui_main.h"
#include "../../../Layers/xrRender/ResourceManager.h"
#include "../../../Layers/xrRender/Shader.h"
#include "../../../Layers/xrRender/SH_RT.h"
#include "../../../Layers/xrRender/dxRenderDeviceRender.h"

#include <d3d11.h>

struct SRTState
{
	IRHIRenderTargetView* RT[4];
	IRHIDepthStencilView* DSV;

	void Save()
	{
		RT[0] = GRHI->GetRenderTargetView(0);
		RT[1] = GRHI->GetRenderTargetView(1);
		RT[2] = GRHI->GetRenderTargetView(2);
		RT[3] = GRHI->GetRenderTargetView(3);
		DSV = GRHI->GetDepthStencilView();
	}

	void Restore()
	{
		for (u32 Idx = 0; Idx < 4; ++Idx)
		{
			RCache.set_RT(RT[Idx], Idx);
		}
		GRHI->SetDepthStencilView(DSV);
		GRHI->ApplyRenderTargetChange();
	}
};

bool CEditorRenderDevice::RenderScreenshotRT(ref_rt& rtColor, ref_rt& rtDepth)
{
	if (!b_is_Ready)
	{
		return false;
	}
	if (!rtColor || !rtDepth)
	{
		return false;
	}

	u32 TexWidth = rtColor->dwWidth;
	u32 TexHeight = rtColor->dwHeight;

	// free managed resource
	Resources->Evict();

	SRTState Saved;
	Saved.Save();

	RCache.set_RT(rtColor->pRT);
	RCache.set_RT(nullptr, 1);
	RCache.set_RT(nullptr, 2);
	RCache.set_RT(nullptr, 3);
	GRHI->SetDepthStencilView(rtDepth->pZRT);
	GRHI->ApplyRenderTargetChange();

	Clear();

	RHIViewport VP = {0, 0, (float)TexWidth, (float)TexHeight, 0.f, 1.f};
	GRHI->SetViewport(VP);
	RCache.set_Stencil(true, D3DCMP_ALWAYS, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
	ResetMaterial();

	Tools->Render();

	GRHI->ApplyRenderTargetChange();

	Saved.Restore();
	return true;
}

bool CEditorRenderDevice::ReadbackRT(ref_rt& RenderTarget, U32Vec& Pixels)
{
	Pixels.resize(RenderTarget->dwWidth * RenderTarget->dwHeight, 0);

	if (RenderTarget->pSurface == nullptr)
	{
		return false;
	}

	ID3D11Resource* Source = (ID3D11Resource*)RenderTarget->pSurface->GetRawTexture();
	if (!Source)
	{
		return false;
	}

	D3D11_TEXTURE2D_DESC TexDesc = {};
	TexDesc.Width = RenderTarget->dwWidth;
	TexDesc.Height = RenderTarget->dwHeight;
	TexDesc.MipLevels = 1;
	TexDesc.ArraySize = 1;

	TexDesc.Format = (DXGI_FORMAT)RenderTarget->fmt;
	TexDesc.SampleDesc.Count = 1;
	TexDesc.SampleDesc.Quality = 0;
	TexDesc.Usage = D3D11_USAGE_STAGING;
	TexDesc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
	TexDesc.BindFlags = 0;
	TexDesc.MiscFlags = 0;

	ID3D11Texture2D* Staging = nullptr;
	if (FAILED(REDevice->CreateTexture2D(&TexDesc, nullptr, &Staging)))
	{
		return false;
	}

	REContext->CopyResource(Staging, Source);

	D3D11_MAPPED_SUBRESOURCE Mapped;
	bool IsValid = SUCCEEDED(REContext->Map(Staging, 0, D3D11_MAP_READ, 0, &Mapped));
	if (IsValid)
	{
		const u8* SrcBits = (const u8*)Mapped.pData;
		u32* Dest = Pixels.data();
		for (u32 y = 0; y < RenderTarget->dwHeight; ++y)
		{
			CopyMemory(Dest + y * RenderTarget->dwWidth, SrcBits + y * Mapped.RowPitch, RenderTarget->dwWidth * sizeof(u32));
		}
		REContext->Unmap(Staging, 0);
	}
	Staging->Release();
	return IsValid;
}

bool CEditorRenderDevice::MakeScreenshot(U32Vec& pixels, u32 width, u32 height)
{
	if (!b_is_Ready)
	{
		return false;
	}

	if (width == 0 || height == 0)
	{
		return false;
	}

	pixels.resize(width * height, 0);

	ref_rt RtColor;
	ref_rt RtDepth;
	RtColor.create("$user$screenshot_color", width, height, ERHI_FORMAT::B8G8R8A8_UNORM);
	RtDepth.create("$user$screenshot_depth", width, height, ERHI_FORMAT::R24G8_TYPELESS);

	if (!RenderScreenshotRT(RtColor, RtDepth))
	{
		return false;
	}

	return ReadbackRT(RtColor, pixels);
}

bool CEditorRenderDevice::DownsampleLODAtlas(xr_vector<ref_rt>& SrcRTs, ref_rt& AtlasRT, u32 TargetW, u32 TargetH, u32 Samples, u32 Quality)
{
	if (!b_is_Ready)
	{
		return false;
	}

	if (SrcRTs.empty() || !AtlasRT || !AtlasRT->pUAView)
	{
		return false;
	}

	ref_cs Compute;
	Compute = EDevice->Resources->_CreateCS("lod_downsample");
	if (!Compute)
	{
		return false;
	}

	xr_vector<IRHIShaderResourceView*> Srvs;
	Srvs.resize(SrcRTs.size());
	for (u32 Idx = 0; Idx < SrcRTs.size(); ++Idx)
	{
		Srvs[Idx] = GRHI->CreateShaderResourceView(SrcRTs[Idx]->pSurface, nullptr);
		if (!Srvs[Idx])
		{
			for (u32 Idx2 = 0; Idx2 < Idx; ++Idx2)
			{
				Srvs[Idx2]->Release();
			}
			return false;
		}
		GRHI->ShaderResourceCache->SetCSResource(Idx, Srvs[Idx]);
	}

	ID3D11UnorderedAccessView* Uav = (ID3D11UnorderedAccessView*)AtlasRT->pUAView->GetRaw();
	if (!Uav)
	{
		for (auto Srv : Srvs)
		{
			Srv->Release();
		}
		return false;
	}

	UINT UavInit = 0;
	RContext->CSSetUnorderedAccessViews(0, 1, &Uav, &UavInit);

	RCache.set_CS(Compute);
	RCache.Compute(((TargetW * Samples) + 7) / 8, (TargetH + 7) / 8, 1);

	for (u32 Idx = 0; Idx < Srvs.size(); ++Idx)
	{
		GRHI->ShaderResourceCache->SetCSResource(Idx, nullptr);
		Srvs[Idx]->Release();
	}
	ID3D11UnorderedAccessView* NullUAV = nullptr;
	RContext->CSSetUnorderedAccessViews(0, 1, &NullUAV, &UavInit);
	GRHI->SetShader(nullptr, ERHI_SHADER_TYPE::CS);

	return true;
}