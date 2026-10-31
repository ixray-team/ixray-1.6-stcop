//---------------------------------------------------------------------------
#include "stdafx.h"

#include "UI_ToolsCustom.h"
#include "device.h"
#include "ui_main.h"
#include "../../../Layers/xrRenderPC_R4/ResourceManager.h"
#include "../../../Layers/xrRenderPC_R4/Shader.h"
#include "../../../Layers/xrRenderPC_R4/SH_RT.h"
#include "../../../Layers/xrRenderPC_R4/dxRenderDeviceRender.h"

#include <d3d11.h>

struct SRTState
{
	IRHIRenderTargetView* RT[4];
	IRHIDepthStencilView* DSV;

	void Save()
	{
		RT[0] = RImplementation.Imm().GetRenderTarget(0);
		RT[1] = RImplementation.Imm().GetRenderTarget(1);
		RT[2] = RImplementation.Imm().GetRenderTarget(2);
		RT[3] = RImplementation.Imm().GetRenderTarget(3);
		DSV = RImplementation.Imm().GetDepthStencil();
	}

	void Restore(GfxCmdList& Cmd)
	{
		for (u32 Idx = 0; Idx < 4; ++Idx)
		{
			Cmd.SetRenderTarget(RT[Idx], Idx);
		}
		Cmd.SetDepthStencil(DSV);
		Cmd.ApplyRenderTargets();
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
	DEV->Evict();

	GfxCmdList& Cmd = RImplementation.Imm();
	SRTState Saved;
	Saved.Save();

	Cmd.SetRenderTarget(rtColor->pRT);
	Cmd.SetRenderTarget(nullptr, 1);
	Cmd.SetRenderTarget(nullptr, 2);
	Cmd.SetRenderTarget(nullptr, 3);
	Cmd.SetDepthStencil(rtDepth->pZRT);
	Cmd.ApplyRenderTargets();

	Clear();

	RHIViewport VP = {0, 0, (float)TexWidth, (float)TexHeight, 0.f, 1.f};
	Cmd.SetViewport(VP);
	Cmd.SetStencil(true, D3DCMP_ALWAYS, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
	ResetMaterial();

	Tools->Render();

	Cmd.ApplyRenderTargets();

	Saved.Restore(Cmd);
	return true;
}

bool CEditorRenderDevice::ReadbackRT(ref_rt& RenderTarget, U32Vec& Pixels)
{
    if (!RenderTarget || !RenderTarget->pRT)
    {
        return false;
    }
    u32 width = RenderTarget->dwWidth;
    u32 height = RenderTarget->dwHeight;
    if (!width || !height || u64(width) * height * sizeof(u32) > UINT32_MAX)
    {
        return false;
    }
    Pixels.resize(size_t(width) * height);
    u32 rowPitch = 0;
    if (GRHI->DevicePtr->ReadRenderTargetPixels(RenderTarget->pRT, Pixels.data(),
        u32(Pixels.size() * sizeof(u32)), width, height, rowPitch))
    {
        return rowPitch == width * sizeof(u32);
    }
    const u64 size = u64(rowPitch) * height;
    if (rowPitch < width * sizeof(u32) || size > UINT32_MAX || size <= Pixels.size() * sizeof(u32))
    {
        return false;
    }
    xr_vector<u8> readback(size_t(size), 0);
    if (!GRHI->DevicePtr->ReadRenderTargetPixels(RenderTarget->pRT, readback.data(), u32(size), width, height, rowPitch))
    {
        return false;
    }
    for (u32 row_idx = 0; row_idx < height; ++row_idx)
    {
        memcpy(Pixels.data() + size_t(row_idx) * width, readback.data() + size_t(row_idx) * rowPitch, width * sizeof(u32));
    }
    return true;
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
	Compute = DEV->_CreateCS("lod_downsample");
	if (!Compute)
	{
		return false;
	}

	GfxCmdList& Cmd = RImplementation.Imm();
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
		Cmd.SetResource(ERHI_SHADER_TYPE::CS, Idx, Srvs[Idx]);
	}

	IRHIUnorderedAccessView* Uav = AtlasRT->pUAView;
	if (!Uav)
	{
		for (auto Srv : Srvs)
		{
			Srv->Release();
		}
		return false;
	}

	UINT UavInit = 0;
	Cmd.SetComputeUAVs(0, 1, &Uav, &UavInit);

	Cmd.SetCS(Compute);
	Cmd.Dispatch(((TargetW * Samples) + 7) / 8, (TargetH + 7) / 8, 1);

	for (u32 Idx = 0; Idx < Srvs.size(); ++Idx)
	{
		Cmd.SetResource(ERHI_SHADER_TYPE::CS, Idx, nullptr);
		Srvs[Idx]->Release();
	}
	IRHIUnorderedAccessView* NullUAV = nullptr;
	Cmd.SetComputeUAVs(0, 1, &NullUAV, &UavInit);
	Cmd.SetShader(nullptr, ERHI_SHADER_TYPE::CS);

	return true;
}