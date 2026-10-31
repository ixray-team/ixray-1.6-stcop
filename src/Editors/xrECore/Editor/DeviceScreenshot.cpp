//---------------------------------------------------------------------------
#include "stdafx.h"

#include "UI_ToolsCustom.h"
#include "EditorSceneSubmit.h"
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
	DEV->Evict();

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

bool CEditorRenderDevice::RenderSceneFrame(const Fmatrix& View, const Fmatrix& Project, U32Vec& Pixels, u32& Width, u32& Height)
{
	ref_rt& Target = EContext.UI->RT;
	if (!b_is_Ready || !Target || Target->pRT == nullptr)
	{
		return false;
	}

	const Fmatrix SavedView = mView;
	const Fmatrix SavedProject = mProject;
	const Fmatrix SavedFull = mFullTransform;
	const Fvector SavedPosition = vCameraPosition;
	const Fvector SavedDirection = vCameraDirection;
	const Fvector SavedTop = vCameraTop;
	const Fvector SavedRight = vCameraRight;

	SRTState Saved;
	Saved.Save();

	mView = View;
	mProject = Project;
	mFullTransform.mul(mProject, mView);

	Fmatrix Camera;
	Camera.invert(mView);
	vCameraPosition.set(Camera.c);
	vCameraDirection.set(Camera.k);
	vCameraTop.set(Camera.j);
	vCameraRight.set(Camera.i);

	RCache.set_xform_view(mView);
	RCache.set_xform_project(mProject);
	::Render->ViewBase.CreateFromMatrix(mFullTransform, FRUSTUM_P_ALL);

	// No history: the temporal passes see a still camera
	mView_old = mView;
	mProject_old = mProject;
	mFullTransform_old = mFullTransform;
	RCache.set_xform_view_old(mView);
	RCache.set_xform_project_old(mProject);
	GEditorDraw.UpscalerReset = true;

	++dwRenderFrame;
	ResetMaterial();

	GEditorSceneSubmit.BeginFrame();
	RImplementation.EditorClearColor.set(EPrefs->scene_clear_color);
	RImplementation.EditorOcclusionCulling = false;
	::Render->Calculate();
	::Render->Render();
	GEditorDraw.UpscalerReset = false;

	// Raw colors: the user gamma, brightness, contrast and color balance stay out of the capture
	CGammaControl& Gamma = dxRenderDeviceRender::Instance().m_Gamma;
	const CGammaControl SavedGamma = Gamma;
	Gamma = CGammaControl();
	RImplementation.EditorPresent(Target->pRT);
	Gamma = SavedGamma;

	const bool IsValid = ReadbackRT(Target, Pixels);
	Width = Target->dwWidth;
	Height = Target->dwHeight;

	mView = SavedView;
	mProject = SavedProject;
	mFullTransform = SavedFull;
	vCameraPosition = SavedPosition;
	vCameraDirection = SavedDirection;
	vCameraTop = SavedTop;
	vCameraRight = SavedRight;
	RCache.set_xform_view(mView);
	RCache.set_xform_project(mProject);
	::Render->ViewBase.CreateFromMatrix(mFullTransform, FRUSTUM_P_ALL);

	Saved.Restore();
	EContext.UI->RedrawScene();
	return IsValid;
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
		RCache.set_CSResource(Idx, Srvs[Idx]);
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
	GRHI->SetComputeUAVs(0, 1, &Uav, &UavInit);

	RCache.set_CS(Compute);
	RCache.Compute(((TargetW * Samples) + 7) / 8, (TargetH + 7) / 8, 1);

	for (u32 Idx = 0; Idx < Srvs.size(); ++Idx)
	{
		RCache.set_CSResource(Idx, nullptr);
		Srvs[Idx]->Release();
	}
	IRHIUnorderedAccessView* NullUAV = nullptr;
	GRHI->SetComputeUAVs(0, 1, &NullUAV, &UavInit);
	GRHI->SetShader(nullptr, ERHI_SHADER_TYPE::CS);

	return true;
}