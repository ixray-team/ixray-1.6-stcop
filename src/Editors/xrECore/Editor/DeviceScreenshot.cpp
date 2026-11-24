//---------------------------------------------------------------------------
#include "stdafx.h"

#include "UI_ToolsCustom.h"
#include "EditorSceneSubmit.h"
#include "device.h"
#include "ui_main.h"
#include "../../../Layers/xrRenderIXR/ResourceManager.h"
#include "../../../Layers/xrRenderIXR/Shader.h"
#include "../../../Layers/xrRenderIXR/SH_RT.h"
#include "../../../Layers/xrRenderIXR/dxRenderDeviceRender.h"
#include "../../../Layers/xrRenderIXR/RenderScope.h"

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
	RenderScope Scope(Cmd, rtColor->pRT, rtDepth->pZRT, TexWidth, TexHeight);

	Clear();

	Cmd.SetStencil(true, D3DCMP_ALWAYS, 0x01, 0xff, 0xff, D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_KEEP);
	ResetMaterial();

	Tools->Render();

	Cmd.ApplyRenderTargets();
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

	GfxCmdList& Cmd = RImplementation.Imm();
	RenderScope Scope(Cmd);

	mView = View;
	mProject = Project;
	mFullTransform.mul(mProject, mView);

	Fmatrix Camera;
	Camera.invert(mView);
	vCameraPosition.set(Camera.c);
	vCameraDirection.set(Camera.k);
	vCameraTop.set(Camera.j);
	vCameraRight.set(Camera.i);

	Cmd.SetView(mView, mProject);
	::Render->ViewBase.CreateFromMatrix(mFullTransform, FRUSTUM_P_ALL);

	// No history: the temporal passes see a still camera
	mView_old = mView;
	mProject_old = mProject;
	mFullTransform_old = mFullTransform;
	Cmd.SetView({ mView, mProject, mView, mProject });
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
	Cmd.SetView(mView, mProject);
	::Render->ViewBase.CreateFromMatrix(mFullTransform, FRUSTUM_P_ALL);

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

	NriBinding Reads[64];

	if (SrcRTs.empty() || SrcRTs.size() > std::size(Reads) || !AtlasRT || !AtlasRT->pUAView)
	{
		return false;
	}

	ref_cs Compute = DEV->_CreateCS("lod_downsample");
	if (!Compute)
	{
		return false;
	}

	for (u32 Idx = 0; Idx < SrcRTs.size(); ++Idx)
	{
		Reads[Idx] = { Idx, SrcRTs[Idx]->pSurface };
	}

	const NriBinding Written = { 0, AtlasRT->pSurface };
	RImplementation.NriPasses.Dispatch(RImplementation.Imm(), Compute._get(), Reads, u32(SrcRTs.size()), &Written, 1, ((TargetW * Samples) + 7) / 8, (TargetH + 7) / 8, 1);
	return true;
}