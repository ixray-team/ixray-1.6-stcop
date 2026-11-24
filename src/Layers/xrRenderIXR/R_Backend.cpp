#include "stdafx.h"
#include "R_Backend.h"
#include "dxRenderDeviceRender.h"
#include "NriPass.h"

// Device dependance
void GfxCmdList::OnDeviceCreate	()
{
	EditorMode			= Device.IsEditorMode();

	xr_vector<u32> Iota(IotaSize);
	std::iota(Iota.begin(), Iota.end(), 0u);
	RHIUtils::CreateVertexBuffer(&IotaVB, Iota.data(), u32(Iota.size() * sizeof(u32)));
	Heap = &RImplementation.Geometry;
	Constants.Create();
	SetView(ViewState);
	// invalidate caching
	Invalidate			();
}

void GfxCmdList::OnDeviceDestroy()
{
	RImplementation.Skinning.Destroy();

	_RELEASE(IotaVB);
	_RELEASE(PulledLayout);
	Heap->Destroy();
	Heap = nullptr;
	if (GRHI->ShaderResourceCache)
	{
		GRHI->ShaderResourceCache->ResetDeviceState();
	}
}

void GfxCmdList::CreatePulledLayout()
{
	static constexpr RHIInputElementDesc DrawId = { "DRAWID", 0, ERHI_FORMAT::R32_UINT, 0, 0, ERHI_INPUT_CLASSIFICATION::INSTANCE_DATA, 1 };
	ref_vs Reference = DEV->_CreateVS("pull_reference");
	RHIBlob* Signature = Reference->Signature;
	CHECK_OR_EXIT(Signature, "pull_reference.vs.hlsl has no input signature");
	CHK_DX(GRHI->CreateInputLayout(&DrawId, 1, Signature->GetBufferPointer(), Signature->GetBufferSize(), &PulledLayout));
}

void GfxCmdList::RestoreInput()
{
	if (ib)
	{
		ib->SetIndexBuffer(false, 0);
	}

	m_pInputLayout = nullptr;
}

void GfxCmdList::RestoreAfterRun()
{
	for (u32 Stage = 0; Stage < std::size(Shaders); ++Stage)
	{
		GRHI->SetShader(Shaders[Stage], ERHI_SHADER_TYPE(Stage));
	}

	if (LastViewport.Width > 0.0f)
	{
		GRHI->SetViewport(LastViewport);
	}

	GRHI->SetScissorRect(&LastScissor);
}

void GfxCmdList::UnbindShaders(bool Forget)
{
	for (u32 Stage = 0; Stage < std::size(Shaders); ++Stage)
	{
		GRHI->SetShader(nullptr, ERHI_SHADER_TYPE(Stage));
		Shaders[Stage] = Forget ? nullptr : Shaders[Stage];
	}
}

void GfxCmdList::SetIndices(IRHIBuffer* _ib)
{
	if (ib != _ib)
	{
		ib = _ib;

		if (ib)
		{
			ib->SetIndexBuffer(false, 0);
		}
		else
		{
			GRHI->ClearIndexBuffer();
		}
	}
}

// Through NRI; the input the old backend's draws bind goes again afterwards
void GfxCmdList::ClearTarget(IRHIRenderTargetView* Target, ERTColor Color)
{
	static const float Colors[][4] = { { 0.0f, 0.0f, 0.0f, 0.0f }, { 0.5f, 0.5f, 0.5f, 0.5f }, { 1.0f, 1.0f, 1.0f, 1.0f } };
	ClearTarget(Target, Colors[u32(Color)]);
}

void GfxCmdList::ClearTarget(IRHIRenderTargetView* Target, const float* Color)
{
	FlushRun();

	if (Target)
	{
		ClearNri(Target, Color);
	}

	RestoreInput();
}

void GfxCmdList::ClearDepthStencil(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET Flags, float Depth, u8 Stencil)
{
	FlushRun();

	if (View)
	{
		ClearNri(View, Flags, Depth, Stencil);
	}

	RestoreInput();
}

void GfxCmdList::CopySurface(IRHISurface* Dest, IRHISurface* Source)
{
	FlushRun();
	CopyNri(Dest, Source);
	RestoreInput();
}
