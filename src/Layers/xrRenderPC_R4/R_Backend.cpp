#include "stdafx.h"
#include "R_Backend.h"
#include "dxRenderDeviceRender.h"

// Device dependance
void GfxCmdList::OnDeviceCreate	()
{
	EditorMode			= Device.IsEditorMode();

	xr_vector<u32> Iota(IotaSize);
	std::iota(Iota.begin(), Iota.end(), 0u);
	RHIUtils::CreateVertexBuffer(&IotaVB, Iota.data(), u32(Iota.size() * sizeof(u32)));
	Heap = &RImplementation.Geometry;

	// streams
	Vertex.Create		(*this);
	Index.Create		();
	Constants.Create();
	SetView(ViewState);
	// invalidate caching
	Invalidate			();
}

void GfxCmdList::OnDeviceDestroy()
{
	// streams
	Index.Destroy		();
	Vertex.Destroy		();
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