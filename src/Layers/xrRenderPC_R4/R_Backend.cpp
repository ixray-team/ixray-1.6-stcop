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
	const RHIInputElementDesc DrawId = { "DRAWID", 0, ERHI_FORMAT::R32_UINT, 0, 0, ERHI_INPUT_CLASSIFICATION::INSTANCE_DATA, 1 };
	PulledDecl = DEV->_CreateDecl(&DrawId, 1);
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
	PulledDecl = nullptr;
	Heap->Destroy();
	Heap = nullptr;
	if (GRHI->ShaderResourceCache)
	{
		GRHI->ShaderResourceCache->ResetDeviceState();
	}
}

void GfxCmdList::CreatePulledLayout()
{
	ref_vs Reference = DEV->_CreateVS("pull_reference");
	CHECK_OR_EXIT(Reference->signature, "pull_reference.vs.hlsl has no input signature");
	RHIBlob* Signature = Reference->signature->signature;
	CHK_DX(GRHI->CreateInputLayout(PulledDecl->dx10_dcl_code.data(), PulledDecl->dx10_dcl_code.size(), Signature->GetBufferPointer(), Signature->GetBufferSize(), &PulledLayout));
}

void GfxCmdList::SetVertices(IRHIBuffer* _vb, u32 _vb_stride)
{
	if ((vb != _vb) || (vb_stride != _vb_stride))
	{
		vb = _vb;
		vb_stride = _vb_stride;

		if (vb)
		{
			vb->SetVertexBuffer(0, _vb_stride, 0);
		}
		else
		{
			GRHI->ClearVertexBuffer(_vb_stride);
		}
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