#include "stdafx.h"
#include "R_Backend.h"
#include "dxRenderDeviceRender.h"

// Create Quad-IB
void GfxCmdList::CreateQuadIB()
{
	const u32 dwTriCount = 4 * (4096 * 8);
	const u32 dwIdxCount = dwTriCount * 2 * 3;
	static u16 IndexBufferStatic[dwIdxCount];
	u16* Indices = IndexBufferStatic;

	int Cnt = 0;
	int ICnt = 0;
	for (int i = 0; i < dwTriCount; ++i)
	{
		Indices[ICnt++] = u16(Cnt + 0);
		Indices[ICnt++] = u16(Cnt + 1);
		Indices[ICnt++] = u16(Cnt + 2);

		Indices[ICnt++] = u16(Cnt + 3);
		Indices[ICnt++] = u16(Cnt + 2);
		Indices[ICnt++] = u16(Cnt + 1);

		Cnt += 4;
	}

	// Create immutable index buffer with initial data
	R_ASSERT(RHIUtils::CreateIndexBuffer(&QuadIB, IndexBufferStatic, dwIdxCount * 2));
}

// Device dependance
void GfxCmdList::OnDeviceCreate	()
{
	EditorMode			= Device.IsEditorMode();
	CreateQuadIB		();

	xr_vector<u32> Iota(65536);
	std::iota(Iota.begin(), Iota.end(), 0u);
	RHIUtils::CreateVertexBuffer(&IotaVB, Iota.data(), u32(Iota.size() * sizeof(u32)));
	const RHIInputElementDesc DrawId = { "DRAWID", 0, ERHI_FORMAT::R32_UINT, 0, 0, ERHI_INPUT_CLASSIFICATION::INSTANCE_DATA, 1 };
	PulledDecl = DEV->_CreateDecl(&DrawId, 1);
	Heap = &RImplementation.Geometry;

	// streams
	Vertex.Create		();
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

	// Quad
	_RELEASE							(QuadIB);

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