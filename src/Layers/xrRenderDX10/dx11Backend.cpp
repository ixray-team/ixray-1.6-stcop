#include "stdafx.h"
#include "dx11Backend.h"
#include "dx10BufferUtils.h"

struct Buffer_DX11
{
	ID3D11Buffer* pBuffer;
};

CBackend_DX11 backend_dx11_impl;

CBackend_DX11::CBackend_DX11()
{
}

CBackend_DX11::~CBackend_DX11()
{
}

IVertexBuffer* CBackend_DX11::CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX11>();
	buffer->pBuffer = nullptr;

	R_CHK(dx10BufferUtils::CreateVertexBuffer(&buffer->pBuffer, data, length, usage == ResourceUsage::IMMUTABLE));

	IVertexBuffer* pBuffer = xr_new<IVertexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

IIndexBuffer* CBackend_DX11::CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX11>();
	buffer->pBuffer = nullptr;

	R_CHK(dx10BufferUtils::CreateIndexBuffer(&buffer->pBuffer, data, length, usage == ResourceUsage::IMMUTABLE));

	IIndexBuffer* pBuffer = xr_new<IIndexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

void CBackend_DX11::set_Vertices(IVertexBuffer* _vb, u32 _vb_stride)
{
	if ((vb != _vb) || (vb_stride != _vb_stride))
	{
		vb = _vb;
		vb_stride = _vb_stride;

		Buffer_DX11* pBuffer = static_cast<Buffer_DX11*>(_vb->m_InternalResource.get());

		u32	iOffset = 0;
		RContext->IASetVertexBuffers(0, 1, &pBuffer->pBuffer, &_vb_stride, &iOffset);
	}
}

void CBackend_DX11::set_Indices(IIndexBuffer* _ib)
{
	if (ib != _ib)
	{
		ib = _ib;

		Buffer_DX11* pBuffer = static_cast<Buffer_DX11*>(_ib->m_InternalResource.get());
		RContext->IASetIndexBuffer(pBuffer->pBuffer, DXGI_FORMAT_R16_UINT, 0);
	}
}

void CBackend_DX11::set_Scissor(Irect* rect)
{
	if (rect)
	{
		StateManager.EnableScissoring();
		RECT* clip = (RECT*)rect;
		RContext->RSSetScissorRects(1, clip);
	}
	else
	{
		StateManager.EnableScissoring(FALSE);
		RContext->RSSetScissorRects(0, 0);
	}
}

void CBackend_DX11::Render(PRIMITIVETYPE T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	//VERIFY(vs);
//RDevice->VSSetShader(vs);
//RDevice->GSSetShader(0);

	D3D_PRIMITIVE_TOPOLOGY Topology = TranslateTopology((D3DPRIMITIVETYPE)T);
	u32	iIndexCount = GetIndexCount((D3DPRIMITIVETYPE)T, PC);

	//!!! HACK !!!
	//if (hs != 0 || ds != 0)
	//{
	//	R_ASSERT(Topology == D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
	//	Topology = D3D11_PRIMITIVE_TOPOLOGY_3_CONTROL_POINT_PATCHLIST;
	//}

	//stat.calls++;
	//stat.verts += countV;
	//stat.polys += PC;
	
	ApplyPrimitieTopology(Topology);

	//CHK_DX(RDevice->DrawIndexedPrimitive(T,baseV, startV, countV,startI,PC));
	//D3DPRIMITIVETYPE Type,
	//INT BaseVertexIndex,
	//UINT MinIndex,
	//UINT NumVertices,
	//UINT StartIndex,
	//UINT PriResmitiveCount

	//UINT IndexCount,
	//UINT StartIndexLocation,
	//INT BaseVertexLocation
	SRVSManager.Apply();
	//ApplyRTandZB();
	//ApplyVertexLayout();
	StateManager.Apply();
	//	State manager may alter constants
//	constants.flush();
	//	Msg("DrawIndexed: Start");
	//	Msg("iIndexCount=%d, startI=%d, baseV=%d", iIndexCount, startI, baseV);
	RContext->DrawIndexed(iIndexCount, startI, baseV);
	//	Msg("DrawIndexed: End\n");

	PGO(Msg("PGO:DIP:%dv/%df", countV, PC));
}

void CBackend_DX11::Render(PRIMITIVETYPE T, u32 startV, u32 PC)
{
	//	TODO: DX10: Remove triangle fan usage from the engine
	if (T == D3DPT_TRIANGLEFAN)
		return;

	//VERIFY(vs);
	//RDevice->VSSetShader(vs);

	D3D_PRIMITIVE_TOPOLOGY Topology = TranslateTopology((D3DPRIMITIVETYPE)T);
	u32	iVertexCount = GetIndexCount((D3DPRIMITIVETYPE)T, PC);

	//stat.calls++;
	//stat.verts += 3 * PC;
	//stat.polys += PC;

	ApplyPrimitieTopology(Topology);
	SRVSManager.Apply();
	//ApplyRTandZB();
	//ApplyVertexLayout();
	StateManager.Apply();
	//	State manager may alter constants
	//constants.flush();
	//	Msg("Draw: Start");
	//	Msg("iVertexCount=%d, startV=%d", iVertexCount, startV);
		//CHK_DX				(RDevice->DrawPrimitive(T, startV, PC));
	RContext->Draw(iVertexCount, startV);
	//	Msg("Draw: End\n");
	PGO(Msg("PGO:DIP:%dv/%df", 3 * PC, PC));
}

void CBackend_DX11::ApplyPrimitieTopology(D3D_PRIMITIVE_TOPOLOGY Topology)
{
	if (m_PrimitiveTopology != Topology)
	{
		m_PrimitiveTopology = Topology;
		RContext->IASetPrimitiveTopology(m_PrimitiveTopology);
	}
}
