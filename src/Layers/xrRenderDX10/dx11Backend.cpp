#include "stdafx.h"
#include "dx11Backend.h"
#include "dx10BufferUtils.h"

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

ITexture2D* CBackend_DX11::CreateTexture2D(const TextureDesc* pDesc, byte* data, u32 length)
{
	auto texture = std::make_shared<Texture_DX11>();
	texture->pTex1D = nullptr;
	texture->pTex2D = nullptr;
	texture->pTex3D = nullptr;
	texture->pSRV = nullptr;

	D3D11_TEXTURE2D_DESC d3dTextureDesc;
	memset(&d3dTextureDesc, 0, sizeof(d3dTextureDesc));
	d3dTextureDesc.Width = pDesc->width;
	d3dTextureDesc.Height = pDesc->height;
	d3dTextureDesc.MipLevels = (pDesc->mipmapLevel < 1 ? 1 : pDesc->mipmapLevel);
	d3dTextureDesc.ArraySize = 1;
	d3dTextureDesc.Format = GetDXGIFormat(pDesc->format);
	d3dTextureDesc.SampleDesc.Count = 1;
	d3dTextureDesc.Usage = D3D11_USAGE_DEFAULT;

	// #TODO: HACK
	if (pDesc->format == FMT_DEPTH32F ||
		pDesc->format == FMT_DEPTH24_STENCIL_8)
		d3dTextureDesc.BindFlags = 0;
	else
		d3dTextureDesc.BindFlags = D3D11_BIND_SHADER_RESOURCE;

	if (pDesc->renderTargetUsage)
		d3dTextureDesc.BindFlags |= D3D11_BIND_RENDER_TARGET;

	d3dTextureDesc.CPUAccessFlags = 0;
	d3dTextureDesc.MiscFlags = 0;

	D3D11_SUBRESOURCE_DATA subresourceData = {};
	subresourceData.pSysMem = data;
	subresourceData.SysMemPitch = length;

	R_CHK(RDevice->CreateTexture2D(&d3dTextureDesc, data ? &subresourceData : NULL, &texture->pTex2D));

	if (d3dTextureDesc.BindFlags == D3D11_BIND_SHADER_RESOURCE)
		R_CHK(RDevice->CreateShaderResourceView(texture->pTex2D, NULL, &texture->pSRV));

	ITexture2D* pTexture = xr_new<ITexture2D>();
	pTexture->m_InternalResource = texture;
	return pTexture;
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

	stats.calls++;
	stats.verts += countV;
	stats.polys += PC;
	
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
	constants.flush();

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

	stats.calls++;
	stats.verts += 3 * PC;
	stats.polys += PC;

	ApplyPrimitieTopology(Topology);
	SRVSManager.Apply();
	//ApplyRTandZB();
	//ApplyVertexLayout();
	StateManager.Apply();
	//	State manager may alter constants
	constants.flush();
	
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
