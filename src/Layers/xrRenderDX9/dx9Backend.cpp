#include "stdafx.h"
#include "dx9Backend.h"

CBackend_DX9 backend_dx9_impl;

CBackend_DX9::CBackend_DX9() :
	CBackendBase()
{
}

CBackend_DX9::~CBackend_DX9()
{
}

IVertexBuffer* CBackend_DX9::CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX9>();
	buffer->pVB = nullptr;
	buffer->pIB = nullptr;

	DWORD dwUsage = 0;
	D3DPOOL dwPool = D3DPOOL_DEFAULT;

	if (usage == ResourceUsage::IMMUTABLE)
		dwPool = D3DPOOL_MANAGED;
	else if (usage == ResourceUsage::DYNAMIC)
		dwUsage = D3DUSAGE_DYNAMIC;
	else
		FATAL("!!!");

	R_CHK(RDevice->CreateVertexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, 0, dwPool, &buffer->pVB, NULL));

	if (data)
	{
		BYTE* bytes = 0;
		R_CHK(buffer->pVB->Lock(0, 0, (void**)&bytes, 0));
		memcpy(bytes, data, length);
		buffer->pVB->Unlock();
	}

	IVertexBuffer* pBuffer = xr_new<IVertexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

IIndexBuffer* CBackend_DX9::CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage)
{
	auto buffer = std::make_shared<Buffer_DX9>();
	buffer->pVB = nullptr;
	buffer->pIB = nullptr;

	DWORD dwUsage = 0;
	D3DPOOL dwPool = D3DPOOL_DEFAULT;

	if (usage == ResourceUsage::IMMUTABLE)
		dwPool = D3DPOOL_MANAGED;
	else if (usage == ResourceUsage::DYNAMIC)
		dwUsage = D3DUSAGE_DYNAMIC;
	else
		FATAL("!!!");

	R_CHK(RDevice->CreateIndexBuffer(length, dwUsage | D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, dwPool, &buffer->pIB, NULL));
	if (data)
	{
		BYTE* bytes = 0;
		R_CHK(buffer->pIB->Lock(0, 0, (void**)&bytes, 0));
		memcpy(bytes, data, length);
		buffer->pIB->Unlock();
	}

	IIndexBuffer* pBuffer = xr_new<IIndexBuffer>();
	pBuffer->m_InternalResource = buffer;
	return pBuffer;
}

ITexture2D* CBackend_DX9::CreateTexture2D(const TextureDesc* pDesc, byte* data, u32 length)
{
	auto texture = std::make_shared<Texture_DX9>();
	texture->pTex = nullptr;

	R_CHK(RDevice->CreateTexture(pDesc->width, pDesc->height, 
		pDesc->mipmapLevel, 0, GetD3DFormat(pDesc->format), 
		D3DPOOL_DEFAULT, &texture->pTex, NULL));

	ITexture2D* pTexture = xr_new<ITexture2D>();
	pTexture->m_InternalResource = texture;
	return pTexture;
}

CTexture* CBackend_DX9::get_ActiveTexture(u32 stage)
{
	if		(stage < CTexture::rstVertex)		return textures_ps[stage];
	else if (stage < CTexture::rstGeometry)		return textures_vs[stage - CTexture::rstVertex];
	VERIFY(!"Invalid texture stage");
	return 0;
}

void CBackend_DX9::set_Vertices(IVertexBuffer* _vb, u32 _vb_stride)
{
	if ((vb != _vb) || (vb_stride != _vb_stride))
	{
		PGO(Msg("PGO:VB:%x,%d", _vb, _vb_stride));
#ifdef DEBUG
		stats.vb++;
#endif
		vb = _vb;
		vb_stride = _vb_stride;

		Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(_vb->m_InternalResource.get());
		R_CHK(RDevice->SetStreamSource(0, pBuffer->pVB, 0, _vb_stride));
	}
}

void CBackend_DX9::set_Indices(IIndexBuffer* _ib)
{
	if (ib != _ib)
	{
		PGO(Msg("PGO:IB:%x", _ib));
#ifdef DEBUG
		stats.ib++;
#endif
		ib = _ib;

		Buffer_DX9* pBuffer = static_cast<Buffer_DX9*>(_ib->m_InternalResource.get());
		R_CHK(RDevice->SetIndices(pBuffer->pIB));
	}
}

void CBackend_DX9::set_Scissor(Irect* rect)
{
	if (rect)
	{
		CHK_DX(RDevice->SetRenderState(D3DRS_SCISSORTESTENABLE, TRUE));
		RECT* clip = (RECT*)rect;
		CHK_DX(RDevice->SetScissorRect(clip));
	}
	else
	{
		CHK_DX(RDevice->SetRenderState(D3DRS_SCISSORTESTENABLE, FALSE));
	}
}

void CBackend_DX9::Render(PRIMITIVETYPE T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	//Fix D3D ERROR
	if (PC == 0)
		return;

	stats.calls++;
	stats.verts += countV;
	stats.polys += PC;
	constants.flush();
	CHK_DX(RDevice->DrawIndexedPrimitive(GetD3DPrimitiveType(T), baseV, startV, countV, startI, PC));
	PGO(Msg("PGO:DIP:%dv/%df", countV, PC));
}

void CBackend_DX9::Render(PRIMITIVETYPE T, u32 startV, u32 PC)
{
	//Fix D3D ERROR
	if (PC == 0)
		return;

	stats.calls++;
	stats.verts += 3 * PC;
	stats.polys += PC;
	constants.flush();
	CHK_DX(RDevice->DrawPrimitive(GetD3DPrimitiveType(T), startV, PC));
	PGO(Msg("PGO:DIP:%dv/%df", 3 * PC, PC));
}
