#include "stdafx.h"
#include "Dx9API.h"
#include <d3d9.h>

#include "Dx9Buffer.h"

static DWORD GetD3D9BufferLockType(eLockType lockType)
{
	switch (lockType)
	{
	case eLOCK_DISCARD:
		return D3DLOCK_DISCARD;
	case eLOCK_NO_DIRTY_UPDATE:
		return D3DLOCK_NO_DIRTY_UPDATE;
	case eLOCK_NOOVERWRITE:
		return D3DLOCK_NOOVERWRITE;
	case eLOCK_NOSYSLOCK:
		return D3DLOCK_NOSYSLOCK;
	case eLOCK_READONLY:
		return D3DLOCK_READONLY;
	default:
		break;
	}

	return 0;
}

CD3D9Buffer::CD3D9Buffer() :
	m_pVertexBuffer(nullptr),
	m_pIndexBuffer(nullptr),
	m_bImmutable(false)
{
}

CD3D9Buffer::~CD3D9Buffer()
{
	if (m_pIndexBuffer)
	{
		m_pIndexBuffer->Release();
		m_pIndexBuffer = nullptr;
	}

	if (m_pVertexBuffer)
	{
		m_pVertexBuffer->Release();
		m_pVertexBuffer = nullptr;
	}
}

HRESULT CD3D9Buffer::Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable)
{
	IDirect3DDevice9* pDevice = (IDirect3DDevice9*)HWRenderDevice;
	R_ASSERT(pDevice);

	m_BufferType = bufferType;
	m_bImmutable = bImmutable;

	D3DPOOL dwPool = D3DPOOL_DEFAULT;
	DWORD dwUsage = 0;

	if (bImmutable)
		dwPool = D3DPOOL_MANAGED;
	else
		dwUsage = D3DUSAGE_DYNAMIC;

	HRESULT hr = S_OK;

	switch (bufferType)
	{
	case eVertexBuffer:
		hr = pDevice->CreateVertexBuffer(DataSize, D3DUSAGE_WRITEONLY | dwUsage, 0, dwPool, &m_pVertexBuffer, NULL);
		SetData(m_pVertexBuffer, pData, DataSize);
		break;
	case eIndexBuffer:
		hr = pDevice->CreateIndexBuffer(DataSize, D3DUSAGE_WRITEONLY | dwUsage, D3DFMT_INDEX16, dwPool, &m_pIndexBuffer, NULL);
		SetData(m_pIndexBuffer, pData, DataSize);
		break;
	case eConstantBuffer:
	default:
		break;
	}

	R_CHK(hr);

	return hr;
}

void CD3D9Buffer::UpdateData(const void* data, int size)
{
}

bool CD3D9Buffer::Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags)
{
	HRESULT hr = S_OK;

	switch (m_BufferType)
	{
	case eVertexBuffer:
		hr = m_pVertexBuffer->Lock(OffsetToLock, SizeToLock, ppbData, GetD3D9BufferLockType(Flags));
		break;
	case eIndexBuffer:
		hr = m_pIndexBuffer->Lock(OffsetToLock, SizeToLock, ppbData, GetD3D9BufferLockType(Flags));
		break;
	default:
		break;
	}

	R_CHK(hr);

	return true;
}

bool CD3D9Buffer::Unlock()
{
	HRESULT hr = S_OK;

	switch (m_BufferType)
	{
	case eVertexBuffer:
		hr = m_pVertexBuffer->Unlock();
		break;
	case eIndexBuffer:
		hr = m_pIndexBuffer->Unlock();
		break;
	default:
		break;
	}

	R_CHK(hr);

	return true;
}

IDirect3DVertexBuffer9* CD3D9Buffer::GetD3DVertexBuffer()
{
	return m_pVertexBuffer;
}

IDirect3DIndexBuffer9* CD3D9Buffer::GetD3DIndexBuffer()
{
	return m_pIndexBuffer;
}

EResourceType CD3D9Buffer::GetType()
{
	switch (m_BufferType)
	{
	case eVertexBuffer:
		return eResourceVertexBuffer;
	case eIndexBuffer:
		return eResourceIndexBuffer;
	// No support in DX9
	//case eConstantBuffer:
	//	break;
	}

	return eResourceUnknown;
}

template<typename T>
void CD3D9Buffer::SetData(T* pBuffer, const void* pData, u32 DataSize)
{
	if (!pData)
		return;

	byte* bytes = 0;
	R_CHK(pBuffer->Lock(0, 0, (void**)&bytes, 0));
	memcpy(bytes, pData, DataSize);
	R_CHK(pBuffer->Unlock());
}