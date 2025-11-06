#include "DX9Buffer.h"

CD3D9Buffer::CD3D9Buffer(IDirect3DDevice9* pDevice, const RHIBufferDesc& desc, const RHIBufferSubresource* pInitData) :
	m_pDev(pDevice),
	m_bufferDesc(desc)
{
	R_ASSERT(m_pDev);

	DWORD usage = 0;
	D3DPOOL pool = D3DPOOL_MANAGED;

	if (m_bufferDesc.Usage == ERHI_USAGE::USAGE_DYNAMIC)
	{
		// FX: Ебаный костыль
		if (pInitData && pInitData->pSysMem)
		{
			usage |= D3DUSAGE_WRITEONLY;
			pool = D3DPOOL_MANAGED;
		}
		else
		{
			usage |= D3DUSAGE_DYNAMIC | D3DUSAGE_WRITEONLY;
			pool = D3DPOOL_DEFAULT;
		}
	}
	else if (m_bufferDesc.Usage == ERHI_USAGE::USAGE_DEFAULT)
	{
		usage |= D3DUSAGE_WRITEONLY;
		pool = D3DPOOL_MANAGED;
	}

	HRESULT hr = E_FAIL;
	if (m_bufferDesc.Type == ERHI_BUFFER_TYPE::VERTEX)
	{
		hr = m_pDev->CreateVertexBuffer(
			desc.Size,
			usage,
			0,
			pool,
			&VertexBuffer,
			nullptr);
	}
	else if (desc.Type == ERHI_BUFFER_TYPE::INDEX)
	{
		hr = m_pDev->CreateIndexBuffer(
			desc.Size,
			usage,
			D3DFMT_INDEX16,
			pool,
			&IndexBuffer,
			nullptr);
	}

	R_CHK(hr);

	if (pInitData && pInitData->pSysMem)
	{
		void* ptr = nullptr;
		if (VertexBuffer)
		{
			VertexBuffer->Lock(0, desc.Size, &ptr, 0);
			memcpy(ptr, pInitData->pSysMem, desc.Size);
			VertexBuffer->Unlock();
		}
		else if (IndexBuffer)
		{
			IndexBuffer->Lock(0, desc.Size, &ptr, 0);
			memcpy(ptr, pInitData->pSysMem, desc.Size);
			IndexBuffer->Unlock();
		}
	}
}


CD3D9Buffer::~CD3D9Buffer()
{
	if (VertexBuffer) VertexBuffer->Release();
	if (IndexBuffer) IndexBuffer->Release();
}

bool CD3D9Buffer::Map(ERHI_BUFFER_MAP MapType, u32 MapFlags, RHIMappedSubresource* pData)
{
	DWORD lockFlags = 0;
	switch (MapType)
	{
	case ERHI_BUFFER_MAP::READ:
		lockFlags = D3DLOCK_READONLY;
		break;
	case ERHI_BUFFER_MAP::WRITE_DISCARD:
		lockFlags = D3DLOCK_DISCARD;
		break;
	case ERHI_BUFFER_MAP::WRITE_NO_OVERWRITE:
		lockFlags = D3DLOCK_NOOVERWRITE;
		break;
	case ERHI_BUFFER_MAP::WRITE:
		lockFlags = 0;
		break;
	default:
		return false; // DX9 limited mapping support
	}

	void* ptr = nullptr;
	HRESULT hr = E_FAIL;

	VERIFY(VertexBuffer || IndexBuffer);

	if (VertexBuffer)
	{
		hr = VertexBuffer->Lock(0, m_bufferDesc.Size, &ptr, lockFlags);
	}
	else if (IndexBuffer)
	{
		hr = IndexBuffer->Lock(0, m_bufferDesc.Size, &ptr, lockFlags);
	}

	if (IndexBuffer == nullptr && VertexBuffer == nullptr)
	{
		Msg("! CD3D9Buffer::Map: Buffer is empty!");
		return false;
	}
	else if (FAILED(hr))
	{
		Msg("! CD3D9Buffer::Map: Failed to map buffer. DirectX Error: %s", Debug.error2string(hr));
		return false;
	}

	if (pData)
	{
		pData->pData = ptr;
		pData->RowPitch = m_bufferDesc.Size;   // ��� ��������
		pData->DepthPitch = m_bufferDesc.Size;
	}

	return true;
}

void CD3D9Buffer::Unmap()
{
	if (VertexBuffer) VertexBuffer->Unlock();
	if (IndexBuffer) IndexBuffer->Unlock();
}

void CD3D9Buffer::UpdateSubresource(void* pData, u32 Size)
{
	R_ASSERT(Size <= m_bufferDesc.Size);

	void* ptr = nullptr;
	if (VertexBuffer)
	{
		if (SUCCEEDED(VertexBuffer->Lock(0, Size, &ptr, 0)))
		{
			memcpy(ptr, pData, Size);
			VertexBuffer->Unlock();
		}
	}
	else if (IndexBuffer)
	{
		if (SUCCEEDED(IndexBuffer->Lock(0, Size, &ptr, 0)))
		{
			memcpy(ptr, pData, Size);
			IndexBuffer->Unlock();
		}
	}
}

void CD3D9Buffer::SetVertexBuffer(u32 StartSlot, const u32 Stride, const u32 Offset)
{
	m_pDev->SetStreamSource(StartSlot, VertexBuffer, Offset, Stride);
}

void CD3D9Buffer::SetIndexBuffer(bool Is32BitBuffer, u32 Offset)
{
	m_pDev->SetIndices(IndexBuffer);
}

void CD3D9Buffer::AddRef()
{
	if (VertexBuffer)
	{
		refCountVB = VertexBuffer->AddRef(); 
		return;
	}

	if (IndexBuffer)
	{
		refCountIB = IndexBuffer->AddRef();
		return;
	}
}

u32 CD3D9Buffer::Release()
{
	if (VertexBuffer)
	{
		HRESULT hr = VertexBuffer->Unlock(); // безопасно, если не заблокирован, вернёт D3DERR_INVALIDCALL
		refCountVB = VertexBuffer->Release();

		if (refCountVB == 0)
		{
			VertexBuffer = nullptr;
		}
	}

	if (IndexBuffer)
	{
		HRESULT hr = IndexBuffer->Unlock();
		refCountIB = IndexBuffer->Release();

		if (refCountIB == 0)
		{
			IndexBuffer = nullptr;
		}
	}
	
	return std::max(refCountVB, refCountIB);
}
