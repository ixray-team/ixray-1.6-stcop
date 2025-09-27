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
			&m_pVB,
			nullptr);
	}
	else if (desc.Type == ERHI_BUFFER_TYPE::INDEX)
	{
		hr = m_pDev->CreateIndexBuffer(
			desc.Size,
			usage,
			D3DFMT_INDEX16,
			pool,
			&m_pIB,
			nullptr);
	}

	R_CHK(hr);

	if (pInitData && pInitData->pSysMem)
	{
		void* ptr = nullptr;
		if (m_pVB)
		{
			m_pVB->Lock(0, desc.Size, &ptr, 0);
			memcpy(ptr, pInitData->pSysMem, desc.Size);
			m_pVB->Unlock();
		}
		else if (m_pIB)
		{
			m_pIB->Lock(0, desc.Size, &ptr, 0);
			memcpy(ptr, pInitData->pSysMem, desc.Size);
			m_pIB->Unlock();
		}
	}
}


CD3D9Buffer::~CD3D9Buffer()
{
	if (m_pVB) m_pVB->Release();
	if (m_pIB) m_pIB->Release();
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
	if (m_pVB)
	{
		hr = m_pVB->Lock(0, m_bufferDesc.Size, &ptr, lockFlags);
	}
	else if (m_pIB)
	{
		hr = m_pIB->Lock(0, m_bufferDesc.Size, &ptr, lockFlags);
	}

	if (FAILED(hr))
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
	if (m_pVB) m_pVB->Unlock();
	if (m_pIB) m_pIB->Unlock();
}

void CD3D9Buffer::UpdateSubresource(void* pData, u32 Size)
{
	R_ASSERT(Size <= m_bufferDesc.Size);

	void* ptr = nullptr;
	if (m_pVB)
	{
		if (SUCCEEDED(m_pVB->Lock(0, Size, &ptr, 0)))
		{
			memcpy(ptr, pData, Size);
			m_pVB->Unlock();
		}
	}
	else if (m_pIB)
	{
		if (SUCCEEDED(m_pIB->Lock(0, Size, &ptr, 0)))
		{
			memcpy(ptr, pData, Size);
			m_pIB->Unlock();
		}
	}
}

void CD3D9Buffer::SetVertexBuffer(u32 StartSlot, const u32 Stride, const u32 Offset)
{
	R_ASSERT(m_pVB);
	m_pDev->SetStreamSource(StartSlot, m_pVB, Offset, Stride);
}

void CD3D9Buffer::SetIndexBuffer(bool Is32BitBuffer, u32 Offset)
{
	R_ASSERT(m_pIB);
	m_pDev->SetIndices(m_pIB);
}

void CD3D9Buffer::AddRef()
{
	if (m_pVB) m_pVB->AddRef();
	if (m_pIB) m_pIB->AddRef();
}

u32 CD3D9Buffer::Release()
{
	u32 refCountVB = 0;
	u32 refCountIB = 0;

	if (m_pVB)
	{
		HRESULT hr = m_pVB->Unlock(); // безопасно, если не заблокирован, вернёт D3DERR_INVALIDCALL
		refCountVB = m_pVB->Release();
		m_pVB = nullptr;
	}

	if (m_pIB)
	{
		HRESULT hr = m_pIB->Unlock();
		refCountIB = m_pIB->Release();
		m_pIB = nullptr;
	}

	return std::max(refCountVB, refCountIB);
}
