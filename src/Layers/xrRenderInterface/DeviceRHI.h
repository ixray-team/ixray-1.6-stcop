#pragma once

#include "../../xrEngine/device.h"

#include <d3d11.h>

enum eBufferType
{
	VERTEX,
	INDEX,
	CONSTANT
};

enum eBufferAccess
{
	DEFAULT,
	IMMUTABLE,
	DYNAMIC
};

enum eBufferMapping
{
	READ,
	WRITE,
	WRITE_DISCARD,
	WRITE_NO_OVERWRITE,
	READ_AND_WRITE
};

class RefCount
{
public:
	virtual ~RefCount() {}

	uint64_t AddRef();
	uint64_t Release();

private:
	// Ref counting
	uint64_t m_RefCount = 0;
};

inline uint64_t RefCount::AddRef()
{
	++m_RefCount;
	return m_RefCount;
}

inline uint64_t RefCount::Release()
{
	assert(m_RefCount > 0);
	--m_RefCount;

	if (m_RefCount == 0)
	{
		delete this;
		return 0;
	}

	return m_RefCount;
}

class IBuffer : public RefCount
{
public:
	virtual ~IBuffer() {}

	virtual void* Map(eBufferMapping Mapping) = 0;
	virtual void  Unmap() = 0;

	virtual void UpdateSubresource(void* pData, size_t Size) = 0;
};

class IRender_RHI
{
public:
	virtual void Create(APILevel API, void* renderDevice, void* renderContext) = 0;

	virtual IBuffer* CreateAPIBuffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable) = 0;

	virtual void SetVertexBuffer(u32 StartSlot, IBuffer* pVertexBuffer, const u32 Stride, const u32 Offset) = 0;
	virtual void SetIndexBuffer(IBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset) = 0;

	virtual void VSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void PSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void HSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void CSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void DSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void GSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
};

extern ENGINE_API IRender_RHI* g_RenderRHI;

// RHI Device ???
class CRenderRHI_DX11 : 
	public IRender_RHI
{
public:
	CRenderRHI_DX11();
	~CRenderRHI_DX11();

	void Create(APILevel API, void* renderDevice, void* renderContext);

	IBuffer* CreateAPIBuffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable) override;

	void SetVertexBuffer(u32 StartSlot, IBuffer* pVertexBuffer, const u32 Stride, const u32 Offset) override;
	void SetIndexBuffer(IBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset) override;

	void VSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void PSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void HSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void CSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void DSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void GSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;

	// DX11 Stuff
	ID3D11Device* GetDevice();
	ID3D11DeviceContext* GetDeviceContext();
};

extern CRenderRHI_DX11 g_RenderRHI_DX11Implementation;


// old globals
extern void* HWRenderDevice;
extern void* HWRenderContext;
extern void* HWSwapchain;

extern void* RenderTexture;
extern void* RenderSRV;
extern void* RenderDSV;

extern void* RenderRTV;
extern void* SwapChainRTV;

namespace RHIUtils
{
	inline bool CreateVertexBuffer(IBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		IBuffer* pBuffer = g_RenderRHI->CreateAPIBuffer(VERTEX, pData, DataSize, bImmutable);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	inline bool CreateIndexBuffer(IBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		IBuffer* pBuffer = g_RenderRHI->CreateAPIBuffer(INDEX, pData, DataSize, bImmutable);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	// Will return nullptr on DX9
	inline bool CreateConstantBuffer(IBuffer** ppBuffer, u32 DataSize)
	{
		IBuffer* pBuffer = g_RenderRHI->CreateAPIBuffer(CONSTANT, NULL, DataSize, FALSE);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}
}