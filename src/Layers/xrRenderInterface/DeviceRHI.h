#pragma once

#include "../../xrEngine/device.h"

#include <d3d11.h>

enum ERHITextureFormat
{
	FMT_UNKNOWN,

	FMT_R8G8,
	FMT_R8G8B8,
	FMT_R8G8B8A8,
	FMT_B8G8R8A8,

	FMT_R5G6B5,
	FMT_G16R16,
	FMT_A16B16G16R16,
	FMT_L8,
	FMT_A8L8,
	FMT_V8U8,
	FMT_Q8W8V8U8,
	FMT_V16U16,
	FMT_D24X8,
	FMT_D24S8,
	FMT_D32F_LOCKABLE,
	FMT_G16R16F,
	FMT_A16B16G16R16F,
	FMT_R32F,
	FMT_R16F,
	FMT_A32B32G32R32F,
	FMT_UYVY,
	FMT_R8G8_B8G8,
	FMT_YUY2,
	FMT_G8R8_G8B8,
	FMT_DXT1,
	FMT_DXT2,
	FMT_DXT3,
	FMT_DXT4,
	FMT_DXT5,

	FMT_MAX_COUNT
};

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

enum eResourceUsage
{
	USAGE_DEFAULT,
	USAGE_IMMUTABLE,
	USAGE_DYNAMIC,
	USAGE_STAGING
};

enum eBufferMapping
{
	READ,
	WRITE,
	WRITE_DISCARD,
	WRITE_NO_OVERWRITE,
	READ_AND_WRITE
};

enum eResourceDimension
{
	RESOURCE_DIMENSION_UNKNOWN,
	RESOURCE_DIMENSION_BUFFER,
	RESOURCE_DIMENSION_TEXTURE1D,
	RESOURCE_DIMENSION_TEXTURE2D,
	RESOURCE_DIMENSION_TEXTURE3D
};

struct STexture2DDesc
{
	u32 Width;
	u32 Height;
	u32 MipLevels;
	u32 ArraySize;
	ERHITextureFormat Format;
	eResourceUsage Usage;
	bool IsRenderTarget;
	bool IsDepthStencil;
};

struct SubresourceData
{
	const void* pSysMem;
	u32 SysMemPitch;
	u32 SysMemSlicePitch;
	u32 SysMemSize;
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

class IRHIResource :
	public RefCount
{
public:
	virtual ~IRHIResource() {}

	virtual void GetType(eResourceDimension* pResourceDimension) = 0;
};

class IBuffer : 
	public RefCount
{
public:
	virtual ~IBuffer() {}

	virtual void* Map(eBufferMapping Mapping) = 0;
	virtual void  Unmap() = 0;

	virtual void UpdateSubresource(void* pData, size_t Size) = 0;
};

class ITexture2D :
	public IRHIResource
{
public:
	virtual ~ITexture2D() {}

	virtual void SetDebugName(const char* name) = 0;
};

class IRender_RHI
{
public:
	virtual void Create(void* renderDevice, void* renderContext) = 0;

	virtual ITexture2D* CreateTexture2D(const STexture2DDesc& textureDesc, const SubresourceData* pSubresourceDesc) = 0;

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

typedef IRender_RHI* (*GetRenderRHIAPIFunc)(APILevel API);

extern ENGINE_API GetRenderRHIAPIFunc g_CreateRHIFunc;
extern ENGINE_API IRender_RHI* g_RenderRHI;

// RHI Device ???
class CRenderRHI_DX11 : 
	public IRender_RHI
{
public:
	CRenderRHI_DX11();
	~CRenderRHI_DX11();

	void Create(void* renderDevice, void* renderContext);

	ITexture2D* CreateTexture2D(const STexture2DDesc& textureDesc, const SubresourceData* pSubresourceDesc) override;

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

struct SPixelFormats
{
	ERHITextureFormat	Format;
	DXGI_FORMAT			PlatformFormat;
};

extern SPixelFormats g_PixelFormats[FMT_MAX_COUNT];

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