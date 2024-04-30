#pragma once

enum eUsage
{
	eUsageStatic,
	eUsageDynamic
};

enum eLockType
{
	eLOCK_DISCARD,
	eLOCK_NO_DIRTY_UPDATE,
	eLOCK_NOOVERWRITE,
	eLOCK_NOSYSLOCK,
	eLOCK_READONLY
};

enum eTextureType
{
	eTexture1D,
	eTexture2D,
	eTexture3D,
	eTextureCubemap,
};

enum eBufferType
{
	eVertexBuffer,
	eIndexBuffer,
	eConstantBuffer
};

enum PixelFormat
{
	FMT_UNKNOWN,

	// Unsigned
	FMT_R8G8B8,
	FMT_R8G8B8A8,

	// Float
	FMT_R16G16B16F,
	FMT_R16G16B16FA16F,
	FMT_R32G32B32F,
	FMT_R32G32B32FA32F,

	// Depth formats
	FMT_DEPTH24,
	FMT_DEPTH24_STENCIL8,
	FMT_DEPTH32,
	FMT_DEPTH32F
};

struct TextureDesc
{
	s32 width;
	s32 height;
	s32 depthOrSliceNum;
	eUsage usage;
	PixelFormat format;
	bool numMips;
};

typedef struct LOCKED_RECT {
	s32  Pitch;
	void* pBits;
} LOCKED_RECT, * LPLOCKED_RECT;

/////////////////////////////////////////////////
// RHI Objects

class IRHIUnknown
{
public:
	virtual ~IRHIUnknown() {}

	// IUnknown interface
	u64 AddRef();
	u64 Release();

private:
	// Ref counting
	u64 m_RefCount = 0;
};

inline u64 IRHIUnknown::AddRef()
{
	++m_RefCount;
	return m_RefCount;
}

inline u64 IRHIUnknown::Release()
{
	R_ASSERT(m_RefCount > 0);
	--m_RefCount;

	if (m_RefCount == 0) { delete this; return 0; }
	return m_RefCount;
}

class IRHITexture : public IRHIUnknown
{
public:
	virtual ~IRHITexture() = default;

	virtual bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) = 0;
	virtual bool UnlockRect(u32 Level) = 0;
};

typedef IRHITexture* LPIRHITEXTURE;

class IRHIBuffer : public IRHIUnknown
{
public:
	virtual ~IRHIBuffer() = default;

	virtual void UpdateData(const void* data, int size) = 0;

	virtual bool Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags) = 0;
	virtual bool Unlock() = 0;
};

typedef IRHIBuffer* LPIRHIBUFFER;

class IRender_RHI
{
public:
	enum class APILevel
	{
		DX9,
		DX11
	};

	APILevel API = APILevel::DX9;

public:
	virtual bool Create(APILevel) = 0;
	virtual bool UpdateBuffers() = 0;
	virtual void ResizeBuffers(u16 Width, u16 Height) = 0;
	virtual void Destroy() = 0;

	virtual void* GetRenderSRV() = 0;
	virtual void* GetRenderDevice() = 0;
	virtual void* GetRenderContext() = 0;
	virtual void* GetRenderTexture() = 0;
	virtual void* GetDepthTexture() = 0;
	virtual void* GetSwapchainTexture() = 0;
	virtual void* GetSwapchain() = 0;

	virtual void FillModes() = 0;
	virtual int GetFeatureLevel() = 0;

	virtual IRHITexture* CreateAPITexture( const TextureDesc* pTextureDesc, const void* pData, const int size ) = 0;
	virtual IRHIBuffer* CreateAPIBuffer( eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable ) = 0;

	virtual void SetVertexBuffer( u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets ) = 0;
	virtual void SetIndexBuffer( IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset ) = 0;
};

extern ENGINE_API IRender_RHI* g_RenderRHI;

namespace RHIUtils
{
	inline bool CreateVertexBuffer(IRHIBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		IRHIBuffer* pBuffer = g_RenderRHI->CreateAPIBuffer(eVertexBuffer, pData, DataSize, bImmutable);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	inline bool CreateIndexBuffer(IRHIBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		IRHIBuffer* pBuffer = g_RenderRHI->CreateAPIBuffer(eIndexBuffer, pData, DataSize, bImmutable);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	// Will return nullptr on DX9
	inline bool CreateConstantBuffer(IRHIBuffer** ppBuffer, u32 DataSize)
	{
		IRHIBuffer* pBuffer = g_RenderRHI->CreateAPIBuffer(eConstantBuffer, NULL, DataSize, FALSE);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}
}
