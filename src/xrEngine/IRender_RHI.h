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

class IRender_Texture
{
public:
	virtual ~IRender_Texture() = default;

	virtual bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, DWORD Flags) = 0;
	virtual bool UnlockRect(u32 Level) = 0;
};

typedef IRender_Texture* LPIRENDER_TEXTURE;

class IRender_BufferBase
{
public:
	virtual ~IRender_BufferBase() = 0;
	virtual void UpdateData(const void* data, int size) = 0;

	virtual bool Lock(LOCKED_RECT* pLockedRect, DWORD Flags) = 0;
	virtual bool Unlock() = 0;
};

typedef IRender_BufferBase* LPIRENDER_BUFFERBASE;

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

	virtual IRender_Texture* CreateAPITexture( const TextureDesc* pTextureDesc, const void* pData, const int size ) = 0;
	virtual IRender_BufferBase* CreateAPIBuffer( eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable ) = 0;
};

extern ENGINE_API IRender_RHI* g_RenderRHI;

namespace RHIUtils
{
	inline bool CreateVertexBuffer(IRender_BufferBase** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		IRender_BufferBase* pBuffer = g_RenderRHI->CreateAPIBuffer(eVertexBuffer, pData, DataSize, bImmutable);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	inline bool CreateIndexBuffer(IRender_BufferBase** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		IRender_BufferBase* pBuffer = g_RenderRHI->CreateAPIBuffer(eIndexBuffer, pData, DataSize, bImmutable);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	inline bool CreateConstantBuffer(IRender_BufferBase** ppBuffer, u32 DataSize)
	{
		IRender_BufferBase* pBuffer = g_RenderRHI->CreateAPIBuffer(eIndexBuffer, NULL, DataSize, FALSE);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}
}
