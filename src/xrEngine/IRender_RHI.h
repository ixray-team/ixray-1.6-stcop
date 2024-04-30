#pragma once

#include "../Layers/xrRenderInterface/TextureFormat.h"

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
	eTextureType1D,
	eTextureType2D,
	eTextureType3D,
	eTextureTypeCubemap,
};

enum eBufferType
{
	eVertexBuffer,
	eIndexBuffer,
	eConstantBuffer
};

enum ETextureFlags
{
	eTextureDefault			= 1 << 0, 
	eTextureRenderTarget	= 1 << 1,
	eTextureDepthStencil	= 1 << 2, 
	eTextureDynamic			= 1 << 3, 
	eTextureScratch			= 1 << 4, 
};

enum EResourceType {
	eResourceUnknown = 0,
	eResourceSurface = 1,
	eResourceVolume = 2,
	eResourceTexture = 3,
	eResourceVolumeTexture = 4,
	eResourceCubeTexture = 5,
	eResourceVertexBuffer = 6,
	eResourceIndexBuffer = 7,
	eResourceConstantBuffer = 8,
};

struct TextureDesc
{
	u32 Width;
	u32 Height;
	u32 DepthOrSliceNum;
	eUsage Usage;
	ERHITextureFormat Format;
	u32 TextureFlags;
	bool NumMips;
	bool DefaultPool;
	bool IsCube;
};

typedef struct LOCKED_RECT {
	u32  Pitch;
	void* pBits;
} LOCKED_RECT, * LPLOCKED_RECT;

typedef struct SUBRESOURCE_DATA
{
	const void* pSysMem;
	u32 SysMemPitch;
	u32 SysMemSlicePitch;
	u32 SysMemSize;
} SUBRESOURCE_DATA, *LPSUBRESOURCE_DATA;


/////////////////////////////////////////////////
// RHI Objects

class IRHIUnknown
{
public:
	virtual ~IRHIUnknown() {}

	// IUnknown interface
	virtual u64 AddRef();
	virtual u64 Release();

	// IDirect3DResource9 interface
	virtual EResourceType GetType() = 0;

protected:
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

	if (m_RefCount == 0)
	{
		delete this; 
		return 0; 
	}

	return m_RefCount;
}

class IRHISurface : public IRHIUnknown
{
};

typedef IRHISurface* LPIRHISURFACE;

class IRHITexture : public IRHIUnknown
{
public:
	virtual bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) = 0;
	virtual bool UnlockRect(u32 Level) = 0;
	virtual void SetStage(u32 Stage) = 0;
	virtual u32 GetLevelCount() = 0;
	virtual bool GetSurfaceLevel(u32 Level, LPIRHISURFACE* ppSurfaceLevel) = 0;
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

	virtual IRHITexture* CreateAPITexture( const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData ) = 0;
	virtual IRHIBuffer* CreateAPIBuffer( eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable ) = 0;

	virtual void SetVertexBuffer( u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets ) = 0;
	virtual void SetIndexBuffer( IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset ) = 0;

	virtual void SetRenderTarget(u32 RenderTargetIndex, IRHISurface* pRenderTarget) = 0;

	virtual ERHITextureFormat GetRHIFormatFromAPI( int dxgiFormat ) = 0;
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

	inline bool CreateTexture(u32 Width, u32 Height, u32 Levels, u32 Usage, ERHITextureFormat Format, bool DefaultPool, IRHITexture** ppTexture, void* pSharedHandle)
	{
		TextureDesc Desc = {};
		Desc.Width = Width;
		Desc.Height = Height;
		Desc.NumMips = Levels;
		Desc.DepthOrSliceNum = 1;
		Desc.Format = Format;
		Desc.TextureFlags = eTextureDefault;
		Desc.DefaultPool = DefaultPool;

		IRHITexture* pTexture = g_RenderRHI->CreateAPITexture(&Desc, nullptr);
		if (!pTexture)
			return false;

		*ppTexture = pTexture;

		return true;
	}
}
