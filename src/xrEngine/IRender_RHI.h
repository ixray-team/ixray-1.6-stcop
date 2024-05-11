#pragma once

#include "../Layers/xrRenderInterface/TextureFormat.h"

enum ERHIUsage
{
	eUsageDefault,
	eUsageRenderTarget,
	eUsageDepthStencil,
	eUsageStatic,
	eUsageDynamic,
	eUsageScratch,
	eUsageImmutable 
};

enum ERHIClearStage
{
	eClearDepth,
	eClearStencil,
	eClearTarget
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

enum EResourceType {
	eResourceUnknown = 0,
	eResourceSurface = 1,
	eResourceVolume = 2,
	eResourceTexture1D = 3,
	eResourceTexture2D = 4,
	eResourceVolumeTexture = 5,
	eResourceCubeTexture = 6,
	eResourceVertexBuffer = 7,
	eResourceIndexBuffer = 8,
	eResourceConstantBuffer = 9,
};

enum RHI_UAV_DIMENSION
{
	RHI_UAV_DIMENSION_UNKNOWN = 0,
	RHI_UAV_DIMENSION_BUFFER = 1,
	RHI_UAV_DIMENSION_TEXTURE1D = 2,
	RHI_UAV_DIMENSION_TEXTURE1DARRAY = 3,
	RHI_UAV_DIMENSION_TEXTURE2D = 4,
	RHI_UAV_DIMENSION_TEXTURE2DARRAY = 5,
	RHI_UAV_DIMENSION_TEXTURE3D = 8
};

struct ClearData
{
	ClearData() = default;
	ClearData(float r, float g, float b, float a) { Color.set(r, g, b, a); }

	Fvector4 Color;
	float Depth = 1;
	float Stencil = 0;
};

struct TextureDesc
{
	u32 Width;
	u32 Height;
	u32 DepthOrSliceNum;
	u32 Usage;
	u32 Depth;
	ERHITextureFormat Format;
	u32 TextureFlags;
	eTextureType TextureType;
	u32 NumMips;
	bool DefaultPool;
	bool IsCube;
	bool IsRenderTarget;
};

typedef struct LOCKED_RECT {
	u32  Pitch;
	void* pBits;
} LOCKED_RECT, * LPLOCKED_RECT;

typedef struct LOCKED_BOX {
	int  RowPitch;
	int  SlicePitch;
	void* pBits;
} LOCKED_BOX, * LPLOCKED_BOX;

typedef struct SUBRESOURCE_DATA
{
	const void* pSysMem;
	u32 SysMemPitch;
	u32 SysMemSlicePitch;
	u32 SysMemSize;
} SUBRESOURCE_DATA, *LPSUBRESOURCE_DATA;

struct SRHIAPIData
{
	void* pRTV;
	void* pDSV;
	void* pSRV;
	void* pUAV;
};

typedef struct RHIBOX {
	u32 Left;
	u32 Top;
	u32 Right;
	u32 Bottom;
	u32 Front;
	u32 Back;
} RHIBOX, *LPRHIBOX;

typedef struct RHI_BUFFER_UAV {
	UINT FirstElement;
	UINT NumElements;
	UINT Flags;
} RHI_BUFFER_UAV;

typedef struct RHI_TEX2D_UAV
{
	UINT MipSlice;
} 	RHI_TEX2D_UAV;

typedef struct RHI_UNORDERED_ACCESS_VIEW_DESC {
	ERHITextureFormat       Format;
	RHI_UAV_DIMENSION		ViewDimension;
	union {
		RHI_BUFFER_UAV      Buffer;
		//D3D11_TEX1D_UAV       Texture1D;
		//D3D11_TEX1D_ARRAY_UAV Texture1DArray;
		RHI_TEX2D_UAV       Texture2D;
		//D3D11_TEX2D_ARRAY_UAV Texture2DArray;
		//D3D11_TEX3D_UAV       Texture3D;
	};

} RHI_UNORDERED_ACCESS_VIEW_DESC;

struct RHI_TEX2D_RT
{
	u32 MipSlice;
};

struct RHI_TEX3D_RT
{
	u32 MipSlice;
	u32 FirstWSlice;
	u32 WSize;
};

const int kMaxRenderTargetTextures = 8;

class IRHITexture;

enum RHI_RTV_DIMENSION
{
	RHI_RTV_DIMENSION_UNKNOWN = 0,
	RHI_RTV_DIMENSION_BUFFER = 1,
	RHI_RTV_DIMENSION_TEXTURE1D = 2,
	RHI_RTV_DIMENSION_TEXTURE1DARRAY = 3,
	RHI_RTV_DIMENSION_TEXTURE2D = 4,
	RHI_RTV_DIMENSION_TEXTURE2DARRAY = 5,
	RHI_RTV_DIMENSION_TEXTURE2DMS = 6,
	RHI_RTV_DIMENSION_TEXTURE2DMSARRAY = 7,
	RHI_RTV_DIMENSION_TEXTURE3D = 8
};

struct RenderTargetCreationDesc
{
	ERHITextureFormat	Format;
	RHI_RTV_DIMENSION	ViewDimension;
	RHI_TEX2D_RT		Texture2D;
	RHI_TEX3D_RT		Texture3D;
};

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

//! Render Target or Surface interface
class IRHISurface : public IRHIUnknown
{
public:
	virtual bool LockRect(LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) = 0;
	virtual bool UnlockRect() = 0;
	virtual void GetAPIData(SRHIAPIData* pAPIData) = 0;
};

//! Depth Stencil Target interface
class IRHIDepthStencilView : public IRHISurface
{
public:
	virtual void SetActive() = 0;
};

typedef IRHISurface* LPIRHISURFACE;

class IRHIUnorderedAccessView : public IRHISurface
{
public:
	virtual ~IRHIUnorderedAccessView() = default;
};

class IRHITexture : public IRHIUnknown
{
public:
	virtual bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) = 0;
	virtual bool UnlockRect(u32 Level) = 0;
	virtual void SetStage(u32 Stage) = 0;
	virtual u32 GetLevelCount() = 0;
	virtual bool GetSurfaceLevel(u32 Level, LPIRHISURFACE* ppSurfaceLevel) = 0;

	virtual Ivector2 GetTextureSize() const = 0;
	virtual void GetAPIData(SRHIAPIData* pAPIData) = 0;
	virtual void GetDesc(TextureDesc* pTextureDesc) = 0;

	virtual void QueryShaderResourceView(void** ppSRV) = 0;
};

typedef IRHITexture* LPIRHITEXTURE;

class IRHIVolumeTexture : public IRHITexture
{
public:
	virtual bool LockBox(u32 Level, LOCKED_BOX* pLockedVolume, const RHIBOX* pBox, u32 Flags) = 0;
	virtual bool UnlockBox(u32 Level) = 0;
};

typedef IRHIVolumeTexture* LPIRHIVOLUMETEXTURE;

class IRHIBuffer : public IRHIUnknown
{
public:
	virtual ~IRHIBuffer() = default;

	virtual void UpdateData(const void* data, int size) = 0;

	virtual bool Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags) = 0;
	virtual bool Unlock() = 0;
};

typedef IRHIBuffer* LPIRHIBUFFER;

class IRHIVertexDeclaration : public IRHIUnknown
{
public:
	virtual ~IRHIVertexDeclaration() = default;
};

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

	virtual IRHITexture* CreateAPITexture1D( const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData ) = 0;
	virtual IRHITexture* CreateAPITexture( const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData ) = 0;
	virtual IRHITexture* CreateAPITexture3D(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData) = 0;
	virtual IRHIBuffer*  CreateAPIBuffer( eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable ) = 0;
	virtual IRHIDepthStencilView* CreateAPIDepthStencilSurface(u32 Width, u32 Height, ERHITextureFormat Format, u32 MultiSample, u32 MultisampleQuality, bool Discard) = 0;
	virtual IRHISurface* CreateAPIOffscreenPlainSurface(u32 Width, u32 Height, ERHITextureFormat Format, bool DefaultPool) = 0;
	virtual IRHIVolumeTexture* CreateAPIVolumeTexture( const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData ) = 0;
	virtual IRHIUnorderedAccessView* CreateAPIUnorderedAccessView( IRHITexture* pTexture, const RHI_UNORDERED_ACCESS_VIEW_DESC& Desc ) = 0;
	virtual IRHISurface* CreateAPIRenderTargetView( IRHITexture* pTexture, const RenderTargetCreationDesc* pDesc ) = 0;
	virtual IRHIDepthStencilView* CreateAPIDepthStencilView( IRHITexture* pTexture, const RenderTargetCreationDesc* pDesc ) = 0;

	virtual void Clear(ERHIClearStage Stage, IRHIUnknown* Ptr, const ClearData& Data) = 0;
	virtual void SetVertexBuffer( u32 StartSlot, IRHIBuffer* pVertexBuffer, const u32 Strides, const u32 Offsets ) = 0;
	virtual void SetIndexBuffer( IRHIBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset ) = 0;

	virtual void SetRenderTarget(u32 RenderTargetIndex, IRHISurface* pRenderTarget) = 0;
	virtual void SetDepthStencilView(IRHIDepthStencilView* pDepthStencilView) = 0;

	virtual void GetRenderTargetData(IRHISurface* pRenderTarget, IRHISurface* pDestSurface) = 0;
	virtual void CopyResource(IRHITexture* pDstResource, IRHITexture* pSrcResource) = 0;
	virtual void StretchRect(IRHISurface* pSourceSurface, const Irect* pSourceRect, IRHISurface* pDestSurface, const Irect* pDestRect) = 0;

	virtual ERHITextureFormat GetRHIFormatFromAPI( int dxgiFormat ) = 0;

	///////////////////////////////////////////////////////
	// Temp inline functions
	inline void ClearRenderTargetView( IRHISurface* pSurface, const float ColorRGBA[4] )
	{
		ClearData clearData = { ColorRGBA[0], ColorRGBA[1], ColorRGBA[2], ColorRGBA[3] };
		Clear( eClearTarget, pSurface, clearData );
	}
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

	inline bool CreateDepthStencilSurface(u32 Width, u32 Height, ERHITextureFormat Format, u32 MultiSample, DWORD MultisampleQuality, BOOL Discard, IRHIDepthStencilView** ppSurface, [[maybe_unused]] HANDLE* pSharedHandle)
	{
		IRHIDepthStencilView* pDepth = g_RenderRHI->CreateAPIDepthStencilSurface(Width, Height, Format, MultiSample, MultisampleQuality, Discard);
		if (!pDepth)
			return false;

		*ppSurface = pDepth;

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
		Desc.Usage = Usage;
		//Desc.TextureFlags = eUsageDefault;
		Desc.DefaultPool = DefaultPool;

		IRHITexture* pTexture = g_RenderRHI->CreateAPITexture(&Desc, nullptr);
		if (!pTexture)
			return false;

		*ppTexture = pTexture;

		return true;
	}

	inline bool CreateVolumeTexture(u32 Width, 
									u32 Height, 
									u32 Depth, 
									u32 Levels, 
									u32 Usage, 
									ERHITextureFormat Format, 
									bool DefaultPool, 
									IRHIVolumeTexture** ppVolumeTexture, 
									void* pSharedHandle)
	{
		TextureDesc Desc = {};
		Desc.Width = Width;
		Desc.Height = Height;
		Desc.NumMips = Levels;
		Desc.DepthOrSliceNum = Depth;
		Desc.Format = Format;
		Desc.Usage = Usage;
		Desc.DefaultPool = DefaultPool;

		IRHIVolumeTexture* pTexture = g_RenderRHI->CreateAPIVolumeTexture(&Desc, nullptr);
		if (!pTexture)
			return false;

		*ppVolumeTexture = pTexture;

		return true;
	}
}
