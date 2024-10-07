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
	FMT_L8, // RENAME TO FMT_R8
	FMT_A8L8,
	FMT_V8U8,
	FMT_Q8W8V8U8, // RENAME TO FMT_R8G8B8A8
	FMT_V16U16,
	FMT_D24X8,
	FMT_D24S8,
	FMT_D32F_LOCKABLE,
	FMT_G16R16F,
	FMT_A16B16G16R16F, // RENAME TO FMT_R16G16B16A16F
	FMT_R32F,
	FMT_R16F,
	FMT_A32B32G32R32F, // RENAME TO FMT_R32G32B32A32F
	FMT_UYVY,
	FMT_R8G8_B8G8,
	FMT_YUY2,
	FMT_G8R8_G8B8,
	FMT_DXT1,
	FMT_DXT2,
	FMT_DXT3,
	FMT_DXT4,
	FMT_DXT5,

	FMT_R32,
	FMT_X8R8G8B8,

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
	BUFFERACCESS_DEFAULT,
	BUFFERACCESS_IMMUTABLE,
	BUFFERACCESS_DYNAMIC
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
	MAPPING_READ,
	MAPPING_WRITE,
	MAPPING_WRITE_DISCARD,
	MAPPING_WRITE_NO_OVERWRITE,
	MAPPING_READ_AND_WRITE
};

enum eResourceDimension
{
	RESOURCE_DIMENSION_UNKNOWN,
	RESOURCE_DIMENSION_BUFFER,
	RESOURCE_DIMENSION_TEXTURE1D,
	RESOURCE_DIMENSION_TEXTURE2D,
	RESOURCE_DIMENSION_TEXTURE3D
};

enum eRTVDimension
{
	RTV_DIMENSION_UNKNOWN,
	RTV_DIMENSION_BUFFER,
	RTV_DIMENSION_TEXTURE1D,
	RTV_DIMENSION_TEXTURE1DARRAY,
	RTV_DIMENSION_TEXTURE2D,
	RTV_DIMENSION_TEXTURE2DARRAY,
	RTV_DIMENSION_TEXTURE2DMS,
	RTV_DIMENSION_TEXTURE2DMSARRAY,
	RTV_DIMENSION_TEXTURE3D
};

enum eDTVDimension
{
	DSV_DIMENSION_UNKNOWN,
	DSV_DIMENSION_TEXTURE1D,
	DSV_DIMENSION_TEXTURE1DARRAY,
	DSV_DIMENSION_TEXTURE2D,
	DSV_DIMENSION_TEXTURE2DARRAY,
	DSV_DIMENSION_TEXTURE2DMS,
	DSV_DIMENSION_TEXTURE2DMSARRAY
};

enum eDSVFlag
{
	DSV_READ_ONLY_DEPTH = 0x1L,
	DSV_READ_ONLY_STENCIL = 0x2L
};

enum eClearFlag
{
	CLEAR_DEPTH = 0x1L,
	CLEAR_STENCIL = 0x2L
};

struct SSampleDesc
{
	UINT Count = 1;
	UINT Quality  = 0;
};

struct STexture1DDesc
{
	u32 Width;
	u32 MipLevels;
	u32 ArraySize;
	ERHITextureFormat Format;
	eResourceUsage Usage;
	SSampleDesc SampleDesc;
	bool IsRenderTarget;
	bool IsDepthStencil;
	bool IsTextureCube;
	bool IsCPURead = false;
	bool NoShaderResourceView = false;
};

struct STexture2DDesc
{
	u32 Width;
	u32 Height;
	u32 MipLevels;
	u32 ArraySize;
	ERHITextureFormat Format;
	eResourceUsage Usage;
	SSampleDesc SampleDesc;
	bool IsRenderTarget;
	bool IsDepthStencil;
	bool IsTextureCube;
	bool IsCPURead = false;
	bool NoShaderResourceView = false;
};

struct STexture3DDesc
{
	u32 Width;
	u32 Height;
	u32 Depth;
	u32 MipLevels;
	ERHITextureFormat Format;
	eResourceUsage Usage;
	SSampleDesc SampleDesc;
	bool IsRenderTarget;
	bool IsDepthStencil;
	bool IsTextureCube;
	bool IsCPURead = false;
	bool NoShaderResourceView = false;
};

struct STexture2DRTV
{
	u32 MipSlice;
};

struct STexture3DRTV
{
	u32 MipSlice;
	u32 FirstWSlice;
	u32 WSize;
};

struct SRenderTargetViewDesc
{
	ERHITextureFormat Format;
	eRTVDimension ViewDimension;

	// #TODO: make it union like in d3d11
	//STexture1DRTV Texture1D;
	STexture2DRTV Texture2D;
	STexture3DRTV Texture3D;

	/* 
	union
	{
		D3D11_BUFFER_RTV Buffer;
		D3D11_TEX1D_RTV Texture1D;
		D3D11_TEX1D_ARRAY_RTV Texture1DArray;
		D3D11_TEX2D_RTV Texture2D;
		D3D11_TEX2D_ARRAY_RTV Texture2DArray;
		D3D11_TEX2DMS_RTV Texture2DMS;
		D3D11_TEX2DMS_ARRAY_RTV Texture2DMSArray;
		D3D11_TEX3D_RTV Texture3D;
	};
	*/
};

struct STexture1DDSV
{
	u32 MipSlice;
};

struct STexture2DDSV
{
	u32 MipSlice;
};

struct SDepthStencilViewDesc
{
	ERHITextureFormat Format;
	eDTVDimension ViewDimension;
	u32 Flags;

	STexture1DDSV Texture1D;
	STexture2DDSV Texture2D;

	// #TODO: make it union like in d3d11
	//union {
	//	D3D11_TEX1D_DSV         Texture1D;
	//	D3D11_TEX1D_ARRAY_DSV   Texture1DArray;
	//	D3D11_TEX2D_DSV         Texture2D;
	//	D3D11_TEX2D_ARRAY_DSV   Texture2DArray;
	//	D3D11_TEX2DMS_DSV       Texture2DMS;
	//	D3D11_TEX2DMS_ARRAY_DSV Texture2DMSArray;
	//};
};

struct SubresourceData
{
	const void* pSysMem;
	u32 SysMemPitch;
	u32 SysMemSlicePitch;
	u32 SysMemSize;
};

struct SMappedSubresource
{
	void* pData;
	u32 RowPitch;
	u32 DepthPitch;
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

	virtual void SetDebugName(const char* name) = 0;
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

class IShaderResourceView;

class ITexture1D :
	public IRHIResource
{
public:
	virtual ~ITexture1D() {}

	virtual void GetDesc(STexture1DDesc* desc) = 0;

	// #TODO: Costyl, remove after refactor of SRVSManager
	virtual void GetShaderResourceView(IShaderResourceView** ppShaderResourceView) = 0;

	virtual void Map(u32 Subresource, eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pMappedTex2D) = 0;
	virtual void Unmap(u32 Subresource) = 0;
};

class ITexture2D :
	public IRHIResource
{
public:
	virtual ~ITexture2D() {}

	virtual void GetDesc(STexture2DDesc* desc) = 0;

	// #TODO: Costyl, remove after refactor of SRVSManager
	virtual void GetShaderResourceView(IShaderResourceView** ppShaderResourceView) = 0;

	virtual void Map(u32 Subresource, eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pMappedTex2D) = 0;
	virtual void Unmap(u32 Subresource) = 0;
};

class ITexture3D :
	public IRHIResource
{
public:
	virtual ~ITexture3D() {}

	virtual void GetDesc(STexture3DDesc* desc) = 0;

	// #TODO: Costyl, remove after refactor of SRVSManager
	virtual void GetShaderResourceView(IShaderResourceView** ppShaderResourceView) = 0;

	virtual void Map(u32 Subresource, eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pMappedTex3D) = 0;
	virtual void Unmap(u32 Subresource) = 0;
};

class IRenderTargetView :
	public IRHIResource
{
public:
	virtual ~IRenderTargetView() {}

	virtual void GetDesc(SRenderTargetViewDesc* desc) = 0;
	virtual void GetResource(IRHIResource** ppResource) = 0; // ADD REF !!!
};

class IDepthStencilView :
	public IRHIResource
{
public:
	virtual ~IDepthStencilView() {}

	virtual void GetDesc(SDepthStencilViewDesc* desc) = 0;
	virtual void GetResource(IRHIResource** ppResource) = 0; // ADD REF !!!
};

class IRender_RHI
{
public:
	virtual void Create(void* renderDevice, void* renderContext) = 0;

	virtual ITexture1D* CreateTexture1D(const STexture1DDesc& textureDesc, const SubresourceData* pSubresourceDesc) = 0;
	virtual ITexture2D* CreateTexture2D(const STexture2DDesc& textureDesc, const SubresourceData* pSubresourceDesc) = 0;
	virtual ITexture3D* CreateTexture3D(const STexture3DDesc& textureDesc, const SubresourceData* pSubresourceDesc) = 0;

	virtual IRenderTargetView* CreateRenderTargetView(IRHIResource* pResource, const SRenderTargetViewDesc* pDesc) = 0;
	virtual IDepthStencilView* CreateDepthStencilView(IRHIResource* pResource, const SDepthStencilViewDesc* pDesc) = 0;

	virtual IBuffer* CreateBuffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable) = 0;

	virtual void SetVertexBuffer(u32 StartSlot, IBuffer* pVertexBuffer, const u32 Stride, const u32 Offset) = 0;
	virtual void SetIndexBuffer(IBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset) = 0;

	virtual void VSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void PSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void HSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void CSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void DSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;
	virtual void GSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) = 0;

	virtual void ClearRenderTargetView(IRenderTargetView* pRenderTargetView, const float ColorRGBA[4]) = 0;
	virtual void ClearDepthStencilView(IDepthStencilView* pDepthStencilView, u32 ClearFlags, float Depth, u8 Stencil) = 0;

	// Note: maximum is 8 render targets.
	virtual void SetRenderTargets(u32 NumViews, IRenderTargetView* const* ppRenderTargetViews, IDepthStencilView* pDepthStencilView) = 0;

	virtual void CopyResource(IRHIResource* pDstResource, IRHIResource* pSrcResource) = 0;
};

typedef IRender_RHI* (*GetRenderRHIAPIFunc)(APILevel API);

// To ForserX: !!! g_CreateRHIFunc and g_RenderRHI should be in xrAbstractions
//extern ENGINE_API GetRenderRHIAPIFunc g_CreateRHIFunc;
//extern ENGINE_API IRender_RHI* g_RenderRHI;

extern IRender_RHI* g_RenderRHI;

// #TODO: COSTYL
class IRHI_ResourceHack
{
public:
	virtual ID3D11Resource* GetD3D11Resource() = 0;
};

// RHI Device ???
class CRenderRHI_DX11 : 
	public IRender_RHI
{
public:
	CRenderRHI_DX11();
	~CRenderRHI_DX11();

	void Create(void* renderDevice, void* renderContext);

	ITexture1D* CreateTexture1D(const STexture1DDesc& textureDesc, const SubresourceData* pSubresourceDesc) override;
	ITexture2D* CreateTexture2D(const STexture2DDesc& textureDesc, const SubresourceData* pSubresourceDesc) override;
	ITexture3D* CreateTexture3D(const STexture3DDesc& textureDesc, const SubresourceData* pSubresourceDesc) override;

	IRenderTargetView* CreateRenderTargetView(IRHIResource* pResource, const SRenderTargetViewDesc* pDesc) override;
	IDepthStencilView* CreateDepthStencilView(IRHIResource* pResource, const SDepthStencilViewDesc* pDesc) override;

	IBuffer* CreateBuffer(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable) override;

	void SetVertexBuffer(u32 StartSlot, IBuffer* pVertexBuffer, const u32 Stride, const u32 Offset) override;
	void SetIndexBuffer(IBuffer* pIndexBuffer, bool Is32BitBuffer, u32 Offset) override;

	void VSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void PSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void HSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void CSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void DSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;
	void GSSetConstantBuffers(u32 StartSlot, u32 NumBuffers, IBuffer* const* ppConstantBuffers) override;

	void ClearRenderTargetView(IRenderTargetView* pRenderTargetView, const float ColorRGBA[4]) override;
	void ClearDepthStencilView(IDepthStencilView* pDepthStencilView, u32 ClearFlags, float Depth, u8 Stencil) override;

	// Note: maximum is 8 render targets.
	void SetRenderTargets(u32 NumViews, IRenderTargetView* const* ppRenderTargetViews, IDepthStencilView* pDepthStencilView) override;

	void CopyResource(IRHIResource* pDstResource, IRHIResource* pSrcResource) override;

	// DX11 Stuff
	ID3D11Device* GetDevice();
	ID3D11DeviceContext* GetDeviceContext();

};

// D3D11 Stuff
D3D11_MAP GetD3D11Map(eBufferMapping Mapping);
u32 GetD3D11BindFlags(eBufferType bufferType);

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
		IBuffer* pBuffer = g_RenderRHI->CreateBuffer(VERTEX, pData, DataSize, bImmutable);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	inline bool CreateIndexBuffer(IBuffer** ppBuffer, const void* pData, u32 DataSize, bool bImmutable = true)
	{
		IBuffer* pBuffer = g_RenderRHI->CreateBuffer(INDEX, pData, DataSize, bImmutable);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}

	// Will return nullptr on DX9
	inline bool CreateConstantBuffer(IBuffer** ppBuffer, u32 DataSize)
	{
		IBuffer* pBuffer = g_RenderRHI->CreateBuffer(CONSTANT, NULL, DataSize, FALSE);
		if (!pBuffer)
			return false;

		*ppBuffer = pBuffer;
		return true;
	}
}