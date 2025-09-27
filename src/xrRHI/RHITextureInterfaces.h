#pragma once

#include "RHITextureFormat.h"
#include "RHIEnums.h"

// Forward declarations
class IRHISurface;
class IRHIShaderResourceView;
class IRHIRenderTargetView;
class IRHIDepthStencilView;

// Shader Resource View Descriptor
struct RHIShaderResourceViewDesc
{
	ERHI_FORMAT Format = ERHI_FORMAT::UNKNOWN;
	u32 ViewDimension = 0;    // D3D11_SRV_DIMENSION
	u32 MostDetailedMip = 0;  // Most detailed mip level
	u32 MipLevels = 0;        // Number of mip levels
	u32 FirstArraySlice = 0;  // For texture arrays
	u32 ArraySize = 1;        // For texture arrays
};

// Render Target View Descriptor
struct RHIRenderTargetViewDesc
{
	ERHI_FORMAT Format = ERHI_FORMAT::UNKNOWN;
	u32 ViewDimension = 0;    // D3D11_RTV_DIMENSION
	u32 MipSlice = 0;         // For texture arrays
	u32 FirstArraySlice = 0;  // For texture arrays
	u32 ArraySize = 1;        // For texture arrays
};

// Depth Stencil View Descriptor
struct RHIDepthStencilViewDesc
{
	ERHI_FORMAT Format = ERHI_FORMAT::UNKNOWN;
	u32 ViewDimension = 0;    // D3D11_DSV_DIMENSION
	u32 Flags = 0;            // D3D11_DSV_FLAG
	u32 MipSlice = 0;         // For texture arrays
	u32 FirstArraySlice = 0;  // For texture arrays
	u32 ArraySize = 1;        // For texture arrays
};

class IRHITexture
{
public:
	virtual ~IRHITexture() = default;
	
	virtual void AddRef() = 0;
	virtual u32 Release() = 0;
	
	virtual void* GetRawTexture() = 0;
	
	virtual u32 GetWidth() const = 0;
	virtual u32 GetHeight() const = 0;
	virtual u32 GetDepth() const = 0;
	virtual u32 GetMipLevels() const = 0;
	virtual u32 GetTextureType() const = 0;
	virtual u32 GetMiscFlags() const = 0;
	virtual u32 GetSampleDescCount() const = 0;
	virtual u32 GetArraySize() const = 0;
	virtual ERHI_FORMAT GetFormat() const = 0;
	virtual ERHI_USAGE GetUsage() const = 0;
};

class IRHISurface :
	public IRHITexture
{
public:
	virtual ~IRHISurface() = default;
	
	virtual IRHIShaderResourceView* GetShaderResourceView() = 0;
	virtual IRHIRenderTargetView* GetRenderTargetView() = 0;
	virtual IRHIDepthStencilView* GetDepthStencilView() = 0;
	
	virtual bool UpdateData(const void* data, u32 size) = 0;
	
	virtual void* Lock(u32 mipLevel = 0, u32* pitch = nullptr) = 0;
	virtual void Unlock() = 0;
};

class IRHIShaderResourceView
{
public:
	virtual ~IRHIShaderResourceView() = default;
	
	virtual void* GetRawSRV() = 0;
	
	virtual IRHISurface* GetSurface() = 0;
	
	virtual void BindToPixelShader(u32 slot) = 0;
	virtual void BindToVertexShader(u32 slot) = 0;
	virtual void BindToGeometryShader(u32 slot) = 0;
	virtual void BindToComputeShader(u32 slot) = 0;
	
	virtual void AddRef() = 0;
	virtual u32 Release() = 0;
};

class IRHIRenderTargetView
{
public:
	virtual ~IRHIRenderTargetView() = default;
	
	virtual void* GetRawRTV() = 0;
	
	virtual IRHISurface* GetSurface() = 0;
	
	virtual void BindAsRenderTarget(u32 slot = 0) = 0;
	virtual void UnbindRenderTarget() = 0;
	
	virtual void AddRef() = 0;
	virtual u32 Release() = 0;
};

class IRHIDepthStencilView
{
public:
	virtual ~IRHIDepthStencilView() = default;
	
	virtual void* GetRawDSV() = 0;
	
	virtual IRHISurface* GetSurface() = 0;
	
	virtual void BindAsDepthStencil() = 0;
	virtual void UnbindDepthStencil() = 0;
	
	virtual void AddRef() = 0;
	virtual u32 Release() = 0;
};

struct RHITextureDesc
{
	u32 Width = 0;
	u32 Height = 0;
	u32 Depth = 1;
	u32 MipLevels = 1;
	ERHI_FORMAT Format = ERHI_FORMAT::UNKNOWN;
	u32 Usage = 0;
	u32 BindFlags = 0;
	u32 CPUAccessFlags = 0;
	u32 MiscFlags = 0;
	u32 ArraySize = 1;
	u32 SampleDescCount = 1;

	RHITextureDesc() = default;
	RHITextureDesc(u32 width, u32 height, ERHI_FORMAT format)
		: Width(width), Height(height), Format(format) {}
};

class IRHITextureFactory
{
public:
	virtual ~IRHITextureFactory() = default;

	virtual IRHISurface* CreateTextureFromFile(const char* filename, u32& memorySize) = 0;
	virtual IRHISurface* CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc) = 0;
	virtual IRHISurface* CreateRenderTarget(const RHITextureDesc& desc) = 0;
	virtual IRHISurface* CreateDepthStencil(const RHITextureDesc& desc) = 0;
	virtual IRHIShaderResourceView* CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc) = 0;
	virtual IRHIRenderTargetView* CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc = {}) = 0;
	virtual IRHIDepthStencilView* CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc = {}) = 0;
};
