/**************************************************************************************
* Copyright (C) 2026 IXRay-Team
* IX-Ray Renderer
***************************************************************************************
* Source code is licensed under the following terms:
*
* "Team" means the maintainers of the official IX-Ray repository.
*
* 1. IX-Ray Team License
*    The Team may copy, modify, merge, publish and distribute this Software
*    and its documentation, in source and binary form, without restriction.
*
* 2. Public Access License
*    Non-exclusive rights granted to everyone else.
*
*    Permitted rights:
*      a. Read and clone the source code.
*      b. Fork the repository and modify the source code for the sole purpose
*         of proposing the change to the official repository. By proposing it,
*         the author grants the Team a worldwide, perpetual, irrevocable,
*         royalty-free right to use, modify, relicense and distribute it.
*      c. Redistribute binaries built and published by the Team, unmodified
*         and together with this notice.
*
* Usage of this Software beyond the rights granted above is strictly prohibited,
* including distribution of the source code or of any modified version of it and
* distribution of binaries not built by the Team.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT
* HOLDERS ARE NOT LIABLE FOR ANY CLAIM OR DAMAGE ARISING FROM IT.
*
* The above copyright notice and this license text must be included in all
* copies or substantial portions of the Software.
**************************************************************************************/
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
	ERHI_SRV_DIMENSION ViewDimension = ERHI_SRV_DIMENSION::UNKNOWN;
	u32 MostDetailedMip = 0;  // Most detailed mip level
	u32 MipLevels = 0;        // Number of mip levels
	u32 FirstArraySlice = 0;  // For texture arrays
	u32 ArraySize = 1;        // For texture arrays
	u32 ElementWidth = 0;
};

// Render Target View Descriptor
struct RHIRenderTargetViewDesc
{
	ERHI_FORMAT Format = ERHI_FORMAT::UNKNOWN;
	ERHI_RTV_DIMENSION ViewDimension = ERHI_RTV_DIMENSION::UNKNOWN;
	u32 MipSlice = 0;         // For texture arrays
	u32 FirstArraySlice = 0;  // For texture arrays
	u32 ArraySize = 1;        // For texture arrays
};

// Depth Stencil View Descriptor
struct RHIDepthStencilViewDesc
{
	ERHI_FORMAT Format = ERHI_FORMAT::UNKNOWN;
	ERHI_DSV_DIMENSION ViewDimension = ERHI_DSV_DIMENSION::UNKNOWN;
	u32 Flags = 0;            // D3D11_DSV_FLAG
	u32 MipSlice = 0;         // For texture arrays
	u32 FirstArraySlice = 0;  // For texture arrays
	u32 ArraySize = 1;        // For texture arrays
};

struct RHIUAVDesc
{
	ERHI_FORMAT Format;
	ERHI_VIEW_DIMENSION ViewDimension;
	u32 FirstElement;
	u32 NumElements;
	u32 MipSlice; // Для текстур
	u32 FirstWSlice; // Для 3D текстур
	u32 WSize; // Для 3D текстур
};

struct RHISubResource
{
	const void* Data;
	u32 DataSize;
	u32 Width;
	u32 Height;
	u32 Depth;
	u32 RowPitch;
	u32 DepthPitch;
	ERHI_FORMAT TextureFormat;

	RHISubResource() : Data(nullptr), DataSize(0), Width(0), Height(0),
		Depth(1), RowPitch(0), DepthPitch(0),
		TextureFormat(ERHI_FORMAT::UNKNOWN) {
	}
};

struct RHIBox
{
	u32 left;
	u32 top;
	u32 front;
	u32 right;
	u32 bottom;
	u32 back;
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
	virtual ERHI_RESOURCE_DIMENSION GetTextureType() const = 0;
	virtual u32 GetMiscFlags() const = 0;
	virtual u32 GetSampleDescCount() const = 0;
	virtual u32 GetArraySize() const = 0;
	virtual ERHI_FORMAT GetFormat() const = 0;
	virtual ERHI_USAGE GetUsage() const = 0;
};

struct RHITextureMetadata
{
	u32 mipmap_count = 0;
	u32 width = 0;
	u32 height = 0;
	int format = (int)ERHI_FORMAT::UNKNOWN;
	bool cubemap = false;
	bool volumemap = false;
};

class IRHISurface :
	public IRHITexture
{
public:
	virtual ~IRHISurface() = default;

	virtual bool UpdateData(u32 mipLevel, u32 arrayLayer, const RHISubResource* subResource, const RHIBox& Box) = 0;

	virtual void* Lock(u32 mipLevel = 0, u32* pitch = nullptr) = 0;
	virtual void Unlock() = 0;
};

class IRHIShaderResourceView
{
public:
	virtual ~IRHIShaderResourceView() = default;

	virtual void* GetRawSRV() = 0;

	virtual void AddRef() = 0;
	virtual u32 Release() = 0;
};

class IRHIRenderTargetView
{
public:
	virtual ~IRHIRenderTargetView() = default;

	virtual void* GetRawRTV() = 0;

	virtual IRHISurface* GetSurface() = 0;

	virtual void AddRef() = 0;
	virtual u32 Release() = 0;
};

class IRHIDepthStencilView
{
public:
	virtual ~IRHIDepthStencilView() = default;

	virtual void* GetRawDSV() = 0;

	virtual IRHISurface* GetSurface() = 0;

	virtual void AddRef() = 0;
	virtual u32 Release() = 0;
	virtual ERHI_DSV_DIMENSION GetDimension() const = 0;
};

struct RHITextureDesc
{
	u32 Width = 0;
	u32 Height = 0;
	u32 Depth = 1;
	u32 MipLevels = 1;
	ERHI_FORMAT Format = ERHI_FORMAT::UNKNOWN;
	ERHI_USAGE Usage = ERHI_USAGE::USAGE_DEFAULT;
	ERHI_BIND_FLAG BindFlags = ERHI_BIND_FLAG::NOT_SET;
	u32 CPUAccessFlags = 0;
	u32 MiscFlags = 0;
	u32 ArraySize = 1;
	u32 SampleDescCount = 1;
	float ClearColor[4] = {};
	bool OptimizedColorClear = true;
	// A texture made through NRI that the old backend wraps through CreateTextureFromMemory instead of making one
	// (ID3D11Resource, ID3D12Resource). Only Format, the typed format its views are made with, and the cube bit of
	// MiscFlags are taken from here, the rest from the texture. Read by shaders for its whole life: D3D12 takes it so
	void* Adopt = nullptr;

	RHITextureDesc() = default;
	RHITextureDesc(u32 width, u32 height, ERHI_FORMAT format)
		: Width(width), Height(height), Format(format) {
	}
};

class IRHIUnorderedAccessView
{
public:
	virtual ~IRHIUnorderedAccessView() = default;
	virtual void* GetRaw() = 0;
	virtual void AddRef() = 0;
	virtual u32 Release() = 0;
};

class IRHITextureFactory
{
public:
	virtual ~IRHITextureFactory() = default;

	virtual IRHISurface* CreateTexture2D(const RHITextureDesc& Desc, const RHISubResource* SubResource) = 0;
	virtual IRHISurface* CreateTexture3D(const RHITextureDesc& Desc, const RHISubResource* SubResource) = 0;
	virtual IRHISurface* CreateTextureFromMemory(const void* data, u32 size, const RHITextureDesc& desc) = 0;
	virtual IRHISurface* CreateRenderTarget(const RHITextureDesc& desc) = 0;
	virtual IRHIShaderResourceView* CreateShaderResourceView(IRHISurface* surface, const RHIShaderResourceViewDesc* desc) = 0;
	virtual IRHIRenderTargetView* CreateRenderTargetView(IRHISurface* surface, const RHIRenderTargetViewDesc& desc = {}) = 0;
	virtual IRHIDepthStencilView* CreateDepthStencilView(IRHISurface* surface, const RHIDepthStencilViewDesc& desc = {}) = 0;
	virtual IRHIUnorderedAccessView* CreateUAV(IRHISurface* pTexture, const RHIUAVDesc& desc) = 0;
};
