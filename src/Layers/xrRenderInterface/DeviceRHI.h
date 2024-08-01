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
	READ_AND_WRITE
};

class IBuffer
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
