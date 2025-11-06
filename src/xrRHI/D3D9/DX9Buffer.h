#pragma once
#include <d3d9.h>
#include "RHI.h"

class CD3D9Buffer :
	public IRHIBuffer
{
public:
	CD3D9Buffer(IDirect3DDevice9* pDevice, const RHIBufferDesc& desc, const RHIBufferSubresource* pInitData);
	~CD3D9Buffer();

	void Create(const RHIBufferDesc& desc = {}, const RHIBufferSubresource* pSubresource = nullptr);

	bool Map(ERHI_BUFFER_MAP MapType, u32 MapFlags, RHIMappedSubresource* pData) override;
	void Unmap() override;

	void UpdateSubresource(void* pData, u32 Size) override;

	void SetVertexBuffer(u32 StartSlot, const u32 Stride, const u32 Offset) override;
	void SetIndexBuffer(bool Is32BitBuffer, u32 Offset) override;

	void AddRef() override;
	u32 Release() override;

private:
	IDirect3DVertexBuffer9* VertexBuffer = nullptr;
	IDirect3DIndexBuffer9* IndexBuffer = nullptr;

	RHIBufferDesc m_bufferDesc;

	IDirect3DDevice9* m_pDev;

	u32 refCountVB = 0;
	u32 refCountIB = 0;
};