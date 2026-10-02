#pragma once
#include <d3d11.h>
#include "RHI.h"

u32 GetD3D11CPUAccess(ERHI_CPU_ACCESS_FLAG flag);

class CD3D11Buffer : 
	public IRHIBuffer
{
public:
	CD3D11Buffer();
	~CD3D11Buffer();

	void Create(const RHIBufferDesc& desc = {}, const RHIBufferSubresource* pSubresource = nullptr);

	bool Map(ERHI_BUFFER_MAP MapType, u32 MapFlags, RHIMappedSubresource* pData) override;
	void Unmap() override;

	void UpdateSubresource(void* pData, u32 Size) override;

	void SetVertexBuffer(u32 StartSlot, const u32 Stride, const u32 Offset) override;
	void SetIndexBuffer(bool Is32BitBuffer, u32 Offset) override;

	void AddRef() override;
	u32 Release() override;
	u32 GetSize() const override { return m_bufferDesc.Size; }

	ID3D11Buffer* GetD3DObject();

private:
	ID3D11Buffer* m_pBuffer;
	RHIBufferDesc m_bufferDesc;
};