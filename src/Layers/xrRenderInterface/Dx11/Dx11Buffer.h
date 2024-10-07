#pragma once
#include <d3d11.h>
#include "DeviceRHI.h"

class CD3D11Buffer : public IBuffer
{
public:
	CD3D11Buffer();
	~CD3D11Buffer();

	HRESULT Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

	void* Map(eBufferMapping Mapping) override;
	void  Unmap() override;

	void UpdateSubresource(void* pData, size_t Size) override;

	ID3D11Buffer* GetD3DObject();

private:
	ID3D11Buffer* m_pBuffer;
	eBufferType m_BufferType;
	bool m_bImmutable;
};