#pragma once

class CD3D11Buffer : public IRHIBuffer
{
public:
	CD3D11Buffer();
	~CD3D11Buffer();

	HRESULT Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

	void UpdateData(const void* data, int size) override;

	bool Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags) override;
	bool Unlock() override;

	ID3D11Buffer* GetD3DBufferObject();

private:
	ID3D11Buffer* m_pBuffer;
	eBufferType m_BufferType;
	bool m_bImmutable;

	// Inherited via IRHIBuffer
	EResourceType GetType() override;
};