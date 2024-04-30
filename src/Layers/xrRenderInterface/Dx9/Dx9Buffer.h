#pragma once

class CD3D9Buffer : public IRHIBuffer
{
public:
	CD3D9Buffer();
	~CD3D9Buffer();

	HRESULT Create(eBufferType bufferType, const void* pData, u32 DataSize, bool bImmutable);

	void UpdateData(const void* data, int size) override;

	bool Lock(u32 OffsetToLock, u32 SizeToLock, void** ppbData, eLockType Flags) override;
	bool Unlock() override;

	IDirect3DVertexBuffer9* GetD3DVertexBuffer();
	IDirect3DIndexBuffer9* GetD3DIndexBuffer();

private:
	template <typename T>
	void SetData(T* pBuffer, const void* pData, u32 DataSize);

private:
	IDirect3DVertexBuffer9* m_pVertexBuffer;
	IDirect3DIndexBuffer9* m_pIndexBuffer;
	eBufferType m_BufferType;
	bool m_bImmutable;

	// Inherited via IRHIBuffer
	EResourceType GetType() override;
};