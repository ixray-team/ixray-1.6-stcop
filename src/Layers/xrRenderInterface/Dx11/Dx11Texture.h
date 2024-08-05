#pragma once

#include "DeviceRHI.h"

class CD3D11Texture2D :
	public ITexture2D
{
public:
	CD3D11Texture2D();
	~CD3D11Texture2D();

	HRESULT Create(const STexture2DDesc& desc, const SubresourceData* pSubresourceData);

	// IRHIResource
	void GetType(eResourceDimension* pResourceDimension) override;
	void SetDebugName(const char* name) override;

	// ITexture2D
	void GetDesc(STexture2DDesc* desc) override;
	void GetShaderResourceView(IShaderResourceView** ppShaderResourceView) override;
	void Map(u32 Subresource, eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pMappedTex2D) override;
	void Unmap(u32 Subresource) override;

private:
	ID3D11Texture2D* m_pTexture2D;
	ID3D11ShaderResourceView* m_pShaderResourceView;
	STexture2DDesc m_TextureDesc;
};

class CD3D11Texture3D :
	public ITexture3D
{
public:
	CD3D11Texture3D();
	~CD3D11Texture3D();

	HRESULT Create(const STexture3DDesc& desc, const SubresourceData* pSubresourceData);

	// IRHIResource
	void GetType(eResourceDimension* pResourceDimension) override;
	void SetDebugName(const char* name) override;

	// ITexture3D
	void GetDesc(STexture3DDesc* desc) override;
	void GetShaderResourceView(IShaderResourceView** ppShaderResourceView) override;
	void Map(u32 Subresource, eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pMappedTex3D) override;
	void Unmap(u32 Subresource) override;

private:
	ID3D11Texture3D* m_pTexture3D;
	ID3D11ShaderResourceView* m_pShaderResourceView;
	STexture3DDesc m_TextureDesc;

};