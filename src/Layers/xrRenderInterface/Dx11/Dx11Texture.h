#pragma once
#include <d3d11.h>
#include "DeviceRHI.h"

class CD3D11Texture1D :
	public ITexture1D
{
public:
	CD3D11Texture1D();
	~CD3D11Texture1D();

	HRESULT Create(const STexture1DDesc& desc, const SubresourceData* pSubresourceData);

	// IRHIResource
	void GetType(eResourceDimension* pResourceDimension) override;
	void SetDebugName(const char* name) override;

	// ITexture1D
	void GetDesc(STexture1DDesc* desc) override;
	void GetShaderResourceView(IShaderResourceView** ppShaderResourceView) override;
	void Map(u32 Subresource, eBufferMapping MapType, u32 MapFlags, SMappedSubresource* pMappedTex1D) override;
	void Unmap(u32 Subresource) override;

	// D3D11
	ID3D11Texture1D* GetD3D11Texture() { return m_pTexture1D; }

private:
	ID3D11Texture1D* m_pTexture1D;
	ID3D11ShaderResourceView* m_pShaderResourceView;
	STexture1DDesc m_TextureDesc;
};

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

	// D3D11
	ID3D11Texture2D* GetD3D11Texture() { return m_pTexture2D; }

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

	// D3D11
	ID3D11Texture3D* GetD3D11Texture() { return m_pTexture3D; }

private:
	ID3D11Texture3D* m_pTexture3D;
	ID3D11ShaderResourceView* m_pShaderResourceView;
	STexture3DDesc m_TextureDesc;

};