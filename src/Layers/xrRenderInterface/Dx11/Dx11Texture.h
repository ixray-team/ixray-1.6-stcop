#pragma once

#include "DeviceRHI.h"

class CD3D11Texture2D :
	public ITexture2D
{
public:
	CD3D11Texture2D();
	~CD3D11Texture2D();

	HRESULT Create(const STexture2DDesc& desc, const SubresourceData* pSubresourceData);

	void GetType(eResourceDimension* pResourceDimension) override;

	void SetDebugName(const char* name) override;

private:
	ID3D11Texture2D* m_pTexture2D;

};