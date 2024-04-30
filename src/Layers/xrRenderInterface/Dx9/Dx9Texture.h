#pragma once

class CD3D9Texture : public IRHITexture
{
public:
	CD3D9Texture();
	~CD3D9Texture();

	HRESULT Create(const TextureDesc* pTextureDesc, LPSUBRESOURCE_DATA pSubresourceData);

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;
	void SetStage(u32 Stage) override;

	void SetData(const void* pData, const int size);

private:
	IDirect3DTexture9* m_pTexture;
	TextureDesc m_textureDesc;

	// Inherited via IRHITexture
	EResourceType GetType() override;

	// Inherited via IRHITexture
	u32 GetLevelCount() override;
};
