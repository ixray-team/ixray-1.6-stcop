#pragma once

class CD3D9Texture : public IRHITexture
{
public:
	CD3D9Texture();
	~CD3D9Texture();

	HRESULT Create(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch);

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;

	void SetData(const void* pData, const int size);

private:
	IDirect3DTexture9* m_pTexture;
	TextureDesc m_textureDesc;
};
