#pragma once

class CD3D11Texture2D : public IRHITexture
{
public:
	CD3D11Texture2D();
	~CD3D11Texture2D();

	HRESULT Create(const TextureDesc* pTextureDesc, const void* pData, const int Size, const int Pitch);

	bool LockRect(u32 Level, LOCKED_RECT* pLockedRect, const Irect* pRect, eLockType Flags) override;
	bool UnlockRect(u32 Level) override;
	void SetStage(u32 Stage) override;

private:
	ID3D11Texture2D* m_pTexture;
	ID3D11ShaderResourceView* m_pTextureSRV;
	TextureDesc					m_TextureDesc;
	int							m_Pitch;
};